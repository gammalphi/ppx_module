open Ppxlib
open Ast_builder.Default

let attr_name = "ppx_module"

type annotation = {
  name: string option;
  cluster: string option;
  parents: bool;
}

let expr_string = function
  | {pexp_desc= Pexp_constant (Pconst_string (s, _, _)); _} -> Some s
  | _ -> None

let expr_bool = function
  | {pexp_desc= Pexp_construct ({txt= Lident "true"; _}, None); _} -> Some true
  | {pexp_desc= Pexp_construct ({txt= Lident "false"; _}, None); _} -> Some false
  | _ -> None

let parse_record_payload ~loc fields =
  let name = ref None in
  let cluster = ref None in
  let parents = ref true in
  let set field value =
    match field with
    | "name" -> name := Some value
    | "cluster" -> cluster := Some value
    | field ->
        Location.raise_errorf ~loc
          "unknown [@@ppx_module] field %S; supported fields are: name, cluster, parents"
          field
  in
  List.iter
    (fun ({txt= lid; loc}, expr) ->
      let field =
        match lid with
        | Lident field -> field
        | _ ->
            Location.raise_errorf ~loc
              "[@@ppx_module] fields must be unqualified"
      in
      match field, expr_string expr, expr_bool expr with
      | ("name" | "cluster"), Some value, _ -> set field value
      | "parents", _, Some value -> parents := value
      | "parents", _, None ->
          Location.raise_errorf ~loc
            "[@@ppx_module] field %S must be a boolean literal"
            field
      | ("name" | "cluster"), None, _ ->
          Location.raise_errorf ~loc
            "[@@ppx_module] field %S must be a string literal"
            field
      | _ ->
          Location.raise_errorf ~loc
            "unknown [@@ppx_module] field %S; supported fields are: name, cluster, parents"
            field)
    fields;
  {name= !name; cluster= !cluster; parents= !parents}

let parse_annotation attr =
  match attr.attr_payload with
  | PStr [] -> {name= None; cluster= None; parents= true}
  | PStr
      [{pstr_desc=
          Pstr_eval
            ({pexp_desc= Pexp_record (fields, None); pexp_loc= loc; _}, _);
        _}] ->
      parse_record_payload ~loc fields
  | _ ->
      Location.raise_errorf
        ~loc:attr.attr_loc
        "[@@ppx_module] expects no payload or a record payload, e.g. \
         [@@ppx_module] or \
         [@@ppx_module { name = \"Memory.Domain\"; cluster = \"memory\" }]"

let annotation mb =
  mb.pmb_attributes
  |> List.find_opt (fun a -> a.attr_name.txt = attr_name)
  |> Option.map parse_annotation

let without_attr attrs =
  List.filter (fun a -> a.attr_name.txt <> attr_name) attrs

let collect_functor_params expr =
  let rec go acc e =
    match e.pmod_desc with
    | Pmod_functor (Named ({txt= Some n; _}, _), body) -> go (acc @ [n]) body
    | Pmod_functor (Named ({txt= None; _}, _), body) -> go acc body
    | Pmod_functor (Unit, body) -> go acc body
    | Pmod_constraint (me, _) -> go acc me
    | _ -> acc
  in
  go [] expr

type metadata = {
  loc: Location.t;
  source_file: string;
  source_line: int;
  source_item_name: string;
  name: string;
  cluster: string option;
  parents: bool;
  lexical_path: string list;
  context: string list;
  functor_params: string list;
}

let default_name source_file =
  source_file
  |> Filename.basename
  |> Filename.remove_extension
  |> String.capitalize_ascii

let module_metadata ~(annotation : annotation) ~context ~lexical_stack item mb =
  let source_file = item.pstr_loc.loc_start.pos_fname in
  let loc = mb.pmb_loc in
  let source_item_name = Option.value ~default:"_" mb.pmb_name.txt in
  let name = Option.value ~default:(default_name source_file) annotation.name in
  {
    loc;
    source_file;
    source_line= loc.loc_start.pos_lnum;
    source_item_name;
    name;
    cluster= annotation.cluster;
    parents= annotation.parents;
    lexical_path= lexical_stack @ [name];
    context;
    functor_params= collect_functor_params mb.pmb_expr;
  }

let string_list_expr ~loc xs = xs |> List.map (estring ~loc) |> elist ~loc

let option_string_expr ~loc = function
  | None -> [%expr None]
  | Some s -> [%expr Some [%e estring ~loc s]]

let edge_expr ~loc kind param =
  let label = estring ~loc param in
  let kind = pexp_construct ~loc {txt= Ldot (Lident "Ppx_module", kind); loc} None in
  let node = pexp_ident ~loc {txt= Ldot (Lident param, "ppx_module_node"); loc} in
  [%expr [%e label], [%e kind], [%e node]]

let gen_bindings meta ~parent_params =
  let loc = meta.loc in
  let parents =
    List.map (edge_expr ~loc "Parent") parent_params
    @ List.map (edge_expr ~loc "Context") meta.context
    |> elist ~loc
  in
  [
    [%stri let ppx_module_name = lazy [%e estring ~loc meta.name]];
    [%stri
      let ppx_module_node =
        Ppx_module.build_with_metadata
          ~source_file:[%e estring ~loc meta.source_file]
          ~source_line:[%e eint ~loc meta.source_line]
          ~source_item_name:[%e estring ~loc meta.source_item_name]
          ~cluster:[%e option_string_expr ~loc meta.cluster]
          ~lexical_path:[%e string_list_expr ~loc meta.lexical_path]
          ~functor_params:[%e string_list_expr ~loc meta.functor_params]
          ~name:ppx_module_name
          ~parents:[%e parents]];
  ]

let inject_into_body meta expr =
  let rec go params_acc e =
    match e.pmod_desc with
    | Pmod_functor (Named (({txt= Some n; _} as nloc), mt), body) ->
        {e with pmod_desc= Pmod_functor (Named (nloc, mt), go (params_acc @ [n]) body)}
    | Pmod_functor (Named (({txt= None; _} as nloc), mt), body) ->
        {e with pmod_desc= Pmod_functor (Named (nloc, mt), go params_acc body)}
    | Pmod_functor (Unit, body) ->
        {e with pmod_desc= Pmod_functor (Unit, go params_acc body)}
    | Pmod_constraint (me, mt) ->
        {e with pmod_desc= Pmod_constraint (go params_acc me, mt)}
    | Pmod_structure items ->
        let parent_params = if meta.parents then params_acc else [] in
        {e with pmod_desc= Pmod_structure (items @ gen_bindings meta ~parent_params)}
    | _ -> e
  in
  go [] expr

let mapper =
  object (self)
    inherit Ast_traverse.map as super
    val mutable context_params : string list = []
    val mutable lexical_stack : string list = []

    method private rewrite_module_binding item mb =
      match annotation mb with
      | None -> super#module_binding mb
      | Some annotation ->
          let saved_context = context_params in
          let saved_lexical = lexical_stack in
          let meta =
            module_metadata
              ~annotation
              ~context:saved_context
              ~lexical_stack:saved_lexical
              item
              mb
          in
          context_params <- saved_context @ meta.functor_params;
          lexical_stack <- meta.lexical_path;
          let mb = super#module_binding mb in
          context_params <- saved_context;
          lexical_stack <- saved_lexical;
          let pmb_expr = inject_into_body meta mb.pmb_expr in
          let pmb_attributes = without_attr mb.pmb_attributes in
          {mb with pmb_expr; pmb_attributes}

    method! structure_item item =
      match item.pstr_desc with
      | Pstr_module mb ->
          {item with pstr_desc= Pstr_module (self#rewrite_module_binding item mb)}
      | Pstr_recmodule mbs ->
          {item with pstr_desc= Pstr_recmodule (List.map (self#rewrite_module_binding item) mbs)}
      | _ -> super#structure_item item
  end

let () = Driver.register_transformation "ppx_module" ~impl:mapper#structure

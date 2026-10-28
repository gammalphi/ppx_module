open Ppxlib
open Ast_builder.Default

let attr_name = "ppx_module"

let unknown_field_error ~loc field =
  Location.raise_errorf ~loc
    "unknown [@@ppx_module] field %S; supported fields are: name, cluster, \
     parents, dependencies, forward"
    field

type annotation = {
  name : string option;
  cluster : string option;
  parents : bool;
  dependencies : (string * string) list;
  forward : string option;
}

let expr_string = function
  | { pexp_desc = Pexp_constant (Pconst_string (s, _, _)); _ } -> Some s
  | _ -> None

let expr_bool = function
  | { pexp_desc = Pexp_construct ({ txt = Lident "true"; _ }, None); _ } ->
      Some true
  | { pexp_desc = Pexp_construct ({ txt = Lident "false"; _ }, None); _ } ->
      Some false
  | _ -> None

let parse_record_payload fields =
  let name = ref None in
  let cluster = ref None in
  let parents = ref true in
  let dependencies = ref [] in
  let forward = ref None in
  let field_name { txt; loc } =
    match txt with
    | Lident field -> field
    | _ ->
        Location.raise_errorf ~loc "[@@ppx_module] fields must be unqualified"
  in
  let parse_string_field ~loc field expr =
    match expr_string expr with
    | Some value -> value
    | None ->
        Location.raise_errorf ~loc
          "[@@ppx_module] field %S must be a string literal" field
  in
  let parse_bool_field ~loc field expr =
    match expr_bool expr with
    | Some value -> value
    | None ->
        Location.raise_errorf ~loc
          "[@@ppx_module] field %S must be a boolean literal" field
  in
  let parse_field (({ loc; _ } as lid), expr) =
    match field_name lid with
    | "name" -> name := Some (parse_string_field ~loc "name" expr)
    | "cluster" -> cluster := Some (parse_string_field ~loc "cluster" expr)
    | "parents" -> parents := parse_bool_field ~loc "parents" expr
    | "forward" -> forward := Some (parse_string_field ~loc "forward" expr)
    | "dependencies" ->
        let rec parse = function
          | { pexp_desc = Pexp_construct ({txt = Lident "[]"; _}, None); _ } -> []
          | { pexp_desc = Pexp_construct ({txt = Lident "::"; _},
              Some {pexp_desc = Pexp_tuple [head; tail]; _}); _ } ->
              let dep = match head.pexp_desc with
                | Pexp_tuple [label; path] ->
                    parse_string_field ~loc "dependencies" label,
                    parse_string_field ~loc "dependencies" path
                | _ -> Location.raise_errorf ~loc "dependencies expects (label, module-path) string pairs"
              in dep :: parse tail
          | _ -> Location.raise_errorf ~loc "dependencies expects a literal list"
        in dependencies := parse expr
    | field -> unknown_field_error ~loc field
  in
  List.iter parse_field fields;
  if !forward <> None && (!name <> None || !cluster <> None || !dependencies <> []) then
    Location.raise_errorf ~loc:Location.none "forward cannot be combined with name, cluster or dependencies";
  { name = !name; cluster = !cluster; parents = !parents;
    dependencies = !dependencies; forward = !forward }

let parse_annotation attr =
  match attr.attr_payload with
  | PStr [] -> { name = None; cluster = None; parents = true; dependencies = []; forward = None }
  | PStr
      [
        {
          pstr_desc =
            Pstr_eval ({ pexp_desc = Pexp_record (fields, None); _ }, _);
          _;
        };
      ] ->
      parse_record_payload fields
  | _ ->
      Location.raise_errorf ~loc:attr.attr_loc
        "[@@ppx_module] expects no payload or a record payload, e.g. \
         [@@ppx_module] or [@@ppx_module { name = \"Memory.Domain\"; cluster = \
         \"memory\" }]"

let annotation mb =
  mb.pmb_attributes
  |> List.find_opt (fun a -> a.attr_name.txt = attr_name)
  |> Option.map parse_annotation

let without_attr attrs =
  List.filter (fun a -> a.attr_name.txt <> attr_name) attrs

let collect_functor_params expr =
  let rec go acc e =
    match e.pmod_desc with
    | Pmod_functor (Named ({ txt = Some n; _ }, _), body) ->
        go (acc @ [ n ]) body
    | Pmod_functor (Named ({ txt = None; _ }, _), body) -> go acc body
    | Pmod_functor (Unit, body) -> go acc body
    | Pmod_constraint (me, _) -> go acc me
    | _ -> acc
  in
  go [] expr

type metadata = {
  loc : Location.t;
  source_file : string;
  source_line : int;
  source_item_name : string;
  name : string;
  cluster : string option;
  parents : bool;
  dependencies : (string * string) list;
  forward : string option;
  lexical_path : string list;
  context : string list;
  functor_params : string list;
}

let default_name source_file =
  source_file |> Filename.basename |> Filename.remove_extension
  |> String.capitalize_ascii

let module_metadata ~(annotation : annotation) ~context ~lexical_stack item mb =
  let source_file = item.pstr_loc.loc_start.pos_fname in
  let loc = mb.pmb_loc in
  let source_item_name = Option.value ~default:"_" mb.pmb_name.txt in
  let name = Option.value ~default:(default_name source_file) annotation.name in
  {
    loc;
    source_file;
    source_line = loc.loc_start.pos_lnum;
    source_item_name;
    name;
    cluster = annotation.cluster;
    parents = annotation.parents;
    dependencies = annotation.dependencies;
    forward = annotation.forward;
    lexical_path = lexical_stack @ [ name ];
    context;
    functor_params = collect_functor_params mb.pmb_expr;
  }

let string_list_expr ~loc xs = xs |> List.map (estring ~loc) |> elist ~loc

let option_string_expr ~loc = function
  | None -> [%expr None]
  | Some s -> [%expr Some [%e estring ~loc s]]

let module_value_expr ~loc module_name value_name =
  let segments = String.split_on_char '.' module_name in
  let valid_segment s =
    String.length s > 0 && s.[0] >= 'A' && s.[0] <= 'Z'
    && String.for_all (function
      | 'A'..'Z' | 'a'..'z' | '0'..'9' | '_' | '\'' -> true
      | _ -> false) s
  in
  if not (List.for_all valid_segment segments) then
    Location.raise_errorf ~loc "invalid module path %S in ppx_module" module_name;
  pexp_ident ~loc { txt = Ldot (Longident.parse module_name, value_name); loc }

type dependency_kind = Parent_dependency | Context_dependency

let dependency_kind_expr ~loc = function
  | Parent_dependency -> [%expr Ppx_module.Parent]
  | Context_dependency -> [%expr Ppx_module.Context]

let dependency_expr ~loc kind param =
  let label = estring ~loc param in
  let kind = dependency_kind_expr ~loc kind in
  let node = module_value_expr ~loc param "ppx_module_node" in
  [%expr [%e label], [%e kind], [%e node]]

let dependency_exprs ~loc ~parents ~context =
  List.map (dependency_expr ~loc Parent_dependency) parents
  @ List.map (dependency_expr ~loc Context_dependency) context
  |> elist ~loc

let gen_bindings meta ~parent_params =
  let loc = meta.loc in
  let parents =
    let explicit = List.map (fun (label, path) ->
      let node = module_value_expr ~loc path "ppx_module_node" in
      [%expr [%e estring ~loc label], Ppx_module.Parent, [%e node]]) meta.dependencies in
    let automatic = List.map (dependency_expr ~loc Parent_dependency) parent_params
      @ List.map (dependency_expr ~loc Context_dependency) meta.context in
    elist ~loc (automatic @ explicit)
  in
  match meta.forward with
  | Some path ->
      [ [%stri let ppx_module_name = [%e module_value_expr ~loc path "ppx_module_name"]];
        [%stri let ppx_module_node = [%e module_value_expr ~loc path "ppx_module_node"]] ]
  | None ->
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
          ~name:ppx_module_name ~parents:[%e parents]];
  ]

let add_generated_bindings meta params_acc items =
  let parent_params = if meta.parents then params_acc else [] in
  items @ gen_bindings meta ~parent_params

let inject_into_body meta expr =
  let rec go params_acc e =
    match e.pmod_desc with
    | Pmod_functor (Named (({ txt = Some n; _ } as nloc), mt), body) ->
        {
          e with
          pmod_desc =
            Pmod_functor (Named (nloc, mt), go (params_acc @ [ n ]) body);
        }
    | Pmod_functor (Named (({ txt = None; _ } as nloc), mt), body) ->
        {
          e with
          pmod_desc = Pmod_functor (Named (nloc, mt), go params_acc body);
        }
    | Pmod_functor (Unit, body) ->
        { e with pmod_desc = Pmod_functor (Unit, go params_acc body) }
    | Pmod_constraint (me, mt) ->
        { e with pmod_desc = Pmod_constraint (go params_acc me, mt) }
    | Pmod_structure items ->
        {
          e with
          pmod_desc =
            Pmod_structure (add_generated_bindings meta params_acc items);
        }
    | _ -> e
  in
  go [] expr

let mapper =
  object (self)
    inherit Ast_traverse.map as super
    val mutable context_params : string list = []
    val mutable lexical_stack : string list = []

    method private map_nested_modules meta mb =
      let saved_context = context_params in
      let saved_lexical = lexical_stack in
      context_params <- saved_context @ meta.functor_params;
      lexical_stack <- meta.lexical_path;
      Fun.protect
        (fun () -> super#module_binding mb)
        ~finally:(fun () ->
          context_params <- saved_context;
          lexical_stack <- saved_lexical)

    method private rewrite_module_binding item mb =
      match annotation mb with
      | None -> super#module_binding mb
      | Some annotation ->
          let meta =
            module_metadata ~annotation ~context:context_params ~lexical_stack
              item mb
          in
          let mb = self#map_nested_modules meta mb in
          let pmb_expr = inject_into_body meta mb.pmb_expr in
          let pmb_attributes = without_attr mb.pmb_attributes in
          { mb with pmb_expr; pmb_attributes }

    method! structure items =
      let attrs = List.filter_map (fun item -> match item.pstr_desc with
        | Pstr_attribute attr when attr.attr_name.txt = attr_name -> Some (item, attr)
        | _ -> None) items in
      match attrs with
      | [] -> super#structure items
      | [(item, attr)] ->
          let annotation = parse_annotation attr in
          let body = List.filter (fun item -> match item.pstr_desc with
            | Pstr_attribute attr -> attr.attr_name.txt <> attr_name
            | _ -> true) items in
          let loc = item.pstr_loc in
          let mb = module_binding ~loc ~name:{txt = Some "File"; loc}
            ~expr:(pmod_structure ~loc []) in
          let meta = module_metadata ~annotation ~context:context_params
            ~lexical_stack item mb in
          let body = super#structure body in
          add_generated_bindings meta [] body
      | _ -> Location.raise_errorf ~loc:(fst (List.hd attrs)).pstr_loc
          "duplicate floating ppx_module annotations"

    method! structure_item item =
      match item.pstr_desc with
      | Pstr_module mb ->
          {
            item with
            pstr_desc = Pstr_module (self#rewrite_module_binding item mb);
          }
      | Pstr_recmodule mbs ->
          {
            item with
            pstr_desc =
              Pstr_recmodule (List.map (self#rewrite_module_binding item) mbs);
          }
      | _ -> super#structure_item item
  end

let () = Driver.register_transformation "ppx_module" ~impl:mapper#structure

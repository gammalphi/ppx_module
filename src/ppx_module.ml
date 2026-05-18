type edge_kind = Parent | Context

type pp_mode = Full | Reachable

type node = {
  id: int;
  name: string;
  parents: (string * edge_kind * node lazy_t) list;
  source_file: string option;
  source_line: int option;
  source_item_name: string option;
  cluster: string option;
  lexical_path: string list;
  functor_params: string list;
  instance: int;
}

let fresh =
  let x = ref 0 in
  fun () -> incr x; !x

let registry : node list ref = ref []
let instances : (string, int) Hashtbl.t = Hashtbl.create 17

let build_node ~source_file ~source_line ~source_item_name ~cluster ~lexical_path
    ~functor_params ~name ~parents =
  Lazy.from_val @@
  let id = fresh () in
  let name = Lazy.force name in
  let instance =
    let next = 1 + Option.value ~default:0 (Hashtbl.find_opt instances name) in
    Hashtbl.replace instances name next;
    next
  in
  let node =
    { id; name; parents; source_file; source_line; source_item_name; cluster;
      lexical_path; functor_params; instance }
  in
  registry := node :: !registry;
  node

let build ~name ~parents =
  build_node ~source_file:None ~source_line:None ~source_item_name:None
    ~cluster:None ~lexical_path:[] ~functor_params:[] ~name ~parents

let build_with_metadata ~source_file ~source_line ~source_item_name ~cluster
    ~lexical_path ~functor_params ~name ~parents =
  build_node ~source_file:(Some source_file) ~source_line:(Some source_line)
    ~source_item_name:(Some source_item_name) ~cluster ~lexical_path
    ~functor_params ~name ~parents

let escape_dot s =
  let b = Buffer.create (String.length s) in
  String.iter
    (function
      | '"' -> Buffer.add_string b "\\\""
      | '\\' -> Buffer.add_string b "\\\\"
      | '\n' -> Buffer.add_string b "\\n"
      | c -> Buffer.add_char b c)
    s;
  Buffer.contents b

let pp_quoted ppf s = Format.fprintf ppf "\"%s\"" (escape_dot s)

let node_id ppf n = Format.fprintf ppf "n%d" n.id

let cluster_id path =
  "cluster_" ^ Digest.(to_hex (string (String.concat "\000" path)))

let compare_node a b = Int.compare a.id b.id

let all_registered () = List.sort compare_node !registry

let force_registered_parents () =
  let rec loop seen_len =
    let nodes = all_registered () in
    List.iter
      (fun n ->
        List.iter (fun (_, _, lazy_parent) -> ignore (Lazy.force lazy_parent)) n.parents)
      nodes;
    let new_len = List.length !registry in
    if new_len <> seen_len then loop new_len
  in
  loop (List.length !registry)

let reachable_from root =
  let visited = Hashtbl.create 16 in
  let rec visit n =
    if not (Hashtbl.mem visited n.id) then begin
      Hashtbl.add visited n.id ();
      List.iter (fun (_, _, lazy_parent) -> visit (Lazy.force lazy_parent)) n.parents
    end
  in
  visit root;
  visited

type cluster_node = {
  label: string;
  path: string list;
  cluster_name: string option;
  children: (string, cluster_node) Hashtbl.t;
  mutable nodes: node list;
}

let make_cluster ?cluster_name path label =
  { label; path; cluster_name; children = Hashtbl.create 4; nodes = [] }

let cluster_path name = "__cluster__:" ^ name

let cluster_color = function
  | "memory" -> "lightblue3"
  | "scalar" -> "palegreen3"
  | "address" -> "khaki3"
  | "offset" -> "orange3"
  | "value" -> "plum3"
  | "wrapper" -> "gray60"
  | "checks" -> "tomato3"
  | _ -> "gray65"

let child_cluster cluster ?cluster_name label =
  match Hashtbl.find_opt cluster.children label with
  | Some child -> child
  | None ->
      let child = make_cluster ?cluster_name (cluster.path @ [label]) label in
      Hashtbl.add cluster.children label child;
      child

let add_to_cluster root (node : node) =
  let root, path =
    match node.cluster with
    | None -> root, node.lexical_path
    | Some name ->
        let cluster = child_cluster root ~cluster_name:name (cluster_path name) in
        cluster, node.lexical_path
  in
  let rec go cluster = function
    | [] -> cluster.nodes <- node :: cluster.nodes
    | label :: rest ->
        let child = child_cluster cluster label in
        go child rest
  in
  go root path

let sorted_children cluster =
  cluster.children
  |> Hashtbl.to_seq_values
  |> List.of_seq
  |> List.sort (fun a b -> String.compare a.label b.label)

let display_instances nodes =
  let counts = Hashtbl.create 17 in
  let display = Hashtbl.create 17 in
  List.iter
    (fun node ->
      let next = 1 + Option.value ~default:0 (Hashtbl.find_opt counts node.name) in
      Hashtbl.replace counts node.name next;
      Hashtbl.add display node.id next)
    nodes;
  display

let pp_node ppf ~reachable ~root ~display_instance node =
  let hidden = not (Hashtbl.mem reachable node.id) in
  let source =
    match node.source_file, node.source_line with
    | Some file, Some line -> Printf.sprintf "%s:%d" (Filename.basename file) line
    | Some file, None -> Filename.basename file
    | None, Some line -> Printf.sprintf "line %d" line
    | None, None -> ""
  in
  let source_item_name = Option.value ~default:"" node.source_item_name in
  let params =
    match node.functor_params with
    | [] -> ""
    | params -> "params: " ^ String.concat ", " params
  in
  let details =
    [ Printf.sprintf "#%d" display_instance; source_item_name; params; source ]
    |> List.filter ((<>) "")
    |> String.concat "\n"
  in
  let label = if details = "" then node.name else node.name ^ "\n" ^ details in
  let color = if hidden then "gray55" else "black" in
  let fontcolor = if hidden then "gray35" else "black" in
  let style = if hidden then "dashed,rounded" else "rounded" in
  let penwidth = if node.id = root.id then "2.2" else "1" in
  Format.fprintf ppf
    "  %a [label=%a shape=box style=%a color=%a fontcolor=%a penwidth=%s];@\n"
    node_id node pp_quoted label pp_quoted style pp_quoted color pp_quoted
    fontcolor penwidth

let rec pp_cluster ppf ~reachable ~root ~display_instances cluster =
  if cluster.path = [] then begin
    List.iter
      (fun node ->
        let display_instance =
          Option.value ~default:node.instance
            (Hashtbl.find_opt display_instances node.id)
        in
        pp_node ppf ~reachable ~root ~display_instance node)
      (List.sort compare_node cluster.nodes);
    List.iter (pp_cluster ppf ~reachable ~root ~display_instances)
      (sorted_children cluster)
  end else begin
    Format.fprintf ppf "  subgraph %s {@\n" (cluster_id cluster.path);
    let label, color, penwidth =
      match cluster.cluster_name with
      | None -> cluster.label, "gray70", "1"
      | Some name -> name, cluster_color name, "2"
    in
    Format.fprintf ppf
      "    label=%a; color=%a; fontcolor=\"gray25\"; style=\"rounded\"; penwidth=%s;@\n"
      pp_quoted label pp_quoted color penwidth;
    List.iter
      (fun node ->
        let display_instance =
          Option.value ~default:node.instance
            (Hashtbl.find_opt display_instances node.id)
        in
        pp_node ppf ~reachable ~root ~display_instance node)
      (List.sort compare_node cluster.nodes);
    List.iter (pp_cluster ppf ~reachable ~root ~display_instances)
      (sorted_children cluster);
    Format.fprintf ppf "  }@\n"
  end

let pp_edge ppf ~reachable src (label, kind) dst =
  let hidden = not (Hashtbl.mem reachable src.id) || not (Hashtbl.mem reachable dst.id) in
  let style =
    match kind, hidden with
    | Parent, false -> "solid"
    | Parent, true -> "dashed"
    | Context, false -> "dashed"
    | Context, true -> "dotted"
  in
  let color = if hidden then "gray55" else "black" in
  Format.fprintf ppf "  %a -> %a [label=%a style=%a color=%a fontcolor=%a];@\n"
    node_id src node_id dst pp_quoted label pp_quoted style pp_quoted color
    pp_quoted color

let pp_legend ppf ~mode =
  Format.fprintf ppf "  subgraph cluster_legend {@\n";
  Format.fprintf ppf
    "    label=\"Legend\"; color=\"gray80\"; fontcolor=\"gray25\"; style=\"rounded\";@\n";
  Format.fprintf ppf
    "    legend_reachable [label=\"reachable node\\n#id\" shape=box style=\"rounded\" color=\"black\"];@\n";
  (match mode with
   | Full ->
       Format.fprintf ppf
         "    legend_hidden [label=\"not reachable from root\" shape=box style=\"dashed,rounded\" color=\"gray55\" fontcolor=\"gray35\"];@\n"
   | Reachable -> ());
  Format.fprintf ppf
    "    legend_cluster [label=\"cluster\" shape=box style=\"rounded\" color=\"lightblue3\" penwidth=2];@\n";
  Format.fprintf ppf
    "    legend_param_a [label=\"parameter dependency\" shape=plaintext];@\n";
  Format.fprintf ppf
    "    legend_param_b [label=\"target\" shape=plaintext];@\n";
  Format.fprintf ppf
    "    legend_context_a [label=\"context capture\" shape=plaintext];@\n";
  Format.fprintf ppf
    "    legend_context_b [label=\"target\" shape=plaintext];@\n";
  Format.fprintf ppf
    "    legend_param_a -> legend_param_b [label=\"Sub\" style=\"solid\"];@\n";
  Format.fprintf ppf
    "    legend_context_a -> legend_context_b [label=\"context\" style=\"dashed\"];@\n";
  Format.fprintf ppf "  }@\n"

let pp ?(mode=Full) ppf root =
  force_registered_parents ();
  let reachable = reachable_from root in
  let nodes =
    match mode with
    | Full -> all_registered ()
    | Reachable ->
        all_registered ()
        |> List.filter (fun node -> Hashtbl.mem reachable node.id)
  in
  let cluster_root = make_cluster [] "" in
  List.iter (add_to_cluster cluster_root) nodes;
  let display_instances = display_instances nodes in
  Format.fprintf ppf "digraph module_stack {@\n";
  Format.fprintf ppf "  graph [compound=true rankdir=TB];@\n";
  Format.fprintf ppf "  node [fontname=\"Helvetica\"];@\n";
  Format.fprintf ppf "  edge [fontname=\"Helvetica\"];@\n";
  pp_cluster ppf ~reachable ~root ~display_instances cluster_root;
  List.iter
    (fun src ->
      List.iter
        (fun (label, kind, lazy_parent) ->
          let dst = Lazy.force lazy_parent in
          if mode = Full
             || (Hashtbl.mem reachable src.id && Hashtbl.mem reachable dst.id)
          then pp_edge ppf ~reachable src (label, kind) dst)
        src.parents)
    nodes;
  pp_legend ppf ~mode;
  Format.fprintf ppf "}@\n"

module type LOGGED = sig
  val ppx_module_name : string Lazy.t
  val ppx_module_node : node Lazy.t
end

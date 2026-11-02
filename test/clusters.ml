let contains ~needle text =
  let rec loop i =
    i + String.length needle <= String.length text
    && (String.sub text i (String.length needle) = needle || loop (i + 1))
  in
  loop 0

let build ?cluster ~path name parents =
  Lazy.force
    (Ppx_module.build_with_metadata ~source_file:"clusters.ml" ~source_line:1
       ~source_item_name:name ~cluster ~lexical_path:path ~functor_params:[]
       ~name:(lazy name) ~parents)

let dot root =
  Format.asprintf "%a" (Ppx_module.pp ~mode:Ppx_module.Reachable ~line_numbers:false) root

let check_cluster ~present label output =
  let needle = Printf.sprintf "label=\"%s\";" label in
  if contains ~needle output <> present then
    failwith (Printf.sprintf "unexpected cluster visibility: %s" label)

let () =
  let singleton = build ~path:["Solo"; "Nested"] "Single" [] in
  let output = dot singleton in
  check_cluster ~present:false "Solo" output;
  check_cluster ~present:false "Nested" output;
  if not (contains ~needle:"Single" output) then failwith "singleton node lost";
  let grouped = build ~cluster:"memory" ~path:["Only"] "Grouped" [] in
  let output = dot grouped in
  check_cluster ~present:true "memory" output;
  check_cluster ~present:false "Only" output;
  let first = build ~path:["Shared"; "Child"] "First" [] in
  let second =
    build ~path:["Shared"] "Second"
      ["Arg", Ppx_module.Parent, Lazy.from_val first]
  in
  let output = dot second in
  check_cluster ~present:true "Shared" output;
  check_cluster ~present:false "Child" output;
  if not (contains ~needle:"label=\"Arg\"" output) then
    failwith "dependency edge lost"

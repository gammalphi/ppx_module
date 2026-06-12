let contains ~needle haystack =
  let needle_len = String.length needle in
  let haystack_len = String.length haystack in
  let rec loop i =
    i + needle_len <= haystack_len
    && (String.sub haystack i needle_len = needle || loop (i + 1))
  in
  needle_len = 0 || loop 0

let check_contains ~label ~needle haystack =
  if not (contains ~needle haystack) then
    failwith (Printf.sprintf "%s: expected output to contain %S" label needle)

let dot ?mode root =
  let buffer = Buffer.create 1024 in
  let ppf = Format.formatter_of_buffer buffer in
  (match mode with
   | None -> Ppx_module.pp ppf root
   | Some mode -> Ppx_module.pp ~mode ppf root);
  Format.pp_print_flush ppf ();
  Buffer.contents buffer

module Default_name = struct
end
[@@ppx_module]

module Named = struct
end
[@@ppx_module { name = "Explicit.Name"; cluster = "memory" }]

module Parent_functor (Parent : Ppx_module.LOGGED) : sig
  include Ppx_module.LOGGED
end = struct
end
[@@ppx_module { name = "ParentFunctor"; cluster = "scalar" }]

module No_parent_functor (Parent : Ppx_module.LOGGED) : sig
  include Ppx_module.LOGGED
end = struct
end
[@@ppx_module { name = "NoParentFunctor"; parents = false }]

module Outer (Ctx : Ppx_module.LOGGED) : sig
  module Inner : Ppx_module.LOGGED

  include Ppx_module.LOGGED
end = struct
  module Inner = struct
  end
  [@@ppx_module { name = "Inner"; cluster = "checks" }]
end
[@@ppx_module { name = "Outer"; cluster = "wrapper" }]

let () =
  let module Parent_instance = Parent_functor (Default_name) in
  let module No_parent_instance = No_parent_functor (Default_name) in
  let module Outer_instance = Outer (Named) in
  let _ = Lazy.force Parent_instance.ppx_module_node in
  let _ = Lazy.force No_parent_instance.ppx_module_node in
  let _ = Lazy.force Outer_instance.ppx_module_node in
  let _ = Lazy.force Outer_instance.Inner.ppx_module_node in
  let output = dot (Lazy.force Outer_instance.Inner.ppx_module_node) in
  check_contains ~label:"default module name" ~needle:"Metadata" output;
  check_contains ~label:"explicit module name" ~needle:"Explicit.Name" output;
  check_contains ~label:"source item metadata" ~needle:"Default_name" output;
  check_contains ~label:"cluster metadata" ~needle:"memory" output;
  check_contains ~label:"functor parameter metadata" ~needle:"params: Parent" output;
  check_contains ~label:"parent edge" ~needle:"label=\"Parent\" style=\"solid\""
    (dot (Lazy.force Parent_instance.ppx_module_node));
  check_contains ~label:"context edge" ~needle:"label=\"Ctx\" style=\"dashed\"" output;
  if
    contains ~needle:"label=\"Parent\" style=\"solid\""
      (dot ~mode:Ppx_module.Reachable
         (Lazy.force No_parent_instance.ppx_module_node))
  then failwith "parents=false: unexpected parent dependency edge"

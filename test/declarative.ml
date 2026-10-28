[@@@warning "-32"]

module Leaf = struct
  [@@@ppx_module { name = "Leaf" }]
end

module Nested = struct
  module Leaf = Leaf
end

module Composite = struct
  include Leaf
  [@@@ppx_module { name = "Composite";
    dependencies = ["Input", "Nested.Leaf"] }]
end

module Forward (Sub : Ppx_module.LOGGED) : Ppx_module.LOGGED = struct
  [@@@ppx_module { forward = "Sub" }]
end

module Alias = Forward(Composite)

module Selected (Sub : Ppx_module.LOGGED) (_ : sig end) = struct
end[@@ppx_module { name = "Selected"; parents = false;
  dependencies = ["Selected input", "Sub"] }]

module Instance = Selected(Composite)(struct end)

(* File annotations are appended after includes, not emitted at their location. *)
[@@@ppx_module { name = "File"; dependencies = ["Root", "Instance"] }]
include Leaf

let () =
  assert (Lazy.force Alias.ppx_module_node == Lazy.force Composite.ppx_module_node);
  assert (Lazy.force Composite.ppx_module_node != Lazy.force Leaf.ppx_module_node);
  let output = Format.asprintf "%a"
    (Ppx_module.pp ~mode:Ppx_module.Reachable ~line_numbers:false)
    (Lazy.force Instance.ppx_module_node) in
  let contains needle =
    let rec loop i =
      i + String.length needle <= String.length output
      && (String.sub output i (String.length needle) = needle || loop (i + 1))
    in loop 0
  in
  List.iter (fun label -> assert (contains label))
    ["Selected input"; "Input"; "Composite"; "Leaf"; "declarative.ml"];
  assert (not (contains "Context"))

(* Nested structures are independently annotated. *)
module Check = struct
  let () = assert (Lazy.force Instance.ppx_module_name = "Selected")
end

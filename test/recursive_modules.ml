module rec A : sig
  include Ppx_module.LOGGED

  val x : int
end = struct
  let x = 1
end
[@@ppx_module { name = "A"; cluster = "test" }]

and B : sig
  val y : int
end = struct
  let y = A.x
end

let () =
  ignore B.y;
  ignore (Lazy.force A.ppx_module_node)

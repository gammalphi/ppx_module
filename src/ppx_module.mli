(** Runtime support for PPX-generated module instantiation graphs.

    The PPX accepts either a bare attribute:

    {[
      [@@ppx_module]
    ]}

    or an optional record payload:

    {[
      [@@ppx_module { cluster = "memory" }]
      [@@ppx_module { name = "Memory.Domain"; cluster = "memory" }]
      [@@ppx_module { parents = false }]
    ]}

    If [name] is omitted, the graph name is derived from the source filename:
    the basename is taken, the extension is removed, and the first character is
    capitalized with [String.capitalize_ascii]. For example,
    [domains/product.ml] defaults to [Product]. *)

type edge_kind = Parent | Context

type pp_mode = Full | Reachable

type node

val build :
  name:string Lazy.t ->
  parents:(string * edge_kind * node Lazy.t) list ->
  node Lazy.t

val build_with_metadata :
  source_file:string ->
  source_line:int ->
  source_item_name:string ->
  cluster:string option ->
  lexical_path:string list ->
  functor_params:string list ->
  name:string Lazy.t ->
  parents:(string * edge_kind * node Lazy.t) list ->
  node Lazy.t

val pp : ?mode:pp_mode -> Format.formatter -> node -> unit

module type LOGGED = sig
  val ppx_module_name : string Lazy.t
  val ppx_module_node : node Lazy.t
end

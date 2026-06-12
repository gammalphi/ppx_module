# ppx_module

`ppx_module` is an OCaml PPX and small runtime library for recording module
instantiation graphs.

It is meant for codebases that use module functors heavily and need a way to
inspect which instantiated module depends on which functor argument or captured
outer context. The generated graph can be printed as Graphviz DOT.

## What It Does

Annotate a module binding with `[@@ppx_module]`:

```ocaml
module Domain = struct
  let value = 1
end
[@@ppx_module]
```

The PPX appends two values to the module body:

```ocaml
val ppx_module_name : string Lazy.t
val ppx_module_node : Ppx_module.node Lazy.t
```

At runtime, forcing `ppx_module_node` registers a node in the module graph.
`Ppx_module.pp` can then print the graph rooted at that node.

## Attributes

The attribute can be bare:

```ocaml
[@@ppx_module]
```

or can take a record payload:

```ocaml
[@@ppx_module { name = "Memory.Domain"; cluster = "memory"; parents = true }]
```

Supported fields:

- `name`: display name for the graph node. If omitted, the capitalized source
  filename is used.
- `cluster`: optional Graphviz cluster name. Some names have predefined colors:
  `memory`, `scalar`, `address`, `offset`, `value`, `wrapper`, and `checks`.
- `parents`: whether functor parameters should be recorded as parent
  dependencies. Defaults to `true`.

## Functor Dependencies

For an annotated functor, named functor parameters are recorded as `Parent`
edges:

```ocaml
module Make (Input : Ppx_module.LOGGED) : sig
  include Ppx_module.LOGGED
end = struct
end
[@@ppx_module { name = "Make" }]
```

The generated node for `Make(Input)` will point to `Input.ppx_module_node`.

Nested annotated modules inside an annotated functor also record outer functor
parameters as `Context` edges:

```ocaml
module Outer (Ctx : Ppx_module.LOGGED) = struct
  module Inner = struct
  end
  [@@ppx_module { name = "Inner" }]
end
[@@ppx_module { name = "Outer" }]
```

Here, `Inner` records that it captured `Ctx` from its enclosing context.

## Printing A Graph

Force a generated node, then pass it to `Ppx_module.pp`:

```ocaml
let () =
  let root = Lazy.force My_module.ppx_module_node in
  Format.printf "%a@." Ppx_module.pp root
```

The output is Graphviz DOT:

```dot
digraph module_stack {
  ...
}
```

To render it:

```sh
dune exec ./my_program.exe > graph.dot
dot -Tsvg graph.dot > graph.svg
```

Use reachable-only mode when you do not want unrelated registered nodes:

```ocaml
Format.printf "%a@." (Ppx_module.pp ~mode:Ppx_module.Reachable) root
```

## Module Signatures

If an annotated module has an explicit signature and callers need access to the
generated values, include `Ppx_module.LOGGED`:

```ocaml
module M : sig
  include Ppx_module.LOGGED

  val value : int
end = struct
  let value = 1
end
[@@ppx_module { name = "M" }]
```

## Dune Setup

Depend on the runtime library and enable the PPX:

```lisp
(executable
 (name main)
 (libraries ppx_module)
 (preprocess
  (pps ppx_module.ppx)))
```

For libraries:

```lisp
(library
 (name my_lib)
 (libraries ppx_module)
 (preprocess
  (pps ppx_module.ppx)))
```

## Development

Build and run the tests with:

```sh
dune runtest
```

The tests cover recursive modules, default and explicit names, clusters, functor
parameter edges, disabled parent edges, context edges, and DOT output fragments.

# ppx_module

A PPX and runtime library that records how modules and functor applications
depend on each other, and prints the result as Graphviz DOT.

Each annotated module or functor application becomes a node. Edges are the
dependencies you declare or the functor parameters the PPX finds. This is not
a whole-program dependency analysis.

## Setup

```lisp
(library
 (name domains)
 (libraries ppx_module)
 (preprocess (pps ppx_module.ppx)))
```

## Annotations

A module binding:

```ocaml
module Interval = struct
  let bottom = None
end
[@@ppx_module { name = "Interval"; cluster = "scalar" }]
```

A whole structure, e.g. a compilation unit, with a floating attribute:

```ocaml
[@@@ppx_module { name = "Interval"; cluster = "scalar" }]

let bottom = None
```

The generated bindings go at the end of the structure, wherever the attribute
is placed, so a later `include` cannot shadow them. At most one floating
annotation is allowed per structure.

Both forms generate the members of `Ppx_module.LOGGED`. Signatures that need to
expose them must include it themselves:

```ocaml
module type DOMAIN = sig
  include Ppx_module.LOGGED
  val bottom : int option
end
```

## Fields

| Field | Default | Meaning |
| --- | --- | --- |
| `name` | source filename, capitalized | Node label. |
| `cluster` | none | Graph cluster. |
| `parents` | `true` | Add edges to the named functor parameters. |
| `dependencies` | `[]` | Extra edges, as `(label, module path)` pairs. |
| `forward` | none | Reuse another module's node instead of creating one. |

All values are literals. Unknown fields are an error. The clusters `memory`,
`scalar`, `address`, `offset`, `value`, `wrapper` and `checks` have predefined
colors.

## Dependencies

### Functor parameters

By default, an annotated functor gets an edge to each named parameter:

```ocaml
module Make (Base : Ppx_module.LOGGED) = struct
  ...
end
[@@ppx_module { name = "Domain" }]
```

Every named parameter must then expose `Ppx_module.LOGGED`. Anonymous and unit
parameters are skipped. Floating annotations do not collect functor parameters.

### Explicit edges

```ocaml
module Combined = struct
  ...
end
[@@ppx_module {
  name = "Combined";
  dependencies = [("Numeric", "Numeric"); ("Enum", "Basis.Enum")];
}]
```

Paths can be dotted but cannot be functor applications. Each target must expose
`Ppx_module.LOGGED` and be in scope at the end of the structure. These edges
are added to the automatic ones.

To link only some parameters, turn off the automatic edges and list the ones
you want:

```ocaml
module Make (Base : Ppx_module.LOGGED) (Config : sig end) = struct
  ...
end
[@@ppx_module { parents = false; dependencies = [("Base", "Base")] }]
```

### Context edges

A module annotated inside an annotated functor gets a `Context` edge to each of
the outer functor's named parameters:

```ocaml
module Outer (Base : Ppx_module.LOGGED) = struct
  module Inner = struct end
  [@@ppx_module { name = "Inner" }]
end
[@@ppx_module { name = "Outer" }]
```

These edges come from scope, not from actual use. `parents = false` does not
remove them.

## Forwarding

Wrappers such as logging layers can reuse the wrapped module's node:

```ocaml
module Wrap (Sub : Ppx_module.LOGGED) = struct
  ...
  [@@@ppx_module { forward = "Sub" }]
end
```

`Wrap(Sub)` then has the same node as `Sub`. `forward` cannot be combined with
`name`, `cluster` or `dependencies`, and `parents` is ignored.

## Printing

```ocaml
let () =
  let root = Lazy.force Domain.ppx_module_node in
  Ppx_module.pp ~mode:Ppx_module.Reachable ~line_numbers:false
    Format.std_formatter root;
  Format.pp_print_flush Format.std_formatter ()
```

```sh
dune exec ./main.exe > stack.dot
dot -Tsvg stack.dot -o stack.svg
```

- `mode`: `Full` (default) prints every registered node and grays out the ones
  not reachable from the root. `Reachable` prints only the reachable ones.
- `line_numbers`: defaults to `true`.

Edges go from a module to its dependencies. Parameter edges are solid and
context edges are dashed.

A node is registered when its module is initialized or its functor applied,
not when the lazy value is forced. `Full` mode can therefore show
instantiated alternatives that end up unused.

## Development

```sh
dune build
dune runtest
```

# TODO

## Test Coverage

- Add PPX expansion tests that lock down the generated OCaml for annotated
  modules, generated bindings, removed attributes, parent edges, and context
  edges.
- Add negative tests for invalid attributes:
  - unknown fields
  - qualified field names
  - non-string `name` / `cluster`
  - non-boolean `parents`
  - invalid payload shapes
- Extend the recursive-module test so it asserts graph output, not only
  successful compilation.
- Replace most substring-based DOT assertions with graph-structure assertions
  once a read-only graph API exists.

## API

- Expose a read-only graph inspection API so callers and tests can inspect graph
  structure without parsing DOT.
- Consider exposing a snapshot type instead of exposing raw runtime internals.
- Add a registry reset function for tests and long-running tools.

## Diagnostics

- Improve generated-code locations so errors involving missing
  `Ppx_module.LOGGED` support point at the relevant functor parameter or
  annotated module.
- Document or enforce the requirement that parent/context dependency modules
  expose `ppx_module_node`.
- Reject duplicate `[@@ppx_module]` attributes instead of silently using the
  first one and removing all of them.

## Naming

- Revisit the default graph-node name. It currently comes from the source
  filename, so multiple annotated modules in one file share the same default
  name.
- Decide whether the default should instead use the module binding name, a
  filename/module-name combination, or remain filename-based with stronger
  documentation.

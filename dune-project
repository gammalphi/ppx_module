(lang dune 3.14)

(name ppx_module)

(authors "Paul Robert <paul.robert@cea.fr>")

(maintainers "Paul Robert <paul.robert@cea.fr>")

(license LGPL-2.1-only)

(source
 (github gammalphi/ppx_module))

(homepage "https://github.com/gammalphi/ppx_module")

(bug_reports "https://github.com/gammalphi/ppx_module/issues")

(generate_opam_files)

(package
 (name ppx_module)
 (synopsis "PPX-generated module instantiation graphs")
 (description
  "An OCaml PPX and runtime library for recording module instantiation graphs.
Annotated modules record functor argument dependencies and captured outer
contexts. The resulting graphs can be printed as Graphviz DOT.")
 (depends
  dune
  (ppxlib (>= 0.35.0))))

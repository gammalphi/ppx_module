(lang dune 3.14)

(name ppx_module)

(maintainers "Paul Robert <paul.robert@cea.fr>")

(generate_opam_files)

(package
 (name ppx_module)
 (synopsis "PPX-generated module instantiation graphs")
 (depends
  dune
  (ppxlib (>= 0.35.0))))

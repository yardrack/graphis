(lang dune 3.11)

(name graphis)
(version 0.1.0)
(license MIT)

(implicit_transitive_deps false)

(package
 (name graphis)
 (depends
  (ocaml (>= 5.0))
  dune
  (odoc :with-doc))
 (synopsis "inspect OCaml values as heap graphs")
 (description "graphis inspects OCaml values, preserves sharing, and emits graphviz DOT heap graphs"))

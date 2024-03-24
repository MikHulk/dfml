(lang dune 3.14)

(name dataframe)

(generate_opam_files true)

(source
 (github MikHulk/dfml))

(authors "Mik H.")

(maintainers "Li Monad")

(package
 (name dataframe)
 (synopsis "Dataframe with ocaml à la Polars.")
 (description "A little dataframe library for filtering, \
               selecting and transforming heterogenous data in place")
 (depends ocaml dune (alcotest :with-test) fmt)
 (tags
  (data)))

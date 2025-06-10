(lang dune 3.16)

(name ooh)

(generate_opam_files true)

(source
 (github smuenzel/ooh))

(authors "Stefan Muenzel")

(maintainers "Stefan Muenzel")

(license MPL-2.0)

(documentation https://url/to/documentation)

(package
  (name        ooh)
  (synopsis    "OCaml out-of-heap")
  (description "Out-of-heap data")
  (depends
   ocaml
   dune
   typerep
   cinaps
   (ppx_jane :with-test)
   ppx_optional
   ppx_pipebang
   ppx_typerep_conv)
  (tags (topics "to describe" your project)))

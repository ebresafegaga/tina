(lang dune 2.8)
(using menhir 2.1)
(generate_opam_files true)

(name tina)
(version dev)
(source (github ebresafegaga/tina))
(authors "Oghenevwogaga Ebresafe")
; (license BSD2)
(maintainers "ebresafegaga@gmail.com")

(package
 (name tina)
 (synopsis "Tina, an educational programming language with first-class types and abilities.")
 (depends
  menhir
  js_of_ocaml
  js_of_ocaml-ppx 
  js_of_ocaml-compiler
  (ocaml (>= 4.08.1))))

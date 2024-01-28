(lang dune 3.12)

(name infer)

(generate_opam_files true)

(source
 (github gmdods/infer-ml))

(authors "Gabriel Domingues")

(license GPL-2.0)

(package
 (name infer)
 (synopsis "Type inference")
 (description "Inference, for Hindley-Milner type systems, the French approach")
 (depends ocaml dune)
 (tags
  (topics "to describe" your project)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

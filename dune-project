(lang dune 3.14)

(name compwiwer)

(generate_opam_files true)

(source
 (github rctcwyvrn/compwiwer))

(authors "Lily")

(maintainers "Lily")

(license LICENSE)

(documentation "")

(package
 (name compwiwer)
 (synopsis "A dumb little compiler project")
 (description "A compiler of a subset of ocaml, targetting both ocaml lambda IR (via malfunction) and x86. For experimentation with static memory management")
 (depends ocaml dune)
 (tags ()))
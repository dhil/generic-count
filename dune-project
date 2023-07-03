(lang dune 3.7)

(name generic_count)

(generate_opam_files true)

(source
 (github dhil/generic-count))

(authors "Daniel Hillerström")

(maintainers "Daniel Hillerström")

(license LICENSE)

;;(documentation https://url/to/documentation)

(package
 (name generic_count)
 (synopsis "Code examples illustrating the use of first-class control to asymptotically speed up generic search")
 (description "Code examples illustrating the use of first-class control to asymptotically speed up generic search")
 (depends ocaml
          dune
          delimcc_of_fxhandler
          multicont)
 (tags
  (topics "control effects" "effect handlers" "asymptotic speedup" "generic search" "generic count" "shift/reset" "callcc")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

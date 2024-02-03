(lang dune 3.10)

(name oed)

(generate_opam_files true)

(source
 (github pawel-kojma/oed))

(authors "pawel-kojma")

(maintainers "pawel-kojma")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name oed)
 (synopsis "A short synopsis")
 (description "Ocaml text editor")
 (depends ocaml dune curses)
 (tags
  (text-editor)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project

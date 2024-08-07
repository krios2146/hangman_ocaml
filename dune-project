(lang dune 3.16)

(name hangman)

(generate_opam_files false)

(source
 (github krios2146/hangman_ocaml))

(authors "Vladimir Kidyaev")

(maintainers "Vladimir Kidyaev")

(license MIT)

(documentation https://github.com/krios2146/hangman_ocaml)

(package
 (name hangman)
 (synopsis "A simple hangman (word guessing) game with a console UI ")
 (depends ocaml dune))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html

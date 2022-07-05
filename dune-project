(lang dune 3.0)

(name lambda-term)
(version 3.2.0)

(generate_opam_files true)

(source
 (github ocaml-community/lambda-term))

(authors "Jérémie Dimino")

(maintainers "opam-devel@lists.ocaml.org")

(license BSD-3-Clause)

(package
 (name lambda-term)
 (synopsis "Terminal manipulation library for OCaml")
 (description
  "Lambda-term is a cross-platform library for manipulating the terminal. It
provides an abstraction for keys, mouse events, colors, as well as a set of
widgets to write curses-like applications. The main objective of lambda-term is
to provide a higher level functional interface to terminal manipulation than,
for example, ncurses, by providing a native OCaml interface instead of bindings
to a C library. Lambda-term integrates with zed to provide text edition
facilities in console applications.")
 (depends
  (ocaml
   (>= 4.08.0))
  logs
  (lwt
   (>= 4.2.0))
  lwt_react
  (mew_vi
   (and
    (>= 0.5.0)
    (< 0.6.0)))
  react
  (zed
   (and
    (>= 3.2.0)
    (< 4.0)))))

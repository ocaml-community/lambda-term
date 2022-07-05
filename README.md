Lambda-Term
===========

[![Build Status](https://travis-ci.org/ocaml-community/lambda-term.svg?branch=master)](https://travis-ci.org/ocaml-community/lambda-term)

Lambda-Term is a cross-platform library for manipulating the
terminal. It provides an abstraction for keys, mouse events, colors,
as well as a set of widgets to write curses-like applications.

The main objective of Lambda-Term is to provide a higher level
functional interface to terminal manipulation than, for example,
ncurses, by providing a native OCaml interface instead of bindings to
a C library.

Lambda-Term integrates with zed to provide text edition facilities in
console applications.

[API Documentation](https://ocaml-community.github.io/lambda-term/)

Installation
------------

To build and install Lambda-Term:

    $ dune build
    $ dune install

Note that this will build Lambda-Term using the development build
profile which has strict compilation flags. If the build fails, try
passing `--profile=release` to `dune` or alternatively create a
`dune-workspace` file with the following contents:

    (lang dune 1.1)
    (profile release)

### HTML API Documentation _(optional)_

To build the documentation:

    $ dune build @doc

You can then consult it by openning
`_build/default/_doc/_html/index.html`.

### Tests _(optional)_

To build and execute tests:

    $ dune runtest

### Examples _(optional)_

To build the examples:

    $ dune build @examples

Binaries for the examples will be in `_build/default/examples`.

Terminal emulators compatibility
--------------------------------

All terminal emulators behave differently, especially regarding how
keystrokes are reported to the application on its standard
input. Lambda-Term tries to handle all of them, but it may happen that
a particular key of combination of keys is not recognized by
Lambda-Term, and thus does not produce the expected effect (for
example: arrow keys or backspace not working).

To check what is reported by your terminal you can run the script
`print_sequences.ml` which at the root of the repository:

    $ ocaml print_sequences.ml
    press 'q' to quit
    \027[A
    \027[D
    \027[C
    \027[A
    \027[D
    a
    z
    e
    q

You can then send the result to jeremie@dimino.org, including:

* the application you are using as terminal emulator,
* the contents of the `TERM` environment variable inside the terminal (`echo $TERM`),
* the output of `print_sequences.ml` with, for each line, the keystroke.

Key bindings
------------

Key bindings can be set in `~/.config/lambda-term-inputrc`. See
[lambda-term-inputrc](lambda-term-inputrc). Useful mappings:

```
# This allows zsh-like searching the history by pressing up/down
[read-line]
up: history-search-prev
down: history-search-next
```

Main modules
------------

* `LTerm`: basic interface to the terminal, it allows to put the terminal
  in _raw_ mode, hide the cursor, render an offscreen array of points, ...
* `LTerm_draw`: drawing functions, for rendering in an offscreen array.
* `LTerm_read_line`: line edition.
* `LTerm_inputrc`: parsing of configurations files for key bindings.
* `LTerm_history`: history and history file management.
* `LTerm_ui`: helpers for writing full-screen applications.
* `LTerm_widget`: widget system (not stable).
* `LTerm_resources`: resources loading for widgets.

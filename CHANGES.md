3.3.2 (2023-08-09)
------------------

* `LTerm_vi`: fix a downward-action issue when act in empty content

3.3.1 (2022-07-04)
------------------

* Fix OCaml 5 compatibility (Thibaut Mattio, #110)

3.3.0 (2022-07-04)
------------------

* Replace Camomile with uu* (Nicolás Ojeda Bär, ZAN DoYe, Thibaut Mattio, Jonah Beckford, #109)

3.2.0 (2022-02-21)
------------------

* Add support for OCaml 5.00 (@kit-ty-kate, #104)

* The inputrc file has been moved from ~/.config/.lambda-term-inputrc to ~/.config/lambda-term-inputrc
  The old location will still be accepted until the next major version (@copy, #97)

3.1.0 (2020-05-30)
------------------

* `LTerm_read_line` and `LTerm_vi`:
  * vi visual mode
  * register support

3.0.1 (2020-05-06)
------------------

* `LTerm_read_line`: fix synchronization bug

3.0.0 (2020-04-25)
------------------

### Additions

* `LTerm_editor`: two editor modes: default and vi

* `LTerm_read_line: class virtual ['a] term`:
  * `method editor_mode : LTerm_editor.mode signal`: the current editor mode
  * `method set_editor_mode : LTerm_editor.mode -> unit`: set the current editor mode

Add initial support for vi editing mode to `LTerm_read_line`:
  * motions:
    * h l 0 ^ $
    * j k gg G
    * w W e E b B ge gE
    * f F t T
    * aw iw aW iW
    * include or inner ( ), [ ], { }, < >, ' and "
    * generic quote: aq? iq? where ? could be any character
    * bracket matching: jump back and forth between matched brakcets
  * delete, change, yank with motions
  * paste: p P
  * line joining: J

Many thanks to @nilsbecker for his feature-request on vi edit mode and the helps during the development on this topic!

### Breaking

  * `LTerm_read_line
    * class virtual ['a] term`: the type signature of `method private exec` is changed
      from
      `method private exec : action list -> 'a Lwt.t`
      to
      `?keys : LTerm_key.t list -> action list -> 'a loop_result Lwt.t`

Since this is a private method and is intended to be used internally, the backward-compatibility will not be affected in most cases.

### General

* Load inputrc file from ~/.config/.lambda-term-inputrc as per XDG conventions (@copy)

2.0.3 (2019-12-31)
------------------

LTerm\_edit: add horizontal scrolling support for wide width character

2.0.2 (2019-08-09)
------------------

LTerm\_history: catch and log `Zed_string.Invalid` exception

2.0.1 (2019-06-17)
------------------

* fix windows build (@db4, #72)
* expand zchar before writing to windows console (@kandu, #75)

2.0 (2019-05-17)
----------------

### Breaking

* LTerm\_draw: type `point` is redefined to use `Zed_char.t` as the essential element to support wide, combined glyph
* functions and methods: change parameter type from `UChar.t` or `Zed_utf8.t` to `Zed_char.t` or `Zed_string.t`
* LTerm\_text
  * function `of_string` is renamed to `of_utf8`
  * function `of_string_maybe_invalid` is renamed to `of_utf8_maybe_invalid`
  * the new `of_string` function is of type `Zed_string.t -> t`
  * the new `of_string_maybe_invalid` function is of type `Zed_string.t -> t`

### General

* depend on zed 2
* Lterm\_draw, LTerm\_widget, LTerm\_read\_line refactored to support wide, combined glyphs
* add name to dune-project (Hannes Mehnert, #70)
* port to dune (Jérémie Dimino, #69)
* README: Add Travis badge (Kevin Ji, #66)
* Add travis config (Anurag Soni, #65)
* opam: update homepage, bug-reports and dev-repo fields (Jérémie Dimino)

1.13 (2018-06-01)
-----------------

* Make lambda-term compatible with Lwt 4 and Camomile 1 (#63,
  @ncihnegn)

1.12 (2017-11-05)
-----------------

* Fix: copy & pasting the terminal output doesn't adds many spaces
  after the end of lines (#52, Deokhwan Kim, fixes diml/utop#186)

* -safe-string compatibility (#54)

1.11 (2017-04-04)
-----------------

* Add history-search-prev and history-search-next (#47, Fabian Hemmer)
* Allow frame widgets to be labeled (#36, Andrew Ray)
* Add an alignment setting to label widgets (#36, Andrew Ray)
* Add scrollbar widgets (#33, Andrew Ray)
* Improve the `lambda-term-inputrc.5` man page (#41, Léon van Velzen)
* Allow editor widgets to request a specific size and add a "double
  editor" example (#42, Fabian Bonk)
* Add `^` to the list of characters recognized in inputrc files (#46,
  github user zhenya1007)
* Switch the build to jbuilder
* Drop compatibility with 4.01

1.10.1 (2016-08-15)
-------------------

* fix a race condition in `LTerm_read_line`. The race would often
  appear when copy&pasting

1.10 (2016-04-07)
-----------------

* add support for editing the current input with an external editor
  (action `edit-with-external-editor`), bound to `C-x C-e` by default
* add forward search in history (action `next-search`), bound to `M-s` by default
* add support for mouse clicks on widgets
  (thanks to Andrew Ray)
* add support for looking up files in XDG locations
  (thanks to Genki Marshall)

1.9 (2015-06-23)
----------------

* add repl example (Martin DeMello)
* add support for custom and local bindings

1.8 (2015-01-07)
----------------

* remove use of deprecated Lwt functions
* add some iTerm2 keys
* fix some invalid use of react

1.7 (2014-10-20)
----------------

* removed hard dependency on camlp4 (thanks to Peter Zotov)
* added styled formatters (thanks to Gabriel Radanne)
* doc fixes
* Extended widget support (thanks to Alexey Vyskubov):
  - added modal frames
  - added radiobutton and checkbutton

1.6 (2014-04-21)
----------------

* Support for React 1.0.0
* fix OpenBSD builds

1.5 (2013-08-07)
----------------

* workaround camomile raising the wrong exception for encoding
  failures
* add more default keybindings

1.4 (2013-03-26)
----------------

* added `C-b`, `C-f`, `C-h`, `M-p`, `M-n` by default
* fix a segfault when running utop in an emacs terminal buffer

1.3 (2012-10-08)
----------------

* fix the bindings for `C-n` and `C-p`
* binds `C-h` to delete-prev-char

1.2 (2012-07-30)
----------------

* better handling of newlines in read-line (avoid square selection bug)
* add a module for managing history
* use camomile for character encoding (remove iconv dependency)
    * include generated tables for color mappings to speed up the build
    * Windows fixes
        * use unicode version of IO console functions
        * better rendering method for read-line

1.1 (2011-08-06)
----------------

* fix a blinking problem on OS-X
* bind the `kill-{prev,next}-word` editing actions
* bind the undo action
* add doc for edition actions
* add `LTerm_key.to_string_compact` to print keys like emacs
* use `Zed_input` for key bindings instead of hash tables
* add support for macros
* add the break action to interrupt read-line
* add manual pages
* allow to get the current pending key sequence in read-line
* make the `LTerm_read_line.term` class more flexible

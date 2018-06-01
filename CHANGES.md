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

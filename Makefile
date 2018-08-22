INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

.PHONY: all
all:
	dune build @install @examples/all

.PHONY: examples
examples:
	dune build @examples/all

.PHONY: asciiart
asciiart:
	dune build examples/asciiart/asciiart.exe

.PHONY: install
install:
	dune install $(INSTALL_ARGS)

.PHONY: uninstall
uninstall:
	dune uninstall $(INSTALL_ARGS)

.PHONY: reinstall
reinstall:
	$(MAKE) uninstall
	$(MAKE) install

.PHONY: test
test:
	dune runtest

.PHONY: all-supported-ocaml-versions
all-supported-ocaml-versions:
	dune build @install @examples/all @runtest --workspace dune-workspace.dev

.PHONY: clean
clean:
	dune clean

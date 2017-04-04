INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)

.PHONY: all
all:
	jbuilder build @install

.PHONY: examples
examples:
	jbuilder build @examples/all

.PHONY: asciiart
asciiart:
	jbuilder build examples/asciiart/asciiart.exe

.PHONY: install
install:
	jbuilder install $(INSTALL_ARGS)

.PHONY: uninstall
uninstall:
	jbuilder uninstall $(INSTALL_ARGS)

.PHONY: reinstall
reinstall:
	$(MAKE) uninstall
	$(MAKE) install

.PHONY: test
test:
	jbuilder runtest

.PHONY: all-supported-ocaml-versions
all-supported-ocaml-versions:
	jbuilder runtest --workspace jbuild-workspace.dev

.PHONY: clean
clean:
	rm -rf _build *.install
	find . -name .merlin -delete

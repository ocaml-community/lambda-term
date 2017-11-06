INSTALL_ARGS := $(if $(PREFIX),--prefix $(PREFIX),)
FLAGS ?= --dev

.PHONY: all
all:
	jbuilder build $(FLAGS) @install @examples/all

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
	jbuilder build @install @examples/all @runtest $(FLAGS) --workspace jbuild-workspace.dev

.PHONY: clean
clean:
	jbuilder clean

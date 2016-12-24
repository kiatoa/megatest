SHELL=/bin/tcsh -f

faceless=icfadm
instdir=/tmp/$(faceless)/general-lib-inst

doas=ssh $(faceless)@$(HOST)

help:
	@echo ""
	@echo "make targets:"
	@echo "============="
	@echo "install      - build and install general_lib egg as icfadm"
	@echo "test         - run unit tests on general-lib.scm (tests code, not egg)"
	@echo "eggs-info     - show chicken-install commands to get eggs upon which general-lib depends"
	@echo "test_example - compile an example scm against installed general_lib egg"
	@echo "clean        - remove binaries and other build artifacts"
	@echo ""

clean:
	rm -f *.so *.import.scm test_glib test_example foo *.c *.o

install:
	$(doas) "if (-e $(instdir)) rm -rf $(instdir)"
	$(doas) "mkdir -p $(instdir)"
	$(doas) "cp -a $(PWD)/* $(instdir)/."
	$(doas) "source /p/foundry/env/pkgs/chicken/4.9.0.1/setup-chicken4x.csh && cd $(instdir) && chicken-install"

# test hostname is "L" which root is available
Linstall:
	sudo chicken-install

test:
	chicken-install -no-install
	csc test_glib.scm

	./test_glib
	if (-e foo) rm -f foo

test_example:
	@csc test_example.scm
	@./test_example
	@rm test_example

eggs-info:
	@echo chicken-install ansi-escape-sequences
	@echo chicken-install slice
	@echo chicken-install rfc3339

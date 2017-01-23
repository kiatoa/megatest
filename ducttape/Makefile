SHELL=/bin/tcsh -f

help:
	@echo ""
	@echo "make targets:"
	@echo "============="
	@echo "install      - build and install general_lib egg as icfadm"
	@echo "test         - run unit tests on ducttape-lib.scm (tests code, not egg)"
	@echo "eggs-info     - show chicken-install commands to get eggs upon which ducttape-lib depends"
	@echo "test_example - compile an example scm against installed general_lib egg"
	@echo "clean        - remove binaries and other build artifacts"
	@echo ""

clean:
	rm -f *.so *.import.scm test_glib test_example foo *.c *.o

install:
	echo tbd
	/bin/false

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

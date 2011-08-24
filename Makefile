# $(glob *.scm) did not work as I expected it to!?

FILES=$(shell ls *.scm)

megatest: $(FILES)
	csc megatest.scm 

dashboard: $(FILES)
	csc dashboard.scm

$(PREFIX)/bin/megatest : megatest
	@echo Installing to PREFIX=$(PREFIX), use ^C to cancel and change
	sleep 5
	cp megatest $(PREFIX)/bin/megatest

$(PREFIX)/bin/dashboard : dashboard $(FILES)
	cp dashboard $(PREFIX)/bin/dashboard

install : $(PREFIX)/bin/megatest $(PREFIX)/bin/dashboard

test: tests/tests.scm
	cd tests;csi -I .. -b -n tests.scm


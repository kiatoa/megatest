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
	cp utils/mt_* $(PREFIX)/bin
	chmod a+x $(PREFIX)/bin/mt_*

# install dashboard as dboard so wrapper script can be called dashboard
$(PREFIX)/bin/dboard : dashboard $(FILES)
	cp dashboard $(PREFIX)/bin/dboard

install : $(PREFIX)/bin/megatest $(PREFIX)/bin/dboard

test: tests/tests.scm
	cd tests;csi -I .. -b -n tests.scm


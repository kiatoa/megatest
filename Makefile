
PREFIX=.

FILES=$(shell ls *.scm)
HELPERS=$(addprefix $(PREFIX)/bin/,mt_laststep mt_runstep)

megatest: $(FILES)
	csc megatest.scm 

dboard: $(FILES)
	csc dashboard.scm -o dboard

$(PREFIX)/bin/megatest : megatest
	@echo Installing to PREFIX=$(PREFIX), use ^C to cancel and change
	sleep 2 
	cp megatest $(PREFIX)/bin/megatest

$(HELPERS)  : utils/mt_*
	cp $< $@
	chmod a+x $@

# install dashboard as dboard so wrapper script can be called dashboard
$(PREFIX)/bin/dboard : dashboard $(FILES)
	cp dboard $(PREFIX)/bin/dboard
	utils/mk_dashboard_wrapper $(PREFIX) > $(PREFIX)/bin/dashboard
	chmod a+x $(PREFIX)/bin/dashboard
	utils/mk_dashboard_wrapper $(PREFIX) > $(PREFIX)/bin/dashboard
	chmod a+x $(PREFIX)/bin/dashboard

install : $(PREFIX)/bin/megatest $(PREFIX)/bin/dboard $(PREFIX)/bin/dashboard $(HELPERS)

test: tests/tests.scm
	cd tests;csi -I .. -b -n tests.scm


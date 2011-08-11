FILES=$(glob *.scm)

megatest: common.scm configf.scm db.scm keys.scm launch.scm megatest.scm process.scm runs.scm gui.scm
	csc megatest.scm 

dashboard: dashboard.scm dashboard-tests.scm
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


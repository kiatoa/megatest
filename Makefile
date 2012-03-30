
PREFIX=.
CSCOPTS= 
INSTALL=install
SRCFILES = common.scm items.scm launch.scm \
           ods.scm runconfig.scm server.scm configf.scm \
           db.scm keys.scm margs.scm megatest-version.scm \
           process.scm runs.scm tasks.scm tests.scm 

GUISRCF  = dashboard.scm dashboard-tests.scm dashboard-guimonitor.scm dashboard-main.scm

OFILES   = $(SRCFILES:%.scm=%.o)
GOFILES  = $(GUISRCF:%.scm=%.o)

HELPERS=$(addprefix $(PREFIX)/bin/,mt_laststep mt_runstep mt_ezstep)

all : megatest dboard

megatest: $(OFILES) megatest.o
	csc $(CSCOPTS) $(OFILES) megatest.o -o megatest

dboard : $(OFILES) $(GOFILES)
	csc $(OFILES) $(GOFILES) -o dboard

# Special dependencies for the includes
tests.o db.o launch.o runs.o dashboard-tests.o dashboard-guimonitor.o dashboard-main.o monitor.o dashboard.o megatest.o : db_records.scm
tests.o runs.o dashboard.o dashboard-tests.o dashboard-main.o  : run_records.scm
db.o ezsteps.o keys.o launch.o megatest.o monitor.o runs-for-ref.o runs.o tests.o : key_records.scm
tests.o tasks.o dashboard-tasks.o : task_records.scm
runs.o : test_records.scm

$(OFILES) $(GOFILES) : common_records.scm 

%.o : %.scm
	csc $(CSCOPTS) -c $<

$(PREFIX)/bin/megatest : megatest
	@echo Installing to PREFIX=$(PREFIX)
	$(INSTALL) megatest $(PREFIX)/bin/megatest

$(HELPERS) : utils/mt_* 
	$(INSTALL) $< $@
	chmod a+x $@

$(PREFIX)/bin/nbfake : utils/nbfake
	$(INSTALL) $< $@
	chmod a+x $@

$(PREFIX)/bin/nbfind : utils/nbfind
	$(INSTALL) $< $@
	chmod a+x $@

# install dashboard as dboard so wrapper script can be called dashboard
$(PREFIX)/bin/dboard : dboard $(FILES)
	$(INSTALL) dboard $(PREFIX)/bin/dboard
	utils/mk_dashboard_wrapper $(PREFIX) > $(PREFIX)/bin/dashboard
	chmod a+x $(PREFIX)/bin/dashboard

install : bin $(PREFIX)/bin/megatest $(PREFIX)/bin/dboard $(PREFIX)/bin/dashboard $(HELPERS) $(PREFIX)/bin/nbfake $(PREFIX)/bin/nbfind

bin : 
	mkdir -p $(PREFIX)/bin

test: tests/tests.scm
	cd tests;csi -I .. -b -n tests.scm

clean : 
	rm -f $(OFILES) $(GOFILES) megatest dboard dboard.o megatest.o

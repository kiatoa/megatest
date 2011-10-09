
PREFIX=.

SRCFILES = common.scm items.scm launch.scm \
           ods.scm runconfig.scm server.scm configf.scm \
           db.scm keys.scm margs.scm megatest-version.scm \
           process.scm runs.scm

GUISRCF  = dashboard.scm dashboard-tests.scm

OFILES   = $(SRCFILES:%.scm=%.o)
GOFILES  = $(GUISRCF:%.scm=%.o)

HELPERS=$(addprefix $(PREFIX)/bin/,mt_laststep mt_runstep)

all : megatest dashboard

megatest: $(OFILES) megatest.o
	csc $(OFILES) megatest.o -o megatest

dashboard: $(OFILES) $(GOFILES)
	csc $(OFILES) $(GOFILES) -o dashboard

db.o launch.o runs.o : db_records.scm

keys.o db.o runs.o launch.o  : key_records.scm

$(OFILES) $(GOFILES) : common_records.scm 

%.o : %.scm
	csc -c $<

$(PREFIX)/bin/megatest : megatest
	@echo Installing to PREFIX=$(PREFIX), use ^C to cancel and change
	sleep 2 
	cp megatest $(PREFIX)/bin/megatest

$(HELPERS)  : utils/mt_*
	cp $< $@
	chmod a+x $@

# install dashboard as dboard so wrapper script can be called dashboard
$(PREFIX)/bin/dboard : dashboard $(FILES)
	cp dashboard $(PREFIX)/bin/dboard
	utils/mk_dashboard_wrapper $(PREFIX) > $(PREFIX)/bin/dashboard
	chmod a+x $(PREFIX)/bin/dashboard

install : $(PREFIX)/bin/megatest $(PREFIX)/bin/dboard $(PREFIX)/bin/dashboard $(HELPERS)

test: tests/tests.scm
	cd tests;csi -I .. -b -n tests.scm


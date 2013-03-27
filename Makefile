
PREFIX=$(PWD)
CSCOPTS= 
INSTALL=install
SRCFILES = common.scm items.scm launch.scm \
           ods.scm runconfig.scm server.scm configf.scm \
           db.scm keys.scm margs.scm megatest-version.scm \
           process.scm runs.scm tasks.scm tests.scm genexample.scm \
	   fs-transport.scm zmq-transport.scm http-transport.scm \
           client.scm gutils.scm synchash.scm

GUISRCF  = dashboard-tests.scm dashboard-guimonitor.scm 

OFILES   = $(SRCFILES:%.scm=%.o)
GOFILES  = $(GUISRCF:%.scm=%.o)

ADTLSCR=mt_laststep mt_runstep mt_ezstep
HELPERS=$(addprefix $(PREFIX)/bin/,$(ADTLSCR))
DEPLOYHELPERS=$(addprefix deploytarg/,$(ADTLSCR))
MTESTHASH=$(shell fossil info|grep checkout:| awk '{print $$2}')

CSIPATH=$(shell which csi)
CKPATH=$(shell dirname $(shell dirname $(CSIPATH)))

all : mtest dboard newdashboard

mtest: $(OFILES) megatest.o
	csc $(CSCOPTS) $(OFILES) megatest.o -o mtest

dboard : $(OFILES) $(GOFILES) dashboard.scm
	csc $(OFILES) dashboard.scm $(GOFILES) -o dboard

newdashboard : newdashboard.scm $(OFILES)
	csc $(OFILES) newdashboard.scm -o newdashboard

deploytarg/libiupcd.so : $(CKPATH)/lib/libiupcd.so
	for i in iup im cd av call sqlite; do \
	  cp $(CKPATH)/lib/lib$$i* deploytarg/ ; \
	done

# puts deployed megatest in directory "megatest"
deploytarg/megatest : $(OFILES) megatest.o
	csc -deploy $(CSCOPTS) $(OFILES) megatest.scm
	rsync -av megatest/ deploytarg/

deploytarg/dashboard :  $(OFILES) $(GOFILES)
	csc -deploy $(OFILES) $(GOFILES) dashboard.scm
	rsync -av dashboard/ deploytarg/


# Special dependencies for the includes
tests.o db.o launch.o runs.o dashboard-tests.o dashboard-guimonitor.o dashboard-main.o monitor.o dashboard.o megatest.o : db_records.scm
tests.o runs.o dashboard.o dashboard-tests.o dashboard-main.o  : run_records.scm
db.o ezsteps.o keys.o launch.o megatest.o monitor.o runs-for-ref.o runs.o tests.o : key_records.scm
tests.o tasks.o dashboard-tasks.o : task_records.scm
runs.o : test_records.scm
megatest.o : megatest-fossil-hash.scm

megatest-fossil-hash.scm : $(SRCFILES) megatest.scm *_records.scm
	echo "(define megatest-fossil-hash \"$(MTESTHASH)\")" > megatest-fossil-hash.new
	if ! diff -q megatest-fossil-hash.new megatest-fossil-hash.scm ; then echo copying .new to .scm;cp -f megatest-fossil-hash.new megatest-fossil-hash.scm;fi

$(OFILES) $(GOFILES) : common_records.scm 

%.o : %.scm
	csc $(CSCOPTS) -c $<

$(PREFIX)/bin/mtest : mtest
	@echo Installing to PREFIX=$(PREFIX)
	$(INSTALL) mtest $(PREFIX)/bin/mtest
	utils/mk_wrapper $(PREFIX) mtest > $(PREFIX)/bin/megatest
	chmod a+x $(PREFIX)/bin/megatest

$(HELPERS) : utils/mt_* 
	$(INSTALL) $< $@
	chmod a+x $@

$(DEPLOYHELPERS) : utils/mt_*
	$(INSTALL) $< $@
	chmod a+X $@

$(PREFIX)/bin/nbfake : utils/nbfake
	$(INSTALL) $< $@
	chmod a+x $@

$(PREFIX)/bin/nbfind : utils/nbfind
	$(INSTALL) $< $@
	chmod a+x $@

deploytarg/nbfake : utils/nbfake
	$(INSTALL) $< $@
	chmod a+x $@

deploytarg/nbfind : utils/nbfind
	$(INSTALL) $< $@
	chmod a+x $@


# install dashboard as dboard so wrapper script can be called dashboard
$(PREFIX)/bin/dboard : dboard $(FILES)
	$(INSTALL) dboard $(PREFIX)/bin/dboard
	utils/mk_wrapper $(PREFIX) dboard > $(PREFIX)/bin/dashboard
	chmod a+x $(PREFIX)/bin/dashboard

install : bin $(PREFIX)/bin/mtest $(PREFIX)/bin/megatest $(PREFIX)/bin/dboard $(PREFIX)/bin/dashboard $(HELPERS) $(PREFIX)/bin/nbfake $(PREFIX)/bin/nbfind $(PREFIX)/bin/newdashboard

deploytarg/apropos.so : Makefile
	chicken-install -prefix deploytarg -deploy apropos base64 canvas-draw csv-xml directory-utils dot-locking extras fmt format hostinfo http-client intarweb json md5 message-digest posix posix-extras readline regex regex-case s11n spiffy spiffy-request-vars sqlite3 srfi-1 srfi-18 srfi-69 tcp test uri-common zmq  

deploy : deploytarg/megatest deploytarg/dashboard $(DEPLOYHELPERS) deploytarg/nbfake deploytarg/nbfind deploytarg/libiupcd.so deploytarg/apropos.so


bin : 
	mkdir -p $(PREFIX)/bin

test: tests/tests.scm
	cd tests;csi -I .. -b -n tests.scm

clean : 
	rm -f $(OFILES) $(GOFILES) megatest dboard dboard.o megatest.o

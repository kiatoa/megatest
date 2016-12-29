# make install CSCOPTS='-accumulate-profile -profile-name $(PWD)/profile-ww$(shell date +%V.%u)'
# rm <files>.o ; make install CSCOPTS='-profile' ; ... ;  chicken-profile | less

PREFIX=$(PWD)
CSCOPTS= 
INSTALL=install
SRCFILES = common.scm items.scm launch.scm \
   ods.scm runconfig.scm server.scm configf.scm \
   db.scm keys.scm margs.scm megatest-version.scm \
   process.scm runs.scm tasks.scm tests.scm genexample.scm \
   http-transport.scm filedb.scm \
   client.scm synchash.scm daemon.scm mt.scm \
   ezsteps.scm lock-queue.scm sdb.scm \
   rmt.scm api.scm tdb.scm rpc-transport.scm \
   portlogger.scm archive.scm env.scm

# Eggs to install (straightforward ones)
EGGS=matchable readline apropos base64 regex-literals format regex-case test coops trace csv \
dot-locking posix-utils posix-extras directory-utils hostinfo tcp-server rpc csv-xml fmt \
json md5 awful http-client spiffy uri-common intarweb spiffy-request-vars \
spiffy-directory-listing ssax sxml-serializer sxml-modifications iup canvas-draw sqlite3

GUISRCF  = dashboard-tests.scm dashboard-guimonitor.scm gutils.scm dcommon.scm tree.scm vg.scm

OFILES   = $(SRCFILES:%.scm=%.o)
GOFILES  = $(GUISRCF:%.scm=%.o)

ADTLSCR=mt_laststep mt_runstep mt_ezstep
HELPERS=$(addprefix $(PREFIX)/bin/,$(ADTLSCR))
DEPLOYHELPERS=$(addprefix deploytarg/,$(ADTLSCR))
MTESTHASH=$(shell fossil info|grep checkout:| awk '{print $$2}')

CSIPATH=$(shell which csi)
CKPATH=$(shell dirname $(shell dirname $(CSIPATH)))
# ARCHSTR=$(shell uname -m)_$(shell uname -r)
# BASH_MACHTYPE=$(shell bash -c "echo \$$MACHTYPE")
# ARCHSTR=$(BASH_MACHTYPE)_$(shell lsb_release -sr)
ARCHSTR=$(shell lsb_release -sr)
# ARCHSTR=$(shell bash -c "echo \$$MACHTYPE")

PNGFILES = $(shell cd docs/manual;ls *png)

all : $(PREFIX)/bin/.$(ARCHSTR) mtest dboard 

mtest: $(OFILES) readline-fix.scm megatest.o
	csc $(CSCOPTS) $(OFILES) megatest.o -o mtest

dboard : $(OFILES) $(GOFILES) dashboard.scm
	csc $(CSCOPTS) $(OFILES) dashboard.scm $(GOFILES) -o dboard

ndboard : newdashboard.scm $(OFILES) $(GOFILES)
	csc $(CSCOPTS) $(OFILES) $(GOFILES) newdashboard.scm -o ndboard

# install documentation to $(PREFIX)/docs
# DOES NOT REBUILD DOCS
#
$(PREFIX)/share/docs/megatest_manual.html : docs/manual/megatest_manual.html
	mkdir -p $(PREFIX)/share/docs
	$(INSTALL) docs/manual/megatest_manual.html $(PREFIX)/share/docs/megatest_manual.html
	for png in $(PNGFILES);do $(INSTALL) docs/manual/$$png $(PREFIX)/share/docs/$$png;done

#multi-dboard : multi-dboard.scm $(OFILES) $(GOFILES)
#	csc $(CSCOPTS) $(OFILES) $(GOFILES) multi-dboard.scm -o multi-dboard

# 
# $(PREFIX)/bin/revtagfsl : utils/revtagfsl.scm
#	csc utils/revtagfsl.scm -o $(PREFIX)/bin/revtagfsl

# Special dependencies for the includes
tests.o db.o launch.o runs.o dashboard-tests.o dashboard-guimonitor.o dashboard-main.o monitor.o dashboard.o  \
archive.o megatest.o : db_records.scm
tests.o runs.o dashboard.o dashboard-tests.o dashboard-main.o  : run_records.scm
db.o ezsteps.o keys.o launch.o megatest.o monitor.o runs-for-ref.o runs.o tests.o : key_records.scm
tests.o tasks.o dashboard-tasks.o : task_records.scm
runs.o : test_records.scm
megatest.o : megatest-fossil-hash.scm
rmt.scm client.scm common.scm configf.scm dashboard-guimonitor.scm dashboard-tests.scm dashboard.scm db.scm dcommon.scm ezsteps.scm fs-transport.scm http-transport.scm index-tree.scm items.scm keys.scm launch.scm megatest.scm monitor.scm mt.scm newdashboard.scm runconfig.scm runs.scm server.scm tdb.scm tests.scm tree.scm : common_records.scm rpc-transport.scm
common_records.scm : altdb.scm
vg.o dashboard.o : vg_records.scm
dcommon.o : run_records.scm
# Temporary while transitioning to new routine
# runs.o : run-tests-queue-classic.scm  run-tests-queue-new.scm

megatest-fossil-hash.scm : $(SRCFILES) megatest.scm *_records.scm
	echo "(define megatest-fossil-hash \"$(MTESTHASH)\")" > megatest-fossil-hash.new
	if ! diff -q megatest-fossil-hash.new megatest-fossil-hash.scm ; then echo copying .new to .scm;cp -f megatest-fossil-hash.new megatest-fossil-hash.scm;fi

$(OFILES) $(GOFILES) : common_records.scm 

%.o : %.scm
	csc $(CSCOPTS) -c $<

$(PREFIX)/bin/.$(ARCHSTR)/mtest : mtest utils/mk_wrapper
	@echo Installing to PREFIX=$(PREFIX)
	$(INSTALL) mtest $(PREFIX)/bin/.$(ARCHSTR)/mtest
	utils/mk_wrapper $(PREFIX) mtest $(PREFIX)/bin/megatest
	chmod a+x $(PREFIX)/bin/megatest

$(PREFIX)/bin/.$(ARCHSTR)/ndboard : ndboard
	$(INSTALL) ndboard $(PREFIX)/bin/.$(ARCHSTR)/ndboard

$(PREFIX)/bin/newdashboard : $(PREFIX)/bin/.$(ARCHSTR)/ndboard utils/mk_wrapper
	utils/mk_wrapper $(PREFIX) ndboard $(PREFIX)/bin/newdashboard
	chmod a+x $(PREFIX)/bin/newdashboard

#$(PREFIX)/bin/.$(ARCHSTR)/mdboard : multi-dboard
#	$(INSTALL) multi-dboard $(PREFIX)/bin/.$(ARCHSTR)/mdboard

# $(PREFIX)/bin/mdboard : $(PREFIX)/bin/.$(ARCHSTR)/mdboard  utils/mk_wrapper
# 	utils/mk_wrapper $(PREFIX) mdboard $(PREFIX)/bin/mdboard
# 	chmod a+x $(PREFIX)/bin/mdboard

# $(HELPERS) : utils/%
# 	$(INSTALL) $< $@
# 	chmod a+x $@

$(PREFIX)/bin/mt_laststep : utils/mt_laststep
	$(INSTALL) $< $@
	chmod a+x $@

$(PREFIX)/bin/mt_runstep : utils/mt_runstep
	$(INSTALL) $< $@
	chmod a+x $@

$(PREFIX)/bin/mt_ezstep : utils/mt_ezstep
	$(INSTALL) $< $@
	chmod a+x $@

$(PREFIX)/bin/mt_xterm : utils/mt_xterm
	$(INSTALL) $< $@
	chmod a+x $@

$(PREFIX)/bin/nbfake : utils/nbfake
	$(INSTALL) $< $@
	chmod a+x $@

$(PREFIX)/bin/remrun : utils/remrun
	$(INSTALL) $< $@
	chmod a+x $@

$(PREFIX)/bin/viewscreen : utils/viewscreen
	$(INSTALL) $< $@
	chmod a+x $@

$(PREFIX)/bin/nbfind : utils/nbfind
	$(INSTALL) $< $@
	chmod a+x $@

$(PREFIX)/bin/loadrunner : utils/loadrunner
	$(INSTALL) $< $@
	chmod a+x $@

# $(PREFIX)/bin/refdb : refdb
# 	$(INSTALL) $< $@
# 	chmod a+x $@

deploytarg/nbfake : utils/nbfake
	$(INSTALL) $< $@
	chmod a+x $@

deploytarg/viewscreen : utils/viewscreen
	$(INSTALL) $< $@
	chmod a+x $@

deploytarg/nbfind : utils/nbfind
	$(INSTALL) $< $@
	chmod a+x $@

$(PREFIX)/bin/mtest-reaper: helpers/mtest-reaper.scm helpers/ducttape-lib.scm helpers/inteldate.scm helpers/mimetypes.scm
	make -C helpers $@ PREFIX=$(PREFIX) INSTALL=$(INSTALL) ARCHSTR=$(ARCHSTR)

mtest-reaper: $(PREFIX)/bin/mtest-reaper

# install dashboard as dboard so wrapper script can be called dashboard
$(PREFIX)/bin/.$(ARCHSTR)/dboard : dboard $(FILES) utils/mk_wrapper
	utils/mk_wrapper $(PREFIX) dboard $(PREFIX)/bin/dashboard
	chmod a+x $(PREFIX)/bin/dashboard
	$(INSTALL) dboard $(PREFIX)/bin/.$(ARCHSTR)/dboard

install : $(PREFIX)/bin/.$(ARCHSTR) $(PREFIX)/bin/.$(ARCHSTR)/mtest $(PREFIX)/bin/megatest \
          $(PREFIX)/bin/.$(ARCHSTR)/dboard $(PREFIX)/bin/dashboard $(HELPERS) $(PREFIX)/bin/nbfake \
	  $(PREFIX)/bin/nbfind $(PREFIX)/bin/loadrunner $(PREFIX)/bin/viewscreen $(PREFIX)/bin/mt_xterm \
	  $(PREFIX)/share/docs/megatest_manual.html $(PREFIX)/bin/remrun

$(PREFIX)/bin/.$(ARCHSTR) : 
	mkdir -p $(PREFIX)/bin/.$(ARCHSTR)
	mkdir -p $(PREFIX)/bin/.$(ARCHSTR)/lib

test: tests/tests.scm
	cd tests;csi -I .. -b -n tests.scm

ext-tests/.fslckout : $(MTQA_FOSSIL)
	mkdir -p ext-tests
	cd ext-tests;fossil open --nested $(MTQA_FOSSIL)

$(MTQA_FOSSIL) :
	fossil clone https://www.kiatoa.com/fossils/megatest_qa $(MTQA_FOSSIL)

clean : 
	rm -f $(OFILES) $(GOFILES) megatest dboard dboard.o megatest.o dashboard.o megatest-fossil-hash.* altdb.scm

#======================================================================
# Make the records files
#======================================================================

# vg_records.scm : records.sh
#	./records.sh

#======================================================================
# Deploy section (not complete yet)
#======================================================================

$(DEPLOYHELPERS) : utils/mt_*
	$(INSTALL) $< $@
	chmod a+X $@

deploytarg/apropos.so : Makefile
	chicken-install -p deploytarg -deploy -keep-installed $(EGGS)

#	for i in apropos base64 canvas-draw csv-xml directory-utils dot-locking extras fmt format hostinfo http-client intarweb json md5 message-digest posix posix-extras readline regex regex-case s11n spiffy spiffy-request-vars sqlite3 srfi-1 srfi-18 srfi-69 tcp test uri-common check-errors synch matchable sql-null tcp-server rpc blob-utils string-utils variable-item defstruct uri-generic sendfile opensll openssl lookup-table list-utils stack; do \
#	chicken-install -prefix deploytarg -deploy $$i;done

# deploytarg/libsqlite3.so : 
# 	CSC_OPTIONS="-Ideploytarg -Ldeploytarg" $CHICKEN_INSTALL -prefix deploytarg -deploy sqlite3

deploy : deploytarg/mtest deploytarg/dboard $(DEPLOYHELPERS) deploytarg/nbfake deploytarg/remrun deploytarg/viewsceen deploytarg/nbfind deploytarg/apropos.so

# deploytarg/libiupcd.so : $(CKPATH)/lib/libiupcd.so
# 	for i in iup im cd av call sqlite; do \
# 	  cp $(CKPATH)/lib/lib$$i* deploytarg/ ; \
# 	done
# 	cp $(CKPATH)/include/*.h deploytarg

# puts deployed megatest in directory "megatest"
deploytarg/mtest : $(OFILES) megatest.o deploytarg/apropos.so
	csc -deploy $(CSCOPTS) $(OFILES) megatest.scm -o deploytarg
	mv deploytarg/deploytarg deploytarg/mtest

deploytarg/dboard :  $(OFILES) $(GOFILES) dashboard.scm deploytarg/apropos.so
	csc -deploy $(OFILES) $(GOFILES) dashboard.scm -o deploytarg
	mv deploytarg/deploytarg deploytarg/dboard

# DATASHAREO=configf.o common.o process.o tree.o dcommon.o margs.o launch.o gutils.o db.o synchash.o server.o \
#            megatest-version.o tdb.o ods.o mt.o keys.o
datashare-testing/sd : datashare.scm $(OFILES)
	csc $(CSCOPTS) datashare.scm $(OFILES) -o datashare-testing/sd

datashare-testing/sdat: sharedat.scm $(OFILES)
	csc $(CSCOPTS) sharedat.scm $(OFILES) -o datashare-testing/sdat

sd : datashare-testing/sd
	mkdir -p /tmp/$(USER)/datashare/disk1 /tmp/$(USER)/basepath

xterm : sd
	(export BASEPATH=/tmp/$(USER)/basepath ; export PATH="$(PWD)/datashare-testing:$(PATH)" ; xterm &)

datashare-testing/spublish : spublish.scm $(OFILES)
	csc $(CSCOPTS) spublish.scm $(OFILES) -o datashare-testing/spublish

datashare-testing/sretrieve : sretrieve.scm megatest-version.o margs.o configf.o process.o 
	csc $(CSCOPTS) sretrieve.scm megatest-version.o margs.o configf.o process.o -o datashare-testing/sretrieve

datashare-testing/sauthorize : sretrieve.scm megatest-version.o margs.o configf.o process.o common.o
	 csc sauthorize.scm megatest-version.o margs.o configf.o process.o common.o -o datashare-testing/sauthorize


sretrieve/sretrieve : datashare-testing/sretrieve
	csc $(CSCOPTS) -deploy -deployed sretrieve.scm megatest-version.o margs.o configf.o process.o
	chicken-install -keep-installed $(PROXY) -deploy -prefix sretrieve defstruct srfi-18 format sql-de-lite \
             srfi-1 posix regex regex-case srfi-69 

# base64 dot-locking \
#             csv-xml z3

#  "(define (toplevel-command . a) #f)"
# if egrep 'version.*3.0' $(shell dirname $(shell dirname $(shell which csi)))/lib/chicken/7/readline.setup-info;then \

readline-fix.scm :
	if [[ $(shell chicken-status | grep readline | awk '{print $4}' | cut -d. -f1) -gt 3 ]];then \
	   echo "(define *use-new-readline* #f)" > readline-fix.scm; \
	else \
	   echo "(define *use-new-readline* #t)" > readline-fix.scm;\
	fi

altdb.scm :
	echo ";; optional alternate db setup" > altdb.scm
	echo "(define *available-db* (make-hash-table))" >> altdb.scm
	if  csi -ne '(use mysql-client)';then \
           echo "(use mysql-client)(hash-table-set! *available-db* 'mysql #t)" >> altdb.scm; \
	fi
	if csi -ne '(use postgresql)';then \
	   echo "(use postgresql)(hash-table-set! *available-db* 'postgresql #t)" >> altdb.scm;\
	fi

portlogger-example : portlogger-example.scm api.o archive.o client.o common.o configf.o daemon.o dashboard-tests.o db.o dcommon.o ezsteps.o filedb.o genexample.o gutils.o http-transport.o items.o keys.o launch.o lock-queue.o margs.o megatest-version.o mt.o ods.o portlogger.o process.o rmt.o rpc-transport.o runconfig.o runs.o sdb.o server.o synchash.o tasks.o tdb.o tests.o tree.o
	csc $(CSCOPTS) portlogger-example.scm api.o archive.o client.o common.o configf.o daemon.o dashboard-tests.o db.o dcommon.o ezsteps.o filedb.o genexample.o gutils.o http-transport.o items.o keys.o launch.o lock-queue.o margs.o megatest-version.o mt.o ods.o portlogger.o process.o rmt.o rpc-transport.o runconfig.o runs.o sdb.o server.o synchash.o tasks.o tdb.o tests.o tree.o


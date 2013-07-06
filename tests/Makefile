# run some tests

BINPATH=$(shell readlink -m $(PWD)/../bin)
MEGATEST=$(BINPATH)/megatest
PATH := $(BINPATH):$(PATH)
RUNNAME := $(shell date +w%V.%u.%H.%M)
IPADDR := "-"
# Set SERVER to "-server -"
SERVER  = 
DEBUG   = 1
LOGGING = 

OS  = $(shell grep ID /etc/*-release|cut -d= -f2)
FS  = $(shell df -T .|tail -1|awk '{print $$2}')
VER = $(shell fsl info|grep checkout|awk '{print $$2}'|cut -c 1-5)

# The NEWTARGET causes some tests to fail. Do not use until this is fixed.
NEWTARGET  = "$(OS)/$(FS)/$(VER)"
TARGET     = "ubuntu/nfs/none"

all : test1 test2 test3 test4 test5 test6 test7 test8 test9

server :
	cd ..;make;make install
	cd fullrun;../../bin/megatest -server - -debug 22 &

stopserver :
	cd ..;make && make install
	cd fullrun;$(MEGATEST) -stop-server 0

test0 : cleanprep
	cd simplerun ; $(MEGATEST) -server - -debug $(DEBUG)

test1 : cleanprep
	rm -f simplerun/megatest.db
	rm -rf simplelinks/ simpleruns/
	mkdir -p simplelinks simpleruns
	cd simplerun;cp ../../*_records.scm .;perl -pi.bak -e 's/define-inline/define/' *_records.scm
	cd simplerun;echo '(load "../tests.scm")' | $(MEGATEST) -repl -debug $(DEBUG)

test2 : fullprep
	cd fullrun;$(MEGATEST) -runtests ez_pass,runfirst/a/% -reqtarg ubuntu/nfs/none :runname $(RUNNAME) -debug $(DEBUG) $(LOGGING)
	cd fullrun;megatest -runtests % -target ubuntu/nfs/none :runname $(RUNNAME)_01 -testpatt %/,%/ai -debug $(DEBUG)
	cd fullrun;megatest -runtests %/,%/ai -target ubuntu/nfs/none :runname $(RUNAME)_02 -debug $(DEBUG)
	cd fullrun;megatest -runtests runfirst/%,%/ai -target ubuntu/nfs/none :runname $(RUNNAME)_02 -debug $(DEBUG)
	cd fullrun;megatest -runtests %/,%/winter -target ubuntu/nfs/none :runname $(RUNNAME)_03  -debug $(DEBUG)
	sleep 40;cd fullrun;megatest -target ubuntu/nfs/none :runname $(RUNNAME) -set-state-status COMPLETED,FORCED :state COMPLETED :status PASS -testpatt ez_p%s,runfirst/ -debug $(DEBUG) $(LOGGING)


test3 : fullprep
	cd fullrun;$(MEGATEST) -runtests runfirst -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_b -debug 10

test4 : cleanprep
	@echo "WARNING: No longer running fullprep, test converage may be lessened"
	cd fullrun;time $(MEGATEST) -debug $(DEBUG) -runtests % -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_b -m "This is a comment specific to a run" -v $(LOGGING)

# NOTE: Only one instance can be a server
test5 : cleanprep
	@echo "WARNING: No longer running fullprep, test converage may be lessened"
	cd fullrun;sleep 0;$(MEGATEST) -runtests % -target $(TARGET) :runname $(RUNNAME)_aa -debug $(DEBUG) $(LOGGING) > aa.log 2> aa.log &
	cd fullrun;sleep 0;$(MEGATEST) -runtests % -target $(TARGET) :runname $(RUNNAME)_ab -debug $(DEBUG) $(LOGGING) > ab.log 2> ab.log &
	cd fullrun;sleep 0;$(MEGATEST) -runtests % -target $(TARGET) :runname $(RUNNAME)_ac -debug $(DEBUG) $(LOGGING) > ac.log 2> ac.log &
	cd fullrun;sleep 0;$(MEGATEST) -runtests % -target $(TARGET) :runname $(RUNNAME)_ad -debug $(DEBUG) $(LOGGING) > ad.log 2> ad.log &	
#	cd fullrun;sleep 0;$(MEGATEST) -runtests % -target $(TARGET) :runname $(RUNNAME)_ae -debug $(DEBUG) $(LOGGING) > ae.log 2> ae.log &	
#	cd fullrun;sleep 0;$(MEGATEST) -runtests % -target $(TARGET) :runname $(RUNNAME)_af -debug $(DEBUG) $(LOGGING) > af.log 2> af.log &	a

# MUST ADD THIS BACK IN ASAP!!!!
	# cd fullrun;sleep 10;$(MEGATEST) -run-wait  -target $(TARGET) :runname % -testpatt % :state RUNNING,LAUNCHED,NOT_STARTED,REMOTEHOSTSTART;echo ALL DONE

test6: fullprep
	cd fullrun;$(MEGATEST) -runtests runfirst -testpatt %/1 -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_itempatt -v
	cd fullrun;$(MEGATEST) -runtests runfirst -testpatt %blahha% -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_itempatt -debug 10
	cd fullrun;$(MEGATEST) -rollup :runname newrun -target ubuntu/nfs/none -debug 10

test7: 
	@echo Only a/c testname c should remain. If there is a run a/b/c then there is a cache issue.
	(cd simplerun; \
	 $(MEGATEST) -server - -daemonize; \
         $(MEGATEST) -remove-runs -target %/% :runname % -testpatt %; \
         $(MEGATEST) -runtests %  -target a/b :runname c; sleep 5; \
	 $(MEGATEST) -remove-runs -target a/c :runname c; \
	 $(MEGATEST) -runtests %  -target a/c :runname c; \
	 $(MEGATEST) -remove-runs -target a/b :runname c -testpatt % ; \
	 $(MEGATEST) -runtests %  -target a/d :runname c;$(MEGATEST) -list-runs %|egrep ^Run:) > test7.log 2> test7.log 
	logpro test7.logpro test7.html < test7.log
	@echo 
	@echo Run \"firefox test7.html\" to see the results.

# This one failed with v1.55
test8a : 
	cd fullrun;$(MEGATEST) -runtests priority_10_waiton_1 -target ubuntu/nfs/none :runname $(RUNNAME)_waiton_single

test8 : test8a
	cd fullrun;$(MEGATEST) -runtests lineitem_fail 1 -target ubuntu/nfs/none :runname $(RUNNAME)_singletest
	cd fullrun;$(MEGATEST) -runtests runfirst/fall 1 -target ubuntu/nfs/none :runname $(RUNNAME)_singleitem
	cd fullrun;$(MEGATEST) -runtests test_mt_vars/2 -target ubuntu/nfs/none :runname $(RUNNAME)_singleitem_waiton

# Some simple checks for bootstrapping and run loop logic 

test9 : minsetup test9a test9b test9c test9d

test9a :
	@echo Run super-simple mintest e, no waitons.
	cd mintest;megatest -runtests e -target $(VER) :runname `date +%H.%M.%S` -debug $(DEBUG)

test9b :
	@echo Run simple mintest d with one waiton c
	cd mintest;megatest -runtests d -target $(VER) :runname `date +%H.%M.%S` -debug $(DEBUG)

test9c :
	@echo Run mintest a with full waiton chain a -> b -> c -> d -> e
	cd mintest;megatest -runtests a -target $(VER) :runname `date +%H.%M.%S` -debug $(DEBUG)

test9d :
	@echo Run an itemized test with no items
	cd mintest;megatest -runtests g -target $(VER) :runname `date +%H.%M.%S` -debug $(DEBUG)

test10 :
	@echo Run a bunch of different targets simultaneously
	(cd fullrun;$(MEGATEST) -server - ;sleep 2)&
	for targ in mint/btrfs/mintdir sunos/sshfs/loc; do \
	   (cd fullrun;$(MEGATEST) -runtests priority_10_waiton_1 -target $$targ :runname $(RUNNAME) &); done
	for sys in ubuntu suse redhat debian;do \
	  for fs in afs nfs zfs; do \
	     for dpath in none tmp; do \
	        (cd fullrun;$(MEGATEST) -runtests priority_10_waiton_1 -target $$sys/$$fs/$$dpath :runname $(RUNNAME) &);\
	     done;done;done

minsetup : 
	cd ..;make && make install
	mkdir -p mintest/runs mintest/links
	cd mintest;megatest -stop-server 0
	cd mintest;megatest -server - -debug $(DEBUG) > server.log 2> server.log & 
	sleep 3
	cd mintest;dashboard -rows 20 &

cleanprep : ../*.scm Makefile */*.config
	mkdir -p fullrun/tmp/mt_runs fullrun/tmp/mt_links
	cd ..;make;make install
	rm -f */logging.db
	touch cleanprep

fullprep : cleanprep
	cd fullrun;$(MEGATEST) -remove-runs :runname $(RUNNAME)% -target %/%/% -testpatt %/%
	cd fullrun;$(BINPATH)/dashboard -rows 15 &

dashboard : cleanprep
	cd fullrun && $(BINPATH)/dashboard -rows 25 &

remove :
	cd fullrun;$(MEGATEST) -remove-runs :runname $(RUN)  -testpatt % -itempatt % :sysname % :fsname % :datapath %

clean  : 
	rm cleanprep

kill :
	killall -v mtest main.sh dboard || true
	rm -f */megatest.db */logging.db */monitor.db || true
	killall -v mtest dboard || true

hardkill : kill
	sleep 5;killall -v mtest main.sh dboard -9

listservers :
	cd fullrun;$(MEGATEST) -list-servers

runforever :
	while(ls); do runname=`date +%F-%R:%S`;(cd fullrun;$(MEGATEST) -runall -target ubuntu/nfs/none :runname $$runname;/home/matt/data/megatest/megatest -runall -target ubuntu/nfs/none :runname $$runname;/home/matt/data/megatest/megatest -runall -target ubuntu/nfs/none :runname $$runname);done

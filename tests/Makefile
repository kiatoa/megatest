# run some tests

BINPATH=$(PWD)/../bin
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

all : test1 test2 test3 test4 test5

server :
	(cd ..;make;make install) && \
	(cd fullrun;../../bin/megatest -server - -debug 22) 

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

test4 :
	@echo "WARNING: No longer running fullprep, test converage may be lessened"
	cd fullrun;time $(MEGATEST) -debug $(DEBUG) -runall -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_b -m "This is a comment specific to a run" -v $(LOGGING)

# NOTE: Only one instance can be a server
test5 : 
	@echo "WARNING: No longer running fullprep, test converage may be lessened"
	cd fullrun;sleep 0;$(MEGATEST) -runtests % -target $(TARGET) :runname $(RUNNAME)_aa -debug $(DEBUG) $(LOGGING) > aa.log 2> aa.log &
	cd fullrun;sleep 0;$(MEGATEST) -runtests % -target $(TARGET) :runname $(RUNNAME)_ab -debug $(DEBUG) $(LOGGING) > ab.log 2> ab.log &
	cd fullrun;sleep 0;$(MEGATEST) -runtests % -target $(TARGET) :runname $(RUNNAME)_ac -debug $(DEBUG) $(LOGGING) > ac.log 2> ac.log &
	cd fullrun;sleep 0;$(MEGATEST) -runtests % -target $(TARGET) :runname $(RUNNAME)_ad -debug $(DEBUG) $(LOGGING) > ad.log 2> ad.log &	
#	cd fullrun;sleep 0;$(MEGATEST) -runtests % -target $(TARGET) :runname $(RUNNAME)_ae -debug $(DEBUG) $(LOGGING) > ae.log 2> ae.log &	
#	cd fullrun;sleep 0;$(MEGATEST) -runtests % -target $(TARGET) :runname $(RUNNAME)_af -debug $(DEBUG) $(LOGGING) > af.log 2> af.log &	

test6: fullprep
	cd fullrun;$(MEGATEST) -runtests runfirst -testpatt %/1 -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_itempatt -v
	cd fullrun;$(MEGATEST) -runtests runfirst -testpatt %blahha% -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_itempatt -debug 10
	cd fullrun;$(MEGATEST) -rollup :runname newrun -target ubuntu/nfs/none -debug 10


cleanprep : ../*.scm Makefile */*.config
	mkdir -p /tmp/mt_runs /tmp/mt_links
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
	cd fullrun;$(MEGATEST) -listservers

runforever :
	while(ls); do runname=`date +%F-%R:%S`;(cd fullrun;$(MEGATEST) -runall -target ubuntu/nfs/none :runname $$runname;/home/matt/data/megatest/megatest -runall -target ubuntu/nfs/none :runname $$runname;/home/matt/data/megatest/megatest -runall -target ubuntu/nfs/none :runname $$runname);done

# run some tests

BINPATH=$(shell realpath ../bin)
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
NEWTARGET  = "-target $(OS)/$(FS)/$(VER)"
TARGET     = "-target ubuntu/nfs/none"

all : test1 test2 test3 test4 test5

test1 : cleanprep
	rm -f simplerun/megatest.db
	rm -rf simplelinks/ simpleruns/
	mkdir -p simplelinks simpleruns
	cd simplerun;cp ../../*_records.scm .;perl -pi.bak -e 's/define-inline/define/' *_records.scm
	cd simplerun;echo '(load "../tests.scm")' | $(MEGATEST) -repl -debug $(DEBUG)

test2 : fullprep
	cd fullrun;$(MEGATEST) -runtests ez_pass,runfirst/a/% -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_a $(SERVER)
	cd fullrun;sleep 20;megatest -target ubuntu/nfs/none :runname $(RUNNAME) -set-state-status :state COMPLETED :status FORCED -testpatt runfirst/% 


test3 : fullprep
	cd fullrun;$(MEGATEST) -runtests runfirst -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_b  $(SERVER) -debug 10

test4 : fullprep
	cd fullrun;$(MEGATEST) $(SERVER) &
	cd fullrun;sleep 5;$(MEGATEST) -debug $(DEBUG) -runall -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_b -m "This is a comment specific to a run" -v $(LOGGING)

# NOTE: Only one instance can be a server
test5 : fullprep
	cd fullrun;$(MEGATEST) $(SERVER) &
	cd fullrun;sleep 10;$(MEGATEST) -runall $(TARGET) :runname $(RUNNAME)_aa -debug $(DEBUG) $(LOGGING) > aa.log 2> aa.log &
	cd fullrun;sleep 10;$(MEGATEST) -runall $(TARGET) :runname $(RUNNAME)_ab -debug $(DEBUG) $(LOGGING) > ab.log 2> ab.log &
	cd fullrun;sleep 10;$(MEGATEST) -runall $(TARGET) :runname $(RUNNAME)_ac -debug $(DEBUG) $(LOGGING) > ac.log 2> ac.log &
	cd fullrun;sleep 10;$(MEGATEST) -runall $(TARGET) :runname $(RUNNAME)_ad -debug $(DEBUG) $(LOGGING) > ad.log 2> ad.log &	
#	cd fullrun;sleep 10;$(MEGATEST) -runall $(TARGET) :runname $(RUNNAME)_ae -debug $(DEBUG) $(LOGGING) > ae.log 2> ae.log &	
#	cd fullrun;sleep 10;$(MEGATEST) -runall $(TARGET) :runname $(RUNNAME)_af -debug $(DEBUG) $(LOGGING) > af.log 2> af.log &	

test6: fullprep
	cd fullrun;$(MEGATEST) -runtests runfirst -itempatt %/1 -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_itempatt -v
	cd fullrun;$(MEGATEST) -runtests runfirst -itempatt %blahha% -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_itempatt -debug 10

cleanprep : ../*.scm Makefile */*.config
	# if [ -e fullrun/megatest.db ]; then sqlite3 fullrun/megatest.db "delete from metadat where var='SERVER';";fi
	mkdir -p /tmp/mt_runs /tmp/mt_links
	cd ..;make install
	rm -f fullrun/logging.db
	touch cleanprep

fullprep : cleanprep
	cd fullrun;$(MEGATEST) -remove-runs :runname $(RUNNAME)% -target %/%/% -testpatt % -itempatt %
	cd fullrun;$(BINPATH)/dboard -rows 15 &

dashboard : cleanprep
	cd fullrun && $(BINPATH)/dashboard -rows 25 &

remove :
	cd fullrun;$(MEGATEST) -remove-runs :runname $(RUN)  -testpatt % -itempatt % :sysname % :fsname % :datapath %

clean  : 
	rm cleanprep

kill :
	killall -v mtest main.sh dboard || true
	rm -f fullrun/megatest.db fullrun/logging.db || true
	killall -v -9 mtest dboard || true

runforever :
	while(ls); do runname=`date +%F-%R:%S`;(cd fullrun;$(MEGATEST) -runall -target ubuntu/nfs/none :runname $$runname;/home/matt/data/megatest/megatest -runall -target ubuntu/nfs/none :runname $$runname;/home/matt/data/megatest/megatest -runall -target ubuntu/nfs/none :runname $$runname);done

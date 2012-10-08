# run some tests

BINPATH=$(shell realpath ../bin)
MEGATEST=$(BINPATH)/megatest
PATH := $(BINPATH):$(PATH)
RUNNAME := $(shell date +w%V.%u.%H.%M)
IPADDR := "-"
# Set SERVER to "-server -"
SERVER := 
DEBUG  := 1

all : test1 test2 test3 test4 test5

test1 : cleanprep
	rm -f simplerun/megatest.db
	rm -rf simplelinks/ simpleruns/
	mkdir -p simplelinks simpleruns
	cd simplerun;cp ../../*_records.scm .;perl -pi.bak -e 's/define-inline/define/' *_records.scm
	cd simplerun;echo '(load "../tests.scm")' | $(MEGATEST) -repl -debug $(DEBUG)

test2 : fullprep
	cd fullrun;$(MEGATEST) -runtests ez_pass,runfirst -reqtarg ubuntu/nfs/none -itempatt a/1 :runname $(RUNNAME)_a $(SERVER)

test3 : fullprep
	cd fullrun;$(MEGATEST) -runtests runfirst -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_b  $(SERVER) -debug 10

test4 : fullprep
	cd fullrun;$(MEGATEST) $(SERVER) &
	cd fullrun;sleep 5;$(MEGATEST) -debug $(DEBUG) -runall -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_b -m "This is a comment specific to a run" -v -logging

# NOTE: Only one instance can be a server
test5 : fullprep
	cd fullrun;$(MEGATEST) $(SERVER) &
	cd fullrun;sleep 10;$(MEGATEST) -runall -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_aa -debug $(DEBUG) -logging > aa.log 2> aa.log &
	cd fullrun;sleep 10;$(MEGATEST) -runall -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_ab -debug $(DEBUG) -logging > ab.log 2> ab.log &
	cd fullrun;sleep 10;$(MEGATEST) -runall -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_ac -debug $(DEBUG) -logging > ac.log 2> ac.log &
	cd fullrun;sleep 10;$(MEGATEST) -runall -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_ad -debug $(DEBUG) -logging > ad.log 2> ad.log &	
#	cd fullrun;sleep 10;$(MEGATEST) -runall -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_ae -debug $(DEBUG) -logging > ae.log 2> ae.log &	
#	cd fullrun;sleep 10;$(MEGATEST) -runall -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_af -debug $(DEBUG) -logging > af.log 2> af.log &	

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

runforever :
	while(ls); do runname=`date +%F-%R:%S`;(cd fullrun;$(MEGATEST) -runall -target ubuntu/nfs/none :runname $$runname;/home/matt/data/megatest/megatest -runall -target ubuntu/nfs/none :runname $$runname;/home/matt/data/megatest/megatest -runall -target ubuntu/nfs/none :runname $$runname);done

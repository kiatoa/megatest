# run some tests

BINPATH=$(shell realpath ../bin)
MEGATEST=$(BINPATH)/megatest
PATH := $(BINPATH):$(PATH)
RUNNAME := $(shell date +w%V.%u.%H.%M)
IPADDR := "-"
# Set SERVER to "-server -"
SERVER := 
DEBUG  := 1

all : test1 test 2 test3

test1 : cleanprep
	mkdir -p simplelinks simpleruns
	cd simplerun;echo '(load "../tests.scm")' | $(MEGATEST) -repl -debug $(DEBUG)

test2 : fullprep
	cd fullrun;$(MEGATEST) -runtests ez_pass -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_a $(SERVER)

test3 : fullprep
	cd fullrun;$(MEGATEST) -runtests runfirst -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_b  $(SERVER) -debug 10

test4 : fullprep
	cd fullrun;$(MEGATEST) -debug $(DEBUG) -runall -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_b -m "This is a comment specific to a run" -v $(SERVER)

test5 : fullprep
	cd fullrun;$(MEGATEST) -runall -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_aa -debug $(DEBUG) > aa.log 2> aa.log &
	cd fullrun;$(MEGATEST) -runall -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_ab -debug $(DEBUG) > ab.log 2> ab.log &
	cd fullrun;$(MEGATEST) -runall -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_ac -debug $(DEBUG) > ac.log 2> ac.log &
	cd fullrun;$(MEGATEST) -runall -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_ad -debug $(DEBUG) > ad.log 2> ad.log &	
#	cd fullrun;$(MEGATEST) -runall -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_ae -debug $(DEBUG) > ae.log 2> ae.log &	
#	cd fullrun;$(MEGATEST) -runall -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_af -debug $(DEBUG) > af.log 2> af.log &	

test6: fullprep
	cd fullrun;$(MEGATEST) -runtests runfirst -itempatt %/1 -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_itempatt -v
	cd fullrun;$(MEGATEST) -runtests runfirst -itempatt %blahha% -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_itempatt -debug 10

cleanprep : ../*.scm Makefile */*.config
	sqlite3 megatest.db "delete from metadat where var='SERVER';"
	mkdir -p /tmp/mt_runs /tmp/mt_links
	cd ..;make install
	touch cleanprep

fullprep : cleanprep
	cd fullrun;$(MEGATEST) -remove-runs :runname $(RUNNAME)% -target %/%/% -testpatt % -itempatt %
	cd fullrun;$(BINPATH)/dboard -rows 15 &

dashboard : cleanprep
	cd fullrun && $(BINPATH)/dboard &

remove :
	cd fullrun;$(MEGATEST) -remove-runs :runname $(RUN)  -testpatt % -itempatt % :sysname % :fsname % :datapath %

clean  : 
	rm cleanprep

runforever :
	while(ls); do runname=`date +%F-%R:%S`;(cd fullrun;$(MEGATEST) -runall -target ubuntu/nfs/none :runname $$runname;/home/matt/data/megatest/megatest -runall -target ubuntu/nfs/none :runname $$runname;/home/matt/data/megatest/megatest -runall -target ubuntu/nfs/none :runname $$runname);done

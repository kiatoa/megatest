# run some tests

BINPATH=$(shell realpath ../bin)
MEGATEST=$(BINPATH)/megatest
PATH := $(BINPATH):$(PATH)
RUNNAME := $(shell date +w%V.%u.%H.%M)
IPADDR := "-"
# Set SERVER to "-server -"
SERVER := 

runall : test1 test2

test1 : cleanprep
	$(MEGATEST) -runtests ez_pass -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_a $(SERVER)

test2 : cleanprep
	$(MEGATEST) -runtests runfirst -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_b  $(SERVER) -debug 10

test3 : cleanprep
	$(MEGATEST) -runall -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_b -m "This is a comment specific to a run" -v $(SERVER)

test4 : cleanprep
	$(MEGATEST) -runall -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_aa -v $(SERVER) >& aa.log &
	$(MEGATEST) -runall -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_ab -v $(SERVER) >& ab.log &
	$(MEGATEST) -runall -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_ac -v $(SERVER) >& ac.log &
	$(MEGATEST) -runall -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_ad -v $(SERVER) >& ad.log &	
	$(MEGATEST) -runtests runfirst -itempatt %/1 -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_itempatt -v
	$(MEGATEST) -runtests runfirst -itempatt %blahha% -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_itempatt -debug 10

cleanprep : ../*.scm Makefile *.config
	sqlite3 megatest.db "delete from metadat where var='SERVER';"
	mkdir -p /tmp/mt_runs /tmp/mt_links
	cd ..;make install
	$(MEGATEST) -remove-runs :runname $(RUNNAME)% -target %/%/% -testpatt % -itempatt %
	$(BINPATH)/dboard -rows 15 &
	touch cleanprep

test :
	csi -b -I .. ../megatest.scm -- -runall -target ubuntu/afs/tmp :runname blah
	cd ../;make test
	make runall

dashboard :
	cd ../;make install
	$(BINPATH)/dboard &

remove :
	(cd ../;make);$(MEGATEST) -remove-runs :runname $(RUN)  -testpatt % -itempatt % :sysname % :fsname % :datapath %

clean  : 
	rm cleanprep

runforever :
	while(ls); do runname=`date +%F-%R:%S`;$(MEGATEST) -runall -target ubuntu/nfs/none :runname $$runname;/home/matt/data/megatest/megatest -runall -target ubuntu/nfs/none :runname $$runname;/home/matt/data/megatest/megatest -runall -target ubuntu/nfs/none :runname $$runname;done

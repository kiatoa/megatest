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
	cd fullrun;$(MEGATEST) -runtests ez_pass -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_a $(SERVER)

test2 : cleanprep
	cd fullrun;$(MEGATEST) -runtests runfirst -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_b  $(SERVER) -debug 10

test3 : cleanprep
	cd fullrun;$(MEGATEST) -runall -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_b -m "This is a comment specific to a run" -v $(SERVER)

test4 : cleanprep
	cd fullrun;$(MEGATEST) -runall -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_aa -v $(SERVER) 2&>1 aa.log &
	cd fullrun;$(MEGATEST) -runall -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_ab -v $(SERVER) 2&>1 ab.log &
	cd fullrun;$(MEGATEST) -runall -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_ac -v $(SERVER) 2&>1 ac.log &
	cd fullrun;$(MEGATEST) -runall -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_ad -v $(SERVER) 2&>1 ad.log &	
	cd fullrun;$(MEGATEST) -runtests runfirst -itempatt %/1 -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_itempatt -v
	cd fullrun;$(MEGATEST) -runtests runfirst -itempatt %blahha% -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_itempatt -debug 10

cleanprep : ../*.scm Makefile *.config
	sqlite3 megatest.db "delete from metadat where var='SERVER';"
	mkdir -p /tmp/mt_runs /tmp/mt_links
	cd ..;make install
	cd fullrun;$(MEGATEST) -remove-runs :runname $(RUNNAME)% -target %/%/% -testpatt % -itempatt %
	cd fullrun;$(BINPATH)/dboard -rows 15 &
	touch cleanprep

test :
	csi -b -I .. ../megatest.scm -- -runall -target ubuntu/afs/tmp :runname blah
	cd ../;make test
	make runall

dashboard :
	cd ../;make install
	cd fullrun;$(BINPATH)/dboard &

remove :
	(cd ../;make);cd fullrun;$(MEGATEST) -remove-runs :runname $(RUN)  -testpatt % -itempatt % :sysname % :fsname % :datapath %

clean  : 
	rm cleanprep

runforever :
	while(ls); do runname=`date +%F-%R:%S`;(cd fullrun;$(MEGATEST) -runall -target ubuntu/nfs/none :runname $$runname;/home/matt/data/megatest/megatest -runall -target ubuntu/nfs/none :runname $$runname;/home/matt/data/megatest/megatest -runall -target ubuntu/nfs/none :runname $$runname);done

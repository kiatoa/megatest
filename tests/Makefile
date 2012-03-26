# run some tests

BINPATH=$(shell realpath ../bin)
MEGATEST=$(BINPATH)/megatest
PATH := $(BINPATH):$(PATH)
RUNNAME := $(shell date +w%V.%u.%H)
IPADDR := "-"
# Set SERVER to "-server -"
SERVER := ""

runall : test1 test2

test1 : cleanprep
	$(MEGATEST) -runtests ez_pass -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_a $(SERVER)

test2 : cleanprep
	$(MEGATEST) -runtests runfirst -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_b  $(SERVER) -debug 10

test3 : cleanprep
	$(MEGATEST) -runall -reqtarg ubuntu/nfs/none :runname $(RUNNAME)_b -m "This is a comment specific to a run" -v $(SERVER)

cleanprep : ../*.scm
	sqlite3 megatest.db "delete from metadat where var='SERVER';"
	mkdir -p /tmp/mt_runs /tmp/mt_links
	cd ..;make
	@sleep 1
	@if ps -def |awk '{print $8}'|grep megatest; then \
	   echo WARNING: These tests will kill megatest and dashboard!; \
	   sleep 3; \
	   killall -9 dboard || true; \
  	   killall -9 megatest || true; \
	fi
	cd ../;make install
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

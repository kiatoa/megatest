# run some tests

BINPATH=$(shell realpath ../bin)
MEGATEST=$(BINPATH)/megatest
PATH := $(BINPATH):$(PATH)

runall :
	cd ../;make install
	mkdir -p /tmp/mt_runs /tmp/mt_links
	$(BINPATH)/dboard -rows 15 &
	$(MEGATEST) -runall -target ubuntu/nfs/none :runname `date +w%V.%u.%H` -m "This is a comment specific to a run" -v

test :
	csi -b -I .. ../megatest.scm -- -runall -target ubuntu/afs/tmp :runname blah
	cd ../;make test
	make runall

dashboard :
	cd ../;make dboard
	$(BINPATH)/dboard &

remove :
	(cd ../;make);$(MEGATEST) -remove-runs :runname $(RUN)  -testpatt % -itempatt % :sysname % :fsname % :datapath %

runforever :
	while(ls); do runname=`date +%F-%R:%S`;$(MEGATEST) -runall -target ubuntu/nfs/none :runname $$runname;/home/matt/data/megatest/megatest -runall -target ubuntu/nfs/none :runname $$runname;/home/matt/data/megatest/megatest -runall -target ubuntu/nfs/none :runname $$runname;done

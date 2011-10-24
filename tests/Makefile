# run some tests

BINPATH=$(shell realpath ../bin)
MEGATEST=$(BINPATH)/megatest
PATH := $(BINPATH):$(PATH)

runall :
	cd ../;make install 
	$(MEGATEST) -keepgoing -runall :sysname ubuntu :fsname nfs :datapath none :runname `date +w%V.%u.%H` -m "This is a comment specific to a run" -v

test :
	csi -b -I .. ../megatest.scm -- -runall :sysname ubuntu :fsname afs :datapath tmp :runname blah
	cd ../;make test
	make runall

dashboard :
	cd ../;make dboard
	$(BINPATH)/dboard &

remove :
	(cd ../;make);$(MEGATEST) -remove-runs :runname $(RUN)  -testpatt % -itempatt % :sysname % :fsname % :datapath %

runforever :
	while(ls); do runname=`date +%F-%R:%S`;$(MEGATEST) -runall :sysname ubuntu :fsname nfs :datapath none :runname $$runname;/home/matt/data/megatest/megatest -runall :sysname ubuntu :fsname nfs :datapath none :runname $$runname;/home/matt/data/megatest/megatest -runall :sysname ubuntu :fsname nfs :datapath none :runname $$runname;done

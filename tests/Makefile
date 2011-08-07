# run some tests

MEGATEST=$(shell realpath ../megatest)

runall :
	cd ../;make 
	$(MEGATEST) -keepgoing -runall :sysname ubuntu :fsname nfs :datapath none :runname `date +%GWW%V.%u` -m "This is a comment specific to a run" -v

test :
	csi -b -I .. ../megatest.scm -- -runall :sysname ubuntu :fsname afs :datapath tmp :runname blah
	cd ../;make test
	make runall

dashboard :
	cd ../;make dashboard
	../dashboard &

remove :
	(cd ../;make);../megatest -remove-runs :runname $(RUN)  -testpatt % -itempatt % :sysname % :fsname % :datapath %

runforever :
	while(ls); do runname=`date +%F-%R:%S`;/home/matt/data/megatest/megatest -runall :sysname ubuntu :fsname nfs :datapath none :runname $$runname;/home/matt/data/megatest/megatest -runall :sysname ubuntu :fsname nfs :datapath none :runname $$runname;/home/matt/data/megatest/megatest -runall :sysname ubuntu :fsname nfs :datapath none :runname $$runname;done

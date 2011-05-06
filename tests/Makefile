# run some tests

MEGATEST=$(shell realpath ../megatest)

runall :
	cd ../;make 
	$(MEGATEST) -runall :sysname ubuntu :fsname nfs :datapath none :runname `date +%GWW%V.%u` -m "This is a comment specific to a run"

test :
	cd ../;make test
	make runall

dashboard :
	cd ../;make dashboard
	../dashboard &

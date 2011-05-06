# run some tests

runall :
	cd ../;make 
	../megatest -runall :sysname ubuntu :fsname nfs :datapath none :runname `date +%GWW%V.%u` -m "This is a comment specific to a run"

test :
	cd ../;make test
	make runall

dashboard :
	cd ../;make dashboard
	../dashboard &

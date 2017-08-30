minimt : minimt.scm db.scm setup.scm direct.scm
	csc minimt.scm

run    : minimt
	export PATH="$(PWD)":$(PATH) ; minimt runrun foo/bar run1 

runseq : clean run 
	sleep 5;tail -F runtest/*log

clean :
	rm -rf runtest/*


BINDIR    = $(PWD)/../../../bin
PATH     := $(BINDIR):$(PATH)
MEGATEST  = $(BINDIR)/megatest
DASHBOARD = $(BINDIR)/dashboard
NEWDASHBOARD = $(BINDIR)/newdashboard
RUNNAME   = a
NUMTESTS  = 20

all :
	$(MEGATEST) -remove-runs -target a/b :runname c -testpatt %/%
	$(MEGATEST) -runtests % -target a/b :runname c

bigbig :
	for tn in a b c d;do \
	   ($(MEGATEST) -runtests % -target a/b :runname $tn & ) ; \
	done

bigrun :
	NUMTESTS=$(NUMTESTS) $(MEGATEST) -runtests bigrun -target a/bigrun :runname a$(shell date +%V)

bigrun2 :
	NUMTESTS=$(NUMTESTS) $(MEGATEST) -runtests bigrun2 -target a/bigrun2 :runname a$(shell date +%V)

bigrun3 :
	NUMTESTS=$(NUMTESTS) $(MEGATEST) -runtests bigrun3 -target a/bigrun3 :runname $(RUNNAME)

dashboard : 
	mkdir -p ../simpleruns
	$(DASHBOARD) -rows 20 &

newdashboard :
	$(NEWDASHBOARD) &

compile :
	(cd ../../..;make -j && make install)

clean :
	rm -rf ../simple*/*/* megatest.db db/* ../simple*/.db/* logs/* monitor.db


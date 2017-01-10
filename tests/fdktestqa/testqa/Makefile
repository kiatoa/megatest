BINDIR    = $(PWD)/../../../bin
PATH     := $(BINDIR):$(PATH)
MEGATEST  = $(BINDIR)/megatest
DASHBOARD = $(BINDIR)/dashboard
NEWDASHBOARD = $(BINDIR)/newdashboard
RUNNAME   = a
NUMTESTS  = 20

all :
	$(MEGATEST) -remove-runs -target a/b -runname c -testpatt %/%
	$(MEGATEST) -run -testpatt % -target a/b -runname c

bigbig :
	for tn in a b c d;do \
	   ($(MEGATEST) -run -testpatt % -target a/b -runname $tn & ) ; \
	done

waitonpatt :
	megatest -remove-runs -runname waitonpatt -target a/b -testpatt %
	NUMTESTS=15 megatest -run -target a/b -runname waitonpatt -testpatt bigrun3/%8

waitonall :
	megatest -remove-runs -runname waitonall -target a/b -testpatt %
	NUMTESTS=20 megatest -run -target a/b -runname waitonall -testpatt alltop

bigrun :
	NUMTESTS=$(NUMTESTS) $(MEGATEST) -run -testpatt bigrun -target a/bigrun -runname a$(shell date +%V)

bigrun2 :
	NUMTESTS=$(NUMTESTS) $(MEGATEST) -run -testpatt bigrun2 -target a/bigrun2 -runname a$(shell date +%V)

bigrun3 :
	NUMTESTS=$(NUMTESTS) $(MEGATEST) -run -testpatt bigrun3 -target a/bigrun3 -runname $(RUNNAME)

dashboard : 
	mkdir -p ../simpleruns
	$(DASHBOARD) -rows 20 &

newdashboard :
	$(NEWDASHBOARD) &

compile :
	(cd ../../..;make -j && make install)

clean :
	rm -rf ../simple*/*/* megatest.db db/* ../simple*/.db/* logs/* monitor.db /tmp/$(USER)/megatest_localdb/testqa .server

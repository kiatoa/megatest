BINDIR    = $(PWD)/../../../bin
PATH     := $(BINDIR):$(PATH)
MEGATEST  = $(BINDIR)/megatest
DASHBOARD = $(BINDIR)/dashboard
RUNNAME   = a


all :
	$(MEGATEST) -remove-runs -target a/b :runname c -testpatt %/%
	$(MEGATEST) -runtests % -target a/b :runname c

bigbig :
	$(MEGATEST) -server - -daemonize ; sleep 3
	for tn in a b c d;do \
	   ($(MEGATEST) -runtests % -target a/b :runname $tn & ) ; \
	done

bigrun :
	$(MEGATEST) -runtests bigrun -target a/bigrun :runname $(RUNNAME)

bigrun2 :
	$(MEGATEST) -runtests bigrun2 -target a/bigrun2 :runname $(RUNNAME)

bigrun3 :
	$(MEGATEST) -runtests bigrun3 -target a/bigrun3 :runname $(RUNNAME)

dashboard : 
	$(DASHBOARD) -rows 20 &

compile :
	$(MEGATEST) -stop-server 0
	(cd ../../..;make && make install)

clean :
	rm -rf ../simple*/*/* megatest.db monitor.db


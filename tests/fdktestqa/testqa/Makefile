BINDIR    = $(PWD)/../../../bin
PATH     := $(BINDIR):$(PATH)
MEGATEST  = $(BINDIR)/megatest
DASHBOARD = $(BINDIR)/dashboard
all :
	$(MEGATEST) -remove-runs -target a/b :runname c -testpatt %/%
	$(MEGATEST) -runtests % -target a/b :runname c

bigbig :
	for tn in a b c d;do \
	   ($(MEGATEST) -runtests % -target a/b :runname $tn & ) ; \
	done

bigrun :
	$(MEGATEST) -runtests bigrun -target a/bigrun :runname a$(shell date +%V)

bigrun2 :
	$(MEGATEST) -runtests bigrun2 -target a/bigrun2 :runname a$(shell date +%V)

dashboard : 
	$(DASHBOARD) -rows 20 &

compile :
	(cd ../../..;make && make install)

clean :
	rm -rf ../simple*/*/* megatest.db db/*

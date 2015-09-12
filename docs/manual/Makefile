
ASCPATH = $(shell which asciidoc)
EXEPATH = $(shell realpath $(ASCPATH))
BINPATH = $(shell dirname $(EXEPATH))
DISPATH = $(shell dirname $(BINPATH))

# broad_goals.csv  needed_features.csv : tables/*.dat
#         ./refdb2csv tables

# in a makefile recipe, $< denotes the first dependency; $@ the target

# design_spec.html : $(SRCFILES) $(CSVFILES)
#         asciidoc -b html5 -a icons -a iconsdir=$(DISPATH)/images/icons -a toc2 design_spec.txt
#

all : server.ps megatest_manual.html client.ps complex-itemmap.png

megatest_manual.html : megatest_manual.txt getting_started.txt writing_tests.txt reference.txt ../plan.txt howto.txt *png
	asciidoc  -b html5 -a icons -a iconsdir=$(DISPATH)/images/icons -a toc2 megatest_manual.txt
#	dos2unix megatest_manual.html

server.ps : server.dot
	dot -Tps server.dot > server.ps

client.ps : client.dot
	dot -Tps client.dot > client.ps

complex-itemmap.png : complex-itemmap.dot
	dot -Tpng complex-itemmap.dot -o complex-itemmap.png
	dot -Tpdf complex-itemmap.dot -o complex-itemmap.pdf

clean:
	rm -f megatest_manual.html

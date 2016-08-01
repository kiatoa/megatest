ASCPATH = $(shell which asciidoc)
EXEPATH = $(shell readlink -f $(ASCPATH))
BINPATH = $(shell dirname $(EXEPATH))
DISPATH = $(shell dirname $(BINPATH))

api.html : api.txt
	asciidoc  -b html5 -a icons -a iconsdir=$(DISPATH)/images/icons -a toc2 api.txt

# all : html/megatest.html megatest.pdf

html/megatest.html : megatest.lyx
	elyxer megatest.lyx html/megatest.html
	fossil add html/*

megatest.pdf : megatest.lyx
	lyx -e pdf2 megatest.lyx


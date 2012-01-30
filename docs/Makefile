all : html/megatest.html megatest.pdf

html/megatest.html : megatest.lyx
	elyxer megatest.lyx html/megatest.html
	fossil add html/*

megatest.pdf : megatest.lyx
	lyx -e pdf2 megatest.lyx


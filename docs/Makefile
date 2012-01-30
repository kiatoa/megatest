all : html/megatest.html megatest.lyx

html/megatest.html : megatest.lyx
	rm -rf megatest.html.LyXconv
	lyx -e html megatest.lyx
	cp megatest.html.LyXconv/* html/
	fossil add html/*

megatest.pdf : megatest.lyx
	lyx -e pdf megatest.lyx



html/megatest.html : megatest.lyx
	rm -rf megatest.html.LyXconv
	lyx -e html megatest.lyx
	cp megatest.html.LyXconv/* html/
	fossil add html/*


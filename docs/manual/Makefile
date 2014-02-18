all : server.pdf megatest_manual.html client.pdf

megatest_manual.html : megatest_manual.txt getting_started.txt writing_tests.txt reference.txt ../plan.txt
	asciidoc megatest_manual.txt
	dos2unix megatest_manual.html

server.pdf : server.dot
	dot -Tpdf server.dot > server.pdf
	
client.pdf : client.dot
	dot -Tpdf client.dot > client.pdf

clean:
	rm -f megatest_manual.html

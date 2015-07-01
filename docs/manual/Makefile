all : server.ps megatest_manual.html client.ps

megatest_manual.html : megatest_manual.txt getting_started.txt writing_tests.txt reference.txt ../plan.txt
	asciidoc megatest_manual.txt
	dos2unix megatest_manual.html

server.ps : server.dot
	dot -Tps server.dot > server.ps

client.ps : client.dot
	dot -Tps client.dot > client.ps

clean:
	rm -f megatest_manual.html

 grep configf:lookup *.scm | sed 's/^.*:lookup//; s/^-number//; s/^ //' | grep -v '^\(section\|test-conf\|tconfig\|testconfig\|dat\|config\|views-cfgdat\)' | perl -pe 's/^\s*(\*configdat\*|configdat|mtconf)//; s/^\s+//; s/\).*$//; s/"//g' | awk '{print $1,$2}' | sort | grep -v section | sort | uniq


#!/bin/bash

echo "Finding unused globals:"

for var in $(egrep '^\s*\(define\s+\*' *.scm|awk '{print $2}'|sort -u);do
    if ! $(egrep -v '^\s*\(define' *scm| grep "$var" > /dev/null);then
	echo "$var not used";
    fi;
done

echo
echo "Finding globals without proper definition in common.scm:"

for var in $(egrep -v '^\s*\(define' *.scm|\
		    grep -P -v '^\s*;'|\
		    grep -P '\*[a-zA-Z]+\S+\*'|\
		    tr '*' '/' |\
		    perl -p -e 's%.*(\/\S+\/).*%$1%'|\
		    egrep '\/[a-zA-Z]+\S+\/'|\
		    sort -u);do
    newvar=$(echo $var | tr '/' '*')
    # echo "VAR is $var, newvar is $newvar"
    if ! $(grep -P '^\s*\(define\s+' common.scm|\
		  grep -P -v '^\s*;'|\
		  grep "$newvar" > /dev/null);then
	echo "$newvar not defined in common.scm"
    fi
done


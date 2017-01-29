#!/bin/bash

while IFS=': ' read x x x x p x x i x; do
    if ! [[ ${i}x == "x" ]];then
	if ! $(echo $i|grep EOF >/dev/null);then
	    fname=$(sudo find -L "/proc/$p/fd" -maxdepth 1 -inum "$i" -exec readlink {} \; -quit)
	    if $(echo $fname | grep megatest.db > /dev/null) || \
	       $(echo $fname | egrep '.db/\d+.db' > /dev/null);then
		echo $fname
	    fi
	fi
    fi
done < /proc/locks

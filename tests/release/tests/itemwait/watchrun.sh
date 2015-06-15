#!/bin/bash

runname=$1

pass=1
alldone=1
while ! $alldone;do
    sleep 5
    bigrun_running=$(megatest -list-runs a|egrep 'bigrun\(.*RUNNING'|wc -l)
    bigrun2_pass=$(megatest -list-runs a|egrep 'bigrun2.*COMPLETED.*PASS')
    if [[ $bigrun_running -gt 0 && $bigrun2_pass -gt 0 ]];then
	pass=0
	alldone=0
    fi
done

if $pass;then
    echo PASS
    exit 0
else
    echo FAIL
    exit 1
fi

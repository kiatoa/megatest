#!/bin/bash

runname=$1

pass=no
alldone=no
while [[ $alldone == no ]];do
    sleep 5
    $MTRUNNER $MTTESTDIR/fdktestqa/testqa $MTPATH megatest -list-runs $runname > list-runs.log
    bigrun_running=$(cat list-runs.log | egrep 'bigrun\(.*RUNNING'|wc -l)
    bigrun2_pass=$(cat list-runs.log   | egrep 'bigrun2.*COMPLETED.*PASS'|wc -l)
    echo "bigrun_running=$bigrun_running, bigrun2_pass=$bigrun2_pass"
    if [[ $bigrun_running -gt 0 ]] && [[ $bigrun2_pass -gt 0 ]];then
	pass=yes
	alldone=yes
    fi
    if [[ $bigrun_running -eq 0 ]];then
	echo "bigrun all done and no bigrun2 found with PASS."
	alldone=yes
    fi
done

if [[ $pass == yes ]];then
    echo PASS
    exit 0
else
    echo FAIL
    exit 1
fi

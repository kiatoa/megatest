#!/bin/bash
if [ $NUMBER -lt 10 ];then 
   sleep 20
   sleep `echo 4 * $NUMBER | bc`
else
   sleep 130
fi

if [[ $RANDOM -lt 10000 ]];then
  exit 1
else
  exit 0
fi

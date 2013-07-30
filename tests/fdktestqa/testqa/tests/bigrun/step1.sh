#!/bin/sh
if [ $NUMBER -lt 15 ];then 
   sleep 2
   sleep `echo 2 * $NUMBER | bc`
else
   sleep 100
fi

exit 0

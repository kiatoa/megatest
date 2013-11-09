#!/bin/bash

for x in `cat all-db-procs.txt`;do
  cat > ~/.megatestrc <<EOF
(require-library trace)
(import trace)
(trace
$x
)
EOF
  fname=`echo "$x" | tr ':!>' '-_g'`
  megatest -runtests sqlitespeed,test2,ez% -target ubuntu/nfs/none :runname $fname > $fname.log
done 



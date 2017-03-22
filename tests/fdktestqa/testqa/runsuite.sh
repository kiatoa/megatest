#!/bin/bash

# (cd ../../..;make && make install) || exit 1
# export PATH=$PWD/../../../bin:$PATH

for i in a b c d e f;do
  # g h i j k l m n o p q r s t u v w x y z;do
  viewscreen megatest -run -testpatt % -target a/b -runname w$(date +%U.%u.%H)$i
done

echo "" > num-running.log
while true; do
  foo=$(megatest -list-runs % | grep RUNNING | wc -l)
  echo "Num running at `date` $foo"
  echo "$foo at `date`" >> num-running.log
  sleep 10
done

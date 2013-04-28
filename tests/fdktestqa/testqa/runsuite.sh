#!/bin/bash

(cd ../../..;make && make install) || exit 1
export PATH=$PWD/../../../bin:$PATH

for i in a b c d e f;do
  # g h i j k l m n o p q r s t u v w x y z;do
  megatest -runtests % -target a/b :runname $i &
done

echo "" > num-running.log
while true; do
  foo=`megatest -list-runs % | grep RUNNING | wc -l`
  echo "Num running at `date` $foo"
  echo "$foo at `date`" >> num-running.log
  # to make the test go at a reasonable clip only gather this info ever minute
  sleep 1m
done

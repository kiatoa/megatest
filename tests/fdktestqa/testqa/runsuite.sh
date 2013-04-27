#!/bin/bash

for i in a b c d e f;do
  # g h i j k l m n o p q r s t u v w x y z;do
  megatest -runtests % -target a/b :runname $i &
done

echo "" > num-running.log
while true; do
  foo=`megatest -list-runs % | grep RUNNING | wc -l`
  echo "Num running: $foo"
  echo $foo >> num-running.log
done

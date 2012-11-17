#!/bin/bash

rm -f mockup.db

echo Compiling mockupserver.scm and mockupclient.scm
csc mockupserver.scm
csc mockupclient.scm

echo Starting server
./mockupserver &

sleep 1

echo Starting clients
IVALS=
for i in a b c d e f g h i j k l m n o p q s t u v w x y z;
  do
  for j in 0 1 2 3 4 5 6 7 8 9;
    do
    echo Starting client $i$j
    ./mockupclient $i$j &
  done
done

wait
# echo "Running for one minute then killing all mockupserver and mockupclient processes"
# sleep 60
# killall -v mockupserver mockupclient

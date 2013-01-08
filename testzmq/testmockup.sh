#!/bin/bash

rm -f mockup.db

echo Compiling mockupserver.scm and mockupclient.scm
csc random.scm
csc mockupserver.scm
csc mockupclient.scm

echo Starting server
./mockupserver &

sleep 1

echo Starting clients
for i in a b c d e; # f g h i j k l m n o p q s t u v w x y z; 
  do
  for k in a b;
    do
    for j in 0 1 2 3 4 5 6 7 8 9;
      do
      waittime=`./random 0 60`
      runtime=`./random 5 120`
      echo "Starting client $i$k$j with waittime $waittime and runtime $runtime" 
      (sleep $waittime;./mockupclient $i$k$j $runtime) &
    done
  done
done

wait
echo testmockup.sh script done
# echo "Waiting for 5 seconds then killing all mockupserver and mockupclient processes"
# sleep 30
# killall -v mockupserver mockupclient

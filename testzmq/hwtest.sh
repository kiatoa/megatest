#!/bin/bash

echo Compiling hwclient and hwserver
csc hwclient.scm
csc hwserver.scm

./hwserver &
sleep 1
for x in a b c d e f g h i j k l m n o p q r s t u v w x y z;do
./hwclient $x &
done

# killall -v hwserver hwclient

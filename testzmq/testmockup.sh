#!/bin/bash

csc mockupserver.scm
csc mockupclient.scm

./mockupserver &

for i in a b;do
  ./mockupclient $i &
done

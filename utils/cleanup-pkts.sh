#!/bin/bash

pushd $1

for x in *.pkt;do
    if grep 'T configf' $x > /dev/null;then
        echo rm $x
    else
        echo skip $x
    fi
done


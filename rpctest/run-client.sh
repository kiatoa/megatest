#!/bin/bash


while ./rpctest client "insert into foo (var,val) values ($RANDOM,$RANDOM);";do
    numrows=$(./rpctest client "select count(id) from foo;") # |wc -l)
    deletefrom=$RANDOM
    echo "numrows=$numrows, deletefrom=$deletefrom"
    if [[ $numrows -gt 300 ]];then
	echo "numrows=$numrows, deletefrom=$deletefrom"
	./rpctest client "delete from foo where var > $deletefrom;"
    fi
done

#!/bin/bash

if [[ $TARGETDISPLAY = "" || $TARGETHOST = "" || $TARGETDIR = "" || $TARGETUSER = "" ]]; then
    msg="You must set the TARGETDISPLAY, TARGETHOST, TARGETDIR and TARGETUSER variables for manual tests"
    echo $msg
    megatest -test-status :state COMPLETED :status FAIL -m $msg
    exit 1
else
    megatest -step setup :state start :status n/a
    xterm -display $TARGETDISPLAY -e ./setupremote.sh
    megatest -step setup :state end :status $?
fi


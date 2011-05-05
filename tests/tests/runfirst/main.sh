#!/bin/bash

megatest -step wasting_time :state start :status n/a -m "This is a test step comment"
sleep 20
megatest -step wasting_time :state end :status $?

megatest -test-status :state COMPLETED :status PASS -setlog thelogfile.log -m "This is a test level comment"

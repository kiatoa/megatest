#!/bin/bash

# megatest -step wasting_time :state start :status n/a -m "This is a test step comment"
# sleep 20
# megatest -step wasting_time :state end :status $?

megatest -runstep wasting_time -logpro wasting_time.logpro "sleep 20;echo all done eh?" -m "This is a test step comment"

megatest -test-status :state COMPLETED :status PASS -m "This is a test level comment"

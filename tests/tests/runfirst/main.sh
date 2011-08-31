#!/bin/bash

# megatest -step wasting_time :state start :status n/a -m "This is a test step comment"
# sleep 20
# megatest -step wasting_time :state end :status $?

touch ../I_was_here

$MT_MEGATEST -runstep wasting_time -logpro wasting_time.logpro "sleep 8;echo all done eh?" -m "This is a test step comment"

$MT_MEGATEST -test-status :state COMPLETED :status $? -m "This is a test level comment" -set-toplog the_top_log.html :value 1e6 :expected_value 1.1e6 :tol 100e3

# $MT_MEGATEST -test-status :state COMPLETED :status FAIL

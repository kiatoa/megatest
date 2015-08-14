#!/bin/bash

# megatest -step wasting_time :state start :status n/a -m "This is a test step comment"
# sleep 20
# megatest -step wasting_time :state end :status $?

$MT_MEGATEST -runstep wasting_time -logpro wasting_time.logpro "sleep 5;echo alldone" -m "This is a test step comment"

$MT_MEGATEST -test-status :state COMPLETED :status $? -m "This is a test level comment" -set-toplog the_top_log.html :first_err "This is the first error"

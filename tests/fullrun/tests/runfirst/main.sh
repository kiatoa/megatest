#!/bin/bash
(export DISPLAY=:0;xterm) 
# megatest -step wasting_time :state start :status n/a -m "This is a test step comment"
# sleep 20
# megatest -step wasting_time :state end :status $?

touch ../I_was_here
mkdir -p $MT_RUN_AREA_HOME/tmp/$USER/$sysname/$fsname/$datapath/$MT_RUNNAME
echo 1 2 3 4 5 > $MT_RUN_AREA_HOME/tmp/$USER/$sysname/$fsname/$datapath/$MT_RUNNAME/the_ans

$MT_MEGATEST -runstep wasting_time -logpro wasting_time.logpro "sleep 8;echo all done eh?" -m "This is a test step comment"

$MT_MEGATEST -load-test-data << EOF
foo,bar,1.2,1.9,>
foo,rab,1.0e9,10e9,1e9
foo,bla,1.2,1.9,<
foo,bal,1.2,1.2,<,,Check for overload
foo,alb,1.2,1.2,<=,Amps,This is the high power circuit test
foo,abl,1.2,1.3,0.1
foo,bra,1.2,pass,silly stuff
faz,bar,10,8mA,,,"this is a comment"
EOF

$MT_MEGATEST -load-test-data << EOF
cat,  var, val, exp, comp, units, comment, status, type
ameas,iout,1.2,1.9,>,Amps,Comment,,meas
EOF
loadstatus=$?

if [[ `basename $PWD` == "mustfail" ]];then
  $MT_MEGATEST -test-status :state COMPLETED :status FAIL
else
  $MT_MEGATEST -test-status :state COMPLETED :status $loadstatus -m "This is a test level comment" :value 10e6 :expected_value 1.1e6 :tol 100e3 :category nada :variable sillyvar :units mFarks :comment "This is the value/expected comment"
fi

env > envfile.txt

# $MT_MEGATEST -test-status :state COMPLETED :status FAIL

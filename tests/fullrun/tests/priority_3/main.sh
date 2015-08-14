#!/bin/bash

# a bunch of steps in 2 second increments
for i in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17;do
  $MT_MEGATEST -step step$i :state start :status running -setlog results$i.html
  sleep 2
  echo "<html><header>Results$i</header><body>Nothing but faux results here!</body></html>" > results$i.html
  $MT_MEGATEST -step step$i :state end :status 0
done

# get a previous test
export EZFAILPATH=`$MT_MEGATEST -test-files lookithome.log -target $MT_TARGET :runname $MT_RUNNAME -testpatt ez_fail`
if [[ -e $EZFAILPATH ]];then
  echo All good!
else
  echo NOT good!
  exit 1
fi

exit 0

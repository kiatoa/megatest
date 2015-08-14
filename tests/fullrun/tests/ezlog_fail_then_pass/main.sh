#!/bin/bash

megatest -step yepstep :state start :status n/a
ls /tmp
megatest -step yepstep :state end :status $?

megatest -load-test-data << EOF
OPER,du,   1.2,  1.2, <   , GBytes  ,System didn't use too much space
EOF

# a bunch of steps in 2 second increments
for i in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17;do
  $MT_MEGATEST -step step$i :state start :status running -setlog results$i.html
  sleep 2
  $MT_MEGATEST -step step$i :state end :status 0
done

megatest -test-status :state COMPLETED :status AUTO

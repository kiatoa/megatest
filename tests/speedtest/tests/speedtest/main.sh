#!/bin/bash

# a bunch of steps in 2 second increments
for i in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17;do
  $MT_MEGATEST -step step$i :state start :status running -setlog results$i.html
  sleep $TEST_DELAY
  $MT_MEGATEST -step step$i :state end :status 0
done

exit 0

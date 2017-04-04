#!/bin/bash

# a bunch of steps in 2 second increments
for i in 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17;do
  echo "start step before $i: `date`"
  $MT_MEGATEST -step step$i :state start :status running -setlog results$i.html
  echo "start step after $i: `date`"
  sleep 2
  echo "end step before $i: `date`"
  $MT_MEGATEST -step step$i :state end :status 0
  echo "end step after $i: `date`"
done

exit 0

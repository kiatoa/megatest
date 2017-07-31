#!/bin/bash
#
# Rollup counts of calls to Megatest from a logging dat file
#
# Usage: mtrept.sh file [host]

if [[ "$2"x != "x" ]];then
  host_name_grep="grep $2 | "
else
  host_name_grep=""
fi
if [[ "$1"x == "x" ]];then
  datfile=/p/fdk/gwa/$USER/.logger/all.dat
else
  datfile=$1
fi
datcopy=/tmp/$USER/all.$PID.dat

if [[ -e $datfile ]];then
   cp $datfile $datcopy
   list_runs=$(grep list-runs $datcopy |$host_name_grep wc -l)
   show_config=$(grep show-config $datcopy |$host_name_grep wc -l)
   list_targets=$(grep list-targets $datcopy |$host_name_grep wc -l)
   mt_run=$(grep ' -run ' $datcopy |$host_name_grep wc -l)
   execute=$(grep ' -execute' $datcopy|$host_name_grep wc -l)
   server=$(grep ' -server' $datcopy|$host_name_grep wc -l)
   sync_to=$(grep ' -sync-to' $datcopy|$host_name_grep wc -l)
   step=$(grep ' -step' $datcopy|$host_name_grep wc -l)
   state_status=$(grep ' -set-state-status' $datcopy|$host_name_grep wc -l)
   test_status=$(grep ' -test-status' $datcopy|$host_name_grep wc -l)
   other=$(egrep -v ' -(list-runs|show-config|list-targets|run|execute|server|sync-to|step|set-state-status|test-status)' $datcopy |$host_name_grep wc -l)
   start_time=$(head -1 $datcopy|awk '{print $1}')
   end_time=$(tail -1 $datcopy | awk '{print $1}')
   minutes=$(echo "($end_time-$start_time)/60.0" | bc)
   hours=$(echo "($minutes/60)"|bc)
   total_calls=$(cat $datcopy |$host_name_grep wc -l)
   
   if [[ $hours -gt 2 ]];then
      echo "Over $hours hour period we have;"
   else
      echo "Over $minutes minutes we have;"
   fi
   echo "    list-runs:    $list_runs"
   echo "    show-config:  $show_config"
   echo "    list-targets: $list_targets"
   echo "    execute:      $execute"
   echo "    run:          $mt_run"
   echo "    server:       $server"
   echo "    step:         $step"
   echo "    test-status:  $test_status"
   echo "    sync-to:      $sync_to"
   echo "    state-status: $state_status"
   echo "    other:        $other"
   echo "    total:        $total_calls"
else
   echo "Could not find input file $datfile"
fi


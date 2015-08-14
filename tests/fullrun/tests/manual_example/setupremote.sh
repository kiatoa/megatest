#!/bin/bash

megatest -step rsyncto :state start :status n/a
echo "First, lets populate your area with necessary files, you may be prompted for your Unix password several times"
rsync -avz $MT_TEST_RUN_DIR/  $TARGETUSER@$TARGETHOST:$TARGETDIR
megatest -step rsyncto :state end :status n/a

megatest -step runtest :state start :status n/a
remotecmd="cd $TARGETDIR;xterm -display $TARGETDISPLAY"
echo Launching $remotecmd on $TARGETHOST as $TARGETUSER
ssh $TARGETUSER@$TARGETHOST $remotecmd
megatest -step runtest :state end :status $?

megatest -step gatherdata :state start :status n/a
rsync -avz $TARGETUSER@$TARGETHOST:$TARGETDIR/results/ $MT_TEST_RUN_DIR/results/
if [[ -e $MT_TEST_RUN_DIR/results/results.csv ]]; then
    megatest -load-test-data < $MT_TEST_RUN_DIR/results/results.csv
fi

if [[ -e $MT_TEST_RUN_DIR/results/final_results.log && $MT_TEST_RUN_DIR/final_results.logpro ]]; then
    logpro $MT_TEST_RUN_DIR/final_results.logpro $MT_TEST_RUN_DIR/final_results.html < $MT_TEST_RUN_DIR/results/final_results.log
    if [[ $? = 0 ]]; then
	finalstatus=PASS
    else
	finalstatus=FAIL
    fi
    megatest -test-status :state COMPLETED :status $finalstatus -setlog final_results.html
fi

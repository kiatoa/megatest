#!/bin/bash -e

filter=`refdb lookup $MT_RUN_AREA_HOME/$CFG_TYPE machines $TARGETHOST filter`

echo "Using filter: $filter"

diskareas=`mount | egrep 'ext|mfs|nfs'| egrep -v "$filter" | awk '{print $3}'`

for dirname in $diskareas;do

    echo "dirname: $dirname"

    # measure the free space
    freespace=`df -P -k $dirname | grep $dirname | awk '{print $4}'`

    # get the minfree allowed from the refdb
    minfree=`refdb lookup $MT_RUN_AREA_HOME/$CFG_TYPE machines $TARGETHOST minfree`

    if [[ "$freespace" -lt "$minfree" ]];then
	echo "ERROR: available space $freespace is less than minimum allowed of $minfree on $dirname"
    else
	echo "INFO: space available of $freespace k on $dirname meets required minimum of $minfree."
    fi
done

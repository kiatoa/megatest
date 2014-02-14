#!/bin/bash -e

diskareas=`mount | egrep 'ext|mfs|nfs'|grep -v proc | awk '{print $3}'`

for dirname in $diskareas;do

    # measure the free space
    freespace=`df -k $dirname | grep $dirname | awk '{print $4}'`

    # get the minfree allowed from the refdb
    minfree=`refdb lookup $MT_RUN_AREA_HOME/cfg machines $TARGETHOST minfree`

    if [[ $freespace -lt $minfree ]];then
	echo "ERROR: available space $freespace is less than minimum allowed of $minfree on $dirname"
    else
	echo "INFO: space available of $freespace k on $dirname meets required minimum of $minfree."
    fi
done

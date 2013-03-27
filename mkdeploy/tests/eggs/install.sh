#!/bin/bash -e
freespace=`df -k /$DIRECTORY | grep $DIRECTORY | awk '{print $4}'`
if [[ $freespace -lt $REQUIRED ]];then
  echo "ERROR: insufficient space on /$DIRECTORY"
  exit 1
else
  echo "There is adequate space on /$DIRECTORY"
fi

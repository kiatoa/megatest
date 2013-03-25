#!/bin/bash -e
diskpath=$1
spacereq=$2
freespace=`df -k $diskpath | grep $diskpath | awk '{print $4}'`
if [[ $freespace -lt $spacereq ]];then
  echo "ERROR: insufficient space on $diskpath"
  exit 1
else
  echo "There is adequate space on $diskpath"
fi

#!/usr/bin/env bash

export LINKSDIR=$1
export RUNSDIR=$2

if [ "x$LINKSDIR" == "x" ];then
   echo Usage: cleanup-links-dir /links/dir/path /runs/dir/path
   exit
fi


echo Removing dangling links....
for lnk in `find $LINKSDIR -type l ! -exec test -r {} \; -print`; do
  echo $lnk
  rm -f $lnk
done

echo Removing empty directories....
find $LINKSDIR -depth -type d -empty -print -exec rmdir {} \;

#!/usr/bin/env bash

# Run your step here
source $PREFIX/buildsetup.sh
# source $PREFIX/setup-chicken4x.sh

if [[ `uname -a | grep x86_64` == "" ]]; then 
    export ARCHSIZE=''
else
    export ARCHSIZE=64_
fi
    # export files="cd-5.4.1_Linux${IUPLIB}_lib.tar.gz im-3.6.3_Linux${IUPLIB}_lib.tar.gz iup-3.5_Linux${IUPLIB}_lib.tar.gz"
if [[ x$USEOLDIUP == "x" ]];then
   export files="cd-5.5.1_Linux${IUPLIB}_${ARCHSIZE}lib.tar.gz im-3.8_Linux${IUPLIB}_${ARCHSIZE}lib.tar.gz iup-3.6_Linux${IUPLIB}_${ARCHSIZE}lib.tar.gz"
else
   echo WARNING: Using old IUP libraries
   export files="cd-5.4.1_Linux${IUPLIB}_${ARCHSIZE}lib.tar.gz im-3.6.3_Linux${IUPLIB}_${ARCHSIZE}lib.tar.gz iup-3.5_Linux${IUPLIB}_${ARCHSIZE}lib.tar.gz"
fi

mkdir -p $PREFIX/iuplib
for a in `echo $files` ; do
    if ! [[ -e ${DOWNLOADS}/$a ]] ; then
	(cd ${DOWNLOADS};wget http://www.kiatoa.com/matt/iup/$a)
    fi
    echo Untarring $a into $PREFIX/lib
    (cd $PREFIX/lib;tar xfvz ${DOWNLOADS}/$a;mv include/* ../include)
done


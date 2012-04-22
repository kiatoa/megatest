#!/bin/bash

# Copyright 2007-2010, Matthew Welland.
# 
#  This program is made available under the GNU GPL version 2.0 or
#  greater. See the accompanying file COPYING for details.
# 
#  This program is distributed WITHOUT ANY WARRANTY; without even the
#  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#  PURPOSE.

echo You may need to do the following first:
echo sudo apt-get install libreadline-dev
echo sudo apt-get install libmotif3 -OR- set KTYPE=26g4
echo KTYPE can be 26 or 26g4
echo You are using PREFIX=$PREFIX
echo You are using proxy="$proxy"
echo Hit ^C now to do that

# A nice way to run this script:
#
# script -c 'PREFIX=/tmp/delme ./installall.sh ' installall.log
# logpro installall.logpro installall.html < installall.log
# firefox installall.html

sleep 5

if [[ $proxy == "" ]]; then 
  echo 'Please set the environment variable "proxy" to host.com:port (e.g. foo.com:1234) to use a proxy'
  echo PROX=""
else
  export http_proxy=http://$proxy
  export PROX="-proxy $proxy"
fi

if [[ $KTYPE == "" ]]; then
  echo 'Using KTYPE=26'
  export KTYPE=26
else
  echo Using KTYPE=$KTYPE
fi

export CHICKEN_VERSION=4.7.3
if ! [[ -e chicken-${CHICKEN_VERSION}.tar.gz ]]; then 
    wget http://code.call-cc.org/dev-snapshots/2011/08/17/chicken-${CHICKEN_VERSION}.tar.gz
fi 

BUILDHOME=$PWD
if [[ $PREFIX == "" ]]; then
   PREFIX=$PWD/inst
fi

export PATH=$PREFIX/bin:$PATH
echo "export PATH=$PREFIX/bin:\$PATH" > setup-chicken4x.sh
export LD_LIBRARY_PATH=$PREFIX/lib
echo "export LD_LIBRARY_PATH=$PREFIX/lib" >> setup-chicken4x.sh

echo PATH=$PATH
echo LD_LIBRARY_PATH=$LD_LIBRARY_PATH

if ! [[ -e $PREFIX/bin/csi ]]; then
    tar xfvz chicken-${CHICKEN_VERSION}.tar.gz
    cd chicken-${CHICKEN_VERSION}
    make PLATFORM=linux PREFIX=$PREFIX
    make PLATFORM=linux PREFIX=$PREFIX install
    cd $BUILDHOME
fi

for f in readline apropos base64 regex-literals format regex-case test coops trace csv dot-locking posix-utils directory-utils hostinfo; do
  chicken-install $PROX $f
done

cd $BUILDHOME

for a in `ls */*.meta|cut -f1 -d/` ; do 
    echo $a
    (cd $a;chicken-install)
done

echo Install sqlite3
if ! [[ -e sqlite-autoconf-3070500.tar.gz ]]; then
    wget http://www.sqlite.org/sqlite-autoconf-3070500.tar.gz
fi

if ! [[ -e $PREFIX/bin/sqlite3 ]] ; then
    if [[ -e sqlite-autoconf-3070500.tar.gz ]]; then
	tar xfz sqlite-autoconf-3070500.tar.gz 
	(cd sqlite-autoconf-3070500;./configure --prefix=$PREFIX;make;make install)
	CSC_OPTIONS="-I$PREFIX/include -L$PREFIX/lib" chicken-install $PROX sqlite3
    fi
fi

chicken-install $PROX sqlite3

if [[ `uname -a | grep x86_64` == "" ]]; then 
    export files="cd-5.4.1_Linux${KTYPE}_lib.tar.gz im-3.6.3_Linux${KTYPE}_lib.tar.gz iup-3.5_Linux${KTYPE}_lib.tar.gz"
else
    export files="cd-5.4.1_Linux${KTYPE}_64_lib.tar.gz im-3.6.3_Linux${KTYPE}_64_lib.tar.gz iup-3.5_Linux${KTYPE}_64_lib.tar.gz"
fi

mkdir $PREFIX/iuplib
for a in `echo $files` ; do
    if ! [[ -e $a ]] ; then
	wget http://www.kiatoa.com/matt/iup/$a
    fi
    (cd $PREFIX/lib;tar xfvz $BUILDHOME/$a;mv include/* ../include)
done

# ffcall obtained from:
# cvs -z3 -d:pserver:anonymous@cvs.savannah.gnu.org:/sources/libffcall co ffcall 

if ! [[ -e ffcall.tar.gz ]] ; then
    wget http://www.kiatoa.com/matt/iup/ffcall.tar.gz 
fi

tar xfvz ffcall.tar.gz

cd ffcall
./configure --prefix=$PREFIX --enable-shared
make
make install


cd $BUILDHOME
export LIBPATH=$PREFIX/lib
export LD_LIBRARY_PATH=$LIBPATH
CSC_OPTIONS="-I$PREFIX/include -L$LIBPATH" chicken-install $PROX -D no-library-checks iup
CSC_OPTIONS="-I$PREFIX/include -L$LIBPATH" chicken-install $PROX -D no-library-checks canvas-draw

# export CD_REL=d704525ebe1c6d08
# if ! [[ -e  Canvas_Draw-$CD_REL.zip ]]; then
#     wget http://www.kiatoa.com/matt/iup/Canvas_Draw-$CD_REL.zip
# fi
# 
# unzip -o Canvas_Draw-$CD_REL.zip
# 
# cd "Canvas Draw-$CD_REL/chicken"
# CSC_OPTIONS="-I$PREFIX/include -L$LIBPATH" chicken-install $PROX -D no-library-checks

echo You may need to add $LD_LIBRARY_PATH to your LD_LIBRARY_PATH variable, a setup-chicken4x.sh 
echo file can be found in the current directory which should work for setting up to run chicken4x

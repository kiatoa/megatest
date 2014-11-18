#! /usr/bin/env bash

# set -x

# Copyright 2007-2014, Matthew Welland.
# 
#  This program is made available under the GNU GPL version 2.0 or
#  greater. See the accompanying file COPYING for details.
# 
#  This program is distributed WITHOUT ANY WARRANTY; without even the
#  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#  PURPOSE.

echo You may need to do the following first:
echo sudo apt-get install libreadline-dev
echo sudo apt-get install libwebkitgtk-dev 
echo sudo apt-get install libmotif3 -OR- set KTYPE=26g4
echo KTYPE can be 26, 26g4, or 32
echo  
echo KTYPE=$KTYPE
echo You are using PREFIX=$PREFIX
echo You are using proxy="$proxy"
echo 
echo "Set additional_libpath to help find gtk or other libraries, don't forget a leading :"

# NOTES:
#
# Centos with security setup may need to do commands such as following as root:
#
# NB// fix the paths first
#
# for a in /localdisk/chicken/4.8.0/lib/*.so;do chcon -t textrel_shlib_t $a; done 

echo ADDITIONAL_LIBPATH=$ADDITIONAL_LIBPATH
echo  
echo To use previous IUP libraries set USEOLDIUP to yes
echo USEOLDIUP=$USEOLDIUP
echo 
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

# Put all the downloaded tar files in tgz
mkdir -p tgz

# http://code.call-cc.org/releases/4.8.0/chicken-4.8.0.5.tar.gz
export CHICKEN_VERSION=4.8.0.5
export CHICKEN_BASEVER=4.8.0
chicken_targz=chicken-${CHICKEN_VERSION}.tar.gz
if ! [[ -e tgz/$chicken_targz ]]; then 
    wget http://code.call-cc.org/releases/${CHICKEN_BASEVER}/${chicken_targz}
    mv $chicken_targz tgz
fi 

BUILDHOME=$PWD
DEPLOYTARG=$BUILDHOME/deploy

if [[ $PREFIX == "" ]]; then
   PREFIX=$PWD/inst
fi

export PATH=$PREFIX/bin:$PATH
export LIBPATH=$PREFIX/lib$ADDITIONAL_LIBPATH
export LD_LIBRARY_PATH=$LIBPATH
export CHICKEN_INSTALL=$PREFIX/bin/chicken-install
echo "export PATH=$PREFIX/bin:\$PATH" > setup-chicken4x.sh
echo "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH" >> setup-chicken4x.sh

echo PATH=$PATH
echo LD_LIBRARY_PATH=$LD_LIBRARY_PATH

if ! [[ -e $PREFIX/bin/csi ]]; then
    tar xfvz tgz/$chicken_targz
    cd chicken-${CHICKEN_VERSION}
    # make PLATFORM=linux PREFIX=$PREFIX spotless
    make PLATFORM=linux PREFIX=$PREFIX
    make PLATFORM=linux PREFIX=$PREFIX install
    cd $BUILDHOME
fi

# Some eggs are quoted since they are reserved to Bash
# for f in matchable readline apropos base64 regex-literals format "regex-case" "test" coops trace csv dot-locking posix-utils posix-extras directory-utils hostinfo tcp rpc csv-xml fmt json md5; do
$CHICKEN_INSTALL $PROX -keep-installed matchable readline apropos base64 regex-literals format "regex-case" "test" coops trace csv dot-locking posix-utils posix-extras directory-utils hostinfo tcp rpc csv-xml fmt json md5 awful http-client spiffy uri-common intarweb http-client spiffy-request-vars spiffy-directory-listing ssax sxml-serializer sxml-modifications logpro
#   if ! [[ -e $PREFIX/lib/chicken/6/$f.so ]];then
#     $CHICKEN_INSTALL $PROX $f
#     # $CHICKEN_INSTALL -deploy -prefix $DEPLOYTARG $PROX $f
#   else
#     echo Skipping install of egg $f as it is already installed
#   fi
# done

cd $BUILDHOME

for a in `ls */*.meta|cut -f1 -d/` ; do 
    echo $a
    (cd $a;$CHICKEN_INSTALL)
done

export LIBPATH=$PREFIX/lib$ADDITIONAL_LIBPATH
export LD_LIBRARY_PATH=$LIBPATH

export SQLITE3_VERSION=3071401
echo Install sqlite3
sqlite3_tgz=sqlite-autoconf-$SQLITE3_VERSION.tar.gz
if ! [[ -e tgz/$sqlite3_tgz ]]; then
    wget http://www.sqlite.org/$sqlite3_tgz
    mv $sqlite3_tgz tgz
fi

if ! [[ -e $PREFIX/bin/sqlite3 ]] ; then
    if [[ -e tgz/sqlite-autoconf-$SQLITE3_VERSION.tar.gz ]]; then
	tar xfz tgz/sqlite-autoconf-$SQLITE3_VERSION.tar.gz 
	(cd sqlite-autoconf-$SQLITE3_VERSION;./configure --prefix=$PREFIX;make;make install)
	# CSC_OPTIONS="-I$PREFIX/include -L$PREFIX/lib" $CHICKEN_INSTALL -prefix $DEPLOYTARG -deploy $PROX sqlite3
	CSC_OPTIONS="-I$PREFIX/include -L$PREFIX/lib" $CHICKEN_INSTALL $PROX sqlite3
    fi
fi

# $CHICKEN_INSTALL $PROX sqlite3

# IUP versions
if [[ x$USEOLDIUP == "x" ]];then
  CDVER=5.7
  IUPVER=3.8
  IMVER=3.8
else
  CDVER=5.7
  IUPVER=3.8
  IMVER=3.8
fi

if [[ `uname -a | grep x86_64` == "" ]]; then 
    export ARCHSIZE=''
else
    export ARCHSIZE=64_
fi
    # export files="cd-5.4.1_Linux${KTYPE}_lib.tar.gz im-3.6.3_Linux${KTYPE}_lib.tar.gz iup-3.5_Linux${KTYPE}_lib.tar.gz"
if [[ x$USEOLDIUP == "x" ]];then
   export files="cd-${CDVER}_Linux${KTYPE}_${ARCHSIZE}lib.tar.gz im-${IMVER}_Linux${KTYPE}_${ARCHSIZE}lib.tar.gz iup-${IUPVER}_Linux${KTYPE}_${ARCHSIZE}lib.tar.gz"
else
   echo WARNING: Using old IUP libraries
   export files="cd-5.4.1_Linux${KTYPE}_${ARCHSIZE}lib.tar.gz im-3.6.3_Linux${KTYPE}_${ARCHSIZE}lib.tar.gz iup-3.5_Linux${KTYPE}_${ARCHSIZE}lib.tar.gz"
fi

mkdir -p $PREFIX/iuplib
for a in `echo $files` ; do
    if ! [[ -e tgz/$a ]] ; then
	wget http://www.kiatoa.com/matt/iup/$a
        mv $a tgz/$a
    fi
    echo Untarring tgz/$a into $BUILDHOME/lib
    (cd $PREFIX/lib;tar xfvz $BUILDHOME/tgz/$a;mv include/* ../include)
    # (cd $DEPLOYTARG;tar xfvz $BUILDHOME/$a)
done

# ffcall obtained from:
# cvs -z3 -d:pserver:anonymous@cvs.savannah.gnu.org:/sources/libffcall co ffcall 

if ! [[ -e tgz/ffcall.tar.gz ]] ; then
    wget http://www.kiatoa.com/matt/iup/ffcall.tar.gz 
    mv ffcall.tar.gz tgz
fi

tar xfvz tgz/ffcall.tar.gz

cd ffcall
./configure --prefix=$PREFIX --enable-shared
make
make install


cd $BUILDHOME
export CSCLIBS=`echo $LD_LIBRARY_PATH | sed 's/:/ -L/g'`
CSC_OPTIONS="-I$PREFIX/include -L$CSCLIBS" $CHICKEN_INSTALL $PROX -D no-library-checks -feature disable-iup-web iup
# CSC_OPTIONS="-I$PREFIX/include -L$CSCLIBS" $CHICKEN_INSTALL $PROX -D no-library-checks -feature disable-iup-web -deploy -prefix $DEPLOYTARG iup
# iup:1.0.2 
CSC_OPTIONS="-I$PREFIX/include -L$CSCLIBS" $CHICKEN_INSTALL $PROX -D no-library-checks canvas-draw
# CSC_OPTIONS="-I$PREFIX/include -L$CSCLIBS" $CHICKEN_INSTALL $PROX -D no-library-checks -deploy -prefix $DEPLOYTARG canvas-draw

# NB// Removed bunch of zmq compiling tricks. Look at older versions of this file if you need to recreate...

cd $BUILDHOME  

# git clone https://bitbucket.org/DerGuteMoritz/zmq/commits/branch/3.2 zmq-3.2
# cd zmq-3.2
# chicken-install
#
# cd $BUILDHOME

## WEBKIT=WebKit-r131972
## if  ! [[ -e ${WEBKIT}.tar.bz2 ]] ; then
##    #    http://builds.nightly.webkit.org/files/trunk/src/WebKit-r131972.tar.bz2
##    wget http://builds.nightly.webkit.org/files/trunk/src/${WEBKIT}.tar.bz2
## fi
## 
## if [[ x$only_it_worked == $I_wish ]] ;then
##    if [[ -e ${WEBKIT}.tar.bz2 ]] ; then
##       tar xfj ${WEBKIT}.tar.bz2
##       cd $WEBKIT
##       ./autogen.sh
##       ./configure --prefix=$PREFIX
##       make
##       make install
##    fi
## fi
## 
## cd $BUILHOME

# export CD_REL=d704525ebe1c6d08
# if ! [[ -e  Canvas_Draw-$CD_REL.zip ]]; then
#     wget http://www.kiatoa.com/matt/iup/Canvas_Draw-$CD_REL.zip
# fi
# 
# unzip -o Canvas_Draw-$CD_REL.zip
# 
# cd "Canvas Draw-$CD_REL/chicken"
# CSC_OPTIONS="-I$PREFIX/include -L$LIBPATH" $CHICKEN_INSTALL $PROX -D no-library-checks

echo You may need to add $LD_LIBRARY_PATH to your LD_LIBRARY_PATH variable, a setup-chicken4x.sh 
echo file can be found in the current directory which should work for setting up to run chicken4x

echo Testing iup
$PREFIX/bin/csi -b -eval '(use iup)(print "Success")'

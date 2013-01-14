#! /bin/env bash

set -x

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
echo sudo apt-get install libwebkitgtk-dev 
echo sudo apt-get install libmotif3 -OR- set KTYPE=26g4
echo KTYPE can be 26, 26g4, or 32
echo KTYPE=$KTYPE
echo You are using PREFIX=$PREFIX
echo You are using proxy="$proxy"
echo 
echo "Set additional_libpath to help find gtk or other libraries, don't forget a leading :"
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

export CHICKEN_VERSION=4.8.0
if ! [[ -e chicken-${CHICKEN_VERSION}.tar.gz ]]; then 
    wget http://code.call-cc.org/releases/${CHICKEN_VERSION}/chicken-${CHICKEN_VERSION}.tar.gz
fi 

BUILDHOME=$PWD
DEPLOYTARG=$BUILDHOME/deploy

if [[ $PREFIX == "" ]]; then
   PREFIX=$PWD/inst
fi

export PATH=$PREFIX/bin:$PATH
export LIBPATH=$PREFIX/lib$ADDITIONAL_LIBPATH
export LD_LIBRARY_PATH=$LIBPATH
echo "export PATH=$PREFIX/bin:\$PATH" > setup-chicken4x.sh
echo "export LD_LIBRARY_PATH=$LD_LIBRARY_PATH" >> setup-chicken4x.sh

echo PATH=$PATH
echo LD_LIBRARY_PATH=$LD_LIBRARY_PATH

if ! [[ -e $PREFIX/bin/csi ]]; then
    tar xfvz chicken-${CHICKEN_VERSION}.tar.gz
    cd chicken-${CHICKEN_VERSION}
    make PLATFORM=linux PREFIX=$PREFIX
    make PLATFORM=linux PREFIX=$PREFIX install
    cd $BUILDHOME
fi

# Some eggs are quoted since they are reserved to Bash
for f in matchable readline apropos base64 regex-literals format "regex-case" "test" coops trace csv dot-locking posix-utils posix-extras directory-utils hostinfo tcp rpc csv-xml fmt json md5 spiffy http-client; do
  if ! [[ -e $PREFIX/lib/chicken/6/$f.so ]];then
    chicken-install $PROX $f
    # chicken-install -deploy -prefix $DEPLOYTARG $PROX $f
  else
    echo Skipping install of egg $f as it is already installed
  fi
done

cd $BUILDHOME

for a in `ls */*.meta|cut -f1 -d/` ; do 
    echo $a
    (cd $a;chicken-install)
done

export LIBPATH=$PREFIX/lib$ADDITIONAL_LIBPATH
export LD_LIBRARY_PATH$=$LIBPATH

export SQLITE3_VERSION=3071401
echo Install sqlite3
if ! [[ -e sqlite-autoconf-$SQLITE3_VERSION.tar.gz ]]; then
    wget http://www.sqlite.org/sqlite-autoconf-$SQLITE3_VERSION.tar.gz
fi

if ! [[ -e $PREFIX/bin/sqlite3 ]] ; then
    if [[ -e sqlite-autoconf-$SQLITE3_VERSION.tar.gz ]]; then
	tar xfz sqlite-autoconf-$SQLITE3_VERSION.tar.gz 
	(cd sqlite-autoconf-$SQLITE3_VERSION;./configure --prefix=$PREFIX;make;make install)
	# CSC_OPTIONS="-I$PREFIX/include -L$PREFIX/lib" chicken-install -prefix $DEPLOYTARG -deploy $PROX sqlite3
	CSC_OPTIONS="-I$PREFIX/include -L$PREFIX/lib" chicken-install $PROX sqlite3
    fi
fi

# chicken-install $PROX sqlite3

if [[ `uname -a | grep x86_64` == "" ]]; then 
    export ARCHSIZE=''
else
    export ARCHSIZE=64_
fi
    # export files="cd-5.4.1_Linux${KTYPE}_lib.tar.gz im-3.6.3_Linux${KTYPE}_lib.tar.gz iup-3.5_Linux${KTYPE}_lib.tar.gz"
if [[ x$USEOLDIUP == "x" ]];then
   export files="cd-5.5.1_Linux${KTYPE}_${ARCHSIZE}lib.tar.gz im-3.8_Linux${KTYPE}_${ARCHSIZE}lib.tar.gz iup-3.6_Linux${KTYPE}_${ARCHSIZE}lib.tar.gz"
else
   echo WARNING: Using old IUP libraries
   export files="cd-5.4.1_Linux${KTYPE}_${ARCHSIZE}lib.tar.gz im-3.6.3_Linux${KTYPE}_${ARCHSIZE}lib.tar.gz iup-3.5_Linux${KTYPE}_${ARCHSIZE}lib.tar.gz"
fi

mkdir -p $PREFIX/iuplib
for a in `echo $files` ; do
    if ! [[ -e $a ]] ; then
	wget http://www.kiatoa.com/matt/iup/$a
    fi
    echo Untarring $a into $BUILDHOME/lib
    (cd $PREFIX/lib;tar xfvz $BUILDHOME/$a;mv include/* ../include)
    # (cd $DEPLOYTARG;tar xfvz $BUILDHOME/$a)
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
export CSCLIBS=`echo $LD_LIBRARY_PATH | sed 's/:/ -L/g'`
CSC_OPTIONS="-I$PREFIX/include -L$CSCLIBS" chicken-install $PROX -D no-library-checks -feature disable-iup-web iup
# CSC_OPTIONS="-I$PREFIX/include -L$CSCLIBS" chicken-install $PROX -D no-library-checks -feature disable-iup-web -deploy -prefix $DEPLOYTARG iup
# iup:1.0.2 
CSC_OPTIONS="-I$PREFIX/include -L$CSCLIBS" chicken-install $PROX -D no-library-checks canvas-draw
# CSC_OPTIONS="-I$PREFIX/include -L$CSCLIBS" chicken-install $PROX -D no-library-checks -deploy -prefix $DEPLOYTARG canvas-draw

# wget http://www.kernel.org/pub/linux/utils/util-linux/v2.22/util-linux-2.22.tar.gz
UTIL_LINUX=2.21
# UTIL_LINUX=2.20.1
if ! [[ -e util-linux-${UTIL_LINUX}.tar.gz ]] ; then
    # wget http://www.kiatoa.com/matt/util-linux-2.20.1.tar.gz
    wget http://www.kernel.org/pub/linux/utils/util-linux/v${UTIL_LINUX}/util-linux-${UTIL_LINUX}.tar.gz
fi

if [[ -e util-linux-${UTIL_LINUX}.tar.gz ]] ; then
    tar xfz util-linux-${UTIL_LINUX}.tar.gz
    cd util-linux-${UTIL_LINUX}
    mkdir -p build
    cd build
    if [[ $UTIL_LINUX = "2.22" ]] ; then
    ../configure --prefix=$PREFIX \
--enable-shared                   \
--disable-use-tty-group		  \
--disable-makeinstall-chown       \
--disable-makeinstall-setuid      \
--disable-libtool-lock		  \
--disable-login			  \
--disable-sulogin		  \
--disable-su			  \
--disable-schedutils		  \
--disable-libmount		  \
--disable-mount			  \
--disable-losetup		  \
--disable-fsck			  \
--disable-partx			  \
--disable-mountpoint		  \
--disable-fallocate		  \
--disable-unshare		  \
--disable-eject			  \
--disable-agetty		  \
--disable-cramfs		  \
--disable-switch_root		  \
--disable-pivot_root		  \
--disable-kill			  \
--disable-libblkid		  \
--disable-utmpdump		  \
--disable-rename		  \
--disable-chsh-only-listed	  \
--disable-wall			  \
--disable-pg-bell		  \
--disable-require-password	  \
--disable-libtool-lock		  \
--disable-nls			  \
--disable-dmesg                   \
--without-ncurses                 
    else
      ../configure --prefix=$PREFIX \
  --enable-shared         \
  --disable-mount         \
  --disable-fsck          \
  --disable-partx         \
  --disable-largefile     \
  --disable-tls           \
  --disable-libmount      \
  --disable-mountpoint    \
  --disable-nls           \
  --disable-rpath         \
  --disable-agetty        \
  --disable-cramfs        \
  --disable-switch_root   \
  --disable-pivot_root    \
  --disable-fallocate     \
  --disable-unshare       \
  --disable-rename        \
  --disable-schedutils    \
  --disable-libblkid      \
  --disable-wall CFLAGS='-fPIC'

#  --disable-makeinstall-chown \
#  --disable-makeinstall-setuid \

#   --disable-chsh-only-listed
#   --disable-pg-bell       let pg not ring the bell on invalid keys
#   --disable-require-password
#   --disable-use-tty-group do not install wall and write setgid tty
#   --disable-makeinstall-chown
#   --disable-makeinstall-setuid
    fi
    
    (cd libuuid;make install)
    # make
    # make install
    cp $PREFIX/include/uuid/uuid.h $PREFIX/include/uuid.h
fi


cd $BUILDHOME

# http://download.zeromq.org/zeromq-3.2.1-rc2.tar.gz
# zpatchlev=-rc2
# http://download.zeromq.org/zeromq-2.2.0.tar.gz
ZEROMQ=zeromq-2.2.0
# ZEROMQ=zeromq-3.2.1
if ! [[ -e ${ZEROMQ}${zpatchlev}.tar.gz ]] ; then
    wget http://download.zeromq.org/${ZEROMQ}${zpatchlev}.tar.gz
fi

if [[ -e ${ZEROMQ}${zpatchlev}.tar.gz ]] ; then
    tar xfz ${ZEROMQ}.tar.gz
    cd ${ZEROMQ}
    ln -s $PREFIX/include/uuid src
    # LDFLAGS=-L$PREFIX/lib ./configure --prefix=$PREFIX 
    
    ./configure --enable-static --prefix=$PREFIX --with-uuid=$PREFIX LDFLAGS="-L$PREFIX/lib" CPPFLAGS="-fPIC -I$PREFIX/include" LIBS="-lgcc"
    # --disable-shared CPPFLAGS="-fPIC 
    # LDFLAGS="-L/usr/lib64 -L$PREFIX/lib" ./configure --enable-static --prefix=$PREFIX 
    make
    make install
    CSC_OPTIONS="-I$PREFIX/include -L$CSCLIBS" chicken-install $PROX zmq
    # CSC_OPTIONS="-I$PREFIX/include -L$CSCLIBS" chicken-install $PROX -deploy -prefix $DEPLOYTARG zmq
fi

cd $BUILDHOME  

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
# CSC_OPTIONS="-I$PREFIX/include -L$LIBPATH" chicken-install $PROX -D no-library-checks

echo You may need to add $LD_LIBRARY_PATH to your LD_LIBRARY_PATH variable, a setup-chicken4x.sh 
echo file can be found in the current directory which should work for setting up to run chicken4x

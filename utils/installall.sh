#! /usr/bin/env bash

# This file installs prerequisites for megatest (chicken, eggs, etc.)
# Before running this script, set PREFIX environment variable
# to chicken install target area.  /opt/chicken is a typical value
# set -x

# Copyright 2007-2014, Matthew Welland.
# 
#  This program is made available under the GNU GPL version 2.0 or
#  greater. See the accompanying file COPYING for details.
# 
#  This program is distributed WITHOUT ANY WARRANTY; without even the
#  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
#  PURPOSE.

if [[ $OPTION=="" ]]; then
    export OPTION=std
fi

echo You may need to do the following first:
echo sudo apt install libreadline-dev
echo sudo apt install libwebkitgtk-dev 
echo sudo apt install libpangox-1.0-0 zlib1g-dev libfreetype6-dev cmake
echo sudo apt install libssl-dev  uuid-dev
echo sudo apt install libmotif3 -OR- set KTYPE=26g4
echo sudo apt install cmake
echo
echo Set OPTION to std, currently OPTION=$OPTION
echo
echo Additionally, if you want mysql-client, you will need to make sure
echo mysql_config is in your path
echo for postgres to install dbi libpq-dev
echo
echo You are using PREFIX=$PREFIX
echo You are using proxy="$proxy"
echo 
echo "Set additional_libpath to help find gtk or other libraries, don't forget a leading :"

SYSTEM_TYPE=$(lsb_release -irs |tr ' ' '_' |tr '\n' '-')$(uname -i)-$OPTION

CHICKEN_VERSION=4.10.0
CHICKEN_BASEVER=4.10.0

# Set up variables
#
case $SYSTEM_TYPE in
Ubuntu-17.04-x86_64-std)
	KTYPE=32
	CDVER=5.11.1
	IUPVER=3.22
	IMVER=3.12
	CHICKEN_VERSION=4.12.0
	CHICKEN_BASEVER=4.12.0
	;;
Ubuntu-16.04-x86_64-std)
	KTYPE=32
	CDVER=5.11.1
	IUPVER=3.22
	IMVER=3.12
	CHICKEN_VERSION=4.12.0
	CHICKEN_BASEVER=4.12.0
	;;
Ubuntu-16.04-i686-std)
	KTYPE=32
	CDVER=5.11.1
	IUPVER=3.22
	IMVER=3.12
        CHICKEN_VERSION=4.12.0
        CHICKEN_BASEVER=4.12.0
	;;
SUSE_LINUX_11-x86_64-std)
	KTYPE=26g4 
	CDVER=5.11.1
	IUPVER=3.22
	IMVER=3.12
  >> $PREFIX/setup-chicken4x.sh
echo "export CHICKEN_DOC_PAGER=cat" >> $PREFIX/setup-chicken4x.sh

echo "setenv PATH $PREFIX/bin:\$PATH" > $PREFIX/setup-chicken4x.csh
echo "setenv LD_LIBRARY_PATH $LD_LIBRARY_PATH:\$LD_LIBRARY_PATH" >> $PREFIX/setup-chicken4x.csh
echo "setenv CHICKEN_DOC_PAGER cat" >> $PREFIX/setup-chicken4x.csh

echo PATH=$PATH
echo LD_LIBRARY_PATH=$LD_LIBRARY_PATH

if ! [[ -e $PREFIX/bin/csi ]]; then
    tar xfz tgz/$chicken_targz
    cd chicken-${CHICKEN_VERSION}
    # make PLATFORM=linux PREFIX=$PREFIX spotless
    make PLATFORM=linux PREFIX=$PREFIX
    make PLATFORM=linux PREFIX=$PREFIX install
    cd $BUILDHOME
fi
cd $BUILDHOME
#if [[ ! -e 1.0.0.tar.gz ]];then
#  wget --no-check-certificate https://github.com/nanomsg/nanomsg/archive/1.0.0.tar.gz 
#  mv 1.0.0 1.0.0.tar.gz
#fi
if ! [[ -e $PREFIX/lib64/libnanomsg.so.1.0.0 ]]; then
        wget --no-check-certificate https://github.com/nanomsg/nanomsg/archive/1.0.0.tar.gz 
        mv 1.0.0 1.0.0.tar.gz
	tar xf 1.0.0.tar.gz 
	cd nanomsg-1.0.0
	./configure --prefix=$PREFIX
	make
	make install
	CSC_OPTIONS="-I$PREFIX/include -L$PREFIX/lib" $CHICKEN_INSTALL $PROX nanomsg
fi
cd $BUILDHOME

export SQLITE3_VERSION=3090200
if ! [[ -e $PREFIX/bin/sqlite3 ]]; then
	echo Install sqlite3
	sqlite3_tgz=sqlite-autoconf-$SQLITE3_VERSION.tar.gz
	if ! [[ -e tgz/$sqlite3_tgz ]]; then
	    wget http://www.sqlite.org/2015/$sqlite3_tgz
	    mv $sqlite3_tgz tgz
	fi

	if ! [[ -e $PREFIX/bin/sqlite3 ]] ; then
	    if [[ -e tgz/sqlite-autoconf-$SQLITE3_VERSION.tar.gz ]]; then
		tar xfz tgz/sqlite-autoconf-$SQLITE3_VERSION.tar.gz 
		(cd sqlite-autoconf-$SQLITE3_VERSION;./configure --prefix=$PREFIX;make;make install)
	    fi
	fi
fi
if ! [[ -e $PREFIX/bin/pg_config ]]; then
	echo Install Postgresql
	pgsql_tgz=postgresql-9.6.4.tar.gz
	if ! [[ -e tgz/$pgsql_tgz ]]; then
	  wget -c https://ftp.postgresql.org/pub/source/v9.6.4/$pgsql_tgz
	  mv $pgsql_tgz tgz
	fi
	if ! [[ -e $PREFIX/bin/pg_config ]]; then
	  if [[ -e tgz/$pgsql_tgz ]]; then
	    tar xfz tgz/$pgsql_tgz
	    (cd postgresql-9.6.4; ./configure --prefix=$PREFIX --with-openssl; make; make install)
	  fi
	fi
fi


cd $BUILDHOME
for egg in "sqlite3" sql-de-lite nanomsg
do
	echo "Installing $egg"
	CSC_OPTIONS="-I$PREFIX/include -L$PREFIX/lib -L$PREFIX/lib64"  $CHICKEN_INSTALL $PROX -keep-installed $egg
	#CSC_OPTIONS="-I$PREFIX/include -L$PREFIX/lib -L$PREFIX/lib64"  $CHICKEN_INSTALL $PROX $egg
	if [ $? -ne 0 ]; then
		echo "$egg failed to install"
		exit 1
	fi
done

# Some eggs are quoted since they are reserved to Bash
# for f in matchable readline apropos base64 regex-literals format "regex-case" "test" coops trace csv dot-locking posix-utils posix-extras directory-utils hostinfo tcp rpc csv-xml fmt json md5; do
# $CHICKEN_INSTALL $PROX -keep-installed matchable readline apropos base64 regex-literals format "regex-case" "test" coops trace csv dot-locking posix-utils posix-extras directory-utils hostinfo tcp rpc csv-xml fmt json md5 awful http-client spiffy uri-common intarweb http-client spiffy-request-vars md5 message-digest http-client spiffy-directory-listing
for egg in matchable readline apropos base64 regex-literals format "regex-case" "test" \
	coops trace csv dot-locking posix-utils posix-extras directory-utils hostinfo \
	tcp rpc csv-xml fmt json md5 awful http-client:0.7.1 spiffy uri-common intarweb http-client \
	spiffy-request-vars s md5 message-digest spiffy-directory-listing ssax sxml-serializer \
	sxml-modifications logpro z3 call-with-environment-variables \
	pathname-expand typed-records simple-exceptions numbers crypt parley srfi-42 \
	alist-lib ansi-escape-sequences args basic-sequences bindings chicken-doc chicken-doc-cmd \
	cock condition-utils debug define-record-and-printer easyffi easyffi-base \
	expand-full ezxdisp filepath foof-loop ini-file irc lalr lazy-seq \
	locale locale-builtin locale-categories locale-components locale-current locale-posix \
	locale-timezone loops low-level-macros procedural-macros refdb rfc3339 scsh-process \
	sexp-diff sha1 shell slice srfi-101 srfi-19 srfi-19-core srfi-19-date srfi-19-io \
	srfi-19-period srfi-19-support srfi-19-time srfi-19-timezone srfi-29 srfi-37 srfi-78 syslog \
	udp uuid uuid-lib zlib postgresql

do
	echo "Installing $egg"
	$CHICKEN_INSTALL $PROX -keep-installed $egg
	#$CHICKEN_INSTALL $PROX $egg
	if [ $? -ne 0 ]; then
		echo "$egg failed to install"
		exit 1
	fi
done
if [[ -e `which mysql_config` ]]; then
  $CHICKEN_INSTALL $PROX mysql-client
fi

cd $BUILDHOME
cd `$PREFIX/bin/csi -p '(chicken-home)'`
curl http://3e8.org/pub/chicken-doc/chicken-doc-repo.tgz | tar zx
cd $BUILDHOME

# $CHICKEN_INSTALL $PROX sqlite3
cd $BUILDHOME

if [[ `uname -a | grep x86_64` == "" ]]; then 
    export ARCHSIZE=''
else
    export ARCHSIZE=64_
fi
    # export files="cd-5.4.1_Linux${KTYPE}_lib.tar.gz im-3.6.3_Linux${KTYPE}_lib.tar.gz iup-3.5_Linux${KTYPE}_lib.tar.gz"
if [[ x$USEOLDIUP == "x" ]];then
   export files="cd/cd-${CDVER}_Linux${KTYPE}_${ARCHSIZE}lib.tar.gz im/im-${IMVER}_Linux${KTYPE}_${ARCHSIZE}lib.tar.gz iup/iup-${IUPVER}_Linux${KTYPE}_${ARCHSIZE}lib.tar.gz"
else
   echo WARNING: Using old IUP libraries
   export files="cd/cd-5.4.1_Linux${KTYPE}_${ARCHSIZE}lib.tar.gz im/im-3.6.3_Linux${KTYPE}_${ARCHSIZE}lib.tar.gz iup/iup-3.5_Linux${KTYPE}_${ARCHSIZE}lib.tar.gz"
fi
echo $files

mkdir -p $PREFIX/iuplib
mkdir -p iup/
for a in `echo $files` ; do
    if ! [[ -e tgz/$a ]] ; then
        echo wget -c -O tgz/$a http://www.kiatoa.com/matt/chicken-build/$a
	wget -c http://www.kiatoa.com/matt/chicken-build/$a
        mv `echo $a | cut -d'/' -f2` tgz/
    fi
    echo Untarring tgz/$a into $BUILDHOME/lib
    tar -xzf tgz/`echo $a | cut -d'/' -f2` -C iup/
    #(cd $PREFIX/lib;tar xfvz $BUILDHOME/tgz/$a;mv include/* ../include)
    # (cd $DEPLOYTARG;tar xfvz $BUILDHOME/$a)
done
cp iup/include/* $PREFIX/include/
cp iup/*.so $PREFIX/lib/
cp iup/*.a $PREFIX/lib/
cp iup/ftgl/lib/*/* $PREFIX/lib/
cd $BUILDHOME
# ffcall obtained from:
# cvs -z3 -d:pserver:anonymous@cvs.savannah.gnu.org:/sources/libffcall co ffcall 
#exit
if ! [[ -e $PREFIX/include/callback.h ]] ; then
	#fossil clone http://www.kiatoa.com/fossils/ffcall ffcall.fossil
	wget -c -O ffcall.tar.gz 'http://www.kiatoa.com/fossils/ffcall/tarball?name=ffcall&uuid=trunk'
	tar -xzf ffcall.tar.gz
	#mkdir -p ffcall
	cd ffcall
	#fossil open ../ffcall.fossil
	./configure --prefix=$PREFIX --enable-shared
	make CC="gcc -fPIC"
	make install
fi
cd $BUILDHOME
#wget -c -O opensrc.tar.gz 'http://www.kiatoa.com/fossils/opensrc/tarball?name=opensrc&uuid=trunk'
# Not working due to login problems.
if ! [[ -e $PREFIX/bin/hs ]] ; then
	#fossil clone http://www.kiatoa.com/fossils/opensrc opensrc.fossil
	#mkdir -p opensrc
	wget -c -O opensrc.tar.gz 'http://www.kiatoa.com/fossils/opensrc/tarball?name=opensrc&uuid=trunk'
	tar -xzf opensrc.tar.gz
	cd opensrc
	#fossil open ../opensrc.fossil
	cd histstore
	$PREFIX/bin/csc histstore.scm -o hs 
	cp -f hs $PREFIX/bin/hs 
	cd ../mutils
	$PREFIX/bin/chicken-install
	cd ../dbi 
	$PREFIX/bin/chicken-install
	cd ../margs
	$PREFIX/bin/chicken-install
	cd ../pkts
	$PREFIX/bin/chicken-install
fi
cd $BUILDHOME

if ! [[ -e $PREFIX/bin/stmlrun ]] ; then
	#fossil clone http://www.kiatoa.com/fossils/stml stml.fossil
	wget -c -O stml.tar.gz 'http://www.kiatoa.com/fossils/stml/tarball?name=stml&uuid=trunk'
	tar -xzf stml.tar.gz
	cd stml
	#fossil open ../stml.fossil
	cp install.cfg.template install.cfg
	echo "TARGDIR=$PREFIX/bin" > install.cfg
	echo "LOGDIR=/tmp/stmlrun" >> install.cfg
	echo "SQLITE3=$PREFIX/bin/sqlite3" >> install.cfg
	cp requirements.scm.template requirements.scm
	which csc
	make clean
	CSCOPTS="-C -fPIC" make
fi

cd $BUILDHOME
export CSCLIBS=`echo $LD_LIBRARY_PATH | sed 's/:/ -L/g'`
IUPEGGVER='iup'
if [[ $IUPVER == "3.5" ]]; then
  IUPEGGVER='iup:1.2.1'
fi

#CSC_OPTIONS="-I$PREFIX/include -L$CSCLIBS" $CHICKEN_INSTALL $PROX -D no-library-checks -feature disable-iup-web iup
CSC_OPTIONS="-I$PREFIX/include -L$PREFIX/lib" $CHICKEN_INSTALL $PROX -D no-library-checks -feature disable-iup-web -feature disable-iup-pplot $IUPEGGVER

# CSC_OPTIONS="-I$PREFIX/include -L$CSCLIBS" $CHICKEN_INSTALL $PROX -D no-library-checks -feature disable-iup-web -deploy -prefix $DEPLOYTARG iup
# iup:1.0.2 
CSC_OPTIONS="-I$PREFIX/include -L$PREFIX/lib" $CHICKEN_INSTALL $PROX -D no-library-checks canvas-draw
# CSC_OPTIONS="-I$PREFIX/include -L$CSCLIBS" $CHICKEN_INSTALL $PROX -D no-library-checks -deploy -prefix $DEPLOYTARG canvas-draw

cd $BUILDHOME  

# install ducttape
if [[ -e ../ducttape ]];then
  cd ../ducttape
  $CHICKEN_INSTALL
else
  echo "ducttape egg not found at ../ducttape. You will need to cd into the ducttape directory in the megatest distribution and run \"chicken-install\""
fi

cd $BUILDHOME
echo You may need to add $LD_LIBRARY_PATH to your LD_LIBRARY_PATH variable, a setup-chicken4x.sh 
echo file can be found in the current directory which should work for setting up to run chicken4x


echo Testing iup
$PREFIX/bin/csi -b -eval '(use iup)(print "Success")'



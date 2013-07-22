#!/usr/bin/env bash

# Run your step here

source $PREFIX/buildsetup.sh

echo Install sqlite3
if ! [[ -e sqlite-autoconf-${SQLITE3_VERSION}.tar.gz ]]; then
    wget http://www.sqlite.org/sqlite-autoconf-${SQLITE3_VERSION}.tar.gz
fi

tar xfz sqlite-autoconf-${SQLITE3_VERSION}.tar.gz 

ls -l sqlite-autoconf-${SQLITE3_VERSION}.tar.gz

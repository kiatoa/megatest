#!/usr/bin/env bash

# Run your step here

source $PREFIX/buildsetup.sh

echo Install sqlite3
if ! [[ -e ${DOWNLOADS}/sqlite-autoconf-${SQLITE3_VERSION}.tar.gz ]]; then
    (cd ${DOWNLOADS};wget http://www.sqlite.org/sqlite-autoconf-${SQLITE3_VERSION}.tar.gz)
fi

tar xfz ${DOWNLOADS}/sqlite-autoconf-${SQLITE3_VERSION}.tar.gz 

ls -l sqlite-autoconf-${SQLITE3_VERSION}.tar.gz

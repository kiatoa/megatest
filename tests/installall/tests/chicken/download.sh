#!/usr/bin/env bash

# Run your step here

source $PREFIX/buildsetup.sh

if ! [[ -e ${DOWNLOADS}/chicken-${CHICKEN_VERSION}.tar.gz ]]; then 
    (cd ${DOWNLOADS};wget http://code.call-cc.org/releases/${CHICKEN_VERSION}/chicken-${CHICKEN_VERSION}.tar.gz)
fi 

ls -l ${DOWNLOADS}/chicken-${CHICKEN_VERSION}.tar.gz

tar xfvz ${DOWNLOADS}/chicken-${CHICKEN_VERSION}.tar.gz

ls -l chicken-${CHICKEN_VERSION}

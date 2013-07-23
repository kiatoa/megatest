#!/usr/bin/env bash

# Run your step here

source $PREFIX/buildsetup.sh

if ! [[ -e ${DOWNLOADS}/chicken-${CHICKEN_VERSION}.tar.gz ]]; then 
  if [ "${CHICKEN_URL}" == "" ]; then
     (cd ${DOWNLOADS};wget http://code.call-cc.org/releases/${CHICKEN_VERSION}/chicken-${CHICKEN_VERSION}.tar.gz)
  else
     (cd ${DOWNLOADS};wget ${CHICKEN_URL})
  fi
fi 

ls -l ${DOWNLOADS}/chicken-${CHICKEN_VERSION}.tar.gz

tar xfvz ${DOWNLOADS}/chicken-${CHICKEN_VERSION}.tar.gz

ls -l chicken-${CHICKEN_VERSION}

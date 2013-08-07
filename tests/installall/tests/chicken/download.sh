#!/usr/bin/env bash

# Run your step here

source $PREFIX/buildsetup.sh

if [ ! -e ${DOWNLOADS}/chicken-${CHICKEN_VERSION}.tar.gz ]; then 
  if [ "${CHICKEN_URL}" == "" ]; then
     CHICKEN_URL=http://code.call-cc.org/releases/${CHICKEN_VERSION}/chicken-${CHICKEN_VERSION}.tar.gz
  fi
  echo "Downloading $CHICKEN_URL"
  (cd ${DOWNLOADS};wget ${CHICKEN_URL})
fi 

ls -l ${DOWNLOADS}/chicken-${CHICKEN_VERSION}.tar.gz

tar xfvz ${DOWNLOADS}/chicken-${CHICKEN_VERSION}.tar.gz

ls -l chicken-${CHICKEN_VERSION}

#!/usr/bin/env bash

# Run your step here

if ! [[ -e chicken-${CHICKEN_VERSION}.tar.gz ]]; then 
    wget http://code.call-cc.org/releases/${CHICKEN_VERSION}/chicken-${CHICKEN_VERSION}.tar.gz
fi 

ls -l chicken-${CHICKEN_VERSION}.tar.gz

tar xfvz chicken-${CHICKEN_VERSION}.tar.gz

ls -l chicken-${CHICKEN_VERSION}

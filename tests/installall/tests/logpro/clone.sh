#!/usr/bin/env bash

# Run your step here
source $PREFIX/buildsetup.sh

fossil clone http://www.kiatoa.com/fossils/logpro logpro.fossil

mkdir src
cd src 
fossil open ../logpro.fossil --nested
fossil co $LOGPRO_VERSION

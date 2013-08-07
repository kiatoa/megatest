#!/usr/bin/env bash

# Run your step here
source $PREFIX/buildsetup.sh

fossil clone http://www.kiatoa.com/fossils/$FSLPKG $FSLPKG.fossil

mkdir src
cd src 
fossil open ../$FSLPKG.fossil --nested
fossil co ${$FSLPKG}_VERSION}

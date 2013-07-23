#!/usr/bin/env bash

# Run your step here
source $PREFIX/buildsetup.sh

mkdir -p $PREFIX/iuplib
for a in cd-5.6.1_Sources.tar.gz  im-3.8.1_Sources.tar.gz  iup-3.8_Sources.tar.gz lua-5.2.1_Sources.tar.gz; do
    if ! [[ -e ${DOWNLOADS}/$a ]] ; then
	(cd ${DOWNLOADS};wget http://www.kiatoa.com/matt/iup/$a)
    fi
    tar xfvz ${DOWNLOADS}/$a
done

find . -type d -exec chmod ug+x {} \;

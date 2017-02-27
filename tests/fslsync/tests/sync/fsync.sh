#!/usr/bin/env bash

# Get the list of fossils from the cache

FILES=$(ls $FSLSAREA/$AREANAME|grep fossil)

# Do the remote sync from CACHE to FOSSILS
ssh $SITENAME /bin/bash <<EOF
for f in $FILES;do
    FOSSLF=$FSLSAREA/$AREANAME/\$f
    CACHEF=$WORKAREA/$SITENAME/
    if [ ! -e \$FOSSLF ];then
	cp \$CACHEF \$FOSSLF
	chmod ug+rw \$FOSSLF
    elif [ \$CACHEF -nt \$FOSSLF ];then
	fossil pull -R \$FOSSLF \$CACHEF
    fi
done
EOF 

# Do the local sync 
for f in $FILES;do
    FOSSLF=$FSLSAREA/$AREANAME/\$f
    CACHEF=$WORKAREA/$SITENAME/
    if [ ! -e \$FOSSLF ];then
	cp \$CACHEF \$FOSSLF
	chmod ug+rw \$FOSSLF
    elif [ \$CACHEF -nt \$FOSSLF ];then
	fossil pull -R \$FOSSLF \$CACHEF
    fi
done

echo done

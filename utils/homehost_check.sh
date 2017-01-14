#!/bin/sh

#exits 1 when current host is not homehost.

if [[ ! -e .homehost ]]; then
    exit 0
fi

homehostname=$( host `cat .homehost` | awk '{print $NF}' | sed 's/\.$//' )
hostname=$( hostname -f )

if [[ $homehostname == $hostname ]]; then
    exit 0
fi
echo "ERROR: this host ($homehostname) is not the megatest homehost ($hostname)"
exit 1


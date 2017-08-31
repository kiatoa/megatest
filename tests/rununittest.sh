#!/bin/bash

# Usage: rununittest.sh testname debuglevel
#
banner $1

# put megatest on path from correct location
mtbindir=$(readlink -f ../bin)
 
export PATH="${mtbindir}:$PATH"

# Clean setup
#
dbdir=$(echo /tmp/$USER/megatest_localdb/simplerun/.[a-zA-Z]*/)
echo "dbdir=$dbdir"
rm -f simplerun/megatest.db simplerun/monitor.db simplerun/db/monitor.db $dbdir
rm -rf simplelinks/ simpleruns/ simplerun/db/ $dbdir
mkdir -p simplelinks simpleruns
(cd simplerun;cp ../../*_records.scm .;perl -pi.bak -e 's/define-inline/define/' *_records.scm)
(cd simplerun;cp ../../altdb.scm .)

# Run the test $1 is the unit test to run
cd simplerun;echo '(load "../tests.scm")' | ../../bin/megatest -repl -debug $2 $1

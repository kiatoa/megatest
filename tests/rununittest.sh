#!/bin/bash

# Usage: rununittest.sh testname debuglevel
#

# put megatest on path from correct location
mtbindir=$(readlink -f ../bin)

export PATH="${mtbindir}:$PATH"

# Clean setup
#
dbdir=$(cd simplerun;megatest -show-config -section setup -var linktree)/.db
rm -f simplerun/megatest.db simplerun/monitor.db simplerun/db/monitor.db $dbdir/*.db
rm -rf simplelinks/ simpleruns/ simplerun/db/ $dbdir
mkdir -p simplelinks simpleruns
(cd simplerun;cp ../../*_records.scm .;perl -pi.bak -e 's/define-inline/define/' *_records.scm)

# Run the test $1 is the unit test to run
cd simplerun;echo '(load "../tests.scm")' | ../../bin/megatest -repl -debug $2 $1

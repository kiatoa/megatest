#!/bin/bash

# Usage: rununittest.sh testname debuglevel
#

# Clean setup
#
rm -f simplerun/megatest.db simplerun/monitor.db simplerun/db/monitor.db
rm -rf simplelinks/ simpleruns/
mkdir -p simplelinks simpleruns
(cd simplerun;cp ../../*_records.scm .;perl -pi.bak -e 's/define-inline/define/' *_records.scm)

# Run the test $1 is the unit test to run
cd simplerun;echo '(load "../tests.scm")' | ../../bin/megatest -repl -debug $2 $1

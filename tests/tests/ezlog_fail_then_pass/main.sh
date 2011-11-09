#!/bin/bash

megatest -step yepstep :state start :status n/a
ls /tmp
megatest -step yepstep :state end :status $?

megatest -load-test-data << EOF
OPER,du,   1.2,  1.2, <   , GBytes  ,System didn't use too much space
EOF

megatest -test-status :state COMPLETED :status AUTO

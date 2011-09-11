#!/bin/bash

# category variable value expected  tol/comp units comment
$MT_MEGATEST -load-test-data << EOF
foo,       bar,     1.9,  1.8,      >
foo,       rab,   1.0e9,  2e9,      1e9
foo,       bla,     1.2,  1.9,      <
foo,       bal,    -1.1,    0,      <   ,         , Check for overload
foo,       alb,     1.2,  1.2,      <=  ,     Amps, This is the high power circuit test
foo,       abl,     1.2,  1.3,      0.1
foo,       bra,     1.2, pass,      silly stuff
faz,       bar,      10,  8mA,          ,         ,"this is a comment"
EOF

# Needed to force rolling up the results and set the test to COMPLETED
$MT_MEGATEST -test-status :state COMPLETED :status AUTO

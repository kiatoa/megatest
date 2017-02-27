#!/bin/bash

$MT_MEGATEST -test-status :state $THESTATE :status $THESTATUS -setlog "nada.html"

# By exiting with non-zero we tell Megatest to preseve the state and status
exit 1

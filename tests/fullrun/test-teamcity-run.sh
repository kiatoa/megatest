#!/bin/bash

(cd ../..;make install) && RN=tcmt_m;megatest -remove-runs -target ubuntu/nfs/none -runname tcmt_m -testpatt %;\
    tcmt -run -target ubuntu/nfs/none -runname tcmt_m -testpatt % -rerun-clean 2>&1 | tee all.log | grep teamcity | tee teamcity.log


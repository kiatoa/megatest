#!/bin/bash

job_order=$1
job_length=$2

echo "START: $job_order" > $job_order.log
sleep $job_length
echo "END: $job_order" >> $job_order.log


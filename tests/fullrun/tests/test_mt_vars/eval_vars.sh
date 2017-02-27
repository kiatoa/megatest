#!/bin/bash

if env | grep VARWITHDOLLARSIGNS | grep USER;then
    exit 1 # fails!
else
    exit 0 # good!
fi
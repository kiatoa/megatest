#!/usr/bin/env bash

grep -e '^export CURRENT' megatest.sh | grep /tmp/nada

#!/bin/bash

bqueues | grep normal |awk '{print $8}'
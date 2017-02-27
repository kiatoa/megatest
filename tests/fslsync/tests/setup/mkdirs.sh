#!/usr/bin/env bash

# Create needed directories both local and remote

# Remote
ssh $SITENAME mkdir -vp $WORKAREA/$SITENAME/$AREANAME

# Local
mkdir -vp $WORKAREA/$SITENAME/$AREANAME

echo done

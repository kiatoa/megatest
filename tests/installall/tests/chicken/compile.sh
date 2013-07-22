#!/usr/bin/env bash

# Run your step here
cd chicken-${CHICKEN_VERSION}
make PLATFORM=${PLATFORM} PREFIX=${PREFIX}

#!/usr/bin/env bash

# Sync to remote cache
rsync -avz $FSLSAREA/$AREANAME/ $SITENAME:$WORKAREA/$SITENAME/$AREANAME/ &
# Sync to local cache
rsync -avz $SITENAME:$FSLSAREA/$AREANAME/ $WORKAREA/$SITENAME/$AREANAME/ &

# Wait until rsyncs complete
wait

echo done

#!/bin/bash

## Enh :
# 1. if /tmp/repo exists, delte it or name it something else
# 2. compare the repo is successfully created

## Usage :
# unlock_db.sh <database-name/complete path>

function unlock_db () {
    repo=$1
    echo $repo
    ls -lrt $repo
    cp $repo /tmp/${USER}_repo.tmp
    ls -lrt /tmp/${USER}_repo.tmp
    ## Eventually compare the sizes of 2 repos
    cmd=$(rm -r $repo && sqlite3 /tmp/${USER}_repo.tmp .dump | sqlite3 $repo)
    echo $cmd
    ls -lrt $repo
    chmod g+w $repo
}

#======================================================================
# T H E   M A I N   H A N D L E R   A N D   P R O C E S S I N G 
#======================================================================

unlock_db $1

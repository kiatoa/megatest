#!/bin/bash

echo manyservers.sh pid $$

logdir=log-manysrv


function reset {
    rm -f .homehost .server .server.lock links/.db/monitor.db .starting-server
    }

function launch_many_servers {
    # count  = $1
    # logdir = $2
    # prefx  = $3
  perl -e 'foreach my $i (1 ... '$1'){print "'$2'/'$3'-srv-$i.log\n"}' | \
     xargs -P $1 -n 1 megatest -server - -run-id 0 -daemonize -log
}

    
function get_srv_pids {
    ps auwx | grep "mtest -server" | grep $logdir | grep -v grep | awk '{print $2}' 
}


if [[ -e $logdir ]]; then rm -rf $logdir; fi
if [[ ! -e $logdir ]]; then mkdir $logdir; fi

reset

simultaneous_servers=20
server_collision_resolution_delay=15
server_timeout_delay=65

echo "Launching $simultaneous_servers simultaneous servers"
launch_many_servers $simultaneous_servers $logdir "first"
echo "Sleeping $server_collision_resolution_delay seconds to allow new servers to die because one is already running."
sleep $server_collision_resolution_delay

pids=`get_srv_pids`
pids_left=`echo $pids | wc -w`
echo "pids_left=$pids_left"
echo "after $server_collision_resolution_delay seconds: servers remaining=$pids_left; expecting 1"
if [[ $pids_left == 1 ]]; then
    echo "All servers but 1 terminated. Still good."
else
    if [[ $pids_left == 0 ]]; then
        echo "All servers died too soon.  Not good. Aborting."
        echo "TEST FAIL"
        exit 1
    else
        echo "Too many servers left.  Not good.  Aborting."
        echo "TEST FAIL"
        echo $pids | xargs kill
        sleep 5
        pids=`get_srv_pids`
        pids_left=`echo $pids | wc -w`
        if [[ ! ( $pids_left == 0 ) ]]; then
            echo $pids | xargs kill -9
        fi
        exit 1
    fi
fi



echo "launching another volley of $simultaneous_servers.  THey should all perish. right away, leaving the one server running."
launch_many_servers $simultaneous_servers $logdir "second"
sleep $server_collision_resolution_delay

pids=`get_srv_pids`
pids_left=`echo $pids | wc -w`
echo "pids_left=$pids_left"
echo "after $server_collision_resolution_delay seconds: servers remaining=$pids_left; expecting 1"
if [[ $pids_left == 1 ]]; then
    echo "All servers but 1 terminated. So far so good."
else
    if [[ $pids_left == 0 ]]; then
        echo "All servers died too soon.  Not good. Aborting."
        echo "TEST FAIL"
        exit 1
    else
        echo "Too many servers left.  Not good.  Aborting."
        echo "TEST FAIL"
        echo $pids | xargs kill
        sleep 5
        pids=`get_srv_pids`
        pids_left=`echo $pids | wc -w`
        if [[ ! ( $pids_left == 0 ) ]]; then
            echo $pids | xargs kill -9
        fi
        exit 1
    fi
fi



echo "sleeping for awhile ($server_timeout_delay seconds) to let server exit on its own for no-request timeout"
sleep $server_timeout_delay
pids=`get_srv_pids`
pids_left=`echo $pids | wc -w`
echo "after $server_timeout_delay seconds: servers remaining=$pids_left; expecting 0"

if [[ $pids_left == 0 ]]; then
    echo "No servers remain. This is good."
    echo "TEST PASS"
    exit 0
else
    echo "Too many servers left.  Not good.  Aborting."
    echo "TEST FAIL"
    echo $pids | xargs kill
    sleep 5
    pids=`get_srv_pids`
    pids_left=`echo $pids | wc -w`
    if [[ ! ( $pids_left == 0 ) ]]; then
        echo $pids | xargs kill -9
    fi
    exit 1
fi

#!/bin/bash

set -o pipefail
set +e

if [ $# -lt 2 ]
then
    echo "$@"
    echo "Usage: attach-screen.sh [attach | terminate] job-id"
    exit 1
fi

TYPE=${1}; shift
JOBID=${1}; shift

if [ $TYPE == "terminate" ]; then
    echo "Terminating screen"
    screen -X -S $JOBID quit
    exit 1
else
    export TERM=vt100; screen -r $JOBID
    if grep -q 0 /home/blaaulas/tactician/last-status; then
        exit 0
    else
        echo "Errors occurred during benching"
        exit 1
    fi
fi


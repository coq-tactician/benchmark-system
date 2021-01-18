#!/bin/bash

set -o pipefail
set +e

if [ $# -lt 1 ]
then
    echo "Usage: attach-screen.sh job-id"
    exit 1
fi

JOBID=${1}; shift

export TERM=vt100; screen -r $JOBID

if [ -f ~/tactician/builds/$JOBID/error ]; then
    echo "Errors occurred during benching"
    exit 1
else
    exit 0
fi

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

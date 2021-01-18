#!/bin/bash

set -o pipefail
set +e

if [ $# -lt 1 ]
then
    echo "Usage: attach-slurm.sh job-id"
    exit 1
fi

JOBID=${1}; shift

while true
do
    { PENDING=$(sattach $JOBID 2>&1 | tee /dev/fd/3 | grep "Job is pending execution" | wc -l); } 3>&1
    EXIT=$?
    if [ $EXIT -ne 0 ]; then
        if [ $PENDING -gt 0 ]
        then
            sleep 1m
        else
            exit $EXIT
        fi
    else
        exit 0
    fi
done

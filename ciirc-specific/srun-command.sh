#!/bin/bash

set -eEu

if [ $# -lt 1 ]
then
    echo "Usage: srun-command.sh command [params..]"
    exit 1
fi

srun "$@"

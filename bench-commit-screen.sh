#!/bin/bash

set -eEu

if [ $# -lt 5 ]
then
    echo "Usage: bench-commit-screen.sh benchmark-target benchmark-repo benchmark-commit benchmark-time package"
    exit 1
fi

NAME="tact$RANDOM"

screen -L -dm -S "$NAME" /home/blaaulas/tactician/benchmark-system/srun-tactician-benchmark.sh "$NAME" "$@"

echo $NAME

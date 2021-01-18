#!/bin/bash

set -eEu

if [ $# -lt 1 ]
then
    echo "Usage: cancel.sh workspace-dir"
    exit 1
fi

DIR=${1}; shift

echo "" > $DIR/queue

# Let's ask nicely
scancel -v -s SIGTERM $(ls $DIR/processors)

sleep 1m

# Let's ask less nicely
scancel -v $(ls $DIR/processors)

# Should not be needed, but lets just be sure
rm -f $DIR/processors/*

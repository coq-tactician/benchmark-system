#!/bin/bash

set -eu

if [ $# -lt 1 ]
then
    echo "Usage: cancel.sh workspace-dir"
    exit 1
fi

DIR=${1}; shift

# Let's ask nicely
echo "" > $DIR/queue
scancel -v -s SIGTERM $(ls $DIR/processors)

sleep 1m

# Let's ask less nicely
echo "" > $DIR/queue
scancel -v -s SIGKILL $(ls $DIR/processors)

# Should not be needed, but lets just be sure
rm -f $DIR/processors/*

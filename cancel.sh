#!/bin/bash

set -eu

if [ $# -lt 1 ]
then
    echo "Usage: cancel.sh workspace-dir"
    exit 1
fi

DIR=${1}; shift

echo "" > $DIR/queue
scancel -v -s SIGTERM $(ls $DIR/processors)

# Should not be needed, but lets just be sure
rm -f $DIR/processors/*

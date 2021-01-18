#!/bin/bash

set -eEu

if [ $# -lt 3 ]
then
    echo "Usage: tee-output.sh stdout-file stderr-file command [arg..]"
    exit 1
fi

STDOUT=${1}; shift
STDERR=${1}; shift

# Taken from https://stackoverflow.com/a/692407
"$@" > >(tee -a $STDOUT) 2> >(tee -a $STDERR >&2)

#!/bin/bash

set -eu
set -o pipefail

if [ $# -lt 3 ]
then
    echo "Usage: bench-commit-screen.sh repo commit packages [settings..]"
    exit 1
fi

# Create global workspace
GLOBALDIR=$(mktemp --directory --tmpdir=/home/blaaulas/tactician/builds XXXXXXXX)
NAME=$(basename $GLOBALDIR)

COMPILECPUS=8

REPO=${1}; shift
COMMIT=${1}; shift
PACKAGES=${1}; shift

SCRIPT=$(realpath $0)
SCRIPTPATH=$(dirname $SCRIPT)
PATH=$SCRIPTPATH:$PATH
export PATH

screen -dm -S $NAME tee-output.sh "$GLOBALDIR"/output.log "$GLOBALDIR"/error.log \
       benchmark-supervisor.sh "$GLOBALDIR" "$COMPILECPUS" "$REPO" "$COMMIT" "$PACKAGES" "$@"
echo $NAME

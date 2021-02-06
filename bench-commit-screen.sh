#!/bin/bash

set -eEu
catch() {
    echo "Error $1 occurred on line $2 in $0" >> $GLOBALDIR/error
}
trap 'catch $? $LINENO' ERR

if [ $# -lt 5 ]
then
    echo "Usage: bench-commit-screen.sh repo commit packages params bench-params"
    exit 1
fi

# Create global workspace
GLOBALDIR=$(mktemp --directory --tmpdir=/home/blaaulas/tactician/builds XXXXXXXX)
NAME=$(basename $GLOBALDIR)

COMPILECPUS=8

REPO=${1}; shift
COMMIT=${1}; shift
PACKAGES=${1}; shift
PARAMS=${1}; shift
BENCHPARAMS=${1}; shift

SCRIPT=$(realpath $0)
SCRIPTPATH=$(dirname $SCRIPT)
PATH=$SCRIPTPATH:$PATH
export PATH

screen -dm -S $NAME tee-output.sh "$GLOBALDIR"/output.log "$GLOBALDIR"/error.log \
       benchmark-supervisor.sh "$GLOBALDIR" "$COMPILECPUS" "$REPO" "$COMMIT" "$PACKAGES" "$PARAMS" "$BENCHPARAMS"
echo $NAME

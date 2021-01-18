#!/bin/bash

set -eu
set -o pipefail

if [ $# -lt 1 ]
then
    echo "Usage: bench-commit-ssh.sh bench-cmd"
    exit 1
fi

CMD=${1}; shift

SCRIPT=$(realpath $0)
SCRIPTPATH=$(dirname $SCRIPT)
# Taken from https://stackoverflow.com/a/24625575
COMMAND_ARRAY=()
while read line; do
    COMMAND_ARRAY+=("$line")
done < <(xargs -n 1 <<< "$SSH_ORIGINAL_COMMAND")
$CMD "${COMMAND_ARRAY[@]}"

#!/bin/bash

set -eu
set -o pipefail

SCRIPT=$(realpath $0)
SCRIPTPATH=$(dirname $SCRIPT)
# Taken from https://stackoverflow.com/a/24625575
COMMAND_ARRAY=()
while read line; do
    COMMAND_ARRAY+=("$line")
done < <(xargs -n 1 <<< "$SSH_ORIGINAL_COMMAND")
"$SCRIPTPATH"/bench-commit-sbatch.sh "${COMMAND_ARRAY[@]}"

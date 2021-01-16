#!/bin/bash

set -eu
set -o pipefail

if [ $# -lt 3 ]
then
    echo "Usage: bench-commit-sbatch.sh repo commit packages [settings..]"
    exit 1
fi

DATAREPO=/home/blaaulas/tactician/benchmark-data
COMPILECPUS=8
SQUEUE=10

REPO=${1}; shift
COMMIT=${1}; shift
PACKAGES=${1}; shift

PARAMSTR=$(echo $* | tr ' ' '-' | tr '=' '-')
DATA=$DATAREPO/$COMMIT/$PARAMSTR

SCRIPT=$(realpath $0)
SCRIPTPATH=$(dirname $SCRIPT)
PATH=$SCRIPTPATH:$PATH
export PATH

mkdir -p "$DATA"

sbatch --job-name=tb.${COMMIT}${PARAMSTR} --cpus-per-task=1 --parsable \
       --time=06:00:00 --mem-per-cpu=4000 --partition compute --ntasks=1 \
       --open-mode=append --parsable \
       --output="$DATA"/output.log \
       --error="$DATA"/error.log \
       srun-command.sh benchmark-supervisor.sh "$DATA" "$COMPILECPUS" "$SQUEUE" \
        "$REPO" "$COMMIT" "$PACKAGES" "$@"

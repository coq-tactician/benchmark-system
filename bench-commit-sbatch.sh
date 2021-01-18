#!/bin/bash

set -eEu
catch() {
    echo "Error $1 occurred on line $2 in $0" >> $GLOBALDIR/error
}
trap 'catch $? $LINENO' ERR

if [ $# -lt 3 ]
then
    echo "Usage: bench-commit-sbatch.sh repo commit packages [settings..]"
    exit 1
fi

# Create global workspace
GLOBALDIR=$(mktemp --directory --tmpdir=/home/blaaulas/tactician/builds XXXXXXXX)

COMPILECPUS=8

REPO=${1}; shift
COMMIT=${1}; shift
PACKAGES=${1}; shift

SCRIPT=$(realpath $0)
SCRIPTPATH=$(dirname $SCRIPT)
PATH=$SCRIPTPATH:$PATH
export PATH

# We oversubscribe on the supervisor job because it does basically nothing
sbatch --job-name=tb.$(basename $GLOBALDIR) --cpus-per-task=1 --oversubscribe \
       --mem-per-cpu=100 --partition compute --ntasks=1 \
       --open-mode=append --parsable \
       --output="$GLOBALDIR"/output.log \
       --error="$GLOBALDIR"/error.log \
       srun-command.sh benchmark-supervisor.sh "$GLOBALDIR" "$COMPILECPUS" \
        "$REPO" "$COMMIT" "$PACKAGES" "$@"

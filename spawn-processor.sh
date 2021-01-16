#!/bin/bash

set -eu

if [ $# -lt 3 ]
then
    echo "Usage: spawn-processor.sh dir global-dir copy-dir"
    exit 1
fi

DIR=${1}; shift
GLOBALDIR=${1}; shift
COPYDIR=${1}; shift

while true
do
    {
        flock -x 3
        AVAILABLE=$(cat "$GLOBALDIR"/squeue-free)
        if [ "$AVAILABLE" -gt 0 ] && [ -s "$GLOBALDIR"/queue ]
        then
            #Spawn a job
            ID=$(sbatch --job-name=pr.$(basename $GLOBALDIR) --cpus-per-task=1 \
                   --time=03:00:00 --mem-per-cpu=4000 --partition compute --ntasks=1 \
                   --open-mode=append --parsable \
                   --output="$GLOBALDIR"/processors-output.log \
                   --error="$GLOBALDIR"/processors-error.log \
                   srun-command.sh processor.sh "$DIR" "$GLOBALDIR" "$COPYDIR")
            touch "$GLOBALDIR"/processors/"$ID"

            AVAILABLE=$(($AVAILABLE - 1))
        else
            exit 0
        fi
        echo $AVAILABLE > "$GLOBALDIR"/squeue-free
    } 3<"$GLOBALDIR"/squeue-free.lock
done

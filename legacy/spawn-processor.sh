#!/bin/bash

set -eEu
catch() {
    echo "Error $1 occurred on line $2 in $0" >> $GLOBALDIR/error
}
trap 'catch $? $LINENO' ERR

if [ $# -lt 2 ]
then
    echo "Usage: spawn-processor.sh dir global-dir"
    exit 1
fi

DIR=${1}; shift
GLOBALDIR=${1}; shift

# cd to global workspace, because the current directory might not exist on the
# node where a job gets spawned
cd $GLOBALDIR

while true
do
    {
        flock -x 3
        AVAILABLE=$(cat ~/tactician/squeue-free)
        if [ "$AVAILABLE" -gt 0 ] && [ -s "$GLOBALDIR"/queue ]
        then
            #Spawn a job
            ID=$(sbatch --job-name=pr.$(basename $GLOBALDIR) --cpus-per-task=1 \
                   --time=04:00:00 --mem-per-cpu=4000 --partition compute --ntasks=1 \
                   --open-mode=append --parsable \
                   --output="$GLOBALDIR"/processor-logs/%A-output.log \
                   --error="$GLOBALDIR"/processor-logs/%A-error.log \
                   srun-command.sh processor.sh "$DIR" "$GLOBALDIR")
            touch "$GLOBALDIR"/processors/"$ID"

            AVAILABLE=$(($AVAILABLE - 1))
        else
            exit 0
        fi
        echo $AVAILABLE > ~/tactician/squeue-free
    } 3< ~/tactician/squeue-free.lock
done

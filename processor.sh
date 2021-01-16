#!/bin/bash

set -eu

if [ $# -lt 3 ]
then
    echo "Usage: processor.sh dir global-dir copy-dir"
    exit 1
fi

MAX_TIME=3600

DIR=${1}; shift
GLOBALDIR=${1}; shift
COPYDIR=${1}; shift

# This processor is no longer in queue, thus we can increase squeue-free
{
    flock -x 3
    AVAILABLE=$(cat "$GLOBALDIR"/squeue-free)
    AVAILABLE=$(($AVAILABLE + 1))
    echo $AVAILABLE > "$GLOBALDIR"/squeue-free
} 3<"$GLOBALDIR"/squeue-free.lock

# Create dir lockfile if not exists
touch ${DIR}.lock

COQOUT=$(mktemp)
COQERR=$(mktemp)

end_script() {
    rm "$GLOBALDIR"/processors/"$SLURM_JOB_ID"
    spawn-processor.sh "$DIR" "$GLOBALDIR" "COPYDIR"
    # Copy generated bench files to the global working directory
    {
        flock -x 3
        echo "Copy bench files"
        # Note the training slash
        # Taken from https://stackoverflow.com/a/32527277
        time rsync -zar  --prune-empty-dirs --include "*/" \
             --include="*.bench" --exclude="*" $DIR/ $GLOBALDIR

    } 3<${DIR}.lock
    {
        flock -x 3
        cat $COQOUT >> "$GLOBALDIR"/coq-out.log
        cat $COQERR >> "$GLOBALDIR"/coq-err.log
    } 3<"$GLOBALDIR"/coq-log.lock
}
trap end_script EXIT

START_TIME="$(date -u +%s)"
while true
do
    CURR_TIME="$(date -u +%s)"
    ELAPSED=$(($CURR_TIME - $START_TIME))
    echo "Elapsed: $ELAPSED"
    [ $ELAPSED -gt $MAX_TIME ] && exit 0

    # Retrieve command to be executed
    {
        flock -x 3
        # File stack manipulations taken from https://stackoverflow.com/a/13805282
        LINE=$(sed -e 1$'{w/dev/stdout\n;d}' -i "$GLOBALDIR"/queue)
        CMD=$(echo "$LINE" | cut -f2)
    } 3<"$GLOBALDIR"/queue.lock

    # Update local copy of build dir
    {
        flock -x 3
        echo "Copy from $COPYDIR"
        # Note the training slash
        time rsync -az $COPYDIR/ $DIR

    } 3<${DIR}.lock

    cd "$DIR"
    eval $(opam env --root=$DIR/.opam --set-root)

    # Execute command
    [ -z "$CMD" ] && exit 0
    echo "$CMD"
    eval "$CMD" 2>> "$COQERR" >> "$COQOUT"
    {
        flock -x 3
        echo "$LINE" >> "$GLOBALDIR"/finished-jobs
    } 3<"$GLOBALDIR"/finished-jobs.lock
done

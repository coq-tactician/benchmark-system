#!/bin/bash

set -eEu
catch() {
    echo "Error $1 occurred on line $2 in $0" >> $GLOBALDIR/error
}
trap 'catch $? $LINENO' ERR

if [ $# -lt 5 ]
then
    echo "Usage: benchmark-supervisor.sh global-dir #cpus repo commit packages [settings..]"
    exit 1
fi

GLOBALDIR=${1}; shift
CPUS=${1}; shift
REPO=${1}; shift
COMMIT=${1}; shift
PACKAGES=${1}; shift

echo "Benchmarking packages $PACKAGES using commit ${REPO}#${COMMIT} with parameters $@"
echo "Workspace directory: ${GLOBALDIR}"

module use ~/.local/easybuild/modules/all
module load git git-lfs bubblewrap OCaml parallel

# Determine local build directory name, and the directory to copy from
DIR=$(mktemp --directory -t tactician-XXXXXX)
echo "Local build directories: ${DIR}"

# Set up data for managing slurm queue
cd $GLOBALDIR
touch coq-log.lock
mkdir processors
mkdir processor-logs
touch all-jobs
touch finished-jobs
touch finished-jobs.lock
touch queue
touch queue.lock

# Create job that performs compilation of packages in opam
ID=$(sbatch --job-name=cp.$(basename $GLOBALDIR) --cpus-per-task="$CPUS" \
         --mem-per-cpu=4000 --partition compute --ntasks=1 \
         --open-mode=append --parsable \
         --output="$GLOBALDIR"/initial-compile-output.log \
         --error="$GLOBALDIR"/initial-compile-error.log \
         srun-command.sh compile-task.sh "$DIR" "$GLOBALDIR" \
         "$CPUS" "$REPO" "$COMMIT" "$PACKAGES" "$@")
touch "$GLOBALDIR"/processors/"$ID"

while true
do
    RUNNING=$(ls "$GLOBALDIR"/processors | wc -l)
    if [ -s "$GLOBALDIR"/queue ] || [ "$RUNNING" -gt 0 ]
    then
        #only spawn new processors when the copy-dir is known
        [ -f $GLOBALDIR/copy-dir ] && spawn-processor.sh "$DIR" "$GLOBALDIR"

        # Calculate progress
        {
            flock -x 3
            TOTAL=$(cat "$GLOBALDIR"/all-jobs | cut -f1 | cat - <(printf 0) | paste -s -d+ - | bc)
        } 3<"$GLOBALDIR"/queue.lock
        {
            flock -x 3
            FINISHED=$(cat "$GLOBALDIR"/finished-jobs | cut -f1 | cat - <(printf 0) | paste -s -d+ - | bc)
        } 3<"$GLOBALDIR"/finished-jobs.lock
        echo "Processed $FINISHED out of $TOTAL lemmas (rough approximation)"
        sleep 1m
    else
        break
    fi
done

echo "Processing done"
echo "Collecting results"

# Create data directory
PARAMSTR=$(echo $* | tr ' ' '-' | tr '=' '-')
DATAREPO=/home/blaaulas/tactician/benchmark-data
DATA=$DATAREPO/$COMMIT/$PARAMSTR
mkdir -p $DATA
echo "Data directory: $DATA"

# Collect results
find $GLOBALDIR/_opam/.opam-switch/build -name '*.bench' | xargs cat > $DATA/combined.bench
find $GLOBALDIR/processor-logs -name '*-output.log' | xargs cat > $DATA/processor-output.log
find $GLOBALDIR/processor-logs -name '*-error.log' | xargs cat > $DATA/processor-error.log
cp $GLOBALDIR/*.log $DATA

# Add and push collected files (also includes log files)
cd $DATA
git add combined.bench *.log
git commit -m "benchmark data for ${REPO}#${COMMIT}"
git push origin master

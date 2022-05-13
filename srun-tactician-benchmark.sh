#!/bin/bash
set -e

if [ $# -lt 6 ]
then
    echo "Usage: bench-commit-screen.sh name benchmark-target benchmark-repo benchmark-commit benchmark-time package"
    exit 1
fi

NAME=${1}; shift
TARGET=${1}; shift
REPO=${1}; shift
COMMIT=${1}; shift
TIME=${1}; shift
PACKAGE=${1}; shift

module use ~/.local/easybuild/modules/all
module load git bubblewrap OCaml Anaconda3 CapnProto
export OPAMROOT=~/.opam
eval $(opam env)

EXIT_CODE=0
srun --job-name="$NAME" --cpus-per-task=30 --time=08:00:00 --mem-per-cpu=4000 --partition compute --ntasks=1 \
     tactician-benchmark /home/blaaulas/tactician/benchmark-data/ \
     "$TARGET" "$REPO" "$COMMIT" "$TIME" 15 "$PACKAGE" -tmp-dir /lscratch/blaaulas/ -delay-benchmark \
    || EXIT_CODE=$?
echo $EXIT_CODE > /home/blaaulas/tactician/last-status

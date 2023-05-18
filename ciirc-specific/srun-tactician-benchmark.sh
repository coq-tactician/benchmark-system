#!/bin/bash
set -e

if [ $# -lt 6 ]
then
    echo "Usage: srun-tactician-benchmark.sh name benchmark-target benchmark-repo benchmark-commit benchmark-time package"
    exit 1
fi

NAME=${1}; shift
TARGET=${1}; shift
REPO=${1}; shift
COMMIT=${1}; shift
TIME=${1}; shift
PACKAGE=${1}; shift

module use ~/.local/easybuild/modules/all
module load GCC git bubblewrap OCaml Anaconda3 CapnProto util-linux
export OPAMROOT=~/.opam
eval $(opam env)
ulimit -n 4096
ulimit -u 65536

EXIT_CODE=0
# Run the command on the head node for now. It does not do much anyway
#srun --job-name="$NAME" --cpus-per-task=1 --time=08:00:00 --mem-per-cpu=1000 --partition compute --ntasks=1 \

tactician-benchmark \
    -benchmark-data /home/blaaulas/tactician/benchmark-data/ \
    -compile-allocator /home/blaaulas/tactician/benchmark-system/slurm/compile_allocator \
    -bench-allocator /home/blaaulas/tactician/benchmark-system/slurm/bench_allocator \
    -max-requests 28 \
    -tmp-dir /lscratch/blaaulas/ \
    -benchmark-target "$TARGET" \
    -benchmark-repo "$REPO" \
    -benchmark-commit "$COMMIT" \
    -benchmark-time "$TIME" \
    "$PACKAGE" \
    || EXIT_CODE=$?
echo $EXIT_CODE > /home/blaaulas/tactician/last-status

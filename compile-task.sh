#!/bin/bash

set -eu

if [ $# -lt 5 ]
then
    echo "Usage: benchmark-supervisor.sh dir global-dir #cpus repo commit packages [settings..]"
    exit 1
fi

DIR=${1}; shift
GLOBALDIR=${1}; shift
CPUS=${1}; shift
REPO=${1}; shift
COMMIT=${1}; shift
PACKAGES=${1}; shift

trap 'rm "$GLOBALDIR"/processors/"$SLURM_JOB_ID"' EXIT

mkdir -p $DIR
COPYDIR=$(mktemp --directory -t tactician-XXXXXX)
COPYDIRBIG=$(hostname):$COPYDIR
echo $COPYDIRBIG > $GLOBALDIR/copy-dir

# Pretend that we are installing in the DIR directory, but we actually do it in COPYDIR
# This is to prevent files generated by local processors to be copied to all nodes
{ time {
      bwrap --dev-bind / / --die-with-parent --bind $COPYDIR $DIR \
            build-initial-opam.sh $DIR $GLOBALDIR $CPUS $REPO $COMMIT $PACKAGES "$@" 2>&3
  }; } 3>&2 2> $GLOBALDIR/base-install-time.log

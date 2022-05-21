# Tactician benchmarking

The distributed benchmarking system of Tactician.

## Input:
- The repository and commit hash of a particular version of Tactician (or one of it's model plugins).
- A set of Opam packages to benchmark on that particular commit.
- Parameters governing which lemmas to benchmark and how long.
- A way to allocate resources either locally, remotely through ssh, or through a HPC cluster.

## Output:
- Every requested lemma will be benchmarked, and the results will be uploaded to the 'benchmark-data'
  repository of the coq-tactician Github organization.

## Environment requirements
This software is designed to be run on a compute cluster (possibly consisting of one machine). We make some
assumptions over this cluster:
- In order to install the software, the Opam package manager version 2.x needs to be available (you can check
  the version by running `opam --version`).
- During operation, a working version of Bubblewrap is required. You can check that Bubblewrap functions properly
  by running `bwrap --dev-bind / / echo "bubblewrap is working!"`.
- Additional required software is Git, Rsync and Bash.
- It is assumed that the cluster is configured uniformly. That is:
  + This software should be installed on all nodes with the same version and in the same location. The easiest way
    to ensure this is using a shared filesystem. The alternative is to carefully install the software manually and
    keep it consistent.
  + A storage location for temporary files is required that has the same path on every node. This location is
    currently assumed **not** to be on a shared filesystem.
  + Nodes should be connected to a shared network and be able to communicate with each other over TCP.
  + The system needs the ability to spawn processes on other remote nodes, either through SSH or
    through commands provided by a HPC cluster orchestrator.

## Installation:
Make sure that you have an Opam switch available. Then clone this repository, and from the root of the repo run
```
opam install .
```

## Usage Details:
For details, please read the output of `tactician-benchmark -help`. This ouput is also replicated below.

## Examples:
- Running a benchmark of coq-tactician-stdlib on a single computer with a time limit of 40 seconds.
  A maximum of 16 cores are used for benchmarking (as ensured by `-maxrequests 16` and `-max-running 16`).
  Additionally, the flag `-delay-benchmark` ensures that benchmarking only starts after the initial
  compilation of Opam packages has finished. This ensures hat the CPU cycles taken by the initial
  compilation does not interfere with the benchmarking.
  ```
  tactician-benchmark \
      -benchmark-data ./benchmark-data/ \
      -compile-allocator ./benchmark-system/local/compile_allocator \
      -bench-allocator ./benchmark-system/local/bench_allocator \
      -max-requests 16 \
      -max-running 16 \
      -delay-benchmark \
      -benchmark-target coq-tactician \
      -benchmark-repo git+ssh://git@github.com/coq-tactician/coq-tactician \
      -benchmark-commit f267afe31ab82b071d3f03d08bc4a6595c30b1d2 \
      -benchmark-time 40 \
      coq-tactician-stdlib
  ```
- Running a benchmark of coq-tactician-stdlib on a SLURM cluster with a time limit of 40 seconds. A maximum of
  28 resource requests are allowed to be submitted to the SLURM queue at the same time. There is no limit on
  the number of simultaneous allocated resources.
  ```
  tactician-benchmark \
      -benchmark-data ./benchmark-data/ \
      -compile-allocator ./benchmark-system/slurm/compile_allocator \
      -bench-allocator ./benchmark-system/slurm/bench_allocator \
      -max-requests 28 \
      -benchmark-target coq-tactician \
      -benchmark-repo git+ssh://git@github.com/coq-tactician/coq-tactician \
      -benchmark-commit f267afe31ab82b071d3f03d08bc4a6595c30b1d2 \
      -benchmark-time 40 \
      coq-tactician-stdlib
  ```

## Arguments
This has been copied from the output of `tactician-benchmark -help`:
```
tactician-benchmark PACKAGE [PACKAGE ...]

  -bench-allocator executable    An executable file that allocates computational
                                 resources for benchmarking. This file may be
                                 executed many times, as governed by the flags
                                 '-max-requests' and '-max-running'. When
                                 executed, this file should request the needed
                                 resources to run one or more Coq processes that
                                 benchmark lemmas for a given amount of time.
                                 Similar to '-compile-allocator', these
                                 resources can be allocated local, remote
                                 through ssh, or through a HPC cluster
                                 scheduler. When the resources have been
                                 allocated, the executable should first output a
                                 line to stdout indicating the number of minutes
                                 the resources can be used. The benchmarking
                                 system may not be able to relinquish the
                                 resources exactly within that time. A safety
                                 margin of ~15 minutes is recommended. Then the
                                 executable should output the prefixes of
                                 commands that can be executed in order to
                                 access the allocated resources (see
                                 '-compile-allocator' for examples). A single
                                 outputted prefix will be used to run a single
                                 Coq benchmarking process. This series of
                                 prefixes should be followed by the string
                                 'done'. After the bencharking system is
                                 finished with the resources it will print
                                 'done' to the stdin of the executable. At that
                                 point the executable should relinquish the
                                 allocated resources and exit.
  -benchmark-commit hash         The hash of the commit that should be
                                 benchmarked.
  -benchmark-data dir            Location of the benchmark-data storage
                                 repository.
  -benchmark-repo URI            The repository where the benchmark target can
                                 be found. Accepts any URI that is accepted by
                                 Opam.
  -benchmark-target string       Name of the Opam package containing the model
                                 that should be benchmarked. This can be any
                                 package that depends on coq-tactician.
  -benchmark-time int            Time limit in seconds for synthesizing an
                                 individual lemma.
  -compile-allocator executable  An executable file that allocates computational
                                 resources for the initial compilation of Opam
                                 packages. When executed, this file should
                                 request the needed resources, which can come
                                 either from the local machine, from a remote
                                 machine through ssh, or from a HPC cluster
                                 scheduler. When the resources have been
                                 allocated, the executable should output the
                                 prefix of a command that can be executed in
                                 order to access the allocated resources on
                                 stdout.
                                 Example prefixes:
                                 - For local resources, this prefix would be the
                                 empty string
                                 - For a remote node accessed through ssh, the
                                 prefix would be 'ssh remote-node'
                                 - For a SLURM cluster, where a job id has been
                                 allocated, the prefix would be 'srun --ntasks=1
                                 --jobid $SLURM_JOB_ID'
                                 After benchmarking system is finished with the
                                 resources, it will print 'done' to the stdin of
                                 the executable. At that point, the executable
                                 should relinquish the allocated resources and
                                 exit.
  -max-requests int              The maximum number of benchmark resource
                                 allocations that may be running at the same
                                 time. Related to '-bench-allocator'. This
                                 governs how much load may be applied to
                                 queueing mechanism of the underlying HPC
                                 cluster. The higher this number, the more
                                 resources will be requested at the same time.
                                 When this number is low, the benchmarking
                                 system may be starved of resources on busy
                                 clusters. When this number is high, this may
                                 flood the underlying queue, which may annoy
                                 other users of the cluster. NOTE: This flag
                                 does not limit the total number of resources
                                 allocated. For that, see '-max-running'.
  [-build-dir dir]               Location of the build. This directory will not
                                 be cleaned up after the benchmark finishes.
                                 Mutually exclusive with -tmp-dir.
  [-debug]                       Show additional debug info on stderr.
  [-delay-benchmark]             Delay the benchmark until the initial build is
                                 fully complete. Useful when the build process
                                 may interfere with the benchmark timings.
  [-exclude file]                A file containing a line-separated list of
                                 lemma names that should not benchmarked. Thos
                                 lemmas are ignored.
  [-exclude-regexp regexp]       A regular expression matching lemmas. Only
                                 lemmas not matching the expression are
                                 benchmarked.
  [-include file]                A file containing a line-separated list of
                                 lemma names that should be benchmarked. Other
                                 lemmas
                                 are ignored.
  [-include-regexp regexp]       A regular expression matching lemmas. Only
                                 lemmas matching the expression are benchmarked.
  [-inject vernacular] ...       Inject Coq vernacular into the compilation and
                                 benchmarking process. Typically used to specify
                                 options. Can be repeated multiple times and
                                 combined with -inject-file.
  [-inject-file file] ...        Inject a file containing Coq vernacular into
                                 the compilation and benchmarking process.
                                 Typically used to specify options. Can be
                                 repeated multiple times and combined with
                                 -inject-file.
  [-max-running int]             The maximum number of benchmark resources
                                 requests that can be requested and running at
                                 the same time. See also '-max-requests'.
                                 WARNING: This number is infinite by default,
                                 which is likely not appropriate for local
                                 resources.
  [-tmp-dir dir]                 Location in which a temporary directory will be
                                 created to store the build. If not supplied, it
                                 is taken from $TMPDIR. After the benchmark is
                                 finished, the directory is cleaned up. Mutually
                                 exclusive with -build-dir.
  [-build-info]                  print info about this build and exit
  [-version]                     print the version of this build and exit
  [-help]                        print this help text and exit
                                 (alias: -?)
```

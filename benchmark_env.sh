#!/bin/bash
module use ~/.local/easybuild/modules/all
module load git bubblewrap OCaml Anaconda3 CapnProto util-linux
export OPAMROOT=~/.opam
eval $(opam env)
ulimit -n 4096

#!/bin/bash
module use ~/.local/easybuild/modules/all
module load git bubblewrap OCaml Anaconda3 CapnProto
export OPAMROOT=~/.opam
eval $(opam env)

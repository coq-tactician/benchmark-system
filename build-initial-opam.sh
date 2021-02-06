#!/bin/bash

set -eEu
catch() {
    echo "Error $1 occurred on line $2 in $0" >> $GLOBALDIR/error
}
trap 'catch $? $LINENO' ERR

if [ $# -lt 8 ]
then
    echo "Usage: build-initial-opam.sh dir global-dir #cpus repo commit packages params bench-params"
    exit 1
fi

DIR=${1}; shift
GLOBALDIR=${1}; shift
CPUS=${1}; shift
REPO=${1}; shift
COMMIT=${1}; shift
PACKAGES=${1}; shift
PARAMS=${1}; shift
BENCHPARAMS=${1}; shift

cd $DIR

# Initialize opam
opam init --bare --root=$DIR/.opam --no-setup --bypass-checks --disable-sandbox
opam switch create . --empty --root=$DIR/.opam
eval $(opam env --root=$DIR/.opam --set-root)

# Add repos
opam repo add coq-released https://coq.inria.fr/opam/released
opam repo add coq-extra-dev https://coq.inria.fr/opam/extra-dev
opam repo add coq-core-dev https://coq.inria.fr/opam/core-dev
opam repo add custom-archive https://github.com/LasseBlaauwbroek/custom-archive.git

# Compile commit
{ time { opam pin add --yes --kind=git "$REPO"#"$COMMIT" \
         --jobs="$CPUS" 2>&3; }; } 3>&2 2> $GLOBALDIR/tactician-install-time.log

# Compile dependencies with tactician injected, but without adding things to the queue
tactician inject
{ time { opam install --verbose --jobs="$CPUS" --yes \
              --deps-only "$PACKAGES" 2>&3; }; } 3>&2 2> $GLOBALDIR/deps-install-time.log

# Configure and inject Tactician
echo "$PARAMS" > $DIR/"Params.v"
echo "$BENCHPARAMS" > $DIR/"BenchParams.v"
cp $(which bench-wrap) $(opam var bin)/bench-wrap
cat <<EOF > $DIR/_opam/.opam-switch/config/coq-tactician.config
#!/bin/bash
opam-version: "2.0"
variables {
    injection-wrappers: "bench-wrap $DIR $GLOBALDIR $DIR/BenchParams.v"
    injection-flags: "-l $DIR/Params.v"
}
EOF

# Compile packages first time
{ time { opam install --verbose --jobs="$CPUS" --yes \
              --keep-build-dir "$PACKAGES" 2>&3; }; } 3>&2 2> $GLOBALDIR/compile-package-time.log

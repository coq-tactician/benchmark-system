#!/bin/bash

set -eEu
catch() {
    echo "Error $1 occurred on line $2 in $0" >> $GLOBALDIR/error
}
trap 'catch $? $LINENO' ERR

if [ $# -lt 6 ]
then
    echo "Usage: build-initial-opam.sh dir global-dir #cpus repo commit packages [settings..]"
    exit 1
fi

DIR=${1}; shift
GLOBALDIR=${1}; shift
CPUS=${1}; shift
REPO=${1}; shift
COMMIT=${1}; shift
PACKAGES=${1}; shift

cd $DIR

# Initialize opam
opam init --bare --root=$DIR/.opam --no-setup --bypass-checks --disable-sandbox
opam switch create . --empty --root=$DIR/.opam
eval $(opam env --root=$DIR/.opam --set-root)

# Add repos
opam repo add coq-released https://coq.inria.fr/opam/released
opam repo add coq-extra-dev https://coq.inria.fr/opam/extra-dev
opam repo add coq-core-dev https://coq.inria.fr/opam/core-dev

# Compile commit
opam pin add --no-action --yes --kind=git coq-tactician "$REPO"#"$COMMIT"
{ time { opam install coq-tactician \
              --yes --jobs="$CPUS" 2>&3; }; } 3>&2 2> $GLOBALDIR/tactician-install-time.log

# Compile dependencies with tactician injected, but without adding things to the queue
tactician inject
{ time { opam install --verbose --jobs="$CPUS" --yes \
              --deps-only "$PACKAGES" 2>&3; }; } 3>&2 2> $GLOBALDIR/deps-install-time.log

# Configure and inject Tactician
PARAMS=$DIR/"Params.v"
touch "$PARAMS"
for ARG in "$@"
do
    echo "$ARG". >> "$PARAMS"
done
cp $(which bench-wrap) $(opam var bin)/bench-wrap
cat <<EOF > $DIR/_opam/.opam-switch/config/coq-tactician.config
#!/bin/bash
opam-version: "2.0"
variables {
    injection-wrappers: "bench-wrap $DIR $GLOBALDIR $PARAMS"
    injection-flags: ""
}
EOF

# Compile packages first time
{ time { opam install --verbose --jobs="$CPUS" --yes \
              --keep-build-dir "$PACKAGES" 2>&3; }; } 3>&2 2> $GLOBALDIR/compile-package-time.log

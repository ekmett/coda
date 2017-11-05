#!/bin/bash

set -e

PKG=$1
EXE=$2
DIR="$3"

if [ "$#" != 3 ]; then
    echo "usage: $0 <pkg-name> <exe-name> <bin-target-folder>"
    echo ""
    echo "(example invocation:    $0 hlint hlint ~/bin  )"
    exit 1
fi

if [ ! -d "$DIR" ]; then
    echo "target folder '$DIR' not found"
    exit 1
fi

if [ -e "$DIR/$EXE" ]; then
    echo "'$DIR/$EXE' exists already - remove it and then call me again"
    echo ""
    ls -l "$DIR/$EXE"
    exit 1
fi

# this needs `cabal-plan:exe:cabal-plan` in PATH
type cabal-plan
type cabal

TMPDIR=$(mktemp -d)

cd $TMPDIR

# todo: add support for version constraints?

cat > dummypkg.cabal <<EOF
name:                dummypkg
version:             0
build-type:          Simple
cabal-version:       >=2.0
library
  default-language: Haskell2010
  build-tool-depends: ${PKG}:${EXE}
--
EOF

cabal new-build lib:dummypkg

EXEBIN=$(cabal-plan list-bin | awk "\$1 == \"${PKG}:exe:${EXE}\" { print \$2 }")

if [ -x "$EXEBIN" ]; then
    cp -sfv "$EXEBIN" "$DIR"
else
    echo "*ERROR* couldn't find ${PKG}:exe:${EXE}"
    echo ""
    cabal-plan list-bin
fi

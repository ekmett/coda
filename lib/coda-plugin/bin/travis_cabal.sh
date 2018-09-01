#!/usr/bin/env bash

shell_session_update() {
  echo "invoked shell_session_update"
}

coda_before_install() {
  set -x
  unset CC
  export HAPPYVER=1.19.5
  export ALEXVER=3.1.7
  export CABALFLAGS="$CABALFLAGS -v2"
  export PATH=/opt/ghc/bin:/opt/ghc-ppa-tools/bin:~/.cabal/bin:/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:/opt/happy/$HAPPYVER/bin:/opt/alex/$ALEXVER/bin:$PATH
  set +x
}

coda_install() {
  set -x
  cabal --version
  cabal update
  set +x
}

coda_script() {
  set -x
  time cabal new-build all
  time cabal new-test all
  # npm install
  # vsce package
  # unzip -v *.vsix
  set +x
}

coda_after_script() {
  set -x
  # cabal install hpc-coveralls
  # hpc-coveralls coda-doctests coda-spec --exclude-dir=test --display-report
  set +x
}

coda_trap() {
  set -e
  $*
  set +e
}

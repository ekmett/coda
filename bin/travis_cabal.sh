#!/usr/bin/env bash

shell_session_update() {
  echo "invoked shell_session_update"
}

coda_before_install() {
  set -x
  unset CC
  if [ $TRAVIS_OS_NAME == "linux" ]; then
    export CXX="g++-4.9" CC="gcc-4.9" DISPLAY=:99.0;
    sh -e /etc/init.d/xvfb start;
    sleep 3;
  fi
  export HAPPYVER=1.19.5
  export ALEXVER=3.1.7
  export PATH=~/.cabal/bin:/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:/opt/happy/$HAPPYVER/bin:/opt/alex/$ALEXVER/bin:$PATH
  export CABALFLAGS="$CABALFLAGS -v2"
  set +x
}

coda_install() {
  set -x
  cabal --version
  echo "$(ghc --version) [$(ghc --print-project-git-commit-id 2> /dev/null || echo '?')]"
  travis_retry cabal update
  sed -i  's/^jobs:.*$/jobs: 2/' $HOME/.cabal/config
  time cabal install --enable-tests --only-dependencies
  set +x
}

coda_script() {
  set -x
  time cabal configure --enable-tests
  time cabal build
  time cabal test --show-details=always
  time cabal sdist
  unzip -v dist/build/*.vsix
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

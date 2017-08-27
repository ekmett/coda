#!/usr/bin/env bash

shell_session_update() {
  echo "invoked shell_session_update"
}

coda_before_install() {
  set -x
  unset CC
  #if [ $TRAVIS_OS_NAME == "linux" ]; then
  #  export CXX="g++-4.9" CC="gcc-4.9" DISPLAY=:99.0;
  #  sh -e /etc/init.d/xvfb start;
  #  sleep 3;
  #fi
  export HAPPYVER=1.19.5
  export ALEXVER=3.1.7
  export CABALFLAGS="$CABALFLAGS -v2"
  # mkdir ~/.hlint
  # curl -L https://github.com/ndmitchell/hlint/releases/download/v$HLINTVER/hlint-$HLINTVER-x86_64-linux.tar.gz | tar -xz --strip-components=1 -C ~/.hlint
  export PATH=/opt/ghc/bin:/opt/ghc-ppa-tools/bin:~/.cabal/bin:/opt/ghc/$GHCVER/bin:/opt/cabal/$CABALVER/bin:/opt/happy/$HAPPYVER/bin:/opt/alex/$ALEXVER/bin:$PATH
  set +x
}

coda_install() {
  set -x
  cabal --version
  echo "$(ghc --version) [$(ghc --print-project-git-commit-id 2> /dev/null || echo '?')]"
  if [ -f $HOME/.cabal/packages/hackage.haskell.org/00-index.tar.gz ]; then
    zcat $HOME/.cabal/packages/hackage.haskell.org/00-index.tar.gz > $HOME/.cabal/packages/hackage.haskell.org/00-index.tar
  fi
  BENCH=${BENCH---enable-benchmarks}
  TEST=${TEST---enable-tests}
  travis_retry cabal update
  sed -i  's/^jobs:.*$/jobs: 2/' $HOME/.cabal/config
  # time cabal install --only-dependencies --enable-tests
  time cabal install --only-dependencies --enable-tests --dry -v $CABALFLAGS> installplan.txt;
  sed -i -e '1,/^Resolving /d' installplan.txt; cat installplan.txt;
  if diff -u installplan.txt $HOME/.cabsnap/installplan.txt; then
    echo "cabal build-cache HIT";
    rm -rfv .ghc;
    cp -a $HOME/.cabsnap/ghc $HOME/.ghc;
    cp -a $HOME/.cabsnap/lib $HOME/.cabsnap/share $HOME/.cabsnap/bin $HOME/.cabal/;
  else
    echo "cabal build-cache MISS";
    rm -rf $HOME/.cabsnap;
    mkdir -p $HOME/.ghc $HOME/.cabal/lib $HOME/.cabal/share $HOME/.cabal/bin;
    cabal install --only-dependencies --enable-tests --enable-benchmarks $CABALFLAGS;
  fi
  if [ ! -d $HOME/.cabsnap ]; then
    echo "snapshotting package-db to build-cache";
    mkdir $HOME/.cabsnap;
    cp -a $HOME/.ghc $HOME/.cabsnap/ghc;
    cp -a $HOME/.cabal/lib $HOME/.cabal/share $HOME/.cabal/bin installplan.txt $HOME/.cabsnap/;
  fi
  set +x
}

coda_script() {
  set -x
  time cabal configure --enable-tests
  time cabal build
  time cabal test --show-details=direct
  time cabal sdist || true
  unzip -v dist/build/*.vsix
  # confirm we can build off of the sdist
  SRC_TGZ=$(cabal info . | awk '{print $2;exit}').tar.gz;
  if [ -f $SRC_TGZ ]; then (cd dist && cabal install --force-reinstalls "$SRC_TGZ"); fi;
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

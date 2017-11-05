#!/usr/bin/env bash

# temporary hack to extend the path to find it in a dist-newstyle folder on my machine without install
BIN_DIR=${0%/*}
ABS_BIN_DIR=`cd "$BIN_DIR"; pwd`
BASE_DIR=${ABS_BIN_DIR%/*}
export PATH="$PATH:$BASE_DIR/dist-newstyle/build/x86_64-osx/ghc-8.2.1/coda-0.0.1/c/coda/build/coda"
export CODA_SERVER_PATH=`which coda`

if [ "X" = "X$CODA_SERVER_PATH" ]; then
  sleep 5
  echo "Content-Length: 97\r\n\r"
  echo '{"jsonrpc":"2.0","id":1,"error":{"code":-32099,"message":"Cannot find coda in the path"}}'
  exit 1
fi

exec coda $@

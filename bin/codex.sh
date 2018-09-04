#!/usr/bin/env bash
EXE=codex
cabal new-build exe:$EXE -v0 -- $* 1>&2
ABSEXE=`cabal new-exec which $EXE`
if [ ! -x "$ABSEXE" ]; then
  sleep 5
  echo "Content-Length: 87\r\n\r"
  echo '{"jsonrpc":"2.0","id":1,"error":{"code":-32099,"message":"Cannot build codex"}}'
  exit 1
fi

exec $ABSEXE $@

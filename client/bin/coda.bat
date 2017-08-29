@echo off

set CODA_SERVER_PATH=
for /f "delims=" %%p in ('where hie') do set CODA_SERVER_PATH=%%p

if [%CODA_SERVER_PATH%] == [] (
  echo Content-Length: 101
  echo:
  echo {"jsonrpc":"2.0","id":1,"error":{"code":-32099,"message":"Cannot find coda.exe in the path"}}
  exit 1
)

coda %1 %2 %3 %4 %5 %6 %7 %8 %9

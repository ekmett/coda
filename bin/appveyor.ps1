$ErrorActionPreference = "Stop"
$env:PATH += ";$PWD"
$env:STACK_ROOT = 'c:\\sr'
Invoke-WebRequest 'http://www.stackage.org/stack/windows-i386' -OutFile 'stack.zip'
7z x -y stack.zip stack.exe
cmd /c '.\stack path 2>&1'
cmd /c '.\stack setup 1>&2 2>&1 > nul'
$env:APPVEYOR_SAVE_CACHE_ON_ERROR = "true"
cmd /c 'echo | .\stack --no-terminal build --test --bench --ghc-options=-rtsopts'
cmd /c '.\stack install'

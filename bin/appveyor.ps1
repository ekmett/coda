$ErrorActionPreference = "Stop"
$env:PATH += ";$PWD"
$env:STACK_ROOT = 'c:\\sr'
"> download stack"
Invoke-WebRequest 'http://www.stackage.org/stack/windows-i386' -OutFile 'stack.zip'
"> unzip stack"
7z x -y stack.zip stack.exe
"> stack path"
cmd /c '.\stack path 2>&1'
"> stack setup"
cmd /c '.\stack setup 2>&1 1>&2 > nul'
$env:APPVEYOR_SAVE_CACHE_ON_ERROR = "true"
"> stack build"
cmd /c 'echo | .\stack --no-terminal build --test --bench --ghc-options=-rtsopts'
"> stack install"
cmd /c '.\stack install'

param(
  [string]$phase
)

$ErrorActionPreference = "Stop"
$env:PATH += ";$PWD"
$env:STACK_ROOT = 'c:\sr'
$env:GNUPGHOME = '/c/project/coda/ci'
$env:STACK_YAML = 'etc/stack.yaml'

if ($phase -eq "install") {
  "> download stack"
  if ($env:PLATFORM -eq "x64") {
    Invoke-WebRequest 'http://www.stackage.org/stack/windows-x86_64' -OutFile 'stack.zip'
  } else {
    Invoke-WebRequest 'http://www.stackage.org/stack/windows-i386' -OutFile 'stack.zip'
  }
  "> unzip stack"
  7z x -y stack.zip bin/stack.exe
} elseif($phase -eq "build_script") {
  "> stack setup"
  cmd /c 'bin\stack setup 2>&1 1>&2 > nul'
  "> stack path"
  cmd /c 'bin\stack path 2>&1'
  "> stack exec env"
  cmd /c 'bin\stack exec env 2>&1'
  $env:APPVEYOR_SAVE_CACHE_ON_ERROR = "true"
  "> stack build"
  cmd /c 'echo | bin\stack --no-terminal build --test --bench --ghc-options=-rtsopts 2>&1'
}

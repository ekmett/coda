param(
  [string]$phase
)

$ErrorActionPreference = "Stop"
$env:PATH += ";$PWD"
$env:STACK_ROOT = 'c:\sr'
$env:GNUPGHOME = '/c/project/coda'

if ($phase -eq "install") {
  "> download stack"
  if ($env:PLATFORM -eq "x64") {
    Invoke-WebRequest 'http://www.stackage.org/stack/windows-x86_64' -OutFile 'stack.zip'
  } else {
    Invoke-WebRequest 'http://www.stackage.org/stack/windows-i386' -OutFile 'stack.zip'
  }
  "> unzip stack"
  7z x -y stack.zip stack.exe
  if ($LASTEXITCODE -ne 0) {exit 1}
  Move-Item -Path stack.exe -Destination bin\stack.exe
  if ($LASTEXITCODE -ne 0) {exit 1}
} elseif($phase -eq "build_script") {
  "> stack setup"
  cmd /c 'bin\stack setup 2>&1 1>&2 > nul'
  if ($LASTEXITCODE -ne 0) {exit 1}
  "> stack path"
  cmd /c 'bin\stack path 2>&1'
  if ($LASTEXITCODE -ne 0) {exit 1}
  "> stack exec env"
  cmd /c 'bin\stack exec env 2>&1'
  if ($LASTEXITCODE -ne 0) {exit 1}
  $env:APPVEYOR_SAVE_CACHE_ON_ERROR = "true"
  "> stack build"
  cmd /c 'echo | bin\stack --no-terminal build --ghc-options=-rtsopts 2>&1'
  if ($LASTEXITCODE -ne 0) {exit 1}
}

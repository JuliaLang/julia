$downloadUrl32bit = 'http://s3.amazonaws.com/julianightlies/bin/winnt/x86/0.3/julia-VERSIONVERSION-win32.exe'
$downloadUrl64bit = 'http://s3.amazonaws.com/julianightlies/bin/winnt/x64/0.3/julia-VERSIONVERSION-win64.exe'

$juliapath = (Join-Path (Get-BinRoot) "julia")

$silentArgs = "/S /D=$juliapath"

Install-ChocolateyPackage 'julia' 'exe' $silentArgs $downloadUrl32bit $downloadUrl64bit

$juliapath = (Join-Path (Get-BinRoot) "julia")

$uninstallerpath = (Join-Path $juliapath 'Uninstall.exe')

$silentArgs = '/S'

Uninstall-ChocolateyPackage 'julia' 'exe' $silentArgs $uninstallerpath

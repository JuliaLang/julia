$downloadUrl32bit = 'http://sourceforge.net/projects/juliachocolateybins/files/julia-JULIAVERSIONJULIAVERSION-COMMITCOMMIT-x86.7z/download'
$downloadUrl64bit = 'http://sourceforge.net/projects/juliachocolateybins/files/julia-JULIAVERSIONJULIAVERSION-COMMITCOMMIT-x86_64.7z/download'



$juliapath = (Join-Path (Get-BinRoot) "julia")
$juliaexepath = (Join-Path (Join-Path $juliapath "bin") "julia.exe")

Install-ChocolateyZipPackage 'julia' $downloadUrl32bit $juliapath $downloadUrl64bit
Generate-BinFile 'julia' $juliaexepath

$juliapath = (Join-Path (Get-BinRoot) "julia")
$juliaexepath = (Join-Path (Join-Path $juliapath "bin") "julia.exe")

Remove-BinFile 'julia' $juliaexepath
Remove-Item $juliapath -Force -Recurse

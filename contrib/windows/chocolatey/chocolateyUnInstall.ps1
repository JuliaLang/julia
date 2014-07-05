try
{
    $juliapath = (Join-Path (Get-BinRoot) "julia")
    $juliaexepath = (Join-Path (Join-Path $juliapath "bin") "julia.exe")
    $startmenufolder = [System.Environment]::GetFolderPath([System.Environment+SpecialFolder]::StartMenu)
    $shortcutfilename = (Join-Path $startmenufolder "Julia.lnk")

    Remove-Item $shortcutfilename -Force -ErrorAction SilentlyContinue
    Remove-BinFile 'julia' $juliaexepath
    Remove-Item $juliapath -Force -Recurse

    Write-ChocolateySuccess 'julia'
}
catch
{
    Write-ChocolateyFailure 'julia' $($_.Exception.Message)
    throw
}

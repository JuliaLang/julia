try
{
    $downloadUrl32bit = 'http://sourceforge.net/projects/juliachocolateybins/files/julia-JULIAVERSIONJULIAVERSION-COMMITCOMMIT-x86.7z/download'
    $downloadUrl64bit = 'http://sourceforge.net/projects/juliachocolateybins/files/julia-JULIAVERSIONJULIAVERSION-COMMITCOMMIT-x86_64.7z/download'

    $juliapath = (Join-Path (Get-BinRoot) "julia")
    $juliaexepath = (Join-Path (Join-Path $juliapath "bin") "julia.exe")
    $startmenufolder = [System.Environment]::GetFolderPath([System.Environment+SpecialFolder]::StartMenu)
    $shortcutfilename = (Join-Path $startmenufolder "Julia.lnk")
    $mydocumentsfolder = [System.Environment]::GetFolderPath([System.Environment+SpecialFolder]::MyDocuments)

    Remove-Item $shortcutfilename -Force -ErrorAction SilentlyContinue
    Remove-BinFile 'julia' $juliaexepath
    Remove-Item $juliapath -Force -Recurse -ErrorAction SilentlyContinue

    Install-ChocolateyZipPackage 'julia' $downloadUrl32bit $juliapath $downloadUrl64bit
    Generate-BinFile 'julia' $juliaexepath
    $WshShell = New-Object -comObject WScript.Shell
    $Shortcut = $WshShell.CreateShortcut($shortcutfilename)
    $Shortcut.TargetPath = $juliaexepath
    $shortcut.WorkingDirectory = $mydocumentsfolder
    $shortcut.Description = "The Julia Language"
    $Shortcut.Save()

    Write-ChocolateySuccess 'julia'
}
catch
{
    Write-ChocolateyFailure 'julia' $($_.Exception.Message)
    throw
}

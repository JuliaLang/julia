$setup = "setup-$env:ARCH.exe".Replace("i686", "x86")
if ( $env:ARCH -eq "x86_64" ) {
$cygwin = "C:\cygwin64"
} else {
$cygwin = "C:\cygwin"
}

mkdir -Force C:\cygdownloads | Out-Null
if ( Test-Path "$cygwin\$setup" ) {
Copy-Item "$cygwin\$setup" "C:\cygdownloads\$setup"
} else {
(new-object net.webclient).DownloadFile(
  "http://cygwin.com/$setup", "C:\cygdownloads\$setup")
}

& "C:\cygdownloads\$setup" -q -n -R C:\cygwin-$env:ARCH `
  -l C:\cygdownloads -s http://mirrors.kernel.org/sourceware/cygwin -g -I `
  -P "make,curl,time,p7zip,mingw64-$env:ARCH-gcc-g++,mingw64-$env:ARCH-gcc-fortran" | Where-Object `
  -FilterScript {$_ -notlike "Installing file *"} | Write-Output

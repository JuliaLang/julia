$setup = "setup-$env:ARCH.exe".Replace("i686", "x86")

mkdir -Force C:\cygdownloads | Out-Null
(new-object net.webclient).DownloadFile(
  "http://cygwin.com/$setup", "C:\cygdownloads\$setup")
& "C:\cygdownloads\$setup" -q -n -R C:\cygwin-$env:ARCH `
  -l C:\cygdownloads -s http://mirrors.mit.edu/cygwin -g -I `
  -P "make,curl,time,p7zip,mingw64-$env:ARCH-gcc-g++,mingw64-$env:ARCH-gcc-fortran" | Where-Object `
  -FilterScript {$_ -notlike "Installing file *"} | Write-Output

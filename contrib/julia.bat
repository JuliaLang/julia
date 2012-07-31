@echo off
@rem
@rem  This file is intended to simplify launching Julia on Windows.
@rem
@rem  It sets the path as needed to reference various lib and bin
@rem  files, and builds sys.ji if needed.
@rem
@rem  If you move this file or alter its working directory, change 
@rem  the line "cd .." to change the directory to julia's base directory
rem

set CWD=%cd%

cd ..

set JULIA_HOME=%CD%/usr/bin

set PATH=.;%PATH;C:\MinGW\bin;C:\MinGW\lib;%JULIA_HOME%\..\lib;C:\Program Files (x86)\Git\bin;C:\Python27

if not exist %JULIA_HOME%\..\lib\julia\sys.ji (echo "Preparing Julia for first launch. This may take a while" &&  cd base && ..\julia.exe -b sysimg.jl && cd ..)

%rem usr\bin\julia-release-basic.exe
julia.exe

cd %CWD%

@echo on

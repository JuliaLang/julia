@echo off
@rem 
@rem  This file is intended to simplify launching Julia on Windows.
@rem
@rem See prepare-julia-env.bat for more info

pushd %cd%
setlocal enableextensions enabledelayedexpansion
call "%~dp0prepare-julia-env.bat" %*
call "%JULIA_HOME%julia-release-readline.exe" %*
endlocal
popd

@echo on

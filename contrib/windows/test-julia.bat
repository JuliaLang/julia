@echo off
@rem 
@rem  This file is intended to simplify launching Julia on Windows.
@rem
@rem See prepare_env.bat for more info

pushd %cd%
setlocal enableextensions enabledelayedexpansion
call %~dp0prepare_env.bat %*
cd %JULIA_HOME%..\lib\julia\test
call %JULIA_HOME%julia-release-readline.exe runtests.jl all
endlocal
popd
pause
@echo on

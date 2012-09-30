@echo off
@rem 
@rem  This file is intended to simplify launching Julia on Windows.
@rem
@rem See prepare_env.bat for more info

pushd %cd%
setlocal enableextensions enabledelayedexpansion
call %~dp0prepare_env.bat %*
%JULIA% %*
endlocal
%popd

@echo on

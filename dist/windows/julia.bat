@echo off
cd /d %~dp0
set PATH=lib;%PATH%
bin\julia %*
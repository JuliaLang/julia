@echo off

pushd %cd%
setlocal enableextensions enabledelayedexpansion
call %~dp0prepare_env.bat %*

if exist %JULIA_HOME%..\sbin\nginx.exe goto nginx
if exist %JULIA_HOME%..\sbin\lighttpd.exe goto lighttpd

:error
echo "Please install either lighttpd or nginx into usr/sbin"
goto end

:nginx
pushd %cd%
cd %JULIA_HOME%..\sbin
start nginx -c %JULIA_HOME%..\etc\nginx.conf
popd
goto julia

:lighttpd
start %JULIA_HOME%..\sbin\lighttpd -D -f %JULIA_HOME%..\etc\lighttpd.conf -m %JULIA_HOME%..\lib
goto julia

:julia
echo "Connect to http://localhost:2000/ for the web REPL."
start http://localhost:2000/
cd %JULIA_HOME%..\bin
julia-release-webserver.exe -p 2001 

:end
endlocal
%popd
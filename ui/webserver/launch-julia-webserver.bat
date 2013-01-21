@echo off

pushd %cd%
setlocal enableextensions enabledelayedexpansion
call %~dp0prepare-julia-env.bat %*

if exist %JULIA_HOME%..\sbin\nginx.exe goto nginx
if exist %JULIA_HOME%..\sbin\lighttpd.exe goto lighttpd

:error
echo Please install either lighttpd or nginx into usr/sbin
goto end

:nginx
pushd %cd%
cd %JULIA_HOME%..\sbin
start /b nginx -c %JULIA_HOME%..\etc\nginx.conf
popd
goto julia

:lighttpd
start /b %JULIA_HOME%..\sbin\lighttpd -D -f %JULIA_HOME%..\etc\lighttpd.conf -m %JULIA_HOME%..\lib
goto julia

:julia
echo Connect to http://localhost:2000/ for the web REPL.
echo Press Ctrl-C to quit, then answer N to prompt
start /b http://localhost:2000/
cd %JULIA_HOME%..\bin
call julia-release-webserver.exe -p 2001 

:end
echo Killing nginx... (this can take a few seconds)
for /F "delims=" %%a in (%JULIA_HOME%../sbin/logs/nginx.pid) do taskkill /f /t /pid %%a
sleep 1
echo Exiting...
endlocal
popd
pause

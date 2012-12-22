@echo off
cd /d %~dp0
if exist ..\sbin\lighttpd.exe goto lighttpd
if exist ..\sbin\nginx.exe goto nginx

:error
echo "Please install either lighttpd or nginx into usr/sbin"

:nginx
start ..\sbin\nginx -c ..\etc\nginx.conf
goto julia

:lighttpd
start ..\sbin\lighttpd -D -f ..\etc\lighttpd.conf -m ..\lib
goto julia

:julia
echo "Connect to http://localhost:2000/ for the web REPL."
julia-release-webserver -p 2001 

:end
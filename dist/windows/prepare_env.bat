@rem
@rem  This file attempts to auto detect and configure the environment
@rem  for starting julia and julia-web-server
@rem
@rem  It sets the path as needed to reference various lib and bin
@rem  files, and builds sys.ji if needed.
@rem

set SYS_PATH=%PATH%
set PATH=%~dp0usr\bin;%~dp0..\usr\bin;%~dp0..\..\usr\bin
set JULIA_EXE=julia-release-readline.exe
for %%A in (%JULIA_EXE%) do set JULIA_HOME=%%~dp$PATH:A
set JULIA=%JULIA_HOME%%JULIA_EXE%
set PATH=.;%SYS_PATH%;C:\MinGW\bin;C:\MinGW\lib;%JULIA_HOME%\..\lib;C:\Program Files\Git\bin;C:\Program Files (x86)\Git\bin;C:\Python27;C:\Python26

if not exist %JULIA_HOME%..\lib\julia\sys.ji (echo "Preparing Julia for first launch. This may take a while" && cd %JULIA_HOME%..\lib\julia\base && %JULIA% -b sysimg.jl && popd && pushd %cd%)

@rem
@rem  This file attempts to auto detect and configure the environment
@rem  for starting julia and julia-web-server
@rem
@rem  It sets the path as needed to reference various lib and bin
@rem  files, and builds sys.ji if needed.
@rem

set SYS_PATH=%PATH%
set PATH=%~dp0bin;%~dp0usr\bin;%~dp0..\usr\bin;%~dp0..\..\usr\bin
set JULIA_EXE=julia-release-readline.exe
for %%A in (%JULIA_EXE%) do set JULIA_HOME=%%~dp$PATH:A
set JULIA=%JULIA_HOME%%JULIA_EXE%
set PATH=%JULIA_HOME%;%JULIA_HOME%..\lib\julia;%JULIA_HOME%..\lib;.;%SYS_PATH%;%~dp0\Git\bin
set HOME=%APPDATA%\julia
set JL_PRIVATE_LIBDIR=lib\julia

if not exist "%JULIA_HOME%..\lib\julia\sys.ji" (echo "Preparing Julia for first launch. This may take a while" && echo "You may see two git related errors. This is completely normal" && cd "%JULIA_HOME%..\share\julia\base" && "%JULIA%" -b sysimg.jl && popd && pushd "%cd%")

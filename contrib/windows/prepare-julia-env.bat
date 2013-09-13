@rem
@rem  This file attempts to auto detect and configure the environment
@rem  for starting julia and julia-web-server
@rem
@rem  It sets the path as needed to reference various lib and bin
@rem  files, and builds sys.ji if needed.
@rem

set SYS_PATH=%PATH%
set PATH=%~dp0bin;%~dp0usr\bin;%~dp0..\usr\bin;%~dp0..\..\usr\bin;%SYS_PATH%
set JULIA_EXE=julia-readline.exe
for %%A in (%JULIA_EXE%) do set JULIA_HOME=%%~dp$PATH:A
set JULIA=%JULIA_HOME%%JULIA_EXE%
set PATH=%JULIA_HOME%;%JULIA_HOME%..\lib\julia;%JULIA_HOME%..\lib;.;%SYS_PATH%;%~dp0\Git\bin;C:\MinGW\msys\1.0\bin;C:\MinGW\bin;C:\Program Files\Git\bin;C:\Program Files (x86)\Git\bin;C:\Python27;C:\Python26;C:\Python25
set JL_PRIVATE_LIBDIR=lib\julia
set JULIA_EDITOR=start

if not exist "%JULIA_HOME%..\lib\julia\sys.ji" (echo "Preparing Julia for first launch. This may take a while" && echo "You may see two git related errors. This is completely normal" && cd "%JULIA_HOME%..\share\julia\base" && "%JULIA%" -b sysimg.jl && popd && pushd "%cd%")

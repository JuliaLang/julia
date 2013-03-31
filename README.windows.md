General Information for Windows
===============================

Please see the README at https://github.com/JuliaLang/julia/blob/master/README.md for more complete information about Julia. This is intended to only include information specific to using Julia on Windows.

Julia runs on Windows XP SP2 or later (including Windows Vista, Windows 7, and Windows 8). Both the 32-bit and 64-bit versions are supported. The 32-bit i686 binary will run on either 32-bit and 64-bit operating systems. The 64-bit x86_64 binary will only run on 64-bit Windows.

Downloading additional libraries (Tk, Cairo, etc) is not necessary. Julia's package manager will acquire them as needed.

Julia requires that the lib and lib/julia directories be part of your `%PATH%` variable to startup. The `julia.bat` script will attempt to do this for you and is the recommended way of running julia on your system. The julia.bat file can be given arguments (e.g. `julia.bat -p 2 script.jl` for running script.jl on two processors) which will be passed directly to julia.exe.

___________________________________________________
Binary Downloads
================

Download the latest version of Julia from the downloads page at https://code.google.com/p/julialang/downloads/list

Unzip the download to a folder. Do not attempt to run Julia without extracting the zip archive first (hint: it won't work). Double-click the file Julia.BAT to launch Julia.

Explore and have fun!

Recommended external libraries:

 - msysGit https://code.google.com/p/msysgit/downloads/list 
 - TortoiseGit https://code.google.com/p/tortoisegit/wiki/Download

Optional external libraries

 - MinGW/MSYS (as described below)

___________________________________________________
Source Compiling
================

There are a number of environments you can use to build julia. Making this easy requires getting the right environment. They are ordered below from worst to best.

Native Compile
--------------

On Windows, you can get the mingw/msys environment from http://www.mingw.org/wiki/Getting_Started. I strongly discourage this. However, if you choose to ignore me, you will need to do the following steps (thanks to Pavan):

1. Download MinGW+ MSYS Innosetup installer from this url 
2. Run this installer to choose from options: Destination Directory (Default- C:\MinGW), MinGW compiler suite (selected C, C++, Fortran), MSYS Basic System (selected), MinGW Developer Toolkit packages (selected). Based on your connection speed, it may take a while to download packages.
3. Set path by choosing ControlPanel > System and Security > System > Advanced System Settings > (Advanced) Environment Variables > Path and add to the end of line: C:\MinGW\bin;C:\MinGW\msys\1.0\bin;
4. At command prompt, type - "mingw-get install wget" - to install wget
5. Install 7-zip (http://www.7-zip.org/download.html), if you don't have it already
5. Download Python-2.6.7_msys.7z from [here](https://osspack32.googlecode.com/files/python-2.6.7_msys.7z) and extract files (using 7-Zip) to respective locations in C:\MinGw\msys\1.0 path, i.e., bin, lib, libs, to get Python 2.6.7
6. Add the following line -  _CRTIMP int __cdecl _resetstkoflw (void); - to C:\MinGW\include\malloc.h at Line 76. This helped in tackling _resetstkoflw errors being showed in compiling codegen.cpp

The recommended way to setup your environment follows:

1. Download and extract mingw to C:/MinGW (or similar location) from http://sourceforge.net/projects/mingwbuilds/files/external-binary-packages/
2. Download and extract msys to C:/MinGW/msys/1.0 (or similar location) from http://sourceforge.net/projects/mingwbuilds/files/host-windows/releases/4.7.2/64-bit/threads-win32/sjlj/
3. Add the line "C:/MinGW /mingw" to C:/MinGW/msys/1.0/etc/fstab (create the file if it doesn't exist)

These sections assume you are familiar with building code. If you are not, you should stop reading now and go the the section on binaries. Regardless of which set of steps you followed above, you are now ready to compile julia. Open a unix shell by launching C:/MinGW/msys/1.0/msys.bat (or your favorite shortcut to that file). 

Run the following commands in your build directory ($HOME at C:/MinGW/msys/1.0/home/your_name is fine)
1. `git clone https://github.com/JuliaLang/julia.git`
2. `cd julia`
3. `make`
NEVER use the `-j` argument to make. Windows will sometimes corrupt your build files. Additionally, make will probably lock up several times during the process, using 100% cpu, but not show progress. The only solution appears to be to kill make from the Task Manager and rerunning make. It should pickup where it left off. Expect this to take a very long time (dozens of hours is not uncommon).

Running julia can be done in two ways:
1. `make run-julia[-release|-debug] [DEFAULT_REPL=(basic|readline)]` (e.g. `make run-julia`)
2. Launching the julia.bat script in usr/bin

Cross-Compile
-------------

If you prefer to cross-compile (i.e. you are sane), the following steps should get you started:

First, you will need to ensure your system has the required dependencies. Note that I build on an Ubuntu system, so the `make dist` may not be fully functional on other systems. On Ubuntu 12.04, the following command will install the required build dependencies.

```
apt-get install \
  mingw-w64 \
  gfortran-mingw-w64 \
  g++-mingw-w64 \
  gdb-mingw-w64 \
  mingw-w64-tools \
  binutils-mingw-w64 \
  gcc-mingw-w64 \
  wine
```

Finally, the build and install process:

1. `git clone https://github.com/JuliaLang/julia.git julia-win32`
2. `echo override XC_HOST = i686-w64-mingw32 >> Make.user`
3. `echo override DEFAULT_REPL = basic >> Make.user`
4. `make && make install`
5. (optional) `mkdir dist-extras && make win-extras` (actually, you probably want to hand execute these steps)
6. move the julia-* directory to the target machine
7. Ensure your target has the following dependencies "libgfortran-3.dll" "libquadmath-0.dll" "libgcc_s_sjlj-1.dll" "libstdc++-6.dll" "libssp-0.dll" in the PATH (or in installed julia-* lib folder). These can be copied from the your unix system (on my Ubuntu 12.04 system, they are at /usr/lib/gcc/i686-w64-mingw32/4.6/) or, if you followed my advice above to setup MinGW, they are are in C:/MinGW/bin on your Windows system.

If you are building for 64-bit windows. The steps are pretty much the same. Just replace i686 in XC_HOST with x86_64.

Important Build Errata
----------------------

- Arpack 3.1.2 has an error in the tar file (a symlink from ./depcomp to a nonexistant file) that prevents it from being extracted on windows (cross-build is fine). To resolve this, we need a tar file that doesn't contain this symlink file. I found the easiest way to solve this was to run `make -C deps get-arpack` and then edit the arpack-*.tar.gz in 7zip to delete the depcomp symlink file before running the primary make target.

- LLVM 3.x has an error on when compiling for 64-bit Windows that causes the build to fail when building the shared library file. Therefore, before building, you will need to change `--enable-shared` to `--disable-shared` in the `LLVM_FLAGS` variable in `deps/Makefile`.

- The cross-build does not have access to git. If you want your sysimg to contain version information from the commit, remove the `#` from the line in Makefile that begins with @#echo `git rev-parse --short HEAD ...` and ends with `... > COMMIT`


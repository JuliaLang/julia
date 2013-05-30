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

 - [msysGit](https://code.google.com/p/msysgit/downloads/list)
 - [TortoiseGit](https://code.google.com/p/tortoisegit/wiki/Download)

Optional external libraries

 - MinGW/MSYS (as described below)

___________________________________________________
Source Compiling
================

There are a few environments you can use to build julia. Making this easy requires getting the right environment.

Native Compile
--------------

On Windows, do not use the mingw/msys environment from http://www.mingw.org as it will miscompile the OpenBLAS math library

The recommended way to setup your environment follows:

1. Download and extract MinGW (e.g. x64-4.8.0-release-win32-seh-rev2.7z) to C:/MinGW (or similar location) from
MinGW-builds [32-bit](http://sourceforge.net/projects/mingwbuilds/files/host-windows/releases/4.8.0/32-bit/threads-win32/sjlj/)
or [64-bit](http://sourceforge.net/projects/mingwbuilds/files/host-windows/releases/4.8.0/64-bit/threads-win32/seh/) 
2. Download and extract MSYS (e.g. msys+7za+wget+svn+git+mercurial+cvs-rev12.7z) to C:/MinGW/msys/1.0 (or similar location) from [MinGW-w64/MSYS](http://sourceforge.net/projects/mingwbuilds/files/external-binary-packages/)
3. Add the line "C:/MinGW /mingw" to C:/MinGW/msys/1.0/etc/fstab (create the file if it doesn't exist)
4. You may need to replace C:/MinGW/msys/1.0/bin/make.exe with C:/MinGW/msys/1.0/bin/make-old.exe or with a copy of make.exe extracted from [mingw-msys](http://sourceforge.net/projects/mingw/files/MSYS/Base/make/make-3.81-3/) (e.g. make-3.81-3-msys-1.0.13-bin.tar.lzma) if the build does not start correctly

These sections assume you are familiar with building code. If you are not, you should stop reading now and go the the section on binaries. Regardless of which set of steps you followed above, you are now ready to compile julia. Open a unix shell by launching C:/MinGW/msys/1.0/msys.bat (or your favorite shortcut to that file). 

Run the following commands in your build directory ($HOME at C:/MinGW/msys/1.0/home/your_name is fine)

1. `git clone https://github.com/JuliaLang/julia.git`
2. `cd julia`
3. `make` Avoid using the `-j` argument to make. Windows will sometimes corrupt your build files. Additionally, make will probably lock up several times during the process, using 100% cpu, but not show progress. The only solution appears to be to kill make from the Task Manager and rerunning make. It should pickup where it left off. Expect this to take a very long time (dozens of hours is not uncommon).

Running julia can be done in two ways:

1. `make run-julia[-release|-debug] [DEFAULT_REPL=(basic|readline)]` (e.g. `make run-julia`)
2. Launching the julia.bat script in usr/bin

Cross-Compile
-------------

If you prefer to cross-compile, the following steps should get you started:

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

Unfortunately, the version of gcc installed by Ubuntu is currently 4.6, which does not compile OpenBLAS correctly. So first we need to replace it. Most binary packages appear to not include gfortran, so we will need to compile it from source. This is typically quite a bit of work, so we will use [this script](https://code.google.com/p/mingw-w64-dgn/) to make it easier.

0. `apt-get install wine subversion cvs`
1. `svn checkout http://mingw-w64-dgn.googlecode.com/svn/trunk/ mingw-w64-dgn`
2. `cd mingw-w64-dgn`
3. edit `rebuild_cross.sh` and make the following two changes:
(a) uncomment `export MAKE_OPT="-j 2"`, if appropriate for your machine
(b) add `fortran` to the end of `--enable-languages=c,c++,objc,obj-c++`
5. `bash update_source.sh`
4. `bash rebuild_cross.sh`
5. `mv cross ~/cross-w64`
6. `export PATH=$HOME/cross-w64/bin:$PATH` # NOTE: it is important that you remember to always do this before using make in the following steps!
Then we can essentially just repeat these steps for the 32-bit compiler:
7. `cd ..`
8. `cp -a mingw-w64-dgn mingw-w32-dgn`
9. `cd mingw-w32-dgn`
10. `rm -r cross build`
11. `bash rebuild_cross.sh 32r`
12. `mv cross ~/cross-w32`
13. `export PATH=$HOME/cross-w32/bin:$PATH` # NOTE: it is important that you remember to always do this before using make in the following steps!

Note: for systems that support rpm-based package managers, the OpenSUSE build service appears to contain a fully up-to-date versions of the necessary environment.

Finally, the build and install process:

1. `git clone https://github.com/JuliaLang/julia.git julia-win32`
2. `echo override XC_HOST = i686-w64-mingw32 >> Make.user`
3. `echo override DEFAULT_REPL = basic >> Make.user`
4. `make -j4`
5. (optional) `mkdir dist-extras && make win-extras` (actually, you probably want to hand execute the steps in this recipe since they may be inaccurate)
4. `make dist`
6. move the julia-* directory/zipfile to the target machine

If you are building for 64-bit windows. The steps are essentially the same. Just replace i686 in XC_HOST with x86_64.

Important Build Errata
----------------------

- Do not use GCC 4.6 or earlier

- LLVM doesn't build with the newly released 4.8 SEH gcc for 64-bit Windows because of an incorrect preprocessor definition. In deps/llvm-3.2/lib/ExecutionEngine/JIT/JIT.cpp, find the section that defines HAVE_EHTABLE_SUPPORT and replace it with an unconditional 0

- While building on native windows, MPFR tests fail. To fix this, edit deps/Makefile and add `MPFR_OPTS += --disable-thread-safe CFLAGS="-DNPRINTF_L -DNPRINTF_T -DNPRINTF_J"` somewhere

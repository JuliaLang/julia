# General Information for Windows

This file describes how to use and build Julia on Windows.
For more information about Julia, please see the [main README](https://github.com/JuliaLang/julia/blob/master/README.md) 

# Binary distribution

Julia runs on Windows XP SP2 or later (including Windows Vista, Windows 7, and Windows 8). Both the 32-bit and 64-bit versions are supported. The 32-bit i686 binary will run on either 32-bit and 64-bit operating systems. The 64-bit x86_64 binary will only run on 64-bit Windows.

1. Download and install [7-Zip](http://www.7-zip.org/download.html). Install the full program, not just the command line version.

2. [Download](http://julialang.org/downloads) the latest version of Julia. Extract the binary to a reasonable destination folder, e.g. `C:\julia`.

3. Double-click the file `julia.bat` to launch Julia.

# Source distribution

## Building on Windows with MinGW-builds/MSYS

1. Download and install the full [7-Zip](http://www.7-zip.org/download.html) program.

2. Download and install the latest [Python 2.x](http://www.python.org/download/releases) release. Do not install Python 3.x.

3. Install [MinGW-builds](http://sourceforge.net/projects/mingwbuilds/), a Windows port of GCC. (Do not use the regular MinGW distribution.)
  1. Download the [MinGW-builds installer](http://downloads.sourceforge.net/project/mingwbuilds/mingw-builds-install/mingw-builds-install.exe) from the [MinGW-builds homepage](http://sourceforge.net/projects/mingwbuilds/). 
  2. Run the installer. When prompted, choose:
    - Version: the most recent version (these instructions were tested with 4.8.1)
    - Architecture: x32 or x64 as desired 
    - Threads: win32 (not posix)
    - Exception: sjlj (for x32) or seh (for x64). Do not choose dwarf2.
    - Build revision: most recent available (tested with 5)
  3. Do **not** install to a directory with spaces in the name. You will have to change the default installation path. Choose instead something like `C:\mingw-builds\x64-4.8.1-win32-seh-rev5\mingw64`.

4. Download and extract the [MSYS distribution for MinGW-builds](http://sourceforge.net/projects/mingwbuilds/files/external-binary-packages/) (e.g. msys+7za+wget+svn+git+mercurial+cvs-rev13.7z) to a directory *without* spaces in the name, e.g. `C:/mingw-builds/msys`.

5. Download the [MSYS distribution of make](http://sourceforge.net/projects/mingw/files/MSYS/Base/make) and use this  `make.exe` to replace the one in the `mingw64\bin` subdirectory of the MinGW-builds installation.

6. Run the `msys.bat` installed in Step 4. Set up MSYS by running at the MSYS prompt:

   ```
    mount C:/mingw-builds/x64-4.8.1-win32-seh-rev5/mingw64/bin /mingw
    mount C:/Python27 /python
    export PATH=$PATH:/mingw/bin:/python
   ```

   Replace the directories as appropriate.

7. Download the Julia source repository and build it
   ```
    git clone https://github.com/JuliaLang/julia.git
    cd julia
    make
   ```
   *Tips:* 
  - The MSYS build of `make` is fragile and will occasionally corrupt the build process. You can minimize the changes of this occurring by only running `make` in serial, i.e. avoid the `-j` argument.
  - When the build process fails for no apparent reason, try running `make` again.
  - Sometimes, `make` will appear to hang, consuming 100% cpu but without apparent progress. If this happens, kill `make` from the Task Manager and try again.
  - Expect this to take a very long time (dozens of hours is not uncommon).
  - If `make` fails complaining that `./flisp/flisp` is not found, force `make` to build FemtoLisp before Julia by running `make -C src/flisp && make`.

8. Run Julia with _either_ of:
  - Using `make`
    ```
    make run-julia
   ```
   (the full syntax is `make run-julia[-release|-debug] [DEFAULT_REPL=(basic|readline)]`)

  - Using the Julia executables directly
    ```
    usr/bin/julia-readline
```
    (or `julia-basic` if you prefer)

## Compiling with MinGW/MSYS2 (experimental)

1. Download and install [7-Zip](http://www.7-zip.org/download.html), [Python 2.x](http://www.python.org/download/releases) and [MinGW-builds](http://sourceforge.net/projects/mingwbuilds/) as described in the previous section.

2. Install and configure [MSYS2](http://sourceforge.net/projects/msys2), a minimal POSIX-like environment for Windows.
  1. Download the latest base [32-bit](http://sourceforge.net/projects/msys2/files/Base/32-bit) or [64-bit](http://sourceforge.net/projects/msys2/files/Base/64-bit) distribution as apprpriate.
  2. Using [7-Zip](http://www.7-zip.org/download.html), extract the archive to a convenient directory, e.g. **C:\msys2\x64-20131126**. You may need to extract the tarball in a separate step. This will create an additional `msys32`/`msys64` subdirectory.
    - Some versions of this archive contain zero-byte files that clash with existing files. If prompted, choose to not overwrite all existing files.
  3. Launch `msys2_shell.bat`, which will initialize MSYS2.
  4. Install the necessary packages:

     ```
    pacman-key --init #Download keys
    pacman -Syu #Update package database and full system upgrade
    pacman -S diffutils git m4 make patch python2 tar
    ln -s /usr/bin/python{2.exe,} #Fixes python2 not found error
```

  5. Edit the `/etc/fstab` file and append a line of the form

     ```
    C:/mingw-builds/x64-4.8.1-win32-seh-rev5/mingw64 /mingw ext3 binary 0 0
```

   Use the actual installation directory of MinGW from Step 2c. Consult the
  [Cygwin manual](http://cygwin.com/cygwin-ug-net/using.html#mount-table) for
  details of how to enter the directory name.

  6. Edit the `~/.bashrc` file and append the line
     ```   
    export PATH=$PATH:/mingw/bin
```

  7. `exit` the MSYS2 shell.

3. Build Julia and its dependencies from source.
  1. Relaunch the MSYS2 shell and type

     ```
    . ~/.bashrc #Some versions of MSYS2 do not run this automatically
    git clone https://github.com/JuliaLang/julia.git
    cd julia
    make
```

  2. Some versions of PCRE (e.g. 8.31) will compile correctly but have a single
  test fail with an error like `** Failed to set locale "fr_FR`
  which will break the entire build. To circumvent the test and allow the rest
  of the build to continue, create an empty `checked` file in the `deps/pcre*`
  directory and rerun `make`.


## Cross-compiling

If you prefer to cross-compile, the following steps should get you started.

### Ubuntu and Mac Dependencies (these steps will work for almost any linux platform)

First, you will need to ensure your system has the required dependencies. We need wine, a system compiler, and some downloaders.

On Ubuntu: ```apt-get install wine subversion cvs gcc wget p7zip-full```

On Mac: Install XCode, XCode command line tools, X11 (now [XQuartz](http://xquartz.macosforge.org/)),
and [MacPorts](http://www.macports.org/install.php) or [Homebrew](http://mxcl.github.io/homebrew/).
Then run ```port install wine wget``` or ```brew install wine wget```, as appropriate.

On Both:

Unfortunately, the version of gcc installed by Ubuntu is currently 4.6, which does not compile OpenBLAS correctly.
On Mac, the situation is the same: the version in MacPorts is very old and Homebrew does not have it. So first we need to get
a cross-compile version of gcc. Most binary packages appear to not include gfortran, so we will need to compile it
from source (or ask @vtjnash to send you a tgz of my build). This is typically quite a bit of work, so we will use
[this script](https://code.google.com/p/mingw-w64-dgn/) to make it easy. 

1. `svn checkout http://mingw-w64-dgn.googlecode.com/svn/trunk/ mingw-w64-dgn`
2. `cd mingw-w64-dgn`
3. edit `rebuild_cross.sh` and make the following two changes:
  a. uncomment `export MAKE_OPT="-j 2"`, if appropriate for your machine
  b. add `fortran` to the end of `--enable-languages=c,c++,objc,obj-c++`
5. `bash update_source.sh`
4. `bash rebuild_cross.sh`
5. `mv cross ~/cross-w64`
6. `export PATH=$HOME/cross-w64/bin:$PATH` # NOTE: it is important that you remember to always do this before using make in the following steps!, you can put this line in your .profile to make it easy

Then we can essentially just repeat these steps for the 32-bit compiler, reusing some of the work:

7. `cd ..`
8. `cp -a mingw-w64-dgn mingw-w32-dgn`
9. `cd mingw-w32-dgn`
10. `rm -r cross build`
11. `bash rebuild_cross.sh 32r`
12. `mv cross ~/cross-w32`
13. `export PATH=$HOME/cross-w32/bin:$PATH` # NOTE: it is important that you remember to always do this before using make in the following steps!, you can put this line in your .profile to make it easy

Note: for systems that support rpm-based package managers, the OpenSUSE build service appears to contain a fully up-to-date versions of the necessary dependencies.

### Arch Linux Dependencies

1. Install the following packages from the official Arch repository:
`sudo pacman -S cloog gcc-ada libmpc p7zip ppl subversion zlib`
2. The rest of the prerequisites consist of the mingw-w64 packages, which are available in the AUR Arch repository. They must be installed exactly in the order they are given or else their installation will fail. The `yaourt` package manager is used for illustration purposes; you may instead follow the [Arch instructions for installing packages from AUR](https://wiki.archlinux.org/index.php/Arch_User_Repository#Installing_packages) or may use your preferred package manager. To start with, install `mingw-w64-binutils` via the command
`yaourt -S mingw-w64-binutils`
3. `yaourt -S mingw-w64-headers-svn`
4. `yaourt -S mingw-w64-headers-bootstrap`
5. `yaourt -S mingw-w64-gcc-base`
6. `yaourt -S mingw-w64-crt-svn`
7. Remove `mingw-w64-headers-bootstrap` without removing its dependent mingw-w64 installed packages by using the command
`yaourt -Rdd mingw-w64-headers-bootstrap`
8. `yaourt -S mingw-w64-winpthreads`
9. Remove `mingw-w64-gcc-base` without removing its installed mingw-w64 dependencies:
`yaourt -Rdd mingw-w64-gcc-base`
10. Complete the installation of the required `mingw-w64` packages:
`yaourt -S mingw-w64-gcc`

### Cross-building Julia

Finally, the build and install process for Julia:

1. `git clone https://github.com/JuliaLang/julia.git julia-win32`
2. `echo override XC_HOST = i686-w64-mingw32 >> Make.user`
3. `echo override DEFAULT_REPL = basic >> Make.user`
4. `make`
5. `make win-extras` (Necessary before running `make dist`p)
4. `make dist`
6. move the julia-* directory / zip file to the target machine

If you are building for 64-bit windows, the steps are essentially the same. Just replace i686 in XC_HOST with x86_64. (note: on Mac, wine only runs in 32-bit mode)

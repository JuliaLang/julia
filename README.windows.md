# General Information for Windows

This file describes how to install, or build, and use Julia on Windows.
For more general information about Julia, please see the [main README](https://github.com/JuliaLang/julia/blob/master/README.md) or the [documentation](http://docs.julialang.org/).

# Unicode font support

The built-in Windows fonts have rather poor coverage of the Unicode character space.
The free [`DejaVu Sans Mono`](http://dejavu-fonts.org/) font can be used as a replacement font in the Windows console.
Since Windows 2000, simply downloading the font and installing it is insufficient, since Windows keeps a list of approved fonts in the registry.

Instructions for adding fonts to the terminal are available at:
[http://support.microsoft.com/default.aspx?scid=kb;EN-US;Q247815](http://support.microsoft.com/default.aspx?scid=kb;EN-US;Q247815)


Additionally, rather than sticking with the default command prompt, you may want to use a different terminal emulator program, such as [`Conemu`](https://code.google.com/p/conemu-maximus5/) or [`Mintty`](https://code.google.com/p/mintty/)
(note that running Julia on Mintty needs a copy of `stty.exe` in your `%PATH%` to work properly).
Alternatively, you may prefer the features of a more full-function IDE, such as [`LightTable`](https://github.com/one-more-minute/Jupiter-LT), [`Sublime-IJulia`](https://github.com/quinnj/Sublime-IJulia), [`IJulia`](https://github.com/JuliaLang/IJulia.jl), or [`Julia Studio`](http://forio.com/labs/julia-studio/).


# Binary distribution

Julia runs on Windows XP-SP2 and later (including Windows Vista, Windows 7, and Windows 8).
Both the 32-bit and 64-bit versions are supported.
The 32-bit (i686) binary will run on either a 32-bit and 64-bit operating system.
The 64-bit (x86_64) binary will only run on 64-bit Windows and will otherwise refuse to launch.

1. Download and install [7-Zip](http://www.7-zip.org/download.html). Install the full program, not just the command line version.

2. [Download](http://julialang.org/downloads) the latest version of Julia. Extract the binary to a reasonable destination folder, e.g. `C:\julia`.

3. Double-click the file `julia.bat` to launch Julia.

# Line endings

Julia uses binary-mode files exclusively. Unlike many other Window's programs, if you write '\n' to a file, you get a '\n' in the file, not some other bit pattern. This matches the behavior exhibited by other operating systems. If you have installed msysGit, it is suggested, but not required, that you configure your system msysGit to use the same convention:

    git config --global core.eol = lf
    git config --global core.autocrlf = input
    
or edit `%USERPROFILE%\.gitconfig` and add/edit the lines:
    
    [core] eol = lf
           autocrlf = input


# Source distribution

## Supported build platforms

- Windows 8: supported (32 and 64 bits)
- Windows 7: supported (32 and 64 bits)
- Windows Vista: unknown
- Windows XP: not supported (however, there have been some reports of success following the msys2 steps)

## Compiling with MinGW/MSYS2

### MSYS2 provides a robust MSYS experience.
### The instructions in this section were tested with the latest versions of all packages specified as of 2014-02-28.

1. Install [7-Zip](http://www.7-zip.org/download.html).

2. Install [Python 2.x](http://www.python.org/download/releases). Do **not** install Python 3.

3. Install [MinGW-builds](http://sourceforge.net/projects/mingwbuilds/), a Windows port of GCC, as follows. Do **not** use the regular MinGW distribution.
  1. Download the [MinGW-builds installer](http://downloads.sourceforge.net/project/mingwbuilds/mingw-builds-install/mingw-builds-install.exe).
  2. Run the installer. When prompted, choose:
    - Version: the most recent version (these instructions were tested with 4.8.1)
    - Architecture: `x32` or `x64` as appropriate and desired.
    - Threads: `win32` (not posix)
    - Exception: `sjlj` (for x32) or `seh` (for x64). Do not choose dwarf2.
    - Build revision: most recent available (tested with 5)
  3. Do **not** install to a directory with spaces in the name. You will have to change the default installation path, for example,
    - `C:\mingw-builds\x64-4.8.1-win32-seh-rev5` for 64 bits
    - `C:\mingw-builds\x32-4.8.1-win32-sjlj-rev5` for 32 bits

4. Install and configure [MSYS2](http://sourceforge.net/projects/msys2), a minimal POSIX-like environment for Windows.

  1. Download the latest base [32-bit](http://sourceforge.net/projects/msys2/files/Base/i686/) or [64-bit](http://sourceforge.net/projects/msys2/files/Base/x86_64/) distribution, consistent with the architecture you chose for MinGW-builds. The archive will have a name like `msys2-base-x86_64-yyyymmdd.tar.xz` and these instructions were tested with `msys2-base-x86_64-20140216.tar.xz`.

  2. Using [7-Zip](http://www.7-zip.org/download.html), extract the archive to any convenient directory.
    - *N.B.* Some versions of this archive contain zero-byte files that clash with existing files. If prompted, choose **not** to overwrite existing files.
    - You may need to extract the tarball in a separate step. This will create an `msys32` or `msys64` directory, according to the architecture you chose.
    - Move the `msys32` or `msys64` directory into your MinGW-builds directory, which is `C:\mingw-builds` if you followed the suggestions in step 3. We will omit the "32" or "64" in the steps below and refer to this as "the msys directory".

  3. Double-click `msys2_shell.bat` in the msys directory. This will initialize MSYS2. The shell will tell you to `exit` and restart the shell. For now, ignore it.

  4. Update MSYS2 and install packages required to build julia, using the `pacman` package manager included in MSYS2:

     ```
    pacman-key --init     #Download keys
    pacman -Syu           #Update package database and full system upgrade
```
    Now `exit` the MSYS2 shell and restart it,  *even if you already restarted it above*. This is necessary in case the system upgrade updated the main MSYS2 libs. Reopen the MSYS2 shell and continue with:

    ```
    pacman -S diffutils git m4 make patch tar msys/openssh
```

  5. Configure your MSYS2 shell for convenience:

     ```
    echo "mount C:/Python27 /python" >> ~/.bashrc
    # uncomment ONE of the following two lines
    #echo "mount C:/mingw-builds/x64-4.8.1-win32-seh-rev5/mingw64 /mingw" >> ~/.bashrc
    #echo "mount C:/mingw-builds/x32-4.8.1-win32-sjlj-rev5/mingw32 /mingw" >> ~/.bashrc
    echo "export PATH=/usr/local/bin:/usr/bin:/opt/bin:/mingw/bin:/python" >> ~/.bashrc
```

     *N.B.* The `export` clobbers whatever `$PATH` is already defined. This is suggested to avoid path-masking. If you use MSYS2 for purposes other than building Julia, you may prefer to append rather than clobber.

     *N.B.* All of the path-separators in the mount commands are unix-style.


  6. Configuration of the toolchain is complete. Now `exit` the MSYS2 shell.

5. Build Julia and its dependencies from source.
  1. Relaunch the MSYS2 shell and type

     ```
    . ~/.bashrc  # Some versions of MSYS2 do not run this automatically
```

     Ignore any warnings you see from `mount` about `/mingw` and `/python` not existing.

  2. Get the Julia sources and start the build:
    ```
    git clone https://github.com/JuliaLang/julia.git
    cd julia
    make -j 4   # Adjust the number of cores (4) to match your build environment.
```

  3. The Julia build can (as of 2014-02-28) fail after building OpenBLAS.
  This appears (?) to be a result of the OpenBLAS build trying to run the Microsoft Visual C++ `lib.exe` tool  -- which we don't need to do -- without checking for its existence.
    This uncaught error kills the Julia build. If this happens, follow the instructions in the helpful error message and continue the build, *viz.*

     ```
    cd deps/openblas-v0.2.9.rc1   # This path will depend on the version of OpenBLAS.
    make install
    cd ../..
    make -j 4   # Adjust the number of cores (4) to match your build environment.
```

  4. Some versions of PCRE (*e.g.* 8.31) will compile correctly but fail a test.
  This will cause the Julia build to fail. To circumvent testing for PCRE and allow the rest
  of the build to continue,
    ```
    touch deps/pcre-8.31/checked  # This path will depend on the version of PCRE.
    make -j 4   # Adjust the number of cores (4) to match your build environment.
```
6. Setup Package Development Environment
  1. The `Pkg` module in Base provides many convenient tools for [developing and publishing packages](http://docs.julialang.org/en/latest/manual/packages/).
  One of the packages added through pacman above was `openssh`, which will allow secure access to GitHub APIs.
  Follow GitHub's [guide](https://help.github.com/articles/generating-ssh-keys) to setting up SSH keys to ensure your local machine can communicate with GitHub effectively.

  5. In case of the issues with building packages (i.e. ICU fails to build with the following error message ```error compiling xp_parse: error compiling xp_make_parser: could not load module libexpat-1: %```) run ```make win-extras``` and then copy everything from the ```dist-extras``` folder into ```usr/bin```.

## Building on Windows with MinGW-builds/MSYS

### The MSYS build of `make` is fragile and may not reliably support parallel builds. Use MSYS2 as described above, if you can. If you must use MSYS, take care to notice the special comments in this section.

1. Install [7-Zip](http://www.7-zip.org/download.html).

2. Install [Python 2.x](http://www.python.org/download/releases). Do **not** install Python 3.

3. Install [MinGW-builds](http://sourceforge.net/projects/mingwbuilds/), a Windows port of GCC. as follows. Do **not** use the regular MinGW distribution.
  1. Download the [MinGW-builds installer](http://downloads.sourceforge.net/project/mingwbuilds/mingw-builds-install/mingw-builds-install.exe).
  2. Run the installer. When prompted, choose:
    - Version: the most recent version (these instructions were tested with 4.8.1)
    - Architecture: x32 or x64 as appropriate and desired.
    - Threads: win32 (not posix)
    - Exception: sjlj (for x32) or seh (for x64). Do not choose dwarf2.
    - Build revision: most recent available (tested with 5)
  3. Do **not** install to a directory with spaces in the name. You will have to change the default installation path. The following instructions will assume `C:\mingw-builds\x64-4.8.1-win32-seh-rev5\mingw64`.

4. Download and extract the [MSYS distribution for MinGW-builds](http://sourceforge.net/projects/mingwbuilds/files/external-binary-packages/) (e.g. msys+7za+wget+svn+git+mercurial+cvs-rev13.7z) to a directory *without* spaces in the name, e.g. `C:/mingw-builds/msys`.

5. Download the [MSYS distribution of make](http://sourceforge.net/projects/mingw/files/MSYS/Base/make) and use this  `make.exe` to replace the one in the `mingw64\bin` subdirectory of the MinGW-builds installation.

6. Run the `msys.bat` installed in Step 4. Set up MSYS by running at the MSYS prompt:

   ```
    mount C:/mingw-builds/x64-4.8.1-win32-seh-rev5/mingw64 /mingw
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
   (the full syntax is `make run-julia[-release|-debug]`)

  - Using the Julia executables directly
    ```
    usr/bin/julia
```

## Cross-compiling

If you prefer to cross-compile, the following steps should get you started.

### Ubuntu and Mac Dependencies (these steps will work for almost any linux platform)

First, you will need to ensure your system has the required dependencies. We need wine (>=1.7.5),
a system compiler, and some downloaders.

On Ubuntu:

    apt-add-repository ppa:ubuntu-wine/ppa
    apt-get upate
    apt-get install wine1.7 subversion cvs gcc wget p7zip-full


On Mac: Install XCode, XCode command line tools, X11 (now [XQuartz](http://xquartz.macosforge.org/)),
and [MacPorts](http://www.macports.org/install.php) or [Homebrew](http://mxcl.github.io/homebrew/).
Then run ```port install wine wget``` or ```brew install wine wget```, as appropriate.

On Both:

Unfortunately, the version of gcc installed by Ubuntu is currently 4.6, which does not compile OpenBLAS correctly.
On Mac, the situation is the same: the version in MacPorts is very old and Homebrew does not have it.
So first we need to get a cross-compile version of gcc.
Most binary packages appear to not include gfortran, so we will need to compile it from source (or ask @vtjnash to send you a tgz of his build).
This is typically quite a bit of work, so we will use [this script](https://code.google.com/p/mingw-w64-dgn/) to make it easy.

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
3. `make`
4. `make win-extras` (Necessary before running `make dist`p)
5. `make dist`
6. move the julia-* directory / zip file to the target machine

If you are building for 64-bit windows, the steps are essentially the same. Just replace i686 in XC_HOST with x86_64. (note: on Mac, wine only runs in 32-bit mode)

## Windows Build Debugging

### Build process is slow/eats memory/hangs my computer

- Disable the Windows [Superfetch](http://en.wikipedia.org/wiki/Windows_Vista_I/O_technologies#SuperFetch) and 
  [Program Compatibility Assistant](http://blogs.msdn.com/b/cjacks/archive/2011/11/22/managing-the-windows-7-program-compatibility-assistant-pca.aspx) services, as they are known to have
  [spurious interactions]((https://cygwin.com/ml/cygwin/2011-12/msg00058.html)) with MinGW/Cygwin.

  As mentioned in the link above: excessive memory use by `svchost` specifically may be investigated in the Task
  Manager by clicking on the high-memory `svchost.exe` process and selecting `Go to Services`. Disable child services
  one-by-one until a culprit is found.

- Beware of [BLODA](https://cygwin.com/faq/faq.html#faq.using.bloda)

  The [vmmap](http://technet.microsoft.com/en-us/sysinternals/dd535533.aspx) tool is indispensable for identifying
  such software conflicts. Use vmmap to inspect the list of loaded DLLs for bash, mintty, or another persistent
  process used to drive the build. Essentially *any* DLL outside of the Windows System directory is potential BLODA.

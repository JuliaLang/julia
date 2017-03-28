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
Alternatively, you may prefer the features of a more full-function IDE, such as [`Juno`](http://junolab.org), [`LightTable`](https://github.com/one-more-minute/Jupiter-LT), [`Sublime-IJulia`](https://github.com/quinnj/Sublime-IJulia), or [`IJulia`](https://github.com/JuliaLang/IJulia.jl).


# Binary distribution

Julia runs on Windows 7 and later.
Both the 32-bit and 64-bit versions are supported.
The 32-bit (i686) binary will run on either a 32-bit and 64-bit operating system.
The 64-bit (x86_64) binary will only run on 64-bit Windows and will otherwise refuse to launch.

1. [Download](http://julialang.org/downloads) the latest version of Julia. Extract the binary to a reasonable destination folder, e.g. `C:\julia`.

2. Double-click the `julia` shortcut to launch Julia.

3. Julia's home directory is the location pointed to by the Windows environment variable `HOME`: this directory is for instance where the startup file `.juliarc.jl` resides. `HOMEDRIVE`\\`HOMEPATH` is used as a fallback if `HOME` is not defined.


# Line endings

Julia uses binary-mode files exclusively. Unlike many other Windows programs, if you write '\n' to a file, you get a '\n' in the file, not some other bit pattern. This matches the behavior exhibited by other operating systems. If you have installed msysGit, it is suggested, but not required, that you configure your system msysGit to use the same convention:

    git config --global core.eol lf
    git config --global core.autocrlf input

or edit `%USERPROFILE%\.gitconfig` and add/edit the lines:

    [core] eol = lf
           autocrlf = input


# Source distribution

## Supported build platforms
- Windows 10: supported (32 and 64 bits)
- Windows 8: supported (32 and 64 bits)
- Windows 7: supported (32 and 64 bits)
- Windows Vista: not officially supported (but probably works anyways)
- Windows XP: not supported (but may work anyways, if you can get around the lack of junction points)

## Compiling with MinGW/MSYS2

### MSYS2 provides a robust MSYS experience.

1. Install [Python 2.x](http://www.python.org/downloads/). Do **not** install Python 3.

2. Install [CMake](http://www.cmake.org/download/).

3. Install and configure [MSYS2](https://msys2.github.io), a minimal POSIX-like environment for Windows.

  1. Download and run the latest installer for the [32-bit](http://sourceforge.net/projects/msys2/files/Base/i686/) or [64-bit](http://sourceforge.net/projects/msys2/files/Base/x86_64/) distribution. The installer will have a name like `msys2-i686-yyyymmdd.exe` or `msys2-x86_64-yyyymmdd.exe`.

  2. Double-click `msys2_shell.bat` in the installed msys directory. Initialize the MSYS2 base system using the `pacman` package manager included in MSYS2:

     ```
    update-core
    pacman --needed -Sy bash pacman pacman-mirrors msys2-runtime # if update-core is not available
    pacman -Syu           #Update package database and full system upgrade
```

  3. Exit and restart MSYS2, then install packages required to build julia:

     ```
    pacman -S diffutils git m4 make patch tar p7zip msys/openssh ca-certificates
```

  4. Configure your MSYS2 shell so Python is visible on the path:

     ```
    echo "export PATH=/usr/local/bin:/usr/bin:/opt/bin:/C/Python27" >> ~/.bashrc
```

     *N.B.* The `export` clobbers whatever `$PATH` is already defined. This is suggested to avoid path-masking. If you use MSYS2 for purposes other than building Julia, you may prefer to append rather than clobber.

     *N.B.* All of the path separators are unix-style. In MSYS2, `/C/` means the root of your `C:\` drive. Replace `/C/Python27` with the location where you installed Python.


  5. Configuration of MSYS2 is complete. Now `exit` the MSYS2 shell.

4. Build Julia and its dependencies from source.
  1. Open a new MSYS2 shell and clone the Julia sources
     ```
    git clone https://github.com/JuliaLang/julia.git
    cd julia
```

  2. Run the following script to download the correct versions of the MinGW-w64 compilers
     ```
    contrib/windows/get_toolchain.sh 32  # for 32 bit Julia
    # or
    contrib/windows/get_toolchain.sh 64  # for 64 bit Julia
```
     Then follow the printed instructions by running either
     ```
    export PATH=$PWD/usr/i686-w64-mingw32/sys-root/mingw/bin:$PATH  # for 32 bit Julia
    # or
    export PATH=$PWD/usr/x86_64-w64-mingw32/sys-root/mingw/bin:$PATH  # for 64 bit Julia
```
     to add the downloaded MinGW-w64 compilers to your path (temporarily, only needed during the shell session when you build Julia).

  3. Specify the location where you installed CMake

     ```
    echo 'override CMAKE=/C/path/to/CMake/bin/cmake.exe' > Make.user
```

  4. Start the build
     ```
    make -j 4   # Adjust the number of cores (4) to match your build environment.
```

5. Setup Package Development Environment
  1. The `Pkg` module in Base provides many convenient tools for [developing and publishing packages](http://docs.julialang.org/en/latest/manual/packages/).
  One of the packages added through pacman above was `openssh`, which will allow secure access to GitHub APIs.
  Follow GitHub's [guide](https://help.github.com/articles/generating-ssh-keys) to setting up SSH keys to ensure your local machine can communicate with GitHub effectively.

  2. In case of the issues with building packages (i.e. ICU fails to build with the following error message ```error compiling xp_parse: error compiling xp_make_parser: could not load module libexpat-1: %```) run ```make win-extras``` and then copy everything from the ```dist-extras``` folder into ```usr/bin```.

## Cygwin-to-MinGW cross compiling

Julia can be also compiled from source in [Cygwin](http://www.cygwin.com), using versions of the MinGW-w64 compilers available through Cygwin's package manager.

1. Download and run Cygwin setup for [32 bit](http://cygwin.com/setup-x86.exe) or [64 bit](http://cygwin.com/setup-x86_64.exe). Note that you can compile either 32 or 64 bit Julia from either 32 or 64 bit Cygwin. 64 bit Cygwin has a slightly smaller but often more up-to-date selection of packages.

2. Select installation location and download mirror.

3. At the "Select Packages" step, select the following:
  1. `git` (under `Devel` category)
  2. `make` (under `Devel` category)
  3. `curl` (under `Net` category)
  4. `patch` (under `Devel` category)
  5. `python` (under `Interpreters` or `Python` category)
  6. `gcc-g++` (under `Devel` category)
  7. `m4` (under `Interpreters` category)
  8. `cmake` (under `Devel` category)
  9. `p7zip` (under `Archive` category)
  10. `mingw64-i686-gcc-g++` and `mingw64-i686-gcc-fortran` (for 32 bit Julia, under `Devel` category)
  11. `mingw64-x86_64-gcc-g++` and `mingw64-x86_64-gcc-fortran` (for 64 bit Julia, under `Devel` category)

4. At the "Resolving Dependencies" step, be sure to leave "Select required packages (RECOMMENDED)" enabled.

5. Allow Cygwin installation to finish, then start a "Cygwin Terminal" (or "Cygwin64 Terminal") from the installed shortcut.

6. Build Julia and its dependencies from source.
  1. Get the Julia sources
     ```
    git clone --recursive https://github.com/JuliaLang/julia.git
    cd julia
```
     *Tips:*
     - If you get an `error: cannot fork() for fetch-pack: Resource temporarily unavailable` from git, add `alias git="env PATH=/usr/bin git"` to `~/.bashrc` and restart Cygwin.

  2. Set the `XC_HOST` variable in `Make.user` to indicate MinGW-w64 cross compilation

     For 32 bit Julia
     ```
    echo 'XC_HOST = i686-w64-mingw32' > Make.user
```
     For 64 bit Julia
     ```
    echo 'XC_HOST = x86_64-w64-mingw32' > Make.user
```

  3. Start the build
    ```
    make -j 4   # Adjust the number of cores (4) to match your build environment.
```

7. Run Julia using the Julia executables directly
    ```
    usr/bin/julia.exe
    usr/bin/julia-debug.exe
```

## Cross-compiling from Unix

If you prefer to cross-compile, the following steps should get you started.

For maximum compatibility with packages that use [WinRPM.jl](https://github.com/JuliaLang/WinRPM.jl) for binary dependencies on Windows, it is recommended that you use OpenSUSE 13.2 for cross-compiling a Windows build of Julia. If you use a different Linux distribution or OS X, install [Vagrant](http://www.vagrantup.com/downloads) and use the following `Vagrantfile`:
```
# Vagrantfile for MinGW-w64 cross-compilation of Julia

$script = <<SCRIPT
# Change the following to i686-w64-mingw32 for 32 bit Julia:
export XC_HOST=x86_64-w64-mingw32
# Change the following to 32 for 32 bit Julia:
export BITS=64
zypper addrepo http://download.opensuse.org/repositories/windows:mingw:win$BITS/openSUSE_13.2/windows:mingw:win$BITS.repo
zypper --gpg-auto-import-keys refresh
zypper -n install --no-recommends git make cmake tar wine which curl \
    python python-xml patch gcc-c++ m4 p7zip.i586 libxml2-tools
zypper -n install mingw$BITS-cross-gcc-c++ mingw$BITS-cross-gcc-fortran \
    mingw$BITS-libstdc++6 mingw$BITS-libgfortran3 mingw$BITS-libssp0
# opensuse packages the mingw runtime dlls under sys-root/mingw/bin, not /usr/lib64/gcc
cp /usr/$XC_HOST/sys-root/mingw/bin/*.dll /usr/lib*/gcc/$XC_HOST/*/
git clone git://github.com/JuliaLang/julia.git julia
cd julia
make -j4 win-extras julia-ui-release
export WINEDEBUG=-all # suppress wine fixme's
# this last step may need to be run interactively
make -j4 binary-dist
SCRIPT

Vagrant.configure("2") do |config|
  config.vm.box = "bento/opensuse-13.2"
  config.vm.provider :virtualbox do |vb|
    # Use VBoxManage to customize the VM. For example to change memory:
    vb.memory = 2048
  end
  config.vm.provision :shell, :inline => $script
end
```

### Cross-building Julia without Vagrant

If you don't care that the build is potentially incompatible with the WinRPM ecosystem (or happen to be on opensuse), use the following steps to cross-compile julia:

First, you will need to ensure your system has the required dependencies. We need wine (>=1.7.5),
a system compiler, and some downloaders.

On Ubuntu (on other linux systems, the dependency names are likely to be similar):

    apt-add-repository ppa:ubuntu-wine/ppa
    apt-get update
    apt-get install wine1.7 subversion cvs gcc wget p7zip-full

On Mac: Install XCode, XCode command line tools, X11 (now [XQuartz](http://xquartz.macosforge.org/)),
and [MacPorts](http://www.macports.org/install.php) or [Homebrew](http://mxcl.github.io/homebrew/).
Then run ```port install wine wget``` or ```brew install wine wget```, as appropriate.

On Both:

Unfortunately, the version of gcc installed by Ubuntu targets pthreads.
On Mac, the situation is similar: the version in MacPorts is very old and Homebrew does not have it.
So first we need to get a cross-compile version of gcc.
Most binary packages appear to not include gfortran, so we will need to compile it from source.
This is typically quite a bit of work, so we will use [this script](http://sourceforge.net/projects/mingw-w64-dgn/) o make it easy.

1. `svn checkout svn://svn.code.sf.net/p/mingw-w64-dgn/code/trunk mingw-w64-dgn`
2. `cd mingw-w64-dgn`
3. edit `rebuild_cross.sh` and make the following two changes:
  a. uncomment `export MAKE_OPT="-j 2"`, if appropriate for your machine
  b. add `fortran` to the end of `--enable-languages=c,c++,objc,obj-c++`
5. `bash update_source.sh`
4. `bash rebuild_cross.sh`
5. `mv cross ~/cross-w64`
6. `export PATH=$HOME/cross-w64/bin:$PATH` # NOTE: it is important that you remember to always do this before using make in the following steps! You can put this line in your .profile to make it easy.

Then we can essentially just repeat these steps for the 32-bit compiler, reusing some of the work:

7. `cd ..`
8. `cp -a mingw-w64-dgn mingw-w32-dgn`
9. `cd mingw-w32-dgn`
10. `rm -r cross build`
11. `bash rebuild_cross.sh 32r`
12. `mv cross ~/cross-w32`
13. `export PATH=$HOME/cross-w32/bin:$PATH` # NOTE: it is important that you remember to always do this before using make in the following steps! You can put this line in your .profile to make it easy.

Note: for systems that support rpm-based package managers, the necessary dependencies can be downloaded from the OpenSUSE build service (see the Vagrant script above).

@vtjnash occassionally upload his builds, so you can also download those to save build time:
- [x86_64-w64-mingw64 gcc-5.2.0 MacOS-10.10 (105 MB)](https://onedrive.live.com/redir?resid=BCAF288A35FC4406!1601&authkey=!ANpFOoCqFGYTcHM&ithint=file%2cgz)
- [i686-w64-mingw64 gcc-5.2.0 MacOS-10.10 (89 MB)](https://onedrive.live.com/redir?resid=BCAF288A35FC4406!1607&authkey=!AFrL8G5G70iYhUU&ithint=file%2cgz)
- [*-w64-mingw64 gcc-4.9.2 Ubuntu-15.04 (168 MB)](https://onedrive.live.com/redir?resid=BCAF288A35FC4406!1602&authkey=!ABhstrqDG-4zdLo&ithint=file%2cgz)

Then run the build:

1. `git clone https://github.com/JuliaLang/julia.git julia-win32`
2. `echo override XC_HOST = i686-w64-mingw32 >> Make.user`
3. `make`
4. `make win-extras` (Necessary before running `make binary-dist`)
5. `make binary-dist`
6. move the julia-*.exe installer to the target machine

If you are building for 64-bit windows, the steps are essentially the same. Just replace i686 in XC_HOST with x86_64. (note: on Mac, wine only runs in 32-bit mode).

## Debugging a cross-compiled build under wine

The most effective way to debug a cross-compiled version of julia on the cross-compilation
host is to install a windows version of gdb and run it under wine as usual. The pre-built packages
available [as part of the MSYS2 project](https://sourceforge.net/projects/msys2/files/REPOS/MINGW/)
are known to work. Apart from the GDB package you may also need the python and termcap packages.
Finally, GDB's prompt may not work when launch from the command line. This can be worked around
by prepending `wineconsole` to the regular GDB invocation.

## Using a Windows VM

[Vagrant](http://www.vagrantup.com/downloads) can also be used with a Windows
guest VM via the `Vagrantfile` in [contrib/windows](contrib/windows/Vagrantfile),
just run `vagrant up` from that folder. To build with Cygwin instead of MSYS2,
replace `config.vm.provision :shell, privileged: false, :inline => $script_msys2`
(near the end of the file) with `config.vm.provision :shell, privileged: false, :inline => $script_cygwin`.

## After compiling

Compiling using one of the options above creates a basic Julia build, but not some
extra components that are included if you run the full Julia binary installer.
If you need these components, the easiest way to get them is to build the installer
yourself using ```make win-extras``` followed by ```make binary-dist```, and then
running the resulting installer.

## Windows Build Debugging

### GDB hangs with cygwin mintty

- Run gdb under the windows console (cmd) instead. gdb [may not function properly](https://www.cygwin.com/ml/cygwin/2009-02/msg00531.html) under mintty with non-cygwin applications. You can use `cmd /c start` to start the windows console from mintty if necessary.

### GDB not attaching to the right process

- Use the PID from the windows task manager or `WINPID` from the `ps` command instead of the PID from unix style command line tools (e.g. `pgrep`). You may need to add the PID column if it is not shown by default in the windows task manager.

### GDB not showing the right backtrace

- When attaching to the julia process, GDB may not be attaching to the right thread. Use `info threads` command to show all the threads and `thread <threadno>` to switch threads.
- Be sure to use a 32 bit version of GDB to debug a 32 bit build of Julia, or a 64 bit version of GDB to debug a 64 bit build of Julia.

### Build process is slow/eats memory/hangs my computer

- Disable the Windows [Superfetch](https://en.wikipedia.org/wiki/Windows_Vista_I/O_technologies#SuperFetch) and
  [Program Compatibility Assistant](http://blogs.msdn.com/b/cjacks/archive/2011/11/22/managing-the-windows-7-program-compatibility-assistant-pca.aspx) services, as they are known to have
  [spurious interactions]((https://cygwin.com/ml/cygwin/2011-12/msg00058.html)) with MinGW/Cygwin.

  As mentioned in the link above: excessive memory use by `svchost` specifically may be investigated in the Task
  Manager by clicking on the high-memory `svchost.exe` process and selecting `Go to Services`. Disable child services
  one-by-one until a culprit is found.

- Beware of [BLODA](https://cygwin.com/faq/faq.html#faq.using.bloda)

  The [vmmap](http://technet.microsoft.com/en-us/sysinternals/dd535533.aspx) tool is indispensable for identifying
  such software conflicts. Use vmmap to inspect the list of loaded DLLs for bash, mintty, or another persistent
  process used to drive the build. Essentially *any* DLL outside of the Windows System directory is potential BLODA.

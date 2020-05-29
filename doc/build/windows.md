# Julia on Windows

This file describes how to install, or build, and use Julia on Windows.

For more general information about Julia, please see the
[main README](https://github.com/JuliaLang/julia/blob/master/README.md)
or the [documentation](https://docs.julialang.org).


## General Information for Windows


### Unicode font support

The built-in Windows fonts have rather poor coverage of the Unicode character
space.  The free [`DejaVu Sans Mono`](https://dejavu-fonts.github.io/) font can be used
as a replacement font in the Windows console.  Since Windows 2000, simply
downloading the font and installing it is insufficient, since Windows keeps a
list of approved fonts in the registry.

Instructions for adding fonts to the terminal are available at
[this answer on superuser.com](https://superuser.com/a/5079)

Additionally, rather than sticking with the default command prompt, you may want
to use a different terminal emulator program, such as
[Conemu](https://conemu.github.io/) or [Mintty](
https://github.com/mintty/mintty) (note that running Julia on Mintty needs a
copy of `stty.exe` in your `%PATH%` to work properly).  Alternatively, you may
prefer the features of a more full-function IDE, such as [Juno](http://junolab.org),
[Sublime-IJulia](https://github.com/quinnj/Sublime-IJulia), or
[IJulia](https://github.com/JuliaLang/IJulia.jl).


### Line endings

Julia uses binary-mode files exclusively.  Unlike many other Windows programs,
if you write `\n` to a file, you get a `\n` in the file, not some other bit
pattern.  This matches the behavior exhibited by other operating systems.  If
you have installed Git for Windows, it is suggested, but not required, that you
configure your system Git to use the same convention:
```sh
git config --global core.eol lf
git config --global core.autocrlf input
```
or edit `%USERPROFILE%\.gitconfig` and add/edit the lines:
```
[core]
    eol = lf
    autocrlf = input
```

## Binary distribution

Julia runs on Windows 7 and later.
Both the 32-bit and 64-bit versions are supported.
The 32-bit (i686) binary will run on either a 32-bit and 64-bit operating system.
The 64-bit (x86_64) binary will only run on 64-bit Windows and will otherwise refuse to launch.

 1. [Download](https://julialang.org/downloads) the latest version of Julia.
    Extract the binary to a reasonable destination folder, e.g. `C:\julia`.
    By default, Julia will be installed into
    `C:\Users\YOURNAME\AppData\Local\Julia-1.0.0\bin`,
    where `YOURNAME` is your Windows user name.

 2. Double-click the `julia` shortcut to launch Julia.

 3. Julia's home directory is the location pointed to by the Windows environment
    variable `%HOME%`: this directory is for instance where the startup file
    `.julia/config/startup.jl` resides. `%HOMEDRIVE%\%HOMEPATH%` is used as a fallback if
    `%HOME%` is not defined.

## Source distribution

### Supported build platforms

 -  Windows 10: supported (32 and 64 bits)
 -  Windows 8: supported (32 and 64 bits)
 -  Windows 7: supported (32 and 64 bits)

### Cygwin-to-MinGW cross-compiling

The recommended way of compiling Julia from source on Windows is by cross
compiling from [Cygwin](https://www.cygwin.com), using versions of the
MinGW-w64 compilers available through Cygwin's package manager.

 1. Download and run Cygwin setup for [32 bit](https://cygwin.com/setup-x86.exe)
    or [64 bit](https://cygwin.com/setup-x86_64.exe). Note, that you can compile
    either 32 or 64 bit Julia from either 32 or 64 bit Cygwin. 64 bit Cygwin
    has a slightly smaller but often more up-to-date selection of packages.

    Advanced: you may skip steps 2-4 by running:

        setup-x86_64.exe -s <url> -q -P cmake,gcc-g++,git,make,patch,curl,m4,python,p7zip,mingw64-i686-gcc-g++,mingw64-i686-gcc-fortran,mingw64-x86_64-gcc-g++,mingw64-x86_64-gcc-fortran
        :: replace <url> with a site from https://cygwin.com/mirrors.html
        :: or run setup manually first and select a mirror

 2. Select installation location and download mirror.

 3. At the '*Select Packages'* step, select the following:

    1.  From the *Devel* category: `cmake`, `gcc-g++`, `git`, `make`, `patch`
    2.  From the *Net* category: `curl`
    3.  From *Interpreters* (or *Python*) category: `m4`, `python`
    4.  From the *Archive* category: `p7zip`
    5.  For 32 bit Julia, and also from the *Devel* category:
        `mingw64-i686-gcc-g++` and `mingw64-i686-gcc-fortran`
    6.  For 64 bit Julia, and also from the *Devel* category:
        `mingw64-x86_64-gcc-g++` and `mingw64-x86_64-gcc-fortran`

 4. At the *'Resolving Dependencies'* step, be sure to leave *'Select required
    packages (RECOMMENDED)'* enabled.

 5. Allow Cygwin installation to finish, then start from the installed shortcut
    a *'Cygwin Terminal'*, or *'Cygwin64 Terminal'*, respectively.

 6. Build Julia and its dependencies from source:

    1. Get the Julia sources
       ```sh
       git clone https://github.com/JuliaLang/julia.git
       cd julia
       ```
       Tip: If you get an `error: cannot fork() for fetch-pack: Resource
       temporarily unavailable` from git, add `alias git="env PATH=/usr/bin git"`
       to `~/.bashrc` and restart Cygwin.

    2. Set the `XC_HOST` variable in `Make.user` to indicate MinGW-w64 cross
       compilation
       ```sh
       echo 'XC_HOST = i686-w64-mingw32' > Make.user     # for 32 bit Julia
       # or
       echo 'XC_HOST = x86_64-w64-mingw32' > Make.user   # for 64 bit Julia
       ```

    3. Start the build
       ```sh
       make -j 4   # Adjust the number of threads (4) to match your build environment.
       ```


    > Protip: build both!
    > ```sh
    > make O=julia-win32 configure
    > make O=julia-win64 configure
    > echo 'XC_HOST = i686-w64-mingw32' > julia-win32/Make.user
    > echo 'XC_HOST = x86_64-w64-mingw32' > julia-win64/Make.user
    > echo 'ifeq ($(BUILDROOT),$(JULIAHOME))
    >         $(error "in-tree build disabled")
    >       endif' >> Make.user
    > make -C julia-win32  # build for Windows x86 in julia-win32 folder
    > make -C julia-win64  # build for Windows x86-64 in julia-win64 folder
    > ```

 7. Run Julia using the Julia executables directly
    ```sh
    usr/bin/julia.exe
    usr/bin/julia-debug.exe
    ```

### Compiling with MinGW/MSYS2

Compiling Julia from source using [MSYS2](https://msys2.github.io) has worked
in the past but is not actively supported. Pull requests to restore support
would be welcome. See a [past version of this file](
https://github.com/JuliaLang/julia/blob/v0.6.0/README.windows.md)
for the former instructions for compiling using MSYS2.


### Cross-compiling from Unix

You can also use MinGW-w64 cross compilers to build a Windows version of Julia from
Linux, Mac, or the Windows Subsystem for Linux (WSL).

For maximum compatibility with packages that use [WinRPM.jl](
https://github.com/JuliaLang/WinRPM.jl) for binary dependencies on Windows, it
is recommended that you use OpenSUSE 42.2 for cross-compiling a Windows build
of Julia.  If you use a different Linux distribution or OS X, install
[Vagrant](https://www.vagrantup.com/downloads.html) and use the following `Vagrantfile`:

```
# Vagrantfile for MinGW-w64 cross-compilation of Julia

$script = <<SCRIPT
# Change the following to i686-w64-mingw32 for 32 bit Julia:
export XC_HOST=x86_64-w64-mingw32
# Change the following to 32 for 32 bit Julia:
export BITS=64
zypper addrepo https://download.opensuse.org/repositories/windows:mingw:win$BITS/openSUSE_Leap_42.2/windows:mingw:win$BITS.repo
zypper --gpg-auto-import-keys refresh
zypper -n install --no-recommends git make cmake tar wine which curl \
    python python-xml patch gcc-c++ m4 p7zip.i586 libxml2-tools winbind
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
make -j4 exe
SCRIPT

Vagrant.configure("2") do |config|
  config.vm.box = "bento/opensuse-leap-42.2"
  config.vm.provider :virtualbox do |vb|
    # Use VBoxManage to customize the VM. For example to change memory:
    vb.memory = 2048
  end
  config.vm.provision :shell, :inline => $script
end
```


### Cross-building Julia without Vagrant

Alternatively, if you want to build it on the local system,
use the following steps to cross-compile julia:

First, you will need to ensure your system has the required dependencies.  We
need wine (>=1.7.5), a system compiler, and some downloaders.

**On Ubuntu** (on other linux systems, the dependency names are likely to be similar):
```sh
apt-get install wine-stable gcc wget p7zip-full winbind mingw-w64 gfortran-mingw-w64
# switch all of the following to their "-posix" variants (interactively):
for pkg in i686-w64-mingw32-g++ i686-w64-mingw32-gcc i686-w64-mingw32-gfortran x86_64-w64-mingw32-g++ x86_64-w64-mingw32-gcc x86_64-w64-mingw32-gfortran; do sudo update-alternatives --config $pkg; done
```

**On Mac**: Install XCode, XCode command line tools, X11 (now [XQuartz](
https://www.xquartz.org/)), and [MacPorts](https://www.macports.org/install.php)
or [Homebrew](https://brew.sh/).  Then run `port install wine wget mingw-w64`,
or `brew install wine wget mingw-w64`, as appropriate.

Then run the build:

 1. `git clone https://github.com/JuliaLang/julia.git julia-win32`
 2. `echo override XC_HOST = i686-w64-mingw32 >> Make.user`
 3. `make`
 4. `make win-extras` (Necessary before running `make binary-dist`)
 5. `make binary-dist` then `make exe` to create the Windows installer.
 6. move the `julia-*.exe` installer to the target machine

If you are building for 64-bit windows, the steps are essentially the same.
Just replace `i686` in `XC_HOST` with `x86_64`. (note: on Mac, wine only runs
in 32-bit mode).


## Debugging a cross-compiled build under wine

The most effective way to debug a cross-compiled version of julia on the
cross-compilation host is to install a windows version of gdb and run it under wine
as usual. The pre-built packages available [as part of the MSYS2 project](
https://sourceforge.net/projects/msys2/files/REPOS/MINGW/) are known to work.
Apart from the GDB package you may also need the python and termcap packages.
Finally, GDB's prompt may not work when launch from the command line. This can
be worked around by prepending `wineconsole` to the regular GDB invocation.


## Using a Windows VM

[Vagrant](https://www.vagrantup.com/downloads.html) can also be used with a Windows
guest VM via the `Vagrantfile` in [contrib/windows](
https://github.com/JuliaLang/julia/blob/master/contrib/windows/Vagrantfile),
just run `vagrant up` from that folder.


## After compiling

Compiling using one of the options above creates a basic Julia build, but not some
extra components that are included if you run the full Julia binary installer.
If you need these components, the easiest way to get them is to build the installer
yourself using ```make win-extras``` followed by ```make binary-dist``` and ```make exe```. Then running the resulting installer.


## Windows Build Debugging


### GDB hangs with cygwin mintty

- Run gdb under the windows console (cmd) instead. gdb [may not function properly](
  https://www.cygwin.com/ml/cygwin/2009-02/msg00531.html) under mintty with non-
  cygwin applications. You can use `cmd /c start` to start the windows console
  from mintty if necessary.

### GDB not attaching to the right process

 - Use the PID from the windows task manager or `WINPID` from the `ps` command
   instead of the PID from unix style command line tools (e.g. `pgrep`).  You
   may need to add the PID column if it is not shown by default in the windows
   task manager.

### GDB not showing the right backtrace

 - When attaching to the julia process, GDB may not be attaching to the right
   thread.  Use `info threads` command to show all the threads and
   `thread <threadno>` to switch threads.
 - Be sure to use a 32 bit version of GDB to debug a 32 bit build of Julia, or
   a 64 bit version of GDB to debug a 64 bit build of Julia.

### Build process is slow/eats memory/hangs my computer

 - Disable the Windows [Superfetch](https://en.wikipedia.org/wiki/Windows_Vista_I/O_technologies#SuperFetch)
   and [Program Compatibility Assistant](
   https://blogs.msdn.com/b/cjacks/archive/2011/11/22/managing-the-windows-7-program-compatibility-assistant-pca.aspx)
   services, as they are known to have [spurious interactions](
   https://cygwin.com/ml/cygwin/2011-12/msg00058.html) with MinGW/Cygwin.

   As mentioned in the link above: excessive memory use by `svchost` specifically
   may be investigated in the Task Manager by clicking on the high-memory
   `svchost.exe` process and selecting `Go to Services`. Disable child services
   one-by-one until a culprit is found.

 - Beware of [BLODA](https://cygwin.com/faq/faq.html#faq.using.bloda).
   The [vmmap](https://technet.microsoft.com/en-us/sysinternals/dd535533.aspx)
   tool is indispensable for identifying such software conflicts. Use vmmap to
   inspect the list of loaded DLLs for bash, mintty, or another persistent
   process used to drive the build. Essentially *any* DLL outside of the Windows
   System directory is potential BLODA.

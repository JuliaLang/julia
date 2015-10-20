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

1. [Download](http://julialang.org/downloads) the latest version of Julia. Extract the binary to a reasonable destination folder, e.g. `C:\julia`.

2. Double-click the `julia` shortcut to launch Julia.

# Line endings

Julia uses binary-mode files exclusively. Unlike many other Windows programs, if you write '\n' to a file, you get a '\n' in the file, not some other bit pattern. This matches the behavior exhibited by other operating systems. If you have installed msysGit, it is suggested, but not required, that you configure your system msysGit to use the same convention:

    git config --global core.eol lf
    git config --global core.autocrlf input

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

1. Install [Python 2.x](http://www.python.org/download/releases). Do **not** install Python 3.

2. Install [CMake](http://www.cmake.org/download/).

3. Install and configure [MSYS2](https://msys2.github.io), a minimal POSIX-like environment for Windows.

  1. Download and run the latest installer for the [32-bit](http://sourceforge.net/projects/msys2/files/Base/i686/) or [64-bit](http://sourceforge.net/projects/msys2/files/Base/x86_64/) distribution. The installer will have a name like `msys2-i686-yyyymmdd.exe` or `msys2-x86_64-yyyymmdd.exe`.

  2. Double-click `msys2_shell.bat` in the installed msys directory. Initialize the MSYS2 base system using the `pacman` package manager included in MSYS2:

     ```
    pacman --needed -Sy bash pacman pacman-mirrors msys2-runtime
```

  3. Exit and restart MSYS2, then install packages required to build julia:

     ```
    pacman -Syu           #Update package database and full system upgrade
    pacman -S diffutils git m4 make patch tar p7zip msys/openssh
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
    git clone -b release-0.3 https://github.com/JuliaLang/julia.git
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

  3. Start the build
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
    git clone --recursive -b release-0.3 https://github.com/JuliaLang/julia.git
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

7. Run Julia with _either_ of:
  - Using `make`
    ```
    make run-julia
   ```
   (the full syntax is `make run-julia[-release|-debug]`)

  - Using the Julia executables directly
    ```
    usr/bin/julia
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
git clone -b release-0.3 git://github.com/JuliaLang/julia.git
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

### Cross-building Julia

Finally, the build and install process for Julia:

1. `git clone -b release-0.3 https://github.com/JuliaLang/julia.git julia-win32`
2. `cd julia-win32`
3. `echo override XC_HOST = i686-w64-mingw32 >> Make.user`
4. `make`
5. `make win-extras` (Necessary before running `make dist`)
6. `make dist`
7. move the julia-*.exe installer to the target machine

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

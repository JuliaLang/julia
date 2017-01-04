<a name="logo"/>
<div align="center">
<a href="http://julialang.org/" target="_blank">
<img src="http://julialang.org/images/logo_hires.png" alt="Julia Logo" width="210" height="142"></img>
</a>
</div>

Linux, OSX: [![Build Status](https://api.travis-ci.org/JuliaLang/julia.svg?branch=master)](https://travis-ci.org/JuliaLang/julia)

Windows: [![Build status](https://ci.appveyor.com/api/projects/status/dvial98s5vi6ealt/branch/master?svg=true)](https://ci.appveyor.com/project/JuliaLang/julia/branch/master)

Code Coverage: [![Coverage Status](https://coveralls.io/repos/JuliaLang/julia/badge.svg?branch=master)](https://coveralls.io/r/JuliaLang/julia?branch=master) [![codecov.io](http://codecov.io/github/JuliaLang/julia/coverage.svg?branch=master)](http://codecov.io/github/JuliaLang/julia?branch=master)

<a name="The-Julia-Language"/>
## The Julia Language

Julia is a high-level, high-performance dynamic language for technical computing.
The main homepage for Julia can be found at [julialang.org](http://julialang.org/).
This is the GitHub repository of Julia source code, including instructions for compiling and installing Julia, below.

<a name="Resources"/>
## Resources

- **Homepage:** <http://julialang.org>
- **Binaries:** <http://julialang.org/downloads/>
- **Documentation:** <http://docs.julialang.org/>
- **Packages:** <http://pkg.julialang.org/>
- **Source code:** <https://github.com/JuliaLang/julia>
- **Git clone URL:** <git://github.com/JuliaLang/julia.git>
- **Discussion forum:** <https://discourse.julialang.org>
- **Mailing lists:** <http://julialang.org/community/>
- **Gitter:** <https://gitter.im/JuliaLang/julia>
- **IRC:** <http://webchat.freenode.net/?channels=julia>
- **Code coverage:** <https://coveralls.io/r/JuliaLang/julia>

New developers may find the notes in [CONTRIBUTING](https://github.com/JuliaLang/julia/blob/master/CONTRIBUTING.md) helpful to start contributing to the Julia codebase.

### External Resources
<a name="External-Resources"/>

- [**StackOverflow**](https://stackoverflow.com/questions/tagged/julia-lang)
- [**Youtube**](https://www.youtube.com/channel/UC9IuUwwE2xdjQUT_LMLONoA)
- [**Twitter**](https://twitter.com/JuliaLanguage)
- [**Meetup**](http://julia.meetup.com/)

<a name="Currently-Supported-Platforms"/>
## Currently Supported Platforms

- **Linux**
- **MacOS**
- **FreeBSD**
- **Windows**

All systems are supported with both x86/64 (64-bit) and x86 (32-bit, except MacOS) architectures. Support for [ARM](https://github.com/JuliaLang/julia/blob/master/README.arm.md), AARCH64, and POWER8 (little-endian) has been added recently.

<a name="Source-Download-and-Compilation"/>
## Source Download and Compilation

First, make sure you have all the [required dependencies](#Required-Build-Tools-External-Libraries) installed.
Then, acquire the source code by cloning the git repository:

    git clone git://github.com/JuliaLang/julia.git

(If you are behind a firewall, you may need to use the `https` protocol instead of the `git` protocol:

    git config --global url."https://".insteadOf git://

Be sure to also configure your system to use the appropriate proxy settings, e.g. by setting the `https_proxy` and `http_proxy` variables.)

By default you will be building the latest unstable version of Julia. However, most users should use the most recent stable version of Julia, which is currently the `0.5` series of releases. You can get this version by changing to the Julia directory and running

    git checkout release-0.5

Now run `make` to build the `julia` executable. To perform a parallel build, use `make -j N` and supply the maximum number of concurrent processes. (See [Platform Specific Build Notes](https://github.com/JuliaLang/julia#platform-specific-build-notes) for details.)
When compiled the first time, it will automatically download and build its [external dependencies](#Required-Build-Tools-External-Libraries).
This takes a while, but only has to be done once. If the defaults in the build do not work for you, and you need to set specific make parameters, you can save them in `Make.user`. The build will automatically check for the existence of `Make.user` and use it if it exists.
Building Julia requires 1.5GiB of disk space and approximately 700MiB of virtual memory.

For builds of julia starting with 0.5.0-dev, you can create out-of-tree builds of Julia by specifying `make O=<build-directory> configure` on the command line. This will create a directory mirror, with all of the necessary Makefiles to build Julia, in the specified directory. These builds will share the source files in Julia and `deps/srccache`. Each out-of-tree build directory can have its own `Make.user` file to override the global `Make.user` file in the top-level folder.

If you need to build Julia in an environment that does not allow access to the outside world, use `make -C deps getall` to download all the necessary files. Then, copy the `julia` directory over to the target environment and build with `make`.

**Note:** the build process fail badly if any of the build directory's parent directories have spaces or other shell meta-characters such as `$` or `:` in their names (this is due to a limitation in GNU make).

Once it is built, you can run the `julia` executable using its full path in the directory created above (the `julia` directory), or, to run it from anywhere, either

- add a soft link to the `julia` executable in the `julia` directory to `/usr/local/bin` (or any suitable directory already in your path), or

- add the `julia` directory to your executable path for this shell session (in `bash`: `export PATH="$(pwd):$PATH"` ; in `csh` or `tcsh`:
`set path= ( $path $cwd )` ), or

- add the `julia` directory to your executable path permanently (e.g. in `.bash_profile`), or

- write `prefix=/path/to/install/folder` into `Make.user` and then run `make install`. If there is a version of Julia already installed in this folder, you should delete it before running `make install`.

Now you should be able to run Julia like this:

    julia

If everything works correctly, you will see a Julia banner and an interactive prompt into which you can enter expressions for evaluation. (Errors related to libraries might be caused by old, incompatible libraries sitting around in your PATH. In that case, try moving the `julia` directory earlier in the PATH).

Your first test of Julia should be to determine whether your
build is working properly. From the UNIX/Windows command prompt inside
the `julia` source directory, type `make testall`. You should see output
that lists a series of tests being run; if they complete without
error, you should be in good shape to start using Julia.

You can read about [getting started](http://docs.julialang.org/en/stable/manual/getting-started/) in the manual.

If you are building a Julia package for distribution on Linux, OS X,
or Windows, take a look at the detailed notes in
[DISTRIBUTING.md](https://github.com/JuliaLang/julia/blob/master/DISTRIBUTING.md).

### Updating an existing source tree

If you have previously downloaded `julia` using `git clone`, you can update the
existing source tree using `git pull` rather than starting anew:

    cd julia
    git pull && make

Assuming that you had made no changes to the source tree that will conflict
with upstream updates, these commands will trigger a build to update to the
latest version.

#### General troubleshooting

1. Over time, the base library may accumulate enough changes such that the
   bootstrapping process in building the system image will fail. If this
   happens, the build may fail with an error like

   ```sh
    *** This error is usually fixed by running 'make clean'. If the error persists, try 'make cleanall' ***
   ```

   As described, running `make clean && make` is usually sufficient.
   Occasionally, the stronger cleanup done by `make cleanall` is needed.

2. New versions of external dependencies may be introduced which may
   occasionally cause conflicts with existing builds of older versions.

   a. Special `make` targets exist to help wipe the existing build of a
      dependency. For example, `make -C deps clean-llvm` will clean out the
      existing build of `llvm` so that `llvm` will be rebuilt from the
      downloaded source distribution the next time `make` is called.
      `make -C deps distclean-llvm` is a stronger wipe which will also delete
      the downloaded source distribution, ensuring that a fresh copy of the
      source distribution will be downloaded and that any new patches will be
      applied the next time `make` is called.

   b. To delete existing binaries of `julia` and all its dependencies,
      delete the `./usr` directory _in the source tree_.

3. If you've updated OS X recently, be sure to run `xcode-select --install` to update the command line tools.
   Otherwise, you could run into errors for missing headers and libraries, such as
   ```ld: library not found for -lcrt1.10.6.o```.

4. If you've moved the source directory, you might get errors such as
    ```CMake Error: The current CMakeCache.txt directory ... is different than the directory ... where     CMakeCache.txt was created.```, in which case you may delete the offending dependency under `deps`

5. In extreme cases, you may wish to reset the source tree to a pristine state.
   The following git commands may be helpful:

   ```sh
    git reset --hard #Forcibly remove any changes to any files under version control
    git clean -x -f -d #Forcibly remove any file or directory not under version control
   ```

   _To avoid losing work, make sure you know what these commands do before you
   run them. `git` will not be able to undo these changes!_



<a name="Uninstalling-Julia"/>
## Uninstalling Julia

Julia does not install anything outside the directory it was cloned into. Julia can be completely uninstalled by deleting this directory. Julia packages are installed in `~/.julia` by default, and can be uninstalled by deleting `~/.julia`.

<a name="Platform-Specific-Notes"/>
## Platform-Specific Build Notes

### Linux

#### General

* GCC version 4.7 or later is required to build Julia.
* To use external shared libraries not in the system library search path, set `USE_SYSTEM_XXX=1` and `LDFLAGS=-Wl,-rpath,/path/to/dir/contains/libXXX.so` in `Make.user`.
  * Instead of setting `LDFLAGS`, putting the library directory into the environment variable `LD_LIBRARY_PATH` (at both compile and run time) also works.
* See also the [external dependencies](#Required-Build-Tools-External-Libraries).

#### Architecture Customization

Julia can be built for a non-generic architecture by configuring the `ARCH` Makefile variable. See the appropriate section of `Make.inc` for additional customization options, such as `MARCH` and `JULIA_CPU_TARGET`.

For example, to build for Pentium 4, set `MARCH=pentium4` and install the necessary system libraries for linking. On Ubuntu, these may include lib32gfortran3 (also manually call `ln -s /usr/lib32/libgfortran3.so.0 /usr/lib32/libgfortran3.so`) and lib32gcc1, lib32stdc++6, among others.

You can also set `MARCH=native` for a maximum-performance build customized for the current machine CPU.


#### Ubuntu

The [julia-deps PPA](https://launchpad.net/~staticfloat/+archive/julia-deps/) contains updated packages for Julia dependencies if you want to use system libraries instead of having them downloaded and built during the build process.  See [System Provided Libraries](#System-Provided-Libraries).

#### RHEL/CentOS 6

On RHEL/CentOS 6 systems, the default compiler (`gcc` 4.4) is too old to build Julia.

Install or contact your systems administrator to install a more recent version of `gcc`. The [Scientific Linux Developer Toolset](http://linux.web.cern.ch/linux/devtoolset/) works well.


#### Linux Build Troubleshooting

 Problem              | Possible Solution
------------------------|---------------------
 OpenBLAS build failure | Set one of the following build options in `Make.user` and build again: <ul><li> `OPENBLAS_TARGET_ARCH=BARCELONA` (AMD CPUs) or `OPENBLAS_TARGET_ARCH=NEHALEM` (Intel CPUs)<ul>Set `OPENBLAS_DYNAMIC_ARCH = 0` to disable compiling multiple architectures in a single binary.</ul></li><li> `OPENBLAS_NO_AVX2 = 1` disables AVX2 instructions, allowing OpenBLAS to compile with `OPENBLAS_DYNAMIC_ARCH = 1` using old versions of binutils </li><li> `USE_SYSTEM_BLAS=1` uses the system provided `libblas` <ul><li>Set `LIBBLAS=-lopenblas` and `LIBBLASNAME=libopenblas` to force the use of the system provided OpenBLAS when multiple BLAS versions are installed. </li></ul></li></ul><p> If you get an error that looks like ```../kernel/x86_64/dgemm_kernel_4x4_haswell.S:1709: Error: no such instruction: `vpermpd $ 0xb1,%ymm0,%ymm0'```, then you need to set `OPENBLAS_DYNAMIC_ARCH = 0` or `OPENBLAS_NO_AVX2 = 1`, or you need a newer version of `binutils` (2.18 or newer). ([Issue #7653](https://github.com/JuliaLang/julia/issues/7653))
Illegal Instruction error | Check if your CPU supports AVX while your OS does not (e.g. through virtualization, as described in [this issue](https://github.com/JuliaLang/julia/issues/3263)).
LibGit2 build failure | If LibGit2 configuration step reports `CMake Error at /usr/share/cmake-3.0/Modules/FindOpenSSL.cmake:294 (list):` then the versions of [OpenSSL and CMake are probably incompatible](https://cmake.org/Bug/view.php?id=15386). The recommended solution is to upgrade the version of CMake to 3.1.2 or above.

### OS X

You need to have the current Xcode command line utilities installed: run `xcode-select --install` in the terminal.
You will need to rerun this terminal command after each OS X update, otherwise you may run into errors involving missing libraries or headers.
You will also need a 64-bit gfortran to compile Julia dependencies. The gfortran-4.7 (and newer) compilers in Brew, Fink, and MacPorts work for building Julia.

Clang is now used by default to build Julia on OS X 10.7 and above. On OS X 10.6, the Julia build will automatically use `gcc`.
On current systems, we recommend that you install the command line tools as described above. Older systems do not have a separate command line tools package from Apple, and will require a full Xcode install.  On these, you will need at least Xcode 4.3.3.  In Xcode prior to v5.0, you can alternatively go to Preferences -> Downloads and select the Command Line Utilities. These steps will ensure that clang v3.1 is installed, which is the minimum version of `clang` required to build Julia.

If you have set `LD_LIBRARY_PATH` or `DYLD_LIBRARY_PATH` in your `.bashrc` or equivalent, Julia may be unable to find various libraries that come bundled with it. These environment variables need to be unset for Julia to work.

If you see build failures in OpenBLAS or if you prefer to experiment, you can use the Apple provided BLAS in vecLib by building with `USE_SYSTEM_BLAS=1`. Julia does not use the Apple provided LAPACK, as it is too old.

When building Julia, or its dependencies, libraries installed by third party package managers can redirect the compiler to use an incompatible version of the software it is looking for. One example of this happening is when a piece of software called the "linker" gives an error involving "Undefined symbols." If that happens, you can usually figure out what software package is causing the error from the names in the error text. This sort of error can be bypassed by, temporarily, uninstalling the offending package. If the offending package cannot be uninstalled by itself, it may be possible to just uninstall the development headers (for example: a package ending in "-dev" in Fink).

### FreeBSD

On *FreeBSD Release 11.0*, install the gfortran, git, cmake, and gmake packages/ports (`pkg install gcc6 gmake git cmake`), and compile Julia with the command:

    $ echo 'FC=gfortran6' >> Make.user
    $ gmake

You must use the `gmake` command on FreeBSD instead of `make`.

Note that Julia is community-supported and we have little control over our upstream dependencies, you may still run into issues with dependencies and YMMV. Current known issues include:

 - The x86 arch doesn't support threading due to lack of compiler runtime library support (set `JULIA_THREADS=0`).
 - OpenBLAS patches in pkg haven't been upstreamed.
 - gfortran can't link binaries. Set `FFLAGS=-Wl,-rpath,/usr/local/lib/gcc6` to work around this (upstream bug submitted to FreeBSD pkg maintainers).
 - System libraries installed by pkg are not on the compiler path by default. You may need to add `LDFLAGS=-L/usr/local/lib` and `CPPFLAGS=-I/usr/local/include` to your environment or `Make.user` file to build successfully.


### Windows

In order to build Julia on Windows, see [README.windows](https://github.com/JuliaLang/julia/blob/master/README.windows.md).

### Vagrant

Julia can be developed in an isolated Vagrant environment. See [the Vagrant README](https://github.com/JuliaLang/julia/blob/master/contrib/vagrant/README.md) for details.

<a name="Required-Build-Tools-External-Libraries"/>
## Required Build Tools and External Libraries

Building Julia requires that the following software be installed:

- **[GNU make]**                — building dependencies.
- **[gcc & g++][gcc]** (>= 4.7) or **[Clang][clang]** (>= 3.1, Xcode 4.3.3 on OS X) — compiling and linking C, C++
- **[python]** (>=2.7)          - needed to build LLVM.
- **[gfortran]**                — compiling and linking Fortran libraries
- **[perl]**                    — preprocessing of header files of libraries.
- **[wget]**, **[curl]**, or **[fetch]** (FreeBSD) — to automatically download external libraries.
- **[m4]**                      — needed to build GMP.
- **[patch]**                   — for modifying source code.
- **[cmake]**                   — needed to build `libgit2`.
- **[pkg-config]**              - needed to build `libgit2` correctly, especially for proxy support

Julia uses the following external libraries, which are automatically downloaded (or in a few cases, included in the Julia source repository) and then compiled from source the first time you run `make`:

- **[LLVM]** (3.9)           — compiler infrastructure.
- **[FemtoLisp]**            — packaged with Julia source, and used to implement the compiler front-end.
- **[libuv]**                — portable, high-performance event-based I/O library
- **[OpenLibm]**             — portable libm library containing elementary math functions.
- **[OpenSpecFun]** (>= 0.4) — library containing Bessel and error functions of complex arguments.
- **[DSFMT]**                — fast Mersenne Twister pseudorandom number generator library.
- **[OpenBLAS]**             — fast, open, and maintained [basic linear algebra subprograms (BLAS)](https://en.wikipedia.org/wiki/Basic_Linear_Algebra_Subprograms) library, based on [Kazushige Goto's](https://en.wikipedia.org/wiki/Kazushige_Goto) famous [GotoBLAS](https://www.tacc.utexas.edu/research-development/tacc-software/gotoblas2).
- **[LAPACK]** (>= 3.5)      — library of linear algebra routines for solving systems of simultaneous linear equations, least-squares solutions of linear systems of equations, eigenvalue problems, and singular value problems.
- **[MKL]** (optional)       – OpenBLAS and LAPACK may be replaced by Intel's MKL library.
- **[AMOS]**                 — subroutines for computing Bessel and Airy functions.
- **[SuiteSparse]** (>= 4.1) — library of linear algebra routines for sparse matrices.
- **[ARPACK]**               — collection of subroutines designed to solve large, sparse eigenvalue problems.
- **[FFTW]** (>= 3.3)        — library for computing fast Fourier transforms very quickly and efficiently.
- **[PCRE]** (>= 10.00)      — Perl-compatible regular expressions library.
- **[GMP]** (>= 5.0)         — GNU multiple precision arithmetic library, needed for `BigInt` support.
- **[MPFR]** (>= 3.0)        — GNU multiple precision floating point library, needed for arbitrary precision floating point (`BigFloat`) support.
- **[libgit2]** (>= 0.23)    — Git linkable library, used by Julia's package manager
- **[curl]** (>= 7.50)       — libcurl provides download and proxy support for Julia's package manager
- **[libssh2]** (>= 1.7)     — library for SSH transport, used by libgit2 for packages with SSH remotes
- **[mbedtls]** (>= 2.2)     — library used for cryptography and transport layer security, used by libssh2
- **[utf8proc]** (>= 2.0)    — a library for processing UTF-8 encoded Unicode strings
- **[libosxunwind]**         — clone of [libunwind], a library that determines the call-chain of a program

[GNU make]:     http://www.gnu.org/software/make
[patch]:        http://www.gnu.org/software/patch
[wget]:         http://www.gnu.org/software/wget
[m4]:           http://www.gnu.org/software/m4
[gcc]:          http://gcc.gnu.org
[clang]:        http://clang.llvm.org
[python]:       https://www.python.org/
[gfortran]:     https://gcc.gnu.org/fortran/
[curl]:         http://curl.haxx.se
[fetch]:        http://www.freebsd.org/cgi/man.cgi?fetch(1)
[perl]:         http://www.perl.org
[cmake]:        http://www.cmake.org
[OpenLibm]:     https://github.com/JuliaLang/openlibm
[OpenSpecFun]:  https://github.com/JuliaLang/openspecfun
[DSFMT]:        http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/#dSFMT
[OpenBLAS]:     https://github.com/xianyi/OpenBLAS
[LAPACK]:       http://www.netlib.org/lapack
[MKL]:          http://software.intel.com/en-us/articles/intel-mkl
[SuiteSparse]:  http://faculty.cse.tamu.edu/davis/suitesparse.html
[AMOS]:         http://netlib.org/amos
[ARPACK]:       http://forge.scilab.org/index.php/p/arpack-ng
[FFTW]:         http://www.fftw.org
[PCRE]:         http://www.pcre.org
[LLVM]:         http://www.llvm.org
[FemtoLisp]:    https://github.com/JeffBezanson/femtolisp
[GMP]:          http://gmplib.org
[MPFR]:         http://www.mpfr.org
[double-conversion]: http://double-conversion.googlecode.com
[libuv]:        https://github.com/JuliaLang/libuv
[libgit2]:      https://libgit2.github.com/
[utf8proc]:     http://julialang.org/utf8proc/
[libosxunwind]: https://github.com/JuliaLang/libosxunwind
[libunwind]:    http://www.nongnu.org/libunwind
[libssh2]:      https://www.libssh2.org
[mbedtls]:      https://tls.mbed.org/
[pkg-config]:   https://www.freedesktop.org/wiki/Software/pkg-config/

<a name="System-Provided-Libraries">
### System Provided Libraries

If you already have one or more of these packages installed on your system, you can prevent Julia from compiling duplicates of these libraries by passing `USE_SYSTEM_...=1` to `make` or adding the line to `Make.user`. The complete list of possible flags can be found in `Make.inc`.

Please be aware that this procedure is not officially supported, as it introduces additional variability into the installation and versioning of the dependencies, and is recommended only for system package maintainers. Unexpected compile errors may result, as the build system will do no further checking to ensure the proper packages are installed.

### SuiteSparse

SuiteSparse is a special case, since it is typically only installed as a static library, while `USE_SYSTEM_SUITESPARSE=1` requires that it is a shared library. Running the script `contrib/repackage_system_suitesparse4.make` will copy your static system SuiteSparse installation into the shared library format required by Julia. `make USE_SYSTEM_SUITESPARSE=1` will then use the SuiteSparse that has been copied into Julia's directory, but will not build a new SuiteSparse library from scratch.

### Intel compilers and Math Kernel Library (MKL)

To build Julia using the Intel compilers (icc, icpc, ifort), and link against
the [MKL] BLAS and LAPACK libraries, first make sure you have a recent version
of the compiler suite (version 15 or later).

For a 64-bit architecture, the environment should be set up as follows:

    # bash
    source /path/to/intel/bin/compilervars.sh intel64

Add the following to the `Make.user` file:

    USEICC = 1
    USEIFC = 1
    USE_INTEL_MKL = 1
    USE_INTEL_MKL_FFT = 1
    USE_INTEL_LIBM = 1

It is highly recommended to start with a fresh clone of the Julia repository.

<a name="Source-Code-Organization"/>
## Source Code Organization

The Julia source code is organized as follows:

    base/          source code for Julia's standard library
    contrib/       editor support for Julia source, miscellaneous scripts
    deps/          external dependencies
    doc/manual     source for the user manual
    doc/stdlib     source for standard library function help text
    examples/      example Julia programs
    src/           source for Julia language core
    test/          test suites
    test/perf      benchmark suites
    ui/            source for various front ends
    usr/           binaries and shared libraries loaded by Julia's standard libraries

<a name="Binary-Installation"/>
## Binary Installation

If you would rather not compile the latest Julia from source, platform-specific tarballs with pre-compiled binaries are also [available for download](http://julialang.org/downloads/).

You can either run the `julia` executable using its full path in the directory created above, or add that directory to your executable path so that you can run the Julia program from anywhere (in the current shell session):

    export PATH="$(pwd)/julia:$PATH"

Now you should be able to run Julia like this:

    julia

On Windows, double-click `usr/bin/julia.exe`.

If everything works correctly, you will see a Julia banner and an interactive prompt into which you can enter expressions for evaluation.
You can read about [getting started](http://julialang.org/manual/getting-started) in the manual.

The following distributions include julia, but the versions may be out of date due to rapid development:

* [Alpine Linux](http://pkgs.alpinelinux.org/package/edge/testing/x86_64/julia)
* [Arch Linux](https://www.archlinux.org/packages/community/i686/julia/)
* [Debian GNU/Linux](http://packages.debian.org/sid/julia)
* [Fedora Linux](https://admin.fedoraproject.org/pkgdb/package/julia/), RHEL/CentOS/OEL/Scientific Linux (EPEL)
  * [Current stable release for Fedora/EPEL](https://copr.fedoraproject.org/coprs/nalimilan/julia/)
  * [Nightly builds for Fedora/EPEL](https://copr.fedoraproject.org/coprs/nalimilan/julia-nightlies/)
* [Gentoo Linux](https://packages.gentoo.org/package/dev-lang/julia)
  * Git Package in the [Science overlay](https://wiki.gentoo.org/wiki/Project:Science/Overlay)
* openSUSE
  * Stable package for openSUSE: [OBS page](https://build.opensuse.org/package/show/science/julia), [1 Click Install](http://software.opensuse.org/download.html?project=science&package=julia)
  * Git package for openSUSE: [OBS page](https://build.opensuse.org/package/show/science/julia-unstable), [1 Click Install](http://software.opensuse.org/download.html?project=science&package=julia-unstable)
* [NixOS](https://github.com/NixOS/nixpkgs/blob/master/pkgs/development/compilers/julia)
* Ubuntu
  * [Ubuntu](http://packages.ubuntu.com/search?keywords=julia)
  * [Nightly builds PPA](https://launchpad.net/~staticfloat/+archive/julianightlies) (depends on the [julia-deps PPA](https://launchpad.net/~staticfloat/+archive/julia-deps/))
* [MacPorts](https://trac.macports.org/browser/trunk/dports/lang/julia/Portfile)
* [OS X Homebrew Tap](https://github.com/staticfloat/homebrew-julia/)

<a name="Editor-Terminal-Setup"/>
## Editor and Terminal Setup

Currently, Julia editing mode support is available for a number of
editors. While Julia modes for
[Emacs](https://github.com/JuliaLang/julia-emacs),
[Sublime Text](https://github.com/JuliaEditorSupport/Julia-sublime), and
[Vim](https://github.com/JuliaLang/julia-vim) have their own repos,
others such as Textmate, Notepad++, and Kate, are in
`contrib/`.

Two major IDEs are supported for Julia: [Juno](http://junolab.org/),
which is based on [Atom](https://atom.io/), and
[JuliaDT](https://github.com/JuliaComputing/JuliaDT), which is an
[Eclipse](http://eclipse.org) plugin. A [Jupyter](http://jupyter.org/) notebooks interface
is available through
[IJulia](https://github.com/JuliaLang/IJulia.jl). The
[Sublime-IJulia](https://github.com/quinnj/Sublime-IJulia) plugin
enables interaction between IJulia and Sublime Text.

In the terminal, Julia makes great use of both control-key and meta-key bindings. To make the meta-key bindings more accessible, many terminal emulator programs (e.g., `Terminal`, `iTerm`, `xterm`, etc.) allow you to use the alt or option key as meta.  See the section in the manual on [interacting with Julia](http://docs.julialang.org/en/latest/manual/interacting-with-julia/) for more details.

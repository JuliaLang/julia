<a name="logo"/>
<div align="center">
<a href="http://julialang.org/" target="_blank">
<img src="http://julialang.org/images/logo_hires.png" alt="Julia Logo" width="210" height="142"></img>
</a>
</div>

[![Build Status](https://api.travis-ci.org/JuliaLang/julia.svg?branch=master)](https://travis-ci.org/JuliaLang/julia)

<a name="The-Julia-Language"/>
## The Julia Language

Julia is a high-level, high-performance dynamic language for technical computing.
The main homepage for Julia can be found at [julialang.org](http://julialang.org/).
This is the GitHub repository of Julia source code, including instructions for compiling and installing Julia, below.

<a name="Resources"/>

- **Homepage:** <http://julialang.org>
- **Binaries:** <http://julialang.org/downloads/>
- **Documentation:** <http://docs.julialang.org/>
- **Packages:** <http://docs.julialang.org/en/latest/packages/packagelist/>
- **Source code:** <https://github.com/JuliaLang/julia>
- **Git clone URL:** <git://github.com/JuliaLang/julia.git>
- **Mailing lists:** <http://julialang.org/community/>
- **IRC:** <http://webchat.freenode.net/?channels=julia>

The mailing list for developer discussion is
<http://groups.google.com/group/julia-dev/>. All are welcome, but the volume
of messages is higher, and the discussions tend to be more esoteric. New
developers may find the notes in [CONTRIBUTING](https://github.com/JuliaLang/julia/blob/master/CONTRIBUTING.md) helpful to start contributing to the Julia codebase.

<a name="Currently-Supported-Platforms"/>
## Currently Supported Platforms

- **GNU/Linux**
- **Darwin/OS X**
- **FreeBSD**
- **Windows**

All systems are supported with both x86/64 (64-bit) and x86 (32-bit) architectures. 

<a name="Source-Download-and-Compilation"/>
## Source Download and Compilation

First, acquire the source code by cloning the git repository:

    git clone git://github.com/JuliaLang/julia.git

If you are behind a firewall and you need to use the https protocol instead of the git protocol:

    git config --global url."https://".insteadOf git://

Next, enter the `julia/` directory and run `make` to build the `julia` executable. To perform a parallel build, use `make -j N` and supply the maximum number of concurrent processes.
When compiled the first time, it will automatically download and build its [external dependencies](#Required-Build-Tools-External-Libraries).
This takes a while, but only has to be done once. If the defaults in the build do not work for you, and you need to set specific make parameters, you can save them in `Make.user`. The build will automatically check for the existence of `Make.user` and use it if it exists.
Building Julia requires 1.5GiB of disk space and approximately 700MiB of virtual memory.

If you need to build Julia in an environment that does not allow access to the outside world, use `make -C deps getall` to download all the necessary files. Then, copy the julia directory over to the target environment and build with `make`.

**Note:** the build process will not work if any of the build directory's parent directories have spaces in their names (this is due to a limitation in GNU make).

Once it is built, you can run the `julia` executable using its full path in the directory created above (the `julia` directory), or, to run it from anywhere,

1. add a soft link to the `julia` executable in the `julia` directory to `/usr/local/bin` (or any suitable directory already in your path), or

2. add the `julia` directory to your executable path for this shell session (in bash: `export PATH="$(pwd):$PATH"` ; in csh or tcsh:
`set path= ( $path $cwd )` ), or

3. add the `julia` directory to your executable path permanently (e.g. in `.bash_profile`).

Now you should be able to run Julia like this:

    julia

If everything works correctly, you will see a Julia banner and an interactive prompt into which you can enter expressions for evaluation. (Errors related to libraries might be caused by old, incompatible libraries sitting around in your PATH. In that case, try moving the `julia` directory earlier in the PATH).

Your first test of Julia should be to determine whether your
build is working properly. From the UNIX/Windows command prompt inside
the julia source directory, type `make testall`. You should see output
that lists a series of tests being run; if they complete without
error, you should be in good shape to start using Julia.

You can read about [getting started](http://julialang.org/manual/getting-started) in the manual.

If you are building a Julia package for distribution on Linux, OS X,
or Windows, take a look at the detailed notes in
[DISTRIBUTING.md](https://github.com/JuliaLang/julia/blob/master/DISTRIBUTING.md).

<a name="Uninstalling-Julia"/>
## Uninstalling Julia

Julia does not install anything outside the directory it was cloned into. Julia can be completely uninstalled by deleting this directory. Julia packages are installed in `~/.julia` by default, and can be uninstalled by deleting `~/.julia`.

<a name="Platform-Specific-Notes"/>
## Platform-Specific Build Notes

### Linux

#### General

* GCC version 4.6 or later is recommended to build Julia.
* To use external shared libraries not in the system library search path, set `USE_SYSTEM_XXX=1` and `LDFLAGS=-Wl,-rpath /path/to/dir/contains/libXXX.so` in `Make.user`.
  * Instead of setting `LDFLAGS`, putting the library directory into the environment variable `LD_LIBRARY_PATH` (at both compile and run time) also works.
* See also the [external dependencies](#Required-Build-Tools-External-Libraries).

#### Ubuntu

The [julia-deps PPA](https://launchpad.net/~staticfloat/+archive/julia-deps/) contains updated packages for julia dependencies if you want to use system libraries instead of having them downloaded and built during the build process.  See [System Provided Libraries](#System-Provided-Libraries).

For a fast and easy current installation, the `before_install` section of [travis.yml](https://github.com/JuliaLang/julia/blob/master/.travis.yml) is a great resource.  Note that those instructions are for Ubuntu 12.04, and for later versions you may need to install newer versions of dependencies, such as `libunwind8-dev` instead of `libunwind7-dev`.

#### RHEL/CentOS 5

On RHEL/CentOS 5 systems, the default compiler (`gcc` 4.1) is too old to build Julia.

If the `gcc44` and `gfortran44` packages are installed, you can specify their use by adding the following to Make.user

    FC = gfortran44
    CC = gcc44
    CXX = g++44

Otherwise, install or contact your systems administrator to install a more recent version of `gcc`.

#### Google Compute Engine

Google Compute Engine is evolving rapidly, as is Julia.  This section is current as of March 2014 and assumes working knowledge of Google Cloud Services.

These notes apply to the Debian 7 image currently available on Google Compute Engine and Julia pre-0.3.  There are only two things you need to do:

1. Install packages required to build on your instance:
  ```
  apt-get install bzip2 gcc gfortran git g++ make m4 ncurses-dev
  ```

2. Now clone `JuliaLang:master` and edit `deps/Versions.make` to select `OPENBLAS_VER = v0.2.9.rc2`. This picks up changes to support the Sandybridge cores used by Google Compute Engine. (Alternatively, you could fall back to the Nehelem architecture via a `make` option, but that would entail performance penalties.)

Now you should be able to build using the generic Linux instructions. These instructions were tested on a `g1-small` instance on 2014-03-28. Other resources include [information on Google Compute Engine](https://cloud.google.com/products/compute-engine/) and a series of [tutorials by Julia Ferraioli](http://www.blog.juliaferraioli.com/2013/12/julia-on-google-compute-engine.html).

#### Linux Build Troubleshooting

 Problem              | Possible Solution
------------------------|---------------------
 OpenBLAS build failure | Set one of the following build options in `Make.user` and build again: <ul><li> `OPENBLAS_TARGET_ARCH=BARCELONA` (AMD CPUs) or `OPENBLAS_TARGET_ARCH=NEHALEM` (Intel CPUs)<ul>Set `OPENBLAS_DYNAMIC_ARCH = 0` to disable compiling multiple architectures in a single binary.</ul></li><li> `USE_SYSTEM_BLAS=1` uses the system provided `libblas` <ul><li>Set `LIBBLAS=-lopenblas` and `LIBBLASNAME=libopenblas` to force the use of the system provided OpenBLAS when multiple BLAS versions are installed </li></ul></li></ul>
 Illegal Instruction error | Check if your CPU supports AVX while your OS does not (e.g. through virtualization, as described in [this issue](https://github.com/JuliaLang/julia/issues/3263)), and try installing LLVM 3.3 instead of LLVM 3.2.

### OS X

It is essential to use a 64-bit gfortran to compile Julia dependencies. The gfortran-4.7 (and newer) compilers in brew and MacPorts work for building Julia. If you do not use brew or MacPorts, you can download and install [gfortran and gcc from hpc.sf.net](http://hpc.sf.net/). The HPC gfortran requires HPC gcc to be installed to function properly. 
Clang is now used by default to build Julia on OS X (10.7 and above). It is recommended that you upgrade to the latest version of Xcode (at least 4.3.3.). You need to have the Xcode command line utilities installed (and updated): run `xcode-select --install` in the terminal (in Xcode prior to v5.0, you can alternatively go to Preferences -> Downloads and select the Command Line Utilities). This will ensure that clang v3.1 is installed, which is the minimum version of `clang` required to build Julia. On OS X 10.6, the Julia build will automatically use `gcc`.

If you have set `LD_LIBRARY_PATH` or `DYLD_LIBRARY_PATH` in your `.bashrc` or equivalent, Julia may be unable to find various libraries that come bundled with it. These environment variables need to be unset for Julia to work.

If you see build failures in OpenBLAS or if you prefer to experiment, you can use the Apple provided BLAS in vecLib by building with `USE_SYSTEM_BLAS=1`. Julia does not use the Apple provided LAPACK, as it is too old.

### FreeBSD

On *FreeBSD Release 9.0*, install the `gcc46`, `git`, and `gmake` packages/ports, and compile Julia with the command:

    $ gmake FC=gfortran46

You must use the `gmake` command on FreeBSD instead of `make`.

### Windows

In order to build Julia on Windows, see [README.windows](https://github.com/JuliaLang/julia/blob/master/README.windows.md).

### Vagrant

Julia can be developed in an isolated Vagrant environment. See [the Vagrant README](https://github.com/JuliaLang/julia/blob/master/contrib/vagrant/README.md) for details.

<a name="Required-Build-Tools-External-Libraries"/>
## Required Build Tools and External Libraries

Building Julia requires that the following software be installed:

- **[GNU make]**                — building dependencies.
- **[gcc & g++][gcc]** or **[Clang][clang]** — compiling and linking C, C++ (if clang, need at least v3.1, Xcode 4.3.3 on OS X)
- **[gfortran][gcc]**           — compiling and linking fortran libraries
- **[git]**                     — version control and package management.
- **[perl]**                    — preprocessing of header files of libraries.
- **[wget]**, **[curl]**, or **fetch** (FreeBSD) — to automatically download external libraries.
- **[m4]**                      — needed to build GMP.
- **[patch]**                   — for modifying source code.

Julia uses the following external libraries, which are automatically downloaded (or in a few cases, included in the Julia source repository) and then compiled from source the first time you run `make`:

- **[LLVM]**                — compiler infrastructure.
- **[FemtoLisp]**           — packaged with Julia source, and used to implement the compiler front-end.
- **[libuv]**               — portable, high-performance event-based I/O library
- **[OpenLibm]**            — a portable libm library containing elementary math functions.
- **[OpenSpecFun]**         — a library containing Bessel and error functions of complex arguments.
- **[DSFMT]**               — a fast Mersenne Twister pseudorandom number generator library.
- **[OpenBLAS]**            — a fast, open, and maintained [basic linear algebra subprograms (BLAS)](http://en.wikipedia.org/wiki/Basic_Linear_Algebra_Subprograms) library, based on [Kazushige Goto's](http://en.wikipedia.org/wiki/Kazushige_Goto) famous [GotoBLAS](http://www.tacc.utexas.edu/tacc-projects/gotoblas2/). The system provided BLAS and LAPACK are used on OS X.
- **[LAPACK]**              — a library of linear algebra routines for solving systems of simultaneous linear equations, least-squares solutions of linear systems of equations, eigenvalue problems, and singular value problems.
- **[MKL]** (optional)      – OpenBLAS and LAPACK may be replaced by Intel's MKL library.
- **[AMOS]**                — subroutines for computing Bessel and Airy functions.
- **[SuiteSparse]**         — a library of linear algebra routines for sparse matrices.
- **[ARPACK]**              — a collection of subroutines designed to solve large, sparse eigenvalue problems.
- **[FFTW]**                — library for computing fast Fourier transforms very quickly and efficiently.
- **[PCRE]**                — Perl-compatible regular expressions library.
- **[GMP]**                 — the GNU multiple precision arithmetic library, needed for bigint support.
- **[MPFR]**                — the GNU multiple precision floating point library, needed for arbitrary precision floating point support.
- **[double-conversion]**   — efficient number-to-text conversion.


[GNU make]:     http://www.gnu.org/software/make/
[patch]:        http://www.gnu.org/software/patch/
[wget]:         http://www.gnu.org/software/wget/
[m4]:           http://www.gnu.org/software/m4/
[gcc]:          http://gcc.gnu.org/
[clang]:        http://clang.llvm.org/
[curl]:         http://curl.haxx.se/
[git]:          http://git-scm.com/
[perl]:         http://www.perl.org/
[OpenLibm]:     https://github.com/JuliaLang/openlibm
[OpenSpecFun]:  https://github.com/JuliaLang/openspecfun
[DSFMT]:        http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/#dSFMT
[OpenBLAS]:     https://github.com/xianyi/OpenBLAS
[LAPACK]:       http://www.netlib.org/lapack/
[MKL]:          http://software.intel.com/en-us/articles/intel-mkl/
[SuiteSparse]:  http://www.cise.ufl.edu/research/sparse/SuiteSparse/
[AMOS]:         http://netlib.org/amos
[ARPACK]:       http://forge.scilab.org/index.php/p/arpack-ng/
[FFTW]:         http://www.fftw.org/
[PCRE]:         http://www.pcre.org/
[LLVM]:         http://www.llvm.org/
[FemtoLisp]:    https://github.com/JeffBezanson/femtolisp
[GMP]:          http://gmplib.org/
[MPFR]:         http://www.mpfr.org/
[double-conversion]: http://double-conversion.googlecode.com/
[libuv]:        https://github.com/JuliaLang/libuv

<a name="System-Provided-Libraries">
### System Provided Libraries

If you already have one or more of these packages installed on your system, you can prevent Julia from compiling duplicates of these libraries by passing `USE_SYSTEM_...=1` to `make` or adding the line to `Make.user`. The complete list of possible flags can be found in `Make.inc`. 

Please be aware that this procedure is not officially supported, as it introduces additional variability into the installation and versioning of the dependencies, and is recommended only for system package maintainers. Unexpected compile errors may result, as the build system will do no further checking to ensure the proper packages are installed.

### SuiteSparse

SuiteSparse is a special case, since it is typically only installed as a static library, while `USE_SYSTEM_SUITESPARSE=1` requires that it is a shared library. Running the script `contrib/repackage_system_suitesparse4.make` will copy your static system SuiteSparse installation into the shared library format required by Julia. `make USE_SYSTEM_SUITESPARSE=1` will then use the SuiteSparse that has been copied into Julia's directory, but will not build a new SuiteSparse library from scratch.

### Intel Math Kernel Libraries

To use the Intel [MKL] BLAS and LAPACK libraries, make sure that MKL version 10.3.6 or higher is installed. For a 64-bit architecture, the MKL environment should be set up as:

    source /path/to/mkl/bin/mklvars.sh intel64 ilp64
    export MKL_INTERFACE_LAYER=ILP64

When building julia, pass the `USE_MKL=1` option to `make` or add the following line to `Make.user`.

    USE_MKL = 1

To rebuild a pre-built Julia source install with MKL support, delete the OpenBLAS, ARPACK, and SuiteSparse dependencies from `deps`, and run `make cleanall testall`.

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

Because of the rapid pace of development at this point, we recommend installing the latest Julia from source, but platform-specific tarballs with pre-compiled binaries are also [available for download](http://julialang.org/downloads/).

You can either run the `julia` executable using its full path in the directory created above, or add that directory to your executable path so that you can run the Julia program from anywhere (in the current shell session):

    export PATH="$(pwd)/julia:$PATH"

Now you should be able to run Julia like this:

    julia

On Windows, double-click `julia.bat`.

If everything works correctly, you will see a Julia banner and an interactive prompt into which you can enter expressions for evaluation.
You can read about [getting started](http://julialang.org/manual/getting-started) in the manual.

The following distributions include julia, but the versions may be out of date due to rapid development:

* [Arch Linux package](https://aur.archlinux.org/packages.php?ID=56877)
* [Debian GNU/Linux](http://packages.debian.org/sid/julia)
* [Gentoo Linux](https://packages.gentoo.org/package/dev-lang/julia)
  * Git Package in the [Science overlay](https://wiki.gentoo.org/wiki/Project:Science/Overlay)
* Ubuntu
  * [Ubuntu 13.04 (Raring Ringtail)](http://packages.ubuntu.com/raring/julia)
  * [Nightly builds PPA](https://launchpad.net/~staticfloat/+archive/julianightlies) (depends on the [julia-deps PPA](https://launchpad.net/~staticfloat/+archive/julia-deps/))
* [OS X Homebrew Tap](https://github.com/staticfloat/homebrew-julia/)

<a name="Editor-Terminal-Setup"/>
## Editor and Terminal Setup

Currently, Julia editing mode support is available for Emacs, Vim, Textmate, Sublime Text, Notepad++, and Kate, in `contrib/`.

In the terminal, Julia makes great use of both control-key and meta-key bindings. To make the meta-key bindings more accessible, many terminal emulator programs (e.g., `Terminal`, `iTerm`, `xterm`, etc) allow you to use the alt or option key as meta.  See the section in the manual on [interacting with Julia](http://docs.julialang.org/en/latest/manual/interacting-with-julia/) for more details.

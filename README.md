<a name="logo"/>
<div align="center">
<a href="https://julialang.org/" target="_blank">
<img src="https://julialang.org/images/logo_hires.png" alt="Julia Logo" width="210" height="142"></img>
</a>
</div>

Linux, OSX: [![Build Status](https://api.travis-ci.org/JuliaLang/julia.svg?branch=master)](https://travis-ci.org/JuliaLang/julia)

Windows: [![Build status](https://ci.appveyor.com/api/projects/status/dvial98s5vi6ealt/branch/master?svg=true)](https://ci.appveyor.com/project/JuliaLang/julia/branch/master)

Code Coverage: [![Coverage Status](https://coveralls.io/repos/JuliaLang/julia/badge.svg?branch=master)](https://coveralls.io/r/JuliaLang/julia?branch=master) [![codecov.io](http://codecov.io/github/JuliaLang/julia/coverage.svg?branch=master)](http://codecov.io/github/JuliaLang/julia?branch=master)

## The Julia Language

Julia is a high-level, high-performance dynamic language for technical computing.
The main homepage for Julia can be found at [julialang.org](https://julialang.org/).
This is the GitHub repository of Julia source code, including instructions for compiling and installing Julia, below.

## Resources

- **Homepage:** <https://julialang.org>
- **Binaries:** <https://julialang.org/downloads/>
- **Documentation:** <https://docs.julialang.org/>
- **Packages:** <http://pkg.julialang.org/>
- **Source code:** <https://github.com/JuliaLang/julia>
- **Git clone URL:** <git://github.com/JuliaLang/julia.git>
- **Discussion forum:** <https://discourse.julialang.org>
- **Mailing lists:** <https://julialang.org/community/>
- **Slack:** <https://julialang.slack.com> (get an invite from <https://slackinvite.julialang.org>)
- **Gitter:** <https://gitter.im/JuliaLang/julia>
- **IRC:** <http://webchat.freenode.net/?channels=julia>
- **Code coverage:** <https://coveralls.io/r/JuliaLang/julia>

New developers may find the notes in [CONTRIBUTING](https://github.com/JuliaLang/julia/blob/master/CONTRIBUTING.md) helpful to start contributing to the Julia codebase.

### External Resources

- [**StackOverflow**](https://stackoverflow.com/questions/tagged/julia-lang)
- [**Youtube**](https://www.youtube.com/channel/UC9IuUwwE2xdjQUT_LMLONoA)
- [**Twitter**](https://twitter.com/JuliaLanguage)
- [**Meetup**](http://julia.meetup.com/)

## Currently Supported Platforms

Julia is built and tested regularly on the following platforms:

| Operating System | Architecture     | CI | Binaries | Support Level |
|:----------------:|:----------------:|:--:|:--------:|:-------------:|
| Linux 2.6.18+    | x86-64 (64-bit)  | ✓  | ✓        | Official      |
|                  | i686 (32-bit)    | ✓  | ✓        | Official      |
|                  | ARM v7 (32-bit)  |    | ✓        | Official      |
|                  | ARM v8 (64-bit)  |    | ✓        | Official      |
|                  | PowerPC (64-bit) |    |          | Community     |
|                  | PTX (64-bit)     | [✓](http://ci.maleadt.net:8010/)  |          | [External](https://github.com/JuliaGPU/CUDAnative.jl)     |
| macOS 10.8+      | x86-64 (64-bit)  | ✓  | ✓        | Official      |
| Windows 7+       | x86-64 (64-bit)  | ✓  | ✓        | Official      |
|                  | i686 (32-bit)    | ✓  | ✓        | Official      |
| FreeBSD 11.0+    | x86-64 (64-bit)  | ✓  |          | Community     |

All systems marked with ✓ for CI are tested using continuous integration for every commit.
Systems with ✓ for binaries have official binaries available on the [downloads](https://julialang.org/downloads) page and are tested regularly. The PTX backend needs a source build and the [CUDAnative.jl](https://github.com/JuliaGPU/CUDAnative.jl) package.
The systems listed here with neither CI nor official binaries are known to build and work, but ongoing support for those platforms is dependent on community efforts.
It's possible that Julia will build and work on other platforms too, and we're always looking to better our platform coverage.
If you're using Julia on a platform not listed here, let us know!

## Source Download and Compilation

First, make sure you have all the [required dependencies](#required-build-tools-and-external-libraries) installed.
Then, acquire the source code by cloning the git repository:

    git clone git://github.com/JuliaLang/julia.git

(If you are behind a firewall, you may need to use the `https` protocol instead of the `git` protocol:

    git config --global url."https://".insteadOf git://

Be sure to also configure your system to use the appropriate proxy settings, e.g. by setting the `https_proxy` and `http_proxy` variables.)

By default you will be building the latest unstable version of Julia. However, most users should use the most recent stable version of Julia, which is currently the `0.6` series of releases. You can get this version by changing to the Julia directory and running

    git checkout v0.6.0

Now run `make` to build the `julia` executable. To perform a parallel build, use `make -j N` and supply the maximum number of concurrent processes. (See [Platform Specific Build Notes](https://github.com/JuliaLang/julia#platform-specific-build-notes) for details.)
When compiled the first time, it will automatically download and build its [external dependencies](#required-build-tools-and-external-libraries).
This takes a while, but only has to be done once. If the defaults in the build do not work for you, and you need to set specific make parameters, you can save them in `Make.user`. The build will automatically check for the existence of `Make.user` and use it if it exists.
Building Julia requires 1.5GiB of disk space and approximately 700MiB of virtual memory.

For builds of julia starting with 0.5.0-dev, you can create out-of-tree builds of Julia by specifying `make O=<build-directory> configure` on the command line. This will create a directory mirror, with all of the necessary Makefiles to build Julia, in the specified directory. These builds will share the source files in Julia and `deps/srccache`. Each out-of-tree build directory can have its own `Make.user` file to override the global `Make.user` file in the top-level folder.

If you need to build Julia on a machine without internet access, use `make -C deps getall` to download all the necessary files. Then, copy the `julia` directory over to the target environment and build with `make`.

**Note:** The build process will fail badly if any of the build directory's parent directories have spaces or other shell meta-characters such as `$` or `:` in their names (this is due to a limitation in GNU make).

Once it is built, you can run the `julia` executable using its full path in the directory created above (the `julia` directory). To run julia from anywhere you can:
- add an alias (in `bash`: `echo "alias julia='/path/to/install/folder/bin/julia'" >> ~/.bashrc && source ~/.bashrc`), or
- add a soft link to the `julia` executable in the `julia` directory to `/usr/local/bin` (or any suitable directory already in your path), or

- add the `julia` directory to your executable path for this shell session (in `bash`: `export PATH="$(pwd):$PATH"` ; in `csh` or `tcsh`:
`set path= ( $path $cwd )` ), or

- add the `julia` directory to your executable path permanently (e.g. in `.bash_profile`), or

- write `prefix=/path/to/install/folder` into `Make.user` and then run `make install`. If there is a version of Julia already installed in this folder, you should delete it before running `make install`.

Now you should be able to run Julia like this:

    julia

If everything works correctly, you will see a Julia banner and an interactive prompt into which you can enter expressions for evaluation. (Errors related to libraries might be caused by old, incompatible libraries sitting around in your PATH. In this case, try moving the `julia` directory earlier in the PATH).

Your first test of Julia determines whether your build is working properly. From the UNIX/Windows command prompt inside
the `julia` source directory, type `make testall`. You should see output that lists a series of running tests;
if they complete without error, you should be in good shape to start using Julia.

You can read about [getting started](https://docs.julialang.org/en/stable/manual/getting-started/) in the manual.

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



## Uninstalling Julia

Julia does not install anything outside the directory it was cloned into. Julia can be completely uninstalled by deleting this directory. Julia packages are installed in `~/.julia` by default, and can be uninstalled by deleting `~/.julia`.

## Platform-Specific Build Notes

### Linux

#### General

* GCC version 4.7 or later is required to build Julia.
* To use external shared libraries not in the system library search path, set `USE_SYSTEM_XXX=1` and `LDFLAGS=-Wl,-rpath,/path/to/dir/contains/libXXX.so` in `Make.user`.
  * Instead of setting `LDFLAGS`, putting the library directory into the environment variable `LD_LIBRARY_PATH` (at both compile and run time) also works.
* See also the [external dependencies](#required-build-tools-and-external-libraries).

#### Architecture Customization

Julia can be built for a non-generic architecture by configuring the `ARCH` Makefile variable. See the appropriate section of `Make.inc` for additional customization options, such as `MARCH` and `JULIA_CPU_TARGET`.

For example, to build for Pentium 4, set `MARCH=pentium4` and install the necessary system libraries for linking. On Ubuntu, these may include lib32gfortran-6-dev, lib32gcc1, and lib32stdc++6, among others.

You can also set `MARCH=native` for a maximum-performance build customized for the current machine CPU.


#### Ubuntu

The [julia-deps PPA](https://launchpad.net/~staticfloat/+archive/julia-deps/) contains updated packages for Julia dependencies if you want to use system libraries instead of having them downloaded and built during the build process.  See [System Provided Libraries](#system-provided-libraries).

#### RHEL/CentOS 6

On RHEL/CentOS 6 systems, the default compiler (`gcc` 4.4) is too old to build Julia.

Install or contact your systems administrator to install a more recent version of `gcc`. The [Scientific Linux Developer Toolset](http://linux.web.cern.ch/linux/devtoolset/) works well.


#### Linux Build Troubleshooting

 Problem              | Possible Solution
------------------------|---------------------
 OpenBLAS build failure | Set one of the following build options in `Make.user` and build again: <ul><li> `OPENBLAS_TARGET_ARCH=BARCELONA` (AMD CPUs) or `OPENBLAS_TARGET_ARCH=NEHALEM` (Intel CPUs)<ul>Set `OPENBLAS_DYNAMIC_ARCH = 0` to disable compiling multiple architectures in a single binary.</ul></li><li> `OPENBLAS_NO_AVX2 = 1` disables AVX2 instructions, allowing OpenBLAS to compile with `OPENBLAS_DYNAMIC_ARCH = 1` using old versions of binutils </li><li> `USE_SYSTEM_BLAS=1` uses the system provided `libblas` <ul><li>Set `LIBBLAS=-lopenblas` and `LIBBLASNAME=libopenblas` to force the use of the system provided OpenBLAS when multiple BLAS versions are installed. </li></ul></li></ul><p> If you get an error that looks like ```../kernel/x86_64/dgemm_kernel_4x4_haswell.S:1709: Error: no such instruction: `vpermpd $ 0xb1,%ymm0,%ymm0'```, then you need to set `OPENBLAS_DYNAMIC_ARCH = 0` or `OPENBLAS_NO_AVX2 = 1`, or you need a newer version of `binutils` (2.18 or newer). ([Issue #7653](https://github.com/JuliaLang/julia/issues/7653))
Illegal Instruction error | Check if your CPU supports AVX while your OS does not (e.g. through virtualization, as described in [this issue](https://github.com/JuliaLang/julia/issues/3263)).

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

Clang is the default compiler on FreeBSD 11.0-RELEASE and above.
The remaining build tools are available from the Ports Collection, and can be installed using `pkg install git gcc gmake cmake`.
To build Julia, simply run `gmake`.
(Note that `gmake` must be used rather than `make`, since `make` on FreeBSD corresponds to the incompatible BSD Make rather than GNU Make.)

It's important to note that the `USE_SYSTEM_*` flags should be used with caution on FreeBSD.
This is because many system libraries, and even libraries from the Ports Collection, link to the system's `libgcc_s.so.1`,
or to another library which links to the system `libgcc_s`.
This library declares its GCC version to be 4.6, which is too old to build Julia, and conflicts with other libraries when linking.
Thus it is highly recommended to simply allow Julia to build all of its dependencies.
If you do choose to use the `USE_SYSTEM_*` flags, note that `/usr/local` is not on the compiler path by default, so you may need
to add `LDFLAGS=-L/usr/local/lib` and `CPPFLAGS=-I/usr/local/include` to your `Make.user`, though doing so may interfere with
other dependencies.

Some known issues on FreeBSD are:

* The x86 architecture does not support threading due to lack of compiler runtime library support, so you may need to
  set `JULIA_THREADS=0` in your `Make.user` if you're on a 32-bit system.

* The `Pkg` test suite segfaults on FreeBSD 11.1, likely due to a change in FreeBSD's default handling of stack guarding.
  See [issue #23328](https://github.com/JuliaLang/julia/issues/23328) for more information.

### Windows

In order to build Julia on Windows, see [README.windows](https://github.com/JuliaLang/julia/blob/master/README.windows.md).

### Vagrant

Julia can be developed in an isolated Vagrant environment. See [the Vagrant README](https://github.com/JuliaLang/julia/blob/master/contrib/vagrant/README.md) for details.

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
- **[DSFMT]**                — fast Mersenne Twister pseudorandom number generator library.
- **[OpenBLAS]**             — fast, open, and maintained [basic linear algebra subprograms (BLAS)](https://en.wikipedia.org/wiki/Basic_Linear_Algebra_Subprograms) library, based on [Kazushige Goto's](https://en.wikipedia.org/wiki/Kazushige_Goto) famous [GotoBLAS](https://www.tacc.utexas.edu/research-development/tacc-software/gotoblas2).
- **[LAPACK]** (>= 3.5)      — library of linear algebra routines for solving systems of simultaneous linear equations, least-squares solutions of linear systems of equations, eigenvalue problems, and singular value problems.
- **[MKL]** (optional)       – OpenBLAS and LAPACK may be replaced by Intel's MKL library.
- **[SuiteSparse]** (>= 4.1) — library of linear algebra routines for sparse matrices.
- **[ARPACK]**               — collection of subroutines designed to solve large, sparse eigenvalue problems.
- **[PCRE]** (>= 10.00)      — Perl-compatible regular expressions library.
- **[GMP]** (>= 5.0)         — GNU multiple precision arithmetic library, needed for `BigInt` support.
- **[MPFR]** (>= 3.0)        — GNU multiple precision floating point library, needed for arbitrary precision floating point (`BigFloat`) support.
- **[libgit2]** (>= 0.23)    — Git linkable library, used by Julia's package manager
- **[curl]** (>= 7.50)       — libcurl provides download and proxy support for Julia's package manager
- **[libssh2]** (>= 1.7)     — library for SSH transport, used by libgit2 for packages with SSH remotes
- **[mbedtls]** (>= 2.2)     — library used for cryptography and transport layer security, used by libssh2
- **[utf8proc]** (>= 2.1)    — a library for processing UTF-8 encoded Unicode strings
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
[DSFMT]:        http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/#dSFMT
[OpenBLAS]:     https://github.com/xianyi/OpenBLAS
[LAPACK]:       http://www.netlib.org/lapack
[MKL]:          http://software.intel.com/en-us/articles/intel-mkl
[SuiteSparse]:  http://faculty.cse.tamu.edu/davis/suitesparse.html
[ARPACK]:       http://forge.scilab.org/index.php/p/arpack-ng
[PCRE]:         http://www.pcre.org
[LLVM]:         http://www.llvm.org
[FemtoLisp]:    https://github.com/JeffBezanson/femtolisp
[GMP]:          http://gmplib.org
[MPFR]:         http://www.mpfr.org
[libuv]:        https://github.com/JuliaLang/libuv
[libgit2]:      https://libgit2.github.com/
[utf8proc]:     https://julialang.org/utf8proc/
[libosxunwind]: https://github.com/JuliaLang/libosxunwind
[libunwind]:    http://www.nongnu.org/libunwind
[libssh2]:      https://www.libssh2.org
[mbedtls]:      https://tls.mbed.org/
[pkg-config]:   https://www.freedesktop.org/wiki/Software/pkg-config/

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
    USE_INTEL_LIBM = 1

It is highly recommended to start with a fresh clone of the Julia repository.

## Source Code Organization

The Julia source code is organized as follows:

    base/          source code for the Base module (part of Julia's standard library)
    stdlib/        source code for other standard library packages
    contrib/       editor support for Julia source, miscellaneous scripts
    deps/          external dependencies
    doc/src/manual source for the user manual
    doc/src/stdlib source for standard library function reference
    examples/      example Julia programs
    src/           source for Julia language core
    test/          test suites
    test/perf      benchmark suites
    ui/            source for various front ends
    usr/           binaries and shared libraries loaded by Julia's standard libraries

## Binary Installation

If you would rather not compile the latest Julia from source, platform-specific tarballs with pre-compiled binaries are also [available for download](https://julialang.org/downloads/).

You can either run the `julia` executable using its full path in the directory created above, or add that directory to your executable path so that you can run the Julia program from anywhere (in the current shell session):

    export PATH="$(pwd)/julia:$PATH"

Now you should be able to run Julia like this:

    julia

On Windows, double-click `usr/bin/julia.exe`.

If everything works correctly, you will see a Julia banner and an interactive prompt into which you can enter expressions for evaluation.
You can read about [getting started](https://julialang.org/manual/getting-started) in the manual.

**Note**: While some system package managers have Julia installers available,
these are not maintained nor endorsed by the Julia project. They may be outdated
and/or unmaintained. We recommend you use the official Julia binaries instead.

## Editor and Terminal Setup

Currently, Julia editing mode support is available for a number of
editors. While Julia modes for
[Emacs](https://github.com/JuliaLang/julia-emacs),
[Sublime Text](https://github.com/JuliaEditorSupport/Julia-sublime), and
[Vim](https://github.com/JuliaLang/julia-vim) have their own repos,
others such as Textmate, Notepad++, and Kate, are in
`contrib/`.

Three major IDEs are supported for Julia: [Juno](http://junolab.org/)
which is based on [Atom](https://atom.io/),
[julia-vscode](https://github.com/JuliaEditorSupport/julia-vscode)
based on [VS Code](https://code.visualstudio.com/), and
[JuliaDT](https://github.com/JuliaComputing/JuliaDT), which is an
[Eclipse](http://eclipse.org) plugin. A [Jupyter](http://jupyter.org/) notebooks interface
is available through
[IJulia](https://github.com/JuliaLang/IJulia.jl). The
[Sublime-IJulia](https://github.com/quinnj/Sublime-IJulia) plugin
enables interaction between IJulia and Sublime Text.

In the terminal, Julia makes great use of both control-key and meta-key bindings. To make the meta-key bindings more accessible, many terminal emulator programs (e.g., `Terminal`, `iTerm`, `xterm`, etc.) allow you to use the alt or option key as meta.  See the section in the manual on [interacting with Julia](https://docs.julialang.org/en/latest/manual/interacting-with-julia) for more details.

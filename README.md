<a name="banner"/>

                   _
       _       _ _(_)_     |
      (_)     | (_) (_)    |   A fresh approach to technical computing
       _ _   _| |_  __ _   |
      | | | | | | |/ _` |  |          http://julialang.org
      | | |_| | | | (_| |  |       julia-users@googlegroups.com
     _/ |\__'_|_|_|\__'_|  |           #julia on freenode
    |__/                   |


[![Build Status](https://api.travis-ci.org/JuliaLang/julia.png?branch=master)](https://travis-ci.org/JuliaLang/julia)

<a name="The-Julia-Language"/>
## The Julia Language

Julia is a high-level, high-performance dynamic language for technical computing.
The main homepage for Julia can be found at [julialang.org](http://julialang.org/).
This is the GitHub repository of Julia source code, including instructions for compiling and installing Julia, below.

<a name="Resources"/>

- **Homepage:** <http://julialang.org>
- **Binaries:** <http://code.google.com/p/julialang/downloads/list>
- **Packages:** <http://docs.julialang.org/en/latest/packages/packagelist/>
- **Mailing lists:** <http://julialang.org/mailing_lists>
- **IRC:** <http://webchat.freenode.net/?channels=julia>
- **Source code:** <https://github.com/JuliaLang/julia>
- **Git clone URL:** <git://github.com/JuliaLang/julia.git>
- **Documentation:** <http://julialang.org/manual/>

The mailing list for developer discussion is
<http://groups.google.com/group/julia-dev/>. All are welcome, but the volume
of messages is higher, and the discussions tend to be more esoteric. New
developers may find the notes in [CONTRIBUTING](https://github.com/JuliaLang/julia/blob/master/CONTRIBUTING.md) helpful to start contributing to the julia codebase.

<a name="Currently-Supported-Platforms"/>
## Currently Supported Platforms

- **GNU/Linux**
- **Darwin/OS X**
- **FreeBSD**
- **Windows**

All systems are supported with both x86/64 (64-bit) and x86 (32-bit) architectures. 

<a name="Source-Download-Compilation"/>
## Source Download & Compilation

First, acquire the source code by cloning the git repository:

    git clone git://github.com/JuliaLang/julia.git

If you are behind a firewall and you need to use the https protocol instead of the git protocol:

    git config --global url."https://".insteadOf git://

Next, enter the `julia/` directory and run `make` to build the `julia` executable. To perform a parallel build, use `make -j N` and supply the maximum number of concurrent processes.
When compiled the first time, it will automatically download and build its [external dependencies](#Required-Build-Tools-External-Libraries).
This takes a while, but only has to be done once. If the defaults in the build do not work for you, and you need to set specific make parameters, you can save them in `Make.user`. The build will automatically check for the existence of `Make.user` and use it if it exists.
Building julia requires 1.5GiB of diskspace and approximately 700MiB of virtual memory.

**Note:** the build process will not work if any of the build directory's parent directories have spaces in their names (this is due to a limitation in GNU make).

Once it is built, you can either run the `julia` executable using its full path in the directory created above, or add that directory to your executable path so that you can run the julia program from anywhere (in the current shell session):

In bash:

    export PATH="$(pwd):$PATH"
    
In csh / tcsh:

    set path= ( $path $cwd )

Now you should be able to run julia like this:

    julia

If everything works correctly, you will see a Julia banner and an interactive prompt into which you can enter expressions for evaluation.
You can read about [getting started](http://julialang.org/manual/getting-started) in the manual.

<a name="Platform-Specific-Notes"/>
### Platform-Specific Notes

#### Linux

GCC version 4.6 or later is recommended to build julia.

If the build fails trying to compile OpenBLAS, set OPENBLAS_TARGET_ARCH to BARCELONA on AMD, or NEHALEM on Intel CPUs in Make.inc and build again.

On some Linux distributions you may need to change how the readline library is linked. If you get a build error involving readline, set `USE_SYSTEM_READLINE=1` in `Make.user`.

On Ubuntu systems, you may also need to install the package `libncurses5-dev`.

On CentOS 5 systems, the default compiler (gcc 4.1) is too old to build julia.

#### OS X

It is essential to use a 64-bit gfortran to compile Julia dependencies. The gfortran-4.7 compilers in brew and macports work for building julia. If you do not use brew or macports, you can download and install [gfortran and gcc from hpc.sf.net](http://hpc.sf.net/). The HPC gfortran requires gcc to function properly. 

Clang is now used by default to build julia on OS X (10.7 and above). Make sure to update to at least Xcode 4.3.3, and update to the latest command line tools from the Xcode preferences. This will ensure that clang v3.1 is installed, which is the minimum version of clang required to build julia. On older systems, the julia build will attempt to use gcc. The build also detects Snow Leopard and sets `USE_SYSTEM_LIBM=1`, `USE_SYSTEM_BLAS=1`, and `USE_SYSTEM_LAPACK=1`.

#### FreeBSD

*Release 9.0:* install the gcc46, git, and gmake packages/ports, and compile julia with the command:

    $ gmake FC=gfortran46

You must use the gmake command on FreeBSD instead of make.

#### MKL

To use the Intel [MKL] BLAS & LAPACK libraries, edit the following settings in `Make.inc`:

    USE_MKL = 1
    MKLLIB = /path/to/mkl/lib/arch

`MKLLIB` points to the directory containing `libmkl_rt.so`. Requires v10.3 or greater.
To rebuild a pre-built Julia source install with MKL support, delete from `deps/`, the OpenBLAS, ARPACK, and SuiteSparse dependencies, then run `make cleanall testall`.

<a name="Required-Build-Tools-External-Libraries"/>
## Required Build Tools & External Libraries

Building Julia requires that the following software be installed:

- **[GNU make]**                — building dependencies.
- **[gcc, g++][gcc]**           — compiling and linking C, C++
- **[clang][clang]**            — clang is the default compiler on OS X (Need at least v3.1, Xcode 4.3.3 on OS X)
- **[gfortran][gcc]**           — compiling and linking fortran libraries
- **[git]**                     — contributions and version control.
- **[perl]**                    — preprocessing of header files of libraries.
- **[wget]**, **[curl]**, or **fetch** (FreeBSD) — to automatically download external libraries.
- **[m4]**                      — needed to build GMP.
- **[patch]**                   — for modifying source code.

Julia uses the following external libraries, which are automatically downloaded (or in a few cases, included in the Julia source repository) and then compiled from source the first time you run `make`:

- **[LLVM]**                — compiler infrastructure. Currently, julia requires LLVM 3.1.
- **[FemtoLisp]**           — packaged with julia source, and used to implement the compiler front-end.
- **[readline]**            — library allowing shell-like line editing in the terminal, with history and familiar key bindings.
- **[libuv]**               — portable, high-performance event-based I/O library
- **[OpenLibm]**            — a portable libm library containing elementary math functions.
- **[DSFMT]**               — a fast Mersenne Twister pseudorandom number generator library.
- **[OpenBLAS]**            — a fast, open, and maintained [basic linear algebra subprograms (BLAS)](http://en.wikipedia.org/wiki/Basic_Linear_Algebra_Subprograms) library, based on [Kazushige Goto's](http://en.wikipedia.org/wiki/Kazushige_Goto) famous [GotoBLAS](http://www.tacc.utexas.edu/tacc-projects/gotoblas2/). The system provided BLAS and LAPACK are used on OS X.
- **[LAPACK]**              — a library of linear algebra routines for solving systems of simultaneous linear equations, least-squares solutions of linear systems of equations, eigenvalue problems, and singular value problems.
- **[MKL]** (optional)      – OpenBLAS & LAPACK may be replaced by Intel's MKL library.
- **[AMOS]**                — subroutines for computing Bessel and Airy functions.
- **[SuiteSparse]**         — a library of linear algebra routines for sparse matrices.
- **[ARPACK]**              — a collection of subroutines designed to solve large, sparse eigenvalue problems.
- **[FFTW]**                — library for computing fast Fourier transforms very quickly and efficiently.
- **[PCRE]**                — Perl-compatible regular expressions library.
- **[GMP]**                 — the GNU multiple precision arithmetic library, needed for bigint support.
- **[D3]**                  — JavaScript visualization library.
- **[double-conversion]**   — efficient number-to-text conversion.
- **[Rmath]**               — basic RNGs and distributions.


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
[readline]:     http://cnswww.cns.cwru.edu/php/chet/readline/rltop.html
[GMP]:          http://gmplib.org/
[D3]:           http://mbostock.github.com/d3/
[double-conversion]: http://double-conversion.googlecode.com/
[Rmath]:        http://cran.r-project.org/doc/manuals/R-admin.html#The-standalone-Rmath-library
[libuv]:        https://github.com/JuliaLang/libuv

If you already have one or more of these packages installed on your system, it is possible to pass `USE_SYSTEM_...=1` to `make` to prevent Julia from compiling duplicates of these libraries. The complete list of possible flags can be found in Make.inc. Please be aware that this procedure is not officially supported, as it introduces additional variablity into the installation and versioning of the dependencies, and is recommended only for system package maintainers. Unexpected compile errors may result, as the build system will do no further checking to ensure the proper packages are installed.

SuiteSparse is a special case, since it is typically only installed as a static library, while `USE_SYSTEM_SUITESPARSE=1` requires that it is a shared library. Running the script `contrib/repackage_system_suitesparse4.make` will copy your static system SuiteSparse installation into the shared library format required by Julia.

<a name="Directories"/>
## Directories

    base/          source code for Julia's standard library
    contrib/       emacs, vim and textmate support for Julia
    deps/          external dependencies
    examples/      example Julia programs
    extras/        useful optional libraries
    src/           source for Julia language core
    test/          unit and functional test cases
    ui/            source for various front ends
    usr/           binaries and shared libraries loaded by Julia's standard libraries

<a name="Binary-Installation"/>
## Binary Installation

Because of the rapid pace of development at this point, we recommend installing the latest Julia from source, but platform-specific tarballs with pre-compiled binaries are also [available for download](http://code.google.com/p/julialang/downloads/list).

You can either run the `julia` executable using its full path in the directory created above, or add that directory to your executable path so that you can run the julia program from anywhere (in the current shell session):

    export PATH="$(pwd)/julia:$PATH"

Now you should be able to run julia like this:

    julia

If everything works correctly, you will see a Julia banner and an interactive prompt into which you can enter expressions for evaluation.
You can read about [getting started](http://julialang.org/manual/getting-started) in the manual.

An [Arch Linux package](https://aur.archlinux.org/packages.php?ID=56877) is also available. Julia has also been added to [Debian](http://packages.debian.org/sid/julia). On OS X, julia is available through [homebrew](http://mxcl.github.com/homebrew/).


<a name="Editor-Terminal-Setup"/>
## Editor & Terminal Setup

Currently, [julia editing mode](https://github.com/JuliaLang/julia/wiki/Configuring-Editors) support is available for Emacs, Vim, Textmate, Notepad++, and Kate.

Adjusting your terminal bindings is optional; everything will work fine without these key bindings.
For the best interactive session experience, however, make sure that your terminal emulator (`Terminal`, `iTerm`, `xterm`, etc.) sends the `^H` sequence for `Backspace` (delete key) and that the `Shift-Enter` key combination sends a `\n` newline character to distinguish it from just pressing `Enter`, which sends a `\r` carriage return character.
These bindings allow custom readline handlers to trap and correctly deal with these key sequences; other programs will continue to behave normally with these bindings.
The first binding makes backspacing through text at the prompt behave more intuitively.
The second binding allows `Shift-Enter` to insert a newline without evaluating the current expression, even when the current expression is complete.
(Pressing an unmodified `Enter` inserts a newline if the current expression is incomplete, evaluates the expression if it is complete, or shows an error if the syntax is irrecoverably invalid.)

On Linux systems, the `Shift-Enter` binding can be set by placing the following line in the file `.xmodmaprc` in your home directory:

    keysym Return = Return Linefeed

<a name="Web-REPL">
## Web REPL

Julia has a web REPL with very preliminary graphics capabilities. The web REPL is currently a showcase to try out new ideas. The web REPL is social - multiple people signing in with a common session name can collaborate within a session.

1. Do `make webrepl` to build the julia webserver.
2. Start the web REPL service with `./usr/bin/launch-julia-webserver`.
3. Point your browser to `http://localhost:2000/`.
4. Try `plot(cumsum(randn(1000)))` and other things.

### Try it Online

[Forio.com](http://forio.com/) is generously hosting and maintaining an instance of Julia's web REPL here: [julia.forio.com](http://julia.forio.com).
This service is best-effort and may not always be up or stable. Be nice!

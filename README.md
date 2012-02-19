<a name="banner"/>

                   _
       _       _ _(_)_     |
      (_)     | (_) (_)    |   A fresh approach to technical computing
       _ _   _| |_  __ _   |
      | | | | | | |/ _` |  |           http://julialang.org
      | | |_| | | | (_| |  |       julia-dev@googlegroups.com
     _/ |\__'_|_|_|\__'_|  |
    |__/                   |


<a name="The-Julia-Language"/>
## The Julia Language

Julia is a high-level, high-performance dynamic language for technical computing.
The main homepage for Julia can be found at [julialang.org](http://julialang.org/).
This is the GitHub repository of Julia source code, including instructions for compiling and installing Julia, below.

<a name="Resources"/>

- **Homepage:** <http://julialang.org>
- **Discussion:** <http://groups.google.com/group/julia-dev/>
- **Source code:** <https://github.com/JuliaLang/julia>
- **Git clone URL:** <git://github.com/JuliaLang/julia.git>
- **Documentation:** <https://github.com/JuliaLang/julia/wiki>

<a name="Supported-Platforms"/>
## Supported Platforms

- **GNU/Linux:** x86/64 (64-bit); x86 (32-bit).
- **Darwin/OS X:** x86/64 (64-bit)

<a name="Source-Download-Compilation"/>
## Source Download & Compilation

First, acquire the source code either by cloning the git repository (requires **[git](http://git-scm.com/)** to be installed):

    git clone git://github.com/JuliaLang/julia.git

or, if you don't have git installed, by using wget and tar to fetch and unpack the source:

    mkdir julia && wget -O - https://github.com/JuliaLang/julia/tarball/master | tar -zxf- -C julia --strip-components 1

Next, enter the `julia/` directory and run `make` to build the `julia` executable.
When compiled the first time, it will automatically download and build its [external dependencies](#Required-Build-Tools-External-Libraries).
This takes a while, but only has to be done once.
*Note:* the build process will not work if any of the build directory's parent directories have spaces in their names (this is due to a limitation in GNU make).

Once it is built, you can either run the `julia` executable using its full path in the directory created above, or add that directory to your executable path so that you can run the julia program from anywhere:

    export PATH="$(pwd)/julia:$PATH"

Now you should be able to run julia like this:

    julia

If everything works correctly, you will see a Julia banner and an interactive prompt into which you can enter expressions for evaluation.
You can read about [getting started](https://github.com/JuliaLang/julia/wiki/Getting-Started) in the manual.

<a name="Platform-Specific-Notes"/>
### Platform-Specific Notes

On some Linux distributions (for instance Ubuntu 11.10) you may need to change how the readline library is linked. If you get a build error involving readline, try changing the value of `USE_SYSTEM_READLINE` in `Make.inc` to `1`.

On Ubuntu, you may also need to install the package `libncurses5-dev`.

If OpenBLAS fails to build in `getarch_2nd.c`, you need to specify the architecture of your processor in Make.inc.

<a name="Required-Build-Tools-External-Libraries"/>
## Required Build Tools & External Libraries

Julia depends on a number of mature, state-of-the art C and Fortran libraries.
When installing from source, these libraries are automatically downloaded and compiled.

- **[GNU make]**                — building dependencies.
- **[gcc, g++, gfortran][gcc]** — compiling and linking C, C++ and Fortran code.
- **[Perl]**                    — preprocessing of header files of libraries.
- **[wget]**                    — to automatically download external libraries:
    - **[LLVM]**                — compiler infrastructure.
    - **[fdlibm]**              — a portable implementation of much of the system-dependent libm math library's functionality.
    - **[MT]**                  — a fast Mersenne Twister pseudorandom number generator library.
    - **[OpenBLAS]**            — a fast, open, and maintained [basic linear algebra subprograms (BLAS)](http://en.wikipedia.org/wiki/Basic_Linear_Algebra_Subprograms) library, based on [Kazushige Goto's](http://en.wikipedia.org/wiki/Kazushige_Goto) famous [GotoBLAS](http://www.tacc.utexas.edu/tacc-projects/gotoblas2/).
    - **[LAPACK]**              — a library of linear algebra routines for solving systems of simultaneous linear equations, least-squares solutions of linear systems of equations, eigenvalue problems, and singular value problems.
    - **[SuiteSparse]**         — a library of linear algebra routines for sparse matrices
    - **[ARPACK]**              — a collection of subroutines designed to solve large, sparse eigenvalue problems.
    - **[FFTW]**                — library for computing fast Fourier transforms very quickly and efficiently.
    - **[PCRE]**                — Perl-compatible regular expressions library.
    - **[GNU readline]**        — library allowing shell-like line editing in the terminal, with history and familiar key bindings.
    - **[D3]**                  — JavaScript visualization library.

[GNU make]:     http://www.gnu.org/software/make/
[gcc]:          http://gcc.gnu.org/
[wget]:         http://www.gnu.org/software/wget/
[Perl]:         http://www.perl.org/
[fdlibm]:       http://www.netlib.org/fdlibm/readme
[MT]:           http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html
[OpenBLAS]:     https://github.com/xianyi/OpenBLAS#readme
[LAPACK]:       http://www.netlib.org/lapack/
[SuiteSparse]:  http://www.cise.ufl.edu/research/sparse/SuiteSparse/
[ARPACK]:       http://forge.scilab.org/index.php/p/arpack-ng/
[FFTW]:         http://www.fftw.org/
[PCRE]:         http://www.pcre.org/
[GNU readline]: http://cnswww.cns.cwru.edu/php/chet/readline/rltop.html
[LLVM]:         http://www.llvm.org/
[D3]:           http://mbostock.github.com/d3/

<a name="Directories"/>
## Directories

    attic/         old, now-unused code
    contrib/       emacs and textmate support for julia
    examples/      example julia programs
    external/      external dependencies
    j/             source code for julia's standard library
    lib/           shared libraries loaded by julia's standard libraries
    src/           source for julia language core
    test/          unit and function tests for julia itself
    ui/            source for various front ends

<a name="Binary-Installation"/>
## Binary Installation

Because of the rapid pace of development at this point, we recommend installing the latest Julia from source, but platform-specific tarballs with containing pre-compiled binaries are also available for download:

- **GNU/Linux x86:** <https://github.com/downloads/JuliaLang/julia/julia-77bdd202ec-Linux-i686.tar.gz>
- **Darwin/OS X x86/64:** <https://github.com/downloads/JuliaLang/julia/julia-c4865bd18d-Darwin-i386.tar.gz>

Download the appropriate tarball and untar it somewhere;
for example, if you are on an OS X (Darwin) x86/64 system, do the following:

    wget https://github.com/downloads/JuliaLang/julia/julia-c4865bd18d-Darwin-i386.tar.gz
    tar zxvf julia-c4865bd18d-Darwin-i386.tar.gz

You can either run the `julia` executable using its full path in the directory created above, or add that directory to your executable path so that you can run the julia program from anywhere:

    export PATH="$(pwd)/julia:$PATH"

Now you should be able to run julia like this:

    julia

If everything works correctly, you will see a Julia banner and an interactive prompt into which you can enter expressions for evaluation.
You can read about [getting started](https://github.com/JuliaLang/julia/wiki/Getting-Started) in the manual.

<a name="Editor-Terminal-Setup"/>
## Editor & Terminal Setup

Julia support is currently available for [Emacs](https://github.com/JuliaLang/julia/tree/master/contrib#Emacs), [Vim](https://github.com/JuliaLang/julia/tree/master/contrib#Vim), and [TextMate](https://github.com/JuliaLang/julia/tree/master/contrib#TextMate).
Support files and instructions for configuring these editors can be found in [`contrib/`](https://github.com/JuliaLang/julia/tree/master/contrib).

Adjusting your terminal bindings is optional; everything will work fine without these key bindings.
For the best interactive session experience, however, make sure that your terminal emulator (`Terminal`, `iTerm`, `xterm`, etc.) sends the `^H` sequence for `Backspace` (delete key) and that the `Shift-Enter` key combination sends a `\n` newline character to distinguish it from just pressing `Enter`, which sends a `\r` carriage return character.
These bindings allow custom readline handlers to trap and correctly deal with these key sequences; other programs will continue behave normally with these bindings.
The first binding makes backspacing through text in the interactive session behave more intuitively.
The second binding allows `Shift-Enter` to insert a newline without evaluating the current expression, even when the current expression is complete.
(Pressing an unmodified `Enter` inserts a newline if the current expression is incomplete, evaluates the expression if it is complete, or shows an error if the syntax is irrecoverably invalid.)

On Linux systems, the `Shift-Enter` binding can be set by placing the following line in the file `.xmodmaprc` in your home directory:

    keysym Return = Return Linefeed

                   _
       _       _ _(_)_     |
      (_)     | (_) (_)    |   A fresh approach to technical computing
       _ _   _| |_  __ _   |
      | | | | | | |/ _` |  |           http://julialang.org
      | | |_| | | | (_| |  |       julia-math@googlegroups.com
     _/ |\__'_|_|_|\__'_|  |
    |__/                   |


<a name="The-Julia-Language"/>
## The Julia language

Julia is a high-level, high-performance dynamic language for numerical and scientific computing.
It provides a sophisticated compiler, distributed parallel execution, and numerical accuracy.
Key features include multiple dispatch, optional typing, and excellent performance through type inference and just-in-time (JIT) compilation.
For a more in-depth discussion of the rationale and advantages of Julia over other systems, see the following highlights, read the [introduction](https://github.com/JuliaLang/julia/wiki/Introduction) in the manual, or [browse all](https://github.com/JuliaLang/julia/wiki/) of the documentation.

### High-Performance JIT Compiler 

Julia is an interactive environment with a high performance JIT compiler, with syntax that is familiar to users of other technical computing environments.
The following [performance benchmarks](https://github.com/JuliaLang/julia/blob/master/test/perf.j) are from a Macbook with 2.1GHz Intel Core 2 Duo:

    +---------------+--------+--------+--------+--------------+---------+
    |    Time       | Julia  | Matlab | Octave | Python 2.7.1 | g++ -O3 |  
    |    (ms)       |        | R2011a |  3.4   | Numpy  1.5.1 |  4.6.1  |   
    +---------------+--------+--------+--------+--------------+---------+
    | fib           |    .5  |  309.  |   570. |      7.49    |    .179 |
    | parse_int     |    .21 |  124.  |   557. |       .63    |    .151 |
    | mandel        |   1.82 |   40.  |   260. |      9.64    |    .53  |
    | quicksort     |    .64 |   71.  |  1611. |     30.6     |    .6   |
    | pi_sum        |  49.5  |   69.  | 20578. |   1289.      |  49.3   |
    | rand_mat_stat |  38.9  |  139.  |   517. |    363.      |         |
    +---------------+--------+--------+--------+--------------+---------+

Relative performance between languages on Linux is similar.
These benchmarks, while not comprehensive, were also not chosen to make Julia look good, but rather to test JIT compiler performance in a range of common problem areas, such as recursion optimization, string parsing, sorting, iterative numerical loops, and random number generation.

*Note:* The C++ benchmark for random matrix statistics is missing because implementing such a complex benchmark in a low-level language, while technically possible, is unrealistic, and it is unclear how a fair comparison implementation should work.

### Designed for Parallelism

Julia does not impose any particular style of parallelism on the user.
Instead, it is flexible enough to support a number of styles of [parallelism](https://github.com/JuliaLang/julia/wiki/Parallel-Computing), and makes it easy for the user to add more.
The following simple example demonstrates how to count the number of heads in a large number of coin tosses in parallel.

    nheads = @parallel (+) for i=1:100000000
      randbit()
    end

This computation is automatically distributed across all available nodes participarting in the computation session, and the result, reduced by summation (`+`), is returned at the calling node.
It is also possible to add and remove nodes participating in a computation session while a session is ongoing, allowing elasticity and fault recovery.

### Free, Open Source & Library-Friendly

The core of the Julia implementation is licensed under the [MIT license][MIT].
Various libraries used by the Julia environment include their own licenses such as the [GPL], [LGPL], [BSD], etc., including the `julia` runtime, which is GPL-licensed (this is similar to Python's licensing arrangement).
Users can easily combine their own code or even proprietary third-party libraries with the `libjulia` core, should they choose to.
Furthermore, Julia makes it easy to call functions from [external C and Fortran shared libraries](https://github.com/JuliaLang/julia/wiki/Calling-C-and-Fortran-Code), without requiring the use of a C or Fortran compiler, or even writing any wrapper code.
You can try calling arbitrary external library functions directly from Julia's interactive prompt, playing with the interface and getting immediate feedback until you get it right.
See [LICENSE](https://github.com/JuliaLang/julia/blob/master/LICENSE) for the full terms Julia's licensing.

[MIT]:  http://en.wikipedia.org/wiki/MIT_License
[GPL]:  http://en.wikipedia.org/wiki/GNU_General_Public_License
[LGPL]: http://en.wikipedia.org/wiki/GNU_Lesser_General_Public_License
[BSD]:  http://en.wikipedia.org/wiki/BSD_licenses

<a name="Resources"/>
## Resources

- **Homepage:** <http://julialang.org>
- **Discussion:** <julia-math@googlegroups.com>
- **Source code:** <https://github.com/JuliaLang/julia>
- **Git clone URL:** <git://github.com/JuliaLang/julia.git> (see [below](#Download-Compilation))
- **Documentation:** <https://github.com/JuliaLang/julia/wiki>

<a name="Required-Build-Tools-External-Libraries"/>
## Required Build Tools & External Libraries

- **[GNU make][]** — building dependencies.
- **[gcc, g++, gfortran][gcc]** — compiling and linking C, C++ and Fortran code.
- **[curl][]** — to automatically download external libraries:
    - **[LLVM][]**         — compiler infrastructure.
    - **[fdlibm][]**       — a portable implementation of much of the system-dependent libm math library's functionality.
    - **[MT][]**           — a fast Mersenne Twister pseudorandom number generator library.
    - **[OpenBLAS][]**     — a fast, open, and maintained [basic linear algebra subprograms (BLAS)](http://en.wikipedia.org/wiki/Basic_Linear_Algebra_Subprograms) library, based on [Kazushige Goto's](http://en.wikipedia.org/wiki/Kazushige_Goto) famous [GotoBLAS](http://www.tacc.utexas.edu/tacc-projects/gotoblas2/).
    - **[LAPACK][]**       — library of linear algebra routines for solving systems of simultaneous linear equations, least-squares solutions of linear systems of equations, eigenvalue problems, and singular value problems.
    - **[ARPACK][]**       — a collection of subroutines designed to solve large, sparse eigenvalue problems.
    - **[FFTW][]**         — library for computing fast Fourier transforms very quickly and efficiently.
    - **[PCRE][]**         — Perl-compatible regular expressions library.
    - **[GNU readline][]** — library allowing shell-like line editing in the terminal, with history and familiar key bindings.

[GNU make]:     http://www.gnu.org/software/make/
[gcc]:          http://gcc.gnu.org/
[curl]:         http://curl.haxx.se/
[fdlibm]:       http://www.netlib.org/fdlibm/readme
[MT]:           http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html
[OpenBLAS]:     https://github.com/xianyi/OpenBLAS#readme
[LAPACK]:       http://www.netlib.org/lapack/
[ARPACK]:       http://www.caam.rice.edu/software/ARPACK/
[FFTW]:         http://www.fftw.org/
[PCRE]:         http://www.pcre.org/
[GNU readline]: http://cnswww.cns.cwru.edu/php/chet/readline/rltop.html
[LLVM]:         http://www.llvm.org/

<a name="Supported-Platforms"/>
## Supported Platforms

- **GNU/Linux:** x86/64 (64-bit); x86 (32-bit).
- **Darwin/OS X:** x86/64 (64-bit); x86 (32-bit) is untested but should work.

<a name="Download-Compilation"/>
## Download & Compilation

First, acquire the source code either by cloning the git repository (requires **[git](http://git-scm.com/)** to be installed):

    git clone git://github.com/JuliaLang/julia.git

or, if you don't have git installed, by using curl and tar to fetch and unpack the source:

    mkdir julia && curl -Lk https://github.com/JuliaLang/julia/tarball/master | tar -zxf- -C julia --strip-components 1

Next, enter the `julia/` directory and run `make` to build the `julia` executable.
When compiled the first time, it will automatically download and build its [external dependencies](#Required-Build-Tools-External-Libraries).
This takes a while, but only has to be done once.

No installation is required; `julia` is currently run from the directory where it was built.
You might, however, want to make a symbolic link for the executable, for example `ln -s JULIA_PATH/julia ~/bin/julia`.
Please note that the build process will not work if any of the build directory's parent directories have spaces in their names (this is due to a limitation in GNU make).

Congratulations, if you've gotten this far, you are ready to try out Julia.
You can read about [getting started](https://github.com/JuliaLang/julia/wiki/Getting-Started) in the manual.

<a name="Directories"/>
## Directories

    attic/         old, now-unused code
    contrib/       emacs and textmate support for julia
    doc/           miscellaneous documentation and notes
    examples/      example julia programs
    external/      external dependencies
    j/             source code for julia's standard library
    lib/           shared libraries loaded by julia's standard libraries
    src/           source for julia language core
    test/          unit and function tests for julia itself
    ui/            source for various front ends

<a name="Emacs-Setup"/>
## Emacs Setup

Add the following line to `~/.emacs`

    (require 'julia-mode "JULIA_PATH/contrib/julia-mode.el")

where `JULIA_PATH` is the location of the top-level julia directory.

<a name="TextMate-Setup"/>
## TextMate Setup

Copy (or symlink) the TextMate Julia bundle into the TextMate application support directory:

    cp -r JULIA_PATH/contrib/Julia.tmbundle ~/Library/Application\ Support/TextMate/Bundles/

where `JULIA_PATH` is the location of the top-level julia directory.
Now select from the menu in TextMate `Bundles > Bundle Editor > Reload Bundles`.
Julia should appear as a file type and be automatically detected for files with the `.j` extension.

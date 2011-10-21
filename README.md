                   _
       _       _ _(_)_     |
      (_)     | (_) (_)    |   A fresh approach to technical computing
       _ _   _| |_  __ _   |
      | | | | | | |/ _` |  |           http://julialang.org
      | | |_| | | | (_| |  |       julia-math@googlegroups.com
     _/ |\__'_|_|_|\__'_|  |
    |__/                   |


<a name="The-Julia-Language"/>
## The Julia Language

Julia is a high-level, high-performance dynamic language for numerical and scientific computing.
It provides a sophisticated compiler, distributed parallel execution, and numerical accuracy.
It also has easy interfaces to mature, best-of-breed C and Fortran libraries for technical computing tasks such as matrix math, random number generation, fast Fourier transforms, and string processing.
Key language features include multiple dispatch, optional typing, and excellent performance through type inference and just-in-time (JIT) compilation.
For a more in-depth discussion of the rationale and advantages of Julia over other systems, see the following highlights or read the [introduction](https://github.com/JuliaLang/julia/wiki/Introduction) in the [manual](https://github.com/JuliaLang/julia/wiki/).

### High-Performance JIT Compiler

Julia is an interactive environment with a high performance JIT compiler, with syntax that is familiar to users of other technical computing environments.
The following [benchmarks](https://github.com/JuliaLang/julia/blob/master/test/perf.j) are from a Macbook with 2.1GHz Intel Core 2 Duo:

    ____________________________________________________________________________________

                          Julia        Matlab      Octave    Python/NumPy    C++ (GCC)
                        46c2c6de       R2011a        3.4      2.7.1/1.5.1    4.6.1 -O3
    ____________________________________________________________________________________

      fib                  .500        309.        570.           7.49          .179
      parse_int            .210        124.        557.            .630         .151
      mandel              1.82          40.0       260.           9.64          .530
      quicksort            .640         71.0      1611.          30.6           .600
      pi_sum             49.5           69.0     20578.        1289.          49.3
      rand_mat_stat      38.9          139.        517.         363.
    ____________________________________________________________________________________

          Figure: benchmark time (ms) across various programming system versions.

Julia beats other high-level systems on all benchmarks — by orders of magnitude on most tests.
Moreover, it comes within a factor of two of heavily optimized, well-written C++ on all but two tests, and is never more than four times as slow.
Relative performance between languages on Linux systems is similar.
These benchmarks, while not comprehensive, do test compiler performance on a range of common code patterns, such as function calls, string parsing, sorting, numerical loops, random number generation, and array operations.
Typical performance relative to C++ exhibits similar characteristics:
you can expect well-written Julia code to come within 4x of well-written C/C++, and it will often be much closer.

*Note:* A C++ implementation of random matrix statistics is missing because this test involves many whole-matrix operations, and it is not clear what an idiomatic implementation would look like.

### Designed for Parallelism

Julia does not impose any particular style of parallelism on the user.
Instead, it provides a number of [key building blocks for distributed computation](https://github.com/JuliaLang/julia/wiki/Parallel-Computing), making it flexible enough to support a number of styles of parallelism, and allowing users to add more.
The following simple example demonstrates how to count the number of heads in a large number of coin tosses in parallel.

    nheads = @parallel (+) for i=1:100000000
      randbit()
    end

This computation is automatically distributed across all available nodes participarting in the computation session, and the result, reduced by summation (`+`), is returned at the calling node.
It is also possible to add and remove compute nodes while a session is running, allowing elasticity and fault recovery.

### Free, Open Source & Library-Friendly

The core of the Julia implementation is licensed under the [MIT license][MIT].
Various libraries used by the Julia environment include their own licenses such as the [GPL], [LGPL], and [BSD] (therefore the environment, which consists of the language, user interfaces, and libraries, is under the GPL).
Users can legally combine Julia with their own C/Fortran code or proprietary third-party libraries, should they choose to.
Furthermore, Julia makes it [easy to call functions](https://github.com/JuliaLang/julia/wiki/Calling-C-and-Fortran-Code) from external C and Fortran shared libraries, without writing any wrapper code or even recompiling existing code.
You can try calling external library functions directly from Julia's interactive prompt, playing with the interface and getting immediate feedback until you get it right.
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

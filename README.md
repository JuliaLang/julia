                   _
       _       _ _(_)_     |
      (_)     | (_) (_)    |   A fresh approach to technical computing
       _ _   _| |_  __ _   |
      | | | | | | |/ _` |  |           http://julialang.org
      | | |_| | | | (_| |  |       julia-math@googlegroups.com
     _/ |\__'_|_|_|\__'_|  |
    |__/                   |

Julia is a very high level dynamic language with optional typing, multiple dispatch, and good performance, achieved by using type inference and just-in-time (JIT) compilation, implemented using LLVM.
The language is multi-paradigm, combining features of functional, object-oriented, and imperative traditions.
For a more in depth discussion of the rationale and advantages of Julia over other systems, see the [Introduction](https://github.com/JuliaLang/julia/wiki/Introduction) in the wiki, or [browse all](https://github.com/JuliaLang/julia/wiki/) of the wiki documentation.

<a name="Resources"/>
## Resources

- **Homepage:** <http://julialang.org>
- **Download:** <https://github.com/JuliaLang/julia>
- **Documentation:** <https://github.com/JuliaLang/julia/wiki>
- **Discussion:** <julia-math@googlegroups.com>

<a name="Required-Build-Tools"/>
## Required Build Tools & External Libraries

- **[GNU make][]** — computing and building dependencies.
- **[gcc, g++, gfortran][gcc]** — compiling and linking C, C++ and Fortran code.
- **[curl][]** — to automatically download external libraries:
    - **[fdlibm][]** — a portable implementation of much system-dependent libm math library.
    - **[OpenBLAS][]** — a fast, open, and maintained [basic linear algebar subprograms (BLAS)](http://en.wikipedia.org/wiki/Basic_Linear_Algebra_Subprograms) library, based on [Kazushige Goto](http://en.wikipedia.org/wiki/Kazushige_Goto)'s famous [GotoBLAS](http://www.tacc.utexas.edu/tacc-projects/gotoblas2/).
    - **[LAPACK][]** — library of linear algebra routines for "solving systems of simultaneous linear equations, least-squares solutions of linear systems of equations, eigenvalue problems, and singular value problems" using generic BLAS functionality.
    - **[ARPACK][]** — "a collection of Fortran77 subroutines designed to solve large scale eigenvalue problems."
    - **[PCRE][]** — Perl-compatible Regular Expressions library
    - **[GNU readline][]** — library allowing line editing in the terminal, with history and familiar key bindings.
    - **[mongoose][]** — a library for writing simple web servers, used for Julia's web-based repl.

[GNU make]:     http://www.gnu.org/software/make/
[gcc]:          http://gcc.gnu.org/
[curl]:         http://curl.haxx.se/
[fdlibm]:       http://www.netlib.org/fdlibm/readme
[OpenBLAS]:     https://github.com/xianyi/OpenBLAS#readme
[LAPACK]:       http://www.netlib.org/lapack/
[ARPACK]:       http://www.caam.rice.edu/software/ARPACK/
[PCRE]:         http://www.pcre.org/
[GNU readline]: http://cnswww.cns.cwru.edu/php/chet/readline/rltop.html
[mongoose]:     http://code.google.com/p/mongoose/

<a name="Supported-Platforms"/>
## Supported Platforms

- **GNU/Linux:** x86 (32-bit); x86/64 (64-bit).
- **OS X:** x86/64 (64-bit); x86 (32-bit) is untested but should work.

<a name="Compilation"/>
## Compilation

- Run `make` in `external/` to download and build external dependencies.
- Run `make` in the top-level directory to build julia.

No installation is required — julia is currently run from the directory where it was built. You might want to make a symbolic link for the executable, for example `ln -s JULIA_PATH/julia ~/bin/julia`.

<a name="Directories"/>
## Directories

    attic/         old, now-unused code
    bench/         benchmarks and performance tests
    contrib/       emacs and textmate support for julia
    doc/           TeX manual and other notes
    external/      external dependencies
    j/             source code for julia's standard library
    lib/           shared libraries loaded by julia's standard libraries
    src/           source for julia language core
    test/          unit and function tests for julia itself
    ui/            source for various frontends

<a name="Emacs-Setup"/>
## Emacs Setup

Add the following line to `~/.emacs`

    (require 'julia-mode "JULIA_PATH/contrib/julia-mode.el")

where `JULIA_PATH` is the location of the top-level julia directory.

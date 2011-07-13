                   _
       _       _ _(_)_     |
      (_)     | (_) (_)    |   A fresh approach to technical computing
       _ _   _| |_  __ _   |
      | | | | | | |/ _` |  |           http://julialang.org
      | | |_| | | | (_| |  |       julia-math@googlegroups.com
     _/ |\__'_|_|_|\__'_|  |
    |__/                   |

Julia is a very high-level, high-performance dynamic language for numerical and scientific computing.
It provides support for state-of-the-art numerical accuracy and performance, while being designed to support parallelism and distributed computation from the ground up.
Some of its notable features include optional typing, multiple dispatch, and excellent performance through type inference and just-in-time (JIT) compilation.
The language is multi-paradigm, combining features of functional, object-oriented, and imperative styles.
For a more in-depth discussion of the rationale and advantages of Julia over other systems, see the [Introduction](https://github.com/JuliaLang/julia/wiki/Introduction) in the wiki, or [browse all](https://github.com/JuliaLang/julia/wiki/) of the wiki documentation.

<a name="Resources"/>
## Resources

- **Homepage:** <http://julialang.org>
- **Source code:** <https://github.com/JuliaLang/julia>
- **Git clone URL:** <git://github.com/JuliaLang/julia.git> (see [below](#Download-Compilation))
- **Documentation:** <https://github.com/JuliaLang/julia/wiki>
- **Discussion:** <julia-math@googlegroups.com>

<a name="Required-Build-Tools-External-Libraries"/>
## Required Build Tools & External Libraries

- **[GNU make][]** — building dependencies.
- **[gcc, g++, gfortran][gcc]** — compiling and linking C, C++ and Fortran code.
- **[curl][]** — to automatically download external libraries:
    - **[LLVM][]**         — compiler infrastructure
    - **[fdlibm][]**       — a portable implementation of much of the system-dependent libm math library's functionality.
    - **[MT][]**	   — a fast Mersenne Twister pseudorandom number generator library.
    - **[OpenBLAS][]**     — a fast, open, and maintained [basic linear algebar subprograms (BLAS)](http://en.wikipedia.org/wiki/Basic_Linear_Algebra_Subprograms) library, based on [Kazushige Goto's](http://en.wikipedia.org/wiki/Kazushige_Goto) famous [GotoBLAS](http://www.tacc.utexas.edu/tacc-projects/gotoblas2/).
    - **[LAPACK][]**       — library of linear algebra routines for solving systems of simultaneous linear equations, least-squares solutions of linear systems of equations, eigenvalue problems, and singular value problems.
    - **[ARPACK][]**       — a collection of subroutines designed to solve large, sparse eigenvalue problems.
    - **[FFTW][]**	   — library for computing fast Fourier transforms very quickly and efficiently.
    - **[PCRE][]**         — Perl-compatible regular expressions library.
    - **[GNU readline][]** — library allowing shell-like line editing in the terminal, with history and familiar key bindings.

[GNU make]:     http://www.gnu.org/software/make/
[gcc]:          http://gcc.gnu.org/
[curl]:         http://curl.haxx.se/
[fdlibm]:       http://www.netlib.org/fdlibm/readme
[MT]:		http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html
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

or by using curl and tar to fetch and unpack the source:

    mkdir julia && curl -Lk https://github.com/JuliaLang/julia/tarball/master | tar -zxf- -C julia --strip-components 1

TODO: check that this actually works once the repository is public.

Next, enter the `julia/` directory and run `make` to build the `julia` executable.
When compiled the first time, it will automatically download and build its [external dependencies](#Required-Build-Tools-External-Libraries).
This takes a while, but only has to be done once.

No installation is required: `julia` is currently run from the directory where it was built.
You might, however, want to make a symbolic link for the executable, for example `ln -s JULIA_PATH/julia ~/bin/julia`.
Please note that the build process will not work if any of the build directory's parent directories have spaces in their names (this is due to a limitation in GNU make).

Congratulations, if you've gotten this far, you are ready to try out Julia.
You can read about [getting started](https://github.com/JuliaLang/julia/wiki/Getting-Started) in the manual.

<a name="Directories"/>
## Directories

    attic/         old, now-unused code
    contrib/       emacs and textmate support for julia
    doc/           miscellaneous documentation and notes
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

<a name="TextMate-Setup"/>
## TextMate Setup

Copy (or symlink) the TextMate Julia bundle into the TextMate application support directory:

    cp -r JULIA_PATH/contrib/Julia.tmbundle ~/Library/Application\ Support/TextMate/Bundles/

where `JULIA_PATH` is the location of the top-level julia directory.
Now select from the menu in TextMate `Bundles > Bundle Editor > Reload Bundles`.
Julia should appear as a file type and be automatically detected for files with the `.j` extension.

<a name="License"/>
## License

Julia is licensed under the [three clause "Modified BSD License"](http://en.wikipedia.org/wiki/BSD_license#3-clause_license_.28.22New_BSD_License.22_or_.22Modified_BSD_License.22.29), with the exception of certain components that link against GPL-licensed libraries (e.g. GNU readline).
See [LICENSE](https://github.com/JuliaLang/julia/blob/master/LICENSE) for the full terms of the license.

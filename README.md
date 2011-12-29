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

Julia is a high-level, high-performance dynamic language for technical computing, with syntax that is familiar to users of other technical computing environments.
It provides a sophisticated compiler, distributed parallel execution, numerical accuracy, and an extensive mathematical function library.
The library, mostly written in Julia itself, also integrates mature, best-of-breed C and Fortran libraries for linear algebra, random number generation, FFTs, and string processing.
More libraries continue to be added over time.
Julia programs are organized around defining functions, and overloading them for different combinations of argument types (which can also be user-defined).
For a more in-depth discussion of the rationale and advantages of Julia over other systems, see the following highlights or read the [introduction](https://github.com/JuliaLang/julia/wiki/Introduction) in the [manual](https://github.com/JuliaLang/julia/wiki/).

### High-Performance JIT Compiler

Julia's LLVM-based JIT compiler combined with the language's design allow it to approach and often match the performance of C/C++.
To get a sense of relative performance of Julia compared to other languages that can or could be used for numerical and scientific computing, we've written a small set of micro-benchmarks in a variety of languages.
The source code for the various implementations can be found here:
[C++](https://github.com/JuliaLang/julia/blob/master/test/perf.cxx#L1),
[Julia](https://github.com/JuliaLang/julia/blob/master/test/perf.j#L1),
[Python](https://github.com/JuliaLang/julia/blob/master/test/perf.py#L1),
[Matlab/Octave](https://github.com/JuliaLang/julia/blob/master/test/perf.m#L1),
[JavaScript](https://github.com/JuliaLang/julia/blob/master/test/perf.js#L1).
We encourage you to skim the code to get a sense for how easy or difficult numerical programming in each language is.
The following micro-benchmark results are from a MacBook Pro with a 2.53GHz Intel Core 2 Duo CPU and 8GB of 1066MHz DDR3 RAM:

    _____________________________________________________________________________________________________________________
                    |             |
                    |  C++ (GCC)  |         Julia  Python/NumPy        Matlab        Octave             R    JavaScript
                    |  4.2.1 -O*  |      54fc2f70   2.7.1/1.5.1        R2011a           3.4         2.9.0   V8 3.6.6.11
    ________________|_____________|______________________________________________________________________________________
                fib |     0.200   |          1.99         28.68       1364.10       2436.51        325.33          1.48
          parse_int |     0.242   |          4.36         16.74        842.76       6593.66       1082.67          2.14
          quicksort |     0.419   |          1.31         61.37        136.92       3356.46        911.91         23.32
             mandel |     0.249   |          7.52         32.45         69.28        868.83        229.00          6.29
             pi_sum |    53.525   |          0.74         18.53          1.08        333.38        251.10          0.76
      rand_mat_stat |     7.279   |          4.78         40.86         11.93         58.54         30.22          9.34
       rand_mat_mul |   235.499   |          0.98          1.19          0.70          1.70          2.12        289.42
    ________________|_____________|______________________________________________________________________________________

      Figure: C++ numbers are absolute benchmark times in milliseconds;
              other timings are relative to C++ (smaller is better).

      *Best timings taken over all optimization levels (0 to 3).

Julia beats other high-level systems on most micro-benchmarks, with a few exceptions for Matlab and JavaScript.
Julia's LLVM JIT code even manages to beat C++ by 25% on the pi summation benchmark and by a small margin on random matrix multiplication.
Relative performance between languages on [other systems](#Supported-Platforms) is similar.
Matlab's ability to beat both C and Julia by such a large margin on random matrix multiplication comes from its use of the proprietary [Intel Math Kernel Library](http://en.wikipedia.org/wiki/Math_Kernel_Library), which has extremely optimized code for matrix multiplication.
Users who have a licensed copy of MKL can use it with Julia, but the default BLAS is a high quality open source implementation (see <a href="#Required-Build-Tools-External-Libraries">below</a> for more details).

These benchmarks, while not comprehensive, do test compiler performance on a range of common code patterns, such as function calls, string parsing, sorting, numerical loops, random number generation, and array operations.
Julia is strong in an area that high-level languages have traditionally been weak:
scalar arithmetic loops, such as that found in the pi summation benchmark.
Matlab's JIT for floating-point arithmetic does very well here too, as does the V8 JavaScript engine.
V8 is very impressive in that it can provide such a dynamic language with C-like performance in so many circumstances.
JavaScript, however, is unable to utilize technical computing libraries such as LAPACK, resulting in poor performance on benchmarks like matrix multiplication.
In contrast with both Matlab and JavaScript, Julia has a more comprehensive approach to eliminating overhead that allows it to consistently optimize all kinds of code for arbitrary user-defined data types, not just certain special cases.

To give a quick taste of what Julia looks like, here is the code used in the Mandelbrot and random matrix statistics benchmarks:

```
function mandel(z)
    c = z
    maxiter = 80
    for n = 1:maxiter
        if abs(z) > 2
            return n-1
        end
        z = z^2 + c
    end
    return maxiter
end

function randmatstat(t)
    n = 5
    v = zeros(t)
    w = zeros(t)
    for i = 1:t
        a = randn(n,n)
        b = randn(n,n)
        c = randn(n,n)
        d = randn(n,n)
        P = [a b c d]
        Q = [a b; c d]
        v[i] = trace((P.'*P)^4)
        w[i] = trace((Q.'*Q)^4)
    end
    std(v)/mean(v), std(w)/mean(w)
end
```

As you can see, the code is quite clear, and should feel familiar to anyone who has programmed in other mathematical languages.
Although C++ beats Julia in the random matrix statistics benchmark by a factor of three, consider how much simpler this code is than the [C++ implementation](https://github.com/JuliaLang/julia/blob/master/test/perf.cxx#L145).
There are more compiler optimizations planned that we hope will close this performance gap in the future.
By design, Julia allows you to range from low-level loop and vector code, up to a high-level programming style, sacrificing some performance, but gaining the ability to express complex algorithms easily.
This continuous spectrum of programming levels is a hallmark of the Julia approach to programming and is very much an intentional feature of the language.

### Designed for Parallelism & Cloud Computing

Julia does not impose any particular style of parallelism on the user.
Instead, it provides a number of [key building blocks for distributed computation](https://github.com/JuliaLang/julia/wiki/Parallel-Computing), making it flexible enough to support a number of styles of parallelism, and allowing users to add more.
The following simple example demonstrates how to count the number of heads in a large number of coin tosses in parallel.

    nheads = @parallel (+) for i=1:100000000
      randbit()
    end

This computation is automatically distributed across all available compute nodes, and the result, reduced by summation (`+`), is returned at the calling node.

Although it is in the early stages, Julia already supports a fully remote cloud computing mode.
Here is a screenshot of a web-based interactive Julia session, plotting an oscillating function and a Gaussian random walk:

<a href="http://julialang.github.com/misc/web_repl.png"><img src="http://julialang.github.com/misc/web_repl.png"/></a>

There will eventually be full support for cloud-based operation, including data management, code editing, execution, debugging, collaboration, analysis, data exploration, and visualization.
The goal is to allow people who work with big data to stop worrying about administering machines and data and get straight to the real problem:
exporing their data and creating the algorithms that can solve the problems presented by their big data.

### Free, Open Source & Library-Friendly

The core of the Julia implementation is licensed under the [MIT license][MIT].
Various libraries used by the Julia environment include their own licenses such as the [GPL], [LGPL], and [BSD] (therefore the environment, which consists of the language, user interfaces, and libraries, is under the GPL).
Core functionality is included in a shared library, so users can easily and legally combine Julia with their own C/Fortran code or proprietary third-party libraries.
Furthermore, Julia makes it [simple to call external functions](https://github.com/JuliaLang/julia/wiki/Calling-C-and-Fortran-Code) in C and Fortran shared libraries, without writing any wrapper code or even recompiling existing code.
You can try calling external library functions directly from Julia's interactive prompt, playing with the interface and getting immediate feedback until you get it right.
See [LICENSE](https://github.com/JuliaLang/julia/blob/master/LICENSE) for the full terms of Julia's licensing.

[MIT]:  http://en.wikipedia.org/wiki/MIT_License
[GPL]:  http://en.wikipedia.org/wiki/GNU_General_Public_License
[LGPL]: http://en.wikipedia.org/wiki/GNU_Lesser_General_Public_License
[BSD]:  http://en.wikipedia.org/wiki/BSD_licenses

<a name="Resources"/>
## Resources

- **Homepage:** <http://julialang.org>
- **Discussion:** <http://groups.google.com/group/julia-dev/>
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
    - **[LAPACK][]**       — a library of linear algebra routines for solving systems of simultaneous linear equations, least-squares solutions of linear systems of equations, eigenvalue problems, and singular value problems.
    - **[SuiteSparse][]**  - a library of linear algebra routines for sparse matrices
    - **[ARPACK][]**       — a collection of subroutines designed to solve large, sparse eigenvalue problems.
    - **[FFTW][]**         — library for computing fast Fourier transforms very quickly and efficiently.
    - **[PCRE][]**         — Perl-compatible regular expressions library.
    - **[GNU readline][]** — library allowing shell-like line editing in the terminal, with history and familiar key bindings.
    - **[D3][]**           — javascript visualization library.

[GNU make]:     http://www.gnu.org/software/make/
[gcc]:          http://gcc.gnu.org/
[curl]:         http://curl.haxx.se/
[fdlibm]:       http://www.netlib.org/fdlibm/readme
[MT]:           http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html
[OpenBLAS]:     https://github.com/xianyi/OpenBLAS#readme
[LAPACK]:       http://www.netlib.org/lapack/
[SuiteSparse]:	http://www.cise.ufl.edu/research/sparse/SuiteSparse/
[ARPACK]:       http://www.caam.rice.edu/software/ARPACK/
[FFTW]:         http://www.fftw.org/
[PCRE]:         http://www.pcre.org/
[GNU readline]: http://cnswww.cns.cwru.edu/php/chet/readline/rltop.html
[LLVM]:         http://www.llvm.org/
[D3]:           http://mbostock.github.com/d3/

<a name="Supported-Platforms"/>
## Supported Platforms

- **GNU/Linux:** x86/64 (64-bit); x86 (32-bit).
- **Darwin/OS X:** x86/64 (64-bit); x86 (32-bit) is untested but should work.

<a name="Binary-Installation"/>
## Binary Installation

Julia's binary installs ship as platform-specific tarballs:

- **GNU/Linux x86/64:** <https://github.com/downloads/JuliaLang/julia/julia-08b1e294ed-Linux-x86_64.tar.gz>
- **Darwin/OS X x86/64:** <https://github.com/downloads/JuliaLang/julia/julia-08b1e294ed-Darwin-x86_64.tar.gz>
- **GNU/Linux x86:** <https://github.com/downloads/JuliaLang/julia/julia-618b93c22c-Linux-i686.tar.gz>

Download the appropriate tarball and untar it somewhere;
for example, if you are on an OS X (Darwin) x86/64 system, do the following:

    curl -OLk https://github.com/downloads/JuliaLang/julia/julia-08b1e294ed-Darwin-x86_64.tar.gz
    tar zxvf julia-08b1e294ed-Darwin-x86_64.tar.gz

You can either run the `julia` executable using its full path in the directory created above, or add that directory to your executable path so that you can run the julia program from anywhere:

    export PATH="$(pwd)/julia:$PATH"

Now you should be able to run julia like this:

    julia

If everything works correctly, you will see a Julia banner and an interactive prompt into which you can enter expressions for evaluation.
You can read about [getting started](https://github.com/JuliaLang/julia/wiki/Getting-Started) in the manual.

<a name="Source-Download-Compilation"/>
## Source Download & Compilation

First, acquire the source code either by cloning the git repository (requires **[git](http://git-scm.com/)** to be installed):

    git clone git://github.com/JuliaLang/julia.git

or, if you don't have git installed, by using curl and tar to fetch and unpack the source:

    mkdir julia && curl -Lk https://github.com/JuliaLang/julia/tarball/master | tar -zxf- -C julia --strip-components 1

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

<a name="Directories"/>
## Directories

    attic/         old, now-unused code
    contrib/       emacs and textmate support for julia
    examples/      example julia programs
    external/      external dependencies
    install/       used for creating binary installs
    j/             source code for julia's standard library
    lib/           shared libraries loaded by julia's standard libraries
    src/           source for julia language core
    test/          unit and function tests for julia itself
    ui/            source for various front ends

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

<a name="banner"/>

                   _
       _       _ _(_)_     |
      (_)     | (_) (_)    |   A fresh approach to technical computing
       _ _   _| |_  __ _   |
      | | | | | | |/ _` |  |          http://julialang.org
      | | |_| | | | (_| |  |       julia-dev@googlegroups.com
     _/ |\__'_|_|_|\__'_|  |           #julia on freenode
    |__/                   |


<a name="The-Julia-Language"/>
## The Julia Language

Julia is a high-level, high-performance dynamic language for technical computing.
The main homepage for Julia can be found at [julialang.org](http://julialang.org/).
This is the GitHub repository of Julia source code, including instructions for compiling and installing Julia, below.

<a name="Resources"/>

- **Homepage:** <http://julialang.org>
- **Mailing list:** <http://groups.google.com/group/julia-dev/>
- **IRC:** <http://webchat.freenode.net/?channels=julia>
- **Source code:** <https://github.com/JuliaLang/julia>
- **Git clone URL:** <git://github.com/JuliaLang/julia.git>
- **Documentation:** <http://julialang.org/manual/>

<a name="Currently-Supported-Platforms"/>
## Currently Supported Platforms

- **GNU/Linux:** x86/64 (64-bit); x86 (32-bit).
- **Darwin/OS X:** x86/64 (64-bit); x86 (32-bit).
- **FreeBSD:** x86/64 (64-bit); x86 (32-bit).

<a name="Source-Download-Compilation"/>
## Source Download & Compilation

First, acquire the source code by cloning the git repository:

    git clone git://github.com/JuliaLang/julia.git

Next, enter the `julia/` directory and run `make` to build the `julia` executable. To perform a parallel build, use `make -j N` and supply the maximum number of concurrent processes.
When compiled the first time, it will automatically download and build its [external dependencies](#Required-Build-Tools-External-Libraries).
This takes a while, but only has to be done once.

**Note:** the build process will not work if any of the build directory's parent directories have spaces in their names (this is due to a limitation in GNU make).

Once it is built, you can either run the `julia` executable using its full path in the directory created above, or add that directory to your executable path so that you can run the julia program from anywhere:

    export PATH="$(pwd)/julia:$PATH"

Now you should be able to run julia like this:

    julia

If everything works correctly, you will see a Julia banner and an interactive prompt into which you can enter expressions for evaluation.
You can read about [getting started](http://julialang.org/manual/getting-started) in the manual.

<a name="Platform-Specific-Notes"/>
### Platform-Specific Notes

On some Linux distributions you may need to change how the readline library is linked. If you get a build error involving readline, try changing the value of `USE_SYSTEM_READLINE` in `Make.inc` to `1`.

On Ubuntu, you may also need to install the package `libncurses5-dev`.

On OS X, you may need to install `gfortran`. Either download and install [gfortran from hpc.sf.net](http://hpc.sf.net/), or [64-bit gfortran from gcc.gnu.org](http://gcc.gnu.org/wiki/GFortranBinaries).

If you get link errors mentioning `gfortran`, it might help to put `/usr/local/gfortran/lib` at the beginning of the `DYLD_LIBRARY_PATH` environment variable.

On FreeBSD the prerequisites can be installed from ports like this:

    cd /usr/ports/devel/gmake
    make install

    cd /usr/ports/ftp/curl
    make install
 
    cd /usr/ports/devel/libunwind
    make install

    cd /usr/ports/lang/gcc45
    make install
    ln -s /usr/local/bin/gfortran45 /usr/local/bin/gfortran

Other versions of gcc are also exist but currently gfortran45 is the one used by all the ports that depend on Fortran.

**Use the gmake command on FreeBSD instead of make**

<a name="Required-Build-Tools-External-Libraries"/>
## Required Build Tools & External Libraries

Building Julia requires that the following software be installed:

- **[GNU make]**                — building dependencies.
- **[gcc, g++, gfortran][gcc]** — compiling and linking C, C++ and Fortran code.
- **[git]**    			— contributions and version control.
- **[perl]**                    — preprocessing of header files of libraries.
- **[wget]** or **[curl]**      — to automatically download external libraries (Linux defaults to `wget`, OS X and FreeBSD to `curl`).
- **[m4]**                      — needed to build GMP.

With the exception of `gfortran`, these are standard on most Linux systems and on any OS X system with `Xcode` and Apple's Developer Tools installed.
Julia uses the following external libraries, which are automatically downloaded (or in a few cases, included in the Julia source repository) and then compiled from source the first time you run `make`:

- **[LLVM]**                — compiler infrastructure. Currently, julia requires LLVM 3.0.
- **[FemtoLisp]**           — packaged with julia source, and used to implement the compiler front-end.
- **[GNU readline]**        — library allowing shell-like line editing in the terminal, with history and familiar key bindings.
- **[fdlibm]**              — a portable implementation of much of the system-dependent libm math library's functionality.
- **[MT]**                  — a fast Mersenne Twister pseudorandom number generator library.
- **[OpenBLAS]**            — a fast, open, and maintained [basic linear algebra subprograms (BLAS)](http://en.wikipedia.org/wiki/Basic_Linear_Algebra_Subprograms) library, based on [Kazushige Goto's](http://en.wikipedia.org/wiki/Kazushige_Goto) famous [GotoBLAS](http://www.tacc.utexas.edu/tacc-projects/gotoblas2/).
- **[LAPACK]**              — a library of linear algebra routines for solving systems of simultaneous linear equations, least-squares solutions of linear systems of equations, eigenvalue problems, and singular value problems.
- **[AMOS]**                — subroutines for computing Bessel and Airy functions.
- **[SuiteSparse]**         — a library of linear algebra routines for sparse matrices.
- **[ARPACK]**              — a collection of subroutines designed to solve large, sparse eigenvalue problems.
- **[FFTW]**                — library for computing fast Fourier transforms very quickly and efficiently.
- **[PCRE]**                — Perl-compatible regular expressions library.
- **[GMP]**                 — the GNU multiple precision arithmetic library, needed for bigint support
- **[D3]**                  — JavaScript visualization library.
- **[double-conversion]**   — efficient number-to-text conversion.

[GNU make]:     http://www.gnu.org/software/make/
[gcc]:          http://gcc.gnu.org/
[wget]:         http://www.gnu.org/software/wget/
[curl]:         http://curl.haxx.se/
[git]:          http://git-scm.com/
[perl]:         http://www.perl.org/
[m4]:           https://www.gnu.org/software/m4/
[fdlibm]:       http://www.netlib.org/fdlibm/readme
[MT]:           http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html
[OpenBLAS]:     https://github.com/xianyi/OpenBLAS#readme
[LAPACK]:       http://www.netlib.org/lapack/
[SuiteSparse]:  http://www.cise.ufl.edu/research/sparse/SuiteSparse/
[AMOS]:         http://netlib.org/amos
[ARPACK]:       http://forge.scilab.org/index.php/p/arpack-ng/
[FFTW]:         http://www.fftw.org/
[PCRE]:         http://www.pcre.org/
[LLVM]:         http://www.llvm.org/
[FemtoLisp]:    https://github.com/JeffBezanson/femtolisp
[GNU readline]: http://cnswww.cns.cwru.edu/php/chet/readline/rltop.html
[GMP]:          http://gmplib.org/
[D3]:           http://mbostock.github.com/d3/
[double-conversion]: http://double-conversion.googlecode.com/

<a name="Directories"/>
## Directories

    attic/         old, now-unused code
    base/          source code for julia's standard library
    contrib/       emacs and textmate support for julia
    examples/      example julia programs
    deps/      external dependencies
    extras/        useful optional libraries
    lib/           shared libraries loaded by julia's standard libraries
    src/           source for julia language core
    test/          unit and function test cases
    ui/            source for various front ends

<a name="Binary-Installation"/>
## Binary Installation

Because of the rapid pace of development at this point, we recommend installing the latest Julia from source, but platform-specific tarballs with pre-compiled binaries are also [available for download](https://github.com/JuliaLang/julia/downloads).
To install from source, download the appropriate tarball and untar it somewhere.
For example, if you are on an OS X (Darwin) x86/64 system, do the following:

    wget https://github.com/downloads/JuliaLang/julia/julia-c4865bd18d-Darwin-i386.tar.gz
    tar zxvf julia-c4865bd18d-Darwin-i386.tar.gz

You can either run the `julia` executable using its full path in the directory created above, or add that directory to your executable path so that you can run the julia program from anywhere:

    export PATH="$(pwd)/julia:$PATH"

Now you should be able to run julia like this:

    julia

If everything works correctly, you will see a Julia banner and an interactive prompt into which you can enter expressions for evaluation.
You can read about [getting started](http://julialang.org/manual/getting-started) in the manual.

An [Arch Linux package](https://aur.archlinux.org/packages.php?ID=56877) is also available.

<a name="Editor-Terminal-Setup"/>
## Editor & Terminal Setup

Currently, [julia editing mode](https://github.com/JuliaLang/julia/wiki/Configuring-Editors) support is available for Emacs, Vim, and Textmate.

Adjusting your terminal bindings is optional; everything will work fine without these key bindings.
For the best interactive session experience, however, make sure that your terminal emulator (`Terminal`, `iTerm`, `xterm`, etc.) sends the `^H` sequence for `Backspace` (delete key) and that the `Shift-Enter` key combination sends a `\n` newline character to distinguish it from just pressing `Enter`, which sends a `\r` carriage return character.
These bindings allow custom readline handlers to trap and correctly deal with these key sequences; other programs will continue behave normally with these bindings.
The first binding makes backspacing through text in the interactive session behave more intuitively.
The second binding allows `Shift-Enter` to insert a newline without evaluating the current expression, even when the current expression is complete.
(Pressing an unmodified `Enter` inserts a newline if the current expression is incomplete, evaluates the expression if it is complete, or shows an error if the syntax is irrecoverably invalid.)

On Linux systems, the `Shift-Enter` binding can be set by placing the following line in the file `.xmodmaprc` in your home directory:

    keysym Return = Return Linefeed

<a name="Web-REPL-and-grahpics">
## Web REPL and graphics

Julia has a web REPL with very preliminary graphics capabilities.
Follow these instructions for setting up the web repl locally.
In deps, doing `make install-lighttpd` will download and build lighttpd.
Use the launch-webserver script to start the webserver and web-repl.
Point your browser to `http://localhost:2000/`.
Try `plot(cumsum(randn(1000)))`

### Run it Online
[julia.forio.com](http://julia.forio.com)

### Pre-installed lighttpd

If you want to use your own `lighttpd`, then the process is something like this:

1. Install `lighttpd`
2. Configure `lighttpd`:
The config file is `/etc/lighttpd/lighttpd.conf`. Add `mod_scgi` to `server.modules`.
Set `server.document-root` to point to `/path/to/julia/ui/website`.
Add something like this to the bottom (you can use whatever port you want):
`scgi.server = (".scgi" => (("host" => "127.0.0.1", "port" => 2001)))`.

3. Start lighttpd: `sudo /etc/init.d/lighttpd start`.
4. Start the julia server: `path/to/julia/julia-release-webserver -p 2001`.
5. Point your browser to `http://localhost:2001`.

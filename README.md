<a name="logo"/>
<div align="center">
<a href="https://julialang.org/" target="_blank">
<img src="https://julialang.org/images/logo_hires.png" alt="Julia Logo" width="210" height="142"></img>
</a>
</div>

Build status:
[![travis][travis-img]](https://travis-ci.org/JuliaLang/julia)
[![appveyor][appveyor-img]](https://ci.appveyor.com/project/JuliaLang/julia/branch/master)

Code coverage:
[![coveralls][coveralls-img]](https://coveralls.io/r/JuliaLang/julia?branch=master)
[![codecov][codecov-img]](https://codecov.io/github/JuliaLang/julia?branch=master)

[travis-img]: https://img.shields.io/travis/JuliaLang/julia/master.svg?label=Linux+/+macOS
[appveyor-img]: https://img.shields.io/appveyor/ci/JuliaLang/julia/master.svg?label=Windows
[coveralls-img]: https://img.shields.io/coveralls/github/JuliaLang/julia/master.svg?label=coveralls
[codecov-img]: https://img.shields.io/codecov/c/github/JuliaLang/julia/master.svg?label=codecov

## The Julia Language

Julia is a high-level, high-performance dynamic language for technical
computing.  The main homepage for Julia can be found at
[julialang.org](https://julialang.org/).  This is the GitHub
repository of Julia source code, including instructions for compiling
and installing Julia, below.

## Resources

- **Homepage:** <https://julialang.org>
- **Binaries:** <https://julialang.org/downloads/>
- **Source code:** <https://github.com/JuliaLang/julia>
- **Documentation:** <https://docs.julialang.org/>
- **Packages:** <https://pkg.julialang.org/>
- **Discussion forum:** <https://discourse.julialang.org>
- **Slack:** <https://julialang.slack.com> (get an invite from <https://slackinvite.julialang.org>)
- **YouTube:** <https://www.youtube.com/user/JuliaLanguage>
- **Code coverage:** <https://coveralls.io/r/JuliaLang/julia>

New developers may find the notes in
[CONTRIBUTING](https://github.com/JuliaLang/julia/blob/master/CONTRIBUTING.md)
helpful to start contributing to the Julia codebase.

### External Resources

- [**StackOverflow**](https://stackoverflow.com/questions/tagged/julia-lang)
- [**Twitter**](https://twitter.com/JuliaLanguage)
- [**Meetup**](https://julia.meetup.com/)

## Currently Supported Platforms

| Operating System | Architecture     | CI | Binaries | Support Level |
|:----------------:|:----------------:|:--:|:--------:|:-------------:|
| macOS 10.8+      | x86-64 (64-bit)  | ✓  | ✓        | Tier 1        |
| Windows 7+       | x86-64 (64-bit)  | ✓  | ✓        | Tier 1        |
|                  | i686 (32-bit)    | ✓  | ✓        | Tier 1        |
| FreeBSD 11.0+    | x86-64 (64-bit)  | [✓](https://build.julialang.org/#/builders/68)  | ✓        | Tier 1        |
| Linux 2.6.18+    | x86-64 (64-bit)  | ✓  | ✓        | Tier 1        |
|                  | i686 (32-bit)    | ✓  | ✓        | Tier 1        |
|                  | ARM v7 (32-bit)  |    | ✓        | Tier 2        |
|                  | ARM v8 (64-bit)  |    |          | Tier 3        |
|                  | x86-64 musl libc |    |          | Tier 3        |
|                  | PowerPC (64-bit) |    |          | Tier 4        |
|                  | PTX (64-bit)     | [✓](https://gitlab.com/JuliaGPU/CUDAnative.jl/pipelines)  |          | [External](https://github.com/JuliaGPU/CUDAnative.jl)     |

All systems marked with ✓ for CI are tested using continuous integration for every commit.
Systems with ✓ for binaries have official binaries available on the
[downloads](https://julialang.org/downloads) page and are tested regularly.
The PTX backend is supported by the [JuliaGPU](https://github.com/JuliaGPU) organization and
requires the [CUDAnative.jl](https://github.com/JuliaGPU/CUDAnative.jl) package.

### Support Tiers

* Tier 1: Julia is guaranteed to build from source and pass all tests on these platforms
  when built with default options. Official binaries are available for releases and CI is
  run on every commit.

* Tier 2: Julia is guaranteed to build from source using default build options, but may
  or may not pass all tests. Official binaries are available on a case-by-case basis.

* Tier 3: Julia may or may not build. If it does, it is unlikely to pass tests.

* Tier 4: Julia is known not to build.

It is possible that Julia will build and work on other platforms too,
and we're always looking to improve our platform coverage.  If you're
using Julia on a platform not listed here, let us know!

## Source Download and Compilation

First, make sure you have all the [required
dependencies](#required-build-tools-and-external-libraries) installed.
Then, acquire the source code by cloning the git repository:

    git clone git://github.com/JuliaLang/julia.git

By default you will be building the latest unstable version of
Julia. However, most users should use the most recent stable version
of Julia. You can get this version by changing to the Julia directory
and running:

    git checkout v1.1.0

Now run `make` to build the `julia` executable.

Building Julia requires 2GiB of disk space and approximately 4GiB of virtual memory.

**Note:** The build process will fail badly if any of the build directory's parent directories have spaces or other shell meta-characters such as `$` or `:` in their names (this is due to a limitation in GNU make).

Once it is built, you can run the `julia` executable after you enter your julia directory and run

    ./julia

If everything works correctly, you will see a Julia banner and an
interactive prompt into which you can enter expressions for
evaluation. (Errors related to libraries might be caused by old,
incompatible libraries sitting around in your PATH. In this case, try
moving the `julia` directory earlier in the PATH). Note that most of
the instructions above apply to unix systems.

Your first test of Julia determines whether your build is working
properly. From the UNIX/Windows command prompt inside the `julia`
source directory, type `make testall`. You should see output that
lists a series of running tests; if they complete without error, you
should be in good shape to start using Julia.

You can read about [getting
started](https://docs.julialang.org/en/stable/manual/getting-started/)
in the manual.

## Uninstalling Julia

Julia does not install anything outside the directory it was cloned
into. Julia can be completely uninstalled by deleting this
directory. Julia packages are installed in `~/.julia` by default, and
can be uninstalled by deleting `~/.julia`.

## Required Build Tools and External Libraries

Building Julia requires that the following software be installed:

- **[GNU make]**                — building dependencies.
- **[gcc & g++][gcc]** (>= 4.7) or **[Clang][clang]** (>= 3.1, Xcode 4.3.3 on macOS) — compiling and linking C, C++.
- **[libatomic][gcc]**          — provided by **[gcc]** and needed to support atomic operations.
- **[python]** (>=2.7)          — needed to build LLVM.
- **[gfortran]**                — compiling and linking Fortran libraries.
- **[perl]**                    — preprocessing of header files of libraries.
- **[wget]**, **[curl]**, or **[fetch]** (FreeBSD) — to automatically download external libraries.
- **[m4]**                      — needed to build GMP.
- **[awk]**                     — helper tool for Makefiles.
- **[patch]**                   — for modifying source code.
- **[cmake]** (>= 3.4.3)        — needed to build `libgit2`.
- **[pkg-config]**              — needed to build `libgit2` correctly, especially for proxy support.

On Debian-based distributions (e.g. Ubuntu), you can easily install them with `apt-get`:
```
sudo apt-get install build-essential libatomic1 python gfortran perl wget m4 cmake pkg-config
```

Julia uses the following external libraries, which are automatically
downloaded (or in a few cases, included in the Julia source
repository) and then compiled from source the first time you run
`make`:

- **[LLVM]** (6.0 + [patches](https://github.com/JuliaLang/julia/tree/master/deps/patches)) — compiler infrastructure (see [note below](#llvm)).
- **[FemtoLisp]**            — packaged with Julia source, and used to implement the compiler front-end.
- **[libuv]**  (custom fork) — portable, high-performance event-based I/O library.
- **[OpenLibm]**             — portable libm library containing elementary math functions.
- **[DSFMT]**                — fast Mersenne Twister pseudorandom number generator library.
- **[OpenBLAS]**             — fast, open, and maintained [basic linear algebra subprograms (BLAS)](https://en.wikipedia.org/wiki/Basic_Linear_Algebra_Subprograms) library, based on [Kazushige Goto's](https://en.wikipedia.org/wiki/Kazushige_Goto) famous [GotoBLAS](https://www.tacc.utexas.edu/research-development/tacc-software/gotoblas2) (see [note below](#blas-and-lapack)).
- **[LAPACK]** (>= 3.5)      — library of linear algebra routines for solving systems of simultaneous linear equations, least-squares solutions of linear systems of equations, eigenvalue problems, and singular value problems.
- **[MKL]** (optional)       – OpenBLAS and LAPACK may be replaced by Intel's MKL library.
- **[SuiteSparse]** (>= 4.1) — library of linear algebra routines for sparse matrices.
- **[PCRE]** (>= 10.00)      — Perl-compatible regular expressions library.
- **[GMP]** (>= 5.0)         — GNU multiple precision arithmetic library, needed for `BigInt` support.
- **[MPFR]** (>= 4.0)        — GNU multiple precision floating point library, needed for arbitrary precision floating point (`BigFloat`) support.
- **[libgit2]** (>= 0.23)    — Git linkable library, used by Julia's package manager.
- **[curl]** (>= 7.50)       — libcurl provides download and proxy support for Julia's package manager.
- **[libssh2]** (>= 1.7)     — library for SSH transport, used by libgit2 for packages with SSH remotes.
- **[mbedtls]** (>= 2.2)     — library used for cryptography and transport layer security, used by libssh2
- **[utf8proc]** (>= 2.1)    — a library for processing UTF-8 encoded Unicode strings.
- **[libosxunwind]**         — fork of [libunwind], a library that determines the call-chain of a program.

[GNU make]:     https://www.gnu.org/software/make
[patch]:        https://www.gnu.org/software/patch
[wget]:         https://www.gnu.org/software/wget
[m4]:           https://www.gnu.org/software/m4
[awk]:          https://www.gnu.org/software/gawk
[gcc]:          https://gcc.gnu.org
[clang]:        https://clang.llvm.org
[python]:       https://www.python.org/
[gfortran]:     https://gcc.gnu.org/fortran/
[curl]:         https://curl.haxx.se
[fetch]:        https://www.freebsd.org/cgi/man.cgi?fetch(1)
[perl]:         https://www.perl.org
[cmake]:        https://www.cmake.org
[OpenLibm]:     https://github.com/JuliaLang/openlibm
[DSFMT]:        http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/#dSFMT
[OpenBLAS]:     https://github.com/xianyi/OpenBLAS
[LAPACK]:       https://www.netlib.org/lapack
[MKL]:          https://software.intel.com/en-us/articles/intel-mkl
[SuiteSparse]:  http://faculty.cse.tamu.edu/davis/suitesparse.html
[PCRE]:         https://www.pcre.org
[LLVM]:         https://www.llvm.org
[FemtoLisp]:    https://github.com/JeffBezanson/femtolisp
[GMP]:          https://gmplib.org
[MPFR]:         https://www.mpfr.org
[libuv]:        https://github.com/JuliaLang/libuv
[libgit2]:      https://libgit2.org/
[utf8proc]:     https://julialang.org/utf8proc/
[libosxunwind]: https://github.com/JuliaLang/libosxunwind
[libunwind]:    https://www.nongnu.org/libunwind
[libssh2]:      https://www.libssh2.org
[mbedtls]:      https://tls.mbed.org/
[pkg-config]:   https://www.freedesktop.org/wiki/Software/pkg-config/

## Source Code Organization

The Julia source code is organized as follows:

    base/          source code for the Base module (part of Julia's standard library)
    stdlib/        source code for other standard library packages
    contrib/       editor support for Julia source, miscellaneous scripts
    deps/          external dependencies
    doc/src/manual source for the user manual
    src/           source for Julia language core
    test/          test suites
    ui/            source for various front ends
    usr/           binaries and shared libraries loaded by Julia's standard libraries

## Binary Installation

If  you  would  rather  not  compile the  latest  Julia  from  source,
platform-specific  tarballs   with  pre-compiled  binaries   are  also
[available for download](https://julialang.org/downloads/).

You can either run the `julia` executable using its full path in the
directory created above, or add that directory to your executable path
so that you can run the Julia program from anywhere (in the current
shell session):

```sh
export PATH="$(pwd)/julia:$PATH"
```
Now you should be able to run Julia like this:

    julia

On Windows, double-click `usr/bin/julia.exe`.

If everything works correctly, you will see a Julia banner and an
interactive prompt into which you can enter expressions for
evaluation.  You can read about [getting
started](https://julialang.org/manual/getting-started) in the manual.

**Note**: Although some system package managers provide Julia, such
installations are neither maintained nor endorsed by the Julia
project. They may be outdated and/or unmaintained. We recommend you
use the official Julia binaries instead.

## Editor and Terminal Setup

Currently, Julia editing mode support is available for a number of
editors. While Julia modes for
[Emacs](https://github.com/JuliaLang/julia-emacs),
[Sublime Text](https://github.com/JuliaEditorSupport/Julia-sublime), and
[Vim](https://github.com/JuliaLang/julia-vim) have their own repos,
others such as Textmate, Notepad++, and Kate, are in
`contrib/`.

Two major IDEs are supported for Julia: [Juno](http://junolab.org/)
which is based on [Atom](https://atom.io/) and
[julia-vscode](https://github.com/JuliaEditorSupport/julia-vscode)
based on [VS Code](https://code.visualstudio.com/). A
[Jupyter](https://jupyter.org/) notebooks interface is available
through [IJulia](https://github.com/JuliaLang/IJulia.jl).

In the terminal, Julia makes great use of both control-key and
meta-key bindings. To make the meta-key bindings more accessible, many
terminal emulator programs (e.g., `Terminal`, `iTerm`, `xterm`, etc.)
allow you to use the alt or option key as meta.  See the section in
the manual on [the Julia
REPL](https://docs.julialang.org/en/latest/stdlib/REPL/) for more
details.

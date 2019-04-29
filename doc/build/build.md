# Building Julia (Detailed)

## Downloading the Julia source code

If you are behind a firewall, you may need to use the `https` protocol instead of the `git` protocol:

```sh
git config --global url."https://".insteadOf git://
```

Be sure to also configure your system to use the appropriate proxy
settings, e.g. by setting the `https_proxy` and `http_proxy`
variables.

## Building Julia

When compiled the first time, the build will automatically download
pre-built [external
dependencies](#required-build-tools-and-external-libraries). If you
prefer to build all the dependencies on your own, add the following in
`Make.user`

```
USE_BINARYBUILDER=0
```

Building Julia requires 5GiB if building all dependencies and approximately 4GiB of virtual memory.

To perform a parallel build, use `make -j N` and supply the maximum
number of concurrent processes. If the defaults in the build do not work for you, and
you need to set specific make parameters, you can save them in
`Make.user`, and place the file in the root of your Julia source. The
build will automatically check for the existence of `Make.user` and
use it if it exists.

You can create out-of-tree builds of Julia by specifying `make
O=<build-directory> configure` on the command line. This will create a
directory mirror, with all of the necessary Makefiles to build Julia,
in the specified directory. These builds will share the source files
in Julia and `deps/srccache`. Each out-of-tree build directory can
have its own `Make.user` file to override the global `Make.user` file
in the top-level folder.

If everything works correctly, you will see a Julia banner and an
interactive prompt into which you can enter expressions for
evaluation. (Errors related to libraries might be caused by old,
incompatible libraries sitting around in your PATH. In this case, try
moving the `julia` directory earlier in the PATH). Note that most of
the instructions above apply to unix systems.

To run julia from anywhere you can:
- add an alias (in `bash`: `echo "alias julia='/path/to/install/folder/bin/julia'" >> ~/.bashrc && source ~/.bashrc`), or

- add a soft link to the `julia` executable in the `julia` directory to `/usr/local/bin` (or any suitable directory already in your path), or

- add the `julia` directory to your executable path for this shell session (in `bash`: `export PATH="$(pwd):$PATH"` ; in `csh` or `tcsh`:
`set path= ( $path $cwd )` ), or

- add the `julia` directory to your executable path permanently (e.g. in `.bash_profile`), or

- write `prefix=/path/to/install/folder` into `Make.user` and then run `make install`. If there is a version of Julia already installed in this folder, you should delete it before running `make install`.

Now you should be able to run Julia like this:

    julia

If you are building a Julia package for distribution on Linux, macOS,
or Windows, take a look at the detailed notes in
[distributing.md](https://github.com/JuliaLang/julia/blob/master/doc/build/distributing.md).

## Updating an existing source tree

If you have previously downloaded `julia` using `git clone`, you can update the
existing source tree using `git pull` rather than starting anew:
```sh
cd julia
git pull && make
```
Assuming that you had made no changes to the source tree that will conflict
with upstream updates, these commands will trigger a build to update to the
latest version.

## General troubleshooting

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

3. If you've updated macOS recently, be sure to run `xcode-select --install` to update the command line tools.
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

## Platform-Specific Notes

Notes for various operating systems:

* [Linux](https://github.com/JuliaLang/julia/blob/master/doc/build/linux.md)
* [macOS](https://github.com/JuliaLang/julia/blob/master/doc/build/macos.md)
* [Windows](https://github.com/JuliaLang/julia/blob/master/doc/build/windows.md)
* [FreeBSD](https://github.com/JuliaLang/julia/blob/master/doc/build/freebsd.md)

Notes for various architectures:

* [ARM](https://github.com/JuliaLang/julia/blob/master/doc/build/arm.md)

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

## Build dependencies

If you already have one or more of these packages installed on your system, you can prevent Julia from compiling duplicates of these libraries by passing `USE_SYSTEM_...=1` to `make` or adding the line to `Make.user`. The complete list of possible flags can be found in `Make.inc`.

Please be aware that this procedure is not officially supported, as it introduces additional variability into the installation and versioning of the dependencies, and is recommended only for system package maintainers. Unexpected compile errors may result, as the build system will do no further checking to ensure the proper packages are installed.

### LLVM

The most complicated dependency is LLVM, for which we require additional patches from upstream (LLVM is not backward compatible).

For packaging Julia with LLVM, we recommend either:
 - bundling a Julia-only LLVM library inside the Julia package, or
 - adding the patches to the LLVM package of the distribution.
   * A complete list of patches is available in `deps/llvm.mk`, and the patches themselves are in `deps/patches/`.
   * The only Julia-specific patch is the lib renaming (`llvm-symver-jlprefix.patch`), which should _not_ be applied to a system LLVM.
   * The remaining patches are all upstream bug fixes, and have been contributed into upstream LLVM.

Using an unpatched or different version of LLVM will result in errors and/or poor performance. Though Julia can be built with newer LLVM versions, support for this should be regarded as experimental and not suitable for packaging.

### libuv

Julia uses a custom fork of libuv. It is a small dependency, and can be safely bundled in the same package as Julia, and will not conflict with the system library. Julia builds should _not_ try to use the system libuv.

### BLAS and LAPACK

As a high-performance numerical language, Julia should be linked to a multi-threaded BLAS and LAPACK, such as OpenBLAS or ATLAS, which will provide much better performance than the reference `libblas` implementations which may be default on some systems.

### Intel MKL

For a 64-bit architecture, the environment should be set up as follows:
```sh
# bash
source /path/to/intel/bin/compilervars.sh intel64
```
Add the following to the `Make.user` file:

    USE_INTEL_MKL = 1

It is highly recommended to start with a fresh clone of the Julia repository.

Notes for building binary distributions
=======================================

These notes are for those wishing to compile a binary distribution of Julia
for distribution on various platforms.  We love users spreading Julia as
far and wide as they can, trying it out on as wide an array of
operating systems and hardware configurations as possible.  As each
platform has specific gotchas and processes that must be followed in
order to create a portable, working Julia distribution, we have
separated most of the notes by OS.

Note that while the code for Julia is
[MIT-licensed, with a few exceptions](https://github.com/JuliaLang/julia/blob/master/LICENSE.md),
the distribution created by the techniques described herein will be
GPL licensed, as various dependent libraries such as `FFTW` and
`SuiteSparse` are GPL licensed. We do hope to have a
non-GPL distribution of Julia in the future.

Versioning and Git
------------------
The Makefile uses both the `VERSION` file and commit hashes and tags from the
git repository to generate the `base/version_git.jl` with information we use to
fill the splash screen and the `versioninfo()` output. If you for some reason
don't want to have the git repository available when building you should
pregenerate the `base/version_git.jl` file with:

    make -C base version_git.jl.phony

Julia has lots of build dependencies where we use patched versions that has not
yet been included by the popular package managers. These dependencies will usually
be automatically downloaded when you build, but if you want to be able to build
Julia on a computer without internet access you should create a full-source-dist
archive with the special make target

    make full-source-dist

that creates a julia-version-commit.tar.gz archive with all required dependencies.

When compiling a tagged release in the git repository, we don't display the
branch/commit hash info in the splash screen. You can use this line to show
a release description of up to 45 characters. To set this line you have
to create a Make.user file containing:

    override TAGGED_RELEASE_BANNER = "my-package-repository build"

Target Architectures
--------------------

By default, Julia optimizes its system image to the native architecture of
the build machine. This is usually not what you want when building packages,
as it will make Julia fail at startup on any machine with incompatible CPUs
(in particular older ones with more restricted instruction sets).

We therefore recommend that you pass the `MARCH` variable when calling `make`,
setting it to the baseline target you intend to support. This will determine
the target CPU for both the Julia executable and libraries, and the system
image (the latter can also be set using `JULIA_CPU_TARGET`). Typically useful
values for x86 CPUs are `x86-64` and `core2` (for 64-bit builds) and
`pentium4` (for 32-bit builds). Unfortunately, CPUs older than Pentium 4
are currently not supported (see
[this issue](https://github.com/JuliaLang/julia/issues/7185)).

The full list of CPU targets supported by LLVM can be obtained by running
`llc -mattr=help`.

Linux
-----

On Linux, `make binary-dist` creates a tarball that contains a fully
functional Julia installation. If you wish to create a distribution
package such as a `.deb`, or `.rpm`, some extra effort is needed. See the
[julia-debian](https://github.com/staticfloat/julia-debian) repository
for an example of what metadata is needed for creating `.deb` packages
for Debian and Ubuntu-based systems. See the
[Fedora package](https://admin.fedoraproject.org/pkgdb/package/julia/)
for RPM-based distributions. Although we have not yet experimented
with it, [Alien](https://wiki.debian.org/Alien) could be used to
generate Julia packages for various Linux distributions.

Julia supports overriding standard installation directories via `prefix`
and other environment variables you can pass when calling `make` and
`make install`. See Make.inc for their list. `DESTDIR` can also be used
to force the installation into a temporary directory.

By default, Julia loads `$prefix/etc/julia/juliarc.jl` as an
installation-wide initialization file. This file can be used by
distribution managers to set up custom paths or initialization code.
For Linux distribution packages, if `$prefix` is
set to `/usr`, there is no `/usr/etc` to look into. This requires
the path to Julia's private `etc` directory to be changed.  This can
be done via the `sysconfdir` make variable when building.  Simply
pass `sysconfdir=/etc` to `make` when building and Julia will first
check `/etc/julia/juliarc.jl` before trying
`$prefix/etc/julia/juliarc.jl`.

OS X
----

To create a binary distribution on OSX, build Julia first, then cd to
`contrib/mac/app`, and run `make` with the same makevars that were used
with `make` when building Julia proper.  This will then
create a `.dmg` file in the `contrib/mac/app` directory holding a
completely self-contained Julia.app.

Note that if you want your `.app` to be able to run on OSX 10.6 Snow
Leopard, you must pass `USE_SYSTEM_LIBUNWIND=1` as one of the make
variables passed to both `make` processes. This disables the use of
`libosxunwind`, a more modern libunwind that relies on OS features
available only in 10.7+. Furthermore, support for OSX 10.6 and 10.7
requires that Julia is built with `USE_LIBCPP=0`.

Windows
-------

The best supported method of creating a Julia distribution on Windows
is to cross-compile from a Linux distribution such as Ubuntu. In-depth
compilation instructions [are
available](https://github.com/JuliaLang/julia/blob/master/README.windows.md).
However the important steps for redistribution are to ensure to `make
win-extras` in between `make` and `make binary-dist`.  After that process is
completed, the `.zip` file created in the head Julia directory will
hold a completely self-contained Julia.

Notes on BLAS and LAPACK
------------------------

Julia builds OpenBLAS by default, which includes the BLAS and LAPACK
libraries. On 32-bit architectures, Julia builds OpenBLAS to use
32-bit integers, while on 64-bit architectures, Julia builds OpenBLAS
to use 64-bit integers (ILP64). It is essential that all Julia functions
that call BLAS and LAPACK API routines use integers of the correct width.

Most BLAS and LAPACK distributions provided on linux distributions,
and even commercial implementations ship libraries that use 32-bit
APIs. In many cases, a 64-bit API is provided as a separate library.

When using vendor provided or OS provided libraries, a `make` option
called `USE_BLAS64` is available as part of the Julia build. When doing
`make USE_BLAS64=0`, Julia will call BLAS and LAPACK assuming a 32-bit
API, where all integers are 32-bit wide, even on a 64-bit architecture.

Other libraries that Julia uses, such as ARPACK and SuiteSparse also
use BLAS and LAPACK internally. The APIs need to be consistent across
all libraries that depend on BLAS and LAPACK. The Julia build process
will build all these libraries correctly, but when overriding defaults
and using system provided libraries, this consistency must be ensured.

Also note that Linux distributions sometimes ship several versions of
OpenBLAS, some of which enable multithreading, and others only working
in a serial fashion. For example, in Fedora, `libopenblasp.so` is threaded,
but `libopenblas.so` is not. We recommend using the former for optimal
performance. To choose an OpenBLAS library whose name is different from
the default `libopenblas.so`, pass `LIBBLAS=-l$(YOURBLAS)` and
`LIBBLASNAME=lib$(YOURBLAS)` to `make`, replacing `$(YOURBLAS)` with the
name of your library. You can also add `.so.0` to the name of the library
if you want your package to work without requiring the unversioned `.so`
symlink.

Finally, OpenBLAS includes its own optimized version of LAPACK. If you
set `USE_SYSTEM_BLAS=1` and `USE_SYSTEM_LAPACK=1`, you should also set
`LIBLAPACK=-l$(YOURBLAS)` and `LIBLAPACKNAME=lib$(YOURBLAS)`. Else, the
reference LAPACK will be used and performance will typically be much lower.

Compilation scripts
===================

The [julia-nightly-packaging](https://github.com/staticfloat/julia-nightly-packaging)
repository contains multiple example scripts to ease the creation of
binary packages. It also includes miscellaneous tools to do things such as
fetching the last good commit that passed the
[Travis](https://travis-ci.org/JuliaLang/julia/builds) tests.


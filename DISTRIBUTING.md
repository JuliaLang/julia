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
[MIT-licensed](https://github.com/JuliaLang/julia/blob/master/LICENSE.md),
the distribution created by the techniques described herein will be
GPL licensed, as various dependent libraries such as `FFTW`, `Rmath`,
`SuiteSparse`, and `git` are GPL licensed. We do hope to have a
non-GPL distribution of Julia in the future.

When compiling a tagged release in the git repository, we don't display the
branch/commit hash info in the splash screen. You can use this line to show
a release description of up to 45 characters. To set this line you have
to create a Make.user file containing:

    override TAGGED_RELEASE_BANNER = "my-package-repository build"


Linux
-----

On Linux, `make dist` creates a tarball that contains a fully functional Julia
installation. If you wish to create a distribution package such as a
`.deb`, or `.rpm`, some extra effort is needed. See the
[julia-debian](http://github.com/staticfloat/julia-debian) repository
for an example of what metadata is needed for creating `.deb` packages
for Debian and Ubuntu-based systems. Although we have not yet experimented
with it, [Alien](https://wiki.debian.org/Alien) could be used to
generate Julia packages for various Linux distributions.

Julia looks for git versioning information when building.  If it does
not find the git executable or the `.git/` directory is unreadable,
the build process will continue, however all versioning information
will be unavailable.  This is the case, for instance, on Canonical's
build servers where the [Ubuntu
nightlies](https://launchpad.net/~staticfloat/+archive/julianightlies)
are built.  Therefore, a workaround must be enacted, where the git
versioning information [is encoded into the
source](https://github.com/staticfloat/julia-nightly-packaging/blob/master/build_ubuntu.sh#L76-78) before upload for building, and the source is modified to [not attempt to look for it](https://github.com/staticfloat/julia-nightly-packaging/blob/master/nogit-workaround.patch) when building.

By default, Julia loads `$PREFIX/etc/julia/juliarc.jl` as an
installation-wide initialization file. This file can be used by
distribution managers to provide paths to various binaries such as a
bundled `git` executable (as we do on OS X), or to setup paths (as
we do on Windows).  For Linux distribution packages, if `$PREFIX` is
set to `/usr`, there is no `/usr/etc` to look into. This requires
the path to Julia's private `etc` directory to be changed.  This can
be done via the `SYSCONFDIR` make variable when building.  Simply
pass `SYSCONFDIR=/etc` to `make` when building and Julia will first
check `/etc/julia/juliarc.jl` before trying
`$PREFIX/etc/julia/juliarc.jl`.

OS X
----

To create a binary distribution on OSX, build Julia first, then cd to
`contrib/mac/app`, and run `make` with the same makevars that were used
with `make` when building Julia proper.  This will then
create a `.dmg` file in the `contrib/mac/app` directory holding a
completely self-contained Julia.app.

Note that if you want your `.app` to be able to run on OSX 10.6 Snow
Leopard, you must pass `USE_SYSTEM_LIBUNWIND=1` as one of the make
variables passed to both `make` processes.  This disables the use of
`libosxunwind`, a more modern libunwind that relies on OS features
available only in 10.7+.  This is the reason why we offer [separate
downloads](http://julialang.org/downloads/) for OS X 10.6 and 10.7+.

Windows
-------

The best supported method of creating a Julia distribution on Windows
is to cross-compile from a Linux distribution such as Ubuntu. In-depth
compilation instructions [are
available](https://github.com/JuliaLang/julia/blob/master/README.windows.md).
However the important steps for redistribution are to ensure to `make
win-extras` in between `make` and `make dist`.  After that process is
completed, the `.zip` file created in the head Julia directory will
hold a completely self-contained Julia.


Compilation scripts
===================

The [julia-nightly-packaging](https://github.com/staticfloat/julia-nightly-packaging) repository contains multiple example scripts to ease the creation of
binary packages. It also includes miscellaneous tools to do things such as
fetching the last good commit that passed the
[Travis](https://travis-ci.org/JuliaLang/julia/builds) tests.


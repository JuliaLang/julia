Binary distribution notes
=========================

Contained herein are notes for those wishing to compile a binary distribution of Julia for dispersal on various platforms.  We love users spreading Julia as far and wide as they can, trying it out on as wide an array of operating systems and hardware configurations as possible.  As each platform has specific gotchas and processes that must be followed in order to create a portable, working Julia distribution, we have separated most of the notes by OS.


Linux
-----
To create a binary distribution on Linux, one need do no more than run `make dist` to create a tarball that contains a fully functional Julia installation.  This installation can be extracted anywhere, however if you wish to create a distribution package such as a `.deb`, or `.rpm`, some more effort is needed.  For all major packaging systems, some extra metadata is needed, see the [julia-debian](http://github.com/staticfloat/julia-debian) repository for an example of what metadata is needed for creating `.deb` packages for Debian and Ubuntu-based systems.

Some miscellaneous things of note when creating distribution packages:

* Julia looks for git versioning information when building.  If it does not find the git executable or the `.git/` directory is unreadable, the build process will continue, however all versioning information will be unavailable.  This is the case, for instance, on canonical's `buildd` servers where the [Ubuntu nightlies](https://launchpad.net/~staticfloat/+archive/julianightlies) are built.  Therefore, a workaround must be enacted, where the git versioning information [is encoded into the source](https://github.com/staticfloat/julia-nightly-packaging/blob/master/build_ubuntu.sh#L76-78) before upload for building, and the source is modified to [not attempt to look for it](https://github.com/staticfloat/julia-nightly-packaging/blob/master/nogit-workaround.patch) when building.

* Julia, by default, loads `$PREFIX/etc/julia/juliarc.jl` as a kind of installation-wide initialization file. This file can be used by distribution managers to provide paths to various binaries such as a bundled `git` executable in the OSX case, or to setup paths such as in the windows case.  For linux distribution packages, if `$PREFIX` is set to `/usr`, there is no `/usr/etc` to be used, therefore the path to julia's private `etc` directory must be changed.  This can be done via the `SYSCONFDIR` make variable when building.  Simply pass `SYSCONFDIR=/etc` to `make` when building and Julia will first check `/etc/julia/juliarc.jl` before trying `$PREFIX/etc/julia/juliarc.jl`.


OSX
---
To create a binary distribution on OSX, first build Julia, then cd to `contrib/mac/app` and run `make` with the same makevars as were passed into the `make` process when building Julia proper.  This will then create a `.dmg` file in the `contrib/mac/app` directory holding a completely self-contained Julia.app.

Note that if you want your .app to be able to run on OSX 10.6 Snow Leopard, you must pass `USE_SYSTEM_LIBUNWIND=1` as one of the make variables passed to both `make` processes.  This disables the use of `libosxunwind`, a more modern libunwind that relies on OS features available only in 10.7+.  This is the reason we offer [separate downloads](http://julialang.org/downloads/) for 10.6 and 10.7+.


Windows
-------
The best supported method of creating a Julia distribution on Windows is to cross-compile from a linux distribution such as Ubuntu. Indepth compilation instructions [are available](https://github.com/JuliaLang/julia/blob/master/README.windows.md), however the important steps for redistrubition are to ensure to `make win-extras` in between `make` and `make dist`.  After that process is completed, the `.zip` file created in the head Julia directory will hold a completely self-contained Julia.app


Compilation scripts
===================

The [julia-nightly-packaging](https://github.com/staticfloat/julia-nightly-packaging) repository contains multiple example scripts to ease the creation of binary packages, as well as miscellaneous tools to do things such as get the last good commit that passed the [Travis](https://travis-ci.org/JuliaLang/julia/builds) testing, etc...
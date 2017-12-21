# Uninstall

To uninstall this Julia distribution, run `julia-uninstall` (located in the
`sbin` sub-directory of the installation prefix).

For example, run the following shell command as root to uninstall:

```
# /usr/local/sbin/julia-uninstall
julia-uninstall: Will uninstall package org.julialang.installer.distribution.
julia-uninstall: Package org.julialang.installer.distribution was installed in prefix "/usr/local".
Proceed with uninstallation? [YES/no]  YES
julia-uninstall: Uninstalling package org.julialang.installer.distribution from "/usr/local".
Forgot package 'org.julialang.installer.distribution' on '/'.
julia-uninstall: Successfully uninstalled package org.julialang.installer.distribution.
```

The uninstall script should only attempt to remove files and (empty)
directories that were originally created by the installer.  The remaining
contents of the installation prefix should be untouched.

# Git

[Git](https://git-scm.com) is not included in this distribution.  One easy way
to obtain Git is by installing the Command Line Tools from Apple.  Simply run
`xcode-select --install` from your shell.

Alternatively, Git may be installed from [Homebrew](http://brew.sh) or
[MacPorts](https://www.macports.org).

# Path Modification

In order for the shell to find and run the `julia` command, the `PATH`
environment variable must contain the path to the `julia` binary.  Typically,
shells running on OS X include `/usr/local/bin` in the `PATH` by default.
Thus, for most systems and installations, running `julia` without its full
pathname should just work.

During installation, if the installer prefix was changed from the default
(`/usr/local`) to `/opt/julia`, for example, then add the directory containing
the `julia` binary (e.g., `/opt/julia/bin`) to the `PATH` environment variable
or run the `julia` binary with its full pathname (e.g.,
`/opt/julia/bin/julia`).

# Native system image

The system image distributed in this installer is not optimized for this
system.  In order to build a system image that is optimized for this system,
follow the method below.

**If the `sudo` command or running with root privileges is unfamiliar, please
skip these instructions.**

This method avoids building the system image as the root user.  After running
the following bash script, the system image should be optimized for this
system.  (tip: highlight the script lines and copy them [⌘+c] and run
`pbpaste|bash -s`)
```
#!/bin/bash
set -e
PREFIX="$(julia -e "println(normpath(JULIA_HOME,\"..\"))")"
cd "$(/usr/bin/mktemp -d -t juliasysimg)"
julia "$PREFIX/share/julia/build_sysimg.jl" "$(pwd)/sys" native
install_name_tool -id "@rpath/sys.dylib" ./sys.dylib
sudo cp -f ./sys.dylib "$PREFIX/lib/julia/"
rm -rf ./*
rmdir $(pwd)
```

# Odds and ends

To find the installation prefix, run the following Julia command in a shell:
```
> julia -e "println(normpath(JULIA_HOME,\"..\"))"
/usr/local/
```

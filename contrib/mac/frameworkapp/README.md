New Julia Launcher App
======================

This builds the Julia framework and a launcher app and packages them in a
product archive for the macOS Installer.

Run `make APPLE_DEVELOPMENT_TEAM=xxxxxxxxxx` to build the product archive.  The
resulting archive may be installed to the home directory with
`installer -pkg~/Documents/pkgs/Julia-1.1.0.pkg -target CurrentUserHomeDirectory`.
To just build the app, build the `appexport` make target.  Read the comments at
the top of the `Makefile` to set appropriate code signing parameters.

The framework is installed in `/Library/Frameworks` and the app in
`/Applications`.  Installation may be system-wide (i.e., relative to `/`) or
local to the user's home directory (i.e., `$Home/Appliations/Julia.app`).

The `julia` binary is embedded in the framework at
`Julia.framework/Helpers/julia`.

Multiple versions of Julia may be installed at once.  Each version is placed in
the `Julia.framework/Versions` directory.  By default, the version is
identified by the Major.Minor version number but may be customized by setting
the `FRAMEWORK_VERSION` make variable.  The resulting product archive will not
overwrite other versions but will upgrade a version if it exists.  Thus, the
`1.1` framework version that is actually the 3rd patch (1.1.3) will overwrite
any existing `1.1` framework version.

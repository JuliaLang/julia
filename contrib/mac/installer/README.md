# Summary

```
> echo "override MARCH = x86-64" >> Make.user
> make [-jN] binary-dist
> cd contrib/mac/installer
> ./buildinstaller path/to/julia-binary-dist.tar.gz <version>
```

# Motivation

These scripts build an OS X installer for Julia for a better user experience.
If you already have Git and use other command line tools regularly, having
Julia in `/usr/local` just feels right.  When Julia is inside an app bundle,
the julia binary is not in the `PATH`, requiring extra knowledge from the user.
Furthermore, the Julia app is merely a wrapper around launching Terminal.app
and running the embedded julia binary.  Using an OS X installer package avoids
the hassle of manually unpacking an archive and moving it into place.
Moreover, the installer has both a UI and command line interface for regular
and power users alike.

This contribution includes a script and the supporting files to create an
installer (with uninstaller!) for Julia on OS X.

# Details

The installer does not include Git and requires admin privileges.  We assume
the user can run `xcode-select --install` to get the Command Line Tools from
Apple, which includes Git.  Alternatively, the user can install Git from
Homebrew or MacPorts.

As per the instructions in DISTRIBUTING.md, MARCH should be set to a low common
denominator if the installer is intended for older hardware.  Setting `MARCH`
to x86-64 should be pretty safe. In Make.user:
```
override MARCH = x86-64
```

Once all build customizations are ready, build the `binary-dist` target:
```
> make binary-dist
```

Then, run the script to build the installer.  The script takes two arguments:
1. the path to the archived binary distribution produced by make, and
2. the version for the OS X installer.

The identities used to sign the installer and binaries may be changed by
setting the environment variables `SIGN_INST_IDENT` and `SIGN_APP_IDENT` before
running the `buildinstaller` script.

```
> ./buildinstaller path/to/julia-binary-dist.tar.gz <version>
```

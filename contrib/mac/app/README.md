Julia OS X packaging
====================

This builds the Julia OS X application bundle (.app folder), and stores it in a disk image
(.dmg file).

The application bundle opens Terminal.app and executes the julia binary (which opens
the REPL). All the Julia binary files and their dependencies are bundled inside this.

Its double-clickable entry point is `julia-terminal` (the bundle's `CFBundleExecutable`):
a real copy of the julia loader placed directly at `Contents/MacOS/julia-terminal`. When
the loader is invoked under that name it relaunches the REPL binary from the bundled tree
(`Contents/Resources/julia/bin/julia`) inside a new Terminal.app window rather than
starting the REPL directly in the (windowless) bundle process. See `cli/loader_exe.c` for
that logic.

The main executable is a real file in `Contents/MacOS/` -- not a symlink, and not inside
`Contents/Resources/` -- because Apple's notary rejects a bundle main executable that is a
symlink or that lives among the sealed resources. Because this copy runs from
`Contents/MacOS/` rather than the tree's `bin/`, the loader is linked with an extra rpath
into the bundled tree so dyld can resolve `libjulia` from there (see `cli/Makefile`);
assembling the bundle is then a plain `cp` needing no Mach-O tools, so it runs on any
host (CI assembles the .app on Linux).

Run `make` to build.

Other files in this directory

* `Info.plist.in` is the template for the bundle's `Info.plist`.
* `julia.icns` is the Julia icon file.

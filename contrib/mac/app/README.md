Julia OS X packaging
====================

This builds the Julia OS X application bundle (.app folder), and stores it in a disk image
(.dmg file).

The application bundle opens Terminal.app and executes the julia binary (which opens
the REPL). All the Julia binary files and their dependencies are bundled inside this.

Its double-clickable entry point is `julia-terminal`, a hardlink to the bundled julia
binary (`Contents/Resources/julia/bin/julia-terminal`, reached via a
`Contents/MacOS/julia-terminal` symlink). When the loader is invoked under the name
`julia-terminal` it relaunches julia inside a new Terminal.app window rather than starting
the REPL directly in the (windowless) bundle process. See `cli/loader_exe.c` for that
logic. A hardlink is used rather than a symlink because LaunchServices resolves a
symlinked `CFBundleExecutable` down to its target, discarding the `julia-terminal` name
that the loader keys off.

Run `make` to build.

Other files in this directory

* `Info.plist.in` is the template for the bundle's `Info.plist`.
* `julia.icns` is the Julia icon file.

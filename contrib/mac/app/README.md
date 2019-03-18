Julia OS X packaging
====================

This builds the Julia OS X application bundle (.app folder), and stores it in a disk image
(.dmg file).

The application bundle is actually just a bundled applet which opens Terminal.app and
executes the julia binary (which opens the REPL). All the Julia binary files and their
dependencies are bundled inside this.

Run `make` to build.

Other files in this directory

* `startup.applescript` is the script which is compiled to the applet.
* `julia.icns` is the Julia icon file.

## FreeBSD

Clang is the default compiler on FreeBSD 11.0-RELEASE and above.
The remaining build tools are available from the Ports Collection, and can be installed using
`pkg install git gcc gmake cmake pkgconf`.
To build Julia, simply run `gmake`.
(Note that `gmake` must be used rather than `make`, since `make` on FreeBSD corresponds to the incompatible BSD Make rather than GNU Make.)

As mentioned above, it is important to note that the `USE_SYSTEM_*` flags should be used with caution on FreeBSD.
This is because many system libraries, and even libraries from the Ports Collection, link to the system's `libgcc_s.so.1`,
or to another library which links to the system `libgcc_s`.
This library declares its GCC version to be 4.6, which is too old to build Julia, and conflicts with other libraries when linking.
Thus it is highly recommended to simply allow Julia to build all of its dependencies.
If you do choose to use the `USE_SYSTEM_*` flags, note that `/usr/local` is not on the compiler path by default, so you may need
to add `LDFLAGS=-L/usr/local/lib` and `CPPFLAGS=-I/usr/local/include` to your `Make.user`, though doing so may interfere with
other dependencies.

Note that the x86 architecture does not support threading due to lack of compiler runtime library support, so you may need to
set `JULIA_THREADS=0` in your `Make.user` if you're on a 32-bit system.

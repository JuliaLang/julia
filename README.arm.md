Compiling on ARM
----------------

Julia has been compiled on several ARMv7 / Cortex A15 Samsung Chromebooks running
Ubuntu Linux. This is a work in progress: several tests are known to fail, and
backtraces are not available.

Please start from the standard [build instructions](README.md#source-download-and-compilation),
in particular the Linux notes.

In addition to the standard `build-essentials` toolchain the following libraries
must be installed to build on ARM:

- libblas3gf, liblapack3gf, libfftw3-dev, libgmp3-dev, libmpfr-dev

Next, create a file in the `julia` directory called `Make.user` with the
following contents:

```
include $(JULIAHOME)/ARM.inc
```

Then proceed to build as described in the primary README.

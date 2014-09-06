Compiling on ARM
----------------

Julia has been compiled on several ARMv7 / Cortex A15 Samsung Chromebooks running
Ubuntu Linux. The port is still a work in progress, and several tests are known to
be failing.

In addition to the standard `build-essentials` toolchain, the following libraries
must be installed to build on ARM:

- libblas3gf, liblapack3gf, libsuitesparse-dev, libfftw3-dev, libgmp3-dev, libmpfr-dev

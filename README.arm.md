# Julia binaries for ARM

[Nightly builds](https://status.julialang.org/download/linux-arm) are
available for ARMv7-A.

# Hardware requirements

Julia requires at least `armv6` and `vfpv2` instruction sets. It's recommended
to use at least `armv7-a`. `armv5` or soft float are not supported.

# Building Julia on ARM

Julia has been compiled on several ARMv7 / Cortex A15 Samsung
Chromebooks running Ubuntu Linux under Crouton, Raspberry Pi systems
and Odroid boards. This is a work in progress - several tests are
known to fail, and backtraces are not available.

Julia on ARM can be built by simply typing `make`, which will download all
the relevant libraries. This is the *recommended* way, and it will take a
few hours.

If you get SIGILL during sysimg.o creation, it is likely that your cpu
does not support VFP.  File an issue on the Julia issue tracker with
the contents of /proc/cpuinfo.

This is the list of known issues on ARM:
 [https://github.com/JuliaLang/julia/labels/arm](https://github.com/JuliaLang/julia/labels/arm)

# Build dependencies

We recommend using at least Ubuntu 14.04 and gcc 4.8, which is part of the
standard `build-essentials`.

In case the build is failing on one of the dependent libraries, one
can install various system libraries instead of building them, by
adding the following lines in `Make.user`:

````
override USE_SYSTEM_BLAS=1
override USE_SYSTEM_LAPACK=1
override USE_SYSTEM_LIBM=1
override USE_SYSTEM_FFTW=1
override USE_SYSTEM_GMP=1
override USE_SYSTEM_MPFR=1
override USE_SYSTEM_ARPACK=1
````

The following command will install all the necessary libraries on Ubuntu.

````
sudo apt-get install libblas3gf liblapack3gf libarpack2 libfftw3-dev libgmp3-dev \
                     libmpfr-dev libblas-dev liblapack-dev cmake gcc-4.8 \
                     g++-4.8 gfortran libgfortran3 m4 libedit-dev
````

Note that OpenBLAS only supports ARMv7. For older ARM variants, using the reference BLAS
may be the simplest thing to do.

# ARM specific build problems

If you run into issues building LLVM, see these notes:
[http://llvm.org/docs/HowToBuildOnARM.html](http://llvm.org/docs/HowToBuildOnARM.html)

## Raspberry Pi 1 / Raspberry Pi Zero

Note: These chips use ARMv6, which is not well supported at the moment. However it is
possible to get a working Julia build.

The Raspberry Pi ARM CPU type is not detected by LLVM.  Before starting the
build, it is recommended to explicitly set the CPU target by adding the
following to `Make.user`:

````
JULIA_CPU_TARGET=arm1176jzf-s
````

It is also preferable to use various system provided dependencies on
ARMv6 as described in [Build Dependencies](#build-dependencies).

You may need to increase the swap file size: edit the `/etc/dphys-swapfile`, changing the line

    CONF_SWAPSIZE=100

to

    CONF_SWAPSIZE=512

Then restart the swapfile service:

    sudo /etc/init.d/dphys-swapfile stop
    sudo /etc/init.d/dphys-swapfile start

## Raspberry Pi 2

For Raspberry Pi 2, which is ARMv7, the default build should work. However, the
CPU type is also not detected by LLVM. Fix this by adding
`JULIA_CPU_TARGET=cortex-a7` to `Make.user`.

Depending on the exact compiler and distribution, there might be a build failure
due to unsupported inline assembly. In that case, add `MARCH=armv7-a` to
`Make.user`.

If building LLVM fails, you can download binaries from the LLVM website:

1.  Download the [LLVM 3.7.0 binaries for ARMv7a] (http://llvm.org/releases/3.7.0/clang+llvm-3.7.0-armv7a-linux-gnueabihf.tar.xz) and extract them in a local directory.
2. Add the following to `Make.user` (adjusting the path to the `llvm-config` binary):

    ```
    override USE_SYSTEM_LLVM=1
    LLVM_CONFIG=${EXTRACTED_LOCATION}/bin/llvm-config
    ```

Please do let us know if you had to download a pre-built LLVM in [#10235](https://github.com/JuliaLang/julia/issues/10235).

## Chromebook

On Chromebooks, you have to first install Crouton.  If you do not have
an Ubuntu chroot running on your Chromebook using Crouton, you can do
so by following these tutorials.

- [Crouton Tutorial 1](http://www.howtogeek.com/162120/how-to-install-ubuntu-linux-on-your-chromebook-with-crouton/)
- [Crouton Tutorial 2](http://lifehacker.com/how-to-install-linux-on-a-chromebook-and-unlock-its-ful-509039343)

These tutorials will end up installing Ubuntu 12.04, and you have to
upgrade to Ubuntu 14.04, or install Ubuntu 14.04 from scratch by
finding appropriate `crouton` help.

## Scaleway cloud hosted ARM servers

On the current [Scaleway](http://scaleway.com) ARM servers, the Julia
build works out of the box.

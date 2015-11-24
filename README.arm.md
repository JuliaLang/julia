# Building Julia on ARM

Julia has been compiled on several ARMv7 / Cortex A15 Samsung
Chromebooks running Ubuntu Linux under Crouton, Raspberry Pi systems
and Odroid boards. This is a work in progress - several tests are
known to fail, and backtraces are not available.

Julia on ARM can be built by simply typing `make`, which will download all
the relevant libraries. This is the *recommended* way, and it will take a
few hours.

If you get SIGILL during sysimg.o creation, it is likely that your cpu does not support VFP.
File an issue on the Julia issue tracker with the contents of /proc/cpuinfo.

This is the list of known issues on ARM:
 [https://github.com/JuliaLang/julia/labels/arm](https://github.com/JuliaLang/julia/labels/arm)

# Build dependencies

We recommend using at least Ubuntu 14.04 and gcc 4.8, which is part of the
standard `build-essentials`.

One can install other system libraries instead of building them,
should the build be troublesome, by adding the following lines in `Make.user`:

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
sudo apt-get install libblas3gf liblapack3gf libfftw3-dev libgmp3-dev libmpfr-dev libblas-dev liblapack-dev cmake gcc-4.8 g++-4.8 gfortran libgfortran3 m4 libedit-dev
````

Note that OpenBLAS only supports ARMv7. For older ARM variants, using the reference BLAS
may be the simplest thing to do.

# ARM specific build problems

If you run into issues building LLVM, see these notes:
[http://llvm.org/docs/HowToBuildOnARM.html](http://llvm.org/docs/HowToBuildOnARM.html)

# Raspberry Pi

The Raspberry Pi ARM CPU type is not detected by LLVM.
Before starting the build, it is recommended to do `export JULIA_CPU_TARGET=arm1176jzf-s`
at the shell to tune the generated code for your CPU architecture.

# Raspberry Pi 2

In the case of Raspberry Pi 2, in case the default build fails, download LLVM binaries from the LLVM website.

1.  Download the [LLVM 3.6.1 binaries for ARMv7a] (http://llvm.org/releases/3.6.1/clang+llvm-3.6.1-armv7a-linux-gnueabihf.tar.xz) and extract them in a local directory.
2.  For each file in the extracted `bin`, `include`, and `lib` subdirectories, create symlinks from the corresponding directory under `/usr/local`.
3. Add the following to `Make.user`:
```
override USE_SYSTEM_LLVM=1
```

# Chromebook

On Chromebooks, you have to first install Crouton.  If you do not have
an Ubuntu chroot running on your Chromebook using Crouton, you can do
so by following these tutorials.

- [Crouton Tutorial 1](http://www.howtogeek.com/162120/how-to-install-ubuntu-linux-on-your-chromebook-with-crouton/)
- [Crouton Tutorial 2](http://lifehacker.com/how-to-install-linux-on-a-chromebook-and-unlock-its-ful-509039343)

These tutorials will end up installing Ubuntu 12.04, and you have to
upgrade to Ubuntu 14.04, or install Ubuntu 14.04 from scratch by
finding appropriate `crouton` help.

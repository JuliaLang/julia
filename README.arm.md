# Building Julia on ARM

Julia has been compiled on several ARMv7 / Cortex A15 Samsung
Chromebooks running Ubuntu Linux under Crouton. This is a work in
progress - several tests are known to fail, and backtraces are not
available.

In addition to the standard `build-essentials` toolchain the following
libraries must be installed to build on ARM. On Debian/Ubuntu, use the
following command:

````
sudo apt-get install libblas3gf liblapack3gf libfftw3-dev libgmp3-dev libmpfr-dev libblas-dev liblapack-dev cmake gcc-4.7 g++-4.7 gfortran libgfortran3
````

Please start from the standard [build
instructions](README.md#source-download-and-compilation), in
particular the Linux notes.

Next, create a file in the `julia` top-level directory called
`Make.user` with the following contents:

```
include $(JULIAHOME)/Make.arm
```

Then proceed to build as described in the primary README. Just typing
`make -j 4` at this stage should build julia.

# Installing Crouton for Chromebooks

If you do not have an Ubuntu chroot running on your Chromebook using
Crouton, you can do so by following these tutorials.

- [Crouton Tutorial 1](http://www.howtogeek.com/162120/how-to-install-ubuntu-linux-on-your-chromebook-with-crouton/)
- [Crouton Tutorial 2](http://lifehacker.com/how-to-install-linux-on-a-chromebook-and-unlock-its-ful-509039343)

# Known ARM issues

- This is the list of known issues on ARM: [https://github.com/JuliaLang/julia/labels/arm](https://github.com/JuliaLang/julia/labels/arm)

# Building LLVM on ARM

- If you run in to issues building LLVM, see these notes: [http://llvm.org/docs/HowToBuildOnARM.html](http://llvm.org/docs/HowToBuildOnARM.html)

# For the Raspberry Pi version 2 (RPi2)

Success in building Julia has been reported using the following procedure:

1.  Follow the steps above to install the `build-essentals` and other packages required for ARM
2.  Install FFTW and libedit using `sudo apt-get install libfftw3-3 libedit-dev`
3.  Download the [LLVM 3.6.0 binaries for ARMv7a] (http://llvm.org/releases/3.6.0/clang+llvm-3.6.0-armv7a-linux-gnueabihf.tar.xz) and extract them in a local directory.
4.  For each file in the extracted `bin`, `include`, and `lib` subdirectories, create symlinks from the corresponding directory under `/usr/local`.
5.  Using the `Make.arm` configuration below, build Julia using `make -j 2`. (Limiting `make` to 2 simultaneous jobs will reduce the possibility of running out of memory/swap space during the build process.)

Note that the resulting Julia binary may fail various tests but will run code and provide the REPL.

`Make.arm` for RPi2:

```
override LLVM_ASSERTIONS=1
LLVM_FLAGS+="--with-cpu=cortex-a9 --with-float=hard --with-abi=aapcs-vfp --with-fpu=neon --enable-targets=arm --enable-optimized --enable-assertions"

override OPENBLAS_DYNAMIC_ARCH=0
override OPENBLAS_TARGET_ARCH=ARMV7
override USE_BLAS64=0

override USE_SYSTEM_LIBM=1
override USE_SYSTEM_FFTW=1
override USE_SYSTEM_GMP=1
override USE_SYSTEM_MPFR=1
override USE_SYSTEM_LAPACK=1

JCFLAGS += -fsigned-char

override USE_SYSTEM_LLVM=1
override USE_SYSTEM_BLAS=1
```


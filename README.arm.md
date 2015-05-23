# Building Julia on ARM

Julia has been compiled on several ARMv7 / Cortex A15 Samsung
Chromebooks running Ubuntu Linux under Crouton, Raspberry Pi systems
and Odroid boards. This is a work in progress - several tests are
known to fail, and backtraces are not available.

Most of the build failures in building the Julia system image are due
to LLVM not being able to detect the correct ARM processor type and
features. Experimenting with different `JULIA_CPU_ARCH` settings can
help in such cases. For example, this is needed on Raspberry Pi, as
discussed below.

This is the list of known issues on ARM:
 [https://github.com/JuliaLang/julia/labels/arm](https://github.com/JuliaLang/julia/labels/arm)

# Build dependencies

In addition to the standard `build-essentials` toolchain the following
libraries must be installed to build on ARM. On Debian/Ubuntu, use the
following command:

````
sudo apt-get install libblas3gf liblapack3gf libfftw3-dev libgmp3-dev libmpfr-dev libblas-dev liblapack-dev cmake gcc-4.7 g++-4.7 gfortran libgfortran3
````

# ARM specific build problems

If you run into issues building LLVM, see these notes:
[http://llvm.org/docs/HowToBuildOnARM.html](http://llvm.org/docs/HowToBuildOnARM.html)

# Raspberry Pi

The Raspberry Pi ARM CPU is not correctly detected by LLVM. Before
starting the build, `export JULIA_CPU_ARCH=arm1176jzf-s`. This tells
LLVM that the CPU has VFP support. See the discussion in
[#10917](https://github.com/JuliaLang/julia/issues/10917).

# Chromebook

On Chromebooks, you have to first install Crouton.  If you do not have
an Ubuntu chroot running on your Chromebook using Crouton, you can do
so by following these tutorials.

- [Crouton Tutorial 1](http://www.howtogeek.com/162120/how-to-install-ubuntu-linux-on-your-chromebook-with-crouton/)
- [Crouton Tutorial 2](http://lifehacker.com/how-to-install-linux-on-a-chromebook-and-unlock-its-ful-509039343)

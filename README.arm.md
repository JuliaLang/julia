# Building Julia on ARM

Julia has been compiled on several ARMv7 / Cortex A15 Samsung
Chromebooks running Ubuntu Linux under Crouton. This is a work in
progress - several tests are known to fail, and backtraces are not
available.

In addition to the standard `build-essentials` toolchain the following
libraries must be installed to build on ARM:

- libblas3gf, liblapack3gf, libfftw3-dev, libgmp3-dev, libmpfr-dev, libblas-dev, liblapack-dev

Please start from the standard [build
instructions](README.md#source-download-and-compilation), in
particular the Linux notes.

Next, create a file in the `julia` top-level directory called
`Make.user` with the following contents:

```
include $(JULIAHOME)/ARM.inc
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

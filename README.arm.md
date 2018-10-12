# Julia on ARM (Linux)

Julia fully supports ARMv8 (AArch64) processors, and supports ARMv7 and ARMv6
(AArch32) with some caveats. This file provides general guidelines for compilation,
in addition to instructions for specific devices.

A list of [known issues](https://github.com/JuliaLang/julia/labels/arm) for ARM is
available. If you encounter difficulties, please create an issue including the output
from `cat /proc/cpuinfo`.


## Building Julia

In most cases, Julia can be successfully built by following the platform independent
[build instructions](https://github.com/JuliaLang/julia/blob/master/README.md).

### Build dependencies

We recommend using at least Ubuntu 14.04 and gcc 4.8, which is part of the
standard `build-essentials`.

In case the build is failing on one of the dependent libraries, one
can install various system libraries instead of building them, by
adding the following lines in `Make.user`:

````
override USE_SYSTEM_BLAS=1
override USE_SYSTEM_LAPACK=1
override USE_SYSTEM_LIBM=1
override USE_SYSTEM_GMP=1
override USE_SYSTEM_MPFR=1
````

The following command will install all the necessary libraries on Ubuntu:

````
sudo apt-get install libblas3gf liblapack3gf libgmp3-dev \
                     libmpfr-dev libblas-dev liblapack-dev cmake gcc-4.8 \
                     g++-4.8 gfortran libgfortran3 m4 libedit-dev
````

If you run into issues building LLVM, see [these notes](http://llvm.org/docs/HowToBuildOnARM.html).


## 32-bit (ARMv6, ARMv7)

Julia has been successfully compiled on several variants of the following ARMv6 & ARMv7 devices:

* ARMv7 / Cortex A15 Samsung Chromebooks running Ubuntu Linux under Crouton;
* [Raspberry Pi](https://www.raspberrypi.org).
* [Odroid](http://www.hardkernel.com/main/main.php).


Julia requires at least the `armv6` and `vfpv2` instruction sets. It's recommended to use  `armv7-a`.
`armv5` or soft float are not supported.

### Binaries

[Nightly builds](https://status.julialang.org/download/linux-arm) are
available for ARMv7-A.

### Device specific instructions

#### Raspberry Pi 1 / Raspberry Pi Zero

The type of ARM CPU used in the Raspberry Pi is not detected by LLVM. Explicitly set the
CPU target by adding the following to `Make.user`:

````
JULIA_CPU_TARGET=arm1176jzf-s
````

It is preferable to use various system provided dependencies on ARMv6 as described in
[Build Dependencies](#build-dependencies).

To complete the build, you may need to increase the swap file size. To do so, edit
`/etc/dphys-swapfile`, changing the line:

    CONF_SWAPSIZE=100

to:

    CONF_SWAPSIZE=512

before restarting the swapfile service:

    sudo /etc/init.d/dphys-swapfile stop
    sudo /etc/init.d/dphys-swapfile start

#### Raspberry Pi 2

The type of ARM CPU used in the Raspberry Pi 2 is not detected by LLVM. Explicitly set the
CPU target by adding the following to `Make.user`:

```JULIA_CPU_TARGET=cortex-a7```

Depending on the exact compiler and distribution, there might be a build failure
due to unsupported inline assembly. In that case, add `MARCH=armv7-a` to
`Make.user`.

If building LLVM fails, you can download binaries from the LLVM website:

1.  Download the [LLVM 3.9.0 binaries for ARMv7a] (http://llvm.org/releases/3.9.0/clang+llvm-3.9.0-armv7a-linux-gnueabihf.tar.xz) and extract them in a local directory.
2. Add the following to `Make.user` (adjusting the path to the `llvm-config` binary):

    ```
    override USE_SYSTEM_LLVM=1
    LLVM_CONFIG=${EXTRACTED_LOCATION}/bin/llvm-config
    ```

Please do let us know if you had to download a pre-built LLVM in [#10235](https://github.com/JuliaLang/julia/issues/10235).

#### Chromebook

On Chromebooks, you have to first install Crouton. If you do not have
an Ubuntu chroot running on your Chromebook using Crouton, you can do
so by following these tutorials.

- [Crouton Tutorial 1](http://www.howtogeek.com/162120/how-to-install-ubuntu-linux-on-your-chromebook-with-crouton/)
- [Crouton Tutorial 2](http://lifehacker.com/how-to-install-linux-on-a-chromebook-and-unlock-its-ful-509039343)

These tutorials will end up installing Ubuntu 12.04, and you have to
upgrade to Ubuntu 14.04, or install Ubuntu 14.04 from scratch by
finding appropriate `crouton` help.

#### Scaleway cloud hosted ARM servers

On the current [Scaleway](http://scaleway.com) ARM servers, the Julia
build works out of the box.

## AArch64 (ARMv8)

Julia has been successfully built on the following ARMv8 devices:

* [nVidia Jetson TX1 & TX2](http://www.nvidia.com/object/embedded-systems-dev-kits-modules.html);
* [X-Gene 1](https://www.apm.com/products/data-center/x-gene-family/x-gene/);
* [Overdrive 3000](https://softiron.com/products/overdrive-3000/);
* [Cavium ThunderX](http://www.cavium.com/ThunderX_ARM_Processors.html) on [packet.net](https://www.packet.net).

Compilation on `ARMv8-A` requires that `Make.user` is configured as follows:

```
MARCH=armv8-a
```

### Device specific instructions

#### nVidia Jetson TX2

Julia builds and runs on the [nVidia Jetson TX2](http://www.nvidia.com/object/embedded-systems-dev-kits-modules.html)
platform with minimal configuration changes.

After configuring `Make.user` as per the `AArch64` instructions in this document,
follow the general [build instructions](https://github.com/JuliaLang/julia/blob/master/README.md).
The majority of the build dependencies specified in the instructions are installed by
the default configuration flashed by [Jetpack 3.0](https://developer.nvidia.com/embedded/jetpack). The remaining tools can be installed by issuing the following command:

```
sudo apt-get install gfortran wget cmake
```

A full parallel build, including LLVM,
will complete in around two hours. All tests pass and CUDA functionality is available
through, e.g., [CUDAdrv](https://github.com/JuliaGPU/CUDAdrv.jl).

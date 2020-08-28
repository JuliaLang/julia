# Building on ARM (Linux)

Julia fully supports ARMv8 (AArch64) processors, and supports ARMv7 and ARMv6
(AArch32) with some caveats. This file provides general guidelines for compilation,
in addition to instructions for specific devices.

A list of [known issues](https://github.com/JuliaLang/julia/labels/arm) for ARM is
available. If you encounter difficulties, please create an issue including the output
from `cat /proc/cpuinfo`.

## 32-bit (ARMv6, ARMv7)

Julia has been successfully compiled on several variants of the following ARMv6 & ARMv7 devices:

* ARMv7 / Cortex A15 Samsung Chromebooks running Ubuntu Linux under Crouton;
* [Raspberry Pi](https://www.raspberrypi.org).
* [Odroid](https://www.hardkernel.com).

Julia requires at least the `armv6` and `vfpv2` instruction sets. It's recommended to use  `armv7-a`.
`armv5` or soft float are not supported.

### Raspberry Pi 1 / Raspberry Pi Zero

If the type of ARM CPU used in the Raspberry Pi is not detected by LLVM, then explicitly set the
CPU target by adding the following to `Make.user`:

````
JULIA_CPU_TARGET=arm1176jzf-s
````

To complete the build, you may need to increase the swap file size. To do so, edit
`/etc/dphys-swapfile`, changing the line:

    CONF_SWAPSIZE=100

to:

    CONF_SWAPSIZE=512

before restarting the swapfile service:

    sudo /etc/init.d/dphys-swapfile stop
    sudo /etc/init.d/dphys-swapfile start

### Raspberry Pi 2

The type of ARM CPU used in the Raspberry Pi 2 is not detected by LLVM. Explicitly set the
CPU target by adding the following to `Make.user`:

```JULIA_CPU_TARGET=cortex-a7```

Depending on the exact compiler and distribution, there might be a build failure
due to unsupported inline assembly. In that case, add `MCPU=armv7-a` to
`Make.user`.

## AArch64 (ARMv8)

Julia has been successfully built on the following ARMv8 devices:

* [nVidia Jetson TX1 & TX2](https://www.nvidia.com/object/embedded-systems-dev-kits-modules.html);
* [X-Gene 1](https://www.apm.com/products/data-center/x-gene-family/x-gene/);
* [Overdrive 3000](https://softiron.com/products/overdrive-3000/);
* [Cavium ThunderX](https://www.cavium.com/ThunderX_ARM_Processors.html) on [packet.net](https://www.packet.net).

Compilation on `ARMv8-A` requires that `Make.user` is configured as follows:

```
MCPU=armv8-a
```

### nVidia Jetson TX2

Julia builds and runs on the [nVidia Jetson TX2](https://www.nvidia.com/object/embedded-systems-dev-kits-modules.html)
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

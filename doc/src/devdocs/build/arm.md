# ARM (Linux)

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

Julia is expected to work and build on ARMv8 cpus. One should follow the general [build instructions](https://github.com/JuliaLang/julia/blob/master/README.md). Julia expects to have around 8GB of ram or swap enabled to build itself.

### Known issues

Starting from Julia v1.10, [JITLink](https://llvm.org/docs/JITLink.html) is automatically enabled on this architecture for all operating systems when linking to LLVM 15 or later versions.
Due to a [bug in LLVM memory manager](https://github.com/llvm/llvm-project/issues/63236), non-trivial workloads may generate too many memory mappings that on Linux can exceed the limit of memory mappings (`mmap`) set in the file `/proc/sys/vm/max_map_count`, resulting in an error like
```
JIT session error: Cannot allocate memory
```
Should this happen, ask your system administrator to increase the limit of memory mappings for example with the command
```
sysctl -w vm.max_map_count=262144
```

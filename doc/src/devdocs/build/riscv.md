# RISC-V (Linux)

Julia has experimental support for 64-bit RISC-V (RV64) processors running
Linux. This file provides general guidelines for compilation, in addition to
instructions for specific devices.

A list of [known issues](https://github.com/JuliaLang/julia/labels/system:riscv)
for RISC-V is available. If you encounter difficulties, please create an issue
including the output from `cat /proc/cpuinfo`.


## Compiling Julia

For now, Julia will need to be compiled entirely from source, i.e., including
all of its dependencies. This can be accomplished with the following
`Make.user`:

```make
USE_BINARYBUILDER := 0
```

Additionally, it is required to indicate what architecture, and optionally which
CPU to build for. This can be done by setting the `MARCH` and `MCPU` variables
in `Make.user`

The `MARCH` variable needs to be set to a RISC-V ISA string, which can be found by
looking at the documentation of your device, or by inspecting `/proc/cpuinfo`. Only
use flags that your compiler supports, e.g., run `gcc -march=help` to see a list of
supported flags. A common value is `rv64gc`, which is a good starting point.

The `MCPU` variable is optional, and can be used to further optimize the
generated code for a specific CPU. If you are unsure, it is recommended to leave
it unset. You can find a list of supported values by running `gcc --target-help`.

For example, if you are using a StarFive VisionFive2, which contains a JH7110
processor based on the SiFive U74, you can set these flags as follows:

```make
MARCH := rv64gc_zba_zbb
MCPU := sifive-u74
```

If you prefer a portable build, you could use:

```make
MARCH := rv64gc

# also set JULIA_CPU_TARGET to the expanded form of rv64gc
# (it normally copies the value of MCPU, which we don't set)
JULIA_CPU_TARGET := generic-rv64,i,m,a,f,d,zicsr,zifencei,c
```

### Cross-compilation

A native build on a RISC-V device may take a very long time, so it's also
possible to cross-compile Julia on a faster machine.

First, get a hold of a RISC-V cross-compilation toolchain that provides
support for C, C++ and Fortran. This can be done by checking-out the
[riscv-gnu-toolchain](https://github.com/riscv-collab/riscv-gnu-toolchain)
repository and building it as follows:

```sh
sudo mkdir /opt/riscv && sudo chown $USER /opt/riscv
./configure --prefix=/opt/riscv --with-languages=c,c++,fortran
make linux -j$(nproc)
```

Then, install the QEMU user-mode emulator for RISC-V, along with `binfmt`
support to enable execution of RISC-V binaries on the host machine. The
exact steps depend on your distribution, e.g., on Arch Linux it involves
installing the `qemu-user-static` and `qemu-user-static-binfmt` packages.
Note that to actually execute RISC-V binaries, QEMU will need to be able to
find the RISC-V system root, which can be achieved by setting the
`QEMU_LD_PREFIX` environment variable to the path of the root filesystem.

Finally, compile Julia with the following `Make.user` variables (in addition to
the ones from the previous section):

```make
XC_HOST=riscv64-unknown-linux-gnu
OS=Linux
export QEMU_LD_PREFIX=/opt/riscv/sysroot
```

Note that you will have to execute `make` with `PATH` set to include the
cross-compilation toolchain, e.g., by running:

```sh
PATH=/opt/riscv/bin:$PATH make -j$(nproc)
```

Because of the RISC-V sysroot we use being very barren, you may need to
add additional libraries that the Julia build system currently expects
to be available system-wide. For example, the build currently relies on
a system-provided `libz`, so you may need to copy this library from the
Julia build into the system root:

```sh
make -C deps install-zlib
cp -v usr/lib/libz.*   /opt/riscv/sysroot/usr/lib
cp -v usr/include/z*.h /opt/riscv/sysroot/usr/include
```

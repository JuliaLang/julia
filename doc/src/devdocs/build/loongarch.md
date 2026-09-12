# LOONGARCH (Linux)

Julia has experimental support for 64-bit RISC-V (RV64) processors running
Linux. This file provides general guidelines for compilation, in addition to
instructions for specific devices.

A list of [known issues](https://github.com/JuliaLang/julia/labels/system:loongarch)
for LOONGARCH is available. If you encounter difficulties, please create an issue
including the output from `cat /proc/cpuinfo`.


## Compiling Julia

To compile Julia for LOONGARCH, you need to manually indicate what architecture, and
optionally which CPU to build for. This can be done by setting the `MARCH` and `MCPU`
variables in `Make.user`

The `MARCH` variable needs to be set to a LOONGARCH ISA string, which can be found by
looking at the documentation of your device, or by inspecting `/proc/cpuinfo`. Only
use flags that your compiler supports, e.g., run `gcc -march=help` to see a list of
supported flags. A common value is `loongarch64`, which is a good starting point.

Build Julia directly with the following command:
```
make USE_BINARYBUILDER=0 -j$(nproc)
```

# Building on Linux

* GCC version 4.7 or later is required to build Julia.
* To use external shared libraries not in the system library search path, set `USE_SYSTEM_XXX=1` and `LDFLAGS=-Wl,-rpath,/path/to/dir/contains/libXXX.so` in `Make.user`.
* Instead of setting `LDFLAGS`, putting the library directory into the environment variable `LD_LIBRARY_PATH` (at both compile and run time) also works.
* The `USE_SYSTEM_*` flags should be used with caution. These are meant only for troubleshooting, porting, and packaging, where package maintainers work closely with the Julia developers to make sure that Julia is built correctly. Production use cases should use the officially provided binaries. Issues arising from the use of these flags will generally not be accepted.
* See also the [external dependencies](#required-build-tools-and-external-libraries).

## Architecture Customization

Julia can be built for a non-generic architecture by configuring the `ARCH` Makefile variable in a `Make.user` file. See the appropriate section of `Make.inc` for additional customization options, such as `MARCH` and `JULIA_CPU_TARGET`.

For example, to build for Pentium 4, set `MARCH=pentium4` and install the necessary system libraries for linking. On Ubuntu, these may include lib32gfortran-6-dev, lib32gcc1, and lib32stdc++6, among others.

You can also set `MARCH=native` in `Make.user` for a maximum-performance build customized for the current machine CPU.

## Linux Build Troubleshooting

### Problem: OpenBLAS build failure

Set one of the following build options in `Make.user` and build again:
- `OPENBLAS_TARGET_ARCH=BARCELONA` (AMD CPUs) or `OPENBLAS_TARGET_ARCH=NEHALEM` (Intel CPUs)
    - Set `OPENBLAS_DYNAMIC_ARCH = 0` to disable compiling multiple architectures in a single binary.
- `OPENBLAS_NO_AVX2 = 1` disables AVX2 instructions, allowing OpenBLAS to compile with `OPENBLAS_DYNAMIC_ARCH = 1` using old versions of binutils
- `USE_SYSTEM_BLAS=1` uses the system provided `libblas`
    - Set `LIBBLAS=-lopenblas` and `LIBBLASNAME=libopenblas` to force the use of the system provided OpenBLAS when multiple BLAS versions are installed.

If you get an error that looks like ```../kernel/x86_64/dgemm_kernel_4x4_haswell.S:1709: Error: no such instruction: `vpermpd $ 0xb1,%ymm0,%ymm0'```, then you need to set `OPENBLAS_DYNAMIC_ARCH = 0` or `OPENBLAS_NO_AVX2 = 1`, or you need a newer version of `binutils` (2.18 or newer). ([Issue #7653](https://github.com/JuliaLang/julia/issues/7653))

If the linker cannot find `gfortran` and you get an error like `julia /usr/bin/x86_64-linux-gnu-ld: cannot find -lgfortran`, check the path with `gfortran -print-file-name=libgfortran.so` and use the output to export something similar to this: `export LDFLAGS=-L/usr/lib/gcc/x86_64-linux-gnu/8/`. See [Issue #6150](https://github.com/JuliaLang/julia/issues/6150#issuecomment-37546803).

### Problem: Illegal Instruction error

Check if your CPU supports AVX while your OS does not (e.g. through virtualization, as described in [this issue](https://github.com/JuliaLang/julia/issues/3263)).

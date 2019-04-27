<a name="logo"/>
<div align="center">
<a href="https://julialang.org/" target="_blank">
<img src="https://julialang.org/images/logo_hires.png" alt="Julia Logo" width="210" height="142"></img>
</a>
</div>

## Downloading the Julia source code

(If you are behind a firewall, you may need to use the `https` protocol instead of the `git` protocol:
```sh
git config --global url."https://".insteadOf git://
```
Be sure to also configure your system to use the appropriate proxy settings, e.g. by setting the `https_proxy` and `http_proxy` variables.)

## Building Julia

When compiled the first time, the build will automatically download pre-built [external dependencies](#required-build-tools-and-external-libraries). If you prefer to build all the dependencies on your own, add the following in `Make.user`
```
USE_BINARYBUILDER=0
```

Building Julia requires 5GiB if building all dependencies and approximately 4GiB of virtual memory.

To perform a parallel build, use `make -j N` and supply the maximum number of concurrent processes. (See [Platform Specific Build Notes](https://github.com/JuliaLang/julia#platform-specific-build-notes) for details.) If the defaults in the build do not work for you, and you need to set specific make parameters, you can save them in `Make.user`, and place the file in the root of your Julia source. The build will automatically check for the existence of `Make.user` and use it if it exists.

You can create out-of-tree builds of Julia by specifying `make O=<build-directory> configure` on the command line. This will create a directory mirror, with all of the necessary Makefiles to build Julia, in the specified directory. These builds will share the source files in Julia and `deps/srccache`. Each out-of-tree build directory can have its own `Make.user` file to override the global `Make.user` file in the top-level folder.

If you need to build Julia on a machine without internet access, use `make -C deps getall` to download all the necessary files. Then, copy the `julia` directory over to the target environment and build with `make`.

To run julia from anywhere you can:
- add an alias (in `bash`: `echo "alias julia='/path/to/install/folder/bin/julia'" >> ~/.bashrc && source ~/.bashrc`), or

- add a soft link to the `julia` executable in the `julia` directory to `/usr/local/bin` (or any suitable directory already in your path), or

- add the `julia` directory to your executable path for this shell session (in `bash`: `export PATH="$(pwd):$PATH"` ; in `csh` or `tcsh`:
`set path= ( $path $cwd )` ), or

- add the `julia` directory to your executable path permanently (e.g. in `.bash_profile`), or

- write `prefix=/path/to/install/folder` into `Make.user` and then run `make install`. If there is a version of Julia already installed in this folder, you should delete it before running `make install`.

Now you should be able to run Julia like this:

    julia

If you are building a Julia package for distribution on Linux, macOS,
or Windows, take a look at the detailed notes in
[DISTRIBUTING.md](https://github.com/JuliaLang/julia/blob/master/DISTRIBUTING.md).

### Updating an existing source tree

If you have previously downloaded `julia` using `git clone`, you can update the
existing source tree using `git pull` rather than starting anew:
```sh
cd julia
git pull && make
```
Assuming that you had made no changes to the source tree that will conflict
with upstream updates, these commands will trigger a build to update to the
latest version.

#### General troubleshooting

1. Over time, the base library may accumulate enough changes such that the
   bootstrapping process in building the system image will fail. If this
   happens, the build may fail with an error like

   ```sh
    *** This error is usually fixed by running 'make clean'. If the error persists, try 'make cleanall' ***
   ```

   As described, running `make clean && make` is usually sufficient.
   Occasionally, the stronger cleanup done by `make cleanall` is needed.

2. New versions of external dependencies may be introduced which may
   occasionally cause conflicts with existing builds of older versions.

   a. Special `make` targets exist to help wipe the existing build of a
      dependency. For example, `make -C deps clean-llvm` will clean out the
      existing build of `llvm` so that `llvm` will be rebuilt from the
      downloaded source distribution the next time `make` is called.
      `make -C deps distclean-llvm` is a stronger wipe which will also delete
      the downloaded source distribution, ensuring that a fresh copy of the
      source distribution will be downloaded and that any new patches will be
      applied the next time `make` is called.

   b. To delete existing binaries of `julia` and all its dependencies,
      delete the `./usr` directory _in the source tree_.

3. If you've updated macOS recently, be sure to run `xcode-select --install` to update the command line tools.
   Otherwise, you could run into errors for missing headers and libraries, such as
   ```ld: library not found for -lcrt1.10.6.o```.

4. If you've moved the source directory, you might get errors such as
    ```CMake Error: The current CMakeCache.txt directory ... is different than the directory ... where     CMakeCache.txt was created.```, in which case you may delete the offending dependency under `deps`

5. In extreme cases, you may wish to reset the source tree to a pristine state.
   The following git commands may be helpful:

   ```sh
    git reset --hard #Forcibly remove any changes to any files under version control
    git clean -x -f -d #Forcibly remove any file or directory not under version control
   ```

   _To avoid losing work, make sure you know what these commands do before you
   run them. `git` will not be able to undo these changes!_

## Platform-Specific Build Notes

### Linux

* GCC version 4.7 or later is required to build Julia.
* To use external shared libraries not in the system library search path, set `USE_SYSTEM_XXX=1` and `LDFLAGS=-Wl,-rpath,/path/to/dir/contains/libXXX.so` in `Make.user`.
  * Instead of setting `LDFLAGS`, putting the library directory into the environment variable `LD_LIBRARY_PATH` (at both compile and run time) also works.
* The `USE_SYSTEM_*` flags should be used with caution. These are meant only for troubleshooting, porting, and packaging, where package maintainers work closely with the Julia developers to make sure that Julia is built correctly. Production use cases should use the officially provided binaries. Issues arising from the use of these flags will generally not be accepted.
* See also the [external dependencies](#required-build-tools-and-external-libraries).

### ARM (Linux)

See [README.arm](https://github.com/JuliaLang/julia/blob/master/doc/README.arm.md) for building on ARM processors.

#### Architecture Customization

Julia can be built for a non-generic architecture by configuring the `ARCH` Makefile variable. See the appropriate section of `Make.inc` for additional customization options, such as `MARCH` and `JULIA_CPU_TARGET`.

For example, to build for Pentium 4, set `MARCH=pentium4` and install the necessary system libraries for linking. On Ubuntu, these may include lib32gfortran-6-dev, lib32gcc1, and lib32stdc++6, among others.

You can also set `MARCH=native` for a maximum-performance build customized for the current machine CPU.

#### Linux Build Troubleshooting

 Problem              | Possible Solution
------------------------|---------------------
 OpenBLAS build failure | Set one of the following build options in `Make.user` and build again: <ul><li> `OPENBLAS_TARGET_ARCH=BARCELONA` (AMD CPUs) or `OPENBLAS_TARGET_ARCH=NEHALEM` (Intel CPUs)<ul>Set `OPENBLAS_DYNAMIC_ARCH = 0` to disable compiling multiple architectures in a single binary.</ul></li><li> `OPENBLAS_NO_AVX2 = 1` disables AVX2 instructions, allowing OpenBLAS to compile with `OPENBLAS_DYNAMIC_ARCH = 1` using old versions of binutils </li><li> `USE_SYSTEM_BLAS=1` uses the system provided `libblas` <ul><li>Set `LIBBLAS=-lopenblas` and `LIBBLASNAME=libopenblas` to force the use of the system provided OpenBLAS when multiple BLAS versions are installed. </li></ul></li></ul><p> If you get an error that looks like ```../kernel/x86_64/dgemm_kernel_4x4_haswell.S:1709: Error: no such instruction: `vpermpd $ 0xb1,%ymm0,%ymm0'```, then you need to set `OPENBLAS_DYNAMIC_ARCH = 0` or `OPENBLAS_NO_AVX2 = 1`, or you need a newer version of `binutils` (2.18 or newer). ([Issue #7653](https://github.com/JuliaLang/julia/issues/7653))</p><p> If the linker cannot find `gfortran` and you get an error like `julia /usr/bin/x86_64-linux-gnu-ld: cannot find -lgfortran`, check the path with `gfortran -print-file-name=libgfortran.so` and use the output to export something similar to this: `export LDFLAGS=-L/usr/lib/gcc/x86_64-linux-gnu/8/`. See [Issue #6150](https://github.com/JuliaLang/julia/issues/6150#issuecomment-37546803).</p>
Illegal Instruction error | Check if your CPU supports AVX while your OS does not (e.g. through virtualization, as described in [this issue](https://github.com/JuliaLang/julia/issues/3263)).

### macOS

You need to have the current Xcode command line utilities installed: run `xcode-select --install` in the terminal.
You will need to rerun this terminal command after each macOS update, otherwise you may run into errors involving missing libraries or headers.
You will also need a 64-bit gfortran to compile Julia dependencies. The gfortran-4.7 (and newer) compilers in Homebrew work for building Julia.

If you have set `LD_LIBRARY_PATH` or `DYLD_LIBRARY_PATH` in your `.bashrc` or equivalent, Julia may be unable to find various libraries that come bundled with it. These environment variables need to be unset for Julia to work.

### FreeBSD

Clang is the default compiler on FreeBSD 11.0-RELEASE and above.
The remaining build tools are available from the Ports Collection, and can be installed using
`pkg install git gcc gmake cmake pkgconf`.
To build Julia, simply run `gmake`.
(Note that `gmake` must be used rather than `make`, since `make` on FreeBSD corresponds to the incompatible BSD Make rather than GNU Make.)

As mentioned above, it is important to note that the `USE_SYSTEM_*` flags should be used with caution on FreeBSD.
This is because many system libraries, and even libraries from the Ports Collection, link to the system's `libgcc_s.so.1`,
or to another library which links to the system `libgcc_s`.
This library declares its GCC version to be 4.6, which is too old to build Julia, and conflicts with other libraries when linking.
Thus it is highly recommended to simply allow Julia to build all of its dependencies.
If you do choose to use the `USE_SYSTEM_*` flags, note that `/usr/local` is not on the compiler path by default, so you may need
to add `LDFLAGS=-L/usr/local/lib` and `CPPFLAGS=-I/usr/local/include` to your `Make.user`, though doing so may interfere with
other dependencies.

Note that the x86 architecture does not support threading due to lack of compiler runtime library support, so you may need to
set `JULIA_THREADS=0` in your `Make.user` if you're on a 32-bit system.

### Windows

In order to build Julia on Windows, see [README.windows](https://github.com/JuliaLang/julia/blob/master/doc/README.windows.md).

### Notes for distribution package maintainers

We understand that package maintainers will typically want to make use of system libraries where possible. Please refer to the above version requirements and additional notes below. It is strongly advised that package maintainers apply the patches included in the Julia repo for LLVM and other libraries, should they choose to use SYSTEM versions. A list of maintained Julia packages for various platforms is available at https://julialang.org/downloads/platform.html.

### System Provided Libraries

If you already have one or more of these packages installed on your system, you can prevent Julia from compiling duplicates of these libraries by passing `USE_SYSTEM_...=1` to `make` or adding the line to `Make.user`. The complete list of possible flags can be found in `Make.inc`.

Please be aware that this procedure is not officially supported, as it introduces additional variability into the installation and versioning of the dependencies, and is recommended only for system package maintainers. Unexpected compile errors may result, as the build system will do no further checking to ensure the proper packages are installed.

### LLVM

The most complicated dependency is LLVM, for which we require additional patches from upstream (LLVM is not backward compatible).

For packaging Julia with LLVM, we recommend either:
 - bundling a Julia-only LLVM library inside the Julia package, or
 - adding the patches to the LLVM package of the distribution.
   * A complete list of patches is available in `deps/llvm.mk`, and the patches themselves are in `deps/patches/`.
   * The only Julia-specific patch is the lib renaming (`llvm-symver-jlprefix.patch`), which should _not_ be applied to a system LLVM.
   * The remaining patches are all upstream bug fixes, and have been contributed into upstream LLVM.

Using an unpatched or different version of LLVM will result in errors and/or poor performance. Though Julia can be built with newer LLVM versions, support for this should be regarded as experimental and not suitable for packaging.

### libuv

Julia uses a custom fork of libuv. It is a small dependency, and can be safely bundled in the same package as Julia, and will not conflict with the system library. Julia builds should _not_ try to use the system libuv.

### BLAS and LAPACK

As a high-performance numerical language, Julia should be linked to a multi-threaded BLAS and LAPACK, such as OpenBLAS or ATLAS, which will provide much better performance than the reference `libblas` implementations which may be default on some systems.

### Intel MKL

For a 64-bit architecture, the environment should be set up as follows:
```sh
# bash
source /path/to/intel/bin/compilervars.sh intel64
```
Add the following to the `Make.user` file:

    USE_INTEL_MKL = 1

It is highly recommended to start with a fresh clone of the Julia repository.

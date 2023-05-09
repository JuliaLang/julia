# Package Images

Julia package images provide object (native code) caches for Julia packages.
They are similar to Julia's [system image](@ref dev-sysimg) and support many of the same features.
In fact the underlying serialization format is the same, and the system image is the base image that the package images are build against.

## High-level overview

Package images are shared libraries that contain both code and data. Like `.ji` cache files, they are generated per package. The data section contains both global data (global variables in the package) as well as the necessary metadata about what methods and types are defined by the package. The code section contains native objects that cache the final output of Julia's LLVM-based compiler.

The command line option `--pkgimages=no` can be used to turn off object caching for this session. Note that this means that cache files have to likely be regenerated.
See [`JULIA_MAX_NUM_PRECOMPILE_FILES`](@ref env-max-num-precompile-files) for the upper limit of variants Julia caches per default.

!!! note
    While the package images present themselves as native shared libraries, they are only an approximation thereof. You will not be able to link against them from a native program and they must be loaded from Julia.


## Linking

Since the package images contain native code, we must run a linker over them before we can use them. You can set the environment variable `JULIA_VERBOSE_LINKING` to `true` to make the package image linking process verbose.

Furthermore, we cannot assume that the user has a working system linker installed. Therefore, Julia ships with LLD, the LLVM linker, to provide a working out of the box experience. In `base/linking.jl`, we implement a limited interface to be able to link package images on all supported platforms.

### Quirks
Despite LLD being a multi-platform linker, it does not provide a consistent interface across platforms. Furthermore, it is meant to be used from `clang` or
another compiler driver, we therefore reimplement some of the logic from `llvm-project/clang/lib/Driver/ToolChains`. Thankfully one can use `lld -flavor` to set lld to the right platform

#### Windows
To avoid having to deal with `link.exe` we use `-flavor gnu`, effectively turning `lld` into a cross-linker from a mingw32 environment. Windows DLLs are required to contain a `_DllMainCRTStartup` function and to minimize our dependence on mingw32 libraries, we inject a stub definition ourselves.

#### MacOS
Dynamic libraries on macOS need to link against `-lSystem`. On recent macOS versions, `-lSystem` is only available for linking when Xcode is available.
To that effect we link with `-undefined dynamic_lookup`.

## Package images optimized for multiple microarchitectures
Similar to [multi-versioning](@ref sysimg-multi-versioning) for system images, package images support multi-versioning. If you are in a heterogenous environment, with a unified cache,
you can set the environment variable `JULIA_CPU_TARGET=generic` to multi-version the object caches.

## Flags that impact package image creation and selection

These are the Julia command line flags that impact cache selection. Package images
that were created with different flags will be rejected.

- `-g`, `--debug-info`: Exact match required since it changes code generation.
- `--check-bounds`: Exact match required since it changes code generation.
- `--inline`: Exact match required since it changes code generation.
- `--pkgimages`: To allow running without object caching enabled.
- `-O`, `--optimize`: Reject package images generated for a lower optimization level,
  but allow for higher optimization levels to be loaded.

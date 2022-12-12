# Package Images

Julia package images provide object (native code) caches for Julia packages.
They are similar to Julia's [system image](@) and support many of the same features.
In fact the underlying serialization format is the same, and the system image is the base image that the package images are build against.

## High-level overview

Package images are shared libraries that contain both code and data. Like `.ji` cache files they are generated per package. The data section contains both global data (global variables in the package) as well as the necessary metadata about what methods and types are defined by the package. The code section contains native objects that cache the final output of Julia's LLVM based compiler.

The command line option `--pkgimage-native-code=no` can be used to turn-off object caching for this session. Note that this means that cache files have to be regenerated.

!!! note
    While the package images present themselves as native shared libraries they are only an approximation thereof. You will not be able to link against them from a native program and they must be loaded from Julia.


## Linking

Since the package images partially contain native code, we must run a linker over them before we can use them. You can use the environment variable `JULIA_VERBOSE_LINKING` to make the pkgimage linking process verbose.

Furthermore we cannot assume that the user has a working system linker installed. Julia now comes with LLD, the LLVM linker, to provide a working out of the box experience. In `base/linking.jl` we implement a limited interface to be able to link package images on all supported platforms.

### Quirks
Despite LLD being a multi-platform linker it does not provide a consistent interface across platforms. Furthermore it is meant to be used from `clang` or
another compiler driver, we therefore reimplement some of the logic from `llvm-project/clang/lib/Driver/ToolChains`. Thankfully one can use `lld -flavor` to set lld to the right platform

#### Windows
To avoid having to deal with `link.exe` we use `-flavor gnu`, effectively turning `lld` into a cross-linker from a mingw32 environment. Windows DLLs are required to contain a `_DllMainCRTStartup` function and to minimize our dependenc on mingw32 libraries we inject a stub definition ourselves.

#### MacOS
Dynamic libraries on MacOS need to link against `-lSystem`. On recent MacOS versions `-lSystem` is only available for linking when Xcode is available.
To that effect we link with `-undefined dynamic_lookup`.

## Package images optimized for multiple microarchitectures
Similar to [multi-versioning](@ref sysimg-multi-versioning) for system images, package images support multi-versioning. If you are in a heterogenous environment, with a unified cache,
you can set the environment variable `JULIA_CPU_TARGET` to multi-version the object caches.

## Flags that impact pkgimage

These are the Julia command line flags that impact cache selection. Pkgimages
that were created with different flags will be rejected.

- `--debug-level`: Exact match required since it changes codegeneration.
- `--checkbounds`: Exact match required since it changes codegeneration.
- `--pkgimage-native-code`: To allow running without object caching enabled.
- `--optimize`: Reject pkgimages generated for a lower optimization level,
  but allow for higher optimization levels to be loaded.

# [Package Images](@id pkgimages)

Julia package images provide object (native code) caches for Julia packages.
They are similar to Julia's [system image](@ref dev-sysimg) and support many of the same features.
In fact the underlying serialization format is the same, and the system image is the base image that the package images are built against.

## High-level overview

A package image is one `.ji` cache file per package that contains both data and code. The data contains both global data (global variables in the package) as well as the necessary metadata about what methods and types are defined by the package. The code is native code that caches the final output of Julia's LLVM-based compiler.

The command line option `--pkgimages=no` can be used to turn off object caching for this session. Note that this means that cache files have to likely be regenerated.
See [`JULIA_MAX_NUM_PRECOMPILE_FILES`](@ref JULIA_MAX_NUM_PRECOMPILE_FILES) for the upper limit of variants Julia caches per default.

!!! note
    A package image is not a shared library of the system. A native program cannot link against it, and tools that learn about code from the dynamic loader do not see it by themselves. Set `ENABLE_GDBLISTENER=1` for a debugger and `ENABLE_JITPROFILING=1` for `perf`, as for code from the JIT.

## Linking and loading

The objects that the compiler produces for a package are linked in the precompile process itself, with LLVM's JITLink used as a static linker (`src/jinative-prelink.cpp`). The result is a section of the `.ji`, laid out the same way on every platform (`src/jinative.h`): code and read-only data that are mapped from the file as they are, data, and a list of the slots in the data that hold an address of the image or of a symbol that it imports.

Loading (`src/jinative-load.c`) maps the segments, adds the load address to the first kind of slot, stores the address of the import in the second, and tells the unwinder where the unwind tables are. The dynamic loader of the system is not involved, and no linker is needed where packages are precompiled. On macOS the section carries an ad-hoc code signature, which the kernel requires to map a file executable.

On architectures that the prelinker does not support (anything but x86-64, AArch64 and 32-bit x86 on ELF), package images are written without native code.

## [Package images optimized for multiple microarchitectures](@id pkgimgs-multi-versioning)

Similar to [multi-versioning](@ref sysimg-multi-versioning) for system images, package images support multi-versioning. This allows creating package caches that can run efficiently on different CPU architectures within the same environment.

See the [`JULIA_CPU_TARGET`](@ref JULIA_CPU_TARGET) environment variable for more information on how to set the CPU target for package images.

## Flags that impact package image creation and selection

These are the Julia command line flags that impact cache selection. Package images
that were created with different flags will be rejected.

- `-g`, `--debug-info`: Exact match required since it changes code generation.
- `--check-bounds`: Exact match required since it changes code generation.
- `--inline`: Exact match required since it changes code generation.
- `--pkgimages`: To allow running without object caching enabled.
- `-O`, `--optimize`: Reject package images generated for a lower optimization level,
  but allow for higher optimization levels to be loaded.
- `--code-coverage`, `--code-coverage-mode`: Whether the image carries coverage
  counters, and their mode, is recorded in the cache identity. A `count` image
  can serve `hit` mode. The coverage scope is not part of the identity: an
  instrumented image carries counters for every statement, and the loading
  process reports only those its scope selects (user code for `user`, files
  under the tracked path for `@<path>`).

Compatible coverage flags do not override dependency identity checks. Selecting
a different image for an out-of-scope dependency can require rebuilding its
dependents as well.

Coverage runs create instrumented package-image variants alongside ordinary
caches for every loaded package, independently of the requested scope. Thus
`--code-coverage=@<path>` reuses the same images as `user` and `all`. An initial
coverage run may need to precompile instrumented dependency images; subsequent
runs can reuse them across selectors.

Because `@<path>` only reports files under the tracked path, it can also use an
ordinary image that contains no code from there, such as a bundled standard
library image. An image only contains code from its own source files, from the
package images it was built against, and from the system image. The loader
therefore accepts an ordinary image when none of the package's source files are
tracked, when every package dependency was accepted the same way (the image pins
their build ids, so an instrumented dependency forces a rebuild of its
dependents), and when the tracked path does not overlap the system image's
sources. Code that names other files in its line information, for example
through `include_string`, is not accounted for. Neither are system image sources
of a relocated installation: the system image records the paths it was built
from, so tracking the installed standard library sources is not detected as an
overlap, but code from there is not reported under those paths either. With an
instrumented system image, ordinary images may have been built against
instrumented dependencies, so they are not used for coverage at all.

The tracked path is part of the requested `Base.CacheFlags`, though not of the
image's identity, so that a precompilation driver collecting no coverage itself,
like the one `Pkg.test(coverage=true)` runs, judges the caches as the test
process will. Precompilation workers inherit it to accept the same dependencies;
the images they produce are instrumented as usual, but switching between
`@<path>` and other selectors can rebuild dependents that were built against
different dependency images.
Instrumented package images also work with an ordinary system image. Reports
include zero counts for instrumented lines that were not executed; precompilation
workloads do not contribute hits.

Without coverage, ordinary builds reject instrumented package images. A build
with an instrumented system image also accepts package images with the same
instrumentation as that system image. Allocation tracking always requires
recompilation because images do not carry allocation counters.

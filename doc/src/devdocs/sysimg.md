# Julia images

In the tradition of Lisp-like programming languages, Julia can dump the state of
the running process to an **image**, which can be loaded in a fresh process to
avoid re-evaluating all of the code in the compiler and standard library.  Julia
images can contain a dump of the heap, and optionally native code.  When started
without the `-J` or one of the `--output-*` command line arguments, Julia will
load the built-in system image, found in `lib/julia/sys.{so,dylib,dll}`.

It is also possible to create an **incremental image**, which is an image that
can be loaded on top of a running Julia process.  This is used to speed up
loading packages.

Pure heap images, without any native code, are `.ji` files.  Images with native
code always contain a heap image, which is either embedded in the dynamic
library, or is in a sidecar `.ji` file (a **split image**).

Users who wish to create customized system images to speed up starting Julia
with a large set of packages should see the [`PackageCompiler.jl`
package](https://github.com/JuliaLang/PackageCompiler.jl).

## Glossary

| Term                         | Definition                                                                                                                                                                                      |
|:-----------------------------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Image                        | Umbrella term for all Julia images: a single `.ji` file, a dynamic library, or a split `.ji` and dynamic library. Includes a heap image and, optionally, native code.                           |
| Heap image                   | The part of a Julia image containing the serialized heap written by `jl_save_system_image_to_stream`.                                                                                           |
| Non-incremental/system image | A standalone image containing a complete heap. Julia must be started with one (outside of bootstrapping): the `-J` option selects it, and a default path is searched.                           |
| Incremental image            | An image designed to be loaded into a process that already has a Julia heap. It contains relocations that are fixed up to refer to objects in the existing heap, and speeds up package loading. |
| Native image                 | An image containing native code in addition to the heap image.                                                                                                                                  |
| Split image                  | A native image whose heap image is in a sidecar `.ji` file instead of embedded in the dynamic library.                                                                                          |
| Package image                | Usually any incremental image (e.g. `jl_restore_package_image_from_stream`), but the `--pkgimages` command line flag refers only to incremental *native* images.                                |

## Coverage instrumentation

Set `JULIA_COVERAGE_IMAGES=1` in `Make.user` before building Julia to compile
`hit`-mode coverage counters into the system image and the bundled package
images. Coverage runs in `hit` mode then reuse the native code instead of
recompiling it in each process. This configuration is intended for coverage
CI: the images retain instrumentation overhead even when coverage is disabled,
and cached inference effects can differ from those of an ordinary build.

Like `--check-bounds`, coverage uses the system image as built, without
invalidating its native code at startup. For newly compiled and interpreted
code, `@<path>` uses the same instrumentation policy as `user`: Base and Core
code are excluded. The path filters reports, not compilation. Compatible image
counters can still report Base and Core locations under the selected path;
relative Base filenames are resolved against the installed Base source directory.
Complete coverage of Base requires suitable instrumented images and `all` for
newly compiled Base code.
An instrumented image carries counters for every statement, and the requested
scope selects which of them contribute to the report. The counters contribute
only when their mode can serve the request: count counters can serve hit mode,
but hit counters cannot serve count mode. An ordinary system image has no
counters. Package images are selected separately by their instrumentation flags.
Allocation tracking retains its per-method recompilation fallback because
images carry no allocation counters. Image generation does not record coverage
hits for its own precompile workload.

## Using images

Julia will dump an image at exit whenever one of the `--output-*` options is
set.  `--output-ji` will produce a pure heap image when used alone, while the
`--output-o` option, or one of the `--output-bc`, `--output-unopt-bc` and
`--output-asm` debugging options trigger the generation of a native image.  If
both `--output-ji` and `--output-o` are used, a split image is created.

When dumping an image, no default system image is loaded.  One can be specified
with `-J`, but if that option is not provided, Julia begins the bootstrapping
process by loading `boot.jl` from the current directory into the `Core` module.

### Incremental images

Incremental images are created using `--output-incremental=yes`.  The package
loading code (see `base/loading.jl`) uses the `.ji` header to determine if an
incremental image is compatible and up-to-date.  Incremental images also contain
the source text that got compiled, which is consumed by development tools like
[`Revise.jl`](https://github.com/timholy/Revise.jl).  Code running while
generating an incremental image must define a module in the `Base.__toplevel__`
module, and must not create any new bindings in any of the existing modules.

```
julia \
    --startup-file=no --history-file=no \
    -Jusr/lib/julia/sys.dylib \
    --output-incremental=yes \
    --output-ji=foo.ji \
    -e 'Base.include(Base.__toplevel__, "foo.jl")'
```

## [Native images for multiple targets](@id sysimg-multi-versioning)

The system image can be compiled simultaneously for multiple CPU
microarchitectures under the same instruction set architecture (ISA). Multiple
versions of the same function may be created with minimal dispatch points
inserted into shared functions in order to take advantage of different ISA
extensions or other microarchitecture features.  The version that offers the
best performance will be selected automatically at runtime based on available
CPU features.

A multi-microarchitecture system image can be enabled by passing multiple
targets during system image compilation. This can be done either with the
[`JULIA_CPU_TARGET`](@ref JULIA_CPU_TARGET) make option or with the `-C` command
line option when running the compilation command manually.  Multiple targets are
separated by `;` in the option string.  The syntax for each target is a CPU name
followed by multiple features separated by `,`.  All features supported by LLVM
are supported and a feature can be disabled with a `-` prefix.  (`+` prefix is
also allowed and ignored to be consistent with LLVM syntax).  Additionally, a
few special features are supported to control the function cloning behavior.

!!! note
    It is good practice to specify either `clone_all` or `base(<n>)` for every
    target apart from the first one. This makes it explicit which targets have
    all functions cloned, and which targets are based on other targets. If this
    is not done, the default behavior is to not clone every function, and to use
    the first target's function definition as the fallback when not cloning a
    function.

1. `clone_all`

    By default, only functions that are the most likely to benefit from
    the microarchitecture features will be cloned.
    When `clone_all` is specified for a target, however,
    **all** functions in the system image will be cloned for the target.
    The negative form `-clone_all` can be used to prevent the built-in
    heuristic from cloning all functions.

2. `base(<n>)`

    Where `<n>` is a placeholder for a non-negative number (e.g. `base(0)`,
    `base(1)`).  By default, a partially cloned (i.e. not `clone_all`) target
    will use functions from the default target (first one specified) if a
    function is not cloned.  This behavior can be changed by specifying a
    different base with the `base(<n>)` option.  The `n`th target (0-based) will
    be used as the base target instead of the default (`0`th) one.  The base
    target has to be either `0` or another `clone_all` target.  Specifying a
    non-`clone_all` target as the base target will cause an error.

3. `opt_size`

    This causes the function for the target to be optimized for size when there
    isn't a significant runtime performance impact. This corresponds to `-Os`
    GCC and Clang option.

4. `min_size`

    This causes the function for the target to be optimized for size that might
    have a significant runtime performance impact. This corresponds to `-Oz`
    Clang option.

As an example, at the time of this writing, the following string is used in the
creation of the official `x86_64` Julia binaries downloadable from
julialang.org:

```
generic;sandybridge,-xsaveopt,clone_all;haswell,-rdrnd,base(1);x86-64-v4,-rdrnd,base(1)
```

This creates a system image with four separate targets; one for a generic
`x86_64` processor, one with a `sandybridge` ISA (explicitly excluding
`xsaveopt`) that explicitly clones all functions, and two targeting the
`haswell` and `x86-64-v4` ISAs, based off of the `sandybridge` version, both
excluding `rdrnd`. When a Julia implementation loads the generated sysimg, it
will check the host processor for matching CPU capability flags, enabling the
highest ISA level possible. Note that the base level (`generic`) requires the
`cx16` instruction, which is disabled in some virtualization software and must
be enabled for the `generic` target to be loaded. Alternatively, a sysimg could
be generated with the target `generic,-cx16` for greater compatibility, however
note that this may cause performance and stability problems in some code.

### Implementation overview

CPU and feature tables come from the
[cpufeatures](https://github.com/JuliaLang/cpufeatures) library. A build-time
generator extracts tables from LLVM's TableGen data and emits standalone C
headers (`generated/target_tables_{x86_64,aarch64,riscv64}.h`).

The runtime library (`libtarget_parsing`) handles target string parsing, host
CPU detection, and serialization.

  - `src/processor.cpp`: parses target strings, detects the host, resolves
    targets, and serializes/deserializes target data for system images.
  - `src/llvm-multiversioning.cpp`: clones functions based on target feature
    diffs.  Functions with loops or CPU queries are always cloned; SIMD, math,
    float16, and bfloat16 functions are cloned when the target adds those
    capabilities.

### Upgrading LLVM

When Julia's LLVM version changes, regenerate the cpufeatures headers:

```
cd cpufeatures
cmake -B build -DCMAKE_PREFIX_PATH=<path-to-new-llvm>
cmake --build build --target gen_target_tables
build/gen_target_tables x86_64-linux-gnu > generated/target_tables_x86_64.h
build/gen_target_tables aarch64-linux-gnu > generated/target_tables_aarch64.h
build/gen_target_tables riscv64-linux-gnu > generated/target_tables_riscv64.h
```

Commit the updated headers and update Julia's cpufeatures dependency. A
`static_assert` in `src/processor.cpp` checks `TARGET_TABLES_LLVM_VERSION_MAJOR`
against Julia's LLVM version, so mismatches are caught at build time.

## Format specification

Julia images contain a snapshot of the heap in a form that can be used directly
by Julia, so pointers, denoted `ptr`, are either 4 or 8 bytes, depending on the
platform.  Integers (written `u8`, `u16`, etc, for unsigned, and `i8`, ... for
signed) use the system endianness.  `usize` is a pointer-sized integer. A `zstr`
is a NUL-terminated string, while `bytes` implies no null termination.  Any
padding is written explicitly.  `f64` is a `Float64`.

### Heap images

Non-split native images contain a heap image including the full header, so the
format of Julia images (also called a `.ji`) will be described first.

| Section            | Notes                               |
|:-------------------|:------------------------------------|
| `.ji` header       |                                     |
| incremental header | Present only for incremental images |
| padding            | Zeros, up to `payload_start`        |
| payload            |                                     |
| source text        | Present only for incremental images |

#### `.ji` header

| Field                 | Encoding  | Notes                                                             |
|:----------------------|:----------|:------------------------------------------------------------------|
| `magic`               | 8 bytes   | `fb 6a 6c 69 0d 0a 1a 0a` / `"\373jli\r\n\032\n"`                 |
| `format_version`      | u16       | Currently `18`                                                    |
| `bom`                 | u16       | Byte order mark, `0xfeff`                                         |
| `pointer_size`        | u8        | `sizeof(void*)`                                                   |
| `reserved`            | u8[3]     | 0                                                                 |
| `flags`               | u32       |                                                                   |
| `checksum`            | u32       | CRC-32C of uncompressed payload                                   |
| `compat`              | byte[128] | An arbitrary string that must match exactly.                      |
| `preferred_base`      | u64       | If the heap is at this address, slide relocations are free.       |
| `payload_start`       | u64       | Offset of the payload from the start of the `.ji`                 |
| `payload_size`        | u64       | Size of the payload as stored (compressed if it is)               |
| `heap_size`           | u64       |                                                                   |
| `pointer_free_offset` | u64       | Payload offset                                                    |
| `gvar_objects`        | u64       | Number of GC-tracked gvars                                        |
| `world`               | u64       | `jl_world_counter` and `jl_require_world`; 0 in incremental image |
| `typeinf_world`       | u64       | `jl_typeinf_world`; 0 in incremental image                        |
| `gensym_counter`      | u64       | 0 in incremental image                                            |
| section table         | see below |                                                                   |

The "payload" consists of the heap, and all of the metadata sections up to and
including the "strtab" section.  The payload is optionally compressed.  The
`payload_size` field is the size of the payload as-stored.  The sizes and
offsets in the section table, described below, are the uncompressed sizes and
offsets from the start of the uncompressed payload, respectively.

Each entry in the section table has this format:

| Field    | Encoding | Notes                                        |
|:---------|:---------|:---------------------------------------------|
| `offset` | u64      | Payload offset; 0 if the section is empty    |
| `size`   | u64      | Size of uncompressed section                 |

The section table describes the sections that follow the heap in the payload, in
order:

| Section        | Notes                         |
|:---------------|:------------------------------|
| `fptrs`        | Only present in native images |
| `imports`      |                               |
| `binds`        |                               |
| `slides`       |                               |
| `fixup_types`  | Empty in system images        |
| `fixup_objs`   |                               |
| `specials`     |                               |
| `gvars`        |                               |
| `auxdata`      |                               |
| `strtab`       |                               |

The heap itself consists of two parts: the ordinary heap, which can contain
relocations, and the pointer-free region, which contains no relocations and can
be skipped by the loader.  The size of these regions together is `heap_size`.

The native library of every native image defines a symbol called
`jl_system_image_checksum`, a `u32` that must match the checksum in this
header.

Before the payload of a native image is loaded, the runtime calls function
pointed to by the `jl_image_unpack` symbol in the image.  Typical unpack
functions are `jl_image_unpack_uncomp`, which does no preprocessing,
`jl_image_unpack_split`, which maps the `.ji` for a split image, and
`jl_image_unpack_zstd`/`jl_image_unpack_split_zstd`, which decompress a zstd
payload.

Flags:

| Bit | Name               | Notes                                            |
|----:|:-------------------|:-------------------------------------------------|
|   0 | `JI_FLAG_PKGIMAGE` | This is an incremental image.                    |
|   1 | `JI_FLAG_SPLIT`    | This `.ji` is a sidecar to a split native image. |

The `compat` field contains these strings, separated by `;`, and padded with
NUL to 128 bytes:
- `JL_BUILD_UNAME`
- `JL_BUILD_ARCH`
- `JULIA_VERSION_STRING`
- `jl_gc_image_abi()`

#### Incremental header

This header is present only when the `JI_FLAG_PKGIMAGE` flag is set in the
main header.

| Field              | Encoding | Notes                                                                    |
|:-------------------|:---------|:-------------------------------------------------------------------------|
| `native_crc`       | u32      | CRC-32C of the native library of a split image, 0 otherwise              |
| `cache_flags`      | u8       |                                                                          |
| `coverage`         | u8       |                                                                          |
| `syntax_version`   | u8       |                                                                          |
| `result_modules`   | variable | Multiple, see below                                                      |
| `deps_size`        | u64      | Byte length of this field, `dependencies` and `prefs_blob`, for skipping |
| `dependencies`     | variable | Multiple, see below                                                      |
| `prefs_blob`       | variable | See below                                                                |
| `required_modules` | variable | Multiple, see below                                                      |
| `clone_targets`    | variable | See below                                                                |
 
##### Cache flags

| Bits | Name            | Notes                                                                           |
|-----:|:----------------|:--------------------------------------------------------------------------------|
|  7-6 | `opt_level`     | Optimization level (`-On`)                                                      |
|    5 | `inline`        | Inlining enabled (`--inline=no/yes`)                                            |
|  4-3 | `check_bounds`  | Bounds-checking: 0 = auto, 1 = always, 2 = never (`--check-bounds=auto/yes/no`) |
|  2-1 | `debug_level`   | Debug info (`-gn`)                                                              |
|    0 | `use_pkgimages` | Same as `JI_FLAG_SPLIT` (TODO: remove eventually)                               |

The `native_crc` is at a fixed offset, where the `.ji` header ends, so that it
can be filled in once the native library exists.  It identifies the library a
`.ji` was written with without loading it, unlike the `checksum` that the
library defines.

The cache flags are used to determine if a native incremental image is
compatible with the command line options for the running Julia process (see
`jl_match_cache_flags`).

##### Coverage

| Value | Name                      | Notes                        |
|------:|:--------------------------|:-----------------------------|
|     0 | `JL_IMAGE_COVERAGE_NONE`  | No code coverage support     |
|     1 | `JL_IMAGE_COVERAGE_HIT`   | `--code-coverage-mode=hit`   |
|     2 | `JL_IMAGE_COVERAGE_COUNT` | `--code-coverage-mode=count` |

##### Result modules
The `result_modules` list contains an entry for each top-level module defined by
the incremental image.  The list is terminated by a single `i32(0)`.  Each entry
has this format:

| Field         | Encoding |
|:--------------|:---------|
| `name_length` | i32      |
| `name`        | bytes    |
| `uuid.hi`     | u64      |
| `uuid.lo`     | u64      |
| `build_id.lo` | u64      |

The high 64 bits of the build ID is not recorded here, because it is the
(zero-extended) `checksum` from the `.ji` header.

##### Dependencies

The `dependencies` list is an `i32(0)`-terminated list of records representing
either an included file, or a package resolution for packages imported with
`using`/`import`.

Included file:

| Field            | Encoding | Notes                                                     |
|:-----------------|:---------|:----------------------------------------------------------|
| `deppath_length` | i32      |                                                           |
| `deppath`        | bytes    | The path of the file                                      |
| `fsize`          | u64      | 0 if `track_content=false`                                |
| `hash`           | u32      | CRC-32C if `track_content=true`, else 0                   |
| `mtime`          | f64      | `-1.0` if `track_content=true`                            |
| `provides_index` | i32      | 1-based index of the result module that included the file |
| `modpath`        | variable | Only if `provides_index` is not 0, see below              |

Package resolution (note the `deppath` field is used to encode information about
how the package was resolved):

| Field            | Encoding | Notes                                                           |
|:-----------------|:---------|:----------------------------------------------------------------|
| `deppath_length` | i32      | 17 + the length of the package name                             |
| `marker`         | u8       | 0, so that the entry is not mistaken for a file path            |
| `uuid`           | u128     | 0 if the package has no UUID                                    |
| `name`           | bytes    | The package name                                                |
| `fsize`          | u64      | 0                                                               |
| `hash`           | u32      | 0                                                               |
| `mtime`          | f64      | 0.0                                                             |
| `provides_index` | i32      | 1-based index of the result module that ran `using` or `import` |
| `modpath`        | variable | Only if `provides_index` is not 0, see below                    |

If a module `A.B.C` includes a file, that file will have the index of the root
module `A` in its `provides_index` field, while its `modpath` will have `B` and
`C`, in order.

The `modpath` is an `i32(0)`-terminated list of this structure:

| Field         | Encoding | Notes                     |
|:--------------|:---------|:--------------------------|
| `name_length` | i32      |                           |
| `name`        | bytes    | The nested module's name  |

##### Preferences

The serialized preferences blob:

| Field               | Encoding |
|:--------------------|:---------|
| `prefs_blob_length` | i32      |
| `prefs_blob`        | bytes    |

##### Required modules

The `required_modules` list is `i32(0)`-terminated, and records all the
top-level modules that were loaded from an image.  Note that multiple required
modules can come from a single image.

| Field         | Encoding |
|:--------------|:---------|
| `name_length` | i32      |
| `name`        | bytes    |
| `uuid.hi`     | u64      |
| `uuid.lo`     | u64      |
| `build_id.hi` | u64      |
| `build_id.lo` | u64      |

##### Clone targets

Unless this is a native image (`JI_FLAG_SPLIT` is set), the `data_size` will be
zero.  The format of the `data` is not yet described here.

| Field       | Encoding |
|:------------|:---------|
| `data_size` | u32      |
| `data`      | bytes    |

#### Payload

The payload begins with the heap, which is used after the image is loaded.  It
is followed by the sections that are used only while the image is loaded
(fptrs, imports, binds, slides, fixup_types, fixup_objs, specials, gvars,
auxdata and strtab), in the order of the section table, which gives the offset
of each from `payload_start`, and its size.

The heap contains Julia data structures in a form that is suitable for use
directly by Julia, after some initialization work has been done.

The `pointer_free_offset` is an offset into the heap section, beyond which there
are no more relocations.  The binds and slides sections do not cover the
pointer-free section.

All objects in the heap image are 16-byte aligned.  The `payload_start`,
`pointer_free_offset` and `heap_size`, as well as the address where we load the
heap, are aligned to `JL_CACHE_BYTE_ALIGNMENT`.  A `preferred_base` other than 0 is
`payload_start` past a page boundary (an allocation granularity boundary on
Windows), so that the file can be mapped with the heap at that address.

##### Relocations

Relocations in the heap section come in two forms.  *Slide* relocations
represent pointers from the heap section to other data in the heap section.
Their representation is that of a normal pointer, assuming the heap is at its
*preferred base address*.  When the heap is anywhere else, the loader consults
the *slides* section to find the address of every slide in the image, to which
it adds the difference between the expected and actual addresses of the heap.

!!! note
    A slide can also have its low bits set, like the type tag of an object.
    This is allowed because the image will only ever be relocated some multiple
    of `JL_CACHE_BYTE_ALIGNMENT`.

*Bind* relocations represent pointers from the heap section to data in other
images, objects and functions from the runtime, and objects that must be unique
in the process, which are looked up or instantiated by the loader.

A bind relocation is stored as a pointer, with the following bit fields, from
the most significant bit down:

| Field           | Width (64 bit) | Width (32 bit) | Notes                                   |
|:----------------|----------------|----------------|:----------------------------------------|
| `BIND_BIT`      | 1              | 1              | Always 1                                |
| `BIND_PAYLOAD`  | 60             | 28             | Index into the imports section          |
| `BIND_LOW_BITS` | 3              | 3              | ORed into the resolved address          |

The purpose of the `BIND_LOW_BITS` field is to set the bits in object headers.

In addition to the heap, some sections (including specials, gvars, and auxdata)
contain fields with the `reloc` type, which have the same encoding as
relocations in the heap.

##### Imports section

Since many bind relocations can share a target, we store the target of these
relocations in the imports section.  The order of entries in the imports section
is topologically sorted in order to help the loader (with the exception of
DataType's `super`, which can have cycles and requires some special handling).
See the auxdata section.

The import section contains an array of `u64`s.  Each `u64` contains these bit
fields, from the uppermost bit down:

| Field     | Width | Notes                    |
|:----------|-------|:-------------------------|
| `type`    | 4     |                          |
| `payload` | 60    | Use determined by `type` |

The following import types exist:
- `IMPORT_IMAGE` (0) - resolves to an address in a dependency image.

  | Field    | Width | Notes                    |
  |:---------|-------|:-------------------------|
  | `type`   | 4     | `IMPORT_IMAGE`           |
  | `unused` | 1     |                          |
  | `depref` | 59    | A dependency reference   |

  A *dependency reference* refers to an address in another image.  Its upper
  `DEPREF_DEPS_BITS` (21) bits are the dependency index, and the lower
  `DEPREF_OFFSET_BITS` (38) bits are the byte offset from the start of that
  image's heap section.  Dependency index 0 is the system image, and index
  `k >= 1` is the image that entry `k` (counting from 1) of `required_modules`
  was loaded from.

  Like a slide, an `IMPORT_IMAGE` can point to arbitrary addresses in the target
  image.  Currently, only the `ptr_or_offset` field of `MemoryRef` has
  relocations to the interior of objects.

- `IMPORT_SPECIAL` (1) - resolves to a special object created by the runtime
  before the system image is loaded.

  | Field           | Width | Notes            |
  |:----------------|-------|:-----------------|
  | `type`          | 4     | `IMPORT_SPECIAL` |
  | `special_index` | 60    |                  |

  Currently, the only `special_index` is `0`, which refers to the root task.

- `IMPORT_FUNCTION` (2) - resolves to the address of a function in the runtime.

  | Field  | Width | Notes             |
  |:-------|-------|:------------------|
  | `type` | 4     | `IMPORT_FUNCTION` |
  | `kind` | 3     | Described below   |
  | `idx`  | 57    | Depends on `kind` |

  The `kind` can be one of:
  - `FUNCTION_BUILTIN` (0): The `idx` is a builtin's index in
    `jl_builtin_f_addrs`, and the import resolves to that builtin function's
    address.
  - `FUNCTION_RUNTIME` (1): The `idx` is a `jl_invoke_api_t` calling convention:
    `args` (1), `const` (2), `sparam` (3) or `interpreted` (4).  The import
    resolves to its entry point (`jl_fptr_args`, ...).  The `specsig` calling
    convention has no entry point in the runtime (an fptr wrapper has been
    generated, see the `fptrs` section).

- `IMPORT_SYMBOL` (3) - resolves to the symbol with the given name, creating it
  if necessary.

  | Field         | Width | Notes                            |
  |:--------------|-------|:---------------------------------|
  | `type`        | 4     | `IMPORT_SYMBOL`                  |
  | `name_offset` | 60    | Offset of the symbol's name in the strtab section, a list of NUL-terminated names |

  The system image does not use `IMPORT_SYMBOL`, because its symbols are
  embedded directly in the heap.  Incremental images can refer to these symbols
  with `IMPORT_IMAGE`, too.

- `IMPORT_SINGLETON` (4) - resolves to the singleton instance of an `IMPORT_DATATYPE` import.

  | Field          | Width | Notes                                            |
  |:---------------|-------|:-------------------------------------------------|
  | `type`         | 4     | `IMPORT_SINGLETON`                               |
  | `import_index` | 60    | Index of the datatype's `IMPORT_DATATYPE` import (must be smaller) |

- `IMPORT_DATATYPE` (5), `IMPORT_METHOD_INSTANCE` (6) and `IMPORT_BINDING` (7) -
  resolves to an object that must be created by the loader.

  | Field         | Width | Notes                                                    |
  |:--------------|-------|:---------------------------------------------------------|
  | `type`        | 4     | One of the three types above                             |
  | `record`      | 60    | Offset of the object's record in the auxdata section     |

  If we know a DataType, MethodInstance, or Binding is owned by the image, we
  can put it directly in the image heap.  These imports are for objects that
  might be shared by many images.  The `record` field points to an offset in the
  auxdata section containing information on how to create the object or find the
  existing one.

  The DataType's `parameters` might depend on some imports.  They must have a
  lower index than the `IMPORT_DATATYPE`.

- `IMPORT_SPECPTR` (8) - resolves to the `specptr` of a `CodeInstance` in a
  dependency image, whose payload is a dependency reference to it.  This is
  null when the dependency's native code is not used.

##### Binds section

The binds section marks every word of the heap (excluding the pointer-free
section) that contains a bind relocation.  There are two formats, of which the
writer picks the most compact:

- If the size of the section contents is
  `8 * ceil(pointer_free_offset / (64 * sizeof(ptr)))`,
  the section is stored in the *bitmap* form.  Every `u64` of
  the data represents 64 words of the heap, least significant bit first.
- Otherwise, the *gaps* form is used: each gap gives the offset from the
 previous bind to the next, in words, and beginning with zero.  Gaps are
  written as either a `u8` from `0x01` to `0xff`, in which case the `u8` is the
  gap size in words, or `0x00` followed by a `u32`, the gap size.

##### Slides section

The slides section is identical to the binds section, but it marks the locations
of slides in the image.  If the heap is mapped at the preferred base address,
this section doesn't need to be read.

##### Auxdata section

The auxdata section contains data that is needed for resolving imports, but can
be discarded after loading is complete.  Imports refer to auxdata through byte
offsets into this section.  Each record starts at a pointer-aligned offset,
and any bytes between records are padding.  The following import types need an
auxdata record:

- `IMPORT_DATATYPE`

  | Field        | Encoding | Notes                                    |
  |:-------------|:---------|:-----------------------------------------|
  | `name`       | reloc    |                                          |
  | `parameters` | reloc    |                                          |
  | `types`      | reloc    | 0 if the field types are computed lazily |
  | `layout`     | reloc    | 0 if the type has no layout              |
  | `instance`   | reloc    | `nothing` for a singleton, see below     |
  | `super`      | reloc    |                                          |
  | `hash`       | u32      |                                          |
  | `flags`      | u16      | See below                                |

  The `flags` are the bit fields of `jl_datatype_t`.

  The `instance` field is a `reloc` pointing to `nothing` if the loader must
  create the instance.  DataTypes that have a special use for their `instance`
  field, like `GenericMemory` types, point directly to the object.

- `IMPORT_METHOD_INSTANCE`

  | Field         | Encoding  |
  |:--------------|:----------|
  | `def`         | reloc     |
  | `specTypes`   | reloc     |
  | `sparam_vals` | reloc     |

- `IMPORT_BINDING`

  | Field  | Encoding |
  |:-------|:---------|
  | `mod`  | reloc    |
  | `name` | reloc    |

##### Fixup sections

The fixup_types and fixup_objs sections contain an array of `usize` heap
offsets of objects which the runtime must do some postprocessing to when loading
the image.

The fixup_types section lists the `DataType`s whose `TypeName` is owned by
another image, but which this image owns because one of its parameters involves
a type owned by this image.

The action for an object in the fixup_objs section depends on its type:

| Objects                                                                | Action                                                                                                    |
|:-----------------------------------------------------------------------|:----------------------------------------------------------------------------------------------------------|
| `Module`s                                                              | Set the high 64 bits of the build ID to the `checksum`, and, in incremental images, register its `using`s |
| `MethodInstance`s of methods owned by another image (incremental only) | Insert into the method's specializations                                                                  |
| `CancellationTokenSource`s                                             | Attach to its parents, and take on their cancellation state                                               |
| `Method`s, `TypeMapEntry`s and `CodeInstance`s (incremental only)      | Activate once the image is loaded                                                                         |

##### Specials

The specials and gvars sections are arrays of `reloc`s.

System images need to identify a number of special heap objects that are
referred to in `libjulia` and `libjulia-internal`.  The specials section
contains a fixed sequence of relocs to these objects:

- `jl_method_table`
- `jl_module_init_order`
- `jl_typeinf_func`
- `jl_compile_and_emit_func`
- `jl_libdl_dlopen_func`
- All `Builtin`s, in ascending order
- `JL_EXPORTED_DATA_POINTERS`, in the order they are declared in
  `jl_exported_data.inc`
- `JL_CONST_GLOBAL_VARS`, in the order they are declared in
  `jl_exported_data.inc`.
- `jl_global_roots_list`
- `jl_global_roots_keyset`
- The root task's TLS `IdDict`
- The root of the symbol table

In an incremental image, the specials section instead contains five relocs,
each to a `Vector{Any}`:
- `worklist`: the modules the image defines (the last one is its top-level
  module)
- `module_init_order`: the modules to initialize, in order
- `extext_methods`: the methods the image adds to functions it does not own
- `extext_records`: for each of those methods, in the same order, its
  activation record (a vector of the methods of other images that it interferes
  with), or `nothing`
- `method_roots_list`: the roots the image adds to methods it does not own, as a
  flat list of alternating `Method`s and vectors of roots

##### Gvars

The gvars section contains `reloc`s describing addresses the native code refers
to (see the `gvar_idxs` array, not described here).  The first `gvar_objects`
entries are GC-managed heap objects, while the remaining ones are pointers to
data not managed by the GC (currently, just `IMPORT_SPECPTR` entries).

##### Fptrs section

The fptrs section is an array of `usize` heap offsets, in which entry `i`
describes function `i` of the native image's function table.  The array may be
shorter than the table, and missing entries are 0.  An entry of 0 means the
function belongs to no `CodeInstance`.  Functions that are invoke wrappers have
their lowest bit (`FPTR_INVOKE`) set, as opposed to functions with the specptr
calling convention.

#### Source text

The source text section starts at `payload_start + payload_size`, and contains an
`i32(0)`-terminated list of source text records:

| Field            | Encoding | Notes                                  |
|:-----------------|:---------|:---------------------------------------|
| `deppath_length` | i32      |                                        |
| `deppath`        | bytes    | As in the `dependencies` list          |
| `srctext_length` | u64      |                                        |
| `srctext`        | bytes    | The source text of the file            |

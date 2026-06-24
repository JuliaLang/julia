# Ahead of Time Compilation

This document describes the design and structure of the ahead-of-time (AOT) compilation system in Julia. This system is used when generating system images and package images. Much of the implementation described here is located in `aotcompile.cpp`, `staticdata.c`, and `processor.cpp`

## Introduction

Though Julia normally compiles code just-in-time (JIT), it is possible to compile code ahead of time and save the resulting code to a file. This can be useful for a number of reasons:
1. To reduce the time it takes to start a Julia process.
2. To reduce the time spent in the JIT compiler instead of executing code (time to first execution, TTFX).
3. To reduce the amount of memory used by the JIT compiler.

## High-Level Overview

The following descriptions are a snapshot of the current implementation details of the end-to-end pipeline that happens internally when the user compiles a new AOT module, such as occurs when they type `using Foo`. These details are likely to change over time as we implement better ways to handle them, so current implementations may not exactly match the dataflow and functions described below.

### Compiling Code Images

Firstly, the methods that need to be compiled to native code must be identified. This can only be done by actually executing the code to be compiled, as the set of methods that need to be compiled depends on the types of the arguments passed to the methods, and method invocations with certain combinations of types may not be known until runtime. During this process, the exact methods that the compiler sees are tracked for later compilation, producing a compilation trace.

!!! note

    Currently when compiling images, Julia runs the trace generation in a different process than the process performing the AOT compilation. This can have impacts when attempting to use a debugger during precompilation. The best way to debug precompilation with a debugger is to use the rr debugger, record the entire process tree, use `rr ps` to identify the relevant failing process, and then use `rr replay -p PID` to replay just the failing process.

Once the methods to be compiled have been identified, they are passed to the `jl_create_system_image` function. This function sets up a number of data structures that will be used when serializing native code to a file, and then calls `jl_create_native` with the array of methods. `jl_create_native` runs codegen on the methods and produces one or more LLVM modules. `jl_create_system_image` then records some useful information about what codegen produced from the module(s).

The module(s) are then passed to `jl_dump_native`, along with the information recorded by `jl_create_system_image`. `jl_dump_native` contains the code necessary to serialize the module(s) to bitcode, object, or assembly files depending on the command-line options passed to Julia. The serialized code and information are then written to a file as an archive.

The final step is to run a system linker on the object files in the archive produced by `jl_dump_native`. Once this step is complete, a shared library containing the compiled code is produced.

### Loading Code Images

When loading a code image, the shared library produced by the linker is loaded into memory. The system image data is then loaded from the shared library. This data contains information about the types, methods, and code instances that were compiled into the shared library. This data is used to restore the state of the runtime to what it was when the code image was compiled.

If the code image was compiled with multiversioning, the loader will pick the appropriate version of each function to use based on the CPU features available on the current machine.

For system images, since no other code has been loaded, the state of the runtime is now the same as it was when the code image was compiled. For package images, the environment may have changed compared to when the code was compiled, so each method must be checked against the global method table to determine if it is still valid code.

## Compiling Methods

### Tracing Compiled Methods

Julia has a command-line flag to record all of the methods that are compiled by the JIT compiler, `--trace-compile=filename`. When a function is compiled and this flag has a filename, Julia will print out a precompile statement to that file with the method and argument types it was called with. This therefore generates a precompile script that can be used later in the AOT compilation process. The [PrecompileTools](https://julialang.github.io/PrecompileTools.jl/stable/) package has tooling that can make taking advantage of this functionality easier for package developers.

### `jl_create_system_image`

`jl_create_system_image` saves all of the Julia-specific metadata necessary to later restore the state of the runtime. This includes data such as code instances, method instances, method tables, and type information. This function also sets up the data structures necessary to serialize the native code to a file. Finally, it calls `jl_create_native` to create one or more LLVM modules containing the native code for the methods passed to it. `jl_create_native` is responsible for running codegen on the methods passed to it.

### `jl_dump_native`

`jl_dump_native` is responsible for serializing the LLVM module containing the native code to a file. In addition to the module, the system image data produced by `jl_create_system_image` is compiled as a global variable. The output of this method is bitcode, object, and/or assembly archives containing the code and system image data.

`jl_dump_native` is typically one of the larger time sinks when emitting native code, with much of the time spent in optimizing LLVM IR and emitting machine code. To use the available cores, it splits the module of compiled native code (the `text` module) into independent *shards* that are optimized and lowered to machine code in parallel. How many shards a module is split into, and how many of them compile concurrently, are sized automatically from the module and the machine (described below); the concurrency can be explicitly overridden by setting the [`JULIA_IMAGE_THREADS`](@ref JULIA_IMAGE_THREADS) environment variable.

#### Threading and sharding

Two distinct quantities control this parallelism, and it is worth keeping them separate:

* The **shard count** is the static number of partitions the `text` module is split into. It fixes the output layout: each shard is serialized once, then independently deserialized into its own LLVM context, optimized, and emitted. Splitting bounds peak working memory (a shard only materializes its own partition) and exposes parallelism — but it is not free, because every shard re-parses the serialized module, a fixed per-shard cost. Splitting a module into more shards than can actually run at once is therefore pure overhead.
* The **thread count** is the ceiling on how many shards compile *concurrently*. Shards are pulled from a shared queue, so the shard count may exceed the thread count; the pool just works through the queue in waves.

**Choosing the shard count.** The shard count is the larger of two terms. A *memory* term, `weight / weight_per_shard`, finely splits large modules so that peak working memory stays bounded by roughly `concurrency * weight_per_shard` regardless of total module size; here `weight` is a codegen-cost estimate for the module (see `compute_module_info` in `aotcompile.cpp`). This term does not depend on the core count, so a given module produces the *same* shard layout on every machine. A *core-fill* term then ramps the shard count up toward the available concurrency, so that a medium module — big enough to parallelize, but too small to trip the memory term — is not emitted as a single serial shard that leaves cores idle. Under a jobserver (see below) "available concurrency" is the count of free tokens probed at the moment the module is partitioned, not the machine's core count: during the dense phase of a parallel precompile the machine is already saturated by inter-package parallelism, few tokens are free, and the ramp stays low so it does not multiply the per-shard re-parse cost without buying parallelism; at the tail, when one big package compiles while the rest are done, most of the budget is free and the same module fans out across the idle cores. A tiny module stays in a single shard and skips the serialize/deserialize round-trip entirely.

**Choosing the thread count.** The thread count aims for all effective cores, then is capped to the shard count (there is no point having more threads than shards) and to a fraction of the module's global-variable count. How the "all cores" aim is bounded depends on who owns the machine. For a *single-process build* — the system image, or a lone `PackageCompiler` run — the process owns the whole machine, so concurrency is bounded by a *memory budget* rather than the core count alone: peak codegen memory grows with the number of concurrent shards, so the thread count is capped at roughly `codegen_memory_budget / per_shard_codegen_memory`. That budget is sized from the machine's total RAM (capped by any cgroup or container limit), leaving headroom for the build process, the GC, and the OS. For a *parallel package precompile*, concurrency is instead coordinated across all worker processes by a jobserver, described next.

**Summary by module size.** With a default per-shard weight target of 500,000, a parallel package precompile splits a module's `text` as follows. ("free cores" is the number of jobserver tokens free when the module is partitioned — near zero during the dense phase, near the full core count at the tail.)

| Module size | Approx. `text` weight | Shards | What happens |
| :--- | :--- | :--- | :--- |
| Small | below 500,000 | 1–3 (a module below ~125,000 stays a single shard) | A tiny module is emitted as one shard with no serialize/deserialize round-trip and compiled serially; slightly larger ones get a few shards, each kept big enough to pay back its per-shard re-parse cost. |
| Medium | 500,000 – 2,000,000 | `min(free cores, weight / 125,000)` | Tracks the parallelism actually free: held low while the machine is busy with other packages (more shards would only add per-shard overhead), but ramped up to fill the cores when they are idle. |
| Large | 2,000,000 – roughly (free cores × 500,000) | `weight / 500,000` (about one per free core) | The memory term takes over; the shards roughly fill the free cores and generally compile together once the worker holds enough jobserver tokens. |
| Extra-large | above roughly (free cores × 500,000) | `weight / 500,000` (more than the thread count) | Shards outnumber the concurrent threads and drain from the queue in waves, holding peak working memory near `concurrency × 500,000` however large the module grows. |

A single-process build (the system image or a lone `PackageCompiler` run) has no jobserver, so the core-fill term ramps toward the memory-bounded core count directly: medium and large modules are split into correspondingly more shards, up to what the memory budget allows. In every case the thread count is additionally capped at one per 100 module globals, so a module with few globals shards and parallelizes less than its weight alone would suggest.

During parallel package precompilation, the orchestrating process shares a single CPU-thread budget across all worker subprocesses via a named-semaphore jobserver (whose name is passed to workers through the `JULIA_PRECOMPILE_JOBSERVER` environment variable, and whose size defaults to one more than the number of effective CPU threads but is overridable via [`JULIA_PRECOMPILE_THREADS`](@ref JULIA_PRECOMPILE_THREADS)). Each worker holds one baseline token while it is CPU-active, and during its imaging phase sizes its thread pool elastically against the shared budget: it starts with the baseline thread plus whatever extra tokens are immediately available, keeps polling for tokens freed by sibling workers while it still has uncompiled shards, and returns each token to the pool as soon as the thread holding it runs out of work. Because a worker's main thread sleeps while its imaging threads run, that baseline token doubles as the imaging phase's first codegen thread rather than being counted twice. A lone worker thus expands to all available cores, workers that start while the machine is busy grow into cores as they free up, and concurrent workers share the pool without oversubscribing the machine. An explicit `JULIA_IMAGE_THREADS` setting takes precedence over the jobserver.

The defaults behind the shard-sizing terms — the per-shard weight target and the per-weight codegen-memory estimate — are governed by intentionally-undocumented `JULIA_IMAGE_*` environment variables read in `aotcompile.cpp`; the per-stage cost of each shard can be inspected with [`JULIA_IMAGE_TIMINGS`](@ref JULIA_IMAGE_TIMINGS).

#### Multiversioning across threads

`jl_dump_native` can also produce native code optimized for multiple architectures, when integrated with the Julia loader. This is triggered by setting the [`JULIA_CPU_TARGET`](@ref JULIA_CPU_TARGET) environment variable and mediated by the multiversioning pass in the optimization pipeline. To make this work with multithreading, an annotation step is added before the module is split into submodules that are emitted on their own threads, and this annotation step uses information available throughout the entire module to decide what functions are cloned for different architectures. Once the annotation has happened, individual threads can emit code for different architectures in parallel, knowing that a different submodule is guaranteed to produce the necessary functions that will be called by a cloned function.

Some other metadata about how the module was serialized is also stored in the archive, such as the number of threads used to serialize the module and the number of functions that were compiled.

### Static Linking

The final step in the AOT compilation process is to run a linker on the object files in the archive produced by `jl_dump_native`. This produces a shared library containing the compiled code. This shared library can then be loaded by Julia to restore the state of the runtime. When compiling a system image, the native linker used by a C compiler is used to produce the final shared library. For package images, the LLVM linker LLD is used to provide a more consistent linking interface.

## Loading Code Images

### Loading the Shared Library

The first step in loading a code image is to load the shared library produced by the linker. This is done by calling `jl_dlopen` on the path to the shared library. This function is responsible for loading the shared library and resolving all of the symbols in the library.

### Loading Native Code

The loader first needs to identify whether the native code that was compiled is valid for the architecture that the loader is running on. This is necessary to avoid executing instructions that older CPUs do not recognize. This is done by checking the CPU features available on the current machine against the CPU features that the code was compiled for. When multiversioning is enabled, the loader will pick the appropriate version of each function to use based on the CPU features available on the current machine. If none of the multiversioned feature sets match the current CPU, the loader will throw an error.

Part of the multiversioning pass creates a number of global arrays of all of the functions in the module. When this process is multithreaded, an array of arrays is created, which the loader reorganizes into one large array with all of the functions that were compiled for this architecture. A similar process occurs for the global variables in the module.

### Setting Up Julia State

The loader then uses the global variables and functions produced from loading native code to set up Julia runtime core data structures in the current process. This setup involves adding types and methods to the Julia runtime, and making the cached native code available for use by other Julia functions and the interpreter. For package images, each method must be validated, in that the global method table's state must match the state that the package image was compiled for. In particular, if a different set of methods exists at the load time compared to compile time of the package image, the method must be invalidated and recompiled on first use. This is necessary to ensure that execution semantics remain the same regardless of if a package was precompiled or if the code was directly executed. System images do not need to perform this validation, since the global method table is empty at load time. Thus, system images have faster load times than package images.

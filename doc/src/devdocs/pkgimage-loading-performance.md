# Package Image Loading Performance Analysis

This document summarizes the performance characteristics of Julia's package image (pkgimage) loading system, based on instrumented timing analysis of `src/staticdata.c`.

## Overview

Package image loading occurs in two main phases:

1. **Restore** (`jl_restore_package_image_from_stream`) - Deserialize and reconstruct the image
2. **Activate** - Register methods and make the package available

## Timing Breakdown by Package Size

### System Image (~188MB)

| Phase | Time (ms) | % of Total |
|-------|-----------|------------|
| apply relocations | 45.2 | 89% |
| read symbols | 4.6 | 9% |
| fptr relocs & registration | 0.6 | 1% |
| **TOTAL** | **50.8** | |

The system image is dominated by relocation processing due to the large number of pointers (1.6M + 7.7M entries) that need adjustment.

### Large Packages (e.g., Plots ~38MB)

| Phase | Time (ms) | % of Total |
|-------|-----------|------------|
| object uniquing (MI, bindings) | 12.1 | 36% |
| apply relocations | 8.0 | 24% |
| type uniquing & caching | 5.7 | 17% |
| fixup objects | 3.7 | 11% |
| read symbols | 2.0 | 6% |
| other | 2.4 | 7% |
| **TOTAL restore** | **33.9** | |
| activate methods | 4.4 | (post-restore) |

Large packages spend most time in uniquing operations - ensuring types and method instances match those already in the runtime.

### Medium Packages (e.g., PlotUtils ~7MB)

| Phase | Time (ms) | % of Total |
|-------|-----------|------------|
| object uniquing | 2.4 | 37% |
| type uniquing | 1.6 | 25% |
| apply relocations | 1.4 | 22% |
| fixup objects | 0.5 | 8% |
| other | 0.5 | 8% |
| **TOTAL restore** | **6.5** | |

### Small Packages (JLLs, ~15-50KB)

JLL packages load in 0.05-0.15ms total, with time split between:

- Type uniquing checks (~0.005-0.01ms)
- Symbol reads (~0.01-0.05ms)
- Relocations (~0.005-0.02ms)

## Key Bottlenecks

### 1. Relocations (System Image)

The `jl_read_relocations` function processes relocation lists that patch pointers in the loaded image. For the system image with ~7.8MB of relocation data (~9.3M total entries), this takes 45ms.

**Current implementation:** Sequential processing with `ios_read` calls and pointer arithmetic.

**Parallel relocation attempt:** Tested a two-phase approach (decode all entries, then apply in parallel with pthreads). Result: **slower** (45ms → 48ms) due to:

- Memory allocation overhead for 120MB+ entry buffer
- Thread creation/join overhead
- Cache effects from non-sequential access

**Potential optimizations:**

- Batch memory operations
- SIMD pointer arithmetic where applicable
- Streaming decode with immediate application (current approach is near-optimal)

### 2. Object Uniquing (Largest Bottleneck for Packages)

Method instances and bindings must be matched with existing runtime objects or created if new. For Plots with ~62K objects, this takes ~12ms.

**Detailed breakdown for Plots:**

| Category | Count | % |
|----------|-------|---|
| MI already done | 36,767 | 59% |
| MI requiring lookup/insert | 25,217 | 41% |
| Bindings done | 6 | ~0% |
| Bindings lookup | 3 | ~0% |

**Per-lookup cost:** ~480ns per MethodInstance lookup/insertion via `jl_specializations_get_linfo`

**Current implementation:**

- Takes `m->writelock` for each insertion
- Lock-free lookup first, then locked retry + insert
- Hash lookup via `jl_smallintset_lookup`
- Allocates new MethodInstance via `jl_get_specialized`
- Inserts into method's specializations cache

**Potential optimizations:**

- **Save-side sorting by method pointer** (see below)
- Batch lock acquisition for MIs from the same method
- Pre-allocate MI objects in bulk before insertion

### 3. Type Uniquing (Large Packages)

The type uniquing phase ensures that types in the loaded image are unified with existing types in the runtime. For Plots with 49,413 types, this takes ~6ms.

**Detailed breakdown for Plots:**

| Category | Count | % |
|----------|-------|---|
| Already done (from deps) | 33,617 | 68% |
| Lookup found in cache | 558 | 1% |
| Must be new type | 9,951 | 20% |
| Lookup miss (created) | 14,865 | 30% |
| Singleton instances | 103 | 0.2% |

**Current implementation:**

- Holds `typecache_lock` for entire loop
- Calls `jl_lookup_cache_type_` which uses `typekey_hash` + linear probing
- Creates new datatype if not found via `jl_new_uninitialized_datatype`

**Potential optimizations:**

- Split into lock-free lookup phase + locked insertion phase
- 68% already-done rate suggests good dependency ordering

### 4. Method Activation (Packages with Many External Methods)

Packages that extend methods from other packages (external methods) have activation overhead. SparseArrays with 2,528 external methods takes 132ms to activate.

**Current implementation:** Sequential method table insertions with world age management.

**Potential optimizations:**

- Batch method table updates
- Deferred invalidation processing

## Data Sizes

Representative size breakdown for the system image:

| Component | Size |
|-----------|------|
| sys data | 118 MB |
| isbits data | 70 MB |
| reloc list | 7.9 MB |
| tags list | 1.9 MB |
| symbols | 0.5 MB |
| fptr list | 0.3 MB |
| gvar list | 0.08 MB |

## Instrumentation

To enable detailed timing output for package **loading**, uncomment `#define JL_DEBUG_LOADING` near the top of `src/staticdata.c` and rebuild Julia:

```c
// Define JL_DEBUG_LOADING to enable detailed timing of pkgimage loading
#define JL_DEBUG_LOADING
```

To enable detailed timing output for package cache **generation** (saving), uncomment `#define JL_DEBUG_SAVING`:

```c
// Define JL_DEBUG_SAVING to enable detailed timing of pkgimage generation
#define JL_DEBUG_SAVING
```

Then rebuild with `make -C src` and run:

```bash
# For loading timing
julia -e "using SomePackage"

# For saving timing (during precompilation)
julia -e "using Pkg; Pkg.precompile()"
```

This will print timing for each phase of image loading/saving, including detailed breakdowns of type and object uniquing operations. The instrumentation is compile-time guarded for zero overhead in release builds.

## Summary of Optimization Opportunities

### Highest Impact: Method Activation (63% of total time)

The `activate methods` phase dominates package loading time, accounting for 206ms out of ~330ms total for `using Plots`. A single package (SparseArrays) takes 139ms - 67% of all method activation time.

**Why SparseArrays is slow:** It extends Base methods (arithmetic operators, indexing, etc.) that have large method tables. Each external method requires:

- `get_intersect_matches` - find all intersecting methods in Base's method tables
- `jl_type_morespecific` checks for each intersection
- `jl_type_intersection2` checks for each MethodInstance
- Potential invalidation of existing compiled code

SparseArrays: 55μs per external method (2,528 methods)
Plots: 1.8μs per external method (2,388 methods)

The 30x difference comes from which methods are being extended - Base's core arithmetic has much larger method tables than plotting-specific methods.

**Potential optimizations:**

| Approach | Expected Impact | Complexity | Status |
|----------|-----------------|------------|--------|
| Batch method activation by method table | 10-30% | Medium | ❌ Rejected - sorting overhead |
| Parallel type intersection checks | 20-40% | High | Not tested |
| Pre-compute intersection flags at precompile | 10-20% | Medium | Not tested |
| Lazy method activation (on-demand) | Up to 100% for unused methods | High | Not tested |
| Skip invalidation during precompilation | 3% | Low | ✅ Implemented |

### Skip Invalidation During Precompilation (Implemented)

During incremental precompilation (`jl_generating_output() && jl_options.incremental`), method activation can skip the expensive invalidation checks because:

1. **No user-compiled code exists to invalidate** - all caches are being built fresh
2. **Dispatch correctness is maintained** - `dispatch_status` and `interferences` are still computed
3. **The final pkgimage captures correct state** - whatever we compute gets saved

**Implementation:** Added `skip_invalidation` flag in `jl_method_table_activate` (`src/gf.c`) that skips:

- Per-MethodInstance type intersection checks (`jl_type_intersection2`)
- Backedge invalidation (`_invalidate_dispatch_backedges`)
- Cache invalidation (`_typename_invalidate_backedges`, `invalidate_mt_cache`)
- Method table invalidation for replaced methods (`jl_method_table_invalidate`)

**Results:**

| Metric | Master | PR | Improvement |
|--------|--------|-----|-------------|
| GLMakie precompilation (247 deps) | 201s | 195s | **3% faster** |

The improvement is modest because the type intersection and morespecific checks (which we keep for dispatch correctness) dominate the activation cost. The invalidation loops we skip are relatively cheap.

### Further Opportunities (Not Yet Implemented)

The remaining expensive operations in method activation are:

1. **`get_intersect_matches`** - Finds all methods in Base that intersect with the new method's signature. Required to compute `dispatch_bits` and update other methods' `interferences`.

2. **`jl_type_morespecific` loop** - Called for each intersecting method to determine ambiguity and update dispatch optimization flags.

3. **Interference set updates** - Updates both the new method's `interferences` and existing methods' interference sets.

**Why these can't be skipped during precompilation:**

- **Interference sets are required for correct dispatch**, not just optimization. Skipping updates to other methods' interference sets causes `MethodError` during precompilation when dispatch relies on these sets to find the correct method.
- The `interferences` field is used in method sorting during dispatch to determine which methods should be considered.
- Even though Base methods aren't saved with the package, their interference sets must be correct during precompilation for any code that runs (e.g., `__init__`, type inference).

**Tested and rejected:** Skipping interference set updates during precompilation caused `MethodError` failures - dispatch couldn't find methods that should have matched.

**Potential future optimizations:**

| Approach | Expected Impact | Complexity | Risk |
|----------|-----------------|------------|------|
| Lazy interference computation (on first dispatch) | 10-30% | Very High | Requires dispatch changes |
| Parallel type intersection checks | 20-40% | High | Thread safety concerns |
| Cache intersection results across pkgimage loads | 5-15% | Medium | Memory overhead |

### Medium Impact: Apply Relocations (16%)

52ms for all packages. Currently sequential processing.

**Status:** Parallel attempt failed due to memory allocation overhead and cache effects.

**Remaining ideas:**

- SIMD pointer arithmetic
- Memory-mapped I/O for large images
- Streaming decode with prefetching

### Lower Impact: Uniquing Operations (12%)

Object uniquing: 24ms (7%)
Type uniquing: 15ms (5%)

**Tested and rejected:** Save-side sorting by method pointer made things 15% slower by destroying natural serialization locality.

**Remaining ideas:**

| Approach | Expected Impact | Complexity | Status |
|----------|-----------------|------------|--------|
| Type uniquing lock scope reduction | 2-6ms | Medium | Not tested |
| Reduce uniquing_objs count at precompile | Variable | High | Not tested |
| Pre-warm method specialization tables | 1-3ms | Low | Not tested |
| Sort uniquing_objs by method pointer | ~3ms | Low | ❌ Rejected |

### Lower Priority

| Target | Time | Notes |
|--------|------|-------|
| read symbols | 19ms | Could investigate compression |
| fixup objects | 9ms | Sequential, may benefit from batching |
| add methods | 5ms | Already fast |

## Rejected Optimizations

### Parallel Relocations

Attempted a two-phase approach: decode all relocation entries into a buffer, then apply in parallel using pthreads.

**Result:** Slower (45ms → 48ms) due to memory allocation overhead, thread creation/join cost, and cache effects from non-sequential access.

### Save-side Sorting of uniquing_objs

**Hypothesis:** Sorting MIs by method pointer during serialization would improve cache locality during load, keeping each method's specializations hash table hot in CPU cache.

**Implementation:** Added parallel arraylist to store method pointers, sorted (offset, method) pairs before writing.

**Results from `using Plots` benchmark:**

| Metric | Master | PR (sorted) | Diff |
|--------|--------|-------------|------|
| Object uniquing (Plots) | 12.1 ms | 13.9 ms | **+15% slower** |
| Total restore (Plots) | 33.9 ms | 36.7 ms | +8% slower |
| Total restore (all 123 pkgs) | 131.1 ms | 222.9 ms | **+70% slower** |
| mi_done / mi_lookup | 36767 / 25217 | 36767 / 25217 | Same |

**Why it failed:** The identical mi_done/mi_lookup ratios show that sorting doesn't change the "already done" cache hit rate. The sorting appears to have **destroyed** natural locality that existed in the original serialization order. Objects are serialized in traversal order, which likely groups related items together. Sorting by method pointer spreads out items that were naturally co-located.

**Lesson learned:** The serialization traversal order may already have good cache locality properties that shouldn't be disturbed.

### Batch Method Activation by Method Table

**Hypothesis:** Sorting external methods by their method table before activation would improve cache locality when accessing `mt->defs` for intersection checks.

**Implementation:** Added qsort to order entries by method table pointer before the activation loop.

**Result:** 10% slower than master. The sorting overhead outweighed any cache benefits, and likely destroyed beneficial natural ordering (methods are serialized in dependency order which may naturally group related activations).

**Lesson learned:** Same as above - the natural serialization order has good properties.

## Package Cache Generation (Precompilation) Analysis

In addition to loading performance, we analyzed package cache **generation** (precompilation) to identify optimization opportunities in the saving phase.

### Timing Breakdown for GLMakie Precompilation

| Phase | Time (s) | % of Total |
|-------|----------|------------|
| include (parse, lower, inference) | 24.6 | 46% |
| codegen (method gen & LLVM IR) | 14.9 | 28% |
| native code generation (3 threads) | 12.6 | 24% |
| serialize incremental image | 0.4 | <1% |
| GC collection | 0.7 | 1% |
| other | 0.5 | <1% |
| **TOTAL** | **53.7** | |

### Redundancy Analysis

We instrumented the codegen phase to detect any redundant compilation work.

Input to `jl_emit_native_impl`:

- 10,347 CodeInstances submitted for compilation
- 104 duplicates skipped (1%) - from multi-world compilation, handled correctly
- 2,530 const_return methods (no codegen needed)
- 7,710 methods actually compiled

Workqueue (call targets discovered during codegen):

- 9,970 items in workqueue
- 100% were already compiled (correct behavior - workqueue is for prototype patching, not recompilation)
- 7,142 duplicate references (72%) - same CodeInstance called from multiple sites

**Conclusion:** No significant redundant compilation work. The duplicates in the workqueue are expected - they represent multiple call sites that need their prototype declarations patched to point to the compiled function, not redundant compilation.

### Parallel Codegen Exploration

We explored parallelizing the codegen phase since it takes ~28% of precompilation time.

#### Approach 1: Per-thread LLVM Contexts

Each worker thread gets its own `LLVMContext` and `jl_codegen_params_t`:

```text
Main thread: partition work → spawn workers → collect modules → merge
```

**Result:** SEGFAULT in `ConstantPointerNull::get`

**Root cause:** Modules from different LLVM contexts cannot be merged or linked. The `Linker` expects all modules to share the same context. Cross-context Value references cause assertion failures.

#### Approach 2: Serialize/Deserialize Modules

Worker threads compile to separate contexts, serialize modules to bitcode, main thread deserializes into shared context:

```text
Worker: codegen → BitcodeWriter → serialize
Main: parseBitcodeFile → Linker::linkInModule
```

**Result:** Symbol not found errors on package load (`_jlplt_ijl_rethrow_38825_got`)

**Root cause:** `jl_codegen_params_t` accumulates state during codegen:

- `workqueue` - call targets for prototype patching
- `global_targets` - references to Julia values
- `external_fns` - external function references

When using per-thread params, this state is lost after serialization. The workqueue contains Function pointers valid only in the source context.

#### Approach 3: Shared Context with Mutex

All threads share a single `jl_codegen_params_t` with mutex protection:

**Result:** Deadlock

**Root cause:** `jl_codegen_params_t` acquires `tsctx_lock` (the ThreadSafeContext lock) in its constructor and holds it for its entire lifetime. Worker threads cannot acquire the same lock to call `jl_emit_codeinst`.

### Architectural Challenges for Parallel Codegen

The current codegen architecture has deep assumptions about single-threaded execution:

1. **LLVM Context Ownership:** `jl_codegen_params_t` owns a `ThreadSafeContext::Lock` that cannot be released and re-acquired
2. **Shared Mutable State:** Many maps and vectors (`workqueue`, `global_targets`, `external_fns`, `mergedConstants`) are updated during codegen
3. **Cross-Module References:** Generated functions reference globals and other functions that may be in different modules
4. **GC Integration:** `temporary_roots` must be managed carefully across threads

Potential future approaches:

| Approach | Expected Impact | Complexity | Challenges |
|----------|-----------------|------------|------------|
| Refactor `jl_codegen_params_t` for explicit lock management | Enable shared context | Very High | Requires careful analysis of all shared state |
| Pipeline parallelism (inference ‖ codegen ‖ LLVM opt) | Better utilization | High | Data dependencies, buffering |
| Batch codegen at module granularity | Coarser parallelism | Medium | Load balancing |
| Parallel LLVM optimization (already done) | ✅ Already parallelized | - | 3 threads used for native code gen |

**Current status:** Parallel codegen disabled. The native code generation phase already uses 3 threads (visible in debug output), so some parallelism exists in the later stages.

## Performance Characteristics

**Object uniquing rate:** ~5.2K objects/ms (~195ns per object average)

- 59% cache hit rate (already uniquified by earlier references)
- 41% require lookup/insertion via `jl_specializations_get_linfo`
- Per-lookup cost: ~480ns

**Type uniquing rate:** ~8.7K types/ms (~115ns per type average)

- 68% cache hit rate (already uniquified by dependencies)
- 1% found via cache lookup
- 31% new types requiring creation

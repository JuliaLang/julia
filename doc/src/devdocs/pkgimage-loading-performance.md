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
| object uniquing (MI, bindings) | 13.3 | 38% |
| apply relocations | 7.8 | 22% |
| type uniquing & caching | 5.8 | 17% |
| fixup objects | 4.2 | 12% |
| read symbols | 2.0 | 6% |
| other | 1.9 | 5% |
| **TOTAL restore** | **35.0** | |
| activate methods | 4.3 | (post-restore) |

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

Method instances and bindings must be matched with existing runtime objects or created if new. For Plots with ~62K objects, this takes ~13-15ms.

**Detailed breakdown for Plots:**

| Category | Count | % |
|----------|-------|---|
| MI already done | 36,766 | 59% |
| MI requiring lookup/insert | 25,217 | 41% |
| Bindings done | 6 | ~0% |
| Bindings lookup | 3 | ~0% |

**Per-lookup cost:** ~530ns per MethodInstance lookup/insertion via `jl_specializations_get_linfo`

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

To enable detailed timing output, set the environment variable:

```bash
JULIA_DEBUG_LOADING=1 julia -e "using SomePackage"
```

This will print timing for each phase of image loading, including detailed breakdowns of type and object uniquing operations.

## Summary of Optimization Opportunities

| Priority | Target | Expected Impact | Complexity | Status |
|----------|--------|-----------------|------------|--------|
| High | Object uniquing (MI sorting) | ~3-4ms for large pkgs | Low | Tested - promising |
| High | Type uniquing lock scope | 2-6ms for large pkgs | Medium | Analysis complete |
| Medium | Parallel relocations | Tested - not beneficial | Medium | ❌ Rejected |
| Medium | Method activation batching | 1-130ms for method-heavy pkgs | Medium | Not started |
| Low | Symbol reading optimization | 1-2ms for large pkgs | Low | Not started |

### Tested Optimizations

#### Save-side sorting of uniquing_objs by method pointer

**Approach:** During pkgimage serialization, sort the `uniquing_objs` list by method pointer before writing. This requires:

1. Add a parallel arraylist `uniquing_objs_methods` to store method pointers alongside offsets
2. In `record_uniquing`, store the method pointer for each MI (NULL for bindings)
3. Before writing `uniquing_objs`, build an array of (offset, method) pairs, sort by method, then write just the offsets

**Implementation details:**

- Add `uniquing_obj_entry_t` struct with offset and method pointer
- Add `uniquing_obj_entry_cmp` comparator function
- Modify `record_uniquing` to also push to `uniquing_objs_methods`
- Before `jl_write_arraylist` for uniquing_objs, sort and reorder

**Results:** ~24% reduction in object uniquing time (14.9ms → 11.3ms for Plots). The improvement comes from better cache locality - consecutive MIs for the same method keep the method's specializations hash table hot in CPU cache.

**Trade-offs:**

- Adds ~10-20ms to pkgimage save time (one-time cost during precompilation)
- Increases serialization memory usage slightly

### Performance Characteristics

**Object uniquing rate:** ~4.1K objects/ms (~240ns per object)

- 59% cache hit rate (already uniquified by earlier references)
- 41% require lookup/insertion via `jl_specializations_get_linfo`

**Type uniquing rate:** ~8.2K types/ms (~122ns per type)

- 68% cache hit rate (already uniquified by dependencies)
- 1% found via cache lookup
- 31% new types requiring creation


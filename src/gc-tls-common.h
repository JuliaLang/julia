// This file is a part of Julia. License is MIT: https://julialang.org/license

// Meant to be included in "julia_threads.h"
#ifndef JL_GC_TLS_COMMON_H
#define JL_GC_TLS_COMMON_H

#include "julia_atomics.h"

// GC threading ------------------------------------------------------------------

#include "arraylist.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    // variable for tracking weak references
    small_arraylist_t weak_refs;
    // Only used by MMTk builds: registry of objects requiring weak
    // processing on death (see jl_gc_set_needs_weak_processing) -
    // currently cancellation token sources sitting on some parent's (weak,
    // intrusive) child list. MMTk never visits individual dead objects, so
    // it detects dead ones by scanning this flat registry after marking
    // (see jl_gc_sweep_weak_processing). The stock GC needs no registry:
    // its sweep visits every dead object's header anyway and detects them
    // there, gated by a per-page flag.
    small_arraylist_t weak_processing_list;
    // live tasks started on this thread
    // that are holding onto a stack from the pool
    small_arraylist_t live_tasks;

    // variable for tracking malloc'd arrays
    small_arraylist_t mallocarrays;

#define JL_N_STACK_POOLS 16
    small_arraylist_t free_stacks[JL_N_STACK_POOLS];
} jl_thread_heap_common_t;

typedef struct {
    _Atomic(int64_t) allocd;
    _Atomic(int64_t) pool_live_bytes;
    _Atomic(uint64_t) malloc;
    _Atomic(uint64_t) realloc;
    _Atomic(uint64_t) poolalloc;
    _Atomic(uint64_t) bigalloc;
    _Atomic(int64_t) free_acc;
    _Atomic(uint64_t) alloc_acc;
} jl_thread_gc_num_common_t;

typedef struct {
    jl_thread_heap_common_t heap;
    jl_thread_gc_num_common_t gc_num;
} jl_gc_tls_states_common_t;

#ifdef __cplusplus
}
#endif

#endif // JL_GC_TLS_COMMON_H

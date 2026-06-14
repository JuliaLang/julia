// This file is a part of Julia. License is MIT: https://julialang.org/license

// Tiered-compilation prototype.
//
// This file provides the runtime-side scaffolding for a tiered compilation model:
//   * a per-CodeInstance call counter (hotness profiling), stored in a side-table
//     keyed by the CodeInstance pointer so that no `jl_code_instance_t` layout or
//     serialization change is required (modeled on src/coverage.c), and
//   * `jl_tier_swap_target`, which performs next-call tier replacement by
//     installing the compiled entry points of a freshly produced (higher-tier)
//     CodeInstance onto a live one.
//
// All behavior is gated behind `jl_tier_enabled()`, which is initialized once at
// startup from the `JULIA_TIER` environment variable and is off by default, so a
// normal build/run is unaffected. Transition logging is gated behind
// `JULIA_TIER_LOG`.

#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>
#include <pthread.h>

#include "julia.h"
#include "julia_internal.h"
#include "support/htable.h"
#include "support/ptrhash.h"

#ifdef __cplusplus
extern "C" {
#endif

// Set once in jl_init_tiered(); read without locking by codegen (via jl_tier_enabled())
// and the helpers below.
static int tiered_enabled = 0;
static int jl_tier_log_enabled = 0;

// Accessor so codegen (in the separate libjulia-codegen) can read the gate without
// relying on a cross-library data symbol.
JL_DLLEXPORT int jl_tier_enabled(void) JL_NOTSAFEPOINT
{
    return tiered_enabled;
}

// jl_code_instance_t* -> uint64_t* (heap-allocated counter slot)
static htable_t tier_counters;
static pthread_mutex_t tier_lock = PTHREAD_MUTEX_INITIALIZER;

void jl_init_tiered(void) JL_NOTSAFEPOINT
{
    htable_new(&tier_counters, 0);
    char *e = getenv("JULIA_TIER");
    tiered_enabled = (e != NULL && *e != '\0' && strcmp(e, "0") != 0);
    char *l = getenv("JULIA_TIER_LOG");
    jl_tier_log_enabled = (l != NULL && *l != '\0');
}

// Return a stable pointer to the 64-bit call counter for `ci`, lazily allocating
// it on first use. The address is stable for the lifetime of the process, which
// lets codegen bake it into the function prologue (see emit_function).
JL_DLLEXPORT uint64_t *jl_tier_counter_pointer(jl_code_instance_t *ci) JL_NOTSAFEPOINT
{
    pthread_mutex_lock(&tier_lock);
    void **bp = ptrhash_bp(&tier_counters, ci);
    if (*bp == HT_NOTFOUND) {
        uint64_t *slot = (uint64_t*)calloc(1, sizeof(uint64_t));
        *bp = slot;
    }
    uint64_t *ret = (uint64_t*)*bp;
    pthread_mutex_unlock(&tier_lock);
    return ret;
}

// Current observed call count for `ci` (0 if never instrumented/called).
JL_DLLEXPORT uint64_t jl_tier_callcount(jl_code_instance_t *ci) JL_NOTSAFEPOINT
{
    pthread_mutex_lock(&tier_lock);
    void *v = ptrhash_get(&tier_counters, ci);
    uint64_t ret = (v == HT_NOTFOUND) ? 0 : *(uint64_t*)v;
    pthread_mutex_unlock(&tier_lock);
    return ret;
}

// Next-call tier replacement: copy the compiled entry points of `new_ci` (a
// higher-tier CodeInstance, possibly under a different compiler `owner`) onto the
// live `old_ci`, so that subsequent dispatch through `old_ci->invoke` runs the
// new code. The caller must keep `new_ci` alive for as long as `old_ci` is in use
// (its JIT-compiled code backs the installed pointers).
//
// The store ordering mirrors copy_to_mi_cache()/jitlayers.cpp: publish specptr
// first, then the invoke pointer, then the invoke-matches-specptr flag.
//
// PoC limitation: this assumes `old_ci` is not being executed concurrently on
// another thread during the swap (true for the manual `promote_tier!` entry).
// A production implementation would use a trampoline indirection (the
// jl_fptr_wait_for_compiled pattern) or on-stack replacement for a fully safe,
// concurrent swap.
JL_DLLEXPORT void jl_tier_swap_target(jl_code_instance_t *old_ci, jl_code_instance_t *new_ci)
{
    uint8_t specsigflags;
    jl_callptr_t invoke;
    void *fptr;
    // waitcompile=1: forces new_ci to be compiled if it was only queued.
    jl_read_codeinst_invoke(new_ci, &specsigflags, &invoke, &fptr, 1);
    if (invoke == NULL)
        return;

    uint64_t calls = jl_tier_callcount(old_ci);

    if (fptr != NULL) {
        jl_atomic_store_release(&old_ci->specptr.fptr, fptr);
        if (specsigflags & JL_CI_FLAGS_SPECPTR_SPECIALIZED)
            jl_atomic_fetch_or_relaxed(&old_ci->flags, JL_CI_FLAGS_SPECPTR_SPECIALIZED);
        jl_atomic_store_release(&old_ci->invoke, invoke);
        jl_atomic_fetch_or_relaxed(&old_ci->flags, JL_CI_FLAGS_INVOKE_MATCHES_SPECPTR);
    }
    else {
        jl_atomic_store_release(&old_ci->invoke, invoke);
    }

    if (jl_tier_log_enabled) {
        jl_method_instance_t *mi = jl_get_ci_mi(old_ci);
        jl_printf(JL_STDERR, "[tier] promote ");
        jl_static_show(JL_STDERR, (jl_value_t*)mi);
        jl_printf(JL_STDERR, " -> owner ");
        jl_static_show(JL_STDERR, new_ci->owner);
        jl_printf(JL_STDERR, " after %" PRIu64 " calls\n", calls);
    }
}

#ifdef __cplusplus
}
#endif

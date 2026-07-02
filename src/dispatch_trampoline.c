// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "julia.h"
#include "julia_internal.h"

// Process-global cache backing `jl_dispatch_trampolines` (the `Core.dispatch_trampolines`
// singleton). See the section comment below for the cache structure.

// ---- dispatch-trampoline cache (@cfunction/@ccallable) ----
// Maps (sigt, rt) -> jl_dispatch_trampoline_t, keyed on the resolution sig `sigt` = `Tuple{typeof(f),
// A...}` alone. Records sharing a `sigt` (differing only in `rt`) are chained through
// `jl_dispatch_trampoline_t.next` and disambiguated by `rt`.

static jl_dispatch_trampoline_t *tramp_alloc_entry(jl_task_t *ct, jl_value_t *sigt, jl_value_t *rt,
                                          int specsig)
{
    jl_dispatch_trampoline_t *e = (jl_dispatch_trampoline_t*)jl_gc_alloc(ct->ptls, sizeof(jl_dispatch_trampoline_t), jl_dispatch_trampoline_type);
    e->sigt = sigt;
    e->rt = rt;
    // last_invoked and next are GC-traced fields; jl_gc_alloc does not zero, so they must be
    // initialized (NULL = unresolved / chain tail) or the GC will trace uninitialized memory.
    e->last_invoked = NULL;
    jl_atomic_store_relaxed(&e->fptr, (void*)NULL);
    jl_atomic_store_relaxed(&e->last_world, (size_t)0);
    jl_atomic_store_relaxed(&e->next, (jl_dispatch_trampoline_t*)NULL);
    e->specsig = specsig ? 1 : 0;
    return e;
}

// Match a record against `rt` within a `sigt` bucket. `rt` is compared by *type equality*
// (jl_types_equal), matching how the TypeMap matches `sigt`; `jl_egal` would be too strict,
// splitting type-equal-but-not-egal return types into duplicate records. `specsig` is a pure
// function of (sigt, rt), so it is carried on the record rather than matched here. Not
// NOTSAFEPOINT: jl_types_equal may allocate/safepoint (the chain is append-only and rooted).
static int tramp_matches(jl_dispatch_trampoline_t *e, jl_value_t *rt)
{
    return e->rt == rt || jl_types_equal(e->rt, rt);
}

// Walk a `sigt` bucket's `.next` chain (acquire) for a matching record. Returns NULL if none.
static jl_dispatch_trampoline_t *tramp_chain_find(jl_dispatch_trampoline_t *e, jl_value_t *rt)
{
    for (; e != NULL; e = jl_atomic_load_acquire(&e->next)) {
        JL_GC_PROMISE_ROOTED(e); // chain node rooted by the (append-only) cache TypeMap
        if (tramp_matches(e, rt))
            return e;
    }
    return NULL;
}

// Lock-free lookup of the trampoline for (sigt, rt). assoc_by_type with subtype=0 is an
// *exact* match on `sigt` (like jl_methtable_lookup); `rt` is disambiguated in the bucket.
// Returns NULL if absent. Safe to call with or without the writelock held.
static jl_dispatch_trampoline_t *tramp_map_lookup(jl_value_t *sigt, jl_value_t *rt)
{
    jl_typemap_t *map = jl_atomic_load_relaxed(&jl_dispatch_trampolines->cache);
    if ((jl_value_t*)map == jl_nothing)
        return NULL;
    struct jl_typemap_assoc search = { sigt, jl_atomic_load_acquire(&jl_world_counter), NULL };
    jl_typemap_entry_t *entry = jl_typemap_assoc_by_type(map, &search, /*offs*/0, /*subtype*/0);
    if (entry == NULL)
        return NULL;
    jl_dispatch_trampoline_t *head = (jl_dispatch_trampoline_t*)jl_atomic_load_acquire((_Atomic(jl_value_t*)*)&entry->func.value);
    return tramp_chain_find(head, rt);
}

// Insert `tr` into the `sigt`-keyed bucket. Caller holds the writelock and must have confirmed
// (under the lock) that (sigt, rt) is absent; `sigt`/`tr` must be kept rooted. The TypeMap
// entry (when first created) is valid in all worlds (min=1, max=~0).
static void tramp_map_insert(jl_value_t *sigt, jl_dispatch_trampoline_t *tr)
{
    jl_typemap_t *map = jl_atomic_load_relaxed(&jl_dispatch_trampolines->cache);
    jl_typemap_entry_t *te = NULL;
    if ((jl_value_t*)map != jl_nothing) {
        struct jl_typemap_assoc search = { sigt, jl_atomic_load_acquire(&jl_world_counter), NULL };
        te = jl_typemap_assoc_by_type(map, &search, /*offs*/0, /*subtype*/0);
    }
    if (te == NULL) {
        // first record for this sigt: a TypeMap entry whose value is the record (a chain of
        // one). `next` was initialized to NULL by tramp_alloc_entry.
        jl_typemap_entry_t *newentry = jl_typemap_alloc(
            (jl_tupletype_t*)sigt, NULL, jl_emptysvec, (jl_value_t*)tr, 1, ~(size_t)0);
        JL_GC_PUSH1(&newentry);
        jl_typemap_insert(&jl_dispatch_trampolines->cache, (jl_value_t*)jl_dispatch_trampolines, newentry, /*offs*/0);
        JL_GC_POP();
        return;
    }
    // append at the tail; the head (`func.value`) never moves, so readers walk it lock-free.
    jl_dispatch_trampoline_t *tail = (jl_dispatch_trampoline_t*)jl_atomic_load_relaxed((_Atomic(jl_value_t*)*)&te->func.value);
    for (;;) {
        jl_dispatch_trampoline_t *n = jl_atomic_load_relaxed(&tail->next);
        if (n == NULL)
            break;
        tail = n;
    }
    jl_atomic_store_release(&tail->next, tr);
    jl_gc_wb(tail, tr);
}

// Intern (or look up) the shared dispatch trampoline record for (sigt, rt).
static jl_dispatch_trampoline_t *jl_get_trampoline(jl_task_t *ct, jl_value_t *sigt, jl_value_t *rt,
                                          int specsig)
{
    jl_dispatch_trampoline_t *e = NULL;
    JL_GC_PUSH1(&e);
    e = tramp_map_lookup(sigt, rt); // lock-free fast path
    if (e == NULL) {
        JL_LOCK(&jl_dispatch_trampolines->writelock);
        e = tramp_map_lookup(sigt, rt); // re-check: another thread may have inserted
        if (e == NULL) {
            e = tramp_alloc_entry(ct, sigt, rt, specsig);
            tramp_map_insert(sigt, e);
        }
        JL_UNLOCK(&jl_dispatch_trampolines->writelock);
    }
    JL_GC_POP();
    return e;
}

// Intern (or look up) the @cfunction/@ccallable dispatch trampoline for (sigt, rt). Several
// call sites to the same target (e.g. juliac's two `_main` entries) share one interned
// trampoline -- and thus one AOT-compiled adapter / fvar slot, which the 1:1 fptr_record wiring
// requires. Exported because codegen (emit_abi_call) lives in libjulia-codegen and cannot
// inline the allocator. `specsig` is the C wrapper's `uses_specsig` choice (a pure function
// of (sigt, rt), so consistent across call sites); caller must root `sigt`/`rt`.
JL_DLLEXPORT jl_dispatch_trampoline_t *jl_new_cfunction_trampoline(jl_value_t *sigt, jl_value_t *rt, int specsig)
{
    return jl_get_trampoline(jl_current_task, sigt, rt, specsig);
}

// Re-intern a trampoline restored from an image into the running cache, so a later
// construction with the same (sigt, rt) shares it. `tr->next` was reset to NULL at
// serialization; rebuilding the `sigt` bucket here re-links it. If an equivalent (sigt, rt)
// is already present (e.g. from a previously-loaded image) leave `tr` standalone rather than
// displacing the canonical entry. Called from the staticdata load fixup.
JL_DLLEXPORT void jl_reintern_trampoline(jl_dispatch_trampoline_t *tr)
{
    JL_GC_PUSH1(&tr);
    JL_LOCK(&jl_dispatch_trampolines->writelock);
    if (tramp_map_lookup(tr->sigt, tr->rt) == NULL)
        tramp_map_insert(tr->sigt, tr);
    JL_UNLOCK(&jl_dispatch_trampolines->writelock);
    JL_GC_POP();
}

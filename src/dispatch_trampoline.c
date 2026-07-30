// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "julia.h"
#include "julia_internal.h"

// ---- dispatch-trampoline cache (invokelatest trampoline) ----
// Maps (sigt, rt, specsig, kind) -> jl_dispatch_trampoline_t, keyed on the dispatch sig
// `sigt` = `Tuple{typeof(f), A...}` and located by (rt, specsig, kind) within each bucket

static jl_dispatch_trampoline_t *trampoline_alloc_entry(jl_task_t *ct, jl_value_t *sigt, jl_value_t *rt,
                                          int specsig, jl_abi_kind_t kind) JL_CANSAFEPOINT
{
    jl_dispatch_trampoline_t *e = (jl_dispatch_trampoline_t*)jl_gc_alloc(ct->ptls, sizeof(jl_dispatch_trampoline_t), jl_dispatch_trampoline_type);
    e->sigt = sigt;
    e->rt = rt;
    e->last_invokee = NULL; // unresolved
    jl_atomic_store_relaxed(&e->fptr, (void*)NULL);
    jl_atomic_store_relaxed(&e->last_world, (size_t)0);
    jl_atomic_store_relaxed(&e->next, (jl_dispatch_trampoline_t*)NULL);
    e->specsig = specsig ? 1 : 0;
    e->kind = (uint8_t)kind;
    return e;
}

typedef struct {
    jl_value_t *rt;
    int specsig;
    jl_abi_kind_t kind;
} trampoline_key_t;

static int trampoline_match(jl_value_t *item, void *keyv) JL_CANSAFEPOINT
{
    jl_dispatch_trampoline_t *e = (jl_dispatch_trampoline_t*)item;
    trampoline_key_t *k = (trampoline_key_t*)keyv;
    return (int)e->specsig == (k->specsig ? 1 : 0)
        && (jl_abi_kind_t)e->kind == k->kind
        && (e->rt == k->rt || jl_types_equal(e->rt, k->rt));
}

// `rt` is compared by type-equality (no sound hash) and buckets stay tiny, so use a
// constant hash and keep plain lists at any size.
static uintptr_t trampoline_hash(jl_value_t *item) JL_NOTSAFEPOINT
{
    (void)item;
    return 0;
}

static const jl_typemap_list_config_t dispatch_trampoline_cache_config = {
    offsetof(jl_dispatch_trampoline_t, next),
    JL_TYPEMAP_LIST_NO_HASHMAP,
    trampoline_hash,
    trampoline_match,
};

JL_DLLEXPORT void jl_reinit_dispatch_trampoline_cache(jl_dispatch_trampoline_cache_t *c) JL_NOTSAFEPOINT
{
    c->cache.config = &dispatch_trampoline_cache_config;
    JL_MUTEX_INIT(&c->writelock, "jl_dispatch_trampolines->writelock");
}

// Fallible lock-free lookup; NULL indicates absence - or a concurrent write
static jl_dispatch_trampoline_t *trampoline_map_lookup(jl_value_t *sigt, jl_value_t *rt, int specsig, jl_abi_kind_t kind) JL_CANSAFEPOINT
{
    trampoline_key_t key = { rt, specsig, kind };
    return (jl_dispatch_trampoline_t*)jl_typemap_list_lookup(&jl_dispatch_trampolines->cache,
            sigt, /*hash*/0, &key);
}

// Caller holds the writelock and has confirmed that `tr` is absent.
static void trampoline_map_insert(jl_value_t *sigt, jl_dispatch_trampoline_t *tr) JL_CANSAFEPOINT
{
    jl_typemap_list_insert(&jl_dispatch_trampolines->cache, (jl_value_t*)jl_dispatch_trampolines,
            sigt, (jl_value_t*)tr);
}

// Return the canonical trampoline for (sigt, rt, specsig, kind).
JL_DLLEXPORT jl_dispatch_trampoline_t *jl_get_dispatch_trampoline(jl_value_t *sigt, jl_value_t *rt, int specsig, jl_abi_kind_t kind) JL_CANSAFEPOINT
{
    jl_dispatch_trampoline_t *e = NULL;
    JL_GC_PUSH1(&e);
    e = trampoline_map_lookup(sigt, rt, specsig, kind);
    if (e == NULL) {
        JL_LOCK(&jl_dispatch_trampolines->writelock);
        e = trampoline_map_lookup(sigt, rt, specsig, kind);
        if (e == NULL) {
            e = trampoline_alloc_entry(jl_current_task, sigt, rt, specsig, kind);
            trampoline_map_insert(sigt, e);
        }
        JL_UNLOCK(&jl_dispatch_trampolines->writelock);
    }
    JL_GC_POP();
    return e;
}

// Reinsert a DispatchTrampoline into the cache, unless an equivalent is already present.
JL_DLLEXPORT jl_dispatch_trampoline_t *jl_insert_dispatch_trampoline(jl_dispatch_trampoline_t *tr) JL_CANSAFEPOINT
{
    jl_dispatch_trampoline_t *e = NULL;
    JL_GC_PUSH2(&tr, &e);
    JL_LOCK(&jl_dispatch_trampolines->writelock);
    e = trampoline_map_lookup(tr->sigt, tr->rt, tr->specsig, (jl_abi_kind_t)tr->kind);
    if (e == NULL) {
        trampoline_map_insert(tr->sigt, tr);
        e = tr;
    }
    JL_UNLOCK(&jl_dispatch_trampolines->writelock);
    JL_GC_POP();
    return e;
}

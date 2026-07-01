// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "julia.h"
#include "julia_internal.h"

// A jl_typemap_list_t cache keyed by `sigt`; ABIAdapters within a sigt bucket are
// distinguished by (rt, ci, specsig, kind) and hash-indexed once a bucket grows.
// An optimistic lookup miss is definitive only under the writelock.

#define MAX_ADAPTER_LIST_COUNT 6 // mirror MAX_METHLIST_COUNT: hash-index buckets above this

typedef struct {
    jl_value_t *rt;
    jl_code_instance_t *ci;
    int specsig;
    jl_abi_kind_t kind;
} abi_adapter_key_t;

// Match `rt` by type equality, as the TypeMap does for `sigt`.
static int abi_adapter_match(jl_value_t *item, void *keyv) JL_CANSAFEPOINT
{
    jl_abi_adapter_t *e = (jl_abi_adapter_t*)item;
    abi_adapter_key_t *k = (abi_adapter_key_t*)keyv;
    return e->ci == k->ci
        && (int)e->specsig == (k->specsig ? 1 : 0)
        && (jl_abi_kind_t)e->kind == k->kind
        && (e->rt == k->rt || jl_types_equal(e->rt, k->rt)); // rt lives in the bucket (key is sigt alone)
}

static uintptr_t abi_adapter_hash_key(jl_code_instance_t *ci, int specsig, jl_abi_kind_t kind) JL_NOTSAFEPOINT
{
    // rt is intentionally omitted from this hash (type hashes have many bugs / limitations) but
    // ci dominates the fan-out anyway since we expect many callee CIs with the same (effectively
    // type-erased) `sigt` in their `from_abi`
    return int64hash((uintptr_t)ci ^ ((uintptr_t)(specsig ? 1 : 0) << 8) ^ (uintptr_t)kind);
}

static uintptr_t abi_adapter_hash(jl_value_t *item) JL_NOTSAFEPOINT
{
    jl_abi_adapter_t *e = (jl_abi_adapter_t*)item;
    return abi_adapter_hash_key(e->ci, e->specsig, (jl_abi_kind_t)e->kind);
}

static const jl_typemap_list_config_t abi_adapter_cache_config = {
    offsetof(jl_abi_adapter_t, next),
    MAX_ADAPTER_LIST_COUNT,
    abi_adapter_hash,
    abi_adapter_match,
};

JL_DLLEXPORT void jl_reinit_abi_adapter_cache(jl_abi_adapter_cache_t *c) JL_NOTSAFEPOINT
{
    c->cache.config = &abi_adapter_cache_config;
    JL_MUTEX_INIT(&c->writelock, "jl_abi_adapters->writelock");
}

// Return whether the target entry point already satisfies `from_abi`.
JL_DLLEXPORT int jl_abi_matches_invoke_api(jl_abi_t from_abi, jl_invoke_api_t api,
        jl_method_instance_t *mi, jl_value_t *rettype) JL_CANSAFEPOINT
{
    // Non-standard kinds require their own frame conversion.
    if (from_abi.kind != JL_ABI_STD)
        return 0;
    if (api == JL_INVOKE_ARGS)
        return !from_abi.specsig && jl_subtype(rettype, from_abi.rt);
    if (api == JL_INVOKE_SPECSIG)
        return from_abi.specsig && jl_egal(mi->specTypes, from_abi.sigt) && jl_egal(rettype, from_abi.rt);
    return 0;
}

// Return a directly compatible specptr, or describe the target to create an adapter for.
static void *abi_adapter_resolve_target(jl_abi_t from_abi, jl_code_instance_t *codeinst,
        void **target, int *target_specsig, jl_callptr_t *invoke) JL_CANSAFEPOINT
{
    *target = NULL;
    *target_specsig = 0;
    *invoke = NULL;
    if (codeinst == NULL)
        return NULL;
    uint8_t specsigflags;
    jl_method_instance_t *mi = jl_get_ci_mi(codeinst);
    void *specptr = NULL;
    jl_callptr_t invoke_ = NULL;
    jl_read_codeinst_invoke(codeinst, &specsigflags, &invoke_, &specptr, /* waitcompile */ 0);
    *invoke = invoke_;
    if (invoke_ == NULL)
        return NULL;
    if (invoke_ == jl_fptr_const_return_addr) {
        return NULL;
    }
    else if (invoke_ == jl_fptr_args_addr) {
        assert(specptr != NULL);
        if (jl_abi_matches_invoke_api(from_abi, JL_INVOKE_ARGS, mi, codeinst->rettype))
            return specptr; // no adapter required
        *target = specptr;
        *target_specsig = 0;
    }
    else if (specsigflags & JL_CI_FLAGS_SPECPTR_SPECIALIZED) {
        assert(specptr != NULL);
        if (jl_abi_matches_invoke_api(from_abi, JL_INVOKE_SPECSIG, mi, codeinst->rettype))
            return specptr; // no adapter required
        *target = specptr;
        *target_specsig = 1;
    }
    return NULL;
}

// Return the target's directly compatible specptr (for a from_abi call), if any.
JL_DLLEXPORT void *jl_abi_matching_specptr(jl_abi_t from_abi, jl_code_instance_t *codeinst) JL_CANSAFEPOINT
{
    void *target;
    int target_specsig;
    jl_callptr_t invoke;
    return abi_adapter_resolve_target(from_abi, codeinst, &target, &target_specsig, &invoke);
}

// Fallible lock-free lookup; NULL indicates absence - or a concurrent write
static jl_abi_adapter_t *abi_adapter_lookup(jl_value_t *sigt, jl_value_t *rt,
        jl_code_instance_t *ci, int specsig, jl_abi_kind_t kind) JL_CANSAFEPOINT
{
    abi_adapter_key_t key = { rt, ci, specsig, kind };
    return (jl_abi_adapter_t*)jl_typemap_list_lookup(&jl_abi_adapters->cache, sigt,
            abi_adapter_hash_key(ci, specsig, kind), &key);
}

// Return a matching target entry point or cached adapter without compiling.
// On a miss, the optional target outputs describe the adapter to compile.
JL_DLLEXPORT void *jl_lookup_abi_adapter(jl_abi_t from_abi, jl_code_instance_t *codeinst,
        void **target, int *target_specsig, jl_callptr_t *invoke, jl_value_t **invokee) JL_CANSAFEPOINT
{
    void *tgt = NULL;
    int ts = 0;
    jl_callptr_t invoke_ = NULL;
    if (invokee)
        *invokee = NULL;
    void *shortcut = abi_adapter_resolve_target(from_abi, codeinst, &tgt, &ts, &invoke_);
    if (target)
        *target = tgt;
    if (target_specsig)
        *target_specsig = ts;
    if (invoke)
        *invoke = invoke_;
    if (shortcut != NULL) {
        if (invokee)
            *invokee = (jl_value_t*)codeinst; // bare CI: no ABIAdapter required
        return shortcut;
    }
    jl_abi_adapter_t *e = abi_adapter_lookup(from_abi.sigt, from_abi.rt, codeinst,
            from_abi.specsig, from_abi.kind);
    if (e == NULL) {
        JL_LOCK(&jl_abi_adapters->writelock);
        e = abi_adapter_lookup(from_abi.sigt, from_abi.rt, codeinst,
                from_abi.specsig, from_abi.kind);
        JL_UNLOCK(&jl_abi_adapters->writelock);
    }
    if (e == NULL)
        return NULL;
    // Cached ABIAdapters always carry a valid fptr, set before publication.
    void *f = jl_atomic_load_relaxed(&e->fptr);
    assert(f != NULL);
    if (invokee)
        *invokee = (jl_value_t*)e;
    return f;
}

// Caller holds the writelock and has confirmed that `entry` is absent.
static void abi_adapter_insert(jl_abi_adapter_t *entry) JL_CANSAFEPOINT
{
    jl_typemap_list_insert(&jl_abi_adapters->cache, (jl_value_t*)jl_abi_adapters,
            entry->sigt, (jl_value_t*)entry);
}

// Allocate a detached ABIAdapter; the caller roots its key fields.
JL_DLLEXPORT jl_abi_adapter_t *jl_new_abi_adapter(jl_value_t *sigt, jl_value_t *rt,
        jl_code_instance_t *ci, int specsig, jl_abi_kind_t kind, void *fptr) JL_CANSAFEPOINT
{
    jl_task_t *ct = jl_current_task;
    jl_abi_adapter_t *e = (jl_abi_adapter_t*)jl_gc_alloc(ct->ptls, sizeof(jl_abi_adapter_t), jl_abi_adapter_type);
    e->sigt = sigt;
    e->rt = rt;
    e->specsig = specsig ? 1 : 0;
    e->kind = kind;
    e->ci = ci;
    jl_atomic_store_relaxed(&e->fptr, fptr);
    jl_atomic_store_relaxed(&e->next, (jl_abi_adapter_t*)NULL);
    return e;
}

// Publish `fptr`, returning the winner if the key is already cached.
JL_DLLEXPORT void *jl_insert_abi_adapter(jl_abi_t from_abi, jl_code_instance_t *codeinst,
        void *fptr, jl_value_t **invokee) JL_CANSAFEPOINT
{
    assert(fptr != NULL);
    jl_abi_adapter_t *e = NULL;
    JL_GC_PUSH1(&e);
    JL_LOCK(&jl_abi_adapters->writelock);
    e = abi_adapter_lookup(from_abi.sigt, from_abi.rt, codeinst,
            from_abi.specsig, from_abi.kind);
    if (e != NULL) {
        // Cached ABIAdapters always carry a valid fptr; keep the winner.
        fptr = jl_atomic_load_relaxed(&e->fptr);
        assert(fptr != NULL);
    }
    else {
        e = jl_new_abi_adapter(from_abi.sigt, from_abi.rt, codeinst,
                from_abi.specsig, from_abi.kind, fptr);
        abi_adapter_insert(e);
    }
    if (invokee)
        *invokee = (jl_value_t*)e;
    JL_UNLOCK(&jl_abi_adapters->writelock);
    JL_GC_POP();
    return fptr;
}

// Reinsert an ABIAdapter into the cache, unless an equivalent is already present.
JL_DLLEXPORT jl_abi_adapter_t *jl_reinsert_abi_adapter(jl_abi_adapter_t *e) JL_CANSAFEPOINT
{
    assert(jl_atomic_load_relaxed(&e->fptr) != NULL);
    jl_abi_adapter_t *canonical = NULL;
    JL_GC_PUSH2(&e, &canonical);
    JL_LOCK(&jl_abi_adapters->writelock);
    canonical = abi_adapter_lookup(e->sigt, e->rt, e->ci, e->specsig, (jl_abi_kind_t)e->kind);
    if (canonical == NULL) {
        abi_adapter_insert(e);
        canonical = e;
    }
    JL_UNLOCK(&jl_abi_adapters->writelock);
    JL_GC_POP();
    return canonical;
}

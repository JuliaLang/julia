// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "julia.h"
#include "julia_internal.h"

// Process-global cache backing `jl_abi_adapters` (the `Core.abi_adapters` singleton).
// See the section comment below for the cache / bucket structure.

// ---- ABI adapter cache (the `adapters` sub-table of jl_abi_adapters) ----
// Sibling of the trampoline cache above: a TypeMap keyed on the adapter `sigt` alone (its
// slot 0 is the function type, never `Union{}`). Each TypeMap entry holds a *bucket* of
// jl_abi_adapter_t records that share one `sigt` but differ in (rt, ci, specsig,
// is_opaque_closure, nargs). Because an adapter `sigt` may be shared by many distinct target
// `ci`, the bucket is adaptive, mirroring the TypeMap's own single/list/cache trichotomy:
//   tier 1-2: `entry->func.value` is a `.next`-chained list of records (kept while
//             count <= MAX_ADAPTER_LIST_COUNT); appended at the tail so the head
//             never moves and readers walk it lock-free;
//   tier 3:   promoted to a `jl_eqtable` (Memory) keyed on the record's `ci`
//             (NULL -> jl_nothing), whose value is a short `.next` chain of the
//             same-ci variants.
// `entry->func.value` is read with acquire / written with release (it is swapped on
// promotion), so lock-free readers see a complete old-or-new representation. As in the
// TypeMap's list->cache conversion, a reader mid-walk during promotion may spuriously miss
// and recover via the writelock-held recheck in jl_get_abi_adapter. The runtime JIT half
// (LLVM emit + record allocation) stays in jitlayers.cpp; these helpers own the TypeMap/
// bucket bookkeeping (which needs libjulia-internal's APIs).

#define MAX_ADAPTER_LIST_COUNT 6 // mirror MAX_METHLIST_COUNT: promote chain -> eqtable above this

// `rt` is compared by *type equality* (jl_types_equal), mirroring the TypeMap's `sigt` match;
// jl_egal would split type-equal-but-not-egal return types into duplicate records. Not
// NOTSAFEPOINT (jl_types_equal may safepoint; the bucket chain is append-only and rooted).
static int abi_adapter_matches(jl_abi_adapter_t *e, jl_value_t *rt, jl_code_instance_t *ci,
                               int specsig, int is_opaque_closure, size_t nargs)
{
    return e->ci == ci
        && (int)e->specsig == (specsig ? 1 : 0)
        && (int)e->is_opaque_closure == (is_opaque_closure ? 1 : 0)
        && e->nargs == nargs
        && (e->rt == rt || jl_types_equal(e->rt, rt)); // rt lives in the bucket (key is sigt alone)
}

// eqtable key for a record's `ci` (eqtable can't key NULL -> jl_nothing sentinel).
static jl_value_t *abi_adapter_cikey(jl_code_instance_t *ci) JL_NOTSAFEPOINT
{
    return ci ? (jl_value_t*)ci : jl_nothing;
}

// Walk a `.next` chain (acquire) for a matching record.
static jl_abi_adapter_t *abi_adapter_chain_find(jl_abi_adapter_t *e, jl_value_t *rt, jl_code_instance_t *ci,
        int specsig, int is_opaque_closure, size_t nargs)
{
    for (; e != NULL; e = jl_atomic_load_acquire(&e->next)) {
        JL_GC_PROMISE_ROOTED(e); // chain node rooted by the (append-only) cache TypeMap
        if (abi_adapter_matches(e, rt, ci, specsig, is_opaque_closure, nargs))
            return e;
    }
    return NULL;
}

// Find a record in `te`'s bucket. `repr` (acquire) is either a chain head (tiers
// 1-2) or a `ci`-keyed eqtable (tier 3); both reduce to a short chain walk.
static jl_abi_adapter_t *abi_adapter_bucket_find(jl_typemap_entry_t *te, jl_value_t *rt, jl_code_instance_t *ci,
        int specsig, int is_opaque_closure, size_t nargs)
{
    jl_value_t *repr = jl_atomic_load_acquire((_Atomic(jl_value_t*)*)&te->func.value);
    if (repr == NULL)
        return NULL;
    if (jl_is_genericmemory(repr)) {
        jl_abi_adapter_t *head = (jl_abi_adapter_t*)jl_eqtable_get(
                (jl_genericmemory_t*)repr, abi_adapter_cikey(ci), NULL);
        return abi_adapter_chain_find(head, rt, ci, specsig, is_opaque_closure, nargs);
    }
    return abi_adapter_chain_find((jl_abi_adapter_t*)repr, rt, ci, specsig, is_opaque_closure, nargs);
}

// Resolve whether (`from_abi`, `codeinst`) needs a wrapping adapter at all. If the target's
// own compiled entry point already satisfies the requested ABI, return that specptr directly
// (no adapter). Otherwise return NULL and fill *target / *target_specsig / *invoke describing
// the target the adapter must bridge to. This is pure metadata resolution -- no codegen -- so
// it is shared by the JIT path (jl_get_abi_adapter) and the codegen-free fallback
// (jl_jit_abi_converter_fallback), which needs the same shortcut before consulting the cache.
// Not JL_NOTSAFEPOINT: the shortcut checks below use jl_subtype, which can allocate. Its inputs
// (from_abi.sigt/rt, codeinst) are rooted by every caller (via the trampoline or the call site).
JL_DLLEXPORT void *jl_abi_adapter_resolve_target(jl_abi_t from_abi, jl_code_instance_t *codeinst,
        void **target, int *target_specsig, jl_callptr_t *invoke)
{
    *target = NULL;
    *target_specsig = 0;
    *invoke = NULL;
    if (codeinst == NULL)
        return NULL;
    uint8_t specsigflags;
    jl_method_instance_t *mi = jl_get_ci_mi(codeinst);
    void *specptr = NULL;
    jl_callptr_t inv = NULL;
    // `waitcompile` must be 0: this is pure metadata resolution (see above) shared with the
    // codegen-free fallback, so it must not trigger `jl_compile_codeinst` -- that would break the
    // no-codegen contract (#61949). A target still mid-compilation simply reads back as
    // `inv == NULL` here: no shortcut, and the caller emits/uses the dynamic-dispatch adapter
    // (which re-resolves at call time).
    jl_read_codeinst_invoke(codeinst, &specsigflags, &inv, &specptr, /* waitcompile */ 0);
    *invoke = inv;
    if (inv == NULL)
        return NULL;
    if (inv == jl_fptr_const_return_addr) {
        // const-return has no specptr to shortcut to: an adapter is always required.
        return NULL;
    }
    else if (inv == jl_fptr_args_addr) {
        assert(specptr != NULL);
        if (!from_abi.specsig && jl_subtype(codeinst->rettype, from_abi.rt))
            return specptr; // no adapter required
        *target = specptr;
        *target_specsig = 0;
    }
    else if (specsigflags & JL_CI_FLAGS_SPECPTR_SPECIALIZED) {
        assert(specptr != NULL);
        if (from_abi.specsig && jl_egal(mi->specTypes, from_abi.sigt) && jl_egal(codeinst->rettype, from_abi.rt))
            return specptr; // no adapter required
        *target = specptr;
        *target_specsig = 1;
    }
    return NULL;
}

// Lock-free lookup of the adapter record for (sigt, rt, ci, specsig, is_opaque_closure,
// nargs). Returns the record or NULL. Safe to call with or without the writelock held.
JL_DLLEXPORT jl_abi_adapter_t *jl_lookup_abi_adapter(jl_value_t *sigt, jl_value_t *rt,
        jl_code_instance_t *ci, int specsig, int is_opaque_closure, size_t nargs)
{
    // Key on `sigt` alone (its slot 0 is the function type, never Union{}); the remaining
    // fields are disambiguated in the bucket (abi_adapter_matches).
    jl_typemap_t *map = jl_atomic_load_relaxed(&jl_abi_adapters->cache);
    jl_abi_adapter_t *found = NULL;
    if ((jl_value_t*)map != jl_nothing) {
        struct jl_typemap_assoc search = { sigt, jl_atomic_load_acquire(&jl_world_counter), NULL };
        jl_typemap_entry_t *te = jl_typemap_assoc_by_type(map, &search, /*offs*/0, /*subtype*/0);
        if (te != NULL)
            found = abi_adapter_bucket_find(te, rt, ci, specsig, is_opaque_closure, nargs);
    }
    return found;
}

// Insert `e` (with `e->next` published via release) into `tbl`'s `ci`-bucket chain by
// prepending. Returns the (possibly reallocated) table.
static jl_genericmemory_t *abi_adapter_eqtable_prepend(jl_genericmemory_t *tbl, jl_abi_adapter_t *e)
{
    jl_value_t *cik = abi_adapter_cikey(e->ci);
    jl_abi_adapter_t *head = (jl_abi_adapter_t*)jl_eqtable_get(tbl, cik, NULL);
    jl_atomic_store_release(&e->next, head); // e not yet published; ready for the put's release
    int inserted = 0;
    return jl_eqtable_put(tbl, cik, (jl_value_t*)e, &inserted);
}

// Install `entry` into the adapters cache. Caller holds the writelock and must have
// confirmed (under the lock) that the key is absent.
JL_DLLEXPORT void jl_install_abi_adapter(jl_abi_adapter_t *entry)
{
    jl_genericmemory_t *tbl = NULL;
    jl_abi_adapter_t *e = NULL, *nxt = NULL;
    JL_GC_PUSH4(&entry, &tbl, &e, &nxt);
    // Key on the adapter sig alone; rt is in the bucket (see jl_lookup_abi_adapter).
    jl_value_t *key = entry->sigt; // rooted via `entry`
    jl_typemap_t *map = jl_atomic_load_relaxed(&jl_abi_adapters->cache);
    jl_typemap_entry_t *te = NULL;
    if ((jl_value_t*)map != jl_nothing) {
        struct jl_typemap_assoc search = { key, jl_atomic_load_acquire(&jl_world_counter), NULL };
        te = jl_typemap_assoc_by_type(map, &search, /*offs*/0, /*subtype*/0);
    }
    if (te == NULL) {
        // first record for this sigt: a TypeMap entry whose value is the record
        // (a chain of one). `next` was initialized to NULL by adapter_alloc_entry.
        jl_typemap_entry_t *newentry = jl_typemap_alloc((jl_tupletype_t*)key, NULL,
                jl_emptysvec, (jl_value_t*)entry, 1, ~(size_t)0);
        // Root `newentry` across the insert: jl_typemap_insert allocates internally (it may
        // build a TypeMapLevel), so a GC there would otherwise collect the freshly-allocated,
        // still-unlinked entry and leave a dangling record in the cache (mirrors tramp_map_insert).
        JL_GC_PUSH1(&newentry);
        jl_typemap_insert(&jl_abi_adapters->cache, (jl_value_t*)jl_abi_adapters, newentry, /*offs*/0);
        JL_GC_POP();
        JL_GC_POP();
        return;
    }
    jl_value_t *repr = jl_atomic_load_relaxed((_Atomic(jl_value_t*)*)&te->func.value);
    if (jl_is_genericmemory(repr)) {
        // tier 3: prepend into the ci-bucket; publish a grown table if it reallocated.
        tbl = abi_adapter_eqtable_prepend((jl_genericmemory_t*)repr, entry);
        if ((jl_value_t*)tbl != repr) {
            jl_atomic_store_release((_Atomic(jl_value_t*)*)&te->func.value, (jl_value_t*)tbl);
            jl_gc_wb(te, tbl);
        }
        JL_GC_POP();
        return;
    }
    // tiers 1-2: a `.next` chain. Find the tail and count.
    jl_abi_adapter_t *head = (jl_abi_adapter_t*)repr;
    jl_abi_adapter_t *tail = head;
    size_t count = 1;
    for (;;) {
        jl_abi_adapter_t *n = jl_atomic_load_relaxed(&tail->next);
        if (n == NULL)
            break;
        tail = n;
        count++;
    }
    if (count < MAX_ADAPTER_LIST_COUNT) {
        // append at the tail; the head (`func.value`) never moves.
        jl_atomic_store_release(&tail->next, entry);
        jl_gc_wb(tail, entry);
        JL_GC_POP();
        return;
    }
    // tier 2 -> 3: promote the chain to a ci-keyed eqtable, then publish it. Regrouping
    // relinks `.next` (concurrent old-chain readers may miss -> locked recheck recovers).
    tbl = jl_alloc_memory_any(16);
    e = head;
    while (e != NULL) {
        nxt = jl_atomic_load_relaxed(&e->next);
        tbl = abi_adapter_eqtable_prepend(tbl, e);
        e = nxt;
    }
    tbl = abi_adapter_eqtable_prepend(tbl, entry);
    jl_atomic_store_release((_Atomic(jl_value_t*)*)&te->func.value, (jl_value_t*)tbl);
    jl_gc_wb(te, tbl);
    JL_GC_POP();
}

// Allocate an ABI-adapter cache record. `fptr` is the (process-local) adapter address; on the
// image-load path it is the address already wired into the trampoline from the image's fvar
// table. Caller keeps the key fields (`sigt`/`rt`/`ci`) rooted. `next` starts as the chain tail.
JL_DLLEXPORT jl_abi_adapter_t *jl_new_abi_adapter_record(jl_value_t *sigt, jl_value_t *rt,
        jl_code_instance_t *ci, int specsig, int is_opaque_closure, size_t nargs, void *fptr)
{
    jl_task_t *ct = jl_current_task;
    jl_abi_adapter_t *e = (jl_abi_adapter_t*)jl_gc_alloc(ct->ptls, sizeof(jl_abi_adapter_t), jl_abi_adapter_type);
    e->sigt = sigt;
    e->rt = rt;
    e->specsig = specsig ? 1 : 0;
    e->is_opaque_closure = is_opaque_closure ? 1 : 0;
    e->nargs = nargs;
    e->ci = ci;
    e->fptr = fptr;
    jl_atomic_store_relaxed(&e->next, (jl_abi_adapter_t*)NULL);
    return e;
}

// Re-install an ABI-adapter record restored from an image into the running cache. `fptr` was
// wired from the image's fvar table (jl_update_all_fptrs) and `next` reset at serialization;
// this rebuilds the `sigt` bucket. Keep-first: if an equivalent key is already present (from
// the sysimage or a previously-loaded pkgimage) leave `e` standalone. Called from the load
// fixup for every serialized record -- including adapters not referenced by any live trampoline
// (e.g. the ci == NULL dynamic-dispatch/`unspecialized` adapter). The load-time counterpart of
// the JIT path's interning in jl_get_abi_adapter.
JL_DLLEXPORT void jl_reintern_abi_adapter(jl_abi_adapter_t *e)
{
    JL_GC_PUSH1(&e);
    JL_LOCK(&jl_abi_adapters->writelock);
    if (jl_lookup_abi_adapter(e->sigt, e->rt, e->ci, e->specsig, e->is_opaque_closure, e->nargs) == NULL)
        jl_install_abi_adapter(e);
    JL_UNLOCK(&jl_abi_adapters->writelock);
    JL_GC_POP();
}

// This file is a part of Julia. License is MIT: https://julialang.org/license


static uint_t idset_hash(size_t idx, jl_value_t *data)
{
    if (idx >= ((jl_genericmemory_t*)data)->length)
        return 0; // We got a OOB access, probably due to a data race
    jl_value_t *x = jl_genericmemory_ptr_ref(data, idx);
    // x should not be NULL, unless there was concurrent corruption
    return x == NULL ? 0 : jl_object_id(x);
}

static int idset_eq(size_t idx, const void *y, jl_value_t *data, uint_t hv)
{
    if (idx >= ((jl_genericmemory_t*)data)->length)
        return 0; // We got a OOB access, probably due to a data race
    jl_value_t *x = jl_genericmemory_ptr_ref(data, idx);
    // x should not be NULL, unless there was concurrent corruption
    return x == NULL ? 0 : jl_egal(x, (jl_value_t*)y);
}

jl_genericmemory_t *jl_idset_rehash(jl_genericmemory_t *keys, jl_genericmemory_t *idxs, size_t newsz)
{
    if (newsz == 0)
        return idxs;
    newsz = next_power_of_two(newsz);
    //if (idxs->length == newsz)
    //    jl_idset_put_idx(keys, idxs, -newsz+1);
    //else
    return smallintset_rehash(idxs, idset_hash, (jl_value_t*)keys, newsz, 0);
}

// Return idx if key is in hash, otherwise -1
// Note: lookup in the IdSet is permitted concurrently, if you avoid deletions,
// and assuming you do use an external lock around all insertions
ssize_t jl_idset_peek_bp(jl_genericmemory_t *keys, jl_genericmemory_t *idxs, jl_value_t *key) JL_NOTSAFEPOINT
{
    uintptr_t hv = jl_object_id(key);
    return jl_smallintset_lookup(idxs, idset_eq, key, (jl_value_t*)keys, hv, 0);
}

jl_value_t *jl_idset_get(jl_genericmemory_t *keys, jl_genericmemory_t *idxs, jl_value_t *key) JL_NOTSAFEPOINT
{
    ssize_t idx = jl_idset_peek_bp(keys, idxs, key);
    if (idx == -1)
        return NULL;
    return jl_genericmemory_ptr_ref(keys, idx);
}


static ssize_t idset_compact(jl_genericmemory_t *keys)
{
    // compact keys before rehashing idxs
    ssize_t i, j;
    ssize_t rehash = 0;
    for (i = j = 0; i < keys->length; i++) {
        jl_value_t *k = jl_genericmemory_ptr_ref(keys, i);
        if (k != NULL) {
            if (i != j) {
                rehash = 1;
                jl_genericmemory_ptr_set(keys, j, k);
                jl_genericmemory_ptr_set(keys, i, NULL);
            }
            j++;
        }
    }
    return rehash ? -j : j;
}

// Insert `key` into the first free slot at the end of the packed prefix of
// `keys` (growing, and compacting order-preservingly, if there is none). On a
// set that never sees `jl_idset_pop`, the keys therefore form a packed prefix
// -- no interior holes -- in insertion order. `Method.interferences` consumers
// rely on exactly that: iteration may stop at the first empty slot, and
// entries appear in method-definition (world) order (see the stable-prefix
// reload in `sort_mlmatches` in gf.c and `eachinterference` in
// Compiler/src/reinfer.jl).
jl_genericmemory_t *jl_idset_put_key(jl_genericmemory_t *keys, jl_value_t *key, ssize_t *newidx)
{
    ssize_t l = keys->length;
    ssize_t i = l;
    while (i > 0 && jl_genericmemory_ptr_ref(keys, i - 1) == NULL)
        i--;
    // i points to the place to insert
    *newidx = i;
    if (i == l) {
        i = idset_compact(keys);
        if (i < 0) {
            *newidx = i - 1;
            i = -i;
        }
        if (i >= l / 3 * 2) {
            size_t nl = l < 4 ? 4 : (l * 3) >> 1; // grow space by 50% if less than 33% free after compacting
            jl_genericmemory_t *nk = jl_alloc_genericmemory(jl_memory_any_type, nl);
            if (i > 0)
                memcpy(nk->ptr, keys->ptr, sizeof(void*) * i);
            keys = nk;
        }
    }
    assert(jl_genericmemory_ptr_ref(keys, i) == NULL);
    jl_genericmemory_ptr_set(keys, i, key);
    return keys;
}

jl_genericmemory_t *jl_idset_put_idx(jl_genericmemory_t *keys, jl_genericmemory_t *idxs, ssize_t idx)
{
    _Atomic(jl_genericmemory_t*) newidxs = idxs;
    JL_GC_PUSH1(&newidxs);
    if (idx < 0) { // full rehash
        smallintset_empty(idxs);
        for (ssize_t i = 0; i < -idx; i++)
            if (jl_genericmemory_ptr_ref(keys, i) != NULL)
                jl_smallintset_insert(&newidxs, NULL, idset_hash, i, (jl_value_t*)keys);
    }
    else {
        jl_smallintset_insert(&newidxs, NULL, idset_hash, idx, (jl_value_t*)keys);
    }
    JL_GC_POP();
    return jl_atomic_load_relaxed(&newidxs);
}

/* returns idx if key is in hash, otherwise -1 */
// Removal NULLs the key's slot in place, leaving an interior hole until a
// later insertion happens to compact the prefix. That breaks the packed-prefix
// guarantee documented at `jl_idset_put_key`, so this must not be used on
// sets whose consumers rely on it (notably `Method.interferences`).
ssize_t jl_idset_pop(jl_genericmemory_t *keys, jl_genericmemory_t *idxs, jl_value_t *key) JL_NOTSAFEPOINT
{
    uintptr_t hv = jl_object_id(key);
    ssize_t idx = jl_smallintset_lookup(idxs, idset_eq, key, (jl_value_t*)keys, hv, 1);
    if (idx != -1)
        jl_genericmemory_ptr_set(keys, idx, NULL);
    return idx;
}

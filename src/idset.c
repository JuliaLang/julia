// This file is a part of Julia. License is MIT: https://julialang.org/license


static uint_t idset_hash(size_t idx, jl_value_t *data)
{
    jl_value_t *x = jl_genericmemory_ptr_ref(data, idx);
    return jl_object_id(x);
}

static int idset_eq(size_t idx, const void *y, jl_value_t *data, uint_t hv)
{
    jl_value_t *x = jl_genericmemory_ptr_ref(data, idx);
    return jl_egal(x, (jl_value_t*)y);
}

JL_DLLEXPORT jl_genericmemory_t *jl_idset_rehash(jl_genericmemory_t *keys, jl_genericmemory_t *idxs, size_t newsz)
{
    newsz = next_power_of_two(newsz);
    return smallintset_rehash(idxs, idset_hash, (jl_value_t*)keys, newsz, 0);
}

/* returns idx if key is in hash, otherwise -1 */
JL_DLLEXPORT ssize_t jl_idset_peek_bp(jl_genericmemory_t *keys, jl_genericmemory_t *idxs, jl_value_t *key) JL_NOTSAFEPOINT
{
    uintptr_t hv = jl_object_id(key);
    return jl_smallintset_lookup(idxs, idset_eq, key, (jl_value_t*)keys, hv);
}

JL_DLLEXPORT jl_genericmemory_t *jl_idset_put_key(jl_genericmemory_t *keys, jl_value_t *key, size_t *newidx)
{
    size_t l = keys->length;
    size_t i = l;
    while (i > 0 && jl_genericmemory_ptr_ref(keys, i - 1) == NULL)
        i--;
    // i points to the place to insert
    if (i == l) {
        size_t nl = l < 4 ? 4 : (l * 3) >> 1; // grow space by 50%
        jl_genericmemory_t *nk = jl_alloc_genericmemory(jl_memory_any_type, nl);
        if (i > 0)
            memcpy(nk->ptr, keys->ptr, sizeof(void*) * i);
        keys = nk;
    }
    assert(jl_genericmemory_ptr_ref(keys, i) == NULL);
    jl_genericmemory_ptr_set(keys, i, key);
    *newidx = i;
    return keys;
}

JL_DLLEXPORT jl_genericmemory_t *jl_idset_put_idx(jl_genericmemory_t *keys, jl_genericmemory_t *idxs, size_t idx)
{
    _Atomic(jl_genericmemory_t*) newidxs = idxs;
    JL_GC_PUSH1(&newidxs);
    jl_smallintset_insert(&newidxs, NULL, idset_hash, idx, (jl_value_t*)keys);
    JL_GC_POP();
    return newidxs;
}

// Note: lookup in the IdSet is permitted concurrently, if you avoid deletions,
// and assuming you do use an external lock around all insertions
JL_DLLEXPORT jl_value_t *jl_idset_get(jl_genericmemory_t *keys, jl_genericmemory_t *idxs, jl_value_t *key, jl_value_t *deflt) JL_NOTSAFEPOINT
{
    ssize_t idx = jl_idset_peek_bp(keys, idxs, key);
    if (idx == -1)
        return deflt;
    return jl_genericmemory_ptr_ref(keys, idx);
}

// This file is a part of Julia. License is MIT: https://julialang.org/license

#define hash_size(h) (jl_array_len(h) / 2)

// compute empirical max-probe for a given size
#define max_probe(size) ((size) <= 1024 ? 16 : (size) >> 6)

#define keyhash(k) jl_object_id(k)
#define h2index(hv, sz) (size_t)(((hv) & ((sz)-1)) * 2)

static int jl_table_assign_bp(jl_array_t **pa, jl_value_t *key, jl_value_t *val);

JL_DLLEXPORT jl_array_t *jl_idtable_rehash(jl_array_t *a, size_t newsz)
{
    size_t sz = jl_array_len(a);
    size_t i;
    jl_value_t **ol = (jl_value_t **)a->data;
    jl_array_t *newa = jl_alloc_vec_any(newsz);
    // keep the original array in the original slot since we need `ol`
    // to be valid in the loop below.
    JL_GC_PUSH1(&newa);
    for (i = 0; i < sz; i += 2) {
        if (ol[i + 1] != NULL) {
            jl_table_assign_bp(&newa, ol[i], ol[i + 1]);
            // it is however necessary here because allocation
            // can (and will) occur in a recursive call inside table_lookup_bp
        }
    }
    JL_GC_POP();
    return newa;
}

static int jl_table_assign_bp(jl_array_t **pa, jl_value_t *key, jl_value_t *val)
{
    // pa points to a **rooted** gc frame slot
    uint_t hv;
    jl_array_t *a = *pa;
    size_t orig, index, iter, empty_slot;
    size_t newsz, sz = hash_size(a);
    assert(sz >= 1);
    size_t maxprobe = max_probe(sz);
    void **tab = (void **)a->data;

    hv = keyhash(key);
    while (1) {
        iter = 0;
        index = h2index(hv, sz);
        sz *= 2;
        orig = index;
        empty_slot = -1;

        do {
            jl_value_t *k2 = (jl_value_t*)tab[index];
            if (k2 == NULL) {
                if (empty_slot == -1)
                    empty_slot = index;
                break;
            }
            if (jl_egal(key, k2)) {
                if (tab[index + 1] != NULL) {
                    tab[index + 1] = val;
                    jl_gc_wb(a, val);
                    return 0;
                }
                // `nothing` is our sentinel value for deletion, so need to keep searching if it's also our search key
                assert(key == jl_nothing);
                if (empty_slot == -1)
                    empty_slot = index;
            }
            if (empty_slot == -1 && tab[index + 1] == NULL) {
                assert(tab[index] == jl_nothing);
                empty_slot = index;
            }

            index = (index + 2) & (sz - 1);
            iter++;
        } while (iter <= maxprobe && index != orig);

        if (empty_slot != -1) {
            tab[empty_slot] = key;
            jl_gc_wb(a, key);
            tab[empty_slot + 1] = val;
            jl_gc_wb(a, val);
            return 1;
        }

        /* table full */
        /* quadruple size, rehash, retry the insert */
        /* it's important to grow the table really fast; otherwise we waste */
        /* lots of time rehashing all the keys over and over. */
        sz = jl_array_len(a);
        if (sz < HT_N_INLINE)
            newsz = HT_N_INLINE;
        else if (sz >= (1 << 19) || (sz <= (1 << 8)))
            newsz = sz << 1;
        else
            newsz = sz << 2;
        *pa = jl_idtable_rehash(*pa, newsz);

        a = *pa;
        tab = (void **)a->data;
        sz = hash_size(a);
        maxprobe = max_probe(sz);
    }
}

/* returns bp if key is in hash, otherwise NULL */
jl_value_t **jl_table_peek_bp(jl_array_t *a, jl_value_t *key)
{
    size_t sz = hash_size(a);
    assert(sz >= 1);
    size_t maxprobe = max_probe(sz);
    void **tab = (void **)a->data;
    uint_t hv = keyhash(key);
    size_t index = h2index(hv, sz);
    sz *= 2;
    size_t orig = index;
    size_t iter = 0;

    do {
        jl_value_t *k2 = (jl_value_t*)jl_atomic_load_relaxed(&tab[index]); // just to ensure the load doesn't get duplicated
        if (k2 == NULL)
            return NULL;
        if (jl_egal(key, k2)) {
            if (tab[index + 1] != NULL)
                return (jl_value_t**)&tab[index + 1];
            // `nothing` is our sentinel value for deletion, so need to keep searching if it's also our search key
            if (key != jl_nothing)
                return NULL; // concurrent insertion hasn't completed yet
        }

        index = (index + 2) & (sz - 1);
        iter++;
    } while (iter <= maxprobe && index != orig);

    return NULL;
}

JL_DLLEXPORT
jl_array_t *jl_eqtable_put(jl_array_t *h, jl_value_t *key, jl_value_t *val, int *p_inserted)
{
    JL_GC_PUSH1(&h);
    // &h may be assigned to in jl_idtable_rehash so it need to be rooted
    int inserted = jl_table_assign_bp(&h, key, val);
    if (p_inserted)
        *p_inserted = inserted;
    JL_GC_POP();
    return h;
}

// Note: lookup in the IdDict it permitted concurrently, if you avoid deletions,
// and assuming you do use an external lock around all insertions
JL_DLLEXPORT
jl_value_t *jl_eqtable_get(jl_array_t *h, jl_value_t *key, jl_value_t *deflt)
{
    jl_value_t **bp = jl_table_peek_bp(h, key);
    return (bp == NULL) ? deflt : *bp;
}

JL_DLLEXPORT
jl_value_t *jl_eqtable_pop(jl_array_t *h, jl_value_t *key, jl_value_t *deflt, int *found)
{
    jl_value_t **bp = jl_table_peek_bp(h, key);
    if (found)
        *found = (bp != NULL);
    if (bp == NULL)
        return deflt;
    jl_value_t *val = *bp;
    *(bp - 1) = jl_nothing; // clear the key
    *bp = NULL;
    return val;
}

JL_DLLEXPORT
size_t jl_eqtable_nextind(jl_array_t *t, size_t i)
{
    if (i & 1)
        i++;
    size_t alen = jl_array_dim0(t);
    while (i < alen && ((void **)t->data)[i + 1] == NULL)
        i += 2;
    if (i >= alen)
        return (size_t)-1;
    return i;
}

#undef hash_size
#undef max_probe

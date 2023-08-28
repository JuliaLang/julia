// This file is a part of Julia. License is MIT: https://julialang.org/license

#define hash_size(h) (h->length / 2)

// compute empirical max-probe for a given size
#define max_probe(size) ((size) <= 1024 ? 16 : (size) >> 6)

#define keyhash(k) jl_object_id_(jl_typetagof(k), k)
#define h2index(hv, sz) (size_t)(((hv) & ((sz)-1)) * 2)

static inline ssize_t jl_table_assign_bp(jl_genericmemory_t **pa, jl_value_t *key, jl_value_t *val, int insert_val);

JL_DLLEXPORT jl_genericmemory_t *jl_idtable_rehash(jl_genericmemory_t *a, size_t newsz)
{
    size_t sz = a->length;
    size_t i;
    jl_value_t **ol = (jl_value_t **) a->ptr;
    jl_genericmemory_t *newa = NULL;
    // keep the original memory in the original slot since we need `ol`
    // to be valid in the loop below.
    JL_GC_PUSH2(&newa, &a);
    newa = jl_alloc_memory_any(newsz);
    for (i = 0; i < sz; i += 2) {
        if (ol[i + 1] != NULL) {
            jl_table_assign_bp(&newa, ol[i], ol[i + 1], 1);
        }
    }
    JL_GC_POP();
    return newa;
}

// returns where a key is stored, or -pos if the key was not present and was inserted at pos
// result is 1-indexed
static inline ssize_t jl_table_assign_bp(jl_genericmemory_t **pa, jl_value_t *key, jl_value_t *val, int insert_val)
{
    // pa points to a **un**rooted address
    uint_t hv;
    jl_genericmemory_t *a = *pa;
    size_t orig, index, iter, empty_slot;
    size_t newsz, sz = hash_size(a);
    if (sz == 0) {
        a = jl_alloc_memory_any(HT_N_INLINE);
        sz = hash_size(a);
        *pa = a;
    }
    size_t maxprobe = max_probe(sz);
    _Atomic(jl_value_t*) *tab = (_Atomic(jl_value_t*)*) a->ptr;

    hv = keyhash(key);
    while (1) {
        iter = 0;
        index = h2index(hv, sz);
        sz *= 2;
        orig = index;
        empty_slot = -1;

        do {
            jl_value_t *k2 = jl_atomic_load_relaxed(&tab[index]);
            if (k2 == NULL) {
                if (empty_slot == -1)
                    empty_slot = index;
                break;
            }
            if (jl_egal(key, k2)) {
                if (jl_atomic_load_relaxed(&tab[index + 1]) != NULL) {
                    if (insert_val == 1) {
                        jl_atomic_store_release(&tab[index + 1], val);
                        jl_gc_wb(a, val);
                    }
                    return index+1;
                }
                // `nothing` is our sentinel value for deletion, so need to keep searching if it's also our search key
                assert(key == jl_nothing);
                if (empty_slot == -1)
                    empty_slot = index;
            }
            if (empty_slot == -1 && jl_atomic_load_relaxed(&tab[index + 1]) == NULL) {
                assert(jl_atomic_load_relaxed(&tab[index]) == jl_nothing);
                empty_slot = index;
            }

            index = (index + 2) & (sz - 1);
            iter++;
        } while (iter <= maxprobe && index != orig);

        if (empty_slot != -1) {
            jl_atomic_store_release(&tab[empty_slot], key);
            jl_gc_wb(a, key);
            if (insert_val == 1) {
                jl_atomic_store_release(&tab[empty_slot + 1], val);
                jl_gc_wb(a, val);
            }
            return -(empty_slot+1);
        }

        /* table full */
        /* quadruple size, rehash, retry the insert */
        /* it's important to grow the table really fast; otherwise we waste */
        /* lots of time rehashing all the keys over and over. */
        sz = a -> length;
        if (sz < HT_N_INLINE)
            newsz = HT_N_INLINE;
        else if (sz >= (1 << 19) || (sz <= (1 << 8)))
            newsz = sz << 1;
        else
            newsz = sz << 2;
        *pa = jl_idtable_rehash(*pa, newsz);

        a = *pa;
        tab =  (_Atomic(jl_value_t*)*) a->ptr;
        sz = hash_size(a);
        maxprobe = max_probe(sz);
    }
}

/* returns bp if key is in hash, otherwise NULL */
inline _Atomic(jl_value_t*) *jl_table_peek_bp(jl_genericmemory_t *a, jl_value_t *key) JL_NOTSAFEPOINT
{
    size_t sz = hash_size(a);
    if (sz == 0)
        return NULL;
    size_t maxprobe = max_probe(sz);
    _Atomic(jl_value_t*) *tab = (_Atomic(jl_value_t*)*) a->ptr;
    uint_t hv = keyhash(key);
    size_t index = h2index(hv, sz);
    sz *= 2;
    size_t orig = index;
    size_t iter = 0;

    do {
        jl_value_t *k2 = jl_atomic_load_relaxed(&tab[index]); // just to ensure the load doesn't get duplicated
        if (k2 == NULL)
            return NULL;
        if (jl_egal(key, k2)) {
            if (jl_atomic_load_relaxed(&tab[index + 1]) != NULL)
                return &tab[index + 1];
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
jl_genericmemory_t *jl_eqtable_put(jl_genericmemory_t *h, jl_value_t *key, jl_value_t *val, int *p_inserted)
{
    ssize_t result = jl_table_assign_bp(&h, key, val, 1);
    if (p_inserted)
        *p_inserted = (result < 0);
    return h;
}

// Note: lookup in the IdDict is permitted concurrently, if you avoid deletions,
// and assuming you do use an external lock around all insertions
JL_DLLEXPORT
jl_value_t *jl_eqtable_get(jl_genericmemory_t *h, jl_value_t *key, jl_value_t *deflt) JL_NOTSAFEPOINT
{
    _Atomic(jl_value_t*) *bp = jl_table_peek_bp(h, key);
    return (bp == NULL) ? deflt : jl_atomic_load_relaxed(bp);
}

jl_value_t *jl_eqtable_getkey(jl_genericmemory_t *h, jl_value_t *key, jl_value_t *deflt) JL_NOTSAFEPOINT
{
    _Atomic(jl_value_t*) *bp = jl_table_peek_bp(h, key);
    return (bp == NULL) ? deflt : jl_atomic_load_relaxed(bp - 1);
}

JL_DLLEXPORT
jl_value_t *jl_eqtable_pop(jl_genericmemory_t *h, jl_value_t *key, jl_value_t *deflt, int *found)
{
    _Atomic(jl_value_t*) *bp = jl_table_peek_bp(h, key);
    if (found)
        *found = (bp != NULL);
    if (bp == NULL)
        return deflt;
    jl_value_t *val = jl_atomic_load_relaxed(bp);
    jl_atomic_store_relaxed(bp - 1, jl_nothing); // clear the key
    jl_atomic_store_relaxed(bp, NULL); // and the value (briefly corrupting the table)
    return val;
}

JL_DLLEXPORT
size_t jl_eqtable_nextind(jl_genericmemory_t *t, size_t i)
{
    if (i & 1)
        i++;
    size_t alen = t->length;
    while (i < alen && ((void**) t->ptr)[i + 1] == NULL)
        i += 2;
    if (i >= alen)
        return (size_t)-1;
    return i;
}

JL_DLLEXPORT
ssize_t jl_eqtable_keyindex(jl_id_dict_t *d, jl_value_t *key)
{
    jl_genericmemory_t *h = d->ht;

    ssize_t index = jl_table_assign_bp(&h, key, NULL, 0);
    jl_atomic_store_release((_Atomic(jl_genericmemory_t*)*)&d->ht, h);
    jl_gc_wb(d, h);

    return index;
}

#undef hash_size
#undef max_probe
#undef h2index

// This file is a part of Julia. License is MIT: https://julialang.org/license

#define hash_size(h) (h->length / 2)

#define keyhash(k) jl_object_id_(jl_typetagof(k), k)
#define h2index(hv, sz) (size_t)(((hv) & ((sz)-1)) * 2)

static inline int jl_table_assign_bp(jl_genericmemory_t **pa, jl_value_t *key, jl_value_t *val, ssize_t count, size_t ndel) JL_CANSAFEPOINT;

static int idtable_slot(void *tab, size_t j) JL_NOTSAFEPOINT
{
    _Atomic(jl_value_t*) *pair = &((_Atomic(jl_value_t*)*)tab)[2 * j];
    return jl_atomic_load_relaxed(&pair[1]) != NULL ? 1 : jl_atomic_load_relaxed(&pair[0]) != NULL ? 2 : 0;
}

// `jl_table_resize` for a table holding `count` entries and at most `ndel` tombstones, or with
// `count == -1` (a caller that counts neither) `jl_table_resize_sampled`
static int idtable_resize(_Atomic(jl_value_t*) *tab, size_t npairs, uint_t hv, size_t iter, ssize_t count, size_t ndel) JL_NOTSAFEPOINT
{
    if (count >= 0)
        return jl_table_resize(npairs, iter, count + 1, ndel);
    return jl_table_resize_sampled(tab, npairs, hv, iter, idtable_slot);
}

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
    ssize_t count = 0;
    for (i = 0; i < sz; i += 2) {
        if (ol[i + 1] != NULL) {
            jl_table_assign_bp(&newa, ol[i], ol[i + 1], count++, 0);
        }
    }
    JL_GC_POP();
    return newa;
}

static inline int jl_table_assign_bp(jl_genericmemory_t **pa, jl_value_t *key, jl_value_t *val, ssize_t count, size_t ndel)
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
    _Atomic(jl_value_t*) *tab = (_Atomic(jl_value_t*)*) a->ptr;

    hv = keyhash(key);
    while (1) {
        iter = 0;
        index = h2index(hv, sz);
        sz *= 2;
        orig = index;
        empty_slot = -1;

        int r = 1;
        do {
            jl_value_t *k2 = jl_atomic_load_relaxed(&tab[index]);
            if (k2 == NULL) {
                // a lookup that misses scans as far as this did: taking an empty slot may
                // first resize the table
                if (empty_slot == -1) {
                    empty_slot = index;
                    r = idtable_resize(tab, sz / 2, hv, iter, count, ndel);
                }
                break;
            }
            if (jl_egal(key, k2)) {
                if (jl_atomic_load_relaxed(&tab[index + 1]) != NULL) {
                    jl_gc_write_atomic(a, tab[index + 1], jl_value_t, val, release);
                    return 0;
                }
                // `nothing` is our sentinel value for deletion, so need to keep searching if it's also our search key
                assert(key == jl_nothing);
            }
            if (empty_slot == -1 && jl_atomic_load_relaxed(&tab[index + 1]) == NULL) { // a tombstone
                assert(k2 == jl_nothing);
                empty_slot = index;
            }

            index = (index + 2) & (sz - 1);
            iter++;
        } while (index != orig);
        if (r > 0 && empty_slot != -1) {
            jl_gc_write_atomic(a, tab[empty_slot], jl_value_t, key, release);
            jl_gc_write_atomic(a, tab[empty_slot + 1], jl_value_t, val, release);
            return 1;
        }

        /* table full, or a long probe: double it (or drop its tombstones) and retry */
        sz = a -> length;
        if (sz < HT_N_INLINE)
            newsz = HT_N_INLINE;
        else
            newsz = r < 0 ? sz : sz << 1;
        *pa = jl_idtable_rehash(*pa, newsz);
        ndel = 0;

        a = *pa;
        tab =  (_Atomic(jl_value_t*)*) a->ptr;
        sz = hash_size(a);
    }
}

/* returns bp if key is in hash, otherwise NULL */
inline _Atomic(jl_value_t*) *jl_table_peek_bp(jl_genericmemory_t *a, jl_value_t *key) JL_NOTSAFEPOINT
{
    size_t sz = hash_size(a);
    if (sz == 0)
        return NULL;
    _Atomic(jl_value_t*) *tab = (_Atomic(jl_value_t*)*) a->ptr;
    uint_t hv = keyhash(key);
    size_t index = h2index(hv, sz);
    sz *= 2;
    size_t orig = index;

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
    } while (index != orig);

    return NULL;
}

JL_DLLEXPORT
jl_genericmemory_t *jl_eqtable_put(jl_genericmemory_t *h, jl_value_t *key, jl_value_t *val, int *p_inserted)
{
    int inserted = jl_table_assign_bp(&h, key, val, -1, 0);
    if (p_inserted)
        *p_inserted = inserted;
    return h;
}

JL_DLLEXPORT
jl_genericmemory_t *jl_eqtable_put_counted(jl_genericmemory_t *h, jl_value_t *key, jl_value_t *val, int *p_inserted, size_t count, size_t ndel)
{
    int inserted = jl_table_assign_bp(&h, key, val, count, ndel);
    if (p_inserted)
        *p_inserted = inserted;
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
    jl_gc_write_atomic(h, bp[-1], jl_value_t, jl_nothing, relaxed); // clear the key
    jl_gc_write_atomic(h, *bp, jl_value_t, NULL, relaxed); // and the value (briefly corrupting the table)
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

#undef hash_size
#undef h2index

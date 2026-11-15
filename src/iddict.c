// This file is a part of Julia. License is MIT: https://julialang.org/license

#define hash_size(h) (h->length / 2)

// past this many probes an insert grows the table, unless it is under half full
#define IDTABLE_PROBE 8

#define keyhash(k) jl_object_id_(jl_typetagof(k), k)
#define h2index(hv, sz) (size_t)(((hv) & ((sz)-1)) * 2)

static inline int jl_table_assign_bp(jl_genericmemory_t **pa, jl_value_t *key, jl_value_t *val) JL_CANSAFEPOINT;

// Whether at least half of the `npairs` slots hold a value, estimated from up to 64
// evenly spaced ones, since the count is kept only by the caller. A run of colliding
// keys mostly falls between the samples, so it does not make a sparse table grow,
// and doubling a table only from half full leaves it at least a quarter full.
static int idtable_half_full(_Atomic(jl_value_t*) *tab, size_t npairs) JL_NOTSAFEPOINT
{
    size_t step = npairs > 64 ? npairs / 64 : 1;
    size_t i, n = 0, m = 0;
    for (i = 0; i < npairs; i += step, m++)
        n += jl_atomic_load_relaxed(&tab[2 * i + 1]) != NULL;
    return n * 2 >= m;
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
    for (i = 0; i < sz; i += 2) {
        if (ol[i + 1] != NULL) {
            jl_table_assign_bp(&newa, ol[i], ol[i + 1]);
        }
    }
    JL_GC_POP();
    return newa;
}

static inline int jl_table_assign_bp(jl_genericmemory_t **pa, jl_value_t *key, jl_value_t *val)
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

        int tombstones = 0;
        do {
            jl_value_t *k2 = jl_atomic_load_relaxed(&tab[index]);
            if (k2 == NULL) {
                if (empty_slot == -1)
                    empty_slot = index;
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
            if (jl_atomic_load_relaxed(&tab[index + 1]) == NULL) { // a tombstone
                assert(k2 == jl_nothing);
                tombstones = 1;
                if (empty_slot == -1)
                    empty_slot = index;
            }

            index = (index + 2) & (sz - 1);
            iter++;
        } while (index != orig);

        // a lookup that misses scans as far as this did (`iter`), and nothing else empties a
        // tombstone: past a long probe, grow a table at least half full, or rehash one with
        // tombstones on the way at the same size
        int same_size = 0;
        if (empty_slot != -1 && iter > IDTABLE_PROBE) {
            if (idtable_half_full(tab, sz / 2))
                empty_slot = -1;
            else if (tombstones)
                empty_slot = -1, same_size = 1;
        }
        if (empty_slot != -1) {
            jl_gc_write_atomic(a, tab[empty_slot], jl_value_t, key, release);
            jl_gc_write_atomic(a, tab[empty_slot + 1], jl_value_t, val, release);
            return 1;
        }

        /* table full, or a long probe: double it (or drop its tombstones) and retry */
        sz = a -> length;
        if (sz < HT_N_INLINE)
            newsz = HT_N_INLINE;
        else
            newsz = same_size ? sz : sz << 1;
        *pa = jl_idtable_rehash(*pa, newsz);

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
    int inserted = jl_table_assign_bp(&h, key, val);
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
#undef IDTABLE_PROBE
#undef h2index

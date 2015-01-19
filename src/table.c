#define hash_size(h) (jl_array_len(h)/2)

// compute empirical max-probe for a given size
#define max_probe(size) ((size)<=1024 ? 16 : (size)>>6)

#define keyhash(k)     jl_object_id(k)
#define h2index(hv,sz) (index_t)(((hv) & ((sz)-1))*2)

static void **jl_table_lookup_bp(jl_array_t **pa, void *key);

void jl_idtable_rehash(jl_array_t **pa, size_t newsz)
{
    size_t sz = jl_array_len(*pa);
    size_t i;
    void **ol = (void**)(*pa)->data;
    *pa = jl_alloc_cell_1d(newsz);
    for(i=0; i < sz; i+=2) {
        if (ol[i+1] != NULL) {
            (*jl_table_lookup_bp(pa, ol[i])) = ol[i+1];
        }
    }
}

static void **jl_table_lookup_bp(jl_array_t **pa, void *key)
{
    uint_t hv;
    jl_array_t *a = *pa;
    size_t orig, index, iter;
    size_t newsz, sz = hash_size(a);
    size_t maxprobe = max_probe(sz);
    void **tab = (void**)a->data;

    hv = keyhash((jl_value_t*)key);
 retry_bp:
    iter = 0;
    index = h2index(hv,sz);
    sz *= 2;
    orig = index;

    do {
        if (tab[index+1] == NULL) {
            tab[index] = key;
            return &tab[index+1];
        }

        if (jl_egal((jl_value_t*)key, (jl_value_t*)tab[index]))
            return &tab[index+1];

        index = (index+2) & (sz-1);
        iter++;
        if (iter > maxprobe)
            break;
    } while (index != orig);

    /* table full */
    /* quadruple size, rehash, retry the insert */
    /* it's important to grow the table really fast; otherwise we waste */
    /* lots of time rehashing all the keys over and over. */
    sz = jl_array_len(a);
    if (sz >= (1<<19) || (sz <= (1<<8)))
        newsz = sz<<1;
    else if (sz <= HT_N_INLINE)
        newsz = HT_N_INLINE;
    else
        newsz = sz<<2;
    jl_idtable_rehash(pa, newsz);

    a = *pa;
    tab = (void**)a->data;
    sz = hash_size(a);
    maxprobe = max_probe(sz);

    goto retry_bp;

    return NULL;
}

/* returns bp if key is in hash, otherwise NULL */
/* if return is non-NULL and *bp == NULL then key was deleted */
static void **jl_table_peek_bp(jl_array_t *a, void *key)
{
    size_t sz = hash_size(a);
    size_t maxprobe = max_probe(sz);
    void **tab = (void**)a->data;
    uint_t hv = keyhash((jl_value_t*)key);
    size_t index = h2index(hv, sz);
    sz *= 2;
    size_t orig = index;
    size_t iter = 0;

    do {
        if (tab[index] == NULL)
            return NULL;
        if (jl_egal((jl_value_t*)key, (jl_value_t*)tab[index]))
            return &tab[index+1];

        index = (index+2) & (sz-1);
        iter++;
        if (iter > maxprobe)
            break;
    } while (index != orig);

    return NULL;
}

DLLEXPORT
jl_array_t *jl_eqtable_put(jl_array_t *h, void *key, void *val)
{
    void **bp = jl_table_lookup_bp(&h, key);
    *bp = val;
    return h;
}

DLLEXPORT
jl_value_t *jl_eqtable_get(jl_array_t *h, void *key, jl_value_t *deflt)
{
    void **bp = jl_table_peek_bp(h, key);
    if (bp == NULL || *bp == NULL)
        return deflt;
    return (jl_value_t*)*bp;
}

DLLEXPORT
jl_value_t *jl_eqtable_pop(jl_array_t *h, void *key, jl_value_t *deflt)
{
    void **bp = jl_table_peek_bp(h, key);
    if (bp == NULL || *bp == NULL)
        return deflt;
    jl_value_t *val = (jl_value_t*)*bp;
    *bp = NULL;
    return val;
}

DLLEXPORT
size_t jl_eqtable_nextind(jl_array_t *t, size_t i)
{
    if (i&1) i++;
    size_t alen = jl_array_dim0(t);
    while (i < alen && ((void**)t->data)[i+1] == NULL)
        i+=2;
    if (i >= alen) return (size_t)-1;
    return i;
}

#undef hash_size
#undef max_probe

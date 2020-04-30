//-*- mode:c -*-

/*
  include this file and call HTIMPL to generate an implementation
*/

#define hash_size(h) ((h)->size/2)

// compute empirical max-probe for a given size
#define max_probe(size) ((size)<=(HT_N_INLINE*2) ? (HT_N_INLINE/2) : (size)>>3)

#define _HTIMPL_EX(HTNAME, HFUNC, EQFUNC, _STATIC)                      \
static void **HTNAME##_lookup_bp_r(htable_t *h, void *key, void *ctx)   \
{                                                                       \
    uint_t hv;                                                          \
    size_t i, orig, index, iter;                                        \
    size_t newsz, sz = hash_size(h);                                    \
    size_t maxprobe = max_probe(sz);                                    \
    void **tab = h->table;                                              \
    void **ol;                                                          \
                                                                        \
    hv = HFUNC((uintptr_t)key, ctx);                                    \
 retry_bp:                                                              \
    iter = 0;                                                           \
    index = (size_t)(hv & (sz-1)) * 2;                                  \
    sz *= 2;                                                            \
    orig = index;                                                       \
                                                                        \
    do {                                                                \
        if (tab[index+1] == HT_NOTFOUND) {                              \
            tab[index] = key;                                           \
            return &tab[index+1];                                       \
        }                                                               \
                                                                        \
        if (EQFUNC(key, tab[index], ctx))                               \
            return &tab[index+1];                                       \
                                                                        \
        index = (index+2) & (sz-1);                                     \
        iter++;                                                         \
        if (iter > maxprobe)                                            \
            break;                                                      \
    } while (index != orig);                                            \
                                                                        \
    /* table full */                                                    \
    /* quadruple size, rehash, retry the insert */                      \
    /* it's important to grow the table really fast; otherwise we waste */ \
    /* lots of time rehashing all the keys over and over. */            \
    sz = h->size;                                                       \
    ol = h->table;                                                      \
    if (sz < HT_N_INLINE)                                              \
        newsz = HT_N_INLINE;                                            \
    else if (sz >= (1<<19) || (sz <= (1<<8)))                           \
        newsz = sz<<1;                                                  \
    else                                                                \
        newsz = sz<<2;                                                  \
    /*printf("trying to allocate %d words.\n", newsz); fflush(stdout);*/ \
    tab = (void**)LLT_ALLOC(newsz*sizeof(void*));                       \
    if (tab == NULL)                                                    \
        return NULL;                                                    \
    for(i=0; i < newsz; i++)                                            \
        tab[i] = HT_NOTFOUND;                                           \
    h->table = tab;                                                     \
    h->size = newsz;                                                    \
    for(i=0; i < sz; i+=2) {                                            \
        if (ol[i+1] != HT_NOTFOUND) {                                   \
            (*HTNAME##_lookup_bp_r(h, ol[i], ctx)) = ol[i+1];           \
        }                                                               \
    }                                                                   \
    if (ol != &h->_space[0])                                            \
        LLT_FREE(ol);                                                   \
                                                                        \
    sz = hash_size(h);                                                  \
    maxprobe = max_probe(sz);                                           \
    tab = h->table;                                                     \
                                                                        \
    goto retry_bp;                                                      \
                                                                        \
    return NULL;                                                        \
}                                                                       \
                                                                        \
_STATIC void HTNAME##_put_r(htable_t *h, void *key, void *val, void *ctx) \
{                                                                       \
    void **bp = HTNAME##_lookup_bp_r(h, key, ctx);                      \
                                                                        \
    *bp = val;                                                          \
}                                                                       \
                                                                        \
_STATIC void **HTNAME##_bp_r(htable_t *h, void *key, void *ctx)         \
{                                                                       \
    return HTNAME##_lookup_bp_r(h, key, ctx);                           \
}                                                                       \
                                                                        \
/* returns bp if key is in hash, otherwise NULL */                      \
/* if return is non-NULL and *bp == HT_NOTFOUND then key was deleted */ \
static void **HTNAME##_peek_bp_r(htable_t *h, void *key, void *ctx)     \
{                                                                       \
    size_t sz = hash_size(h);                                           \
    size_t maxprobe = max_probe(sz);                                    \
    void **tab = h->table;                                              \
    size_t index = (size_t)(HFUNC((uintptr_t)key, ctx) & (sz-1)) * 2;   \
    sz *= 2;                                                            \
    size_t orig = index;                                                \
    size_t iter = 0;                                                    \
                                                                        \
    do {                                                                \
        if (tab[index] == HT_NOTFOUND)                                  \
            return NULL;                                                \
        if (EQFUNC(key, tab[index], ctx))                               \
            return &tab[index+1];                                       \
                                                                        \
        index = (index+2) & (sz-1);                                     \
        iter++;                                                         \
        if (iter > maxprobe)                                            \
            break;                                                      \
    } while (index != orig);                                            \
                                                                        \
    return NULL;                                                        \
}                                                                       \
                                                                        \
_STATIC void *HTNAME##_get_r(htable_t *h, void *key, void *ctx)         \
{                                                                       \
    void **bp = HTNAME##_peek_bp_r(h, key, ctx);                        \
    if (bp == NULL)                                                     \
        return HT_NOTFOUND;                                             \
    return *bp;                                                         \
}                                                                       \
                                                                        \
_STATIC int HTNAME##_has_r(htable_t *h, void *key, void *ctx)           \
{                                                                       \
    return (HTNAME##_get_r(h, key, ctx) != HT_NOTFOUND);                \
}                                                                       \
                                                                        \
_STATIC int HTNAME##_remove_r(htable_t *h, void *key, void *ctx)        \
{                                                                       \
    void **bp = HTNAME##_peek_bp_r(h, key, ctx);                        \
    if (bp != NULL) {                                                   \
        *bp = HT_NOTFOUND;                                              \
        return 1;                                                       \
    }                                                                   \
    return 0;                                                           \
}                                                                       \
                                                                        \
_STATIC void HTNAME##_adjoin_r(htable_t *h, void *key, void *val, void *ctx) \
{                                                                       \
    void **bp = HTNAME##_lookup_bp_r(h, key, ctx);                      \
    if (*bp == HT_NOTFOUND)                                             \
        *bp = val;                                                      \
}

#define HTIMPL(HTNAME, HFUNC, EQFUNC)                                   \
static uint_t HTNAME##_hfunc_wrapper(uintptr_t key, void *ctx)          \
{                                                                       \
    (void)ctx;                                                          \
    return HFUNC(key);                                                  \
}                                                                       \
                                                                        \
static uint_t HTNAME##_eqfunc_wrapper(void *key1, void *key2, void *ctx) \
{                                                                       \
    (void)ctx;                                                          \
    return EQFUNC(key1, key2);                                          \
}                                                                       \
                                                                        \
_HTIMPL_EX(HTNAME, HTNAME##_hfunc_wrapper, HTNAME##_eqfunc_wrapper, static) \
                                                                        \
void HTNAME##_put(htable_t *h, void *key, void *val)                    \
{                                                                       \
    HTNAME##_put_r(h, key, val, NULL);                                  \
}                                                                       \
                                                                        \
void **HTNAME##_bp(htable_t *h, void *key)                              \
{                                                                       \
    return HTNAME##_bp_r(h, key, NULL);                                 \
}                                                                       \
                                                                        \
void *HTNAME##_get(htable_t *h, void *key)                              \
{                                                                       \
    return HTNAME##_get_r(h, key, NULL);                                \
}                                                                       \
                                                                        \
int HTNAME##_has(htable_t *h, void *key)                                \
{                                                                       \
    return HTNAME##_has_r(h, key, NULL);                                \
}                                                                       \
                                                                        \
int HTNAME##_remove(htable_t *h, void *key)                             \
{                                                                       \
    return HTNAME##_remove_r(h, key, NULL);                             \
}                                                                       \
                                                                        \
void HTNAME##_adjoin(htable_t *h, void *key, void *val)                 \
{                                                                       \
    HTNAME##_adjoin_r(h, key, val, NULL);                               \
}

#define HTIMPL_R(HTNAME, HFUNC, EQFUNC) _HTIMPL_EX(HTNAME, HFUNC, EQFUNC, )

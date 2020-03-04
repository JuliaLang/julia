// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <stdlib.h>
#include <string.h>
#include "julia.h"
#include "julia_internal.h"
#ifndef _OS_WINDOWS_
#include <unistd.h>
#endif
#include "julia_assert.h"

// compute empirical max-probe for a given size
#define max_probe(size) ((size) <= 1024 ? 16 : (size) >> 6)
#define h2index(hv, sz) (size_t)((hv) & ((sz)-1))

#ifdef __cplusplus
extern "C" {
#endif

jl_value_t **jl_typeset_lookup_bp(jl_svec_t **pcache, typeset_eq eq, const void *key, uint_t hv, jl_value_t **notfound)
{
    jl_svec_t *cache = jl_atomic_load_relaxed(pcache);
    size_t sz = jl_svec_len(cache);
    if (sz == 0)
        return notfound;
    JL_GC_PUSH1(&cache);
    size_t maxprobe = max_probe(sz);
    jl_value_t **tab = (jl_value_t**)jl_svec_data(cache);
    size_t index = h2index(hv, sz);
    size_t orig = index;
    size_t iter = 0;
    do {
        jl_value_t *val = jl_atomic_load_relaxed(&tab[index]);
        if (val == NULL) {
            JL_GC_POP();
            return notfound;
        }
        if (eq(val, key, hv)) {
            JL_GC_POP();
            return &tab[index];
        }
        index = (index + 1) & (sz - 1);
        iter++;
    } while (iter <= maxprobe && index != orig);
    JL_GC_POP();
    return notfound;
}

jl_value_t *jl_typeset_lookup(jl_svec_t **cache, typeset_eq eq, const void *key, uint_t hv, jl_value_t *notfound)
{
    return *jl_typeset_lookup_bp(cache, eq, key, hv, &notfound);
}

static int typeset_insert_(jl_svec_t *a, uint_t hv, jl_value_t *val)
{
    jl_value_t **tab = (jl_value_t**)jl_svec_data(a);
    size_t sz = jl_svec_len(a);
    if (sz <= 1)
        return 0;
    size_t orig, index, iter;
    iter = 0;
    index = h2index(hv, sz);
    orig = index;
    size_t maxprobe = max_probe(sz);
    do {
        if (tab[index] == NULL) {
            tab[index] = val;
            jl_gc_wb(a, val);
            return 1;
        }
        index = (index + 1) & (sz - 1);
        iter++;
    } while (iter <= maxprobe && index != orig);
    return 0;
}

static void typeset_rehash(jl_svec_t **cache, jl_value_t *parent, typeset_hash hash, size_t newsz);

void jl_typeset_insert(jl_svec_t **cache, jl_value_t *parent, typeset_hash hash, jl_value_t *val)
{
    while (1) {
        if (typeset_insert_(*cache, hash(val), val))
            return;

        /* table full */
        /* rehash to grow and retry the insert */
        /* it's important to grow the table really fast; otherwise we waste */
        /* lots of time rehashing all the keys over and over. */
        size_t newsz;
        size_t sz = jl_svec_len(*cache);
        if (sz < HT_N_INLINE)
            newsz = HT_N_INLINE;
        else if (sz >= (1 << 19) || (sz <= (1 << 8)))
            newsz = sz << 1;
        else
            newsz = sz << 2;
        typeset_rehash(cache, parent, hash, newsz);
    }
}

static void typeset_rehash(jl_svec_t **cache, jl_value_t *parent, typeset_hash hash, size_t newsz)
{
    jl_svec_t *a = *cache;
    jl_value_t **ol = (jl_value_t**)jl_svec_data(a);
    size_t sz = jl_svec_len(a);
    while (1) {
        size_t i;
        jl_svec_t *newa = jl_alloc_svec(newsz);
        JL_GC_PUSH1(&newa);
        for (i = 0; i < sz; i += 1) {
            jl_value_t *val = ol[i];
            if (val != NULL) {
                if (!typeset_insert_(newa, hash(val), val)) {
                    break;
                }
            }
        }
        JL_GC_POP();
        if (i == sz) {
            *cache = newa;
            jl_gc_wb(parent, newa);
            return;
        }
        newsz <<= 1;
    }
}


#ifdef __cplusplus
}
#endif

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

jl_value_t **const jl_notfound = &jl_nothing;

jl_value_t **jl_typeset_lookup_bp(struct jl_typeset_t const *set, jl_value_t *ty, uint_t hv)
{
    jl_svec_t *cache = jl_atomic_load_relaxed(set->cache);
    size_t sz = jl_svec_len(cache);
    if (sz == 0)
        return jl_notfound;
    JL_GC_PUSH1(&cache);
    size_t maxprobe = max_probe(sz);
    jl_value_t **tab = (jl_value_t**)jl_svec_data(cache);
    size_t index = h2index(hv, sz);
    size_t orig = index;
    size_t iter = 0;
    do {
        jl_value_t *val = tab[index];
        if (val == NULL) {
            JL_GC_POP();
            return jl_notfound;
        }
        if (set->eq(set, val, ty)) {
            JL_GC_POP();
            return &tab[index];
        }
        index = (index + 1) & (sz - 1);
        iter++;
    } while (iter <= maxprobe && index != orig);
    JL_GC_POP();
    return jl_notfound;
}

jl_value_t *jl_typeset_lookup(struct jl_typeset_t *const set, jl_value_t *ty, uint_t hv)
{
    return *jl_typeset_lookup_bp(set, ty, hv);
}

static int typeset_insert_(struct jl_typeset_t const *set, jl_svec_t *a, jl_value_t *val)
{
    uint_t hv = set->hash(set, val);
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

static void typeset_rehash(struct jl_typeset_t const *set, size_t newsz);

void jl_typeset_insert(struct jl_typeset_t const *set, jl_value_t *val)
{
    while (1) {
        if (typeset_insert_(set, *set->cache, val))
            return;

        /* table full */
        /* rehash to grow and retry the insert */
        /* it's important to grow the table really fast; otherwise we waste */
        /* lots of time rehashing all the keys over and over. */
        size_t newsz;
        size_t sz = jl_svec_len(*set->cache);
        if (sz < HT_N_INLINE)
            newsz = HT_N_INLINE;
        else if (sz >= (1 << 19) || (sz <= (1 << 8)))
            newsz = sz << 1;
        else
            newsz = sz << 2;
        typeset_rehash(set, newsz);
    }
}

static void typeset_rehash(struct jl_typeset_t const *set, size_t newsz)
{
    jl_svec_t *a = *set->cache;
    jl_value_t **ol = (jl_value_t**)jl_svec_data(a);
    size_t sz = jl_svec_len(a);
    while (1) {
        size_t i;
        jl_svec_t *newa = jl_alloc_svec(newsz);
        JL_GC_PUSH1(&newa);
        for (i = 0; i < sz; i += 1) {
            jl_value_t *val = ol[i];
            if (val != NULL) {
                if (!typeset_insert_(set, newa, val)) {
                    break;
                }
            }
        }
        JL_GC_POP();
        if (i == sz) {
            *set->cache = newa;
            jl_gc_wb(set->parent, newa);
            return;
        }
        newsz <<= 1;
    }
}


#ifdef __cplusplus
}
#endif

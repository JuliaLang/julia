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

static inline size_t jl_intref(const jl_array_t *arr, size_t idx) JL_NOTSAFEPOINT
{
    jl_value_t *el = jl_tparam0(jl_typeof(arr));
    if (el == (jl_value_t*)jl_uint8_type)
        return ((uint8_t*)jl_array_data(arr))[idx];
    else if (el == (jl_value_t*)jl_uint16_type)
        return ((uint16_t*)jl_array_data(arr))[idx];
    else if (el == (jl_value_t*)jl_uint32_type)
        return ((uint32_t*)jl_array_data(arr))[idx];
    else
        abort();
}

static inline void jl_intset(const jl_array_t *arr, size_t idx, size_t val) JL_NOTSAFEPOINT
{
    jl_value_t *el = jl_tparam0(jl_typeof(arr));
    if (el == (jl_value_t*)jl_uint8_type)
        ((uint8_t*)jl_array_data(arr))[idx] = val;
    else if (el == (jl_value_t*)jl_uint16_type)
        ((uint16_t*)jl_array_data(arr))[idx] = val;
    else if (el == (jl_value_t*)jl_uint32_type)
        ((uint32_t*)jl_array_data(arr))[idx] = val;
    else
        abort();
}

static inline size_t jl_max_int(const jl_array_t *arr)
{
    jl_value_t *el = jl_tparam0(jl_typeof(arr));
    if (el == (jl_value_t*)jl_uint8_type)
        return 0xFF;
    else if (el == (jl_value_t*)jl_uint16_type)
        return 0xFFFF;
    else if (el == (jl_value_t*)jl_uint32_type)
        return 0xFFFFFFFF;
    else if (el == (jl_value_t*)jl_any_type)
        return 0;
    else
        abort();
}

static jl_array_t *jl_alloc_int_1d(size_t np, size_t len)
{
    jl_value_t *ty;
    if (np < 0xFF) {
        ty = jl_array_uint8_type;
     }
    else if (np < 0xFFFF) {
        static jl_value_t *int16 JL_ALWAYS_LEAFTYPE = NULL;
        if (int16 == NULL)
            int16 = jl_apply_array_type((jl_value_t*)jl_uint16_type, 1);
        ty = int16;
    }
    else {
        assert(np < 0x7FFFFFFF);
        static jl_value_t *int32 JL_ALWAYS_LEAFTYPE = NULL;
        if (int32 == NULL)
            int32 = jl_apply_array_type((jl_value_t*)jl_uint32_type, 1);
        ty = int32;
    }
    jl_array_t *a = jl_alloc_array_1d(ty, len);
    memset(a->data, 0, len * a->elsize);
    return a;
}

ssize_t jl_smallintset_lookup(jl_array_t *cache, smallintset_eq eq, const void *key, jl_svec_t *data, uint_t hv)
{
    size_t sz = jl_array_len(cache);
    if (sz == 0)
        return -1;
    JL_GC_PUSH1(&cache);
    size_t maxprobe = max_probe(sz);
    size_t index = h2index(hv, sz);
    size_t orig = index;
    size_t iter = 0;
    do {
        size_t val1 = jl_intref(cache, index);
        if (val1 == 0) {
            JL_GC_POP();
            return -1;
        }
        if (eq(val1 - 1, key, data, hv)) {
            JL_GC_POP();
            return val1 - 1;
        }
        index = (index + 1) & (sz - 1);
        iter++;
    } while (iter <= maxprobe && index != orig);
    JL_GC_POP();
    return -1;
}

static int smallintset_insert_(jl_array_t *a, uint_t hv, size_t val1)
{
    size_t sz = jl_array_len(a);
    if (sz <= 1)
        return 0;
    size_t orig, index, iter;
    iter = 0;
    index = h2index(hv, sz);
    orig = index;
    size_t maxprobe = max_probe(sz);
    do {
        if (jl_intref(a, index) == 0) {
            jl_intset(a, index, val1);
            return 1;
        }
        index = (index + 1) & (sz - 1);
        iter++;
    } while (iter <= maxprobe && index != orig);
    return 0;
}

static void smallintset_rehash(jl_array_t **cache, jl_value_t *parent, smallintset_hash hash, jl_svec_t *data, size_t newsz, size_t np);

void jl_smallintset_insert(jl_array_t **cache, jl_value_t *parent, smallintset_hash hash, size_t val, jl_svec_t *data)
{
    if (val + 1 >  jl_max_int(*cache))
        smallintset_rehash(cache, parent, hash, data, jl_array_len(*cache), val + 1);
    while (1) {
        if (smallintset_insert_(*cache, hash(val, data), val + 1))
            return;

        /* table full */
        /* rehash to grow and retry the insert */
        /* it's important to grow the table really fast; otherwise we waste */
        /* lots of time rehashing all the keys over and over. */
        size_t newsz;
        size_t sz = jl_array_len(*cache);
        if (sz < HT_N_INLINE)
            newsz = HT_N_INLINE;
        else if (sz >= (1 << 19) || (sz <= (1 << 8)))
            newsz = sz << 1;
        else
            newsz = sz << 2;
        smallintset_rehash(cache, parent, hash, data, newsz, 0);
    }
}

static void smallintset_rehash(jl_array_t **cache, jl_value_t *parent, smallintset_hash hash, jl_svec_t *data, size_t newsz, size_t np)
{
    jl_array_t *a = *cache;
    size_t sz = jl_array_len(a);
    size_t i;
    for (i = 0; i < sz; i += 1) {
        size_t val = jl_intref(a, i);
        if (val > np)
            np = val;
    }
    while (1) {
        jl_array_t *newa = jl_alloc_int_1d(np, newsz);
        JL_GC_PUSH1(&newa);
        for (i = 0; i < sz; i += 1) {
            size_t val1 = jl_intref(a, i);
            if (val1 != 0) {
                if (!smallintset_insert_(newa, hash(val1 - 1, data), val1)) {
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

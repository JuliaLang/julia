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

// a set of small positive integers representing the indices into another set
// (or dict) where the hash is derived from the keys in the set via the lambdas
// `hash` and `eq` supports concurrent calls to jl_smallintset_lookup (giving
// acquire ordering), provided that a lock is held over calls to
// smallintset_rehash, and the elements of `data` support release-consume
// atomics.

#ifdef __cplusplus
extern "C" {
#endif

static inline size_t ignore_tombstone(size_t val, size_t tombstone) JL_NOTSAFEPOINT
{
    return val == tombstone ? 0 : val;
}
static inline size_t jl_intref(const jl_genericmemory_t *arr, size_t idx) JL_NOTSAFEPOINT
{
    jl_value_t *el = (jl_value_t*)jl_typetagof(arr);
    if (el == jl_memory_uint8_type)
        return ignore_tombstone(jl_atomic_load_relaxed(&((_Atomic(uint8_t)*)arr->ptr)[idx]), (uint8_t)-1);
    else if (el == jl_memory_uint16_type)
        return ignore_tombstone(jl_atomic_load_relaxed(&((_Atomic(uint16_t)*)arr->ptr)[idx]), (uint16_t)-1);
    else if (el == jl_memory_uint32_type)
        return ignore_tombstone(jl_atomic_load_relaxed(&((_Atomic(uint32_t)*)arr->ptr)[idx]), (uint32_t)-1);
    else
        abort();
}

static inline size_t acquire_tombstone(size_t val, size_t tombstone) JL_NOTSAFEPOINT
{
    return val == tombstone ? (size_t)-1 : val;
}
static inline size_t jl_intref_acquire(const jl_genericmemory_t *arr, size_t idx) JL_NOTSAFEPOINT
{
    jl_value_t *el = (jl_value_t*)jl_typetagof(arr);
    if (el == jl_memory_uint8_type)
        return acquire_tombstone(jl_atomic_load_acquire(&((_Atomic(uint8_t)*)arr->ptr)[idx]), (uint8_t)-1);
    else if (el == jl_memory_uint16_type)
        return acquire_tombstone(jl_atomic_load_acquire(&((_Atomic(uint16_t)*)arr->ptr)[idx]), (uint16_t)-1);
    else if (el == jl_memory_uint32_type)
        return acquire_tombstone(jl_atomic_load_acquire(&((_Atomic(uint32_t)*)arr->ptr)[idx]), (uint32_t)-1);
    else
        abort();
}

static inline void jl_intset_release(const jl_genericmemory_t *arr, size_t idx, size_t val) JL_NOTSAFEPOINT
{
    jl_value_t *el = (jl_value_t*)jl_typetagof(arr);
    if (el == jl_memory_uint8_type)
        jl_atomic_store_release(&((_Atomic(uint8_t)*)arr->ptr)[idx], val);
    else if (el == jl_memory_uint16_type)
        jl_atomic_store_release(&((_Atomic(uint16_t)*)arr->ptr)[idx], val);
    else if (el == jl_memory_uint32_type)
        jl_atomic_store_release(&((_Atomic(uint32_t)*)arr->ptr)[idx], val);
    else
        abort();
}

static inline size_t jl_max_int(const jl_genericmemory_t *arr) JL_NOTSAFEPOINT
{
    jl_value_t *el = (jl_value_t*)jl_typetagof(arr);
    if (el == jl_memory_uint8_type)
        return 0xFF;
    else if (el == jl_memory_uint16_type)
        return 0xFFFF;
    else if (el == jl_memory_uint32_type)
        return 0xFFFFFFFF;
    else if (el == jl_memory_any_type)
        return 0;
    else
        abort();
}

void smallintset_empty(const jl_genericmemory_t *a) JL_NOTSAFEPOINT
{
    size_t elsize;
    jl_value_t *el = (jl_value_t*)jl_typetagof(a);
    if (el == jl_memory_uint8_type)
        elsize = sizeof(uint8_t);
    else if (el == jl_memory_uint16_type)
        elsize = sizeof(uint16_t);
    else if (el == jl_memory_uint32_type)
        elsize = sizeof(uint32_t);
    else if (el == jl_memory_any_type)
        elsize = 0;
    else
        abort();
    memset(a->ptr, 0, a->length * elsize);
}

static jl_genericmemory_t *jl_alloc_int_1d(size_t np, size_t len)
{
    jl_value_t *ty;
    if (np < 0xFF)
        ty = jl_memory_uint8_type;
    else if (np < 0xFFFF)
        ty = jl_memory_uint16_type;
    else
        ty = jl_memory_uint32_type;
    assert(np < 0x7FFFFFFF);
    jl_genericmemory_t *a = jl_alloc_genericmemory(ty, len);
    smallintset_empty(a);
    return a;
}

ssize_t jl_smallintset_lookup(jl_genericmemory_t *cache, smallintset_eq eq, const void *key, jl_value_t *data, uint_t hv, int pop)
{
    size_t sz = cache->length;
    if (sz == 0)
        return -1;
    JL_GC_PUSH1(&cache);
    size_t maxprobe = max_probe(sz);
    size_t index = h2index(hv, sz);
    size_t orig = index;
    size_t iter = 0;
    do {
        size_t val1 = jl_intref_acquire(cache, index);
        if (val1 == 0) {
            JL_GC_POP();
            return -1;
        }
        if (val1 != -1 && eq(val1 - 1, key, data, hv)) {
            JL_GC_POP();
            if (pop)
                jl_intset_release(cache, index, (size_t)-1); // replace with tombstone
            return val1 - 1;
        }
        index = (index + 1) & (sz - 1);
        iter++;
    } while (iter <= maxprobe && index != orig);
    JL_GC_POP();
    return -1;
}

static int smallintset_insert_(jl_genericmemory_t *a, uint_t hv, size_t val1) JL_NOTSAFEPOINT
{
    size_t sz = a->length;
    if (sz <= 1)
        return 0;
    size_t orig, index, iter;
    iter = 0;
    index = h2index(hv, sz);
    orig = index;
    size_t maxprobe = max_probe(sz);
    do {
        if (jl_intref(a, index) == 0) {
            jl_intset_release(a, index, val1);
            return 1;
        }
        index = (index + 1) & (sz - 1);
        iter++;
    } while (iter <= maxprobe && index != orig);
    return 0;
}
//}

void jl_smallintset_insert(_Atomic(jl_genericmemory_t*) *pcache, jl_value_t *parent, smallintset_hash hash, size_t val, jl_value_t *data)
{
    jl_genericmemory_t *a = jl_atomic_load_relaxed(pcache);
    if (val + 1 >= jl_max_int(a)) {
        a = smallintset_rehash(a, hash, data, a->length, val + 1);
        jl_atomic_store_release(pcache, a);
        if (parent) jl_gc_wb(parent, a);
    }
    while (1) {
        if (smallintset_insert_(a, hash(val, data), val + 1))
            return;

        /* table full */
        /* rehash to grow and retry the insert */
        /* it's important to grow the table really fast; otherwise we waste */
        /* lots of time rehashing all the keys over and over. */
        size_t newsz;
        a = jl_atomic_load_relaxed(pcache);
        size_t sz = a->length;
        if (sz < HT_N_INLINE)
            newsz = HT_N_INLINE;
        else if (sz >= (1 << 19) || (sz <= (1 << 8)))
            newsz = sz << 1;
        else
            newsz = sz << 2;
        a = smallintset_rehash(a, hash, data, newsz, 0);
        jl_atomic_store_release(pcache, a);
        if (parent) jl_gc_wb(parent, a);
    }
}

jl_genericmemory_t* smallintset_rehash(jl_genericmemory_t* a, smallintset_hash hash, jl_value_t *data, size_t newsz, size_t np)
{
    size_t sz = a->length;
    size_t i;
    for (i = 0; i < sz; i += 1) {
        size_t val = jl_intref(a, i);
        if (val > np)
            np = val;
    }
    while (1) {
        jl_genericmemory_t *newa = jl_alloc_int_1d(np + 1, newsz);
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
        if (i == sz)
            return newa;
        newsz <<= 1;
    }
}

#ifdef __cplusplus
}
#endif

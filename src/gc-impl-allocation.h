// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifdef THIRD_PARY_GC
#error "This file should not be compiled with MMTK GC"
#endif

#ifndef JL_GC_IMPL_ALLOCATION_H
#define JL_GC_IMPL_ALLOCATION_H

#include <stdint.h>
#include "gc-interface-allocation.h"
#include "julia_threads.h"

#ifdef __cplusplus
extern "C" {
#endif

// pools are 16376 bytes large (GC_POOL_SZ - GC_PAGE_OFFSET)
static const int jl_gc_sizeclasses[] = {
#ifdef _P64
    8,
#elif MAX_ALIGN > 4
    // ARM and PowerPC have max alignment larger than pointer,
    // make sure allocation of size 8 has that alignment.
    4, 8,
#else
    4, 8, 12,
#endif

    // 16 pools at 8-byte spacing
    // the 8-byte aligned pools are only used for Strings
    16, 24, 32, 40, 48, 56, 64, 72, 80, 88, 96, 104, 112, 120, 128, 136,
    // 8 pools at 16-byte spacing
    144, 160, 176, 192, 208, 224, 240, 256,

    // the following tables are computed for maximum packing efficiency via the formula:
    // pg = GC_SMALL_PAGE ? 2^12 : 2^14
    // sz = (div.(pg-8, rng).÷16)*16; hcat(sz, (pg-8).÷sz, pg .- (pg-8).÷sz.*sz)'

#ifdef GC_SMALL_PAGE
    // rng = 15:-1:2 (14 pools)
    272, 288, 304, 336, 368, 400, 448, 496, 576, 672, 816, 1008, 1360, 2032
//  15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, /pool
//  16, 64, 144, 64, 48, 96, 64, 128, 64, 64, 16, 64, 16, 32, bytes lost
#else
    // rng = 60:-4:32 (8 pools)
    272, 288, 304, 336, 368, 400, 448, 496,
//  60, 56, 53, 48, 44, 40, 36, 33, /pool
//  64, 256, 272, 256, 192, 384, 256,  16, bytes lost

    // rng = 30:-2:16 (8 pools)
    544, 576, 624, 672, 736, 816, 896, 1008,
//  30, 28, 26, 24, 22, 20, 18, 16, /pool
//  64, 256, 160, 256, 192,  64, 256, 256, bytes lost

    // rng = 15:-1:8 (8 pools)
    1088, 1168, 1248, 1360, 1488, 1632, 1808, 2032
//   15, 14, 13, 12, 11, 10, 9, 8, /pool
//   64, 32, 160, 64, 16, 64, 112,  128, bytes lost
#endif
};
#ifdef GC_SMALL_PAGE
#ifdef _P64
#  define JL_GC_N_POOLS 39
#elif MAX_ALIGN == 8
#  define JL_GC_N_POOLS 40
#else
#  define JL_GC_N_POOLS 41
#endif
#else
#ifdef _P64
#  define JL_GC_N_POOLS 49
#elif MAX_ALIGN == 8
#  define JL_GC_N_POOLS 50
#else
#  define JL_GC_N_POOLS 51
#endif
#endif
static_assert(sizeof(jl_gc_sizeclasses) / sizeof(jl_gc_sizeclasses[0]) == JL_GC_N_POOLS, "");

// the following table is computed as:
// [searchsortedfirst(jl_gc_sizeclasses, i) - 1 for i = 0:16:jl_gc_sizeclasses[end]]
static const uint8_t szclass_table[] =
#ifdef GC_SMALL_PAGE
    {0,1,3,5,7,9,11,13,15,17,18,19,20,21,22,23,24,25,26,27,28,28,29,29,30,30,31,31,31,32,32,32,33,33,33,33,33,34,34,34,34,34,34,35,35,35,35,35,35,35,35,35,36,36,36,36,36,36,36,36,36,36,36,36,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38};
#else
    {0,1,3,5,7,9,11,13,15,17,18,19,20,21,22,23,24,25,26,27,28,28,29,29,30,30,31,31,31,32,32,32,33,33,33,34,34,35,35,35,36,36,36,37,37,37,37,38,38,38,38,38,39,39,39,39,39,40,40,40,40,40,40,40,41,41,41,41,41,42,42,42,42,42,43,43,43,43,43,44,44,44,44,44,44,44,45,45,45,45,45,45,45,45,46,46,46,46,46,46,46,46,46,47,47,47,47,47,47,47,47,47,47,47,48,48,48,48,48,48,48,48,48,48,48,48,48,48};
#endif
static_assert(sizeof(szclass_table) == 128, "");

STATIC_INLINE uint8_t JL_CONST_FUNC jl_gc_szclass(unsigned sz) JL_NOTSAFEPOINT
{
    assert(sz <= 2032);
#ifdef _P64
    if (sz <= 8)
        return 0;
    const int N = 0;
#elif MAX_ALIGN == 8
    if (sz <= 8)
        return (sz >= 4 ? 1 : 0);
    const int N = 1;
#else
    if (sz <= 12)
        return (sz >= 8 ? 2 : (sz >= 4 ? 1 : 0));
    const int N = 2;
#endif
    uint8_t klass = szclass_table[(sz + 15) / 16];
    return klass + N;
}

STATIC_INLINE uint8_t JL_CONST_FUNC jl_gc_szclass_align8(unsigned sz) JL_NOTSAFEPOINT
{
    if (sz >= 16 && sz <= 152) {
#ifdef _P64
        const int N = 0;
#elif MAX_ALIGN == 8
        const int N = 1;
#else
        const int N = 2;
#endif
        return (sz + 7)/8 - 1 + N;
    }
    return jl_gc_szclass(sz);
}

STATIC_INLINE jl_value_t *jl_gc_alloc_(jl_ptls_t ptls, size_t sz, void *ty)
{
    jl_value_t *v;
    const size_t allocsz = sz + sizeof(jl_taggedvalue_t);
    if (sz <= GC_MAX_SZCLASS) {
        int pool_id = jl_gc_szclass(allocsz);
        jl_gc_pool_t *p = &ptls->heap.norm_pools[pool_id];
        int osize = jl_gc_sizeclasses[pool_id];
        // We call `jl_gc_pool_alloc_noinline` instead of `jl_gc_pool_alloc` to avoid double-counting in
        // the Allocations Profiler. (See https://github.com/JuliaLang/julia/pull/43868 for more details.)
        v = jl_gc_pool_alloc_noinline(ptls, (char*)p - (char*)ptls, osize);
    }
    else {
        if (allocsz < sz) // overflow in adding offs, size was "negative"
            jl_throw(jl_memory_exception);
        v = jl_gc_big_alloc_noinline(ptls, allocsz);
    }
    jl_set_typeof(v, ty);
    maybe_record_alloc_to_profile(v, sz, (jl_datatype_t*)ty);
    return v;
}

/* Programming style note: When using jl_gc_alloc, do not JL_GC_PUSH it into a
 * gc frame, until it has been fully initialized. An uninitialized value in a
 * gc frame can crash upon encountering the first safepoint. By delaying use of
 * the JL_GC_PUSH macro until the value has been initialized, any accidental
 * safepoints will be caught by the GC analyzer.
 */
JL_DLLEXPORT jl_value_t *jl_gc_alloc(jl_ptls_t ptls, size_t sz, void *ty);
// On GCC, only inline when sz is constant
#ifdef __GNUC__
#  define jl_gc_alloc(ptls, sz, ty)  \
    (__builtin_constant_p(sz) ?      \
     jl_gc_alloc_(ptls, sz, ty) :    \
     (jl_gc_alloc)(ptls, sz, ty))
#else
#  define jl_gc_alloc(ptls, sz, ty) jl_gc_alloc_(ptls, sz, ty)
#endif

#ifdef __cplusplus
}
#endif

#endif // JL_GC_IMPL_ALLOCATION_H

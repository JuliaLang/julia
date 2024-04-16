// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef GC_PAGE_PROFILER_H
#define GC_PAGE_PROFILER_H

#include "gc.h"

#ifdef __cplusplus
extern "C" {
#endif

#define GC_TYPE_STR_MAXLEN (512)

typedef struct {
    arraylist_t typestrs;
    char *data;
    int osize;
} gc_page_profiler_serializer_t;

// mutex for page profile
extern uv_mutex_t page_profile_lock;

// Serializer functions
gc_page_profiler_serializer_t gc_page_serializer_create(void) JL_NOTSAFEPOINT;
void gc_page_serializer_init(gc_page_profiler_serializer_t *serializer, jl_gc_pagemeta_t *pg) JL_NOTSAFEPOINT;
void gc_page_serializer_destroy(gc_page_profiler_serializer_t *serializer) JL_NOTSAFEPOINT;
void gc_page_serializer_write(gc_page_profiler_serializer_t *serializer, const char *str) JL_NOTSAFEPOINT;
// Page profile functions
#define GC_SERIALIZER_EMPTY ((const char *)0x1)
#define GC_SERIALIZER_GARBAGE ((const char *)0x2)
STATIC_INLINE void gc_page_profile_write_empty_page(gc_page_profiler_serializer_t *serializer,
                                 int enabled) JL_NOTSAFEPOINT
{
    if (__unlikely(enabled)) {
        gc_page_serializer_write(serializer, GC_SERIALIZER_EMPTY);
    }
}
STATIC_INLINE void gc_page_profile_write_garbage(gc_page_profiler_serializer_t *serializer,
                                                 int enabled) JL_NOTSAFEPOINT
{
    if (__unlikely(enabled)) {
        gc_page_serializer_write(serializer, GC_SERIALIZER_GARBAGE);
    }
}
STATIC_INLINE void gc_page_profile_write_live_obj(gc_page_profiler_serializer_t *serializer,
                                                  jl_taggedvalue_t *v,
                                                  int enabled) JL_NOTSAFEPOINT
{
    if (__unlikely(enabled)) {
        const char *name = jl_typeof_str(jl_valueof(v));
        gc_page_serializer_write(serializer, name);
    }
}
void gc_enable_page_profile(void) JL_NOTSAFEPOINT;
void gc_disable_page_profile(void) JL_NOTSAFEPOINT;
int gc_page_profile_is_enabled(void) JL_NOTSAFEPOINT;
void gc_page_profile_write_to_file(gc_page_profiler_serializer_t *serializer) JL_NOTSAFEPOINT;

#ifdef __cplusplus
}
#endif

#endif // GC_PAGE_PROFILER_H

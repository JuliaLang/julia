// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef GC_PAGE_PROFILER_H
#define GC_PAGE_PROFILER_H

#include "gc.h"

#ifdef __cplusplus
extern "C" {
#endif

#define GC_PAGE_PROFILER_SERIALIZER_INIT_CAPACITY (4096)

typedef struct {
    arraylist_t typestrs;
    char *data;
    int osize;
    char *buffers;
    size_t cursor;
    size_t capacity;
} gc_page_profiler_serializer_t;

// mutex for page profile
extern uv_mutex_t page_profile_lock;
// whether page profiling is enabled
extern int page_profile_enabled;

// Serializer functions
gc_page_profiler_serializer_t gc_page_serializer_create(void) JL_NOTSAFEPOINT;
void gc_page_serializer_init(gc_page_profiler_serializer_t *serializer,
                             jl_gc_pagemeta_t *pg) JL_NOTSAFEPOINT;
void gc_page_serializer_destroy(gc_page_profiler_serializer_t *serializer) JL_NOTSAFEPOINT;
void gc_page_serializer_write(gc_page_profiler_serializer_t *serializer,
                              const char *str) JL_NOTSAFEPOINT;
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
STATIC_INLINE char *gc_page_profile_request_buffer(gc_page_profiler_serializer_t *serializer, size_t size) JL_NOTSAFEPOINT
{
    while (serializer->cursor + size >= serializer->capacity) {
        serializer->capacity *= 2;
        serializer->buffers = (char *)realloc_s(serializer->buffers, serializer->capacity);
    }
    char *p = &serializer->buffers[serializer->cursor];
    memset(p, 0, size);
    serializer->cursor += size;
    return p;
}
STATIC_INLINE void gc_page_profile_write_live_obj(gc_page_profiler_serializer_t *serializer,
                                                  jl_taggedvalue_t *v,
                                                  int enabled) JL_NOTSAFEPOINT
{
    if (__unlikely(enabled)) {
        jl_value_t *a = jl_valueof(v);
        jl_value_t *t = jl_typeof(a);
        ios_t str_;
        int ios_need_close = 0;
        char *type_name = NULL;
        char *type_name_in_serializer = NULL;
        if (t == (jl_value_t *)jl_get_buff_tag()) {
            type_name = "Buffer";
            type_name_in_serializer =
                gc_page_profile_request_buffer(serializer, strlen(type_name) + 1);
            strcpy(type_name_in_serializer, type_name);
        }
        else if (jl_is_string(a)) {
            type_name = "String";
            type_name_in_serializer =
                gc_page_profile_request_buffer(serializer, strlen(type_name) + 1);
            strcpy(type_name_in_serializer, type_name);
        }
        else if (jl_is_symbol(a)) {
            type_name = jl_symbol_name((jl_sym_t *)a);
            type_name_in_serializer =
                gc_page_profile_request_buffer(serializer, strlen(type_name) + 1);
            strcpy(type_name_in_serializer, type_name);
        }
        else if (jl_is_simplevector(a)) {
            type_name = "SimpleVector";
            type_name_in_serializer =
                gc_page_profile_request_buffer(serializer, strlen(type_name) + 1);
            strcpy(type_name_in_serializer, type_name);
        }
        else if (jl_is_module(a)) {
            type_name = jl_symbol_name_(((jl_module_t *)a)->name);
            type_name_in_serializer =
                gc_page_profile_request_buffer(serializer, strlen(type_name) + 1);
            strcpy(type_name_in_serializer, type_name);
        }
        else if (jl_is_task(a)) {
            type_name = "Task";
            type_name_in_serializer =
                gc_page_profile_request_buffer(serializer, strlen(type_name) + 1);
            strcpy(type_name_in_serializer, type_name);
        }
        else if (jl_is_datatype(a)) {
            ios_need_close = 1;
            ios_mem(&str_, 0);
            JL_STREAM *str = (JL_STREAM *)&str_;
            jl_static_show(str, a);
            type_name = str_.buf;
            type_name_in_serializer =
                gc_page_profile_request_buffer(serializer, str_.size + 1);
            memcpy(type_name_in_serializer, type_name, str_.size);
        }
        else {
            ios_need_close = 1;
            ios_mem(&str_, 0);
            JL_STREAM *str = (JL_STREAM *)&str_;
            jl_static_show(str, t);
            type_name = str_.buf;
            type_name_in_serializer =
                gc_page_profile_request_buffer(serializer, str_.size + 1);
            memcpy(type_name_in_serializer, type_name, str_.size);
        }
        gc_page_serializer_write(serializer, type_name_in_serializer);
        if (ios_need_close) {
            ios_close(&str_);
        }
        jl_may_leak(type_name_in_serializer);
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

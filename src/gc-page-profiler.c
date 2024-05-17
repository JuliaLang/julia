// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "gc-page-profiler.h"

#ifdef __cplusplus
extern "C" {
#endif

// whether page profiling is enabled
int page_profile_enabled;
// number of pages written
size_t page_profile_pages_written;
// stream to write page profile to
ios_t *page_profile_stream;
// mutex for page profile
uv_mutex_t page_profile_lock;

gc_page_profiler_serializer_t gc_page_serializer_create(void) JL_NOTSAFEPOINT
{
    gc_page_profiler_serializer_t serializer;
    if (__unlikely(page_profile_enabled)) {
        arraylist_new(&serializer.typestrs, GC_PAGE_SZ);
        serializer.buffers = (char *)malloc_s(GC_PAGE_PROFILER_SERIALIZER_INIT_CAPACITY);
        serializer.cursor = 0;
    }
    else {
        serializer.typestrs.len = 0;
    }
    return serializer;
}

void gc_page_serializer_init(gc_page_profiler_serializer_t *serializer,
                             jl_gc_pagemeta_t *pg) JL_NOTSAFEPOINT
{
    if (__unlikely(page_profile_enabled)) {
        serializer->typestrs.len = 0;
        serializer->data = (char *)pg->data;
        serializer->osize = pg->osize;
        serializer->cursor = 0;
        serializer->capacity = GC_PAGE_PROFILER_SERIALIZER_INIT_CAPACITY;
    }
}

void gc_page_serializer_destroy(gc_page_profiler_serializer_t *serializer) JL_NOTSAFEPOINT
{
    if (__unlikely(page_profile_enabled)) {
        arraylist_free(&serializer->typestrs);
        free(serializer->buffers);
    }
}

void gc_page_serializer_write(gc_page_profiler_serializer_t *serializer,
                              const char *str) JL_NOTSAFEPOINT
{
    if (__unlikely(page_profile_enabled)) {
        arraylist_push(&serializer->typestrs, (void *)str);
    }
}

void gc_enable_page_profile(void) JL_NOTSAFEPOINT
{
    page_profile_enabled = 1;
}

void gc_disable_page_profile(void) JL_NOTSAFEPOINT
{
    page_profile_enabled = 0;
}

int gc_page_profile_is_enabled(void) JL_NOTSAFEPOINT
{
    return page_profile_enabled;
}

void gc_page_profile_write_preamble(gc_page_profiler_serializer_t *serializer)
    JL_NOTSAFEPOINT
{
    if (__unlikely(page_profile_enabled)) {
        const size_t large_enough_str_size = 4096;
        char str[large_enough_str_size];
        snprintf(str, large_enough_str_size,
                 "{\"address\": \"%p\",\"object_size\": %d,\"objects\": [",
                 serializer->data, serializer->osize);
        ios_write(page_profile_stream, str, strlen(str));
    }
}

void gc_page_profile_write_epilogue(gc_page_profiler_serializer_t *serializer)
    JL_NOTSAFEPOINT
{
    if (__unlikely(page_profile_enabled)) {
        const char *str = "]}";
        ios_write(page_profile_stream, str, strlen(str));
    }
}

void gc_page_profile_write_comma(gc_page_profiler_serializer_t *serializer) JL_NOTSAFEPOINT
{
    if (__unlikely(page_profile_enabled)) {
        // write comma if not first page
        if (page_profile_pages_written > 0) {
            const char *str = ",";
            ios_write(page_profile_stream, str, strlen(str));
        }
    }
}

void gc_page_profile_write_to_file(gc_page_profiler_serializer_t *serializer)
    JL_NOTSAFEPOINT
{
    size_t large_enough_str_size = 4096;
    if (__unlikely(page_profile_enabled)) {
        // write to file
        uv_mutex_lock(&page_profile_lock);
        gc_page_profile_write_comma(serializer);
        gc_page_profile_write_preamble(serializer);
        char *str = (char *)malloc_s(large_enough_str_size);
        for (size_t i = 0; i < serializer->typestrs.len; i++) {
            const char *name = (const char *)serializer->typestrs.items[i];
            if (name == GC_SERIALIZER_EMPTY) {
                snprintf(str, large_enough_str_size, "\"empty\",");
            }
            else if (name == GC_SERIALIZER_GARBAGE) {
                snprintf(str, large_enough_str_size, "\"garbage\",");
            }
            else {
                while ((strlen(name) + 1) > large_enough_str_size) {
                    large_enough_str_size *= 2;
                    str = (char *)realloc_s(str, large_enough_str_size);
                }
                snprintf(str, large_enough_str_size, "\"%s\",", name);
            }
            // remove trailing comma for last element
            if (i == serializer->typestrs.len - 1) {
                str[strlen(str) - 1] = '\0';
            }
            ios_write(page_profile_stream, str, strlen(str));
        }
        free(str);
        gc_page_profile_write_epilogue(serializer);
        page_profile_pages_written++;
        uv_mutex_unlock(&page_profile_lock);
    }
}

void gc_page_profile_write_json_preamble(ios_t *stream) JL_NOTSAFEPOINT
{
    if (__unlikely(page_profile_enabled)) {
        uv_mutex_lock(&page_profile_lock);
        const char *str = "{\"pages\": [";
        ios_write(stream, str, strlen(str));
        uv_mutex_unlock(&page_profile_lock);
    }
}

void gc_page_profile_write_json_epilogue(ios_t *stream) JL_NOTSAFEPOINT
{
    if (__unlikely(page_profile_enabled)) {
        uv_mutex_lock(&page_profile_lock);
        const char *str = "]}";
        ios_write(stream, str, strlen(str));
        uv_mutex_unlock(&page_profile_lock);
    }
}

JL_DLLEXPORT void jl_gc_take_page_profile(ios_t *stream)
{
    gc_enable_page_profile();
    page_profile_pages_written = 0;
    page_profile_stream = stream;
    gc_page_profile_write_json_preamble(stream);
    jl_gc_collect(JL_GC_FULL);
    gc_page_profile_write_json_epilogue(stream);
    gc_disable_page_profile();
}

#ifdef __cplusplus
}
#endif

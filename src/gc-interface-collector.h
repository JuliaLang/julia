// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "julia.h"
#include "julia_gcext.h"
#include "julia_assert.h"

#ifndef JL_GC_INTERFACE_COLLECTOR_H
#define JL_GC_INTERFACE_COLLECTOR_H

// =======
// Callbacks
// =======

typedef void (*jl_gc_cb_func_t)(void);

typedef struct jl_gc_callback_list_t {
    struct jl_gc_callback_list_t *next;
    jl_gc_cb_func_t func;
} jl_gc_callback_list_t;

#define gc_invoke_callbacks(ty, list, args) \
    do { \
        for (jl_gc_callback_list_t *cb = list; \
                cb != NULL; \
                cb = cb->next) \
        { \
            ((ty)(cb->func)) args; \
        } \
    } while (0)

JL_DLLEXPORT void jl_gc_set_cb_root_scanner(jl_gc_cb_root_scanner_t cb, int enable);
JL_DLLEXPORT void jl_gc_set_cb_task_scanner(jl_gc_cb_task_scanner_t cb, int enable);
JL_DLLEXPORT void jl_gc_set_cb_pre_gc(jl_gc_cb_pre_gc_t cb, int enable);
JL_DLLEXPORT void jl_gc_set_cb_post_gc(jl_gc_cb_post_gc_t cb, int enable);
JL_DLLEXPORT void jl_gc_set_cb_notify_external_alloc(jl_gc_cb_notify_external_alloc_t cb, int enable);
JL_DLLEXPORT void jl_gc_set_cb_notify_external_free(jl_gc_cb_notify_external_free_t cb, int enable);

// =======
// malloc wrappers, aligned allocation
// =======

void *jl_malloc_aligned(size_t sz, size_t align);
void *jl_realloc_aligned(void *p, size_t sz, size_t oldsz, size_t align);
void jl_free_aligned(void *p) JL_NOTSAFEPOINT;
#define malloc_cache_align(sz) jl_malloc_aligned(sz, JL_CACHE_BYTE_ALIGNMENT)
#define realloc_cache_align(p, sz, oldsz) jl_realloc_aligned(p, sz, oldsz, JL_CACHE_BYTE_ALIGNMENT)

// =======
// weak references
// =======

JL_DLLEXPORT jl_weakref_t *jl_gc_new_weakref_th(jl_ptls_t ptls, jl_value_t *value);

// =======
// big values
// =======

// Size includes the tag and the tag is not cleared!!
jl_value_t *jl_gc_big_alloc_inner(jl_ptls_t ptls, size_t sz);
// Size includes the tag and the tag is not cleared!!
jl_value_t *jl_gc_pool_alloc_inner(jl_ptls_t ptls, int pool_offset, int osize);
void jl_gc_free_array(jl_array_t *a) JL_NOTSAFEPOINT;

// =======
// Collection triggers
// =======

void enable_collection(void);
void disable_collection(void);
void maybe_collect(jl_ptls_t ptls);
JL_DLLEXPORT void jl_gc_collect(jl_gc_collection_t collection);

// =======
// Per-thread GC initialization
// =======

void jl_init_thread_heap(jl_ptls_t ptls);
void jl_post_init_thread_heap(jl_ptls_t ptls);

// =======
// System-wide GC initialization
// =======

void jl_gc_init(void);

// =======
// allocation wrappers that track allocation and let collection run
// =======

JL_DLLEXPORT void *jl_gc_counted_malloc(size_t sz);
JL_DLLEXPORT void *jl_gc_counted_calloc(size_t nm, size_t sz);
JL_DLLEXPORT void jl_gc_counted_free_with_size(void *p, size_t sz);
JL_DLLEXPORT void *jl_gc_counted_realloc_with_old_size(void *p, size_t old, size_t sz);

// =======
// String reallocation
// =======

jl_value_t *jl_gc_realloc_string(jl_value_t *s, size_t sz);

// =======
//  helpers for conservative/foreign object GC support
// =======

JL_DLLEXPORT int jl_gc_enable_conservative_gc_support(void);
JL_DLLEXPORT int jl_gc_conservative_gc_support_enabled(void);
JL_DLLEXPORT int jl_gc_mark_queue_obj(jl_ptls_t ptls, jl_value_t *obj);
JL_DLLEXPORT void jl_gc_mark_queue_objarray(jl_ptls_t ptls, jl_value_t *parent, jl_value_t **objs, size_t nobjs);
JL_DLLEXPORT jl_value_t *jl_gc_internal_obj_base_ptr(void *p);

// =======
//  Permanent allocation
// =======

void *jl_gc_perm_alloc(size_t sz, int zero, unsigned align, unsigned offset);
void jl_gc_notify_image_load(const char* img_data, size_t len);
void jl_gc_notify_image_alloc(char* img_data, size_t len);

#endif // JL_GC_INTERFACE_COLLECTOR_H

#ifndef JL_GCEXT_H
#define JL_GCEXT_H

// requires including "julia.h" beforehand.

// Kinds of callbacks. For each kind of callback, there must be
// a corresponding type with the same name, but in all lowercase,
// and with a "_t" suffix.

typedef void (*jl_gc_cb_root_scanner_t)(int full);
typedef void (*jl_gc_cb_task_scanner_t)(jl_task_t *task, int full);
typedef void (*jl_gc_cb_pre_gc_t)(int full);
typedef void (*jl_gc_cb_post_gc_t)(int full);

JL_DLLEXPORT void jl_gc_set_cb_root_scanner(jl_gc_cb_root_scanner_t cb, int enable);
JL_DLLEXPORT void jl_gc_set_cb_task_scanner(jl_gc_cb_task_scanner_t cb, int enable);
JL_DLLEXPORT void jl_gc_set_cb_pre_gc(jl_gc_cb_pre_gc_t cb, int enable);
JL_DLLEXPORT void jl_gc_set_cb_post_gc(jl_gc_cb_post_gc_t cb, int enable);

// Types for mark and sweep functions.
// We make the cache and sp parameters opaque so that the internals
// do not get exposed.
typedef uintptr_t (*jl_markfunc_t)(jl_ptls_t, jl_value_t *obj);
typedef void (*jl_sweepfunc_t)(jl_value_t *obj);

// Function to create a new foreign type with custom
// mark and sweep functions.
JL_DLLEXPORT jl_datatype_t *jl_new_foreign_type(
        jl_sym_t *name,
        jl_module_t *module,
        jl_datatype_t *super,
        jl_markfunc_t markfunc,
        jl_sweepfunc_t sweepfunc,
        int haspointers,
        int large);

// Field layout descriptor for custom types that do
// not fit Julia layout conventions. This is associated with
// jl_datatype_t instances where fielddesc_type == 3.

typedef struct {
    jl_markfunc_t markfunc;
    jl_sweepfunc_t sweepfunc;
} jl_fielddescdyn_t;

JL_DLLEXPORT void *jl_gc_alloc_typed(jl_ptls_t ptls, size_t sz, void *ty);
JL_DLLEXPORT int jl_gc_mark_queue_obj(jl_ptls_t ptls, jl_value_t *obj);
JL_DLLEXPORT void jl_gc_mark_queue_objarray(jl_ptls_t ptls, jl_value_t *parent,
    jl_value_t **objs, size_t nobjs);

JL_DLLEXPORT void jl_gc_schedule_foreign_sweepfunc(jl_ptls_t ptls, jl_value_t * bj);

#endif // _JULIA_GCEXT_H

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
typedef void (*jl_gc_cb_notify_external_alloc_t)(void *addr, size_t size);
typedef void (*jl_gc_cb_notify_external_free_t)(void *addr);

JL_DLLEXPORT void jl_gc_set_cb_root_scanner(jl_gc_cb_root_scanner_t cb, int enable);
JL_DLLEXPORT void jl_gc_set_cb_task_scanner(jl_gc_cb_task_scanner_t cb, int enable);
JL_DLLEXPORT void jl_gc_set_cb_pre_gc(jl_gc_cb_pre_gc_t cb, int enable);
JL_DLLEXPORT void jl_gc_set_cb_post_gc(jl_gc_cb_post_gc_t cb, int enable);
JL_DLLEXPORT void jl_gc_set_cb_notify_external_alloc(jl_gc_cb_notify_external_alloc_t cb,
        int enable);
JL_DLLEXPORT void jl_gc_set_cb_notify_external_free(jl_gc_cb_notify_external_free_t cb,
        int enable);

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

JL_DLLEXPORT size_t jl_gc_max_internal_obj_size(void);
JL_DLLEXPORT size_t jl_gc_external_obj_hdr_size(void);

// Field layout descriptor for custom types that do
// not fit Julia layout conventions. This is associated with
// jl_datatype_t instances where fielddesc_type == 3.

typedef struct {
    jl_markfunc_t markfunc;
    jl_sweepfunc_t sweepfunc;
} jl_fielddescdyn_t;

// Allocate an object with a foreign type.
JL_DLLEXPORT void *jl_gc_alloc_typed(jl_ptls_t ptls, size_t sz, void *ty);
// Queue an object or array of objects for scanning by the garbage collector.
// These functions must only be called from within a root scanner callback
// or from within a custom mark function.
JL_DLLEXPORT int jl_gc_mark_queue_obj(jl_ptls_t ptls, jl_value_t *obj);
JL_DLLEXPORT void jl_gc_mark_queue_objarray(jl_ptls_t ptls, jl_value_t *parent,
    jl_value_t **objs, size_t nobjs);

// Calling this function on an object of a foreign type will cause the
// sweep function to be called during garbage collection. This function
// must be called at most once per object. If not enabled for an object,
// the sweep function will not be called. Normally, this should happen
// right after allocating such an object.
JL_DLLEXPORT void jl_gc_schedule_foreign_sweepfunc(jl_ptls_t ptls, jl_value_t * bj);

// CAUTION: The following functions enable support for conservative
// marking. Conservative marking will treat any machine word that points
// to an object (potentially including its interior) as a valid
// reference, regardless of whether it actually is one or just an
// integer that happens to match an actual address.
//
// This is a sharp tool and should only be used as a measure of last
// resort. The user should be familiar with the risk of memory leaks
// (especially on 32-bit machines) if used inappropriately and how
// optimizing compilers can hide references from conservative stack
// scanning. In particular, arrays must be kept explicitly visible to
// the GC (by using JL_GC_PUSH1(), storing them in a location marked by
// the Julia GC, etc.) while their contents are being accessed. The
// reason is that array contents aren't marked separately, so if the
// object itself is not visible to the GC, neither are the contents.

// Enable support for conservative scanning. The function returns
// whether support was already enabled. The function may implicitly
// trigger a full garbage collection to properly update all internal
// data structures.
JL_DLLEXPORT int jl_gc_enable_conservative_gc_support(void);

// This function returns whether support for conservative scanning has
// been enabled. The return values are the same as for
// jl_gc_enable_conservative_gc_support().
JL_DLLEXPORT int jl_gc_conservative_gc_support_enabled(void);

// Returns the base address of a memory block, assuming it is stored in
// a julia memory pool. Return NULL otherwise.
//
// NOTE: This will only work for internal pool allocations. For external
// allocations, the user must track allocations using the notification
// callbacks above and verify that they are valid objects. Note that
// external allocations may not all be valid objects and that for those,
// the user *must* validate that they have a proper type, i.e. that
// jl_typeof(obj) is an actual type object.
JL_DLLEXPORT jl_value_t *jl_gc_internal_obj_base_ptr(void *p);


#endif // _JULIA_GCEXT_H

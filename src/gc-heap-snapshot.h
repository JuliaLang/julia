// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_GC_HEAP_SNAPSHOT_H
#define JL_GC_HEAP_SNAPSHOT_H

#include "julia.h"
#include "ios.h"

#ifdef __cplusplus
extern "C" {
#endif


// ---------------------------------------------------------------------
// Functions to call from GC when heap snapshot is enabled
// ---------------------------------------------------------------------
void _gc_heap_snapshot_record_root(void *snapshot, jl_value_t *root, char *name) JL_NOTSAFEPOINT;
void _gc_heap_snapshot_record_frame_to_object_edge(void *snapshot, void *from, jl_value_t *to) JL_NOTSAFEPOINT;
void _gc_heap_snapshot_record_task_to_frame_edge(void *snapshot, jl_task_t *from, void *to) JL_NOTSAFEPOINT;
void _gc_heap_snapshot_record_frame_to_frame_edge(void *snapshot, jl_gcframe_t *from, jl_gcframe_t *to) JL_NOTSAFEPOINT;
void _gc_heap_snapshot_record_array_edge(void *snapshot, jl_value_t *from, jl_value_t *to, size_t index) JL_NOTSAFEPOINT;
void _gc_heap_snapshot_record_object_edge(void *snapshot, jl_value_t *from, jl_value_t *to, void* slot) JL_NOTSAFEPOINT;
void _gc_heap_snapshot_record_property_edge(void *snapshot, jl_value_t *from, jl_value_t *to, const char *property) JL_NOTSAFEPOINT;
void _gc_heap_snapshot_record_module_to_binding(void *snapshot, jl_module_t* module, jl_value_t *bindings, jl_value_t *bindingkeyset) JL_NOTSAFEPOINT;
// Used for objects managed by GC, but which aren't exposed in the julia object, so have no
// field or index.  i.e. they're not reachable from julia code, but we _will_ hit them in
// the GC mark phase (so we can check their type tag to get the size).
void _gc_heap_snapshot_record_internal_array_edge(void *snapshot, jl_value_t *from, jl_value_t *to) JL_NOTSAFEPOINT;
// Used for objects manually allocated in C (outside julia GC), to still tell the heap snapshot about the
// size of the object, even though we're never going to mark that object.
void _gc_heap_snapshot_record_foreign_memory_edge(void *snapshot, jl_value_t *from, void* to, size_t bytes) JL_NOTSAFEPOINT;
// Used for objects that are reachable from the GC roots
void _gc_heap_snapshot_record_gc_roots(void *snapshot, jl_value_t *root, char *name) JL_NOTSAFEPOINT;
// Used for objects that are reachable from the finalizer list
void _gc_heap_snapshot_record_finlist(void *snapshot, jl_value_t *finlist, size_t index) JL_NOTSAFEPOINT;
// Used for objects reachable from the binding partition pointer union
void _gc_heap_snapshot_record_binding_partition_edge(void *snapshot, jl_value_t *from, jl_value_t *to) JL_NOTSAFEPOINT;
void *_gc_start_custom_heap_snapshot(ios_t *nodes, ios_t *edges,
    ios_t *strings, ios_t *json, char redact_data);
void _gc_finish_custom_heap_snapshot(void *snapshot);

extern int gc_heap_snapshot_enabled;
extern int prev_sweep_full;
extern jl_mutex_t heapsnapshot_lock;

int gc_slot_to_fieldidx(void *_obj, void *slot, jl_datatype_t *vt) JL_NOTSAFEPOINT;
int gc_slot_to_arrayidx(void *_obj, void *begin) JL_NOTSAFEPOINT;

static inline void gc_heap_snapshot_record_frame_to_object_edge(void *snapshot, void *from, jl_value_t *to) JL_NOTSAFEPOINT
{
    if (__unlikely(snapshot)) {
        _gc_heap_snapshot_record_frame_to_object_edge(snapshot, from, to);
    }
}
static inline void gc_heap_snapshot_record_task_to_frame_edge(void *snapshot, jl_task_t *from, void *to) JL_NOTSAFEPOINT
{
    if (__unlikely(snapshot)) {
        _gc_heap_snapshot_record_task_to_frame_edge(snapshot, from, to);
    }
}
static inline void gc_heap_snapshot_record_frame_to_frame_edge(void *snapshot, jl_gcframe_t *from, jl_gcframe_t *to) JL_NOTSAFEPOINT
{
    if (__unlikely(snapshot)) {
        _gc_heap_snapshot_record_frame_to_frame_edge(snapshot, from, to);
    }
}
static inline void gc_heap_snapshot_record_root(void *snapshot, jl_value_t *root, char *name) JL_NOTSAFEPOINT
{
    if (__unlikely(snapshot)) {
        _gc_heap_snapshot_record_root(snapshot, root, name);
    }
}
static inline void gc_heap_snapshot_record_array_edge_index(void *snapshot, jl_value_t *from, jl_value_t *to, size_t index) JL_NOTSAFEPOINT
{
    if (__unlikely(snapshot && from != NULL && to != NULL)) {
        _gc_heap_snapshot_record_array_edge(snapshot, from, to, index);
    }
}
static inline void gc_heap_snapshot_record_array_edge(void *snapshot, jl_value_t *from, jl_value_t **to) JL_NOTSAFEPOINT
{
    if (__unlikely(snapshot)) {
        _gc_heap_snapshot_record_array_edge(snapshot, from, *to, gc_slot_to_arrayidx(from, to));
    }
}
static inline void gc_heap_snapshot_record_object_edge(void *snapshot, jl_value_t *from, jl_value_t **to) JL_NOTSAFEPOINT
{
    if (__unlikely(snapshot)) {
        _gc_heap_snapshot_record_object_edge(snapshot, from, *to, to);
    }
}
static inline void gc_heap_snapshot_record_property_edge(void *snapshot, jl_value_t *from, jl_value_t *to, const char *property) JL_NOTSAFEPOINT
{
    if (__unlikely(snapshot)) {
        _gc_heap_snapshot_record_property_edge(snapshot, from, to, property);
    }
}

static inline void gc_heap_snapshot_record_module_to_binding(void *snapshot, jl_module_t* module, jl_value_t *bindings, jl_value_t *bindingkeyset) JL_NOTSAFEPOINT
{
    if (__unlikely(snapshot && bindings != NULL && bindingkeyset != NULL)) {
        _gc_heap_snapshot_record_module_to_binding(snapshot, module, bindings, bindingkeyset);
    }
}

static inline void gc_heap_snapshot_record_internal_array_edge(void *snapshot, jl_value_t *from, jl_value_t *to) JL_NOTSAFEPOINT
{
    if (__unlikely(snapshot)) {
        _gc_heap_snapshot_record_internal_array_edge(snapshot, from, to);
    }
}

static inline void gc_heap_snapshot_record_binding_partition_edge(void *snapshot, jl_value_t *from, jl_value_t *to) JL_NOTSAFEPOINT
{
    if (__unlikely(snapshot)) {
        _gc_heap_snapshot_record_binding_partition_edge(snapshot, from, to);
    }
}

static inline void gc_heap_snapshot_record_foreign_memory_edge(void *snapshot, jl_value_t *from, void* to, size_t bytes) JL_NOTSAFEPOINT
{
    if (__unlikely(snapshot)) {
        _gc_heap_snapshot_record_foreign_memory_edge(snapshot, from, to, bytes);
    }
}

static inline void gc_heap_snapshot_record_gc_roots(void *snapshot, jl_value_t *root, char *name) JL_NOTSAFEPOINT
{
    if (__unlikely(snapshot && root != NULL)) {
        _gc_heap_snapshot_record_gc_roots(snapshot, root, name);
    }
}

static inline void gc_heap_snapshot_record_finlist(void *snapshot, jl_value_t *finlist, size_t index) JL_NOTSAFEPOINT
{
    if (__unlikely(snapshot && finlist != NULL)) {
        _gc_heap_snapshot_record_finlist(snapshot, finlist, index);
    }
}

// ---------------------------------------------------------------------
// Functions to call from Julia to take heap snapshot
// ---------------------------------------------------------------------
JL_DLLEXPORT void jl_gc_take_heap_snapshot(ios_t *nodes, ios_t *edges,
    ios_t *strings, ios_t *json, char redact_data);


#ifdef __cplusplus
}
#endif


#endif  // JL_GC_HEAP_SNAPSHOT_H

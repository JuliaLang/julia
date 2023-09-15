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
void _gc_heap_snapshot_record_root(jl_value_t *root, char *name) JL_NOTSAFEPOINT;
void _gc_heap_snapshot_record_frame_to_object_edge(void *from, jl_value_t *to) JL_NOTSAFEPOINT;
void _gc_heap_snapshot_record_task_to_frame_edge(jl_task_t *from, void *to) JL_NOTSAFEPOINT;
void _gc_heap_snapshot_record_frame_to_frame_edge(jl_gcframe_t *from, jl_gcframe_t *to) JL_NOTSAFEPOINT;
void _gc_heap_snapshot_record_array_edge(jl_value_t *from, jl_value_t *to, size_t index) JL_NOTSAFEPOINT;
void _gc_heap_snapshot_record_object_edge(jl_value_t *from, jl_value_t *to, void* slot) JL_NOTSAFEPOINT;
void _gc_heap_snapshot_record_module_to_binding(jl_module_t* module, jl_binding_t* binding) JL_NOTSAFEPOINT;
// Used for objects managed by GC, but which aren't exposed in the julia object, so have no
// field or index.  i.e. they're not reachable from julia code, but we _will_ hit them in
// the GC mark phase (so we can check their type tag to get the size).
void _gc_heap_snapshot_record_internal_array_edge(jl_value_t *from, jl_value_t *to) JL_NOTSAFEPOINT;
// Used for objects manually allocated in C (outside julia GC), to still tell the heap snapshot about the
// size of the object, even though we're never going to mark that object.
void _gc_heap_snapshot_record_hidden_edge(jl_value_t *from, void* to, size_t bytes, uint16_t alloc_type) JL_NOTSAFEPOINT;


extern int gc_heap_snapshot_enabled;
extern int prev_sweep_full;

int gc_slot_to_fieldidx(void *_obj, void *slot, jl_datatype_t *vt) JL_NOTSAFEPOINT;
int gc_slot_to_arrayidx(void *_obj, void *begin) JL_NOTSAFEPOINT;

static inline void gc_heap_snapshot_record_frame_to_object_edge(void *from, jl_value_t *to) JL_NOTSAFEPOINT
{
    if (__unlikely(gc_heap_snapshot_enabled && prev_sweep_full)) {
        _gc_heap_snapshot_record_frame_to_object_edge(from, to);
    }
}
static inline void gc_heap_snapshot_record_task_to_frame_edge(jl_task_t *from, void *to) JL_NOTSAFEPOINT
{
    if (__unlikely(gc_heap_snapshot_enabled && prev_sweep_full)) {
        _gc_heap_snapshot_record_task_to_frame_edge(from, to);
    }
}
static inline void gc_heap_snapshot_record_frame_to_frame_edge(jl_gcframe_t *from, jl_gcframe_t *to) JL_NOTSAFEPOINT
{
    if (__unlikely(gc_heap_snapshot_enabled && prev_sweep_full)) {
        _gc_heap_snapshot_record_frame_to_frame_edge(from, to);
    }
}
static inline void gc_heap_snapshot_record_root(jl_value_t *root, char *name) JL_NOTSAFEPOINT
{
    if (__unlikely(gc_heap_snapshot_enabled && prev_sweep_full)) {
        _gc_heap_snapshot_record_root(root, name);
    }
}
static inline void gc_heap_snapshot_record_array_edge(jl_value_t *from, jl_value_t **to) JL_NOTSAFEPOINT
{
    if (__unlikely(gc_heap_snapshot_enabled && prev_sweep_full)) {
        _gc_heap_snapshot_record_array_edge(from, *to, gc_slot_to_arrayidx(from, to));
    }
}
static inline void gc_heap_snapshot_record_object_edge(jl_value_t *from, jl_value_t **to) JL_NOTSAFEPOINT
{
    if (__unlikely(gc_heap_snapshot_enabled && prev_sweep_full)) {
        _gc_heap_snapshot_record_object_edge(from, *to, to);
    }
}

static inline void gc_heap_snapshot_record_module_to_binding(jl_module_t* module, jl_binding_t* binding) JL_NOTSAFEPOINT
{
    if (__unlikely(gc_heap_snapshot_enabled && prev_sweep_full)) {
        _gc_heap_snapshot_record_module_to_binding(module, binding);
    }
}

static inline void gc_heap_snapshot_record_internal_array_edge(jl_value_t *from, jl_value_t *to) JL_NOTSAFEPOINT
{
    if (__unlikely(gc_heap_snapshot_enabled && prev_sweep_full)) {
        _gc_heap_snapshot_record_internal_array_edge(from, to);
    }
}

static inline void gc_heap_snapshot_record_hidden_edge(jl_value_t *from, void* to, size_t bytes, uint16_t alloc_type) JL_NOTSAFEPOINT
{
    if (__unlikely(gc_heap_snapshot_enabled && prev_sweep_full)) {
        _gc_heap_snapshot_record_hidden_edge(from, to, bytes, alloc_type);
    }
}

// ---------------------------------------------------------------------
// Functions to call from Julia to take heap snapshot
// ---------------------------------------------------------------------
JL_DLLEXPORT void jl_gc_take_heap_snapshot(ios_t *stream, char all_one);


#ifdef __cplusplus
}
#endif


#endif  // JL_GC_HEAP_SNAPSHOT_H

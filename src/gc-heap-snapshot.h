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
void _gc_heap_snapshot_record_frame_to_object_edge(jl_gcframe_t *from, jl_value_t *to) JL_NOTSAFEPOINT;
void _gc_heap_snapshot_record_task_to_frame_edge(jl_task_t *from, jl_gcframe_t *to) JL_NOTSAFEPOINT;
void _gc_heap_snapshot_record_frame_to_frame_edge(jl_gcframe_t *from, jl_gcframe_t *to) JL_NOTSAFEPOINT;
void _gc_heap_snapshot_record_array_edge(jl_value_t *from, jl_value_t *to, size_t index) JL_NOTSAFEPOINT;
void _gc_heap_snapshot_record_module_edge(jl_module_t *from, jl_value_t *to, char *name) JL_NOTSAFEPOINT;
void _gc_heap_snapshot_record_object_edge(jl_value_t *from, jl_value_t *to, void* slot) JL_NOTSAFEPOINT;
// Used for objects managed by GC, but which aren't exposed in the julia object, so have no
// field or index.  i.e. they're not reacahable from julia code, but we _will_ hit them in
// the GC mark phase (so we can check their type tag to get the size).
void _gc_heap_snapshot_record_internal_edge(jl_value_t *from, jl_value_t *to) JL_NOTSAFEPOINT;
// Used for objects manually allocated in C (outside julia GC), to still tell the heap snapshot about the
// size of the object, even though we're never going to mark that object.
void _gc_heap_snapshot_record_hidden_edge(jl_value_t *from, size_t bytes) JL_NOTSAFEPOINT;


extern int gc_heap_snapshot_enabled;

static inline void gc_heap_snapshot_record_frame_to_object_edge(jl_gcframe_t *from, jl_value_t *to) {
    if (__unlikely(gc_heap_snapshot_enabled)) {
        _gc_heap_snapshot_record_frame_to_object_edge(from, to);
    }
}
static inline void gc_heap_snapshot_record_task_to_frame_edge(jl_task_t *from, jl_gcframe_t *to) {
    if (__unlikely(gc_heap_snapshot_enabled)) {
        _gc_heap_snapshot_record_task_to_frame_edge(from, to);
    }
}
static inline void gc_heap_snapshot_record_frame_to_frame_edge(jl_gcframe_t *from, jl_gcframe_t *to) {
    if (__unlikely(gc_heap_snapshot_enabled)) {
        _gc_heap_snapshot_record_frame_to_frame_edge(from, to);
    }
}
static inline void gc_heap_snapshot_record_root(jl_value_t *root, char *name) JL_NOTSAFEPOINT {
    if (__unlikely(gc_heap_snapshot_enabled)) {
        _gc_heap_snapshot_record_root(root, name);
    }
}
static inline void gc_heap_snapshot_record_array_edge(jl_value_t *from, jl_value_t *to, size_t index) JL_NOTSAFEPOINT {
    if (__unlikely(gc_heap_snapshot_enabled)) {
        _gc_heap_snapshot_record_array_edge(from, to, index);
    }
}
static inline void gc_heap_snapshot_record_module_edge(jl_module_t *from, jl_value_t *to, char *name) JL_NOTSAFEPOINT {
    if (__unlikely(gc_heap_snapshot_enabled)) {
        _gc_heap_snapshot_record_module_edge(from, to, name);
    }
}
static inline void gc_heap_snapshot_record_object_edge(jl_value_t *from, jl_value_t *to, void* slot) JL_NOTSAFEPOINT {
    if (__unlikely(gc_heap_snapshot_enabled)) {
        _gc_heap_snapshot_record_object_edge(from, to, slot);
    }
}
static inline void gc_heap_snapshot_record_internal_edge(jl_value_t *from, jl_value_t *to) JL_NOTSAFEPOINT {
    if (__unlikely(gc_heap_snapshot_enabled)) {
        _gc_heap_snapshot_record_internal_edge(from, to);
    }
}
static inline void gc_heap_snapshot_record_hidden_edge(jl_value_t *from, size_t bytes) JL_NOTSAFEPOINT {
    if (__unlikely(gc_heap_snapshot_enabled)) {
        _gc_heap_snapshot_record_hidden_edge(from, bytes);
    }
}

// ---------------------------------------------------------------------
// Functions to call from Julia to take heap snapshot
// ---------------------------------------------------------------------
JL_DLLEXPORT void jl_gc_take_heap_snapshot(ios_t *stream);


#ifdef __cplusplus
}
#endif


#endif  // JL_GC_HEAP_SNAPSHOT_H

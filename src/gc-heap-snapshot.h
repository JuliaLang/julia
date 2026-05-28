// This file is a part of Julia. License is MIT: https://julialang.org/license

#ifndef JL_GC_HEAP_SNAPSHOT_H
#define JL_GC_HEAP_SNAPSHOT_H

#include "julia.h"
#include "ios.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
    JL_SNAPSHOT_GENERATION_YOUNG    = 0,
    JL_SNAPSHOT_GENERATION_OLD      = 1,
    JL_SNAPSHOT_GENERATION_IMMORTAL = 2,
} jl_gc_snapshot_generation_t;

// ---------------------------------------------------------------------
// Functions to call from GC when heap snapshot is enabled
// ---------------------------------------------------------------------
void _gc_heap_snapshot_record_root(jl_value_t *root, char *name) JL_NOTSAFEPOINT;
void _gc_heap_snapshot_record_frame_to_object_edge(void *from, jl_value_t *to) JL_NOTSAFEPOINT;
void _gc_heap_snapshot_record_task_to_frame_edge(jl_task_t *from, void *to) JL_NOTSAFEPOINT;
void _gc_heap_snapshot_record_frame_to_frame_edge(jl_gcframe_t *from, jl_gcframe_t *to) JL_NOTSAFEPOINT;
void _gc_heap_snapshot_record_array_edge(jl_value_t *from, jl_value_t *to, size_t index) JL_NOTSAFEPOINT;
void _gc_heap_snapshot_record_object_edge(jl_value_t *from, jl_value_t *to, void* slot) JL_NOTSAFEPOINT;
void _gc_heap_snapshot_record_module_to_binding(jl_module_t* module, jl_value_t *bindings, jl_value_t *bindingkeyset) JL_NOTSAFEPOINT;
// Used for objects managed by GC, but which aren't exposed in the julia object, so have no
// field or index.  i.e. they're not reachable from julia code, but we _will_ hit them in
// the GC mark phase (so we can check their type tag to get the size).
void _gc_heap_snapshot_record_internal_array_edge(jl_value_t *from, jl_value_t *to) JL_NOTSAFEPOINT;
// Used for objects manually allocated in C (outside julia GC), to still tell the heap snapshot about the
// size of the object, even though we're never going to mark that object.
void _gc_heap_snapshot_record_foreign_memory_edge(jl_value_t *from, void* to, size_t bytes) JL_NOTSAFEPOINT;
// Used for objects that are reachable from the GC roots
void _gc_heap_snapshot_record_gc_roots(jl_value_t *root, char *name) JL_NOTSAFEPOINT;
// Used for objects that are reachable from the finalizer list
void _gc_heap_snapshot_record_finlist(jl_value_t *finlist, size_t index) JL_NOTSAFEPOINT;
// Used for objects reachable from the binding partition pointer union
void _gc_heap_snapshot_record_binding_partition_edge(jl_value_t *from, jl_value_t *to) JL_NOTSAFEPOINT;
// Records an old-generation remset entry under the synthetic `[remset]` node
// (the old -> young generational frontier).
void _gc_heap_snapshot_record_remset_entry(jl_value_t *entry) JL_NOTSAFEPOINT;
// Records an image remset entry under the synthetic `[image]` node
// (the immortal -> old generational frontier).
void _gc_heap_snapshot_record_image_remset_entry(jl_value_t *entry) JL_NOTSAFEPOINT;
// Returns non-zero if `a` (an already-marked image object) has not yet been seen
// by the in-progress snapshot, so the mark loop should descend into it. Only
// consulted while recording an `:immortal` snapshot; never mutates GC state.
int _gc_heap_snapshot_try_claim_image(jl_value_t *a) JL_NOTSAFEPOINT;

// Set for the whole duration of a `jl_gc_take_heap_snapshot` call.
extern int gc_heap_snapshot_enabled;
// The requested `jl_gc_snapshot_generation_t` for the in-progress snapshot.
extern int gc_heap_snapshot_max_generation;
// Set by the GC for exactly the one mark pass whose depth matches the requested
// generation; all of the recording hooks below gate on this.
extern int gc_heap_snapshot_recording;
extern jl_mutex_t heapsnapshot_lock;

int gc_slot_to_fieldidx(void *_obj, void *slot, jl_datatype_t *vt) JL_NOTSAFEPOINT;
int gc_slot_to_arrayidx(void *_obj, void *begin) JL_NOTSAFEPOINT;

static inline void gc_heap_snapshot_record_frame_to_object_edge(void *from, jl_value_t *to) JL_NOTSAFEPOINT
{
    if (__unlikely(gc_heap_snapshot_recording)) {
        _gc_heap_snapshot_record_frame_to_object_edge(from, to);
    }
}
static inline void gc_heap_snapshot_record_task_to_frame_edge(jl_task_t *from, void *to) JL_NOTSAFEPOINT
{
    if (__unlikely(gc_heap_snapshot_recording)) {
        _gc_heap_snapshot_record_task_to_frame_edge(from, to);
    }
}
static inline void gc_heap_snapshot_record_frame_to_frame_edge(jl_gcframe_t *from, jl_gcframe_t *to) JL_NOTSAFEPOINT
{
    if (__unlikely(gc_heap_snapshot_recording)) {
        _gc_heap_snapshot_record_frame_to_frame_edge(from, to);
    }
}
static inline void gc_heap_snapshot_record_root(jl_value_t *root, char *name) JL_NOTSAFEPOINT
{
    if (__unlikely(gc_heap_snapshot_recording)) {
        _gc_heap_snapshot_record_root(root, name);
    }
}
static inline void gc_heap_snapshot_record_array_edge_index(jl_value_t *from, jl_value_t *to, size_t index) JL_NOTSAFEPOINT
{
    if (__unlikely(gc_heap_snapshot_recording && from != NULL && to != NULL)) {
        _gc_heap_snapshot_record_array_edge(from, to, index);
    }
}
static inline void gc_heap_snapshot_record_array_edge(jl_value_t *from, jl_value_t **to) JL_NOTSAFEPOINT
{
    if (__unlikely(gc_heap_snapshot_recording)) {
        _gc_heap_snapshot_record_array_edge(from, *to, gc_slot_to_arrayidx(from, to));
    }
}
static inline void gc_heap_snapshot_record_object_edge(jl_value_t *from, jl_value_t **to) JL_NOTSAFEPOINT
{
    if (__unlikely(gc_heap_snapshot_recording)) {
        _gc_heap_snapshot_record_object_edge(from, *to, to);
    }
}

static inline void gc_heap_snapshot_record_module_to_binding(jl_module_t* module, jl_value_t *bindings, jl_value_t *bindingkeyset) JL_NOTSAFEPOINT
{
    if (__unlikely(gc_heap_snapshot_recording) && bindings != NULL && bindingkeyset != NULL) {
        _gc_heap_snapshot_record_module_to_binding(module, bindings, bindingkeyset);
    }
}

static inline void gc_heap_snapshot_record_internal_array_edge(jl_value_t *from, jl_value_t *to) JL_NOTSAFEPOINT
{
    if (__unlikely(gc_heap_snapshot_recording)) {
        _gc_heap_snapshot_record_internal_array_edge(from, to);
    }
}

static inline void gc_heap_snapshot_record_binding_partition_edge(jl_value_t *from, jl_value_t *to) JL_NOTSAFEPOINT
{
    if (__unlikely(gc_heap_snapshot_recording)) {
        _gc_heap_snapshot_record_binding_partition_edge(from, to);
    }
}

static inline void gc_heap_snapshot_record_foreign_memory_edge(jl_value_t *from, void* to, size_t bytes) JL_NOTSAFEPOINT
{
    if (__unlikely(gc_heap_snapshot_recording)) {
        _gc_heap_snapshot_record_foreign_memory_edge(from, to, bytes);
    }
}

static inline void gc_heap_snapshot_record_gc_roots(jl_value_t *root, char *name) JL_NOTSAFEPOINT
{
    if (__unlikely(gc_heap_snapshot_recording && root != NULL)) {
        _gc_heap_snapshot_record_gc_roots(root, name);
    }
}

static inline void gc_heap_snapshot_record_finlist(jl_value_t *finlist, size_t index) JL_NOTSAFEPOINT
{
    if (__unlikely(gc_heap_snapshot_recording && finlist != NULL)) {
        _gc_heap_snapshot_record_finlist(finlist, index);
    }
}

// ---------------------------------------------------------------------
// Functions to call from Julia to take heap snapshot
// ---------------------------------------------------------------------
JL_DLLEXPORT void jl_gc_take_heap_snapshot(ios_t *nodes, ios_t *edges,
    ios_t *strings, ios_t *json, char all_one, char redact_data, char max_generation);


#ifdef __cplusplus
}
#endif


#endif  // JL_GC_HEAP_SNAPSHOT_H

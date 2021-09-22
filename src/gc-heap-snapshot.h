#ifndef JL_GC_HEAP_SNAPSHOT_H
#define JL_GC_HEAP_SNAPSHOT_H

#include "julia.h"

#ifdef __cplusplus
extern "C" {
#endif


// ---------------------------------------------------------------------
// Functions to call from GC when heap snapshot is enabled
// ---------------------------------------------------------------------
// TODO: remove JL_DLLEXPORT
JL_DLLEXPORT void _gc_heap_snapshot_record_array_edge(jl_value_t *from, jl_value_t *to, size_t index) JL_NOTSAFEPOINT;
JL_DLLEXPORT void _gc_heap_snapshot_record_module_edge(jl_module_t *from, jl_value_t *to, char *name) JL_NOTSAFEPOINT;
JL_DLLEXPORT void _gc_heap_snapshot_record_object_edge(jl_value_t *from, jl_value_t *to, size_t field_index) JL_NOTSAFEPOINT;
// Used for objects managed by GC, but which aren't exposed in the julia object, so have no
// field or index.  i.e. they're not reacahable from julia code, but we _will_ hit them in
// the GC mark phase (so we can check their type tag to get the size).
JL_DLLEXPORT void _gc_heap_snapshot_record_internal_edge(jl_value_t *from, jl_value_t *to) JL_NOTSAFEPOINT;
// Used for objects manually allocated in C (outside julia GC), to still tell the heap snapshot about the
// size of the object, even though we're never going to mark that object.
JL_DLLEXPORT void _gc_heap_snapshot_record_hidden_edge(jl_value_t *from, size_t bytes) JL_NOTSAFEPOINT;


extern int gc_heap_snapshot_enabled;
#define RETURN_IF_HEAP_SNAPSHOT_NOT_ENABLED() if (!gc_heap_snapshot_enabled) {return;}


static inline void gc_heap_snapshot_record_array_edge(jl_value_t *from, jl_value_t *to, size_t index) {
    RETURN_IF_HEAP_SNAPSHOT_NOT_ENABLED();
    _gc_heap_snapshot_record_array_edge(from, to, index);
}
static inline void gc_heap_snapshot_record_module_edge(jl_module_t *from, jl_value_t *to, char *name) {
    RETURN_IF_HEAP_SNAPSHOT_NOT_ENABLED();
    _gc_heap_snapshot_record_module_edge(from, to, name);
}
static inline void gc_heap_snapshot_record_object_edge(jl_value_t *from, jl_value_t *to, size_t field_index) {
    RETURN_IF_HEAP_SNAPSHOT_NOT_ENABLED();
    _gc_heap_snapshot_record_object_edge(from, to, field_index);
}
static inline void gc_heap_snapshot_record_internal_edge(jl_value_t *from, jl_value_t *to) {
    RETURN_IF_HEAP_SNAPSHOT_NOT_ENABLED();
    _gc_heap_snapshot_record_internal_edge(from, to);
}
static inline void gc_heap_snapshot_record_hidden_edge(jl_value_t *from, size_t bytes) {
    RETURN_IF_HEAP_SNAPSHOT_NOT_ENABLED();
    _gc_heap_snapshot_record_hidden_edge(from, bytes);
}

// ---------------------------------------------------------------------
// Functions to call from Julia to take heap snapshot
// ---------------------------------------------------------------------
JL_DLLEXPORT void jl_gc_take_heap_snapshot(JL_STREAM *stream);


#ifdef __cplusplus
}
#endif


#endif  // JL_GC_HEAP_SNAPSHOT_H

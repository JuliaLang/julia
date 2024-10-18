// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  Garbage Collection interface that must be implemented by third-party GCs
*/

#ifndef JL_GC_INTERFACE_H
#define JL_GC_INTERFACE_H

#include "dtypes.h"

#ifdef __cplusplus
extern "C" {
#endif

struct _jl_tls_states_t;
struct _jl_value_t;
struct _jl_weakref_t;
struct _jl_datatype_t;

// ========================================================================= //
// GC Metrics
// ========================================================================= //

// This struct must be kept in sync with the Julia type of the same name in base/timing.jl
typedef struct {
    int64_t allocd;
    int64_t deferred_alloc;
    int64_t freed;
    uint64_t malloc;
    uint64_t realloc;
    uint64_t poolalloc;
    uint64_t bigalloc;
    uint64_t freecall;
    uint64_t total_time;
    uint64_t total_allocd;
    size_t interval;
    int pause;
    int full_sweep;
    uint64_t max_pause;
    uint64_t max_memory;
    uint64_t time_to_safepoint;
    uint64_t max_time_to_safepoint;
    uint64_t total_time_to_safepoint;
    uint64_t sweep_time;
    uint64_t mark_time;
    uint64_t stack_pool_sweep_time;
    uint64_t total_sweep_time;
    uint64_t total_mark_time;
    uint64_t total_stack_pool_sweep_time;
    uint64_t last_full_sweep;
    uint64_t last_incremental_sweep;
} jl_gc_num_t;

// ========================================================================= //
// System-wide Initialization
// ========================================================================= //

// System-wide initialization function. Responsible for initializing global locks as well as
// global memory parameters (e.g. target heap size) used by the collector.
void jl_gc_init(void);
// Spawns GC threads.
void jl_start_gc_threads(void);

// ========================================================================= //
// Per-thread Initialization
// ========================================================================= //

// Initializes thread-local data structures such as thread-local object pools,
// thread-local remembered sets and thread-local allocation counters.
// Should be called exactly once per Julia thread.
void jl_init_thread_heap(struct _jl_tls_states_t *ptls) JL_NOTSAFEPOINT;
// Deallocates any memory previously used for thread-local GC data structures.
// Mostly used to ensure that we perform this memory cleanup for foreign threads that are
// about to leave Julia.
void jl_free_thread_gc_state(struct _jl_tls_states_t *ptls);

// ========================================================================= //
// Controls
// ========================================================================= //

typedef enum {
    JL_GC_AUTO = 0, // use heuristics to determine the collection type
    JL_GC_FULL = 1, // force a full collection
    JL_GC_INCREMENTAL = 2, // force an incremental collection
} jl_gc_collection_t;
// Enables or disables (depending on the value of the argument) the collector. Returns
// whether GC was previously enabled.
JL_DLLEXPORT int jl_gc_enable(int on);
// Returns whether the collector is enabled.
JL_DLLEXPORT int jl_gc_is_enabled(void);
// Sets a soft limit to Julia's heap.
JL_DLLEXPORT void jl_gc_set_max_memory(uint64_t max_mem);
// Runs a GC cycle. This function's parameter determines whether we're running an
// incremental, full, or automatic (i.e. heuristic driven) collection. Returns whether we
// should run a collection cycle again (e.g. a full mark right after a full sweep to ensure
// we do a full heap traversal).
JL_DLLEXPORT void jl_gc_collect(jl_gc_collection_t collection);

// ========================================================================= //
// Metrics
// ========================================================================= //

// Retrieves Julia's `GC_Num` (structure that stores GC statistics).
JL_DLLEXPORT jl_gc_num_t jl_gc_num(void);
// Returns the difference between the current value of total live bytes now
// (live bytes at the last collection plus number of bytes allocated since then),
// compared to the value at the last time this function was called.
JL_DLLEXPORT int64_t jl_gc_diff_total_bytes(void) JL_NOTSAFEPOINT;
// Returns the difference between the current value of total live bytes now
// (live bytes at the last collection plus number of bytes allocated since then)
// compared to the value at the last time this function was called. The offset parameter
// is subtracted from this value in order to obtain the return value.
JL_DLLEXPORT int64_t jl_gc_sync_total_bytes(int64_t offset) JL_NOTSAFEPOINT;
// Returns the number of pool allocated bytes. This could always return 0 for GC
// implementations that do not use pools.
JL_DLLEXPORT int64_t jl_gc_pool_live_bytes(void);
// Returns the number of live bytes at the end of the last collection cycle
// (doesn't include the number of allocated bytes since then).
JL_DLLEXPORT int64_t jl_gc_live_bytes(void);
// Stores the number of live bytes at the end of the last collection cycle plus the number
// of bytes we allocated since then into the 64-bit integer pointer passed as an argument.
JL_DLLEXPORT void jl_gc_get_total_bytes(int64_t *bytes) JL_NOTSAFEPOINT;
// Retrieves the value of Julia's soft heap limit.
JL_DLLEXPORT uint64_t jl_gc_get_max_memory(void);
// High-resolution (nano-seconds) value of total time spent in GC.
JL_DLLEXPORT uint64_t jl_gc_total_hrtime(void);

// ========================================================================= //
// Allocation
// ========================================================================= //

// Allocates small objects and increments Julia allocation counterst. Size of the object
// header must be included in the object size. The (possibly unused in some implementations)
// offset to the arena in which we're allocating is passed in the second parameter, and the
// object size in the third parameter. If thread-local allocators are used, then this
// function should allocate in the thread-local allocator of the thread referenced by the
// jl_ptls_t argument. An additional (last) parameter containing information about the type
// of the object being allocated may be used to record an allocation of that type in the
// allocation profiler.
JL_DLLEXPORT struct _jl_value_t *jl_gc_small_alloc(struct _jl_tls_states_t *ptls,
                                                   int offset, int osize,
                                                   struct _jl_value_t *type);
// Description: Allocates large objects and increments Julia allocation counters. Size of
// the object header must be included in the object size. If thread-local allocators are
// used, then this function should allocate in the thread-local allocator of the thread
// referenced by the jl_ptls_t argument. An additional (last) parameter containing
// information about the type of the object being allocated may be used to record an
// allocation of that type in the allocation profiler.
JL_DLLEXPORT struct _jl_value_t *jl_gc_big_alloc(struct _jl_tls_states_t *ptls, size_t sz,
                                                 struct _jl_value_t *type);
// Wrapper around Libc malloc that updates Julia allocation counters.
JL_DLLEXPORT void *jl_gc_counted_malloc(size_t sz);
// Wrapper around Libc calloc that updates Julia allocation counters.
JL_DLLEXPORT void *jl_gc_counted_calloc(size_t nm, size_t sz);
// Wrapper around Libc free that updates Julia allocation counters.
JL_DLLEXPORT void jl_gc_counted_free_with_size(void *p, size_t sz);
// Wrapper around Libc realloc that updates Julia allocation counters.
JL_DLLEXPORT void *jl_gc_counted_realloc_with_old_size(void *p, size_t old, size_t sz);
// Wrapper around Libc malloc that allocates a memory region with a few additional machine
// words before the actual payload that are used to record the size of the requested
// allocation. Also updates Julia allocation counters. The function returns a pointer to the
// payload as a result of the allocation.
JL_DLLEXPORT void *jl_malloc(size_t sz);
// Wrapper around Libc calloc that allocates a memory region with a few additional machine
// words before the actual payload that are used to record the size of the requested
// allocation. Also updates Julia allocation counters. The function returns a pointer to the
// payload as a result of the allocation.
JL_DLLEXPORT void *jl_calloc(size_t nm, size_t sz);
// Wrapper around Libc free that takes a pointer to the payload of a memory region allocated
// with jl_malloc or jl_calloc, and uses the size information stored in the first machine
// words of the memory buffer update Julia allocation counters, and then frees the
// corresponding memory buffer.
JL_DLLEXPORT void jl_free(void *p);
// Wrapper around Libc realloc that takes a memory region allocated with jl_malloc or
// jl_calloc, and uses the size information stored in the first machine words of the memory
// buffer to update Julia allocation counters, reallocating the corresponding memory buffer
// in the end.
JL_DLLEXPORT void *jl_realloc(void *p, size_t sz);
// Wrapper around Libc malloc that's used to dynamically allocate memory for Arrays and
// Strings. It increments Julia allocation counters and should check whether we're close to
// the Julia heap target, and therefore, whether we should run a collection. Note that this
// doesn't record the size of the allocation request in a side metadata (i.e. a few words in
// front of the memory payload): this function is used for Julia object allocations, and we
// assume that there is already a field in the Julia object being allocated that we may use
// to store the size of the memory buffer.
JL_DLLEXPORT void *jl_gc_managed_malloc(size_t sz);
// Allocates a new weak-reference, assigns its value and increments Julia allocation
// counters. If thread-local allocators are used, then this function should allocate in the
// thread-local allocator of the thread referenced by the first jl_ptls_t argument.
JL_DLLEXPORT struct _jl_weakref_t *jl_gc_new_weakref_th(struct _jl_tls_states_t *ptls,
                                                        struct _jl_value_t *value);
// Allocates a new weak-reference, assigns its value and increments Julia allocation
// counters. If thread-local allocators are used, then this function should allocate in the
// thread-local allocator of the current thread.
JL_DLLEXPORT struct _jl_weakref_t *jl_gc_new_weakref(struct _jl_value_t *value);
// Allocates an object whose size is specified by the function argument and increments Julia
// allocation counters. If thread-local allocators are used, then this function should
// allocate in the thread-local allocator of the current thread.
JL_DLLEXPORT struct _jl_value_t *jl_gc_allocobj(size_t sz);
// Permanently allocates a memory slot of the size specified by the first parameter. This
// block of memory is allocated in an immortal region that is never swept. The second
// parameter specifies whether the memory should be filled with zeros. The third and fourth
// parameters specify the alignment and an offset in bytes, respectively. Specifically, the
// pointer obtained by advancing the result of this function by the number of bytes
// specified in the fourth parameter will be aligned according to the value given by the
// third parameter in bytes.
JL_DLLEXPORT void *jl_gc_perm_alloc(size_t sz, int zero, unsigned align,
                                    unsigned offset) JL_NOTSAFEPOINT;
// Permanently allocates an object of the size specified by the first parameter. Size of the
// object header must be included in the object size. This object is allocated in an
// immortal region that is never swept. The second parameter specifies the type of the
// object being allocated and will be used to set the object header.
struct _jl_value_t *jl_gc_permobj(size_t sz, void *ty) JL_NOTSAFEPOINT;

// ========================================================================= //
// Runtime Write-Barriers
// ========================================================================= //

// Write barrier slow-path. If a generational collector is used,
// it may enqueue an old object into the remembered set of the calling thread.
JL_DLLEXPORT void jl_gc_queue_root(const struct _jl_value_t *ptr) JL_NOTSAFEPOINT;
// In a generational collector is used, this function walks over the fields of the
// object specified by the second parameter (as defined by the data type in the third
// parameter). If a field points to a young object, the first parameter is enqueued into the
// remembered set of the calling thread.
JL_DLLEXPORT void jl_gc_queue_multiroot(const struct _jl_value_t *root, const void *stored,
                                        struct _jl_datatype_t *dt) JL_NOTSAFEPOINT;
// If a generational collector is used, checks whether the function argument points to an
// old object, and if so, calls the write barrier slow path above. In most cases, this
// function is used when its caller has verified that there is a young reference in the
// object that's being passed as an argument to this function.
STATIC_INLINE void jl_gc_wb_back(const void *ptr) JL_NOTSAFEPOINT;
// Write barrier function that must be used after pointer writes to heap-allocated objects –
// the value of the field being written must also point to a heap-allocated object.
// If a generational collector is used, it may check whether the two function arguments are
// in different GC generations (i.e. if the first argument points to an old object and the
// second argument points to a young object), and if so, call the write barrier slow-path.
STATIC_INLINE void jl_gc_wb(const void *parent, const void *ptr) JL_NOTSAFEPOINT;
// Freshly allocated objects are known to be in the young generation until the next safepoint,
// so write barriers can be omitted until the next allocation. This function is a no-op that
// can be used to annotate that a write barrier would be required were it not for this property
// (as opposed to somebody just having forgotten to think about write barriers).
STATIC_INLINE void jl_gc_wb_fresh(const void *parent, const void *ptr) JL_NOTSAFEPOINT {}
// Used to annotate that a write barrier would be required, but may be omitted because `ptr`
// is known to be an old object.
STATIC_INLINE void jl_gc_wb_knownold(const void *parent, const void *ptr) JL_NOTSAFEPOINT {}
// Write-barrier function that must be used after copying multiple fields of an object into
// another. It should be semantically equivalent to triggering multiple write barriers – one
// per field of the object being copied, but may be special-cased for performance reasons.
STATIC_INLINE void jl_gc_multi_wb(const void *parent,
                                  const struct _jl_value_t *ptr) JL_NOTSAFEPOINT;

#ifdef __cplusplus
}
#endif

#endif

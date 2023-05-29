#ifndef MMTK_H
#define MMTK_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include "gc.h"

#ifdef __cplusplus
extern "C" {
#endif


typedef struct {
    void* a;
    void* b;
} closure_pointer;
typedef void* MMTk_Mutator;
typedef void* MMTk_TraceLocal;
typedef void (*ProcessEdgeFn)(closure_pointer closure, void* slot);
typedef void (*ProcessOffsetEdgeFn)(closure_pointer closure, void* slot, int offset);

/**
 * Allocation
 */
extern MMTk_Mutator mmtk_bind_mutator(void *tls, int tid);
extern void mmtk_add_mutator_ref(void* mutator_ref);
extern void mmtk_destroy_mutator(MMTk_Mutator mutator);

extern void* mmtk_alloc(MMTk_Mutator mutator, size_t size,
    size_t align, size_t offset, int allocator);

extern void* mmtk_alloc_large(MMTk_Mutator mutator, size_t size,
    size_t align, size_t offset, int allocator);    

extern void mmtk_post_alloc(MMTk_Mutator mutator, void* refer,
    size_t bytes, int allocator);

extern void mmtk_add_object_to_mmtk_roots(void *obj);
extern void mmtk_process_root_edges(closure_pointer c, void* slot);

extern void* mmtk_counted_malloc(size_t size);
extern void* mmtk_malloc(size_t size);
extern void* mmtk_counted_calloc(size_t n, size_t size);
extern void* mmtk_calloc(size_t n, size_t size);
extern void* mmtk_realloc(void* addr, size_t size);
extern void* mmtk_realloc_with_old_size(void* addr, size_t size, size_t old_size);
extern void mmtk_free_with_size(void* addr, size_t old_size);
extern void mmtk_free(void* addr);
extern void* mmtk_malloc_aligned(size_t size, size_t alignment);
extern void mmtk_free_aligned(void* addr);

extern bool mmtk_is_live_object(void* ref);
extern bool mmtk_is_mapped_object(void* ref);
extern bool mmtk_is_mapped_address(void* addr);
extern void mmtk_modify_check(void* ref);
extern int mmtk_object_is_managed_by_mmtk(void* addr);
extern void mmtk_runtime_panic(void);
extern void mmtk_unreachable(void);

extern void mmtk_set_vm_space(void* addr, size_t size);
extern void mmtk_immortal_region_post_alloc(void* addr, size_t size);

// Write barriers
extern void mmtk_memory_region_copy(MMTk_Mutator mutator, void* src_obj, void* src_addr, void* dst_obj, void* dst_addr, size_t size);
extern void mmtk_object_reference_write_post(MMTk_Mutator mutator, const void* src, const void* target);
extern void mmtk_object_reference_write_slow(MMTk_Mutator mutator, const void* src, const void* target);
extern const uint8_t MMTK_NEEDS_WRITE_BARRIER;
extern const uint8_t MMTK_NO_BARRIER;
extern const uint8_t MMTK_OBJECT_BARRIER;
extern const void* MMTK_SIDE_LOG_BIT_BASE_ADDRESS;

/**
 * Julia-specific
 */

// When we call upcalls from Rust, we assume:
// * int is 4 bytes
// * size_t is 8 bytes
typedef struct {
    void (* scan_julia_obj) (jl_value_t* obj, closure_pointer closure, ProcessEdgeFn process_edge, ProcessOffsetEdgeFn process_offset_edge);
    void (* scan_julia_exc_obj) (jl_task_t* obj, closure_pointer closure, ProcessEdgeFn process_edge);
    void* (* get_stackbase) (int16_t tid);
    void (* calculate_roots) (jl_ptls_t tls);
    void (* run_finalizer_function) (jl_value_t* obj, jl_value_t* function, bool is_ptr);
    int (* get_jl_last_err) (void);
    void (* set_jl_last_err) (int e);
    // FIXME: I don't think this is correct, as we pass an object reference to get_lo_size, in the same way as get_so_size.
    size_t (* get_lo_size) (bigval_t obj);
    size_t (* get_so_size) (jl_value_t* obj);
    void* (* get_obj_start_ref) (jl_value_t* obj);
    void (* wait_for_the_world) (void);
    int8_t (* set_gc_initial_state) (jl_ptls_t tls);
    void (* set_gc_final_state) (int8_t old_state);
    void (* set_gc_old_state) (int8_t old_state);
    void (* mmtk_jl_run_finalizers) (jl_ptls_t tls);
    void (* jl_throw_out_of_memory_error) (void);
    void (* mark_object_as_scanned) (jl_value_t* obj);
    int8_t (* object_has_been_scanned) (jl_value_t* obj);
    void (* sweep_malloced_array) (void);
    void (* wait_in_a_safepoint) (void);
    void (* exit_from_safepoint) (int8_t old_state);
} Julia_Upcalls;

/**
 * Misc
 */
extern void mmtk_gc_init(uintptr_t min_heap_size, uintptr_t max_heap_size, uintptr_t n_gcthreads, Julia_Upcalls *calls, uintptr_t header_size);
extern bool mmtk_will_never_move(void* object);
extern bool mmtk_process(char* name, char* value);
extern void mmtk_scan_region(void);
extern void mmtk_handle_user_collection_request(void *tls, uint8_t collection);
extern void mmtk_initialize_collection(void* tls);
extern void mmtk_enable_collection(void);
extern void mmtk_disable_collection(void);
extern void mmtk_start_control_collector(void *tls);
extern void mmtk_start_worker(void *tls, void* worker, void* mmtk);
extern void mmtk_process_julia_obj(void* addr);
extern void mmtk_register_finalizer(void* obj, void* function, bool is_ptr);
extern void mmtk_run_finalizers_for_obj(void* obj);
extern void mmtk_run_finalizers(bool at_exit);
extern void mmtk_gc_poll(void *tls);

/**
 * VM Accounting
 */
extern size_t mmtk_free_bytes(void);
extern size_t mmtk_total_bytes(void);
extern size_t mmtk_used_bytes(void);
extern void* mmtk_starting_heap_address(void);
extern void* mmtk_last_heap_address(void);

/**
 * Reference Processing
 */
extern void mmtk_add_weak_candidate(void* ref);
extern void mmtk_add_soft_candidate(void* ref);
extern void mmtk_add_phantom_candidate(void* ref);

extern void mmtk_harness_begin(void *tls);
extern void mmtk_harness_end(void);

#ifdef __cplusplus
}
#endif

#endif // MMTK_H


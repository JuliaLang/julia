#ifndef MMTK_H
#define MMTK_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef void* MMTk_Mutator;
typedef void* MMTk_TraceLocal;
typedef void (*ProcessSlotFn)(void* closure, void* slot);
typedef void (*ProcessOffsetSlotFn)(void* closure, void* slot, int offset);

typedef struct {
    void** ptr;
    size_t cap;
} RootsWorkBuffer;

typedef struct {
    RootsWorkBuffer (*report_slots_func)(void** buf, size_t size, size_t cap, void* data, bool renew);
    RootsWorkBuffer (*report_nodes_func)(void** buf, size_t size, size_t cap, void* data, bool renew);
    RootsWorkBuffer (*report_tpinned_nodes_func)(void** buf, size_t size, size_t cap, void* data, bool renew);
    void* data;
} RootsWorkClosure;

/**
 * Allocation
 */
extern MMTk_Mutator mmtk_bind_mutator(void *tls, int tid);
extern void mmtk_post_bind_mutator(MMTk_Mutator mutator, MMTk_Mutator original_mutator);
extern void mmtk_destroy_mutator(MMTk_Mutator mutator);

extern void* mmtk_alloc(MMTk_Mutator mutator, size_t size,
    size_t align, size_t offset, int allocator);

extern void* mmtk_alloc_large(MMTk_Mutator mutator, size_t size,
    size_t align, size_t offset, int allocator);    

extern void mmtk_post_alloc(MMTk_Mutator mutator, void* refer,
    size_t bytes, int allocator);

extern bool mmtk_is_live_object(void* ref);
extern bool mmtk_is_mapped_object(void* ref);
extern bool mmtk_is_mapped_address(void* addr);
extern int mmtk_object_is_managed_by_mmtk(void* addr);
extern void mmtk_runtime_panic(void);
extern void mmtk_unreachable(void);
extern unsigned char mmtk_pin_object(void* obj);
extern bool mmtk_is_pinned(void* obj);

extern void mmtk_set_vm_space(void* addr, size_t size);
extern void mmtk_immortal_region_post_alloc(void* addr, size_t size);

// Write barriers
extern void mmtk_memory_region_copy(MMTk_Mutator mutator, void* src_obj, void* src_addr, void* dst_obj, void* dst_addr, size_t size);
extern void mmtk_object_reference_write_post(MMTk_Mutator mutator, const void* src, const void* target);
extern void mmtk_object_reference_write_slow(MMTk_Mutator mutator, const void* src, const void* target);
extern const void* MMTK_SIDE_LOG_BIT_BASE_ADDRESS;

extern uintptr_t JULIA_MALLOC_BYTES;

/**
 * Julia-specific
 */

// When we call upcalls from Rust, we assume:
// * int is 4 bytes
// * size_t is 8 bytes
typedef struct {
    void (* scan_julia_exc_obj) (void* obj, void* closure, ProcessSlotFn process_slot);
    void* (* get_stackbase) (int16_t tid);
    void (* jl_throw_out_of_memory_error) (void);
    void (* sweep_malloced_memory) (void);
    void (* sweep_stack_pools) (void);
    void (* wait_in_a_safepoint) (void);
    void (* exit_from_safepoint) (int8_t old_state);
    uint64_t (* jl_hrtime) (void);
    void (* update_gc_stats) (uint64_t, size_t, bool);
    uintptr_t (* get_abi_structs_checksum_c) (void);
    void* (* get_thread_finalizer_list) (void* tls);
    void* (* get_to_finalize_list)(void);
    void* (* get_marked_finalizers_list)(void);
    void (*arraylist_grow)(void* a, size_t n);
    int* (*get_jl_gc_have_pending_finalizers)(void);
    void (*scan_vm_specific_roots)(RootsWorkClosure* closure);
    void (*update_inlined_array) (void* from, void* to);
    void (*prepare_to_collect)(void);
    void* (* get_owner_address)(void* m);
    size_t (* mmtk_genericmemory_how)(void* m);
} Julia_Upcalls;

/**
 * Misc
 */
extern void mmtk_gc_init(uintptr_t min_heap_size, uintptr_t max_heap_size, uintptr_t n_gcthreads, Julia_Upcalls *calls, uintptr_t header_size, uintptr_t tag);
extern bool mmtk_will_never_move(void* object);
extern bool mmtk_process(char* name, char* value);
extern void mmtk_scan_region(void);
extern void mmtk_handle_user_collection_request(void *tls, uint8_t collection);
extern void mmtk_initialize_collection(void* tls);
extern void mmtk_start_control_collector(void *tls);
extern void mmtk_start_worker(void *tls, void* worker, void* mmtk);
extern void mmtk_process_julia_obj(void* addr);
extern void mmtk_register_finalizer(void* obj, void* function, bool is_ptr);
extern void mmtk_run_finalizers_for_obj(void* obj);
extern void mmtk_run_finalizers(bool at_exit);
extern void mmtk_gc_poll(void *tls);
extern void mmtk_julia_copy_stack_check(int copy_stack);
extern void* mmtk_get_possibly_forwared(void* object);

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


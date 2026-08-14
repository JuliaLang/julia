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
extern const char* get_mmtk_version(void);

extern void mmtk_set_vm_space(void* addr, size_t size);
extern void mmtk_immortal_region_post_alloc(void* addr, size_t size);

// Write barriers
extern void mmtk_memory_region_copy(MMTk_Mutator mutator, void* src_obj, void* src_addr, void* dst_obj, void* dst_addr, size_t size);
extern void mmtk_object_reference_write_pre(MMTk_Mutator mutator, const void* src, const void* target);
extern void mmtk_object_reference_write_post(MMTk_Mutator mutator, const void* src, const void* target);
extern void mmtk_object_reference_write_slow(MMTk_Mutator mutator, const void* src, const void* target);
extern _Atomic(uintptr_t) JULIA_MALLOC_BYTES;

/**
 * Misc
 */
extern void mmtk_gc_init(uintptr_t min_heap_size, uintptr_t max_heap_size, uintptr_t n_gcthreads, uintptr_t n_concurrent_gcthreads, uintptr_t header_size, uintptr_t tag);
extern bool mmtk_will_never_move(void* object);
extern bool mmtk_is_moving(void);
extern const char* mmtk_get_plan_name(void);
extern bool mmtk_process(char* name, char* value);
extern void mmtk_scan_region(void);
extern void mmtk_handle_user_collection_request(void *tls, uint8_t collection);
// mmtk_disable_collection() return values:
//   0 (MMTK_DISABLE_COLLECTION_OK): succeeded, collection is now disabled.
//   1 (MMTK_DISABLE_COLLECTION_RETRY): failed; a GC pause has been requested but mutators have
//     not all stopped yet. This thread may itself be one of the ones the pause is waiting to
//     stop, so do a safepoint check (which is exactly how a mutator cooperates with letting the
//     pause start) and retry.
//   2 (MMTK_DISABLE_COLLECTION_WAIT_FOR_NEW_GC_EPOCH): failed; a stop-the-world pause is active,
//     or a concurrent GC's background work is currently running. Either way there is no useful
//     safepoint action to take. Instead, call mmtk_gc_epoch() *before* retrying disable, and if
//     it fails again with this value, call mmtk_wait_for_new_gc_epoch() with that epoch value
//     before retrying again.
// Any other failure (there should not be any) is a hard bug and mmtk_disable_collection() panics.
#define MMTK_DISABLE_COLLECTION_OK 0
#define MMTK_DISABLE_COLLECTION_RETRY 1
#define MMTK_DISABLE_COLLECTION_WAIT_FOR_NEW_GC_EPOCH 2
extern int mmtk_disable_collection(void);
extern int mmtk_enable_collection(void);
extern int mmtk_is_collection_enabled(void);
extern uint64_t mmtk_gc_epoch(void);
extern void mmtk_wait_for_new_gc_epoch(uint64_t last_seen_epoch);
extern void mmtk_initialize_collection(void* tls);
extern void mmtk_start_control_collector(void *tls);
extern void mmtk_start_worker(void *tls, void* worker, void* mmtk);
extern void mmtk_process_julia_obj(void* addr);
extern void mmtk_register_finalizer(void* obj, void* function, bool is_ptr);
extern void mmtk_run_finalizers_for_obj(void* obj);
extern void mmtk_run_finalizers(bool at_exit);
extern void mmtk_gc_poll(void *tls);
extern void mmtk_julia_copy_stack_check(int copy_stack);
extern void* mmtk_get_possibly_forwarded(void* object);
extern void mmtk_block_thread_for_gc(void);
extern void mmtk_set_concurrent_marking_enabled(bool enabled);
extern void* mmtk_new_mutator_iterator(void);
extern void* mmtk_get_next_mutator_tls(void*);
extern void* mmtk_close_mutator_iterator(void*);
extern void mmtk_notify_task_resume(void *mutator, const void *task);

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

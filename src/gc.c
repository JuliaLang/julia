// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "gc.h"
#include "julia_gcext.h"
#include "julia_assert.h"
#ifdef __GLIBC__
#include <malloc.h> // for malloc_trim
#endif

#ifdef __cplusplus
extern "C" {
#endif

// Linked list of callback functions

typedef void (*jl_gc_cb_func_t)(void);

typedef struct jl_gc_callback_list_t {
    struct jl_gc_callback_list_t *next;
    jl_gc_cb_func_t func;
} jl_gc_callback_list_t;

static jl_gc_callback_list_t *gc_cblist_root_scanner;
static jl_gc_callback_list_t *gc_cblist_task_scanner;
static jl_gc_callback_list_t *gc_cblist_pre_gc;
static jl_gc_callback_list_t *gc_cblist_post_gc;
static jl_gc_callback_list_t *gc_cblist_notify_external_alloc;
static jl_gc_callback_list_t *gc_cblist_notify_external_free;

#define gc_invoke_callbacks(ty, list, args) \
    do { \
        for (jl_gc_callback_list_t *cb = list; \
                cb != NULL; \
                cb = cb->next) \
        { \
            ((ty)(cb->func)) args; \
        } \
    } while (0)

static void jl_gc_register_callback(jl_gc_callback_list_t **list,
        jl_gc_cb_func_t func)
{
    while (*list != NULL) {
        if ((*list)->func == func)
            return;
        list = &((*list)->next);
    }
    *list = (jl_gc_callback_list_t *)malloc_s(sizeof(jl_gc_callback_list_t));
    (*list)->next = NULL;
    (*list)->func = func;
}

static void jl_gc_deregister_callback(jl_gc_callback_list_t **list,
        jl_gc_cb_func_t func)
{
    while (*list != NULL) {
        if ((*list)->func == func) {
            jl_gc_callback_list_t *tmp = *list;
            (*list) = (*list)->next;
            free(tmp);
            return;
        }
        list = &((*list)->next);
    }
}

JL_DLLEXPORT void jl_gc_set_cb_root_scanner(jl_gc_cb_root_scanner_t cb, int enable)
{
    if (enable)
        jl_gc_register_callback(&gc_cblist_root_scanner, (jl_gc_cb_func_t)cb);
    else
        jl_gc_deregister_callback(&gc_cblist_root_scanner, (jl_gc_cb_func_t)cb);
}

JL_DLLEXPORT void jl_gc_set_cb_task_scanner(jl_gc_cb_task_scanner_t cb, int enable)
{
    if (enable)
        jl_gc_register_callback(&gc_cblist_task_scanner, (jl_gc_cb_func_t)cb);
    else
        jl_gc_deregister_callback(&gc_cblist_task_scanner, (jl_gc_cb_func_t)cb);
}

JL_DLLEXPORT void jl_gc_set_cb_pre_gc(jl_gc_cb_pre_gc_t cb, int enable)
{
    if (enable)
        jl_gc_register_callback(&gc_cblist_pre_gc, (jl_gc_cb_func_t)cb);
    else
        jl_gc_deregister_callback(&gc_cblist_pre_gc, (jl_gc_cb_func_t)cb);
}

JL_DLLEXPORT void jl_gc_set_cb_post_gc(jl_gc_cb_post_gc_t cb, int enable)
{
    if (enable)
        jl_gc_register_callback(&gc_cblist_post_gc, (jl_gc_cb_func_t)cb);
    else
        jl_gc_deregister_callback(&gc_cblist_post_gc, (jl_gc_cb_func_t)cb);
}

JL_DLLEXPORT void jl_gc_set_cb_notify_external_alloc(jl_gc_cb_notify_external_alloc_t cb, int enable)
{
    if (enable)
        jl_gc_register_callback(&gc_cblist_notify_external_alloc, (jl_gc_cb_func_t)cb);
    else
        jl_gc_deregister_callback(&gc_cblist_notify_external_alloc, (jl_gc_cb_func_t)cb);
}

JL_DLLEXPORT void jl_gc_set_cb_notify_external_free(jl_gc_cb_notify_external_free_t cb, int enable)
{
    if (enable)
        jl_gc_register_callback(&gc_cblist_notify_external_free, (jl_gc_cb_func_t)cb);
    else
        jl_gc_deregister_callback(&gc_cblist_notify_external_free, (jl_gc_cb_func_t)cb);
}

// Save/restore local mark stack to/from thread-local storage.

STATIC_INLINE void export_gc_state(jl_ptls_t ptls, jl_gc_mark_sp_t *sp) {
    ptls->gc_mark_sp = *sp;
}

STATIC_INLINE void import_gc_state(jl_ptls_t ptls, jl_gc_mark_sp_t *sp) {
    // Has the stack been reallocated in the meantime?
    *sp = ptls->gc_mark_sp;
}

// Protect all access to `finalizer_list_marked` and `to_finalize`.
// For accessing `ptls->finalizers`, the lock is needed if a thread
// is going to realloc the buffer (of its own list) or accessing the
// list of another thread
static jl_mutex_t finalizers_lock;
static jl_mutex_t gc_cache_lock;

// Flag that tells us whether we need to support conservative marking
// of objects.
static int support_conservative_marking = 0;

/**
 * Note about GC synchronization:
 *
 * When entering `jl_gc_collect()`, `jl_gc_running` is atomically changed from
 * `0` to `1` to make sure that only one thread can be running the GC. Other
 * threads that enters `jl_gc_collect()` at the same time (or later calling
 * from unmanaged code) will wait in `jl_gc_collect()` until the GC is finished.
 *
 * Before starting the mark phase the GC thread calls `jl_safepoint_gc_start()`
 * and `jl_gc_wait_for_the_world()`
 * to make sure all the thread are in a safe state for the GC. The function
 * activates the safepoint and wait for all the threads to get ready for the
 * GC (`gc_state != 0`). It also acquires the `finalizers` lock so that no
 * other thread will access them when the GC is running.
 *
 * During the mark and sweep phase of the GC, the threads that are not running
 * the GC should either be running unmanaged code (or code section that does
 * not have a GC critical region mainly including storing to the stack or
 * another object) or paused at a safepoint and wait for the GC to finish.
 * If a thread want to switch from running unmanaged code to running managed
 * code, it has to perform a GC safepoint check after setting the `gc_state`
 * flag (see `jl_gc_state_save_and_set()`. it is possible that the thread might
 * have `gc_state == 0` in the middle of the GC transition back before entering
 * the safepoint. This is fine since the thread won't be executing any GC
 * critical region during that time).
 *
 * The finalizers are run after the GC finishes in normal mode (the `gc_state`
 * when `jl_gc_collect` is called) with `jl_in_finalizer = 1`. (TODO:) When we
 * have proper support of GC transition in codegen, we should execute the
 * finalizers in unmanaged (GC safe) mode.
 */

jl_gc_num_t gc_num = {0,0,0,0,0,0,0,0,0,0,0,0,0,0};
static size_t last_long_collect_interval;

pagetable_t memory_map;

// List of marked big objects.  Not per-thread.  Accessed only by master thread.
bigval_t *big_objects_marked = NULL;

// finalization
// `ptls->finalizers` and `finalizer_list_marked` might have tagged pointers.
// If an object pointer has the lowest bit set, the next pointer is an unboxed
// c function pointer.
// `to_finalize` should not have tagged pointers.
arraylist_t finalizer_list_marked;
arraylist_t to_finalize;

NOINLINE uintptr_t gc_get_stack_ptr(void)
{
    return (uintptr_t)jl_get_frame_addr();
}

#define should_timeout() 0

static void jl_gc_wait_for_the_world(void)
{
    if (jl_n_threads > 1)
        jl_wake_libuv();
    for (int i = 0;i < jl_n_threads;i++) {
        jl_ptls_t ptls2 = jl_all_tls_states[i];
        // FIXME: The acquire load pairs with the release stores
        // in the signal handler of safepoint so we are sure that
        // all the stores on those threads are visible. However,
        // we're currently not using atomic stores in mutator threads.
        // We should either use atomic store release there too or use signals
        // to flush the memory operations on those threads.
        while (!ptls2->gc_state || !jl_atomic_load_acquire(&ptls2->gc_state)) {
            jl_cpu_pause(); // yield?
        }
    }
}

// malloc wrappers, aligned allocation

#if defined(_OS_WINDOWS_)
STATIC_INLINE void *jl_malloc_aligned(size_t sz, size_t align)
{
    return _aligned_malloc(sz ? sz : 1, align);
}
STATIC_INLINE void *jl_realloc_aligned(void *p, size_t sz, size_t oldsz,
                                       size_t align)
{
    (void)oldsz;
    return _aligned_realloc(p, sz ? sz : 1, align);
}
STATIC_INLINE void jl_free_aligned(void *p) JL_NOTSAFEPOINT
{
    _aligned_free(p);
}
#else
STATIC_INLINE void *jl_malloc_aligned(size_t sz, size_t align)
{
#if defined(_P64) || defined(__APPLE__)
    if (align <= 16)
        return malloc(sz);
#endif
    void *ptr;
    if (posix_memalign(&ptr, align, sz))
        return NULL;
    return ptr;
}
STATIC_INLINE void *jl_realloc_aligned(void *d, size_t sz, size_t oldsz,
                                       size_t align)
{
#if defined(_P64) || defined(__APPLE__)
    if (align <= 16)
        return realloc(d, sz);
#endif
    void *b = jl_malloc_aligned(sz, align);
    if (b != NULL) {
        memcpy(b, d, oldsz > sz ? sz : oldsz);
        free(d);
    }
    return b;
}
STATIC_INLINE void jl_free_aligned(void *p) JL_NOTSAFEPOINT
{
    free(p);
}
#endif
#define malloc_cache_align(sz) jl_malloc_aligned(sz, JL_CACHE_BYTE_ALIGNMENT)
#define realloc_cache_align(p, sz, oldsz) jl_realloc_aligned(p, sz, oldsz, JL_CACHE_BYTE_ALIGNMENT)

static void schedule_finalization(void *o, void *f) JL_NOTSAFEPOINT
{
    arraylist_push(&to_finalize, o);
    arraylist_push(&to_finalize, f);
}

static void run_finalizer(jl_ptls_t ptls, jl_value_t *o, jl_value_t *ff)
{
    if (gc_ptr_tag(o, 1)) {
        ((void (*)(void*))ff)(gc_ptr_clear_tag(o, 1));
        return;
    }
    jl_value_t *args[2] = {ff,o};
    JL_TRY {
        size_t last_age = jl_get_ptls_states()->world_age;
        jl_get_ptls_states()->world_age = jl_world_counter;
        jl_apply(args, 2);
        jl_get_ptls_states()->world_age = last_age;
    }
    JL_CATCH {
        jl_printf(JL_STDERR, "error in running finalizer: ");
        jl_static_show(JL_STDERR, jl_current_exception());
        jl_printf(JL_STDERR, "\n");
    }
}

// if `need_sync` is true, the `list` is the `finalizers` list of another
// thread and we need additional synchronizations
static void finalize_object(arraylist_t *list, jl_value_t *o,
                            arraylist_t *copied_list, int need_sync) JL_NOTSAFEPOINT
{
    // The acquire load makes sure that the first `len` objects are valid.
    // If `need_sync` is true, all mutations of the content should be limited
    // to the first `oldlen` elements and no mutation is allowed after the
    // new length is published with the `cmpxchg` at the end of the function.
    // This way, the mutation should not conflict with the owning thread,
    // which only writes to locations later than `len`
    // and will not resize the buffer without acquiring the lock.
    size_t len = need_sync ? jl_atomic_load_acquire(&list->len) : list->len;
    size_t oldlen = len;
    void **items = list->items;
    size_t j = 0;
    for (size_t i = 0; i < len; i += 2) {
        void *v = items[i];
        int move = 0;
        if (o == (jl_value_t*)gc_ptr_clear_tag(v, 1)) {
            void *f = items[i + 1];
            move = 1;
            arraylist_push(copied_list, v);
            arraylist_push(copied_list, f);
        }
        if (move || __unlikely(!v)) {
            // remove item
        }
        else {
            if (j < i) {
                items[j] = items[i];
                items[j+1] = items[i+1];
            }
            j += 2;
        }
    }
    len = j;
    if (oldlen == len)
        return;
    if (need_sync) {
        // The memset needs to be unconditional since the thread might have
        // already read the length.
        // The `memset` (like any other content mutation) has to be done
        // **before** the `cmpxchg` which publishes the length.
        memset(&items[len], 0, (oldlen - len) * sizeof(void*));
        jl_atomic_compare_exchange(&list->len, oldlen, len);
    }
    else {
        list->len = len;
    }
}

// The first two entries are assumed to be empty and the rest are assumed to
// be pointers to `jl_value_t` objects
static void jl_gc_push_arraylist(jl_ptls_t ptls, arraylist_t *list)
{
    void **items = list->items;
    items[0] = (void*)JL_GC_ENCODE_PUSHARGS(list->len - 2);
    items[1] = ptls->pgcstack;
    ptls->pgcstack = (jl_gcframe_t*)items;
}

// Same assumption as `jl_gc_push_arraylist`. Requires the finalizers lock
// to be hold for the current thread and will release the lock when the
// function returns.
static void jl_gc_run_finalizers_in_list(jl_ptls_t ptls, arraylist_t *list)
{
    // empty out the first two entries for the GC frame
    arraylist_push(list, list->items[0]);
    arraylist_push(list, list->items[1]);
    jl_gc_push_arraylist(ptls, list);
    jl_value_t **items = (jl_value_t**)list->items;
    size_t len = list->len;
    JL_UNLOCK_NOGC(&finalizers_lock);
    // run finalizers in reverse order they were added, so lower-level finalizers run last
    for (size_t i = len-4; i >= 2; i -= 2)
        run_finalizer(ptls, items[i], items[i + 1]);
    // first entries were moved last to make room for GC frame metadata
    run_finalizer(ptls, items[len-2], items[len-1]);
    // matches the jl_gc_push_arraylist above
    JL_GC_POP();
}

static void run_finalizers(jl_ptls_t ptls)
{
    // Racy fast path:
    // The race here should be OK since the race can only happen if
    // another thread is writing to it with the lock held. In such case,
    // we don't need to run pending finalizers since the writer thread
    // will flush it.
    if (to_finalize.len == 0)
        return;
    JL_LOCK_NOGC(&finalizers_lock);
    if (to_finalize.len == 0) {
        JL_UNLOCK_NOGC(&finalizers_lock);
        return;
    }
    arraylist_t copied_list;
    memcpy(&copied_list, &to_finalize, sizeof(copied_list));
    if (to_finalize.items == to_finalize._space) {
        copied_list.items = copied_list._space;
    }
    arraylist_new(&to_finalize, 0);
    // This releases the finalizers lock.
    jl_gc_run_finalizers_in_list(ptls, &copied_list);
    arraylist_free(&copied_list);
}

JL_DLLEXPORT void jl_gc_enable_finalizers(jl_ptls_t ptls, int on)
{
    int old_val = ptls->finalizers_inhibited;
    int new_val = old_val + (on ? -1 : 1);
    ptls->finalizers_inhibited = new_val;
    if (!new_val && old_val && !ptls->in_finalizer) {
        ptls->in_finalizer = 1;
        run_finalizers(ptls);
        ptls->in_finalizer = 0;
    }
}

static void schedule_all_finalizers(arraylist_t *flist) JL_NOTSAFEPOINT
{
    void **items = flist->items;
    size_t len = flist->len;
    for(size_t i = 0; i < len; i+=2) {
        void *v = items[i];
        void *f = items[i + 1];
        if (__unlikely(!v))
            continue;
        schedule_finalization(v, f);
    }
    flist->len = 0;
}

void jl_gc_run_all_finalizers(jl_ptls_t ptls)
{
    schedule_all_finalizers(&finalizer_list_marked);
    for (int i = 0;i < jl_n_threads;i++) {
        jl_ptls_t ptls2 = jl_all_tls_states[i];
        schedule_all_finalizers(&ptls2->finalizers);
    }
    run_finalizers(ptls);
}

static void gc_add_finalizer_(jl_ptls_t ptls, void *v, void *f)
{
    int8_t gc_state = jl_gc_unsafe_enter(ptls);
    arraylist_t *a = &ptls->finalizers;
    // This acquire load and the release store at the end are used to
    // synchronize with `finalize_object` on another thread. Apart from the GC,
    // which is blocked by entering a unsafe region, there might be only
    // one other thread accessing our list in `finalize_object`
    // (only one thread since it needs to acquire the finalizer lock).
    // Similar to `finalize_object`, all content mutation has to be done
    // between the acquire and the release of the length.
    size_t oldlen = jl_atomic_load_acquire(&a->len);
    if (__unlikely(oldlen + 2 > a->max)) {
        JL_LOCK_NOGC(&finalizers_lock);
        // `a->len` might have been modified.
        // Another possibility is to always grow the array to `oldlen + 2` but
        // it's simpler this way and uses slightly less memory =)
        oldlen = a->len;
        arraylist_grow(a, 2);
        a->len = oldlen;
        JL_UNLOCK_NOGC(&finalizers_lock);
    }
    void **items = a->items;
    items[oldlen] = v;
    items[oldlen + 1] = f;
    jl_atomic_store_release(&a->len, oldlen + 2);
    jl_gc_unsafe_leave(ptls, gc_state);
}

JL_DLLEXPORT void jl_gc_add_ptr_finalizer(jl_ptls_t ptls, jl_value_t *v, void *f)
{
    gc_add_finalizer_(ptls, (void*)(((uintptr_t)v) | 1), f);
}

JL_DLLEXPORT void jl_gc_add_finalizer_th(jl_ptls_t ptls, jl_value_t *v, jl_function_t *f)
{
    if (__unlikely(jl_typeis(f, jl_voidpointer_type))) {
        jl_gc_add_ptr_finalizer(ptls, v, jl_unbox_voidpointer(f));
    }
    else {
        gc_add_finalizer_(ptls, v, f);
    }
}

JL_DLLEXPORT void jl_finalize_th(jl_ptls_t ptls, jl_value_t *o)
{
    JL_LOCK_NOGC(&finalizers_lock);
    // Copy the finalizers into a temporary list so that code in the finalizer
    // won't change the list as we loop through them.
    // This list is also used as the GC frame when we are running the finalizers
    arraylist_t copied_list;
    arraylist_new(&copied_list, 0);
    // No need to check the to_finalize list since the user is apparently
    // still holding a reference to the object
    for (int i = 0;i < jl_n_threads;i++) {
        jl_ptls_t ptls2 = jl_all_tls_states[i];
        finalize_object(&ptls2->finalizers, o, &copied_list, ptls != ptls2);
    }
    finalize_object(&finalizer_list_marked, o, &copied_list, 0);
    if (copied_list.len > 0) {
        // This releases the finalizers lock.
        jl_gc_run_finalizers_in_list(ptls, &copied_list);
    }
    else {
        JL_UNLOCK_NOGC(&finalizers_lock);
    }
    arraylist_free(&copied_list);
}

static void gc_sweep_foreign_objs_in_list(arraylist_t *objs)
{
    size_t p = 0;
    for (size_t i = 0; i < objs->len; i++) {
        jl_value_t *v = (jl_value_t *)(objs->items[i]);
        jl_datatype_t *t = (jl_datatype_t *)(jl_typeof(v));
        const jl_datatype_layout_t *layout = t->layout;
        jl_fielddescdyn_t *desc = (jl_fielddescdyn_t*)jl_dt_layout_fields(layout);
        if (!gc_ptr_tag(v, 1)) {
            desc->sweepfunc(v);
        }
        else {
            objs->items[p++] = v;
        }
    }
    objs->len = p;
}

static void gc_sweep_foreign_objs(void)
{
    for (int i = 0;i < jl_n_threads; i++) {
        jl_ptls_t ptls2 = jl_all_tls_states[i];
        gc_sweep_foreign_objs_in_list(&ptls2->sweep_objs);
    }
}

// GC knobs and self-measurement variables
static int64_t last_gc_total_bytes = 0;

#ifdef _P64
#define default_collect_interval (5600*1024*sizeof(void*))
static size_t max_collect_interval = 1250000000UL;
#else
#define default_collect_interval (3200*1024*sizeof(void*))
static size_t max_collect_interval =  500000000UL;
#endif

// global variables for GC stats

// Resetting the object to a young object, this is used when marking the
// finalizer list to collect them the next time because the object is very
// likely dead. This also won't break the GC invariance since these objects
// are not reachable from anywhere else.
static int mark_reset_age = 0;

/*
 * The state transition looks like :
 *
 * ([(quick)sweep] means either a sweep or a quicksweep)
 *
 * <-[(quick)sweep]-
 *                 |
 *     ---->  GC_OLD  <--[(quick)sweep && age>promotion]--
 *     |     |                                           |
 *     |     |  GC_MARKED (in remset)                    |
 *     |     |     ^            |                        |
 *     |   [mark]  |          [mark]                     |
 *     |     |     |            |                        |
 *     |     |     |            |                        |
 *  [sweep]  | [write barrier]  |                        |
 *     |     v     |            v                        |
 *     ----- GC_OLD_MARKED <----                         |
 *              |               ^                        |
 *              |               |                        |
 *              --[quicksweep]---                        |
 *                                                       |
 *  ========= above this line objects are old =========  |
 *                                                       |
 *  ----[new]------> GC_CLEAN ------[mark]-----------> GC_MARKED
 *                    |    ^                                   |
 *  <-[(quick)sweep]---    |                                   |
 *                         --[(quick)sweep && age<=promotion]---
 */

// A quick sweep is a sweep where `!sweep_full`
// It means we won't touch GC_OLD_MARKED objects (old gen).

// When a reachable object has survived more than PROMOTE_AGE+1 collections
// it is tagged with GC_OLD during sweep and will be promoted on next mark
// because at that point we can know easily if it references young objects.
// Marked old objects that reference young ones are kept in the remset.

// When a write barrier triggers, the offending marked object is both queued,
// so as not to trigger the barrier again, and put in the remset.


#define PROMOTE_AGE 1
// this cannot be increased as is without changing :
// - sweep_page which is specialized for 1bit age
// - the size of the age storage in jl_gc_pagemeta_t


static int64_t scanned_bytes; // young bytes scanned while marking
static int64_t perm_scanned_bytes; // old bytes scanned while marking
static int prev_sweep_full = 1;

#define inc_sat(v,s) v = (v) >= s ? s : (v)+1

// Full collection heuristics
static int64_t live_bytes = 0;
static int64_t promoted_bytes = 0;
static int64_t last_full_live = 0;  // live_bytes after last full collection
static int64_t last_live_bytes = 0; // live_bytes at last collection
static int64_t grown_heap_age = 0;  // # of collects since live_bytes grew and remained
#ifdef __GLIBC__
// maxrss at last malloc_trim
static int64_t last_trim_maxrss = 0;
#endif

static void gc_sync_cache_nolock(jl_ptls_t ptls, jl_gc_mark_cache_t *gc_cache) JL_NOTSAFEPOINT
{
    const int nbig = gc_cache->nbig_obj;
    for (int i = 0; i < nbig; i++) {
        void *ptr = gc_cache->big_obj[i];
        bigval_t *hdr = (bigval_t*)gc_ptr_clear_tag(ptr, 1);
        gc_big_object_unlink(hdr);
        if (gc_ptr_tag(ptr, 1)) {
            gc_big_object_link(hdr, &ptls->heap.big_objects);
        }
        else {
            // Move hdr from `big_objects` list to `big_objects_marked list`
            gc_big_object_link(hdr, &big_objects_marked);
        }
    }
    gc_cache->nbig_obj = 0;
    perm_scanned_bytes += gc_cache->perm_scanned_bytes;
    scanned_bytes += gc_cache->scanned_bytes;
    gc_cache->perm_scanned_bytes = 0;
    gc_cache->scanned_bytes = 0;
}

static void gc_sync_cache(jl_ptls_t ptls) JL_NOTSAFEPOINT
{
    JL_LOCK_NOGC(&gc_cache_lock);
    gc_sync_cache_nolock(ptls, &ptls->gc_cache);
    JL_UNLOCK_NOGC(&gc_cache_lock);
}

// No other threads can be running marking at the same time
static void gc_sync_all_caches_nolock(jl_ptls_t ptls)
{
    for (int t_i = 0; t_i < jl_n_threads; t_i++) {
        jl_ptls_t ptls2 = jl_all_tls_states[t_i];
        gc_sync_cache_nolock(ptls, &ptls2->gc_cache);
    }
}

STATIC_INLINE void gc_queue_big_marked(jl_ptls_t ptls, bigval_t *hdr,
                                       int toyoung) JL_NOTSAFEPOINT
{
    const int nentry = sizeof(ptls->gc_cache.big_obj) / sizeof(void*);
    size_t nobj = ptls->gc_cache.nbig_obj;
    if (__unlikely(nobj >= nentry)) {
        gc_sync_cache(ptls);
        nobj = 0;
    }
    uintptr_t v = (uintptr_t)hdr;
    ptls->gc_cache.big_obj[nobj] = (void*)(toyoung ? (v | 1) : v);
    ptls->gc_cache.nbig_obj = nobj + 1;
}

// `gc_setmark_tag` can be called concurrently on multiple threads.
// In all cases, the function atomically sets the mark bits and returns
// the GC bits set as well as if the tag was unchanged by this thread.
// All concurrent calls on the same object are guaranteed to be setting the
// bits to the same value.
// For normal objects, this is the bits with only `GC_MARKED` changed to `1`
// For buffers, this is the bits of the owner object.
// For `mark_reset_age`, this is `GC_MARKED` with `GC_OLD` cleared.
// The return value is `1` if the object was not marked before.
// Returning `0` can happen if another thread marked it in parallel.
STATIC_INLINE int gc_setmark_tag(jl_taggedvalue_t *o, uint8_t mark_mode,
                                 uintptr_t tag, uint8_t *bits) JL_NOTSAFEPOINT
{
    assert(!gc_marked(tag));
    assert(gc_marked(mark_mode));
    if (mark_reset_age) {
        // Reset the object as if it was just allocated
        mark_mode = GC_MARKED;
        tag = gc_set_bits(tag, mark_mode);
    }
    else {
        if (gc_old(tag))
            mark_mode = GC_OLD_MARKED;
        tag = tag | mark_mode;
        assert((tag & 0x3) == mark_mode);
    }
    *bits = mark_mode;
    tag = jl_atomic_exchange_relaxed(&o->header, tag);
    verify_val(jl_valueof(o));
    return !gc_marked(tag);
}

// This function should be called exactly once during marking for each big
// object being marked to update the big objects metadata.
STATIC_INLINE void gc_setmark_big(jl_ptls_t ptls, jl_taggedvalue_t *o,
                                  uint8_t mark_mode) JL_NOTSAFEPOINT
{
    assert(!page_metadata(o));
    bigval_t *hdr = bigval_header(o);
    if (mark_mode == GC_OLD_MARKED) {
        ptls->gc_cache.perm_scanned_bytes += hdr->sz & ~3;
        gc_queue_big_marked(ptls, hdr, 0);
    }
    else {
        ptls->gc_cache.scanned_bytes += hdr->sz & ~3;
        // We can't easily tell if the object is old or being promoted
        // from the gc bits but if the `age` is `0` then the object
        // must be already on a young list.
        if (mark_reset_age && hdr->age) {
            // Reset the object as if it was just allocated
            hdr->age = 0;
            gc_queue_big_marked(ptls, hdr, 1);
        }
    }
    objprofile_count(jl_typeof(jl_valueof(o)),
                     mark_mode == GC_OLD_MARKED, hdr->sz & ~3);
}

// This function should be called exactly once during marking for each pool
// object being marked to update the page metadata.
STATIC_INLINE void gc_setmark_pool_(jl_ptls_t ptls, jl_taggedvalue_t *o,
                                    uint8_t mark_mode,
                                    jl_gc_pagemeta_t *page) JL_NOTSAFEPOINT
{
#ifdef MEMDEBUG
    gc_setmark_big(ptls, o, mark_mode);
#else
    jl_assume(page);
    if (mark_mode == GC_OLD_MARKED) {
        ptls->gc_cache.perm_scanned_bytes += page->osize;
        jl_atomic_fetch_add_relaxed(&page->nold, 1);
    }
    else {
        ptls->gc_cache.scanned_bytes += page->osize;
        if (mark_reset_age) {
            page->has_young = 1;
            char *page_begin = gc_page_data(o) + GC_PAGE_OFFSET;
            int obj_id = (((char*)o) - page_begin) / page->osize;
            uint8_t *ages = page->ages + obj_id / 8;
            jl_atomic_fetch_and_relaxed(ages, ~(1 << (obj_id % 8)));
        }
    }
    objprofile_count(jl_typeof(jl_valueof(o)),
                     mark_mode == GC_OLD_MARKED, page->osize);
    page->has_marked = 1;
#endif
}

STATIC_INLINE void gc_setmark_pool(jl_ptls_t ptls, jl_taggedvalue_t *o,
                                   uint8_t mark_mode) JL_NOTSAFEPOINT
{
    gc_setmark_pool_(ptls, o, mark_mode, page_metadata(o));
}

STATIC_INLINE void gc_setmark(jl_ptls_t ptls, jl_taggedvalue_t *o,
                              uint8_t mark_mode, size_t sz) JL_NOTSAFEPOINT
{
    if (sz <= GC_MAX_SZCLASS) {
        gc_setmark_pool(ptls, o, mark_mode);
    }
    else {
        gc_setmark_big(ptls, o, mark_mode);
    }
}

STATIC_INLINE void gc_setmark_buf_(jl_ptls_t ptls, void *o, uint8_t mark_mode, size_t minsz) JL_NOTSAFEPOINT
{
    jl_taggedvalue_t *buf = jl_astaggedvalue(o);
    uintptr_t tag = buf->header;
    if (gc_marked(tag))
        return;
    uint8_t bits;
    // If the object is larger than the max pool size it can't be a pool object.
    // This should be accurate most of the time but there might be corner cases
    // where the size estimate is a little off so we do a pool lookup to make
    // sure.
    if (__likely(gc_setmark_tag(buf, mark_mode, tag, &bits)) && !gc_verifying) {
        if (minsz <= GC_MAX_SZCLASS) {
            jl_gc_pagemeta_t *page = page_metadata(buf);
            if (page) {
                gc_setmark_pool_(ptls, buf, bits, page);
                return;
            }
        }
        gc_setmark_big(ptls, buf, bits);
    }
}

void gc_setmark_buf(jl_ptls_t ptls, void *o, uint8_t mark_mode, size_t minsz) JL_NOTSAFEPOINT
{
    gc_setmark_buf_(ptls, o, mark_mode, minsz);
}

void jl_gc_force_mark_old(jl_ptls_t ptls, jl_value_t *v) JL_NOTSAFEPOINT
{
    jl_taggedvalue_t *o = jl_astaggedvalue(v);
    jl_datatype_t *dt = (jl_datatype_t*)jl_typeof(v);
    size_t dtsz = jl_datatype_size(dt);
    if (o->bits.gc == GC_OLD_MARKED)
        return;
    o->bits.gc = GC_OLD_MARKED;
    if (dt == jl_simplevector_type) {
        size_t l = jl_svec_len(v);
        dtsz = l * sizeof(void*) + sizeof(jl_svec_t);
    }
    else if (dt->name == jl_array_typename) {
        jl_array_t *a = (jl_array_t*)v;
        if (!a->flags.pooled)
            dtsz = GC_MAX_SZCLASS + 1;
    }
    else if (dt == jl_module_type) {
        dtsz = sizeof(jl_module_t);
    }
    else if (dt == jl_task_type) {
        dtsz = sizeof(jl_task_t);
    }
    else if (dt == jl_symbol_type) {
        return;
    }
    gc_setmark(ptls, o, GC_OLD_MARKED, dtsz);
    if (dt->layout->npointers != 0)
        jl_gc_queue_root(v);
}

static inline void maybe_collect(jl_ptls_t ptls)
{
    if (ptls->gc_num.allocd >= 0 || gc_debug_check_other()) {
        jl_gc_collect(JL_GC_AUTO);
    }
    else {
        jl_gc_safepoint_(ptls);
    }
}

// weak references

JL_DLLEXPORT jl_weakref_t *jl_gc_new_weakref_th(jl_ptls_t ptls,
                                                jl_value_t *value)
{
    jl_weakref_t *wr = (jl_weakref_t*)jl_gc_alloc(ptls, sizeof(void*),
                                                  jl_weakref_type);
    wr->value = value;  // NOTE: wb not needed here
    arraylist_push(&ptls->heap.weak_refs, wr);
    return wr;
}

static void sweep_weak_refs(void)
{
    for (int i = 0; i < jl_n_threads; i++) {
        jl_ptls_t ptls2 = jl_all_tls_states[i];
        size_t n = 0;
        size_t ndel = 0;
        size_t l = ptls2->heap.weak_refs.len;
        void **lst = ptls2->heap.weak_refs.items;
        if (l == 0)
            continue;
        while (1) {
            jl_weakref_t *wr = (jl_weakref_t*)lst[n];
            if (gc_marked(jl_astaggedvalue(wr)->bits.gc)) {
                // weakref itself is alive,
                // so the user could still re-set it to a new value
                if (!gc_marked(jl_astaggedvalue(wr->value)->bits.gc))
                    wr->value = (jl_value_t*)jl_nothing;
                n++;
            }
            else {
                ndel++;
            }
            if (n >= l - ndel)
                break;
            void *tmp = lst[n];
            lst[n] = lst[n + ndel];
            lst[n + ndel] = tmp;
        }
        ptls2->heap.weak_refs.len -= ndel;
    }
}

// big value list

// Size includes the tag and the tag is not cleared!!
JL_DLLEXPORT jl_value_t *jl_gc_big_alloc(jl_ptls_t ptls, size_t sz)
{
    maybe_collect(ptls);
    size_t offs = offsetof(bigval_t, header);
    assert(sz >= sizeof(jl_taggedvalue_t) && "sz must include tag");
    static_assert(offsetof(bigval_t, header) >= sizeof(void*), "Empty bigval header?");
    static_assert(sizeof(bigval_t) % JL_HEAP_ALIGNMENT == 0, "");
    size_t allocsz = LLT_ALIGN(sz + offs, JL_CACHE_BYTE_ALIGNMENT);
    if (allocsz < sz)  // overflow in adding offs, size was "negative"
        jl_throw(jl_memory_exception);
    bigval_t *v = (bigval_t*)malloc_cache_align(allocsz);
    if (v == NULL)
        jl_throw(jl_memory_exception);
    gc_invoke_callbacks(jl_gc_cb_notify_external_alloc_t,
        gc_cblist_notify_external_alloc, (v, allocsz));
    ptls->gc_num.allocd += allocsz;
    ptls->gc_num.bigalloc++;
#ifdef MEMDEBUG
    memset(v, 0xee, allocsz);
#endif
    v->sz = allocsz;
    v->age = 0;
    gc_big_object_link(v, &ptls->heap.big_objects);
    return jl_valueof(&v->header);
}

// Sweep list rooted at *pv, removing and freeing any unmarked objects.
// Return pointer to last `next` field in the culled list.
static bigval_t **sweep_big_list(int sweep_full, bigval_t **pv) JL_NOTSAFEPOINT
{
    bigval_t *v = *pv;
    while (v != NULL) {
        bigval_t *nxt = v->next;
        int bits = v->bits.gc;
        int old_bits = bits;
        if (gc_marked(bits)) {
            pv = &v->next;
            int age = v->age;
            if (age >= PROMOTE_AGE || bits == GC_OLD_MARKED) {
                if (sweep_full || bits == GC_MARKED) {
                    bits = GC_OLD;
                }
            }
            else {
                inc_sat(age, PROMOTE_AGE);
                v->age = age;
                bits = GC_CLEAN;
            }
            v->bits.gc = bits;
        }
        else {
            // Remove v from list and free it
            *pv = nxt;
            if (nxt)
                nxt->prev = pv;
            gc_num.freed += v->sz&~3;
#ifdef MEMDEBUG
            memset(v, 0xbb, v->sz&~3);
#endif
            gc_invoke_callbacks(jl_gc_cb_notify_external_free_t,
                gc_cblist_notify_external_free, (v));
            jl_free_aligned(v);
        }
        gc_time_count_big(old_bits, bits);
        v = nxt;
    }
    return pv;
}

static void sweep_big(jl_ptls_t ptls, int sweep_full) JL_NOTSAFEPOINT
{
    gc_time_big_start();
    for (int i = 0;i < jl_n_threads;i++)
        sweep_big_list(sweep_full, &jl_all_tls_states[i]->heap.big_objects);
    if (sweep_full) {
        bigval_t **last_next = sweep_big_list(sweep_full, &big_objects_marked);
        // Move all survivors from big_objects_marked list to big_objects list.
        if (ptls->heap.big_objects)
            ptls->heap.big_objects->prev = last_next;
        *last_next = ptls->heap.big_objects;
        ptls->heap.big_objects = big_objects_marked;
        if (ptls->heap.big_objects)
            ptls->heap.big_objects->prev = &ptls->heap.big_objects;
        big_objects_marked = NULL;
    }
    gc_time_big_end();
}

// tracking Arrays with malloc'd storage

void jl_gc_track_malloced_array(jl_ptls_t ptls, jl_array_t *a) JL_NOTSAFEPOINT
{
    // This is **NOT** a GC safe point.
    mallocarray_t *ma;
    if (ptls->heap.mafreelist == NULL) {
        ma = (mallocarray_t*)malloc_s(sizeof(mallocarray_t));
    }
    else {
        ma = ptls->heap.mafreelist;
        ptls->heap.mafreelist = ma->next;
    }
    ma->a = a;
    ma->next = ptls->heap.mallocarrays;
    ptls->heap.mallocarrays = ma;
}

void jl_gc_count_allocd(size_t sz) JL_NOTSAFEPOINT
{
    jl_ptls_t ptls = jl_get_ptls_states();
    ptls->gc_num.allocd += sz;
}

static void combine_thread_gc_counts(jl_gc_num_t *dest) JL_NOTSAFEPOINT
{
    for (int i = 0; i < jl_n_threads; i++) {
        jl_ptls_t ptls = jl_all_tls_states[i];
        if (ptls) {
            dest->allocd += (jl_atomic_load_relaxed(&ptls->gc_num.allocd) + gc_num.interval);
            dest->freed += jl_atomic_load_relaxed(&ptls->gc_num.freed);
            dest->malloc += jl_atomic_load_relaxed(&ptls->gc_num.malloc);
            dest->realloc += jl_atomic_load_relaxed(&ptls->gc_num.realloc);
            dest->poolalloc += jl_atomic_load_relaxed(&ptls->gc_num.poolalloc);
            dest->bigalloc += jl_atomic_load_relaxed(&ptls->gc_num.bigalloc);
            dest->freecall += jl_atomic_load_relaxed(&ptls->gc_num.freecall);
        }
    }
}

static void reset_thread_gc_counts(void) JL_NOTSAFEPOINT
{
    for (int i = 0; i < jl_n_threads; i++) {
        jl_ptls_t ptls = jl_all_tls_states[i];
        if (ptls) {
            memset(&ptls->gc_num, 0, sizeof(jl_thread_gc_num_t));
            ptls->gc_num.allocd = -(int64_t)gc_num.interval;
        }
    }
}

void jl_gc_reset_alloc_count(void) JL_NOTSAFEPOINT
{
    combine_thread_gc_counts(&gc_num);
    live_bytes += (gc_num.deferred_alloc + gc_num.allocd);
    gc_num.allocd = 0;
    gc_num.deferred_alloc = 0;
    reset_thread_gc_counts();
}

static size_t array_nbytes(jl_array_t *a) JL_NOTSAFEPOINT
{
    size_t sz = 0;
    int isbitsunion = jl_array_isbitsunion(a);
    if (jl_array_ndims(a) == 1)
        sz = a->elsize * a->maxsize + ((a->elsize == 1 && !isbitsunion) ? 1 : 0);
    else
        sz = a->elsize * jl_array_len(a);
    if (isbitsunion)
        // account for isbits Union array selector bytes
        sz += jl_array_len(a);
    return sz;
}

static void jl_gc_free_array(jl_array_t *a) JL_NOTSAFEPOINT
{
    if (a->flags.how == 2) {
        char *d = (char*)a->data - a->offset*a->elsize;
        if (a->flags.isaligned)
            jl_free_aligned(d);
        else
            free(d);
        gc_num.freed += array_nbytes(a);
    }
}

static void sweep_malloced_arrays(void) JL_NOTSAFEPOINT
{
    gc_time_mallocd_array_start();
    for (int t_i = 0;t_i < jl_n_threads;t_i++) {
        jl_ptls_t ptls2 = jl_all_tls_states[t_i];
        mallocarray_t *ma = ptls2->heap.mallocarrays;
        mallocarray_t **pma = &ptls2->heap.mallocarrays;
        while (ma != NULL) {
            mallocarray_t *nxt = ma->next;
            int bits = jl_astaggedvalue(ma->a)->bits.gc;
            if (gc_marked(bits)) {
                pma = &ma->next;
            }
            else {
                *pma = nxt;
                assert(ma->a->flags.how == 2);
                jl_gc_free_array(ma->a);
                ma->next = ptls2->heap.mafreelist;
                ptls2->heap.mafreelist = ma;
            }
            gc_time_count_mallocd_array(bits);
            ma = nxt;
        }
    }
    gc_time_mallocd_array_end();
}

// pool allocation
static inline jl_taggedvalue_t *reset_page(const jl_gc_pool_t *p, jl_gc_pagemeta_t *pg, jl_taggedvalue_t *fl) JL_NOTSAFEPOINT
{
    assert(GC_PAGE_OFFSET >= sizeof(void*));
    pg->nfree = (GC_PAGE_SZ - GC_PAGE_OFFSET) / p->osize;
    jl_ptls_t ptls2 = jl_all_tls_states[pg->thread_n];
    pg->pool_n = p - ptls2->heap.norm_pools;
    memset(pg->ages, 0, GC_PAGE_SZ / 8 / p->osize + 1);
    jl_taggedvalue_t *beg = (jl_taggedvalue_t*)(pg->data + GC_PAGE_OFFSET);
    jl_taggedvalue_t *next = (jl_taggedvalue_t*)pg->data;
    if (fl == NULL) {
        next->next = NULL;
    }
    else {
        // Insert free page after first page.
        // This prevents unnecessary fragmentation from multiple pages
        // being allocated from at the same time. Instead, objects will
        // only ever be allocated from the first object in the list.
        // This is specifically being relied on by the implementation
        // of jl_gc_internal_obj_base_ptr() so that the function does
        // not have to traverse the entire list.
        jl_taggedvalue_t *flpage = (jl_taggedvalue_t *)gc_page_data(fl);
        next->next = flpage->next;
        flpage->next = beg;
        beg = fl;
    }
    pg->has_young = 0;
    pg->has_marked = 0;
    pg->fl_begin_offset = -1;
    pg->fl_end_offset = -1;
    return beg;
}

// Add a new page to the pool. Discards any pages in `p->newpages` before.
static NOINLINE jl_taggedvalue_t *add_page(jl_gc_pool_t *p) JL_NOTSAFEPOINT
{
    // Do not pass in `ptls` as argument. This slows down the fast path
    // in pool_alloc significantly
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_gc_pagemeta_t *pg = jl_gc_alloc_page();
    pg->osize = p->osize;
    pg->ages = (uint8_t*)malloc_s(GC_PAGE_SZ / 8 / p->osize + 1);
    pg->thread_n = ptls->tid;
    jl_taggedvalue_t *fl = reset_page(p, pg, NULL);
    p->newpages = fl;
    return fl;
}

// Size includes the tag and the tag is not cleared!!
JL_DLLEXPORT jl_value_t *jl_gc_pool_alloc(jl_ptls_t ptls, int pool_offset,
                                          int osize)
{
    // Use the pool offset instead of the pool address as the argument
    // to workaround a llvm bug.
    // Ref https://llvm.org/bugs/show_bug.cgi?id=27190
    jl_gc_pool_t *p = (jl_gc_pool_t*)((char*)ptls + pool_offset);
    assert(ptls->gc_state == 0);
#ifdef MEMDEBUG
    return jl_gc_big_alloc(ptls, osize);
#endif
    maybe_collect(ptls);
    ptls->gc_num.allocd += osize;
    ptls->gc_num.poolalloc++;
    // first try to use the freelist
    jl_taggedvalue_t *v = p->freelist;
    if (v) {
        jl_taggedvalue_t *next = v->next;
        p->freelist = next;
        if (__unlikely(gc_page_data(v) != gc_page_data(next))) {
            // we only update pg's fields when the freelist changes page
            // since pg's metadata is likely not in cache
            jl_gc_pagemeta_t *pg = jl_assume(page_metadata(v));
            assert(pg->osize == p->osize);
            pg->nfree = 0;
            pg->has_young = 1;
        }
        return jl_valueof(v);
    }
    // if the freelist is empty we reuse empty but not freed pages
    v = p->newpages;
    jl_taggedvalue_t *next = (jl_taggedvalue_t*)((char*)v + osize);
    // If there's no pages left or the current page is used up,
    // we need to use the slow path.
    char *cur_page = gc_page_data((char*)v - 1);
    if (__unlikely(!v || cur_page + GC_PAGE_SZ < (char*)next)) {
        if (v) {
            // like the freelist case,
            // but only update the page metadata when it is full
            jl_gc_pagemeta_t *pg = jl_assume(page_metadata((char*)v - 1));
            assert(pg->osize == p->osize);
            pg->nfree = 0;
            pg->has_young = 1;
            v = *(jl_taggedvalue_t**)cur_page;
        }
        // Not an else!!
        if (!v)
            v = add_page(p);
        next = (jl_taggedvalue_t*)((char*)v + osize);
    }
    p->newpages = next;
    return jl_valueof(v);
}

int jl_gc_classify_pools(size_t sz, int *osize)
{
    if (sz > GC_MAX_SZCLASS)
        return -1;
    size_t allocsz = sz + sizeof(jl_taggedvalue_t);
    int klass = jl_gc_szclass(allocsz);
    *osize = jl_gc_sizeclasses[klass];
    return (int)(intptr_t)(&((jl_ptls_t)0)->heap.norm_pools[klass]);
}

// sweep phase

int64_t lazy_freed_pages = 0;

// Returns pointer to terminal pointer of list rooted at *pfl.
static jl_taggedvalue_t **sweep_page(jl_gc_pool_t *p, jl_gc_pagemeta_t *pg, jl_taggedvalue_t **pfl, int sweep_full, int osize) JL_NOTSAFEPOINT
{
    char *data = pg->data;
    uint8_t *ages = pg->ages;
    jl_taggedvalue_t *v = (jl_taggedvalue_t*)(data + GC_PAGE_OFFSET);
    char *lim = (char*)v + GC_PAGE_SZ - GC_PAGE_OFFSET - osize;
    size_t old_nfree = pg->nfree;
    size_t nfree;

    int freedall = 1;
    int pg_skpd = 1;
    if (!pg->has_marked) {
        // lazy version: (empty) if the whole page was already unused, free it (return it to the pool)
        // eager version: (freedall) free page as soon as possible
        // the eager one uses less memory.
        // FIXME - need to do accounting on a per-thread basis
        // on quick sweeps, keep a few pages empty but allocated for performance
        if (!sweep_full && lazy_freed_pages <= default_collect_interval / GC_PAGE_SZ) {
            jl_taggedvalue_t *begin = reset_page(p, pg, p->newpages);
            p->newpages = begin;
            begin->next = (jl_taggedvalue_t*)0;
            lazy_freed_pages++;
        }
        else {
            jl_gc_free_page(data);
        }
        nfree = (GC_PAGE_SZ - GC_PAGE_OFFSET) / osize;
        goto done;
    }
    // For quick sweep, we might be able to skip the page if the page doesn't
    // have any young live cell before marking.
    if (!sweep_full && !pg->has_young) {
        assert(!prev_sweep_full || pg->prev_nold >= pg->nold);
        if (!prev_sweep_full || pg->prev_nold == pg->nold) {
            // the position of the freelist begin/end in this page
            // is stored in its metadata
            if (pg->fl_begin_offset != (uint16_t)-1) {
                *pfl = page_pfl_beg(pg);
                pfl = (jl_taggedvalue_t**)page_pfl_end(pg);
            }
            freedall = 0;
            nfree = pg->nfree;
            goto done;
        }
    }

    pg_skpd = 0;
    {  // scope to avoid clang goto errors
        int has_marked = 0;
        int has_young = 0;
        int16_t prev_nold = 0;
        int pg_nfree = 0;
        jl_taggedvalue_t **pfl_begin = NULL;
        uint8_t msk = 1; // mask for the age bit in the current age byte
        while ((char*)v <= lim) {
            int bits = v->bits.gc;
            if (!gc_marked(bits)) {
                *pfl = v;
                pfl = &v->next;
                pfl_begin = pfl_begin ? pfl_begin : pfl;
                pg_nfree++;
                *ages &= ~msk;
            }
            else { // marked young or old
                if (*ages & msk || bits == GC_OLD_MARKED) { // old enough
                    // `!age && bits == GC_OLD_MARKED` is possible for
                    // non-first-class objects like `jl_binding_t`
                    if (sweep_full || bits == GC_MARKED) {
                        bits = v->bits.gc = GC_OLD; // promote
                    }
                    prev_nold++;
                }
                else {
                    assert(bits == GC_MARKED);
                    bits = v->bits.gc = GC_CLEAN; // unmark
                    has_young = 1;
                }
                has_marked |= gc_marked(bits);
                *ages |= msk;
                freedall = 0;
            }
            v = (jl_taggedvalue_t*)((char*)v + osize);
            msk <<= 1;
            if (!msk) {
                msk = 1;
                ages++;
            }
        }

        assert(!freedall);
        pg->has_marked = has_marked;
        pg->has_young = has_young;
        if (pfl_begin) {
            pg->fl_begin_offset = (char*)pfl_begin - data;
            pg->fl_end_offset = (char*)pfl - data;
        }
        else {
            pg->fl_begin_offset = -1;
            pg->fl_end_offset = -1;
        }

        pg->nfree = pg_nfree;
        if (sweep_full) {
            pg->nold = 0;
            pg->prev_nold = prev_nold;
        }
    }
    nfree = pg->nfree;

done:
    gc_time_count_page(freedall, pg_skpd);
    gc_num.freed += (nfree - old_nfree) * osize;
    return pfl;
}

// the actual sweeping over all allocated pages in a memory pool
static inline void sweep_pool_page(jl_taggedvalue_t ***pfl, jl_gc_pagemeta_t *pg, int sweep_full) JL_NOTSAFEPOINT
{
    int p_n = pg->pool_n;
    int t_n = pg->thread_n;
    jl_ptls_t ptls2 = jl_all_tls_states[t_n];
    jl_gc_pool_t *p = &ptls2->heap.norm_pools[p_n];
    int osize = pg->osize;
    pfl[t_n * JL_GC_N_POOLS + p_n] = sweep_page(p, pg, pfl[t_n * JL_GC_N_POOLS + p_n], sweep_full, osize);
}

// sweep over a pagetable0 for all allocated pages
static inline int sweep_pool_pagetable0(jl_taggedvalue_t ***pfl, pagetable0_t *pagetable0, int sweep_full) JL_NOTSAFEPOINT
{
    unsigned ub = 0;
    unsigned alloc = 0;
    for (unsigned pg_i = 0; pg_i <= pagetable0->ub; pg_i++) {
        uint32_t line = pagetable0->allocmap[pg_i];
        unsigned j;
        if (!line)
            continue;
        ub = pg_i;
        alloc = 1;
        for (j = 0; line; j++, line >>= 1) {
            unsigned next = ffs_u32(line);
            j += next;
            line >>= next;
            jl_gc_pagemeta_t *pg = pagetable0->meta[pg_i * 32 + j];
            sweep_pool_page(pfl, pg, sweep_full);
        }
    }
    pagetable0->ub = ub;
    return alloc;
}

// sweep over pagetable1 for all pagetable0 that may contain allocated pages
static inline int sweep_pool_pagetable1(jl_taggedvalue_t ***pfl, pagetable1_t *pagetable1, int sweep_full) JL_NOTSAFEPOINT
{
    unsigned ub = 0;
    unsigned alloc = 0;
    for (unsigned pg_i = 0; pg_i <= pagetable1->ub; pg_i++) {
        uint32_t line = pagetable1->allocmap0[pg_i];
        unsigned j;
        for (j = 0; line; j++, line >>= 1) {
            unsigned next = ffs_u32(line);
            j += next;
            line >>= next;
            pagetable0_t *pagetable0 = pagetable1->meta0[pg_i * 32 + j];
            if (pagetable0 && !sweep_pool_pagetable0(pfl, pagetable0, sweep_full))
                pagetable1->allocmap0[pg_i] &= ~(1 << j); // no allocations found, remember that for next time
        }
        if (pagetable1->allocmap0[pg_i]) {
            ub = pg_i;
            alloc = 1;
        }
    }
    pagetable1->ub = ub;
    return alloc;
}

// sweep over all memory for all pagetable1 that may contain allocated pages
static void sweep_pool_pagetable(jl_taggedvalue_t ***pfl, int sweep_full) JL_NOTSAFEPOINT
{
    if (REGION2_PG_COUNT == 1) { // compile-time optimization
        pagetable1_t *pagetable1 = memory_map.meta1[0];
        if (pagetable1)
            sweep_pool_pagetable1(pfl, pagetable1, sweep_full);
        return;
    }
    unsigned ub = 0;
    for (unsigned pg_i = 0; pg_i <= memory_map.ub; pg_i++) {
        uint32_t line = memory_map.allocmap1[pg_i];
        unsigned j;
        for (j = 0; line; j++, line >>= 1) {
            unsigned next = ffs_u32(line);
            j += next;
            line >>= next;
            pagetable1_t *pagetable1 = memory_map.meta1[pg_i * 32 + j];
            if (pagetable1 && !sweep_pool_pagetable1(pfl, pagetable1, sweep_full))
                memory_map.allocmap1[pg_i] &= ~(1 << j); // no allocations found, remember that for next time
        }
        if (memory_map.allocmap1[pg_i]) {
            ub = pg_i;
        }
    }
    memory_map.ub = ub;
}

// sweep over all memory that is being used and not in a pool
static void gc_sweep_other(jl_ptls_t ptls, int sweep_full) JL_NOTSAFEPOINT
{
    sweep_malloced_arrays();
    sweep_big(ptls, sweep_full);
}

static void gc_pool_sync_nfree(jl_gc_pagemeta_t *pg, jl_taggedvalue_t *last) JL_NOTSAFEPOINT
{
    assert(pg->fl_begin_offset != (uint16_t)-1);
    char *cur_pg = gc_page_data(last);
    // Fast path for page that has no allocation
    jl_taggedvalue_t *fl_beg = (jl_taggedvalue_t*)(cur_pg + pg->fl_begin_offset);
    if (last == fl_beg)
        return;
    int nfree = 0;
    do {
        nfree++;
        last = last->next;
    } while (gc_page_data(last) == cur_pg);
    pg->nfree = nfree;
}

// setup the data-structures for a sweep over all memory pools
static void gc_sweep_pool(int sweep_full)
{
    gc_time_pool_start();
    lazy_freed_pages = 0;

    // For the benfit of the analyzer, which doesn't know that jl_n_threads
    // doesn't change over the course of this function
    size_t n_threads = jl_n_threads;

    // allocate enough space to hold the end of the free list chain
    // for every thread and pool size
    jl_taggedvalue_t ***pfl = (jl_taggedvalue_t ***) alloca(n_threads * JL_GC_N_POOLS * sizeof(jl_taggedvalue_t**));

    // update metadata of pages that were pointed to by freelist or newpages from a pool
    // i.e. pages being the current allocation target
    for (int t_i = 0; t_i < n_threads; t_i++) {
        jl_ptls_t ptls2 = jl_all_tls_states[t_i];
        for (int i = 0; i < JL_GC_N_POOLS; i++) {
            jl_gc_pool_t *p = &ptls2->heap.norm_pools[i];
            jl_taggedvalue_t *last = p->freelist;
            if (last) {
                jl_gc_pagemeta_t *pg = jl_assume(page_metadata(last));
                gc_pool_sync_nfree(pg, last);
                pg->has_young = 1;
            }
            p->freelist =  NULL;
            pfl[t_i * JL_GC_N_POOLS + i] = &p->freelist;

            last = p->newpages;
            if (last) {
                char *last_p = (char*)last;
                jl_gc_pagemeta_t *pg = jl_assume(page_metadata(last_p - 1));
                assert(last_p - gc_page_data(last_p - 1) >= GC_PAGE_OFFSET);
                pg->nfree = (GC_PAGE_SZ - (last_p - gc_page_data(last_p - 1))) / p->osize;
                pg->has_young = 1;
            }
            p->newpages = NULL;
        }
    }

    // the actual sweeping
    sweep_pool_pagetable(pfl, sweep_full);

    // null out terminal pointers of free lists
    for (int t_i = 0; t_i < n_threads; t_i++) {
        for (int i = 0; i < JL_GC_N_POOLS; i++) {
            *pfl[t_i * JL_GC_N_POOLS + i] = NULL;
        }
    }

    gc_time_pool_end(sweep_full);
}

static void gc_sweep_perm_alloc(void)
{
    uint64_t t0 = jl_hrtime();
    gc_sweep_sysimg();
    gc_time_sysimg_end(t0);
}

// mark phase

JL_DLLEXPORT void jl_gc_queue_root(jl_value_t *ptr)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_taggedvalue_t *o = jl_astaggedvalue(ptr);
    // The modification of the `gc_bits` is not atomic but it
    // should be safe here since GC is not allowed to run here and we only
    // write GC_OLD to the GC bits outside GC. This could cause
    // duplicated objects in the remset but that shouldn't be a problem.
    o->bits.gc = GC_MARKED;
    arraylist_push(ptls->heap.remset, ptr);
    ptls->heap.remset_nptr++; // conservative
}

void jl_gc_queue_multiroot(jl_value_t *parent, jl_value_t *ptr) JL_NOTSAFEPOINT
{
    // first check if this is really necessary
    // TODO: should we store this info in one of the extra gc bits?
    jl_datatype_t *dt = (jl_datatype_t*)jl_typeof(ptr);
    const jl_datatype_layout_t *ly = dt->layout;
    uint32_t npointers = ly->npointers;
    //if (npointers == 0) // this was checked by the caller
    //    return;
    jl_value_t *ptrf = ((jl_value_t**)ptr)[ly->first_ptr];
    if (ptrf && (jl_astaggedvalue(ptrf)->bits.gc & 1) == 0) {
        // this pointer was young, move the barrier back now
        jl_gc_wb_back(parent);
        return;
    }
    const uint8_t *ptrs8 = (const uint8_t *)jl_dt_layout_ptrs(ly);
    const uint16_t *ptrs16 = (const uint16_t *)jl_dt_layout_ptrs(ly);
    const uint32_t *ptrs32 = (const uint32_t*)jl_dt_layout_ptrs(ly);
    for (size_t i = 1; i < npointers; i++) {
        uint32_t fld;
        if (ly->fielddesc_type == 0) {
            fld = ptrs8[i];
        }
        else if (ly->fielddesc_type == 1) {
            fld = ptrs16[i];
        }
        else {
            assert(ly->fielddesc_type == 2);
            fld = ptrs32[i];
        }
        jl_value_t *ptrf = ((jl_value_t**)ptr)[fld];
        if (ptrf && (jl_astaggedvalue(ptrf)->bits.gc & 1) == 0) {
            // this pointer was young, move the barrier back now
            jl_gc_wb_back(parent);
            return;
        }
    }
}

void gc_queue_binding(jl_binding_t *bnd)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_taggedvalue_t *buf = jl_astaggedvalue(bnd);
    buf->bits.gc = GC_MARKED;
    arraylist_push(&ptls->heap.rem_bindings, bnd);
}


#ifdef JL_DEBUG_BUILD
static void *volatile gc_findval; // for usage from gdb, for finding the gc-root for a value
#endif

static void *sysimg_base;
static void *sysimg_end;
void jl_gc_set_permalloc_region(void *start, void *end)
{
    sysimg_base = start;
    sysimg_end = end;
}


// Handle the case where the stack is only partially copied.
STATIC_INLINE uintptr_t gc_get_stack_addr(void *_addr, uintptr_t offset,
                                          uintptr_t lb, uintptr_t ub)
{
    uintptr_t addr = (uintptr_t)_addr;
    if (addr >= lb && addr < ub)
        return addr + offset;
    return addr;
}

STATIC_INLINE uintptr_t gc_read_stack(void *_addr, uintptr_t offset,
                                      uintptr_t lb, uintptr_t ub)
{
    uintptr_t real_addr = gc_get_stack_addr(_addr, offset, lb, ub);
    return *(uintptr_t*)real_addr;
}

JL_NORETURN NOINLINE void gc_assert_datatype_fail(jl_ptls_t ptls, jl_datatype_t *vt,
                                                  jl_gc_mark_sp_t sp)
{
    jl_printf(JL_STDOUT, "GC error (probable corruption) :\n");
    gc_debug_print_status();
    jl_(vt);
    gc_debug_critical_error();
    gc_mark_loop_unwind(ptls, sp, 0);
    abort();
}

// This stores the label address in the mark loop function.
// We can't directly store that to a global array so we need some hack to get that.
// See the call to `gc_mark_loop` in init with a `NULL` `ptls`.
void *gc_mark_label_addrs[_GC_MARK_L_MAX];

// Double the mark stack (both pc and data) with the lock held.
static void NOINLINE gc_mark_stack_resize(jl_gc_mark_cache_t *gc_cache, jl_gc_mark_sp_t *sp) JL_NOTSAFEPOINT
{
    jl_gc_mark_data_t *old_data = gc_cache->data_stack;
    void **pc_stack = sp->pc_start;
    size_t stack_size = (char*)sp->pc_end - (char*)pc_stack;
    JL_LOCK_NOGC(&gc_cache->stack_lock);
    gc_cache->data_stack = (jl_gc_mark_data_t *)realloc_s(old_data, stack_size * 2 * sizeof(jl_gc_mark_data_t));
    sp->data = (jl_gc_mark_data_t *)(((char*)sp->data) + (((char*)gc_cache->data_stack) - ((char*)old_data)));

    sp->pc_start = gc_cache->pc_stack = (void**)realloc_s(pc_stack, stack_size * 2 * sizeof(void*));
    gc_cache->pc_stack_end = sp->pc_end = sp->pc_start + stack_size * 2;
    sp->pc += sp->pc_start - pc_stack;
    JL_UNLOCK_NOGC(&gc_cache->stack_lock);
}

// Push a work item to the stack. The type of the work item is marked with `pc`.
// The data needed is in `data` and is of size `data_size`.
// If there isn't enough space on the stack, the stack will be resized with the stack
// lock held. The caller should invalidate any local cache of the stack addresses that's not
// in `gc_cache` or `sp`
// The `sp` will be updated on return if `inc` is true.
STATIC_INLINE void gc_mark_stack_push(jl_gc_mark_cache_t *gc_cache, jl_gc_mark_sp_t *sp,
                                      void *pc, void *data, size_t data_size, int inc) JL_NOTSAFEPOINT
{
    assert(data_size <= sizeof(jl_gc_mark_data_t));
    if (__unlikely(sp->pc == sp->pc_end))
        gc_mark_stack_resize(gc_cache, sp);
    *sp->pc = pc;
    memcpy(sp->data, data, data_size);
    if (inc) {
        sp->data = (jl_gc_mark_data_t *)(((char*)sp->data) + data_size);
        sp->pc++;
    }
}

// Check if the reference is non-NULL and atomically set the mark bit.
// Update `*nptr`, which is the `nptr` field of the parent item, if the object is young.
// Return the tag (with GC bits cleared) and the GC bits in `*ptag` and `*pbits`.
// Return whether the object needs to be scanned / have metadata updated.
STATIC_INLINE int gc_try_setmark(jl_value_t *obj, uintptr_t *nptr,
                                 uintptr_t *ptag, uint8_t *pbits) JL_NOTSAFEPOINT
{
    if (!obj)
        return 0;
    jl_taggedvalue_t *o = jl_astaggedvalue(obj);
    uintptr_t tag = o->header;
    if (!gc_marked(tag)) {
        uint8_t bits;
        int res = gc_setmark_tag(o, GC_MARKED, tag, &bits);
        if (!gc_old(bits))
            *nptr = *nptr | 1;
        *ptag = tag & ~(uintptr_t)0xf;
        *pbits = bits;
        return __likely(res);
    }
    else if (!gc_old(tag)) {
        *nptr = *nptr | 1;
    }
    return 0;
}

// Queue a finalizer list to be scanned in the mark loop. Start marking from index `start`.
void gc_mark_queue_finlist(jl_gc_mark_cache_t *gc_cache, jl_gc_mark_sp_t *sp,
                           arraylist_t *list, size_t start)
{
    size_t len = list->len;
    if (len <= start)
        return;
    jl_value_t **items = (jl_value_t**)list->items;
    gc_mark_finlist_t markdata = {items + start, items + len};
    gc_mark_stack_push(gc_cache, sp, gc_mark_label_addrs[GC_MARK_L_finlist],
                       &markdata, sizeof(markdata), 1);
}

// Queue a object to be scanned. The object should already be marked and the GC metadata
// should already be updated for it. Only scanning of the object should be performed.
STATIC_INLINE void gc_mark_queue_scan_obj(jl_gc_mark_cache_t *gc_cache, jl_gc_mark_sp_t *sp,
                                          jl_value_t *obj)
{
    jl_taggedvalue_t *o = jl_astaggedvalue(obj);
    uintptr_t tag = o->header;
    uint8_t bits = tag & 0xf;
    tag = tag & ~(uintptr_t)0xf;
    gc_mark_marked_obj_t data = {obj, tag, bits};
    gc_mark_stack_push(gc_cache, sp, gc_mark_label_addrs[GC_MARK_L_scan_only],
                       &data, sizeof(data), 1);
}

// Mark and queue a object to be scanned.
// The object will be marked atomically which can also happen concurrently.
// It will be queued if the object wasn't marked already (or concurrently by another thread)
// Returns whether the object is young.
STATIC_INLINE int gc_mark_queue_obj(jl_gc_mark_cache_t *gc_cache, jl_gc_mark_sp_t *sp, void *_obj) JL_NOTSAFEPOINT
{
    jl_value_t *obj = (jl_value_t*)jl_assume(_obj);
    uintptr_t nptr = 0;
    uintptr_t tag = 0;
    uint8_t bits = 0;
    if (!gc_try_setmark(obj, &nptr, &tag, &bits))
        return (int)nptr;
    gc_mark_marked_obj_t data = {obj, tag, bits};
    gc_mark_stack_push(gc_cache, sp, gc_mark_label_addrs[GC_MARK_L_marked_obj],
                       &data, sizeof(data), 1);
    return (int)nptr;
}

int jl_gc_mark_queue_obj_explicit(jl_gc_mark_cache_t *gc_cache, jl_gc_mark_sp_t *sp, jl_value_t *obj)
{
    return gc_mark_queue_obj(gc_cache, sp, obj);
}

JL_DLLEXPORT int jl_gc_mark_queue_obj(jl_ptls_t ptls, jl_value_t *obj)
{
    return gc_mark_queue_obj(&ptls->gc_cache, &ptls->gc_mark_sp, obj);
}

JL_DLLEXPORT void jl_gc_mark_queue_objarray(jl_ptls_t ptls, jl_value_t *parent,
                                            jl_value_t **objs, size_t nobjs)
{
    gc_mark_objarray_t data = { parent, objs, objs + nobjs, 1,
                                jl_astaggedvalue(parent)->bits.gc & 2 };
    gc_mark_stack_push(&ptls->gc_cache, &ptls->gc_mark_sp,
                       gc_mark_label_addrs[GC_MARK_L_objarray],
                       &data, sizeof(data), 1);
}


// Check if `nptr` is tagged for `old + refyoung`,
// Push the object to the remset and update the `nptr` counter if necessary.
STATIC_INLINE void gc_mark_push_remset(jl_ptls_t ptls, jl_value_t *obj, uintptr_t nptr) JL_NOTSAFEPOINT
{
    if (__unlikely((nptr & 0x3) == 0x3)) {
        ptls->heap.remset_nptr += nptr >> 2;
        arraylist_t *remset = ptls->heap.remset;
        size_t len = remset->len;
        if (__unlikely(len >= remset->max)) {
            arraylist_push(remset, obj);
        }
        else {
            remset->len = len + 1;
            remset->items[len] = obj;
        }
    }
}

// Scan a dense array of object references, see `gc_mark_objarray_t`
STATIC_INLINE int gc_mark_scan_objarray(jl_ptls_t ptls, jl_gc_mark_sp_t *sp,
                                        gc_mark_objarray_t *objary,
                                        jl_value_t **begin, jl_value_t **end,
                                        jl_value_t **pnew_obj, uintptr_t *ptag, uint8_t *pbits)
{
    (void)jl_assume(objary == (gc_mark_objarray_t*)sp->data);
    for (; begin < end; begin += objary->step) {
        *pnew_obj = *begin;
        if (*pnew_obj)
            verify_parent2("obj array", objary->parent, begin, "elem(%d)",
                           gc_slot_to_arrayidx(objary->parent, begin));
        if (!gc_try_setmark(*pnew_obj, &objary->nptr, ptag, pbits))
            continue;
        begin += objary->step;
        // Found an object to mark
        if (begin < end) {
            // Haven't done with this one yet. Update the content and push it back
            objary->begin = begin;
            gc_repush_markdata(sp, gc_mark_objarray_t);
        }
        else {
            // Finished scanning this one, finish up by checking the GC invariance
            // and let the next item replacing the current one directly.
            gc_mark_push_remset(ptls, objary->parent, objary->nptr);
        }
        return 1;
    }
    gc_mark_push_remset(ptls, objary->parent, objary->nptr);
    return 0;
}

// Scan a sparse array of object references, see `gc_mark_objarray_t`
STATIC_INLINE int gc_mark_scan_array8(jl_ptls_t ptls, jl_gc_mark_sp_t *sp,
                                      gc_mark_array8_t *ary8,
                                      jl_value_t **begin, jl_value_t **end,
                                      uint8_t *elem_begin, uint8_t *elem_end,
                                      jl_value_t **pnew_obj, uintptr_t *ptag, uint8_t *pbits)
{
    (void)jl_assume(ary8 == (gc_mark_array8_t*)sp->data);
    size_t elsize = ((jl_array_t*)ary8->elem.parent)->elsize / sizeof(jl_value_t*);
    for (; begin < end; begin += elsize) {
        for (; elem_begin < elem_end; elem_begin++) {
            jl_value_t **slot = &begin[*elem_begin];
            *pnew_obj = *slot;
            if (*pnew_obj)
                verify_parent2("array", ary8->elem.parent, slot, "elem(%d)",
                               gc_slot_to_arrayidx(ary8->elem.parent, begin));
            if (!gc_try_setmark(*pnew_obj, &ary8->elem.nptr, ptag, pbits))
                continue;
            elem_begin++;
            // Found an object to mark
            if (elem_begin < elem_end) {
                // Haven't done with this one yet. Update the content and push it back
                ary8->elem.begin = elem_begin;
                ary8->begin = begin;
                gc_repush_markdata(sp, gc_mark_array8_t);
            }
            else {
                begin += elsize;
                if (begin < end) {
                    // Haven't done with this array yet. Reset the content and push it back
                    ary8->elem.begin = ary8->rebegin;
                    ary8->begin = begin;
                    gc_repush_markdata(sp, gc_mark_array8_t);
                }
                else {
                    // Finished scanning this one, finish up by checking the GC invariance
                    // and let the next item replacing the current one directly.
                    gc_mark_push_remset(ptls, ary8->elem.parent, ary8->elem.nptr);
                }
            }
            return 1;
        }
        elem_begin = ary8->rebegin;
    }
    gc_mark_push_remset(ptls, ary8->elem.parent, ary8->elem.nptr);
    return 0;
}


// Scan an object with 8bits field descriptors. see `gc_mark_obj8_t`
STATIC_INLINE int gc_mark_scan_obj8(jl_ptls_t ptls, jl_gc_mark_sp_t *sp, gc_mark_obj8_t *obj8,
                                    char *parent, uint8_t *begin, uint8_t *end,
                                    jl_value_t **pnew_obj, uintptr_t *ptag, uint8_t *pbits)
{
    (void)jl_assume(obj8 == (gc_mark_obj8_t*)sp->data);
    (void)jl_assume(begin < end);
    for (; begin < end; begin++) {
        jl_value_t **slot = &((jl_value_t**)parent)[*begin];
        *pnew_obj = *slot;
        if (*pnew_obj)
            verify_parent2("object", parent, slot, "field(%d)",
                           gc_slot_to_fieldidx(parent, slot));
        if (!gc_try_setmark(*pnew_obj, &obj8->nptr, ptag, pbits))
            continue;
        begin++;
        // Found an object to mark
        if (begin < end) {
            // Haven't done with this one yet. Update the content and push it back
            obj8->begin = begin;
            gc_repush_markdata(sp, gc_mark_obj8_t);
        }
        else {
            // Finished scanning this one, finish up by checking the GC invariance
            // and let the next item replacing the current one directly.
            gc_mark_push_remset(ptls, obj8->parent, obj8->nptr);
        }
        return 1;
    }
    gc_mark_push_remset(ptls, obj8->parent, obj8->nptr);
    return 0;
}

// Scan an object with 16bits field descriptors. see `gc_mark_obj16_t`
STATIC_INLINE int gc_mark_scan_obj16(jl_ptls_t ptls, jl_gc_mark_sp_t *sp, gc_mark_obj16_t *obj16,
                                     char *parent, uint16_t *begin, uint16_t *end,
                                     jl_value_t **pnew_obj, uintptr_t *ptag, uint8_t *pbits) JL_NOTSAFEPOINT
{
    (void)jl_assume(obj16 == (gc_mark_obj16_t*)sp->data);
    (void)jl_assume(begin < end);
    for (; begin < end; begin++) {
        jl_value_t **slot = &((jl_value_t**)parent)[*begin];
        *pnew_obj = *slot;
        if (*pnew_obj)
            verify_parent2("object", parent, slot, "field(%d)",
                           gc_slot_to_fieldidx(parent, slot));
        if (!gc_try_setmark(*pnew_obj, &obj16->nptr, ptag, pbits))
            continue;
        begin++;
        // Found an object to mark
        if (begin < end) {
            // Haven't done with this one yet. Update the content and push it back
            obj16->begin = begin;
            gc_repush_markdata(sp, gc_mark_obj16_t);
        }
        else {
            // Finished scanning this one, finish up by checking the GC invariance
            // and let the next item replacing the current one directly.
            gc_mark_push_remset(ptls, obj16->parent, obj16->nptr);
        }
        return 1;
    }
    gc_mark_push_remset(ptls, obj16->parent, obj16->nptr);
    return 0;
}

// Scan an object with 32bits field descriptors. see `gc_mark_obj32_t`
STATIC_INLINE int gc_mark_scan_obj32(jl_ptls_t ptls, jl_gc_mark_sp_t *sp, gc_mark_obj32_t *obj32,
                                     char *parent, uint32_t *begin, uint32_t *end,
                                     jl_value_t **pnew_obj, uintptr_t *ptag, uint8_t *pbits)
{
    (void)jl_assume(obj32 == (gc_mark_obj32_t*)sp->data);
    (void)jl_assume(begin < end);
    for (; begin < end; begin++) {
        jl_value_t **slot = &((jl_value_t**)parent)[*begin];
        *pnew_obj = *slot;
        if (*pnew_obj)
            verify_parent2("object", parent, slot, "field(%d)",
                           gc_slot_to_fieldidx(parent, slot));
        if (!gc_try_setmark(*pnew_obj, &obj32->nptr, ptag, pbits))
            continue;
        begin++;
        // Found an object to mark
        if (begin < end) {
            // Haven't done with this one yet. Update the content and push it back
            obj32->begin = begin;
            gc_repush_markdata(sp, gc_mark_obj32_t);
        }
        else {
            // Finished scanning this one, finish up by checking the GC invariance
            // and let the next item replacing the current one directly.
            gc_mark_push_remset(ptls, obj32->parent, obj32->nptr);
        }
        return 1;
    }
    gc_mark_push_remset(ptls, obj32->parent, obj32->nptr);
    return 0;
}

#if defined(__GNUC__) && !defined(_OS_EMSCRIPTEN_)
#  define gc_mark_laddr(name) (&&name)
#  define gc_mark_jmp(ptr) goto *(ptr)
#else
#define gc_mark_laddr(name) ((void*)(uintptr_t)GC_MARK_L_##name)
#define gc_mark_jmp(ptr) do {                   \
        switch ((int)(uintptr_t)ptr) {          \
        case GC_MARK_L_marked_obj:              \
            goto marked_obj;                    \
        case GC_MARK_L_scan_only:               \
            goto scan_only;                     \
        case GC_MARK_L_finlist:                 \
            goto finlist;                       \
        case GC_MARK_L_objarray:                \
            goto objarray;                      \
        case GC_MARK_L_array8:                  \
            goto array8;                        \
        case GC_MARK_L_obj8:                    \
            goto obj8;                          \
        case GC_MARK_L_obj16:                   \
            goto obj16;                         \
        case GC_MARK_L_obj32:                   \
            goto obj32;                         \
        case GC_MARK_L_stack:                   \
            goto stack;                         \
        case GC_MARK_L_excstack:                \
            goto excstack;                      \
        case GC_MARK_L_module_binding:          \
            goto module_binding;                \
        default:                                \
            abort();                            \
        }                                       \
    } while (0)
#endif

// This is the main marking loop.
// It uses an iterative (mostly) Depth-first search (DFS) to mark all the objects.
// Instead of using the native stack, two stacks are manually maintained,
// one (fixed-size) pc stack which stores the return address and one (variable-size)
// data stack which stores the local variables needed by the scanning code.
// Using a manually maintained stack has a few advantages
//
// 1. We can resize the stack as we go and never worry about stack overflow
//    This is especitally useful when enters the GC in a deep call stack.
//    It also removes the very deep GC call stack in a profile.
// 2. We can minimize the number of local variables to save on the stack.
//    This includes minimizing the sizes of the stack frames and only saving variables
//    that have been changed before making "function calls" (i.e. `goto mark;`)
// 3. We can perform end-of-loop tail-call optimization for common cases.
// 4. The marking can be interrupted more easily since all the states are maintained
//    in a well-defined format already.
//    This will be useful if we want to have incremental marking again.
// 5. The frames can be stolen by another thread more easily and it is not necessary
//    to copy works to be stolen to another queue. Useful for parallel marking.
//    (Will still require synchronization in stack popping of course.)
// 6. A flat function (i.e. no or very few function calls) also give the compiler
//    opportunity to keep more states in registers that doesn't have to be spilled as often.
//
// We use two stacks so that the thief on another thread can steal the fixed sized pc stack
// and use that to figure out the size of the struct on the variable size data stack.
//
// The main disadvantages are that we bypass some stack-based CPU optimizations including the
// stack engine and return address prediction.
// Using two stacks also double the number of operations on the stack pointer
// though we still only need to use one of them (the pc stack pointer) for bounds check.
// In general, it seems that the reduction of stack memory ops and instructions count
// have a larger positive effect on the performance. =)

// As a general guide we do not want to make non-inlined function calls in this function
// if possible since a large number of registers has to be spilled when that happens.
// This is especially true on on X86 which doesn't have many (any?)
// callee saved general purpose registers.
// (OTOH, the spill will likely make use of the stack engine which is otherwise idle so
//  the performance impact is minimum as long as it's not in the hottest path)

// There are three external entry points to the loop, corresponding to label
// `marked_obj`, `scan_only` and `finlist` (see the corresponding functions
// `gc_mark_queue_obj`, `gc_mark_queue_scan_obj` and `gc_mark_queue_finlist` above).
// The scanning of the object starts with `goto mark`, which updates the metadata and scans
// the object whose information is stored in `new_obj`, `tag` and `bits`.
// The branches in `mark` will dispatch the object to one of the scan "loop"s to be scanned
// as either a normal julia object or one of the special objects with specific storage format.
// Each of the scan "loop" will perform a DFS of the object in the following way
//
// 1. When encountering an pointer (julia object reference) slots, load, perform NULL check
//    and atomically set the mark bits to determine if the object needs to be scanned.
// 2. If yes, it'll push itself back onto the mark stack (after updating fields that are changed)
//    using `gc_repush_markdata` to increment the stack pointers.
//    This step can also be replaced by a tail call by finishing up the marking of the current
//    object when the end of the current object is reached.
// 3. Jump to `mark`. The marking of the current object will be resumed after the child is
//    scanned by popping the stack frame back.
//
// Some of the special object scannings use BFS to simplify the code (Task and Module).

// The jumps from the dispatch to the scan "loop"s are done by first pushing a frame
// to the stacks while only increment the data stack pointer before jumping to the loop
// This way the scan "loop" gets exactly what it expects after a stack pop.
// Additional optimizations are done for some of the common cases by skipping
// the unnecessary data stack pointer increment and the load from the stack
// (i.e. store to load forwaring). See `objary_loaded`, `obj8_loaded` and `obj16_loaded`.
JL_EXTENSION NOINLINE void gc_mark_loop(jl_ptls_t ptls, jl_gc_mark_sp_t sp)
{
    if (__unlikely(ptls == NULL)) {
        gc_mark_label_addrs[GC_MARK_L_marked_obj] = gc_mark_laddr(marked_obj);
        gc_mark_label_addrs[GC_MARK_L_scan_only] = gc_mark_laddr(scan_only);
        gc_mark_label_addrs[GC_MARK_L_finlist] = gc_mark_laddr(finlist);
        gc_mark_label_addrs[GC_MARK_L_objarray] = gc_mark_laddr(objarray);
        gc_mark_label_addrs[GC_MARK_L_array8] = gc_mark_laddr(array8);
        gc_mark_label_addrs[GC_MARK_L_obj8] = gc_mark_laddr(obj8);
        gc_mark_label_addrs[GC_MARK_L_obj16] = gc_mark_laddr(obj16);
        gc_mark_label_addrs[GC_MARK_L_obj32] = gc_mark_laddr(obj32);
        gc_mark_label_addrs[GC_MARK_L_stack] = gc_mark_laddr(stack);
        gc_mark_label_addrs[GC_MARK_L_excstack] = gc_mark_laddr(excstack);
        gc_mark_label_addrs[GC_MARK_L_module_binding] = gc_mark_laddr(module_binding);
        return;
    }

    jl_value_t *new_obj = NULL;
    uintptr_t tag = 0;
    uint8_t bits = 0;
    int meta_updated = 0;

    gc_mark_objarray_t *objary;
    jl_value_t **objary_begin;
    jl_value_t **objary_end;

    gc_mark_array8_t *ary8;

    gc_mark_obj8_t *obj8;
    char *obj8_parent;
    uint8_t *obj8_begin;
    uint8_t *obj8_end;

    gc_mark_obj16_t *obj16;
    char *obj16_parent;
    uint16_t *obj16_begin;
    uint16_t *obj16_end;

pop:
    if (sp.pc == sp.pc_start) {
        // TODO: stealing form another thread
        return;
    }
    sp.pc--;
    gc_mark_jmp(*sp.pc); // computed goto

marked_obj: {
        // An object that has been marked and needs have metadata updated and scanned.
        gc_mark_marked_obj_t *obj = gc_pop_markdata(&sp, gc_mark_marked_obj_t);
        new_obj = obj->obj;
        tag = obj->tag;
        bits = obj->bits;
        goto mark;
    }

scan_only: {
        // An object that has been marked and needs to be scanned.
        gc_mark_marked_obj_t *obj = gc_pop_markdata(&sp, gc_mark_marked_obj_t);
        new_obj = obj->obj;
        tag = obj->tag;
        bits = obj->bits;
        meta_updated = 1;
        goto mark;
    }

objarray:
    objary = gc_pop_markdata(&sp, gc_mark_objarray_t);
    objary_begin = objary->begin;
    objary_end = objary->end;
objarray_loaded:
    if (gc_mark_scan_objarray(ptls, &sp, objary, objary_begin, objary_end,
                              &new_obj, &tag, &bits))
        goto mark;
    goto pop;

array8:
    ary8 = gc_pop_markdata(&sp, gc_mark_array8_t);
    objary_begin = ary8->begin;
    objary_end = ary8->end;
    obj8_begin = ary8->elem.begin;
    obj8_end = ary8->elem.end;
array8_loaded:
    if (gc_mark_scan_array8(ptls, &sp, ary8, objary_begin, objary_end, obj8_begin, obj8_end,
                            &new_obj, &tag, &bits))
        goto mark;
    goto pop;


obj8:
    obj8 = gc_pop_markdata(&sp, gc_mark_obj8_t);
    obj8_parent = (char*)obj8->parent;
    obj8_begin = obj8->begin;
    obj8_end = obj8->end;
obj8_loaded:
    if (gc_mark_scan_obj8(ptls, &sp, obj8, obj8_parent, obj8_begin, obj8_end,
                          &new_obj, &tag, &bits))
        goto mark;
    goto pop;

obj16:
    obj16 = gc_pop_markdata(&sp, gc_mark_obj16_t);
    obj16_parent = (char*)obj16->parent;
    obj16_begin = obj16->begin;
    obj16_end = obj16->end;
obj16_loaded:
    if (gc_mark_scan_obj16(ptls, &sp, obj16, obj16_parent, obj16_begin, obj16_end,
                           &new_obj, &tag, &bits))
        goto mark;
    goto pop;

obj32: {
        gc_mark_obj32_t *obj32 = gc_pop_markdata(&sp, gc_mark_obj32_t);
        char *parent = (char*)obj32->parent;
        uint32_t *begin = obj32->begin;
        uint32_t *end = obj32->end;
        if (gc_mark_scan_obj32(ptls, &sp, obj32, parent, begin, end, &new_obj, &tag, &bits))
            goto mark;
        goto pop;
    }

stack: {
        // Scan the stack. see `gc_mark_stackframe_t`
        // The task object this stack belongs to is being scanned separately as a normal
        // 8bit field descriptor object.
        gc_mark_stackframe_t *stack = gc_pop_markdata(&sp, gc_mark_stackframe_t);
        jl_gcframe_t *s = stack->s;
        uint32_t i = stack->i;
        uint32_t nroots = stack->nroots;
        uintptr_t offset = stack->offset;
        uintptr_t lb = stack->lb;
        uintptr_t ub = stack->ub;
        uint32_t nr = nroots >> 2;
        uintptr_t nptr = 0;
        while (1) {
            jl_value_t ***rts = (jl_value_t***)(((void**)s) + 2);
            for (; i < nr; i++) {
                if (nroots & 1) {
                    void **slot = (void**)gc_read_stack(&rts[i], offset, lb, ub);
                    new_obj = (jl_value_t*)gc_read_stack(slot, offset, lb, ub);
                }
                else {
                    new_obj = (jl_value_t*)gc_read_stack(&rts[i], offset, lb, ub);
                    if (gc_ptr_tag(new_obj, 1)) {
                        // handle tagged pointers in finalizer list
                        new_obj = gc_ptr_clear_tag(new_obj, 1);
                        i++;
                    }
                }
                if (!gc_try_setmark(new_obj, &nptr, &tag, &bits))
                    continue;
                i++;
                if (i < nr) {
                    // Haven't done with this one yet. Update the content and push it back
                    stack->i = i;
                    gc_repush_markdata(&sp, gc_mark_stackframe_t);
                }
                else if ((s = (jl_gcframe_t*)gc_read_stack(&s->prev, offset, lb, ub))) {
                    stack->s = s;
                    stack->i = 0;
                    uintptr_t new_nroots = gc_read_stack(&s->nroots, offset, lb, ub);
                    assert(new_nroots <= UINT32_MAX);
                    stack->nroots = (uint32_t)new_nroots;
                    gc_repush_markdata(&sp, gc_mark_stackframe_t);
                }
                goto mark;
            }
            s = (jl_gcframe_t*)gc_read_stack(&s->prev, offset, lb, ub);
            if (s != 0) {
                stack->s = s;
                i = 0;
                uintptr_t new_nroots = gc_read_stack(&s->nroots, offset, lb, ub);
                assert(new_nroots <= UINT32_MAX);
                nroots = stack->nroots = (uint32_t)new_nroots;
                nr = nroots >> 2;
                continue;
            }
            goto pop;
        }
    }

excstack: {
        // Scan an exception stack
        gc_mark_excstack_t *stackitr = gc_pop_markdata(&sp, gc_mark_excstack_t);
        jl_excstack_t *excstack = stackitr->s;
        size_t itr = stackitr->itr;
        size_t bt_index = stackitr->bt_index;
        size_t jlval_index = stackitr->jlval_index;
        while (itr > 0) {
            size_t bt_size = jl_excstack_bt_size(excstack, itr);
            jl_bt_element_t *bt_data = jl_excstack_bt_data(excstack, itr);
            for (; bt_index < bt_size; bt_index += jl_bt_entry_size(bt_data + bt_index)) {
                jl_bt_element_t *bt_entry = bt_data + bt_index;
                if (jl_bt_is_native(bt_entry))
                    continue;
                // Found an extended backtrace entry: iterate over any
                // GC-managed values inside.
                size_t njlvals = jl_bt_num_jlvals(bt_entry);
                while (jlval_index < njlvals) {
                    new_obj = jl_bt_entry_jlvalue(bt_entry, jlval_index);
                    uintptr_t nptr = 0;
                    jlval_index += 1;
                    if (gc_try_setmark(new_obj, &nptr, &tag, &bits)) {
                        stackitr->itr = itr;
                        stackitr->bt_index = bt_index;
                        stackitr->jlval_index = jlval_index;
                        gc_repush_markdata(&sp, gc_mark_excstack_t);
                        goto mark;
                    }
                }
                jlval_index = 0;
            }
            // The exception comes last - mark it
            new_obj = jl_excstack_exception(excstack, itr);
            itr = jl_excstack_next(excstack, itr);
            bt_index = 0;
            jlval_index = 0;
            uintptr_t nptr = 0;
            if (gc_try_setmark(new_obj, &nptr, &tag, &bits)) {
                stackitr->itr = itr;
                stackitr->bt_index = bt_index;
                stackitr->jlval_index = jlval_index;
                gc_repush_markdata(&sp, gc_mark_excstack_t);
                goto mark;
            }
        }
        goto pop;
    }

module_binding: {
        // Scan a module. see `gc_mark_binding_t`
        // Other fields of the module will be scanned after the bindings are scanned
        gc_mark_binding_t *binding = gc_pop_markdata(&sp, gc_mark_binding_t);
        jl_binding_t **begin = binding->begin;
        jl_binding_t **end = binding->end;
        uint8_t mbits = binding->bits;
        for (; begin < end; begin += 2) {
            jl_binding_t *b = *begin;
            if (b == (jl_binding_t*)HT_NOTFOUND)
                continue;
            if ((void*)b >= sysimg_base && (void*)b < sysimg_end) {
                jl_taggedvalue_t *buf = jl_astaggedvalue(b);
                uintptr_t tag = buf->header;
                uint8_t bits;
                if (!gc_marked(tag))
                    gc_setmark_tag(buf, GC_OLD_MARKED, tag, &bits);
            }
            else {
                gc_setmark_buf_(ptls, b, mbits, sizeof(jl_binding_t));
            }
            void *vb = jl_astaggedvalue(b);
            verify_parent1("module", binding->parent, &vb, "binding_buff");
            (void)vb;
            jl_value_t *value = b->value;
            jl_value_t *globalref = b->globalref;
            if (value) {
                verify_parent2("module", binding->parent,
                               &b->value, "binding(%s)", jl_symbol_name(b->name));
                if (gc_try_setmark(value, &binding->nptr, &tag, &bits)) {
                    new_obj = value;
                    begin += 2;
                    binding->begin = begin;
                    gc_repush_markdata(&sp, gc_mark_binding_t);
                    uintptr_t gr_tag;
                    uint8_t gr_bits;
                    if (gc_try_setmark(globalref, &binding->nptr, &gr_tag, &gr_bits)) {
                        gc_mark_marked_obj_t data = {globalref, gr_tag, gr_bits};
                        gc_mark_stack_push(&ptls->gc_cache, &sp, gc_mark_laddr(marked_obj),
                                           &data, sizeof(data), 1);
                    }
                    goto mark;
                }
            }
            if (gc_try_setmark(globalref, &binding->nptr, &tag, &bits)) {
                begin += 2;
                binding->begin = begin;
                gc_repush_markdata(&sp, gc_mark_binding_t);
                new_obj = globalref;
                goto mark;
            }
        }
        jl_module_t *m = binding->parent;
        int scanparent = gc_try_setmark((jl_value_t*)m->parent, &binding->nptr, &tag, &bits);
        size_t nusings = m->usings.len;
        if (nusings) {
            // this is only necessary because bindings for "using" modules
            // are added only when accessed. therefore if a module is replaced
            // after "using" it but before accessing it, this array might
            // contain the only reference.
            objary_begin = (jl_value_t**)m->usings.items;
            objary_end = objary_begin + nusings;
            gc_mark_objarray_t data = {(jl_value_t*)m, objary_begin, objary_end, 1, binding->nptr};
            gc_mark_stack_push(&ptls->gc_cache, &sp, gc_mark_laddr(objarray),
                               &data, sizeof(data), 0);
            if (!scanparent) {
                objary = (gc_mark_objarray_t*)sp.data;
                goto objarray_loaded;
            }
            sp.data = (jl_gc_mark_data_t *)(((char*)sp.data) + sizeof(data));
            sp.pc++;
        }
        else {
            gc_mark_push_remset(ptls, (jl_value_t*)m, binding->nptr);
        }
        if (scanparent) {
            new_obj = (jl_value_t*)m->parent;
            goto mark;
        }
        goto pop;
    }

finlist: {
        // Scan a finalizer (or format compatible) list. see `gc_mark_finlist_t`
        gc_mark_finlist_t *finlist = gc_pop_markdata(&sp, gc_mark_finlist_t);
        jl_value_t **begin = finlist->begin;
        jl_value_t **end = finlist->end;
        for (; begin < end; begin++) {
            new_obj = *begin;
            if (__unlikely(!new_obj))
                continue;
            if (gc_ptr_tag(new_obj, 1)) {
                new_obj = (jl_value_t*)gc_ptr_clear_tag(new_obj, 1);
                begin++;
                assert(begin < end);
            }
            uintptr_t nptr = 0;
            if (!gc_try_setmark(new_obj, &nptr, &tag, &bits))
                continue;
            begin++;
            // Found an object to mark
            if (begin < end) {
                // Haven't done with this one yet. Update the content and push it back
                finlist->begin = begin;
                gc_repush_markdata(&sp, gc_mark_finlist_t);
            }
            goto mark;
        }
        goto pop;
    }

mark: {
        // Generic scanning entry point.
        // Expects `new_obj`, `tag` and `bits` to be set correctly.
#ifdef JL_DEBUG_BUILD
        if (new_obj == gc_findval)
            jl_raise_debugger();
#endif
        jl_taggedvalue_t *o = jl_astaggedvalue(new_obj);
        jl_datatype_t *vt = (jl_datatype_t*)tag;
        int foreign_alloc = 0;
        int update_meta = __likely(!meta_updated && !gc_verifying);
        if (update_meta && (void*)o >= sysimg_base && (void*)o < sysimg_end) {
            foreign_alloc = 1;
            update_meta = 0;
        }
        meta_updated = 0;
        // Symbols are always marked
        assert(vt != jl_symbol_type);
        if (vt == jl_simplevector_type) {
            size_t l = jl_svec_len(new_obj);
            jl_value_t **data = jl_svec_data(new_obj);
            size_t dtsz = l * sizeof(void*) + sizeof(jl_svec_t);
            if (update_meta)
                gc_setmark(ptls, o, bits, dtsz);
            else if (foreign_alloc)
                objprofile_count(vt, bits == GC_OLD_MARKED, dtsz);
            uintptr_t nptr = (l << 2) | (bits & GC_OLD);
            objary_begin = data;
            objary_end = data + l;
            gc_mark_objarray_t markdata = {new_obj, objary_begin, objary_end, 1, nptr};
            gc_mark_stack_push(&ptls->gc_cache, &sp, gc_mark_laddr(objarray),
                               &markdata, sizeof(markdata), 0);
            objary = (gc_mark_objarray_t*)sp.data;
            goto objarray_loaded;
        }
        else if (vt->name == jl_array_typename) {
            jl_array_t *a = (jl_array_t*)new_obj;
            jl_array_flags_t flags = a->flags;
            if (update_meta) {
                if (flags.pooled)
                    gc_setmark_pool(ptls, o, bits);
                else
                    gc_setmark_big(ptls, o, bits);
            }
            else if (foreign_alloc)
                objprofile_count(vt, bits == GC_OLD_MARKED, sizeof(jl_array_t));
            if (flags.how == 1) {
                void *val_buf = jl_astaggedvalue((char*)a->data - a->offset * a->elsize);
                verify_parent1("array", new_obj, &val_buf, "buffer ('loc' addr is meaningless)");
                (void)val_buf;
                gc_setmark_buf_(ptls, (char*)a->data - a->offset * a->elsize,
                                bits, array_nbytes(a));
            }
            else if (flags.how == 2) {
                if (update_meta || foreign_alloc) {
                    objprofile_count(jl_malloc_tag, bits == GC_OLD_MARKED,
                                     array_nbytes(a));
                    if (bits == GC_OLD_MARKED) {
                        ptls->gc_cache.perm_scanned_bytes += array_nbytes(a);
                    }
                    else {
                        ptls->gc_cache.scanned_bytes += array_nbytes(a);
                    }
                }
            }
            else if (flags.how == 3) {
                jl_value_t *owner = jl_array_data_owner(a);
                uintptr_t nptr = (1 << 2) | (bits & GC_OLD);
                int markowner = gc_try_setmark(owner, &nptr, &tag, &bits);
                gc_mark_push_remset(ptls, new_obj, nptr);
                if (markowner) {
                    new_obj = owner;
                    goto mark;
                }
                goto pop;
            }
            if (a->data == NULL || jl_array_len(a) == 0)
                goto pop;
            if (flags.ptrarray) {
                size_t l = jl_array_len(a);
                uintptr_t nptr = (l << 2) | (bits & GC_OLD);
                objary_begin = (jl_value_t**)a->data;
                objary_end = objary_begin + l;
                gc_mark_objarray_t markdata = {new_obj, objary_begin, objary_end, 1, nptr};
                gc_mark_stack_push(&ptls->gc_cache, &sp, gc_mark_laddr(objarray),
                                   &markdata, sizeof(markdata), 0);
                objary = (gc_mark_objarray_t*)sp.data;
                goto objarray_loaded;
            }
            else if (flags.hasptr) {
                jl_datatype_t *et = (jl_datatype_t*)jl_tparam0(vt);
                const jl_datatype_layout_t *layout = et->layout;
                unsigned npointers = layout->npointers;
                unsigned elsize = a->elsize / sizeof(jl_value_t*);
                size_t l = jl_array_len(a);
                uintptr_t nptr = ((l * npointers) << 2) | (bits & GC_OLD);
                objary_begin = (jl_value_t**)a->data;
                objary_end = objary_begin + l * elsize;
                if (npointers == 1) { // TODO: detect anytime time stride is uniform?
                    objary_begin += layout->first_ptr;
                    gc_mark_objarray_t markdata = {new_obj, objary_begin, objary_end, elsize, nptr};
                    gc_mark_stack_push(&ptls->gc_cache, &sp, gc_mark_laddr(objarray),
                                       &markdata, sizeof(markdata), 0);
                    objary = (gc_mark_objarray_t*)sp.data;
                    goto objarray_loaded;
                }
                else if (layout->fielddesc_type == 0) {
                    obj8_begin = (uint8_t*)jl_dt_layout_ptrs(layout);
                    obj8_end = obj8_begin + npointers;
                    gc_mark_array8_t markdata = {objary_begin, objary_end, obj8_begin, {new_obj, obj8_begin, obj8_end, nptr}};
                    gc_mark_stack_push(&ptls->gc_cache, &sp, gc_mark_laddr(array8),
                                       &markdata, sizeof(markdata), 0);
                    ary8 = (gc_mark_array8_t*)sp.data;
                    goto array8_loaded;
                }
                else {
                    assert(0 && "unimplemented");
                }
            }
            goto pop;
        }
        else if (vt == jl_module_type) {
            if (update_meta)
                gc_setmark(ptls, o, bits, sizeof(jl_module_t));
            else if (foreign_alloc)
                objprofile_count(vt, bits == GC_OLD_MARKED, sizeof(jl_module_t));
            jl_module_t *m = (jl_module_t*)new_obj;
            jl_binding_t **table = (jl_binding_t**)m->bindings.table;
            size_t bsize = m->bindings.size;
            uintptr_t nptr = ((bsize + m->usings.len + 1) << 2) | (bits & GC_OLD);
            gc_mark_binding_t markdata = {m, table + 1, table + bsize, nptr, bits};
            gc_mark_stack_push(&ptls->gc_cache, &sp, gc_mark_laddr(module_binding),
                               &markdata, sizeof(markdata), 0);
            sp.data = (jl_gc_mark_data_t *)(((char*)sp.data) + sizeof(markdata));
            goto module_binding;
        }
        else if (vt == jl_task_type) {
            if (update_meta)
                gc_setmark(ptls, o, bits, sizeof(jl_task_t));
            else if (foreign_alloc)
                objprofile_count(vt, bits == GC_OLD_MARKED, sizeof(jl_task_t));
            jl_task_t *ta = (jl_task_t*)new_obj;
            gc_scrub_record_task(ta);
            void *stkbuf = ta->stkbuf;
            int16_t tid = ta->tid;
            jl_ptls_t ptls2 = NULL;
            if (tid != -1)
                ptls2 = jl_all_tls_states[tid];
            if (gc_cblist_task_scanner) {
                export_gc_state(ptls, &sp);
                gc_invoke_callbacks(jl_gc_cb_task_scanner_t,
                    gc_cblist_task_scanner,
                    (ta, ptls2 != NULL && ta == ptls2->root_task));
                import_gc_state(ptls, &sp);
            }
#ifdef COPY_STACKS
            if (stkbuf && ta->copy_stack)
                gc_setmark_buf_(ptls, stkbuf, bits, ta->bufsz);
#endif
            jl_gcframe_t *s = NULL;
            size_t nroots;
            uintptr_t offset = 0;
            uintptr_t lb = 0;
            uintptr_t ub = (uintptr_t)-1;
            if (ptls2 && ta == ptls2->current_task) {
                s = ptls2->pgcstack;
            }
            else if (stkbuf) {
                s = ta->gcstack;
#ifdef COPY_STACKS
                if (ta->copy_stack) {
                    assert(tid != -1 && ptls2 != NULL);
                    ub = (uintptr_t)ptls2->stackbase;
                    lb = ub - ta->copy_stack;
                    offset = (uintptr_t)stkbuf - lb;
                }
#endif
            }
            if (s) {
                nroots = gc_read_stack(&s->nroots, offset, lb, ub);
                assert(nroots <= UINT32_MAX);
                gc_mark_stackframe_t stackdata = {s, 0, (uint32_t)nroots, offset, lb, ub};
                gc_mark_stack_push(&ptls->gc_cache, &sp, gc_mark_laddr(stack),
                                   &stackdata, sizeof(stackdata), 1);
            }
            if (ta->excstack) {
                gc_setmark_buf_(ptls, ta->excstack, bits, sizeof(jl_excstack_t) +
                                sizeof(uintptr_t)*ta->excstack->reserved_size);
                gc_mark_excstack_t stackdata = {ta->excstack, ta->excstack->top, 0, 0};
                gc_mark_stack_push(&ptls->gc_cache, &sp, gc_mark_laddr(excstack),
                                   &stackdata, sizeof(stackdata), 1);
            }
            const jl_datatype_layout_t *layout = jl_task_type->layout;
            assert(layout->fielddesc_type == 0);
            assert(layout->nfields > 0);
            uint32_t npointers = layout->npointers;
            obj8_begin = (uint8_t*)jl_dt_layout_ptrs(layout);
            obj8_end = obj8_begin + npointers;
            // assume tasks always reference young objects: set lowest bit
            uintptr_t nptr = (npointers << 2) | 1 | bits;
            gc_mark_obj8_t markdata = {new_obj, obj8_begin, obj8_end, nptr};
            gc_mark_stack_push(&ptls->gc_cache, &sp, gc_mark_laddr(obj8),
                               &markdata, sizeof(markdata), 0);
            obj8 = (gc_mark_obj8_t*)sp.data;
            obj8_parent = (char*)ta;
            goto obj8_loaded;
        }
        else if (vt == jl_string_type) {
            size_t dtsz = jl_string_len(new_obj) + sizeof(size_t) + 1;
            if (update_meta)
                gc_setmark(ptls, o, bits, dtsz);
            else if (foreign_alloc)
                objprofile_count(vt, bits == GC_OLD_MARKED, dtsz);
            goto pop;
        }
        else {
            if (__unlikely(!jl_is_datatype(vt)))
                gc_assert_datatype_fail(ptls, vt, sp);
            size_t dtsz = jl_datatype_size(vt);
            if (update_meta)
                gc_setmark(ptls, o, bits, dtsz);
            else if (foreign_alloc)
                objprofile_count(vt, bits == GC_OLD_MARKED, dtsz);
            if (vt == jl_weakref_type)
                goto pop;
            const jl_datatype_layout_t *layout = vt->layout;
            uint32_t npointers = layout->npointers;
            if (npointers == 0)
                goto pop;
            uintptr_t nptr = npointers << 2 | (bits & GC_OLD);
            assert(layout->nfields > 0 && layout->fielddesc_type != 3 && "opaque types should have been handled specially");
            if (layout->fielddesc_type == 0) {
                obj8_parent = (char*)new_obj;
                obj8_begin = (uint8_t*)jl_dt_layout_ptrs(layout);
                obj8_end = obj8_begin + npointers;
                assert(obj8_begin < obj8_end);
                gc_mark_obj8_t markdata = {new_obj, obj8_begin, obj8_end, nptr};
                gc_mark_stack_push(&ptls->gc_cache, &sp, gc_mark_laddr(obj8),
                                   &markdata, sizeof(markdata), 0);
                obj8 = (gc_mark_obj8_t*)sp.data;
                goto obj8_loaded;
            }
            else if (layout->fielddesc_type == 1) {
                obj16_parent = (char*)new_obj;
                obj16_begin = (uint16_t*)jl_dt_layout_ptrs(layout);
                obj16_end = obj16_begin + npointers;
                assert(obj16_begin < obj16_end);
                gc_mark_obj16_t markdata = {new_obj, obj16_begin, obj16_end, nptr};
                gc_mark_stack_push(&ptls->gc_cache, &sp, gc_mark_laddr(obj16),
                                   &markdata, sizeof(markdata), 0);
                obj16 = (gc_mark_obj16_t*)sp.data;
                goto obj16_loaded;
            }
            else if (layout->fielddesc_type == 2) {
                // This is very uncommon
                // Do not do store to load forwarding to save some code size
                uint32_t *obj32_begin = (uint32_t*)jl_dt_layout_ptrs(layout);
                uint32_t *obj32_end = obj32_begin + npointers;
                gc_mark_obj32_t markdata = {new_obj, obj32_begin, obj32_end, nptr};
                gc_mark_stack_push(&ptls->gc_cache, &sp, gc_mark_laddr(obj32),
                                   &markdata, sizeof(markdata), 0);
                sp.data = (jl_gc_mark_data_t *)(((char*)sp.data) + sizeof(markdata));
                goto obj32;
            }
            else {
                assert(layout->fielddesc_type == 3);
                jl_fielddescdyn_t *desc = (jl_fielddescdyn_t*)jl_dt_layout_fields(layout);
                int old = jl_astaggedvalue(new_obj)->bits.gc & 2;
                export_gc_state(ptls, &sp);
                uintptr_t young = desc->markfunc(ptls, new_obj);
                import_gc_state(ptls, &sp);
                if (old && young)
                    gc_mark_push_remset(ptls, new_obj, young * 4 + 3);
                goto pop;
            }
        }
    }
}

static void jl_gc_queue_thread_local(jl_gc_mark_cache_t *gc_cache, jl_gc_mark_sp_t *sp,
                                     jl_ptls_t ptls2)
{
    gc_mark_queue_obj(gc_cache, sp, ptls2->current_task);
    gc_mark_queue_obj(gc_cache, sp, ptls2->root_task);
    if (ptls2->next_task)
        gc_mark_queue_obj(gc_cache, sp, ptls2->next_task);
    if (ptls2->previous_exception)
        gc_mark_queue_obj(gc_cache, sp, ptls2->previous_exception);
}

void jl_gc_mark_enqueued_tasks(jl_gc_mark_cache_t *gc_cache, jl_gc_mark_sp_t *sp);

// mark the initial root set
static void mark_roots(jl_gc_mark_cache_t *gc_cache, jl_gc_mark_sp_t *sp)
{
    // modules
    gc_mark_queue_obj(gc_cache, sp, jl_main_module);

    // tasks
    jl_gc_mark_enqueued_tasks(gc_cache, sp);

    // invisible builtin values
    if (jl_an_empty_vec_any != NULL)
        gc_mark_queue_obj(gc_cache, sp, jl_an_empty_vec_any);
    if (jl_module_init_order != NULL)
        gc_mark_queue_obj(gc_cache, sp, jl_module_init_order);
    for (size_t i = 0; i < jl_current_modules.size; i += 2) {
        if (jl_current_modules.table[i + 1] != HT_NOTFOUND) {
            gc_mark_queue_obj(gc_cache, sp, jl_current_modules.table[i]);
        }
    }
    gc_mark_queue_obj(gc_cache, sp, jl_anytuple_type_type);
    for (size_t i = 0; i < N_CALL_CACHE; i++)
        if (call_cache[i])
            gc_mark_queue_obj(gc_cache, sp, call_cache[i]);
    if (jl_all_methods != NULL)
        gc_mark_queue_obj(gc_cache, sp, jl_all_methods);
    if (_jl_debug_method_invalidation != NULL)
        gc_mark_queue_obj(gc_cache, sp, _jl_debug_method_invalidation);

    // constants
    gc_mark_queue_obj(gc_cache, sp, jl_emptytuple_type);
}

// find unmarked objects that need to be finalized from the finalizer list "list".
// this must happen last in the mark phase.
static void sweep_finalizer_list(arraylist_t *list)
{
    void **items = list->items;
    size_t len = list->len;
    size_t j = 0;
    for (size_t i=0; i < len; i+=2) {
        void *v0 = items[i];
        void *v = gc_ptr_clear_tag(v0, 1);
        if (__unlikely(!v0)) {
            // remove from this list
            continue;
        }

        void *fin = items[i+1];
        int isfreed = !gc_marked(jl_astaggedvalue(v)->bits.gc);
        int isold = (list != &finalizer_list_marked &&
                     jl_astaggedvalue(v)->bits.gc == GC_OLD_MARKED &&
                     jl_astaggedvalue(fin)->bits.gc == GC_OLD_MARKED);
        if (isfreed || isold) {
            // remove from this list
        }
        else {
            if (j < i) {
                items[j] = items[i];
                items[j+1] = items[i+1];
            }
            j += 2;
        }
        if (isfreed) {
            schedule_finalization(v0, fin);
        }
        if (isold) {
            // The caller relies on the new objects to be pushed to the end of
            // the list!!
            arraylist_push(&finalizer_list_marked, v0);
            arraylist_push(&finalizer_list_marked, fin);
        }
    }
    list->len = j;
}

// collector entry point and control
static volatile uint32_t jl_gc_disable_counter = 0;

JL_DLLEXPORT int jl_gc_enable(int on)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    int prev = !ptls->disable_gc;
    ptls->disable_gc = (on == 0);
    if (on && !prev) {
        // disable -> enable
        if (jl_atomic_fetch_add(&jl_gc_disable_counter, -1) == 1) {
            gc_num.allocd += gc_num.deferred_alloc;
            gc_num.deferred_alloc = 0;
        }
    }
    else if (prev && !on) {
        // enable -> disable
        jl_atomic_fetch_add(&jl_gc_disable_counter, 1);
        // check if the GC is running and wait for it to finish
        jl_gc_safepoint_(ptls);
    }
    return prev;
}

JL_DLLEXPORT int jl_gc_is_enabled(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    return !ptls->disable_gc;
}

JL_DLLEXPORT void jl_gc_get_total_bytes(int64_t *bytes)
{
    jl_gc_num_t num = gc_num;
    combine_thread_gc_counts(&num);
    // Sync this logic with `base/util.jl:GC_Diff`
    *bytes = (num.total_allocd + num.deferred_alloc + num.allocd);
}

JL_DLLEXPORT uint64_t jl_gc_total_hrtime(void)
{
    return gc_num.total_time;
}

JL_DLLEXPORT jl_gc_num_t jl_gc_num(void)
{
    jl_gc_num_t num = gc_num;
    combine_thread_gc_counts(&num);
    return num;
}

// TODO: these were supposed to be thread local
JL_DLLEXPORT int64_t jl_gc_diff_total_bytes(void)
{
    int64_t oldtb = last_gc_total_bytes;
    int64_t newtb;
    jl_gc_get_total_bytes(&newtb);
    last_gc_total_bytes = newtb;
    return newtb - oldtb;
}

JL_DLLEXPORT int64_t jl_gc_sync_total_bytes(int64_t offset)
{
    int64_t oldtb = last_gc_total_bytes;
    int64_t newtb;
    jl_gc_get_total_bytes(&newtb);
    last_gc_total_bytes = newtb - offset;
    return newtb - oldtb;
}

JL_DLLEXPORT int64_t jl_gc_live_bytes(void)
{
    return live_bytes;
}

static void jl_gc_premark(jl_ptls_t ptls2)
{
    arraylist_t *remset = ptls2->heap.remset;
    ptls2->heap.remset = ptls2->heap.last_remset;
    ptls2->heap.last_remset = remset;
    ptls2->heap.remset->len = 0;
    ptls2->heap.remset_nptr = 0;

    // avoid counting remembered objects & bindings twice
    // in `perm_scanned_bytes`
    size_t len = remset->len;
    void **items = remset->items;
    for (size_t i = 0; i < len; i++) {
        jl_value_t *item = (jl_value_t*)items[i];
        objprofile_count(jl_typeof(item), 2, 0);
        jl_astaggedvalue(item)->bits.gc = GC_OLD_MARKED;
    }
    len = ptls2->heap.rem_bindings.len;
    items = ptls2->heap.rem_bindings.items;
    for (size_t i = 0; i < len; i++) {
        void *ptr = items[i];
        jl_astaggedvalue(ptr)->bits.gc = GC_OLD_MARKED;
    }
}

static void jl_gc_queue_remset(jl_gc_mark_cache_t *gc_cache, jl_gc_mark_sp_t *sp, jl_ptls_t ptls2)
{
    size_t len = ptls2->heap.last_remset->len;
    void **items = ptls2->heap.last_remset->items;
    for (size_t i = 0; i < len; i++)
        gc_mark_queue_scan_obj(gc_cache, sp, (jl_value_t*)items[i]);
    int n_bnd_refyoung = 0;
    len = ptls2->heap.rem_bindings.len;
    items = ptls2->heap.rem_bindings.items;
    for (size_t i = 0; i < len; i++) {
        jl_binding_t *ptr = (jl_binding_t*)items[i];
        // A null pointer can happen here when the binding is cleaned up
        // as an exception is thrown after it was already queued (#10221)
        if (!ptr->value) continue;
        if (gc_mark_queue_obj(gc_cache, sp, ptr->value)) {
            items[n_bnd_refyoung] = ptr;
            n_bnd_refyoung++;
        }
    }
    ptls2->heap.rem_bindings.len = n_bnd_refyoung;
}

static void jl_gc_queue_bt_buf(jl_gc_mark_cache_t *gc_cache, jl_gc_mark_sp_t *sp, jl_ptls_t ptls2)
{
    jl_bt_element_t *bt_data = ptls2->bt_data;
    size_t bt_size = ptls2->bt_size;
    for (size_t i = 0; i < bt_size; i += jl_bt_entry_size(bt_data + i)) {
        jl_bt_element_t *bt_entry = bt_data + i;
        if (jl_bt_is_native(bt_entry))
            continue;
        size_t njlvals = jl_bt_num_jlvals(bt_entry);
        for (size_t j = 0; j < njlvals; j++)
            gc_mark_queue_obj(gc_cache, sp, jl_bt_entry_jlvalue(bt_entry, j));
    }
}

size_t jl_maxrss(void);

// Only one thread should be running in this function
static int _jl_gc_collect(jl_ptls_t ptls, jl_gc_collection_t collection)
{
    combine_thread_gc_counts(&gc_num);

    jl_gc_mark_cache_t *gc_cache = &ptls->gc_cache;
    jl_gc_mark_sp_t sp;
    gc_mark_sp_init(gc_cache, &sp);

    uint64_t t0 = jl_hrtime();
    int64_t last_perm_scanned_bytes = perm_scanned_bytes;

    // 1. fix GC bits of objects in the remset.
    for (int t_i = 0; t_i < jl_n_threads; t_i++)
        jl_gc_premark(jl_all_tls_states[t_i]);

    for (int t_i = 0; t_i < jl_n_threads; t_i++) {
        jl_ptls_t ptls2 = jl_all_tls_states[t_i];
        // 2.1. mark every object in the `last_remsets` and `rem_binding`
        jl_gc_queue_remset(gc_cache, &sp, ptls2);
        // 2.2. mark every thread local root
        jl_gc_queue_thread_local(gc_cache, &sp, ptls2);
        // 2.3. mark any managed objects in the backtrace buffer
        jl_gc_queue_bt_buf(gc_cache, &sp, ptls2);
    }

    // 3. walk roots
    mark_roots(gc_cache, &sp);
    if (gc_cblist_root_scanner) {
        export_gc_state(ptls, &sp);
        gc_invoke_callbacks(jl_gc_cb_root_scanner_t,
            gc_cblist_root_scanner, (collection));
        import_gc_state(ptls, &sp);
    }
    gc_mark_loop(ptls, sp);
    gc_mark_sp_init(gc_cache, &sp);
    gc_num.since_sweep += gc_num.allocd;
    gc_settime_premark_end();
    gc_time_mark_pause(t0, scanned_bytes, perm_scanned_bytes);
    int64_t actual_allocd = gc_num.since_sweep;
    // marking is over

    // 4. check for objects to finalize
    // Record the length of the marked list since we need to
    // mark the object moved to the marked list from the
    // `finalizer_list` by `sweep_finalizer_list`
    size_t orig_marked_len = finalizer_list_marked.len;
    for (int i = 0;i < jl_n_threads;i++) {
        jl_ptls_t ptls2 = jl_all_tls_states[i];
        sweep_finalizer_list(&ptls2->finalizers);
    }
    if (prev_sweep_full) {
        sweep_finalizer_list(&finalizer_list_marked);
        orig_marked_len = 0;
    }
    for (int i = 0;i < jl_n_threads;i++) {
        jl_ptls_t ptls2 = jl_all_tls_states[i];
        gc_mark_queue_finlist(gc_cache, &sp, &ptls2->finalizers, 0);
    }
    gc_mark_queue_finlist(gc_cache, &sp, &finalizer_list_marked, orig_marked_len);
    // "Flush" the mark stack before flipping the reset_age bit
    // so that the objects are not incorrectly reset.
    gc_mark_loop(ptls, sp);
    gc_mark_sp_init(gc_cache, &sp);
    // Conservative marking relies on age to tell allocated objects
    // and freelist entries apart.
    mark_reset_age = !support_conservative_marking;
    // Reset the age and old bit for any unmarked objects referenced by the
    // `to_finalize` list. These objects are only reachable from this list
    // and should not be referenced by any old objects so this won't break
    // the GC invariant.
    gc_mark_queue_finlist(gc_cache, &sp, &to_finalize, 0);
    gc_mark_loop(ptls, sp);
    mark_reset_age = 0;
    gc_settime_postmark_end();

    // Flush everything in mark cache
    gc_sync_all_caches_nolock(ptls);

    int64_t live_sz_ub = live_bytes + actual_allocd;
    int64_t live_sz_est = scanned_bytes + perm_scanned_bytes;
    int64_t estimate_freed = live_sz_ub - live_sz_est;

    gc_verify(ptls);

    gc_stats_all_pool();
    gc_stats_big_obj();
    objprofile_printall();
    objprofile_reset();
    gc_num.total_allocd += gc_num.since_sweep;
    if (!prev_sweep_full)
        promoted_bytes += perm_scanned_bytes - last_perm_scanned_bytes;
    // 5. next collection decision
    int not_freed_enough = estimate_freed < (7*(actual_allocd/10));
    int nptr = 0;
    for (int i = 0;i < jl_n_threads;i++)
        nptr += jl_all_tls_states[i]->heap.remset_nptr;
    int large_frontier = nptr*sizeof(void*) >= default_collect_interval; // many pointers in the intergen frontier => "quick" mark is not quick
    int sweep_full;
    int recollect = 0;
    // trigger a full collection if the number of live bytes doubles since the last full
    // collection and then remains at least that high for a while.
    if (grown_heap_age == 0) {
        if (live_bytes > 2 * last_full_live)
            grown_heap_age = 1;
    }
    else if (live_bytes >= last_live_bytes) {
        grown_heap_age++;
    }
    if (collection == JL_GC_INCREMENTAL) {
        sweep_full = 0;
    } else if ((collection == JL_GC_FULL || large_frontier ||
         ((not_freed_enough || promoted_bytes >= gc_num.interval) &&
          (promoted_bytes >= default_collect_interval || prev_sweep_full)) ||
         grown_heap_age > 1) &&
        gc_num.pause > 1) {
        recollect = (collection == JL_GC_FULL);
        if (large_frontier)
            gc_num.interval = last_long_collect_interval;
        if (not_freed_enough || large_frontier) {
            if (gc_num.interval <= 2*(max_collect_interval/5)) {
                gc_num.interval = 5 * (gc_num.interval / 2);
            }
        }
        last_long_collect_interval = gc_num.interval;
        sweep_full = 1;
        promoted_bytes = 0;
    }
    else {
        // reset interval to default, or at least half of live_bytes
        int64_t half = live_bytes/2;
        if (default_collect_interval < half && half <= max_collect_interval)
            gc_num.interval = half;
        else
            gc_num.interval = default_collect_interval;
        sweep_full = gc_sweep_always_full;
    }
    if (sweep_full)
        perm_scanned_bytes = 0;
    scanned_bytes = 0;
    // 5. start sweeping
    sweep_weak_refs();
    sweep_stack_pools();
    gc_sweep_foreign_objs();
    gc_sweep_other(ptls, sweep_full);
    gc_scrub();
    gc_verify_tags();
    gc_sweep_pool(sweep_full);
    if (sweep_full)
        gc_sweep_perm_alloc();
    // sweeping is over
    // 6. if it is a quick sweep, put back the remembered objects in queued state
    // so that we don't trigger the barrier again on them.
    for (int t_i = 0;t_i < jl_n_threads;t_i++) {
        jl_ptls_t ptls2 = jl_all_tls_states[t_i];
        if (!sweep_full) {
            for (int i = 0; i < ptls2->heap.remset->len; i++) {
                jl_astaggedvalue(ptls2->heap.remset->items[i])->bits.gc = GC_MARKED;
            }
            for (int i = 0; i < ptls2->heap.rem_bindings.len; i++) {
                void *ptr = ptls2->heap.rem_bindings.items[i];
                jl_astaggedvalue(ptr)->bits.gc = GC_MARKED;
            }
        }
        else {
            ptls2->heap.remset->len = 0;
            ptls2->heap.rem_bindings.len = 0;
        }
    }

#ifdef __GLIBC__
    if (sweep_full) {
        // issue #30653
        // empirically, the malloc runaway seemed to occur within a growth gap
        // of about 20-25%
        if (jl_maxrss() > (last_trim_maxrss/4)*5) {
            malloc_trim(0);
            last_trim_maxrss = jl_maxrss();
        }
    }
#endif

    uint64_t gc_end_t = jl_hrtime();
    uint64_t pause = gc_end_t - t0;
    gc_final_pause_end(t0, gc_end_t);
    gc_time_sweep_pause(gc_end_t, actual_allocd, live_bytes,
                        estimate_freed, sweep_full);
    gc_num.full_sweep += sweep_full;
    gc_num.allocd = 0;
    last_live_bytes = live_bytes;
    live_bytes += -gc_num.freed + gc_num.since_sweep;
    if (prev_sweep_full) {
        last_full_live = live_bytes;
        grown_heap_age = 0;
    }
    prev_sweep_full = sweep_full;
    gc_num.pause += !recollect;
    gc_num.total_time += pause;
    gc_num.since_sweep = 0;
    gc_num.freed = 0;
    reset_thread_gc_counts();

    return recollect;
}

JL_DLLEXPORT void jl_gc_collect(jl_gc_collection_t collection)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (jl_gc_disable_counter) {
        size_t localbytes = ptls->gc_num.allocd + gc_num.interval;
        ptls->gc_num.allocd = -(int64_t)gc_num.interval;
        jl_atomic_add_fetch(&gc_num.deferred_alloc, localbytes);
        return;
    }
    gc_debug_print();

    int8_t old_state = jl_gc_state(ptls);
    ptls->gc_state = JL_GC_STATE_WAITING;
    // `jl_safepoint_start_gc()` makes sure only one thread can
    // run the GC.
    if (!jl_safepoint_start_gc()) {
        // Multithread only. See assertion in `safepoint.c`
        jl_gc_state_set(ptls, old_state, JL_GC_STATE_WAITING);
        return;
    }
    JL_TIMING(GC);
    int last_errno = errno;
#ifdef _OS_WINDOWS_
    DWORD last_error = GetLastError();
#endif
    // Now we are ready to wait for other threads to hit the safepoint,
    // we can do a few things that doesn't require synchronization.
    // TODO (concurrently queue objects)
    // no-op for non-threading
    jl_gc_wait_for_the_world();
    gc_invoke_callbacks(jl_gc_cb_pre_gc_t,
        gc_cblist_pre_gc, (collection));

    if (!jl_gc_disable_counter) {
        JL_LOCK_NOGC(&finalizers_lock);
        if (_jl_gc_collect(ptls, collection)) {
            // recollect
            int ret = _jl_gc_collect(ptls, JL_GC_AUTO);
            (void)ret;
            assert(!ret);
        }
        JL_UNLOCK_NOGC(&finalizers_lock);
    }

    // no-op for non-threading
    jl_safepoint_end_gc();
    jl_gc_state_set(ptls, old_state, JL_GC_STATE_WAITING);

    // Only disable finalizers on current thread
    // Doing this on all threads is racy (it's impossible to check
    // or wait for finalizers on other threads without dead lock).
    if (!ptls->finalizers_inhibited) {
        int8_t was_in_finalizer = ptls->in_finalizer;
        ptls->in_finalizer = 1;
        run_finalizers(ptls);
        ptls->in_finalizer = was_in_finalizer;
    }
    gc_invoke_callbacks(jl_gc_cb_post_gc_t,
        gc_cblist_post_gc, (collection));
#ifdef _OS_WINDOWS_
    SetLastError(last_error);
#endif
    errno = last_errno;
}

void gc_mark_queue_all_roots(jl_ptls_t ptls, jl_gc_mark_sp_t *sp)
{
    jl_gc_mark_cache_t *gc_cache = &ptls->gc_cache;
    for (size_t i = 0; i < jl_n_threads; i++)
        jl_gc_queue_thread_local(gc_cache, sp, jl_all_tls_states[i]);
    mark_roots(gc_cache, sp);
}

// allocator entry points

JL_DLLEXPORT jl_value_t *(jl_gc_alloc)(jl_ptls_t ptls, size_t sz, void *ty)
{
    return jl_gc_alloc_(ptls, sz, ty);
}

// Per-thread initialization
void jl_init_thread_heap(jl_ptls_t ptls)
{
    jl_thread_heap_t *heap = &ptls->heap;
    jl_gc_pool_t *p = heap->norm_pools;
    for(int i=0; i < JL_GC_N_POOLS; i++) {
        assert((jl_gc_sizeclasses[i] < 16 &&
                jl_gc_sizeclasses[i] % sizeof(void*) == 0) ||
               (jl_gc_sizeclasses[i] % 16 == 0));
        p[i].osize = jl_gc_sizeclasses[i];
        p[i].freelist = NULL;
        p[i].newpages = NULL;
    }
    arraylist_new(&heap->weak_refs, 0);
    arraylist_new(&heap->live_tasks, 0);
    heap->mallocarrays = NULL;
    heap->mafreelist = NULL;
    heap->big_objects = NULL;
    arraylist_new(&heap->rem_bindings, 0);
    heap->remset = &heap->_remset[0];
    heap->last_remset = &heap->_remset[1];
    arraylist_new(heap->remset, 0);
    arraylist_new(heap->last_remset, 0);
    arraylist_new(&ptls->finalizers, 0);
    arraylist_new(&ptls->sweep_objs, 0);

    jl_gc_mark_cache_t *gc_cache = &ptls->gc_cache;
    gc_cache->perm_scanned_bytes = 0;
    gc_cache->scanned_bytes = 0;
    gc_cache->nbig_obj = 0;
    JL_MUTEX_INIT(&gc_cache->stack_lock);
    size_t init_size = 1024;
    gc_cache->pc_stack = (void**)malloc_s(init_size * sizeof(void*));
    gc_cache->pc_stack_end = gc_cache->pc_stack + init_size;
    gc_cache->data_stack = (jl_gc_mark_data_t *)malloc_s(init_size * sizeof(jl_gc_mark_data_t));

    memset(&ptls->gc_num, 0, sizeof(jl_thread_gc_num_t));
    assert(gc_num.interval == default_collect_interval);
    ptls->gc_num.allocd = -(int64_t)gc_num.interval;
}

// System-wide initializations
void jl_gc_init(void)
{
    jl_gc_init_page();
    gc_debug_init();

    arraylist_new(&finalizer_list_marked, 0);
    arraylist_new(&to_finalize, 0);

    gc_num.interval = default_collect_interval;
    last_long_collect_interval = default_collect_interval;
    gc_num.allocd = 0;

#ifdef _P64
    // on a big memory machine, set max_collect_interval to totalmem / ncores / 2
    uint64_t total_mem = uv_get_total_memory();
    uint64_t constrained_mem = uv_get_constrained_memory();
    if (constrained_mem > 0 && constrained_mem < total_mem)
        total_mem = constrained_mem;
    size_t maxmem = total_mem / jl_cpu_threads() / 2;
    if (maxmem > max_collect_interval)
        max_collect_interval = maxmem;
#endif
    jl_gc_mark_sp_t sp = {NULL, NULL, NULL, NULL};
    gc_mark_loop(NULL, sp);
}

// callback for passing OOM errors from gmp
JL_DLLEXPORT void jl_throw_out_of_memory_error(void)
{
    jl_throw(jl_memory_exception);
}

// allocation wrappers that track allocation and let collection run

JL_DLLEXPORT void *jl_gc_counted_malloc(size_t sz)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (ptls && ptls->world_age) {
        maybe_collect(ptls);
        ptls->gc_num.allocd += sz;
        ptls->gc_num.malloc++;
    }
    return malloc(sz);
}

JL_DLLEXPORT void *jl_gc_counted_calloc(size_t nm, size_t sz)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (ptls && ptls->world_age) {
        maybe_collect(ptls);
        ptls->gc_num.allocd += nm*sz;
        ptls->gc_num.malloc++;
    }
    return calloc(nm, sz);
}

JL_DLLEXPORT void jl_gc_counted_free_with_size(void *p, size_t sz)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    free(p);
    if (ptls && ptls->world_age) {
        ptls->gc_num.freed += sz;
        ptls->gc_num.freecall++;
    }
}

JL_DLLEXPORT void *jl_gc_counted_realloc_with_old_size(void *p, size_t old, size_t sz)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (ptls && ptls->world_age) {
        maybe_collect(ptls);
        if (sz < old)
            ptls->gc_num.freed += (old - sz);
        else
            ptls->gc_num.allocd += (sz - old);
        ptls->gc_num.realloc++;
    }
    return realloc(p, sz);
}

// allocation wrappers that save the size of allocations, to allow using
// jl_gc_counted_* functions with a libc-compatible API.

JL_DLLEXPORT void *jl_malloc(size_t sz)
{
    int64_t *p = (int64_t *)jl_gc_counted_malloc(sz + JL_SMALL_BYTE_ALIGNMENT);
    if (p == NULL)
        return NULL;
    p[0] = sz;
    return (void *)(p + 2); // assumes JL_SMALL_BYTE_ALIGNMENT == 16
}

JL_DLLEXPORT void *jl_calloc(size_t nm, size_t sz)
{
    size_t nmsz = nm*sz;
    int64_t *p = (int64_t *)jl_gc_counted_calloc(nmsz + JL_SMALL_BYTE_ALIGNMENT, 1);
    if (p == NULL)
        return NULL;
    p[0] = nmsz;
    return (void *)(p + 2); // assumes JL_SMALL_BYTE_ALIGNMENT == 16
}

JL_DLLEXPORT void jl_free(void *p)
{
    if (p != NULL) {
        int64_t *pp = (int64_t *)p - 2;
        size_t sz = pp[0];
        jl_gc_counted_free_with_size(pp, sz + JL_SMALL_BYTE_ALIGNMENT);
    }
}

JL_DLLEXPORT void *jl_realloc(void *p, size_t sz)
{
    int64_t *pp;
    size_t szold;
    if (p == NULL) {
        pp = NULL;
        szold = 0;
    }
    else {
        pp = (int64_t *)p - 2;
        szold = pp[0] + JL_SMALL_BYTE_ALIGNMENT;
    }
    int64_t *pnew = (int64_t *)jl_gc_counted_realloc_with_old_size(pp, szold, sz + JL_SMALL_BYTE_ALIGNMENT);
    if (pnew == NULL)
        return NULL;
    pnew[0] = sz;
    return (void *)(pnew + 2); // assumes JL_SMALL_BYTE_ALIGNMENT == 16
}

// allocating blocks for Arrays and Strings

JL_DLLEXPORT void *jl_gc_managed_malloc(size_t sz)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    maybe_collect(ptls);
    size_t allocsz = LLT_ALIGN(sz, JL_CACHE_BYTE_ALIGNMENT);
    if (allocsz < sz)  // overflow in adding offs, size was "negative"
        jl_throw(jl_memory_exception);
    ptls->gc_num.allocd += allocsz;
    ptls->gc_num.malloc++;
    int last_errno = errno;
#ifdef _OS_WINDOWS_
    DWORD last_error = GetLastError();
#endif
    void *b = malloc_cache_align(allocsz);
    if (b == NULL)
        jl_throw(jl_memory_exception);
#ifdef _OS_WINDOWS_
    SetLastError(last_error);
#endif
    errno = last_errno;
    return b;
}

static void *gc_managed_realloc_(jl_ptls_t ptls, void *d, size_t sz, size_t oldsz,
                                 int isaligned, jl_value_t *owner, int8_t can_collect)
{
    if (can_collect)
        maybe_collect(ptls);

    size_t allocsz = LLT_ALIGN(sz, JL_CACHE_BYTE_ALIGNMENT);
    if (allocsz < sz)  // overflow in adding offs, size was "negative"
        jl_throw(jl_memory_exception);

    if (jl_astaggedvalue(owner)->bits.gc == GC_OLD_MARKED) {
        ptls->gc_cache.perm_scanned_bytes += allocsz - oldsz;
        live_bytes += allocsz - oldsz;
    }
    else if (allocsz < oldsz)
        ptls->gc_num.freed += (oldsz - allocsz);
    else
        ptls->gc_num.allocd += (allocsz - oldsz);
    ptls->gc_num.realloc++;

    int last_errno = errno;
#ifdef _OS_WINDOWS_
    DWORD last_error = GetLastError();
#endif
    void *b;
    if (isaligned)
        b = realloc_cache_align(d, allocsz, oldsz);
    else
        b = realloc(d, allocsz);
    if (b == NULL)
        jl_throw(jl_memory_exception);
#ifdef _OS_WINDOWS_
    SetLastError(last_error);
#endif
    errno = last_errno;

    return b;
}

JL_DLLEXPORT void *jl_gc_managed_realloc(void *d, size_t sz, size_t oldsz,
                                         int isaligned, jl_value_t *owner)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    return gc_managed_realloc_(ptls, d, sz, oldsz, isaligned, owner, 1);
}

jl_value_t *jl_gc_realloc_string(jl_value_t *s, size_t sz)
{
    size_t len = jl_string_len(s);
    if (sz <= len) return s;
    jl_taggedvalue_t *v = jl_astaggedvalue(s);
    size_t strsz = len + sizeof(size_t) + 1;
    if (strsz <= GC_MAX_SZCLASS ||
        // TODO: because of issue #17971 we can't resize old objects
        gc_marked(v->bits.gc)) {
        // pool allocated; can't be grown in place so allocate a new object.
        jl_value_t *snew = jl_alloc_string(sz);
        memcpy(jl_string_data(snew), jl_string_data(s), len);
        return snew;
    }
    size_t newsz = sz + sizeof(size_t) + 1;
    size_t offs = sizeof(bigval_t);
    size_t oldsz = LLT_ALIGN(strsz + offs, JL_CACHE_BYTE_ALIGNMENT);
    size_t allocsz = LLT_ALIGN(newsz + offs, JL_CACHE_BYTE_ALIGNMENT);
    if (allocsz < sz)  // overflow in adding offs, size was "negative"
        jl_throw(jl_memory_exception);
    bigval_t *hdr = bigval_header(v);
    jl_ptls_t ptls = jl_get_ptls_states();
    maybe_collect(ptls); // don't want this to happen during jl_gc_managed_realloc
    gc_big_object_unlink(hdr);
    // TODO: this is not safe since it frees the old pointer. ideally we'd like
    // the old pointer to be left alone if we can't grow in place.
    // for now it's up to the caller to make sure there are no references to the
    // old pointer.
    bigval_t *newbig = (bigval_t*)gc_managed_realloc_(ptls, hdr, allocsz, oldsz, 1, s, 0);
    newbig->sz = allocsz;
    newbig->age = 0;
    gc_big_object_link(newbig, &ptls->heap.big_objects);
    jl_value_t *snew = jl_valueof(&newbig->header);
    *(size_t*)snew = sz;
    return snew;
}

// Perm gen allocator
// 2M pool
#define GC_PERM_POOL_SIZE (2 * 1024 * 1024)
// 20k limit for pool allocation. At most 1% fragmentation
#define GC_PERM_POOL_LIMIT (20 * 1024)
jl_mutex_t gc_perm_lock = {0, 0};
static uintptr_t gc_perm_pool = 0;
static uintptr_t gc_perm_end = 0;

static void *gc_perm_alloc_large(size_t sz, int zero, unsigned align, unsigned offset) JL_NOTSAFEPOINT
{
    // `align` must be power of two
    assert(offset == 0 || offset < align);
    const size_t malloc_align = sizeof(void*) == 8 ? 16 : 4;
    if (align > 1 && (offset != 0 || align > malloc_align))
        sz += align - 1;
    int last_errno = errno;
#ifdef _OS_WINDOWS_
    DWORD last_error = GetLastError();
#endif
    uintptr_t base = (uintptr_t)(zero ? calloc(1, sz) : malloc(sz));
    if (base == 0)
        jl_throw(jl_memory_exception);
#ifdef _OS_WINDOWS_
    SetLastError(last_error);
#endif
    errno = last_errno;
    jl_may_leak(base);
    unsigned diff = (offset - base) % align;
    return (void*)(base + diff);
}

STATIC_INLINE void *gc_try_perm_alloc_pool(size_t sz, unsigned align, unsigned offset) JL_NOTSAFEPOINT
{
    uintptr_t pool = LLT_ALIGN(gc_perm_pool + offset, (uintptr_t)align) - offset;
    uintptr_t end = pool + sz;
    if (end > gc_perm_end)
        return NULL;
    gc_perm_pool = end;
    return (void*)jl_assume(pool);
}

// **NOT** a safepoint
void *jl_gc_perm_alloc_nolock(size_t sz, int zero, unsigned align, unsigned offset)
{
    // The caller should have acquired `gc_perm_lock`
    assert(align < GC_PERM_POOL_LIMIT);
#ifndef MEMDEBUG
    if (__unlikely(sz > GC_PERM_POOL_LIMIT))
#endif
        return gc_perm_alloc_large(sz, zero, align, offset);
    void *ptr = gc_try_perm_alloc_pool(sz, align, offset);
    if (__likely(ptr))
        return ptr;
    int last_errno = errno;
#ifdef _OS_WINDOWS_
    DWORD last_error = GetLastError();
    void *pool = VirtualAlloc(NULL, GC_PERM_POOL_SIZE, MEM_COMMIT, PAGE_READWRITE);
    SetLastError(last_error);
    errno = last_errno;
    if (__unlikely(pool == NULL))
        return NULL;
#else
    void *pool = mmap(0, GC_PERM_POOL_SIZE, PROT_READ | PROT_WRITE,
                      MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    errno = last_errno;
    if (__unlikely(pool == MAP_FAILED))
        return NULL;
#endif
    gc_perm_pool = (uintptr_t)pool;
    gc_perm_end = gc_perm_pool + GC_PERM_POOL_SIZE;
    return gc_try_perm_alloc_pool(sz, align, offset);
}

// **NOT** a safepoint
void *jl_gc_perm_alloc(size_t sz, int zero, unsigned align, unsigned offset)
{
    assert(align < GC_PERM_POOL_LIMIT);
#ifndef MEMDEBUG
    if (__unlikely(sz > GC_PERM_POOL_LIMIT))
#endif
        return gc_perm_alloc_large(sz, zero, align, offset);
    JL_LOCK_NOGC(&gc_perm_lock);
    void *p = jl_gc_perm_alloc_nolock(sz, zero, align, offset);
    JL_UNLOCK_NOGC(&gc_perm_lock);
    return p;
}

JL_DLLEXPORT void jl_gc_add_finalizer(jl_value_t *v, jl_function_t *f)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_gc_add_finalizer_th(ptls, v, f);
}

JL_DLLEXPORT void jl_finalize(jl_value_t *o)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_finalize_th(ptls, o);
}

JL_DLLEXPORT jl_weakref_t *jl_gc_new_weakref(jl_value_t *value)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    return jl_gc_new_weakref_th(ptls, value);
}

JL_DLLEXPORT jl_value_t *jl_gc_allocobj(size_t sz)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    return jl_gc_alloc(ptls, sz, NULL);
}

JL_DLLEXPORT jl_value_t *jl_gc_alloc_0w(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    return jl_gc_alloc(ptls, 0, NULL);
}

JL_DLLEXPORT jl_value_t *jl_gc_alloc_1w(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    return jl_gc_alloc(ptls, sizeof(void*), NULL);
}

JL_DLLEXPORT jl_value_t *jl_gc_alloc_2w(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    return jl_gc_alloc(ptls, sizeof(void*) * 2, NULL);
}

JL_DLLEXPORT jl_value_t *jl_gc_alloc_3w(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    return jl_gc_alloc(ptls, sizeof(void*) * 3, NULL);
}

JL_DLLEXPORT int jl_gc_enable_conservative_gc_support(void)
{
    static_assert(jl_buff_tag % GC_PAGE_SZ == 0,
        "jl_buff_tag must be a multiple of GC_PAGE_SZ");
    if (jl_is_initialized()) {
        int result = jl_atomic_fetch_or(&support_conservative_marking, 1);
        if (!result) {
            // Do a full collection to ensure that age bits are updated
            // properly. We don't have to worry about race conditions
            // for this part, as allocation itself is unproblematic and
            // a collection will wait for safepoints.
            jl_gc_collect(JL_GC_FULL);
        }
        return result;
    } else {
        int result = support_conservative_marking;
        support_conservative_marking = 1;
        return result;
    }
}

JL_DLLEXPORT int jl_gc_conservative_gc_support_enabled(void)
{
    return jl_atomic_load(&support_conservative_marking);
}

JL_DLLEXPORT jl_value_t *jl_gc_internal_obj_base_ptr(void *p)
{
    p = (char *) p - 1;
    jl_gc_pagemeta_t *meta = page_metadata(p);
    if (meta && meta->ages) {
        char *page = gc_page_data(p);
        // offset within page.
        size_t off = (char *)p - page;
        if (off < GC_PAGE_OFFSET)
            return NULL;
        // offset within object
        size_t off2 = (off - GC_PAGE_OFFSET);
        size_t osize = meta->osize;
        off2 %= osize;
        if (off - off2 + osize > GC_PAGE_SZ)
            return NULL;
        jl_taggedvalue_t *cell = (jl_taggedvalue_t *)((char *)p - off2);
        // We have to distinguish between three cases:
        // 1. We are on a page where every cell is allocated.
        // 2. We are on a page where objects are currently bump-allocated
        //    from the corresponding pool->newpages list.
        // 3. We are on a page with a freelist that is used for object
        //    allocation.
        if (meta->nfree == 0) {
            // case 1: full page; `cell` must be an object
            goto valid_object;
        }
        jl_gc_pool_t *pool =
            jl_all_tls_states[meta->thread_n]->heap.norm_pools +
            meta->pool_n;
        if (meta->fl_begin_offset == (uint16_t) -1) {
            // case 2: this is a page on the newpages list
            jl_taggedvalue_t *newpages = pool->newpages;
            // Check if the page is being allocated from via newpages
            if (!newpages)
                return NULL;
            char *data = gc_page_data(newpages);
            if (data != meta->data) {
                // Pages on newpages form a linked list where only the
                // first one is allocated from (see reset_page()).
                // All other pages are empty.
                return NULL;
            }
            // This is the first page on the newpages list, where objects
            // are allocated from.
            if ((char *)cell >= (char *)newpages) // past allocation pointer
                return NULL;
            goto valid_object;
        }
        // case 3: this is a page with a freelist
        // marked or old objects can't be on the freelist
        if (cell->bits.gc)
            goto valid_object;
        // When allocating from a freelist, three subcases are possible:
        // * The freelist of a page has been exhausted; this was handled
        //   under case 1, as nfree == 0.
        // * The freelist of the page has not been used, and the age bits
        //   reflect whether a cell is on the freelist or an object.
        // * The freelist is currently being allocated from. In this case,
        //   pool->freelist will point to the current page; any cell with
        //   a lower address will be an allocated object, and for cells
        //   with the same or a higher address, the corresponding age
        //   bit will reflect whether it's on the freelist.
        // Age bits are set in sweep_page() and are 0 for freelist
        // entries and 1 for live objects. The above subcases arise
        // because allocating a cell will not update the age bit, so we
        // need extra logic for pages that have been allocated from.
        unsigned obj_id = (off - off2) / osize;
        // We now distinguish between the second and third subcase.
        // Freelist entries are consumed in ascending order. Anything
        // before the freelist pointer was either live during the last
        // sweep or has been allocated since.
        if (gc_page_data(cell) == gc_page_data(pool->freelist)
            && (char *)cell < (char *)pool->freelist)
            goto valid_object;
        // We know now that the age bit reflects liveness status during
        // the last sweep and that the cell has not been reused since.
        if (!(meta->ages[obj_id / 8] & (1 << (obj_id % 8)))) {
            return NULL;
        }
        // Not a freelist entry, therefore a valid object.
    valid_object:
        // We have to treat objects with type `jl_buff_tag` differently,
        // as they must not be passed to the usual marking functions.
        // Note that jl_buff_tag is a multiple of GC_PAGE_SZ, thus it
        // cannot be a type reference.
        if ((cell->header & ~(uintptr_t) 3) == jl_buff_tag)
            return NULL;
        return jl_valueof(cell);
    }
    return NULL;
}

JL_DLLEXPORT size_t jl_gc_max_internal_obj_size(void)
{
    return GC_MAX_SZCLASS;
}

JL_DLLEXPORT size_t jl_gc_external_obj_hdr_size(void)
{
    return sizeof(bigval_t);
}


JL_DLLEXPORT void * jl_gc_alloc_typed(jl_ptls_t ptls, size_t sz, void *ty)
{
    return jl_gc_alloc(ptls, sz, ty);
}

JL_DLLEXPORT void jl_gc_schedule_foreign_sweepfunc(jl_ptls_t ptls, jl_value_t *obj)
{
    arraylist_push(&ptls->sweep_objs, obj);
}

#ifdef __cplusplus
}
#endif

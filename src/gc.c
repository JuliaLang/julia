// This file is a part of Julia. License is MIT: http://julialang.org/license

#include "gc.h"

#ifdef __cplusplus
extern "C" {
#endif

// Protect all access to `finalizer_list_marked` and `to_finalize`.
// For accessing `ptls->finalizers`, the lock is needed if a thread
// is going to realloc the buffer (of its own list) or accessing the
// list of another thread
static jl_mutex_t finalizers_lock;
static jl_mutex_t gc_cache_lock;

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

region_t regions[REGION_COUNT];

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

#ifdef JULIA_ENABLE_THREADING
static void jl_gc_wait_for_the_world(void)
{
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
#else
static inline void jl_gc_wait_for_the_world(void)
{
}
#endif

// malloc wrappers, aligned allocation

#define malloc_cache_align(sz) jl_malloc_aligned(sz, JL_CACHE_BYTE_ALIGNMENT)
#define realloc_cache_align(p, sz, oldsz) jl_realloc_aligned(p, sz, oldsz, JL_CACHE_BYTE_ALIGNMENT)

static void schedule_finalization(void *o, void *f)
{
    arraylist_push(&to_finalize, o);
    arraylist_push(&to_finalize, f);
}

static void run_finalizer(jl_ptls_t ptls, jl_value_t *o, jl_value_t *ff)
{
    assert(!jl_typeis(ff, jl_voidpointer_type));
    jl_value_t *args[2] = {ff,o};
    JL_TRY {
        size_t last_age = jl_get_ptls_states()->world_age;
        jl_get_ptls_states()->world_age = jl_world_counter;
        jl_apply(args, 2);
        jl_get_ptls_states()->world_age = last_age;
    }
    JL_CATCH {
        jl_printf(JL_STDERR, "error in running finalizer: ");
        jl_static_show(JL_STDERR, ptls->exception_in_transit);
        jl_printf(JL_STDERR, "\n");
    }
}

// if `need_sync` is true, the `list` is the `finalizers` list of another
// thread and we need additional synchronizations
static void finalize_object(arraylist_t *list, jl_value_t *o,
                            arraylist_t *copied_list, int need_sync)
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
    for (size_t i = 0; i < len; i += 2) {
        void *v = items[i];
        int move = 0;
        if (o == (jl_value_t*)gc_ptr_clear_tag(v, 1)) {
            void *f = items[i + 1];
            move = 1;
            if (gc_ptr_tag(v, 1)) {
                ((void (*)(void*))f)(o);
            }
            else {
                arraylist_push(copied_list, o);
                arraylist_push(copied_list, f);
            }
        }
        if (move || __unlikely(!v)) {
            if (i < len - 2) {
                items[i] = items[len - 2];
                items[i + 1] = items[len - 1];
                i -= 2;
            }
            len -= 2;
        }
    }
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
    items[0] = (void*)(((uintptr_t)list->len - 2) << 1);
    items[1] = ptls->pgcstack;
    ptls->pgcstack = (jl_gcframe_t*)items;
}

// Same assumption as `jl_gc_push_arraylist`. Requires the finalizers lock
// to be hold for the current thread and will release the lock when the
// function returns.
static void jl_gc_run_finalizers_in_list(jl_ptls_t ptls, arraylist_t *list)
{
    size_t len = list->len;
    jl_value_t **items = (jl_value_t**)list->items;
    jl_gc_push_arraylist(ptls, list);
    JL_UNLOCK_NOGC(&finalizers_lock);
    for (size_t i = 2;i < len;i += 2)
        run_finalizer(ptls, items[i], items[i + 1]);
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
    // empty out the first two entries for the GC frame
    arraylist_push(&copied_list, copied_list.items[0]);
    arraylist_push(&copied_list, copied_list.items[1]);
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

static void schedule_all_finalizers(arraylist_t *flist)
{
    void **items = flist->items;
    size_t len = flist->len;
    for(size_t i = 0; i < len; i+=2) {
        void *v = items[i];
        void *f = items[i + 1];
        if (__unlikely(!v))
            continue;
        if (!gc_ptr_tag(v, 1)) {
            schedule_finalization(v, f);
        }
        else {
            ((void (*)(void*))f)(gc_ptr_clear_tag(v, 1));
        }
    }
    flist->len = 0;
}

void jl_gc_run_all_finalizers(jl_ptls_t ptls)
{
    for (int i = 0;i < jl_n_threads;i++) {
        jl_ptls_t ptls2 = jl_all_tls_states[i];
        schedule_all_finalizers(&ptls2->finalizers);
    }
    schedule_all_finalizers(&finalizer_list_marked);
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
        // Another possiblility is to always grow the array to `oldlen + 2` but
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

STATIC_INLINE void gc_add_ptr_finalizer(jl_ptls_t ptls, jl_value_t *v, void *f)
{
    gc_add_finalizer_(ptls, (void*)(((uintptr_t)v) | 1), f);
}

JL_DLLEXPORT void jl_gc_add_finalizer_th(jl_ptls_t ptls, jl_value_t *v,
                                         jl_function_t *f)
{
    if (__unlikely(jl_typeis(f, jl_voidpointer_type))) {
        gc_add_ptr_finalizer(ptls, v, jl_unbox_voidpointer(f));
    }
    else {
        gc_add_finalizer_(ptls, v, f);
    }
}

JL_DLLEXPORT void jl_gc_add_ptr_finalizer(jl_ptls_t ptls, jl_value_t *v, void *f)
{
    gc_add_ptr_finalizer(ptls, v, f);
}

JL_DLLEXPORT void jl_finalize_th(jl_ptls_t ptls, jl_value_t *o)
{
    JL_LOCK_NOGC(&finalizers_lock);
    // Copy the finalizers into a temporary list so that code in the finalizer
    // won't change the list as we loop through them.
    // This list is also used as the GC frame when we are running the finalizers
    arraylist_t copied_list;
    arraylist_new(&copied_list, 0);
    arraylist_push(&copied_list, NULL); // GC frame size to be filled later
    arraylist_push(&copied_list, NULL); // pgcstack to be filled later
    // No need to check the to_finalize list since the user is apparently
    // still holding a reference to the object
    for (int i = 0;i < jl_n_threads;i++) {
        jl_ptls_t ptls2 = jl_all_tls_states[i];
        finalize_object(&ptls2->finalizers, o, &copied_list, ptls != ptls2);
    }
    finalize_object(&finalizer_list_marked, o, &copied_list, 0);
    if (copied_list.len > 2) {
        // This releases the finalizers lock.
        jl_gc_run_finalizers_in_list(ptls, &copied_list);
    }
    else {
        JL_UNLOCK_NOGC(&finalizers_lock);
    }
    arraylist_free(&copied_list);
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
// - the size of the age storage in region_t


static int64_t scanned_bytes; // young bytes scanned while marking
static int64_t perm_scanned_bytes; // old bytes scanned while marking
static int prev_sweep_full = 1;

#define inc_sat(v,s) v = (v) >= s ? s : (v)+1

// Full collection heuristics
static int64_t live_bytes = 0;
static int64_t promoted_bytes = 0;

static int64_t last_full_live_ub = 0;
static int64_t last_full_live_est = 0;
// upper bound and estimated live object sizes
// This heuristic should be really unlikely to trigger.
// However, this should be simple enough to trigger a full collection
// when it's necessary if other heuristics are messed up.
// It is also possible to take the total memory available into account
// if necessary.
STATIC_INLINE int gc_check_heap_size(int64_t sz_ub, int64_t sz_est)
{
    if (__unlikely(!last_full_live_ub || last_full_live_ub > sz_ub)) {
        last_full_live_ub = sz_ub;
    }
    else if (__unlikely(last_full_live_ub * 3 / 2 < sz_ub)) {
        return 1;
    }
    if (__unlikely(!last_full_live_est || last_full_live_est > sz_est)) {
        last_full_live_est = sz_est;
    }
    else if (__unlikely(last_full_live_est * 2 < sz_est)) {
        return 1;
    }
    return 0;
}

STATIC_INLINE void gc_update_heap_size(int64_t sz_ub, int64_t sz_est)
{
    last_full_live_ub = sz_ub;
    last_full_live_est = sz_est;
}

static void gc_sync_cache_nolock(jl_ptls_t ptls, jl_gc_mark_cache_t *gc_cache)
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

static void gc_sync_cache(jl_ptls_t ptls)
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
                                       int toyoung)
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

// `gc_setmark_big` and `gc_setmark_pool` can be called concurrently on
// multiple threads. In all cases (except gc-debug),
// the functions atomically sets the mark bits and updates the metadata
// if the bits are changed.
// All concurrent calls on the same object guarantees to be setting the
// bits to the same value.
// For normal objects, this is the bits with only `GC_MARKED` changed to `1`
// For buffers, this is the bits of the owner object.
// For `mark_reset_age`, this is `GC_MARKED` with `GC_OLD` cleared.
static inline uint16_t gc_setmark_big(jl_ptls_t ptls, jl_taggedvalue_t *o,
                                      int8_t mark_mode, uintptr_t tag)
{
    assert(!gc_marked(tag));
    if (gc_verifying) {
        o->bits.gc = mark_mode;
        return mark_mode | (1 << 8);
    }
    assert(find_region(o) == NULL);
    bigval_t *hdr = bigval_header(o);
    if (mark_reset_age) {
        mark_mode = GC_MARKED;
    }
    else if (gc_old(tag)) {
        mark_mode = GC_OLD_MARKED;
    }
    tag = gc_set_bits(tag, mark_mode);
    tag = jl_atomic_exchange_relaxed(&o->header, tag);
    uint16_t tag_changed = !gc_marked(tag);
    if (tag_changed) {
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
    verify_val(jl_valueof(o));
    return (tag_changed << 8) | mark_mode;
}

static inline uint16_t gc_setmark_pool_(jl_ptls_t ptls, jl_taggedvalue_t *o,
                                        int8_t mark_mode, region_t *r,
                                        uintptr_t tag)
{
    assert(!gc_marked(tag));
#ifdef MEMDEBUG
    return gc_setmark_big(ptls, o, mark_mode, tag);
#endif
    assert(r != NULL);
    if (gc_verifying) {
        o->bits.gc = mark_mode;
        return mark_mode | (1 << 8);
    }
    jl_gc_pagemeta_t *page = page_metadata_(o, r);
    if (mark_reset_age) {
        // Reset the object as if it was just allocated
        mark_mode = GC_MARKED;
    }
    else if (gc_old(tag)) {
        mark_mode = GC_OLD_MARKED;
    }
    tag = gc_set_bits(tag, mark_mode);
    tag = jl_atomic_exchange_relaxed(&o->header, tag);
    uint16_t tag_changed = !gc_marked(tag);
    if (tag_changed) {
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
    }
    assert(gc_marked(mark_mode));
    verify_val(jl_valueof(o));
    return (tag_changed << 8) | mark_mode;
}

static inline uint16_t gc_setmark_pool(jl_ptls_t ptls, jl_taggedvalue_t *o,
                                       int8_t mark_mode, uintptr_t tag)
{
    assert(!gc_marked(tag));
    return gc_setmark_pool_(ptls, o, mark_mode, find_region(o), tag);
}

static inline uint16_t gc_setmark(jl_ptls_t ptls, jl_value_t *v,
                                  size_t sz, uintptr_t tag)
{
    assert(!gc_marked(tag));
    jl_taggedvalue_t *o = jl_astaggedvalue(v);
    if (sz <= GC_MAX_SZCLASS)
        return gc_setmark_pool(ptls, o, GC_MARKED, tag);
    else
        return gc_setmark_big(ptls, o, GC_MARKED, tag);
}

inline void gc_setmark_buf(jl_ptls_t ptls, void *o,
                           int8_t mark_mode, size_t minsz)
{
    jl_taggedvalue_t *buf = jl_astaggedvalue(o);
    uintptr_t tag = buf->header;
    if (gc_marked(tag))
        return;
    // If the object is larger than the max pool size it can't be a pool object.
    // This should be accurate most of the time but there might be corner cases
    // where the size estimate is a little off so we do a pool lookup to make
    // sure.
    if (minsz <= GC_MAX_SZCLASS) {
        region_t *r = find_region(buf);
        if (r) {
            gc_setmark_pool_(ptls, buf, mark_mode, r, tag);
            return;
        }
    }
    gc_setmark_big(ptls, buf, mark_mode, tag);
}

#define should_collect() (__unlikely(gc_num.allocd>0))

static inline int maybe_collect(jl_ptls_t ptls)
{
    if (should_collect() || gc_debug_check_other()) {
        jl_gc_collect(0);
        return 1;
    }
    jl_gc_safepoint_(ptls);
    return 0;
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
    for (int i = 0;i < jl_n_threads;i++) {
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
                // weakref itself is alive
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
            lst[n+ndel] = tmp;
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
    size_t allocsz = LLT_ALIGN(sz + offs, JL_CACHE_BYTE_ALIGNMENT);
    if (allocsz < sz)  // overflow in adding offs, size was "negative"
        jl_throw(jl_memory_exception);
    bigval_t *v = (bigval_t*)malloc_cache_align(allocsz);
    if (v == NULL)
        jl_throw(jl_memory_exception);
#ifdef JULIA_ENABLE_THREADING
    jl_atomic_fetch_add(&gc_num.allocd, allocsz);
#else
    gc_num.allocd += allocsz;
#endif
    gc_num.bigalloc++;
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
static bigval_t **sweep_big_list(int sweep_full, bigval_t **pv)
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
            jl_free_aligned(v);
        }
        gc_time_count_big(old_bits, bits);
        v = nxt;
    }
    return pv;
}

static void sweep_big(jl_ptls_t ptls, int sweep_full)
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

void jl_gc_track_malloced_array(jl_ptls_t ptls, jl_array_t *a)
{
    // This is **NOT** a GC safe point.
    mallocarray_t *ma;
    if (ptls->heap.mafreelist == NULL) {
        ma = (mallocarray_t*)malloc(sizeof(mallocarray_t));
    }
    else {
        ma = ptls->heap.mafreelist;
        ptls->heap.mafreelist = ma->next;
    }
    ma->a = a;
    ma->next = ptls->heap.mallocarrays;
    ptls->heap.mallocarrays = ma;
}

void jl_gc_count_allocd(size_t sz)
{
    // This is **NOT** a GC safe point.
    gc_num.allocd += sz;
}

void jl_gc_reset_alloc_count(void)
{
    live_bytes += (gc_num.deferred_alloc + (gc_num.allocd + gc_num.interval));
    gc_num.allocd = -(int64_t)gc_num.interval;
    gc_num.deferred_alloc = 0;
}

static size_t array_nbytes(jl_array_t *a)
{
    size_t sz = 0;
    if (jl_array_ndims(a)==1)
        sz = a->elsize * a->maxsize + (a->elsize == 1 ? 1 : 0);
    else
        sz = a->elsize * jl_array_len(a);
    return sz;
}

static void jl_gc_free_array(jl_array_t *a)
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

static void sweep_malloced_arrays(void)
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
static inline jl_taggedvalue_t *reset_page(const jl_gc_pool_t *p, jl_gc_pagemeta_t *pg, jl_taggedvalue_t *fl)
{
    assert(GC_PAGE_OFFSET >= sizeof(void*));
    pg->nfree = (GC_PAGE_SZ - GC_PAGE_OFFSET) / p->osize;
    jl_ptls_t ptls2 = jl_all_tls_states[pg->thread_n];
    pg->pool_n = p - ptls2->heap.norm_pools;
    memset(pg->ages, 0, GC_PAGE_SZ / 8 / p->osize + 1);
    jl_taggedvalue_t *beg = (jl_taggedvalue_t*)(pg->data + GC_PAGE_OFFSET);
    jl_taggedvalue_t *next = (jl_taggedvalue_t*)pg->data;
    next->next = fl;
    pg->has_young = 0;
    pg->has_marked = 0;
    pg->fl_begin_offset = -1;
    pg->fl_end_offset = -1;
    return beg;
}

// Add a new page to the pool. Discards any pages in `p->newpages` before.
static NOINLINE jl_taggedvalue_t *add_page(jl_gc_pool_t *p)
{
    // Do not pass in `ptls` as argument. This slows down the fast path
    // in pool_alloc significantly
    jl_ptls_t ptls = jl_get_ptls_states();
    char *data = (char*)jl_gc_alloc_page();
    if (data == NULL)
        jl_throw(jl_memory_exception);
    jl_gc_pagemeta_t *pg = page_metadata(data + GC_PAGE_OFFSET);
    pg->data = data;
    pg->osize = p->osize;
    pg->ages = (uint8_t*)malloc(GC_PAGE_SZ / 8 / p->osize + 1);
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
#ifdef JULIA_ENABLE_THREADING
    assert(ptls->gc_state == 0);
#endif
#ifdef MEMDEBUG
    return jl_gc_big_alloc(ptls, osize);
#endif
    // FIXME - need JL_ATOMIC_FETCH_AND_ADD here
    if (__unlikely((gc_num.allocd += osize) >= 0) || gc_debug_check_pool()) {
        //gc_num.allocd -= osize;
        jl_gc_collect(0);
        //gc_num.allocd += osize;
    }
    else {
        jl_gc_safepoint_(ptls);
    }
    gc_num.poolalloc++;
    // first try to use the freelist
    jl_taggedvalue_t *v = p->freelist;
    if (v) {
        jl_taggedvalue_t *next = v->next;
        p->freelist = next;
        if (__unlikely(gc_page_data(v) != gc_page_data(next))) {
            // we only update pg's fields when the freelist changes page
            // since pg's metadata is likely not in cache
            jl_gc_pagemeta_t *pg = page_metadata(v);
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
            jl_gc_pagemeta_t *pg = page_metadata((char*)v - 1);
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
static jl_taggedvalue_t **sweep_page(jl_gc_pool_t *p, jl_gc_pagemeta_t *pg, jl_taggedvalue_t **pfl, int sweep_full, int osize)
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
        // lazy version: (empty) if the whole page was already unused, free it
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

static void sweep_pool_region(jl_taggedvalue_t ***pfl, int region_i, int sweep_full)
{
    region_t *region = &regions[region_i];

    // the actual sweeping
    int ub = 0;
    int lb = region->lb;
    for (int pg_i = 0; pg_i <= region->ub; pg_i++) {
        uint32_t line = region->allocmap[pg_i];
        if (line) {
            ub = pg_i;
            for (int j = 0; j < 32; j++) {
                if ((line >> j) & 1) {
                    jl_gc_pagemeta_t *pg = &region->meta[pg_i*32 + j];
                    int p_n = pg->pool_n;
                    int t_n = pg->thread_n;
                    jl_ptls_t ptls2 = jl_all_tls_states[t_n];
                    jl_gc_pool_t *p = &ptls2->heap.norm_pools[p_n];
                    int osize = pg->osize;
                    pfl[t_n * JL_GC_N_POOLS + p_n] = sweep_page(p, pg, pfl[t_n * JL_GC_N_POOLS + p_n], sweep_full, osize);
                }
            }
        }
        else if (pg_i < lb) {
            lb = pg_i;
        }
    }
    region->ub = ub;
    region->lb = lb;
}

static void gc_sweep_other(jl_ptls_t ptls, int sweep_full)
{
    sweep_malloced_arrays();
    sweep_big(ptls, sweep_full);
}

static void gc_pool_sync_nfree(jl_gc_pagemeta_t *pg, jl_taggedvalue_t *last)
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

static void gc_sweep_pool(int sweep_full)
{
    gc_time_pool_start();
    lazy_freed_pages = 0;

    jl_taggedvalue_t ***pfl = (jl_taggedvalue_t ***) alloca(jl_n_threads * JL_GC_N_POOLS * sizeof(jl_taggedvalue_t**));

    // update metadata of pages that were pointed to by freelist or newpages from a pool
    // i.e. pages being the current allocation target
    for (int t_i = 0; t_i < jl_n_threads; t_i++) {
        jl_ptls_t ptls2 = jl_all_tls_states[t_i];
        for (int i = 0; i < JL_GC_N_POOLS; i++) {
            jl_gc_pool_t *p = &ptls2->heap.norm_pools[i];
            jl_taggedvalue_t *last = p->freelist;
            if (last) {
                jl_gc_pagemeta_t *pg = page_metadata(last);
                gc_pool_sync_nfree(pg, last);
                pg->has_young = 1;
            }
            p->freelist =  NULL;
            pfl[t_i * JL_GC_N_POOLS + i] = &p->freelist;

            last = p->newpages;
            if (last) {
                char *last_p = (char*)last;
                jl_gc_pagemeta_t *pg = page_metadata(last_p - 1);
                assert(last_p - gc_page_data(last_p - 1) >= GC_PAGE_OFFSET);
                pg->nfree = (GC_PAGE_SZ - (last_p - gc_page_data(last_p - 1))) / p->osize;
                pg->has_young = 1;
            }
            p->newpages = NULL;
        }
    }

    for (int i = 0; i < REGION_COUNT; i++) {
        if (!regions[i].pages)
            break;
        sweep_pool_region(pfl, i, sweep_full);
    }


    // null out terminal pointers of free lists
    for (int t_i = 0; t_i < jl_n_threads; t_i++) {
        for (int i = 0; i < JL_GC_N_POOLS; i++) {
            *pfl[t_i * JL_GC_N_POOLS + i] = NULL;
        }
    }

    gc_time_pool_end(sweep_full);
}

// mark phase

static jl_value_t **mark_stack = NULL;
static jl_value_t **mark_stack_base = NULL;
static size_t mark_stack_size = 0;
static size_t mark_sp = 0;

static void grow_mark_stack(void)
{
    size_t newsz = mark_stack_size>0 ? mark_stack_size*2 : 32000;
    size_t offset = mark_stack - mark_stack_base;
    mark_stack_base = (jl_value_t**)realloc(mark_stack_base, newsz*sizeof(void*));
    if (mark_stack_base == NULL) {
        jl_printf(JL_STDERR, "Couldn't grow mark stack to : %" PRIuPTR "\n",
                  (uintptr_t)newsz);
        exit(1);
    }
    mark_stack = mark_stack_base + offset;
    mark_stack_size = newsz;
}

JL_DLLEXPORT void jl_gc_queue_root(jl_value_t *ptr)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_taggedvalue_t *o = jl_astaggedvalue(ptr);
#ifndef JULIA_ENABLE_THREADING
    // Disable this assert since it can happen with multithreading (same
    // with the ones in gc_queue_binding) when two threads are writing
    // to the same object.
    assert(o->bits.gc == GC_OLD_MARKED);
#endif
    // The modification of the `gc_bits` is not atomic but it
    // should be safe here since GC is not allowed to run here and we only
    // write GC_OLD to the GC bits outside GC. This could cause
    // duplicated objects in the remset but that shouldn't be a problem.
    o->bits.gc = GC_MARKED;
    arraylist_push(ptls->heap.remset, ptr);
    ptls->heap.remset_nptr++; // conservative
}

void gc_queue_binding(jl_binding_t *bnd)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_taggedvalue_t *buf = jl_astaggedvalue(bnd);
#ifndef JULIA_ENABLE_THREADING
    // Will fail for multithreading. See `jl_gc_queue_root`
    assert(buf->bits.gc == GC_OLD_MARKED);
#endif
    buf->bits.gc = GC_MARKED;
    arraylist_push(&ptls->heap.rem_bindings, bnd);
}

static void gc_scan_obj(jl_ptls_t ptls, jl_value_t *v, int d, uintptr_t tag);
static uint16_t gc_mark_obj(jl_ptls_t ptls, jl_value_t *v, uintptr_t tag);
#ifdef JL_DEBUG_BUILD
static void *volatile gc_findval; // for usage from gdb, for finding the gc-root for a value
#endif
// Returns whether the object is young
static inline int gc_push_root(jl_ptls_t ptls, void *v, int d) // v isa jl_value_t*
{
#ifdef JL_DEBUG_BUILD
    if (v == gc_findval)
        jl_raise_debugger();
#endif
    assert(v != NULL);
    jl_taggedvalue_t *o = jl_astaggedvalue(v);
    verify_val(v);
    const uintptr_t tag = o->header;
    if (!gc_marked(tag)) {
        uint16_t mark_res = gc_mark_obj(ptls, (jl_value_t*)v, tag);
        assert(gc_marked(o->header));
        if (mark_res >> 8)
            gc_scan_obj(ptls, (jl_value_t*)v, d,
                        gc_set_bits(tag, mark_res & 0xff));
        return !gc_old(mark_res);
    }
    return !gc_old(tag);
}

// TODO rename this as it is misleading now
void jl_gc_setmark(jl_ptls_t ptls, jl_value_t *v)
{
    jl_taggedvalue_t *o = jl_astaggedvalue(v);
    uintptr_t tag = o->header;
    if (!gc_marked(tag)) {
        gc_setmark_pool(ptls, o, GC_MARKED, tag);
    }
}

NOINLINE static int gc_mark_module(jl_ptls_t ptls, jl_module_t *m,
                                   int d, int8_t bits)
{
    size_t i;
    int refyoung = 0;
    void **table = m->bindings.table;
    for(i=1; i < m->bindings.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            gc_setmark_buf(ptls, b, bits, sizeof(jl_binding_t));
            void *vb = jl_astaggedvalue(b);
            verify_parent1("module", m, &vb, "binding_buff");
            (void)vb;
            if (b->value != NULL) {
                verify_parent2("module", m, &b->value, "binding(%s)",
                               jl_symbol_name(b->name));
                refyoung |= gc_push_root(ptls, b->value, d);
            }
            if (b->globalref != NULL)
                refyoung |= gc_push_root(ptls, b->globalref, d);
        }
    }
    // this is only necessary because bindings for "using" modules
    // are added only when accessed. therefore if a module is replaced
    // after "using" it but before accessing it, this array might
    // contain the only reference.
    for(i=0; i < m->usings.len; i++) {
        refyoung |= gc_push_root(ptls, m->usings.items[i], d);
    }

    if (m->parent) {
        refyoung |= gc_push_root(ptls, m->parent, d);
    }

    return refyoung;
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

static void gc_mark_stack(jl_ptls_t ptls, jl_value_t *ta, jl_gcframe_t *s,
                          uintptr_t offset, uintptr_t lb, uintptr_t ub, int d)
{
    while (s != NULL) {
        jl_value_t ***rts = (jl_value_t***)(((void**)s) + 2);
        size_t nroots = gc_read_stack(&s->nroots, offset, lb, ub);
        size_t nr = nroots >> 1;
        if (nroots & 1) {
            for (size_t i = 0; i < nr; i++) {
                void **slot = (void**)gc_read_stack(&rts[i], offset, lb, ub);
                void *obj = (void*)gc_read_stack(slot, offset, lb, ub);
                if (obj != NULL) {
                    gc_push_root(ptls, obj, d);
                }
            }
        }
        else {
            for (size_t i=0; i < nr; i++) {
                void *obj = (void*)gc_read_stack(&rts[i], offset, lb, ub);
                if (obj) {
                    verify_parent2("task", ta,
                                   gc_get_stack_addr(&rts[i], offset, lb, ub),
                                   "stack(%d)", (int)i);
                    gc_push_root(ptls, obj, d);
                }
            }
        }
        s = (jl_gcframe_t*)gc_read_stack(&s->prev, offset, lb, ub);
    }
}

static void gc_mark_task_stack(jl_ptls_t ptls, jl_task_t *ta, int d, int8_t bits)
{
    gc_scrub_record_task(ta);
    int stkbuf = (ta->stkbuf != (void*)(intptr_t)-1 && ta->stkbuf != NULL);
    int16_t tid = ta->tid;
    jl_ptls_t ptls2 = jl_all_tls_states[tid];
    if (stkbuf) {
#ifdef COPY_STACKS
        gc_setmark_buf(ptls, ta->stkbuf, bits, ta->bufsz);
#else
        // stkbuf isn't owned by julia for the root task
        if (ta != ptls2->root_task) {
            gc_setmark_buf(ptls, ta->stkbuf, bits, ta->ssize);
        }
#endif
    }
    if (ta == ptls2->current_task) {
        gc_mark_stack(ptls, (jl_value_t*)ta, ptls2->pgcstack,
                      0, 0, (uintptr_t)-1, d);
    }
    else if (stkbuf) {
        uintptr_t offset = 0;
        uintptr_t lb = 0;
        uintptr_t ub = (uintptr_t)-1;
#ifdef COPY_STACKS
        ub = (uintptr_t)ptls2->stackbase;
        lb = ub - ta->ssize;
        offset = (uintptr_t)ta->stkbuf - lb;
#endif
        gc_mark_stack(ptls, (jl_value_t*)ta, ta->gcstack, offset, lb, ub, d);
    }
}

NOINLINE static void gc_mark_task(jl_ptls_t ptls, jl_task_t *ta,
                                  int d, int8_t bits)
{
    if (ta->parent) gc_push_root(ptls, ta->parent, d);
    gc_push_root(ptls, ta->tls, d);
    gc_push_root(ptls, ta->consumers, d);
    gc_push_root(ptls, ta->donenotify, d);
    gc_push_root(ptls, ta->exception, d);
    if (ta->backtrace) gc_push_root(ptls, ta->backtrace, d);
    if (ta->start)  gc_push_root(ptls, ta->start, d);
    if (ta->result) gc_push_root(ptls, ta->result, d);
    gc_mark_task_stack(ptls, ta, d, bits);
}

void gc_mark_object_list(jl_ptls_t ptls, arraylist_t *list, size_t start)
{
    void **items = list->items;
    size_t len = list->len;
    for (size_t i = start;i < len;i++) {
        void *v = items[i];
        if (__unlikely(!v))
            continue;
        if (gc_ptr_tag(v, 1)) {
            v = gc_ptr_clear_tag(v, 1);
            i++;
            assert(i < len);
        }
        gc_push_root(ptls, v, 0);
    }
}

STATIC_INLINE void gc_assert_datatype(jl_datatype_t *vt)
{
    if (__likely(jl_is_datatype(vt)))
        return;
    jl_printf(JL_STDOUT, "GC error (probable corruption) :\n");
    gc_debug_print_status();
    jl_(vt);
    gc_debug_critical_error();
    abort();
}

// for chasing down unwanted references
/*
static jl_value_t *lookforme = NULL;
JL_DLLEXPORT void jl_gc_lookfor(jl_value_t *v) { lookforme = v; }
*/

#define MAX_MARK_DEPTH 400
// Scan a marked object `v` and recursively mark its children.
// The object will be queued on the mark stack when recursion depth
// becomes too high.
// It does so assuming that the tag of the (marked) object is `tag`.
// If `v` is `GC_OLD_MARKED` and some of its children are `GC_MARKED` (young),
// `v` is added to the remset
static void gc_scan_obj(jl_ptls_t ptls, jl_value_t *v, int d, uintptr_t tag)
{
    assert(v != NULL);
    assert(gc_marked(tag));
    jl_datatype_t *vt = (jl_datatype_t*)(tag & ~(uintptr_t)15);
#ifdef JL_DEBUG_BUILD
    gc_assert_datatype(vt); // should have checked in `gc_mark_obj`
#endif
    int refyoung = 0, nptr = 0;
    const int8_t bits = tag & 0xf;

    assert(vt != jl_symbol_type);
    // weakref should not be marked
    if (vt == jl_weakref_type)
        return;
    // fast path
    if (vt->layout->pointerfree)
        return;
    d++;
    if (d >= MAX_MARK_DEPTH)
        goto queue_the_root;

    // some values have special representations
    if (vt == jl_simplevector_type) {
        size_t l = jl_svec_len(v);
        jl_value_t **data = jl_svec_data(v);
        nptr += l;
        for(size_t i=0; i < l; i++) {
            jl_value_t *elt = data[i];
            if (elt != NULL) {
                verify_parent2("svec", v, &data[i], "elem(%d)", (int)i);
                refyoung |= gc_push_root(ptls, elt, d);
            }
        }
    }
    else if (vt->name == jl_array_typename) {
        jl_array_t *a = (jl_array_t*)v;
        jl_array_flags_t flags = a->flags;
        if (flags.how == 3) {
            jl_value_t *owner = jl_array_data_owner(a);
            refyoung |= gc_push_root(ptls, owner, d);
            goto ret;
        }
        else if (flags.how == 1) {
            void *val_buf = jl_astaggedvalue((char*)a->data - a->offset*a->elsize);
            verify_parent1("array", v, &val_buf, "buffer ('loc' addr is meaningless)");
            (void)val_buf;
            gc_setmark_buf(ptls, (char*)a->data - a->offset*a->elsize,
                           bits, array_nbytes(a));
        }
        if (flags.ptrarray && a->data != NULL) {
            size_t l = jl_array_len(a);
            if (l > 100000 && d > MAX_MARK_DEPTH-10) {
                // don't mark long arrays at high depth, to try to avoid
                // copying the whole array into the mark queue
                goto queue_the_root;
            }
            else {
                nptr += l;
                void *data = a->data;
                for (size_t i=0; i < l; i++) {
                    jl_value_t *elt = ((jl_value_t**)data)[i];
                    if (elt != NULL) {
                        verify_parent2("array", v, &((jl_value_t**)data)[i], "elem(%d)", (int)i);
                        refyoung |= gc_push_root(ptls, elt, d);
                    }
                    // try to split large array marking (incremental mark TODO)
                    // if (should_timeout() && l > 1000) goto queue_the_root;
                }
            }
        }
    }
    else if (vt == jl_module_type) {
        // should increase nptr here
        refyoung |= gc_mark_module(ptls, (jl_module_t*)v, d, bits);
    }
    else if (vt == jl_task_type) {
        // ditto nptr
        gc_mark_task(ptls, (jl_task_t*)v, d, bits);
        // tasks should always be remarked since we do not trigger the write barrier
        // for stores to stack slots
        refyoung = 1;
    }
    else {
        int nf = (int)jl_datatype_nfields(vt);
        for(int i=0; i < nf; i++) {
            if (jl_field_isptr(vt, i)) {
                nptr++;
                jl_value_t **slot = (jl_value_t**)((char*)v +
                                                   jl_field_offset(vt, i));
                jl_value_t *fld = *slot;
                if (fld) {
                    verify_parent2("object", v, slot, "field(%d)", i);
                    refyoung |= gc_push_root(ptls, fld, d);
                }
            }
        }
    }

ret:
    if ((bits == GC_OLD_MARKED) && refyoung && !gc_verifying) {
        ptls->heap.remset_nptr += nptr;
        // v is an old object referencing young objects
        arraylist_push(ptls->heap.remset, v);
    }
    return;

queue_the_root:
    if (mark_sp >= mark_stack_size)
        grow_mark_stack();
    mark_stack[mark_sp++] = (jl_value_t*)v;
}

// Mark an object (without scanning it)
// The top `int8_t` of the return value is set to `1` if the object was not
// marked before and the object needs to be scanned.
// Returning `0` in these bits can happen if another thread
// marked it in parallel or if the object is known to not reference
// any object.
// The bottom `int8_t` of the return value is the new GC bits.
static uint16_t gc_mark_obj(jl_ptls_t ptls, jl_value_t *v, uintptr_t tag)
{
    assert(v != NULL);
    assert(!gc_marked(tag));
    jl_datatype_t *vt = (jl_datatype_t*)(tag & ~(uintptr_t)15);
    gc_assert_datatype(vt);
    // Symbols are always marked
    assert(vt != jl_symbol_type);
    // Do not initialize `mark_res` to catch branches forgetting to set `mark_res`
    // using compiler warnings.
    uint16_t mark_res;

    // some values have special representations
    if (vt == jl_simplevector_type) {
        size_t l = jl_svec_len(v);
        mark_res = gc_setmark(ptls, v, l * sizeof(void*) + sizeof(jl_svec_t),
                              tag);
    }
    else if (vt->name == jl_array_typename) {
        jl_array_t *a = (jl_array_t*)v;
        jl_taggedvalue_t *o = jl_astaggedvalue(v);
        jl_array_flags_t flags = a->flags;
        mark_res = (flags.pooled ? gc_setmark_pool(ptls, o, GC_MARKED, tag) :
                    gc_setmark_big(ptls, o, GC_MARKED, tag));
        if (flags.how == 2 && (mark_res >> 8)) {
            uint8_t bits = mark_res & 0xff;
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
    else if (vt == jl_module_type) {
        mark_res = gc_setmark(ptls, v, sizeof(jl_module_t), tag);
    }
    else if (vt == jl_task_type) {
        mark_res = gc_setmark(ptls, v, sizeof(jl_task_t), tag);
    }
    else if (vt == jl_string_type) {
        // String cannot reference any object.
        mark_res = gc_setmark(ptls, v, jl_string_len(v) + sizeof(size_t) + 1,
                              tag) & 0xff;
    }
    else {
        mark_res = gc_setmark(ptls, v, jl_datatype_size(vt), tag);
    }
    return mark_res;
}

void visit_mark_stack(jl_ptls_t ptls)
{
    while (mark_sp > 0 && !should_timeout()) {
        jl_value_t *v = mark_stack[--mark_sp];
        assert(jl_astaggedvalue(v)->bits.gc);
        gc_scan_obj(ptls, v, 0, jl_astaggedvalue(v)->header);
    }
    assert(!mark_sp);
}

extern jl_array_t *jl_module_init_order;
extern jl_typemap_entry_t *call_cache[N_CALL_CACHE];
extern jl_array_t *jl_all_methods;

static void jl_gc_mark_thread_local(jl_ptls_t ptls, jl_ptls_t ptls2)
{
    // `current_module` might not have a value when the thread is not
    // running.
    if (ptls2->current_module)
        gc_push_root(ptls, ptls2->current_module, 0);
    gc_push_root(ptls, ptls2->current_task, 0);
    gc_push_root(ptls, ptls2->root_task, 0);
    gc_push_root(ptls, ptls2->exception_in_transit, 0);
    gc_push_root(ptls, ptls2->task_arg_in_transit, 0);
}

// mark the initial root set
static void mark_roots(jl_ptls_t ptls)
{
    // modules
    gc_push_root(ptls, jl_main_module, 0);
    gc_push_root(ptls, jl_internal_main_module, 0);

    // invisible builtin values
    if (jl_an_empty_vec_any != NULL)
        gc_push_root(ptls, jl_an_empty_vec_any, 0);
    if (jl_module_init_order != NULL)
        gc_push_root(ptls, jl_module_init_order, 0);
    gc_push_root(ptls, jl_cfunction_list.unknown, 0);
    gc_push_root(ptls, jl_anytuple_type_type, 0);
    gc_push_root(ptls, jl_ANY_flag, 0);
    for (size_t i = 0; i < N_CALL_CACHE; i++)
        if (call_cache[i])
            gc_push_root(ptls, call_cache[i], 0);
    if (jl_all_methods != NULL)
        gc_push_root(ptls, jl_all_methods, 0);

    // gc_push_root(ptls, jl_unprotect_stack_func, 0);

    // constants
    gc_push_root(ptls, jl_typetype_type, 0);
    gc_push_root(ptls, jl_emptytuple_type, 0);
}

// find unmarked objects that need to be finalized from the finalizer list "list".
// this must happen last in the mark phase.
static void sweep_finalizer_list(arraylist_t *list)
{
    void **items = list->items;
    size_t len = list->len;
    for (size_t i=0; i < len; i+=2) {
        void *v0 = items[i];
        int is_cptr = gc_ptr_tag(v0, 1);
        void *v = gc_ptr_clear_tag(v0, 1);
        if (__unlikely(!v0)) {
            // remove from this list
            if (i < len - 2) {
                items[i] = items[len - 2];
                items[i + 1] = items[len - 1];
                i -= 2;
            }
            len -= 2;
            continue;
        }

        void *fin = items[i+1];
        int isfreed = !gc_marked(jl_astaggedvalue(v)->bits.gc);
        int isold = (list != &finalizer_list_marked &&
                     jl_astaggedvalue(v)->bits.gc == GC_OLD_MARKED &&
                     (is_cptr || jl_astaggedvalue(fin)->bits.gc == GC_OLD_MARKED));
        if (isfreed || isold) {
            // remove from this list
            if (i < len - 2) {
                items[i] = items[len - 2];
                items[i + 1] = items[len - 1];
                i -= 2;
            }
            len -= 2;
        }
        if (isfreed) {
            // schedule finalizer or execute right away if it is not julia code
            if (is_cptr) {
                ((void (*)(void*))fin)(jl_data_ptr(v));
                continue;
            }
            schedule_finalization(v, fin);
        }
        if (isold) {
            // The caller relies on the new objects to be pushed to the end of
            // the list!!
            arraylist_push(&finalizer_list_marked, v0);
            arraylist_push(&finalizer_list_marked, fin);
        }
    }
    list->len = len;
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

JL_DLLEXPORT int64_t jl_gc_total_bytes(void)
{
    // Sync this logic with `base/util.jl:GC_Diff`
    return (gc_num.total_allocd + gc_num.deferred_alloc +
            gc_num.allocd + gc_num.interval);
}
JL_DLLEXPORT uint64_t jl_gc_total_hrtime(void)
{
    return gc_num.total_time;
}
JL_DLLEXPORT jl_gc_num_t jl_gc_num(void)
{
    return gc_num;
}

JL_DLLEXPORT int64_t jl_gc_diff_total_bytes(void)
{
    int64_t oldtb = last_gc_total_bytes;
    int64_t newtb = jl_gc_total_bytes();
    last_gc_total_bytes = newtb;
    return newtb - oldtb;
}
void jl_gc_sync_total_bytes(void) {last_gc_total_bytes = jl_gc_total_bytes();}

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

static void jl_gc_mark_remset(jl_ptls_t ptls, jl_ptls_t ptls2)
{
    size_t len = ptls2->heap.last_remset->len;
    void **items = ptls2->heap.last_remset->items;
    for (size_t i = 0; i < len; i++) {
        jl_value_t *item = (jl_value_t*)items[i];
        gc_scan_obj(ptls, item, 0, jl_astaggedvalue(item)->header);
    }
    int n_bnd_refyoung = 0;
    len = ptls2->heap.rem_bindings.len;
    items = ptls2->heap.rem_bindings.items;
    for (size_t i = 0; i < len; i++) {
        jl_binding_t *ptr = (jl_binding_t*)items[i];
        // A null pointer can happen here when the binding is cleaned up
        // as an exception is thrown after it was already queued (#10221)
        if (!ptr->value) continue;
        if (gc_push_root(ptls, ptr->value, 0)) {
            items[n_bnd_refyoung] = ptr;
            n_bnd_refyoung++;
        }
    }
    ptls2->heap.rem_bindings.len = n_bnd_refyoung;
}

static void jl_gc_mark_ptrfree(jl_ptls_t ptls)
{
    // Pointer-free objects, can be marked concurrently
    jl_mark_box_caches(ptls);
    jl_gc_setmark(ptls, (jl_value_t*)jl_emptysvec);
    jl_gc_setmark(ptls, jl_emptytuple);
    jl_gc_setmark(ptls, jl_true);
    jl_gc_setmark(ptls, jl_false);
}

// Only one thread should be running in this function
static int _jl_gc_collect(jl_ptls_t ptls, int full)
{
    uint64_t t0 = jl_hrtime();
    int64_t last_perm_scanned_bytes = perm_scanned_bytes;
    assert(mark_sp == 0);

    // 1. fix GC bits of objects in the remset.
    for (int t_i = 0; t_i < jl_n_threads; t_i++)
        jl_gc_premark(jl_all_tls_states[t_i]);

    for (int t_i = 0; t_i < jl_n_threads; t_i++) {
        jl_ptls_t ptls2 = jl_all_tls_states[t_i];
        // 2.1. mark every object in the `last_remsets` and `rem_binding`
        jl_gc_mark_remset(ptls, ptls2);
        // 2.2. mark every thread local root
        jl_gc_mark_thread_local(ptls, ptls2);
    }

    // 3. walk roots
    mark_roots(ptls);
    visit_mark_stack(ptls);
    gc_num.since_sweep += gc_num.allocd + (int64_t)gc_num.interval;
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
        gc_mark_object_list(ptls, &ptls2->finalizers, 0);
    }
    gc_mark_object_list(ptls, &finalizer_list_marked, orig_marked_len);
    // "Flush" the mark stack before flipping the reset_age bit
    // so that the objects are not incorrectly resetted.
    visit_mark_stack(ptls);
    mark_reset_age = 1;
    // Reset the age and old bit for any unmarked objects referenced by the
    // `to_finalize` list. These objects are only reachable from this list
    // and should not be referenced by any old objects so this won't break
    // the GC invariant.
    gc_mark_object_list(ptls, &to_finalize, 0);
    visit_mark_stack(ptls);
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
    if ((full || large_frontier ||
         ((not_freed_enough || promoted_bytes >= gc_num.interval) &&
          (promoted_bytes >= default_collect_interval || prev_sweep_full)) ||
         gc_check_heap_size(live_sz_ub, live_sz_est)) &&
        gc_num.pause > 1) {
        gc_update_heap_size(live_sz_ub, live_sz_est);
        recollect = full;
        if (large_frontier)
            gc_num.interval = last_long_collect_interval;
        if (not_freed_enough || large_frontier) {
            if (gc_num.interval < default_collect_interval) {
                gc_num.interval = default_collect_interval;
            }
            else if (gc_num.interval <= 2*(max_collect_interval/5)) {
                gc_num.interval = 5 * (gc_num.interval / 2);
            }
        }
        last_long_collect_interval = gc_num.interval;
        sweep_full = 1;
        promoted_bytes = 0;
    }
    else {
        gc_num.interval = default_collect_interval / 2;
        sweep_full = gc_sweep_always_full;
    }
    if (sweep_full)
        perm_scanned_bytes = 0;
    scanned_bytes = 0;
    // 5. start sweeping
    sweep_weak_refs();
    gc_sweep_other(ptls, sweep_full);
    gc_scrub();
    gc_verify_tags();
    gc_sweep_pool(sweep_full);
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

    uint64_t gc_end_t = jl_hrtime();
    uint64_t pause = gc_end_t - t0;
    gc_final_pause_end(t0, gc_end_t);
    gc_time_sweep_pause(gc_end_t, actual_allocd, live_bytes,
                        estimate_freed, sweep_full);
    gc_num.full_sweep += sweep_full;
    prev_sweep_full = sweep_full;
    gc_num.allocd = -(int64_t)gc_num.interval;
    live_bytes += -gc_num.freed + gc_num.since_sweep;
    gc_num.pause += !recollect;
    gc_num.total_time += pause;
    gc_num.since_sweep = 0;
    gc_num.freed = 0;

    return recollect;
}

JL_DLLEXPORT void jl_gc_collect(int full)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (jl_gc_disable_counter) {
        gc_num.deferred_alloc += (gc_num.allocd + gc_num.interval);
        gc_num.allocd = -(int64_t)gc_num.interval;
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
    // Now we are ready to wait for other threads to hit the safepoint,
    // we can do a few things that doesn't require synchronization.
    jl_gc_mark_ptrfree(ptls);
    // no-op for non-threading
    jl_gc_wait_for_the_world();

    if (!jl_gc_disable_counter) {
        JL_LOCK_NOGC(&finalizers_lock);
        if (_jl_gc_collect(ptls, full)) {
            jl_gc_mark_ptrfree(ptls);
            int ret = _jl_gc_collect(ptls, 0);
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
}

void mark_all_roots(jl_ptls_t ptls)
{
    for (size_t i = 0; i < jl_n_threads; i++)
        jl_gc_mark_thread_local(ptls, jl_all_tls_states[i]);
    mark_roots(ptls);
    jl_gc_mark_ptrfree(ptls);
}

// allocator entry points

JL_DLLEXPORT jl_value_t *(jl_gc_alloc)(jl_ptls_t ptls, size_t sz, void *ty)
{
    return jl_gc_alloc_(ptls, sz, ty);
}

// Per-thread initialization (when threading is fully implemented)
void jl_mk_thread_heap(jl_ptls_t ptls)
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
    heap->mallocarrays = NULL;
    heap->mafreelist = NULL;
    heap->big_objects = NULL;
    arraylist_new(&heap->rem_bindings, 0);
    heap->remset = &heap->_remset[0];
    heap->last_remset = &heap->_remset[1];
    arraylist_new(heap->remset, 0);
    arraylist_new(heap->last_remset, 0);
    arraylist_new(&ptls->finalizers, 0);
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
    gc_num.allocd = -default_collect_interval;

#ifdef _P64
    // on a big memory machine, set max_collect_interval to totalmem/ncores/2
    size_t maxmem = (uv_get_total_memory()/jl_cpu_cores())/2;
    if (maxmem > max_collect_interval)
        max_collect_interval = maxmem;
#endif
}

JL_DLLEXPORT void *jl_gc_counted_malloc(size_t sz)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    sz += JL_SMALL_BYTE_ALIGNMENT;
    maybe_collect(ptls);
    gc_num.allocd += sz;
    gc_num.malloc++;
    void *b = malloc(sz);
    if (b == NULL)
        jl_throw(jl_memory_exception);
    return b;
}

JL_DLLEXPORT void *jl_gc_counted_calloc(size_t nm, size_t sz)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    nm += JL_SMALL_BYTE_ALIGNMENT;
    maybe_collect(ptls);
    gc_num.allocd += nm*sz;
    gc_num.malloc++;
    void *b = calloc(nm, sz);
    if (b == NULL)
        jl_throw(jl_memory_exception);
    return b;
}

JL_DLLEXPORT void jl_gc_counted_free(void *p, size_t sz)
{
    free(p);
    gc_num.freed += sz + JL_SMALL_BYTE_ALIGNMENT;
    gc_num.freecall++;
}

JL_DLLEXPORT void *jl_gc_counted_realloc_with_old_size(void *p, size_t old, size_t sz)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    old += JL_SMALL_BYTE_ALIGNMENT;
    sz += JL_SMALL_BYTE_ALIGNMENT;
    maybe_collect(ptls);
    if (sz < old)
       gc_num.freed += (old - sz);
    else
       gc_num.allocd += (sz - old);
    gc_num.realloc++;
    void *b = realloc(p, sz);
    if (b == NULL)
        jl_throw(jl_memory_exception);
    return b;
}

JL_DLLEXPORT void *jl_malloc(size_t sz)
{
    int64_t *p = (int64_t *)jl_gc_counted_malloc(sz);
    p[0] = sz;
    return (void *)(p + 2);
}

JL_DLLEXPORT void *jl_calloc(size_t nm, size_t sz)
{
    int64_t *p;
    size_t nmsz = nm*sz;
    p = (int64_t *)jl_gc_counted_calloc(nmsz, 1);
    p[0] = nmsz;
    return (void *)(p + 2);
}

JL_DLLEXPORT void jl_free(void *p)
{
    if (p != NULL) {
        int64_t *pp = (int64_t *)p - 2;
        size_t sz = pp[0];
        jl_gc_counted_free(pp, sz);
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
        szold = pp[0];
    }
    int64_t *pnew = (int64_t *)jl_gc_counted_realloc_with_old_size(pp, szold, sz);
    pnew[0] = sz;
    return (void *)(pnew + 2);
}

JL_DLLEXPORT void *jl_gc_managed_malloc(size_t sz)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    maybe_collect(ptls);
    size_t allocsz = LLT_ALIGN(sz, JL_CACHE_BYTE_ALIGNMENT);
    if (allocsz < sz)  // overflow in adding offs, size was "negative"
        jl_throw(jl_memory_exception);
    gc_num.allocd += allocsz;
    gc_num.malloc++;
    void *b = malloc_cache_align(allocsz);
    if (b == NULL)
        jl_throw(jl_memory_exception);
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
        gc_num.freed += (oldsz - allocsz);
    else
        gc_num.allocd += (allocsz - oldsz);
    gc_num.realloc++;

    void *b;
    if (isaligned)
        b = realloc_cache_align(d, allocsz, oldsz);
    else
        b = realloc(d, allocsz);
    if (b == NULL)
        jl_throw(jl_memory_exception);

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
    size_t offs = offsetof(bigval_t, header);
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
    bigval_t *newbig =
        (bigval_t*)gc_managed_realloc_(ptls, hdr, allocsz, LLT_ALIGN(strsz+offs, JL_CACHE_BYTE_ALIGNMENT),
                                       1, s, 0);
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
static char *gc_perm_pool = NULL;
static size_t gc_perm_size = 0;

// **NOT** a safepoint
void *jl_gc_perm_alloc_nolock(size_t sz)
{
    // The caller should have acquired `gc_perm_lock`
#ifndef MEMDEBUG
    if (__unlikely(sz > GC_PERM_POOL_LIMIT))
#endif
        return malloc(sz);
    sz = LLT_ALIGN(sz, JL_SMALL_BYTE_ALIGNMENT);
    if (__unlikely(sz > gc_perm_size)) {
#ifdef _OS_WINDOWS_
        void *pool = VirtualAlloc(NULL,
                                  GC_PERM_POOL_SIZE + JL_SMALL_BYTE_ALIGNMENT,
                                  MEM_COMMIT, PAGE_READWRITE);
        if (__unlikely(pool == NULL))
            return NULL;
        pool = (void*)LLT_ALIGN((uintptr_t)pool, JL_SMALL_BYTE_ALIGNMENT);
#else
        void *pool = mmap(0, GC_PERM_POOL_SIZE, PROT_READ | PROT_WRITE,
                          MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
        if (__unlikely(pool == MAP_FAILED))
            return NULL;
#endif
        gc_perm_pool = (char*)pool;
        gc_perm_size = GC_PERM_POOL_SIZE;
    }
    assert(((uintptr_t)gc_perm_pool) % JL_SMALL_BYTE_ALIGNMENT == 0);
    void *p = gc_perm_pool;
    gc_perm_size -= sz;
    gc_perm_pool += sz;
    return p;
}

// **NOT** a safepoint
void *jl_gc_perm_alloc(size_t sz)
{
#ifndef MEMDEBUG
    if (__unlikely(sz > GC_PERM_POOL_LIMIT))
#endif
        return malloc(sz);
    JL_LOCK_NOGC(&gc_perm_lock);
    void *p = jl_gc_perm_alloc_nolock(sz);
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

#ifdef __cplusplus
}
#endif

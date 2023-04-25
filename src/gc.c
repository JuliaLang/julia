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

// Protect all access to `finalizer_list_marked` and `to_finalize`.
// For accessing `ptls->finalizers`, the lock is needed if a thread
// is going to realloc the buffer (of its own list) or accessing the
// list of another thread
static jl_mutex_t finalizers_lock;
static uv_mutex_t gc_cache_lock;

// mutex for gc-heap-snapshot.
jl_mutex_t heapsnapshot_lock;

// Flag that tells us whether we need to support conservative marking
// of objects.
static _Atomic(int) support_conservative_marking = 0;

/**
 * Note about GC synchronization:
 *
 * When entering `jl_gc_collect()`, `jl_gc_running` is atomically changed from
 * `0` to `1` to make sure that only one thread can be running the GC. Other
 * threads that enters `jl_gc_collect()` at the same time (or later calling
 * from unmanaged code) will wait in `jl_gc_collect()` until the GC is finished.
 *
 * Before starting the mark phase the GC thread calls `jl_safepoint_start_gc()`
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

jl_gc_num_t gc_num = {0};
static size_t last_long_collect_interval;
int gc_n_threads;
jl_ptls_t* gc_all_tls_states;
const uint64_t _jl_buff_tag[3] = {0x4eadc0004eadc000ull, 0x4eadc0004eadc000ull, 0x4eadc0004eadc000ull}; // aka 0xHEADER00
JL_DLLEXPORT uintptr_t jl_get_buff_tag(void)
{
    return jl_buff_tag;
}

pagetable_t memory_map;

// List of marked big objects.  Not per-thread.  Accessed only by master thread.
bigval_t *big_objects_marked = NULL;

// Eytzinger tree of images. Used for very fast jl_object_in_image queries during gc
// See https://algorithmica.org/en/eytzinger
static arraylist_t eytzinger_image_tree;
static arraylist_t eytzinger_idxs;
static uintptr_t gc_img_min;
static uintptr_t gc_img_max;

// -- Finalization --
// `ptls->finalizers` and `finalizer_list_marked` might have tagged pointers.
// If an object pointer has the lowest bit set, the next pointer is an unboxed c function pointer.
// If an object pointer has the second lowest bit set, the current pointer is a c object pointer.
//   It must be aligned at least 4, and it finalized immediately (at "quiescence").
// `to_finalize` should not have tagged pointers.
arraylist_t finalizer_list_marked;
arraylist_t to_finalize;
JL_DLLEXPORT _Atomic(int) jl_gc_have_pending_finalizers = 0;

static int ptr_cmp(const void *l, const void *r)
{
    uintptr_t left = *(const uintptr_t*)l;
    uintptr_t right = *(const uintptr_t*)r;
    // jl_safe_printf("cmp %p %p\n", (void*)left, (void*)right);
    return (left > right) - (left < right);
}

// Build an eytzinger tree from a sorted array
static int eytzinger(uintptr_t *src, uintptr_t *dest, size_t i, size_t k, size_t n)
{
    if (k <= n) {
        i = eytzinger(src, dest, i, 2 * k, n);
        dest[k-1] = src[i];
        i++;
        i = eytzinger(src, dest, i, 2 * k + 1, n);
    }
    return i;
}

static size_t eyt_obj_idx(jl_value_t *obj) JL_NOTSAFEPOINT
{
    size_t n = eytzinger_image_tree.len - 1;
    if (n == 0)
        return n;
    assert(n % 2 == 0 && "Eytzinger tree not even length!");
    uintptr_t cmp = (uintptr_t) obj;
    if (cmp <= gc_img_min || cmp > gc_img_max)
        return n;
    uintptr_t *tree = (uintptr_t*)eytzinger_image_tree.items;
    size_t k = 1;
    // note that k preserves the history of how we got to the current node
    while (k <= n) {
        int greater = (cmp > tree[k - 1]);
        k <<= 1;
        k |= greater;
    }
    // Free to assume k is nonzero, since we start with k = 1
    // and cmp > gc_img_min
    // This shift does a fast revert of the path until we get
    // to a node that evaluated less than cmp.
    k >>= (__builtin_ctzll(k) + 1);
    assert(k != 0);
    assert(k <= n && "Eytzinger tree index out of bounds!");
    assert(tree[k - 1] < cmp && "Failed to find lower bound for object!");
    return k - 1;
}

//used in staticdata.c after we add an image
void rebuild_image_blob_tree(void)
{
    size_t inc = 1 + jl_linkage_blobs.len - eytzinger_image_tree.len;
    assert(eytzinger_idxs.len == eytzinger_image_tree.len);
    assert(eytzinger_idxs.max == eytzinger_image_tree.max);
    arraylist_grow(&eytzinger_idxs, inc);
    arraylist_grow(&eytzinger_image_tree, inc);
    eytzinger_idxs.items[eytzinger_idxs.len - 1] = (void*)jl_linkage_blobs.len;
    eytzinger_image_tree.items[eytzinger_image_tree.len - 1] = (void*)1; // outside image
    for (size_t i = 0; i < jl_linkage_blobs.len; i++) {
        assert((uintptr_t) jl_linkage_blobs.items[i] % 4 == 0 && "Linkage blob not 4-byte aligned!");
        // We abuse the pointer here a little so that a couple of properties are true:
        // 1. a start and an end are never the same value. This simplifies the binary search.
        // 2. ends are always after starts. This also simplifies the binary search.
        // We assume that there exist no 0-size blobs, but that's a safe assumption
        // since it means nothing could be there anyways
        uintptr_t val = (uintptr_t) jl_linkage_blobs.items[i];
        eytzinger_idxs.items[i] = (void*)(val + (i & 1));
    }
    qsort(eytzinger_idxs.items, eytzinger_idxs.len - 1, sizeof(void*), ptr_cmp);
    gc_img_min = (uintptr_t) eytzinger_idxs.items[0];
    gc_img_max = (uintptr_t) eytzinger_idxs.items[eytzinger_idxs.len - 2] + 1;
    eytzinger((uintptr_t*)eytzinger_idxs.items, (uintptr_t*)eytzinger_image_tree.items, 0, 1, eytzinger_idxs.len - 1);
    // Reuse the scratch memory to store the indices
    // Still O(nlogn) because binary search
    for (size_t i = 0; i < jl_linkage_blobs.len; i ++) {
        uintptr_t val = (uintptr_t) jl_linkage_blobs.items[i];
        // This is the same computation as in the prior for loop
        uintptr_t eyt_val = val + (i & 1);
        size_t eyt_idx = eyt_obj_idx((jl_value_t*)(eyt_val + 1)); assert(eyt_idx < eytzinger_idxs.len - 1);
        assert(eytzinger_image_tree.items[eyt_idx] == (void*)eyt_val && "Eytzinger tree failed to find object!");
        if (i & 1)
            eytzinger_idxs.items[eyt_idx] = (void*)n_linkage_blobs();
        else
            eytzinger_idxs.items[eyt_idx] = (void*)(i / 2);
    }
}

static int eyt_obj_in_img(jl_value_t *obj) JL_NOTSAFEPOINT
{
    assert((uintptr_t) obj % 4 == 0 && "Object not 4-byte aligned!");
    int idx = eyt_obj_idx(obj);
    // Now we use a tiny trick: tree[idx] & 1 is whether or not tree[idx] is a
    // start (0) or an end (1) of a blob. If it's a start, then the object is
    // in the image, otherwise it is not.
    int in_image = ((uintptr_t)eytzinger_image_tree.items[idx] & 1) == 0;
    return in_image;
}

size_t external_blob_index(jl_value_t *v) JL_NOTSAFEPOINT
{
    assert((uintptr_t) v % 4 == 0 && "Object not 4-byte aligned!");
    int eyt_idx = eyt_obj_idx(v);
    // We fill the invalid slots with the length, so we can just return that
    size_t idx = (size_t) eytzinger_idxs.items[eyt_idx];
    return idx;
}

uint8_t jl_object_in_image(jl_value_t *obj) JL_NOTSAFEPOINT
{
    return eyt_obj_in_img(obj);
}

NOINLINE uintptr_t gc_get_stack_ptr(void)
{
    return (uintptr_t)jl_get_frame_addr();
}

#define should_timeout() 0

void jl_gc_wait_for_the_world(jl_ptls_t* gc_all_tls_states, int gc_n_threads)
{
    JL_TIMING(GC, GC_Stop);
#ifdef USE_TRACY
    TracyCZoneCtx ctx = *(JL_TIMING_CURRENT_BLOCK->tracy_ctx);
    TracyCZoneColor(ctx, 0x696969);
#endif
    assert(gc_n_threads);
    if (gc_n_threads > 1)
        jl_wake_libuv();
    for (int i = 0; i < gc_n_threads; i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[i];
        if (ptls2 != NULL) {
            // This acquire load pairs with the release stores
            // in the signal handler of safepoint so we are sure that
            // all the stores on those threads are visible.
            // We're currently also using atomic store release in mutator threads
            // (in jl_gc_state_set), but we may want to use signals to flush the
            // memory operations on those threads lazily instead.
            while (!jl_atomic_load_relaxed(&ptls2->gc_state) || !jl_atomic_load_acquire(&ptls2->gc_state))
                jl_cpu_pause(); // yield?
        }
    }
}


void jl_gc_wait_for_the_world(jl_ptls_t* gc_all_tls_states, int gc_n_threads);

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
    // doesn't need release, since we'll keep checking (on the reader) until we see the work and
    // release our lock, and that will have a release barrier by then
    jl_atomic_store_relaxed(&jl_gc_have_pending_finalizers, 1);
}

static void run_finalizer(jl_task_t *ct, void *o, void *ff)
{
    int ptr_finalizer = gc_ptr_tag(o, 1);
    o = gc_ptr_clear_tag(o, 3);
    if (ptr_finalizer) {
        ((void (*)(void*))ff)((void*)o);
        return;
    }
    JL_TRY {
        size_t last_age = ct->world_age;
        ct->world_age = jl_atomic_load_acquire(&jl_world_counter);
        jl_apply_generic((jl_value_t*)ff, (jl_value_t**)&o, 1);
        ct->world_age = last_age;
    }
    JL_CATCH {
        jl_printf((JL_STREAM*)STDERR_FILENO, "error in running finalizer: ");
        jl_static_show((JL_STREAM*)STDERR_FILENO, jl_current_exception());
        jl_printf((JL_STREAM*)STDERR_FILENO, "\n");
        jlbacktrace(); // written to STDERR_FILENO
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
    size_t len = need_sync ? jl_atomic_load_acquire((_Atomic(size_t)*)&list->len) : list->len;
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
        jl_atomic_cmpswap((_Atomic(size_t)*)&list->len, &oldlen, len);
    }
    else {
        list->len = len;
    }
}

// The first two entries are assumed to be empty and the rest are assumed to
// be pointers to `jl_value_t` objects
static void jl_gc_push_arraylist(jl_task_t *ct, arraylist_t *list) JL_NOTSAFEPOINT
{
    void **items = list->items;
    items[0] = (void*)JL_GC_ENCODE_PUSHARGS(list->len - 2);
    items[1] = ct->gcstack;
    ct->gcstack = (jl_gcframe_t*)items;
}

// Same assumption as `jl_gc_push_arraylist`. Requires the finalizers lock
// to be hold for the current thread and will release the lock when the
// function returns.
static void jl_gc_run_finalizers_in_list(jl_task_t *ct, arraylist_t *list) JL_NOTSAFEPOINT_LEAVE
{
    // Avoid marking `ct` as non-migratable via an `@async` task (as noted in the docstring
    // of `finalizer`) in a finalizer:
    uint8_t sticky = ct->sticky;
    // empty out the first two entries for the GC frame
    arraylist_push(list, list->items[0]);
    arraylist_push(list, list->items[1]);
    jl_gc_push_arraylist(ct, list);
    void **items = list->items;
    size_t len = list->len;
    JL_UNLOCK_NOGC(&finalizers_lock);
    // run finalizers in reverse order they were added, so lower-level finalizers run last
    for (size_t i = len-4; i >= 2; i -= 2)
        run_finalizer(ct, items[i], items[i + 1]);
    // first entries were moved last to make room for GC frame metadata
    run_finalizer(ct, items[len-2], items[len-1]);
    // matches the jl_gc_push_arraylist above
    JL_GC_POP();
    ct->sticky = sticky;
}

static uint64_t finalizer_rngState[JL_RNG_SIZE];

void jl_rng_split(uint64_t dst[JL_RNG_SIZE], uint64_t src[JL_RNG_SIZE]) JL_NOTSAFEPOINT;

JL_DLLEXPORT void jl_gc_init_finalizer_rng_state(void)
{
    jl_rng_split(finalizer_rngState, jl_current_task->rngState);
}

static void run_finalizers(jl_task_t *ct)
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
    jl_atomic_store_relaxed(&jl_gc_have_pending_finalizers, 0);
    arraylist_new(&to_finalize, 0);

    uint64_t save_rngState[JL_RNG_SIZE];
    memcpy(&save_rngState[0], &ct->rngState[0], sizeof(save_rngState));
    jl_rng_split(ct->rngState, finalizer_rngState);

    // This releases the finalizers lock.
    int8_t was_in_finalizer = ct->ptls->in_finalizer;
    ct->ptls->in_finalizer = 1;
    jl_gc_run_finalizers_in_list(ct, &copied_list);
    ct->ptls->in_finalizer = was_in_finalizer;
    arraylist_free(&copied_list);

    memcpy(&ct->rngState[0], &save_rngState[0], sizeof(save_rngState));
}

JL_DLLEXPORT void jl_gc_run_pending_finalizers(jl_task_t *ct)
{
    if (ct == NULL)
        ct = jl_current_task;
    jl_ptls_t ptls = ct->ptls;
    if (!ptls->in_finalizer && ptls->locks.len == 0 && ptls->finalizers_inhibited == 0) {
        run_finalizers(ct);
    }
}

JL_DLLEXPORT int jl_gc_get_finalizers_inhibited(jl_ptls_t ptls)
{
    if (ptls == NULL)
        ptls = jl_current_task->ptls;
    return ptls->finalizers_inhibited;
}

JL_DLLEXPORT void jl_gc_disable_finalizers_internal(void)
{
    jl_ptls_t ptls = jl_current_task->ptls;
    ptls->finalizers_inhibited++;
}

JL_DLLEXPORT void jl_gc_enable_finalizers_internal(void)
{
    jl_task_t *ct = jl_current_task;
#ifdef NDEBUG
    ct->ptls->finalizers_inhibited--;
#else
    jl_gc_enable_finalizers(ct, 1);
#endif
}

JL_DLLEXPORT void jl_gc_enable_finalizers(jl_task_t *ct, int on)
{
    if (ct == NULL)
        ct = jl_current_task;
    jl_ptls_t ptls = ct->ptls;
    int old_val = ptls->finalizers_inhibited;
    int new_val = old_val + (on ? -1 : 1);
    if (new_val < 0) {
        JL_TRY {
            jl_error(""); // get a backtrace
        }
        JL_CATCH {
            jl_printf((JL_STREAM*)STDERR_FILENO, "WARNING: GC finalizers already enabled on this thread.\n");
            // Only print the backtrace once, to avoid spamming the logs
            static int backtrace_printed = 0;
            if (backtrace_printed == 0) {
                backtrace_printed = 1;
                jlbacktrace(); // written to STDERR_FILENO
            }
        }
        return;
    }
    ptls->finalizers_inhibited = new_val;
    if (jl_atomic_load_relaxed(&jl_gc_have_pending_finalizers)) {
        jl_gc_run_pending_finalizers(ct);
    }
}

JL_DLLEXPORT int8_t jl_gc_is_in_finalizer(void)
{
    return jl_current_task->ptls->in_finalizer;
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

void jl_gc_run_all_finalizers(jl_task_t *ct)
{
    int gc_n_threads;
    jl_ptls_t* gc_all_tls_states;
    gc_n_threads = jl_atomic_load_acquire(&jl_n_threads);
    gc_all_tls_states = jl_atomic_load_relaxed(&jl_all_tls_states);
    schedule_all_finalizers(&finalizer_list_marked);
    for (int i = 0; i < gc_n_threads; i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[i];
        if (ptls2 != NULL)
            schedule_all_finalizers(&ptls2->finalizers);
    }
    gc_n_threads = 0;
    gc_all_tls_states = NULL;
    run_finalizers(ct);
}

void jl_gc_add_finalizer_(jl_ptls_t ptls, void *v, void *f) JL_NOTSAFEPOINT
{
    assert(jl_atomic_load_relaxed(&ptls->gc_state) == 0);
    arraylist_t *a = &ptls->finalizers;
    // This acquire load and the release store at the end are used to
    // synchronize with `finalize_object` on another thread. Apart from the GC,
    // which is blocked by entering a unsafe region, there might be only
    // one other thread accessing our list in `finalize_object`
    // (only one thread since it needs to acquire the finalizer lock).
    // Similar to `finalize_object`, all content mutation has to be done
    // between the acquire and the release of the length.
    size_t oldlen = jl_atomic_load_acquire((_Atomic(size_t)*)&a->len);
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
    jl_atomic_store_release((_Atomic(size_t)*)&a->len, oldlen + 2);
}

JL_DLLEXPORT void jl_gc_add_ptr_finalizer(jl_ptls_t ptls, jl_value_t *v, void *f) JL_NOTSAFEPOINT
{
    jl_gc_add_finalizer_(ptls, (void*)(((uintptr_t)v) | 1), f);
}

// schedule f(v) to call at the next quiescent interval (aka after the next safepoint/region on all threads)
JL_DLLEXPORT void jl_gc_add_quiescent(jl_ptls_t ptls, void **v, void *f) JL_NOTSAFEPOINT
{
    assert(!gc_ptr_tag(v, 3));
    jl_gc_add_finalizer_(ptls, (void*)(((uintptr_t)v) | 3), f);
}

JL_DLLEXPORT void jl_gc_add_finalizer_th(jl_ptls_t ptls, jl_value_t *v, jl_function_t *f) JL_NOTSAFEPOINT
{
    if (__unlikely(jl_typeis(f, jl_voidpointer_type))) {
        jl_gc_add_ptr_finalizer(ptls, v, jl_unbox_voidpointer(f));
    }
    else {
        jl_gc_add_finalizer_(ptls, v, f);
    }
}

JL_DLLEXPORT void jl_finalize_th(jl_task_t *ct, jl_value_t *o)
{
    JL_LOCK_NOGC(&finalizers_lock);
    // Copy the finalizers into a temporary list so that code in the finalizer
    // won't change the list as we loop through them.
    // This list is also used as the GC frame when we are running the finalizers
    arraylist_t copied_list;
    arraylist_new(&copied_list, 0);
    // No need to check the to_finalize list since the user is apparently
    // still holding a reference to the object
    int gc_n_threads;
    jl_ptls_t* gc_all_tls_states;
    gc_n_threads = jl_atomic_load_acquire(&jl_n_threads);
    gc_all_tls_states = jl_atomic_load_relaxed(&jl_all_tls_states);
    for (int i = 0; i < gc_n_threads; i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[i];
        if (ptls2 != NULL)
            finalize_object(&ptls2->finalizers, o, &copied_list, jl_atomic_load_relaxed(&ct->tid) != i);
    }
    finalize_object(&finalizer_list_marked, o, &copied_list, 0);
    gc_n_threads = 0;
    gc_all_tls_states = NULL;
    if (copied_list.len > 0) {
        // This releases the finalizers lock.
        jl_gc_run_finalizers_in_list(ct, &copied_list);
    }
    else {
        JL_UNLOCK_NOGC(&finalizers_lock);
    }
    arraylist_free(&copied_list);
}

// explicitly scheduled objects for the sweepfunc callback
static void gc_sweep_foreign_objs_in_list(arraylist_t *objs)
{
    size_t p = 0;
    for (size_t i = 0; i < objs->len; i++) {
        jl_value_t *v = (jl_value_t*)(objs->items[i]);
        jl_datatype_t *t = (jl_datatype_t*)(jl_typeof(v));
        const jl_datatype_layout_t *layout = t->layout;
        jl_fielddescdyn_t *desc = (jl_fielddescdyn_t*)jl_dt_layout_fields(layout);

        int bits = jl_astaggedvalue(v)->bits.gc;
        if (!gc_marked(bits))
            desc->sweepfunc(v);
        else
            objs->items[p++] = v;
    }
    objs->len = p;
}

static void gc_sweep_foreign_objs(void)
{
    assert(gc_n_threads);
    for (int i = 0; i < gc_n_threads; i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[i];
        if (ptls2 != NULL)
            gc_sweep_foreign_objs_in_list(&ptls2->sweep_objs);
    }
}

// GC knobs and self-measurement variables
static int64_t last_gc_total_bytes = 0;

// max_total_memory is a suggestion.  We try very hard to stay
// under this limit, but we will go above it rather than halting.
#ifdef _P64
typedef uint64_t memsize_t;
static const size_t default_collect_interval = 5600 * 1024 * sizeof(void*);
static const size_t max_collect_interval = 1250000000UL;
static size_t total_mem;
// We expose this to the user/ci as jl_gc_set_max_memory
static memsize_t max_total_memory = (memsize_t) 2 * 1024 * 1024 * 1024 * 1024 * 1024;
#else
typedef uint32_t memsize_t;
static const size_t default_collect_interval = 3200 * 1024 * sizeof(void*);
static const size_t max_collect_interval =  500000000UL;
// Work really hard to stay within 2GB
// Alternative is to risk running out of address space
// on 32 bit architectures.
static memsize_t max_total_memory = (memsize_t) 2 * 1024 * 1024 * 1024;
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
int prev_sweep_full = 1;

#define inc_sat(v,s) v = (v) >= s ? s : (v)+1

// Full collection heuristics
static int64_t live_bytes = 0;
static int64_t promoted_bytes = 0;
static int64_t last_live_bytes = 0; // live_bytes at last collection
static int64_t t_start = 0; // Time GC starts;
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
    uv_mutex_lock(&gc_cache_lock);
    gc_sync_cache_nolock(ptls, &ptls->gc_cache);
    uv_mutex_unlock(&gc_cache_lock);
}

// No other threads can be running marking at the same time
static void gc_sync_all_caches_nolock(jl_ptls_t ptls)
{
    assert(gc_n_threads);
    for (int t_i = 0; t_i < gc_n_threads; t_i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[t_i];
        if (ptls2 != NULL)
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

// Atomically set the mark bit for object and return whether it was previously unmarked
FORCE_INLINE int gc_try_setmark_tag(jl_taggedvalue_t *o, uint8_t mark_mode) JL_NOTSAFEPOINT
{
    assert(gc_marked(mark_mode));
    uintptr_t tag = o->header;
    if (gc_marked(tag))
        return 0;
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
    tag = jl_atomic_exchange_relaxed((_Atomic(uintptr_t)*)&o->header, tag);
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
        static_assert(sizeof(_Atomic(uint16_t)) == sizeof(page->nold), "");
        jl_atomic_fetch_add_relaxed((_Atomic(uint16_t)*)&page->nold, 1);
    }
    else {
        ptls->gc_cache.scanned_bytes += page->osize;
        if (mark_reset_age) {
            page->has_young = 1;
            char *page_begin = gc_page_data(o) + GC_PAGE_OFFSET;
            int obj_id = (((char*)o) - page_begin) / page->osize;
            uint32_t *ages = page->ages + obj_id / 32;
            jl_atomic_fetch_and_relaxed((_Atomic(uint32_t)*)ages, ~(1 << (obj_id % 32)));
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
    uint8_t bits = (gc_old(buf->header) && !mark_reset_age) ? GC_OLD_MARKED : GC_MARKED;;
    // If the object is larger than the max pool size it can't be a pool object.
    // This should be accurate most of the time but there might be corner cases
    // where the size estimate is a little off so we do a pool lookup to make
    // sure.
    if (__likely(gc_try_setmark_tag(buf, mark_mode)) && !gc_verifying) {
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

STATIC_INLINE void maybe_collect(jl_ptls_t ptls)
{
    if (jl_atomic_load_relaxed(&ptls->gc_num.allocd) >= 0 || jl_gc_debug_check_other()) {
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

static void clear_weak_refs(void)
{
    assert(gc_n_threads);
    for (int i = 0; i < gc_n_threads; i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[i];
        if (ptls2 != NULL) {
            size_t n, l = ptls2->heap.weak_refs.len;
            void **lst = ptls2->heap.weak_refs.items;
            for (n = 0; n < l; n++) {
                jl_weakref_t *wr = (jl_weakref_t*)lst[n];
                if (!gc_marked(jl_astaggedvalue(wr->value)->bits.gc))
                    wr->value = (jl_value_t*)jl_nothing;
            }
        }
    }
}

static void sweep_weak_refs(void)
{
    assert(gc_n_threads);
    for (int i = 0; i < gc_n_threads; i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[i];
        if (ptls2 != NULL) {
            size_t n = 0;
            size_t ndel = 0;
            size_t l = ptls2->heap.weak_refs.len;
            void **lst = ptls2->heap.weak_refs.items;
            if (l == 0)
                continue;
            while (1) {
                jl_weakref_t *wr = (jl_weakref_t*)lst[n];
                if (gc_marked(jl_astaggedvalue(wr)->bits.gc))
                    n++;
                else
                    ndel++;
                if (n >= l - ndel)
                    break;
                void *tmp = lst[n];
                lst[n] = lst[n + ndel];
                lst[n + ndel] = tmp;
            }
            ptls2->heap.weak_refs.len -= ndel;
        }
    }
}


// big value list

// Size includes the tag and the tag is not cleared!!
STATIC_INLINE jl_value_t *jl_gc_big_alloc_inner(jl_ptls_t ptls, size_t sz)
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
    jl_atomic_store_relaxed(&ptls->gc_num.allocd,
        jl_atomic_load_relaxed(&ptls->gc_num.allocd) + allocsz);
    jl_atomic_store_relaxed(&ptls->gc_num.bigalloc,
        jl_atomic_load_relaxed(&ptls->gc_num.bigalloc) + 1);
#ifdef MEMDEBUG
    memset(v, 0xee, allocsz);
#endif
    v->sz = allocsz;
    v->age = 0;
    gc_big_object_link(v, &ptls->heap.big_objects);
    return jl_valueof(&v->header);
}

// Instrumented version of jl_gc_big_alloc_inner, called into by LLVM-generated code.
JL_DLLEXPORT jl_value_t *jl_gc_big_alloc(jl_ptls_t ptls, size_t sz)
{
    jl_value_t *val = jl_gc_big_alloc_inner(ptls, sz);
    maybe_record_alloc_to_profile(val, sz, jl_gc_unknown_type_tag);
    return val;
}

// This wrapper exists only to prevent `jl_gc_big_alloc_inner` from being inlined into
// its callers. We provide an external-facing interface for callers, and inline `jl_gc_big_alloc_inner`
// into this. (See https://github.com/JuliaLang/julia/pull/43868 for more details.)
jl_value_t *jl_gc_big_alloc_noinline(jl_ptls_t ptls, size_t sz) {
    return jl_gc_big_alloc_inner(ptls, sz);
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
    assert(gc_n_threads);
    for (int i = 0; i < gc_n_threads; i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[i];
        if (ptls2 != NULL)
            sweep_big_list(sweep_full, &ptls2->heap.big_objects);
    }
    if (sweep_full) {
        bigval_t **last_next = sweep_big_list(sweep_full, &big_objects_marked);
        // Move all survivors from big_objects_marked list to the big_objects list of this thread.
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
    jl_ptls_t ptls = jl_current_task->ptls;
    jl_atomic_store_relaxed(&ptls->gc_num.allocd,
        jl_atomic_load_relaxed(&ptls->gc_num.allocd) + sz);
}

static void combine_thread_gc_counts(jl_gc_num_t *dest) JL_NOTSAFEPOINT
{
    int gc_n_threads;
    jl_ptls_t* gc_all_tls_states;
    gc_n_threads = jl_atomic_load_acquire(&jl_n_threads);
    gc_all_tls_states = jl_atomic_load_relaxed(&jl_all_tls_states);
    for (int i = 0; i < gc_n_threads; i++) {
        jl_ptls_t ptls = gc_all_tls_states[i];
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
    int gc_n_threads;
    jl_ptls_t* gc_all_tls_states;
    gc_n_threads = jl_atomic_load_acquire(&jl_n_threads);
    gc_all_tls_states = jl_atomic_load_relaxed(&jl_all_tls_states);
    for (int i = 0; i < gc_n_threads; i++) {
        jl_ptls_t ptls = gc_all_tls_states[i];
        if (ptls != NULL) {
            memset(&ptls->gc_num, 0, sizeof(ptls->gc_num));
            jl_atomic_store_relaxed(&ptls->gc_num.allocd, -(int64_t)gc_num.interval);
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

size_t jl_array_nbytes(jl_array_t *a) JL_NOTSAFEPOINT
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
        gc_num.freed += jl_array_nbytes(a);
        gc_num.freecall++;
    }
}

static void sweep_malloced_arrays(void) JL_NOTSAFEPOINT
{
    gc_time_mallocd_array_start();
    assert(gc_n_threads);
    for (int t_i = 0; t_i < gc_n_threads; t_i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[t_i];
        if (ptls2 != NULL) {
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
    }
    gc_time_mallocd_array_end();
}

// pool allocation
STATIC_INLINE jl_taggedvalue_t *reset_page(jl_ptls_t ptls2, const jl_gc_pool_t *p, jl_gc_pagemeta_t *pg, jl_taggedvalue_t *fl) JL_NOTSAFEPOINT
{
    assert(GC_PAGE_OFFSET >= sizeof(void*));
    pg->nfree = (GC_PAGE_SZ - GC_PAGE_OFFSET) / p->osize;
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
    jl_ptls_t ptls = jl_current_task->ptls;
    jl_gc_pagemeta_t *pg = jl_gc_alloc_page();
    pg->osize = p->osize;
    pg->ages = (uint32_t*)malloc_s(LLT_ALIGN(GC_PAGE_SZ / 8 / p->osize + 1, sizeof(uint32_t)));
    pg->thread_n = ptls->tid;
    jl_taggedvalue_t *fl = reset_page(ptls, p, pg, NULL);
    p->newpages = fl;
    return fl;
}

// Size includes the tag and the tag is not cleared!!
STATIC_INLINE jl_value_t *jl_gc_pool_alloc_inner(jl_ptls_t ptls, int pool_offset,
                                          int osize)
{
    // Use the pool offset instead of the pool address as the argument
    // to workaround a llvm bug.
    // Ref https://llvm.org/bugs/show_bug.cgi?id=27190
    jl_gc_pool_t *p = (jl_gc_pool_t*)((char*)ptls + pool_offset);
    assert(jl_atomic_load_relaxed(&ptls->gc_state) == 0);
#ifdef MEMDEBUG
    return jl_gc_big_alloc(ptls, osize);
#endif
    maybe_collect(ptls);
    jl_atomic_store_relaxed(&ptls->gc_num.allocd,
        jl_atomic_load_relaxed(&ptls->gc_num.allocd) + osize);
    jl_atomic_store_relaxed(&ptls->gc_num.poolalloc,
        jl_atomic_load_relaxed(&ptls->gc_num.poolalloc) + 1);
    // first try to use the freelist
    jl_taggedvalue_t *v = p->freelist;
    if (v != NULL) {
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
        msan_allocated_memory(v, osize);
        return jl_valueof(v);
    }
    // if the freelist is empty we reuse empty but not freed pages
    v = p->newpages;
    jl_taggedvalue_t *next = (jl_taggedvalue_t*)((char*)v + osize);
    // If there's no pages left or the current page is used up,
    // we need to use the slow path.
    char *cur_page = gc_page_data((char*)v - 1);
    if (__unlikely(v == NULL || cur_page + GC_PAGE_SZ < (char*)next)) {
        if (v != NULL) {
            // like the freelist case,
            // but only update the page metadata when it is full
            jl_gc_pagemeta_t *pg = jl_assume(page_metadata((char*)v - 1));
            assert(pg->osize == p->osize);
            pg->nfree = 0;
            pg->has_young = 1;
            v = *(jl_taggedvalue_t**)cur_page;
        }
        // Not an else!!
        if (v == NULL)
            v = add_page(p);
        next = (jl_taggedvalue_t*)((char*)v + osize);
    }
    p->newpages = next;
    msan_allocated_memory(v, osize);
    return jl_valueof(v);
}

// Instrumented version of jl_gc_pool_alloc_inner, called into by LLVM-generated code.
JL_DLLEXPORT jl_value_t *jl_gc_pool_alloc(jl_ptls_t ptls, int pool_offset,
                                          int osize)
{
    jl_value_t *val = jl_gc_pool_alloc_inner(ptls, pool_offset, osize);
    maybe_record_alloc_to_profile(val, osize, jl_gc_unknown_type_tag);
    return val;
}

// This wrapper exists only to prevent `jl_gc_pool_alloc_inner` from being inlined into
// its callers. We provide an external-facing interface for callers, and inline `jl_gc_pool_alloc_inner`
// into this. (See https://github.com/JuliaLang/julia/pull/43868 for more details.)
jl_value_t *jl_gc_pool_alloc_noinline(jl_ptls_t ptls, int pool_offset, int osize) {
    return jl_gc_pool_alloc_inner(ptls, pool_offset, osize);
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
    uint32_t *ages = pg->ages;
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
            jl_ptls_t ptls2 = gc_all_tls_states[pg->thread_n];
            jl_taggedvalue_t *begin = reset_page(ptls2, p, pg, p->newpages);
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
        uint32_t msk = 1; // mask for the age bit in the current age byte
        uint32_t age = *ages;
        while ((char*)v <= lim) {
            if (!msk) {
                msk = 1;
                *ages = age;
                ages++;
                age = *ages;
            }
            int bits = v->bits.gc;
            if (!gc_marked(bits)) {
                *pfl = v;
                pfl = &v->next;
                pfl_begin = pfl_begin ? pfl_begin : pfl;
                pg_nfree++;
                age &= ~msk;
            }
            else { // marked young or old
                if (age & msk || bits == GC_OLD_MARKED) { // old enough
                    // `!age && bits == GC_OLD_MARKED` is possible for
                    // non-first-class objects like array buffers
                    // (they may get promoted by jl_gc_wb_buf for example,
                    // or explicitly by jl_gc_force_mark_old)
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
                age |= msk;
                freedall = 0;
            }
            v = (jl_taggedvalue_t*)((char*)v + osize);
            msk <<= 1;
        }
        *ages = age;
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
STATIC_INLINE void sweep_pool_page(jl_taggedvalue_t ***pfl, jl_gc_pagemeta_t *pg, int sweep_full) JL_NOTSAFEPOINT
{
    int p_n = pg->pool_n;
    int t_n = pg->thread_n;
    jl_ptls_t ptls2 = gc_all_tls_states[t_n];
    jl_gc_pool_t *p = &ptls2->heap.norm_pools[p_n];
    int osize = pg->osize;
    pfl[t_n * JL_GC_N_POOLS + p_n] = sweep_page(p, pg, pfl[t_n * JL_GC_N_POOLS + p_n], sweep_full, osize);
}

// sweep over a pagetable0 for all allocated pages
STATIC_INLINE int sweep_pool_pagetable0(jl_taggedvalue_t ***pfl, pagetable0_t *pagetable0, int sweep_full) JL_NOTSAFEPOINT
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
STATIC_INLINE int sweep_pool_pagetable1(jl_taggedvalue_t ***pfl, pagetable1_t *pagetable1, int sweep_full) JL_NOTSAFEPOINT
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
        if (pagetable1 != NULL)
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

    // For the benefit of the analyzer, which doesn't know that gc_n_threads
    // doesn't change over the course of this function
    size_t n_threads = gc_n_threads;

    // allocate enough space to hold the end of the free list chain
    // for every thread and pool size
    jl_taggedvalue_t ***pfl = (jl_taggedvalue_t ***) alloca(n_threads * JL_GC_N_POOLS * sizeof(jl_taggedvalue_t**));

    // update metadata of pages that were pointed to by freelist or newpages from a pool
    // i.e. pages being the current allocation target
    for (int t_i = 0; t_i < n_threads; t_i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[t_i];
        if (ptls2 == NULL) {
            for (int i = 0; i < JL_GC_N_POOLS; i++) {
                pfl[t_i * JL_GC_N_POOLS + i] = NULL;
            }
            continue;
        }
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
        jl_ptls_t ptls2 = gc_all_tls_states[t_i];
        if (ptls2 != NULL) {
            for (int i = 0; i < JL_GC_N_POOLS; i++) {
                *pfl[t_i * JL_GC_N_POOLS + i] = NULL;
            }
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

JL_DLLEXPORT void jl_gc_queue_root(const jl_value_t *ptr)
{
    jl_ptls_t ptls = jl_current_task->ptls;
    jl_taggedvalue_t *o = jl_astaggedvalue(ptr);
    // The modification of the `gc_bits` is not atomic but it
    // should be safe here since GC is not allowed to run here and we only
    // write GC_OLD to the GC bits outside GC. This could cause
    // duplicated objects in the remset but that shouldn't be a problem.
    o->bits.gc = GC_MARKED;
    arraylist_push(ptls->heap.remset, (jl_value_t*)ptr);
    ptls->heap.remset_nptr++; // conservative
}

void jl_gc_queue_multiroot(const jl_value_t *parent, const jl_value_t *ptr) JL_NOTSAFEPOINT
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


#ifdef JL_DEBUG_BUILD
static void *volatile gc_findval; // for usage from gdb, for finding the gc-root for a value
#endif


// Handle the case where the stack is only partially copied.
STATIC_INLINE uintptr_t gc_get_stack_addr(void *_addr, uintptr_t offset,
                                          uintptr_t lb, uintptr_t ub) JL_NOTSAFEPOINT
{
    uintptr_t addr = (uintptr_t)_addr;
    if (addr >= lb && addr < ub)
        return addr + offset;
    return addr;
}

STATIC_INLINE uintptr_t gc_read_stack(void *_addr, uintptr_t offset,
                                      uintptr_t lb, uintptr_t ub) JL_NOTSAFEPOINT
{
    uintptr_t real_addr = gc_get_stack_addr(_addr, offset, lb, ub);
    return *(uintptr_t*)real_addr;
}

JL_NORETURN NOINLINE void gc_assert_datatype_fail(jl_ptls_t ptls, jl_datatype_t *vt,
                                                  jl_gc_markqueue_t *mq) JL_NOTSAFEPOINT
{
    jl_safe_printf("GC error (probable corruption) :\n");
    jl_gc_debug_print_status();
    jl_(vt);
    jl_gc_debug_critical_error();
    gc_mark_loop_unwind(ptls, mq, 0);
    abort();
}

// Check if `nptr` is tagged for `old + refyoung`,
// Push the object to the remset and update the `nptr` counter if necessary.
STATIC_INLINE void gc_mark_push_remset(jl_ptls_t ptls, jl_value_t *obj,
                                       uintptr_t nptr) JL_NOTSAFEPOINT
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

// Double the mark queue
static NOINLINE void gc_markqueue_resize(jl_gc_markqueue_t *mq) JL_NOTSAFEPOINT
{
    jl_value_t **old_start = mq->start;
    size_t old_queue_size = (mq->end - mq->start);
    size_t offset = (mq->current - old_start);
    mq->start = (jl_value_t **)realloc_s(old_start, 2 * old_queue_size * sizeof(jl_value_t *));
    mq->current = (mq->start + offset);
    mq->end = (mq->start + 2 * old_queue_size);
}

// Push a work item to the queue
STATIC_INLINE void gc_markqueue_push(jl_gc_markqueue_t *mq, jl_value_t *obj) JL_NOTSAFEPOINT
{
    if (__unlikely(mq->current == mq->end))
        gc_markqueue_resize(mq);
    *mq->current = obj;
    mq->current++;
}

// Pop from the mark queue
STATIC_INLINE jl_value_t *gc_markqueue_pop(jl_gc_markqueue_t *mq)
{
    jl_value_t *obj = NULL;
    if (mq->current != mq->start) {
        mq->current--;
        obj = *mq->current;
    }
    return obj;
}

// Double the chunk queue
static NOINLINE void gc_chunkqueue_resize(jl_gc_markqueue_t *mq) JL_NOTSAFEPOINT
{
    jl_gc_chunk_t *old_start = mq->chunk_start;
    size_t old_queue_size = (mq->chunk_end - mq->chunk_start);
    size_t offset = (mq->current_chunk - old_start);
    mq->chunk_start = (jl_gc_chunk_t *)realloc_s(old_start, 2 * old_queue_size * sizeof(jl_gc_chunk_t));
    mq->current_chunk = (mq->chunk_start + offset);
    mq->chunk_end = (mq->chunk_start + 2 * old_queue_size);
}

// Push chunk `*c` into chunk queue
STATIC_INLINE void gc_chunkqueue_push(jl_gc_markqueue_t *mq, jl_gc_chunk_t *c) JL_NOTSAFEPOINT
{
    if (__unlikely(mq->current_chunk == mq->chunk_end))
        gc_chunkqueue_resize(mq);
    *mq->current_chunk = *c;
    mq->current_chunk++;
}

// Pop chunk from chunk queue
STATIC_INLINE jl_gc_chunk_t gc_chunkqueue_pop(jl_gc_markqueue_t *mq) JL_NOTSAFEPOINT
{
    jl_gc_chunk_t c = {.cid = GC_empty_chunk};
    if (mq->current_chunk != mq->chunk_start) {
        mq->current_chunk--;
        c = *mq->current_chunk;
    }
    return c;
}

// Enqueue an unmarked obj. last bit of `nptr` is set if `_obj` is young
STATIC_INLINE void gc_try_claim_and_push(jl_gc_markqueue_t *mq, void *_obj,
                           uintptr_t *nptr) JL_NOTSAFEPOINT
{
    if (_obj == NULL)
        return;
    jl_value_t *obj = (jl_value_t *)jl_assume(_obj);
    jl_taggedvalue_t *o = jl_astaggedvalue(obj);
    if (!gc_old(o->header) && nptr)
        *nptr |= 1;
    if (gc_try_setmark_tag(o, GC_MARKED))
        gc_markqueue_push(mq, obj);
}

// Mark object with 8bit field descriptors
STATIC_INLINE jl_value_t *gc_mark_obj8(jl_ptls_t ptls, char *obj8_parent, uint8_t *obj8_begin,
                         uint8_t *obj8_end, uintptr_t nptr) JL_NOTSAFEPOINT
{
    (void)jl_assume(obj8_begin < obj8_end);
    jl_gc_markqueue_t *mq = &ptls->mark_queue;
    jl_value_t **slot = NULL;
    jl_value_t *new_obj = NULL;
    for (; obj8_begin < obj8_end; obj8_begin++) {
        slot = &((jl_value_t**)obj8_parent)[*obj8_begin];
        new_obj = *slot;
        if (new_obj != NULL) {
            verify_parent2("object", obj8_parent, slot, "field(%d)",
                            gc_slot_to_fieldidx(obj8_parent, slot, (jl_datatype_t*)jl_typeof(obj8_parent)));
            if (obj8_begin + 1 != obj8_end) {
                gc_try_claim_and_push(mq, new_obj, &nptr);
            }
            else {
                // Unroll marking of last item to avoid pushing
                // and popping it right away
                jl_taggedvalue_t *o = jl_astaggedvalue(new_obj);
                nptr |= !gc_old(o->header);
                if (!gc_try_setmark_tag(o, GC_MARKED)) new_obj = NULL;
            }
            gc_heap_snapshot_record_object_edge((jl_value_t*)obj8_parent, slot);
        }
    }
    gc_mark_push_remset(ptls, (jl_value_t *)obj8_parent, nptr);
    return new_obj;
}

// Mark object with 16bit field descriptors
STATIC_INLINE jl_value_t *gc_mark_obj16(jl_ptls_t ptls, char *obj16_parent, uint16_t *obj16_begin,
                          uint16_t *obj16_end, uintptr_t nptr) JL_NOTSAFEPOINT
{
    (void)jl_assume(obj16_begin < obj16_end);
    jl_gc_markqueue_t *mq = &ptls->mark_queue;
    jl_value_t **slot = NULL;
    jl_value_t *new_obj = NULL;
    for (; obj16_begin < obj16_end; obj16_begin++) {
        slot = &((jl_value_t **)obj16_parent)[*obj16_begin];
        new_obj = *slot;
        if (new_obj != NULL) {
            verify_parent2("object", obj16_parent, slot, "field(%d)",
                            gc_slot_to_fieldidx(obj16_parent, slot, (jl_datatype_t*)jl_typeof(obj16_parent)));
            gc_try_claim_and_push(mq, new_obj, &nptr);
            if (obj16_begin + 1 != obj16_end) {
                gc_try_claim_and_push(mq, new_obj, &nptr);
            }
            else {
                // Unroll marking of last item to avoid pushing
                // and popping it right away
                jl_taggedvalue_t *o = jl_astaggedvalue(new_obj);
                nptr |= !gc_old(o->header);
                if (!gc_try_setmark_tag(o, GC_MARKED)) new_obj = NULL;
            }
            gc_heap_snapshot_record_object_edge((jl_value_t*)obj16_parent, slot);
        }
    }
    gc_mark_push_remset(ptls, (jl_value_t *)obj16_parent, nptr);
    return new_obj;
}

// Mark object with 32bit field descriptors
STATIC_INLINE jl_value_t *gc_mark_obj32(jl_ptls_t ptls, char *obj32_parent, uint32_t *obj32_begin,
                          uint32_t *obj32_end, uintptr_t nptr) JL_NOTSAFEPOINT
{
    (void)jl_assume(obj32_begin < obj32_end);
    jl_gc_markqueue_t *mq = &ptls->mark_queue;
    jl_value_t **slot = NULL;
    jl_value_t *new_obj = NULL;
    for (; obj32_begin < obj32_end; obj32_begin++) {
        slot = &((jl_value_t **)obj32_parent)[*obj32_begin];
        new_obj = *slot;
        if (new_obj != NULL) {
            verify_parent2("object", obj32_parent, slot, "field(%d)",
                            gc_slot_to_fieldidx(obj32_parent, slot, (jl_datatype_t*)jl_typeof(obj32_parent)));
            if (obj32_begin + 1 != obj32_end) {
                gc_try_claim_and_push(mq, new_obj, &nptr);
            }
            else {
                // Unroll marking of last item to avoid pushing
                // and popping it right away
                jl_taggedvalue_t *o = jl_astaggedvalue(new_obj);
                nptr |= !gc_old(o->header);
                if (!gc_try_setmark_tag(o, GC_MARKED)) new_obj = NULL;
            }
            gc_heap_snapshot_record_object_edge((jl_value_t*)obj32_parent, slot);
        }
    }
    gc_mark_push_remset(ptls, (jl_value_t *)obj32_parent, nptr);
    return new_obj;
}

// Mark object array
STATIC_INLINE void gc_mark_objarray(jl_ptls_t ptls, jl_value_t *obj_parent, jl_value_t **obj_begin,
                      jl_value_t **obj_end, uint32_t step, uintptr_t nptr) JL_NOTSAFEPOINT
{
    jl_gc_markqueue_t *mq = &ptls->mark_queue;
    jl_value_t *new_obj;
    // Decide whether need to chunk objary
    (void)jl_assume(step > 0);
    if ((nptr & 0x2) == 0x2) {
        // pre-scan this object: most of this object should be old, so look for
        // the first young object before starting this chunk
        // (this also would be valid for young objects, but probably less beneficial)
        for (; obj_begin < obj_end; obj_begin += step) {
            new_obj = *obj_begin;
            if (new_obj != NULL) {
                verify_parent2("obj array", obj_parent, obj_begin, "elem(%d)",
                               gc_slot_to_arrayidx(obj_parent, obj_begin));
                jl_taggedvalue_t *o = jl_astaggedvalue(new_obj);
                if (!gc_old(o->header))
                    nptr |= 1;
                if (!gc_marked(o->header))
                    break;
                gc_heap_snapshot_record_array_edge(obj_parent, &new_obj);
            }
        }
    }
    size_t too_big = (obj_end - obj_begin) / MAX_REFS_AT_ONCE > step; // use this order of operations to avoid idiv
    jl_value_t **scan_end = obj_end;
    if (too_big) {
        scan_end = obj_begin + step * MAX_REFS_AT_ONCE;
    }
    for (; obj_begin < scan_end; obj_begin += step) {
        new_obj = *obj_begin;
        if (new_obj != NULL) {
            verify_parent2("obj array", obj_parent, obj_begin, "elem(%d)",
                        gc_slot_to_arrayidx(obj_parent, obj_begin));
            gc_try_claim_and_push(mq, new_obj, &nptr);
            gc_heap_snapshot_record_array_edge(obj_parent, &new_obj);
        }
    }
    if (too_big) {
        jl_gc_chunk_t c = {GC_objary_chunk, obj_parent, scan_end,
                            obj_end,      NULL,       NULL,
                            step,         nptr};
        gc_chunkqueue_push(mq, &c);
    }
    else {
        gc_mark_push_remset(ptls, obj_parent, nptr);
    }
}

// Mark array with 8bit field descriptors
STATIC_INLINE void gc_mark_array8(jl_ptls_t ptls, jl_value_t *ary8_parent, jl_value_t **ary8_begin,
                    jl_value_t **ary8_end, uint8_t *elem_begin, uint8_t *elem_end,
                    uintptr_t nptr) JL_NOTSAFEPOINT
{
    jl_gc_markqueue_t *mq = &ptls->mark_queue;
    jl_value_t *new_obj;
    size_t elsize = ((jl_array_t *)ary8_parent)->elsize / sizeof(jl_value_t *);
    assert(elsize > 0);
    // Decide whether need to chunk objary
    if ((nptr & 0x2) == 0x2) {
        // pre-scan this object: most of this object should be old, so look for
        // the first young object before starting this chunk
        // (this also would be valid for young objects, but probably less beneficial)
        for (; ary8_begin < ary8_end; ary8_begin += elsize) {
            int early_end = 0;
            for (uint8_t *pindex = elem_begin; pindex < elem_end; pindex++) {
                new_obj = ary8_begin[*pindex];
                if (new_obj != NULL) {
                    verify_parent2("array", ary8_parent, &new_obj, "elem(%d)",
                                gc_slot_to_arrayidx(ary8_parent, ary8_begin));
                    jl_taggedvalue_t *o = jl_astaggedvalue(new_obj);
                    if (!gc_old(o->header))
                        nptr |= 1;
                    if (!gc_marked(o->header)){
                        early_end = 1;
                        break;
                    }
                    gc_heap_snapshot_record_array_edge(ary8_parent, &new_obj);
                }
            }
            if (early_end)
                break;
        }
    }
    size_t too_big = (ary8_end - ary8_begin) / MAX_REFS_AT_ONCE > elsize; // use this order of operations to avoid idiv
    jl_value_t **scan_end = ary8_end;
    if (too_big) {
        scan_end = ary8_begin + elsize * MAX_REFS_AT_ONCE;
    }
    for (; ary8_begin < ary8_end; ary8_begin += elsize) {
        for (uint8_t *pindex = elem_begin; pindex < elem_end; pindex++) {
            new_obj = ary8_begin[*pindex];
            if (new_obj != NULL) {
                verify_parent2("array", ary8_parent, &new_obj, "elem(%d)",
                               gc_slot_to_arrayidx(ary8_parent, ary8_begin));
                gc_try_claim_and_push(mq, new_obj, &nptr);
                gc_heap_snapshot_record_array_edge(ary8_parent, &new_obj);
            }
        }
    }
    if (too_big) {
        jl_gc_chunk_t c = {GC_ary8_chunk, ary8_parent, scan_end,
                           ary8_end,        elem_begin,       elem_end,
                           0,               nptr};
        gc_chunkqueue_push(mq, &c);
    }
    else {
        gc_mark_push_remset(ptls, ary8_parent, nptr);
    }
}

// Mark array with 16bit field descriptors
STATIC_INLINE void gc_mark_array16(jl_ptls_t ptls, jl_value_t *ary16_parent, jl_value_t **ary16_begin,
                     jl_value_t **ary16_end, uint16_t *elem_begin, uint16_t *elem_end,
                     uintptr_t nptr) JL_NOTSAFEPOINT
{
    jl_gc_markqueue_t *mq = &ptls->mark_queue;
    jl_value_t *new_obj;
    size_t elsize = ((jl_array_t *)ary16_parent)->elsize / sizeof(jl_value_t *);
    assert(elsize > 0);
    // Decide whether need to chunk objary
    if ((nptr & 0x2) == 0x2) {
        // pre-scan this object: most of this object should be old, so look for
        // the first young object before starting this chunk
        // (this also would be valid for young objects, but probably less beneficial)
        for (; ary16_begin < ary16_end; ary16_begin += elsize) {
            int early_end = 0;
            for (uint16_t *pindex = elem_begin; pindex < elem_end; pindex++) {
                new_obj = ary16_begin[*pindex];
                if (new_obj != NULL) {
                    verify_parent2("array", ary16_parent, &new_obj, "elem(%d)",
                                gc_slot_to_arrayidx(ary16_parent, ary16_begin));
                    jl_taggedvalue_t *o = jl_astaggedvalue(new_obj);
                    if (!gc_old(o->header))
                        nptr |= 1;
                    if (!gc_marked(o->header)){
                        early_end = 1;
                        break;
                    }
                    gc_heap_snapshot_record_array_edge(ary16_parent, &new_obj);
                }
            }
            if (early_end)
                break;
        }
    }
    size_t too_big = (ary16_end - ary16_begin) / MAX_REFS_AT_ONCE > elsize; // use this order of operations to avoid idiv
    jl_value_t **scan_end = ary16_end;
    if (too_big) {
        scan_end = ary16_begin + elsize * MAX_REFS_AT_ONCE;
    }
    for (; ary16_begin < scan_end; ary16_begin += elsize) {
        for (uint16_t *pindex = elem_begin; pindex < elem_end; pindex++) {
            new_obj = ary16_begin[*pindex];
            if (new_obj != NULL) {
                verify_parent2("array", ary16_parent, &new_obj, "elem(%d)",
                               gc_slot_to_arrayidx(ary16_parent, ary16_begin));
                gc_try_claim_and_push(mq, new_obj, &nptr);
                gc_heap_snapshot_record_array_edge(ary16_parent, &new_obj);
            }
        }
    }
    if (too_big) {
        jl_gc_chunk_t c = {GC_ary16_chunk, ary16_parent, scan_end,
                            ary16_end,      elem_begin,       elem_end,
                            elsize,         nptr};
        gc_chunkqueue_push(mq, &c);
    }
    else {
        gc_mark_push_remset(ptls, ary16_parent, nptr);
    }
}

// Mark chunk of large array
STATIC_INLINE void gc_mark_chunk(jl_ptls_t ptls, jl_gc_markqueue_t *mq, jl_gc_chunk_t *c) JL_NOTSAFEPOINT
{
    switch (c->cid) {
        case GC_objary_chunk: {
            jl_value_t *obj_parent = c->parent;
            jl_value_t **obj_begin = c->begin;
            jl_value_t **obj_end = c->end;
            uint32_t step = c->step;
            uintptr_t nptr = c->nptr;
            gc_mark_objarray(ptls, obj_parent, obj_begin, obj_end, step,
                             nptr);
            break;
        }
        case GC_ary8_chunk: {
            jl_value_t *ary8_parent = c->parent;
            jl_value_t **ary8_begin = c->begin;
            jl_value_t **ary8_end = c->end;
            uint8_t *elem_begin = (uint8_t *)c->elem_begin;
            uint8_t *elem_end = (uint8_t *)c->elem_end;
            uintptr_t nptr = c->nptr;
            gc_mark_array8(ptls, ary8_parent, ary8_begin, ary8_end, elem_begin, elem_end,
                           nptr);
            break;
        }
        case GC_ary16_chunk: {
            jl_value_t *ary16_parent = c->parent;
            jl_value_t **ary16_begin = c->begin;
            jl_value_t **ary16_end = c->end;
            uint16_t *elem_begin = (uint16_t *)c->elem_begin;
            uint16_t *elem_end = (uint16_t *)c->elem_end;
            uintptr_t nptr = c->nptr;
            gc_mark_array16(ptls, ary16_parent, ary16_begin, ary16_end, elem_begin, elem_end,
                            nptr);
            break;
        }
        case GC_finlist_chunk: {
            jl_value_t **fl_begin = c->begin;
            jl_value_t **fl_end = c->end;
            gc_mark_finlist_(mq, fl_begin, fl_end);
            break;
        }
        default: {
            // `empty-chunk` should be checked by caller
            jl_safe_printf("GC internal error: chunk mismatch cid=%d\n", c->cid);
            abort();
        }
    }
}

// Mark gc frame
STATIC_INLINE void gc_mark_stack(jl_ptls_t ptls, jl_gcframe_t *s, uint32_t nroots, uintptr_t offset,
                   uintptr_t lb, uintptr_t ub) JL_NOTSAFEPOINT
{
    jl_gc_markqueue_t *mq = &ptls->mark_queue;
    jl_value_t *new_obj;
    uint32_t nr = nroots >> 2;
    while (1) {
        jl_value_t ***rts = (jl_value_t ***)(((void **)s) + 2);
        for (uint32_t i = 0; i < nr; i++) {
            if (nroots & 1) {
                void **slot = (void **)gc_read_stack(&rts[i], offset, lb, ub);
                new_obj = (jl_value_t *)gc_read_stack(slot, offset, lb, ub);
            }
            else {
                new_obj = (jl_value_t *)gc_read_stack(&rts[i], offset, lb, ub);
                if (gc_ptr_tag(new_obj, 1)) {
                    // handle tagged pointers in finalizer list
                    new_obj = (jl_value_t *)gc_ptr_clear_tag(new_obj, 1);
                    // skip over the finalizer fptr
                    i++;
                }
                if (gc_ptr_tag(new_obj, 2))
                    continue;
            }
            if (new_obj != NULL) {
                gc_try_claim_and_push(mq, new_obj, NULL);
                gc_heap_snapshot_record_frame_to_object_edge(s, new_obj);
            }
        }
        jl_gcframe_t *sprev = (jl_gcframe_t *)gc_read_stack(&s->prev, offset, lb, ub);
        if (sprev == NULL)
            break;
        gc_heap_snapshot_record_frame_to_frame_edge(s, sprev);
        s = sprev;
        uintptr_t new_nroots = gc_read_stack(&s->nroots, offset, lb, ub);
        assert(new_nroots <= UINT32_MAX);
        nroots = (uint32_t)new_nroots;
        nr = nroots >> 2;
    }
}

// Mark exception stack
STATIC_INLINE void gc_mark_excstack(jl_ptls_t ptls, jl_excstack_t *excstack, size_t itr) JL_NOTSAFEPOINT
{
    jl_gc_markqueue_t *mq = &ptls->mark_queue;
    jl_value_t *new_obj;
    while (itr > 0) {
        size_t bt_size = jl_excstack_bt_size(excstack, itr);
        jl_bt_element_t *bt_data = jl_excstack_bt_data(excstack, itr);
        for (size_t bt_index = 0; bt_index < bt_size;
             bt_index += jl_bt_entry_size(bt_data + bt_index)) {
            jl_bt_element_t *bt_entry = bt_data + bt_index;
            if (jl_bt_is_native(bt_entry))
                continue;
            // Found an extended backtrace entry: iterate over any
            // GC-managed values inside.
            size_t njlvals = jl_bt_num_jlvals(bt_entry);
            for (size_t jlval_index = 0; jlval_index < njlvals; jlval_index++) {
                new_obj = jl_bt_entry_jlvalue(bt_entry, jlval_index);
                gc_try_claim_and_push(mq, new_obj, NULL);
                gc_heap_snapshot_record_frame_to_object_edge(bt_entry, new_obj);
            }
        }
        // The exception comes last - mark it
        new_obj = jl_excstack_exception(excstack, itr);
        itr = jl_excstack_next(excstack, itr);
        gc_try_claim_and_push(mq, new_obj, NULL);
        gc_heap_snapshot_record_frame_to_object_edge(excstack, new_obj);
    }
}

// Mark module binding
STATIC_INLINE void gc_mark_module_binding(jl_ptls_t ptls, jl_module_t *mb_parent, jl_binding_t **mb_begin,
                            jl_binding_t **mb_end, uintptr_t nptr,
                            uint8_t bits) JL_NOTSAFEPOINT
{
    jl_gc_markqueue_t *mq = &ptls->mark_queue;
    for (; mb_begin < mb_end; mb_begin++) {
        jl_binding_t *b = *mb_begin;
        if (b == (jl_binding_t *)jl_nothing)
            continue;
        verify_parent1("module", mb_parent, mb_begin, "binding_buff");
        gc_try_claim_and_push(mq, b, &nptr);
    }
    jl_value_t *bindings = (jl_value_t *)jl_atomic_load_relaxed(&mb_parent->bindings);
    gc_try_claim_and_push(mq, bindings, &nptr);
    jl_value_t *bindingkeyset = (jl_value_t *)jl_atomic_load_relaxed(&mb_parent->bindingkeyset);
    gc_try_claim_and_push(mq, bindingkeyset, &nptr);
    gc_try_claim_and_push(mq, (jl_value_t *)mb_parent->parent, &nptr);
    size_t nusings = mb_parent->usings.len;
    if (nusings > 0) {
        // this is only necessary because bindings for "using" modules
        // are added only when accessed. therefore if a module is replaced
        // after "using" it but before accessing it, this array might
        // contain the only reference.
        jl_value_t *obj_parent = (jl_value_t *)mb_parent;
        jl_value_t **objary_begin = (jl_value_t **)mb_parent->usings.items;
        jl_value_t **objary_end = objary_begin + nusings;
        gc_mark_objarray(ptls, obj_parent, objary_begin, objary_end, 1, nptr);
    }
    else {
        gc_mark_push_remset(ptls, (jl_value_t *)mb_parent, nptr);
    }
}

void gc_mark_finlist_(jl_gc_markqueue_t *mq, jl_value_t **fl_begin, jl_value_t **fl_end)
{
    jl_value_t *new_obj;
    // Decide whether need to chunk finlist
    size_t nrefs = (fl_end - fl_begin);
    if (nrefs > MAX_REFS_AT_ONCE) {
        jl_gc_chunk_t c = {GC_finlist_chunk, NULL, fl_begin + MAX_REFS_AT_ONCE, fl_end, 0, 0, 0, 0};
        gc_chunkqueue_push(mq, &c);
        fl_end = fl_begin + MAX_REFS_AT_ONCE;
    }
    for (; fl_begin < fl_end; fl_begin++) {
        new_obj = *fl_begin;
        if (__unlikely(!new_obj))
            continue;
        if (gc_ptr_tag(new_obj, 1)) {
            new_obj = (jl_value_t *)gc_ptr_clear_tag(new_obj, 1);
            fl_begin++;
            assert(fl_begin < fl_end);
        }
        if (gc_ptr_tag(new_obj, 2))
            continue;
        gc_try_claim_and_push(mq, new_obj, NULL);
    }
}

// Mark finalizer list (or list of objects following same format)
void gc_mark_finlist(jl_gc_markqueue_t *mq, arraylist_t *list, size_t start)
{
    size_t len = list->len;
    if (len <= start)
        return;
    jl_value_t **fl_begin = (jl_value_t **)list->items + start;
    jl_value_t **fl_end = (jl_value_t **)list->items + len;
    gc_mark_finlist_(mq, fl_begin, fl_end);
}

JL_DLLEXPORT int jl_gc_mark_queue_obj(jl_ptls_t ptls, jl_value_t *obj)
{
    int may_claim = gc_try_setmark_tag(jl_astaggedvalue(obj), GC_MARKED);
    if (may_claim)
        gc_markqueue_push(&ptls->mark_queue, obj);
    return may_claim;
}

JL_DLLEXPORT void jl_gc_mark_queue_objarray(jl_ptls_t ptls, jl_value_t *parent,
                                            jl_value_t **objs, size_t nobjs)
{
    uintptr_t nptr = (nobjs << 2) | (jl_astaggedvalue(parent)->bits.gc & 2);
    gc_mark_objarray(ptls, parent, objs, objs + nobjs, 1, nptr);
}

// Enqueue and mark all outgoing references from `new_obj` which have not been marked
// yet. `meta_updated` is mostly used to make sure we don't update metadata twice for
// objects which have been enqueued into the `remset`
FORCE_INLINE void gc_mark_outrefs(jl_ptls_t ptls, jl_gc_markqueue_t *mq, void *_new_obj,
                              int meta_updated)
{
    jl_value_t *new_obj = (jl_value_t *)_new_obj;
    mark_obj: {
    #ifdef JL_DEBUG_BUILD
        if (new_obj == gc_findval)
            jl_raise_debugger();
    #endif
        jl_taggedvalue_t *o = jl_astaggedvalue(new_obj);
        jl_datatype_t *vt = (jl_datatype_t *)(o->header & ~(uintptr_t)0xf);
        uint8_t bits = (gc_old(o->header) && !mark_reset_age) ? GC_OLD_MARKED : GC_MARKED;
        int update_meta = __likely(!meta_updated && !gc_verifying);
        int foreign_alloc = 0;
        // directly point at eyt_obj_in_img to encourage inlining
        if (update_meta && o->bits.in_image) {
            foreign_alloc = 1;
            update_meta = 0;
        }
        // Symbols are always marked
        assert(vt != jl_symbol_type);
        if (vt == jl_simplevector_type) {
            size_t l = jl_svec_len(new_obj);
            jl_value_t **data = jl_svec_data(new_obj);
            size_t dtsz = l * sizeof(void *) + sizeof(jl_svec_t);
            if (update_meta)
                gc_setmark(ptls, o, bits, dtsz);
            else if (foreign_alloc)
                objprofile_count(vt, bits == GC_OLD_MARKED, dtsz);
            jl_value_t *objary_parent = new_obj;
            jl_value_t **objary_begin = data;
            jl_value_t **objary_end = data + l;
            uint32_t step = 1;
            uintptr_t nptr = (l << 2) | (bits & GC_OLD);
            gc_mark_objarray(ptls, objary_parent, objary_begin, objary_end, step, nptr);
        }
        else if (vt->name == jl_array_typename) {
            jl_array_t *a = (jl_array_t *)new_obj;
            jl_array_flags_t flags = a->flags;
            if (update_meta) {
                if (flags.pooled)
                    gc_setmark_pool(ptls, o, bits);
                else
                    gc_setmark_big(ptls, o, bits);
            }
            else if (foreign_alloc) {
                objprofile_count(vt, bits == GC_OLD_MARKED, sizeof(jl_array_t));
            }
            if (flags.how == 0) {
                void *data_ptr = (char*)a + sizeof(jl_array_t) +jl_array_ndimwords(a->flags.ndims) * sizeof(size_t);
                gc_heap_snapshot_record_hidden_edge(new_obj, data_ptr, jl_array_nbytes(a), 2);
            }
            if (flags.how == 1) {
                void *val_buf = jl_astaggedvalue((char*)a->data - a->offset * a->elsize);
                verify_parent1("array", new_obj, &val_buf, "buffer ('loc' addr is meaningless)");
                gc_heap_snapshot_record_hidden_edge(new_obj, jl_valueof(val_buf), jl_array_nbytes(a), flags.pooled);
                (void)val_buf;
                gc_setmark_buf_(ptls, (char*)a->data - a->offset * a->elsize,
                                bits, jl_array_nbytes(a));
            }
            else if (flags.how == 2) {
                if (update_meta || foreign_alloc) {
                    objprofile_count(jl_malloc_tag, bits == GC_OLD_MARKED,
                                     jl_array_nbytes(a));
                    gc_heap_snapshot_record_hidden_edge(new_obj, a->data, jl_array_nbytes(a), flags.pooled);
                    if (bits == GC_OLD_MARKED) {
                        ptls->gc_cache.perm_scanned_bytes += jl_array_nbytes(a);
                    }
                    else {
                        ptls->gc_cache.scanned_bytes += jl_array_nbytes(a);
                    }
                }
            }
            else if (flags.how == 3) {
                jl_value_t *owner = jl_array_data_owner(a);
                uintptr_t nptr = (1 << 2) | (bits & GC_OLD);
                gc_try_claim_and_push(mq, owner, &nptr);
                gc_heap_snapshot_record_internal_array_edge(new_obj, owner);
                gc_mark_push_remset(ptls, new_obj, nptr);
                return;
            }
            if (!a->data || jl_array_len(a) == 0)
                return;
            if (flags.ptrarray) {
                if ((jl_datatype_t *)jl_tparam0(vt) == jl_symbol_type)
                    return;
                size_t l = jl_array_len(a);
                jl_value_t *objary_parent = new_obj;
                jl_value_t **objary_begin = (jl_value_t **)a->data;
                jl_value_t **objary_end = objary_begin + l;
                uint32_t step = 1;
                uintptr_t nptr = (l << 2) | (bits & GC_OLD);
                gc_mark_objarray(ptls, objary_parent, objary_begin, objary_end, step, nptr);
            }
            else if (flags.hasptr) {
                jl_datatype_t *et = (jl_datatype_t *)jl_tparam0(vt);
                const jl_datatype_layout_t *layout = et->layout;
                unsigned npointers = layout->npointers;
                unsigned elsize = a->elsize / sizeof(jl_value_t *);
                size_t l = jl_array_len(a);
                jl_value_t *objary_parent = new_obj;
                jl_value_t **objary_begin = (jl_value_t **)a->data;
                jl_value_t **objary_end = objary_begin + l * elsize;
                uint32_t step = elsize;
                uintptr_t nptr = ((l * npointers) << 2) | (bits & GC_OLD);
                if (npointers == 1) { // TODO: detect anytime time stride is uniform?
                    objary_begin += layout->first_ptr;
                    gc_mark_objarray(ptls, objary_parent, objary_begin, objary_end, step, nptr);
                }
                else if (layout->fielddesc_type == 0) {
                    uint8_t *obj8_begin = (uint8_t *)jl_dt_layout_ptrs(layout);
                    uint8_t *obj8_end = obj8_begin + npointers;
                    gc_mark_array8(ptls, objary_parent, objary_begin, objary_end, obj8_begin,
                                   obj8_end, nptr);
                }
                else if (layout->fielddesc_type == 1) {
                    uint16_t *obj16_begin = (uint16_t *)jl_dt_layout_ptrs(layout);
                    uint16_t *obj16_end = obj16_begin + npointers;
                    gc_mark_array16(ptls, objary_parent, objary_begin, objary_end, obj16_begin,
                                    obj16_end, nptr);
                }
                else {
                    assert(0 && "unimplemented");
                }
            }
        }
        else if (vt == jl_module_type) {
            if (update_meta)
                gc_setmark(ptls, o, bits, sizeof(jl_module_t));
            else if (foreign_alloc)
                objprofile_count(vt, bits == GC_OLD_MARKED, sizeof(jl_module_t));
            jl_module_t *mb_parent = (jl_module_t *)new_obj;
            jl_svec_t *bindings = jl_atomic_load_relaxed(&mb_parent->bindings);
            jl_binding_t **table = (jl_binding_t**)jl_svec_data(bindings);
            size_t bsize = jl_svec_len(bindings);
            uintptr_t nptr = ((bsize + mb_parent->usings.len + 1) << 2) | (bits & GC_OLD);
            jl_binding_t **mb_begin = table + 1;
            jl_binding_t **mb_end = table + bsize;
            gc_mark_module_binding(ptls, mb_parent, mb_begin, mb_end, nptr, bits);
        }
        else if (vt == jl_task_type) {
            if (update_meta)
                gc_setmark(ptls, o, bits, sizeof(jl_task_t));
            else if (foreign_alloc)
                objprofile_count(vt, bits == GC_OLD_MARKED, sizeof(jl_task_t));
            jl_task_t *ta = (jl_task_t *)new_obj;
            gc_scrub_record_task(ta);
            if (gc_cblist_task_scanner) {
                int16_t tid = jl_atomic_load_relaxed(&ta->tid);
                gc_invoke_callbacks(jl_gc_cb_task_scanner_t, gc_cblist_task_scanner,
                                    (ta, tid != -1 && ta == gc_all_tls_states[tid]->root_task));
            }
    #ifdef COPY_STACKS
            void *stkbuf = ta->stkbuf;
            if (stkbuf && ta->copy_stack) {
                gc_setmark_buf_(ptls, stkbuf, bits, ta->bufsz);
                // For gc_heap_snapshot_record:
                // TODO: attribute size of stack
                // TODO: edge to stack data
                // TODO: synthetic node for stack data (how big is it?)
            }
    #endif
            jl_gcframe_t *s = ta->gcstack;
            size_t nroots;
            uintptr_t offset = 0;
            uintptr_t lb = 0;
            uintptr_t ub = (uintptr_t)-1;
    #ifdef COPY_STACKS
            if (stkbuf && ta->copy_stack && !ta->ptls) {
                int16_t tid = jl_atomic_load_relaxed(&ta->tid);
                assert(tid >= 0);
                jl_ptls_t ptls2 = gc_all_tls_states[tid];
                ub = (uintptr_t)ptls2->stackbase;
                lb = ub - ta->copy_stack;
                offset = (uintptr_t)stkbuf - lb;
            }
    #endif
            if (s != NULL) {
                nroots = gc_read_stack(&s->nroots, offset, lb, ub);
                gc_heap_snapshot_record_task_to_frame_edge(ta, s);
                assert(nroots <= UINT32_MAX);
                gc_mark_stack(ptls, s, (uint32_t)nroots, offset, lb, ub);
            }
            if (ta->excstack) {
                jl_excstack_t *excstack = ta->excstack;
                gc_heap_snapshot_record_task_to_frame_edge(ta, excstack);
                size_t itr = ta->excstack->top;
                gc_setmark_buf_(ptls, excstack, bits,
                                sizeof(jl_excstack_t) +
                                    sizeof(uintptr_t) * excstack->reserved_size);
                gc_mark_excstack(ptls, excstack, itr);
            }
            const jl_datatype_layout_t *layout = jl_task_type->layout;
            assert(layout->fielddesc_type == 0);
            assert(layout->nfields > 0);
            uint32_t npointers = layout->npointers;
            char *obj8_parent = (char *)ta;
            uint8_t *obj8_begin = (uint8_t *)jl_dt_layout_ptrs(layout);
            uint8_t *obj8_end = obj8_begin + npointers;
            // assume tasks always reference young objects: set lowest bit
            uintptr_t nptr = (npointers << 2) | 1 | bits;
            new_obj = gc_mark_obj8(ptls, obj8_parent, obj8_begin, obj8_end, nptr);
            if (new_obj != NULL) {
                if (!meta_updated)
                    goto mark_obj;
                else
                    gc_markqueue_push(mq, new_obj);
            }
        }
        else if (vt == jl_string_type) {
            size_t dtsz = jl_string_len(new_obj) + sizeof(size_t) + 1;
            if (update_meta)
                gc_setmark(ptls, o, bits, dtsz);
            else if (foreign_alloc)
                objprofile_count(vt, bits == GC_OLD_MARKED, dtsz);
        }
        else {
            if (__unlikely(!jl_is_datatype(vt)))
                gc_assert_datatype_fail(ptls, vt, mq);
            size_t dtsz = jl_datatype_size(vt);
            if (update_meta)
                gc_setmark(ptls, o, bits, dtsz);
            else if (foreign_alloc)
                objprofile_count(vt, bits == GC_OLD_MARKED, dtsz);
            if (vt == jl_weakref_type)
                return;
            const jl_datatype_layout_t *layout = vt->layout;
            uint32_t npointers = layout->npointers;
            if (npointers == 0)
                return;
            uintptr_t nptr = (npointers << 2 | (bits & GC_OLD));
            assert((layout->nfields > 0 || layout->fielddesc_type == 3) &&
                   "opaque types should have been handled specially");
            if (layout->fielddesc_type == 0) {
                char *obj8_parent = (char *)new_obj;
                uint8_t *obj8_begin = (uint8_t *)jl_dt_layout_ptrs(layout);
                uint8_t *obj8_end = obj8_begin + npointers;
                assert(obj8_begin < obj8_end);
                new_obj = gc_mark_obj8(ptls, obj8_parent, obj8_begin, obj8_end, nptr);
                if (new_obj != NULL) {
                    if (!meta_updated)
                        goto mark_obj;
                    else
                        gc_markqueue_push(mq, new_obj);
                }
            }
            else if (layout->fielddesc_type == 1) {
                char *obj16_parent = (char *)new_obj;
                uint16_t *obj16_begin = (uint16_t *)jl_dt_layout_ptrs(layout);
                uint16_t *obj16_end = obj16_begin + npointers;
                assert(obj16_begin < obj16_end);
                new_obj = gc_mark_obj16(ptls, obj16_parent, obj16_begin, obj16_end, nptr);
                if (new_obj != NULL) {
                    if (!meta_updated)
                        goto mark_obj;
                    else
                        gc_markqueue_push(mq, new_obj);
                }
            }
            else if (layout->fielddesc_type == 2) {
                // This is very uncommon
                // Do not do store to load forwarding to save some code size
                char *obj32_parent = (char *)new_obj;
                uint32_t *obj32_begin = (uint32_t *)jl_dt_layout_ptrs(layout);
                uint32_t *obj32_end = obj32_begin + npointers;
                assert(obj32_begin < obj32_end);
                new_obj = gc_mark_obj32(ptls, obj32_parent, obj32_begin, obj32_end, nptr);
                if (new_obj != NULL) {
                    if (!meta_updated)
                        goto mark_obj;
                    else
                        gc_markqueue_push(mq, new_obj);
                }
            }
            else {
                assert(layout->fielddesc_type == 3);
                jl_fielddescdyn_t *desc = (jl_fielddescdyn_t *)jl_dt_layout_fields(layout);
                int old = jl_astaggedvalue(new_obj)->bits.gc & 2;
                uintptr_t young = desc->markfunc(ptls, new_obj);
                if (old && young)
                    gc_mark_push_remset(ptls, new_obj, young * 4 + 3);
            }
        }
    }
}

// Used in gc-debug
void gc_mark_loop_(jl_ptls_t ptls, jl_gc_markqueue_t *mq)
{
    while (1) {
        void *new_obj = (void *)gc_markqueue_pop(&ptls->mark_queue);
        // No more objects to mark
        if (__unlikely(new_obj == NULL)) {
            // TODO: work-stealing comes here...
            return;
        }
        gc_mark_outrefs(ptls, mq, new_obj, 0);
    }
}

// Drain items from worker's own chunkqueue
void gc_drain_own_chunkqueue(jl_ptls_t ptls, jl_gc_markqueue_t *mq)
{
    jl_gc_chunk_t c = {.cid = GC_empty_chunk};
    do {
        c = gc_chunkqueue_pop(mq);
        if (c.cid != GC_empty_chunk) {
            gc_mark_chunk(ptls, mq, &c);
            gc_mark_loop_(ptls, mq);
        }
    } while (c.cid != GC_empty_chunk);
}

// Main mark loop. Single stack (allocated on the heap) of `jl_value_t *`
// is used to keep track of processed items. Maintaning this stack (instead of
// native one) avoids stack overflow when marking deep objects and
// makes it easier to implement parallel marking via work-stealing
JL_EXTENSION NOINLINE void gc_mark_loop(jl_ptls_t ptls)
{
    gc_mark_loop_(ptls, &ptls->mark_queue);
    gc_drain_own_chunkqueue(ptls, &ptls->mark_queue);
}

static void gc_premark(jl_ptls_t ptls2)
{
    arraylist_t *remset = ptls2->heap.remset;
    ptls2->heap.remset = ptls2->heap.last_remset;
    ptls2->heap.last_remset = remset;
    ptls2->heap.remset->len = 0;
    ptls2->heap.remset_nptr = 0;
    // avoid counting remembered objects
    // in `perm_scanned_bytes`
    size_t len = remset->len;
    void **items = remset->items;
    for (size_t i = 0; i < len; i++) {
        jl_value_t *item = (jl_value_t *)items[i];
        objprofile_count(jl_typeof(item), 2, 0);
        jl_astaggedvalue(item)->bits.gc = GC_OLD_MARKED;
    }
}

static void gc_queue_thread_local(jl_gc_markqueue_t *mq, jl_ptls_t ptls2)
{
    jl_task_t *task;
    task = ptls2->root_task;
    if (task != NULL) {
        gc_try_claim_and_push(mq, task, NULL);
        gc_heap_snapshot_record_root((jl_value_t*)task, "root task");
    }
    task = jl_atomic_load_relaxed(&ptls2->current_task);
    if (task != NULL) {
        gc_try_claim_and_push(mq, task, NULL);
        gc_heap_snapshot_record_root((jl_value_t*)task, "current task");
    }
    task = ptls2->next_task;
    if (task != NULL) {
        gc_try_claim_and_push(mq, task, NULL);
        gc_heap_snapshot_record_root((jl_value_t*)task, "next task");
    }
    task = ptls2->previous_task;
    if (task != NULL) {
        gc_try_claim_and_push(mq, task, NULL);
        gc_heap_snapshot_record_root((jl_value_t*)task, "previous task");
    }
    if (ptls2->previous_exception) {
        gc_try_claim_and_push(mq, ptls2->previous_exception, NULL);
        gc_heap_snapshot_record_root((jl_value_t*)ptls2->previous_exception, "previous exception");
    }
}

static void gc_queue_bt_buf(jl_gc_markqueue_t *mq, jl_ptls_t ptls2)
{
    jl_bt_element_t *bt_data = ptls2->bt_data;
    size_t bt_size = ptls2->bt_size;
    for (size_t i = 0; i < bt_size; i += jl_bt_entry_size(bt_data + i)) {
        jl_bt_element_t *bt_entry = bt_data + i;
        if (jl_bt_is_native(bt_entry))
            continue;
        size_t njlvals = jl_bt_num_jlvals(bt_entry);
        for (size_t j = 0; j < njlvals; j++)
            gc_try_claim_and_push(mq, jl_bt_entry_jlvalue(bt_entry, j), NULL);
    }
}

static void gc_queue_remset(jl_ptls_t ptls, jl_ptls_t ptls2)
{
    size_t len = ptls2->heap.last_remset->len;
    void **items = ptls2->heap.last_remset->items;
    for (size_t i = 0; i < len; i++) {
        // Objects in the `remset` are already marked,
        // so a `gc_try_claim_and_push` wouldn't work here
        gc_mark_outrefs(ptls, &ptls->mark_queue, (jl_value_t *)items[i], 1);
    }
}

extern jl_value_t *cmpswap_names JL_GLOBALLY_ROOTED;
extern jl_task_t *wait_empty JL_GLOBALLY_ROOTED;

// mark the initial root set
static void gc_mark_roots(jl_gc_markqueue_t *mq)
{
    // modules
    gc_try_claim_and_push(mq, jl_main_module, NULL);
    gc_heap_snapshot_record_root((jl_value_t*)jl_main_module, "main_module");
    // invisible builtin values
    gc_try_claim_and_push(mq, jl_an_empty_vec_any, NULL);
    gc_try_claim_and_push(mq, jl_module_init_order, NULL);
    for (size_t i = 0; i < jl_current_modules.size; i += 2) {
        if (jl_current_modules.table[i + 1] != HT_NOTFOUND) {
            gc_try_claim_and_push(mq, jl_current_modules.table[i], NULL);
            gc_heap_snapshot_record_root((jl_value_t*)jl_current_modules.table[i], "top level module");
        }
    }
    gc_try_claim_and_push(mq, jl_anytuple_type_type, NULL);
    for (size_t i = 0; i < N_CALL_CACHE; i++) {
        jl_typemap_entry_t *v = jl_atomic_load_relaxed(&call_cache[i]);
        gc_try_claim_and_push(mq, v, NULL);
    }
    gc_try_claim_and_push(mq, jl_all_methods, NULL);
    gc_try_claim_and_push(mq, _jl_debug_method_invalidation, NULL);
    // constants
    gc_try_claim_and_push(mq, jl_emptytuple_type, NULL);
    gc_try_claim_and_push(mq, cmpswap_names, NULL);
    gc_try_claim_and_push(mq, jl_global_roots_table, NULL);
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
        void *v = gc_ptr_clear_tag(v0, 3);
        if (__unlikely(!v0)) {
            // remove from this list
            continue;
        }

        void *fin = items[i+1];
        int isfreed;
        int isold;
        if (gc_ptr_tag(v0, 2)) {
            isfreed = 1;
            isold = 0;
        }
        else {
            isfreed = !gc_marked(jl_astaggedvalue(v)->bits.gc);
            isold = (list != &finalizer_list_marked &&
                     jl_astaggedvalue(v)->bits.gc == GC_OLD_MARKED &&
                     jl_astaggedvalue(fin)->bits.gc == GC_OLD_MARKED);
        }
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
static _Atomic(uint32_t) jl_gc_disable_counter = 1;

JL_DLLEXPORT int jl_gc_enable(int on)
{
    jl_ptls_t ptls = jl_current_task->ptls;
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
    jl_ptls_t ptls = jl_current_task->ptls;
    return !ptls->disable_gc;
}

JL_DLLEXPORT void jl_gc_get_total_bytes(int64_t *bytes) JL_NOTSAFEPOINT
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

JL_DLLEXPORT void jl_gc_reset_stats(void)
{
    gc_num.max_pause = 0;
    gc_num.max_memory = 0;
    gc_num.max_time_to_safepoint = 0;
}

// TODO: these were supposed to be thread local
JL_DLLEXPORT int64_t jl_gc_diff_total_bytes(void) JL_NOTSAFEPOINT
{
    int64_t oldtb = last_gc_total_bytes;
    int64_t newtb;
    jl_gc_get_total_bytes(&newtb);
    last_gc_total_bytes = newtb;
    return newtb - oldtb;
}

JL_DLLEXPORT int64_t jl_gc_sync_total_bytes(int64_t offset) JL_NOTSAFEPOINT
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

size_t jl_maxrss(void);

// Only one thread should be running in this function
static int _jl_gc_collect(jl_ptls_t ptls, jl_gc_collection_t collection)
{
    combine_thread_gc_counts(&gc_num);

#ifdef USE_TRACY
    TracyCPlot("Heap size", live_bytes + gc_num.allocd);
#endif

    jl_gc_markqueue_t *mq = &ptls->mark_queue;

    uint64_t gc_start_time = jl_hrtime();
    int64_t last_perm_scanned_bytes = perm_scanned_bytes;
    uint64_t start_mark_time = jl_hrtime();
    JL_PROBE_GC_MARK_BEGIN();
    {
        JL_TIMING(GC, GC_Mark);

        // 1. fix GC bits of objects in the remset.
        assert(gc_n_threads);
        for (int t_i = 0; t_i < gc_n_threads; t_i++) {
            jl_ptls_t ptls2 = gc_all_tls_states[t_i];
            if (ptls2 != NULL)
                gc_premark(ptls2);
        }

        assert(gc_n_threads);
        for (int t_i = 0; t_i < gc_n_threads; t_i++) {
            jl_ptls_t ptls2 = gc_all_tls_states[t_i];
            if (ptls2 != NULL) {
                // 2.1. mark every thread local root
                gc_queue_thread_local(mq, ptls2);
                // 2.2. mark any managed objects in the backtrace buffer
                // TODO: treat these as roots for gc_heap_snapshot_record
                gc_queue_bt_buf(mq, ptls2);
                // 2.3. mark every object in the `last_remsets` and `rem_binding`
                gc_queue_remset(ptls, ptls2);
            }
        }

        // 3. walk roots
        gc_mark_roots(mq);
        if (gc_cblist_root_scanner) {
            gc_invoke_callbacks(jl_gc_cb_root_scanner_t,
                gc_cblist_root_scanner, (collection));
        }
        gc_mark_loop(ptls);

        // 4. check for objects to finalize
        clear_weak_refs();
        // Record the length of the marked list since we need to
        // mark the object moved to the marked list from the
        // `finalizer_list` by `sweep_finalizer_list`
        size_t orig_marked_len = finalizer_list_marked.len;
        assert(gc_n_threads);
        for (int i = 0; i < gc_n_threads; i++) {
            jl_ptls_t ptls2 = gc_all_tls_states[i];
            if (ptls2 != NULL)
                sweep_finalizer_list(&ptls2->finalizers);
        }
        if (prev_sweep_full) {
            sweep_finalizer_list(&finalizer_list_marked);
            orig_marked_len = 0;
        }
        assert(gc_n_threads);
        for (int i = 0; i < gc_n_threads; i++) {
            jl_ptls_t ptls2 = gc_all_tls_states[i];
            if (ptls2 != NULL)
                gc_mark_finlist(mq, &ptls2->finalizers, 0);
        }
        gc_mark_finlist(mq, &finalizer_list_marked, orig_marked_len);
        // "Flush" the mark stack before flipping the reset_age bit
        // so that the objects are not incorrectly reset.
        gc_mark_loop(ptls);
        // Conservative marking relies on age to tell allocated objects
        // and freelist entries apart.
        mark_reset_age = !jl_gc_conservative_gc_support_enabled();
        // Reset the age and old bit for any unmarked objects referenced by the
        // `to_finalize` list. These objects are only reachable from this list
        // and should not be referenced by any old objects so this won't break
        // the GC invariant.
        gc_mark_finlist(mq, &to_finalize, 0);
        gc_mark_loop(ptls);
        mark_reset_age = 0;
    }

    gc_num.since_sweep += gc_num.allocd;
    JL_PROBE_GC_MARK_END(scanned_bytes, perm_scanned_bytes);
    gc_settime_premark_end();
    gc_time_mark_pause(gc_start_time, scanned_bytes, perm_scanned_bytes);
    uint64_t end_mark_time = jl_hrtime();
    uint64_t mark_time = end_mark_time - start_mark_time;
    gc_num.mark_time = mark_time;
    gc_num.total_mark_time += mark_time;
    int64_t actual_allocd = gc_num.since_sweep;
    gc_settime_postmark_end();
    // marking is over

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
    int not_freed_enough = (collection == JL_GC_AUTO) && estimate_freed < (7*(actual_allocd/10));
    int nptr = 0;
    assert(gc_n_threads);
    for (int i = 0; i < gc_n_threads; i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[i];
        if (ptls2 != NULL)
            nptr += ptls2->heap.remset_nptr;
    }

    // many pointers in the intergen frontier => "quick" mark is not quick
    int large_frontier = nptr*sizeof(void*) >= default_collect_interval;
    int sweep_full = 0;
    int recollect = 0;

    // update heuristics only if this GC was automatically triggered
    if (collection == JL_GC_AUTO) {
        if (large_frontier) {
            sweep_full = 1;
            gc_num.interval = last_long_collect_interval;
        }
        if (not_freed_enough || large_frontier) {
            gc_num.interval = gc_num.interval * 2;
        }

        size_t maxmem = 0;
#ifdef _P64
        // on a big memory machine, increase max_collect_interval to totalmem / nthreads / 2
        maxmem = total_mem / gc_n_threads / 2;
#endif
        if (maxmem < max_collect_interval)
            maxmem = max_collect_interval;
        if (gc_num.interval > maxmem) {
            sweep_full = 1;
            gc_num.interval = maxmem;
        }
    }

    // If the live data outgrows the suggested max_total_memory
    // we keep going with minimum intervals and full gcs until
    // we either free some space or get an OOM error.
    if (live_bytes > max_total_memory) {
        sweep_full = 1;
    }
    if (gc_sweep_always_full) {
        sweep_full = 1;
    }
    if (collection == JL_GC_FULL && !prev_sweep_full) {
        sweep_full = 1;
        recollect = 1;
    }
    if (sweep_full) {
        // these are the difference between the number of gc-perm bytes scanned
        // on the first collection after sweep_full, and the current scan
        perm_scanned_bytes = 0;
        promoted_bytes = 0;
        last_long_collect_interval = gc_num.interval;
    }
    scanned_bytes = 0;
    // 6. start sweeping
    uint64_t start_sweep_time = jl_hrtime();
    JL_PROBE_GC_SWEEP_BEGIN(sweep_full);
    {
        JL_TIMING(GC, GC_Sweep);
#ifdef USE_TRACY
        if (sweep_full) {
            TracyCZoneCtx ctx = *(JL_TIMING_CURRENT_BLOCK->tracy_ctx);
            TracyCZoneColor(ctx, 0xFFA500);
        }
#endif
        sweep_weak_refs();
        sweep_stack_pools();
        gc_sweep_foreign_objs();
        gc_sweep_other(ptls, sweep_full);
        gc_scrub();
        gc_verify_tags();
        gc_sweep_pool(sweep_full);
        if (sweep_full)
            gc_sweep_perm_alloc();
    }
    JL_PROBE_GC_SWEEP_END();

    uint64_t gc_end_time = jl_hrtime();
    uint64_t pause = gc_end_time - gc_start_time;
    uint64_t sweep_time = gc_end_time - start_sweep_time;
    gc_num.total_sweep_time += sweep_time;
    gc_num.sweep_time = sweep_time;

    // sweeping is over
    // 7. if it is a quick sweep, put back the remembered objects in queued state
    // so that we don't trigger the barrier again on them.
    assert(gc_n_threads);
    for (int t_i = 0; t_i < gc_n_threads; t_i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[t_i];
        if (ptls2 == NULL)
            continue;
        if (!sweep_full) {
            for (int i = 0; i < ptls2->heap.remset->len; i++) {
                void *ptr = ptls2->heap.remset->items[i];
                jl_astaggedvalue(ptr)->bits.gc = GC_MARKED;
            }
        }
        else {
            ptls2->heap.remset->len = 0;
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

    _report_gc_finished(pause, gc_num.freed, sweep_full, recollect);

    gc_final_pause_end(gc_start_time, gc_end_time);
    gc_time_sweep_pause(gc_end_time, actual_allocd, live_bytes,
                        estimate_freed, sweep_full);
    gc_num.full_sweep += sweep_full;
    uint64_t max_memory = last_live_bytes + gc_num.allocd;
    if (max_memory > gc_num.max_memory) {
        gc_num.max_memory = max_memory;
    }

    gc_num.allocd = 0;
    last_live_bytes = live_bytes;
    live_bytes += -gc_num.freed + gc_num.since_sweep;

    if (collection == JL_GC_AUTO) {
        //If we aren't freeing enough or are seeing lots and lots of pointers let it increase faster
        if(!not_freed_enough || large_frontier) {
            int64_t tot = 2 * (live_bytes + gc_num.since_sweep) / 3;
            if (gc_num.interval > tot) {
                gc_num.interval = tot;
                last_long_collect_interval = tot;
            }
        // If the current interval is larger than half the live data decrease the interval
        } else {
            int64_t half = (live_bytes / 2);
            if (gc_num.interval > half)
                gc_num.interval = half;
        }

        // But never go below default
        if (gc_num.interval < default_collect_interval) gc_num.interval = default_collect_interval;
    }

    if (gc_num.interval + live_bytes > max_total_memory) {
        if (live_bytes < max_total_memory) {
            gc_num.interval = max_total_memory - live_bytes;
            last_long_collect_interval = max_total_memory - live_bytes;
        }
        else {
            // We can't stay under our goal so let's go back to
            // the minimum interval and hope things get better
            gc_num.interval = default_collect_interval;
        }
    }

    gc_time_summary(sweep_full, t_start, gc_end_time, gc_num.freed,
                    live_bytes, gc_num.interval, pause,
                    gc_num.time_to_safepoint,
                    gc_num.mark_time, gc_num.sweep_time);

    prev_sweep_full = sweep_full;
    gc_num.pause += !recollect;
    gc_num.total_time += pause;
    gc_num.since_sweep = 0;
    gc_num.freed = 0;
    if (pause > gc_num.max_pause) {
        gc_num.max_pause = pause;
    }
    reset_thread_gc_counts();

    return recollect;
}

JL_DLLEXPORT void jl_gc_collect(jl_gc_collection_t collection)
{
    JL_PROBE_GC_BEGIN(collection);

    jl_task_t *ct = jl_current_task;
    jl_ptls_t ptls = ct->ptls;
    if (jl_atomic_load_relaxed(&jl_gc_disable_counter)) {
        size_t localbytes = jl_atomic_load_relaxed(&ptls->gc_num.allocd) + gc_num.interval;
        jl_atomic_store_relaxed(&ptls->gc_num.allocd, -(int64_t)gc_num.interval);
        static_assert(sizeof(_Atomic(uint64_t)) == sizeof(gc_num.deferred_alloc), "");
        jl_atomic_fetch_add((_Atomic(uint64_t)*)&gc_num.deferred_alloc, localbytes);
        return;
    }
    jl_gc_debug_print();

    int8_t old_state = jl_atomic_load_relaxed(&ptls->gc_state);
    jl_atomic_store_release(&ptls->gc_state, JL_GC_STATE_WAITING);
    // `jl_safepoint_start_gc()` makes sure only one thread can
    // run the GC.
    uint64_t t0 = jl_hrtime();
    if (!jl_safepoint_start_gc()) {
        // Multithread only. See assertion in `safepoint.c`
        jl_gc_state_set(ptls, old_state, JL_GC_STATE_WAITING);
        return;
    }

    JL_TIMING_SUSPEND(GC, ct);
    JL_TIMING(GC, GC);

    int last_errno = errno;
#ifdef _OS_WINDOWS_
    DWORD last_error = GetLastError();
#endif
    // Now we are ready to wait for other threads to hit the safepoint,
    // we can do a few things that doesn't require synchronization.
    //
    // We must sync here with the tls_lock operations, so that we have a
    // seq-cst order between these events now we know that either the new
    // thread must run into our safepoint flag or we must observe the
    // existence of the thread in the jl_n_threads count.
    //
    // TODO: concurrently queue objects
    jl_fence();
    gc_n_threads = jl_atomic_load_acquire(&jl_n_threads);
    gc_all_tls_states = jl_atomic_load_relaxed(&jl_all_tls_states);
    jl_gc_wait_for_the_world(gc_all_tls_states, gc_n_threads);
    JL_PROBE_GC_STOP_THE_WORLD();

    uint64_t t1 = jl_hrtime();
    uint64_t duration = t1 - t0;
    if (duration > gc_num.max_time_to_safepoint)
        gc_num.max_time_to_safepoint = duration;
    gc_num.time_to_safepoint = duration;
    gc_num.total_time_to_safepoint += duration;

    gc_invoke_callbacks(jl_gc_cb_pre_gc_t,
        gc_cblist_pre_gc, (collection));

    if (!jl_atomic_load_relaxed(&jl_gc_disable_counter)) {
        JL_LOCK_NOGC(&finalizers_lock); // all the other threads are stopped, so this does not make sense, right? otherwise, failing that, this seems like plausibly a deadlock
#ifndef __clang_gcanalyzer__
        if (_jl_gc_collect(ptls, collection)) {
            // recollect
            int ret = _jl_gc_collect(ptls, JL_GC_AUTO);
            (void)ret;
            assert(!ret);
        }
#endif
        JL_UNLOCK_NOGC(&finalizers_lock);
    }

    gc_n_threads = 0;
    gc_all_tls_states = NULL;
    jl_safepoint_end_gc();
    jl_gc_state_set(ptls, old_state, JL_GC_STATE_WAITING);
    JL_PROBE_GC_END();

    // Only disable finalizers on current thread
    // Doing this on all threads is racy (it's impossible to check
    // or wait for finalizers on other threads without dead lock).
    if (!ptls->finalizers_inhibited && ptls->locks.len == 0) {
        JL_TIMING(GC, GC_Finalizers);
        run_finalizers(ct);
    }
    JL_PROBE_GC_FINALIZER();

    gc_invoke_callbacks(jl_gc_cb_post_gc_t,
        gc_cblist_post_gc, (collection));
#ifdef _OS_WINDOWS_
    SetLastError(last_error);
#endif
    errno = last_errno;

#ifdef USE_TRACY
    TracyCPlot("Heap size", jl_gc_live_bytes());
#endif
}

void gc_mark_queue_all_roots(jl_ptls_t ptls, jl_gc_markqueue_t *mq)
{
    assert(gc_n_threads);
    for (size_t i = 0; i < gc_n_threads; i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[i];
        if (ptls2)
            gc_queue_thread_local(mq, ptls2);
    }
    gc_mark_roots(mq);
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
    for (int i = 0; i < JL_GC_N_POOLS; i++) {
        p[i].osize = jl_gc_sizeclasses[i];
        p[i].freelist = NULL;
        p[i].newpages = NULL;
    }
    arraylist_new(&heap->weak_refs, 0);
    arraylist_new(&heap->live_tasks, 0);
    heap->mallocarrays = NULL;
    heap->mafreelist = NULL;
    heap->big_objects = NULL;
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

    // Initialize GC mark-queue
    size_t init_size = (1 << 18);
    jl_gc_markqueue_t *mq = &ptls->mark_queue;
    mq->start = (jl_value_t **)malloc_s(init_size * sizeof(jl_value_t *));
    mq->current = mq->start;
    mq->end = mq->start + init_size;
    size_t cq_init_size = (1 << 14);
    mq->current_chunk = mq->chunk_start = (jl_gc_chunk_t *)malloc_s(cq_init_size * sizeof(jl_gc_chunk_t));
    mq->chunk_end = mq->chunk_start + cq_init_size;

    memset(&ptls->gc_num, 0, sizeof(ptls->gc_num));
    jl_atomic_store_relaxed(&ptls->gc_num.allocd, -(int64_t)gc_num.interval);
}

// System-wide initializations
void jl_gc_init(void)
{

    JL_MUTEX_INIT(&heapsnapshot_lock, "heapsnapshot_lock");
    JL_MUTEX_INIT(&finalizers_lock, "finalizers_lock");
    uv_mutex_init(&gc_cache_lock);
    uv_mutex_init(&gc_perm_lock);

    jl_gc_init_page();
    jl_gc_debug_init();

    arraylist_new(&finalizer_list_marked, 0);
    arraylist_new(&to_finalize, 0);
    arraylist_new(&eytzinger_image_tree, 0);
    arraylist_new(&eytzinger_idxs, 0);
    arraylist_push(&eytzinger_idxs, (void*)0);
    arraylist_push(&eytzinger_image_tree, (void*)1); // outside image

    gc_num.interval = default_collect_interval;
    last_long_collect_interval = default_collect_interval;
    gc_num.allocd = 0;
    gc_num.max_pause = 0;
    gc_num.max_memory = 0;

#ifdef _P64
    total_mem = uv_get_total_memory();
    uint64_t constrained_mem = uv_get_constrained_memory();
    if (constrained_mem > 0 && constrained_mem < total_mem)
        total_mem = constrained_mem;
    double percent;
    if (total_mem < 128e9)
        percent = total_mem * 2.34375e-12 + 0.6; // 60% at 0 gigs and 90% at 128 to not
    else                                         // overcommit too much on memory contrained devices
        percent = 0.9;
    max_total_memory = total_mem * percent;
#endif
    if (jl_options.heap_size_hint)
        jl_gc_set_max_memory(jl_options.heap_size_hint);

#ifdef USE_TRACY
    TracyCPlotConfig("Heap size", TracyPlotFormatMemory, /* rectilinear */ 0, /* fill */ 1, /* color */ 0);
#endif
    t_start = jl_hrtime();
}

JL_DLLEXPORT void jl_gc_set_max_memory(uint64_t max_mem)
{
    if (max_mem > 0
        && max_mem < (uint64_t)1 << (sizeof(memsize_t) * 8 - 1)) {
        max_total_memory = max_mem;
    }
}

JL_DLLEXPORT uint64_t jl_gc_get_max_memory(void)
{
    return max_total_memory;
}

// callback for passing OOM errors from gmp
JL_DLLEXPORT void jl_throw_out_of_memory_error(void)
{
    jl_throw(jl_memory_exception);
}

// allocation wrappers that track allocation and let collection run

JL_DLLEXPORT void *jl_gc_counted_malloc(size_t sz)
{
    jl_gcframe_t **pgcstack = jl_get_pgcstack();
    jl_task_t *ct = jl_current_task;
    if (pgcstack != NULL && ct->world_age) {
        jl_ptls_t ptls = ct->ptls;
        maybe_collect(ptls);
        jl_atomic_store_relaxed(&ptls->gc_num.allocd,
            jl_atomic_load_relaxed(&ptls->gc_num.allocd) + sz);
        jl_atomic_store_relaxed(&ptls->gc_num.malloc,
            jl_atomic_load_relaxed(&ptls->gc_num.malloc) + 1);
    }
    return malloc(sz);
}

JL_DLLEXPORT void *jl_gc_counted_calloc(size_t nm, size_t sz)
{
    jl_gcframe_t **pgcstack = jl_get_pgcstack();
    jl_task_t *ct = jl_current_task;
    if (pgcstack != NULL && ct->world_age) {
        jl_ptls_t ptls = ct->ptls;
        maybe_collect(ptls);
        jl_atomic_store_relaxed(&ptls->gc_num.allocd,
            jl_atomic_load_relaxed(&ptls->gc_num.allocd) + nm*sz);
        jl_atomic_store_relaxed(&ptls->gc_num.malloc,
            jl_atomic_load_relaxed(&ptls->gc_num.malloc) + 1);
    }
    return calloc(nm, sz);
}

JL_DLLEXPORT void jl_gc_counted_free_with_size(void *p, size_t sz)
{
    jl_gcframe_t **pgcstack = jl_get_pgcstack();
    jl_task_t *ct = jl_current_task;
    free(p);
    if (pgcstack != NULL && ct->world_age) {
        jl_ptls_t ptls = ct->ptls;
        jl_atomic_store_relaxed(&ptls->gc_num.freed,
            jl_atomic_load_relaxed(&ptls->gc_num.freed) + sz);
        jl_atomic_store_relaxed(&ptls->gc_num.freecall,
            jl_atomic_load_relaxed(&ptls->gc_num.freecall) + 1);
    }
}

JL_DLLEXPORT void *jl_gc_counted_realloc_with_old_size(void *p, size_t old, size_t sz)
{
    jl_gcframe_t **pgcstack = jl_get_pgcstack();
    jl_task_t *ct = jl_current_task;
    if (pgcstack != NULL && ct->world_age) {
        jl_ptls_t ptls = ct->ptls;
        maybe_collect(ptls);
        if (sz < old)
            jl_atomic_store_relaxed(&ptls->gc_num.freed,
                jl_atomic_load_relaxed(&ptls->gc_num.freed) + (old - sz));
        else
            jl_atomic_store_relaxed(&ptls->gc_num.allocd,
                jl_atomic_load_relaxed(&ptls->gc_num.allocd) + (sz - old));
        jl_atomic_store_relaxed(&ptls->gc_num.realloc,
            jl_atomic_load_relaxed(&ptls->gc_num.realloc) + 1);
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

//_unchecked_calloc does not check for potential overflow of nm*sz
STATIC_INLINE void *_unchecked_calloc(size_t nm, size_t sz) {
    size_t nmsz = nm*sz;
    int64_t *p = (int64_t *)jl_gc_counted_calloc(nmsz + JL_SMALL_BYTE_ALIGNMENT, 1);
    if (p == NULL)
        return NULL;
    p[0] = nmsz;
    return (void *)(p + 2); // assumes JL_SMALL_BYTE_ALIGNMENT == 16
}

JL_DLLEXPORT void *jl_calloc(size_t nm, size_t sz)
{
    if (nm > SSIZE_MAX/sz - JL_SMALL_BYTE_ALIGNMENT)
        return NULL;
    return _unchecked_calloc(nm, sz);
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
    jl_ptls_t ptls = jl_current_task->ptls;
    maybe_collect(ptls);
    size_t allocsz = LLT_ALIGN(sz, JL_CACHE_BYTE_ALIGNMENT);
    if (allocsz < sz)  // overflow in adding offs, size was "negative"
        jl_throw(jl_memory_exception);
    jl_atomic_store_relaxed(&ptls->gc_num.allocd,
        jl_atomic_load_relaxed(&ptls->gc_num.allocd) + allocsz);
    jl_atomic_store_relaxed(&ptls->gc_num.malloc,
        jl_atomic_load_relaxed(&ptls->gc_num.malloc) + 1);
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
    // jl_gc_managed_malloc is currently always used for allocating array buffers.
    maybe_record_alloc_to_profile((jl_value_t*)b, sz, (jl_datatype_t*)jl_buff_tag);
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
        jl_atomic_store_relaxed(&ptls->gc_num.freed,
            jl_atomic_load_relaxed(&ptls->gc_num.freed) + (oldsz - allocsz));
    else
        jl_atomic_store_relaxed(&ptls->gc_num.allocd,
            jl_atomic_load_relaxed(&ptls->gc_num.allocd) + (allocsz - oldsz));
    jl_atomic_store_relaxed(&ptls->gc_num.realloc,
        jl_atomic_load_relaxed(&ptls->gc_num.realloc) + 1);

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
    maybe_record_alloc_to_profile((jl_value_t*)b, sz, jl_gc_unknown_type_tag);
    return b;
}

JL_DLLEXPORT void *jl_gc_managed_realloc(void *d, size_t sz, size_t oldsz,
                                         int isaligned, jl_value_t *owner)
{
    jl_ptls_t ptls = jl_current_task->ptls;
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
    jl_ptls_t ptls = jl_current_task->ptls;
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
uv_mutex_t gc_perm_lock;
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
    void *base = zero ? calloc(1, sz) : malloc(sz);
    if (base == NULL)
        jl_throw(jl_memory_exception);
#ifdef _OS_WINDOWS_
    SetLastError(last_error);
#endif
    errno = last_errno;
    jl_may_leak(base);
    assert(align > 0);
    unsigned diff = (offset - (uintptr_t)base) % align;
    return (void*)((char*)base + diff);
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
    uv_mutex_lock(&gc_perm_lock);
    void *p = jl_gc_perm_alloc_nolock(sz, zero, align, offset);
    uv_mutex_unlock(&gc_perm_lock);
    return p;
}

JL_DLLEXPORT void jl_gc_add_finalizer(jl_value_t *v, jl_function_t *f)
{
    jl_ptls_t ptls = jl_current_task->ptls;
    jl_gc_add_finalizer_th(ptls, v, f);
}

JL_DLLEXPORT void jl_finalize(jl_value_t *o)
{
    jl_finalize_th(jl_current_task, o);
}

JL_DLLEXPORT jl_weakref_t *jl_gc_new_weakref(jl_value_t *value)
{
    jl_ptls_t ptls = jl_current_task->ptls;
    return jl_gc_new_weakref_th(ptls, value);
}

JL_DLLEXPORT jl_value_t *jl_gc_allocobj(size_t sz)
{
    jl_ptls_t ptls = jl_current_task->ptls;
    return jl_gc_alloc(ptls, sz, NULL);
}

JL_DLLEXPORT jl_value_t *jl_gc_alloc_0w(void)
{
    jl_ptls_t ptls = jl_current_task->ptls;
    return jl_gc_alloc(ptls, 0, NULL);
}

JL_DLLEXPORT jl_value_t *jl_gc_alloc_1w(void)
{
    jl_ptls_t ptls = jl_current_task->ptls;
    return jl_gc_alloc(ptls, sizeof(void*), NULL);
}

JL_DLLEXPORT jl_value_t *jl_gc_alloc_2w(void)
{
    jl_ptls_t ptls = jl_current_task->ptls;
    return jl_gc_alloc(ptls, sizeof(void*) * 2, NULL);
}

JL_DLLEXPORT jl_value_t *jl_gc_alloc_3w(void)
{
    jl_ptls_t ptls = jl_current_task->ptls;
    return jl_gc_alloc(ptls, sizeof(void*) * 3, NULL);
}

JL_DLLEXPORT int jl_gc_enable_conservative_gc_support(void)
{
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
        int result = jl_atomic_load(&support_conservative_marking);
        jl_atomic_store(&support_conservative_marking, 1);
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
            gc_all_tls_states[meta->thread_n]->heap.norm_pools +
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
        if (!(meta->ages[obj_id / 32] & (1 << (obj_id % 32)))) {
            return NULL;
        }
        // Not a freelist entry, therefore a valid object.
    valid_object:
        // We have to treat objects with type `jl_buff_tag` differently,
        // as they must not be passed to the usual marking functions.
        // Note that jl_buff_tag is real pointer into libjulia,
        // thus it cannot be a type reference.
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

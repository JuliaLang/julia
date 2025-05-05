// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "gc-common.h"
#include "julia.h"
#include "julia_atomics.h"
#include "julia_gcext.h"
#include "julia_assert.h"
#include "threading.h"

#ifdef __cplusplus
extern "C" {
#endif

// =========================================================================== //
// GC Metrics
// =========================================================================== //

jl_gc_num_t gc_num = {0};

JL_DLLEXPORT uint64_t jl_gc_total_hrtime(void)
{
    return gc_num.total_time;
}

// =========================================================================== //
// GC Callbacks
// =========================================================================== //

jl_gc_callback_list_t *gc_cblist_root_scanner;
jl_gc_callback_list_t *gc_cblist_task_scanner;
jl_gc_callback_list_t *gc_cblist_pre_gc;
jl_gc_callback_list_t *gc_cblist_post_gc;
jl_gc_callback_list_t *gc_cblist_notify_external_alloc;
jl_gc_callback_list_t *gc_cblist_notify_external_free;
jl_gc_callback_list_t *gc_cblist_notify_gc_pressure;

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

JL_DLLEXPORT void jl_gc_set_cb_notify_gc_pressure(jl_gc_cb_notify_gc_pressure_t cb, int enable)
{
    if (enable)
        jl_gc_register_callback(&gc_cblist_notify_gc_pressure, (jl_gc_cb_func_t)cb);
    else
        jl_gc_deregister_callback(&gc_cblist_notify_gc_pressure, (jl_gc_cb_func_t)cb);
}

// =========================================================================== //
// malloc wrappers, aligned allocation
// =========================================================================== //

#if defined(_OS_WINDOWS_)
// helper function based partly on wine msvcrt80+ heap.c
// but with several fixes to improve the correctness of the computation and remove unnecessary parameters
#define SAVED_PTR(x) ((void *)((DWORD_PTR)((char *)x - sizeof(void *)) & \
                               ~(sizeof(void *) - 1)))
static size_t _jl_aligned_msize(void *p)
{
    void *alloc_ptr = *(void**)SAVED_PTR(p);
    return _msize(alloc_ptr) - ((char*)p - (char*)alloc_ptr);
}
#undef SAVED_PTR
#endif

size_t memory_block_usable_size(void *p, int isaligned) JL_NOTSAFEPOINT
{
#if defined(_OS_WINDOWS_)
    if (isaligned)
        return _jl_aligned_msize(p);
    else
        return _msize(p);
#elif defined(_OS_DARWIN_)
    return malloc_size(p);
#else
    return malloc_usable_size(p);
#endif
}

// =========================================================================== //
// Finalization
// =========================================================================== //

jl_mutex_t finalizers_lock;
arraylist_t finalizer_list_marked;
arraylist_t to_finalize;
JL_DLLEXPORT _Atomic(int) jl_gc_have_pending_finalizers = 0;

void schedule_finalization(void *o, void *f) JL_NOTSAFEPOINT
{
    arraylist_push(&to_finalize, o);
    arraylist_push(&to_finalize, f);
    // doesn't need release, since we'll keep checking (on the reader) until we see the work and
    // release our lock, and that will have a release barrier by then
    jl_atomic_store_relaxed(&jl_gc_have_pending_finalizers, 1);
}

void run_finalizer(jl_task_t *ct, void *o, void *ff)
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
        jl_static_show((JL_STREAM*)STDERR_FILENO, jl_current_exception(ct));
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

void run_finalizers(jl_task_t *ct, int finalizers_thread)
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
    ct->ptls->in_finalizer = !finalizers_thread;
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
    if (!ptls->in_finalizer && ptls->locks.len == 0 && ptls->finalizers_inhibited == 0 && ptls->engine_nqueued == 0) {
        run_finalizers(ct, 0);
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
    // this is called from `jl_atexit_hook`; threads could still be running
    // so we have to guard the finalizers' lists
    JL_LOCK_NOGC(&finalizers_lock);
    schedule_all_finalizers(&finalizer_list_marked);
    for (int i = 0; i < gc_n_threads; i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[i];
        if (ptls2 != NULL)
            schedule_all_finalizers(&ptls2->finalizers);
    }
    // unlock here because `run_finalizers` locks this
    JL_UNLOCK_NOGC(&finalizers_lock);
    run_finalizers(ct, 1);
}

void jl_gc_add_finalizer_(jl_ptls_t ptls, void *v, void *f) JL_NOTSAFEPOINT
{
    assert(jl_atomic_load_relaxed(&ptls->gc_state) == JL_GC_STATE_UNSAFE);
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
    if (__unlikely(jl_typetagis(f, jl_voidpointer_type))) {
        jl_gc_add_ptr_finalizer(ptls, v, jl_unbox_voidpointer(f));
    }
    else {
        jl_gc_add_finalizer_(ptls, v, f);
    }
}

JL_DLLEXPORT void jl_gc_add_finalizer(jl_value_t *v, jl_function_t *f)
{
    jl_ptls_t ptls = jl_current_task->ptls;
    jl_gc_add_finalizer_th(ptls, v, f);
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
    if (copied_list.len > 0) {
        // This releases the finalizers lock.
        jl_gc_run_finalizers_in_list(ct, &copied_list);
    }
    else {
        JL_UNLOCK_NOGC(&finalizers_lock);
    }
    arraylist_free(&copied_list);
}

JL_DLLEXPORT void jl_finalize(jl_value_t *o)
{
    jl_finalize_th(jl_current_task, o);
}

// =========================================================================== //
// Threading
// =========================================================================== //

int gc_n_threads;
jl_ptls_t* gc_all_tls_states;

// =========================================================================== //
// Allocation
// =========================================================================== //

JL_DLLEXPORT void * jl_gc_alloc_typed(jl_ptls_t ptls, size_t sz, void *ty)
{
    return jl_gc_alloc(ptls, sz, ty);
}

JL_DLLEXPORT jl_value_t *jl_gc_allocobj(size_t sz)
{
    jl_ptls_t ptls = jl_current_task->ptls;
    return jl_gc_alloc(ptls, sz, NULL);
}

// allocator entry points

JL_DLLEXPORT jl_value_t *(jl_gc_alloc)(jl_ptls_t ptls, size_t sz, void *ty)
{
    return jl_gc_alloc_(ptls, sz, ty);
}

JL_DLLEXPORT void *jl_malloc(size_t sz)
{
    return jl_gc_counted_malloc(sz);
}

//_unchecked_calloc does not check for potential overflow of nm*sz
STATIC_INLINE void *_unchecked_calloc(size_t nm, size_t sz) {
    size_t nmsz = nm*sz;
    return jl_gc_counted_calloc(nmsz, 1);
}

JL_DLLEXPORT void *jl_calloc(size_t nm, size_t sz)
{
    if (nm > SSIZE_MAX/sz)
        return NULL;
    return _unchecked_calloc(nm, sz);
}

JL_DLLEXPORT void jl_free(void *p)
{
    if (p != NULL) {
        size_t sz = memory_block_usable_size(p, 0);
        return jl_gc_counted_free_with_size(p, sz);
    }
}

JL_DLLEXPORT void *jl_realloc(void *p, size_t sz)
{
    size_t old = p ? memory_block_usable_size(p, 0) : 0;
    return jl_gc_counted_realloc_with_old_size(p, old, sz);
}

// =========================================================================== //
// Generic Memory
// =========================================================================== //

size_t jl_genericmemory_nbytes(jl_genericmemory_t *m) JL_NOTSAFEPOINT
{
    const jl_datatype_layout_t *layout = ((jl_datatype_t*)jl_typetagof(m))->layout;
    size_t sz = layout->size * m->length;
    if (layout->flags.arrayelem_isunion)
        // account for isbits Union array selector bytes
        sz += m->length;
    return sz;
}

// tracking Memorys with malloc'd storage
void jl_gc_track_malloced_genericmemory(jl_ptls_t ptls, jl_genericmemory_t *m, int isaligned){
    // This is **NOT** a GC safe point.
    void *a = (void*)((uintptr_t)m | !!isaligned);
    small_arraylist_push(&ptls->gc_tls_common.heap.mallocarrays, a);
}

// =========================================================================== //
// GC Debug
// =========================================================================== //

int gc_slot_to_fieldidx(void *obj, void *slot, jl_datatype_t *vt) JL_NOTSAFEPOINT
{
    int nf = (int)jl_datatype_nfields(vt);
    for (int i = 1; i < nf; i++) {
        if (slot < (void*)((char*)obj + jl_field_offset(vt, i)))
            return i - 1;
    }
    return nf - 1;
}

int gc_slot_to_arrayidx(void *obj, void *_slot) JL_NOTSAFEPOINT
{
    char *slot = (char*)_slot;
    jl_datatype_t *vt = (jl_datatype_t*)jl_typeof(obj);
    char *start = NULL;
    size_t len = 0;
    size_t elsize = sizeof(void*);
    if (vt == jl_module_type) {
        jl_module_t *m = (jl_module_t*)obj;
        start = (char*)m->usings.items;
        len = module_usings_length(m);
        elsize = sizeof(struct _jl_module_using);
    }
    else if (vt == jl_simplevector_type) {
        start = (char*)jl_svec_data(obj);
        len = jl_svec_len(obj);
    }
    if (slot < start || slot >= start + elsize * len)
        return -1;
    return (slot - start) / elsize;
}

// =========================================================================== //
// GC Control
// =========================================================================== //

JL_DLLEXPORT uint32_t jl_get_gc_disable_counter(void) {
    return jl_atomic_load_acquire(&jl_gc_disable_counter);
}

JL_DLLEXPORT int jl_gc_is_enabled(void)
{
    jl_ptls_t ptls = jl_current_task->ptls;
    return !ptls->disable_gc;
}

int gc_logging_enabled = 0;

JL_DLLEXPORT void jl_enable_gc_logging(int enable) {
    gc_logging_enabled = enable;
}

JL_DLLEXPORT int jl_is_gc_logging_enabled(void) {
    return gc_logging_enabled;
}


// collector entry point and control
_Atomic(uint32_t) jl_gc_disable_counter = 1;

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

// =========================================================================== //
// MISC
// =========================================================================== //

JL_DLLEXPORT jl_weakref_t *jl_gc_new_weakref(jl_value_t *value)
{
    jl_ptls_t ptls = jl_current_task->ptls;
    return jl_gc_new_weakref_th(ptls, value);
}

const uint64_t _jl_buff_tag[3] = {0x4eadc0004eadc000ull, 0x4eadc0004eadc000ull, 0x4eadc0004eadc000ull}; // aka 0xHEADER00
JL_DLLEXPORT uintptr_t jl_get_buff_tag(void) JL_NOTSAFEPOINT
{
    return jl_buff_tag;
}

// callback for passing OOM errors from gmp
JL_DLLEXPORT void jl_throw_out_of_memory_error(void)
{
    jl_throw(jl_memory_exception);
}

// Sweeping mtarraylist_buffers:
// These buffers are made unreachable via `mtarraylist_resizeto` from mtarraylist.c
// and are freed at the end of GC via jl_gc_sweep_stack_pools_and_mtarraylist_buffers
void sweep_mtarraylist_buffers(void) JL_NOTSAFEPOINT
{
    for (int i = 0; i < gc_n_threads; i++) {
        jl_ptls_t ptls = gc_all_tls_states[i];
        if (ptls == NULL) {
            continue;
        }
        small_arraylist_t *buffers = &ptls->lazily_freed_mtarraylist_buffers;
        void *buf;
        while ((buf = small_arraylist_pop(buffers)) != NULL) {
            free(buf);
        }
    }
}

#ifdef __cplusplus
}
#endif

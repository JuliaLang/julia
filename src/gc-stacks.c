// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "gc-common.h"
#include "threading.h"
#ifndef _OS_WINDOWS_
#  include <sys/resource.h>
#endif

#ifdef _P64
# ifdef _OS_WINDOWS_
#  define MAX_STACK_MAPPINGS 500
# else
#  define MAX_STACK_MAPPINGS 30000
# endif
#else
# ifdef _OS_WINDOWS_
#  define MAX_STACK_MAPPINGS 250
# else
#  define MAX_STACK_MAPPINGS 500
# endif
#endif

const size_t jl_guard_size = (4096 * 8);
static _Atomic(uint32_t) num_stack_mappings = 0;

#ifdef _OS_WINDOWS_
#define MAP_FAILED NULL
static void *malloc_stack(size_t bufsz) JL_NOTSAFEPOINT
{
    void *stk = VirtualAlloc(NULL, bufsz, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
    if (stk == NULL)
        return MAP_FAILED;

    // set up a guard page to detect stack overflow
    DWORD dwOldProtect;
    if (!VirtualProtect(stk, jl_guard_size, PAGE_READWRITE | PAGE_GUARD, &dwOldProtect)) {
        VirtualFree(stk, 0, MEM_RELEASE);
        return MAP_FAILED;
    }

    jl_atomic_fetch_add_relaxed(&num_stack_mappings, 1);
    return stk;
}


void free_stack(void *stkbuf, size_t bufsz) JL_NOTSAFEPOINT
{
    VirtualFree(stkbuf, 0, MEM_RELEASE);
    jl_atomic_fetch_add_relaxed(&num_stack_mappings, -1);
}

#else

static void *malloc_stack(size_t bufsz) JL_NOTSAFEPOINT
{
# ifdef _OS_OPENBSD_
    // we don't set up a guard page to detect stack overflow: on OpenBSD, any
    // mmap-ed region has guard page managed by the kernel, so there is no
    // need for it. Additionally, a memory region used as stack (memory
    // allocated with MAP_STACK option) has strict permission, and you can't
    // "create" a guard page on such memory by using `mprotect` on it
    void* stk = mmap(0, bufsz, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS | MAP_STACK, -1, 0);
    if (stk == MAP_FAILED)
        return MAP_FAILED;
# else
    void* stk = mmap(0, bufsz, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (stk == MAP_FAILED)
        return MAP_FAILED;

    // set up a guard page to detect stack overflow
    if (mprotect(stk, jl_guard_size, PROT_NONE) == -1) {
        munmap(stk, bufsz);
        return MAP_FAILED;
    }
# endif

    jl_atomic_fetch_add_relaxed(&num_stack_mappings, 1);
    return stk;
}

void free_stack(void *stkbuf, size_t bufsz) JL_NOTSAFEPOINT
{
    munmap(stkbuf, bufsz);
    jl_atomic_fetch_add_relaxed(&num_stack_mappings, -1);
}
#endif

JL_DLLEXPORT uint32_t jl_get_num_stack_mappings(void) JL_NOTSAFEPOINT
{
    return jl_atomic_load_relaxed(&num_stack_mappings);
}

const unsigned pool_sizes[] = {
    128 * 1024,
    192 * 1024,
    256 * 1024,
    384 * 1024,
    512 * 1024,
    768 * 1024,
    1024 * 1024,
    1537 * 1024,
    2048 * 1024,
    3 * 1024 * 1024,
    4 * 1024 * 1024,
    6 * 1024 * 1024,
    8 * 1024 * 1024,
    12 * 1024 * 1024,
    16 * 1024 * 1024,
    24 * 1024 * 1024,
};

static_assert(sizeof(pool_sizes) == JL_N_STACK_POOLS * sizeof(pool_sizes[0]), "JL_N_STACK_POOLS size mismatch");

static unsigned select_pool(size_t nb) JL_NOTSAFEPOINT
{
    unsigned pool_id = 0;
    while (pool_sizes[pool_id] < nb)
        pool_id++;
    return pool_id;
}


void _jl_free_stack(jl_ptls_t ptls, void *stkbuf, size_t bufsz) JL_NOTSAFEPOINT
{
#ifdef _COMPILER_ASAN_ENABLED_
    __asan_unpoison_stack_memory((uintptr_t)stkbuf, bufsz);
#endif
    if (bufsz <= pool_sizes[JL_N_STACK_POOLS - 1]) {
        unsigned pool_id = select_pool(bufsz);
        if (pool_sizes[pool_id] == bufsz) {
            small_arraylist_push(&ptls->gc_tls_common.heap.free_stacks[pool_id], stkbuf);
            return;
        }
    }
    free_stack(stkbuf, bufsz);
}


JL_DLLEXPORT void jl_free_stack(void *stkbuf, size_t bufsz)
{
    jl_task_t *ct = jl_current_task;
    _jl_free_stack(ct->ptls, stkbuf, bufsz);
}


void jl_release_task_stack(jl_ptls_t ptls, jl_task_t *task)
{
    // avoid adding an original thread stack to the free list
    if (task == ptls->root_task && !task->ctx.copy_stack)
        return;
    void *stkbuf = task->ctx.stkbuf;
    size_t bufsz = task->ctx.bufsz;
    if (bufsz <= pool_sizes[JL_N_STACK_POOLS - 1]) {
        unsigned pool_id = select_pool(bufsz);
        if (pool_sizes[pool_id] == bufsz) {
            task->ctx.stkbuf = NULL;
#ifdef _COMPILER_ASAN_ENABLED_
            __asan_unpoison_stack_memory((uintptr_t)stkbuf, bufsz);
#endif
            small_arraylist_push(&ptls->gc_tls_common.heap.free_stacks[pool_id], stkbuf);
        }
    }
}


JL_DLLEXPORT void *jl_malloc_stack(size_t *bufsz, jl_task_t *owner) JL_NOTSAFEPOINT
{
    jl_task_t *ct = jl_current_task;
    jl_ptls_t ptls = ct->ptls;
    size_t ssize = *bufsz;
    void *stk = NULL;
    if (ssize <= pool_sizes[JL_N_STACK_POOLS - 1]) {
        unsigned pool_id = select_pool(ssize);
        ssize = pool_sizes[pool_id];
        small_arraylist_t *pool = &ptls->gc_tls_common.heap.free_stacks[pool_id];
        if (pool->len > 0) {
            stk = small_arraylist_pop(pool);
        }
    }
    else {
        ssize = LLT_ALIGN(ssize, jl_page_size);
    }
    if (stk == NULL) {
        if (jl_atomic_load_relaxed(&num_stack_mappings) >= MAX_STACK_MAPPINGS) {
            // we accept that this can go over by as much as nthreads since it's not a CAS
            errno = ENOMEM;
            return NULL;
        }
        // TODO: allocate blocks of stacks? but need to mprotect individually anyways
        stk = malloc_stack(ssize);
        if (stk == MAP_FAILED)
            return NULL;
    }
    *bufsz = ssize;
    if (owner) {
        small_arraylist_t *live_tasks = &ptls->gc_tls_common.heap.live_tasks;
        mtarraylist_push(live_tasks, owner);
    }
    return stk;
}

// Builds a list of the live tasks. Racy: `live_tasks` can expand at any time.
arraylist_t *jl_get_all_tasks_arraylist(void) JL_NOTSAFEPOINT
{
    arraylist_t *tasks = (arraylist_t*)malloc_s(sizeof(arraylist_t));
    arraylist_new(tasks, 0);
    size_t nthreads = jl_atomic_load_acquire(&jl_n_threads);
    jl_ptls_t *allstates = jl_atomic_load_relaxed(&jl_all_tls_states);
    for (size_t i = 0; i < nthreads; i++) {
        // skip GC threads...
        if (gc_is_collector_thread(i)) {
            continue;
        }
        jl_ptls_t ptls2 = allstates[i];
        if (ptls2 == NULL) {
            continue;
        }
        jl_task_t *t = ptls2->root_task;
        if (t->ctx.stkbuf != NULL) {
            arraylist_push(tasks, t);
        }
        small_arraylist_t *live_tasks = &ptls2->gc_tls_common.heap.live_tasks;
        size_t n = mtarraylist_length(live_tasks);
        for (size_t i = 0; i < n; i++) {
            jl_task_t *t = (jl_task_t*)mtarraylist_get(live_tasks, i);
            assert(t != NULL);
            if (t->ctx.stkbuf != NULL) {
                arraylist_push(tasks, t);
            }
        }
    }
    return tasks;
}

JL_DLLEXPORT jl_array_t *jl_live_tasks(void)
{
    size_t nthreads = jl_atomic_load_acquire(&jl_n_threads);
    jl_ptls_t *allstates = jl_atomic_load_relaxed(&jl_all_tls_states);
    size_t l = 0; // l is not reset on restart, so we keep getting more aggressive at making a big enough list everything it fails
restart:
    for (size_t i = 0; i < nthreads; i++) {
        jl_ptls_t ptls2 = allstates[i];
        if (ptls2 == NULL)
            continue;
        small_arraylist_t *live_tasks = &ptls2->gc_tls_common.heap.live_tasks;
        size_t n = mtarraylist_length(live_tasks);
        l += n + (ptls2->root_task->ctx.stkbuf != NULL);
    }
    l += l / 20; // add 5% for margin of estimation error
    jl_array_t *a = jl_alloc_vec_any(l); // may gc, changing the number of tasks and forcing us to reload everything
    nthreads = jl_atomic_load_acquire(&jl_n_threads);
    allstates = jl_atomic_load_relaxed(&jl_all_tls_states);
    size_t j = 0;
    for (size_t i = 0; i < nthreads; i++) {
        jl_ptls_t ptls2 = allstates[i];
        if (ptls2 == NULL)
            continue;
        jl_task_t *t = ptls2->root_task;
        if (t->ctx.stkbuf != NULL) {
            if (j == l)
                goto restart;
            jl_array_data(a,void*)[j++] = t;
        }
        small_arraylist_t *live_tasks = &ptls2->gc_tls_common.heap.live_tasks;
        size_t n = mtarraylist_length(live_tasks);
        for (size_t i = 0; i < n; i++) {
            jl_task_t *t = (jl_task_t*)mtarraylist_get(live_tasks, i);
            if (t->ctx.stkbuf != NULL) {
                if (j == l)
                    goto restart;
                jl_array_data(a,void*)[j++] = t;
            }
        }
    }
    if (j < l) {
        JL_GC_PUSH1(&a);
        jl_array_del_end(a, l - j);
        JL_GC_POP();
    }
    return a;
}

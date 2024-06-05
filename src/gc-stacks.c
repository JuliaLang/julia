// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "gc.h"
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

// number of stacks to always keep available per pool
#define MIN_STACK_MAPPINGS_PER_POOL 5

#if defined(_OS_WINDOWS_) || (!defined(_OS_OPENBSD_) && !defined(JL_HAVE_UCONTEXT) && !defined(JL_HAVE_SIGALTSTACK))
#define JL_USE_GUARD_PAGE   1
const size_t jl_guard_size = (4096 * 8);
#else
const size_t jl_guard_size = 0;
#endif

static _Atomic(uint32_t) num_stack_mappings = 0;

#ifdef _OS_WINDOWS_
#define MAP_FAILED NULL
static void *malloc_stack(size_t bufsz) JL_NOTSAFEPOINT
{
    size_t guard_size = LLT_ALIGN(jl_guard_size, jl_page_size);
    bufsz += guard_size;

    void *stk = VirtualAlloc(NULL, bufsz, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
    if (stk == NULL)
        return MAP_FAILED;

    // set up a guard page to detect stack overflow
    DWORD dwOldProtect;
    if (!VirtualProtect(stk, jl_guard_size, PAGE_READWRITE | PAGE_GUARD, &dwOldProtect)) {
        VirtualFree(stk, 0, MEM_RELEASE);
        return MAP_FAILED;
    }
    stk = (char *)stk + guard_size;

    jl_atomic_fetch_add_relaxed(&num_stack_mappings, 1);
    return stk;
}


static void free_stack(void *stkbuf, size_t bufsz) JL_NOTSAFEPOINT
{
#ifdef JL_USE_GUARD_PAGE
    size_t guard_size = LLT_ALIGN(jl_guard_size, jl_page_size);
    bufsz += guard_size;
    stkbuf = (char *)stkbuf - guard_size;
#endif

    VirtualFree(stkbuf, 0, MEM_RELEASE);
    jl_atomic_fetch_add_relaxed(&num_stack_mappings, -1);
}

#else

# ifdef _OS_OPENBSD_
static void *malloc_stack(size_t bufsz) JL_NOTSAFEPOINT
{
    void* stk = mmap(0, bufsz, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS | MAP_STACK, -1, 0);
    if (stk == MAP_FAILED)
        return MAP_FAILED;

    // we don't set up a guard page to detect stack overflow: on OpenBSD, any
    // mmap-ed region has guard page managed by the kernel, so there is no
    // need for it. Additionally, a memory region used as stack (memory
    // allocated with MAP_STACK option) has strict permission, and you can't
    // "create" a guard page on such memory by using `mprotect` on it

    jl_atomic_fetch_add_relaxed(&num_stack_mappings, 1);
    return stk;
}
# else
static void *malloc_stack(size_t bufsz) JL_NOTSAFEPOINT
{
#ifdef JL_USE_GUARD_PAGE
    size_t guard_size = LLT_ALIGN(jl_guard_size, jl_page_size);
    bufsz += guard_size;
#endif

    void* stk = mmap(0, bufsz, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (stk == MAP_FAILED)
        return MAP_FAILED;

#ifdef JL_USE_GUARD_PAGE
    // set up a guard page to detect stack overflow
    if (mprotect(stk, jl_guard_size, PROT_NONE) == -1) {
        munmap(stk, bufsz);
        return MAP_FAILED;
    }
    stk = (char *)stk + guard_size;
#endif

    jl_atomic_fetch_add_relaxed(&num_stack_mappings, 1);
    return stk;
}
# endif

static void free_stack(void *stkbuf, size_t bufsz) JL_NOTSAFEPOINT
{
#ifdef JL_USE_GUARD_PAGE
    size_t guard_size = LLT_ALIGN(jl_guard_size, jl_page_size);
    bufsz += guard_size;
    stkbuf = (char *)stkbuf - guard_size;
#endif

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


static void _jl_free_stack(jl_ptls_t ptls, void *stkbuf, size_t bufsz) JL_NOTSAFEPOINT
{
#ifdef _COMPILER_ASAN_ENABLED_
    __asan_unpoison_stack_memory((uintptr_t)stkbuf, bufsz);
#endif
    if (bufsz <= pool_sizes[JL_N_STACK_POOLS - 1]) {
        unsigned pool_id = select_pool(bufsz);
        if (pool_sizes[pool_id] == bufsz) {
            small_arraylist_push(&ptls->heap.free_stacks[pool_id], stkbuf);
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
    if (task == ptls->root_task && !task->copy_stack)
        return;
    void *stkbuf = task->stkbuf;
    size_t bufsz = task->bufsz;
    if (bufsz <= pool_sizes[JL_N_STACK_POOLS - 1]) {
        unsigned pool_id = select_pool(bufsz);
        if (pool_sizes[pool_id] == bufsz) {
            task->stkbuf = NULL;
#ifdef _COMPILER_ASAN_ENABLED_
            __asan_unpoison_stack_memory((uintptr_t)stkbuf, bufsz);
#endif
            small_arraylist_push(&ptls->heap.free_stacks[pool_id], stkbuf);
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
        small_arraylist_t *pool = &ptls->heap.free_stacks[pool_id];
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
        small_arraylist_t *live_tasks = &ptls->heap.live_tasks;
        mtarraylist_push(live_tasks, owner);
    }
    return stk;
}

void sweep_stack_pools(void) JL_NOTSAFEPOINT
{
    // Stack sweeping algorithm:
    //    // deallocate stacks if we have too many sitting around unused
    //    for (stk in halfof(free_stacks))
    //        free_stack(stk, pool_sz);
    //    // then sweep the task stacks
    //    for (t in live_tasks)
    //        if (!gc-marked(t))
    //            stkbuf = t->stkbuf
    //            bufsz = t->bufsz
    //            if (stkbuf)
    //                push(free_stacks[sz], stkbuf)
    assert(gc_n_threads);
    for (int i = 0; i < gc_n_threads; i++) {
        jl_ptls_t ptls2 = gc_all_tls_states[i];
        if (ptls2 == NULL)
            continue;

        // free half of stacks that remain unused since last sweep
        for (int p = 0; p < JL_N_STACK_POOLS; p++) {
            small_arraylist_t *al = &ptls2->heap.free_stacks[p];
            size_t n_to_free;
            if (jl_atomic_load_relaxed(&ptls2->current_task) == NULL) {
                n_to_free = al->len; // not alive yet or dead, so it does not need these anymore
            }
            else if (al->len > MIN_STACK_MAPPINGS_PER_POOL) {
                n_to_free = al->len / 2;
                if (n_to_free > (al->len - MIN_STACK_MAPPINGS_PER_POOL))
                    n_to_free = al->len - MIN_STACK_MAPPINGS_PER_POOL;
            }
            else {
                n_to_free = 0;
            }
            for (int n = 0; n < n_to_free; n++) {
                void *stk = small_arraylist_pop(al);
                free_stack(stk, pool_sizes[p]);
            }
            if (jl_atomic_load_relaxed(&ptls2->current_task) == NULL) {
                small_arraylist_free(al);
            }
        }
        if (jl_atomic_load_relaxed(&ptls2->current_task) == NULL) {
            small_arraylist_free(ptls2->heap.free_stacks);
        }

        small_arraylist_t *live_tasks = &ptls2->heap.live_tasks;
        size_t n = 0;
        size_t ndel = 0;
        size_t l = live_tasks->len;
        void **lst = live_tasks->items;
        if (l == 0)
            continue;
        while (1) {
            jl_task_t *t = (jl_task_t*)lst[n];
            assert(jl_is_task(t));
            if (gc_marked(jl_astaggedvalue(t)->bits.gc)) {
                if (t->stkbuf == NULL)
                    ndel++; // jl_release_task_stack called
                else
                    n++;
            }
            else {
                ndel++;
                void *stkbuf = t->stkbuf;
                size_t bufsz = t->bufsz;
                if (stkbuf) {
                    t->stkbuf = NULL;
                    _jl_free_stack(ptls2, stkbuf, bufsz);
                }
#ifdef _COMPILER_TSAN_ENABLED_
                if (t->ctx.tsan_state) {
                    __tsan_destroy_fiber(t->ctx.tsan_state);
                    t->ctx.tsan_state = NULL;
                }
#endif
            }
            if (n >= l - ndel)
                break;
            void *tmp = lst[n];
            lst[n] = lst[n + ndel];
            lst[n + ndel] = tmp;
        }
        live_tasks->len -= ndel;
    }
}

JL_DLLEXPORT jl_array_t *jl_live_tasks(void)
{
    size_t nthreads = jl_atomic_load_acquire(&jl_n_threads);
    jl_ptls_t *allstates = jl_atomic_load_relaxed(&jl_all_tls_states);
    size_t l = 0; // l is not reset on restart, so we keep getting more aggressive at making a big enough list everything it fails
restart:
    for (size_t i = 0; i < nthreads; i++) {
        // skip GC threads since they don't have tasks
        if (gc_first_tid <= i && i < gc_first_tid + jl_n_gcthreads) {
            continue;
        }
        jl_ptls_t ptls2 = allstates[i];
        if (ptls2 == NULL)
            continue;
        small_arraylist_t *live_tasks = &ptls2->heap.live_tasks;
        size_t n = mtarraylist_length(live_tasks);
        l += n + (ptls2->root_task->stkbuf != NULL);
    }
    l += l / 20; // add 5% for margin of estimation error
    jl_array_t *a = jl_alloc_vec_any(l); // may gc, changing the number of tasks and forcing us to reload everything
    nthreads = jl_atomic_load_acquire(&jl_n_threads);
    allstates = jl_atomic_load_relaxed(&jl_all_tls_states);
    size_t j = 0;
    for (size_t i = 0; i < nthreads; i++) {
        // skip GC threads since they don't have tasks
        if (gc_first_tid <= i && i < gc_first_tid + jl_n_gcthreads) {
            continue;
        }
        jl_ptls_t ptls2 = allstates[i];
        if (ptls2 == NULL)
            continue;
        jl_task_t *t = ptls2->root_task;
        if (t->stkbuf != NULL) {
            if (j == l)
                goto restart;
            jl_array_data(a,void*)[j++] = t;
        }
        small_arraylist_t *live_tasks = &ptls2->heap.live_tasks;
        size_t n = mtarraylist_length(live_tasks);
        for (size_t i = 0; i < n; i++) {
            jl_task_t *t = (jl_task_t*)mtarraylist_get(live_tasks, i);
            if (t->stkbuf != NULL) {
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

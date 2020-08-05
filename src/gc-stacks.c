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

const size_t jl_guard_size = (4096 * 8);
static volatile uint32_t num_stack_mappings = 0;

#ifdef _OS_WINDOWS_
#define MAP_FAILED NULL
static void *malloc_stack(size_t bufsz) JL_NOTSAFEPOINT
{
    void *stk = VirtualAlloc(NULL, bufsz, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
    if (stk == NULL)
        return MAP_FAILED;
    DWORD dwOldProtect;
    if (!VirtualProtect(stk, jl_guard_size, PAGE_READWRITE | PAGE_GUARD, &dwOldProtect)) {
        VirtualFree(stk, 0, MEM_RELEASE);
        return MAP_FAILED;
    }
    jl_atomic_fetch_add(&num_stack_mappings, 1);
    return stk;
}


static void free_stack(void *stkbuf, size_t bufsz)
{
    VirtualFree(stkbuf, 0, MEM_RELEASE);
    jl_atomic_fetch_add(&num_stack_mappings, -1);
}

#else

static void *malloc_stack(size_t bufsz) JL_NOTSAFEPOINT
{
    void* stk = mmap(0, bufsz, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    if (stk == MAP_FAILED)
        return MAP_FAILED;
#if !defined(JL_HAVE_UCONTEXT) && !defined(JL_HAVE_SIGALTSTACK)
    // setup a guard page to detect stack overflow
    if (mprotect(stk, jl_guard_size, PROT_NONE) == -1) {
        munmap(stk, bufsz);
        return MAP_FAILED;
    }
#endif
    jl_atomic_fetch_add(&num_stack_mappings, 1);
    return stk;
}

static void free_stack(void *stkbuf, size_t bufsz)
{
    munmap(stkbuf, bufsz);
    jl_atomic_fetch_add(&num_stack_mappings, -1);
}
#endif


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


static void _jl_free_stack(jl_ptls_t ptls, void *stkbuf, size_t bufsz)
{
    if (bufsz <= pool_sizes[JL_N_STACK_POOLS - 1]) {
        unsigned pool_id = select_pool(bufsz);
        if (pool_sizes[pool_id] == bufsz) {
            arraylist_push(&ptls->heap.free_stacks[pool_id], stkbuf);
            return;
        }
    }
    free_stack(stkbuf, bufsz);
}


JL_DLLEXPORT void jl_free_stack(void *stkbuf, size_t bufsz)
{
    _jl_free_stack(jl_get_ptls_states(), stkbuf, bufsz);
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
            arraylist_push(&ptls->heap.free_stacks[pool_id], stkbuf);
        }
    }
}


JL_DLLEXPORT void *jl_malloc_stack(size_t *bufsz, jl_task_t *owner) JL_NOTSAFEPOINT
{
    jl_ptls_t ptls = jl_get_ptls_states();
    size_t ssize = *bufsz;
    void *stk = NULL;
    if (ssize <= pool_sizes[JL_N_STACK_POOLS - 1]) {
        unsigned pool_id = select_pool(ssize);
        ssize = pool_sizes[pool_id];
        arraylist_t *pool = &ptls->heap.free_stacks[pool_id];
        if (pool->len > 0) {
            stk = arraylist_pop(pool);
        }
    }
    else {
        ssize = LLT_ALIGN(ssize, jl_page_size);
    }
    if (stk == NULL) {
        if (num_stack_mappings >= MAX_STACK_MAPPINGS)
            return NULL;
        // TODO: allocate blocks of stacks? but need to mprotect individually anyways
        stk = malloc_stack(ssize);
        if (stk == MAP_FAILED)
            return NULL;
    }
    *bufsz = ssize;
    if (owner) {
        arraylist_t *live_tasks = &ptls->heap.live_tasks;
        arraylist_push(live_tasks, owner);
    }
    return stk;
}

void sweep_stack_pools(void)
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
    for (int i = 0; i < jl_n_threads; i++) {
        jl_ptls_t ptls2 = jl_all_tls_states[i];

        // free half of stacks that remain unused since last sweep
        for (int p = 0; p < JL_N_STACK_POOLS; p++) {
            arraylist_t *al = &ptls2->heap.free_stacks[p];
            size_t n_to_free;
            if (al->len > MIN_STACK_MAPPINGS_PER_POOL) {
                n_to_free = al->len / 2;
                if (n_to_free > (al->len - MIN_STACK_MAPPINGS_PER_POOL))
                    n_to_free = al->len - MIN_STACK_MAPPINGS_PER_POOL;
            }
            else {
                n_to_free = 0;
            }
            for (int n = 0; n < n_to_free; n++) {
                void *stk = arraylist_pop(al);
                free_stack(stk, pool_sizes[p]);
            }
        }

        arraylist_t *live_tasks = &ptls2->heap.live_tasks;
        size_t n = 0;
        size_t ndel = 0;
        size_t l = live_tasks->len;
        void **lst = live_tasks->items;
        if (l == 0)
            continue;
        while (1) {
            jl_task_t *t = (jl_task_t*)lst[n];
            if (gc_marked(jl_astaggedvalue(t)->bits.gc)) {
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
#ifdef JL_TSAN_ENABLED
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

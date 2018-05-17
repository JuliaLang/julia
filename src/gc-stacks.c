// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "gc.h"
#ifndef _OS_WINDOWS_
#  include <sys/resource.h>
#endif

const size_t jl_guard_size = (4096 * 16);

#ifdef _OS_WINDOWS_
#define MAP_FAILED NULL
static void *malloc_stack(size_t bufsz)
{
    void *stk = VirtualAlloc(NULL, bufsz, MEM_RESERVE | MEM_COMMIT, PAGE_READWRITE);
    if (stk == NULL)
        return MAP_FAILED;
    DWORD dwOldProtect;
    if (!VirtualProtect(stk, jl_guard_size, PAGE_READWRITE | PAGE_GUARD, &dwOldProtect)) {
        VirtualFree(stk, 0, MEM_RELEASE);
        return MAP_FAILED;
    }
    return stk;
}


static void free_stack(void *stkbuf, size_t bufsz)
{
    VirtualFree(stkbuf, 0, MEM_RELEASE);
}

#else

static void *malloc_stack(size_t bufsz)
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
    return stk;
}

static void free_stack(void *stkbuf, size_t bufsz)
{
    munmap(stkbuf, bufsz);
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

static unsigned select_pool(size_t nb)
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


JL_DLLEXPORT void *jl_malloc_stack(size_t *bufsz, jl_task_t *owner)
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
        // TODO: allocate blocks of stacks? but need to mprotect individually anyways
        stk = malloc_stack(ssize);
        if (stk == MAP_FAILED)
            jl_throw(jl_memory_exception);
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
//    TODO: deallocate stacks if we have too many sitting around unused
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

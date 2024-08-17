// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

// this file provides some alternate API functions for small_arraylist (push and add)
// which can be safely observed from other threads concurrently
// there is only permitted to be a single writer thread (or a mutex)
// but there can be any number of observers

typedef struct {
    _Atomic(uint32_t) len;
    uint32_t max;
    _Atomic(_Atomic(void*)*) items;
    _Atomic(void*) _space[SMALL_AL_N_INLINE];
} small_mtarraylist_t;

// change capacity to at least newlen
static void mtarraylist_resizeto(small_mtarraylist_t *a, size_t len, size_t newlen) JL_NOTSAFEPOINT
{
    size_t max = a->max;
    if (newlen > max) {
        size_t nm = max * 2;
        if (nm == 0)
            nm = 1;
        while (newlen > nm)
            nm *= 2;
        void *olditems = (void*)jl_atomic_load_relaxed(&a->items);
        void *p = calloc_s(nm * sizeof(void*));
        memcpy(p, olditems, len * sizeof(void*));
        jl_atomic_store_release(&a->items, (_Atomic(void*)*)p);
        a->max = nm;
        if (olditems != (void*)&a->_space[0]) {
            jl_task_t *ct = jl_current_task;
            jl_gc_add_quiescent(ct->ptls, (void**)olditems, free);
        }
    }
}

// single-threaded
void mtarraylist_push(small_arraylist_t *_a, void *elt)
{
    small_mtarraylist_t *a = (small_mtarraylist_t*)_a;
    size_t len = jl_atomic_load_relaxed(&a->len);
    mtarraylist_resizeto(a, len, len + 1);
    jl_atomic_store_release(&jl_atomic_load_relaxed(&a->items)[len], elt);
    jl_atomic_store_release(&a->len, len + 1);
}

// single-threaded
void mtarraylist_add(small_arraylist_t *_a, void *elt, size_t idx)
{
    small_mtarraylist_t *a = (small_mtarraylist_t*)_a;
    size_t len = jl_atomic_load_relaxed(&a->len);
    mtarraylist_resizeto(a, len, idx + 1);
    jl_atomic_store_release(&jl_atomic_load_relaxed(&a->items)[idx], elt);
    if (jl_atomic_load_relaxed(&a->len) < idx + 1)
        jl_atomic_store_release(&a->len, idx + 1);
}

// concurrent-safe
size_t mtarraylist_length(small_arraylist_t *_a)
{
    small_mtarraylist_t *a = (small_mtarraylist_t*)_a;
    return jl_atomic_load_relaxed(&a->len);
}

// concurrent-safe
void *mtarraylist_get(small_arraylist_t *_a, size_t idx)
{
    small_mtarraylist_t *a = (small_mtarraylist_t*)_a;
    size_t len = jl_atomic_load_acquire(&a->len);
    if (idx >= len)
        return NULL;
    return jl_atomic_load_relaxed(&jl_atomic_load_relaxed(&a->items)[idx]);
}

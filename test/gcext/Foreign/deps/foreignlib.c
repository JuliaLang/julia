// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "julia.h"
#include "julia_gcext.h"

// TODO make these atomics
int nmarks = 0;
int nsweeps = 0;

uintptr_t mark(jl_ptls_t ptls, jl_value_t *p)
{
    nmarks += 1;
    return 0;
}

void sweep(jl_value_t *p)
{
    nsweeps++;
}

JL_DLLEXPORT jl_datatype_t *declare_foreign(jl_sym_t* name, jl_module_t *module, jl_datatype_t *parent)
{
     return jl_new_foreign_type(name, module, parent, mark, sweep, 1, 0);
}

// #define GC_MAX_SZCLASS (2032 - sizeof(void *))

JL_DLLEXPORT int reinit_foreign(jl_datatype_t *dt)
{
    int ret = jl_reinit_foreign_type(dt, mark, sweep);
    nmarks = nsweeps = 0;
    if (ret == 0)
        return 0;
    if (dt->layout->npointers != 1)
        return -1;
    if (dt->layout->size != 0)
        return -2;
    return ret;
}

JL_DLLEXPORT jl_value_t *allocate_foreign(jl_ptls_t ptls, size_t sz, jl_datatype_t *dt)
{
    jl_value_t* obj = jl_gc_alloc_typed(ptls, sz, dt);
    jl_gc_schedule_foreign_sweepfunc(ptls, obj);
    return obj;
}

JL_DLLEXPORT int nmark_counter()
{
    return nmarks;
}

JL_DLLEXPORT int nsweep_counter()
{
    return nsweeps;
}

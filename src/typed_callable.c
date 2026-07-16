// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "julia.h"
#include "julia_internal.h"
#include "builtin_proto.h"

// TypedCallable{A,R} wraps a callable and dispatches it in the latest world.
// Each instance holds a shared trampoline for `Tuple{typeof(f), A...}`.

// Construct a TypedCallable, using `tr` when the optimizer supplied one.
static jl_typed_callable_t *typed_callable_construct(jl_task_t *ct, jl_value_t *f,
        jl_tupletype_t *argt, jl_value_t *rt, jl_dispatch_trampoline_t *tr) JL_CANSAFEPOINT
{
    jl_value_t *sigt = NULL;
    jl_value_t *tc_type = NULL;
    JL_GC_PUSH3(&sigt, &tc_type, &tr);
    if (tr == NULL) {
        sigt = jl_argtype_with_function(f, (jl_value_t*)argt); // Tuple{typeof(f), A...}
        tr = jl_get_dispatch_trampoline(sigt, rt, /*specsig*/1, JL_ABI_TYPED_CALLABLE);
    }
    tc_type = jl_apply_type2((jl_value_t*)jl_typed_callable_type, (jl_value_t*)argt, rt);
    jl_typed_callable_t *tc = (jl_typed_callable_t*)jl_gc_alloc(ct->ptls, sizeof(jl_typed_callable_t), tc_type);
    tc->f = f;
    tc->trampoline = (jl_value_t*)tr;
    JL_GC_POP();
    return tc;
}

JL_DLLEXPORT jl_typed_callable_t *jl_new_typed_callable(jl_value_t *f, jl_tupletype_t *argt, jl_value_t *rt) JL_CANSAFEPOINT
{
    if (!jl_is_tuple_type((jl_value_t*)argt))
        jl_error("TypedCallable argument tuple must be a tuple type");
    JL_TYPECHK(TypedCallable, type, rt);
    return typed_callable_construct(jl_current_task, f, argt, rt, /*tr*/NULL);
}

// Construct a TypedCallable with a trampoline supplied by the optimizer.
JL_DLLEXPORT jl_typed_callable_t *jl_new_typed_callable_resolved(jl_dispatch_trampoline_t *tr,
        jl_value_t *f, jl_tupletype_t *argt, jl_value_t *rt) JL_CANSAFEPOINT
{
    if (!jl_is_tuple_type((jl_value_t*)argt))
        jl_error("TypedCallable argument tuple must be a tuple type");
    JL_TYPECHK(TypedCallable, type, rt);
    return typed_callable_construct(jl_current_task, f, argt, rt, tr);
}

// Construct a TypedCallable with an optional statically resolved trampoline.
JL_CALLABLE(jl_f__typed_callable) JL_CANSAFEPOINT
{
    JL_NARGS(_typed_callable, 3, 4);
    if (nargs == 4) {
        if (!jl_typetagis(args[0], jl_dispatch_trampoline_type))
            jl_type_error("_typed_callable", (jl_value_t*)jl_dispatch_trampoline_type, args[0]);
        return (jl_value_t*)jl_new_typed_callable_resolved((jl_dispatch_trampoline_t*)args[0], args[1],
                (jl_tupletype_t*)args[2], args[3]);
    }
    return (jl_value_t*)jl_new_typed_callable(args[0], (jl_tupletype_t*)args[1], args[2]);
}

// Type-check arguments, then dispatch `tc->f` in the latest world.
JL_CALLABLE(jl_f_typed_callable_call) JL_CANSAFEPOINT
{
    jl_typed_callable_t *tc = (jl_typed_callable_t*)F;
    jl_value_t *argt = jl_tparam0(jl_typeof(tc));
    if (!jl_tupletype_length_compat(argt, nargs))
        jl_method_error(F, args, nargs + 1, jl_atomic_load_acquire(&jl_world_counter));
    argt = jl_unwrap_unionall(argt);
    assert(jl_is_datatype(argt));
    jl_svec_t *types = jl_get_fieldtypes((jl_datatype_t*)argt);
    size_t ntypes = jl_svec_len(types);
    for (int i = 0; i < nargs; ++i) {
        jl_value_t *typ = i >= ntypes ? jl_svecref(types, ntypes-1) : jl_svecref(types, i);
        if (jl_is_vararg(typ))
            typ = jl_unwrap_vararg(typ);
        jl_typeassert(args[i], typ);
    }
    jl_task_t *ct = jl_current_task;
    size_t last_age = ct->world_age;
    ct->world_age = jl_atomic_load_acquire(&jl_world_counter);
    jl_value_t *res = jl_apply_generic(tc->f, args, nargs);
    ct->world_age = last_age;
    jl_value_t *rt = jl_tparam1(jl_typeof(tc));
    JL_GC_PUSH1(&res);
    jl_typeassert(res, rt);
    JL_GC_POP();
    return res;
}

// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "julia.h"
#include "julia_internal.h"

jl_opaque_closure_t *jl_new_opaque_closure(jl_tupletype_t *argt, jl_value_t *isva,
    jl_value_t *rt_lb, jl_value_t *rt_ub, jl_value_t *source, jl_value_t **env, size_t nenv)
{
    if (!jl_is_tuple_type((jl_value_t*)argt)) {
        jl_error("OpaqueClosure argument tuple must be a tuple type");
    }
    JL_TYPECHK(new_opaque_closure, bool, isva);
    JL_TYPECHK(new_opaque_closure, type, rt_lb);
    JL_TYPECHK(new_opaque_closure, type, rt_ub);
    JL_TYPECHK(new_opaque_closure, method, source);
    jl_task_t *ct = jl_current_task;
    jl_value_t *oc_type JL_ALWAYS_LEAFTYPE;
    oc_type = jl_apply_type2((jl_value_t*)jl_opaque_closure_type, (jl_value_t*)argt, rt_ub);
    JL_GC_PROMISE_ROOTED(oc_type);
    jl_value_t *captures = NULL, *sigtype = NULL;
    jl_svec_t *sig_args = NULL;
    JL_GC_PUSH3(&captures, &sigtype, &sig_args);
    captures = jl_f_tuple(NULL, env, nenv);

    size_t nsig = 1 + jl_svec_len(argt->parameters);
    sig_args = jl_alloc_svec_uninit(nsig);
    jl_svecset(sig_args, 0, jl_typeof(captures));
    for (size_t i = 0; i < nsig-1; ++i) {
        jl_svecset(sig_args, 1+i, jl_tparam(argt, i));
    }
    sigtype = (jl_value_t*)jl_apply_tuple_type_v(jl_svec_data(sig_args), nsig);
    jl_method_instance_t *mi = jl_specializations_get_linfo((jl_method_t*)source, sigtype, jl_emptysvec);
    size_t world = jl_atomic_load_acquire(&jl_world_counter);
    jl_code_instance_t *ci = jl_compile_method_internal(mi, world);

    jl_opaque_closure_t *oc = (jl_opaque_closure_t*)jl_gc_alloc(ct->ptls, sizeof(jl_opaque_closure_t), oc_type);
    JL_GC_POP();
    oc->source = (jl_method_t*)source;
    oc->isva = jl_unbox_bool(isva);
    if (ci->invoke == jl_fptr_interpret_call)
        oc->invoke = (jl_fptr_args_t)jl_interpret_opaque_closure;
    else if (ci->invoke == jl_fptr_args)
        oc->invoke = jl_atomic_load_relaxed(&ci->specptr.fptr1);
    else
        oc->invoke = (jl_fptr_args_t)ci->invoke;
    oc->specptr = NULL;
    oc->captures = captures;
    oc->world = world;
    return oc;
}

JL_CALLABLE(jl_new_opaque_closure_jlcall)
{
    if (nargs < 5)
        jl_error("new_opaque_closure: Not enough arguments");
    return (jl_value_t*)jl_new_opaque_closure((jl_tupletype_t*)args[0],
        args[1], args[2], args[3], args[4], &args[5], nargs-5);
}


// check whether the specified number of arguments is compatible with the
// specified number of parameters of the tuple type
STATIC_INLINE int jl_tupletype_length_compat(jl_value_t *v, size_t nargs) JL_NOTSAFEPOINT
{
    v = jl_unwrap_unionall(v);
    assert(jl_is_tuple_type(v));
    size_t nparams = jl_nparams(v);
    if (nparams == 0)
        return nargs == 0;
    jl_value_t *va = jl_tparam(v,nparams-1);
    if (jl_is_vararg(va)) {
        jl_value_t *len = jl_unwrap_vararg_num(va);
        if (len &&jl_is_long(len))
            return nargs == nparams - 1 + jl_unbox_long(len);
        return nargs >= nparams - 1;
    }
    return nparams == nargs;
}

JL_CALLABLE(jl_f_opaque_closure_call)
{
    jl_opaque_closure_t* oc = (jl_opaque_closure_t*)F;
    jl_value_t *argt = jl_tparam0(jl_typeof(oc));
    if (!jl_tupletype_length_compat(argt, nargs))
        jl_method_error(F, args, nargs + 1, oc->world);
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
    return oc->invoke(F, args, nargs);
}

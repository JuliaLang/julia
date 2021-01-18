#include "julia.h"
#include "julia_internal.h"

JL_DLLEXPORT jl_value_t *jl_invoke_opaque_closure(jl_opaque_closure_t *oc, jl_value_t **args, size_t nargs)
{
    jl_value_t *argt = jl_tparam0(jl_typeof(oc));
    if (!jl_tupletype_length_compat(argt, nargs))
        jl_error("Incorrect argument count for OpaqueClosure");
    argt = jl_unwrap_unionall(argt);
    assert(jl_is_datatype(argt));
    jl_svec_t *types = jl_get_fieldtypes((jl_datatype_t*)argt);
    size_t ntypes = jl_svec_len(types);
    for (int i = 0; i < nargs; ++i) {
        jl_typeassert(args[i], i > ntypes ?
            jl_unwrap_vararg(jl_svecref(types, ntypes-1)) : jl_svecref(types, i));
    }
    jl_value_t *ret = NULL;
    JL_GC_PUSH1(&ret);
    jl_ptls_t ptls = jl_get_ptls_states();
    size_t last_age = ptls->world_age;
    ptls->world_age = oc->world;
    ret = jl_interpret_opaque_closure(oc, args, nargs);
    jl_typeassert(ret, jl_tparam1(jl_typeof(oc)));
    ptls->world_age = last_age;
    JL_GC_POP();
    return ret;
}

jl_opaque_closure_t *jl_new_opaque_closure(jl_tupletype_t *argt, jl_value_t *isva,
    jl_value_t *rt_lb, jl_value_t *rt_ub, jl_method_t *source, jl_value_t **env, size_t nenv)
{
    if (!jl_is_tuple_type((jl_value_t*)argt)) {
        jl_error("OpaqueClosure argument tuple must be a tuple type");
    }
    JL_TYPECHK(new_opaque_closure, bool, isva);
    JL_TYPECHK(new_opaque_closure, type, rt_lb);
    JL_TYPECHK(new_opaque_closure, type, rt_ub);
    JL_TYPECHK(new_opaque_closure, method, source);
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_value_t *oc_type JL_ALWAYS_LEAFTYPE;
    oc_type = jl_apply_type2((jl_value_t*)jl_opaque_closure_type, (jl_value_t*)argt, rt_ub);
    JL_GC_PROMISE_ROOTED(oc_type);
    jl_value_t *captures = NULL;
    JL_GC_PUSH1(&captures);
    captures = jl_f_tuple(NULL, env, nenv);
    jl_opaque_closure_t *oc = (jl_opaque_closure_t*)jl_gc_alloc(ptls, sizeof(jl_opaque_closure_t), oc_type);
    JL_GC_POP();
    oc->source = source;
    oc->isva = jl_unbox_bool(isva);
    oc->invoke = (jl_fptr_args_t)jl_invoke_opaque_closure;
    oc->specptr = NULL;
    oc->captures = captures;
    oc->world = jl_world_counter;
    return oc;
}

JL_CALLABLE(jl_new_opaque_closure_jlcall)
{
    if (nargs < 5)
        jl_error("new_opaque_closure: Not enough arguments");
    return (jl_value_t*)jl_new_opaque_closure((jl_tupletype_t*)args[0],
        args[1], args[2], args[3], (jl_method_t*)args[4], &args[5], nargs-5);
}

JL_CALLABLE(jl_f_opaque_closure_call)
{
    jl_opaque_closure_t* oc = (jl_opaque_closure_t*)F;
    return oc->invoke(F, args, nargs);
}

#include "julia.h"
#include "julia_internal.h"

JL_DLLEXPORT jl_value_t *jl_invoke_opaque_closure(jl_opaque_closure_t *clos, jl_value_t **args, size_t nargs)
{
    // TODO: Compiler support
    jl_value_t *argt = jl_tparam0(jl_typeof(clos));
    if (nargs != jl_tupletype_length(argt))
        jl_error("Incorrect argument count for OpaqueClosure");
    argt = jl_unwrap_unionall(argt);
    assert(jl_is_datatype(argt));
    for (int i = 0; i < nargs; ++i)
        jl_typeassert(args[i], jl_field_type((jl_datatype_t*)argt, i));
    jl_value_t *ret;
    JL_GC_PUSH1(&ret);
    if (jl_is_method(clos->source)) {
        // args[0] is implicitly the environment, not the closure object itself.
        // N.B.: jl_interpret_opaque_closure handles this internally.
        ret = jl_gf_invoke_by_method((jl_method_t*)clos->source, (jl_value_t*)clos->env, args, nargs + 1);
    }
    else {
        ret = jl_interpret_opaque_closure(clos, args, nargs);
    }
    jl_typeassert(ret, jl_tparam1(jl_typeof(clos)));
    JL_GC_POP();
    return ret;
}

jl_opaque_closure_t *jl_new_opaque_closure(jl_tupletype_t *argt, jl_value_t *rt_lb, jl_value_t *rt_ub, jl_value_t *source, jl_value_t **env, size_t nenv)
{
    JL_TYPECHK(new_opaque_closure, type, (jl_value_t*)argt);
    JL_TYPECHK(new_opaque_closure, type, rt_lb);
    JL_TYPECHK(new_opaque_closure, type, rt_ub);
    if (!jl_is_method(source) && !jl_is_code_info(source)) {
        jl_error("Invalid OpaqueClosure source");
    }
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_value_t *clos_t;
    jl_opaque_closure_t *clos;
    JL_GC_PUSH2(&clos_t, &clos);
    clos_t = jl_apply_type2((jl_value_t*)jl_opaque_closure_type, (jl_value_t*)argt, rt_ub);
    clos = (jl_opaque_closure_t*)jl_gc_alloc(ptls, sizeof(jl_opaque_closure_t), clos_t);
    clos->source = (jl_code_info_t*)source;
    clos->invoke = (jl_fptr_args_t)jl_invoke_opaque_closure;
    clos->specptr = NULL;
    clos->env = jl_f_tuple(NULL, env, nenv);
    JL_GC_POP();
    return clos;
}

JL_CALLABLE(jl_new_opaque_closure_jlcall)
{
    if (nargs < 4)
        jl_error("new_opaque_closure: Not enough arguments");
    return (jl_value_t*)jl_new_opaque_closure((jl_tupletype_t*)args[0],
        args[1], args[2], args[3], &args[4], nargs-4);
}

JL_CALLABLE(jl_f_opaque_closure_call)
{
    jl_opaque_closure_t* opaque = (jl_opaque_closure_t*)F;
    return opaque->invoke(F, args, nargs);
}

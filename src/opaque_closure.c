#include "julia.h"
#include "julia_internal.h"

JL_DLLEXPORT jl_value_t *jl_invoke_opaque_closure(jl_opaque_closure_t *clos, jl_value_t **args, size_t nargs)
{
    // TODO: Compiler support
    jl_tupletype_t *argt = (jl_tupletype_t*)jl_tparam0(jl_typeof(clos));
    if (nargs != jl_nparams(argt))
        jl_error("Incorrect argument count for OpaqueClosure");
    for (int i = 0; i < nargs; ++i)
        jl_typeassert(args[i], jl_field_type(argt, i));
    jl_value_t *ret;
    JL_GC_PUSH1(&ret);
    if (jl_is_method(clos->source)) {
        // args[0] is implicitly the environment, not the closure object itself.
        // N.B.: jl_interpret_opaque_closure handles this internally.
        ret = jl_gf_invoke_by_method((jl_method_t*)clos->source, (jl_value_t*)clos->env, args, nargs + 1);
    } else {
        ret = jl_interpret_opaque_closure(clos, args, nargs);
    }
    jl_typeassert(ret, jl_tparam1(jl_typeof(clos)));
    JL_GC_POP();
    return ret;
}

jl_opaque_closure_t *jl_new_opaque_closure(jl_tupletype_t *argt, jl_value_t *rt_lb, jl_value_t *rt_ub, jl_value_t *source, jl_value_t **env, size_t nenv)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_value_t *clos_t;
    jl_opaque_closure_t *clos;
    JL_GC_PUSH2(&clos_t, &clos);
    clos_t = jl_apply_type2((jl_value_t*)jl_opaque_closure_type, (jl_value_t*)argt, rt_ub);
    clos = (jl_opaque_closure_t*)jl_gc_alloc(ptls, sizeof(jl_opaque_closure_t), clos_t);
    clos->source = (jl_code_info_t*)source;
    clos->fptr1 = (jl_fptr_args_t)jl_invoke_opaque_closure;
    clos->fptr = NULL;
    clos->env = jl_f_tuple(NULL, env, nenv);
    JL_GC_POP();
    return clos;
}


JL_DLLEXPORT jl_method_t* jl_mk_opaque_closure_method(jl_module_t *def_mod)
{
    jl_method_t *m = jl_new_method_uninit(def_mod);
    m->name = jl_symbol("OpaqueClosure");
    m->sig = (jl_value_t*)jl_anytuple_type;
    m->slot_syms = jl_an_empty_string;
    return m;
}

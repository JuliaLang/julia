#include "julia.h"
#include "julia_internal.h"

JL_DLLEXPORT jl_value_t *jl_invoke_yakc(jl_yakc_t *yakc, jl_value_t **args, size_t nargs)
{
    // TODO: Compiler support
    jl_tupletype_t *argt = jl_tparam0(jl_typeof(yakc));
    if (nargs != jl_nparams(argt))
        jl_error("Incorrect argument count for YAKC");
    for (int i = 0; i < nargs; ++i)
        jl_typeassert(args[i], jl_field_type(argt, i));
    jl_value_t *ret;
    JL_GC_PUSH1(&ret);
    if (jl_is_method(yakc->source)) {
        ret = jl_gf_invoke_by_method((jl_method_t*)yakc->source, yakc, args, nargs + 1);
    } else {
        ret = jl_interpret_yakc(yakc, args, nargs);
    }
    jl_typeassert(ret, jl_tparam1(jl_typeof(yakc)));
    JL_GC_POP();
    return ret;
}

jl_yakc_t *jl_new_yakc(jl_tupletype_t *argt, jl_value_t *rt_lb, jl_value_t *rt_ub, jl_value_t *source, jl_value_t **env, size_t nenv)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_value_t *yakc_t;
    jl_yakc_t *yakc;
    JL_GC_PUSH2(&yakc_t, &yakc);
    yakc_t = jl_apply_type2((jl_value_t*)jl_yakc_type, argt, rt_ub);
    yakc = jl_gc_alloc(ptls, sizeof(jl_yakc_t), yakc_t);
    yakc->source = source;
    yakc->fptr1 = jl_invoke_yakc;
    yakc->fptr = NULL;
    yakc->env = jl_f_tuple(NULL, env, nenv);
    JL_GC_POP();
    return yakc;
}


JL_DLLEXPORT jl_method_t* jl_mk_yakc_method(jl_module_t *def_mod)
{
    jl_method_t *m = jl_new_method_uninit(def_mod);
    m->name = jl_symbol("YAKC");
    m->sig = (jl_value_t*)jl_anytuple_type;
    m->slot_syms = jl_an_empty_string;
    return m;
}

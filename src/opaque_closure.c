// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "julia.h"
#include "julia_internal.h"

jl_value_t *jl_fptr_const_opaque_closure(jl_opaque_closure_t *oc, jl_value_t **args, size_t nargs)
{
    return oc->captures;
}

// determine whether `argt` is a valid argument type tuple for the given opaque closure method
JL_DLLEXPORT int jl_is_valid_oc_argtype(jl_tupletype_t *argt, jl_method_t *source)
{
    if (!source->isva) {
        if (jl_is_va_tuple(argt))
            return 0;
        if (jl_nparams(argt)+1 > source->nargs)
            return 0;
    }
    if (jl_nparams(argt) + 1 - jl_is_va_tuple(argt) < source->nargs - source->isva)
        return 0;
    return 1;
}

static jl_value_t *prepend_type(jl_value_t *t0, jl_tupletype_t *t)
{
    jl_svec_t *sig_args = NULL;
    JL_GC_PUSH1(&sig_args);
    size_t nsig = 1 + jl_svec_len(t->parameters);
    sig_args = jl_alloc_svec_uninit(nsig);
    jl_svecset(sig_args, 0, t0);
    for (size_t i = 0; i < nsig-1; ++i) {
        jl_svecset(sig_args, 1+i, jl_tparam(t, i));
    }
    jl_value_t *sigtype = (jl_value_t*)jl_apply_tuple_type_v(jl_svec_data(sig_args), nsig);
    JL_GC_POP();
    return sigtype;
}

static jl_opaque_closure_t *new_opaque_closure(jl_tupletype_t *argt, jl_value_t *rt_lb, jl_value_t *rt_ub,
    jl_value_t *source_, jl_value_t *captures)
{
    if (!jl_is_tuple_type((jl_value_t*)argt)) {
        jl_error("OpaqueClosure argument tuple must be a tuple type");
    }
    JL_TYPECHK(new_opaque_closure, type, rt_lb);
    JL_TYPECHK(new_opaque_closure, type, rt_ub);
    JL_TYPECHK(new_opaque_closure, method, source_);
    jl_method_t *source = (jl_method_t*)source_;
    if (!source->isva) {
        if (jl_is_va_tuple(argt))
            jl_error("Argument type tuple is vararg but method is not");
        if (jl_nparams(argt)+1 > source->nargs)
            jl_error("Argument type tuple has too many required arguments for method");
    }
    if (jl_nparams(argt) + 1 - jl_is_va_tuple(argt) < source->nargs - source->isva)
        jl_error("Argument type tuple has too few required arguments for method");
    jl_value_t *sigtype = NULL;
    JL_GC_PUSH1(&sigtype);
    sigtype = prepend_type(jl_typeof(captures), argt);

    jl_value_t *oc_type JL_ALWAYS_LEAFTYPE;
    oc_type = jl_apply_type2((jl_value_t*)jl_opaque_closure_type, (jl_value_t*)argt, rt_ub);
    JL_GC_PROMISE_ROOTED(oc_type);

    jl_method_instance_t *mi = jl_specializations_get_linfo(source, sigtype, jl_emptysvec);
    size_t world = jl_atomic_load_acquire(&jl_world_counter);
    jl_code_instance_t *ci = jl_compile_method_internal(mi, world);

    jl_task_t *ct = jl_current_task;
    jl_opaque_closure_t *oc = (jl_opaque_closure_t*)jl_gc_alloc(ct->ptls, sizeof(jl_opaque_closure_t), oc_type);
    JL_GC_POP();
    oc->source = source;
    oc->captures = captures;
    oc->specptr = NULL;
    if (jl_atomic_load_relaxed(&ci->invoke) == jl_fptr_interpret_call) {
        oc->invoke = (jl_fptr_args_t)jl_interpret_opaque_closure;
    }
    else if (jl_atomic_load_relaxed(&ci->invoke) == jl_fptr_args) {
        oc->invoke = jl_atomic_load_relaxed(&ci->specptr.fptr1);
    }
    else if (jl_atomic_load_relaxed(&ci->invoke) == jl_fptr_const_return) {
        oc->invoke = (jl_fptr_args_t)jl_fptr_const_opaque_closure;
        oc->captures = ci->rettype_const;
    }
    else {
        oc->invoke = (jl_fptr_args_t)jl_atomic_load_relaxed(&ci->invoke);
    }
    oc->world = world;
    return oc;
}

jl_opaque_closure_t *jl_new_opaque_closure(jl_tupletype_t *argt, jl_value_t *rt_lb, jl_value_t *rt_ub,
    jl_value_t *source_, jl_value_t **env, size_t nenv)
{
    jl_value_t *captures = jl_f_tuple(NULL, env, nenv);
    JL_GC_PUSH1(&captures);
    jl_opaque_closure_t *oc = new_opaque_closure(argt, rt_lb, rt_ub, source_, captures);
    JL_GC_POP();
    return oc;
}

jl_method_t *jl_make_opaque_closure_method(jl_module_t *module, jl_value_t *name,
    int nargs, jl_value_t *functionloc, jl_code_info_t *ci, int isva);

JL_DLLEXPORT jl_code_instance_t* jl_new_codeinst(
        jl_method_instance_t *mi, jl_value_t *rettype,
        jl_value_t *inferred_const, jl_value_t *inferred,
        int32_t const_flags, size_t min_world, size_t max_world,
        uint32_t ipo_effects, uint32_t effects, jl_value_t *argescapes,
        uint8_t relocatability);

JL_DLLEXPORT void jl_mi_cache_insert(jl_method_instance_t *mi JL_ROOTING_ARGUMENT,
                                     jl_code_instance_t *ci JL_ROOTED_ARGUMENT JL_MAYBE_UNROOTED);

JL_DLLEXPORT jl_opaque_closure_t *jl_new_opaque_closure_from_code_info(jl_tupletype_t *argt, jl_value_t *rt_lb, jl_value_t *rt_ub,
    jl_module_t *mod, jl_code_info_t *ci, int lineno, jl_value_t *file, int nargs, int isva, jl_value_t *env)
{
    if (!ci->inferred)
        jl_error("CodeInfo must already be inferred");
    jl_value_t *root = NULL, *sigtype = NULL;
    jl_code_instance_t *inst = NULL;
    JL_GC_PUSH3(&root, &sigtype, &inst);
    root = jl_box_long(lineno);
    root = jl_new_struct(jl_linenumbernode_type, root, file);
    root = (jl_value_t*)jl_make_opaque_closure_method(mod, jl_nothing, nargs, root, ci, isva);

    sigtype = prepend_type(jl_typeof(env), argt);
    jl_method_instance_t *mi = jl_specializations_get_linfo((jl_method_t*)root, sigtype, jl_emptysvec);
    inst = jl_new_codeinst(mi, rt_ub, NULL, (jl_value_t*)ci,
        0, ((jl_method_t*)root)->primary_world, -1, 0, 0, jl_nothing, 0);
    jl_mi_cache_insert(mi, inst);

    jl_opaque_closure_t *oc = new_opaque_closure(argt, rt_lb, rt_ub, root, env);
    JL_GC_POP();
    return oc;
}

JL_CALLABLE(jl_new_opaque_closure_jlcall)
{
    if (nargs < 4)
        jl_error("new_opaque_closure: Not enough arguments");
    return (jl_value_t*)jl_new_opaque_closure((jl_tupletype_t*)args[0],
        args[1], args[2], args[3], &args[4], nargs-4);
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

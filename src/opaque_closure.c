// This file is a part of Julia. License is MIT: https://julialang.org/license

#include "julia.h"
#include "julia_internal.h"

jl_value_t *jl_fptr_const_opaque_closure(jl_opaque_closure_t *oc, jl_value_t **args, size_t nargs)
{
    return oc->captures;
}

jl_value_t *jl_fptr_const_opaque_closure_typeerror(jl_opaque_closure_t *oc, jl_value_t **args, size_t nargs)
{
    jl_type_error("OpaqueClosure", jl_tparam1(jl_typeof(oc)), oc->captures);
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

static jl_opaque_closure_t *new_opaque_closure(jl_tupletype_t *argt, jl_value_t *rt_lb, jl_value_t *rt_ub,
    jl_value_t *source_, jl_value_t *captures, int do_compile)
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
    jl_value_t *selected_rt = rt_ub;
    JL_GC_PUSH2(&sigtype, &selected_rt);
    sigtype = jl_argtype_with_function(captures, (jl_value_t*)argt);

    jl_method_instance_t *mi = NULL;
    if (source->source) {
        mi = jl_specializations_get_linfo(source, sigtype, jl_emptysvec);
    }
    else {
        mi = (jl_method_instance_t *)jl_atomic_load_relaxed(&source->specializations);
        if (!jl_subtype(sigtype, mi->specTypes)) {
            jl_error("sigtype mismatch in optimized opaque closure");
        }
    }
    jl_task_t *ct = jl_current_task;
    size_t world = ct->world_age;
    jl_code_instance_t *ci = NULL;
    if (do_compile) {
        ci = jl_compile_method_internal(mi, world);
    }

    jl_fptr_args_t callptr = (jl_fptr_args_t)jl_interpret_opaque_closure;
    void *specptr = NULL;

    if (ci) {
        uint8_t specsigflags;
        jl_callptr_t invoke;
        jl_read_codeinst_invoke(ci, &specsigflags, &invoke, &specptr, 1);
        callptr = (jl_fptr_args_t)invoke; // codegen puts the object (or a jl_fptr_interpret_call token )here for us, even though it was the wrong type to put here

        selected_rt = ci->rettype;
        // If we're not allowed to generate a specsig with this, rt, fall
        // back to the invoke wrapper. We could instead generate a specsig->specsig
        // wrapper, but lets leave that for later.
        if (!jl_subtype(rt_lb, selected_rt)) {
            // TODO: It would be better to try to get a specialization with the
            // correct rt check here (or we could codegen a wrapper).
            specptr = NULL; // this will force codegen of the unspecialized version
            callptr = (jl_fptr_args_t)jl_interpret_opaque_closure;
            jl_value_t *ts[2] = {rt_lb, (jl_value_t*)ci->rettype};
            selected_rt = jl_type_union(ts, 2);
        }
        if (!jl_subtype(ci->rettype, rt_ub)) {
            // TODO: It would be better to try to get a specialization with the
            // correct rt check here (or we could codegen a wrapper).
            specptr = NULL; // this will force codegen of the unspecialized version
            callptr = (jl_fptr_args_t)jl_interpret_opaque_closure;
            selected_rt = jl_type_intersection(rt_ub, selected_rt);
        }

        if (callptr == (jl_fptr_args_t)jl_fptr_interpret_call) {
            callptr = (jl_fptr_args_t)jl_interpret_opaque_closure;
        }
        else if (callptr == (jl_fptr_args_t)jl_fptr_args && specptr != NULL) {
            callptr = (jl_fptr_args_t)specptr;
        }
        else if (callptr == (jl_fptr_args_t)jl_fptr_const_return) {
            callptr = jl_isa(ci->rettype_const, selected_rt) ?
                (jl_fptr_args_t)jl_fptr_const_opaque_closure :
                (jl_fptr_args_t)jl_fptr_const_opaque_closure_typeerror;
            captures = ci->rettype_const;
        }
    }

    jl_value_t *oc_type JL_ALWAYS_LEAFTYPE = jl_apply_type2((jl_value_t*)jl_opaque_closure_type, (jl_value_t*)argt, selected_rt);
    JL_GC_PROMISE_ROOTED(oc_type);

    if (specptr == NULL) {
        jl_method_instance_t *mi_generic = jl_specializations_get_linfo(jl_opaque_closure_method, sigtype, jl_emptysvec);

        // OC wrapper methods are not world dependent and have no edges or other info
        ci = jl_get_method_inferred(mi_generic, selected_rt, 1, ~(size_t)0, NULL, NULL);
        if (!jl_atomic_load_acquire(&ci->invoke)) {
            jl_emit_codeinst_to_jit(ci, NULL); // confusing this actually calls jl_emit_oc_wrapper and never actually compiles ci (which would be impossible since it cannot have source)
            jl_compile_codeinst(ci);
        }
        specptr = jl_atomic_load_relaxed(&ci->specptr.fptr);
    }
    jl_opaque_closure_t *oc = (jl_opaque_closure_t*)jl_gc_alloc(ct->ptls, sizeof(jl_opaque_closure_t), oc_type);
    oc->source = source;
    oc->captures = captures;
    oc->world = world;
    oc->invoke = callptr;
    oc->specptr = specptr;

    JL_GC_POP();
    return oc;
}

jl_opaque_closure_t *jl_new_opaque_closure(jl_tupletype_t *argt, jl_value_t *rt_lb, jl_value_t *rt_ub,
    jl_value_t *source_, jl_value_t **env, size_t nenv, int do_compile)
{
    jl_value_t *captures = jl_f_tuple(NULL, env, nenv);
    JL_GC_PUSH1(&captures);
    jl_opaque_closure_t *oc = new_opaque_closure(argt, rt_lb, rt_ub, source_, captures, do_compile);
    JL_GC_POP();
    return oc;
}

JL_DLLEXPORT jl_opaque_closure_t *jl_new_opaque_closure_from_code_info(jl_tupletype_t *argt, jl_value_t *rt_lb, jl_value_t *rt_ub,
    jl_module_t *mod, jl_code_info_t *ci, int lineno, jl_value_t *file, int nargs, int isva, jl_value_t *env, int do_compile, int isinferred)
{
    jl_value_t *root = NULL, *sigtype = NULL;
    jl_code_instance_t *inst = NULL;
    jl_svec_t *edges = NULL;
    JL_GC_PUSH4(&root, &sigtype, &inst, &edges);
    root = jl_box_long(lineno);
    root = jl_new_struct(jl_linenumbernode_type, root, file);
    jl_method_t *meth = jl_make_opaque_closure_method(mod, jl_nothing, nargs, root, ci, isva, isinferred);
    root = (jl_value_t*)meth;
    size_t world = jl_current_task->world_age;
    // these are only legal in the current world since they are not in any tables
    jl_atomic_store_release(&meth->primary_world, world);

    if (isinferred) {
        jl_value_t *argslotty = jl_array_ptr_ref(ci->slottypes, 0);
        sigtype = jl_argtype_with_function_type(argslotty, (jl_value_t*)argt);
        jl_method_instance_t *mi = jl_specializations_get_linfo((jl_method_t*)root, sigtype, jl_emptysvec);
        edges = (jl_svec_t*)ci->edges;
        if (!jl_is_svec(edges))
            edges = jl_emptysvec; // OC doesn't really have edges, so just drop them for now
        inst = jl_new_codeinst(mi, jl_nothing, rt_ub, (jl_value_t*)jl_any_type, NULL, (jl_value_t*)ci,
            0, world, world, 0, jl_nothing, ci->debuginfo, edges);
        jl_mi_cache_insert(mi, inst);
    }

    jl_opaque_closure_t *oc = new_opaque_closure(argt, rt_lb, rt_ub, root, env, do_compile);
    JL_GC_POP();
    return oc;
}

JL_CALLABLE(jl_new_opaque_closure_jlcall)
{
    if (nargs < 5)
        jl_error("new_opaque_closure: Not enough arguments");
    return (jl_value_t*)jl_new_opaque_closure((jl_tupletype_t*)args[0],
        args[1], args[2], args[4], &args[5], nargs-5, 1);
}

// check whether the specified number of arguments is compatible with the
// specified number of parameters of the tuple type
int jl_tupletype_length_compat(jl_value_t *v, size_t nargs)
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
    jl_opaque_closure_t *oc = (jl_opaque_closure_t*)F;
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

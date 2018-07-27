// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  Generic Functions
  . method table and lookup
  . GF constructor
  . dispatch
  . static parameter inference
  . method specialization and caching, invoking type inference
*/
#include <stdlib.h>
#include <string.h>
#include "julia.h"
#include "julia_internal.h"
#ifndef _OS_WINDOWS_
#include <unistd.h>
#endif
#include "julia_assert.h"

// The compilation signature is not used to cache the method if the number of overlapping methods is greater than this
#define MAX_UNSPECIALIZED_CONFLICTS 32

#ifdef __cplusplus
extern "C" {
#endif

JL_DLLEXPORT size_t jl_world_counter = 1;
JL_DLLEXPORT size_t jl_get_world_counter(void)
{
    return jl_world_counter;
}

JL_DLLEXPORT size_t jl_get_tls_world_age(void)
{
    return jl_get_ptls_states()->world_age;
}

JL_DLLEXPORT jl_value_t *jl_invoke(jl_method_instance_t *meth, jl_value_t **args, uint32_t nargs)
{
    jl_callptr_t fptr = meth->invoke;
    if (fptr != jl_fptr_trampoline) {
        return fptr(meth, args, nargs);
    }
    else {
        // if this hasn't been inferred (compiled) yet,
        // inferring it might not be able to handle the world range
        // so we just do a generic apply here
        // because that might actually be faster
        // since it can go through the unrolled caches for this world
        // and if inference is successful, this meth would get updated anyways,
        // and we'll get the fast path here next time

        // TODO: if `meth` came from an `invoke` call, we should make sure
        // meth->def is called instead of doing normal dispatch.

        return jl_apply(args, nargs);
    }
}

/// ----- Handling for Julia callbacks ----- ///

JL_DLLEXPORT int8_t jl_is_in_pure_context(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    return ptls->in_pure_callback;
}

JL_DLLEXPORT void jl_trace_method(jl_method_t *m)
{
    assert(jl_is_method(m));
    m->traced = 1;
}

JL_DLLEXPORT void jl_untrace_method(jl_method_t *m)
{
    assert(jl_is_method(m));
    m->traced = 0;
}

JL_DLLEXPORT void jl_trace_linfo(jl_method_instance_t *linfo)
{
    assert(jl_is_method_instance(linfo));
    linfo->compile_traced = 1;
}

JL_DLLEXPORT void jl_untrace_linfo(jl_method_instance_t *linfo)
{
    assert(jl_is_method_instance(linfo));
    linfo->compile_traced = 0;
}

static tracer_cb jl_method_tracer = NULL;
JL_DLLEXPORT void jl_register_method_tracer(void (*callback)(jl_method_instance_t *tracee))
{
    jl_method_tracer = (tracer_cb)callback;
}

tracer_cb jl_newmeth_tracer = NULL;
JL_DLLEXPORT void jl_register_newmeth_tracer(void (*callback)(jl_method_t *tracee))
{
    jl_newmeth_tracer = (tracer_cb)callback;
}

tracer_cb jl_linfo_tracer = NULL;
JL_DLLEXPORT void jl_register_linfo_tracer(void (*callback)(jl_method_instance_t *tracee))
{
    jl_linfo_tracer = (tracer_cb)callback;
}

void jl_call_tracer(tracer_cb callback, jl_value_t *tracee)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    int last_in = ptls->in_pure_callback;
    JL_TRY {
        ptls->in_pure_callback = 1;
        callback(tracee);
        ptls->in_pure_callback = last_in;
    }
    JL_CATCH {
        ptls->in_pure_callback = last_in;
        jl_printf(JL_STDERR, "WARNING: tracer callback function threw an error:\n");
        jl_static_show(JL_STDERR, ptls->exception_in_transit);
        jl_printf(JL_STDERR, "\n");
        jlbacktrace();
    }
}

/// ----- Definitions for various internal TypeMaps ----- ///

const struct jl_typemap_info method_defs = {
    0, &jl_method_type
};
const struct jl_typemap_info lambda_cache = {
    0, &jl_method_instance_type
};
const struct jl_typemap_info tfunc_cache = {
    1, &jl_any_type
};

static int8_t jl_cachearg_offset(jl_methtable_t *mt)
{
    // TODO: consider reverting this when we can split on Type{...} better
    return 1; //(mt == jl_type_type_mt) ? 0 : 1;
}

/// ----- Insertion logic for special entries ----- ///

// get or create the MethodInstance for a specialization
JL_DLLEXPORT jl_method_instance_t *jl_specializations_get_linfo(jl_method_t *m, jl_value_t *type, jl_svec_t *sparams, size_t world)
{
    assert(world >= m->min_world && "typemap lookup is corrupted");
    JL_LOCK(&m->writelock);
    jl_typemap_entry_t *sf =
        jl_typemap_assoc_by_type(m->specializations, type, NULL, /*subtype*/0, /*offs*/0, world, /*max_world_mask*/0);
    if (sf && jl_is_method_instance(sf->func.value)) {
        jl_method_instance_t *linfo = (jl_method_instance_t*)sf->func.value;
        assert(linfo->min_world <= sf->min_world && linfo->max_world >= sf->max_world);
        JL_UNLOCK(&m->writelock);
        return linfo;
    }
    jl_method_instance_t *li = jl_get_specialized(m, type, sparams);
    JL_GC_PUSH1(&li);
    // TODO: fuse lookup and insert steps
    // pick an initial world that is likely to be valid both before and after inference
    if (world > jl_world_counter) {
        li->min_world = jl_world_counter;
    }
    else {
        li->min_world = world;
    }
    if (world == jl_world_counter) {
        li->max_world = ~(size_t)0;
    }
    else {
        li->max_world = world;
    }
    jl_typemap_insert(&m->specializations, (jl_value_t*)m, (jl_tupletype_t*)type,
            NULL, jl_emptysvec, (jl_value_t*)li, 0, &tfunc_cache,
            li->min_world, li->max_world, NULL);
    JL_UNLOCK(&m->writelock);
    JL_GC_POP();
    return li;
}

JL_DLLEXPORT jl_value_t *jl_specializations_lookup(jl_method_t *m, jl_value_t *type, size_t world)
{
    jl_typemap_entry_t *sf = jl_typemap_assoc_by_type(
            m->specializations, type, NULL, /*subtype*/0, /*offs*/0, world, /*max_world_mask*/0);
    if (!sf)
        return jl_nothing;
    return sf->func.value;
}

JL_DLLEXPORT jl_value_t *jl_methtable_lookup(jl_methtable_t *mt, jl_value_t *type, size_t world)
{
    jl_typemap_entry_t *sf = jl_typemap_assoc_by_type(
            mt->defs, type, NULL, /*subtype*/0, /*offs*/0, world, /*max_world_mask*/0);
    if (!sf)
        return jl_nothing;
    return sf->func.value;
}

// ----- MethodInstance specialization instantiation ----- //

JL_DLLEXPORT jl_method_t *jl_new_method_uninit(jl_module_t*);

void jl_mk_builtin_func(jl_datatype_t *dt, const char *name, jl_fptr_args_t fptr)
{
    jl_sym_t *sname = jl_symbol(name);
    if (dt == NULL) {
        jl_value_t *f = jl_new_generic_function_with_supertype(sname, jl_core_module, jl_builtin_type, 0);
        jl_set_const(jl_core_module, sname, f);
        dt = (jl_datatype_t*)jl_typeof(f);
    }
    jl_method_instance_t *li = jl_new_method_instance_uninit();
    li->invoke = jl_fptr_args;
    li->specptr.fptr1 = fptr;
    li->specTypes = (jl_value_t*)jl_anytuple_type;
    li->min_world = 1;
    li->max_world = ~(size_t)0;

    JL_GC_PUSH1(&li);
    jl_method_t *m = jl_new_method_uninit(jl_core_module);
    li->def.method = m;
    jl_gc_wb(li, m);
    m->name = sname;
    m->module = jl_core_module;
    m->isva = 1;
    m->nargs = 2;
    m->sig = (jl_value_t*)jl_anytuple_type;
    m->sparam_syms = jl_emptysvec;

    jl_methtable_t *mt = dt->name->mt;
    jl_typemap_insert(&mt->cache, (jl_value_t*)mt, jl_anytuple_type,
        NULL, jl_emptysvec, (jl_value_t*)li, 0, &lambda_cache, 1, ~(size_t)0, NULL);
    JL_GC_POP();
}

// run type inference on lambda "li" for given argument types.
// returns the inferred source, and may cache the result in li
// if successful, also updates the li argument to describe the validity of this src
// if inference doesn't occur (or can't finish), returns NULL instead
jl_code_info_t *jl_type_infer(jl_method_instance_t **pli, size_t world, int force)
{
    JL_TIMING(INFERENCE);
    if (jl_typeinf_func == NULL)
        return NULL;
    static int in_inference;
    if (in_inference > 2)
        return NULL;

    jl_code_info_t *src = NULL;
#ifdef ENABLE_INFERENCE
    jl_method_instance_t *li = *pli;
    if (li->inInference && !force)
        return NULL;

    jl_value_t **fargs;
    JL_GC_PUSHARGS(fargs, 3);
    fargs[0] = (jl_value_t*)jl_typeinf_func;
    fargs[1] = (jl_value_t*)li;
    fargs[2] = jl_box_ulong(world);
#ifdef TRACE_INFERENCE
    if (li->specTypes != (jl_value_t*)jl_emptytuple_type) {
        jl_printf(JL_STDERR,"inference on ");
        jl_static_show_func_sig(JL_STDERR, (jl_value_t*)li->specTypes);
        jl_printf(JL_STDERR, "\n");
    }
#endif
    jl_ptls_t ptls = jl_get_ptls_states();
    size_t last_age = ptls->world_age;
    ptls->world_age = jl_typeinf_world;
    li->inInference = 1;
    in_inference++;
    jl_svec_t *linfo_src = (jl_svec_t*)jl_apply_with_saved_exception_state(fargs, 3, 0);
    ptls->world_age = last_age;
    in_inference--;
    li->inInference = 0;

    if (linfo_src &&
            jl_is_svec(linfo_src) && jl_svec_len(linfo_src) == 2 &&
            jl_is_method_instance(jl_svecref(linfo_src, 0)) &&
            jl_is_code_info(jl_svecref(linfo_src, 1))) {
        *pli = (jl_method_instance_t*)jl_svecref(linfo_src, 0);
        src = (jl_code_info_t*)jl_svecref(linfo_src, 1);
    }
    JL_GC_POP();
#endif
    return src;
}

int jl_is_rettype_inferred(jl_method_instance_t *li)
{
    if (!li->inferred)
        return 0;
    if (jl_is_code_info(li->inferred) && !((jl_code_info_t*)li->inferred)->inferred)
        return 0;
    return 1;
}

struct set_world {
    jl_method_instance_t *replaced;
    size_t world;
};

static int set_max_world2(jl_typemap_entry_t *entry, void *closure0)
{
    struct set_world *closure = (struct set_world*)closure0;
    // entry->max_world should be <= closure->replaced->max_world
    if (entry->func.linfo == closure->replaced && entry->max_world > closure->world) {
        entry->max_world = closure->world;
    }
    return 1;
}

static int set_min_world2(jl_typemap_entry_t *entry, void *closure0)
{
    struct set_world *closure = (struct set_world*)closure0;
    // entry->min_world should be >= closure->replaced->min_world and >= closure->world
    if (entry->func.linfo == closure->replaced) {
        entry->min_world = closure->world;
    }
    return 1;
}

static void update_world_bound(jl_method_instance_t *replaced, jl_typemap_visitor_fptr fptr, size_t world)
{
    struct set_world update;
    update.replaced = replaced;
    update.world = world;

    jl_method_t *m = replaced->def.method;
    // update the world-valid in the specializations caches
    jl_typemap_visitor(m->specializations, fptr, (void*)&update);
    // update the world-valid in the invoke cache
    if (m->invokes.unknown != NULL)
        jl_typemap_visitor(m->invokes, fptr, (void*)&update);
    // update the world-valid in the gf cache
    jl_datatype_t *gf = jl_first_argument_datatype((jl_value_t*)m->sig);
    assert(jl_is_datatype(gf) && gf->name->mt && "method signature invalid?");
    jl_typemap_visitor(gf->name->mt->cache, fptr, (void*)&update);
}

JL_DLLEXPORT jl_method_instance_t* jl_set_method_inferred(
        jl_method_instance_t *li, jl_value_t *rettype,
        jl_value_t *inferred_const, jl_value_t *inferred,
        int32_t const_flags, size_t min_world, size_t max_world)
{
    JL_GC_PUSH1(&li);
    assert(min_world <= max_world && "attempting to set invalid world constraints");
    assert(li->inInference && "shouldn't be caching an inference result for a MethodInstance that wasn't being inferred");
    if (min_world != li->min_world || max_world != li->max_world) {
        if (!jl_is_method(li->def.method)) {
            // thunks don't have multiple references, so just update in-place
            li->min_world = min_world;
            li->max_world = max_world;
        }
        else {
            JL_LOCK(&li->def.method->writelock);
            assert(min_world >= li->def.method->min_world);
            int isinferred =  jl_is_rettype_inferred(li);
            if (!isinferred && li->min_world >= min_world && li->max_world <= max_world) {
                // expand the current (uninferred) entry to cover the full inferred range
                // only update the specializations though, since the method table may have other
                // reasons for needing a narrower applicability range
                struct set_world update;
                update.replaced = li;
                if (li->min_world != min_world) {
                    li->min_world = min_world;
                    update.world = min_world;
                    jl_typemap_visitor(li->def.method->specializations, set_min_world2, (void*)&update);
                }
                if (li->max_world != max_world) {
                    li->max_world = max_world;
                    update.world = max_world;
                    jl_typemap_visitor(li->def.method->specializations, set_max_world2, (void*)&update);
                }
            }
            else {
                // clip applicability of old method instance (uninferred or inferred)
                // to make it easier to find the inferred method
                // (even though the real applicability was unchanged)
                // there are 6(!) regions here to consider + boundary conditions for each
                if (li->max_world >= min_world && li->min_world <= max_world) {
                    // there is a non-zero overlap between [li->min, li->max] and [min, max]
                    // there are now 4 regions left to consider
                    // TODO: also take into account li->def.method->world range when computing preferred division
                    if (li->max_world > max_world) {
                        // prefer making it applicable to future ages,
                        // as those are more likely to be useful
                        update_world_bound(li, set_min_world2, max_world + 1);
                    }
                    else if (li->min_world < min_world) {
                        assert(min_world > 1 && "logic violation: min(li->min_world) == 1 (by construction), so min(min_world) == 2");
                        update_world_bound(li, set_max_world2, min_world - 1);
                    }
                    else {
                        // old inferred li is fully covered by new inference result, so just delete it
                        assert(isinferred);
                        update_world_bound(li, set_max_world2, li->min_world - 1);
                    }
                }

                // build a new entry to describe the new (inferred) applicability
                li = jl_get_specialized(li->def.method, li->specTypes, li->sparam_vals);
                li->min_world = min_world;
                li->max_world = max_world;
                jl_typemap_insert(&li->def.method->specializations, li->def.value,
                        (jl_tupletype_t*)li->specTypes, NULL, jl_emptysvec,
                        (jl_value_t*)li, 0, &tfunc_cache,
                        li->min_world, li->max_world, NULL);
            }
            JL_UNLOCK(&li->def.method->writelock);
        }
    }

    // changing rettype changes the llvm signature,
    // so clear all of the llvm state at the same time
    li->invoke = jl_fptr_trampoline;
    li->functionObjectsDecls.functionObject = NULL;
    li->functionObjectsDecls.specFunctionObject = NULL;
    li->rettype = rettype;
    jl_gc_wb(li, rettype);
    li->inferred = inferred;
    jl_gc_wb(li, inferred);
    if (const_flags & 2) {
        li->inferred_const = inferred_const;
        jl_gc_wb(li, inferred_const);
    }
    if (const_flags & 1) {
        assert(const_flags & 2);
        li->invoke = jl_fptr_const_return;
    }
    li->specptr.fptr = NULL;
    JL_GC_POP();
    return li;
}

static int get_spec_unspec_list(jl_typemap_entry_t *l, void *closure)
{
    if (jl_is_method_instance(l->func.value) && !jl_is_rettype_inferred(l->func.linfo))
        jl_array_ptr_1d_push((jl_array_t*)closure, l->func.value);
    return 1;
}

static int get_method_unspec_list(jl_typemap_entry_t *def, void *closure)
{
    jl_typemap_visitor(def->func.method->specializations, get_spec_unspec_list, closure);
    return 1;
}

static void foreach_mtable_in_module(
        jl_module_t *m,
        void (*visit)(jl_methtable_t *mt, void *env),
        void *env,
        jl_array_t *visited)
{
    size_t i;
    void **table = m->bindings.table;
    jl_eqtable_put(visited, m, jl_true, NULL);
    for (i = 1; i < m->bindings.size; i += 2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            if (b->owner == m && b->value && b->constp) {
                jl_value_t *v = jl_unwrap_unionall(b->value);
                if (jl_is_datatype(v)) {
                    jl_typename_t *tn = ((jl_datatype_t*)v)->name;
                    if (tn->module == m && tn->name == b->name) {
                        jl_methtable_t *mt = tn->mt;
                        if (mt != NULL && (jl_value_t*)mt != jl_nothing && mt != jl_type_type_mt) {
                            visit(mt, env);
                        }
                    }
                }
                else if (jl_is_module(v)) {
                    jl_module_t *child = (jl_module_t*)v;
                    if (child != m && child->parent == m && child->name == b->name &&
                        !jl_eqtable_get(visited, v, NULL)) {
                        // this is the original/primary binding for the submodule
                        foreach_mtable_in_module(child, visit, env, visited);
                    }
                }
            }
        }
    }
}

void jl_foreach_reachable_mtable(void (*visit)(jl_methtable_t *mt, void *env), void *env)
{
    jl_array_t *visited = jl_alloc_vec_any(16);
    jl_array_t *mod_array = NULL;
    JL_GC_PUSH2(&visited, &mod_array);
    mod_array = jl_get_loaded_modules();
    visit(jl_type_type_mt, env);
    if (mod_array) {
        int i;
        for (i = 0; i < jl_array_len(mod_array); i++) {
            jl_module_t *m = (jl_module_t*)jl_array_ptr_ref(mod_array, i);
            assert(jl_is_module(m));
            if (!jl_eqtable_get(visited, (jl_value_t*)m, NULL))
                foreach_mtable_in_module(m, visit, env, visited);
        }
    }
    else {
        foreach_mtable_in_module(jl_main_module, visit, env, visited);
    }
    JL_GC_POP();
}

static void reset_mt_caches(jl_methtable_t *mt, void *env)
{
    // removes all method caches
    if (mt->defs.unknown != jl_nothing) // make sure not to reset builtin functions
        mt->cache.unknown = jl_nothing;
    jl_typemap_visitor(mt->defs, get_method_unspec_list, env);
}


jl_function_t *jl_typeinf_func = NULL;
size_t jl_typeinf_world = 0;

JL_DLLEXPORT void jl_set_typeinf_func(jl_value_t *f)
{
    jl_typeinf_func = (jl_function_t*)f;
    jl_typeinf_world = jl_get_tls_world_age();
    ++jl_world_counter; // make type-inference the only thing in this world
    // give type inference a chance to see all of these
    // TODO: also reinfer if max_world != ~(size_t)0
    jl_array_t *unspec = jl_alloc_vec_any(0);
    JL_GC_PUSH1(&unspec);
    jl_foreach_reachable_mtable(reset_mt_caches, (void*)unspec);
    size_t i, l;
    for (i = 0, l = jl_array_len(unspec); i < l; i++) {
        jl_method_instance_t *li = (jl_method_instance_t*)jl_array_ptr_ref(unspec, i);
        if (!jl_is_rettype_inferred(li))
            jl_type_infer(&li, jl_world_counter, 1);
    }
    JL_GC_POP();
}

static int very_general_type(jl_value_t *t)
{
    return (t == (jl_value_t*)jl_any_type || t == (jl_value_t*)jl_type_type || jl_types_equal(t, (jl_value_t*)jl_type_type));
}

jl_value_t *jl_nth_slot_type(jl_value_t *sig, size_t i)
{
    sig = jl_unwrap_unionall(sig);
    size_t len = jl_field_count(sig);
    if (len == 0)
        return NULL;
    if (i < len-1)
        return jl_tparam(sig, i);
    if (jl_is_vararg_type(jl_tparam(sig,len-1)))
        return jl_unwrap_vararg(jl_tparam(sig,len-1));
    if (i == len-1)
        return jl_tparam(sig, i);
    return NULL;
}

// if concrete_match returns false, the sig may specify `Type{T::DataType}`, while the `tt` contained DataType
// in this case, subtyping is wrong, and this may not actually match at runtime
// since it may instead match any kind of `Type{T::Type}`
//static int concrete_match(jl_tupletype_t *tt, jl_value_t *sig)
//{
//    size_t i, np;
//    for (i = 0, np = jl_nparams(tt); i < np; i++) {
//        jl_value_t *elt = jl_tparam(tt, i);
//        jl_value_t *decl_i = jl_nth_slot_type((jl_value_t*)sig, i);
//        if (jl_is_kind(elt)) {
//            // check whether this match may be exact at runtime
//            if (!jl_subtype(elt, decl_i))
//                return 0;
//        }
//    }
//    return 1;
//}

static jl_value_t *ml_matches(union jl_typemap_t ml, int offs,
                              jl_tupletype_t *type, int lim, int include_ambiguous,
                              size_t world, size_t *min_valid, size_t *max_valid);

// get the compilation signature specialization for this method
static void jl_compilation_sig(
    jl_tupletype_t *const tt, // the original tupletype of the call : this is expected to be a relative simple type (no Varags, Union, UnionAll, etc.)
    jl_svec_t *sparams,
    jl_method_t *definition,
    intptr_t nspec,
    // output:
    jl_svec_t **const newparams)
{
    if (definition->generator) {
        // staged functions aren't optimized
        // so assume the caller was intelligent about calling us
        return;
    }

    jl_value_t *decl = definition->sig;
    assert(jl_is_tuple_type(tt));
    size_t i, np = jl_nparams(tt);
    size_t nargs = definition->nargs; // == jl_field_count(jl_unwrap_unionall(decl));
    for (i = 0; i < np; i++) {
        jl_value_t *elt = jl_tparam(tt, i);
        jl_value_t *decl_i = jl_nth_slot_type(decl, i);
        size_t i_arg = (i < nargs - 1 ? i : nargs - 1);

        if (jl_is_kind(decl_i)) {
            // if we can prove the match was against the kind (not a Type)
            // we want to put that in the cache instead
            if (!*newparams) *newparams = jl_svec_copy(tt->parameters);
            elt = decl_i;
            jl_svecset(*newparams, i, elt);
        }
        else if (jl_is_type_type(elt)) {
            // if the declared type was not Any or Union{Type, ...},
            // then the match must been with the kind (e.g. UnionAll or DataType)
            // and the result of matching the type signature
            // needs to be restricted to the concrete type 'kind'
            jl_value_t *kind = jl_typeof(jl_tparam0(elt));
            if (jl_subtype(kind, decl_i) && !jl_subtype((jl_value_t*)jl_type_type, decl_i)) {
                // if we can prove the match was against the kind (not a Type)
                // it's simpler (and thus better) to put that cache instead
                if (!*newparams) *newparams = jl_svec_copy(tt->parameters);
                elt = kind;
                jl_svecset(*newparams, i, elt);
            }
        }

        if (jl_is_kind(elt)) {
            // kind slots always need guard entries (checking for subtypes of Type)
            continue;
        }

        if (i_arg > 0 && i_arg <= sizeof(definition->nospecialize) * 8 &&
                (definition->nospecialize & (1 << (i_arg - 1)))) {
            if (!jl_has_free_typevars(decl_i) && !jl_is_kind(decl_i)) {
                if (decl_i != elt) {
                    if (!*newparams) *newparams = jl_svec_copy(tt->parameters);
                    jl_svecset(*newparams, i, (jl_value_t*)decl_i);
                }
                continue;
            }
        }

        if (jl_is_type_type(elt)) {
            if (very_general_type(decl_i)) {
                /*
                  here's a fairly simple heuristic: if this argument slot's
                  declared type is general (Type or Any),
                  then don't specialize for every Type that got passed.

                  Since every type x has its own type Type{x}, this would be
                  excessive specialization for an Any slot.

                  This may require guard entries due to other potential matches.
                  In particular, TypeConstructors are problematic because they can
                  be alternate representations of any type. Extensionally, TC == TC.body,
                  but typeof(TC) != typeof(TC.body). This creates an ambiguity:
                  Type{TC} is type-equal to Type{TC.body}, yet a slot
                  x::TypeConstructor matches the first but not the second, while
                  also matching all other TypeConstructors. This means neither
                  Type{TC} nor TypeConstructor is more specific.
                */
                if (!*newparams) *newparams = jl_svec_copy(tt->parameters);
                jl_svecset(*newparams, i, jl_typetype_type);
            }
            else if (jl_is_type_type(jl_tparam0(elt)) &&
                     // try to give up on specializing type parameters for Type{Type{Type{...}}}
                     (jl_is_type_type(jl_tparam0(jl_tparam0(elt))) || !jl_has_free_typevars(decl_i))) {
                // TODO: this is probably solidly unsound and would corrupt the cache in many cases
                /*
                  actual argument was Type{...}, we computed its type as
                  Type{Type{...}}. we must avoid unbounded nesting here, so
                  cache the signature as Type{T}, unless something more
                  specific like Type{Type{Int32}} was actually declared.
                  this can be determined using a type intersection.
                */
                if (!*newparams) *newparams = jl_svec_copy(tt->parameters);
                if (i < nargs || !definition->isva) {
                    jl_value_t *di = jl_type_intersection(decl_i, (jl_value_t*)jl_typetype_type);
                    assert(di != (jl_value_t*)jl_bottom_type);
                    // issue #11355: DataType has a UID and so would take precedence in the cache
                    if (jl_is_kind(di))
                        jl_svecset(*newparams, i, (jl_value_t*)jl_typetype_type);
                    else
                        jl_svecset(*newparams, i, di);
                    // TODO: recompute static parameter values, so in extreme cases we
                    // can give `T=Type` instead of `T=Type{Type{Type{...`.   /* make editors happy:}}} */
                }
                else {
                    jl_svecset(*newparams, i, (jl_value_t*)jl_typetype_type);
                }
            }
        }

        int notcalled_func = (i_arg > 0 && i_arg <= 8 && !(definition->called & (1 << (i_arg - 1))) &&
                              jl_subtype(elt, (jl_value_t*)jl_function_type));
        if (notcalled_func && (decl_i == (jl_value_t*)jl_any_type ||
                               decl_i == (jl_value_t*)jl_function_type ||
                               (jl_is_uniontype(decl_i) && // Base.Callable
                                ((((jl_uniontype_t*)decl_i)->a == (jl_value_t*)jl_function_type &&
                                  ((jl_uniontype_t*)decl_i)->b == (jl_value_t*)jl_type_type) ||
                                 (((jl_uniontype_t*)decl_i)->b == (jl_value_t*)jl_function_type &&
                                  ((jl_uniontype_t*)decl_i)->a == (jl_value_t*)jl_type_type))))) {
            // and attempt to despecialize types marked Function, Callable, or Any
            // when called with a subtype of Function but is not called
            if (!*newparams) *newparams = jl_svec_copy(tt->parameters);
            jl_svecset(*newparams, i, (jl_value_t*)jl_function_type);
            continue;
        }
    }

    // for varargs methods, only specialize up to max_args.
    // in general, here we want to find the biggest type that's not a
    // supertype of any other method signatures. so far we are conservative
    // and the types we find should be bigger.
    if (jl_nparams(tt) >= nspec && jl_va_tuple_kind((jl_datatype_t*)definition->sig) == JL_VARARG_UNBOUND) {
        jl_svec_t *limited = jl_alloc_svec(nspec);
        jl_value_t *temp = NULL;
        JL_GC_PUSH2(&limited, &temp);
        if (!*newparams) *newparams = tt->parameters;
        size_t i;
        for (i = 0; i < nspec - 1; i++) {
            jl_svecset(limited, i, jl_svecref(*newparams, i));
        }
        jl_value_t *lasttype = jl_svecref(*newparams, i - 1);
        // if all subsequent arguments are subtypes of lasttype, specialize
        // on that instead of decl. for example, if decl is
        // (Any...)
        // and type is
        // (Symbol, Symbol, Symbol)
        // then specialize as (Symbol...), but if type is
        // (Symbol, Int32, Expr)
        // then specialize as (Any...)
        size_t j = i;
        int all_are_subtypes = 1;
        for (; j < jl_svec_len(*newparams); j++) {
            if (!jl_subtype(jl_svecref(*newparams, j), lasttype)) {
                all_are_subtypes = 0;
                break;
            }
        }
        if (all_are_subtypes) {
            // avoid Type{Type{...}}...
            if (jl_is_type_type(lasttype) && jl_is_type_type(jl_tparam0(lasttype)))
                lasttype = (jl_value_t*)jl_type_type;
            jl_svecset(limited, i, jl_wrap_vararg(lasttype, (jl_value_t*)NULL));
        }
        else {
            jl_value_t *unw = jl_unwrap_unionall(definition->sig);
            jl_value_t *lastdeclt = jl_tparam(unw, jl_nparams(unw) - 1);
            int nsp = jl_svec_len(sparams);
            if (nsp > 0) {
                jl_svec_t *env = jl_alloc_svec_uninit(2 * nsp);
                temp = (jl_value_t*)env;
                jl_unionall_t *ua = (jl_unionall_t*)definition->sig;
                for (j = 0; j < nsp; j++) {
                    assert(jl_is_unionall(ua));
                    jl_svecset(env, j * 2, ua->var);
                    jl_svecset(env, j * 2 + 1, jl_svecref(sparams, j));
                    ua = (jl_unionall_t*)ua->body;
                }
                lastdeclt = (jl_value_t*)jl_instantiate_type_with((jl_value_t*)lastdeclt,
                                                                  jl_svec_data(env), nsp);
            }
            jl_svecset(limited, i, lastdeclt);
        }
        *newparams = limited;
        // now there is a problem: the widened signature is more
        // general than just the given arguments, so it might conflict
        // with another definition that doesn't have cache instances yet.
        // to fix this, we insert guard cache entries for all intersections
        // of this signature and definitions. those guard entries will
        // supersede this one in conflicted cases, alerting us that there
        // should actually be a cache miss.
        // TODO: the above analysis assumes that there will never
        // be a call attempted that should throw a no-method error
        JL_GC_POP();
    }
}

// compute whether this type signature is a possible return value from jl_compilation_sig given a concrete-type for `tt`
JL_DLLEXPORT int jl_isa_compileable_sig(
    jl_tupletype_t *type,
    jl_method_t *definition)
{
    jl_value_t *decl = definition->sig;

    if (!jl_is_datatype(type) || jl_has_free_typevars((jl_value_t*)type))
        return 0;

    size_t i, np = jl_nparams(type);
    size_t nargs = definition->nargs; // == jl_field_count(jl_unwrap_unionall(decl));
    if (np == 0)
        return nargs == 0;
    if (jl_is_vararg_type(jl_tparam(type, np - 1))) {
        if (!definition->isva || np <= nargs)
            return 0;
    }
    else if (definition->isva ? np != nargs : np < nargs) {
        return 0;
    }

    if (definition->generator) {
        // staged functions aren't optimized
        // so assume the caller was intelligent about calling us
        return type->isdispatchtuple;
    }

    for (i = 0; i < np; i++) {
        jl_value_t *elt = jl_tparam(type, i);
        jl_value_t *decl_i = jl_nth_slot_type((jl_value_t*)decl, i);
        size_t i_arg = (i < nargs - 1 ? i : nargs - 1);

        if (jl_is_vararg_type(elt)) { // varargs are always considered compilable
            if (!jl_has_free_typevars(elt))
                continue;
            return 0;
        }

        if (i_arg > 0 && i_arg <= sizeof(definition->nospecialize) * 8 &&
                (definition->nospecialize & (1 << (i_arg - 1)))) {
            if (!jl_has_free_typevars(decl_i) && !jl_is_kind(decl_i)) {
                if (jl_egal(elt, decl_i))
                    continue;
                return 0;
            }
        }

        if (jl_is_kind(elt)) {
            // kind slots always get guard entries (checking for subtypes of Type)
            if (decl_i == elt || jl_subtype((jl_value_t*)jl_type_type, decl_i))
                continue;
            // TODO: other code paths that could reach here
            return 0;
        }

        if (jl_is_type_type(elt)) {
            // if the declared type was not Any or Union{Type, ...},
            // then the match must been with kind, such as UnionAll or DataType,
            // and the result of matching the type signature
            // needs to be corrected to the concrete type 'kind'
            jl_value_t *kind = jl_typeof(jl_tparam0(elt));
            if (kind != (jl_value_t*)jl_tvar_type && jl_subtype(kind, decl_i)) {
                if (!jl_subtype((jl_value_t*)jl_type_type, decl_i))
                    return 0;
            }

            if (very_general_type(decl_i)) {
                /*
                  here's a fairly simple heuristic: if this argument slot's
                  declared type is general (Type or Any),
                  then don't specialize for every Type that got passed.

                  Since every type x has its own type Type{x}, this would be
                  excessive specialization for an Any slot.

                  This may require guard entries due to other potential matches.
                  In particular, TypeConstructors are problematic because they can
                  be alternate representations of any type. Extensionally, TC == TC.body,
                  but typeof(TC) != typeof(TC.body). This creates an ambiguity:
                  Type{TC} is type-equal to Type{TC.body}, yet a slot
                  x::TypeConstructor matches the first but not the second, while
                  also matching all other TypeConstructors. This means neither
                  Type{TC} nor TypeConstructor is more specific.
                */
                if (elt != (jl_value_t*)jl_typetype_type)
                    return 0;
            }
            else if (jl_is_type_type(jl_tparam0(elt)) &&
                     // give up on specializing static parameters for Type{Type{Type{...}}}
                     (jl_is_type_type(jl_tparam0(jl_tparam0(elt))) || !jl_has_free_typevars(decl_i))) {
                /*
                  actual argument was Type{...}, we computed its type as
                  Type{Type{...}}. we must avoid unbounded nesting here, so
                  cache the signature as Type{T}, unless something more
                  specific like Type{Type{Int32}} was actually declared.
                  this can be determined using a type intersection.
                */
                if (i < nargs || !definition->isva) {
                    jl_value_t *di = jl_type_intersection(decl_i, (jl_value_t*)jl_typetype_type);
                    JL_GC_PUSH1(&di);
                    assert(di != (jl_value_t*)jl_bottom_type);
                    if (jl_is_kind(di)) {
                        JL_GC_POP();
                        return 0;
                    }
                    else if (!jl_subtype(di, elt) || !jl_subtype(elt, di)) {
                        JL_GC_POP();
                        return 0;
                    }
                    JL_GC_POP();
                }
                else {
                    return 0;
                }
            }
            continue;
        }

        int notcalled_func = (i_arg > 0 && i_arg <= 8 && !(definition->called & (1 << (i_arg - 1))) &&
                              jl_subtype(elt, (jl_value_t*)jl_function_type));
        if (notcalled_func && (decl_i == (jl_value_t*)jl_any_type ||
                               decl_i == (jl_value_t*)jl_function_type ||
                               (jl_is_uniontype(decl_i) && // Base.Callable
                                ((((jl_uniontype_t*)decl_i)->a == (jl_value_t*)jl_function_type &&
                                  ((jl_uniontype_t*)decl_i)->b == (jl_value_t*)jl_type_type) ||
                                 (((jl_uniontype_t*)decl_i)->b == (jl_value_t*)jl_function_type &&
                                  ((jl_uniontype_t*)decl_i)->a == (jl_value_t*)jl_type_type))))) {
            // and attempt to despecialize types marked Function, Callable, or Any
            // when called with a subtype of Function but is not called
            if (elt == (jl_value_t*)jl_function_type)
                continue;
            return 0;
        }

        if (!jl_is_concrete_type(elt))
            return 0;
    }
    return 1;
}

static jl_method_instance_t *cache_method(
        jl_methtable_t *mt, union jl_typemap_t *cache, jl_value_t *parent,
        jl_tupletype_t *tt, // the original tupletype of the signature
        jl_method_t *definition,
        size_t world,
        jl_svec_t *sparams,
        int allow_exec)
{
    // caller must hold the mt->writelock
    // short-circuit (now that we hold the lock) if this entry is already present
    jl_typemap_entry_t *entry = jl_typemap_assoc_by_type(*cache, (jl_value_t*)tt, NULL, /*subtype*/1, jl_cachearg_offset(mt), world, /*max_world_mask*/0);
    if (entry && entry->func.value)
        return (jl_method_instance_t*)entry->func.value;

    jl_value_t *temp = NULL;
    jl_value_t *temp2 = NULL;
    jl_value_t *temp3 = NULL;
    jl_method_instance_t *newmeth = NULL;
    jl_svec_t *newparams = NULL;
    JL_GC_PUSH5(&temp, &temp2, &temp3, &newmeth, &newparams);

    int cache_with_orig = 1;
    jl_tupletype_t *compilationsig = tt;
    intptr_t nspec = (mt == jl_type_type_mt ? definition->nargs + 1 : mt->max_args + 2);
    jl_compilation_sig(tt, sparams, definition, nspec, &newparams);
    if (newparams) {
        cache_with_orig = 0;
        compilationsig = jl_apply_tuple_type(newparams);
        temp2 = (jl_value_t*)compilationsig;
    }
    newmeth = jl_specializations_get_linfo(definition, (jl_value_t*)compilationsig, sparams, world);

    jl_tupletype_t *cachett = tt;
    jl_svec_t* guardsigs = jl_emptysvec;
    size_t min_valid = definition->min_world;
    size_t max_valid = ~(size_t)0;
    if (!cache_with_orig) {
        // now examine what will happen if we chose to use this sig in the cache
        temp = ml_matches(mt->defs, 0, compilationsig, -1, 0, world, &min_valid, &max_valid); // TODO: use MAX_UNSPECIALIZED_CONFLICTS?
        int guards = 0;
        if (temp == jl_false) {
            cache_with_orig = 1;
        }
        else {
            int unmatched_tvars = 0;
            size_t i, l = jl_array_len(temp);
            for (i = 0; i < l; i++) {
                jl_value_t *m = jl_array_ptr_ref(temp, i);
                jl_value_t *env = jl_svecref(m, 1);
                int k, l;
                for (k = 0, l = jl_svec_len(env); k < l; k++) {
                    if (jl_is_typevar(jl_svecref(env, k))) {
                        unmatched_tvars = 1;
                        break;
                    }
                }
                if (unmatched_tvars || guards > MAX_UNSPECIALIZED_CONFLICTS) {
                    // if distinguishing a guard entry from the generalized signature
                    // would require matching type vars then bail out, since the
                    // method cache matching algorithm cannot do that.
                    //
                    // also bail if this requires too many guard entries
                    cache_with_orig = 1;
                    break;
                }
                if (((jl_method_t*)jl_svecref(m, 2)) != definition) {
                    guards++;
                }
            }
        }
        if (!cache_with_orig && guards > 0) {
            // use guard entries as placeholders to prevent this cached method
            // from matching when another more specific definition also exists
            size_t i, l;
            guardsigs = jl_alloc_svec(guards);
            temp3 = (jl_value_t*)guardsigs;
            guards = 0;
            for (i = 0, l = jl_array_len(temp); i < l; i++) {
                jl_value_t *m = jl_array_ptr_ref(temp, i);
                jl_method_t *other = (jl_method_t*)jl_svecref(m, 2);
                if (other != definition) {
                    jl_svecset(guardsigs, guards, (jl_tupletype_t*)jl_svecref(m, 0));
                    guards++;
                    // alternative approach: insert sentinel entry
                    //jl_typemap_insert(cache, parent, (jl_tupletype_t*)jl_svecref(m, 0),
                    //        NULL, jl_emptysvec, /*guard*/NULL, jl_cachearg_offset(mt), &lambda_cache, other->min_world, other->max_world, NULL);
                }
            }
        }
        if (!cache_with_orig) {
            // determined above that there's no ambiguity in also using compilationsig as the cacheablesig
            cachett = compilationsig;
        }
    }

    // here we infer types and specialize the method
    if (newmeth->min_world > min_valid)
        min_valid = newmeth->min_world;
    if (newmeth->max_world < max_valid)
        max_valid = newmeth->max_world;

    // now scan `cachett` and ensure that `Type{T}` in the cache will be matched exactly by `typeof(T)`
    // and also reduce the complexity of rejecting this entry in the cache
    // by replacing non-simple types with jl_any_type to build a new `type`
    // (for example, if the signature contains jl_function_type)
    // TODO: this is also related to how we should handle partial matches
    //       (which currently might miss detection of a MethodError)
    jl_tupletype_t *simplett = NULL;
    size_t i, np = jl_nparams(cachett);
    newparams = NULL;
    for (i = 0; i < np; i++) {
        jl_value_t *elt = jl_svecref(cachett->parameters, i);
        if (jl_is_vararg_type(elt)) {
        }
        else if (jl_is_type_type(elt)) {
            // TODO: if (!jl_is_singleton(elt)) ...
            jl_value_t *kind = jl_typeof(jl_tparam0(elt));
            if (!newparams) newparams = jl_svec_copy(cachett->parameters);
            jl_svecset(newparams, i, kind);
        }
        else if (!jl_is_concrete_type(elt)) { // for example, jl_function_type or jl_tuple_type
            if (!newparams) newparams = jl_svec_copy(cachett->parameters);
            jl_svecset(newparams, i, jl_any_type);
        }
    }
    if (newparams) {
        simplett = jl_apply_tuple_type(newparams);
        temp2 = (jl_value_t*)simplett;
    }

    // TODO: short-circuit if this exact entry is already present
    // to avoid adding a new duplicate copy of it
    // (need to verify that simplett and guardsigs are actually a full match first though)
    //if (cachett != tt) {
    //    jl_typemap_entry_t *entry = jl_typemap_assoc_by_type(*cache, (jl_value_t*)cachett, NULL, /*subtype*/0, jl_cachearg_offset(mt), world, /*max_world_mask*/0);
    //    if (entry && entry->func.value)
    //        return (jl_method_instance_t*)entry->func.value;
    //}

    jl_typemap_insert(cache, parent, cachett, simplett, guardsigs,
            (jl_value_t*)newmeth, jl_cachearg_offset(mt), &lambda_cache,
            min_valid, max_valid, NULL);

    if (definition->traced && jl_method_tracer && allow_exec)
        jl_call_tracer(jl_method_tracer, (jl_value_t*)newmeth);
    JL_GC_POP();
    return newmeth;
}

static jl_method_instance_t *jl_mt_assoc_by_type(jl_methtable_t *mt, jl_datatype_t *tt, int mt_cache, int allow_exec, size_t world)
{
    // caller must hold the mt->writelock
    jl_typemap_entry_t *entry = NULL;
    entry = jl_typemap_assoc_by_type(mt->cache, (jl_value_t*)tt, NULL, /*subtype*/1, jl_cachearg_offset(mt), world, /*max_world_mask*/0);
    if (entry && entry->func.value) {
        assert(entry->func.linfo->min_world <= entry->min_world && entry->func.linfo->max_world >= entry->max_world &&
                "typemap consistency error: MethodInstance doesn't apply to full range of its entry");
        return entry->func.linfo;
    }

    jl_method_instance_t *nf = NULL;
    jl_svec_t *env = jl_emptysvec;
    jl_svec_t *newparams = NULL;
    JL_GC_PUSH3(&tt, &env, &newparams);
    entry = jl_typemap_assoc_by_type(mt->defs, (jl_value_t*)tt, &env, /*subtype*/1, /*offs*/0, world, /*max_world_mask*/0);
    if (entry != NULL) {
        jl_method_t *m = entry->func.method;
        if (!jl_has_call_ambiguities((jl_value_t*)tt, m)) {
            if (jl_options.trace_compile) {
                if (!jl_has_free_typevars((jl_value_t*)tt)) {
                    jl_printf(JL_STDERR, "precompile(");
                    jl_static_show(JL_STDERR, (jl_value_t*)tt);
                    jl_printf(JL_STDERR, ")\n");
                }
            }
            if (!mt_cache) {
                intptr_t nspec = (mt == jl_type_type_mt ? m->nargs + 1 : mt->max_args + 2);
                jl_compilation_sig(tt, env, m, nspec, &newparams);
                if (newparams)
                    tt = jl_apply_tuple_type(newparams);
                nf = jl_specializations_get_linfo(m, (jl_value_t*)tt, env, world);
                assert(nf->min_world <= world && nf->max_world >= world);
            }
            else {
                nf = cache_method(mt, &mt->cache, (jl_value_t*)mt, tt, m, world, env, allow_exec);
            }
        }
    }
    JL_GC_POP();
    return nf;
}

void print_func_loc(JL_STREAM *s, jl_method_t *m)
{
    long lno = m->line;
    if (lno > 0) {
        char *fname = jl_symbol_name((jl_sym_t*)m->file);
        jl_printf(s, " at %s:%ld", fname, lno);
    }
}

/*
  record ambiguous method priorities

  the relative priority of A and B is ambiguous if
  !subtype(A,B) && !subtype(B,A) && no corresponding tuple
  elements are disjoint.

  for example, (AbstractArray, AbstractMatrix) and (AbstractMatrix, AbstractArray) are ambiguous.
  however, (AbstractArray, AbstractMatrix, Foo) and (AbstractMatrix, AbstractArray, Bar) are fine
  since Foo and Bar are disjoint, so there would be no confusion over
  which one to call.
*/
struct ambiguous_matches_env {
    struct typemap_intersection_env match;
    union jl_typemap_t defs;
    jl_typemap_entry_t *newentry;
    jl_value_t *shadowed;
    int after;
};
const int eager_ambiguity_printing = 0;
static int check_ambiguous_visitor(jl_typemap_entry_t *oldentry, struct typemap_intersection_env *closure0)
{
    struct ambiguous_matches_env *closure = container_of(closure0, struct ambiguous_matches_env, match);
    if (oldentry == closure->newentry) {
        closure->after = 1;
        return 1;
    }
    if (oldentry->max_world < ~(size_t)0)
        return 1;
    union jl_typemap_t map = closure->defs;
    jl_tupletype_t *type = (jl_tupletype_t*)closure->match.type;
    jl_method_t *m = closure->newentry->func.method;
    jl_tupletype_t *sig = oldentry->sig;
    jl_value_t *isect = closure->match.ti;

    // we know type ∩ sig != Union{} and
    // we are assuming that
    //        !jl_type_morespecific(type, sig) [before]
    //     or !jl_type_morespecific(sig, type) [after]
    // based on their sort order in the typemap
    // now we are checking that the reverse is true
    int msp;
    if (closure->match.issubty) {
        assert(closure->after);
        msp = 1;
    }
    else if (closure->after) {
        assert(!jl_subtype((jl_value_t*)sig, (jl_value_t*)type));
        msp = jl_type_morespecific_no_subtype((jl_value_t*)type, (jl_value_t*)sig);
    }
    else {
        if (jl_subtype((jl_value_t*)sig, (jl_value_t*)type))
            msp = 1;
        else
            msp = jl_type_morespecific_no_subtype((jl_value_t*)sig, (jl_value_t*)type);
    }

    if (!msp) {
        // see if the intersection is covered by another existing method
        // that will resolve the ambiguity (by being more specific than either)
        // (if type-morespecific made a mistake, this also might end up finding
        // that isect == type or isect == sig and return the original match)
        jl_typemap_entry_t *l = jl_typemap_assoc_by_type(
                map, isect, NULL, /*subtype*/0, /*offs*/0,
                closure->newentry->min_world, /*max_world_mask*/0);
        if (l != NULL) // ok, intersection is covered
            return 1;
        jl_method_t *mambig = oldentry->func.method;
        if (m->ambig == jl_nothing) {
            m->ambig = (jl_value_t*) jl_alloc_vec_any(0);
            jl_gc_wb(m, m->ambig);
        }
        if (mambig->ambig == jl_nothing) {
            mambig->ambig = (jl_value_t*) jl_alloc_vec_any(0);
            jl_gc_wb(mambig, mambig->ambig);
        }
        jl_array_ptr_1d_push((jl_array_t*) m->ambig, (jl_value_t*) mambig);
        jl_array_ptr_1d_push((jl_array_t*) mambig->ambig, (jl_value_t*) m);
        if (eager_ambiguity_printing) {
            JL_STREAM *s = JL_STDERR;
            jl_printf(s, "WARNING: New definition \n    ");
            jl_static_show_func_sig(s, (jl_value_t*)type);
            print_func_loc(s, m);
            jl_printf(s, "\nis ambiguous with: \n    ");
            jl_static_show_func_sig(s, (jl_value_t*)sig);
            print_func_loc(s, oldentry->func.method);
            jl_printf(s, ".\nTo fix, define \n    ");
            jl_static_show_func_sig(s, isect);
            jl_printf(s, "\nbefore the new definition.\n");
        }
    }
    if (!msp || closure->after) {
        // record that this method definition is being partially replaced
        // (either with a real definition, or an ambiguity error)
        if (closure->shadowed == NULL) {
            closure->shadowed = oldentry->func.value;
        }
        else if (!jl_is_array(closure->shadowed)) {
            jl_array_t *list = jl_alloc_vec_any(2);
            jl_array_ptr_set(list, 0, closure->shadowed);
            jl_array_ptr_set(list, 1, oldentry->func.value);
            closure->shadowed = (jl_value_t*)list;
        }
        else {
            jl_array_ptr_1d_push((jl_array_t*)closure->shadowed, oldentry->func.value);
        }
    }
    return 1;
}

static jl_value_t *check_ambiguous_matches(union jl_typemap_t defs, jl_typemap_entry_t *newentry, jl_typemap_intersection_visitor_fptr fptr)
{
    jl_tupletype_t *type = newentry->sig;
    jl_tupletype_t *ttypes = (jl_tupletype_t*)jl_unwrap_unionall((jl_value_t*)type);
    size_t l = jl_nparams(ttypes);
    jl_value_t *va = NULL;
    if (l > 0) {
        va = jl_tparam(ttypes, l - 1);
        if (jl_is_vararg_type(va))
            va = jl_unwrap_vararg(va);
        else
            va = NULL;
    }
    struct ambiguous_matches_env env;
    env.match.fptr = fptr;
    env.match.type = (jl_value_t*)type;
    env.match.va = va;
    env.match.ti = NULL;
    env.match.env = jl_emptysvec;
    env.defs = defs;
    env.newentry = newentry;
    env.shadowed = NULL;
    env.after = 0;
    JL_GC_PUSH3(&env.match.env, &env.match.ti, &env.shadowed);
    jl_typemap_intersection_visitor(defs, 0, &env.match);
    JL_GC_POP();
    return env.shadowed;
}

static int check_disabled_ambiguous_visitor(jl_typemap_entry_t *oldentry, struct typemap_intersection_env *closure0)
{
    struct ambiguous_matches_env *closure = container_of(closure0, struct ambiguous_matches_env, match);
    if (oldentry == closure->newentry) {
        closure->after = 1;
        return 1;
    }
    if (!closure->after || oldentry->max_world < ~(size_t)0)  // the second condition prevents us from confusion in multiple cycles of add/delete
        return 1;
    jl_tupletype_t *sig = oldentry->sig;
    jl_value_t *isect = closure->match.ti;
    if (closure->shadowed == NULL)
        closure->shadowed = (jl_value_t*)jl_alloc_vec_any(0);

    int i, l = jl_array_len(closure->shadowed);
    for (i = 0; i < l; i++) {
        jl_method_t *mth = (jl_method_t*)jl_array_ptr_ref(closure->shadowed, i);
        jl_value_t *isect2 = jl_type_intersection(mth->sig, (jl_value_t*)sig);
        // see if the intersection was covered by precisely the disabled method
        // that means we now need to record the ambiguity
        if (jl_types_equal(isect, isect2)) {
            jl_method_t *mambig = mth;
            jl_method_t *m = oldentry->func.method;
            if (m->ambig == jl_nothing) {
                m->ambig = (jl_value_t*) jl_alloc_vec_any(0);
                jl_gc_wb(m, m->ambig);
            }
            if (mambig->ambig == jl_nothing) {
                mambig->ambig = (jl_value_t*) jl_alloc_vec_any(0);
                jl_gc_wb(mambig, mambig->ambig);
            }
            jl_array_ptr_1d_push((jl_array_t*) m->ambig, (jl_value_t*) mambig);
            jl_array_ptr_1d_push((jl_array_t*) mambig->ambig, (jl_value_t*) m);
        }
    }

    jl_array_ptr_1d_push((jl_array_t*)closure->shadowed, oldentry->func.value);
    return 1;
}


static void method_overwrite(jl_typemap_entry_t *newentry, jl_method_t *oldvalue)
{
    // method overwritten
    jl_method_t *method = (jl_method_t*)newentry->func.method;
    jl_module_t *newmod = method->module;
    jl_module_t *oldmod = oldvalue->module;
    if (jl_options.warn_overwrite == JL_OPTIONS_WARN_OVERWRITE_ON) {
        JL_STREAM *s = JL_STDERR;
        jl_printf(s, "WARNING: Method definition ");
        jl_static_show_func_sig(s, (jl_value_t*)newentry->sig);
        jl_printf(s, " in module %s", jl_symbol_name(oldmod->name));
        print_func_loc(s, oldvalue);
        jl_printf(s, " overwritten");
        if (oldmod != newmod)
            jl_printf(s, " in module %s", jl_symbol_name(newmod->name));
        print_func_loc(s, method);
        jl_printf(s, ".\n");
        jl_uv_flush(s);
    }
}

static void update_max_args(jl_methtable_t *mt, jl_value_t *type)
{
    if (mt == jl_type_type_mt)
        return;
    type = jl_unwrap_unionall(type);
    assert(jl_is_datatype(type));
    size_t na = jl_nparams(type);
    if (jl_va_tuple_kind((jl_datatype_t*)type) == JL_VARARG_UNBOUND)
        na--;
    if (na > mt->max_args)
        mt->max_args = na;
}

static int JL_DEBUG_METHOD_INVALIDATION = 0;

// invalidate cached methods that had an edge to a replaced method
static void invalidate_method_instance(jl_method_instance_t *replaced, size_t max_world, int depth)
{
    if (!jl_is_method(replaced->def.method))
        return;
    JL_LOCK_NOGC(&replaced->def.method->writelock);
    jl_array_t *backedges = replaced->backedges;
    if (replaced->max_world > max_world) {
        // recurse to all backedges to update their valid range also
        assert(replaced->min_world - 1 <= max_world && "attempting to set invalid world constraints");
        if (JL_DEBUG_METHOD_INVALIDATION) {
            int d0 = depth;
            while (d0-- > 0)
                jl_uv_puts(JL_STDOUT, " ", 1);
            jl_static_show(JL_STDOUT, (jl_value_t*)replaced);
            jl_uv_puts(JL_STDOUT, "\n", 1);
        }
        replaced->max_world = max_world;
        update_world_bound(replaced, set_max_world2, max_world);
        if (backedges) {
            size_t i, l = jl_array_len(backedges);
            for (i = 0; i < l; i++) {
                jl_method_instance_t *replaced = (jl_method_instance_t*)jl_array_ptr_ref(backedges, i);
                invalidate_method_instance(replaced, max_world, depth + 1);
            }
        }
    }
    replaced->backedges = NULL;
    JL_UNLOCK_NOGC(&replaced->def.method->writelock);
}

// invalidate cached methods that overlap this definition
struct invalidate_conflicting_env {
    struct typemap_intersection_env match;
    size_t max_world;
    int invalidated;
};
static int invalidate_backedges(jl_typemap_entry_t *oldentry, struct typemap_intersection_env *closure0)
{
    struct invalidate_conflicting_env *closure = container_of(closure0, struct invalidate_conflicting_env, match);
    if (oldentry->max_world > closure->max_world) {
        struct set_world def;
        def.replaced = oldentry->func.linfo;
        def.world = closure->max_world;
        jl_method_t *m = def.replaced->def.method;

        // truncate the max-valid in the invoke cache
        if (m->invokes.unknown != NULL)
            jl_typemap_visitor(m->invokes, set_max_world2, (void*)&def);
        // invalidate mt cache entries
        jl_datatype_t *gf = jl_first_argument_datatype((jl_value_t*)m->sig);
        assert(jl_is_datatype(gf) && gf->name->mt && "method signature invalid?");
        jl_typemap_visitor(gf->name->mt->cache, set_max_world2, (void*)&def);

        // invalidate backedges
        JL_LOCK_NOGC(&def.replaced->def.method->writelock);
        jl_array_t *backedges = def.replaced->backedges;
        if (backedges) {
            size_t i, l = jl_array_len(backedges);
            jl_method_instance_t **replaced = (jl_method_instance_t**)jl_array_data(backedges);
            for (i = 0; i < l; i++) {
                invalidate_method_instance(replaced[i], closure->max_world, 0);
            }
        }
        closure->invalidated = 1;
        def.replaced->backedges = NULL;
        JL_UNLOCK_NOGC(&def.replaced->def.method->writelock);
    }
    return 1;
}

// add a backedge from callee to caller
JL_DLLEXPORT void jl_method_instance_add_backedge(jl_method_instance_t *callee, jl_method_instance_t *caller)
{
    assert(callee->def.method->min_world <= caller->min_world && callee->max_world >= caller->max_world);
    JL_LOCK(&callee->def.method->writelock);
    if (!callee->backedges) {
        // lazy-init the backedges array
        callee->backedges = jl_alloc_vec_any(1);
        jl_gc_wb(callee, callee->backedges);
        jl_array_ptr_set(callee->backedges, 0, caller);
    }
    else {
        size_t i, l = jl_array_len(callee->backedges);
        for (i = 0; i < l; i++) {
            if (jl_array_ptr_ref(callee->backedges, i) == (jl_value_t*)caller)
                break;
        }
        if (i == l) {
            jl_array_ptr_1d_push(callee->backedges, (jl_value_t*)caller);
        }
    }
    JL_UNLOCK(&callee->def.method->writelock);
}

// add a backedge from a non-existent signature to caller
JL_DLLEXPORT void jl_method_table_add_backedge(jl_methtable_t *mt, jl_value_t *typ, jl_value_t *caller)
{
    JL_LOCK(&mt->writelock);
    if (!mt->backedges) {
        // lazy-init the backedges array
        mt->backedges = jl_alloc_vec_any(2);
        jl_gc_wb(mt, mt->backedges);
        jl_array_ptr_set(mt->backedges, 0, typ);
        jl_array_ptr_set(mt->backedges, 1, caller);
    }
    else {
        size_t i, l = jl_array_len(mt->backedges);
        for (i = 1; i < l; i += 2) {
            if (jl_types_equal(jl_array_ptr_ref(mt->backedges, i - 1), typ)) {
                if (jl_array_ptr_ref(mt->backedges, i) == caller) {
                    JL_UNLOCK(&mt->writelock);
                    return;
                }
                // reuse the already cached instance of this type
                typ = jl_array_ptr_ref(mt->backedges, i - 1);
            }
        }
        jl_array_ptr_1d_push(mt->backedges, typ);
        jl_array_ptr_1d_push(mt->backedges, caller);
    }
    JL_UNLOCK(&mt->writelock);
}

void jl_method_instance_delete(jl_method_instance_t *mi)
{
    invalidate_method_instance(mi, mi->min_world - 1, 0);
    if (JL_DEBUG_METHOD_INVALIDATION)
        jl_uv_puts(JL_STDOUT, "<<<\n", 4);
}

static int typemap_search(jl_typemap_entry_t *entry, void *closure)
{
    if ((void*)(entry->func.method) == *(jl_method_t**)closure) {
        *(jl_typemap_entry_t**)closure = entry;
        return 0;
    }
    return 1;
}

JL_DLLEXPORT void jl_method_table_disable(jl_methtable_t *mt, jl_method_t *method)
{
    jl_typemap_entry_t *methodentry = (jl_typemap_entry_t*)(method);
    if (jl_typemap_visitor(mt->defs, typemap_search, &methodentry))
        jl_error("method not in method table");
    JL_LOCK(&mt->writelock);
    // Narrow the world age on the method to make it uncallable
    methodentry->max_world = jl_world_counter++;
    // Recompute ambiguities (deleting a more specific method might reveal ambiguities that it previously resolved)
    check_ambiguous_matches(mt->defs, methodentry, check_disabled_ambiguous_visitor); // TODO: decrease repeated work?
    // Invalidate the backedges
    struct invalidate_conflicting_env env;
    env.invalidated = 0;
    env.max_world = methodentry->max_world;
    jl_typemap_visitor(methodentry->func.method->specializations, (jl_typemap_visitor_fptr)invalidate_backedges, &env);
    JL_UNLOCK(&mt->writelock);
}

JL_DLLEXPORT void jl_method_table_insert(jl_methtable_t *mt, jl_method_t *method, jl_tupletype_t *simpletype)
{
    assert(jl_is_method(method));
    assert(jl_is_mtable(mt));
    jl_value_t *type = method->sig;
    jl_value_t *oldvalue = NULL;
    struct invalidate_conflicting_env env;
    env.invalidated = 0;
    env.max_world = method->min_world - 1;
    JL_GC_PUSH1(&oldvalue);
    JL_LOCK(&mt->writelock);
    jl_typemap_entry_t *newentry = jl_typemap_insert(&mt->defs, (jl_value_t*)mt,
            (jl_tupletype_t*)type, simpletype, jl_emptysvec, (jl_value_t*)method, 0, &method_defs,
            method->min_world, ~(size_t)0, &oldvalue);
    if (oldvalue) {
        if (oldvalue == (jl_value_t*)method) {
            // redundant add of same method; no need to do anything
            JL_UNLOCK(&mt->writelock);
            JL_GC_POP();
            return;
        }
        method->ambig = ((jl_method_t*)oldvalue)->ambig;
        method_overwrite(newentry, (jl_method_t*)oldvalue);
    }
    else {
        oldvalue = check_ambiguous_matches(mt->defs, newentry, check_ambiguous_visitor);
        if (mt->backedges) {
            jl_value_t **backedges = (jl_value_t**)jl_array_data(mt->backedges);
            size_t i, na = jl_array_len(mt->backedges);
            size_t ins = 0;
            for (i = 1; i < na; i += 2) {
                jl_value_t *backedgetyp = backedges[i - 1];
                if (!jl_has_empty_intersection(backedgetyp, (jl_value_t*)type)) {
                    jl_method_instance_t *backedge = (jl_method_instance_t*)backedges[i];
                    invalidate_method_instance(backedge, env.max_world, 0);
                    env.invalidated = 1;
                }
                else {
                    backedges[ins++] = backedges[i - 1];
                    backedges[ins++] = backedges[i - 0];
                }
            }
            if (ins == 0)
                mt->backedges = NULL;
            else
                jl_array_del_end(mt->backedges, na - ins);
        }
    }
    if (oldvalue) {
        jl_datatype_t *unw = (jl_datatype_t*)jl_unwrap_unionall(type);
        size_t l = jl_svec_len(unw->parameters);
        jl_value_t *va = NULL;
        if (l > 0) {
            va = jl_tparam(unw, l - 1);
            if (jl_is_vararg_type(va))
                va = jl_unwrap_vararg(va);
            else
                va = NULL;
        }
        env.match.va = va;
        env.match.type = (jl_value_t*)type;
        env.match.fptr = invalidate_backedges;
        env.match.env = NULL;

        if (jl_is_method(oldvalue)) {
            jl_typemap_intersection_visitor(((jl_method_t*)oldvalue)->specializations, 0, &env.match);
        }
        else {
            assert(jl_is_array(oldvalue));
            jl_method_t **d = (jl_method_t**)jl_array_ptr_data(oldvalue);
            size_t i, n = jl_array_len(oldvalue);
            for (i = 0; i < n; i++) {
                jl_typemap_intersection_visitor(d[i]->specializations, 0, &env.match);
            }
        }
    }
    if (env.invalidated && JL_DEBUG_METHOD_INVALIDATION) {
        jl_uv_puts(JL_STDOUT, ">> ", 3);
        jl_static_show(JL_STDOUT, (jl_value_t*)method);
        jl_uv_puts(JL_STDOUT, " ", 1);
        jl_static_show(JL_STDOUT, (jl_value_t*)type);
        jl_uv_puts(JL_STDOUT, "\n", 1);
    }
    update_max_args(mt, type);
    JL_UNLOCK(&mt->writelock);
    JL_GC_POP();
}

void JL_NORETURN jl_method_error_bare(jl_function_t *f, jl_value_t *args, size_t world)
{
    if (jl_methoderror_type) {
        jl_value_t *e = jl_new_struct_uninit(jl_methoderror_type);
        struct jl_method_error {
            jl_value_t *f;
            jl_value_t *args;
            size_t world;
        } *pe = (struct jl_method_error*)e,
           ee = {f, args, world};
        *pe = ee;
        jl_throw(e);
    }
    else {
        jl_printf((JL_STREAM*)STDERR_FILENO, "A method error occurred before the base MethodError type was defined. Aborting...\n");
        jl_static_show((JL_STREAM*)STDERR_FILENO,(jl_value_t*)f); jl_printf((JL_STREAM*)STDERR_FILENO," world %u\n", (unsigned)world);
        jl_static_show((JL_STREAM*)STDERR_FILENO,args); jl_printf((JL_STREAM*)STDERR_FILENO,"\n");
        jl_ptls_t ptls = jl_get_ptls_states();
        ptls->bt_size = rec_backtrace(ptls->bt_data, JL_MAX_BT_SIZE);
        jl_critical_error(0, NULL, ptls->bt_data, &ptls->bt_size);
        abort();
    }
    // not reached
}

void JL_NORETURN jl_method_error(jl_function_t *f, jl_value_t **args, size_t na, size_t world)
{
    jl_value_t *argtup = jl_f_tuple(NULL, args+1, na-1);
    JL_GC_PUSH1(&argtup);
    jl_method_error_bare(f, argtup, world);
    // not reached
}

jl_tupletype_t *arg_type_tuple(jl_value_t **args, size_t nargs)
{
    jl_tupletype_t *tt;
    size_t i;
    if (nargs * sizeof(jl_value_t*) < jl_page_size) {
        jl_value_t **types;
        JL_GC_PUSHARGS(types, nargs);
        for (i = 0; i < nargs; i++) {
            jl_value_t *ai = args[i];
            if (jl_is_type(ai))
                types[i] = (jl_value_t*)jl_wrap_Type(ai);
            else
                types[i] = jl_typeof(ai);
        }
        // if `ai` has free type vars this will not be a valid (concrete) type.
        // TODO: it would be really nice to only dispatch and cache those as
        // `jl_typeof(ai)`, but that will require some redesign of the caching
        // logic.
        tt = jl_apply_tuple_type_v(types, nargs);
        JL_GC_POP();
    }
    else {
        jl_svec_t *types = jl_alloc_svec(nargs);
        JL_GC_PUSH1(&types);
        for (i = 0; i < nargs; i++) {
            jl_value_t *ai = args[i];
            if (jl_is_type(ai))
                jl_svecset(types, i, (jl_value_t*)jl_wrap_Type(ai));
            else
                jl_svecset(types, i, jl_typeof(ai));
        }
        tt = jl_apply_tuple_type(types);
        JL_GC_POP();
    }
    return tt;
}

static jl_method_instance_t *jl_method_lookup_by_type(
        jl_methtable_t *mt, jl_tupletype_t *types,
        int cache, int allow_exec, size_t world)
{
    jl_typemap_entry_t *entry = jl_typemap_assoc_by_type(mt->cache, (jl_value_t*)types, NULL, /*subtype*/1, jl_cachearg_offset(mt), world, /*max_world_mask*/0);
    if (entry) {
        jl_method_instance_t *linfo = (jl_method_instance_t*)entry->func.value;
        assert(linfo->min_world <= entry->min_world && linfo->max_world >= entry->max_world &&
                "typemap consistency error: MethodInstance doesn't apply to full range of its entry");
        return linfo;
    }
    JL_LOCK(&mt->writelock);
    if (jl_is_datatype((jl_value_t*)types) && types->isdispatchtuple)
        cache = 1;
    jl_method_instance_t *sf = jl_mt_assoc_by_type(mt, types, cache, allow_exec, world);
    JL_UNLOCK(&mt->writelock);
    return sf;
}

JL_DLLEXPORT int jl_method_exists(jl_methtable_t *mt, jl_tupletype_t *types, size_t world)
{
    return jl_method_lookup_by_type(mt, types, /*cache*/0, /*allow_exec*/1, world) != NULL;
}

jl_method_instance_t *jl_method_lookup(jl_methtable_t *mt, jl_value_t **args, size_t nargs, int cache, size_t world)
{
    jl_typemap_entry_t *entry = jl_typemap_assoc_exact(mt->cache, args, nargs, jl_cachearg_offset(mt), world);
    if (entry)
        return entry->func.linfo;
    JL_LOCK(&mt->writelock);
    jl_tupletype_t *tt = arg_type_tuple(args, nargs);
    JL_GC_PUSH1(&tt);
    jl_method_instance_t *sf = jl_mt_assoc_by_type(mt, tt, cache, 1, world);
    JL_GC_POP();
    JL_UNLOCK(&mt->writelock);
    return sf;
}

// return a Vector{Any} of svecs, each describing a method match:
// Any[svec(tt, spvals, m), ...]
// tt is the intersection of the type argument and the method signature,
// spvals is any matched static parameter values, m is the Method,
//
// lim is the max # of methods to return. if there are more, returns jl_false.
// -1 for no limit.
JL_DLLEXPORT jl_value_t *jl_matching_methods(jl_tupletype_t *types, int lim, int include_ambiguous,
                                             size_t world, size_t *min_valid, size_t *max_valid)
{
    jl_value_t *unw = jl_unwrap_unionall((jl_value_t*)types);
    if (jl_is_tuple_type(unw) && jl_tparam0(unw) == jl_bottom_type)
        return (jl_value_t*)jl_alloc_vec_any(0);
    jl_datatype_t *dt = jl_first_argument_datatype(unw);
    if (dt == NULL || !jl_is_datatype(dt))
        return jl_false; // indeterminate - ml_matches can't deal with this case
    jl_methtable_t *mt = dt->name->mt;
    if (mt == NULL)
        return (jl_value_t*)jl_alloc_vec_any(0);
    return ml_matches(mt->defs, 0, types, lim, include_ambiguous, world, min_valid, max_valid);
}

jl_callptr_t jl_compile_method_internal(jl_method_instance_t **pli, size_t world)
{
    jl_method_instance_t *li = *pli;
    jl_callptr_t fptr = li->invoke;
    if (fptr != jl_fptr_trampoline)
        return fptr;

   if (jl_options.compile_enabled == JL_OPTIONS_COMPILE_OFF ||
            jl_options.compile_enabled == JL_OPTIONS_COMPILE_MIN) {
        // copy fptr from the template method definition
        jl_method_t *def = li->def.method;
        if (jl_is_method(def) && def->unspecialized) {
            jl_method_instance_t *unspec = def->unspecialized;
            if (unspec->invoke != jl_fptr_trampoline) {
                li->functionObjectsDecls.functionObject = NULL;
                li->functionObjectsDecls.specFunctionObject = NULL;
                li->specptr = unspec->specptr;
                li->inferred = unspec->inferred;
                if (li->inferred)
                    jl_gc_wb(li, li->inferred);
                li->inferred_const = unspec->inferred_const;
                if (li->inferred_const)
                    jl_gc_wb(li, li->inferred_const);
                li->invoke = unspec->invoke;
                return fptr;
            }
        }
        jl_code_info_t *src = jl_code_for_interpreter(li);
        if (!jl_code_requires_compiler(src)) {
            li->inferred = (jl_value_t*)src;
            jl_gc_wb(li, src);
            li->functionObjectsDecls.functionObject = NULL;
            li->functionObjectsDecls.specFunctionObject = NULL;
            li->invoke = jl_fptr_interpret_call;
            return jl_fptr_interpret_call;
        }
        if (jl_options.compile_enabled == JL_OPTIONS_COMPILE_OFF) {
            jl_printf(JL_STDERR, "code missing for ");
            jl_static_show(JL_STDERR, (jl_value_t*)li);
            jl_printf(JL_STDERR, " : sysimg may not have been built with --compile=all\n");
        }
    }

    jl_llvm_functions_t decls = li->functionObjectsDecls;
    if (decls.functionObject == NULL) {
        // if we don't have decls already, try to generate it now
        jl_code_info_t *src = NULL;
        if (jl_is_method(li->def.method) && !jl_is_rettype_inferred(li) &&
                 jl_symbol_name(li->def.method->name)[0] != '@') {
            // don't bother with typeinf on macros or toplevel thunks
            // but try to infer everything else
            src = jl_type_infer(pli, world, 0);
            li = *pli;
        }

        // check again, because jl_type_infer may have changed li or compiled it
        fptr = li->invoke;
        if (fptr != jl_fptr_trampoline)
            return fptr;
        decls = li->functionObjectsDecls;
        if (decls.functionObject == NULL) {
            decls = jl_compile_linfo(pli, src, world, &jl_default_cgparams);
            li = *pli;
        }

        // check again, of course
        fptr = li->invoke;
        if (fptr != jl_fptr_trampoline)
            return fptr;

        // if it's not inferred (inference failed / disabled), try to just use the unspecialized fptr
        if (!li->inferred) {
            if (jl_is_method(li->def.method) && li->def.method->unspecialized) {
                jl_method_instance_t *unspec = li->def.method->unspecialized;
                fptr = unspec->invoke;
                if (fptr != jl_fptr_trampoline) {
                    li->specptr = unspec->specptr;
                    li->inferred_const = unspec->inferred_const;
                    if (li->inferred_const)
                        jl_gc_wb(li, li->inferred_const);
                    li->invoke = fptr;
                    return fptr;
                }
            }
        }
    }

    // ask codegen to make the fptr
    fptr = jl_generate_fptr(pli, decls, world);
    return fptr;
}

JL_DLLEXPORT jl_value_t *jl_fptr_trampoline(jl_method_instance_t *m, jl_value_t **args, uint32_t nargs)
{
    size_t world = jl_get_ptls_states()->world_age;
    jl_callptr_t fptr = jl_compile_method_internal(&m, world);
    return fptr(m, args, nargs);
}

JL_DLLEXPORT jl_value_t *jl_fptr_const_return(jl_method_instance_t *m, jl_value_t **args, uint32_t nargs)
{
    return m->inferred_const;
}

JL_DLLEXPORT jl_value_t *jl_fptr_args(jl_method_instance_t *m, jl_value_t **args, uint32_t nargs)
{
    return m->specptr.fptr1(args[0], &args[1], nargs - 1);
}

JL_DLLEXPORT jl_value_t *jl_fptr_sparam(jl_method_instance_t *m, jl_value_t **args, uint32_t nargs)
{
    return m->specptr.fptr3(m->sparam_vals, args[0], &args[1], nargs - 1);
}

// Return the index of the invoke api, if known
JL_DLLEXPORT int32_t jl_invoke_api(jl_method_instance_t *mi)
{
    jl_callptr_t f = mi->invoke;
    if (f == &jl_fptr_trampoline)
        return 0;
    if (f == &jl_fptr_args)
        return 1;
    if (f == &jl_fptr_const_return)
        return 2;
    if (f == &jl_fptr_sparam)
        return 3;
    if (f == &jl_fptr_interpret_call)
        return 4;
    return -1;
}

// compile-time method lookup
jl_method_instance_t *jl_get_specialization1(jl_tupletype_t *types, size_t world, int mt_cache)
{
    JL_TIMING(METHOD_LOOKUP_COMPILE);
    if (jl_has_free_typevars((jl_value_t*)types))
        return NULL; // don't poison the cache due to a malformed query
    if (!jl_has_concrete_subtype((jl_value_t*)types))
        return NULL;

    // find if exactly 1 method matches (issue #7302)
    size_t min_valid = 0;
    size_t max_valid = ~(size_t)0;
    jl_value_t *matches = jl_matching_methods(types, 1, 1, world, &min_valid, &max_valid);
    if (matches == jl_false || jl_array_len(matches) != 1)
        return NULL;
    jl_tupletype_t *tt = NULL;
    jl_svec_t *newparams = NULL;
    JL_GC_PUSH3(&matches, &tt, &newparams);
    jl_svec_t *match = (jl_svec_t*)jl_array_ptr_ref(matches, 0);
    jl_method_t *m = (jl_method_t*)jl_svecref(match, 2);
    jl_svec_t *env = (jl_svec_t*)jl_svecref(match, 1);
    jl_tupletype_t *ti = (jl_tupletype_t*)jl_svecref(match, 0);
    jl_method_instance_t *nf = NULL;
    if (jl_is_datatype(ti)) {
        jl_datatype_t *dt = jl_first_argument_datatype((jl_value_t*)ti);
        if (dt && jl_is_datatype(dt)) {
            // get the specialization without caching it
            jl_methtable_t *mt = dt->name->mt;
            if (mt_cache && ((jl_datatype_t*)ti)->isdispatchtuple) {
                // Since we also use this presence in the cache
                // to trigger compilation when producing `.ji` files,
                // inject it there now if we think it will be
                // used via dispatch later (e.g. because it was hinted via a call to `precompile`)
                jl_methtable_t *mt = dt->name->mt;
                JL_LOCK(&mt->writelock);
                nf = cache_method(mt, &mt->cache, (jl_value_t*)mt, ti, m, world, env, /*allow_exec*/1);
                JL_UNLOCK(&mt->writelock);
                assert(nf->min_world <= world && nf->max_world >= world);
            }
            else {
                intptr_t nspec = (mt == jl_type_type_mt ? m->nargs + 1 : mt->max_args + 2);
                jl_compilation_sig(ti, env, m, nspec, &newparams);
                tt = (newparams ? jl_apply_tuple_type(newparams) : ti);
                int is_compileable = ((jl_datatype_t*)ti)->isdispatchtuple ||
                    jl_isa_compileable_sig(tt, m);
                if (is_compileable) {
                    nf = jl_specializations_get_linfo(m, (jl_value_t*)tt, env, world);
                    assert(nf->min_world <= world && nf->max_world >= world);
                }
            }
        }
    }
    JL_GC_POP();
    return nf;
}

JL_DLLEXPORT int jl_compile_hint(jl_tupletype_t *types)
{
    size_t world = jl_world_counter;
    jl_method_instance_t *li = jl_get_specialization1(types, world, 1);
    if (li == NULL)
        return 0;
    if (jl_generating_output()) {
        jl_code_info_t *src = NULL;
        // If we are saving ji files (e.g. package pre-compilation or intermediate sysimg build steps),
        // don't bother generating anything since it won't be saved.
        if (!jl_is_rettype_inferred(li))
            src = jl_type_infer(&li, world, 0);
        if (li->invoke != jl_fptr_const_return) {
            if (jl_options.outputo || jl_options.outputbc || jl_options.outputunoptbc) {
                // If we are saving LLVM or native code, generate the LLVM IR so that it'll
                // be included in the saved LLVM module.
                jl_compile_linfo(&li, src, world, &jl_default_cgparams);
                if (jl_typeinf_func && li->def.method->module == ((jl_datatype_t*)jl_typeof(jl_typeinf_func))->name->module) {
                    size_t world = jl_typeinf_world;
                    // if it's part of the compiler, also attempt to compile for the compiler world too
                    jl_method_instance_t *li = jl_get_specialization1(types, world, 1);
                    if (li != NULL)
                        jl_compile_linfo(&li, NULL, world, &jl_default_cgparams);
                }
            }
        }
        // In addition to full compilation of the compilation-signature, if `types` is more specific (e.g. due to nospecialize),
        // also run inference now on the original `types`, since that may help us guide inference to find
        // additional useful methods that should be compiled
        //ALT: if (jl_is_datatype(types) && ((jl_datatype_t*)types)->isdispatchtuple && !jl_egal(li->specTypes, types))
        //ALT: if (jl_subtype(types, li->specTypes))
        if (!jl_subtype(li->specTypes, (jl_value_t*)types)) {
            jl_svec_t *tpenv2 = jl_emptysvec;
            jl_value_t *types2 = NULL;
            JL_GC_PUSH2(&tpenv2, &types2);
            types2 = jl_type_intersection_env((jl_value_t*)types, (jl_value_t*)li->def.method->sig, &tpenv2);
            jl_method_instance_t *li2 = jl_specializations_get_linfo(li->def.method, (jl_value_t*)types2, tpenv2, world);
            JL_GC_POP();
            if (!jl_is_rettype_inferred(li2))
                (void)jl_type_infer(&li2, world, 0);
        }
    }
    else {
        // Otherwise (this branch), assuming we are at runtime (normal JIT) and
        // we should generate the native code immediately in preparation for use.
        (void)jl_compile_method_internal(&li, world);
    }
    return 1;
}

JL_DLLEXPORT jl_value_t *jl_get_spec_lambda(jl_tupletype_t *types, size_t world)
{
    jl_method_instance_t *li = jl_get_specialization1(types, world, 0);
    if (!li || jl_has_call_ambiguities((jl_value_t*)types, li->def.method))
        return jl_nothing;
    return (jl_value_t*)li;
}

// see if a call to m with computed from `types` is ambiguous
JL_DLLEXPORT int jl_is_call_ambiguous(jl_value_t *types, jl_method_t *m)
{
    if (m->ambig == jl_nothing)
        return 0;
    for (size_t i = 0; i < jl_array_len(m->ambig); i++) {
        jl_method_t *mambig = (jl_method_t*)jl_array_ptr_ref(m->ambig, i);
        if (jl_subtype((jl_value_t*)types, (jl_value_t*)mambig->sig))
            return 1;
    }
    return 0;
}

// see if a call to m with a subtype of `types` might be ambiguous
// if types is from a call signature (isdispatchtuple), this is the same as jl_is_call_ambiguous above
JL_DLLEXPORT int jl_has_call_ambiguities(jl_value_t *types, jl_method_t *m)
{
    if (m->ambig == jl_nothing)
        return 0;
    for (size_t i = 0; i < jl_array_len(m->ambig); i++) {
        jl_method_t *mambig = (jl_method_t*)jl_array_ptr_ref(m->ambig, i);
        if (!jl_has_empty_intersection(mambig->sig, types))
            return 1;
    }
    return 0;
}

// add type of `f` to front of argument tuple type
jl_value_t *jl_argtype_with_function(jl_function_t *f, jl_value_t *types0)
{
    jl_value_t *types = jl_unwrap_unionall(types0);
    size_t l = jl_nparams(types);
    jl_value_t *tt = (jl_value_t*)jl_alloc_svec(1+l);
    size_t i;
    JL_GC_PUSH1(&tt);
    if (jl_is_type(f))
        jl_svecset(tt, 0, jl_wrap_Type(f));
    else
        jl_svecset(tt, 0, jl_typeof(f));
    for(i=0; i < l; i++)
        jl_svecset(tt, i+1, jl_tparam(types,i));
    tt = (jl_value_t*)jl_apply_tuple_type((jl_svec_t*)tt);
    tt = jl_rewrap_unionall(tt, types0);
    JL_GC_POP();
    return tt;
}

#ifdef JL_TRACE
static int trace_en = 0;
static int error_en = 1;
static void __attribute__ ((unused)) enable_trace(int x) { trace_en=x; }
static void show_call(jl_value_t *F, jl_value_t **args, uint32_t nargs)
{
    jl_static_show(JL_STDOUT, F);
    jl_printf(JL_STDOUT, "(");
    for(size_t i=0; i < nargs; i++) {
        if (i > 0) jl_printf(JL_STDOUT, ", ");
        jl_static_show(JL_STDOUT, jl_typeof(args[i]));
    }
    jl_printf(JL_STDOUT, ")");
}
#endif

static jl_value_t *verify_type(jl_value_t *v)
{
    assert(jl_typeof(jl_typeof(v)));
    return v;
}

STATIC_INLINE int sig_match_fast(jl_value_t **args, jl_value_t **sig, size_t i, size_t n)
{
    // NOTE: This function is a huge performance hot spot!!
    for (; i < n; i++) {
        jl_value_t *decl = sig[i];
        jl_value_t *a = args[i];
        if ((jl_value_t*)jl_typeof(a) != decl) {
            /*
              we are only matching concrete types here, and those types are
              hash-consed, so pointer comparison should work.
            */
            return 0;
        }
    }
    return 1;
}

jl_typemap_entry_t *call_cache[N_CALL_CACHE];
static uint8_t pick_which[N_CALL_CACHE];
#ifdef JL_GF_PROFILE
size_t ncalls;
void call_cache_stats()
{
    int pick_which_stat[4] = {0, 0, 0, 0};
    int i, count = 0;
    for (i = 0; i < N_CALL_CACHE; i++) {
        if (call_cache[i])
            count++;
    }
    for (i = 0; i < N_CALL_CACHE; i++) {
        ++pick_which_stat[pick_which[i] & 3];
    }
    jl_safe_printf("cache occupied: %d / %d; pick_which stats: {%d, %d, %d, %d}\n",
            count, N_CALL_CACHE,
            pick_which_stat[0], pick_which_stat[1], pick_which_stat[2], pick_which_stat[3]);
}
#endif

STATIC_INLINE jl_method_instance_t *jl_lookup_generic_(jl_value_t **args, uint32_t nargs,
                                                       uint32_t callsite, size_t world)
{
#ifdef JL_GF_PROFILE
    ncalls++;
#endif
#ifdef JL_TRACE
    int traceen = trace_en; //&& ((char*)&mt < jl_stack_hi-6000000);
    if (traceen)
        show_call(args[0], &args[1], nargs-1);
#endif

    /*
      search order:
      check associative hash based on callsite address for leafsig match
      look at concrete signatures
      if there is an exact match, return it
      otherwise look for a matching generic signature
      if no concrete or generic match, raise error
      if no generic match, use the concrete one even if inexact
      otherwise instantiate the generic method and use it
    */
    // compute the entry hashes
    // use different parts of the value
    // so that a collision across all of
    // them is less likely
    uint32_t cache_idx[4] = {
        (callsite) & (N_CALL_CACHE - 1),
        (callsite >> 8) & (N_CALL_CACHE - 1),
        (callsite >> 16) & (N_CALL_CACHE - 1),
        (callsite >> 24 | callsite << 8) & (N_CALL_CACHE - 1)};
    jl_typemap_entry_t *entry = NULL;
    jl_methtable_t *mt = NULL;
    int i;
    // check each cache entry to see if it matches
    for (i = 0; i < 4; i++) {
        entry = call_cache[cache_idx[i]];
        if (entry && nargs == jl_svec_len(entry->sig->parameters) &&
            sig_match_fast(args, jl_svec_data(entry->sig->parameters), 0, nargs) &&
            world >= entry->min_world && world <= entry->max_world) {
            break;
        }
    }
    // if no method was found in the associative cache, check the full cache
    if (i == 4) {
        JL_TIMING(METHOD_LOOKUP_FAST);
        jl_value_t *F = args[0];
        mt = jl_gf_mtable(F);
        entry = jl_typemap_assoc_exact(mt->cache, args, nargs, jl_cachearg_offset(mt), world);
        if (entry && entry->isleafsig && entry->simplesig == (void*)jl_nothing && entry->guardsigs == jl_emptysvec) {
            // put the entry into the cache if it's valid for a leafsig lookup,
            // using pick_which to slightly randomize where it ends up
            call_cache[cache_idx[++pick_which[cache_idx[0]] & 3]] = entry;
        }
    }

    jl_method_instance_t *mfunc = NULL;
    if (entry) {
        mfunc = entry->func.linfo;
    }
    else {
        JL_LOCK(&mt->writelock);
        // cache miss case
        JL_TIMING(METHOD_LOOKUP_SLOW);
        jl_tupletype_t *tt = arg_type_tuple(args, nargs);
        JL_GC_PUSH1(&tt);
        mfunc = jl_mt_assoc_by_type(mt, tt, /*cache*/1, /*allow_exec*/1, world);
        JL_GC_POP();
        JL_UNLOCK(&mt->writelock);
        if (mfunc == NULL) {
#ifdef JL_TRACE
            if (error_en)
                show_call(args[0], args, nargs);
#endif
            jl_method_error((jl_function_t*)args[0], args, nargs, world);
            // unreachable
        }
    }

#ifdef JL_TRACE
    if (traceen)
        jl_printf(JL_STDOUT, " at %s:%d\n", jl_symbol_name(mfunc->def.method->file), mfunc->def.method->line);
#endif
    return mfunc;
}

jl_method_instance_t *jl_lookup_generic(jl_value_t **args, uint32_t nargs, uint32_t callsite,
                                        size_t world)
{
    return jl_lookup_generic_(args, nargs, callsite, world);
}

JL_DLLEXPORT jl_value_t *jl_apply_generic(jl_value_t **args, uint32_t nargs)
{
    jl_method_instance_t *mfunc = jl_lookup_generic_(args, nargs,
                                                     jl_int32hash_fast(jl_return_address()),
                                                     jl_get_ptls_states()->world_age);
    jl_value_t *res = mfunc->invoke(mfunc, args, nargs);
    return verify_type(res);
}

JL_DLLEXPORT jl_value_t *jl_gf_invoke_lookup(jl_value_t *types, size_t world)
{
    jl_methtable_t *mt = jl_first_argument_datatype(types)->name->mt;
    jl_svec_t *env = jl_emptysvec;
    JL_GC_PUSH1(&env);
    jl_typemap_entry_t *entry = jl_typemap_assoc_by_type(
            mt->defs, types, /*env*/&env, /*subtype*/1, /*offs*/0, world, /*max_world_mask*/0);
    JL_GC_POP();
    if (!entry)
        return jl_nothing;
    if (jl_is_call_ambiguous(types, entry->func.method))
        return jl_nothing;
    return (jl_value_t*)entry;
}

// invoke()
// this does method dispatch with a set of types to match other than the
// types of the actual arguments. this means it sometimes does NOT call the
// most specific method for the argument types, so we need different logic.
// first we use the given types to look up a definition, then we perform
// caching and specialization within just that definition.
// every definition has its own private method table for this purpose.
//
// NOTE: assumes argument type is a subtype of the lookup type.
jl_value_t *jl_gf_invoke(jl_value_t *types0, jl_value_t **args, size_t nargs)
{
    size_t world = jl_get_ptls_states()->world_age;
    jl_svec_t *tpenv = jl_emptysvec;
    jl_tupletype_t *tt = NULL;
    jl_value_t *types = NULL;
    JL_GC_PUSH3(&types, &tpenv, &tt);
    jl_value_t *gf = args[0];
    types = jl_argtype_with_function(gf, types0);
    jl_methtable_t *mt = jl_gf_mtable(gf);
    jl_typemap_entry_t *entry = (jl_typemap_entry_t*)jl_gf_invoke_lookup(types, world);

    if ((jl_value_t*)entry == jl_nothing) {
        jl_method_error_bare(gf, types0, world);
        // unreachable
    }

    // now we have found the matching definition.
    // next look for or create a specialization of this definition.

    jl_method_t *method = entry->func.method;
    jl_method_instance_t *mfunc = NULL;
    jl_typemap_entry_t *tm = NULL;
    if (method->invokes.unknown != NULL)
        tm = jl_typemap_assoc_exact(method->invokes, args, nargs, jl_cachearg_offset(mt), world);
    if (tm) {
        mfunc = tm->func.linfo;
    }
    else {
        JL_LOCK(&method->writelock);
        tt = arg_type_tuple(args, nargs);
        if (jl_is_unionall(method->sig)) {
            int sub = jl_subtype_matching((jl_value_t*)tt, (jl_value_t*)method->sig, &tpenv);
            assert(sub); (void)sub;
        }

        if (method->invokes.unknown == NULL)
            method->invokes.unknown = jl_nothing;

        mfunc = cache_method(mt, &method->invokes, entry->func.value, tt, method, world, tpenv, 1);
        JL_UNLOCK(&method->writelock);
    }
    JL_GC_POP();
    return mfunc->invoke(mfunc, args, nargs);
}

JL_DLLEXPORT jl_value_t *jl_get_invoke_lambda(jl_methtable_t *mt,
                                              jl_typemap_entry_t *entry,
                                              jl_value_t *tt,
                                              size_t world)
{
    // TODO: refactor this method to be more like `jl_get_specialization1`
    if (!jl_is_datatype(tt) || !((jl_datatype_t*)tt)->isdispatchtuple)
        return jl_nothing;

    jl_method_t *method = entry->func.method;
    jl_typemap_entry_t *tm = NULL;
    if (method->invokes.unknown != NULL) {
        tm = jl_typemap_assoc_by_type(method->invokes, tt, NULL, /*subtype*/1,
                                      jl_cachearg_offset(mt), world, /*max_world_mask*/0);
        if (tm) {
            return (jl_value_t*)tm->func.linfo;
        }
    }

    JL_LOCK(&method->writelock);
    if (method->invokes.unknown != NULL) {
        tm = jl_typemap_assoc_by_type(method->invokes, tt, NULL, /*subtype*/1,
                                      jl_cachearg_offset(mt), world, /*max_world_mask*/0);
        if (tm) {
            jl_method_instance_t *mfunc = tm->func.linfo;
            JL_UNLOCK(&method->writelock);
            return (jl_value_t*)mfunc;
        }
    }
    jl_svec_t *tpenv = jl_emptysvec;
    JL_GC_PUSH1(&tpenv);
    if (jl_is_unionall(entry->sig)) {
        jl_value_t *ti =
            jl_type_intersection_env(tt, (jl_value_t*)entry->sig, &tpenv);
        assert(ti != (jl_value_t*)jl_bottom_type);
        (void)ti;
    }

    if (method->invokes.unknown == NULL)
        method->invokes.unknown = jl_nothing;

    jl_method_instance_t *mfunc = cache_method(mt, &method->invokes, entry->func.value,
                                               (jl_tupletype_t*)tt, method, world, tpenv, 1);
    JL_GC_POP();
    JL_UNLOCK(&method->writelock);
    return (jl_value_t*)mfunc;
}

// Return value is rooted globally
jl_function_t *jl_new_generic_function_with_supertype(jl_sym_t *name, jl_module_t *module, jl_datatype_t *st, int iskw)
{
    // type name is function name prefixed with #
    size_t l = strlen(jl_symbol_name(name));
    char *prefixed;
    if (iskw) {
        prefixed = (char*)malloc(l+5);
        strcpy(&prefixed[0], "#kw#");
        strcpy(&prefixed[4], jl_symbol_name(name));
    }
    else {
        prefixed = (char*)malloc(l+2);
        prefixed[0] = '#';
        strcpy(&prefixed[1], jl_symbol_name(name));
    }
    jl_sym_t *tname = jl_symbol(prefixed);
    free(prefixed);
    jl_datatype_t *ftype = (jl_datatype_t*)jl_new_datatype(
            tname, module, st, jl_emptysvec, jl_emptysvec, jl_emptysvec, 0, 0, 0);
    assert(jl_is_datatype(ftype));
    JL_GC_PUSH1(&ftype);
    ftype->name->mt->name = name;
    jl_gc_wb(ftype->name->mt, name);
    jl_set_const(module, tname, (jl_value_t*)ftype);
    jl_value_t *f = jl_new_struct(ftype);
    ftype->instance = f; jl_gc_wb(ftype, f);
    JL_GC_POP();
    return (jl_function_t*)f;
}

JL_DLLEXPORT jl_function_t *jl_get_kwsorter(jl_value_t *ty)
{
    jl_datatype_t *dt = (jl_datatype_t*)jl_argument_datatype(ty);
    if ((jl_value_t*)dt == jl_nothing)
        jl_error("cannot get keyword sorter for abstract type");
    jl_typename_t *tn = dt->name;
    jl_methtable_t *mt = tn->mt;
    if (!mt->kwsorter) {
        JL_LOCK(&mt->writelock);
        if (!mt->kwsorter) {
            mt->kwsorter = jl_new_generic_function_with_supertype(tn->name, mt->module, jl_function_type, 1);
            jl_gc_wb(mt, mt->kwsorter);
        }
        JL_UNLOCK(&mt->writelock);
    }
    return mt->kwsorter;
}

jl_function_t *jl_new_generic_function(jl_sym_t *name, jl_module_t *module)
{
    return jl_new_generic_function_with_supertype(name, module, jl_function_type, 0);
}

struct ml_matches_env {
    struct typemap_intersection_env match;
    // results:
    jl_value_t *t; // array of svec(argtypes, params, Method)
    size_t min_valid;
    size_t max_valid;
    // temporary:
    jl_svec_t *matc;   // current working svec
    // inputs:
    size_t world;
    int lim;
    int include_ambiguous;  // whether ambiguous matches should be included
};
static int ml_matches_visitor(jl_typemap_entry_t *ml, struct typemap_intersection_env *closure0)
{
    struct ml_matches_env *closure = container_of(closure0, struct ml_matches_env, match);
    int i;
    if (closure->world != 0) { // use zero as a flag value for returning all matches
        // ignore method table entries that have been replaced in the current world
        if (closure->world < ml->min_world) {
            if (closure->max_valid >= ml->min_world)
                closure->max_valid = ml->min_world - 1;
            return 1;
        }
        else if (closure->world > ml->max_world) {
            // ignore method table entries that are part of a later world
            if (closure->min_valid <= ml->max_world)
                closure->min_valid = ml->max_world + 1;
            return 1;
        }
        else {
            // intersect the env valid range with method's valid range
            if (closure->min_valid < ml->min_world)
                closure->min_valid = ml->min_world;
            if (closure->max_valid > ml->max_world)
                closure->max_valid = ml->max_world;
        }
    }
    // a method is shadowed if type <: S <: m->sig where S is the
    // signature of another applicable method
    /*
      more generally, we can stop when the type is a subtype of the
      union of all the signatures examined so far.
    */
    jl_method_t *meth = ml->func.method;
    assert(meth);
    int skip = 0;
    size_t len = jl_array_len(closure->t);
    if (closure->lim >= 0) {
        // we can skip this match if the types are already covered
        // by a prior (more specific) match. but only do this in
        // the "limited" mode used by type inference.
        for (i = 0; i < len; i++) {
            jl_value_t *prior_ti = jl_svecref(jl_array_ptr_ref(closure->t, i), 0);
            // TODO: should be possible to remove the `isdispatchtuple` check,
            // but we still need it in case an intersection was approximate.
            if (jl_is_datatype(prior_ti) && ((jl_datatype_t*)prior_ti)->isdispatchtuple &&
                    jl_subtype(closure->match.ti, prior_ti)) {
                skip = 1;
                break;
            }
        }
    }
    if (!skip) {
        int done = closure0->issubty; // stop; signature fully covers queried type
        // if we reach a definition that fully covers the arguments but there are
        // ambiguities, then this method might not actually match, so we shouldn't
        // add it to the results.
        int return_this_match = 1;
        if (meth->ambig != jl_nothing && (!closure->include_ambiguous || done)) {
            jl_svec_t *env = NULL;
            jl_value_t *mti = NULL;
            JL_GC_PUSH2(&env, &mti);
            for (size_t j = 0; j < jl_array_len(meth->ambig); j++) {
                jl_method_t *mambig = (jl_method_t*)jl_array_ptr_ref(meth->ambig, j);
                env = jl_emptysvec;
                mti = jl_type_intersection_env((jl_value_t*)closure->match.type,
                                               (jl_value_t*)mambig->sig, &env);
                if (mti != (jl_value_t*)jl_bottom_type) {
                    if (closure->include_ambiguous) {
                        assert(done);
                        int k;
                        for (k = 0; k < len; k++) {
                            if ((jl_value_t*)mambig == jl_svecref(jl_array_ptr_ref(closure->t, k), 2))
                                break;
                        }
                        if (k >= len) {
                            if (len == 0) {
                                closure->t = (jl_value_t*)jl_alloc_vec_any(0);
                            }
                            mti = (jl_value_t*)jl_svec(3, mti, env, mambig);
                            jl_array_ptr_1d_push((jl_array_t*)closure->t, mti);
                            len++;
                        }
                    }
                    else {
                        // the current method definitely never matches if the intersection with this method
                        // is also fully covered by an ambiguous method's signature
                        if (jl_subtype(closure->match.ti, mambig->sig)) {
                            return_this_match = 0;
                            break;
                        }
                    }
                }
            }
            JL_GC_POP();
        }
        if (return_this_match) {
            if (closure->lim >= 0 && len >= closure->lim) {
                closure->t = (jl_value_t*)jl_false;
                return 0; // terminate search
            }
            closure->matc = jl_svec(3, closure->match.ti, closure->match.env, meth);
            if (len == 0) {
                closure->t = (jl_value_t*)jl_alloc_vec_any(1);
                jl_array_ptr_set(closure->t, 0, (jl_value_t*)closure->matc);
            }
            else {
                jl_array_ptr_1d_push((jl_array_t*)closure->t, (jl_value_t*)closure->matc);
            }
        }
        if (done)
            return 0;
    }
    return 1;
}

// This is the collect form of calling jl_typemap_intersection_visitor
// with optimizations to skip fully shadowed methods.
//
// Returns a match as an array of svec(argtypes, static_params, Method).
// See below for the meaning of lim.
static jl_value_t *ml_matches(union jl_typemap_t defs, int offs,
                              jl_tupletype_t *type, int lim, int include_ambiguous,
                              size_t world, size_t *min_valid, size_t *max_valid)
{
    jl_value_t *unw = jl_unwrap_unionall((jl_value_t*)type);
    size_t l = jl_svec_len(((jl_datatype_t*)unw)->parameters);
    jl_value_t *va = NULL;
    if (l > 0) {
        va = jl_tparam(unw, l - 1);
        if (jl_is_vararg_type(va))
            va = jl_unwrap_vararg(va);
        else
            va = NULL;
    }
    struct ml_matches_env env;
    env.match.fptr = ml_matches_visitor;
    env.match.type = (jl_value_t*)type;
    env.match.va = va;
    env.match.ti = NULL;
    env.match.env = jl_emptysvec;
    env.t = jl_an_empty_vec_any;
    env.matc = NULL;
    env.lim = lim;
    env.include_ambiguous = include_ambiguous;
    env.world = world;
    env.min_valid = *min_valid;
    env.max_valid = *max_valid;
    JL_GC_PUSH4(&env.t, &env.matc, &env.match.env, &env.match.ti);
    jl_typemap_intersection_visitor(defs, offs, &env.match);
    JL_GC_POP();
    *min_valid = env.min_valid;
    *max_valid = env.max_valid;
    return env.t;
}

// see if it might be possible to construct an instance of `typ`
// if ninitialized == nfields, but a fieldtype is Union{},
// that type will not be constructable, for example, tested recursively
int jl_has_concrete_subtype(jl_value_t *typ)
{
    if (typ == jl_bottom_type)
        return 0;
    typ = jl_unwrap_unionall(typ);
    if (jl_is_vararg_type(typ))
        typ = jl_unwrap_vararg(typ);
    if (!jl_is_datatype(typ))
        return 1;
    if (((jl_datatype_t*)typ)->name == jl_namedtuple_typename)
        return jl_has_concrete_subtype(jl_tparam1(typ));
    jl_svec_t *fields = ((jl_datatype_t*)typ)->types;
    size_t i, l = jl_svec_len(fields);
    if (l != ((jl_datatype_t*)typ)->ninitialized)
        if (((jl_datatype_t*)typ)->name != jl_tuple_typename)
            return 1;
    for (i = 0; i < l; i++) {
        jl_value_t *ft = jl_svecref(fields, i);
        if (!jl_has_concrete_subtype(ft))
            return 0;
    }
    return 1;
}

// TODO: separate the codegen and typeinf locks
//   currently using a coarser lock seems like
//   the best way to avoid acquisition priority
//   ordering violations
//static jl_mutex_t typeinf_lock;
#define typeinf_lock codegen_lock

JL_DLLEXPORT void jl_typeinf_begin(void)
{
    JL_LOCK(&typeinf_lock);
}

JL_DLLEXPORT void jl_typeinf_end(void)
{
    JL_UNLOCK(&typeinf_lock);
}

#ifdef __cplusplus
}
#endif

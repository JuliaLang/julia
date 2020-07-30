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

JL_DLLEXPORT size_t jl_world_counter = 1; // TODO: should this be atomic release/consume?
JL_DLLEXPORT size_t jl_get_world_counter(void)
{
    return jl_world_counter;
}

JL_DLLEXPORT size_t jl_get_tls_world_age(void)
{
    return jl_get_ptls_states()->world_age;
}

/// ----- Handling for Julia callbacks ----- ///

JL_DLLEXPORT int8_t jl_is_in_pure_context(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    return ptls->in_pure_callback;
}

tracer_cb jl_newmeth_tracer = NULL;
JL_DLLEXPORT void jl_register_newmeth_tracer(void (*callback)(jl_method_t *tracee))
{
    jl_newmeth_tracer = (tracer_cb)callback;
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
        jl_static_show(JL_STDERR, jl_current_exception());
        jl_printf(JL_STDERR, "\n");
        jlbacktrace();
    }
}

/// ----- Definitions for various internal TypeMaps ----- ///

const struct jl_typemap_info method_defs = {
    1, &jl_method_type
};
const struct jl_typemap_info lambda_cache = {
    0, &jl_method_instance_type
};

static int8_t jl_cachearg_offset(jl_methtable_t *mt)
{
    return mt->offs;
}

/// ----- Insertion logic for special entries ----- ///


static uint_t speccache_hash(size_t idx, jl_svec_t *data)
{
    jl_method_instance_t *ml = (jl_method_instance_t*)jl_svecref(data, idx);
    jl_value_t *sig = ml->specTypes;
    if (jl_is_unionall(sig))
        sig = jl_unwrap_unionall(sig);
    return ((jl_datatype_t*)sig)->hash;
}

static int speccache_eq(size_t idx, const void *ty, jl_svec_t *data, uint_t hv)
{
    jl_method_instance_t *ml = (jl_method_instance_t*)jl_svecref(data, idx);
    jl_value_t *sig = ml->specTypes;
    if (ty == sig)
        return 1;
    uint_t h2 = ((jl_datatype_t*)(jl_is_unionall(sig) ? jl_unwrap_unionall(sig) : sig))->hash;
    if (h2 != hv)
        return 0;
    return jl_types_equal(sig, (jl_value_t*)ty);
}

// get or create the MethodInstance for a specialization
JL_DLLEXPORT jl_method_instance_t *jl_specializations_get_linfo(jl_method_t *m JL_PROPAGATES_ROOT, jl_value_t *type, jl_svec_t *sparams)
{
    uint_t hv = ((jl_datatype_t*)(jl_is_unionall(type) ? jl_unwrap_unionall(type) : type))->hash;
    for (int locked = 0; ; locked++) {
        jl_array_t *speckeyset = jl_atomic_load_acquire(&m->speckeyset);
        jl_svec_t *specializations = jl_atomic_load_acquire(&m->specializations);
        size_t i, cl = jl_svec_len(specializations);
        if (hv) {
            ssize_t idx = jl_smallintset_lookup(speckeyset, speccache_eq, type, specializations, hv);
            if (idx != -1) {
                jl_method_instance_t *mi = (jl_method_instance_t*)jl_svecref(specializations, idx);
                if (locked)
                    JL_UNLOCK(&m->writelock);
                return mi;
            }
        }
        else {
            jl_method_instance_t **data = (jl_method_instance_t**)jl_svec_data(specializations);
            JL_GC_PUSH1(&specializations); // clang-sa doesn't realize this loop uses specializations
            for (i = cl; i > 0; i--) {
                jl_method_instance_t *mi = jl_atomic_load_relaxed(&data[i - 1]);
                if (mi == NULL)
                    break;
                if (jl_types_equal(mi->specTypes, type)) {
                    if (locked)
                        JL_UNLOCK(&m->writelock);
                    JL_GC_POP();
                    return mi;
                }
            }
            JL_GC_POP();
        }
        if (!sparams) // can't insert without knowing this
            return NULL;
        if (!locked) {
            JL_LOCK(&m->writelock);
        }
        else {
            if (hv) {
                jl_method_instance_t **data = (jl_method_instance_t**)jl_svec_data(specializations);
                for (i = 0; i < cl; i++) {
                    jl_method_instance_t *mi = jl_atomic_load_relaxed(&data[i]);
                    if (mi == NULL)
                        break;
                    assert(!jl_types_equal(mi->specTypes, type));
                }
            }
            jl_method_instance_t *mi = jl_get_specialized(m, type, sparams);
            JL_GC_PUSH1(&mi);
            if (hv ? (i + 1 >= cl || jl_svecref(specializations, i + 1) != NULL) : (i <= 1 || jl_svecref(specializations, i - 2) != NULL)) {
                size_t ncl = cl < 8 ? 8 : (cl*3)>>1;
                jl_svec_t *nc = jl_alloc_svec_uninit(ncl);
                if (i > 0)
                    memcpy((char*)jl_svec_data(nc), jl_svec_data(specializations), sizeof(void*) * i);
                memset((char*)jl_svec_data(nc) + sizeof(void*) * i, 0, sizeof(void*) * (ncl - cl));
                if (i < cl)
                    memcpy((char*)jl_svec_data(nc) + sizeof(void*) * (i + ncl - cl),
                           (char*)jl_svec_data(specializations) + sizeof(void*) * i,
                           sizeof(void*) * (cl - i));
                jl_atomic_store_release(&m->specializations, nc);
                jl_gc_wb(m, nc);
                specializations = nc;
                if (!hv)
                    i += ncl - cl;
            }
            if (!hv)
                i -= 1;
            assert(jl_svecref(specializations, i) == NULL);
            jl_svecset(specializations, i, mi); // jl_atomic_store_release?
            if (hv) {
                // TODO: fuse lookup and insert steps?
                jl_smallintset_insert(&m->speckeyset, (jl_value_t*)m, speccache_hash, i, specializations);
            }
            JL_UNLOCK(&m->writelock);
            JL_GC_POP();
            return mi;
        }
    }
}

JL_DLLEXPORT jl_value_t *jl_specializations_lookup(jl_method_t *m, jl_value_t *type)
{
    jl_value_t *mi = (jl_value_t*)jl_specializations_get_linfo(m, type, NULL);
    if (mi == NULL)
        return jl_nothing;
    return mi;
}

JL_DLLEXPORT jl_value_t *jl_methtable_lookup(jl_methtable_t *mt, jl_value_t *type, size_t world)
{
    struct jl_typemap_assoc search = {type, world, NULL, 0, ~(size_t)0};
    jl_typemap_entry_t *sf = jl_typemap_assoc_by_type(mt->defs, &search, /*offs*/0, /*subtype*/0);
    if (!sf)
        return jl_nothing;
    return sf->func.value;
}

// ----- MethodInstance specialization instantiation ----- //

JL_DLLEXPORT jl_method_t *jl_new_method_uninit(jl_module_t*);
JL_DLLEXPORT jl_code_instance_t* jl_new_codeinst(
        jl_method_instance_t *mi, jl_value_t *rettype,
        jl_value_t *inferred_const, jl_value_t *inferred,
        int32_t const_flags, size_t min_world, size_t max_world);
JL_DLLEXPORT void jl_mi_cache_insert(jl_method_instance_t *mi JL_ROOTING_ARGUMENT,
                                     jl_code_instance_t *ci JL_ROOTED_ARGUMENT JL_MAYBE_UNROOTED);

jl_datatype_t *jl_mk_builtin_func(jl_datatype_t *dt, const char *name, jl_fptr_args_t fptr) JL_GC_DISABLED
{
    jl_sym_t *sname = jl_symbol(name);
    if (dt == NULL) {
        jl_value_t *f = jl_new_generic_function_with_supertype(sname, jl_core_module, jl_builtin_type);
        jl_set_const(jl_core_module, sname, f);
        dt = (jl_datatype_t*)jl_typeof(f);
    }

    jl_method_t *m = jl_new_method_uninit(jl_core_module);
    m->name = sname;
    m->module = jl_core_module;
    m->isva = 1;
    m->nargs = 2;
    m->sig = (jl_value_t*)jl_anytuple_type;
    m->slot_syms = jl_an_empty_string;

    jl_typemap_entry_t *newentry = NULL;
    JL_GC_PUSH2(&m, &newentry);
    jl_method_instance_t *mi = jl_get_specialized(m, (jl_value_t*)jl_anytuple_type, jl_emptysvec);
    m->unspecialized = mi;
    jl_gc_wb(m, mi);

    jl_code_instance_t *codeinst = jl_new_codeinst(mi,
        (jl_value_t*)jl_any_type, jl_nothing, jl_nothing,
        0, 1, ~(size_t)0);
    jl_mi_cache_insert(mi, codeinst);
    codeinst->specptr.fptr1 = fptr;
    codeinst->invoke = jl_fptr_args;

    jl_methtable_t *mt = dt->name->mt;
    newentry = jl_typemap_alloc(jl_anytuple_type, NULL, jl_emptysvec,
            (jl_value_t*)mi, 1, ~(size_t)0);
    jl_typemap_insert(&mt->cache, (jl_value_t*)mt, newentry, 0, &lambda_cache);

    mt->frozen = 1;
    JL_GC_POP();
    return dt;
}

// run type inference on lambda "mi" for given argument types.
// returns the inferred source, and may cache the result in mi
// if successful, also updates the mi argument to describe the validity of this src
// if inference doesn't occur (or can't finish), returns NULL instead
jl_code_info_t *jl_type_infer(jl_method_instance_t *mi, size_t world, int force)
{
    JL_TIMING(INFERENCE);
    if (jl_typeinf_func == NULL)
        return NULL;
    if (jl_is_method(mi->def.method) && mi->def.method->unspecialized == mi)
        return NULL; // avoid inferring the unspecialized method
    static int in_inference;
    if (in_inference > 2)
        return NULL;

    jl_code_info_t *src = NULL;
#ifdef ENABLE_INFERENCE
    if (mi->inInference && !force)
        return NULL;
    if (jl_is_method(mi->def.method) && mi->def.method->unspecialized == mi)
        return NULL; // be careful never to infer the unspecialized method, this would not be valid

    jl_value_t **fargs;
    JL_GC_PUSHARGS(fargs, 3);
    fargs[0] = (jl_value_t*)jl_typeinf_func;
    fargs[1] = (jl_value_t*)mi;
    fargs[2] = jl_box_ulong(world);
#ifdef TRACE_INFERENCE
    if (mi->specTypes != (jl_value_t*)jl_emptytuple_type) {
        jl_printf(JL_STDERR,"inference on ");
        jl_static_show_func_sig(JL_STDERR, (jl_value_t*)mi->specTypes);
        jl_printf(JL_STDERR, "\n");
    }
#endif
    jl_ptls_t ptls = jl_get_ptls_states();
    int last_errno = errno;
#ifdef _OS_WINDOWS_
    DWORD last_error = GetLastError();
#endif
    size_t last_age = ptls->world_age;
    ptls->world_age = jl_typeinf_world;
    mi->inInference = 1;
    in_inference++;
    JL_TRY {
        src = (jl_code_info_t*)jl_apply(fargs, 3);
    }
    JL_CATCH {
        jl_printf(JL_STDERR, "Internal error: encountered unexpected error in runtime:\n");
        jl_static_show(JL_STDERR, jl_current_exception());
        jl_printf(JL_STDERR, "\n");
        jlbacktrace(); // written to STDERR_FILENO
        src = NULL;
    }
    ptls->world_age = last_age;
    in_inference--;
    mi->inInference = 0;
#ifdef _OS_WINDOWS_
    SetLastError(last_error);
#endif
    errno = last_errno;

    if (src && !jl_is_code_info(src)) {
        src = NULL;
    }
    JL_GC_POP();
#endif
    return src;
}

JL_DLLEXPORT jl_value_t *jl_call_in_typeinf_world(jl_value_t **args, int nargs)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    size_t last_age = ptls->world_age;
    ptls->world_age = jl_typeinf_world;
    jl_value_t *ret = jl_apply(args, nargs);
    ptls->world_age = last_age;
    return ret;
}

JL_DLLEXPORT jl_value_t *jl_rettype_inferred(jl_method_instance_t *mi, size_t min_world, size_t max_world) JL_NOTSAFEPOINT
{
    jl_code_instance_t *codeinst = mi->cache;
    while (codeinst) {
        if (codeinst->min_world <= min_world && max_world <= codeinst->max_world) {
            jl_value_t *code = codeinst->inferred;
            if (code && (code == jl_nothing || jl_ir_flag_inferred((jl_array_t*)code)))
                return (jl_value_t*)codeinst;
        }
        codeinst = jl_atomic_load_relaxed(&codeinst->next);
    }
    return (jl_value_t*)jl_nothing;
}


JL_DLLEXPORT jl_code_instance_t *jl_get_method_inferred(
        jl_method_instance_t *mi JL_PROPAGATES_ROOT, jl_value_t *rettype,
        size_t min_world, size_t max_world)
{
    jl_code_instance_t *codeinst = mi->cache;
    while (codeinst) {
        if (codeinst->min_world == min_world &&
            codeinst->max_world == max_world &&
            jl_egal(codeinst->rettype, rettype)) {
            return codeinst;
        }
        codeinst = jl_atomic_load_relaxed(&codeinst->next);
    }
    codeinst = jl_new_codeinst(
        mi, rettype, NULL, NULL,
        0, min_world, max_world);
    jl_mi_cache_insert(mi, codeinst);
    return codeinst;
}

JL_DLLEXPORT jl_code_instance_t *jl_new_codeinst(
        jl_method_instance_t *mi, jl_value_t *rettype,
        jl_value_t *inferred_const, jl_value_t *inferred,
        int32_t const_flags, size_t min_world, size_t max_world
        /*, jl_array_t *edges, int absolute_max*/)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    assert(min_world <= max_world && "attempting to set invalid world constraints");
    jl_code_instance_t *codeinst = (jl_code_instance_t*)jl_gc_alloc(ptls, sizeof(jl_code_instance_t),
            jl_code_instance_type);
    JL_GC_PUSH1(&codeinst);
    codeinst->def = mi;
    codeinst->min_world = min_world;
    codeinst->max_world = max_world;
    codeinst->rettype = rettype;
    codeinst->inferred = inferred;
    //codeinst->edges = NULL;
    if ((const_flags & 2) == 0)
        inferred_const = NULL;
    codeinst->rettype_const = inferred_const;
    codeinst->invoke = NULL;
    if ((const_flags & 1) != 0) {
        assert(const_flags & 2);
        jl_atomic_store_release(&codeinst->invoke, jl_fptr_const_return);
    }
    codeinst->specptr.fptr = NULL;
    codeinst->isspecsig = 0;
    codeinst->precompile = 0;
    codeinst->next = NULL;
    JL_GC_POP();
    return codeinst;
}

JL_DLLEXPORT void jl_mi_cache_insert(jl_method_instance_t *mi JL_ROOTING_ARGUMENT,
                                     jl_code_instance_t *ci JL_ROOTED_ARGUMENT JL_MAYBE_UNROOTED)
{
    JL_GC_PUSH1(&ci);
    if (jl_is_method(mi->def.method))
        JL_LOCK(&mi->def.method->writelock);
    ci->next = mi->cache;
    jl_atomic_store_release(&mi->cache, ci);
    jl_gc_wb(mi, ci);
    if (jl_is_method(mi->def.method))
        JL_UNLOCK(&mi->def.method->writelock);
    JL_GC_POP();
    return;
}

static int get_method_unspec_list(jl_typemap_entry_t *def, void *closure)
{
    jl_svec_t *specializations = def->func.method->specializations;
    size_t i, l = jl_svec_len(specializations);
    for (i = 0; i < l; i++) {
        jl_method_instance_t *mi = (jl_method_instance_t*)jl_svecref(specializations, i);
        if (mi) {
            assert(jl_is_method_instance(mi));
            if (jl_rettype_inferred(mi, jl_world_counter, jl_world_counter) == jl_nothing)
                jl_array_ptr_1d_push((jl_array_t*)closure, (jl_value_t*)mi);
        }
    }
    return 1;
}

static void foreach_mtable_in_module(
        jl_module_t *m,
        void (*visit)(jl_methtable_t *mt, void *env),
        void *env,
        jl_array_t **visited)
{
    size_t i;
    void **table = m->bindings.table;
    *visited = jl_eqtable_put(*visited, (jl_value_t*)m, jl_true, NULL);
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
                        !jl_eqtable_get(*visited, v, NULL)) {
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
                foreach_mtable_in_module(m, visit, env, &visited);
        }
    }
    else {
        foreach_mtable_in_module(jl_main_module, visit, env, &visited);
    }
    JL_GC_POP();
}

static void reset_mt_caches(jl_methtable_t *mt, void *env)
{
    // removes all method caches
    if (mt->defs != jl_nothing) { // make sure not to reset builtin functions
        mt->leafcache = (jl_array_t*)jl_an_empty_vec_any;
        mt->cache = jl_nothing;
    }
    jl_typemap_visitor(mt->defs, get_method_unspec_list, env);
}


jl_function_t *jl_typeinf_func = NULL;
size_t jl_typeinf_world = 0;

JL_DLLEXPORT void jl_set_typeinf_func(jl_value_t *f)
{
    jl_typeinf_func = (jl_function_t*)f;
    jl_typeinf_world = jl_get_tls_world_age();
    ++jl_world_counter; // make type-inference the only thing in this world
    if (jl_typeinf_world == 0) {
        // give type inference a chance to see all of these
        // TODO: also reinfer if max_world != ~(size_t)0
        jl_array_t *unspec = jl_alloc_vec_any(0);
        JL_GC_PUSH1(&unspec);
        jl_foreach_reachable_mtable(reset_mt_caches, (void*)unspec);
        size_t i, l;
        for (i = 0, l = jl_array_len(unspec); i < l; i++) {
            jl_method_instance_t *mi = (jl_method_instance_t*)jl_array_ptr_ref(unspec, i);
            if (jl_rettype_inferred(mi, jl_world_counter, jl_world_counter) == jl_nothing)
                jl_type_infer(mi, jl_world_counter, 1);
        }
        JL_GC_POP();
    }
}

static int very_general_type(jl_value_t *t)
{
    return (t == (jl_value_t*)jl_any_type || jl_types_equal(t, (jl_value_t*)jl_type_type));
}

jl_value_t *jl_nth_slot_type(jl_value_t *sig, size_t i)
{
    sig = jl_unwrap_unionall(sig);
    size_t len = jl_nparams(sig);
    if (len == 0)
        return NULL;
    if (i < len-1)
        return jl_tparam(sig, i);
    if (jl_is_vararg_type(jl_tparam(sig, len-1)))
        return jl_unwrap_vararg(jl_tparam(sig, len-1));
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

static jl_value_t *ml_matches(jl_methtable_t *mt, int offs,
                              jl_tupletype_t *type, int lim, int include_ambiguous,
                              int intersections, size_t world, int cache_result,
                              size_t *min_valid, size_t *max_valid, int *has_ambiguity);

// get the compilation signature specialization for this method
static void jl_compilation_sig(
    jl_tupletype_t *const tt, // the original tupletype of the call : this is expected to be a relative simple type (no Varags, Union, UnionAll, etc.)
    jl_svec_t *sparams,
    jl_method_t *definition,
    intptr_t nspec,
    // output:
    jl_svec_t **const newparams JL_REQUIRE_ROOTED_SLOT)
{
    if (definition->generator) {
        // staged functions aren't optimized
        // so assume the caller was intelligent about calling us
        return;
    }

    jl_value_t *decl = definition->sig;
    assert(jl_is_tuple_type(tt));
    size_t i, np = jl_nparams(tt);
    size_t nargs = definition->nargs; // == jl_nparams(jl_unwrap_unionall(decl));
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
        else if (jl_is_kind(elt)) {
            // not triggered for isdispatchtuple(tt), this attempts to handle
            // some cases of adapting a random signature into a compilation signature
            // if we get a kind, where we don't expect to accept one, widen it to something more expected (Type{T})
            if (!(jl_subtype(elt, decl_i) && !jl_subtype((jl_value_t*)jl_type_type, decl_i))) {
                if (!*newparams) *newparams = jl_svec_copy(tt->parameters);
                elt = (jl_value_t*)jl_type_type;
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

        if (jl_types_equal(elt, (jl_value_t*)jl_type_type)) { // elt == Type{T} where T
            // not triggered for isdispatchtuple(tt), this attempts to handle
            // some cases of adapting a random signature into a compilation signature
        }
        else if (!jl_is_datatype(elt) && jl_subtype(elt, (jl_value_t*)jl_type_type)) { // elt <: Type{T}
            // not triggered for isdispatchtuple(tt), this attempts to handle
            // some cases of adapting a random signature into a compilation signature
            if (!*newparams) *newparams = jl_svec_copy(tt->parameters);
            jl_svecset(*newparams, i, jl_type_type);
        }
        else if (jl_is_type_type(elt)) { // elt isa Type{T}
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
                // don't apply this heuristic if the argument is called (issue #36783)
                int iscalled = i_arg > 0 && i_arg <= 8 && (definition->called & (1 << (i_arg - 1)));
                if (!iscalled) {
                    if (!*newparams) *newparams = jl_svec_copy(tt->parameters);
                    jl_svecset(*newparams, i, jl_type_type);
                }
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
                    jl_value_t *di = jl_type_intersection(decl_i, (jl_value_t*)jl_type_type);
                    assert(di != (jl_value_t*)jl_bottom_type);
                    // issue #11355: DataType has a UID and so would take precedence in the cache
                    if (jl_is_kind(di))
                        jl_svecset(*newparams, i, (jl_value_t*)jl_type_type);
                    else
                        jl_svecset(*newparams, i, di);
                    // TODO: recompute static parameter values, so in extreme cases we
                    // can give `T=Type` instead of `T=Type{Type{Type{...`.   /* make editors happy:}}} */
                }
                else {
                    jl_svecset(*newparams, i, (jl_value_t*)jl_type_type);
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
        }
    }

    // for varargs methods, only specialize up to max_args.
    // in general, here we want to find the biggest type that's not a
    // supertype of any other method signatures. so far we are conservative
    // and the types we find should be bigger.
    if (jl_nparams(tt) >= nspec && jl_va_tuple_kind((jl_datatype_t*)decl) == JL_VARARG_UNBOUND) {
        jl_svec_t *limited = jl_alloc_svec(nspec);
        JL_GC_PUSH1(&limited);
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
            // avoid Vararg{Type{Type{...}}}
            if (jl_is_type_type(lasttype) && jl_is_type_type(jl_tparam0(lasttype)))
                lasttype = (jl_value_t*)jl_type_type;
            jl_svecset(limited, i, jl_wrap_vararg(lasttype, (jl_value_t*)NULL));
        }
        else {
            jl_value_t *unw = jl_unwrap_unionall(decl);
            jl_value_t *lastdeclt = jl_tparam(unw, nargs - 1);
            assert(jl_is_vararg_type(lastdeclt) && jl_nparams(unw) == nargs);
            int nsp = jl_svec_len(sparams);
            if (nsp > 0 && jl_has_free_typevars(lastdeclt)) {
                assert(jl_subtype_env_size(decl) == nsp);
                lastdeclt = jl_instantiate_type_in_env(lastdeclt, (jl_unionall_t*)decl, jl_svec_data(sparams));
                // TODO: rewrap_unionall(lastdeclt, sparams) if any sparams isa TypeVar???
                // TODO: if we made any replacements above, sparams may now be incorrect
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
    size_t nargs = definition->nargs; // == jl_nparams(jl_unwrap_unionall(decl));
    if (np == 0)
        return nargs == 0;

    if (definition->generator) {
        // staged functions aren't optimized
        // so assume the caller was intelligent about calling us
        return (definition->isva ? np >= nargs - 1 : np == nargs) && type->isdispatchtuple;
    }

    // for varargs methods, only specialize up to max_args (>= nargs + 1).
    // in general, here we want to find the biggest type that's not a
    // supertype of any other method signatures. so far we are conservative
    // and the types we find should be bigger.
    if (definition->isva) {
        unsigned nspec_min = nargs + 1; // min number of non-vararg values before vararg
        unsigned nspec_max = INT32_MAX; // max number of non-vararg values before vararg
        jl_methtable_t *mt = jl_method_table_for(decl);
        if ((jl_value_t*)mt != jl_nothing) {
            // try to refine estimate of min and max
            if (mt != jl_type_type_mt && mt != jl_nonfunction_mt)
                nspec_min = mt->max_args + 2;
            else
                nspec_max = nspec_min;
        }
        int isbound = (jl_va_tuple_kind((jl_datatype_t*)decl) == JL_VARARG_UNBOUND);
        if (jl_is_vararg_type(jl_tparam(type, np - 1))) {
            if (!isbound || np < nspec_min || np > nspec_max)
                return 0;
        }
        else {
            if (np < nargs - 1 || (isbound && np >= nspec_max))
                return 0;
        }
    }
    else if (np != nargs || jl_is_vararg_type(jl_tparam(type, np - 1))) {
        return 0;
    }

    for (i = 0; i < np; i++) {
        jl_value_t *elt = jl_tparam(type, i);
        jl_value_t *decl_i = jl_nth_slot_type((jl_value_t*)decl, i);
        size_t i_arg = (i < nargs - 1 ? i : nargs - 1);

        if (jl_is_vararg_type(elt)) {
            elt = jl_unwrap_vararg(elt);
            if (jl_has_free_typevars(decl_i)) {
                // TODO: in this case, answer semi-conservatively that these varargs are always compilable
                // we don't have the ability to get sparams, so deciding if elt
                // is a potential result of jl_instantiate_type_in_env for decl_i
                // for any sparams that is consistent with the rest of the arguments
                // seems like it would be extremely difficult
                // and hopefully the upstream code probably gave us something reasonable
                continue;
            }
            else if (jl_egal(elt, decl_i)) {
                continue;
            }
            else if (jl_is_type_type(elt) && jl_is_type_type(jl_tparam0(elt))) {
                return 0;
            }
            // else, it needs to meet the usual rules
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
            if (jl_subtype(elt, decl_i) && !jl_subtype((jl_value_t*)jl_type_type, decl_i))
                continue;
            // TODO: other code paths that could reach here
            return 0;
        }
        else if (jl_is_kind(decl_i)) {
            return 0;
        }

        if (jl_is_type_type(jl_unwrap_unionall(elt))) {
            int iscalled = i_arg > 0 && i_arg <= 8 && (definition->called & (1 << (i_arg - 1)));
            if (jl_types_equal(elt, (jl_value_t*)jl_type_type)) {
                if (!iscalled && very_general_type(decl_i))
                    continue;
                if (i >= nargs && definition->isva)
                    continue;
                return 0;
            }
            if (!iscalled && very_general_type(decl_i))
                return 0;
            if (!jl_is_datatype(elt))
                return 0;

            // if the declared type was not Any or Union{Type, ...},
            // then the match must been with kind, such as UnionAll or DataType,
            // and the result of matching the type signature
            // needs to be corrected to the concrete type 'kind' (and not to Type)
            jl_value_t *kind = jl_typeof(jl_tparam0(elt));
            if (kind == jl_bottom_type)
                return 0; // Type{Union{}} gets normalized to typeof(Union{})
            if (jl_subtype(kind, decl_i) && !jl_subtype((jl_value_t*)jl_type_type, decl_i))
                return 0; // gets turned into a kind

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
                    jl_value_t *di = jl_type_intersection(decl_i, (jl_value_t*)jl_type_type);
                    JL_GC_PUSH1(&di);
                    assert(di != (jl_value_t*)jl_bottom_type);
                    if (jl_is_kind(di)) {
                        JL_GC_POP();
                        return 0;
                    }
                    else if (!jl_types_equal(di, elt)) {
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


static int concretesig_equal(jl_value_t *tt, jl_value_t *simplesig) JL_NOTSAFEPOINT
{
    jl_value_t **types = jl_svec_data(((jl_datatype_t*)tt)->parameters);
    jl_value_t **sigs = jl_svec_data(((jl_datatype_t*)simplesig)->parameters);
    size_t i, lensig = jl_nparams(simplesig);
    assert(lensig == jl_nparams(tt));
    assert(lensig > 0 && !jl_is_vararg_type(jl_tparam(simplesig, lensig - 1)));
    for (i = 0; i < lensig; i++) {
        jl_value_t *decl = sigs[i];
        jl_value_t *a = types[i];
        if (a != decl && decl != (jl_value_t*)jl_any_type) {
            if (!(jl_is_type_type(a) && jl_typeof(jl_tparam0(a)) == decl))
                return 0;
        }
    }
    return 1;
}

static inline jl_typemap_entry_t *lookup_leafcache(jl_array_t *leafcache JL_PROPAGATES_ROOT, jl_value_t *tt, size_t world) JL_NOTSAFEPOINT
{
    jl_typemap_entry_t *entry = (jl_typemap_entry_t*)jl_eqtable_get(leafcache, (jl_value_t*)tt, NULL);
    if (entry) {
        do {
            if (entry->min_world <= world && world <= entry->max_world) {
                if (entry->simplesig == (void*)jl_nothing || concretesig_equal(tt, (jl_value_t*)entry->simplesig))
                    return entry;
            }
            entry = entry->next;
        } while ((jl_value_t*)entry != jl_nothing);
    }
    return NULL;
}

static jl_method_instance_t *cache_method(
        jl_methtable_t *mt, jl_typemap_t **cache, jl_value_t *parent JL_PROPAGATES_ROOT,
        jl_tupletype_t *tt, // the original tupletype of the signature
        jl_method_t *definition,
        size_t world, size_t min_valid, size_t max_valid,
        jl_svec_t *sparams)
{
    // caller must hold the mt->writelock
    // short-circuit (now that we hold the lock) if this entry is already present
    int8_t offs = mt ? jl_cachearg_offset(mt) : 1;
    { // scope block
        if (mt) {
            jl_array_t *leafcache = jl_atomic_load_relaxed(&mt->leafcache);
            jl_typemap_entry_t *entry = lookup_leafcache(leafcache, (jl_value_t*)tt, world);
            if (entry)
                return entry->func.linfo;
        }
        struct jl_typemap_assoc search = {(jl_value_t*)tt, world, NULL, 0, ~(size_t)0};
        jl_typemap_entry_t *entry = jl_typemap_assoc_by_type(*cache, &search, offs, /*subtype*/1);
        if (entry && entry->func.value)
            return entry->func.linfo;
    }

    jl_value_t *temp = NULL;
    jl_value_t *temp2 = NULL;
    jl_value_t *temp3 = NULL;
    jl_method_instance_t *newmeth = NULL;
    jl_svec_t *newparams = NULL;
    JL_GC_PUSH5(&temp, &temp2, &temp3, &newmeth, &newparams);

    int cache_with_orig = 1;
    jl_tupletype_t *compilationsig = tt;
    intptr_t nspec = (mt == NULL || mt == jl_type_type_mt || mt == jl_nonfunction_mt ? definition->nargs + 1 : mt->max_args + 2);
    jl_compilation_sig(tt, sparams, definition, nspec, &newparams);
    if (newparams) {
        cache_with_orig = 0;
        compilationsig = jl_apply_tuple_type(newparams);
        temp2 = (jl_value_t*)compilationsig;
        // In most cases `!jl_isa_compileable_sig(tt, definition))`,
        // although for some cases, (notably Varargs)
        // we might choose a replacement type that's preferable but not strictly better
    }
    // TODO: maybe assert(jl_isa_compileable_sig(compilationsig, definition));
    newmeth = jl_specializations_get_linfo(definition, (jl_value_t*)compilationsig, sparams);

    jl_tupletype_t *cachett = tt;
    jl_svec_t* guardsigs = jl_emptysvec;
    if (!cache_with_orig && mt) {
        // now examine what will happen if we chose to use this sig in the cache
        // TODO: should we first check `compilationsig <: definition`?
        size_t min_valid2 = 1;
        size_t max_valid2 = ~(size_t)0;
        int ambig = 0;
        temp = ml_matches(mt, 0, compilationsig, MAX_UNSPECIALIZED_CONFLICTS, 1, 1, world, 0, &min_valid2, &max_valid2, &ambig);
        int guards = 0;
        if (temp == jl_false) {
            cache_with_orig = 1;
        }
        else {
            int unmatched_tvars = 0;
            size_t i, l = jl_array_len(temp);
            for (i = 0; i < l; i++) {
                jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(temp, i);
                jl_svec_t *env = matc->sparams;
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
                if (matc->method != definition) {
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
                jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(temp, i);
                jl_method_t *other = matc->method;
                if (other != definition) {
                    jl_svecset(guardsigs, guards, matc->spec_types);
                    guards++;
                    // alternative approach: insert sentinel entry
                    //jl_typemap_insert(cache, parent, (jl_tupletype_t*)matc->spec_types,
                    //        NULL, jl_emptysvec, /*guard*/NULL, jl_cachearg_offset(mt), &lambda_cache, other->min_world, other->max_world);
                }
            }
        }
        if (!cache_with_orig) {
            // determined above that there's no ambiguity in also using compilationsig as the cacheablesig
            min_valid = min_valid2;
            max_valid = max_valid2;
            cachett = compilationsig;
        }
    }

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

    // short-circuit if an existing entry is already present
    // that satisfies our requirements
    if (cachett != tt) {
        struct jl_typemap_assoc search = {(jl_value_t*)cachett, world, NULL, 0, ~(size_t)0};
        jl_typemap_entry_t *entry = jl_typemap_assoc_by_type(*cache, &search, offs, /*subtype*/1);
        if (entry && jl_egal((jl_value_t*)entry->simplesig, simplett ? (jl_value_t*)simplett : jl_nothing) &&
                jl_egal((jl_value_t*)guardsigs, (jl_value_t*)entry->guardsigs)) {
            JL_GC_POP();
            return entry->func.linfo;
        }
    }

    jl_typemap_entry_t *newentry = jl_typemap_alloc(cachett, simplett, guardsigs, (jl_value_t*)newmeth, min_valid, max_valid);
    temp = (jl_value_t*)newentry;
    if (mt && cachett == tt && jl_svec_len(guardsigs) == 0 && tt->hash && !tt->hasfreetypevars) {
        // we check `tt->hash` exists, since otherwise the NamedTuple
        // constructor and `structdiff` method pollutes this lookup with a lot
        // of garbage in the linear table search
        if (jl_lookup_cache_type_(tt) == NULL) {
            // if this type isn't normally in the cache, force it in there now
            // anyways so that we can depend on it as a token (especially since
            // we just cached it in memory as this method signature anyways)
            JL_LOCK(&typecache_lock);
            if (jl_lookup_cache_type_(tt) == NULL)
                jl_cache_type_(tt);
            JL_UNLOCK(&typecache_lock); // Might GC
        }
        jl_typemap_entry_t *old = (jl_typemap_entry_t*)jl_eqtable_get(mt->leafcache, (jl_value_t*)tt, jl_nothing);
        newentry->next = old;
        jl_gc_wb(newentry, old);
        mt->leafcache = jl_eqtable_put(mt->leafcache, (jl_value_t*)tt, (jl_value_t*)newentry, NULL);
        jl_gc_wb(mt, mt->leafcache);
    }
    else {
         jl_typemap_insert(cache, parent, newentry, offs, &lambda_cache);
    }

    JL_GC_POP();
    return newmeth;
}

static jl_method_match_t *_gf_invoke_lookup(jl_value_t *types JL_PROPAGATES_ROOT, size_t world, size_t *min_valid, size_t *max_valid);

static jl_method_instance_t *jl_mt_assoc_by_type(jl_methtable_t *mt, jl_datatype_t *tt, size_t world)
{
    // caller must hold the mt->writelock
    assert(tt->isdispatchtuple || tt->hasfreetypevars);
    if (tt->isdispatchtuple) {
        jl_array_t *leafcache = jl_atomic_load_relaxed(&mt->leafcache);
        jl_typemap_entry_t *entry = lookup_leafcache(leafcache, (jl_value_t*)tt, world);
        if (entry)
            return entry->func.linfo;
    }

    struct jl_typemap_assoc search = {(jl_value_t*)tt, world, NULL, 0, ~(size_t)0};
    jl_typemap_entry_t *entry = jl_typemap_assoc_by_type(mt->cache, &search, jl_cachearg_offset(mt), /*subtype*/1);
    if (entry)
        return entry->func.linfo;

    size_t min_valid = 0;
    size_t max_valid = ~(size_t)0;
    jl_method_match_t *matc = _gf_invoke_lookup((jl_value_t*)tt, world, &min_valid, &max_valid);
    jl_method_instance_t *nf = NULL;
    if (matc) {
        JL_GC_PUSH1(&matc);
        jl_method_t *m = matc->method;
        jl_svec_t *env = matc->sparams;
        nf = cache_method(mt, &mt->cache, (jl_value_t*)mt, tt, m, world, min_valid, max_valid, env);
        JL_GC_POP();
    }
    return nf;
}


struct matches_env {
    struct typemap_intersection_env match;
    jl_typemap_entry_t *newentry;
    jl_value_t *shadowed;
};
static int get_intersect_visitor(jl_typemap_entry_t *oldentry, struct typemap_intersection_env *closure0)
{
    struct matches_env *closure = container_of(closure0, struct matches_env, match);
    if (oldentry == closure->newentry)
        return 1;
    if (oldentry->max_world < ~(size_t)0 || oldentry->min_world == closure->newentry->min_world)
        // skip if no world has both active
        // also be careful not to try to scan something from the current dump-reload though
        return 1;
    jl_method_t *oldmethod = oldentry->func.method;
    if (closure->shadowed == NULL)
        closure->shadowed = (jl_value_t*)jl_alloc_vec_any(0);
    jl_array_ptr_1d_push((jl_array_t*)closure->shadowed, (jl_value_t*)oldmethod);
    return 1;
}

static jl_value_t *get_intersect_matches(jl_typemap_t *defs, jl_typemap_entry_t *newentry)
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
    struct matches_env env = {{get_intersect_visitor, (jl_value_t*)type, va}};
    env.match.ti = NULL;
    env.match.env = jl_emptysvec;
    env.newentry = newentry;
    env.shadowed = NULL;
    JL_GC_PUSH3(&env.match.env, &env.match.ti, &env.shadowed);
    jl_typemap_intersection_visitor(defs, 0, &env.match);
    JL_GC_POP();
    return env.shadowed;
}

void print_func_loc(JL_STREAM *s, jl_method_t *m)
{
    long lno = m->line;
    if (lno > 0) {
        char *fname = jl_symbol_name((jl_sym_t*)m->file);
        jl_printf(s, " at %s:%ld", fname, lno);
    }
}

static int is_anonfn_typename(char *name)
{
    if (name[0] != '#' || name[1] == '#')
        return 0;
    char *other = strrchr(name, '#');
    return other > &name[1] && other[1] > '0' && other[1] <= '9';
}

static void method_overwrite(jl_typemap_entry_t *newentry, jl_method_t *oldvalue)
{
    // method overwritten
    jl_method_t *method = (jl_method_t*)newentry->func.method;
    jl_module_t *newmod = method->module;
    jl_module_t *oldmod = oldvalue->module;
    jl_datatype_t *dt = jl_first_argument_datatype(oldvalue->sig);
    int anon = dt && is_anonfn_typename(jl_symbol_name(dt->name->name));
    if ((jl_options.warn_overwrite == JL_OPTIONS_WARN_OVERWRITE_ON) ||
        (jl_options.incremental && jl_generating_output()) || anon) {
        JL_STREAM *s = JL_STDERR;
        jl_printf(s, "WARNING: Method definition ");
        jl_static_show_func_sig(s, (jl_value_t*)newentry->sig);
        jl_printf(s, " in module %s", jl_symbol_name(oldmod->name));
        print_func_loc(s, oldvalue);
        jl_printf(s, " overwritten");
        if (oldmod != newmod)
            jl_printf(s, " in module %s", jl_symbol_name(newmod->name));
        if (method->line > 0 && method->line == oldvalue->line && method->file == oldvalue->file)
            jl_printf(s, anon ? " on the same line" : " on the same line (check for duplicate calls to `include`)");
        else
            print_func_loc(s, method);
        jl_printf(s, ".\n");
        jl_uv_flush(s);
    }
    if (jl_options.incremental && jl_generating_output())
        jl_printf(JL_STDERR, "  ** incremental compilation may be fatally broken for this module **\n\n");
}

static void update_max_args(jl_methtable_t *mt, jl_value_t *type)
{
    if (mt == jl_type_type_mt || mt == jl_nonfunction_mt)
        return;
    type = jl_unwrap_unionall(type);
    assert(jl_is_datatype(type));
    size_t na = jl_nparams(type);
    if (jl_va_tuple_kind((jl_datatype_t*)type) == JL_VARARG_UNBOUND)
        na--;
    if (na > mt->max_args)
        mt->max_args = na;
}

jl_array_t *_jl_debug_method_invalidation JL_GLOBALLY_ROOTED = NULL;
JL_DLLEXPORT jl_value_t *jl_debug_method_invalidation(int state)
{
    /* After calling with `state = 1`, caller is responsible for
       holding a reference to the returned array until this is called
       again with `state = 0`. */
    if (state) {
        if (_jl_debug_method_invalidation)
            return (jl_value_t*) _jl_debug_method_invalidation;
        _jl_debug_method_invalidation = jl_alloc_array_1d(jl_array_any_type, 0);
        return (jl_value_t*) _jl_debug_method_invalidation;
    }
    _jl_debug_method_invalidation = NULL;
    return jl_nothing;
}

// recursively invalidate cached methods that had an edge to a replaced method
static void invalidate_method_instance(jl_method_instance_t *replaced, size_t max_world, int depth)
{
    if (_jl_debug_method_invalidation) {
        jl_value_t *boxeddepth = NULL;
        JL_GC_PUSH1(&boxeddepth);
        jl_array_ptr_1d_push(_jl_debug_method_invalidation, (jl_value_t*)replaced);
        boxeddepth = jl_box_int32(depth);
        jl_array_ptr_1d_push(_jl_debug_method_invalidation, boxeddepth);
        JL_GC_POP();
    }
    if (!jl_is_method(replaced->def.method))
        return; // shouldn't happen, but better to be safe
    JL_LOCK_NOGC(&replaced->def.method->writelock);
    jl_code_instance_t *codeinst = replaced->cache;
    while (codeinst) {
        if (codeinst->max_world == ~(size_t)0) {
            assert(codeinst->min_world - 1 <= max_world && "attempting to set illogical world constraints (probable race condition)");
            codeinst->max_world = max_world;
        }
        assert(codeinst->max_world <= max_world);
        codeinst = jl_atomic_load_relaxed(&codeinst->next);
    }
    // recurse to all backedges to update their valid range also
    jl_array_t *backedges = replaced->backedges;
    if (backedges) {
        replaced->backedges = NULL;
        size_t i, l = jl_array_len(backedges);
        for (i = 0; i < l; i++) {
            jl_method_instance_t *replaced = (jl_method_instance_t*)jl_array_ptr_ref(backedges, i);
            invalidate_method_instance(replaced, max_world, depth + 1);
        }
    }
    JL_UNLOCK_NOGC(&replaced->def.method->writelock);
}

// invalidate cached methods that overlap this definition
static void invalidate_backedges(jl_method_instance_t *replaced_mi, size_t max_world, const char *why)
{
    JL_LOCK_NOGC(&replaced_mi->def.method->writelock);
    jl_array_t *backedges = replaced_mi->backedges;
    if (backedges) {
        // invalidate callers (if any)
        replaced_mi->backedges = NULL;
        size_t i, l = jl_array_len(backedges);
        jl_method_instance_t **replaced = (jl_method_instance_t**)jl_array_ptr_data(backedges);
        for (i = 0; i < l; i++) {
            invalidate_method_instance(replaced[i], max_world, 1);
        }
    }
    JL_UNLOCK_NOGC(&replaced_mi->def.method->writelock);
    if (why && _jl_debug_method_invalidation) {
        jl_array_ptr_1d_push(_jl_debug_method_invalidation, (jl_value_t*)replaced_mi);
        jl_value_t *loctag = jl_cstr_to_string(why);
        JL_GC_PUSH1(&loctag);
        jl_array_ptr_1d_push(_jl_debug_method_invalidation, loctag);
        JL_GC_POP();
    }
}

// add a backedge from callee to caller
JL_DLLEXPORT void jl_method_instance_add_backedge(jl_method_instance_t *callee, jl_method_instance_t *caller)
{
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

struct invalidate_mt_env {
    jl_typemap_entry_t *newentry;
    jl_array_t *shadowed;
    size_t max_world;
    int invalidated;
};
static int invalidate_mt_cache(jl_typemap_entry_t *oldentry, void *closure0)
{
    struct invalidate_mt_env *env = (struct invalidate_mt_env*)closure0;
    JL_GC_PROMISE_ROOTED(env->newentry);
    if (oldentry->max_world == ~(size_t)0) {
        jl_method_instance_t *mi = oldentry->func.linfo;
        int intersects = 0;
        jl_method_instance_t **d = (jl_method_instance_t**)jl_array_ptr_data(env->shadowed);
        size_t i, n = jl_array_len(env->shadowed);
        for (i = 0; i < n; i++) {
            if (mi == d[i]) {
                intersects = 1;
                break;
            }
        }
        if (intersects) {
            if (_jl_debug_method_invalidation) {
                jl_array_ptr_1d_push(_jl_debug_method_invalidation, (jl_value_t*)oldentry);
                jl_value_t *loctag = jl_cstr_to_string("invalidate_mt_cache");
                JL_GC_PUSH1(&loctag);
                jl_array_ptr_1d_push(_jl_debug_method_invalidation, loctag);
                JL_GC_POP();
            }
            oldentry->max_world = env->max_world;
            env->invalidated = 1;
        }
    }
    return 1;
}
static int disable_mt_cache(jl_typemap_entry_t *oldentry, void *closure0)
{
    struct invalidate_mt_env *env = (struct invalidate_mt_env*)closure0;
    if (oldentry->max_world < ~(size_t)0)
        return 1;
    jl_method_t *m = oldentry->func.linfo->def.method;
    if (m == env->newentry->func.method)
        oldentry->max_world = env->max_world;
    return 1;
}

static int typemap_search(jl_typemap_entry_t *entry, void *closure)
{
    if ((void*)(entry->func.method) == *(jl_method_t**)closure) {
        *(jl_typemap_entry_t**)closure = entry;
        return 0;
    }
    return 1;
}

static jl_typemap_entry_t *do_typemap_search(jl_methtable_t *mt JL_PROPAGATES_ROOT, jl_method_t *method) JL_NOTSAFEPOINT;

#ifndef __clang_analyzer__
static jl_typemap_entry_t *do_typemap_search(jl_methtable_t *mt JL_PROPAGATES_ROOT, jl_method_t *method) JL_NOTSAFEPOINT {
    jl_value_t *closure = (jl_value_t*)(method);
    if (jl_typemap_visitor(mt->defs, typemap_search, &closure))
        jl_error("method not in method table");
    return (jl_typemap_entry_t *)closure;
}
#endif

static void jl_method_table_invalidate(jl_methtable_t *mt, jl_typemap_entry_t *methodentry, jl_method_t *method, size_t max_world)
{
    method->deleted_world = methodentry->max_world = max_world;
    // drop this method from mt->cache
    struct invalidate_mt_env mt_cache_env;
    mt_cache_env.max_world = max_world;
    mt_cache_env.newentry = methodentry;
    mt_cache_env.shadowed = NULL;
    mt_cache_env.invalidated = 0;
    jl_typemap_visitor(mt->cache, disable_mt_cache, (void*)&mt_cache_env);
    jl_array_t *leafcache = mt->leafcache;
    size_t i, l = jl_array_len(leafcache);
    for (i = 1; i < l; i += 2) {
        jl_typemap_entry_t *oldentry = (jl_typemap_entry_t*)jl_array_ptr_ref(leafcache, i);
        if (oldentry) {
            while ((jl_value_t*)oldentry != jl_nothing) {
                if (oldentry->max_world == ~(size_t)0)
                    oldentry->max_world = mt_cache_env.max_world;
                oldentry = oldentry->next;
            }
        }
    }
    // Invalidate the backedges
    int invalidated = 0;
    jl_svec_t *specializations = methodentry->func.method->specializations;
    l = jl_svec_len(specializations);
    for (i = 0; i < l; i++) {
        jl_method_instance_t *mi = (jl_method_instance_t*)jl_svecref(specializations, i);
        if (mi) {
            invalidated = 1;
            invalidate_backedges(mi, methodentry->max_world, "jl_method_table_disable");
        }
    }
    if (invalidated && _jl_debug_method_invalidation) {
        jl_array_ptr_1d_push(_jl_debug_method_invalidation, (jl_value_t*)method);
        jl_value_t *loctag = jl_cstr_to_string("jl_method_table_disable");
        JL_GC_PUSH1(&loctag);
        jl_array_ptr_1d_push(_jl_debug_method_invalidation, loctag);
        JL_GC_POP();
    }
}

JL_DLLEXPORT void jl_method_table_disable(jl_methtable_t *mt, jl_method_t *method)
{
    if (jl_options.incremental && jl_generating_output())
        jl_printf(JL_STDERR, "WARNING: method deletion during Module precompile may lead to undefined behavior"
                             "\n  ** incremental compilation may be fatally broken for this module **\n\n");
    jl_typemap_entry_t *methodentry = do_typemap_search(mt, method);
    JL_LOCK(&mt->writelock);
    // Narrow the world age on the method to make it uncallable
    jl_method_table_invalidate(mt, methodentry, method, jl_world_counter++);
    JL_UNLOCK(&mt->writelock);
}

JL_DLLEXPORT void jl_method_table_insert(jl_methtable_t *mt, jl_method_t *method, jl_tupletype_t *simpletype)
{
    JL_TIMING(ADD_METHOD);
    assert(jl_is_method(method));
    assert(jl_is_mtable(mt));
    jl_value_t *type = method->sig;
    jl_value_t *oldvalue = NULL;
    jl_array_t *oldmi = NULL;
    if (method->primary_world == 1)
        method->primary_world = ++jl_world_counter;
    size_t max_world = method->primary_world - 1;
    jl_value_t *loctag = NULL;  // debug info for invalidation
    jl_value_t *isect = NULL;
    jl_typemap_entry_t *newentry = NULL;
    JL_GC_PUSH5(&oldvalue, &oldmi, &newentry, &loctag, &isect);
    JL_LOCK(&mt->writelock);
    // first find if we have an existing entry to delete
    struct jl_typemap_assoc search = {(jl_value_t*)type, method->primary_world, NULL, 0, ~(size_t)0};
    jl_typemap_entry_t *oldentry = jl_typemap_assoc_by_type(mt->defs, &search, /*offs*/0, /*subtype*/0);
    // then add our new entry
    newentry = jl_typemap_alloc((jl_tupletype_t*)type, simpletype, jl_emptysvec,
            (jl_value_t*)method, method->primary_world, method->deleted_world);
    jl_typemap_insert(&mt->defs, (jl_value_t*)mt, newentry, 0, &method_defs);
    if (oldentry) {
        jl_method_t *m = oldentry->func.method;
        method_overwrite(newentry, m);
        jl_method_table_invalidate(mt, oldentry, m, max_world);
    }
    else {
        oldvalue = get_intersect_matches(mt->defs, newentry);

        int invalidated = 0;
        jl_method_t **d;
        size_t j, n;
        if (oldvalue == NULL) {
            d = NULL;
            n = 0;
        }
        else {
            assert(jl_is_array(oldvalue));
            d = (jl_method_t**)jl_array_ptr_data(oldvalue);
            n = jl_array_len(oldvalue);
        }
        if (mt->backedges) {
            jl_value_t **backedges = jl_array_ptr_data(mt->backedges);
            size_t i, na = jl_array_len(mt->backedges);
            size_t ins = 0;
            for (i = 1; i < na; i += 2) {
                jl_value_t *backedgetyp = backedges[i - 1];
                isect = jl_type_intersection(backedgetyp, (jl_value_t*)type);
                if (isect != jl_bottom_type) {
                    // see if the intersection was actually already fully
                    // covered by anything (method or ambiguity is okay)
                    size_t j;
                    for (j = 0; j < n; j++) {
                        jl_method_t *m = d[j];
                        if (jl_subtype(isect, m->sig))
                            break;
                    }
                    if (j != n)
                        isect = jl_bottom_type;
                }
                if (isect != jl_bottom_type) {
                    jl_method_instance_t *backedge = (jl_method_instance_t*)backedges[i];
                    invalidate_method_instance(backedge, max_world, 0);
                    invalidated = 1;
                    if (_jl_debug_method_invalidation)
                        jl_array_ptr_1d_push(_jl_debug_method_invalidation, (jl_value_t*)backedgetyp);
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
        if (oldvalue) {
            oldmi = jl_alloc_vec_any(0);
            enum morespec_options {
                morespec_unknown,
                morespec_isnot,
                morespec_is
            };
            char *morespec = (char*)alloca(n);
            memset(morespec, morespec_unknown, n);
            for (j = 0; j < n; j++) {
                jl_method_t *m = d[j];
                if (morespec[j] == (char)morespec_is)
                    continue;
                jl_svec_t *specializations = jl_atomic_load_acquire(&m->specializations);
                jl_method_instance_t **data = (jl_method_instance_t**)jl_svec_data(specializations);
                size_t i, l = jl_svec_len(specializations);
                enum morespec_options ambig = morespec_unknown;
                for (i = 0; i < l; i++) {
                    jl_method_instance_t *mi = jl_atomic_load_relaxed(&data[i]);
                    if (mi == NULL)
                        continue;
                    isect = jl_type_intersection(m->sig, (jl_value_t*)mi->specTypes);
                    isect = jl_type_intersection(type, isect);
                    if (isect != jl_bottom_type) {
                        if (morespec[j] == (char)morespec_unknown)
                            morespec[j] = (char)jl_type_morespecific(m->sig, type) ? morespec_is : morespec_isnot;
                        if (morespec[j] == (char)morespec_is)
                            // not actually shadowing--the existing method is still better
                            break;
                        if (ambig == morespec_unknown)
                            ambig = jl_type_morespecific(type, m->sig) ? morespec_is : morespec_isnot;
                        // replacing a method--see if this really was the selected method previously
                        // over the intersection
                        if (ambig == morespec_isnot)  {
                            size_t k;
                            for (k = 0; k < n; k++) {
                                jl_method_t *m2 = d[k];
                                if (m == m2 || !jl_subtype(isect, m2->sig))
                                    continue;
                                if (morespec[k] == (char)morespec_unknown)
                                    morespec[k] = (char)jl_type_morespecific(m2->sig, type) ? morespec_is : morespec_isnot;
                                if (morespec[k] == (char)morespec_is)
                                    // not actually shadowing this--m2 will still be better
                                    break;
                                // since m2 was also a previous match over isect,
                                // see if m was also previously dominant over all m2
                                if (!jl_type_morespecific(m->sig, m2->sig))
                                    break;
                            }
                            if (k != n)
                                continue;
                        }
                        jl_array_ptr_1d_push(oldmi, (jl_value_t*)mi);
                        if (mi->backedges) {
                            invalidated = 1;
                            invalidate_backedges(mi, max_world, "jl_method_table_insert");
                        }
                    }
                }
            }
            if (jl_array_len(oldmi)) {
                // search mt->cache and leafcache and drop anything that might overlap with the new method
                // TODO: keep track of just the `mi` for which shadowing was true (to avoid recomputing that here)
                struct invalidate_mt_env mt_cache_env;
                mt_cache_env.max_world = max_world;
                mt_cache_env.shadowed = oldmi;
                mt_cache_env.newentry = newentry;
                mt_cache_env.invalidated = 0;

                jl_typemap_visitor(mt->cache, invalidate_mt_cache, (void*)&mt_cache_env);
                jl_array_t *leafcache = mt->leafcache;
                size_t i, l = jl_array_len(leafcache);
                for (i = 1; i < l; i += 2) {
                    jl_value_t *entry = jl_array_ptr_ref(leafcache, i);
                    if (entry) {
                        while (entry != jl_nothing) {
                            invalidate_mt_cache((jl_typemap_entry_t*)entry, (void*)&mt_cache_env);
                            entry = (jl_value_t*)((jl_typemap_entry_t*)entry)->next;
                        }
                    }
                }
            }
        }
        if (invalidated && _jl_debug_method_invalidation) {
            jl_array_ptr_1d_push(_jl_debug_method_invalidation, (jl_value_t*)method);
            loctag = jl_cstr_to_string("jl_method_table_insert");
            jl_array_ptr_1d_push(_jl_debug_method_invalidation, loctag);
        }
        update_max_args(mt, type);
    }
    JL_UNLOCK(&mt->writelock);
    JL_GC_POP();
}

static void JL_NORETURN jl_method_error_bare(jl_function_t *f, jl_value_t *args, size_t world)
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
        ptls->bt_size = rec_backtrace(ptls->bt_data, JL_MAX_BT_SIZE, 0);
        jl_critical_error(0, NULL, ptls->bt_data, &ptls->bt_size);
        abort();
    }
    // not reached
}

void JL_NORETURN jl_method_error(jl_function_t *f, jl_value_t **args, size_t na, size_t world)
{
    jl_value_t *argtup = jl_f_tuple(NULL, args, na - 1);
    JL_GC_PUSH1(&argtup);
    jl_method_error_bare(f, argtup, world);
    // not reached
}

jl_tupletype_t *arg_type_tuple(jl_value_t *arg1, jl_value_t **args, size_t nargs)
{
    return jl_inst_arg_tuple_type(arg1, args, nargs, 1);
}

jl_tupletype_t *lookup_arg_type_tuple(jl_value_t *arg1 JL_PROPAGATES_ROOT, jl_value_t **args, size_t nargs)
{
    return jl_lookup_arg_tuple_type(arg1, args, nargs, 1);
}

jl_method_instance_t *jl_method_lookup(jl_value_t **args, size_t nargs, size_t world)
{
    assert(nargs > 0 && "expected caller to handle this case");
    jl_methtable_t *mt = jl_gf_mtable(args[0]);
    jl_typemap_entry_t *entry = jl_typemap_assoc_exact(mt->cache, args[0], &args[1], nargs, jl_cachearg_offset(mt), world);
    if (entry)
        return entry->func.linfo;
    jl_tupletype_t *tt = arg_type_tuple(args[0], &args[1], nargs);
    jl_array_t *leafcache = jl_atomic_load_relaxed(&mt->leafcache);
    entry = lookup_leafcache(leafcache, (jl_value_t*)tt, world);
    if (entry)
        return entry->func.linfo;
    JL_GC_PUSH1(&tt);
    JL_LOCK(&mt->writelock);
    jl_method_instance_t *sf = jl_mt_assoc_by_type(mt, tt, world);
    JL_GC_POP();
    JL_UNLOCK(&mt->writelock);
    return sf;
}

// return a Vector{Any} of svecs, each describing a method match:
// Any[svec(tt, spvals, m, full), ...]
// tt is the intersection of the type argument and the method signature,
// spvals is any matched static parameter values, m is the Method,
// full is a boolean indicating if that method fully covers the input
//
// lim is the max # of methods to return. if there are more, returns jl_false.
// -1 for no limit.
JL_DLLEXPORT jl_value_t *jl_matching_methods(jl_tupletype_t *types, int lim, int include_ambiguous,
                                             size_t world, size_t *min_valid, size_t *max_valid, int *ambig)
{
    JL_TIMING(METHOD_MATCH);
    *ambig = 0;
    jl_value_t *unw = jl_unwrap_unionall((jl_value_t*)types);
    if (jl_is_tuple_type(unw) && jl_tparam0(unw) == jl_bottom_type)
        return (jl_value_t*)jl_an_empty_vec_any;
    jl_methtable_t *mt = jl_method_table_for(unw);
    if ((jl_value_t*)mt == jl_nothing)
        return jl_false; // indeterminate - ml_matches can't deal with this case
    return ml_matches(mt, 0, types, lim, include_ambiguous, 1, world, 1, min_valid, max_valid, ambig);
}

jl_method_instance_t *jl_get_unspecialized(jl_method_instance_t *method JL_PROPAGATES_ROOT)
{
    // one unspecialized version of a function can be shared among all cached specializations
    jl_method_t *def = method->def.method;
    if (!jl_is_method(def) || def->source == NULL) {
        // generated functions might instead randomly just never get inferred, sorry
        return method;
    }
    if (def->unspecialized == NULL) {
        JL_LOCK(&def->writelock);
        if (def->unspecialized == NULL) {
            def->unspecialized = jl_get_specialized(def, def->sig, jl_emptysvec);
            jl_gc_wb(def, def->unspecialized);
        }
        JL_UNLOCK(&def->writelock);
    }
    return def->unspecialized;
}


jl_code_instance_t *jl_method_compiled(jl_method_instance_t *mi, size_t world)
{
    jl_code_instance_t *codeinst;
    codeinst = mi->cache;

    while (codeinst) {
        if (codeinst->min_world <= world && world <= codeinst->max_world && codeinst->invoke != NULL) {
            return codeinst;
        }
        codeinst = jl_atomic_load_relaxed(&codeinst->next);
    }
    return NULL;
}

jl_code_instance_t *jl_compile_method_internal(jl_method_instance_t *mi, size_t world)
{
    jl_code_instance_t *codeinst = jl_method_compiled(mi, world);
    if (codeinst)
        return codeinst;

    if (jl_options.compile_enabled == JL_OPTIONS_COMPILE_OFF ||
        jl_options.compile_enabled == JL_OPTIONS_COMPILE_MIN) {
        // copy fptr from the template method definition
        jl_method_t *def = mi->def.method;
        if (jl_is_method(def) && def->unspecialized) {
            jl_code_instance_t *unspec = def->unspecialized->cache;
            if (unspec && unspec->invoke != NULL) {
                jl_code_instance_t *codeinst = jl_new_codeinst(mi,
                    (jl_value_t*)jl_any_type, NULL, NULL,
                    0, 1, ~(size_t)0);
                codeinst->isspecsig = 0;
                codeinst->specptr = unspec->specptr;
                codeinst->rettype_const = unspec->rettype_const;
                jl_atomic_store_release(&codeinst->invoke, unspec->invoke);
                jl_mi_cache_insert(mi, codeinst);
                return codeinst;
            }
        }
        jl_code_info_t *src = jl_code_for_interpreter(mi);
        if (!jl_code_requires_compiler(src)) {
            jl_code_instance_t *codeinst = jl_new_codeinst(mi,
                (jl_value_t*)jl_any_type, NULL, NULL,
                0, 1, ~(size_t)0);
            jl_atomic_store_release(&codeinst->invoke, jl_fptr_interpret_call);
            jl_mi_cache_insert(mi, codeinst);
            return codeinst;
        }
        if (jl_options.compile_enabled == JL_OPTIONS_COMPILE_OFF) {
            jl_printf(JL_STDERR, "code missing for ");
            jl_static_show(JL_STDERR, (jl_value_t*)mi);
            jl_printf(JL_STDERR, " : sysimg may not have been built with --compile=all\n");
        }
    }

    codeinst = jl_generate_fptr(mi, world);
    if (!codeinst) {
        jl_method_instance_t *unspec = jl_get_unspecialized(mi);
        jl_code_instance_t *ucache = jl_get_method_inferred(unspec, (jl_value_t*)jl_any_type, 1, ~(size_t)0);
        // ask codegen to make the fptr for unspec
        if (ucache->invoke == NULL)
            jl_generate_fptr_for_unspecialized(ucache);
        if (ucache->invoke != jl_fptr_sparam &&
            ucache->invoke != jl_fptr_interpret_call) {
            return ucache;
        }
        codeinst = jl_new_codeinst(mi, (jl_value_t*)jl_any_type, NULL, NULL,
            0, 1, ~(size_t)0);
        codeinst->isspecsig = 0;
        codeinst->specptr = ucache->specptr;
        codeinst->rettype_const = ucache->rettype_const;
        jl_atomic_store_release(&codeinst->invoke, ucache->invoke);
        jl_mi_cache_insert(mi, codeinst);
    }
    codeinst->precompile = 1;
    return codeinst;
}


JL_DLLEXPORT jl_value_t *jl_fptr_const_return(jl_value_t *f, jl_value_t **args, uint32_t nargs, jl_code_instance_t *m)
{
    return m->rettype_const;
}

JL_DLLEXPORT jl_value_t *jl_fptr_args(jl_value_t *f, jl_value_t **args, uint32_t nargs, jl_code_instance_t *m)
{
    return m->specptr.fptr1(f, args, nargs);
}

JL_DLLEXPORT jl_value_t *jl_fptr_sparam(jl_value_t *f, jl_value_t **args, uint32_t nargs, jl_code_instance_t *m)
{
    jl_svec_t *sparams = m->def->sparam_vals;
    assert(sparams != jl_emptysvec);
    return m->specptr.fptr3(f, args, nargs, sparams);
}

// Return the index of the invoke api, if known
JL_DLLEXPORT int32_t jl_invoke_api(jl_code_instance_t *codeinst)
{
    jl_callptr_t f = codeinst->invoke;
    if (f == NULL)
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
jl_method_instance_t *jl_get_specialization1(jl_tupletype_t *types JL_PROPAGATES_ROOT, size_t world, size_t *min_valid, size_t *max_valid, int mt_cache)
{
    JL_TIMING(METHOD_LOOKUP_COMPILE);
    if (jl_has_free_typevars((jl_value_t*)types))
        return NULL; // don't poison the cache due to a malformed query
    if (!jl_has_concrete_subtype((jl_value_t*)types))
        return NULL;

    // find if exactly 1 method matches (issue #7302)
    size_t min_valid2 = 1;
    size_t max_valid2 = ~(size_t)0;
    int ambig = 0;
    jl_value_t *matches = jl_matching_methods(types, 1, 1, world, &min_valid2, &max_valid2, &ambig);
    if (*min_valid < min_valid2)
        *min_valid = min_valid2;
    if (*max_valid > max_valid2)
        *max_valid = max_valid2;
    if (matches == jl_false || jl_array_len(matches) != 1 || ambig)
        return NULL;
    jl_tupletype_t *tt = NULL;
    jl_svec_t *newparams = NULL;
    JL_GC_PUSH3(&matches, &tt, &newparams);
    jl_method_match_t *match = (jl_method_match_t*)jl_array_ptr_ref(matches, 0);
    jl_method_t *m = match->method;
    jl_svec_t *env = match->sparams;
    jl_tupletype_t *ti = match->spec_types;
    jl_method_instance_t *nf = NULL;
    if (jl_is_datatype(ti)) {
        jl_methtable_t *mt = jl_method_table_for((jl_value_t*)ti);
        if ((jl_value_t*)mt != jl_nothing) {
            // get the specialization without caching it
            if (mt_cache && ((jl_datatype_t*)ti)->isdispatchtuple) {
                // Since we also use this presence in the cache
                // to trigger compilation when producing `.ji` files,
                // inject it there now if we think it will be
                // used via dispatch later (e.g. because it was hinted via a call to `precompile`)
                JL_LOCK(&mt->writelock);
                nf = cache_method(mt, &mt->cache, (jl_value_t*)mt, ti, m, world, min_valid2, max_valid2, env);
                JL_UNLOCK(&mt->writelock);
            }
            else {
                intptr_t nspec = (mt == jl_type_type_mt || mt == jl_nonfunction_mt ? m->nargs + 1 : mt->max_args + 2);
                jl_compilation_sig(ti, env, m, nspec, &newparams);
                tt = (newparams ? jl_apply_tuple_type(newparams) : ti);
                int is_compileable = ((jl_datatype_t*)ti)->isdispatchtuple ||
                    jl_isa_compileable_sig(tt, m);
                if (is_compileable) {
                    nf = jl_specializations_get_linfo(m, (jl_value_t*)tt, env);
                }
            }
        }
    }
    JL_GC_POP();
    return nf;
}

static void _generate_from_hint(jl_method_instance_t *mi, size_t world)
{
    int generating_llvm = jl_options.outputo || jl_options.outputbc || jl_options.outputunoptbc || jl_options.outputasm;
    // If we are saving ji files (e.g. package pre-compilation or intermediate sysimg build steps),
    // don't bother generating anything since it won't be saved.
    if (jl_rettype_inferred(mi, world, world) == jl_nothing)
        (void)jl_type_infer(mi, world, 1);
    // If we are saving ji files (e.g. package pre-compilation or intermediate sysimg build steps),
    // don't bother generating output in the current environment
    if (generating_llvm) {
        jl_value_t *codeinst = jl_rettype_inferred(mi, world, world);
        if (codeinst != jl_nothing) {
            if (((jl_code_instance_t*)codeinst)->invoke == jl_fptr_const_return)
                return; // probably not a good idea to generate code
            ((jl_code_instance_t*)codeinst)->precompile = 1;
        }
        // If we are saving LLVM or native code, generate the LLVM IR so that it'll
        // be included in the saved LLVM module.
        // TODO: compilation is now stateless
    }
}

void jl_compile_now(jl_method_instance_t *mi)
{
    size_t world = jl_world_counter;
    size_t tworld = jl_typeinf_world;
    _generate_from_hint(mi, world);
    if (jl_typeinf_func && mi->def.method->primary_world <= tworld) {
        // if it's part of the compiler, also attempt to compile for the compiler world too
        _generate_from_hint(mi, tworld);
    }
}

JL_DLLEXPORT int jl_compile_hint(jl_tupletype_t *types)
{
    size_t world = jl_world_counter;
    size_t tworld = jl_typeinf_world;
    size_t min_valid = 0;
    size_t max_valid = ~(size_t)0;
    jl_method_instance_t *mi = jl_get_specialization1(types, world, &min_valid, &max_valid, 1);
    if (mi == NULL)
        return 0;
    JL_GC_PROMISE_ROOTED(mi);
    if (jl_generating_output()) {
        jl_compile_now(mi);
        // In addition to full compilation of the compilation-signature, if `types` is more specific (e.g. due to nospecialize),
        // also run inference now on the original `types`, since that may help us guide inference to find
        // additional useful methods that should be compiled
        //ALT: if (jl_is_datatype(types) && ((jl_datatype_t*)types)->isdispatchtuple && !jl_egal(mi->specTypes, types))
        //ALT: if (jl_subtype(types, mi->specTypes))
        if (!jl_subtype(mi->specTypes, (jl_value_t*)types)) {
            jl_svec_t *tpenv2 = jl_emptysvec;
            jl_value_t *types2 = NULL;
            JL_GC_PUSH2(&tpenv2, &types2);
            types2 = jl_type_intersection_env((jl_value_t*)types, (jl_value_t*)mi->def.method->sig, &tpenv2);
            jl_method_instance_t *li2 = jl_specializations_get_linfo(mi->def.method, (jl_value_t*)types2, tpenv2);
            JL_GC_POP();
            if (jl_rettype_inferred(li2, world, world) == jl_nothing)
                (void)jl_type_infer(li2, world, 1);
            if (jl_typeinf_func && mi->def.method->primary_world <= tworld) {
                if (jl_rettype_inferred(li2, tworld, tworld) == jl_nothing)
                    (void)jl_type_infer(li2, tworld, 1);
            }
        }
    }
    else {
        // Otherwise (this branch), assuming we are at runtime (normal JIT) and
        // we should generate the native code immediately in preparation for use.
        (void)jl_compile_method_internal(mi, world);
    }
    return 1;
}

JL_DLLEXPORT jl_value_t *jl_get_spec_lambda(jl_tupletype_t *types, size_t world, size_t *min_valid, size_t *max_valid)
{
    jl_method_instance_t *mi = jl_get_specialization1(types, world, min_valid, max_valid, 0);
    if (!mi)
        return jl_nothing;
    return (jl_value_t*)mi;
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

STATIC_INLINE jl_value_t *verify_type(jl_value_t *v) JL_NOTSAFEPOINT
{
    assert(v && jl_typeof(v) && jl_typeof(jl_typeof(v)) == (jl_value_t*)jl_datatype_type);
    return v;
}

STATIC_INLINE jl_value_t *_jl_invoke(jl_value_t *F, jl_value_t **args, uint32_t nargs, jl_method_instance_t *mfunc, size_t world)
{
    // manually inlined copy of jl_method_compiled
    jl_code_instance_t *codeinst = mfunc->cache;
    while (codeinst) {
        if (codeinst->min_world <= world && world <= codeinst->max_world && codeinst->invoke != NULL) {
            jl_value_t *res = codeinst->invoke(F, args, nargs, codeinst);
            return verify_type(res);
        }
        codeinst = jl_atomic_load_relaxed(&codeinst->next);
    }
    int64_t last_alloc = jl_options.malloc_log ? jl_gc_diff_total_bytes() : 0;
    int last_errno = errno;
#ifdef _OS_WINDOWS_
    DWORD last_error = GetLastError();
#endif
    codeinst = jl_compile_method_internal(mfunc, world);
#ifdef _OS_WINDOWS_
    SetLastError(last_error);
#endif
    errno = last_errno;
    if (jl_options.malloc_log)
        jl_gc_sync_total_bytes(last_alloc); // discard allocation count from compilation
    jl_value_t *res = codeinst->invoke(F, args, nargs, codeinst);
    return verify_type(res);
}

JL_DLLEXPORT jl_value_t *jl_invoke(jl_value_t *F, jl_value_t **args, uint32_t nargs, jl_method_instance_t *mfunc)
{
    size_t world = jl_get_ptls_states()->world_age;
    return _jl_invoke(F, args, nargs, mfunc, world);
}

STATIC_INLINE int sig_match_fast(jl_value_t *arg1t, jl_value_t **args, jl_value_t **sig, size_t n)
{
    // NOTE: This function is a huge performance hot spot!!
    if (arg1t != sig[0])
        return 0;
    size_t i;
    for (i = 1; i < n; i++) {
        jl_value_t *decl = sig[i];
        jl_value_t *a = args[i - 1];
        if (jl_typeof(a) != decl) {
            /*
              we are only matching concrete types here, and those types are
              hash-consed, so pointer comparison should work.
            */
            return 0;
        }
    }
    return 1;
}

jl_typemap_entry_t *call_cache[N_CALL_CACHE] JL_GLOBALLY_ROOTED;
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

STATIC_INLINE jl_method_instance_t *jl_lookup_generic_(jl_value_t *F, jl_value_t **args, uint32_t nargs,
                                                       uint32_t callsite, size_t world)
{
#ifdef JL_GF_PROFILE
    ncalls++;
#endif
#ifdef JL_TRACE
    int traceen = trace_en; //&& ((char*)&mt < jl_stack_hi-6000000);
    if (traceen)
        show_call(F, args, nargs);
#endif
    nargs++; // add F to argument count
    jl_value_t *FT = jl_typeof(F);

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
    //#pragma unroll
    //for (i = 0; i < 4; i++) {
    //    LOOP_BODY(i);
    //}
#define LOOP_BODY(_i) do { \
            i = _i; \
            entry = call_cache[cache_idx[i]]; \
            if (entry && nargs == jl_svec_len(entry->sig->parameters) && \
                sig_match_fast(FT, args, jl_svec_data(entry->sig->parameters), nargs) && \
                world >= entry->min_world && world <= entry->max_world) { \
                goto have_entry; \
            } \
        } while (0);
    LOOP_BODY(0);
    LOOP_BODY(1);
    LOOP_BODY(2);
    LOOP_BODY(3);
#undef LOOP_BODY
    i = 4;
    jl_tupletype_t *tt = NULL;
    int64_t last_alloc;
    if (i == 4) {
        // if no method was found in the associative cache, check the full cache
        JL_TIMING(METHOD_LOOKUP_FAST);
        mt = jl_gf_mtable(F);
        jl_array_t *leafcache = jl_atomic_load_relaxed(&mt->leafcache);
        entry = NULL;
        if (leafcache != (jl_array_t*)jl_an_empty_vec_any && jl_typeis(mt->cache, jl_typemap_level_type)) {
            // hashing args is expensive, but looking at mt->cache is probably even more expensive
            tt = lookup_arg_type_tuple(F, args, nargs);
            if (tt != NULL)
                entry = lookup_leafcache(leafcache, (jl_value_t*)tt, world);
        }
        if (entry == NULL) {
            entry = jl_typemap_assoc_exact(mt->cache, F, args, nargs, jl_cachearg_offset(mt), world);
            if (entry == NULL) {
                last_alloc = jl_options.malloc_log ? jl_gc_diff_total_bytes() : 0;
                if (tt == NULL) {
                    tt = arg_type_tuple(F, args, nargs);
                    entry = lookup_leafcache(leafcache, (jl_value_t*)tt, world);
                }
            }
        }
        if (entry != NULL && entry->isleafsig && entry->simplesig == (void*)jl_nothing && entry->guardsigs == jl_emptysvec) {
            // put the entry into the cache if it's valid for a leafsig lookup,
            // using pick_which to slightly randomize where it ends up
            call_cache[cache_idx[++pick_which[cache_idx[0]] & 3]] = entry;
        }
    }

    jl_method_instance_t *mfunc;
    if (entry) {
have_entry:
        mfunc = entry->func.linfo;
    }
    else {
        JL_GC_PUSH1(&tt);
        assert(tt);
        JL_LOCK(&mt->writelock);
        // cache miss case
        JL_TIMING(METHOD_LOOKUP_SLOW);
        mfunc = jl_mt_assoc_by_type(mt, tt, world);
        JL_GC_POP();
        JL_UNLOCK(&mt->writelock);
        if (jl_options.malloc_log)
            jl_gc_sync_total_bytes(last_alloc); // discard allocation count from compilation
        if (mfunc == NULL) {
#ifdef JL_TRACE
            if (error_en)
                show_call(F, args, nargs);
#endif
            jl_method_error(F, args, nargs, world);
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
    return jl_lookup_generic_(args[0], &args[1], nargs - 1, callsite, world);
}

JL_DLLEXPORT jl_value_t *jl_apply_generic(jl_value_t *F, jl_value_t **args, uint32_t nargs)
{
    size_t world = jl_get_ptls_states()->world_age;
    jl_method_instance_t *mfunc = jl_lookup_generic_(F, args, nargs,
                                                     jl_int32hash_fast(jl_return_address()),
                                                     world);
    JL_GC_PROMISE_ROOTED(mfunc);
    return _jl_invoke(F, args, nargs, mfunc, world);
}

static jl_method_match_t *_gf_invoke_lookup(jl_value_t *types JL_PROPAGATES_ROOT, size_t world, size_t *min_valid, size_t *max_valid)
{
    jl_value_t *unw = jl_unwrap_unionall((jl_value_t*)types);
    if (jl_is_tuple_type(unw) && jl_tparam0(unw) == jl_bottom_type)
        return NULL;
    jl_methtable_t *mt = jl_method_table_for(unw);
    if ((jl_value_t*)mt == jl_nothing)
        return NULL;
    int ambig = 0;
    jl_value_t *matches = ml_matches(mt, 0, (jl_tupletype_t*)types, 1, 0, 0, world, 1, min_valid, max_valid, &ambig);
    if (matches == jl_false || jl_array_len(matches) != 1)
        return NULL;
    jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(matches, 0);
    return matc;
}

JL_DLLEXPORT jl_value_t *jl_gf_invoke_lookup(jl_value_t *types, size_t world)
{
    // Deprecated: Use jl_gf_invoke_lookup_worlds for future development
    size_t min_valid = 0;
    size_t max_valid = ~(size_t)0;
    jl_method_match_t *matc = _gf_invoke_lookup(types, world, &min_valid, &max_valid);
    if (matc == NULL)
        return jl_nothing;
    return (jl_value_t*)matc->method;
}


JL_DLLEXPORT jl_value_t *jl_gf_invoke_lookup_worlds(jl_value_t *types, size_t world, size_t *min_world, size_t *max_world)
{
    jl_method_match_t *matc = _gf_invoke_lookup(types, world, min_world, max_world);
    if (matc == NULL)
        return jl_nothing;
    return (jl_value_t*)matc->method;
}

static jl_value_t *jl_gf_invoke_by_method(jl_method_t *method, jl_value_t *gf, jl_value_t **args, size_t nargs);

// invoke()
// this does method dispatch with a set of types to match other than the
// types of the actual arguments. this means it sometimes does NOT call the
// most specific method for the argument types, so we need different logic.
// first we use the given types to look up a definition, then we perform
// caching and specialization within just that definition.
// every definition has its own private method table for this purpose.
//
// NOTE: assumes argument type is a subtype of the lookup type.
jl_value_t *jl_gf_invoke(jl_value_t *types0, jl_value_t *gf, jl_value_t **args, size_t nargs)
{
    size_t world = jl_get_ptls_states()->world_age;
    jl_value_t *types = NULL;
    JL_GC_PUSH1(&types);
    types = jl_argtype_with_function(gf, types0);
    jl_method_t *method = (jl_method_t*)jl_gf_invoke_lookup(types, world);
    JL_GC_PROMISE_ROOTED(method);

    if ((jl_value_t*)method == jl_nothing) {
        jl_method_error_bare(gf, types0, world);
        // unreachable
    }

    // now we have found the matching definition.
    // next look for or create a specialization of this definition.
    JL_GC_POP();
    return jl_gf_invoke_by_method(method, gf, args, nargs);
}

static jl_value_t *jl_gf_invoke_by_method(jl_method_t *method, jl_value_t *gf, jl_value_t **args, size_t nargs)
{
    jl_method_instance_t *mfunc = NULL;
    jl_typemap_entry_t *tm = NULL;
    if (method->invokes != NULL)
        tm = jl_typemap_assoc_exact(method->invokes, gf, args, nargs, 1, 1);
    if (tm) {
        mfunc = tm->func.linfo;
    }
    else {
        int64_t last_alloc = jl_options.malloc_log ? jl_gc_diff_total_bytes() : 0;
        jl_svec_t *tpenv = jl_emptysvec;
        jl_tupletype_t *tt = NULL;
        JL_GC_PUSH2(&tpenv, &tt);
        JL_LOCK(&method->writelock);
        tt = arg_type_tuple(gf, args, nargs);
        if (jl_is_unionall(method->sig)) {
            int sub = jl_subtype_matching((jl_value_t*)tt, (jl_value_t*)method->sig, &tpenv);
            assert(sub); (void)sub;
        }

        if (method->invokes == NULL)
            method->invokes = jl_nothing;

        mfunc = cache_method(NULL, &method->invokes, (jl_value_t*)method, tt, method, 1, 1, ~(size_t)0, tpenv);
        JL_UNLOCK(&method->writelock);
        JL_GC_POP();
        if (jl_options.malloc_log)
            jl_gc_sync_total_bytes(last_alloc); // discard allocation count from compilation
    }
    JL_GC_PROMISE_ROOTED(mfunc);
    size_t world = jl_get_ptls_states()->world_age;
    return _jl_invoke(gf, args, nargs - 1, mfunc, world);
}

JL_DLLEXPORT jl_value_t *jl_get_invoke_lambda(jl_method_t *method, jl_value_t *tt)
{
    // TODO: refactor this method to be more like `jl_get_specialization1`
    if (!jl_is_datatype(tt) || !((jl_datatype_t*)tt)->isdispatchtuple)
        return jl_nothing;

    jl_typemap_entry_t *tm = NULL;
    struct jl_typemap_assoc search = {(jl_value_t*)tt, 1, NULL, 0, ~(size_t)0};
    if (method->invokes != NULL) {
        tm = jl_typemap_assoc_by_type(method->invokes, &search, /*offs*/1, /*subtype*/1);
        if (tm) {
            return (jl_value_t*)tm->func.linfo;
        }
    }

    JL_LOCK(&method->writelock);
    if (method->invokes != NULL) {
        tm = jl_typemap_assoc_by_type(method->invokes, &search, /*offs*/1, /*subtype*/1);
        if (tm) {
            jl_method_instance_t *mfunc = tm->func.linfo;
            JL_UNLOCK(&method->writelock);
            return (jl_value_t*)mfunc;
        }
    }

    jl_svec_t *tpenv = jl_emptysvec;
    JL_GC_PUSH1(&tpenv);
    if (jl_is_unionall(method->sig)) {
        jl_value_t *ti =
            jl_type_intersection_env(tt, (jl_value_t*)method->sig, &tpenv);
        assert(ti != (jl_value_t*)jl_bottom_type);
        (void)ti;
    }

    if (method->invokes == NULL)
        method->invokes = jl_nothing;

    jl_method_instance_t *mfunc = cache_method(NULL, &method->invokes, (jl_value_t*)method,
                                               (jl_tupletype_t*)tt, method, 1, 1, ~(size_t)0, tpenv);
    JL_GC_POP();
    JL_UNLOCK(&method->writelock);
    return (jl_value_t*)mfunc;
}

// Return value is rooted globally
jl_function_t *jl_new_generic_function_with_supertype(jl_sym_t *name, jl_module_t *module, jl_datatype_t *st)
{
    // type name is function name prefixed with #
    size_t l = strlen(jl_symbol_name(name));
    char *prefixed;
    prefixed = (char*)malloc_s(l+2);
    prefixed[0] = '#';
    strcpy(&prefixed[1], jl_symbol_name(name));
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
    jl_methtable_t *mt = jl_argument_method_table(ty);
    if ((jl_value_t*)mt == jl_nothing)
        jl_error("cannot get keyword sorter for abstract type");
    if (!mt->kwsorter) {
        JL_LOCK(&mt->writelock);
        if (!mt->kwsorter) {
            char *name;
            if (mt == jl_nonfunction_mt) {
                name = jl_symbol_name(mt->name);
            }
            else {
                jl_datatype_t *dt = (jl_datatype_t*)jl_argument_datatype(ty);
                assert(jl_is_datatype(dt));
                name = jl_symbol_name(dt->name->name);
                if (name[0] == '#')
                    name++;
            }
            size_t l = strlen(name);
            char *suffixed = (char*)malloc_s(l+5);
            strcpy(&suffixed[0], name);
            strcpy(&suffixed[l], "##kw");
            jl_sym_t *fname = jl_symbol(suffixed);
            mt->kwsorter = jl_new_generic_function_with_supertype(fname, mt->module, jl_function_type);
            jl_gc_wb(mt, mt->kwsorter);
        }
        JL_UNLOCK(&mt->writelock);
    }
    return mt->kwsorter;
}

jl_function_t *jl_new_generic_function(jl_sym_t *name, jl_module_t *module)
{
    return jl_new_generic_function_with_supertype(name, module, jl_function_type);
}

struct ml_matches_env {
    // inputs:
    struct typemap_intersection_env match;
    int intersections;
    size_t world;
    int lim;
    // results:
    jl_value_t *t; // array of method matches
    size_t min_valid;
    size_t max_valid;
    // temporary:
    jl_method_match_t *matc; // current working method match
};

enum SIGNATURE_FULLY_COVERS {
    NOT_FULLY_COVERS = 0,
    FULLY_COVERS = 1,
    SENTINEL    = 2,
};

static jl_method_match_t *make_method_match(jl_tupletype_t *spec_types, jl_svec_t *sparams, jl_method_t *method, enum SIGNATURE_FULLY_COVERS fully_covers)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_method_match_t *match = (jl_method_match_t*)jl_gc_alloc(ptls, sizeof(jl_method_match_t), jl_method_match_type);
    match->spec_types = spec_types;
    match->sparams = sparams;
    match->method = method;
    match->fully_covers = fully_covers;
    return match;
}

static int ml_matches_visitor(jl_typemap_entry_t *ml, struct typemap_intersection_env *closure0)
{
    struct ml_matches_env *closure = container_of(closure0, struct ml_matches_env, match);
    if (closure->intersections == 0 && !closure0->issubty)
        return 1;
    if (closure->world < ml->min_world) {
        // ignore method table entries that are part of a later world
        if (closure->max_valid >= ml->min_world)
            closure->max_valid = ml->min_world - 1;
        return 1;
    }
    else if (closure->world > ml->max_world) {
        // ignore method table entries that have been replaced in the current world
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
    jl_method_t *meth = ml->func.method;
    if (closure->lim >= 0 && jl_is_dispatch_tupletype(meth->sig)) {
        if (closure->lim == 0)
            return 0;
        closure->lim--;
    }
    closure->matc = make_method_match((jl_tupletype_t*)closure->match.ti,
        closure->match.env, meth,
        closure->match.issubty ? FULLY_COVERS : NOT_FULLY_COVERS);
    size_t len = jl_array_len(closure->t);
    if (len == 0) {
        closure->t = (jl_value_t*)jl_alloc_vec_any(1);
        jl_array_ptr_set(closure->t, 0, (jl_value_t*)closure->matc);
    }
    else {
        jl_array_ptr_1d_push((jl_array_t*)closure->t, (jl_value_t*)closure->matc);
    }
    return 1;
}

// This is the collect form of calling jl_typemap_intersection_visitor
// with optimizations to skip fully shadowed methods.
//
// Returns a match as an array of svec(argtypes, static_params, Method, fully-covers).
//
// See below for the meaning of lim.
//
// fully-covers is a Bool indicating subtyping, though temporarily it may be
// tri-values, with `nothing` indicating a match that is not a subtype, but
// which is dominated by one which is (and thus should be excluded unless ambiguous)
static jl_value_t *ml_matches(jl_methtable_t *mt, int offs,
                              jl_tupletype_t *type, int lim, int include_ambiguous,
                              int intersections, size_t world, int cache_result,
                              size_t *min_valid, size_t *max_valid, int *has_ambiguity)
{
    jl_typemap_t *defs = mt->defs;
    if (defs == jl_nothing) // special-case: ignore builtin functions
        return jl_an_empty_vec_any;
    jl_value_t *unw = jl_unwrap_unionall((jl_value_t*)type);
    assert(jl_is_datatype(unw));
    size_t l = jl_svec_len(((jl_datatype_t*)unw)->parameters);
    jl_value_t *va = NULL;
    if (l > 0) {
        va = jl_tparam(unw, l - 1);
        if (jl_is_vararg_type(va))
            va = jl_unwrap_vararg(va);
        else
            va = NULL;
    }
    struct ml_matches_env env = {{ml_matches_visitor, (jl_value_t*)type, va}, intersections, world, lim};
    env.match.ti = NULL;
    env.match.env = jl_emptysvec;
    env.t = jl_an_empty_vec_any;
    env.matc = NULL;
    env.min_valid = *min_valid;
    env.max_valid = *max_valid;
    struct jl_typemap_assoc search = {(jl_value_t*)type, world, jl_emptysvec, 1, ~(size_t)0};
    JL_GC_PUSH5(&env.t, &env.matc, &env.match.env, &search.env, &env.match.ti);

    // check the leaf cache if this type can be in there
    if (((jl_datatype_t*)unw)->isdispatchtuple) {
        jl_array_t *leafcache = jl_atomic_load_relaxed(&mt->leafcache);
        jl_typemap_entry_t *entry = lookup_leafcache(leafcache, (jl_value_t*)type, world);
        if (entry) {
            jl_method_instance_t *mi = entry->func.linfo;
            jl_method_t *meth = mi->def.method;
            if (!jl_is_unionall(meth->sig)) {
                env.match.env = jl_emptysvec;
                env.match.ti = unw;
            }
            else if (jl_egal((jl_value_t*)type, mi->specTypes)) {
                env.match.env = mi->sparam_vals;
                env.match.ti = mi->specTypes;
            }
            else {
                // this just calls jl_subtype_env (since we know that `type <: meth->sig` by transitivity)
                env.match.ti = jl_type_intersection_env((jl_value_t*)type, (jl_value_t*)meth->sig, &env.match.env);
            }
            env.matc = make_method_match((jl_tupletype_t*)env.match.ti,
                env.match.env, meth, FULLY_COVERS);
            env.t = (jl_value_t*)jl_alloc_vec_any(1);
            jl_array_ptr_set(env.t, 0, env.matc);
            if (*min_valid < entry->min_world)
                *min_valid = entry->min_world;
            if (*max_valid > entry->max_world)
                *max_valid = entry->max_world;
            JL_GC_POP();
            return env.t;
        }
    }
    // then check the full cache if it seems profitable
    if (((jl_datatype_t*)unw)->isdispatchtuple) {
        jl_typemap_entry_t *entry = jl_typemap_assoc_by_type(mt->cache, &search, jl_cachearg_offset(mt), /*subtype*/1);
        if (entry && (((jl_datatype_t*)unw)->isdispatchtuple || entry->guardsigs == jl_emptysvec)) {
            jl_method_instance_t *mi = entry->func.linfo;
            jl_method_t *meth = mi->def.method;
            if (!jl_is_unionall(meth->sig) && ((jl_datatype_t*)unw)->isdispatchtuple) {
                env.match.env = jl_emptysvec;
                env.match.ti = unw;
            }
            else {
                // this just calls jl_subtype_env (since we know that `type <: meth->sig` by transitivity)
                env.match.ti = jl_type_intersection_env((jl_value_t*)type, (jl_value_t*)meth->sig, &env.match.env);
            }
            env.matc = make_method_match((jl_tupletype_t*)env.match.ti,
                env.match.env, meth, FULLY_COVERS);
            env.t = (jl_value_t*)jl_alloc_vec_any(1);
            jl_array_ptr_set(env.t, 0, env.matc);
            *min_valid = entry->min_world;
            *max_valid = entry->max_world;
            JL_GC_POP();
            return env.t;
        }
    }
    if (!jl_typemap_intersection_visitor(defs, offs, &env.match)) {
        JL_GC_POP();
        return jl_false;
    }
    *min_valid = env.min_valid;
    *max_valid = env.max_valid;
    // done with many of these values now
    env.match.ti = NULL; env.matc = NULL; env.match.env = NULL; search.env = NULL;
    size_t i, j, len = jl_array_len(env.t);
    jl_method_match_t *minmax = NULL;
    int minmax_ambig = 0;
    int all_subtypes = 1;
    if (len > 1) {
        // first try to pre-process the results to find the most specific result that fully covers the input
        // (since we can do this in linear time, and the rest is O(n^2)
        //   - first see if this might even be profitable, given the requested output we need to compute
        for (i = 0; i < len; i++) {
            jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(env.t, i);
            if (matc->fully_covers != FULLY_COVERS) {
                all_subtypes = 0;
                break;
            }
        }
        if (all_subtypes || !include_ambiguous) {
            //   - then find a candidate for the best of these method results
            //     (If we have a reason to compute this. There's no point in
            //     finding the minmax now, if we still need to examine all
            //     methods for ambiguities later.)
            for (i = 0; i < len; i++) {
                jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(env.t, i);
                if (matc->fully_covers == FULLY_COVERS) {
                    jl_method_t *m = matc->method;
                    if (minmax != NULL) {
                        jl_method_t *minmaxm = minmax->method;
                        if (jl_type_morespecific((jl_value_t*)minmaxm->sig, (jl_value_t*)m->sig))
                            continue;
                    }
                    minmax = matc;
                }
            }
            //   - then see if it dominated all of the other choices
            if (minmax != NULL) {
                for (i = 0; i < len; i++) {
                    jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(env.t, i);
                    if (matc == minmax)
                        break;
                    if (matc->fully_covers == FULLY_COVERS) {
                        jl_method_t *m = matc->method;
                        jl_method_t *minmaxm = minmax->method;
                        if (!jl_type_morespecific((jl_value_t*)minmaxm->sig, (jl_value_t*)m->sig)) {
                            minmax_ambig = 1;
                            *has_ambiguity = 1;
                            if (include_ambiguous)
                                minmax = NULL;
                            break;
                        }
                    }
                }
            }
            //   - it may even dominate some choices that are not subtypes!
            //     move those into the subtype group, where we're filter them out shortly after
            if (!all_subtypes && minmax) {
                jl_method_t *minmaxm = minmax->method;
                all_subtypes = 1;
                for (i = 0; i < len; i++) {
                    jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(env.t, i);
                    if (matc->fully_covers != FULLY_COVERS) {
                        jl_method_t *m = matc->method;
                        if (jl_type_morespecific((jl_value_t*)minmaxm->sig, (jl_value_t*)m->sig))
                            matc->fully_covers = SENTINEL; // put a sentinel value here for sorting
                        else
                            all_subtypes = 0;
                    }
                }
            }
            //    - now we might have a fast-return here, if we see that
            //      we've already processed all of the possible outputs
            if (all_subtypes) {
                if (minmax_ambig) {
                    if (!include_ambiguous) {
                        len = 0;
                        env.t = jl_an_empty_vec_any;
                    }
                }
                else {
                    assert(minmax != NULL);
                    jl_array_ptr_set(env.t, 0, minmax);
                    jl_array_del_end((jl_array_t*)env.t, len - 1);
                    len = 1;
                }
            }
        }
    }
    if (len > 1) {
        // need to partially domsort the graph now into a list
        // (this is an insertion sort attempt)
        // if we have a minmax method, we ignore anything less specific
        // we'll clean that up next
        for (i = 1; i < len; i++) {
            env.matc = (jl_method_match_t*)jl_array_ptr_ref(env.t, i);
            jl_method_t *m = env.matc->method;
            int subt = env.matc->fully_covers != NOT_FULLY_COVERS;
            if (minmax != NULL && subt) {
                continue; // already the biggest
            }
            for (j = 0; j < i; j++) {
                jl_method_match_t *matc2 = (jl_method_match_t *)jl_array_ptr_ref(env.t, i - j - 1);
                jl_method_t *m2 = matc2->method;
                int subt2 = matc2->fully_covers != NOT_FULLY_COVERS;
                if (!subt2 && subt)
                    break;
                if (subt == subt2)
                    if (subt || !jl_has_empty_intersection(m->sig, m2->sig))
                        if (!jl_type_morespecific((jl_value_t*)m->sig, (jl_value_t*)m2->sig))
                            break;
                jl_array_ptr_set(env.t, i - j, matc2);
            }
            jl_array_ptr_set(env.t, i - j, env.matc);
        }
        // final step to finish sort:
        // we stopped early with just having all non-subtypes before all
        // subtypes, but the case on the boundary might be wrongly placed:
        // check for that now
        if (!minmax) {
            for (i = 0; i < len; i++) {
                jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(env.t, i);
                if (matc->fully_covers == FULLY_COVERS)
                    break;
            }
            for (; i > 0; i--) {
                env.matc = (jl_method_match_t*)jl_array_ptr_ref(env.t, i - 1);
                jl_method_t *m = env.matc->method;
                for (j = i; j < len; j++) {
                    jl_method_match_t *matc2 = (jl_method_match_t*)jl_array_ptr_ref(env.t, j);
                    jl_method_t *m2 = matc2->method;
                    if (matc2->fully_covers != FULLY_COVERS)
                        break;
                    if (!jl_type_morespecific((jl_value_t*)m2->sig, (jl_value_t*)m->sig))
                        break;
                    jl_array_ptr_set(env.t, j - 1, matc2);
                }
                if (j == i)
                    break;
                env.matc->fully_covers = SENTINEL;
                jl_array_ptr_set(env.t, j - 1, env.matc);
            }
        }
        char *skip = (char*)alloca(len);
        memset(skip, 0, len);
        // since we had a minmax method, now may now be able to cleanup some of our sort result
        if (minmax_ambig && !include_ambiguous) {
            for (i = 0; i < len; i++) {
                jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(env.t, i);
                if (matc->fully_covers != NOT_FULLY_COVERS) {
                    skip[i] = 1;
                }
            }
        }
        else if (minmax != NULL) {
            assert(all_subtypes || !include_ambiguous);
            for (i = 0; i < len; i++) {
                jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(env.t, i);
                if (minmax != matc && matc->fully_covers != NOT_FULLY_COVERS) {
                    skip[i] = 1;
                }
            }
        }
        // now that the results are (mostly) sorted, assign group numbers to each ambiguity
        // by computing the specificity-ambiguity matrix covering this query
        uint32_t *ambig_groupid = (uint32_t*)alloca(len * sizeof(uint32_t));
        // as we go, keep a rough count of how many methods are disjoint, which
        // gives us a lower bound on how many methods we will be returning
        // and lets us stop early if we reach our limit
        int ndisjoint = 0;
        for (i = 0; i < len; i++) {
            // can't use skip[i] here, since we still need to make sure the minmax dominates
            jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(env.t, i);
            jl_method_t *m = matc->method;
            int subt = matc->fully_covers == FULLY_COVERS; // jl_subtype((jl_value_t*)type, (jl_value_t*)m->sig)
            ambig_groupid[i] = i;
            int disjoint = 1;
            for (j = i; j > 0; j--) {
                jl_method_match_t *matc2 = (jl_method_match_t*)jl_array_ptr_ref(env.t, j - 1);
                jl_method_t *m2 = matc2->method;
                int subt2 = matc2->fully_covers == FULLY_COVERS; // jl_subtype((jl_value_t*)type, (jl_value_t*)m2->sig)
                if (skip[j - 1]) {
                    // if there was a minmax method, we can just pretend these are all in the same group
                    // they're all together but unsorted in the list, since we'll drop them all later anyways
                    assert(matc2->fully_covers != NOT_FULLY_COVERS);
                    disjoint = 0;
                    ambig_groupid[i] = j - 1; // ambiguity covering range [i:j)
                }
                else if (subt || subt2 || !jl_has_empty_intersection(m->sig, m2->sig)) {
                    if (!jl_type_morespecific((jl_value_t*)m2->sig, (jl_value_t*)m->sig))
                        ambig_groupid[i] = j - 1; // ambiguity covering range [i:j)
                    disjoint = 0;
                }
            }
            if (disjoint && lim >= 0) {
                ndisjoint += 1;
                if (ndisjoint > lim) {
                    JL_GC_POP();
                    return jl_false;
                }
            }
        }
        // then we'll merge those numbers to assign each item in the group the same number
        uint32_t groupid = 0;
        uint32_t grouphi = 0;
        for (i = 0; i < len; i++) {
            j = len - i - 1;
            uint32_t agid = ambig_groupid[j];
            if (agid != j) { // thus agid < j
                if (grouphi == 0) {
                    groupid = agid;
                    grouphi = j;
                }
                else if (agid < groupid) {
                    groupid = agid;
                }
            }
            if (grouphi && j == groupid) {
                do {
                    ambig_groupid[grouphi--] = groupid;
                } while (grouphi > j);
                ambig_groupid[j] = groupid;
                groupid = 0;
                grouphi = 0;
            }
        }
        // always remove matches after the first subtype, now that we've sorted the list for ambiguities
        for (i = 0; i < len; i++) {
            jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(env.t, i);
            if (matc->fully_covers == FULLY_COVERS) { // jl_subtype((jl_value_t*)type, (jl_value_t*)m->sig)
                uint32_t agid = ambig_groupid[i];
                while (i < len && agid == ambig_groupid[i])
                    i++; // keep ambiguous ones
                for (; i < len; i++)
                    skip[i] = 1; // drop the rest
            }
        }
        // when limited, skip matches that are covered by earlier ones (and aren't perhaps ambiguous with them)
        if (lim >= 0) {
            for (i = 0; i < len; i++) {
                if (skip[i])
                    continue;
                jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(env.t, i);
                jl_method_t *m = matc->method;
                jl_tupletype_t *ti = matc->spec_types;
                if (matc->fully_covers == FULLY_COVERS)
                    break; // remaining matches are ambiguous or already skipped
                for (j = 0; j < i; j++) {
                    jl_method_match_t *matc2 = (jl_method_match_t*)jl_array_ptr_ref(env.t, j);
                    jl_method_t *m2 = matc2->method;
                    if (jl_subtype((jl_value_t*)ti, m2->sig)) {
                        if (ambig_groupid[i] != ambig_groupid[j]) {
                            skip[i] = 1;
                            break;
                        }
                        else if (!include_ambiguous) {
                            if (!jl_type_morespecific((jl_value_t*)m->sig, (jl_value_t*)m2->sig)) {
                                skip[i] = 1;
                                break;
                            }
                        }
                    }
                }
            }
        }
        // Compute whether anything could be ambiguous by seeing if any two
        // methods in the result are in the same ambiguity group.
        for (i = 0; i < len; i++) {
            if (!skip[i]) {
                uint32_t agid = ambig_groupid[i];
                for (; i < len; i++) {
                    if (!skip[i]) {
                        if (agid == ambig_groupid[i]) {
                            *has_ambiguity = 1;
                            break;
                        }
                        agid = ambig_groupid[i];
                    }
                }
                break;
            }
        }
        // If we're only returning possible matches, now filter out any method
        // whose intersection is fully ambiguous with the group it is in.
        if (!include_ambiguous) {
            for (i = 0; i < len; i++) {
                if (skip[i])
                    continue;
                uint32_t agid = ambig_groupid[i];
                jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(env.t, i);
                jl_method_t *m = matc->method;
                jl_tupletype_t *ti = matc->spec_types;
                int subt = matc->fully_covers == FULLY_COVERS; // jl_subtype((jl_value_t*)type, (jl_value_t*)m->sig)
                char ambig1 = 0;
                for (j = agid; j < len && ambig_groupid[j] == agid; j++) {
                    if (j == i)
                        continue;
                    jl_method_match_t *matc2 = (jl_method_match_t*)jl_array_ptr_ref(env.t, j);
                    jl_method_t *m2 = matc2->method;
                    int subt2 = matc2->fully_covers == FULLY_COVERS; // jl_subtype((jl_value_t*)type, (jl_value_t*)m2->sig)
                    // if their intersection contributes to the ambiguity cycle
                    if (subt || subt2 || !jl_has_empty_intersection((jl_value_t*)ti, m2->sig)) {
                        // and the contribution of m is ambiguous with the portion of the cycle from m2
                        if (subt2 || jl_subtype((jl_value_t*)ti, m2->sig)) {
                            // but they aren't themselves simply ordered (here
                            // we don't consider that a third method might be
                            // disrupting that ordering and just consider them
                            // pairwise to keep this simple).
                            if (!jl_type_morespecific((jl_value_t*)m->sig, (jl_value_t*)m2->sig) &&
                                !jl_type_morespecific((jl_value_t*)m2->sig, (jl_value_t*)m->sig)) {
                                ambig1 = 1;
                            }
                        }
                        else {
                            // otherwise some aspect of m is not ambiguous
                            ambig1 = 0;
                            break;
                        }
                    }
                }
                if (ambig1)
                    skip[i] = 1;
            }
        }
        // cleanup array to remove skipped entries
        for (i = 0, j = 0; i < len; i++) {
            jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(env.t, i);
            if (!skip[i]) {
                jl_array_ptr_set(env.t, j++, matc);
                // remove our sentinel entry markers
                if (matc->fully_covers == SENTINEL)
                    matc->fully_covers = NOT_FULLY_COVERS;
            }
        }
        if (j != len)
            jl_array_del_end((jl_array_t*)env.t, len - j);
        len = j;
    }
    if (cache_result && ((jl_datatype_t*)unw)->isdispatchtuple) { // cache_result parameter keeps this from being recursive
        if (len == 1 && !*has_ambiguity) {
            env.matc = (jl_method_match_t*)jl_array_ptr_ref(env.t, 0);
            jl_method_t *meth = env.matc->method;
            jl_svec_t *tpenv = env.matc->sparams;
            cache_method(mt, &mt->cache, (jl_value_t*)mt, type, meth, world, env.min_valid, env.max_valid, tpenv);
        }
    }
    JL_GC_POP();
    if (lim >= 0 && len > lim)
        return jl_false;
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
    return ((jl_datatype_t*)typ)->has_concrete_subtype;
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

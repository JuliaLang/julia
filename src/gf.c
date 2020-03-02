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
    return mt->offs;
}

/// ----- Insertion logic for special entries ----- ///

// get or create the MethodInstance for a specialization
JL_DLLEXPORT jl_method_instance_t *jl_specializations_get_linfo(jl_method_t *m JL_PROPAGATES_ROOT, jl_value_t *type, jl_svec_t *sparams)
{
    JL_LOCK(&m->writelock);
    struct jl_typemap_assoc search = {type, 1, 0, NULL, 0, ~(size_t)0};
    jl_typemap_entry_t *sf = jl_typemap_assoc_by_type(m->specializations, &search, /*offs*/0, /*subtype*/0);
    if (sf && jl_is_method_instance(sf->func.value)) {
        JL_UNLOCK(&m->writelock);
        return sf->func.linfo;
    }
    jl_method_instance_t *mi = jl_get_specialized(m, type, sparams);
    JL_GC_PUSH1(&mi);
    // TODO: fuse lookup and insert steps
    jl_typemap_insert(&m->specializations, (jl_value_t*)m, (jl_tupletype_t*)type,
            NULL, jl_emptysvec, (jl_value_t*)mi, 0, &tfunc_cache,
            1, ~(size_t)0);
    JL_UNLOCK(&m->writelock);
    JL_GC_POP();
    return mi;
}

JL_DLLEXPORT jl_value_t *jl_specializations_lookup(jl_method_t *m, jl_value_t *type)
{
    struct jl_typemap_assoc search = {type, 1, 0, NULL, 0, ~(size_t)0};
    jl_typemap_entry_t *sf = jl_typemap_assoc_by_type(m->specializations, &search, /*offs*/0, /*subtype*/0);
    if (!sf)
        return jl_nothing;
    return sf->func.value;
}

JL_DLLEXPORT jl_value_t *jl_methtable_lookup(jl_methtable_t *mt, jl_value_t *type, size_t world)
{
    struct jl_typemap_assoc search = {type, world, 0, NULL, 0, ~(size_t)0};
    jl_typemap_entry_t *sf = jl_typemap_assoc_by_type(mt->defs, &search, /*offs*/0, /*subtype*/0);
    if (!sf)
        return jl_nothing;
    return sf->func.value;
}

// ----- MethodInstance specialization instantiation ----- //

JL_DLLEXPORT jl_method_t *jl_new_method_uninit(jl_module_t*);
JL_DLLEXPORT jl_code_instance_t* jl_set_method_inferred(
        jl_method_instance_t *mi, jl_value_t *rettype,
        jl_value_t *inferred_const, jl_value_t *inferred,
        int32_t const_flags, size_t min_world, size_t max_world);

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

    JL_GC_PUSH1(&m);
    jl_method_instance_t *mi = jl_get_specialized(m, (jl_value_t*)jl_anytuple_type, jl_emptysvec);
    m->unspecialized = mi;
    jl_gc_wb(m, mi);

    jl_code_instance_t *codeinst = jl_set_method_inferred(mi, (jl_value_t*)jl_any_type, jl_nothing, jl_nothing,
        0, 1, ~(size_t)0);
    codeinst->specptr.fptr1 = fptr;
    codeinst->invoke = jl_fptr_args;

    jl_methtable_t *mt = dt->name->mt;
    jl_typemap_insert(&mt->cache, (jl_value_t*)mt, jl_anytuple_type,
        NULL, jl_emptysvec, (jl_value_t*)mi, 0, &lambda_cache, 1, ~(size_t)0);
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
            if (code && (code == jl_nothing || jl_ast_flag_inferred((jl_array_t*)code)))
                return (jl_value_t *)codeinst;
        }
        codeinst = codeinst->next;
    }
    return (jl_value_t *)jl_nothing;
}


JL_DLLEXPORT jl_code_instance_t *jl_get_method_inferred(
        jl_method_instance_t *mi, jl_value_t *rettype,
        size_t min_world, size_t max_world)
{
    jl_code_instance_t *codeinst = mi->cache;
    while (codeinst) {
        if (codeinst->min_world == min_world &&
            codeinst->max_world == max_world &&
            jl_egal(codeinst->rettype, rettype)) {
            return codeinst;
        }
        codeinst = codeinst->next;
    }
    return jl_set_method_inferred(
        mi, rettype, NULL, NULL,
        0, min_world, max_world);
}

JL_DLLEXPORT jl_code_instance_t *jl_set_method_inferred(
        jl_method_instance_t *mi JL_PROPAGATES_ROOT, jl_value_t *rettype,
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
    codeinst->functionObjectsDecls.functionObject = NULL;
    codeinst->functionObjectsDecls.specFunctionObject = NULL;
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
    if (jl_is_method(mi->def.method))
        JL_LOCK(&mi->def.method->writelock);
    codeinst->next = mi->cache;
    mi->cache = codeinst;
    jl_gc_wb(mi, codeinst);
    if (jl_is_method(mi->def.method))
        JL_UNLOCK(&mi->def.method->writelock);
    JL_GC_POP();
    return codeinst;
}

static int get_spec_unspec_list(jl_typemap_entry_t *l, void *closure)
{
    jl_method_instance_t *mi = l->func.linfo;
    assert(jl_is_method_instance(mi));
    if (jl_rettype_inferred(mi, jl_world_counter, jl_world_counter) == jl_nothing)
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
    jl_eqtable_put(visited, (jl_value_t*)m, jl_true, NULL);
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
    if (mt->defs != jl_nothing) // make sure not to reset builtin functions
        mt->cache = jl_nothing;
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

static jl_value_t *ml_matches(jl_methtable_t *mt, jl_tupletype_t *type,
                              int lim, int include_ambiguous, int stop_if_deleted, int insert_cache,
                              size_t world, size_t *min_valid, size_t *max_valid);

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
                elt = (jl_value_t*)jl_typetype_type;
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

        if (jl_types_equal(elt, (jl_value_t*)jl_typetype_type)) { // elt == Type{T} where T
            // not triggered for isdispatchtuple(tt), this attempts to handle
            // some cases of adapting a random signature into a compilation signature
        }
        else if (!jl_is_datatype(elt) && jl_subtype(elt, (jl_value_t*)jl_type_type)) { // elt <: Type{T}
            // not triggered for isdispatchtuple(tt), this attempts to handle
            // some cases of adapting a random signature into a compilation signature
            if (!*newparams) *newparams = jl_svec_copy(tt->parameters);
            jl_svecset(*newparams, i, jl_typetype_type);
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
            if (jl_types_equal(elt, (jl_value_t*)jl_type_type)) {
                if (very_general_type(decl_i))
                    continue;
                if (i >= nargs && definition->isva)
                    continue;
                return 0;
            }
            if (very_general_type(decl_i))
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
                    jl_value_t *di = jl_type_intersection(decl_i, (jl_value_t*)jl_typetype_type);
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

static jl_method_instance_t *cache_method(
        jl_methtable_t *mt, jl_typemap_t **cache, jl_value_t *parent JL_PROPAGATES_ROOT,
        jl_tupletype_t *tt, // the original tupletype of the signature
        jl_method_t *definition,
        size_t world,
        jl_svec_t *sparams)
{
    // caller must hold the mt->writelock
    // short-circuit (now that we hold the lock) if this entry is already present
    int8_t offs = mt ? jl_cachearg_offset(mt) : 1;
    { // scope block
        struct jl_typemap_assoc search = {(jl_value_t*)tt, world, 0, NULL, 0, ~(size_t)0};
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

    jl_tupletype_t *cachett = tt;
    jl_svec_t* guardsigs = jl_emptysvec;
    size_t min_valid = 1;
    size_t max_valid = ~(size_t)0;
    if (!cache_with_orig && mt) {
        // now examine what will happen if we chose to use this sig in the cache
        // TODO: should we first check `compilationsig <: definition`?
        temp = ml_matches(mt, compilationsig, MAX_UNSPECIALIZED_CONFLICTS, 1, 0, 0, world, &min_valid, &max_valid);
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
                    //        NULL, jl_emptysvec, /*guard*/NULL, jl_cachearg_offset(mt), &lambda_cache, other->min_world, other->max_world);
                }
            }
        }
        if (cache_with_orig) {
            min_valid = 1;
            max_valid = ~(size_t)0;
        }
        else {
            // determined above that there's no ambiguity in also using compilationsig as the cacheablesig
            cachett = compilationsig;
        }
    }

    if (cache_with_orig && mt) {
        // now examine defs to determine the min/max-valid range for this lookup result
        (void)ml_matches(mt, cachett, -1, 0, 0, 0, world, &min_valid, &max_valid);
    }
    assert(mt == NULL || min_valid > 1);

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

    // short-circuit if there's already an entry present
    // to avoid adding a new duplicate copy of it
    if (cachett != tt && simplett == NULL) {
        struct jl_typemap_assoc search = {(jl_value_t*)cachett, min_valid, 0, NULL, 0, ~(size_t)0};
        jl_typemap_entry_t *entry = jl_typemap_assoc_by_type(*cache, &search, offs, /*subtype*/1);
        if (entry && (jl_value_t*)entry->simplesig == jl_nothing) {
            if (jl_egal((jl_value_t*)guardsigs, (jl_value_t*)entry->guardsigs)) {
                // just update the existing entry to reflect new knowledge
                if (entry->min_world > min_valid)
                    entry->min_world = min_valid;
                if (entry->max_world < max_valid)
                    entry->max_world = max_valid;
                newmeth = entry->func.linfo;
                assert(newmeth != NULL);
                JL_GC_POP();
                return newmeth;
            }
        }
    }

    newmeth = jl_specializations_get_linfo(definition, (jl_value_t*)compilationsig, sparams);
    jl_typemap_insert(cache, parent, cachett, simplett, guardsigs,
            (jl_value_t*)newmeth, offs, &lambda_cache,
            min_valid, max_valid);

    JL_GC_POP();
    return newmeth;
}

// this is the general entry point for taking a match (returned by jl_typemap_assoc_by_type with subtype=1)
// and getting the most-specific one (or NULL, if there isn't one that is most specific)
static jl_typemap_entry_t *jl_typemap_morespecific_by_type(jl_typemap_entry_t *first JL_PROPAGATES_ROOT, jl_value_t *types, jl_svec_t **penv, size_t world)
{
    jl_typemap_entry_t *candidate = first;
    jl_value_t *resorted = first->func.method->resorted;
    // pick a method out of the resorted list that is more specific than any other applicable method
    if ((jl_value_t*)resorted != jl_nothing) {
        size_t i, l = jl_array_len(resorted);
        //int isambig = 0;
        for (i = 0; i < l; i++) {
            jl_typemap_entry_t *prior = (jl_typemap_entry_t*)jl_array_ptr_ref(resorted, i);
            if (prior->min_world <= world && world <= prior->max_world && jl_subtype(types, (jl_value_t*)prior->sig)) {
                if (candidate == first || jl_type_morespecific((jl_value_t*)prior->sig, (jl_value_t*)candidate->sig)) {
                    candidate = prior;
                }
                //else if (!jl_type_morespecific((jl_value_t*)candidate->sig, (jl_value_t*)prior->sig)) {
                //    isambig = 1;
                //}
            }
        }
        // and then make sure it is more specific than all other applicable (unsorted) methods
        //if (isambig)
        //    return NULL;
        if (candidate != first) {
            jl_value_t *ambigs = first->func.method->ambig;
            if ((jl_value_t*)ambigs != jl_nothing) {
                size_t i, l = jl_array_len(ambigs);
                for (i = 0; i < l; i++) {
                    jl_typemap_entry_t *ambig = (jl_typemap_entry_t*)jl_array_ptr_ref(ambigs, i);
                    if (ambig->min_world <= world && world <= ambig->max_world && jl_subtype(types, (jl_value_t*)ambig->sig)) {
                        if (!jl_type_morespecific((jl_value_t*)candidate->sig, (jl_value_t*)ambig->sig))
                            return NULL;
                    }
                }
            }
            //for (i = 0; i < l; i++) {
            //    jl_typemap_entry_t *prior = (jl_typemap_entry_t*)jl_array_ptr_ref(resorted, i);
            //    if (prior == candidate)
            //        break; // already checked the rest
            //    if (prior->min_world <= world && world <= prior->max_world && jl_subtype(types, (jl_value_t*)prior->sig)) {
            //        if (!jl_type_morespecific((jl_value_t*)candidate->sig, (jl_value_t*)prior->sig))
            //            return NULL;
            //    }
            //}
        }
    }
    // and then make sure none of the ambiguous methods with our candidate are applicable:
    // even if it it is a better match than our first method
    // it might actually still be only net ambiguous when we consider ambiguities
    jl_value_t *ambigs = candidate->func.method->ambig;
    if ((jl_value_t*)ambigs != jl_nothing) {
        size_t i, l = jl_array_len(ambigs);
        for (i = 0; i < l; i++) {
            jl_typemap_entry_t *ambig = (jl_typemap_entry_t*)jl_array_ptr_ref(ambigs, i);
            if (ambig->min_world <= world && world <= ambig->max_world && jl_subtype(types, (jl_value_t*)ambig->sig)) {
                return NULL;
            }
        }
    }
    if (candidate != first && penv) {
        // get the updated `env` for our match
        int match = jl_subtype_matching(types, (jl_value_t*)candidate->sig, penv);
        assert(match); (void)match;
    }
    return candidate;
}

static jl_method_instance_t *jl_mt_assoc_by_type(jl_methtable_t *mt, jl_datatype_t *tt, int mt_cache, size_t world)
{
    // caller must hold the mt->writelock
    struct jl_typemap_assoc search = {(jl_value_t*)tt, world, 0, NULL, 0, ~(size_t)0};
    jl_typemap_entry_t *entry = jl_typemap_assoc_by_type(mt->cache, &search, jl_cachearg_offset(mt), /*subtype*/1);
    if (entry && entry->func.value)
        return entry->func.linfo;

    jl_method_instance_t *nf = NULL;
    jl_svec_t *newparams = NULL;
    search.env = jl_emptysvec;
    JL_GC_PUSH3(&tt, &search.env, &newparams);
    entry = jl_typemap_assoc_by_type(mt->defs, &search, /*offs*/0, /*subtype*/1);

    if (entry != NULL) {
        entry = jl_typemap_morespecific_by_type(entry, (jl_value_t*)tt, &search.env, world);
        if (entry != NULL) {
            jl_method_t *m = entry->func.method;
            jl_svec_t *env = search.env;
            if (!mt_cache) {
                intptr_t nspec = (mt == jl_type_type_mt ? m->nargs + 1 : mt->max_args + 2);
                jl_compilation_sig(tt, env, m, nspec, &newparams);
                if (newparams)
                    tt = jl_apply_tuple_type(newparams);
                nf = jl_specializations_get_linfo(m, (jl_value_t*)tt, env);
            }
            else {
                nf = cache_method(mt, &mt->cache, (jl_value_t*)mt, tt, m, world, env);
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
    jl_typemap_t *defs;
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
        return 1; // no world has both active

    jl_typemap_entry_t *before = (closure->after ? closure->newentry : oldentry);
    jl_typemap_entry_t *after = (closure->after ? oldentry : closure->newentry);
    jl_tupletype_t *type = (jl_tupletype_t*)closure->match.type;
    jl_tupletype_t *sig = oldentry->sig;
    jl_value_t *isect = closure->match.ti;

    int msp = 1; // TODO: msp is a really terrible name
    int shadowed = 0;
    int reorder = 0;
    if (closure->match.issubty) { // (new)type <: (old)sig
        // new entry is more specific
        if (!closure->after)
            reorder = 1;
        shadowed = 1;
        // TODO: can stop search now? (copy remainder from oldentry)
    }
    else if (jl_subtype((jl_value_t*)sig, (jl_value_t*)type)) {
        // old entry is more specific
        if (closure->after)
            reorder = 1;
    }
    else if (jl_type_morespecific_no_subtype((jl_value_t*)type, (jl_value_t*)sig)) {
        // new entry is more specific
        if (!closure->after)
            reorder = 1;
        shadowed = 1;
    }
    else if (jl_type_morespecific_no_subtype((jl_value_t*)sig, (jl_value_t*)type)) {
        // old entry is more specific
        if (closure->after)
            reorder = 1;
    }
    else {
        // sort order is ambiguous
        reorder = 1;
        shadowed = 1;
        msp = 0;
    }
    // TODO: also complete detection of a specificity cycle

    // see if the intersection is covered by another existing entry
    // that will resolve the ambiguity (by being more specific than either)
    // (in some cases, this also might end up finding
    // that isect == type or isect == sig and return the original match)
    if (reorder) {
        size_t world = closure->newentry->min_world;
        if (oldentry->min_world > world)
            world = oldentry->min_world;
        int exact1 = jl_subtype(isect, (jl_value_t*)sig);
        int exact2 = jl_subtype(isect, (jl_value_t*)type);
        // TODO: we might like to use `subtype = exact1 && exact2` here, but check_disabled_ambiguous_visitor
        // won't be able to handle that, so we might end up making some unnecessary mambig entries here
        // but I don't have any examples of such
        struct jl_typemap_assoc search = {(jl_value_t*)isect, world, 0, NULL, 0, ~(size_t)0};
        jl_typemap_entry_t *l = jl_typemap_assoc_by_type(closure->defs, &search, /*offs*/0, /*subtype*/0);
        //assert((!subtype || l != after) && "bad typemap lookup result"); // should find `before` first
        if (l != NULL && l != before && l != after) {
            if ((exact1 || jl_type_morespecific_no_subtype((jl_value_t*)l->sig, (jl_value_t*)sig)) &&  // no subtype since `l->sig >: isect >: sig`
                (exact2 || jl_type_morespecific_no_subtype((jl_value_t*)l->sig, (jl_value_t*)type))) { // no subtype since `l->sig >: isect >: type`
                // ok, intersection is already covered by a more specific method
                reorder = 0; // this lack of ordering doesn't matter (unless we delete this method--then it will)
                shadowed = 0; // we wouldn't find anything that mattered
            }
        }
    }

    // ok: record specificity ordering violation
    if (reorder) {
        jl_method_t *beforem = before->func.method;
        jl_method_t *afterm = after->func.method;
        if (msp) {
            if ((jl_value_t*)beforem->resorted == jl_nothing) {
                beforem->resorted = (jl_value_t*)jl_alloc_vec_any(0);
                jl_gc_wb(beforem, beforem->resorted);
            }
            jl_array_ptr_1d_push((jl_array_t*)beforem->resorted, (jl_value_t*)after);
        }
        else {
            if ((jl_value_t*)beforem->ambig == jl_nothing) {
                beforem->ambig = (jl_value_t*)jl_alloc_vec_any(0);
                jl_gc_wb(beforem, beforem->ambig);
            }
            if ((jl_value_t*)afterm->ambig == jl_nothing) {
                afterm->ambig = (jl_value_t*)jl_alloc_vec_any(0);
                jl_gc_wb(afterm, afterm->ambig);
            }
            jl_array_ptr_1d_push((jl_array_t*)beforem->ambig, (jl_value_t*)after);
            jl_array_ptr_1d_push((jl_array_t*)afterm->ambig, (jl_value_t*)before);
            if (eager_ambiguity_printing) {
                jl_method_t *m1 = closure->newentry->func.method;
                jl_method_t *m2 = oldentry->func.method;
                JL_STREAM *s = JL_STDERR;
                jl_printf(s, "WARNING: New definition \n    ");
                jl_static_show_func_sig(s, (jl_value_t*)type);
                print_func_loc(s, m1);
                jl_printf(s, "\nis ambiguous with: \n    ");
                jl_static_show_func_sig(s, (jl_value_t*)sig);
                print_func_loc(s, m2);
                jl_printf(s, ".\nTo fix, define \n    ");
                jl_static_show_func_sig(s, isect);
                jl_printf(s, "\nbefore the new definition.\n");
            }
        }
    }

    // ok: record that this method definition is being partially replaced
    // (either with a real definition, or an ambiguity error)
    if (shadowed) {
        if (closure->shadowed == NULL) {
            closure->shadowed = (jl_value_t*)oldentry;
        }
        else if (!jl_is_array(closure->shadowed)) {
            jl_array_t *list = jl_alloc_vec_any(2);
            jl_array_ptr_set(list, 0, closure->shadowed);
            jl_array_ptr_set(list, 1, (jl_value_t*)oldentry);
            closure->shadowed = (jl_value_t*)list;
        }
        else {
            jl_array_ptr_1d_push((jl_array_t*)closure->shadowed, (jl_value_t*)oldentry);
        }
    }
    return 1;
}

static jl_value_t *check_ambiguous_matches(jl_typemap_t *defs, jl_typemap_entry_t *newentry, jl_typemap_intersection_visitor_fptr fptr)
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
    struct ambiguous_matches_env env = {{fptr, (jl_value_t*)type, va}};
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
    if (oldentry->max_world < ~(size_t)0)
        return 1;
    jl_tupletype_t *sig = oldentry->sig;
    jl_value_t *type = closure->match.type;
    jl_value_t *isect2 = NULL;
    if (closure->shadowed == NULL)
        closure->shadowed = (jl_value_t*)jl_alloc_vec_any(0);
    JL_GC_PUSH1(&isect2);
    int i, l = jl_array_len(closure->shadowed);
    for (i = 0; i < l; i++) {
        jl_typemap_entry_t *before = (jl_typemap_entry_t*)jl_array_ptr_ref(closure->shadowed, i);
        isect2 = jl_type_intersection((jl_value_t*)before->sig, (jl_value_t*)sig);
        // see if the intersection was covered by precisely the disabled method
        // that means we now need to record the ambiguity
        if (jl_types_equal(type, isect2)) {
            jl_method_t *beforem = before->func.method;
            jl_method_t *afterm = oldentry->func.method;
            int msp = jl_type_morespecific((jl_value_t*)sig, (jl_value_t*)before->sig);
            if (msp) {
                if ((jl_value_t*)beforem->resorted == jl_nothing) {
                    beforem->resorted = (jl_value_t*)jl_alloc_vec_any(0);
                    jl_gc_wb(beforem, beforem->resorted);
                }
                jl_array_ptr_1d_push((jl_array_t*)beforem->resorted, (jl_value_t*)oldentry);
            }
            else if (!jl_type_morespecific((jl_value_t*)before->sig, (jl_value_t*)sig)) {
                if ((jl_value_t*)beforem->ambig == jl_nothing) {
                    beforem->ambig = (jl_value_t*)jl_alloc_vec_any(0);
                    jl_gc_wb(beforem, beforem->ambig);
                }
                if ((jl_value_t*)afterm->ambig == jl_nothing) {
                    afterm->ambig = (jl_value_t*)jl_alloc_vec_any(0);
                    jl_gc_wb(afterm, afterm->ambig);
                }
                jl_array_ptr_1d_push((jl_array_t*)beforem->ambig, (jl_value_t*)oldentry);
                jl_array_ptr_1d_push((jl_array_t*)afterm->ambig, (jl_value_t*)before);
            }
        }
    }
    JL_GC_POP();
    jl_array_ptr_1d_push((jl_array_t*)closure->shadowed, (jl_value_t*)oldentry);
    return 1;
}


static void method_overwrite(jl_typemap_entry_t *newentry, jl_method_t *oldvalue)
{
    // method overwritten
    if (jl_options.warn_overwrite == JL_OPTIONS_WARN_OVERWRITE_ON) {
        jl_method_t *method = (jl_method_t*)newentry->func.method;
        jl_module_t *newmod = method->module;
        jl_module_t *oldmod = oldvalue->module;
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

int JL_DEBUG_METHOD_INVALIDATION = 0;

// recursively invalidate cached methods that had an edge to a replaced method
static void invalidate_method_instance(jl_method_instance_t *replaced, size_t max_world, int depth)
{
    if (JL_DEBUG_METHOD_INVALIDATION) {
        int d0 = depth;
        while (d0-- > 0)
            jl_uv_puts(JL_STDOUT, " ", 1);
        jl_static_show(JL_STDOUT, (jl_value_t*)replaced);
        jl_uv_puts(JL_STDOUT, "\n", 1);
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
        codeinst = codeinst->next;
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
struct invalidate_conflicting_env {
    struct typemap_intersection_env match;
    size_t max_world;
    int invalidated;
};
static int invalidate_backedges(jl_typemap_entry_t *oldentry, struct typemap_intersection_env *closure0)
{
    struct invalidate_conflicting_env *closure = container_of(closure0, struct invalidate_conflicting_env, match);
    jl_method_instance_t *replaced_linfo = oldentry->func.linfo;
    JL_LOCK_NOGC(&replaced_linfo->def.method->writelock);
    jl_array_t *backedges = replaced_linfo->backedges;
    if (backedges) {
        // invalidate callers (if any)
        replaced_linfo->backedges = NULL;
        size_t i, l = jl_array_len(backedges);
        jl_method_instance_t **replaced = (jl_method_instance_t**)jl_array_ptr_data(backedges);
        for (i = 0; i < l; i++) {
            invalidate_method_instance(replaced[i], closure->max_world, 1);
        }
        closure->invalidated = 1;
    }
    JL_UNLOCK_NOGC(&replaced_linfo->def.method->writelock);
    return 1;
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
    jl_value_t *shadowed;
    size_t max_world;
};
static int invalidate_mt_cache(jl_typemap_entry_t *oldentry, void *closure0)
{
    struct invalidate_mt_env *env = (struct invalidate_mt_env*)closure0;
    if (oldentry->max_world == ~(size_t)0) {
        jl_method_instance_t *mi = oldentry->func.linfo;
        jl_method_t *m = mi->def.method;
        int intersects = 0;
        if (jl_is_method(env->shadowed)) {
            if ((jl_value_t*)m == env->shadowed) {
                intersects = 1;
            }
        }
        else {
            assert(jl_is_array(env->shadowed));
            jl_typemap_entry_t **d = (jl_typemap_entry_t**)jl_array_ptr_data(env->shadowed);
            size_t i, n = jl_array_len(env->shadowed);
            for (i = 0; i < n; i++) {
                if (m == d[i]->func.method) {
                    intersects = 1;
                    break;
                }
            }
        }
        if (intersects) {
            if (JL_DEBUG_METHOD_INVALIDATION) {
                jl_uv_puts(JL_STDOUT, "-- ", 3);
                jl_static_show(JL_STDOUT, (jl_value_t*)mi);
                jl_uv_puts(JL_STDOUT, "\n", 1);
            }
            oldentry->max_world = env->max_world;
        }
    }
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

// TODO: decrease repeated work?
// This implementation is stupidly inefficient, but probably correct
JL_DLLEXPORT void jl_method_table_disable(jl_methtable_t *mt, jl_method_t *method)
{
    if (jl_options.incremental && jl_generating_output())
        jl_printf(JL_STDERR, "WARNING: method deletion during Module precompile may lead to undefined behavior"
                             "\n  ** incremental compilation may be fatally broken for this module **\n\n");
    jl_typemap_entry_t *methodentry = do_typemap_search(mt, method);
    JL_LOCK(&mt->writelock);
    // Narrow the world age on the method to make it uncallable
    method->deleted_world = methodentry->max_world = jl_world_counter++;
    // Recompute ambiguities (deleting a more specific method might reveal ambiguities that it previously resolved)
    (void)check_ambiguous_matches(mt->defs, methodentry, check_disabled_ambiguous_visitor);
    // drop this method from mt->cache
    struct invalidate_mt_env mt_cache_env;
    mt_cache_env.max_world = methodentry->max_world - 1;
    mt_cache_env.shadowed = (jl_value_t*)method;
    jl_typemap_visitor(mt->cache, invalidate_mt_cache, (void*)&mt_cache_env);
    // Invalidate the backedges
    struct invalidate_conflicting_env env = {{NULL, NULL, NULL}};
    env.invalidated = 0;
    env.max_world = methodentry->max_world;
    jl_typemap_visitor(methodentry->func.method->specializations, (jl_typemap_visitor_fptr)invalidate_backedges, &env.match);
    JL_UNLOCK(&mt->writelock);
}

JL_DLLEXPORT void jl_method_table_insert(jl_methtable_t *mt, jl_method_t *method, jl_tupletype_t *simpletype)
{
    JL_TIMING(ADD_METHOD);
    assert(jl_is_method(method));
    assert(jl_is_mtable(mt));
    jl_value_t *type = method->sig;
    jl_value_t *oldvalue = NULL;
    if (method->primary_world == 1)
        method->primary_world = ++jl_world_counter;
    size_t max_world = method->primary_world - 1;
    int invalidated = 0;
    JL_GC_PUSH1(&oldvalue);
    JL_LOCK(&mt->writelock);
    // first delete the existing entry (we'll disable it later)
    struct jl_typemap_assoc search = {(jl_value_t*)type, method->primary_world, 0, NULL, 0, ~(size_t)0};
    jl_typemap_entry_t *oldentry = jl_typemap_assoc_by_type(mt->defs, &search, /*offs*/0, /*subtype*/0);
    if (oldentry) {
        oldentry->max_world = method->primary_world - 1;
        // TODO: just append our new entry right here
    }
    // then add our new entry
    jl_typemap_entry_t *newentry = jl_typemap_insert(&mt->defs, (jl_value_t*)mt,
            (jl_tupletype_t*)type, simpletype, jl_emptysvec, (jl_value_t*)method, 0, &method_defs,
            method->primary_world, method->deleted_world);
    oldvalue = check_ambiguous_matches(mt->defs, newentry, check_ambiguous_visitor);
    if (oldentry) {
        oldvalue = oldentry->func.value;
        method_overwrite(newentry, (jl_method_t*)oldvalue);
    }
    else {
        if (mt->backedges) {
            jl_value_t **backedges = jl_array_ptr_data(mt->backedges);
            size_t i, na = jl_array_len(mt->backedges);
            size_t ins = 0;
            for (i = 1; i < na; i += 2) {
                jl_value_t *backedgetyp = backedges[i - 1];
                if (!jl_has_empty_intersection(backedgetyp, (jl_value_t*)type)) {
                    jl_method_instance_t *backedge = (jl_method_instance_t*)backedges[i];
                    invalidate_method_instance(backedge, max_world, 0);
                    invalidated = 1;
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
        if (jl_typeis(oldvalue, jl_typemap_entry_type))
            oldvalue = ((jl_typemap_entry_t*)oldvalue)->func.value; // a method
        // drop anything in mt->cache that might overlap with the new method
        struct invalidate_mt_env mt_cache_env;
        mt_cache_env.max_world = max_world;
        mt_cache_env.shadowed = oldvalue;
        jl_typemap_visitor(mt->cache, invalidate_mt_cache, (void*)&mt_cache_env);
        //TODO: if it's small, might it be better to drop it all too?
        //if (mt != jl_type_type_mt) {
        //    mt->cache = jl_nothing;
        //}

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
        struct invalidate_conflicting_env env = {{invalidate_backedges, (jl_value_t*)type, va}};
        env.invalidated = 0;
        env.max_world = max_world;
        env.match.env = NULL;

        if (jl_is_method(oldvalue)) {
            jl_typemap_intersection_visitor(((jl_method_t*)oldvalue)->specializations, 0, &env.match);
        }
        else {
            assert(jl_is_array(oldvalue));
            jl_typemap_entry_t **d = (jl_typemap_entry_t**)jl_array_ptr_data(oldvalue);
            size_t i, n = jl_array_len(oldvalue);
            for (i = 0; i < n; i++) {
                jl_typemap_intersection_visitor(d[i]->func.method->specializations, 0, &env.match);
            }
        }
        if (env.invalidated)
            invalidated = 1;
    }
    if (invalidated && JL_DEBUG_METHOD_INVALIDATION) {
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
    jl_tupletype_t *tt;
    size_t i;
    int onstack = (nargs * sizeof(jl_value_t*) < jl_page_size);
    jl_value_t **roots;
    jl_value_t **types;
    JL_GC_PUSHARGS(roots, onstack ? nargs : 1);
    if (onstack) {
        types = roots;
    }
    else {
        roots[0] = (jl_value_t*)jl_alloc_svec(nargs);
        types = jl_svec_data(roots[0]);
    }
    for (i = 0; i < nargs; i++) {
        jl_value_t *ai = (i == 0 ? arg1 : args[i - 1]);
        if (jl_is_type(ai)) {
            // if `ai` has free type vars this will not be a valid (concrete) type.
            // TODO: it would be really nice to only dispatch and cache those as
            // `jl_typeof(ai)`, but that will require some redesign of the caching
            // logic.
            types[i] = (jl_value_t*)jl_wrap_Type(ai);
            if (!onstack)
                jl_gc_wb(roots[0], types[i]);
        }
        else {
            types[i] = jl_typeof(ai);
        }
    }
    if (onstack)
        tt = jl_apply_tuple_type_v(types, nargs);
    else
        tt = jl_apply_tuple_type((jl_svec_t*)roots[0]);
    JL_GC_POP();
    return tt;
}

jl_method_instance_t *jl_method_lookup(jl_value_t **args, size_t nargs, int cache, size_t world)
{
    assert(nargs > 0 && "expected caller to handle this case");
    jl_methtable_t *mt = jl_gf_mtable(args[0]);
    jl_typemap_entry_t *entry = jl_typemap_assoc_exact(mt->cache, args[0], &args[1], nargs, jl_cachearg_offset(mt), world);
    if (entry)
        return entry->func.linfo;
    JL_LOCK(&mt->writelock);
    jl_tupletype_t *tt = arg_type_tuple(args[0], &args[1], nargs);
    JL_GC_PUSH1(&tt);
    jl_method_instance_t *sf = jl_mt_assoc_by_type(mt, tt, cache, world);
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
    JL_TIMING(METHOD_MATCH);
    jl_value_t *unw = jl_unwrap_unionall((jl_value_t*)types);
    if (jl_is_tuple_type(unw) && jl_tparam0(unw) == jl_bottom_type)
        return (jl_value_t*)jl_an_empty_vec_any;
    jl_methtable_t *mt = jl_method_table_for(unw);
    if ((jl_value_t*)mt == jl_nothing)
        return jl_false; // indeterminate - ml_matches can't deal with this case
    jl_value_t *t = ml_matches(mt, types, lim, include_ambiguous, 0, 1, world, min_valid, max_valid);
    return t;
}

// returns the matching methods, but stop and return failure if that would skip over a deleted method
JL_DLLEXPORT jl_value_t *jl_matching_methods_or_deleted(jl_tupletype_t *types, size_t world, size_t *min_valid)
{
    JL_TIMING(METHOD_MATCH);
    size_t max_valid = ~(size_t)0;
    jl_value_t *unw = jl_unwrap_unionall((jl_value_t*)types);
    if (jl_is_tuple_type(unw) && jl_tparam0(unw) == jl_bottom_type)
        return (jl_value_t*)jl_an_empty_vec_any;
    jl_methtable_t *mt = jl_method_table_for(unw);
    if ((jl_value_t*)mt == jl_nothing)
        return jl_false; // indeterminate - ml_matches can't deal with this case
    jl_value_t *t = ml_matches(mt, types, -1, 1, 1, 1, world, min_valid, &max_valid);
    return t;
}

jl_method_instance_t *jl_get_unspecialized(jl_method_instance_t *method JL_PROPAGATES_ROOT)
{
    // one unspecialized version of a function can be shared among all cached specializations
    jl_method_t *def = method->def.method;
    if (def->source == NULL) {
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

jl_code_instance_t *jl_compile_method_internal(jl_method_instance_t *mi, size_t world)
{
    jl_code_instance_t *codeinst;
    codeinst = mi->cache;

    while (codeinst) {
        if (codeinst->min_world <= world && world <= codeinst->max_world && codeinst->invoke != NULL) {
            return codeinst;
        }
        codeinst = codeinst->next;
    }

    if (jl_options.compile_enabled == JL_OPTIONS_COMPILE_OFF ||
        jl_options.compile_enabled == JL_OPTIONS_COMPILE_MIN) {
        // copy fptr from the template method definition
        jl_method_t *def = mi->def.method;
        if (jl_is_method(def) && def->unspecialized) {
            jl_code_instance_t *unspec = def->unspecialized->cache;
            if (unspec && unspec->invoke != NULL) {
                jl_code_instance_t *codeinst = jl_set_method_inferred(mi, (jl_value_t*)jl_any_type, NULL, NULL,
                    0, 1, ~(size_t)0);
                codeinst->specptr = unspec->specptr;
                codeinst->rettype_const = unspec->rettype_const;
                jl_atomic_store_release(&codeinst->invoke, unspec->invoke);
                return codeinst;
            }
        }
        jl_code_info_t *src = jl_code_for_interpreter(mi);
        if (!jl_code_requires_compiler(src)) {
            jl_code_instance_t *codeinst = jl_set_method_inferred(mi, (jl_value_t*)jl_any_type, NULL, NULL,
                0, 1, ~(size_t)0);
            jl_atomic_store_release(&codeinst->invoke, jl_fptr_interpret_call);
            return codeinst;
        }
        if (jl_options.compile_enabled == JL_OPTIONS_COMPILE_OFF) {
            jl_printf(JL_STDERR, "code missing for ");
            jl_static_show(JL_STDERR, (jl_value_t*)mi);
            jl_printf(JL_STDERR, " : sysimg may not have been built with --compile=all\n");
        }
    }

    JL_LOCK(&codegen_lock);
    codeinst = mi->cache;
    while (codeinst) {
        if (codeinst->min_world <= world && world <= codeinst->max_world && codeinst->functionObjectsDecls.functionObject != NULL)
            break;
        codeinst = codeinst->next;
    }
    if (codeinst == NULL) {
        // if we don't have any decls already, try to generate it now
        jl_code_info_t *src = NULL;
        if (jl_is_method(mi->def.method) && jl_rettype_inferred(mi, world, world) == jl_nothing &&
                 jl_symbol_name(mi->def.method->name)[0] != '@') {
            // don't bother with typeinf on macros or toplevel thunks
            // but try to infer everything else
            src = jl_type_infer(mi, world, 0);
        }
        codeinst = jl_compile_linfo(mi, src, world, &jl_default_cgparams);
        if (!codeinst) {
            jl_method_instance_t *unspec = jl_get_unspecialized(mi);
            jl_code_instance_t *ucache = jl_get_method_inferred(unspec, (jl_value_t*)jl_any_type, 1, ~(size_t)0);
            // ask codegen to make the fptr for unspec
            if (ucache->invoke == NULL)
                jl_generate_fptr(ucache);
            if (ucache->invoke != jl_fptr_sparam &&
                ucache->invoke != jl_fptr_interpret_call) {
                JL_UNLOCK(&codegen_lock);
                return ucache;
            }
            jl_code_instance_t *codeinst = jl_set_method_inferred(mi, (jl_value_t*)jl_any_type, NULL, NULL,
                0, 1, ~(size_t)0);
            codeinst->specptr = ucache->specptr;
            codeinst->rettype_const = ucache->rettype_const;
            jl_atomic_store_release(&codeinst->invoke, ucache->invoke);
            JL_UNLOCK(&codegen_lock);
            return codeinst;
        }
    }

    JL_UNLOCK(&codegen_lock);
    jl_generate_fptr(codeinst);
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
    jl_value_t *matches = jl_matching_methods(types, 1, 1, world, min_valid, max_valid);
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
        jl_methtable_t *mt = jl_method_table_for((jl_value_t*)ti);
        if ((jl_value_t*)mt != jl_nothing) {
            // get the specialization without caching it
            if (mt_cache && ((jl_datatype_t*)ti)->isdispatchtuple) {
                // Since we also use this presence in the cache
                // to trigger compilation when producing `.ji` files,
                // inject it there now if we think it will be
                // used via dispatch later (e.g. because it was hinted via a call to `precompile`)
                JL_LOCK(&mt->writelock);
                nf = cache_method(mt, &mt->cache, (jl_value_t*)mt, ti, m, world, env);
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
    int generating_llvm = jl_options.outputo || jl_options.outputbc || jl_options.outputunoptbc;
    jl_code_info_t *src = NULL;
    // If we are saving ji files (e.g. package pre-compilation or intermediate sysimg build steps),
    // don't bother generating anything since it won't be saved.
    if (jl_rettype_inferred(mi, world, world) == jl_nothing)
        src = jl_type_infer(mi, world, 1);
    if (generating_llvm) {
        jl_value_t *codeinst = jl_rettype_inferred(mi, world, world);
        if (codeinst != jl_nothing)
            if (((jl_code_instance_t*)codeinst)->invoke == jl_fptr_const_return)
                return; // probably not a good idea to generate code
        // If we are saving LLVM or native code, generate the LLVM IR so that it'll
        // be included in the saved LLVM module.
        (void)jl_compile_linfo(mi, src, world, &jl_default_cgparams);
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

#ifndef NDEBUG
// see if a call to m with a subtype of `types` might be ambiguous
static int jl_has_call_ambiguities(jl_value_t *types, jl_method_t *m, size_t world)
{
    if (m->ambig == jl_nothing)
        return 0;
    for (size_t i = 0; i < jl_array_len(m->ambig); i++) {
        jl_typemap_entry_t *mambig = (jl_typemap_entry_t*)jl_array_ptr_ref(m->ambig, i);
        if (mambig->min_world <= world && world <= mambig->max_world)
            if (!jl_has_empty_intersection((jl_value_t*)mambig->sig, types))
                return 1;
    }
    return 0;
}
#endif

JL_DLLEXPORT jl_value_t *jl_get_spec_lambda(jl_tupletype_t *types, size_t world, size_t *min_valid, size_t *max_valid)
{
    jl_method_instance_t *mi = jl_get_specialization1(types, world, min_valid, max_valid, 0);
    if (!mi)
        return jl_nothing;
    assert(!jl_has_call_ambiguities((jl_value_t*)types, mi->def.method, world));
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
    assert(jl_typeof(jl_typeof(v)));
    return v;
}

STATIC_INLINE jl_value_t *_jl_invoke(jl_value_t *F, jl_value_t **args, uint32_t nargs, jl_method_instance_t *mfunc, size_t world)
{
    // manually inline key parts of jl_compile_method_internal:
    jl_code_instance_t *codeinst = mfunc->cache;
    while (codeinst) {
        if (codeinst->min_world <= world && world <= codeinst->max_world && codeinst->invoke != NULL) {
            jl_value_t *res = codeinst->invoke(F, args, nargs, codeinst);
            return verify_type(res);
        }
        codeinst = codeinst->next;
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
    // if no method was found in the associative cache, check the full cache
    if (i == 4) {
        JL_TIMING(METHOD_LOOKUP_FAST);
        mt = jl_gf_mtable(F);
        entry = jl_typemap_assoc_exact(mt->cache, F, args, nargs, jl_cachearg_offset(mt), world);
        if (entry && entry->isleafsig && entry->simplesig == (void*)jl_nothing && entry->guardsigs == jl_emptysvec) {
            // put the entry into the cache if it's valid for a leafsig lookup,
            // using pick_which to slightly randomize where it ends up
            call_cache[cache_idx[++pick_which[cache_idx[0]] & 3]] = entry;
            goto have_entry;
        }
    }

    jl_method_instance_t *mfunc;
    if (entry) {
have_entry:
        mfunc = entry->func.linfo;
    }
    else {
        int64_t last_alloc = jl_options.malloc_log ? jl_gc_diff_total_bytes() : 0;
        JL_LOCK(&mt->writelock);
        // cache miss case
        JL_TIMING(METHOD_LOOKUP_SLOW);
        jl_tupletype_t *tt = arg_type_tuple(F, args, nargs);
        JL_GC_PUSH1(&tt);
        mfunc = jl_mt_assoc_by_type(mt, tt, /*cache*/1, world);
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

JL_DLLEXPORT jl_value_t *jl_gf_invoke_lookup(jl_value_t *types JL_PROPAGATES_ROOT, size_t world)
{
    jl_methtable_t *mt = jl_method_table_for(types);
    if ((jl_value_t*)mt == jl_nothing)
        return jl_nothing;

    // XXX: return min/max world
    struct jl_typemap_assoc search = {(jl_value_t*)types, world, 0, NULL, 0, ~(size_t)0};
    jl_typemap_entry_t *entry = jl_typemap_assoc_by_type(mt->defs, &search, /*offs*/0, /*subtype*/1);
    if (entry == NULL)
        return jl_nothing;
    jl_typemap_entry_t *m = jl_typemap_morespecific_by_type(entry, (jl_value_t*)types, NULL, world);
    if (m == NULL)
        return jl_nothing;
    return (jl_value_t*)m;
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
    jl_typemap_entry_t *entry = (jl_typemap_entry_t*)jl_gf_invoke_lookup(types, world);

    if ((jl_value_t*)entry == jl_nothing) {
        jl_method_error_bare(gf, types0, world);
        // unreachable
    }

    // now we have found the matching definition.
    // next look for or create a specialization of this definition.
    JL_GC_POP();
    jl_method_t *method = entry->func.method;
    JL_GC_PROMISE_ROOTED(method);
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

        mfunc = cache_method(NULL, &method->invokes, (jl_value_t*)method, tt, method, 1, tpenv);
        JL_UNLOCK(&method->writelock);
        JL_GC_POP();
        if (jl_options.malloc_log)
            jl_gc_sync_total_bytes(last_alloc); // discard allocation count from compilation
    }
    JL_GC_PROMISE_ROOTED(mfunc);
    size_t world = jl_get_ptls_states()->world_age;
    return _jl_invoke(gf, args, nargs - 1, mfunc, world);
}

JL_DLLEXPORT jl_value_t *jl_get_invoke_lambda(jl_typemap_entry_t *entry, jl_value_t *tt)
{
    // TODO: refactor this method to be more like `jl_get_specialization1`
    if (!jl_is_datatype(tt) || !((jl_datatype_t*)tt)->isdispatchtuple)
        return jl_nothing;

    jl_method_t *method = entry->func.method;
    jl_typemap_entry_t *tm = NULL;
    struct jl_typemap_assoc search = {(jl_value_t*)tt, 1, 0, NULL, 0, ~(size_t)0};
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
                                               (jl_tupletype_t*)tt, method, 1, tpenv);
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
    struct typemap_intersection_env match;
    // results:
    jl_value_t *t; // array of svec(argtypes, params, Method)
    size_t min_valid;
    size_t max_valid;
    // temporary:
    jl_svec_t *matc; // current working svec
    htable_t visited;
    // inputs:
    size_t world;
    int lim;
    int include_ambiguous; // whether ambiguous matches should be included
    int stop_if_deleted; // whether encountering a conflicting deleted match should abort the search
};

static int ml_matches_visitor(jl_typemap_entry_t *ml, struct typemap_intersection_env *closure0)
{
    struct ml_matches_env *closure = container_of(closure0, struct ml_matches_env, match);
    int deleted = 0;
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
        deleted = 1;
        if (closure->stop_if_deleted)
            closure->t = jl_false;
        return closure->stop_if_deleted ? 0 : 1;
    }
    else {
        // intersect the env valid range with method's valid range
        if (closure->min_valid < ml->min_world)
            closure->min_valid = ml->min_world;
        if (closure->max_valid > ml->max_world)
            closure->max_valid = ml->max_world;
    }
    jl_method_t *meth = ml->func.method;
    assert(meth);
    // see if we've already visited this due to method table ordering
    if ((jl_value_t*)meth->ambig != jl_nothing) {
        // keep track that we've already visited this ambiguous method
        // since we could see it again
        void **visited = ptrhash_bp(&closure->visited, (void*)ml);
        if (*visited != HT_NOTFOUND)
            return 1;
        *visited = (void*)visited;
    }
    else if (ptrhash_get(&closure->visited, (void*)ml) != HT_NOTFOUND) {
        return 1; // already visited this (via the `resorted` or `ambig` lists)
    }
    // a method is shadowed if type <: S <: m->sig where S is the
    // signature of another applicable method
    /*
      more generally, we can stop when the type is a subtype of the
      union of all the signatures examined so far.
    */
    int done = closure0->issubty && !deleted; // stop; signature fully covers queried type
    // if this method would never actually match, we shouldn't add it to the results
    // in certain cases
    int return_this_match = 1;
    if ((jl_value_t*)meth->resorted != jl_nothing) {
        // first consider adding any more specific matching methods that got put in the table later than us
        jl_value_t *ti = closure->match.ti;
        jl_svec_t *env = closure->match.env;
        JL_GC_PUSH2(&ti, &env);
        size_t j, l = jl_array_len(meth->resorted);
        for (j = 0; j < l; j++) {
            jl_typemap_entry_t *prior = (jl_typemap_entry_t*)jl_array_ptr_ref(meth->resorted, j);
            if (ptrhash_get(&closure->visited, (void*)prior) != HT_NOTFOUND)
                continue; // we've already considered this method
            closure->match.env = jl_emptysvec;
            closure->match.ti = jl_type_intersection_env_s((jl_value_t*)closure->match.type,
                    (jl_value_t*)prior->sig, &closure->match.env, &closure->match.issubty);
            if (closure->match.issubty)
                return_this_match = 0;
            closure->match.issubty = 0; // don't return 0 recursively below
            if (closure->match.ti != (jl_value_t*)jl_bottom_type) {
                // also may need to check `prior` against all `meth->ambig`
                if (!closure->include_ambiguous && (jl_value_t*)meth->ambig != jl_nothing) {
                    size_t j, l = jl_array_len(meth->ambig);
                    for (j = 0; j < l; j++) {
                        jl_typemap_entry_t *mambig = (jl_typemap_entry_t*)jl_array_ptr_ref(meth->ambig, j);
                        jl_value_t *sig2 = (jl_value_t*)mambig->sig;
                        // TODO: involve and update world
                        if (jl_subtype(closure->match.ti, sig2)) {
                            if (!jl_type_morespecific((jl_value_t*)prior->sig, (jl_value_t*)sig2))
                                break;
                        }
                    }
                    if (j != l)
                        continue; // skip this match--it's not unambiguously better
                }
                if (!ml_matches_visitor(prior, closure0)) {
                    JL_GC_POP();
                    return 0; // enough matches already--fast terminate
                }
                ptrhash_put(&closure->visited, (void*)prior, (void*)prior); // won't need to consider it again
            }
        }
        JL_GC_POP();
        closure->match.ti = ti;
        closure->match.env = env;
    }
    if (return_this_match && !closure->include_ambiguous && (jl_value_t*)meth->ambig != jl_nothing) {
        // the current method definitely never matches if the intersection with this method
        // is also fully ambiguous with another method's signature
        size_t j, l = jl_array_len(meth->ambig);
        for (j = 0; j < l; j++) {
            jl_typemap_entry_t *mambig = (jl_typemap_entry_t*)jl_array_ptr_ref(meth->ambig, j);
            jl_value_t *sig2 = (jl_value_t*)mambig->sig;
            // TODO: involve and update world
            if (jl_subtype(closure->match.ti, sig2)) {
                return_this_match = 0;
                break;
            }
        }
    }
    size_t len = jl_array_len(closure->t);
    if (return_this_match && closure->lim >= 0) {
        // we can skip this match if the type intersection is already fully covered
        // by a prior (more specific) match. but only do this in
        // the "limited" mode used by type inference.
        int i;
        for (i = 0; i < len; i++) {
            jl_method_t *prior_match = (jl_method_t*)jl_svecref(jl_array_ptr_ref(closure->t, i), 2);
            jl_value_t *sig2 = (jl_value_t*)prior_match->sig;
            if (closure->include_ambiguous && (jl_value_t*)meth->ambig != jl_nothing) {
                // check that prior sig is not actually just ambiguous with this--may need to include both
                size_t j, l = jl_array_len(meth->ambig);
                for (j = 0; j < l; j++) {
                    jl_typemap_entry_t *mambig = (jl_typemap_entry_t*)jl_array_ptr_ref(meth->ambig, j);
                    if (mambig->func.method == prior_match)
                        break; // continue below
                }
                if (j != l)
                    continue; // still include this match
            }
            if (jl_subtype(closure->match.ti, sig2)) {
                return_this_match = 0;
                break;
            }
        }
    }
    if (return_this_match) {
        if (closure->lim >= 0 && len >= closure->lim) {
            closure->t = (jl_value_t*)jl_false;
            return 0; // too many matches--terminate search
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
    if (closure->include_ambiguous && (jl_value_t*)meth->ambig != jl_nothing) {
        size_t j, l = jl_array_len(meth->ambig);
        for (j = 0; j < l; j++) {
            jl_typemap_entry_t *prior = (jl_typemap_entry_t*)jl_array_ptr_ref(meth->ambig, j);
            closure->match.env = jl_emptysvec;
            closure->match.ti = jl_type_intersection_env((jl_value_t*)closure->match.type,
                    (jl_value_t*)prior->sig, &closure->match.env);
            if (closure->match.ti != (jl_value_t*)jl_bottom_type) {
                if (!ml_matches_visitor(prior, closure0))
                    return 0; // enough matches already--fast terminate
            }
        }
    }
    return !done;
}

struct jl_timing_params {
    uint64_t start;
    jl_value_t *obj;
};
static void _jl_timing_emit(struct jl_timing_params *timing)
{
    jl_printf(JL_STDOUT, "%llu ", uv_hrtime() - timing->start);
    jl_static_show(JL_STDOUT, timing->obj);
    jl_printf(JL_STDOUT, "\n");
}

#define STDERR_TIMING(obj_) \
    __attribute__((cleanup(_jl_timing_emit))) \
    struct jl_timing_params __timing_obj; \
    __timing_obj.start = uv_hrtime(); \
    __timing_obj.obj = (jl_value_t*)(obj_);
//#undef STDERR_TIMING
//#define STDERR_TIMING(obj_)

// This is the collect form of calling jl_typemap_intersection_visitor
// with optimizations to skip fully shadowed methods.
//
// Returns a match as an array of svec(argtypes, static_params, Method).
// See below for the meaning of lim.
static jl_value_t *ml_matches(jl_methtable_t *mt, jl_tupletype_t *type,
                              int lim, int include_ambiguous, int stop_if_deleted, int insert_cache,
                              size_t world, size_t *min_valid, size_t *max_valid)
{
    //STDERR_TIMING(type);

    jl_value_t *unw = jl_unwrap_unionall((jl_value_t*)type);
    assert(jl_is_datatype(unw));
    if (jl_nparams(unw) == 2 && jl_tparam1(unw) == jl_char_type &&
        jl_is_type_type(jl_tparam0(unw)) && jl_tparam0(jl_tparam0(unw)) == jl_int64_type) {
        jl_static_show(JL_STDOUT, unw);
    }

    if (insert_cache && ((jl_datatype_t*)unw)->isdispatchtuple) { // scope block
        jl_typemap_t *cache = mt->cache;
        struct jl_typemap_assoc search = {(jl_value_t*)type, world, 0, NULL, 0, ~(size_t)0};
        jl_typemap_entry_t *entry = jl_typemap_assoc_by_type(cache, &search, jl_cachearg_offset(mt), /*subtype*/1);
        if (entry && entry->func.value && entry->guardsigs == jl_emptysvec) {
            jl_method_instance_t *mi = entry->func.linfo;
            jl_method_t *meth = mi->def.method;
            jl_value_t *tpenv2 = (jl_value_t*)jl_emptysvec;
            jl_value_t *types2 = NULL;
            JL_GC_PUSH2(&tpenv2, &types2);
            if (jl_egal((jl_value_t*)type, mi->specTypes)) {
                tpenv2 = (jl_value_t*)mi->sparam_vals;
                types2 = mi->specTypes;
            }
            else {
                // TODO: should we use jl_subtype_env instead (since we know that `type <: meth->sig` by transitivity)
                types2 = jl_type_intersection_env((jl_value_t*)type, (jl_value_t*)meth->sig, (jl_svec_t**)&tpenv2);
            }
            types2 = (jl_value_t*)jl_svec(3, types2, tpenv2, meth);
            tpenv2 = (jl_value_t*)jl_alloc_vec_any(1);
            jl_array_ptr_set(tpenv2, 0, types2);
            *min_valid = entry->min_world;
            *max_valid = entry->max_world;
            JL_GC_POP();
            return tpenv2;
        }
    }

    size_t l = jl_svec_len(((jl_datatype_t*)unw)->parameters);
    jl_value_t *va = NULL;
    if (l > 0) {
        va = jl_tparam(unw, l - 1);
        if (jl_is_vararg_type(va))
            va = jl_unwrap_vararg(va);
        else
            va = NULL;
    }
    struct ml_matches_env env = {{ml_matches_visitor, (jl_value_t*)type, va}};
    env.match.ti = NULL;
    env.match.env = jl_emptysvec;
    env.t = jl_an_empty_vec_any;
    env.matc = NULL;
    env.lim = lim;
    env.include_ambiguous = include_ambiguous;
    env.stop_if_deleted = stop_if_deleted;
    env.world = world;
    env.min_valid = *min_valid;
    env.max_valid = *max_valid;
    struct jl_typemap_assoc search = {(jl_value_t*)type, world, stop_if_deleted ? (~(size_t)0) >> 1 : 0, jl_emptysvec, *min_valid, *max_valid};
    JL_GC_PUSH5(&env.t, &env.matc, &env.match.env, &search.env, &env.match.ti);
    htable_new(&env.visited, 0);
    jl_typemap_t *defs = mt->defs;
    int offs = 0;
    if (((jl_datatype_t*)unw)->isdispatchtuple) {
        jl_typemap_entry_t *ml = jl_typemap_assoc_by_type(defs, &search, offs, /*subtype*/1);
        env.min_valid = search.min_valid;
        env.max_valid = search.max_valid;
        if (ml) {
            if (stop_if_deleted && ml->max_world < world) {
                env.t = jl_false;
            }
            else {
                env.match.ti = (jl_value_t*)type;
                env.match.env = search.env;
                env.match.issubty = 1;
                env.match.fptr(ml, &env.match); // also matches all ambig and prior, as needed
            }
        }
    }
    else {
        jl_typemap_intersection_visitor(defs, offs, &env.match);
    }
    htable_free(&env.visited);
    // insert_cache is false if we're being called from inside cache_method, because we need a base case
    if (insert_cache && ((jl_datatype_t*)unw)->isdispatchtuple && jl_is_array(env.t) && jl_array_len(env.t) == 1) { // scope block
        env.matc = (jl_svec_t*)jl_array_ptr_ref(env.t, 0); // (ti, env, meth)
        jl_method_t *meth = (jl_method_t*)jl_svecref(env.matc, 2);
        jl_svec_t *spenv = (jl_svec_t*)jl_svecref(env.matc, 1);
        jl_tupletype_t *ti = (jl_tupletype_t*)jl_svecref(env.matc, 0);
        jl_method_instance_t *mi = cache_method(mt, &mt->cache, (jl_value_t*)mt, ti, meth, world, spenv);
        (void)mi;
    }
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

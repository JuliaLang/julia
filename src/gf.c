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

JL_DLLEXPORT _Atomic(size_t) jl_world_counter = 1; // uses atomic acquire/release
JL_DLLEXPORT size_t jl_get_world_counter(void) JL_NOTSAFEPOINT
{
    jl_task_t *ct = jl_current_task;
    if (ct->ptls->in_pure_callback)
        return ~(size_t)0;
    return jl_atomic_load_acquire(&jl_world_counter);
}

JL_DLLEXPORT size_t jl_get_tls_world_age(void) JL_NOTSAFEPOINT
{
    return jl_current_task->world_age;
}

/// ----- Handling for Julia callbacks ----- ///

JL_DLLEXPORT int8_t jl_is_in_pure_context(void)
{
    jl_task_t *ct = jl_current_task;
    return ct->ptls->in_pure_callback;
}

tracer_cb jl_newmeth_tracer = NULL;
JL_DLLEXPORT void jl_register_newmeth_tracer(void (*callback)(jl_method_t *tracee))
{
    jl_newmeth_tracer = (tracer_cb)callback;
}

void jl_call_tracer(tracer_cb callback, jl_value_t *tracee)
{
    jl_task_t *ct = jl_current_task;
    int last_in = ct->ptls->in_pure_callback;
    JL_TRY {
        ct->ptls->in_pure_callback = 1;
        callback(tracee);
        ct->ptls->in_pure_callback = last_in;
    }
    JL_CATCH {
        ct->ptls->in_pure_callback = last_in;
        jl_printf((JL_STREAM*)STDERR_FILENO, "WARNING: tracer callback function threw an error:\n");
        jl_static_show((JL_STREAM*)STDERR_FILENO, jl_current_exception());
        jl_printf((JL_STREAM*)STDERR_FILENO, "\n");
        jlbacktrace(); // written to STDERR_FILENO
    }
}

/// ----- Definitions for various internal TypeMaps ----- ///

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
static jl_method_instance_t *jl_specializations_get_linfo_(jl_method_t *m JL_PROPAGATES_ROOT, jl_value_t *type, jl_svec_t *sparams, jl_method_instance_t *mi_insert)
{
    if (m->sig == (jl_value_t*)jl_anytuple_type && jl_atomic_load_relaxed(&m->unspecialized) != NULL)
        return jl_atomic_load_relaxed(&m->unspecialized); // handle builtin methods
    jl_value_t *ut = jl_is_unionall(type) ? jl_unwrap_unionall(type) : type;
    JL_TYPECHK(specializations, datatype, ut);
    uint_t hv = ((jl_datatype_t*)ut)->hash;
    for (int locked = 0; ; locked++) {
        jl_array_t *speckeyset = jl_atomic_load_acquire(&m->speckeyset);
        jl_svec_t *specializations = jl_atomic_load_relaxed(&m->specializations);
        size_t i = -1, cl = jl_svec_len(specializations);
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
            _Atomic(jl_method_instance_t*) *data = (_Atomic(jl_method_instance_t*)*)jl_svec_data(specializations);
            JL_GC_PUSH1(&specializations); // clang-sa doesn't realize this loop uses specializations
            for (i = cl; i > 0; i--) {
                jl_method_instance_t *mi = jl_atomic_load_relaxed(&data[i - 1]);
                if ((jl_value_t*)mi == jl_nothing)
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
                _Atomic(jl_method_instance_t*) *data = (_Atomic(jl_method_instance_t*)*)jl_svec_data(specializations);
                for (i = 0; i < cl; i++) {
                    jl_method_instance_t *mi = jl_atomic_load_relaxed(&data[i]);
                    if ((jl_value_t*)mi == jl_nothing)
                        break;
                    assert(!jl_types_equal(mi->specTypes, type));
                }
            }
            jl_method_instance_t *mi = mi_insert ? mi_insert : jl_get_specialized(m, type, sparams);
            JL_GC_PUSH1(&mi);
            if (hv ? (i + 1 >= cl || jl_svecref(specializations, i + 1) != jl_nothing) : (i <= 1 || jl_svecref(specializations, i - 2) != jl_nothing)) {
                size_t ncl = cl < 8 ? 8 : (cl*3)>>1;
                jl_svec_t *nc = jl_alloc_svec_uninit(ncl);
                if (i > 0)
                    memcpy((char*)jl_svec_data(nc), jl_svec_data(specializations), sizeof(void*) * i);
                for (int j = 0; j < ncl - cl; j++)
                    jl_svecset(nc, j+i, jl_nothing);
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
            assert(jl_svecref(specializations, i) == jl_nothing);
            jl_svecset(specializations, i, mi); // jl_atomic_store_relaxed?
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

JL_DLLEXPORT jl_method_instance_t *jl_specializations_get_linfo(jl_method_t *m JL_PROPAGATES_ROOT, jl_value_t *type, jl_svec_t *sparams)
{
    return jl_specializations_get_linfo_(m, type, sparams, NULL);
}

jl_method_instance_t *jl_specializations_get_or_insert(jl_method_instance_t *mi)
{
    jl_method_t *m = mi->def.method;
    jl_value_t *type = mi->specTypes;
    jl_svec_t *sparams = mi->sparam_vals;
    return jl_specializations_get_linfo_(m, type, sparams, mi);
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
    jl_typemap_entry_t *sf = jl_typemap_assoc_by_type(jl_atomic_load_relaxed(&mt->defs), &search, /*offs*/0, /*subtype*/0);
    if (!sf)
        return jl_nothing;
    return sf->func.value;
}

// ----- MethodInstance specialization instantiation ----- //

JL_DLLEXPORT jl_code_instance_t* jl_new_codeinst(
        jl_method_instance_t *mi, jl_value_t *rettype,
        jl_value_t *inferred_const, jl_value_t *inferred,
        int32_t const_flags, size_t min_world, size_t max_world,
        uint32_t ipo_effects, uint32_t effects, jl_value_t *argescapes,
        uint8_t relocatability);

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
    m->nospecialize = 0;
    m->nospecialize = ~m->nospecialize;

    jl_methtable_t *mt = dt->name->mt;
    jl_typemap_entry_t *newentry = NULL;
    JL_GC_PUSH2(&m, &newentry);

    newentry = jl_typemap_alloc(jl_anytuple_type, NULL, jl_emptysvec,
            (jl_value_t*)m, 1, ~(size_t)0);
    jl_typemap_insert(&mt->defs, (jl_value_t*)mt, newentry, 0);

    jl_method_instance_t *mi = jl_get_specialized(m, (jl_value_t*)jl_anytuple_type, jl_emptysvec);
    jl_atomic_store_relaxed(&m->unspecialized, mi);
    jl_gc_wb(m, mi);

    jl_code_instance_t *codeinst = jl_new_codeinst(mi,
        (jl_value_t*)jl_any_type, jl_nothing, jl_nothing,
        0, 1, ~(size_t)0, 0, 0, jl_nothing, 0);
    jl_mi_cache_insert(mi, codeinst);
    jl_atomic_store_relaxed(&codeinst->specptr.fptr1, fptr);
    jl_atomic_store_relaxed(&codeinst->invoke, jl_fptr_args);

    newentry = jl_typemap_alloc(jl_anytuple_type, NULL, jl_emptysvec,
            (jl_value_t*)mi, 1, ~(size_t)0);
    jl_typemap_insert(&mt->cache, (jl_value_t*)mt, newentry, 0);

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
    jl_task_t *ct = jl_current_task;
    if (ct->reentrant_inference == (uint16_t)-1) {
        // We must avoid attempting to re-enter inference here
        assert(0 && "attempted to enter inference while writing out image");
        abort();
    }
    if (ct->reentrant_inference > 2)
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
    int last_errno = errno;
#ifdef _OS_WINDOWS_
    DWORD last_error = GetLastError();
#endif
    size_t last_age = ct->world_age;
    ct->world_age = jl_typeinf_world;
    mi->inInference = 1;
    ct->reentrant_inference++;
    JL_TRY {
        src = (jl_code_info_t*)jl_apply(fargs, 3);
    }
    JL_CATCH {
        jl_value_t *e = jl_current_exception();
        if (e == jl_stackovf_exception) {
            jl_printf((JL_STREAM*)STDERR_FILENO, "Internal error: stack overflow in type inference of ");
            jl_static_show_func_sig((JL_STREAM*)STDERR_FILENO, (jl_value_t*)mi->specTypes);
            jl_printf((JL_STREAM*)STDERR_FILENO, ".\n");
            jl_printf((JL_STREAM*)STDERR_FILENO, "This might be caused by recursion over very long tuples or argument lists.\n");
        }
        else {
            jl_printf((JL_STREAM*)STDERR_FILENO, "Internal error: encountered unexpected error in runtime:\n");
            jl_static_show((JL_STREAM*)STDERR_FILENO, e);
            jl_printf((JL_STREAM*)STDERR_FILENO, "\n");
            jlbacktrace(); // written to STDERR_FILENO
        }
        src = NULL;
    }
    ct->world_age = last_age;
    ct->reentrant_inference--;
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
    jl_task_t *ct = jl_current_task;
    size_t last_age = ct->world_age;
    ct->world_age = jl_typeinf_world;
    jl_value_t *ret = jl_apply(args, nargs);
    ct->world_age = last_age;
    return ret;
}

JL_DLLEXPORT jl_value_t *jl_rettype_inferred(jl_method_instance_t *mi, size_t min_world, size_t max_world) JL_NOTSAFEPOINT
{
    jl_code_instance_t *codeinst = jl_atomic_load_relaxed(&mi->cache);
    while (codeinst) {
        if (codeinst->min_world <= min_world && max_world <= codeinst->max_world) {
            jl_value_t *code = jl_atomic_load_relaxed(&codeinst->inferred);
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
    jl_code_instance_t *codeinst = jl_atomic_load_relaxed(&mi->cache);
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
        0, min_world, max_world, 0, 0, jl_nothing, 0);
    jl_mi_cache_insert(mi, codeinst);
    return codeinst;
}

JL_DLLEXPORT jl_code_instance_t *jl_new_codeinst(
        jl_method_instance_t *mi, jl_value_t *rettype,
        jl_value_t *inferred_const, jl_value_t *inferred,
        int32_t const_flags, size_t min_world, size_t max_world,
        uint32_t ipo_effects, uint32_t effects, jl_value_t *argescapes,
        uint8_t relocatability
        /*, jl_array_t *edges, int absolute_max*/)
{
    jl_task_t *ct = jl_current_task;
    assert(min_world <= max_world && "attempting to set invalid world constraints");
    jl_code_instance_t *codeinst = (jl_code_instance_t*)jl_gc_alloc(ct->ptls, sizeof(jl_code_instance_t),
            jl_code_instance_type);
    codeinst->def = mi;
    codeinst->min_world = min_world;
    codeinst->max_world = max_world;
    codeinst->rettype = rettype;
    jl_atomic_store_release(&codeinst->inferred, inferred);
    //codeinst->edges = NULL;
    if ((const_flags & 2) == 0)
        inferred_const = NULL;
    codeinst->rettype_const = inferred_const;
    jl_atomic_store_relaxed(&codeinst->specptr.fptr, NULL);
    jl_atomic_store_relaxed(&codeinst->invoke, NULL);
    if ((const_flags & 1) != 0) {
        assert(const_flags & 2);
        jl_atomic_store_relaxed(&codeinst->invoke, jl_fptr_const_return);
    }
    jl_atomic_store_relaxed(&codeinst->specsigflags, 0);
    jl_atomic_store_relaxed(&codeinst->precompile, 0);
    jl_atomic_store_relaxed(&codeinst->next, NULL);
    codeinst->ipo_purity_bits = ipo_effects;
    jl_atomic_store_relaxed(&codeinst->purity_bits, effects);
    codeinst->argescapes = argescapes;
    codeinst->relocatability = relocatability;
    return codeinst;
}

JL_DLLEXPORT void jl_mi_cache_insert(jl_method_instance_t *mi JL_ROOTING_ARGUMENT,
                                     jl_code_instance_t *ci JL_ROOTED_ARGUMENT JL_MAYBE_UNROOTED)
{
    JL_GC_PUSH1(&ci);
    if (jl_is_method(mi->def.method))
        JL_LOCK(&mi->def.method->writelock);
    jl_code_instance_t *oldci = jl_atomic_load_relaxed(&mi->cache);
    jl_atomic_store_relaxed(&ci->next, oldci);
    if (oldci)
        jl_gc_wb(ci, oldci);
    jl_atomic_store_release(&mi->cache, ci);
    jl_gc_wb(mi, ci);
    if (jl_is_method(mi->def.method))
        JL_UNLOCK(&mi->def.method->writelock);
    JL_GC_POP();
    return;
}

static int get_method_unspec_list(jl_typemap_entry_t *def, void *closure)
{
    jl_svec_t *specializations = jl_atomic_load_relaxed(&def->func.method->specializations);
    size_t i, l = jl_svec_len(specializations);
    size_t world = jl_atomic_load_acquire(&jl_world_counter);
    for (i = 0; i < l; i++) {
        jl_method_instance_t *mi = (jl_method_instance_t*)jl_svecref(specializations, i);
        if ((jl_value_t*)mi != jl_nothing) {
            assert(jl_is_method_instance(mi));
            if (jl_rettype_inferred(mi, world, world) == jl_nothing)
                jl_array_ptr_1d_push((jl_array_t*)closure, (jl_value_t*)mi);
        }
    }
    return 1;
}

int foreach_mtable_in_module(
        jl_module_t *m,
        int (*visit)(jl_methtable_t *mt, void *env),
        void *env)
{
    jl_svec_t *table = jl_atomic_load_relaxed(&m->bindings);
    for (size_t i = 0; i < jl_svec_len(table); i++) {
        jl_binding_t *b = (jl_binding_t*)jl_svec_ref(table, i);
        if ((void*)b == jl_nothing)
            break;
        jl_sym_t *name = b->globalref->name;
        if (jl_atomic_load_relaxed(&b->owner) == b && b->constp) {
            jl_value_t *v = jl_atomic_load_relaxed(&b->value);
            if (v) {
                jl_value_t *uw = jl_unwrap_unionall(v);
                if (jl_is_datatype(uw)) {
                    jl_typename_t *tn = ((jl_datatype_t*)uw)->name;
                    if (tn->module == m && tn->name == name && tn->wrapper == v) {
                        // this is the original/primary binding for the type (name/wrapper)
                        jl_methtable_t *mt = tn->mt;
                        if (mt != NULL && (jl_value_t*)mt != jl_nothing && mt != jl_type_type_mt && mt != jl_nonfunction_mt) {
                            assert(mt->module == m);
                            if (!visit(mt, env))
                                return 0;
                        }
                    }
                }
                else if (jl_is_module(v)) {
                    jl_module_t *child = (jl_module_t*)v;
                    if (child != m && child->parent == m && child->name == name) {
                        // this is the original/primary binding for the submodule
                        if (!foreach_mtable_in_module(child, visit, env))
                            return 0;
                    }
                }
                else if (jl_is_mtable(v)) {
                    jl_methtable_t *mt = (jl_methtable_t*)v;
                    if (mt->module == m && mt->name == name) {
                        // this is probably an external method table here, so let's
                        // assume so as there is no way to precisely distinguish them
                        if (!visit(mt, env))
                            return 0;
                    }
                }
            }
        }
        table = jl_atomic_load_relaxed(&m->bindings);
    }
    return 1;
}

int jl_foreach_reachable_mtable(int (*visit)(jl_methtable_t *mt, void *env), void *env)
{
    if (!visit(jl_type_type_mt, env))
        return 0;
    if (!visit(jl_nonfunction_mt, env))
        return 0;
    jl_array_t *mod_array = jl_get_loaded_modules();
    if (mod_array) {
        JL_GC_PUSH1(&mod_array);
        int i;
        for (i = 0; i < jl_array_len(mod_array); i++) {
            jl_module_t *m = (jl_module_t*)jl_array_ptr_ref(mod_array, i);
            assert(jl_is_module(m));
            if (m->parent == m) // some toplevel modules (really just Base) aren't actually
                if (!foreach_mtable_in_module(m, visit, env)) {
                    JL_GC_POP();
                    return 0;
                }
        }
        JL_GC_POP();
    }
    else {
        if (!foreach_mtable_in_module(jl_main_module, visit, env))
            return 0;
        if (!foreach_mtable_in_module(jl_core_module, visit, env))
            return 0;
    }
    return 1;
}

static int reset_mt_caches(jl_methtable_t *mt, void *env)
{
    // removes all method caches
    // this might not be entirely safe (GC or MT), thus we only do it very early in bootstrapping
    if (!mt->frozen) { // make sure not to reset builtin functions
        jl_atomic_store_release(&mt->leafcache, (jl_array_t*)jl_an_empty_vec_any);
        jl_atomic_store_release(&mt->cache, jl_nothing);
    }
    jl_typemap_visitor(jl_atomic_load_relaxed(&mt->defs), get_method_unspec_list, env);
    return 1;
}


jl_function_t *jl_typeinf_func JL_GLOBALLY_ROOTED = NULL;
JL_DLLEXPORT size_t jl_typeinf_world = 1;

JL_DLLEXPORT void jl_set_typeinf_func(jl_value_t *f)
{
    size_t newfunc = jl_typeinf_world == 1 && jl_typeinf_func == NULL;
    jl_typeinf_func = (jl_function_t*)f;
    jl_typeinf_world = jl_get_tls_world_age();
    int world = jl_atomic_fetch_add(&jl_world_counter, 1) + 1; // make type-inference the only thing in this world
    if (newfunc) {
        // give type inference a chance to see all of these
        // TODO: also reinfer if max_world != ~(size_t)0
        jl_array_t *unspec = jl_alloc_vec_any(0);
        JL_GC_PUSH1(&unspec);
        jl_foreach_reachable_mtable(reset_mt_caches, (void*)unspec);
        size_t i, l;
        for (i = 0, l = jl_array_len(unspec); i < l; i++) {
            jl_method_instance_t *mi = (jl_method_instance_t*)jl_array_ptr_ref(unspec, i);
            if (jl_rettype_inferred(mi, world, world) == jl_nothing)
                jl_type_infer(mi, world, 1);
        }
        JL_GC_POP();
    }
}

static int very_general_type(jl_value_t *t)
{
    return (t == (jl_value_t*)jl_any_type || jl_types_equal(t, (jl_value_t*)jl_type_type));
}

jl_value_t *jl_nth_slot_type(jl_value_t *sig, size_t i) JL_NOTSAFEPOINT
{
    sig = jl_unwrap_unionall(sig);
    size_t len = jl_nparams(sig);
    if (i < len-1)
        return jl_tparam(sig, i);
    jl_value_t *p = jl_tparam(sig, len-1);
    if (jl_is_vararg(p))
        p = jl_unwrap_vararg(p);
    return p;
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

static jl_value_t *inst_varargp_in_env(jl_value_t *decl, jl_svec_t *sparams)
{
    jl_value_t *unw = jl_unwrap_unionall(decl);
    jl_value_t *vm = jl_tparam(unw, jl_nparams(unw) - 1);
    assert(jl_is_vararg(vm));
    int nsp = jl_svec_len(sparams);
    if (nsp > 0 && jl_has_free_typevars(vm)) {
        JL_GC_PUSH1(&vm);
        assert(jl_subtype_env_size(decl) == nsp);
        vm = jl_instantiate_type_in_env(vm, (jl_unionall_t*)decl, jl_svec_data(sparams));
        assert(jl_is_vararg(vm));
        // rewrap_unionall(lastdeclt, sparams) if any sparams isa TypeVar
        // for example, `Tuple{Vararg{Union{Nothing,Int,Val{T}}}} where T`
        // and the user called it with `Tuple{Vararg{Union{Nothing,Int},N}}`, then T is unbound
        jl_value_t **sp = jl_svec_data(sparams);
        while (jl_is_unionall(decl)) {
            jl_tvar_t *v = (jl_tvar_t*)*sp;
            if (jl_is_typevar(v)) {
                // must unwrap and re-wrap Vararg object explicitly here since jl_type_unionall handles it differently
                jl_value_t *T = ((jl_vararg_t*)vm)->T;
                jl_value_t *N = ((jl_vararg_t*)vm)->N;
                int T_has_tv = T && jl_has_typevar(T, v);
                int N_has_tv = N && jl_has_typevar(N, v); // n.b. JL_VARARG_UNBOUND check means this should be false
                assert(!N_has_tv || N == (jl_value_t*)v);
                if (T_has_tv)
                    vm = jl_type_unionall(v, T);
                if (N_has_tv)
                    N = NULL;
                vm = (jl_value_t*)jl_wrap_vararg(vm, N); // this cannot throw for these inputs
            }
            sp++;
            decl = ((jl_unionall_t*)decl)->body;
            nsp--;
        }
        assert(nsp == 0);
        JL_GC_POP();
    }
    return vm;
}

static jl_value_t *ml_matches(jl_methtable_t *mt,
                              jl_tupletype_t *type, int lim, int include_ambiguous,
                              int intersections, size_t world, int cache_result,
                              size_t *min_valid, size_t *max_valid, int *ambig);

// get the compilation signature specialization for this method
static void jl_compilation_sig(
    jl_tupletype_t *const tt, // the original tupletype of the call (or DataType from precompile)
    jl_svec_t *sparams,
    jl_method_t *definition,
    intptr_t nspec,
    // output:
    jl_svec_t **const newparams JL_REQUIRE_ROOTED_SLOT)
{
    assert(jl_is_tuple_type(tt));
    jl_value_t *decl = definition->sig;
    size_t nargs = definition->nargs; // == jl_nparams(jl_unwrap_unionall(decl));

    if (definition->generator) {
        // staged functions aren't optimized
        // so assume the caller was intelligent about calling us
        return;
    }

    if (decl == (jl_value_t*)jl_anytuple_type && jl_atomic_load_relaxed(&definition->unspecialized)) {
        *newparams = jl_anytuple_type->parameters; // handle builtin methods
        return;
    }

    // some early sanity checks
    size_t i, np = jl_nparams(tt);
    switch (jl_va_tuple_kind((jl_datatype_t*)decl)) {
    case JL_VARARG_NONE:
        if (jl_is_va_tuple(tt))
            // odd
            return;
        if (np != nargs)
            // there are not enough input parameters to make this into a compilation sig
            return;
        break;
    case JL_VARARG_INT:
    case JL_VARARG_BOUND:
        if (jl_is_va_tuple(tt))
            // the length needed is not known, but required for compilation
            return;
        if (np < nargs - 1)
            // there are not enough input parameters to make this into a compilation sig
            return;
        break;
    case JL_VARARG_UNBOUND:
        if (np < nspec && jl_is_va_tuple(tt))
            // there are insufficient given parameters for jl_isa_compileable_sig now to like this type
            // (there were probably fewer methods defined when we first selected this signature)
            return;
        break;
    }

    jl_value_t *type_i = NULL;
    JL_GC_PUSH1(&type_i);
    for (i = 0; i < np; i++) {
        jl_value_t *elt = jl_tparam(tt, i);
        if (jl_is_vararg(elt))
            elt = jl_unwrap_vararg(elt);
        jl_value_t *decl_i = jl_nth_slot_type(decl, i);
        type_i = jl_rewrap_unionall(decl_i, decl);
        size_t i_arg = (i < nargs - 1 ? i : nargs - 1);

        if (jl_is_kind(type_i)) {
            // if we can prove the match was against the kind (not a Type)
            // we want to put that in the cache instead
            if (!*newparams) *newparams = jl_svec_copy(tt->parameters);
            elt = type_i;
            jl_svecset(*newparams, i, elt);
        }
        else if (jl_is_type_type(elt)) {
            // if the declared type was not Any or Union{Type, ...},
            // then the match must been with the kind (e.g. UnionAll or DataType)
            // and the result of matching the type signature
            // needs to be restricted to the concrete type 'kind'
            jl_value_t *kind = jl_typeof(jl_tparam0(elt));
            if (jl_subtype(kind, type_i) && !jl_subtype((jl_value_t*)jl_type_type, type_i)) {
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
            if (!(jl_subtype(elt, type_i) && !jl_subtype((jl_value_t*)jl_type_type, type_i))) {
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
                    // n.b. it is possible here that !(elt <: decl_i), if elt was something unusual from intersection
                    // so this might narrow the result slightly, though still being compatible with the declared signature
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
            if (!jl_has_free_typevars(decl_i) && very_general_type(type_i)) {
                /*
                  Here's a fairly simple heuristic: if this argument slot's
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

                  But don't apply this heuristic if the argument is called (issue #36783).
                */
                int iscalled = i_arg > 0 && i_arg <= 8 && (definition->called & (1 << (i_arg - 1)));
                if (!iscalled) {
                    if (!*newparams) *newparams = jl_svec_copy(tt->parameters);
                    jl_svecset(*newparams, i, jl_type_type);
                }
            }
            else if (jl_is_type_type(jl_tparam0(elt)) &&
                     // try to give up on specializing type parameters for Type{Type{Type{...}}}
                     (jl_is_type_type(jl_tparam0(jl_tparam0(elt))) || !jl_has_free_typevars(decl_i))) {
                /*
                  actual argument was Type{...}, we computed its type as
                  Type{Type{...}}. we like to avoid unbounded nesting here, so
                  compile (and hopefully cache) the signature as Type{T},
                  unless something more specific like Type{Type{Int32}} was
                  actually declared. this can be determined using a type
                  intersection.
                */
                if (!*newparams) *newparams = jl_svec_copy(tt->parameters);
                if (i < nargs || !definition->isva) {
                    jl_value_t *di = jl_type_intersection(type_i, (jl_value_t*)jl_type_type);
                    assert(di != (jl_value_t*)jl_bottom_type);
                    // issue #11355: DataType has a UID and so would take precedence in the cache
                    if (jl_is_kind(di))
                        jl_svecset(*newparams, i, (jl_value_t*)jl_type_type);
                    else
                        jl_svecset(*newparams, i, di);
                }
                else {
                    jl_svecset(*newparams, i, (jl_value_t*)jl_type_type);
                }
            }
        }

        int notcalled_func = (i_arg > 0 && i_arg <= 8 && !(definition->called & (1 << (i_arg - 1))) &&
                              !jl_has_free_typevars(decl_i) &&
                              jl_subtype(elt, (jl_value_t*)jl_function_type));
        if (notcalled_func && (type_i == (jl_value_t*)jl_any_type ||
                               type_i == (jl_value_t*)jl_function_type ||
                               (jl_is_uniontype(type_i) && // Base.Callable
                                ((((jl_uniontype_t*)type_i)->a == (jl_value_t*)jl_function_type &&
                                  ((jl_uniontype_t*)type_i)->b == (jl_value_t*)jl_type_type) ||
                                 (((jl_uniontype_t*)type_i)->b == (jl_value_t*)jl_function_type &&
                                  ((jl_uniontype_t*)type_i)->a == (jl_value_t*)jl_type_type))))) {
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
    if (np >= nspec && jl_va_tuple_kind((jl_datatype_t*)decl) == JL_VARARG_UNBOUND) {
        if (!*newparams) *newparams = tt->parameters;
        type_i = jl_svecref(*newparams, nspec - 2);
        // if all subsequent arguments are subtypes of type_i, specialize
        // on that instead of decl. for example, if decl is
        // (Any...)
        // and type is
        // (Symbol, Symbol, Symbol)
        // then specialize as (Symbol...), but if type is
        // (Symbol, Int32, Expr)
        // then specialize as (Any...)
        size_t j = nspec - 1;
        int all_are_subtypes = 1;
        for (; j < jl_svec_len(*newparams); j++) {
            jl_value_t *paramj = jl_svecref(*newparams, j);
            if (jl_is_vararg(paramj))
                paramj = jl_unwrap_vararg(paramj);
            if (!jl_subtype(paramj, type_i)) {
                all_are_subtypes = 0;
                break;
            }
        }
        if (all_are_subtypes) {
            // avoid Vararg{Type{Type{...}}}
            if (jl_is_type_type(type_i) && jl_is_type_type(jl_tparam0(type_i)))
                type_i = (jl_value_t*)jl_type_type;
            type_i = (jl_value_t*)jl_wrap_vararg(type_i, (jl_value_t*)NULL); // this cannot throw for these inputs
        }
        else {
            type_i = inst_varargp_in_env(decl, sparams);
        }
        jl_svec_t *limited = jl_alloc_svec(nspec);
        size_t i;
        for (i = 0; i < nspec - 1; i++) {
            jl_svecset(limited, i, jl_svecref(*newparams, i));
        }
        jl_svecset(limited, i, type_i);
        *newparams = limited;
    }
    JL_GC_POP();
}

// compute whether this type signature is a possible return value from jl_compilation_sig given a concrete-type for `tt`
JL_DLLEXPORT int jl_isa_compileable_sig(
    jl_tupletype_t *type,
    jl_svec_t *sparams,
    jl_method_t *definition)
{
    jl_value_t *decl = definition->sig;

    if (!jl_is_datatype(type) || jl_has_free_typevars((jl_value_t*)type))
        return 0;
    if (definition->sig == (jl_value_t*)jl_anytuple_type && jl_atomic_load_relaxed(&definition->unspecialized))
        return jl_egal((jl_value_t*)type, definition->sig); // handle builtin methods

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
        jl_methtable_t *kwmt = mt == jl_kwcall_mt ? jl_kwmethod_table_for(decl) : mt;
        if ((jl_value_t*)mt != jl_nothing) {
            // try to refine estimate of min and max
            if (kwmt != NULL && kwmt != jl_type_type_mt && kwmt != jl_nonfunction_mt && kwmt != jl_kwcall_mt)
                // new methods may be added, increasing nspec_min later
                nspec_min = jl_atomic_load_relaxed(&kwmt->max_args) + 2 + 2 * (mt == jl_kwcall_mt);
            else
                // nspec is always nargs+1, regardless of the other contents of these mt
                nspec_max = nspec_min;
        }
        int isunbound = (jl_va_tuple_kind((jl_datatype_t*)decl) == JL_VARARG_UNBOUND);
        if (jl_is_vararg(jl_tparam(type, np - 1))) {
            if (!isunbound || np < nspec_min || np > nspec_max)
                return 0;
        }
        else {
            if (np < nargs - 1 || (isunbound && np >= nspec_max))
                return 0;
        }
    }
    else if (np != nargs || jl_is_vararg(jl_tparam(type, np - 1))) {
        return 0;
    }

    jl_value_t *type_i = NULL;
    JL_GC_PUSH1(&type_i);
    for (i = 0; i < np; i++) {
        jl_value_t *elt = jl_tparam(type, i);
        size_t i_arg = (i < nargs - 1 ? i : nargs - 1);

        if (jl_is_vararg(elt)) {
            type_i = inst_varargp_in_env(decl, sparams);
            if (jl_has_free_typevars(type_i)) {
                JL_GC_POP();
                return 0; // something went badly wrong?
            }
            if (jl_egal(elt, type_i))
                continue; // elt could be chosen by inst_varargp_in_env for these sparams
            elt = jl_unwrap_vararg(elt);
            if (jl_is_type_type(elt) && jl_is_type_type(jl_tparam0(elt))) {
                JL_GC_POP();
                return 0; // elt would be set equal to jl_type_type instead
            }
            // else, elt also needs to meet the usual rules
        }

        jl_value_t *decl_i = jl_nth_slot_type(decl, i);
        type_i = jl_rewrap_unionall(decl_i, decl);

        if (i_arg > 0 && i_arg <= sizeof(definition->nospecialize) * 8 &&
                (definition->nospecialize & (1 << (i_arg - 1)))) {
            if (!jl_has_free_typevars(decl_i) && !jl_is_kind(decl_i)) {
                if (jl_egal(elt, decl_i))
                    continue;
                JL_GC_POP();
                return 0;
            }
        }

        if (jl_is_kind(elt)) {
            // kind slots always get guard entries (checking for subtypes of Type)
            if (jl_subtype(elt, type_i) && !jl_subtype((jl_value_t*)jl_type_type, type_i))
                continue;
            // TODO: other code paths that could reach here?
            JL_GC_POP();
            return 0;
        }
        else if (jl_is_kind(type_i)) {
            JL_GC_POP();
            return 0;
        }

        if (jl_is_type_type(jl_unwrap_unionall(elt))) {
            int iscalled = (i_arg > 0 && i_arg <= 8 && (definition->called & (1 << (i_arg - 1)))) ||
                           jl_has_free_typevars(decl_i);
            if (jl_types_equal(elt, (jl_value_t*)jl_type_type)) {
                if (!iscalled && very_general_type(type_i))
                    continue;
                if (i >= nargs && definition->isva)
                    continue;
                JL_GC_POP();
                return 0;
            }
            if (!iscalled && very_general_type(type_i)) {
                JL_GC_POP();
                return 0;
            }
            if (!jl_is_datatype(elt)) {
                JL_GC_POP();
                return 0;
            }

            // if the declared type was not Any or Union{Type, ...},
            // then the match must been with kind, such as UnionAll or DataType,
            // and the result of matching the type signature
            // needs to be corrected to the concrete type 'kind' (and not to Type)
            jl_value_t *kind = jl_typeof(jl_tparam0(elt));
            if (kind == jl_bottom_type) {
                JL_GC_POP();
                return 0; // Type{Union{}} gets normalized to typeof(Union{})
            }
            if (jl_subtype(kind, type_i) && !jl_subtype((jl_value_t*)jl_type_type, type_i)) {
                JL_GC_POP();
                return 0; // gets turned into a kind
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
                    type_i = jl_type_intersection(type_i, (jl_value_t*)jl_type_type);
                    assert(type_i != (jl_value_t*)jl_bottom_type);
                    if (jl_is_kind(type_i)) {
                        JL_GC_POP();
                        return 0;
                    }
                    else if (!jl_types_equal(type_i, elt)) {
                        JL_GC_POP();
                        return 0;
                    }
                    continue;
                }
                else {
                    JL_GC_POP();
                    return 0;
                }
            }
            continue;
        }

        int notcalled_func = (i_arg > 0 && i_arg <= 8 && !(definition->called & (1 << (i_arg - 1))) &&
                              !jl_has_free_typevars(decl_i) &&
                              jl_subtype(elt, (jl_value_t*)jl_function_type));
        if (notcalled_func && (type_i == (jl_value_t*)jl_any_type ||
                               type_i == (jl_value_t*)jl_function_type ||
                               (jl_is_uniontype(type_i) && // Base.Callable
                                ((((jl_uniontype_t*)type_i)->a == (jl_value_t*)jl_function_type &&
                                  ((jl_uniontype_t*)type_i)->b == (jl_value_t*)jl_type_type) ||
                                 (((jl_uniontype_t*)type_i)->b == (jl_value_t*)jl_function_type &&
                                  ((jl_uniontype_t*)type_i)->a == (jl_value_t*)jl_type_type))))) {
            // and attempt to despecialize types marked Function, Callable, or Any
            // when called with a subtype of Function but is not called
            if (elt == (jl_value_t*)jl_function_type)
                continue;
            JL_GC_POP();
            return 0;
        }

        if (!jl_is_concrete_type(elt)) {
            JL_GC_POP();
            return 0;
        }
    }
    JL_GC_POP();
    return 1;
}


static int concretesig_equal(jl_value_t *tt, jl_value_t *simplesig) JL_NOTSAFEPOINT
{
    jl_value_t **types = jl_svec_data(((jl_datatype_t*)tt)->parameters);
    jl_value_t **sigs = jl_svec_data(((jl_datatype_t*)simplesig)->parameters);
    size_t i, lensig = jl_nparams(simplesig);
    assert(lensig == jl_nparams(tt));
    assert(lensig > 0 && !jl_is_vararg(jl_tparam(simplesig, lensig - 1)));
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
            entry = jl_atomic_load_relaxed(&entry->next);
        } while ((jl_value_t*)entry != jl_nothing);
    }
    return NULL;
}

static jl_method_instance_t *cache_method(
        jl_methtable_t *mt, _Atomic(jl_typemap_t*) *cache, jl_value_t *parent JL_PROPAGATES_ROOT,
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
        jl_typemap_entry_t *entry = jl_typemap_assoc_by_type(jl_atomic_load_relaxed(cache), &search, offs, /*subtype*/1);
        if (entry && entry->func.value)
            return entry->func.linfo;
    }

    jl_value_t *temp = NULL;
    jl_value_t *temp2 = NULL;
    jl_value_t *temp3 = NULL;
    jl_method_instance_t *newmeth = NULL;
    jl_svec_t *newparams = NULL;
    JL_GC_PUSH5(&temp, &temp2, &temp3, &newmeth, &newparams);

    // Consider if we can cache with the preferred compile signature
    // so that we can minimize the number of required cache entries.
    int cache_with_orig = 1;
    jl_tupletype_t *compilationsig = tt;
    jl_methtable_t *kwmt = mt == jl_kwcall_mt ? jl_kwmethod_table_for(definition->sig) : mt;
    intptr_t nspec = (kwmt == NULL || kwmt == jl_type_type_mt || kwmt == jl_nonfunction_mt || kwmt == jl_kwcall_mt ? definition->nargs + 1 : jl_atomic_load_relaxed(&kwmt->max_args) + 2 + 2 * (mt == jl_kwcall_mt));
    jl_compilation_sig(tt, sparams, definition, nspec, &newparams);
    if (newparams) {
        temp2 = (jl_value_t*)jl_apply_tuple_type(newparams);
        // Now there may be a problem: the widened signature is more general
        // than just the given arguments, so it might conflict with another
        // definition that does not have cache instances yet. To fix this, we
        // may insert guard cache entries for all intersections of this
        // signature and definitions. Those guard entries will supersede this
        // one in conflicted cases, alerting us that there should actually be a
        // cache miss. Alternatively, we may use the original signature in the
        // cache, but use this return for compilation.
        //
        // In most cases `!jl_isa_compileable_sig(tt, sparams, definition)`,
        // although for some cases, (notably Varargs)
        // we might choose a replacement type that's preferable but not strictly better
        int issubty;
        temp = jl_type_intersection_env_s(temp2, (jl_value_t*)definition->sig, &newparams, &issubty);
        assert(temp != (jl_value_t*)jl_bottom_type); (void)temp;
        if (jl_egal((jl_value_t*)newparams, (jl_value_t*)sparams)) {
            cache_with_orig = !issubty;
            compilationsig = (jl_datatype_t*)temp2;
        }
        newparams = NULL;
    }
    // TODO: maybe assert(jl_isa_compileable_sig(compilationsig, sparams, definition));
    newmeth = jl_specializations_get_linfo(definition, (jl_value_t*)compilationsig, sparams);

    jl_tupletype_t *cachett = tt;
    jl_svec_t* guardsigs = jl_emptysvec;
    if (!cache_with_orig && mt) {
        // now examine what will happen if we chose to use this sig in the cache
        size_t min_valid2 = 1;
        size_t max_valid2 = ~(size_t)0;
        temp = ml_matches(mt, compilationsig, MAX_UNSPECIALIZED_CONFLICTS, 1, 1, world, 0, &min_valid2, &max_valid2, NULL);
        int guards = 0;
        if (temp == jl_nothing) {
            cache_with_orig = 1;
        }
        else {
            int unmatched_tvars = 0;
            size_t i, l = jl_array_len(temp);
            for (i = 0; i < l; i++) {
                jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(temp, i);
                if (matc->method == definition)
                    continue;
                jl_svec_t *env = matc->sparams;
                int k, l;
                for (k = 0, l = jl_svec_len(env); k < l; k++) {
                    jl_value_t *env_k = jl_svecref(env, k);
                    if (jl_is_typevar(env_k) || jl_is_vararg(env_k)) {
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
                guards++;
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
                    //        NULL, jl_emptysvec, /*guard*/NULL, jl_cachearg_offset(mt), other->min_world, other->max_world);
                }
            }
            assert(guards == jl_svec_len(guardsigs));
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
        if (jl_is_vararg(elt)) {
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
        jl_typemap_entry_t *entry = jl_typemap_assoc_by_type(jl_atomic_load_relaxed(cache), &search, offs, /*subtype*/1);
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
        jl_array_t *oldcache = jl_atomic_load_relaxed(&mt->leafcache);
        jl_typemap_entry_t *old = (jl_typemap_entry_t*)jl_eqtable_get(oldcache, (jl_value_t*)tt, jl_nothing);
        jl_atomic_store_relaxed(&newentry->next, old);
        jl_gc_wb(newentry, old);
        jl_array_t *newcache = (jl_array_t*)jl_eqtable_put(jl_atomic_load_relaxed(&mt->leafcache), (jl_value_t*)tt, (jl_value_t*)newentry, NULL);
        if (newcache != oldcache) {
            jl_atomic_store_release(&mt->leafcache, newcache);
            jl_gc_wb(mt, newcache);
        }
    }
    else {
         jl_typemap_insert(cache, parent, newentry, offs);
    }

    JL_GC_POP();
    return newmeth;
}

static jl_method_match_t *_gf_invoke_lookup(jl_value_t *types JL_PROPAGATES_ROOT, jl_value_t *mt, size_t world, size_t *min_valid, size_t *max_valid);

static jl_method_instance_t *jl_mt_assoc_by_type(jl_methtable_t *mt JL_PROPAGATES_ROOT, jl_datatype_t *tt, size_t world)
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
    jl_typemap_entry_t *entry = jl_typemap_assoc_by_type(jl_atomic_load_relaxed(&mt->cache), &search, jl_cachearg_offset(mt), /*subtype*/1);
    if (entry)
        return entry->func.linfo;

    size_t min_valid = 0;
    size_t max_valid = ~(size_t)0;
    jl_method_match_t *matc = _gf_invoke_lookup((jl_value_t*)tt, jl_nothing, world, &min_valid, &max_valid);
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
        if (jl_is_vararg(va))
            va = jl_unwrap_vararg(va);
        else
            va = NULL;
    }
    struct matches_env env = {{get_intersect_visitor, (jl_value_t*)type, va,
            /* .ti = */ NULL, /* .env = */ jl_emptysvec, /* .issubty = */ 0},
        /* .newentry = */ newentry, /* .shadowed */ NULL};
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
    jl_datatype_t *dt = jl_nth_argument_datatype(oldvalue->sig, 1);
    if (dt == (jl_datatype_t*)jl_typeof(jl_kwcall_func))
        dt = jl_nth_argument_datatype(oldvalue->sig, 3);
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
    if (mt == jl_type_type_mt || mt == jl_nonfunction_mt || mt == jl_kwcall_mt)
        return;
    type = jl_unwrap_unionall(type);
    assert(jl_is_datatype(type));
    size_t na = jl_nparams(type);
    if (jl_va_tuple_kind((jl_datatype_t*)type) == JL_VARARG_UNBOUND)
        na--;
    // update occurs inside mt->writelock
    if (na > jl_atomic_load_relaxed(&mt->max_args))
        jl_atomic_store_relaxed(&mt->max_args, na);
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

// call external callbacks registered with this method_instance
static void invalidate_external(jl_method_instance_t *mi, size_t max_world) {
    jl_array_t *callbacks = mi->callbacks;
    if (callbacks) {
        // AbstractInterpreter allows for MethodInstances to be present in non-local caches
        // inform those caches about the invalidation.
        JL_TRY {
            size_t i, l = jl_array_len(callbacks);
            jl_value_t **args;
            JL_GC_PUSHARGS(args, 3);
            // these arguments are constant per call
            args[1] = (jl_value_t*)mi;
            args[2] = jl_box_uint32(max_world);

            jl_task_t *ct = jl_current_task;
            size_t last_age = ct->world_age;
            ct->world_age = jl_get_world_counter();

            jl_value_t **cbs = (jl_value_t**)jl_array_ptr_data(callbacks);
            for (i = 0; i < l; i++) {
                args[0] = cbs[i];
                jl_apply(args, 3);
            }
            ct->world_age = last_age;
            JL_GC_POP();
        }
        JL_CATCH {
            jl_printf((JL_STREAM*)STDERR_FILENO, "error in invalidation callback: ");
            jl_static_show((JL_STREAM*)STDERR_FILENO, jl_current_exception());
            jl_printf((JL_STREAM*)STDERR_FILENO, "\n");
            jlbacktrace(); // written to STDERR_FILENO
        }
    }
}

static void do_nothing_with_codeinst(jl_code_instance_t *ci) {}

// recursively invalidate cached methods that had an edge to a replaced method
static void invalidate_method_instance(void (*f)(jl_code_instance_t*), jl_method_instance_t *replaced, size_t max_world, int depth)
{
    if (_jl_debug_method_invalidation) {
        jl_value_t *boxeddepth = NULL;
        JL_GC_PUSH1(&boxeddepth);
        jl_array_ptr_1d_push(_jl_debug_method_invalidation, (jl_value_t*)replaced);
        boxeddepth = jl_box_int32(depth);
        jl_array_ptr_1d_push(_jl_debug_method_invalidation, boxeddepth);
        JL_GC_POP();
    }
    //jl_static_show(JL_STDERR, (jl_value_t*)replaced);
    if (!jl_is_method(replaced->def.method))
        return; // shouldn't happen, but better to be safe
    JL_LOCK(&replaced->def.method->writelock);
    jl_code_instance_t *codeinst = jl_atomic_load_relaxed(&replaced->cache);
    while (codeinst) {
        if (codeinst->max_world == ~(size_t)0) {
            assert(codeinst->min_world - 1 <= max_world && "attempting to set illogical world constraints (probable race condition)");
            codeinst->max_world = max_world;
        }
        assert(codeinst->max_world <= max_world);
        JL_GC_PUSH1(&codeinst);
        (*f)(codeinst);
        JL_GC_POP();
        codeinst = jl_atomic_load_relaxed(&codeinst->next);
    }
    // recurse to all backedges to update their valid range also
    jl_array_t *backedges = replaced->backedges;
    if (backedges) {
        JL_GC_PUSH1(&backedges);
        replaced->backedges = NULL;
        size_t i = 0, l = jl_array_len(backedges);
        jl_method_instance_t *replaced;
        while (i < l) {
            i = get_next_edge(backedges, i, NULL, &replaced);
            invalidate_method_instance(f, replaced, max_world, depth + 1);
        }
        JL_GC_POP();
    }
    JL_UNLOCK(&replaced->def.method->writelock);
}

// invalidate cached methods that overlap this definition
static void invalidate_backedges(void (*f)(jl_code_instance_t*), jl_method_instance_t *replaced_mi, size_t max_world, const char *why)
{
    JL_LOCK(&replaced_mi->def.method->writelock);
    jl_array_t *backedges = replaced_mi->backedges;
    //jl_static_show(JL_STDERR, (jl_value_t*)replaced_mi);
    if (backedges) {
        // invalidate callers (if any)
        replaced_mi->backedges = NULL;
        JL_GC_PUSH1(&backedges);
        size_t i = 0, l = jl_array_len(backedges);
        jl_method_instance_t *replaced;
        while (i < l) {
            i = get_next_edge(backedges, i, NULL, &replaced);
            invalidate_method_instance(f, replaced, max_world, 1);
        }
        JL_GC_POP();
    }
    JL_UNLOCK(&replaced_mi->def.method->writelock);
    if (why && _jl_debug_method_invalidation) {
        jl_array_ptr_1d_push(_jl_debug_method_invalidation, (jl_value_t*)replaced_mi);
        jl_value_t *loctag = jl_cstr_to_string(why);
        JL_GC_PUSH1(&loctag);
        jl_array_ptr_1d_push(_jl_debug_method_invalidation, loctag);
        JL_GC_POP();
    }
}

// add a backedge from callee to caller
JL_DLLEXPORT void jl_method_instance_add_backedge(jl_method_instance_t *callee, jl_value_t *invokesig, jl_method_instance_t *caller)
{
    JL_LOCK(&callee->def.method->writelock);
    if (invokesig == jl_nothing)
        invokesig = NULL;      // julia uses `nothing` but C uses NULL (#undef)
    if (!callee->backedges) {
        // lazy-init the backedges array
        callee->backedges = jl_alloc_vec_any(0);
        jl_gc_wb(callee, callee->backedges);
        push_edge(callee->backedges, invokesig, caller);
    }
    else {
        size_t i = 0, l = jl_array_len(callee->backedges);
        int found = 0;
        jl_value_t *invokeTypes;
        jl_method_instance_t *mi;
        while (i < l) {
            i = get_next_edge(callee->backedges, i, &invokeTypes, &mi);
            // TODO: it would be better to canonicalize (how?) the Tuple-type so
            // that we don't have to call `jl_egal`
            if (mi == caller && ((invokesig == NULL && invokeTypes == NULL) ||
                                 (invokesig && invokeTypes && jl_egal(invokesig, invokeTypes)))) {
                found = 1;
                break;
            }
        }
        if (!found) {
            push_edge(callee->backedges, invokesig, caller);
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
                jl_array_ptr_1d_push(_jl_debug_method_invalidation, (jl_value_t*)mi);
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

#ifndef __clang_gcanalyzer__ /* in general, jl_typemap_visitor could be a safepoint, but not for typemap_search */
static jl_typemap_entry_t *do_typemap_search(jl_methtable_t *mt JL_PROPAGATES_ROOT, jl_method_t *method) JL_NOTSAFEPOINT {
    jl_value_t *closure = (jl_value_t*)(method);
    if (jl_typemap_visitor(jl_atomic_load_relaxed(&mt->defs), typemap_search, &closure))
        jl_error("method not in method table");
    return (jl_typemap_entry_t *)closure;
}
#endif

static void jl_method_table_invalidate(jl_methtable_t *mt, jl_typemap_entry_t *methodentry, jl_method_t *method, size_t max_world)
{
    assert(!method->is_for_opaque_closure);
    method->deleted_world = methodentry->max_world = max_world;
    // drop this method from mt->cache
    struct invalidate_mt_env mt_cache_env;
    mt_cache_env.max_world = max_world;
    mt_cache_env.newentry = methodentry;
    mt_cache_env.shadowed = NULL;
    mt_cache_env.invalidated = 0;
    jl_typemap_visitor(jl_atomic_load_relaxed(&mt->cache), disable_mt_cache, (void*)&mt_cache_env);
    jl_array_t *leafcache = jl_atomic_load_relaxed(&mt->leafcache);
    size_t i, l = jl_array_len(leafcache);
    for (i = 1; i < l; i += 2) {
        jl_typemap_entry_t *oldentry = (jl_typemap_entry_t*)jl_array_ptr_ref(leafcache, i);
        if (oldentry) {
            while ((jl_value_t*)oldentry != jl_nothing) {
                if (oldentry->max_world == ~(size_t)0)
                    oldentry->max_world = mt_cache_env.max_world;
                oldentry = jl_atomic_load_relaxed(&oldentry->next);
            }
        }
    }
    // Invalidate the backedges
    int invalidated = 0;
    jl_svec_t *specializations = jl_atomic_load_relaxed(&methodentry->func.method->specializations);
    l = jl_svec_len(specializations);
    for (i = 0; i < l; i++) {
        jl_method_instance_t *mi = (jl_method_instance_t*)jl_svecref(specializations, i);
        if ((jl_value_t*)mi != jl_nothing) {
            invalidated = 1;
            invalidate_external(mi, methodentry->max_world);
            invalidate_backedges(&do_nothing_with_codeinst, mi, methodentry->max_world, "jl_method_table_disable");
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
    size_t world = jl_atomic_fetch_add(&jl_world_counter, 1);
    jl_method_table_invalidate(mt, methodentry, method, world);
    JL_UNLOCK(&mt->writelock);
}

static int jl_type_intersection2(jl_value_t *t1, jl_value_t *t2, jl_value_t **isect, jl_value_t **isect2)
{
    *isect2 = NULL;
    int is_subty = 0;
    *isect = jl_type_intersection_env_s(t1, t2, NULL, &is_subty);
    if (*isect == jl_bottom_type)
        return 0;
    if (is_subty)
        return 1;
    // TODO: sometimes type intersection returns types with free variables
    if (jl_has_free_typevars(t1) || jl_has_free_typevars(t2))
        return 1;
    // determine if type-intersection can be convinced to give a better, non-bad answer
    // if the intersection was imprecise, see if we can do better by switching the types
    *isect2 = jl_type_intersection(t2, t1);
    if (*isect2 == jl_bottom_type) {
        *isect = jl_bottom_type;
        *isect2 = NULL;
        return 0;
    }
    if (jl_types_egal(*isect2, *isect)) {
        *isect2 = NULL;
    }
    return 1;
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
        method->primary_world = jl_atomic_fetch_add(&jl_world_counter, 1) + 1;
    size_t max_world = method->primary_world - 1;
    jl_value_t *loctag = NULL;  // debug info for invalidation
    jl_value_t *isect = NULL;
    jl_value_t *isect2 = NULL;
    jl_value_t *isect3 = NULL;
    jl_typemap_entry_t *newentry = NULL;
    JL_GC_PUSH7(&oldvalue, &oldmi, &newentry, &loctag, &isect, &isect2, &isect3);
    JL_LOCK(&mt->writelock);
    // first find if we have an existing entry to delete
    struct jl_typemap_assoc search = {(jl_value_t*)type, method->primary_world, NULL, 0, ~(size_t)0};
    jl_typemap_entry_t *oldentry = jl_typemap_assoc_by_type(jl_atomic_load_relaxed(&mt->defs), &search, /*offs*/0, /*subtype*/0);
    // then add our new entry
    newentry = jl_typemap_alloc((jl_tupletype_t*)type, simpletype, jl_emptysvec,
            (jl_value_t*)method, method->primary_world, method->deleted_world);
    jl_typemap_insert(&mt->defs, (jl_value_t*)mt, newentry, 0);
    if (oldentry) {
        jl_method_t *m = oldentry->func.method;
        method_overwrite(newentry, m);
        jl_method_table_invalidate(mt, oldentry, m, max_world);
    }
    else {
        oldvalue = get_intersect_matches(jl_atomic_load_relaxed(&mt->defs), newentry);

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
                int missing = 0;
                if (jl_type_intersection2(backedgetyp, (jl_value_t*)type, &isect, &isect2)) {
                    // See if the intersection was actually already fully
                    // covered, but that the new method is ambiguous.
                    //  -> no previous method: now there is one, need to update the missing edge
                    //  -> one+ previously matching method(s):
                    //    -> more specific then all of them: need to update the missing edge
                    //      -> some may have been ambiguous: now there is a replacement
                    //      -> some may have been called: now there is a replacement (also will be detected in the loop later)
                    //    -> less specific or ambiguous with any one of them: can ignore the missing edge (not missing)
                    //      -> some may have been ambiguous: still are
                    //      -> some may have been called: they may be partly replaced (will be detected in the loop later)
                    missing = 1;
                    size_t j;
                    for (j = 0; j < n; j++) {
                        jl_method_t *m = d[j];
                        if (jl_subtype(isect, m->sig) || (isect2 && jl_subtype(isect2, m->sig))) {
                            // We now know that there actually was a previous
                            // method for this part of the type intersection.
                            if (!jl_type_morespecific(type, m->sig)) {
                                missing = 0;
                                break;
                            }
                        }
                    }
                }
                if (missing) {
                    jl_method_instance_t *backedge = (jl_method_instance_t*)backedges[i];
                    invalidate_external(backedge, max_world);
                    invalidate_method_instance(&do_nothing_with_codeinst, backedge, max_world, 0);
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
                jl_svec_t *specializations = jl_atomic_load_relaxed(&m->specializations);
                _Atomic(jl_method_instance_t*) *data = (_Atomic(jl_method_instance_t*)*)jl_svec_data(specializations);
                size_t i, l = jl_svec_len(specializations);
                enum morespec_options ambig = morespec_unknown;
                for (i = 0; i < l; i++) {
                    jl_method_instance_t *mi = jl_atomic_load_relaxed(&data[i]);
                    if ((jl_value_t*)mi == jl_nothing)
                        continue;
                    isect3 = jl_type_intersection(m->sig, (jl_value_t*)mi->specTypes);
                    if (jl_type_intersection2(type, isect3, &isect, &isect2)) {
                        if (morespec[j] == (char)morespec_unknown)
                            morespec[j] = (char)(jl_type_morespecific(m->sig, type) ? morespec_is : morespec_isnot);
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
                                if (m == m2 || !(jl_subtype(isect, m2->sig) || (isect && jl_subtype(isect, m2->sig))))
                                    continue;
                                if (morespec[k] == (char)morespec_unknown)
                                    morespec[k] = (char)(jl_type_morespecific(m2->sig, type) ? morespec_is : morespec_isnot);
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
                        // Before deciding whether to invalidate `mi`, check each backedge for `invoke`s
                        if (mi->backedges) {
                            jl_array_t *backedges = mi->backedges;
                            size_t ib = 0, insb = 0, nb = jl_array_len(backedges);
                            jl_value_t *invokeTypes;
                            jl_method_instance_t *caller;
                            while (ib < nb) {
                                ib = get_next_edge(backedges, ib, &invokeTypes, &caller);
                                if (!invokeTypes) {
                                    // ordinary dispatch, invalidate
                                    invalidate_method_instance(&do_nothing_with_codeinst, caller, max_world, 1);
                                    invalidated = 1;
                                } else {
                                    // invoke-dispatch, check invokeTypes for validity
                                    struct jl_typemap_assoc search = {invokeTypes, method->primary_world, NULL, 0, ~(size_t)0};
                                    oldentry = jl_typemap_assoc_by_type(jl_atomic_load_relaxed(&mt->defs), &search, /*offs*/0, /*subtype*/0);
                                    if (oldentry && oldentry->func.method == mi->def.method) {
                                        // We can safely keep this method
                                        jl_array_ptr_set(backedges, insb++, invokeTypes);
                                        jl_array_ptr_set(backedges, insb++, caller);
                                    } else {
                                        invalidate_method_instance(&do_nothing_with_codeinst, caller, max_world, 1);
                                        invalidated = 1;
                                    }
                                }
                            }
                            jl_array_del_end(backedges, nb - insb);
                        }
                        if (!mi->backedges || jl_array_len(mi->backedges) == 0) {
                            jl_array_ptr_1d_push(oldmi, (jl_value_t*)mi);
                            invalidate_external(mi, max_world);
                            if (mi->backedges) {
                                invalidated = 1;
                                invalidate_backedges(&do_nothing_with_codeinst, mi, max_world, "jl_method_table_insert");
                            }
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

                jl_typemap_visitor(jl_atomic_load_relaxed(&mt->cache), invalidate_mt_cache, (void*)&mt_cache_env);
                jl_array_t *leafcache = jl_atomic_load_relaxed(&mt->leafcache);
                size_t i, l = jl_array_len(leafcache);
                for (i = 1; i < l; i += 2) {
                    jl_value_t *entry = jl_array_ptr_ref(leafcache, i);
                    if (entry) {
                        while (entry != jl_nothing) {
                            invalidate_mt_cache((jl_typemap_entry_t*)entry, (void*)&mt_cache_env);
                            entry = (jl_value_t*)jl_atomic_load_relaxed(&((jl_typemap_entry_t*)entry)->next);
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
        jl_ptls_t ptls = jl_current_task->ptls;
        ptls->bt_size = rec_backtrace(ptls->bt_data, JL_MAX_BT_SIZE, 0);
        jl_critical_error(0, 0, NULL, jl_current_task);
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

static jl_tupletype_t *lookup_arg_type_tuple(jl_value_t *arg1 JL_PROPAGATES_ROOT, jl_value_t **args, size_t nargs)
{
    return jl_lookup_arg_tuple_type(arg1, args, nargs, 1);
}

jl_method_instance_t *jl_method_lookup(jl_value_t **args, size_t nargs, size_t world)
{
    assert(nargs > 0 && "expected caller to handle this case");
    jl_methtable_t *mt = jl_gf_mtable(args[0]);
    jl_typemap_t *cache = jl_atomic_load_relaxed(&mt->cache); // XXX: gc root for this?
    jl_typemap_entry_t *entry = jl_typemap_assoc_exact(cache, args[0], &args[1], nargs, jl_cachearg_offset(mt), world);
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
    JL_UNLOCK(&mt->writelock);
    JL_GC_POP();
    return sf;
}

// return a Vector{Any} of svecs, each describing a method match:
// Any[svec(tt, spvals, m, full), ...]
// tt is the intersection of the type argument and the method signature,
// spvals is any matched static parameter values, m is the Method,
// full is a boolean indicating if that method fully covers the input
//
// lim is the max # of methods to return. if there are more, returns jl_false.
// Negative values stand for no limit.
// Unless lim == -1, remove matches that are unambiguously covered by earlier ones
JL_DLLEXPORT jl_value_t *jl_matching_methods(jl_tupletype_t *types, jl_value_t *mt, int lim, int include_ambiguous,
                                             size_t world, size_t *min_valid, size_t *max_valid, int *ambig)
{
    JL_TIMING(METHOD_MATCH);
    if (ambig != NULL)
        *ambig = 0;
    jl_value_t *unw = jl_unwrap_unionall((jl_value_t*)types);
    if (!jl_is_tuple_type(unw))
        return (jl_value_t*)jl_an_empty_vec_any;
    if (unw == (jl_value_t*)jl_emptytuple_type || jl_tparam0(unw) == jl_bottom_type)
        return (jl_value_t*)jl_an_empty_vec_any;
    if (mt == jl_nothing)
        mt = (jl_value_t*)jl_method_table_for(unw);
    if (mt == jl_nothing)
        mt = NULL;
    return ml_matches((jl_methtable_t*)mt, types, lim, include_ambiguous, 1, world, 1, min_valid, max_valid, ambig);
}

jl_method_instance_t *jl_get_unspecialized_from_mi(jl_method_instance_t *method JL_PROPAGATES_ROOT)
{
    jl_method_t *def = method->def.method;
    jl_method_instance_t *mi = jl_get_unspecialized(def);
    if (mi == NULL) {
        return method;
    }
    return mi;
}

jl_method_instance_t *jl_get_unspecialized(jl_method_t *def JL_PROPAGATES_ROOT)
{
    // one unspecialized version of a function can be shared among all cached specializations
    if (!jl_is_method(def) || def->source == NULL) {
        // generated functions might instead randomly just never get inferred, sorry
        return NULL;
    }
    jl_method_instance_t *unspec = jl_atomic_load_relaxed(&def->unspecialized);
    if (unspec == NULL) {
        JL_LOCK(&def->writelock);
        unspec = jl_atomic_load_relaxed(&def->unspecialized);
        if (unspec == NULL) {
            unspec = jl_get_specialized(def, def->sig, jl_emptysvec);
            jl_atomic_store_release(&def->unspecialized, unspec);
            jl_gc_wb(def, unspec);
        }
        JL_UNLOCK(&def->writelock);
    }
    return unspec;
}


jl_code_instance_t *jl_method_compiled(jl_method_instance_t *mi, size_t world)
{
    jl_code_instance_t *codeinst = jl_atomic_load_relaxed(&mi->cache);
    while (codeinst) {
        if (codeinst->min_world <= world && world <= codeinst->max_world) {
            if (jl_atomic_load_relaxed(&codeinst->invoke) != NULL)
                return codeinst;
        }
        codeinst = jl_atomic_load_relaxed(&codeinst->next);
    }
    return NULL;
}

jl_mutex_t precomp_statement_out_lock;

static void record_precompile_statement(jl_method_instance_t *mi)
{
    static ios_t f_precompile;
    static JL_STREAM* s_precompile = NULL;
    jl_method_t *def = mi->def.method;
    if (jl_options.trace_compile == NULL)
        return;
    if (!jl_is_method(def))
        return;

    JL_LOCK(&precomp_statement_out_lock);
    if (s_precompile == NULL) {
        const char *t = jl_options.trace_compile;
        if (!strncmp(t, "stderr", 6)) {
            s_precompile = JL_STDERR;
        }
        else {
            if (ios_file(&f_precompile, t, 1, 1, 1, 1) == NULL)
                jl_errorf("cannot open precompile statement file \"%s\" for writing", t);
            s_precompile = (JL_STREAM*) &f_precompile;
        }
    }
    if (!jl_has_free_typevars(mi->specTypes)) {
        jl_printf(s_precompile, "precompile(");
        jl_static_show(s_precompile, mi->specTypes);
        jl_printf(s_precompile, ")\n");
        if (s_precompile != JL_STDERR)
            ios_flush(&f_precompile);
    }
    JL_UNLOCK(&precomp_statement_out_lock);
}

jl_method_instance_t *jl_normalize_to_compilable_mi(jl_method_instance_t *mi JL_PROPAGATES_ROOT);

jl_code_instance_t *jl_compile_method_internal(jl_method_instance_t *mi, size_t world)
{
    // quick check if we already have a compiled result
    jl_code_instance_t *codeinst = jl_method_compiled(mi, world);
    if (codeinst)
        return codeinst;

    // if mi has a better (wider) signature preferred for compilation use that
    // instead and just copy it here for caching
    jl_method_instance_t *mi2 = jl_normalize_to_compilable_mi(mi);
    if (mi2 != mi) {
        jl_code_instance_t *codeinst2 = jl_compile_method_internal(mi2, world);
        jl_code_instance_t *codeinst = jl_get_method_inferred(
                mi, codeinst2->rettype,
                codeinst2->min_world, codeinst2->max_world);
        if (jl_atomic_load_relaxed(&codeinst->invoke) == NULL) {
            codeinst->rettype_const = codeinst2->rettype_const;
            uint8_t specsigflags = jl_atomic_load_acquire(&codeinst2->specsigflags);
            jl_callptr_t invoke = jl_atomic_load_acquire(&codeinst2->invoke);
            void *fptr = jl_atomic_load_relaxed(&codeinst2->specptr.fptr);
            if (fptr != NULL) {
                while (!(specsigflags & 0b10)) {
                    jl_cpu_pause();
                    specsigflags = jl_atomic_load_acquire(&codeinst2->specsigflags);
                }
                invoke = jl_atomic_load_relaxed(&codeinst2->invoke);
                void *prev_fptr = NULL;
                // see jitlayers.cpp for the ordering restrictions here
                if (jl_atomic_cmpswap_acqrel(&codeinst->specptr.fptr, &prev_fptr, fptr)) {
                    jl_atomic_store_relaxed(&codeinst->specsigflags, specsigflags & 0b1);
                    jl_atomic_store_release(&codeinst->invoke, invoke);
                    jl_atomic_store_release(&codeinst->specsigflags, specsigflags);
                } else {
                    // someone else already compiled it
                    while (!(jl_atomic_load_acquire(&codeinst->specsigflags) & 0b10)) {
                        jl_cpu_pause();
                    }
                    // codeinst is now set up fully, safe to return
                }
            } else {
                jl_callptr_t prev = NULL;
                jl_atomic_cmpswap_acqrel(&codeinst->invoke, &prev, invoke);
            }
        }
        // don't call record_precompile_statement here, since we already compiled it as mi2 which is better
        return codeinst;
    }

    int compile_option = jl_options.compile_enabled;
    jl_method_t *def = mi->def.method;
    // disabling compilation per-module can override global setting
    if (jl_is_method(def)) {
        int mod_setting = jl_get_module_compile(((jl_method_t*)def)->module);
        if (mod_setting == JL_OPTIONS_COMPILE_OFF ||
            mod_setting == JL_OPTIONS_COMPILE_MIN)
            compile_option = ((jl_method_t*)def)->module->compile;
    }

    // if compilation is disabled or source is unavailable, try calling unspecialized version
    if (compile_option == JL_OPTIONS_COMPILE_OFF ||
        compile_option == JL_OPTIONS_COMPILE_MIN ||
        def->source == jl_nothing) {
        // copy fptr from the template method definition
        if (jl_is_method(def)) {
            jl_method_instance_t *unspecmi = jl_atomic_load_relaxed(&def->unspecialized);
            if (unspecmi) {
                jl_code_instance_t *unspec = jl_atomic_load_relaxed(&unspecmi->cache);
                jl_callptr_t unspec_invoke = NULL;
                if (unspec && (unspec_invoke = jl_atomic_load_acquire(&unspec->invoke))) {
                    jl_code_instance_t *codeinst = jl_new_codeinst(mi,
                        (jl_value_t*)jl_any_type, NULL, NULL,
                        0, 1, ~(size_t)0, 0, 0, jl_nothing, 0);
                    void *unspec_fptr = jl_atomic_load_relaxed(&unspec->specptr.fptr);
                    if (unspec_fptr) {
                        // wait until invoke and specsigflags are properly set
                        while (!(jl_atomic_load_acquire(&unspec->specsigflags) & 0b10)) {
                            jl_cpu_pause();
                        }
                        unspec_invoke = jl_atomic_load_relaxed(&unspec->invoke);
                    }
                    jl_atomic_store_release(&codeinst->specptr.fptr, unspec_fptr);
                    codeinst->rettype_const = unspec->rettype_const;
                    jl_atomic_store_release(&codeinst->invoke, unspec_invoke);
                    jl_mi_cache_insert(mi, codeinst);
                    record_precompile_statement(mi);
                    return codeinst;
                }
            }
        }
    }

    // if that didn't work and compilation is off, try running in the interpreter
    if (compile_option == JL_OPTIONS_COMPILE_OFF ||
        compile_option == JL_OPTIONS_COMPILE_MIN) {
        jl_code_info_t *src = jl_code_for_interpreter(mi, world);
        if (!jl_code_requires_compiler(src, 0)) {
            jl_code_instance_t *codeinst = jl_new_codeinst(mi,
                (jl_value_t*)jl_any_type, NULL, NULL,
                0, 1, ~(size_t)0, 0, 0, jl_nothing, 0);
            jl_atomic_store_release(&codeinst->invoke, jl_fptr_interpret_call);
            jl_mi_cache_insert(mi, codeinst);
            record_precompile_statement(mi);
            return codeinst;
        }
        if (compile_option == JL_OPTIONS_COMPILE_OFF) {
            jl_printf(JL_STDERR, "code missing for ");
            jl_static_show(JL_STDERR, (jl_value_t*)mi);
            jl_printf(JL_STDERR, " : sysimg may not have been built with --compile=all\n");
        }
    }

    codeinst = jl_generate_fptr(mi, world);
    if (!codeinst) {
        jl_method_instance_t *unspec = jl_get_unspecialized_from_mi(mi);
        jl_code_instance_t *ucache = jl_get_method_inferred(unspec, (jl_value_t*)jl_any_type, 1, ~(size_t)0);
        // ask codegen to make the fptr for unspec
        jl_callptr_t ucache_invoke = jl_atomic_load_acquire(&ucache->invoke);
        if (ucache_invoke == NULL) {
            if (def->source == jl_nothing && (jl_atomic_load_relaxed(&ucache->def->uninferred) == jl_nothing ||
                                              jl_atomic_load_relaxed(&ucache->def->uninferred) == NULL)) {
                jl_printf(JL_STDERR, "source not available for ");
                jl_static_show(JL_STDERR, (jl_value_t*)mi);
                jl_printf(JL_STDERR, "\n");
                jl_error("source missing for method that needs to be compiled");
            }
            jl_generate_fptr_for_unspecialized(ucache);
            ucache_invoke = jl_atomic_load_acquire(&ucache->invoke);
        }
        assert(ucache_invoke != NULL);
        if (ucache_invoke != jl_fptr_sparam &&
            ucache_invoke != jl_fptr_interpret_call) {
            // only these care about the exact specTypes, otherwise we can use it directly
            return ucache;
        }
        codeinst = jl_new_codeinst(mi, (jl_value_t*)jl_any_type, NULL, NULL,
            0, 1, ~(size_t)0, 0, 0, jl_nothing, 0);
        void *unspec_fptr = jl_atomic_load_relaxed(&ucache->specptr.fptr);
        if (unspec_fptr) {
            // wait until invoke and specsigflags are properly set
            while (!(jl_atomic_load_acquire(&ucache->specsigflags) & 0b10)) {
                jl_cpu_pause();
            }
            ucache_invoke = jl_atomic_load_relaxed(&ucache->invoke);
        }
        // unspec is always not specsig, but might use specptr
        jl_atomic_store_relaxed(&codeinst->specsigflags, jl_atomic_load_relaxed(&ucache->specsigflags) & 0b10);
        jl_atomic_store_relaxed(&codeinst->specptr.fptr, unspec_fptr);
        codeinst->rettype_const = ucache->rettype_const;
        jl_atomic_store_release(&codeinst->invoke, ucache_invoke);
        jl_mi_cache_insert(mi, codeinst);
    }
    else {
        record_precompile_statement(mi);
    }
    jl_atomic_store_relaxed(&codeinst->precompile, 1);
    return codeinst;
}

jl_value_t *jl_fptr_const_return(jl_value_t *f, jl_value_t **args, uint32_t nargs, jl_code_instance_t *m)
{
    return m->rettype_const;
}

jl_value_t *jl_fptr_args(jl_value_t *f, jl_value_t **args, uint32_t nargs, jl_code_instance_t *m)
{
    jl_fptr_args_t invoke = jl_atomic_load_relaxed(&m->specptr.fptr1);
    assert(invoke && "Forgot to set specptr for jl_fptr_args!");
    return invoke(f, args, nargs);
}

jl_value_t *jl_fptr_sparam(jl_value_t *f, jl_value_t **args, uint32_t nargs, jl_code_instance_t *m)
{
    jl_svec_t *sparams = m->def->sparam_vals;
    assert(sparams != jl_emptysvec);
    jl_fptr_sparam_t invoke = jl_atomic_load_relaxed(&m->specptr.fptr3);
    assert(invoke && "Forgot to set specptr for jl_fptr_sparam!");
    return invoke(f, args, nargs, sparams);
}

JL_DLLEXPORT jl_callptr_t jl_fptr_args_addr = &jl_fptr_args;

JL_DLLEXPORT jl_callptr_t jl_fptr_const_return_addr = &jl_fptr_const_return;

JL_DLLEXPORT jl_callptr_t jl_fptr_sparam_addr = &jl_fptr_sparam;

// Return the index of the invoke api, if known
JL_DLLEXPORT int32_t jl_invoke_api(jl_code_instance_t *codeinst)
{
    jl_callptr_t f = jl_atomic_load_relaxed(&codeinst->invoke);
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

JL_DLLEXPORT jl_value_t *jl_normalize_to_compilable_sig(jl_methtable_t *mt, jl_tupletype_t *ti, jl_svec_t *env, jl_method_t *m)
{
    jl_tupletype_t *tt = NULL;
    jl_svec_t *newparams = NULL;
    JL_GC_PUSH2(&tt, &newparams);
    jl_methtable_t *kwmt = mt == jl_kwcall_mt ? jl_kwmethod_table_for(m->sig) : mt;
    intptr_t nspec = (kwmt == NULL || kwmt == jl_type_type_mt || kwmt == jl_nonfunction_mt || kwmt == jl_kwcall_mt ? m->nargs + 1 : jl_atomic_load_relaxed(&kwmt->max_args) + 2 + 2 * (mt == jl_kwcall_mt));
    jl_compilation_sig(ti, env, m, nspec, &newparams);
    int is_compileable = ((jl_datatype_t*)ti)->isdispatchtuple;
    if (newparams) {
        tt = jl_apply_tuple_type(newparams);
        if (!is_compileable) {
            // compute new env, if used below
            jl_value_t *ti = jl_type_intersection_env((jl_value_t*)tt, (jl_value_t*)m->sig, &newparams);
            assert(ti != jl_bottom_type); (void)ti;
            env = newparams;
        }
    }
    else {
        tt = ti;
    }
    if (!is_compileable)
        is_compileable = jl_isa_compileable_sig(tt, env, m);
    JL_GC_POP();
    return is_compileable ? (jl_value_t*)tt : jl_nothing;
}

jl_method_instance_t *jl_normalize_to_compilable_mi(jl_method_instance_t *mi JL_PROPAGATES_ROOT)
{
    jl_method_t *def = mi->def.method;
    if (!jl_is_method(def) || !jl_is_datatype(mi->specTypes))
        return mi;
    jl_methtable_t *mt = jl_method_get_table(def);
    if ((jl_value_t*)mt == jl_nothing)
        return mi;
    jl_value_t *compilationsig = jl_normalize_to_compilable_sig(mt, (jl_datatype_t*)mi->specTypes, mi->sparam_vals, def);
    if (compilationsig == jl_nothing || jl_egal(compilationsig, mi->specTypes))
        return mi;
    jl_svec_t *env = NULL;
    JL_GC_PUSH2(&compilationsig, &env);
    jl_value_t *ti = jl_type_intersection_env((jl_value_t*)compilationsig, (jl_value_t*)def->sig, &env);
    assert(ti != jl_bottom_type); (void)ti;
    mi = jl_specializations_get_linfo(def, (jl_value_t*)compilationsig, env);
    JL_GC_POP();
    return mi;
}

// return a MethodInstance for a compileable method_match
jl_method_instance_t *jl_method_match_to_mi(jl_method_match_t *match, size_t world, size_t min_valid, size_t max_valid, int mt_cache)
{
    jl_method_t *m = match->method;
    jl_svec_t *env = match->sparams;
    jl_tupletype_t *ti = match->spec_types;
    jl_method_instance_t *mi = NULL;
    if (jl_is_datatype(ti)) {
        jl_methtable_t *mt = jl_method_get_table(m);
        if ((jl_value_t*)mt != jl_nothing) {
            // get the specialization, possibly also caching it
            if (mt_cache && ((jl_datatype_t*)ti)->isdispatchtuple) {
                // Since we also use this presence in the cache
                // to trigger compilation when producing `.ji` files,
                // inject it there now if we think it will be
                // used via dispatch later (e.g. because it was hinted via a call to `precompile`)
                JL_LOCK(&mt->writelock);
                mi = cache_method(mt, &mt->cache, (jl_value_t*)mt, ti, m, world, min_valid, max_valid, env);
                JL_UNLOCK(&mt->writelock);
            }
            else {
                jl_value_t *tt = jl_normalize_to_compilable_sig(mt, ti, env, m);
                if (tt != jl_nothing) {
                    JL_GC_PUSH2(&tt, &env);
                    if (!jl_egal(tt, (jl_value_t*)ti)) {
                        jl_value_t *ti = jl_type_intersection_env((jl_value_t*)tt, (jl_value_t*)m->sig, &env);
                        assert(ti != jl_bottom_type); (void)ti;
                    }
                    mi = jl_specializations_get_linfo(m, (jl_value_t*)tt, env);
                    JL_GC_POP();
                }
            }
        }
    }
    return mi;
}

// compile-time method lookup
jl_method_instance_t *jl_get_specialization1(jl_tupletype_t *types JL_PROPAGATES_ROOT, size_t world, size_t *min_valid, size_t *max_valid, int mt_cache)
{
    if (jl_has_free_typevars((jl_value_t*)types))
        return NULL; // don't poison the cache due to a malformed query
    if (!jl_has_concrete_subtype((jl_value_t*)types))
        return NULL;

    // find if exactly 1 method matches (issue #7302)
    size_t min_valid2 = 1;
    size_t max_valid2 = ~(size_t)0;
    int ambig = 0;
    jl_value_t *matches = jl_matching_methods(types, jl_nothing, 1, 1, world, &min_valid2, &max_valid2, &ambig);
    if (*min_valid < min_valid2)
        *min_valid = min_valid2;
    if (*max_valid > max_valid2)
        *max_valid = max_valid2;
    if (matches == jl_nothing || jl_array_len(matches) != 1 || ambig)
        return NULL;
    JL_GC_PUSH1(&matches);
    jl_method_match_t *match = (jl_method_match_t*)jl_array_ptr_ref(matches, 0);
    jl_method_instance_t *mi = jl_method_match_to_mi(match, world, min_valid2, max_valid2, mt_cache);
    JL_GC_POP();
    return mi;
}

// Get a MethodInstance for a precompile() call. This uses a special kind of lookup that
// tries to find a method for which the requested signature is compileable.
static jl_method_instance_t *jl_get_compile_hint_specialization(jl_tupletype_t *types JL_PROPAGATES_ROOT, size_t world, size_t *min_valid, size_t *max_valid, int mt_cache)
{
    if (jl_has_free_typevars((jl_value_t*)types))
        return NULL; // don't poison the cache due to a malformed query
    if (!jl_has_concrete_subtype((jl_value_t*)types))
        return NULL;

    size_t min_valid2 = 1;
    size_t max_valid2 = ~(size_t)0;
    int ambig = 0;
    jl_value_t *matches = jl_matching_methods(types, jl_nothing, -1, 0, world, &min_valid2, &max_valid2, &ambig);
    if (*min_valid < min_valid2)
        *min_valid = min_valid2;
    if (*max_valid > max_valid2)
        *max_valid = max_valid2;
    size_t i, n = jl_array_len(matches);
    if (n == 0)
        return NULL;
    JL_GC_PUSH1(&matches);
    jl_method_match_t *match = NULL;
    if (n == 1) {
        match = (jl_method_match_t*)jl_array_ptr_ref(matches, 0);
    }
    else if (jl_is_datatype(types)) {
        // first, select methods for which `types` is compileable
        size_t count = 0;
        for (i = 0; i < n; i++) {
            jl_method_match_t *match1 = (jl_method_match_t*)jl_array_ptr_ref(matches, i);
            if (jl_isa_compileable_sig(types, match1->sparams, match1->method))
                jl_array_ptr_set(matches, count++, (jl_value_t*)match1);
        }
        jl_array_del_end((jl_array_t*)matches, n - count);
        n = count;
        // now remove methods that are more specific than others in the list.
        // this is because the intent of precompiling e.g. f(::DataType) is to
        // compile that exact method if it exists, and not lots of f(::Type{X}) methods
        int exclude;
        count = 0;
        for (i = 0; i < n; i++) {
            jl_method_match_t *match1 = (jl_method_match_t*)jl_array_ptr_ref(matches, i);
            exclude = 0;
            for (size_t j = n-1; j > i; j--) {  // more general methods maybe more likely to be at end
                jl_method_match_t *match2 = (jl_method_match_t*)jl_array_ptr_ref(matches, j);
                if (jl_type_morespecific(match1->method->sig, match2->method->sig)) {
                    exclude = 1;
                    break;
                }
            }
            if (!exclude)
                jl_array_ptr_set(matches, count++, (jl_value_t*)match1);
            if (count > 1)
                break;
        }
        // at this point if there are 0 matches left we found nothing, or if there are
        // more than one the request is ambiguous and we ignore it.
        if (count == 1)
            match = (jl_method_match_t*)jl_array_ptr_ref(matches, 0);
    }
    jl_method_instance_t *mi = NULL;
    if (match != NULL)
        mi = jl_method_match_to_mi(match, world, min_valid2, max_valid2, mt_cache);
    JL_GC_POP();
    return mi;
}

static void _generate_from_hint(jl_method_instance_t *mi, size_t world)
{
    jl_value_t *codeinst = jl_rettype_inferred(mi, world, world);
    if (codeinst == jl_nothing) {
        (void)jl_type_infer(mi, world, 1);
        codeinst = jl_rettype_inferred(mi, world, world);
    }
    if (codeinst != jl_nothing) {
        if (jl_atomic_load_relaxed(&((jl_code_instance_t*)codeinst)->invoke) == jl_fptr_const_return)
            return; // probably not a good idea to generate code
        jl_atomic_store_relaxed(&((jl_code_instance_t*)codeinst)->precompile, 1);
    }
}

static void jl_compile_now(jl_method_instance_t *mi)
{
    size_t world = jl_atomic_load_acquire(&jl_world_counter);
    size_t tworld = jl_typeinf_world;
    _generate_from_hint(mi, world);
    if (jl_typeinf_func && mi->def.method->primary_world <= tworld) {
        // if it's part of the compiler, also attempt to compile for the compiler world too
        _generate_from_hint(mi, tworld);
    }
}

JL_DLLEXPORT void jl_compile_method_instance(jl_method_instance_t *mi, jl_tupletype_t *types, size_t world)
{
    size_t tworld = jl_typeinf_world;
    jl_atomic_store_relaxed(&mi->precompiled, 1);
    if (jl_generating_output()) {
        jl_compile_now(mi);
        // In addition to full compilation of the compilation-signature, if `types` is more specific (e.g. due to nospecialize),
        // also run inference now on the original `types`, since that may help us guide inference to find
        // additional useful methods that should be compiled
        //ALT: if (jl_is_datatype(types) && ((jl_datatype_t*)types)->isdispatchtuple && !jl_egal(mi->specTypes, types))
        //ALT: if (jl_subtype(types, mi->specTypes))
        if (types && !jl_subtype(mi->specTypes, (jl_value_t*)types)) {
            jl_svec_t *tpenv2 = jl_emptysvec;
            jl_value_t *types2 = NULL;
            JL_GC_PUSH2(&tpenv2, &types2);
            types2 = jl_type_intersection_env((jl_value_t*)types, (jl_value_t*)mi->def.method->sig, &tpenv2);
            jl_method_instance_t *mi2 = jl_specializations_get_linfo(mi->def.method, (jl_value_t*)types2, tpenv2);
            JL_GC_POP();
            jl_atomic_store_relaxed(&mi2->precompiled, 1);
            if (jl_rettype_inferred(mi2, world, world) == jl_nothing)
                (void)jl_type_infer(mi2, world, 1);
            if (jl_typeinf_func && mi->def.method->primary_world <= tworld) {
                if (jl_rettype_inferred(mi2, tworld, tworld) == jl_nothing)
                    (void)jl_type_infer(mi2, tworld, 1);
            }
        }
    }
    else {
        // Otherwise (this branch), assuming we are at runtime (normal JIT) and
        // we should generate the native code immediately in preparation for use.
        (void)jl_compile_method_internal(mi, world);
    }
}

JL_DLLEXPORT int jl_compile_hint(jl_tupletype_t *types)
{
    size_t world = jl_atomic_load_acquire(&jl_world_counter);
    size_t min_valid = 0;
    size_t max_valid = ~(size_t)0;
    jl_method_instance_t *mi = jl_get_compile_hint_specialization(types, world, &min_valid, &max_valid, 1);
    if (mi == NULL)
        return 0;
    JL_GC_PROMISE_ROOTED(mi);
    jl_compile_method_instance(mi, types, world);
    return 1;
}

// add type of `f` to front of argument tuple type
static jl_value_t *jl_argtype_with_function(jl_function_t *f, jl_value_t *types0)
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
    tt = jl_rewrap_unionall_(tt, types0);
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
    jl_code_instance_t *codeinst = jl_atomic_load_relaxed(&mfunc->cache);
    while (codeinst) {
        if (codeinst->min_world <= world && world <= codeinst->max_world) {
            jl_callptr_t invoke = jl_atomic_load_acquire(&codeinst->invoke);
            if (invoke != NULL) {
                jl_value_t *res = invoke(F, args, nargs, codeinst);
                return verify_type(res);
            }
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
    jl_callptr_t invoke = jl_atomic_load_acquire(&codeinst->invoke);
    jl_value_t *res = invoke(F, args, nargs, codeinst);
    return verify_type(res);
}

JL_DLLEXPORT jl_value_t *jl_invoke(jl_value_t *F, jl_value_t **args, uint32_t nargs, jl_method_instance_t *mfunc)
{
    size_t world = jl_current_task->world_age;
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

_Atomic(jl_typemap_entry_t*) call_cache[N_CALL_CACHE] JL_GLOBALLY_ROOTED;
static _Atomic(uint8_t) pick_which[N_CALL_CACHE];
#ifdef JL_GF_PROFILE
size_t ncalls;
void call_cache_stats()
{
    int pick_which_stat[4] = {0, 0, 0, 0};
    int i, count = 0;
    for (i = 0; i < N_CALL_CACHE; i++) {
        if (jl_atomic_load_relaxed(&call_cache[i]))
            count++;
        ++pick_which_stat[jl_atomic_load_relaxed(&pick_which[i]) & 3];
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
            entry = jl_atomic_load_relaxed(&call_cache[cache_idx[i]]); \
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
        if (leafcache != (jl_array_t*)jl_an_empty_vec_any &&
                jl_typeis(jl_atomic_load_relaxed(&mt->cache), jl_typemap_level_type)) {
            // hashing args is expensive, but looking at mt->cache is probably even more expensive
            tt = lookup_arg_type_tuple(F, args, nargs);
            if (tt != NULL)
                entry = lookup_leafcache(leafcache, (jl_value_t*)tt, world);
        }
        if (entry == NULL) {
            jl_typemap_t *cache = jl_atomic_load_relaxed(&mt->cache); // XXX: gc root required?
            entry = jl_typemap_assoc_exact(cache, F, args, nargs, jl_cachearg_offset(mt), world);
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
            // (intentionally not atomically synchronized, since we're just using it for randomness)
            // TODO: use the thread's `cong` instead as a source of randomness
            int which = jl_atomic_load_relaxed(&pick_which[cache_idx[0]]) + 1;
            jl_atomic_store_relaxed(&pick_which[cache_idx[0]], which);
            jl_atomic_store_release(&call_cache[cache_idx[which & 3]], entry);
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
        JL_UNLOCK(&mt->writelock);
        JL_GC_POP();
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

JL_DLLEXPORT jl_value_t *jl_apply_generic(jl_value_t *F, jl_value_t **args, uint32_t nargs)
{
    size_t world = jl_current_task->world_age;
    jl_method_instance_t *mfunc = jl_lookup_generic_(F, args, nargs,
                                                     jl_int32hash_fast(jl_return_address()),
                                                     world);
    JL_GC_PROMISE_ROOTED(mfunc);
    return _jl_invoke(F, args, nargs, mfunc, world);
}

static jl_method_match_t *_gf_invoke_lookup(jl_value_t *types JL_PROPAGATES_ROOT, jl_value_t *mt, size_t world, size_t *min_valid, size_t *max_valid)
{
    jl_value_t *unw = jl_unwrap_unionall((jl_value_t*)types);
    if (!jl_is_tuple_type(unw))
        return NULL;
    if (jl_tparam0(unw) == jl_bottom_type)
        return NULL;
    if (mt == jl_nothing)
        mt = (jl_value_t*)jl_method_table_for(unw);
    if (mt == jl_nothing)
        mt = NULL;
    jl_value_t *matches = ml_matches((jl_methtable_t*)mt, (jl_tupletype_t*)types, 1, 0, 0, world, 1, min_valid, max_valid, NULL);
    if (matches == jl_nothing || jl_array_len(matches) != 1)
        return NULL;
    jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(matches, 0);
    return matc;
}

JL_DLLEXPORT jl_value_t *jl_gf_invoke_lookup(jl_value_t *types, jl_value_t *mt, size_t world)
{
    // Deprecated: Use jl_gf_invoke_lookup_worlds for future development
    size_t min_valid = 0;
    size_t max_valid = ~(size_t)0;
    jl_method_match_t *matc = _gf_invoke_lookup(types, mt, world, &min_valid, &max_valid);
    if (matc == NULL)
        return jl_nothing;
    return (jl_value_t*)matc->method;
}


JL_DLLEXPORT jl_value_t *jl_gf_invoke_lookup_worlds(jl_value_t *types, jl_value_t *mt, size_t world, size_t *min_world, size_t *max_world)
{
    jl_method_match_t *matc = _gf_invoke_lookup(types, mt, world, min_world, max_world);
    if (matc == NULL)
        return jl_nothing;
    return (jl_value_t*)matc;
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
jl_value_t *jl_gf_invoke(jl_value_t *types0, jl_value_t *gf, jl_value_t **args, size_t nargs)
{
    size_t world = jl_current_task->world_age;
    jl_value_t *types = NULL;
    JL_GC_PUSH1(&types);
    types = jl_argtype_with_function(gf, types0);
    jl_method_t *method = (jl_method_t*)jl_gf_invoke_lookup(types, jl_nothing, world);
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

jl_value_t *jl_gf_invoke_by_method(jl_method_t *method, jl_value_t *gf, jl_value_t **args, size_t nargs)
{
    jl_method_instance_t *mfunc = NULL;
    jl_typemap_entry_t *tm = NULL;
    jl_typemap_t *invokes = jl_atomic_load_relaxed(&method->invokes);
    if (invokes != jl_nothing)
        tm = jl_typemap_assoc_exact(invokes, gf, args, nargs, 1, 1);
    if (tm) {
        mfunc = tm->func.linfo;
    }
    else {
        int64_t last_alloc = jl_options.malloc_log ? jl_gc_diff_total_bytes() : 0;
        jl_svec_t *tpenv = jl_emptysvec;
        jl_tupletype_t *tt = NULL;
        JL_GC_PUSH2(&tpenv, &tt);
        JL_LOCK(&method->writelock);
        invokes = jl_atomic_load_relaxed(&method->invokes);
        tm = jl_typemap_assoc_exact(invokes, gf, args, nargs, 1, 1);
        if (tm) {
            mfunc = tm->func.linfo;
        }
        else {
            tt = arg_type_tuple(gf, args, nargs);
            if (jl_is_unionall(method->sig)) {
                int sub = jl_subtype_matching((jl_value_t*)tt, (jl_value_t*)method->sig, &tpenv);
                assert(sub); (void)sub;
            }

            mfunc = cache_method(NULL, &method->invokes, (jl_value_t*)method, tt, method, 1, 1, ~(size_t)0, tpenv);
        }
        JL_UNLOCK(&method->writelock);
        JL_GC_POP();
        if (jl_options.malloc_log)
            jl_gc_sync_total_bytes(last_alloc); // discard allocation count from compilation
    }
    JL_GC_PROMISE_ROOTED(mfunc);
    size_t world = jl_current_task->world_age;
    return _jl_invoke(gf, args, nargs - 1, mfunc, world);
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
            tname, module, st, jl_emptysvec, jl_emptysvec, jl_emptysvec, jl_emptysvec,
            0, 0, 0);
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
    jl_task_t *ct = jl_current_task;
    jl_method_match_t *match = (jl_method_match_t*)jl_gc_alloc(ct->ptls, sizeof(jl_method_match_t), jl_method_match_type);
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

static int ml_mtable_visitor(jl_methtable_t *mt, void *env)
{
    return jl_typemap_intersection_visitor(jl_atomic_load_relaxed(&mt->defs), 0, (struct typemap_intersection_env*)env);
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
static jl_value_t *ml_matches(jl_methtable_t *mt,
                              jl_tupletype_t *type, int lim, int include_ambiguous,
                              int intersections, size_t world, int cache_result,
                              size_t *min_valid, size_t *max_valid, int *ambig)
{
    if (world > jl_atomic_load_acquire(&jl_world_counter))
        return jl_nothing; // the future is not enumerable
    int has_ambiguity = 0;
    jl_value_t *unw = jl_unwrap_unionall((jl_value_t*)type);
    assert(jl_is_datatype(unw));
    size_t l = jl_svec_len(((jl_datatype_t*)unw)->parameters);
    jl_value_t *va = NULL;
    if (l > 0) {
        va = jl_tparam(unw, l - 1);
        if (jl_is_vararg(va))
            va = jl_unwrap_vararg(va);
        else
            va = NULL;
    }
    struct ml_matches_env env = {{ml_matches_visitor, (jl_value_t*)type, va,
            /* .ti = */ NULL, /* .env = */ jl_emptysvec, /* .issubty = */ 0},
        intersections, world, lim, /* .t = */ jl_an_empty_vec_any,
        /* .min_valid = */ *min_valid, /* .max_valid = */ *max_valid, /* .matc = */ NULL};
    struct jl_typemap_assoc search = {(jl_value_t*)type, world, jl_emptysvec, 1, ~(size_t)0};
    jl_value_t *isect2 = NULL;
    JL_GC_PUSH6(&env.t, &env.matc, &env.match.env, &search.env, &env.match.ti, &isect2);

    if (mt) {
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
            jl_typemap_entry_t *entry = jl_typemap_assoc_by_type(jl_atomic_load_relaxed(&mt->cache), &search, jl_cachearg_offset(mt), /*subtype*/1);
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
                if (*min_valid < entry->min_world)
                    *min_valid = entry->min_world;
                if (*max_valid > entry->max_world)
                    *max_valid = entry->max_world;
                JL_GC_POP();
                return env.t;
            }
        }
        if (!jl_typemap_intersection_visitor(jl_atomic_load_relaxed(&mt->defs), 0, &env.match)) {
            JL_GC_POP();
            return jl_nothing;
        }
    }
    else {
        // else: scan everything
        if (!jl_foreach_reachable_mtable(ml_mtable_visitor, &env.match)) {
            JL_GC_POP();
            return jl_nothing;
        }
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
        // first try to pre-process the results to find the most specific
        // result that fully covers the input, since we can do this in linear
        // time, and the rest is O(n^2)
        //   - first find a candidate for the best of these method results
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
            else {
                all_subtypes = 0;
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
                        minmax = NULL;
                        has_ambiguity = 1;
                        break;
                    }
                }
            }
        }
        //   - it may even dominate some choices that are not subtypes!
        //     move those into the subtype group, where we're filter them out shortly after
        //     (potentially avoiding reporting these as an ambiguity, and
        //     potentially allowing us to hit the next fast path)
        //   - we could always check here if *any* FULLY_COVERS method is
        //     more-specific (instead of just considering minmax), but that may
        //     cost much extra and is less likely to help us hit a fast path
        //     (we will look for this later, when we compute ambig_groupid, for
        //     correctness)
        if (!all_subtypes && minmax != NULL) {
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
                else if (lim == 1) {
                    JL_GC_POP();
                    return jl_nothing;
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
    if (len > 1) {
        // need to partially domsort the graph now into a list
        // (this is an insertion sort attempt)
        // if we have a minmax method, we ignore anything less specific
        // we'll clean that up next
        for (i = 1; i < len; i++) {
            env.matc = (jl_method_match_t*)jl_array_ptr_ref(env.t, i);
            jl_method_t *m = env.matc->method;
            int subt = env.matc->fully_covers != NOT_FULLY_COVERS;
            if ((minmax != NULL || (minmax_ambig && !include_ambiguous)) && subt) {
                continue; // already the biggest (skip will filter others)
            }
            for (j = 0; j < i; j++) {
                jl_method_match_t *matc2 = (jl_method_match_t *)jl_array_ptr_ref(env.t, i - j - 1);
                jl_method_t *m2 = matc2->method;
                int subt2 = matc2->fully_covers != NOT_FULLY_COVERS;
                if (!subt2 && subt)
                    break;
                if (subt == subt2) {
                    if (lim != -1) {
                        if (subt || !jl_has_empty_intersection(m->sig, m2->sig))
                            if (!jl_type_morespecific((jl_value_t*)m->sig, (jl_value_t*)m2->sig))
                                break;
                    }
                    else {
                        // if unlimited, use approximate sorting, with the only
                        // main downside being that it may be overly-
                        // conservative at reporting existence of ambiguities
                        if (jl_type_morespecific((jl_value_t*)m2->sig, (jl_value_t*)m->sig))
                            break;
                    }
                }
                jl_array_ptr_set(env.t, i - j, matc2);
            }
            jl_array_ptr_set(env.t, i - j, env.matc);
        }
        char *skip = (char*)alloca(len);
        memset(skip, 0, len);
        // if we had a minmax method (any subtypes), now may now be able to
        // quickly cleanup some of our sort result
        if (minmax != NULL || (minmax_ambig && !include_ambiguous)) {
            for (i = 0; i < len; i++) {
                jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(env.t, i);
                if (minmax != matc && matc->fully_covers != NOT_FULLY_COVERS) {
                    skip[i] = 1;
                }
            }
        }
        if (include_ambiguous && lim == -1 && ambig == NULL && !minmax_ambig) {
            // in this case, we don't actually need to compute the ambiguity
            // information at all as the user doesn't need us to filter them
            // out or report them
        }
        else {
            // now that the results are (mostly) sorted, assign group numbers to each ambiguity
            // by computing the specificity-ambiguity matrix covering this query
            uint32_t *ambig_groupid = (uint32_t*)alloca(len * sizeof(uint32_t));
            for (i = 0; i < len; i++)
                ambig_groupid[i] = i;
            // as we go, keep a rough count of how many methods are disjoint, which
            // gives us a lower bound on how many methods we will be returning
            // and lets us stop early if we reach our limit
            int ndisjoint = minmax ? 1 : 0;
            for (i = 0; i < len; i++) {
                jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(env.t, i);
                if (skip[i]) {
                    // if there was a minmax method, we can just pretend the rest are all in the same group:
                    // they're all together but unsorted in the list, since we'll drop them all later anyways
                    assert(matc->fully_covers != NOT_FULLY_COVERS);
                    if (ambig_groupid[len - 1] > i)
                        ambig_groupid[len - 1] = i; // ambiguity covering range [i:len)
                    break;
                }
                jl_method_t *m = matc->method;
                int subt = matc->fully_covers == FULLY_COVERS; // jl_subtype((jl_value_t*)type, (jl_value_t*)m->sig)
                int rsubt = jl_egal((jl_value_t*)matc->spec_types, m->sig);
                int disjoint = 1;
                for (j = len; j > i; j--) {
                    if (ambig_groupid[j - 1] < i) {
                        disjoint = 0;
                        break;
                    }
                    jl_method_match_t *matc2 = (jl_method_match_t*)jl_array_ptr_ref(env.t, j - 1);
                    // can't use skip[j - 1] here, since we still need to make sure the minmax dominates
                    jl_method_t *m2 = matc2->method;
                    int subt2 = matc2->fully_covers == FULLY_COVERS; // jl_subtype((jl_value_t*)type, (jl_value_t*)m2->sig)
                    int rsubt2 = jl_egal((jl_value_t*)matc2->spec_types, m2->sig);
                    jl_value_t *ti;
                    if (!subt && !subt2 && rsubt && rsubt2 && lim == -1 && ambig == NULL)
                        // these would only be filtered out of the list as
                        // ambiguous if they are also type-equal, as we
                        // aren't skipping matches and the user doesn't
                        // care if we report any ambiguities
                        continue;
                    if (jl_type_morespecific((jl_value_t*)m->sig, (jl_value_t*)m2->sig))
                        continue;
                    if (subt) {
                        ti = (jl_value_t*)matc2->spec_types;
                        isect2 = NULL;
                    }
                    else if (subt2) {
                        ti = (jl_value_t*)matc->spec_types;
                        isect2 = NULL;
                    }
                    else {
                        jl_type_intersection2((jl_value_t*)matc->spec_types, (jl_value_t*)matc2->spec_types, &env.match.ti, &isect2);
                        ti = env.match.ti;
                    }
                    if (ti != jl_bottom_type) {
                        disjoint = 0;
                        // m and m2 are ambiguous, but let's see if we can find another method (m3)
                        // that dominates their intersection, and means we can ignore this
                        size_t k;
                        for (k = i; k > 0; k--) {
                            jl_method_match_t *matc3 = (jl_method_match_t*)jl_array_ptr_ref(env.t, k - 1);
                            jl_method_t *m3 = matc3->method;
                            if ((jl_subtype(ti, m3->sig) || (isect2 && jl_subtype(isect2, m3->sig)))
                                    && jl_type_morespecific((jl_value_t*)m3->sig, (jl_value_t*)m->sig)
                                    && jl_type_morespecific((jl_value_t*)m3->sig, (jl_value_t*)m2->sig))
                                break;
                        }
                        if (k == 0) {
                            ambig_groupid[j - 1] = i; // ambiguity covering range [i:j)
                            isect2 = NULL;
                            break;
                        }
                    }
                    isect2 = NULL;
                }
                if (disjoint && lim >= 0) {
                    ndisjoint += 1;
                    if (ndisjoint > lim) {
                        JL_GC_POP();
                        return jl_nothing;
                    }
                }
            }
            // then we'll merge those numbers to assign each item in the group the same number
            // (similar to Kosaraju's SCC algorithm?)
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
            if (lim != -1) {
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
            // remaining methods in the result are in the same ambiguity group.
            assert(len > 0);
            uint32_t agid = ambig_groupid[0];
            for (i = 1; i < len; i++) {
                if (!skip[i]) {
                    if (agid == ambig_groupid[i]) {
                        has_ambiguity = 1;
                        break;
                    }
                    agid = ambig_groupid[i];
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
                                    break;
                                }
                            }
                        }
                    }
                    if (ambig1)
                        skip[i] = 1;
                }
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
    if (mt && cache_result && ((jl_datatype_t*)unw)->isdispatchtuple) { // cache_result parameter keeps this from being recursive
        if (len == 1 && !has_ambiguity) {
            env.matc = (jl_method_match_t*)jl_array_ptr_ref(env.t, 0);
            jl_method_t *meth = env.matc->method;
            jl_svec_t *tpenv = env.matc->sparams;
            JL_LOCK(&mt->writelock);
            cache_method(mt, &mt->cache, (jl_value_t*)mt, (jl_tupletype_t*)unw, meth, world, env.min_valid, env.max_valid, tpenv);
            JL_UNLOCK(&mt->writelock);
        }
    }
    if (ambig != NULL)
        *ambig = has_ambiguity;
    JL_GC_POP();
    if (lim >= 0 && len > lim)
        return jl_nothing;
    return env.t;
}

// see if it might be possible to construct an instance of `typ`
// if n_uninitialized == 0, but a fieldtype is Union{},
// that type will not be constructable, for example, tested recursively
int jl_has_concrete_subtype(jl_value_t *typ)
{
    if (typ == jl_bottom_type)
        return 0;
    typ = jl_unwrap_unionall(typ);
    if (jl_is_vararg(typ))
        typ = jl_unwrap_vararg(typ);
    if (!jl_is_datatype(typ))
        return 1;
    return ((jl_datatype_t*)typ)->has_concrete_subtype;
}

JL_DLLEXPORT void jl_typeinf_timing_begin(void)
{
    jl_task_t *ct = jl_current_task;
    if (!ct->reentrant_timing++) {
        ct->inference_start_time = jl_hrtime();
    }
}

JL_DLLEXPORT void jl_typeinf_timing_end(void)
{
    jl_task_t *ct = jl_current_task;
    if (!--ct->reentrant_timing) {
        if (jl_atomic_load_relaxed(&jl_measure_compile_time_enabled)) {
            uint64_t inftime = jl_hrtime() - ct->inference_start_time;
            jl_atomic_fetch_add_relaxed(&jl_cumulative_compile_time, inftime);
        }
        ct->inference_start_time = 0;
    }
}

JL_DLLEXPORT void jl_typeinf_lock_begin(void)
{
    JL_LOCK(&jl_codegen_lock);
}

JL_DLLEXPORT void jl_typeinf_lock_end(void)
{
    JL_UNLOCK(&jl_codegen_lock);
}

#ifdef __cplusplus
}
#endif

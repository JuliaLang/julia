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

_Atomic(int) allow_new_worlds = 1;
JL_DLLEXPORT _Atomic(size_t) jl_world_counter = 1; // uses atomic acquire/release
jl_mutex_t world_counter_lock;
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

// Compute the maximum number of times to unroll Varargs{T}, based on
// m->max_varargs (if specified) or a heuristic based on the maximum
// number of non-varargs arguments in the provided method table.
//
// If provided, `may_increase` is set to 1 if the returned value is
// heuristic-based and has a chance of increasing in the future.
static size_t get_max_varargs(
        jl_method_t *m,
        jl_methtable_t *kwmt,
        jl_methtable_t *mt,
        uint8_t *may_increase) JL_NOTSAFEPOINT
{
    size_t max_varargs = 1;
    if (may_increase != NULL)
        *may_increase = 0;

    if (m->max_varargs != UINT8_MAX)
        max_varargs = m->max_varargs;
    else if (kwmt != NULL && kwmt != jl_type_type_mt && kwmt != jl_nonfunction_mt && kwmt != jl_kwcall_mt) {
        if (may_increase != NULL)
            *may_increase = 1; // `max_args` can increase as new methods are inserted

        max_varargs = jl_atomic_load_relaxed(&kwmt->max_args) + 2;
        if (mt == jl_kwcall_mt)
            max_varargs += 2;
        max_varargs -= m->nargs;
    }
    return max_varargs;
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
        jl_static_show((JL_STREAM*)STDERR_FILENO, jl_current_exception(ct));
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


static uint_t speccache_hash(size_t idx, jl_value_t *data)
{
    jl_method_instance_t *ml = (jl_method_instance_t*)jl_svecref(data, idx); // This must always happen inside the lock
    jl_value_t *sig = ml->specTypes;
    if (jl_is_unionall(sig))
        sig = jl_unwrap_unionall(sig);
    return ((jl_datatype_t*)sig)->hash;
}

static int speccache_eq(size_t idx, const void *ty, jl_value_t *data, uint_t hv)
{
    if (idx >= jl_svec_len(data))
        return 0; // We got a OOB access, probably due to a data race
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
    if (m->sig == (jl_value_t*)jl_anytuple_type && jl_atomic_load_relaxed(&m->unspecialized) != NULL && m != jl_opaque_closure_method && !m->is_for_opaque_closure)
        return jl_atomic_load_relaxed(&m->unspecialized); // handle builtin methods
    jl_value_t *ut = jl_is_unionall(type) ? jl_unwrap_unionall(type) : type;
    JL_TYPECHK(specializations, datatype, ut);
    uint_t hv = ((jl_datatype_t*)ut)->hash;
    jl_genericmemory_t *speckeyset = NULL;
    jl_value_t *specializations = NULL;
    size_t i = -1, cl = 0, lastcl;
    for (int locked = 0; locked < 2; locked++) {
        if (locked) {
            if (!sparams) // can't insert without knowing this
                return NULL;
            JL_LOCK(&m->writelock);
        }
        lastcl = cl;
        speckeyset = jl_atomic_load_acquire(&m->speckeyset);
        specializations = jl_atomic_load_relaxed(&m->specializations);
        if (specializations == (jl_value_t*)jl_emptysvec)
            continue;
        if (!jl_is_svec(specializations)) {
            jl_method_instance_t *mi = (jl_method_instance_t*)specializations;
            if (jl_types_equal(mi->specTypes, type)) {
                if (locked)
                    JL_UNLOCK(&m->writelock);
                return mi;
            }
            continue;
        }
        cl = jl_svec_len(specializations);
        if (hv) {
            ssize_t idx = jl_smallintset_lookup(speckeyset, speccache_eq, type, specializations, hv, 0);
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
            // the last lastcl-i-1 elements are already checked when locked, so start search with the new elements only
            for (i += cl - lastcl; i > 0; i--) {
                jl_method_instance_t *mi = jl_atomic_load_relaxed(&data[i]);
                if ((jl_value_t*)mi == jl_nothing)
                    break;
                if (jl_types_equal(mi->specTypes, type)) {
                    if (locked)
                        JL_UNLOCK(&m->writelock);
                    JL_GC_POP();
                    return mi;
                }
            }
            // i points to the first unchecked element, or the place to insert
            JL_GC_POP();
        }
    }
    jl_method_instance_t *mi = mi_insert ? mi_insert : jl_get_specialized(m, type, sparams);
    if (specializations == (jl_value_t*)jl_emptysvec) {
        jl_atomic_store_release(&m->specializations, (jl_value_t*)mi);
        jl_gc_wb(m, mi);
    }
    else {
        JL_GC_PUSH1(&mi);
        if (!jl_is_svec(specializations)) {
            jl_method_instance_t *mi = (jl_method_instance_t*)specializations;
            jl_value_t *type = mi->specTypes;
            jl_value_t *ut = jl_is_unionall(type) ? jl_unwrap_unionall(type) : type;
            uint_t hv = ((jl_datatype_t*)ut)->hash;
            cl = 7;
            i = cl - 1;
            specializations = (jl_value_t*)jl_svec_fill(cl, jl_nothing);
            jl_svecset(specializations, hv ? 0 : i--, mi);
            jl_atomic_store_release(&m->specializations, specializations);
            jl_gc_wb(m, specializations);
            if (hv)
                jl_smallintset_insert(&m->speckeyset, (jl_value_t*)m, speccache_hash, 0, specializations);
        }
        if (hv) {
            _Atomic(jl_method_instance_t*) *data = (_Atomic(jl_method_instance_t*)*)jl_svec_data(specializations);
            for (i = 0; i < cl; i++) {
                jl_method_instance_t *mi = jl_atomic_load_relaxed(&data[i]);
                if ((jl_value_t*)mi == jl_nothing)
                    break;
                assert(!jl_types_equal(mi->specTypes, type));
            }
            // i points at the place to insert
        }
        if (hv ? (i + 1 >= cl || jl_svecref(specializations, i + 1) != jl_nothing) : (i <= 1 || jl_svecref(specializations, i - 2) != jl_nothing)) {
            size_t ncl = cl < 7 ? 7 : (cl*3)>>1;
            jl_svec_t *nc = jl_alloc_svec_uninit(ncl);
            if (i > 0)
                memcpy((char*)jl_svec_data(nc), jl_svec_data(specializations), sizeof(void*) * i);
            for (int j = 0; j < ncl - cl; j++)
                jl_svecset(nc, j+i, jl_nothing);
            if (i < cl)
                memcpy((char*)jl_svec_data(nc) + sizeof(void*) * (i + ncl - cl),
                       (char*)jl_svec_data(specializations) + sizeof(void*) * i,
                       sizeof(void*) * (cl - i));
            specializations = (jl_value_t*)nc;
            jl_atomic_store_release(&m->specializations, specializations);
            jl_gc_wb(m, specializations);
            if (!hv)
                i += ncl - cl;
        }
        assert(jl_svecref(specializations, i) == jl_nothing);
        jl_svecset(specializations, i, mi);
        if (hv)
            jl_smallintset_insert(&m->speckeyset, (jl_value_t*)m, speccache_hash, i, specializations);
        JL_GC_POP();
    }
    JL_UNLOCK(&m->writelock); // may gc
    return mi;
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
    // TODO: this is sort of an odd lookup strategy (and the only user of
    // jl_typemap_assoc_by_type with subtype=0), while normally jl_gf_invoke_lookup would be
    // expected to be used instead
    struct jl_typemap_assoc search = {type, world, NULL};
    jl_typemap_entry_t *sf = jl_typemap_assoc_by_type(jl_atomic_load_relaxed(&mt->defs), &search, jl_cachearg_offset(mt), /*subtype*/0);
    if (!sf)
        return jl_nothing;
    return sf->func.value;
}

// ----- MethodInstance specialization instantiation ----- //

jl_datatype_t *jl_mk_builtin_func(jl_datatype_t *dt, const char *name, jl_fptr_args_t fptr) JL_GC_DISABLED
{
    jl_sym_t *sname = jl_symbol(name);
    if (dt == NULL) {
        // Builtins are specially considered available from world 0
        jl_value_t *f = jl_new_generic_function_with_supertype(sname, jl_core_module, jl_builtin_type, 0);
        jl_set_initial_const(jl_core_module, sname, f, 0);
        dt = (jl_datatype_t*)jl_typeof(f);
    }

    jl_method_t *m = jl_new_method_uninit(jl_core_module);
    m->name = sname;
    m->module = jl_core_module;
    m->isva = 1;
    m->nargs = 2;
    jl_atomic_store_relaxed(&m->primary_world, 1);
    jl_atomic_store_relaxed(&m->dispatch_status, METHOD_SIG_LATEST_ONLY | METHOD_SIG_LATEST_ONLY);
    m->sig = (jl_value_t*)jl_anytuple_type;
    m->slot_syms = jl_an_empty_string;
    m->nospecialize = 0;
    m->nospecialize = ~m->nospecialize;

    jl_methtable_t *mt = dt->name->mt;
    jl_typemap_entry_t *newentry = NULL;
    JL_GC_PUSH2(&m, &newentry);

    newentry = jl_typemap_alloc(jl_anytuple_type, NULL, jl_emptysvec,
            (jl_value_t*)m, 1, ~(size_t)0);
    jl_typemap_insert(&mt->defs, (jl_value_t*)mt, newentry, jl_cachearg_offset(mt));

    jl_method_instance_t *mi = jl_get_specialized(m, (jl_value_t*)jl_anytuple_type, jl_emptysvec);
    jl_atomic_store_relaxed(&m->unspecialized, mi);
    jl_gc_wb(m, mi);

    jl_code_instance_t *codeinst = jl_new_codeinst(mi, jl_nothing,
        (jl_value_t*)jl_any_type, (jl_value_t*)jl_any_type, jl_nothing, jl_nothing,
        0, 1, ~(size_t)0, 0, jl_nothing, NULL, NULL);
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

// only relevant for bootstrapping. otherwise fairly broken.
static int emit_codeinst_and_edges(jl_code_instance_t *codeinst)
{
    jl_value_t *code = jl_atomic_load_relaxed(&codeinst->inferred);
    if (code) {
        if (jl_atomic_load_relaxed(&codeinst->invoke) != NULL)
            return 1;
        if (code != jl_nothing) {
            JL_GC_PUSH1(&code);
            jl_method_instance_t *mi = jl_get_ci_mi(codeinst);
            jl_method_t *def = mi->def.method;
            if (jl_is_string(code) && jl_is_method(def))
                code = (jl_value_t*)jl_uncompress_ir(def, codeinst, (jl_value_t*)code);
            if (jl_is_code_info(code)) {
                jl_emit_codeinst_to_jit(codeinst, (jl_code_info_t*)code);
                if (0) {
                    // next emit all the invoke edges too (if this seems profitable)
                    jl_array_t *src = ((jl_code_info_t*)code)->code;
                    for (size_t i = 0; i < jl_array_dim0(src); i++) {
                        jl_value_t *stmt = jl_array_ptr_ref(src, i);
                        if (jl_is_expr(stmt) && ((jl_expr_t*)stmt)->head == jl_assign_sym)
                            stmt = jl_exprarg(stmt, 1);
                        if (jl_is_expr(stmt) && ((jl_expr_t*)stmt)->head == jl_invoke_sym) {
                            jl_value_t *invoke = jl_exprarg(stmt, 0);
                            if (jl_is_code_instance(invoke))
                                emit_codeinst_and_edges((jl_code_instance_t*)invoke);
                        }
                    }
                }
                JL_GC_POP();
                return 1;
            }
            JL_GC_POP();
        }
    }
    return 0;
}

// Opportunistic SOURCE_MODE_ABI cache lookup, only for bootstrapping.
static jl_code_instance_t *jl_method_inferred_with_abi(jl_method_instance_t *mi JL_PROPAGATES_ROOT, size_t world)
{
    jl_code_instance_t *codeinst = jl_atomic_load_relaxed(&mi->cache);
    for (; codeinst; codeinst = jl_atomic_load_relaxed(&codeinst->next)) {
        if (codeinst->owner != jl_nothing)
            continue;
        if (jl_atomic_load_relaxed(&codeinst->min_world) <= world && world <= jl_atomic_load_relaxed(&codeinst->max_world)) {
            if (emit_codeinst_and_edges(codeinst))
                return codeinst;
        }
    }
    return NULL;
}

// run type inference on lambda "mi" for given argument types.
// returns the inferred source, and may cache the result in mi
// if successful, also updates the mi argument to describe the validity of this src
// if inference doesn't occur (or can't finish), returns NULL instead
jl_code_instance_t *jl_type_infer(jl_method_instance_t *mi, size_t world, uint8_t source_mode)
{
    if (jl_typeinf_func == NULL) {
        if (source_mode == SOURCE_MODE_ABI)
            return jl_method_inferred_with_abi(mi, world);
        else
            return NULL;
    }
    jl_task_t *ct = jl_current_task;
    if (ct->reentrant_timing & 0b1000) {
        // We must avoid attempting to re-enter inference here
        assert(0 && "attempted to enter inference while writing out image");
        abort();
    }
    // In case we use higher bits later, mask them out
    if ((ct->reentrant_timing & 0b1111) >= 0b110)
        return NULL;

    jl_code_instance_t *ci = NULL;
#ifdef ENABLE_INFERENCE
    if (jl_engine_hasreserved(mi, jl_nothing)) // don't recur on a thread on the same MethodInstance--force it to interpret it until the inference has finished
        return NULL;
    JL_TIMING(INFERENCE, INFERENCE);
    jl_value_t **fargs;
    JL_GC_PUSHARGS(fargs, 4);
    fargs[0] = (jl_value_t*)jl_typeinf_func;
    fargs[1] = (jl_value_t*)mi;
    fargs[2] = jl_box_ulong(world);
    fargs[3] = jl_box_uint8(source_mode);
    int last_errno = errno;
#ifdef _OS_WINDOWS_
    DWORD last_error = GetLastError();
#endif

    jl_timing_show_method_instance(mi, JL_TIMING_DEFAULT_BLOCK);
#ifdef TRACE_INFERENCE
    if (mi->specTypes != (jl_value_t*)jl_emptytuple_type) {
        jl_printf(JL_STDERR,"inference on ");
        jl_static_show_func_sig(JL_STDERR, (jl_value_t*)mi->specTypes);
        jl_printf(JL_STDERR, "\n");
    }
#endif
    int last_pure = ct->ptls->in_pure_callback;
    ct->ptls->in_pure_callback = 0;
    size_t last_age = ct->world_age;
    ct->world_age = jl_typeinf_world;
    // first bit is for reentrant timing,
    // so adding 1 to the bit above performs
    // inference reentrancy counter addition.
    // Note that this is only safe because
    // the counter varies from 0-3; if we
    // increase that limit, we'll need to
    // allocate another bit for the counter.
    ct->reentrant_timing += 0b10;
    JL_TRY {
        ci = (jl_code_instance_t*)jl_apply(fargs, 4);
    }
    JL_CATCH {
        jl_value_t *e = jl_current_exception(ct);
        jl_printf((JL_STREAM*)STDERR_FILENO, "Internal error: during type inference of\n");
        jl_static_show_func_sig((JL_STREAM*)STDERR_FILENO, (jl_value_t*)mi->specTypes);
        jl_printf((JL_STREAM*)STDERR_FILENO, "\nEncountered ");
        if (e == jl_stackovf_exception) {
            jl_printf((JL_STREAM*)STDERR_FILENO, "stack overflow.\n");
            jl_printf((JL_STREAM*)STDERR_FILENO, "This might be caused by recursion over very long tuples or argument lists.\n");
        }
        else {
            jl_printf((JL_STREAM*)STDERR_FILENO, "unexpected error in runtime:\n");
            jl_static_show((JL_STREAM*)STDERR_FILENO, e);
            jl_printf((JL_STREAM*)STDERR_FILENO, "\n");
            jlbacktrace(); // written to STDERR_FILENO
        }
        ci = NULL;
#ifndef JL_NDEBUG
        abort();
#endif
    }
    ct->world_age = last_age;
    ct->reentrant_timing -= 0b10;
    ct->ptls->in_pure_callback = last_pure;
#ifdef _OS_WINDOWS_
    SetLastError(last_error);
#endif
    errno = last_errno;

    if (ci && !jl_is_code_instance(ci)) {
        ci = NULL;
    }
    JL_GC_POP();
#endif

    return ci;
}

// Attempt to run `Core.Compiler.code_typed` on the lambda "mi"
JL_DLLEXPORT jl_code_info_t *jl_gdbcodetyped1(jl_method_instance_t *mi, size_t world)
{
    jl_task_t *ct = jl_current_task;
    jl_code_info_t *ci = NULL;
    int last_errno = errno;
#ifdef _OS_WINDOWS_
    DWORD last_error = GetLastError();
#endif
    int last_pure = ct->ptls->in_pure_callback;
    ct->ptls->in_pure_callback = 0;
    size_t last_age = ct->world_age;
    ct->world_age = jl_typeinf_world;
    jl_value_t **fargs;
    JL_GC_PUSHARGS(fargs, 4);
    jl_module_t *CC = (jl_module_t*)jl_get_global(jl_core_module, jl_symbol("Compiler"));
    if (CC != NULL && jl_is_module(CC)) {
        fargs[0] = jl_get_global(CC, jl_symbol("NativeInterpreter"));;
        fargs[1] = jl_box_ulong(world);
        fargs[1] = jl_apply(fargs, 2);
        fargs[0] = jl_get_global(CC, jl_symbol("typeinf_code"));
        fargs[2] = (jl_value_t*)mi;
        fargs[3] = jl_true;
        ci = (jl_code_info_t*)jl_apply(fargs, 4);
    }
    ct->world_age = last_age;
    ct->ptls->in_pure_callback = last_pure;
#ifdef _OS_WINDOWS_
    SetLastError(last_error);
#endif
    errno = last_errno;
    if (ci && !jl_is_code_info(ci)) {
        ci = NULL;
    }
    JL_GC_POP();
    return ci;
}

JL_DLLEXPORT jl_value_t *jl_call_in_typeinf_world(jl_value_t **args, int nargs)
{
    jl_task_t *ct = jl_current_task;
    size_t last_age = ct->world_age;
    ct->world_age = jl_typeinf_world;
    int last_pure = ct->ptls->in_pure_callback;
    ct->ptls->in_pure_callback = 0;
    jl_value_t *ret = jl_apply(args, nargs);
    ct->ptls->in_pure_callback = last_pure;
    ct->world_age = last_age;
    return ret;
}

JL_DLLEXPORT jl_code_instance_t *jl_get_method_inferred(
        jl_method_instance_t *mi JL_PROPAGATES_ROOT, jl_value_t *rettype,
        size_t min_world, size_t max_world, jl_debuginfo_t *di, jl_svec_t *edges)
{
    jl_value_t *owner = jl_nothing; // TODO: owner should be arg
    jl_code_instance_t *codeinst = jl_atomic_load_relaxed(&mi->cache);
    while (codeinst) {
        if (jl_atomic_load_relaxed(&codeinst->min_world) == min_world &&
            jl_atomic_load_relaxed(&codeinst->max_world) == max_world &&
            jl_egal(codeinst->owner, owner) &&
            jl_egal(codeinst->rettype, rettype)) {
            if (di == NULL)
                return codeinst;
            jl_debuginfo_t *debuginfo = jl_atomic_load_relaxed(&codeinst->debuginfo);
            if (di != debuginfo) {
                if (!(debuginfo == NULL && jl_atomic_cmpswap_relaxed(&codeinst->debuginfo, &debuginfo, di)))
                    if (!(debuginfo && jl_egal((jl_value_t*)debuginfo, (jl_value_t*)di)))
                        continue;
            }
            // TODO: this is implied by the matching worlds, since it is intrinsic, so do we really need to verify it?
            jl_svec_t *e = jl_atomic_load_relaxed(&codeinst->edges);
            if (e && jl_egal((jl_value_t*)e, (jl_value_t*)edges))
                return codeinst;
        }
        codeinst = jl_atomic_load_relaxed(&codeinst->next);
    }
    codeinst = jl_new_codeinst(
        mi, owner, rettype, (jl_value_t*)jl_any_type, NULL, NULL,
        0, min_world, max_world, 0, jl_nothing, di, edges);
    jl_mi_cache_insert(mi, codeinst);
    return codeinst;
}

JL_DLLEXPORT int jl_mi_cache_has_ci(jl_method_instance_t *mi,
                                    jl_code_instance_t *ci)
{
    jl_code_instance_t *codeinst = jl_atomic_load_relaxed(&mi->cache);
    while (codeinst) {
        if (codeinst == ci)
            return 1;
        codeinst = jl_atomic_load_relaxed(&codeinst->next);
    }
    return 0;
}

// look for something with an egal ABI and properties that is already in the JIT for a whole edge (target_world=0) or can be added to the JIT with new source just for target_world.
JL_DLLEXPORT jl_code_instance_t *jl_get_ci_equiv(jl_code_instance_t *ci JL_PROPAGATES_ROOT, size_t target_world) JL_NOTSAFEPOINT
{
    jl_value_t *def = ci->def;
    jl_method_instance_t *mi = jl_get_ci_mi(ci);
    jl_value_t *owner = ci->owner;
    jl_value_t *rettype = ci->rettype;
    size_t min_world = jl_atomic_load_relaxed(&ci->min_world);
    size_t max_world = jl_atomic_load_relaxed(&ci->max_world);
    jl_code_instance_t *codeinst = jl_atomic_load_relaxed(&mi->cache);
    while (codeinst) {
        if (codeinst != ci &&
            jl_atomic_load_relaxed(&codeinst->inferred) != NULL &&
            (target_world ? 1 : jl_atomic_load_relaxed(&codeinst->invoke) != NULL) &&
            jl_atomic_load_relaxed(&codeinst->min_world) <= (target_world ? target_world : min_world) &&
            jl_atomic_load_relaxed(&codeinst->max_world) >= (target_world ? target_world : max_world) &&
            jl_egal(codeinst->def, def) &&
            jl_egal(codeinst->owner, owner) &&
            jl_egal(codeinst->rettype, rettype)) {
            return codeinst;
        }
        codeinst = jl_atomic_load_relaxed(&codeinst->next);
    }
    return ci;
}


JL_DLLEXPORT jl_code_instance_t *jl_new_codeinst(
        jl_method_instance_t *mi, jl_value_t *owner,
        jl_value_t *rettype, jl_value_t *exctype,
        jl_value_t *inferred_const, jl_value_t *inferred,
        int32_t const_flags, size_t min_world, size_t max_world,
        uint32_t effects, jl_value_t *analysis_results,
        jl_debuginfo_t *di, jl_svec_t *edges /*, int absolute_max*/)
{
    assert(min_world <= max_world && "attempting to set invalid world constraints");
    //assert((!jl_is_method(mi->def.value) || max_world != ~(size_t)0 || min_world <= 1 || edges == NULL || jl_svec_len(edges) != 0) && "missing edges");
    jl_task_t *ct = jl_current_task;
    jl_code_instance_t *codeinst = (jl_code_instance_t*)jl_gc_alloc(ct->ptls, sizeof(jl_code_instance_t),
            jl_code_instance_type);
    codeinst->def = (jl_value_t*)mi;
    codeinst->owner = owner;
    jl_atomic_store_relaxed(&codeinst->edges, edges);
    jl_atomic_store_relaxed(&codeinst->min_world, min_world);
    jl_atomic_store_relaxed(&codeinst->max_world, max_world);
    codeinst->rettype = rettype;
    codeinst->exctype = exctype;
    jl_atomic_store_release(&codeinst->inferred, inferred);
    if ((const_flags & 2) == 0)
        inferred_const = NULL;
    codeinst->rettype_const = inferred_const;
    jl_atomic_store_relaxed(&codeinst->debuginfo, (jl_value_t*)di == jl_nothing ? NULL : di);
    jl_atomic_store_relaxed(&codeinst->specptr.fptr, NULL);
    jl_atomic_store_relaxed(&codeinst->invoke, NULL);
    if ((const_flags & 1) != 0) {
        assert(const_flags & 2);
        jl_atomic_store_relaxed(&codeinst->invoke, jl_fptr_const_return);
    }
    codeinst->time_infer_total = 0;
    codeinst->time_infer_self = 0;
    jl_atomic_store_relaxed(&codeinst->time_compile, 0);
    jl_atomic_store_relaxed(&codeinst->specsigflags, 0);
    jl_atomic_store_relaxed(&codeinst->precompile, 0);
    jl_atomic_store_relaxed(&codeinst->next, NULL);
    jl_atomic_store_relaxed(&codeinst->ipo_purity_bits, effects);
    codeinst->analysis_results = analysis_results;
    return codeinst;
}

JL_DLLEXPORT void jl_update_codeinst(
        jl_code_instance_t *codeinst, jl_value_t *inferred,
        int32_t const_flags, size_t min_world, size_t max_world,
        uint32_t effects, jl_value_t *analysis_results,
        double time_infer_total, double time_infer_cache_saved, double time_infer_self,
        jl_debuginfo_t *di, jl_svec_t *edges /* , int absolute_max*/)
{
    assert(min_world <= max_world && "attempting to set invalid world constraints");
    //assert((!jl_is_method(codeinst->def->def.value) || max_world != ~(size_t)0 || min_world <= 1 || jl_svec_len(edges) != 0) && "missing edges");
    codeinst->analysis_results = analysis_results;
    jl_gc_wb(codeinst, analysis_results);
    codeinst->time_infer_total = julia_double_to_half(time_infer_total);
    codeinst->time_infer_cache_saved = julia_double_to_half(time_infer_cache_saved);
    codeinst->time_infer_self = julia_double_to_half(time_infer_self);
    jl_atomic_store_relaxed(&codeinst->ipo_purity_bits, effects);
    jl_atomic_store_relaxed(&codeinst->debuginfo, di);
    jl_gc_wb(codeinst, di);
    jl_atomic_store_relaxed(&codeinst->edges, edges);
    jl_gc_wb(codeinst, edges);
    if ((const_flags & 1) != 0) {
        assert(codeinst->rettype_const);
        jl_atomic_store_release(&codeinst->invoke, jl_fptr_const_return);
    }
    jl_atomic_store_release(&codeinst->inferred, inferred);
    jl_gc_wb(codeinst, inferred);
    jl_atomic_store_relaxed(&codeinst->min_world, min_world); // XXX: these should be unchanged?
    jl_atomic_store_relaxed(&codeinst->max_world, max_world); // since the edges shouldn't change after jl_fill_codeinst
}

JL_DLLEXPORT void jl_fill_codeinst(
        jl_code_instance_t *codeinst,
        jl_value_t *rettype, jl_value_t *exctype,
        jl_value_t *inferred_const,
        int32_t const_flags, size_t min_world, size_t max_world,
        uint32_t effects, jl_value_t *analysis_results,
        jl_debuginfo_t *di, jl_svec_t *edges /* , int absolute_max*/)
{
    assert(min_world <= max_world && "attempting to set invalid world constraints");
    //assert((!jl_is_method(codeinst->def->def.value) || max_world != ~(size_t)0 || min_world <= 1 || jl_svec_len(edges) != 0) && "missing edges");
    codeinst->rettype = rettype;
    jl_gc_wb(codeinst, rettype);
    codeinst->exctype = exctype;
    jl_gc_wb(codeinst, exctype);
    if ((const_flags & 2) != 0) {
        codeinst->rettype_const = inferred_const;
        jl_gc_wb(codeinst, inferred_const);
    }
    jl_atomic_store_relaxed(&codeinst->edges, edges);
    jl_gc_wb(codeinst, edges);
    if ((jl_value_t*)di != jl_nothing) {
        jl_atomic_store_relaxed(&codeinst->debuginfo, di);
        jl_gc_wb(codeinst, di);
    }
    if ((const_flags & 1) != 0) {
        // TODO: may want to follow ordering restrictions here (see jitlayers.cpp)
        assert(const_flags & 2);
        jl_atomic_store_release(&codeinst->invoke, jl_fptr_const_return);
    }
    jl_atomic_store_relaxed(&codeinst->ipo_purity_bits, effects);
    codeinst->analysis_results = analysis_results;
    assert(jl_atomic_load_relaxed(&codeinst->min_world) == 1);
    assert(jl_atomic_load_relaxed(&codeinst->max_world) == 0);
    jl_atomic_store_release(&codeinst->inferred, jl_nothing);
    jl_atomic_store_release(&codeinst->min_world, min_world);
    jl_atomic_store_release(&codeinst->max_world, max_world);
}

JL_DLLEXPORT jl_code_instance_t *jl_new_codeinst_uninit(jl_method_instance_t *mi, jl_value_t *owner)
{
    jl_code_instance_t *codeinst = jl_new_codeinst(mi, owner, NULL, NULL, NULL, NULL, 0, 0, 0, 0, NULL, NULL, NULL);
    jl_atomic_store_relaxed(&codeinst->min_world, 1); // make temporarily invalid before returning, so that jl_fill_codeinst is valid later
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

JL_DLLEXPORT int jl_mi_try_insert(jl_method_instance_t *mi JL_ROOTING_ARGUMENT,
                                   jl_code_instance_t *expected_ci,
                                   jl_code_instance_t *ci JL_ROOTED_ARGUMENT JL_MAYBE_UNROOTED)
{
    JL_GC_PUSH1(&ci);
    if (jl_is_method(mi->def.method))
        JL_LOCK(&mi->def.method->writelock);
    jl_code_instance_t *oldci = jl_atomic_load_relaxed(&mi->cache);
    int ret = 0;
    if (oldci == expected_ci) {
        jl_atomic_store_relaxed(&ci->next, oldci);
        if (oldci)
            jl_gc_wb(ci, oldci);
        jl_atomic_store_release(&mi->cache, ci);
        jl_gc_wb(mi, ci);
        ret = 1;
    }
    if (jl_is_method(mi->def.method))
        JL_UNLOCK(&mi->def.method->writelock);
    JL_GC_POP();
    return ret;
}

int foreach_mtable_in_module(
        jl_module_t *m,
        int (*visit)(jl_methtable_t *mt, void *env),
        void *env)
{
    jl_svec_t *table = jl_atomic_load_relaxed(&m->bindings);
    for (size_t i = 0; i < jl_svec_len(table); i++) {
        jl_binding_t *b = (jl_binding_t*)jl_svecref(table, i);
        if ((void*)b == jl_nothing)
            break;
        jl_sym_t *name = b->globalref->name;
        jl_value_t *v = jl_get_binding_value_if_const(b);
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
        for (i = 0; i < jl_array_nrows(mod_array); i++) {
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


jl_function_t *jl_typeinf_func JL_GLOBALLY_ROOTED = NULL;
JL_DLLEXPORT size_t jl_typeinf_world = 1;

JL_DLLEXPORT void jl_set_typeinf_func(jl_value_t *f)
{
    jl_typeinf_func = (jl_function_t*)f;
    jl_typeinf_world = jl_get_tls_world_age();
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
                vm = T_has_tv ? jl_type_unionall(v, T) : T;
                if (N_has_tv)
                    N = NULL;
                vm = (jl_value_t*)jl_wrap_vararg(vm, N, 1, 0); // this cannot throw for these inputs
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
    intptr_t max_varargs,
    // output:
    jl_svec_t **const newparams JL_REQUIRE_ROOTED_SLOT)
{
    assert(jl_is_tuple_type(tt));
    jl_value_t *decl = definition->sig;
    size_t nargs = definition->nargs; // == jl_nparams(jl_unwrap_unionall(decl));
    size_t nspec = max_varargs + nargs;

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
            // (there were probably fewer methods defined when we first selected this signature, or
            //  the max varargs limit was not reached indicating the type is already fully-specialized)
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
        if (max_varargs > 0) {
            type_i = jl_svecref(*newparams, nspec - 2);
        } else {
            // If max varargs is zero, always specialize to (Any...) since
            // there is no preceding parameter to use for `type_i`
            type_i = jl_bottom_type;
        }
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
            type_i = (jl_value_t*)jl_wrap_vararg(type_i, (jl_value_t*)NULL, 1, 0); // this cannot throw for these inputs
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
        unsigned nspec_min = nargs + 1; // min number of arg values (including tail vararg)
        unsigned nspec_max = INT32_MAX; // max number of arg values (including tail vararg)
        jl_methtable_t *mt = jl_method_table_for(decl);
        jl_methtable_t *kwmt = mt == jl_kwcall_mt ? jl_kwmethod_table_for(decl) : mt;
        if ((jl_value_t*)mt != jl_nothing) {
            // try to refine estimate of min and max
            uint8_t heuristic_used = 0;
            nspec_max = nspec_min = nargs + get_max_varargs(definition, kwmt, mt, &heuristic_used);
            if (heuristic_used)
                nspec_max = INT32_MAX; // new methods may be added, increasing nspec_min later
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

static inline jl_typemap_entry_t *lookup_leafcache(jl_genericmemory_t *leafcache JL_PROPAGATES_ROOT, jl_value_t *tt, size_t world) JL_NOTSAFEPOINT
{
    jl_typemap_entry_t *entry = (jl_typemap_entry_t*)jl_eqtable_get(leafcache, (jl_value_t*)tt, NULL);
    if (entry) {
        do {
            if (jl_atomic_load_relaxed(&entry->min_world) <= world && world <= jl_atomic_load_relaxed(&entry->max_world)) {
                if (entry->simplesig == (void*)jl_nothing || concretesig_equal(tt, (jl_value_t*)entry->simplesig))
                    return entry;
            }
            entry = jl_atomic_load_relaxed(&entry->next);
        } while ((jl_value_t*)entry != jl_nothing);
    }
    return NULL;
}
jl_method_instance_t *cache_method(
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
            jl_genericmemory_t *leafcache = jl_atomic_load_relaxed(&mt->leafcache);
            jl_typemap_entry_t *entry = lookup_leafcache(leafcache, (jl_value_t*)tt, world);
            if (entry)
                return entry->func.linfo;
        }
        struct jl_typemap_assoc search = {(jl_value_t*)tt, world, NULL};
        jl_typemap_t *cacheentry = jl_atomic_load_relaxed(cache);
        assert(cacheentry != NULL);
        jl_typemap_entry_t *entry = jl_typemap_assoc_by_type(cacheentry, &search, offs, /*subtype*/1);
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
    intptr_t max_varargs = get_max_varargs(definition, kwmt, mt, NULL);
    jl_compilation_sig(tt, sparams, definition, max_varargs, &newparams);
    if (newparams) {
        temp2 = jl_apply_tuple_type(newparams, 1);
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
    if (newmeth->cache_with_orig)
        cache_with_orig = 1;

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
            size_t i, l = jl_array_nrows(temp);
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
            for (i = 0, l = jl_array_nrows(temp); i < l; i++) {
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
        else {
            // do not revisit this decision
            newmeth->cache_with_orig = 1;
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
        simplett = (jl_datatype_t*)jl_apply_tuple_type(newparams, 1);
        temp2 = (jl_value_t*)simplett;
    }

    // short-circuit if an existing entry is already present
    // that satisfies our requirements
    if (cachett != tt) {
        struct jl_typemap_assoc search = {(jl_value_t*)cachett, world, NULL};
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
        jl_genericmemory_t *oldcache = jl_atomic_load_relaxed(&mt->leafcache);
        jl_typemap_entry_t *old = (jl_typemap_entry_t*)jl_eqtable_get(oldcache, (jl_value_t*)tt, jl_nothing);
        jl_atomic_store_relaxed(&newentry->next, old);
        jl_gc_wb(newentry, old);
        jl_genericmemory_t *newcache = jl_eqtable_put(jl_atomic_load_relaxed(&mt->leafcache), (jl_value_t*)tt, (jl_value_t*)newentry, NULL);
        if (newcache != oldcache) {
            jl_atomic_store_release(&mt->leafcache, newcache);
            jl_gc_wb(mt, newcache);
        }
    }
    else {
         jl_typemap_insert(cache, parent, newentry, offs);
         if (mt) {
             jl_datatype_t *dt = jl_nth_argument_datatype((jl_value_t*)tt, 1);
             if (dt) {
                 jl_typename_t *tn = dt->name;
                 int cache_entry_count = jl_atomic_load_relaxed(&tn->cache_entry_count);
                 if (cache_entry_count < 31)
                     jl_atomic_store_relaxed(&tn->cache_entry_count, cache_entry_count + 1);
             }
         }
    }

    JL_GC_POP();
    return newmeth;
}

static jl_method_match_t *_gf_invoke_lookup(jl_value_t *types JL_PROPAGATES_ROOT, jl_value_t *mt, size_t world, size_t *min_valid, size_t *max_valid);

static jl_method_instance_t *jl_mt_assoc_by_type(jl_methtable_t *mt JL_PROPAGATES_ROOT, jl_datatype_t *tt JL_MAYBE_UNROOTED, size_t world)
{
    jl_genericmemory_t *leafcache = jl_atomic_load_relaxed(&mt->leafcache);
    jl_typemap_entry_t *entry = lookup_leafcache(leafcache, (jl_value_t*)tt, world);
    if (entry)
        return entry->func.linfo;
    JL_TIMING(METHOD_LOOKUP_SLOW, METHOD_LOOKUP_SLOW);
    jl_method_match_t *matc = NULL;
    JL_GC_PUSH2(&tt, &matc);
    JL_LOCK(&mt->writelock);
    assert(tt->isdispatchtuple || tt->hasfreetypevars);
    jl_method_instance_t *mi = NULL;
    if (tt->isdispatchtuple) {
        jl_genericmemory_t *leafcache = jl_atomic_load_relaxed(&mt->leafcache);
        jl_typemap_entry_t *entry = lookup_leafcache(leafcache, (jl_value_t*)tt, world);
        if (entry)
            mi = entry->func.linfo;
    }

    if (!mi) {
        struct jl_typemap_assoc search = {(jl_value_t*)tt, world, NULL};
        jl_typemap_entry_t *entry = jl_typemap_assoc_by_type(jl_atomic_load_relaxed(&mt->cache), &search, jl_cachearg_offset(mt), /*subtype*/1);
        if (entry)
            mi = entry->func.linfo;
    }

    if (!mi) {
        size_t min_valid = 0;
        size_t max_valid = ~(size_t)0;
        matc = _gf_invoke_lookup((jl_value_t*)tt, jl_nothing, world, &min_valid, &max_valid);
        if (matc) {
            jl_method_t *m = matc->method;
            jl_svec_t *env = matc->sparams;
            mi = cache_method(mt, &mt->cache, (jl_value_t*)mt, tt, m, world, min_valid, max_valid, env);
        }
    }
    JL_UNLOCK(&mt->writelock);
    JL_GC_POP();
    return mi;
}


struct matches_env {
    struct typemap_intersection_env match;
    jl_typemap_entry_t *newentry;
    jl_value_t *shadowed;
    jl_typemap_entry_t *replaced;
};

static int get_intersect_visitor(jl_typemap_entry_t *oldentry, struct typemap_intersection_env *closure0)
{
    struct matches_env *closure = container_of(closure0, struct matches_env, match);
    assert(oldentry != closure->newentry && "entry already added");
    assert(jl_atomic_load_relaxed(&oldentry->min_world) <= jl_atomic_load_relaxed(&closure->newentry->min_world) && "old method cannot be newer than new method");
    assert(jl_atomic_load_relaxed(&oldentry->max_world) != jl_atomic_load_relaxed(&closure->newentry->min_world) && "method cannot be added at the same time as method deleted");
    // don't need to consider other similar methods if this oldentry will always fully intersect with them and dominates all of them
    typemap_slurp_search(oldentry, &closure->match);
    jl_method_t *oldmethod = oldentry->func.method;
    if (closure->match.issubty // e.g. jl_subtype(closure->newentry.sig, oldentry->sig)
        && jl_subtype(oldmethod->sig, (jl_value_t*)closure->newentry->sig)) { // e.g. jl_type_equal(closure->newentry->sig, oldentry->sig)
        if (closure->replaced == NULL || jl_atomic_load_relaxed(&closure->replaced->min_world) < jl_atomic_load_relaxed(&oldentry->min_world))
            closure->replaced = oldentry; // must pick the newest insertion (both are still valid)
    }
    if (closure->shadowed == NULL)
        closure->shadowed = (jl_value_t*)jl_alloc_vec_any(0);
    jl_array_ptr_1d_push((jl_array_t*)closure->shadowed, (jl_value_t*)oldmethod);
    return 1;
}

static jl_value_t *get_intersect_matches(jl_typemap_t *defs, jl_typemap_entry_t *newentry, jl_typemap_entry_t **replaced, int8_t offs, size_t world)
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
    // search for all intersecting methods active in the previous world, to determine the changes needed to be made for the next world
    struct matches_env env = {{get_intersect_visitor, (jl_value_t*)type, va, /* .search_slurp = */ 0,
            /* .min_valid = */ world, /* .max_valid = */ world,
            /* .ti = */ NULL, /* .env = */ jl_emptysvec, /* .issubty = */ 0},
        /* .newentry = */ newentry, /* .shadowed */ NULL, /* .replaced */ NULL};
    JL_GC_PUSH3(&env.match.env, &env.match.ti, &env.shadowed);
    jl_typemap_intersection_visitor(defs, offs, &env.match);
    env.match.env = NULL;
    env.match.ti = NULL;
    *replaced = env.replaced;
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
    if (jl_generating_output() && jl_options.incremental) {
        jl_printf(JL_STDERR, "ERROR: Method overwriting is not permitted during Module precompilation. Use `__precompile__(false)` to opt-out of precompilation.\n");
        jl_throw(jl_precompilable_error);
    }
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

static void _invalidate_backedges(jl_method_instance_t *replaced_mi, jl_code_instance_t *replaced_ci, size_t max_world, int depth);

// recursively invalidate cached methods that had an edge to a replaced method
static void invalidate_code_instance(jl_code_instance_t *replaced, size_t max_world, int depth)
{
    jl_timing_counter_inc(JL_TIMING_COUNTER_Invalidations, 1);
    if (_jl_debug_method_invalidation) {
        jl_value_t *boxeddepth = NULL;
        JL_GC_PUSH1(&boxeddepth);
        jl_array_ptr_1d_push(_jl_debug_method_invalidation, (jl_value_t*)replaced->def);
        boxeddepth = jl_box_int32(depth);
        jl_array_ptr_1d_push(_jl_debug_method_invalidation, boxeddepth);
        JL_GC_POP();
    }
    //jl_static_show(JL_STDERR, (jl_value_t*)replaced->def);
    jl_method_instance_t *replaced_mi = jl_get_ci_mi(replaced);
    if (!jl_is_method(replaced_mi->def.method))
        return; // shouldn't happen, but better to be safe
    JL_LOCK(&replaced_mi->def.method->writelock);
    size_t replacedmaxworld = jl_atomic_load_relaxed(&replaced->max_world);
    if (replacedmaxworld == ~(size_t)0) {
        assert(jl_atomic_load_relaxed(&replaced->min_world) - 1 <= max_world && "attempting to set illogical world constraints (probable race condition)");
        jl_atomic_store_release(&replaced->max_world, max_world);
        // recurse to all backedges to update their valid range also
        _invalidate_backedges(replaced_mi, replaced, max_world, depth + 1);
    } else {
        assert(jl_atomic_load_relaxed(&replaced->max_world) <= max_world);
    }
    JL_UNLOCK(&replaced_mi->def.method->writelock);
}

JL_DLLEXPORT void jl_invalidate_code_instance(jl_code_instance_t *replaced, size_t max_world)
{
    invalidate_code_instance(replaced, max_world, 1);
}

static void _invalidate_backedges(jl_method_instance_t *replaced_mi, jl_code_instance_t *replaced_ci, size_t max_world, int depth) {
    uint8_t recursion_flags = 0;
    jl_array_t *backedges = jl_mi_get_backedges_mutate(replaced_mi, &recursion_flags);
    if (!backedges)
        return;
    // invalidate callers (if any)
    if (!replaced_ci) {
        // We know all backedges are deleted - clear them eagerly
        // Clears both array and flags
        replaced_mi->backedges = NULL;
        jl_atomic_fetch_and_relaxed(&replaced_mi->flags, ~MI_FLAG_BACKEDGES_ALL);
    }
    JL_GC_PUSH1(&backedges);
    size_t i = 0, l = jl_array_nrows(backedges);
    size_t ins = 0;
    jl_code_instance_t *replaced;
    while (i < l) {
        jl_value_t *invokesig = NULL;
        i = get_next_edge(backedges, i, &invokesig, &replaced);
        if (!replaced) {
            ins = i;
            continue;
        }
        JL_GC_PROMISE_ROOTED(replaced); // propagated by get_next_edge from backedges
        if (replaced_ci) {
            // If we're invalidating a particular codeinstance, only invalidate
            // this backedge it actually has an edge for our codeinstance.
            jl_svec_t *edges = jl_atomic_load_relaxed(&replaced->edges);
            for (size_t j = 0; j < jl_svec_len(edges); ++j) {
                jl_value_t *edge = jl_svecref(edges, j);
                if (edge == (jl_value_t*)replaced_mi || edge == (jl_value_t*)replaced_ci)
                    goto found;
            }
            ins = set_next_edge(backedges, ins, invokesig, replaced);
            continue;
        found:;
            ins = clear_next_edge(backedges, ins, invokesig, replaced);
            jl_atomic_fetch_or(&replaced_mi->flags, MI_FLAG_BACKEDGES_DIRTY);
            /* fallthrough */
        }
        invalidate_code_instance(replaced, max_world, depth);
        if (replaced_ci && !replaced_mi->backedges) {
            // Fast-path early out. If `invalidate_code_instance` invalidated
            // the entire mi via a recursive edge, there's no point to keep
            // iterating - they'll already have been invalidated.
            break;
        }
    }
    if (replaced_ci)
        jl_mi_done_backedges(replaced_mi, recursion_flags);
    JL_GC_POP();
}

enum morespec_options {
    morespec_unknown,
    morespec_isnot,
    morespec_is
};

// check if `type` is replacing `m` with an ambiguity here, given other methods in `d` that already match it
static int is_replacing(char ambig, jl_value_t *type, jl_method_t *m, jl_method_t *const *d, size_t n, jl_value_t *isect, jl_value_t *isect2, char *morespec)
{
    size_t k;
    for (k = 0; k < n; k++) {
        jl_method_t *m2 = d[k];
        // see if m2 also fully covered this intersection
        if (m == m2 || !(jl_subtype(isect, m2->sig) || (isect2 && jl_subtype(isect2, m2->sig))))
            continue;
        if (morespec[k] == (char)morespec_unknown)
            morespec[k] = (char)(jl_type_morespecific(m2->sig, type) ? morespec_is : morespec_isnot);
        if (morespec[k] == (char)morespec_is)
            // not actually shadowing this--m2 will still be better
            return 0;
        // if type is not more specific than m (thus now dominating it)
        // then there is a new ambiguity here,
        // since m2 was also a previous match over isect,
        // see if m was previously dominant over all m2
        // or if this was already ambiguous before
        if (ambig == morespec_is && !jl_type_morespecific(m->sig, m2->sig)) {
            // m and m2 were previously ambiguous over the full intersection of mi with type, and will still be ambiguous with addition of type
            return 0;
        }
    }
    return 1;
}

static int _invalidate_dispatch_backedges(jl_method_instance_t *mi, jl_value_t *type, jl_method_t *m,
        jl_method_t *const *d, size_t n, int replaced_dispatch, int ambig,
        size_t max_world, char *morespec)
{
    uint8_t backedge_recursion_flags = 0;
    jl_array_t *backedges = jl_mi_get_backedges_mutate(mi, &backedge_recursion_flags);
    if (!backedges)
        return 0;
    size_t ib = 0, insb = 0, nb = jl_array_nrows(backedges);
    jl_value_t *invokeTypes;
    jl_code_instance_t *caller;
    int invalidated_any = 0;
    while (mi->backedges && ib < nb) {
        ib = get_next_edge(backedges, ib, &invokeTypes, &caller);
        if (!caller) {
            insb = ib;
            continue;
        }
        JL_GC_PROMISE_ROOTED(caller); // propagated by get_next_edge from backedges
        int replaced_edge;
        if (invokeTypes) {
            // n.b. normally we must have mi.specTypes <: invokeTypes <: m.sig (though it might not strictly hold), so we only need to check the other subtypes
            if (jl_egal(invokeTypes, jl_get_ci_mi(caller)->def.method->sig))
                replaced_edge = 0; // if invokeTypes == m.sig, then the only way to change this invoke is to replace the method itself
            else
                replaced_edge = jl_subtype(invokeTypes, type) && is_replacing(ambig, type, m, d, n, invokeTypes, NULL, morespec);
        }
        else {
            replaced_edge = replaced_dispatch;
        }
        if (replaced_edge) {
            invalidate_code_instance(caller, max_world, 1);
            insb = clear_next_edge(backedges, insb, invokeTypes, caller);
            jl_atomic_fetch_or(&mi->flags, MI_FLAG_BACKEDGES_DIRTY);
            invalidated_any = 1;
        }
        else {
            insb = set_next_edge(backedges, insb, invokeTypes, caller);
        }
    }
    jl_mi_done_backedges(mi, backedge_recursion_flags);
    return invalidated_any;
}

// invalidate cached methods that overlap this definition
static void invalidate_backedges(jl_method_instance_t *replaced_mi, size_t max_world, const char *why)
{
    JL_LOCK(&replaced_mi->def.method->writelock);
    _invalidate_backedges(replaced_mi, NULL, max_world, 1);
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
JL_DLLEXPORT void jl_method_instance_add_backedge(jl_method_instance_t *callee, jl_value_t *invokesig, jl_code_instance_t *caller)
{
    if (!jl_atomic_load_relaxed(&allow_new_worlds))
        return;
    if (invokesig == jl_nothing)
        invokesig = NULL;      // julia uses `nothing` but C uses NULL (#undef)
    assert(jl_is_method_instance(callee));
    assert(jl_is_code_instance(caller));
    assert(invokesig == NULL || jl_is_type(invokesig));
    JL_LOCK(&callee->def.method->writelock);
    if (jl_atomic_load_relaxed(&allow_new_worlds)) {
        int found = 0;
        jl_array_t *backedges = jl_mi_get_backedges(callee);
        // TODO: use jl_cache_type_(invokesig) like cache_method does to save memory
        if (!backedges) {
            // lazy-init the backedges array
            backedges = jl_alloc_vec_any(0);
            callee->backedges = backedges;
            jl_gc_wb(callee, backedges);
        }
        else {
            size_t i = 0, l = jl_array_nrows(backedges);
            for (i = 0; i < l; i++) {
                // optimized version of while (i < l) i = get_next_edge(callee->backedges, i, &invokeTypes, &mi);
                jl_value_t *ciedge = jl_array_ptr_ref(backedges, i);
                if (ciedge != (jl_value_t*)caller)
                    continue;
                jl_value_t *invokeTypes = i > 0 ? jl_array_ptr_ref(backedges, i - 1) : NULL;
                if (invokeTypes && jl_is_method_instance(invokeTypes))
                    invokeTypes = NULL;
                if ((invokesig == NULL && invokeTypes == NULL) ||
                    (invokesig && invokeTypes && jl_types_equal(invokesig, invokeTypes))) {
                    found = 1;
                    break;
                }
            }
        }
        if (!found)
            push_edge(backedges, invokesig, caller);
    }
    JL_UNLOCK(&callee->def.method->writelock);
}

// add a backedge from a non-existent signature to caller
JL_DLLEXPORT void jl_method_table_add_backedge(jl_methtable_t *mt, jl_value_t *typ, jl_code_instance_t *caller)
{
    assert(jl_is_code_instance(caller));
    if (!jl_atomic_load_relaxed(&allow_new_worlds))
        return;
    JL_LOCK(&mt->writelock);
    if (jl_atomic_load_relaxed(&allow_new_worlds)) {
        if (!mt->backedges) {
            // lazy-init the backedges array
            mt->backedges = jl_alloc_vec_any(2);
            jl_gc_wb(mt, mt->backedges);
            jl_array_ptr_set(mt->backedges, 0, typ);
            jl_array_ptr_set(mt->backedges, 1, caller);
        }
        else {
            // check if the edge is already present and avoid adding a duplicate
            size_t i, l = jl_array_nrows(mt->backedges);
            for (i = 1; i < l; i += 2) {
                if (jl_array_ptr_ref(mt->backedges, i) == (jl_value_t*)caller) {
                    if (jl_types_equal(jl_array_ptr_ref(mt->backedges, i - 1), typ)) {
                        JL_UNLOCK(&mt->writelock);
                        return;
                    }
                }
            }
            // reuse an already cached instance of this type, if possible
            // TODO: use jl_cache_type_(tt) like cache_method does, instead of this linear scan?
            for (i = 1; i < l; i += 2) {
                if (jl_array_ptr_ref(mt->backedges, i) != (jl_value_t*)caller) {
                    if (jl_types_equal(jl_array_ptr_ref(mt->backedges, i - 1), typ)) {
                        typ = jl_array_ptr_ref(mt->backedges, i - 1);
                        break;
                    }
                }
            }
            jl_array_ptr_1d_push(mt->backedges, typ);
            jl_array_ptr_1d_push(mt->backedges, (jl_value_t*)caller);
        }
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
    if (jl_atomic_load_relaxed(&oldentry->max_world) == ~(size_t)0) {
        jl_method_instance_t *mi = oldentry->func.linfo;
        int intersects = 0;
        jl_method_instance_t **d = (jl_method_instance_t**)jl_array_ptr_data(env->shadowed);
        size_t i, n = jl_array_nrows(env->shadowed);
        for (i = 0; i < n; i++) {
            if (mi == d[i]) {
                intersects = 1;
                break;
            }
        }
        if (intersects && (jl_value_t*)oldentry->sig != mi->specTypes) {
            // the entry may point to a widened MethodInstance, in which case it is worthwhile to check if the new method
            // actually has any meaningful intersection with the old one
            intersects = !jl_has_empty_intersection((jl_value_t*)oldentry->sig, (jl_value_t*)env->newentry->sig);
        }
        if (intersects && oldentry->guardsigs != jl_emptysvec) {
            // similarly, if it already matches an existing guardsigs, this is already safe to keep
            size_t i, l;
            for (i = 0, l = jl_svec_len(oldentry->guardsigs); i < l; i++) {
                // see corresponding code in jl_typemap_entry_assoc_exact
                if (jl_subtype((jl_value_t*)env->newentry->sig, jl_svecref(oldentry->guardsigs, i))) {
                    intersects = 0;
                    break;
                }
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
            jl_atomic_store_relaxed(&oldentry->max_world, env->max_world);
            env->invalidated = 1;
        }
    }
    return 1;
}

struct disable_mt_env {
    jl_method_t *replaced;
    size_t max_world;
};
static int disable_mt_cache(jl_typemap_entry_t *oldentry, void *closure0)
{
    struct disable_mt_env *env = (struct disable_mt_env*)closure0;
    if (jl_atomic_load_relaxed(&oldentry->max_world) < ~(size_t)0)
        return 1;
    jl_method_t *m = oldentry->func.linfo->def.method;
    if (m == env->replaced)
        jl_atomic_store_relaxed(&oldentry->max_world, env->max_world);
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

static jl_typemap_entry_t *do_typemap_search(jl_methtable_t *mt JL_PROPAGATES_ROOT, jl_method_t *method) {
    jl_value_t *closure = (jl_value_t*)(method);
    if (jl_typemap_visitor(jl_atomic_load_relaxed(&mt->defs), typemap_search, &closure))
        jl_error("method not in method table");
    return (jl_typemap_entry_t *)closure;
}

static void jl_method_table_invalidate(jl_methtable_t *mt, jl_method_t *replaced, size_t max_world)
{
    if (jl_options.incremental && jl_generating_output())
        jl_error("Method deletion is not possible during Module precompile.");
    assert(!replaced->is_for_opaque_closure);
    assert(jl_atomic_load_relaxed(&jl_world_counter) == max_world);
    // drop this method from mt->cache
    struct disable_mt_env mt_cache_env;
    mt_cache_env.max_world = max_world;
    mt_cache_env.replaced = replaced;
    jl_typemap_visitor(jl_atomic_load_relaxed(&mt->cache), disable_mt_cache, (void*)&mt_cache_env);
    jl_genericmemory_t *leafcache = jl_atomic_load_relaxed(&mt->leafcache);
    size_t i, l = leafcache->length;
    for (i = 1; i < l; i += 2) {
        jl_typemap_entry_t *oldentry = (jl_typemap_entry_t*)jl_genericmemory_ptr_ref(leafcache, i);
        if (oldentry) {
            while ((jl_value_t*)oldentry != jl_nothing) {
                disable_mt_cache(oldentry, (void*)&mt_cache_env);
                oldentry = jl_atomic_load_relaxed(&oldentry->next);
            }
        }
    }
    // Invalidate the backedges
    int invalidated = 0;
    jl_value_t *specializations = jl_atomic_load_relaxed(&replaced->specializations);
    JL_GC_PUSH1(&specializations);
    if (!jl_is_svec(specializations))
        specializations = (jl_value_t*)jl_svec1(specializations);
    l = jl_svec_len(specializations);
    for (i = 0; i < l; i++) {
        jl_method_instance_t *mi = (jl_method_instance_t*)jl_svecref(specializations, i);
        if ((jl_value_t*)mi != jl_nothing) {
            invalidated = 1;
            invalidate_backedges(mi, max_world, "jl_method_table_disable");
        }
    }
    JL_GC_POP();
    // XXX: this might have resolved an ambiguity, for which we have not tracked the edge here,
    // and thus now introduce a mistake into inference
    if (invalidated && _jl_debug_method_invalidation) {
        jl_array_ptr_1d_push(_jl_debug_method_invalidation, (jl_value_t*)replaced);
        jl_value_t *loctag = jl_cstr_to_string("jl_method_table_disable");
        JL_GC_PUSH1(&loctag);
        jl_array_ptr_1d_push(_jl_debug_method_invalidation, loctag);
        JL_GC_POP();
    }
}

static int erase_method_backedges(jl_typemap_entry_t *def, void *closure)
{
    jl_method_t *method = def->func.method;
    JL_LOCK(&method->writelock);
    jl_value_t *specializations = jl_atomic_load_relaxed(&method->specializations);
    if (jl_is_svec(specializations)) {
        size_t i, l = jl_svec_len(specializations);
        for (i = 0; i < l; i++) {
            jl_method_instance_t *mi = (jl_method_instance_t*)jl_svecref(specializations, i);
            if ((jl_value_t*)mi != jl_nothing) {
                mi->backedges = 0;
            }
        }
    }
    else {
        jl_method_instance_t *mi = (jl_method_instance_t*)specializations;
        mi->backedges = 0;
    }
    JL_UNLOCK(&method->writelock);
    return 1;
}

static int erase_all_backedges(jl_methtable_t *mt, void *env)
{
    // removes all method caches
    // this might not be entirely safe (GC or MT), thus we only do it very early in bootstrapping
    JL_LOCK(&mt->writelock);
    mt->backedges = NULL;
    JL_UNLOCK(&mt->writelock);
    jl_typemap_visitor(jl_atomic_load_relaxed(&mt->defs), erase_method_backedges, env);
    return 1;
}

JL_DLLEXPORT void jl_disable_new_worlds(void)
{
    if (jl_generating_output())
        jl_error("Disabling Method changes is not possible when generating output.");
    JL_LOCK(&world_counter_lock);
    jl_atomic_store_relaxed(&allow_new_worlds, 0);
    JL_UNLOCK(&world_counter_lock);
    jl_foreach_reachable_mtable(erase_all_backedges, (void*)NULL);
}

JL_DLLEXPORT void jl_method_table_disable(jl_methtable_t *mt, jl_method_t *method)
{
    jl_typemap_entry_t *methodentry = do_typemap_search(mt, method);
    JL_LOCK(&world_counter_lock);
    if (!jl_atomic_load_relaxed(&allow_new_worlds))
        jl_error("Method changes have been disabled via a call to disable_new_worlds.");
    int enabled = jl_atomic_load_relaxed(&methodentry->max_world) == ~(size_t)0;
    if (enabled) {
        JL_LOCK(&mt->writelock);
        // Narrow the world age on the method to make it uncallable
        size_t world = jl_atomic_load_relaxed(&jl_world_counter);
        assert(method == methodentry->func.method);
        jl_atomic_store_relaxed(&method->dispatch_status, 0);
        assert(jl_atomic_load_relaxed(&methodentry->max_world) == ~(size_t)0);
        jl_atomic_store_relaxed(&methodentry->max_world, world);
        jl_method_table_invalidate(mt, method, world);
        jl_atomic_store_release(&jl_world_counter, world + 1);
        JL_UNLOCK(&mt->writelock);
    }
    JL_UNLOCK(&world_counter_lock);
    if (!enabled)
        jl_errorf("Method of %s already disabled", jl_symbol_name(method->name));
}

static int jl_type_intersection2(jl_value_t *t1, jl_value_t *t2, jl_value_t **isect JL_REQUIRE_ROOTED_SLOT, jl_value_t **isect2 JL_REQUIRE_ROOTED_SLOT)
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

jl_typemap_entry_t *jl_method_table_add(jl_methtable_t *mt, jl_method_t *method, jl_tupletype_t *simpletype)
{
    JL_TIMING(ADD_METHOD, ADD_METHOD);
    assert(jl_is_method(method));
    assert(jl_is_mtable(mt));
    jl_timing_show_method(method, JL_TIMING_DEFAULT_BLOCK);
    jl_typemap_entry_t *newentry = NULL;
    JL_GC_PUSH1(&newentry);
    JL_LOCK(&mt->writelock);
    // add our new entry
    assert(jl_atomic_load_relaxed(&method->primary_world) == ~(size_t)0); // min-world
    assert((jl_atomic_load_relaxed(&method->dispatch_status) & METHOD_SIG_LATEST_WHICH) == 0);
    assert((jl_atomic_load_relaxed(&method->dispatch_status) & METHOD_SIG_LATEST_ONLY) == 0);
    newentry = jl_typemap_alloc((jl_tupletype_t*)method->sig, simpletype, jl_emptysvec, (jl_value_t*)method, ~(size_t)0, 1);
    jl_typemap_insert(&mt->defs, (jl_value_t*)mt, newentry, jl_cachearg_offset(mt));
    update_max_args(mt, method->sig);
    JL_UNLOCK(&mt->writelock);
    JL_GC_POP();
    return newentry;
}

void jl_method_table_activate(jl_methtable_t *mt, jl_typemap_entry_t *newentry)
{
    JL_TIMING(ADD_METHOD, ADD_METHOD);
    jl_method_t *method = newentry->func.method;
    assert(jl_is_mtable(mt));
    assert(jl_is_method(method));
    jl_timing_show_method(method, JL_TIMING_DEFAULT_BLOCK);
    jl_value_t *type = (jl_value_t*)newentry->sig;
    jl_value_t *oldvalue = NULL;
    jl_array_t *oldmi = NULL;
    JL_LOCK(&mt->writelock);
    size_t world = jl_atomic_load_relaxed(&method->primary_world);
    assert(world == jl_atomic_load_relaxed(&jl_world_counter) + 1); // min-world
    assert((jl_atomic_load_relaxed(&method->dispatch_status) & METHOD_SIG_LATEST_WHICH) == 0);
    assert((jl_atomic_load_relaxed(&method->dispatch_status) & METHOD_SIG_LATEST_ONLY) == 0);
    assert(jl_atomic_load_relaxed(&newentry->min_world) == ~(size_t)0);
    assert(jl_atomic_load_relaxed(&newentry->max_world) == 1);
    jl_atomic_store_relaxed(&newentry->min_world, world);
    jl_atomic_store_relaxed(&method->primary_world, world);
    size_t max_world = world - 1;
    jl_value_t *loctag = NULL;  // debug info for invalidation
    jl_value_t *isect = NULL;
    jl_value_t *isect2 = NULL;
    jl_value_t *isect3 = NULL;
    JL_GC_PUSH6(&oldvalue, &oldmi, &loctag, &isect, &isect2, &isect3);
    jl_typemap_entry_t *replaced = NULL;
    // then check what entries we replaced
    oldvalue = get_intersect_matches(jl_atomic_load_relaxed(&mt->defs), newentry, &replaced, jl_cachearg_offset(mt), max_world);
    int invalidated = 0;
    int only = !(jl_atomic_load_relaxed(&method->dispatch_status) & METHOD_SIG_PRECOMPILE_MANY); // will compute if this will be currently the only result that would returned from `ml_matches` given `sig`
    if (replaced) {
        oldvalue = (jl_value_t*)replaced;
        jl_method_t *m = replaced->func.method;
        invalidated = 1;
        method_overwrite(newentry, m);
        // this is an optimized version of below, given we know the type-intersection is exact
        jl_method_table_invalidate(mt, m, max_world);
        int m_dispatch = jl_atomic_load_relaxed(&m->dispatch_status);
        jl_atomic_store_relaxed(&m->dispatch_status, 0);
        only = m_dispatch & METHOD_SIG_LATEST_ONLY;
    }
    else {
        jl_method_t *const *d;
        size_t j, n;
        if (oldvalue == NULL) {
            d = NULL;
            n = 0;
        }
        else {
            assert(jl_is_array(oldvalue));
            d = (jl_method_t**)jl_array_ptr_data(oldvalue);
            n = jl_array_nrows(oldvalue);
        }
        if (mt->backedges) {
            jl_value_t **backedges = jl_array_ptr_data(mt->backedges);
            size_t i, na = jl_array_nrows(mt->backedges);
            size_t ins = 0;
            for (i = 1; i < na; i += 2) {
                jl_value_t *backedgetyp = backedges[i - 1];
                JL_GC_PROMISE_ROOTED(backedgetyp);
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
                    // c.f. `is_replacing`, which is a similar query, but with an existing method match to compare against
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
                    jl_code_instance_t *backedge = (jl_code_instance_t*)backedges[i];
                    JL_GC_PROMISE_ROOTED(backedge);
                    invalidate_code_instance(backedge, max_world, 0);
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
            char *morespec = (char*)alloca(n);
            memset(morespec, morespec_unknown, n);
            for (j = 0; j < n; j++) {
                jl_method_t *m = d[j];
                if (morespec[j] == (char)morespec_is) {
                    only = 0;
                    continue;
                }
                loctag = jl_atomic_load_relaxed(&m->specializations); // use loctag for a gcroot
                _Atomic(jl_method_instance_t*) *data;
                size_t l;
                if (jl_is_svec(loctag)) {
                    data = (_Atomic(jl_method_instance_t*)*)jl_svec_data(loctag);
                    l = jl_svec_len(loctag);
                }
                else {
                    data = (_Atomic(jl_method_instance_t*)*) &loctag;
                    l = 1;
                }
                enum morespec_options ambig = morespec_unknown;
                for (size_t i = 0; i < l; i++) {
                    jl_method_instance_t *mi = jl_atomic_load_relaxed(&data[i]);
                    if ((jl_value_t*)mi == jl_nothing)
                        continue;
                    isect3 = jl_type_intersection(m->sig, (jl_value_t*)mi->specTypes);
                    if (jl_type_intersection2(type, isect3, &isect, &isect2)) {
                        // TODO: this only checks pair-wise for ambiguities, but the ambiguities could arise from the interaction of multiple methods
                        // and thus might miss a case where we introduce an ambiguity between two existing methods
                        // We could instead work to sort this into 3 groups `morespecific .. ambiguous .. lesspecific`, with `type` in ambiguous,
                        // such that everything in `morespecific` dominates everything in `ambiguous`, and everything in `ambiguous` dominates everything in `lessspecific`
                        // And then compute where each isect falls, and whether it changed group--necessitating invalidation--or not.
                        if (morespec[j] == (char)morespec_unknown)
                            morespec[j] = (char)(jl_type_morespecific(m->sig, type) ? morespec_is : morespec_isnot);
                        if (morespec[j] == (char)morespec_is)
                            // not actually shadowing--the existing method is still better
                            break;
                        if (ambig == morespec_unknown)
                            ambig = jl_type_morespecific(type, m->sig) ? morespec_isnot : morespec_is;
                        // replacing a method--see if this really was the selected method previously
                        // over the intersection (not ambiguous) and the new method will be selected now (morespec_is)
                        int replaced_dispatch = is_replacing(ambig, type, m, d, n, isect, isect2, morespec);
                        // found that this specialization dispatch got replaced by m
                        // call invalidate_backedges(mi, max_world, "jl_method_table_insert");
                        // but ignore invoke-type edges
                        int invalidatedmi = _invalidate_dispatch_backedges(mi, type, m, d, n, replaced_dispatch, ambig, max_world, morespec);
                        jl_array_ptr_1d_push(oldmi, (jl_value_t*)mi);
                        if (_jl_debug_method_invalidation && invalidatedmi) {
                            jl_array_ptr_1d_push(_jl_debug_method_invalidation, (jl_value_t*)mi);
                            loctag = jl_cstr_to_string("jl_method_table_insert");
                            jl_array_ptr_1d_push(_jl_debug_method_invalidation, loctag);
                        }
                        invalidated |= invalidatedmi;
                    }
                }
                // now compute and store updates to METHOD_SIG_LATEST_ONLY
                int m_dispatch = jl_atomic_load_relaxed(&m->dispatch_status);
                if (m_dispatch & METHOD_SIG_LATEST_ONLY) {
                    if (morespec[j] == (char)morespec_unknown)
                        morespec[j] = (char)(jl_type_morespecific(m->sig, type) ? morespec_is : morespec_isnot);
                    if (morespec[j] == (char)morespec_isnot)
                        jl_atomic_store_relaxed(&m->dispatch_status, ~METHOD_SIG_LATEST_ONLY & m_dispatch);
                }
                if (only) {
                    if (morespec[j] == (char)morespec_is || ambig == morespec_is ||
                        (ambig == morespec_unknown && !jl_type_morespecific(type, m->sig))) {
                        only = 0;
                    }
                }
            }
            if (jl_array_nrows(oldmi)) {
                // search mt->cache and leafcache and drop anything that might overlap with the new method
                // this is very cheap, so we don't mind being fairly conservative at over-approximating this
                struct invalidate_mt_env mt_cache_env;
                mt_cache_env.max_world = max_world;
                mt_cache_env.shadowed = oldmi;
                mt_cache_env.newentry = newentry;
                mt_cache_env.invalidated = 0;

                jl_typemap_visitor(jl_atomic_load_relaxed(&mt->cache), invalidate_mt_cache, (void*)&mt_cache_env);
                jl_genericmemory_t *leafcache = jl_atomic_load_relaxed(&mt->leafcache);
                size_t i, l = leafcache->length;
                for (i = 1; i < l; i += 2) {
                    jl_value_t *entry = jl_genericmemory_ptr_ref(leafcache, i);
                    if (entry) {
                        while (entry != jl_nothing) {
                            invalidate_mt_cache((jl_typemap_entry_t*)entry, (void*)&mt_cache_env);
                            entry = (jl_value_t*)jl_atomic_load_relaxed(&((jl_typemap_entry_t*)entry)->next);
                        }
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
    jl_atomic_store_relaxed(&newentry->max_world, ~(size_t)0);
    jl_atomic_store_relaxed(&method->dispatch_status, METHOD_SIG_LATEST_WHICH | (only ? METHOD_SIG_LATEST_ONLY : 0)); // TODO: this should be sequenced fully after the world counter store
    JL_UNLOCK(&mt->writelock);
    JL_GC_POP();
}

JL_DLLEXPORT void jl_method_table_insert(jl_methtable_t *mt, jl_method_t *method, jl_tupletype_t *simpletype)
{
    jl_typemap_entry_t *newentry = jl_method_table_add(mt, method, simpletype);
    JL_GC_PUSH1(&newentry);
    JL_LOCK(&world_counter_lock);
    if (!jl_atomic_load_relaxed(&allow_new_worlds))
        jl_error("Method changes have been disabled via a call to disable_new_worlds.");
    size_t world = jl_atomic_load_relaxed(&jl_world_counter) + 1;
    jl_atomic_store_relaxed(&method->primary_world, world);
    jl_method_table_activate(mt, newentry);
    jl_atomic_store_release(&jl_world_counter, world);
    JL_UNLOCK(&world_counter_lock);
    JL_GC_POP();
}

static void JL_NORETURN jl_method_error_bare(jl_value_t *f, jl_value_t *args, size_t world)
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

void JL_NORETURN jl_method_error(jl_value_t *f, jl_value_t **args, size_t na, size_t world)
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

JL_DLLEXPORT jl_value_t *jl_method_lookup_by_tt(jl_tupletype_t *tt, size_t world, jl_value_t *_mt)
{
    jl_methtable_t *mt = NULL;
    if (_mt == jl_nothing)
        mt = jl_gf_ft_mtable(jl_tparam0(tt));
    else {
        assert(jl_isa(_mt, (jl_value_t*)jl_methtable_type));
        mt = (jl_methtable_t*) _mt;
    }
    jl_method_instance_t* mi = jl_mt_assoc_by_type(mt, tt, world);
    if (!mi)
        return jl_nothing;
    return (jl_value_t*) mi;
}

JL_DLLEXPORT jl_method_instance_t *jl_method_lookup(jl_value_t **args, size_t nargs, size_t world)
{
    assert(nargs > 0 && "expected caller to handle this case");
    jl_methtable_t *mt = jl_gf_mtable(args[0]);
    jl_typemap_t *cache = jl_atomic_load_relaxed(&mt->cache); // XXX: gc root for this?
    jl_typemap_entry_t *entry = jl_typemap_assoc_exact(cache, args[0], &args[1], nargs, jl_cachearg_offset(mt), world);
    if (entry)
        return entry->func.linfo;
    jl_tupletype_t *tt = arg_type_tuple(args[0], &args[1], nargs);
    return jl_mt_assoc_by_type(mt, tt, world);
}

// return a Vector{Any} of svecs, each describing a method match:
// Any[svec(tt, spvals, m, full), ...]
// tt is the intersection of the type argument and the method signature,
// spvals is any matched static parameter values, m is the Method,
// full is a boolean indicating if that method fully covers the input
//
// lim is the max # of methods to return. if there are more, returns jl_nothing.
// Negative values stand for no limit.
// Unless lim == -1, remove matches that are unambiguously covered by earlier ones
JL_DLLEXPORT jl_value_t *jl_matching_methods(jl_tupletype_t *types, jl_value_t *mt, int lim, int include_ambiguous,
                                             size_t world, size_t *min_valid, size_t *max_valid, int *ambig)
{
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

JL_DLLEXPORT jl_method_instance_t *jl_get_unspecialized(jl_method_t *def JL_PROPAGATES_ROOT)
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

STATIC_INLINE jl_value_t *_jl_rettype_inferred(jl_value_t *owner, jl_method_instance_t *mi, size_t min_world, size_t max_world) JL_NOTSAFEPOINT
{
    jl_code_instance_t *codeinst = jl_atomic_load_relaxed(&mi->cache);
    while (codeinst) {
        if (jl_atomic_load_relaxed(&codeinst->min_world) <= min_world &&
            max_world <= jl_atomic_load_relaxed(&codeinst->max_world) &&
            jl_egal(codeinst->owner, owner)) {

            jl_value_t *code = jl_atomic_load_relaxed(&codeinst->inferred);
            if (code)
                return (jl_value_t*)codeinst;
        }
        codeinst = jl_atomic_load_relaxed(&codeinst->next);
    }
    return (jl_value_t*)jl_nothing;
}

JL_DLLEXPORT jl_value_t *jl_rettype_inferred(jl_value_t *owner, jl_method_instance_t *mi, size_t min_world, size_t max_world) JL_NOTSAFEPOINT
{
    return (jl_value_t*)_jl_rettype_inferred(owner, mi, min_world, max_world);
}

JL_DLLEXPORT jl_value_t *jl_rettype_inferred_native(jl_method_instance_t *mi, size_t min_world, size_t max_world) JL_NOTSAFEPOINT
{
    return (jl_value_t*)_jl_rettype_inferred(jl_nothing, mi, min_world, max_world);
}

JL_DLLEXPORT jl_value_t *(*const jl_rettype_inferred_addr)(jl_method_instance_t *mi, size_t min_world, size_t max_world) JL_NOTSAFEPOINT = jl_rettype_inferred_native;

jl_code_instance_t *jl_method_compiled(jl_method_instance_t *mi, size_t world)
{
    jl_code_instance_t *codeinst = jl_atomic_load_relaxed(&mi->cache);
    for (; codeinst; codeinst = jl_atomic_load_relaxed(&codeinst->next)) {
        if (codeinst->owner != jl_nothing)
            continue;
        if (jl_atomic_load_relaxed(&codeinst->min_world) <= world && world <= jl_atomic_load_relaxed(&codeinst->max_world)) {
            if (jl_atomic_load_relaxed(&codeinst->invoke) != NULL)
                return codeinst;
        }
    }
    return NULL;
}

jl_mutex_t precomp_statement_out_lock;

_Atomic(uint8_t) jl_force_trace_compile_timing_enabled = 0;

/**
 * @brief Enable force trace compile to stderr with timing.
 */
JL_DLLEXPORT void jl_force_trace_compile_timing_enable(void)
{
    // Increment the flag to allow reentrant callers to `@trace_compile`.
    jl_atomic_fetch_add(&jl_force_trace_compile_timing_enabled, 1);
}
/**
 * @brief Disable force trace compile to stderr with timing.
 */
JL_DLLEXPORT void jl_force_trace_compile_timing_disable(void)
{
    // Increment the flag to allow reentrant callers to `@trace_compile`.
    jl_atomic_fetch_add(&jl_force_trace_compile_timing_enabled, -1);
}

static void record_precompile_statement(jl_method_instance_t *mi, double compilation_time, int is_recompile)
{
    static ios_t f_precompile;
    static JL_STREAM* s_precompile = NULL;
    jl_method_t *def = mi->def.method;
    uint8_t force_trace_compile = jl_atomic_load_relaxed(&jl_force_trace_compile_timing_enabled);
    if (force_trace_compile == 0 && jl_options.trace_compile == NULL)
        return;
    if (!jl_is_method(def))
        return;
    if (def->is_for_opaque_closure)
        return; // OpaqueClosure methods cannot be looked up by their types, so are incompatible with `precompile(...)`

    JL_LOCK(&precomp_statement_out_lock);
    if (s_precompile == NULL) {
        const char *t = jl_options.trace_compile;
        if (force_trace_compile || !strncmp(t, "stderr", 6)) {
            s_precompile = JL_STDERR;
        }
        else {
            if (ios_file(&f_precompile, t, 1, 1, 1, 1) == NULL)
                jl_errorf("cannot open precompile statement file \"%s\" for writing", t);
            s_precompile = (JL_STREAM*) &f_precompile;
        }
    }
    if (!jl_has_free_typevars(mi->specTypes)) {
        if (is_recompile && s_precompile == JL_STDERR && jl_options.color != JL_OPTIONS_COLOR_OFF)
            jl_printf(s_precompile, "\e[33m");
        if (force_trace_compile || jl_options.trace_compile_timing)
            jl_printf(s_precompile, "#= %6.1f ms =# ", compilation_time / 1e6);
        jl_printf(s_precompile, "precompile(");
        jl_static_show(s_precompile, mi->specTypes);
        jl_printf(s_precompile, ")");
        if (is_recompile) {
            jl_printf(s_precompile, " # recompile");
            if (s_precompile == JL_STDERR && jl_options.color != JL_OPTIONS_COLOR_OFF) {
                jl_printf(s_precompile, "\e[0m");
            }
        }
        jl_printf(s_precompile, "\n");
        if (s_precompile != JL_STDERR)
            ios_flush(&f_precompile);
    }
    JL_UNLOCK(&precomp_statement_out_lock);
}

jl_mutex_t dispatch_statement_out_lock;

_Atomic(uint8_t) jl_force_trace_dispatch_enabled = 0;

/**
 * @brief Enable force trace dispatch to stderr.
 */
JL_DLLEXPORT void jl_force_trace_dispatch_enable(void)
{
    // Increment the flag to allow reentrant callers to `@trace_dispatch`.
    jl_atomic_fetch_add(&jl_force_trace_dispatch_enabled, 1);
}
/**
 * @brief Disable force trace dispatch to stderr.
 */
JL_DLLEXPORT void jl_force_trace_dispatch_disable(void)
{
    // Increment the flag to allow reentrant callers to `@trace_dispatch`.
    jl_atomic_fetch_add(&jl_force_trace_dispatch_enabled, -1);
}

static void record_dispatch_statement(jl_method_instance_t *mi)
{
    static ios_t f_dispatch;
    static JL_STREAM* s_dispatch = NULL;
    jl_method_t *def = mi->def.method;
    if (!jl_is_method(def))
        return;

    uint8_t force_trace_dispatch = jl_atomic_load_relaxed(&jl_force_trace_dispatch_enabled);
    JL_LOCK(&dispatch_statement_out_lock);
    if (s_dispatch == NULL) {
        const char *t = jl_options.trace_dispatch;
        if (force_trace_dispatch || !strncmp(t, "stderr", 6)) {
            s_dispatch = JL_STDERR;
        }
        else {
            if (ios_file(&f_dispatch, t, 1, 1, 1, 1) == NULL)
                jl_errorf("cannot open dispatch statement file \"%s\" for writing", t);
            s_dispatch = (JL_STREAM*) &f_dispatch;
        }
    }
    if (!jl_has_free_typevars(mi->specTypes)) {
        jl_printf(s_dispatch, "precompile(");
        jl_static_show(s_dispatch, mi->specTypes);
        jl_printf(s_dispatch, ")\n");
        if (s_dispatch != JL_STDERR)
            ios_flush(&f_dispatch);
    }
    JL_UNLOCK(&dispatch_statement_out_lock);
}

// If waitcompile is 0, this will return NULL if compiling is on-going in the JIT. This is
// useful for the JIT itself, since it just doesn't cause redundant work or missed updates,
// but merely causes it to look into the current JIT worklist.
void jl_read_codeinst_invoke(jl_code_instance_t *ci, uint8_t *specsigflags, jl_callptr_t *invoke, void **specptr, int waitcompile)
{
    uint8_t flags = jl_atomic_load_acquire(&ci->specsigflags); // happens-before for subsequent read of fptr
    while (1) {
        jl_callptr_t initial_invoke = jl_atomic_load_acquire(&ci->invoke); // happens-before for subsequent read of fptr
        if (initial_invoke == jl_fptr_wait_for_compiled_addr) {
            if (!waitcompile) {
                *invoke = NULL;
                *specptr = NULL;
                *specsigflags = 0b00;
                return;
            }
            jl_compile_codeinst(ci);
            initial_invoke = jl_atomic_load_acquire(&ci->invoke); // happens-before for subsequent read of fptr
        }
        void *fptr = jl_atomic_load_relaxed(&ci->specptr.fptr);
        // TODO: if fptr is NULL, it may mean we read this too fast, and should have spun and waited for jl_compile_codeinst to finish
        if (initial_invoke == NULL || fptr == NULL) {
            *invoke = initial_invoke;
            *specptr = NULL;
            *specsigflags = 0b00;
            return;
        }
        while (!(flags & 0b10)) {
            jl_cpu_pause();
            flags = jl_atomic_load_acquire(&ci->specsigflags);
        }
        jl_callptr_t final_invoke = jl_atomic_load_relaxed(&ci->invoke);
        if (final_invoke == initial_invoke) {
            *invoke = final_invoke;
            *specptr = fptr;
            *specsigflags = flags;
            return;
        }
    }
}

jl_method_instance_t *jl_normalize_to_compilable_mi(jl_method_instance_t *mi JL_PROPAGATES_ROOT);

JL_DLLEXPORT void jl_add_codeinst_to_cache(jl_code_instance_t *codeinst, jl_code_info_t *src)
{
    assert(jl_is_code_info(src));
    jl_method_instance_t *mi = jl_get_ci_mi(codeinst);
    if (jl_generating_output() && jl_is_method(mi->def.method) && jl_atomic_load_relaxed(&codeinst->inferred) == jl_nothing) {
        jl_value_t *compressed = jl_compress_ir(mi->def.method, src);
        // These should already be compatible (and should be an assert), but make sure of it anyways
        if (jl_is_svec(src->edges)) {
            jl_atomic_store_release(&codeinst->edges, (jl_svec_t*)src->edges);
            jl_gc_wb(codeinst, src->edges);
        }
        jl_atomic_store_release(&codeinst->debuginfo, src->debuginfo);
        jl_gc_wb(codeinst, src->debuginfo);
        jl_atomic_store_release(&codeinst->inferred, compressed);
        jl_gc_wb(codeinst, compressed);
    }
}


JL_DLLEXPORT void jl_add_codeinst_to_jit(jl_code_instance_t *codeinst, jl_code_info_t *src)
{
    assert(jl_is_code_info(src));
    jl_emit_codeinst_to_jit(codeinst, src);
    jl_add_codeinst_to_cache(codeinst, src);
}

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
                jl_atomic_load_relaxed(&codeinst2->min_world),
                jl_atomic_load_relaxed(&codeinst2->max_world),
                jl_atomic_load_relaxed(&codeinst2->debuginfo),
                jl_atomic_load_relaxed(&codeinst2->edges));
        if (jl_atomic_load_relaxed(&codeinst->invoke) == NULL) {
            codeinst->rettype_const = codeinst2->rettype_const;
            jl_gc_wb(codeinst, codeinst->rettype_const);
            uint8_t specsigflags;
            jl_callptr_t invoke;
            void *fptr;
            jl_read_codeinst_invoke(codeinst2, &specsigflags, &invoke, &fptr, 1);
            if (fptr != NULL) {
                void *prev_fptr = NULL;
                // see jitlayers.cpp for the ordering restrictions here
                if (jl_atomic_cmpswap_acqrel(&codeinst->specptr.fptr, &prev_fptr, fptr)) {
                    jl_atomic_store_relaxed(&codeinst->specsigflags, specsigflags & 0b1);
                    jl_atomic_store_release(&codeinst->invoke, invoke);
                    // unspec is probably not specsig, but might be using specptr
                    jl_atomic_store_release(&codeinst->specsigflags, specsigflags & ~0b1); // clear specsig flag
                }
                else {
                    // someone else already compiled it
                    while (!(jl_atomic_load_acquire(&codeinst->specsigflags) & 0b10)) {
                        jl_cpu_pause();
                    }
                    // codeinst is now set up fully, safe to return
                }
            }
            else {
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
        (jl_is_method(def) && def->source == jl_nothing)) {
        // copy fptr from the template method definition
        if (jl_is_method(def)) {
            jl_method_instance_t *unspecmi = jl_atomic_load_relaxed(&def->unspecialized);
            if (unspecmi) {
                jl_code_instance_t *unspec = jl_atomic_load_relaxed(&unspecmi->cache);
                if (unspec && jl_atomic_load_acquire(&unspec->invoke) != NULL) {
                    uint8_t specsigflags;
                    jl_callptr_t invoke;
                    void *fptr;
                    jl_read_codeinst_invoke(unspec, &specsigflags, &invoke, &fptr, 1);
                    jl_code_instance_t *codeinst = jl_new_codeinst(mi, jl_nothing,
                        (jl_value_t*)jl_any_type, (jl_value_t*)jl_any_type, NULL, NULL,
                        0, 1, ~(size_t)0, 0, jl_nothing, NULL, NULL);
                    codeinst->rettype_const = unspec->rettype_const;
                    jl_atomic_store_relaxed(&codeinst->specptr.fptr, fptr);
                    jl_atomic_store_relaxed(&codeinst->invoke, invoke);
                    // unspec is probably not specsig, but might be using specptr
                    jl_atomic_store_relaxed(&codeinst->specsigflags, specsigflags & ~0b1); // clear specsig flag
                    jl_mi_cache_insert(mi, codeinst);
                    record_precompile_statement(mi, 0, 0);
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
            jl_code_instance_t *codeinst = jl_new_codeinst(mi, jl_nothing,
                (jl_value_t*)jl_any_type, (jl_value_t*)jl_any_type, NULL, NULL,
                0, 1, ~(size_t)0, 0, jl_nothing, NULL, NULL);
            jl_atomic_store_release(&codeinst->invoke, jl_fptr_interpret_call);
            jl_mi_cache_insert(mi, codeinst);
            record_precompile_statement(mi, 0, 0);
            return codeinst;
        }
        if (compile_option == JL_OPTIONS_COMPILE_OFF) {
            jl_printf(JL_STDERR, "No compiled code available for ");
            jl_static_show(JL_STDERR, (jl_value_t*)mi);
            jl_printf(JL_STDERR, " : sysimg may not have been built with --compile=all\n");
        }
    }

    // Ok, compilation is enabled. We'll need to try to compile something (probably).

    // Everything from here on is considered (user facing) compile time
    uint64_t start = jl_typeinf_timing_begin();

    // Is a recompile if there is cached code, and it was compiled (not only inferred) before
    int is_recompile = 0;
    jl_code_instance_t *codeinst_old = jl_atomic_load_relaxed(&mi->cache);
    while (codeinst_old != NULL) {
        if (jl_atomic_load_relaxed(&codeinst_old->invoke) != NULL) {
            is_recompile = 1;
            break;
        }
        codeinst_old = jl_atomic_load_relaxed(&codeinst_old->next);
    }

    // jl_type_infer will internally do a cache lookup and jl_engine_reserve call
    // to synchronize this across threads
    if (!codeinst) {
        // Don't bother inferring toplevel thunks or macros - the performance cost of inference is likely
        // to significantly exceed the actual runtime.
        int should_skip_inference = !jl_is_method(mi->def.method) || jl_symbol_name(mi->def.method->name)[0] == '@';

        if (!should_skip_inference) {
            codeinst = jl_type_infer(mi, world, SOURCE_MODE_ABI);
        }
    }

    if (codeinst) {
        if (jl_is_compiled_codeinst(codeinst)) {
            jl_typeinf_timing_end(start, is_recompile);
            // Already compiled - e.g. constabi, or compiled by a different thread while we were waiting.
            return codeinst;
        }

        JL_GC_PUSH1(&codeinst);
        double compile_time = jl_hrtime();
        int did_compile = jl_compile_codeinst(codeinst);
        compile_time = jl_hrtime() - compile_time;

        if (jl_atomic_load_relaxed(&codeinst->invoke) == NULL) {
            // Something went wrong. Bail to the fallback path.
            codeinst = NULL;
        }
        else if (did_compile && codeinst->owner == jl_nothing) {
            record_precompile_statement(mi, compile_time, is_recompile);
        }
        JL_GC_POP();
    }
    if (!codeinst) {
        jl_method_instance_t *unspec = jl_get_unspecialized(def);
        if (unspec == NULL)
            unspec = mi;
        jl_code_instance_t *ucache = jl_get_method_inferred(unspec, (jl_value_t*)jl_any_type, 1, ~(size_t)0, NULL, NULL);
        // ask codegen to make the fptr for unspec
        jl_callptr_t ucache_invoke = jl_atomic_load_acquire(&ucache->invoke);
        if (ucache_invoke == NULL) {
            if ((!jl_is_method(def) || def->source == jl_nothing) &&
                !jl_cached_uninferred(jl_atomic_load_relaxed(&jl_get_ci_mi(ucache)->cache), world)) {
                jl_throw(jl_new_struct(jl_missingcodeerror_type, (jl_value_t*)mi));
            }
            jl_generate_fptr_for_unspecialized(ucache);
            ucache_invoke = jl_atomic_load_acquire(&ucache->invoke);
        }
        assert(ucache_invoke != NULL);
        if (ucache_invoke != jl_fptr_sparam &&
            ucache_invoke != jl_fptr_interpret_call) {
            // only these care about the exact specTypes, otherwise we can use it directly
            jl_typeinf_timing_end(start, is_recompile);
            return ucache;
        }
        uint8_t specsigflags;
        jl_callptr_t invoke;
        void *fptr;
        jl_read_codeinst_invoke(ucache, &specsigflags, &invoke, &fptr, 1);
        codeinst = jl_new_codeinst(mi, jl_nothing,
            (jl_value_t*)jl_any_type, (jl_value_t*)jl_any_type, NULL, NULL,
            0, 1, ~(size_t)0, 0, jl_nothing, NULL, NULL);
        codeinst->rettype_const = ucache->rettype_const;
        // unspec is always not specsig, but might use specptr
        jl_atomic_store_relaxed(&codeinst->specptr.fptr, fptr);
        jl_atomic_store_relaxed(&codeinst->invoke, invoke);
        jl_atomic_store_relaxed(&codeinst->specsigflags, specsigflags & ~0b1); // clear specsig flag
        jl_mi_cache_insert(mi, codeinst);
    }
    jl_atomic_store_relaxed(&codeinst->precompile, 1);
    jl_typeinf_timing_end(start, is_recompile);
    return codeinst;
}

JL_DLLEXPORT jl_value_t *jl_fptr_const_return(jl_value_t *f, jl_value_t **args, uint32_t nargs, jl_code_instance_t *m)
{
    return m->rettype_const;
}

JL_DLLEXPORT jl_value_t *jl_fptr_args(jl_value_t *f, jl_value_t **args, uint32_t nargs, jl_code_instance_t *m)
{
    jl_fptr_args_t invoke = jl_atomic_load_relaxed(&m->specptr.fptr1);
    assert(invoke && "Forgot to set specptr for jl_fptr_args!");
    return invoke(f, args, nargs);
}

JL_DLLEXPORT jl_value_t *jl_fptr_sparam(jl_value_t *f, jl_value_t **args, uint32_t nargs, jl_code_instance_t *m)
{
    jl_svec_t *sparams = jl_get_ci_mi(m)->sparam_vals;
    assert(sparams != jl_emptysvec);
    jl_fptr_sparam_t invoke = jl_atomic_load_relaxed(&m->specptr.fptr3);
    assert(invoke && "Forgot to set specptr for jl_fptr_sparam!");
    return invoke(f, args, nargs, sparams);
}

jl_value_t *jl_fptr_wait_for_compiled(jl_value_t *f, jl_value_t **args, uint32_t nargs, jl_code_instance_t *m)
{
    jl_callptr_t invoke = jl_atomic_load_acquire(&m->invoke);
    if (invoke == &jl_fptr_wait_for_compiled) {
        int64_t last_alloc = jl_options.malloc_log ? jl_gc_diff_total_bytes() : 0;
        int last_errno = errno;
#ifdef _OS_WINDOWS_
        DWORD last_error = GetLastError();
#endif
        jl_compile_codeinst(m);
#ifdef _OS_WINDOWS_
        SetLastError(last_error);
#endif
        errno = last_errno;
        if (jl_options.malloc_log)
            jl_gc_sync_total_bytes(last_alloc); // discard allocation count from compilation
        invoke = jl_atomic_load_acquire(&m->invoke);
    }
    return invoke(f, args, nargs, m);
}

// test whether codeinst->invoke is usable already without further compilation needed
JL_DLLEXPORT int jl_is_compiled_codeinst(jl_code_instance_t *codeinst)
{
    jl_callptr_t invoke = jl_atomic_load_relaxed(&codeinst->invoke);
    if (invoke == NULL || invoke == &jl_fptr_wait_for_compiled)
        return 0;
    return 1;
}

JL_DLLEXPORT const jl_callptr_t jl_fptr_args_addr = &jl_fptr_args;

JL_DLLEXPORT const jl_callptr_t jl_fptr_const_return_addr = &jl_fptr_const_return;

JL_DLLEXPORT const jl_callptr_t jl_fptr_sparam_addr = &jl_fptr_sparam;

JL_DLLEXPORT const jl_callptr_t jl_f_opaque_closure_call_addr = (jl_callptr_t)&jl_f_opaque_closure_call;

JL_DLLEXPORT const jl_callptr_t jl_fptr_wait_for_compiled_addr = &jl_fptr_wait_for_compiled;

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

JL_DLLEXPORT jl_value_t *jl_normalize_to_compilable_sig(jl_methtable_t *mt, jl_tupletype_t *ti, jl_svec_t *env, jl_method_t *m,
                                                        int return_if_compileable)
{
    jl_tupletype_t *tt = NULL;
    jl_svec_t *newparams = NULL;
    JL_GC_PUSH2(&tt, &newparams);
    jl_methtable_t *kwmt = mt == jl_kwcall_mt ? jl_kwmethod_table_for(m->sig) : mt;
    intptr_t max_varargs = get_max_varargs(m, kwmt, mt, NULL);
    jl_compilation_sig(ti, env, m, max_varargs, &newparams);
    int is_compileable = ((jl_datatype_t*)ti)->isdispatchtuple;
    if (newparams) {
        tt = (jl_datatype_t*)jl_apply_tuple_type(newparams, 1);
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
    return (!return_if_compileable || is_compileable) ? (jl_value_t*)tt : jl_nothing;
}

jl_method_instance_t *jl_normalize_to_compilable_mi(jl_method_instance_t *mi JL_PROPAGATES_ROOT)
{
    jl_method_t *def = mi->def.method;
    if (!jl_is_method(def) || !jl_is_datatype(mi->specTypes))
        return mi;
    jl_methtable_t *mt = jl_method_get_table(def);
    if ((jl_value_t*)mt == jl_nothing)
        return mi;
    jl_value_t *compilationsig = jl_normalize_to_compilable_sig(mt, (jl_datatype_t*)mi->specTypes, mi->sparam_vals, def, 1);
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
JL_DLLEXPORT jl_method_instance_t *jl_method_match_to_mi(jl_method_match_t *match, size_t world, size_t min_valid, size_t max_valid, int mt_cache)
{
    jl_method_t *m = match->method;
    JL_GC_PROMISE_ROOTED(m);
    jl_svec_t *env = match->sparams;
    jl_tupletype_t *ti = match->spec_types;
    jl_method_instance_t *mi = NULL;
    if (jl_is_datatype(ti)) {
        jl_methtable_t *mt = jl_method_get_table(m);
        assert(mt != NULL);
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
                jl_value_t *tt = jl_normalize_to_compilable_sig(mt, ti, env, m, 1);
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
// intersect types with the MT, and return a single compileable specialization that covers the intersection.
jl_method_instance_t *jl_get_specialization1(jl_tupletype_t *types, size_t world, int mt_cache)
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
    if (matches == jl_nothing || jl_array_nrows(matches) != 1 || ambig)
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
    size_t i, n = jl_array_nrows(matches);
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
                if (jl_method_morespecific(match1->method, match2->method)) {
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
    jl_value_t *codeinst = jl_rettype_inferred_native(mi, world, world);
    if (codeinst == jl_nothing) {
        (void)jl_type_infer(mi, world, SOURCE_MODE_NOT_REQUIRED);
        codeinst = jl_rettype_inferred_native(mi, world, world);
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
    if (jl_typeinf_func && jl_atomic_load_relaxed(&mi->def.method->primary_world) <= tworld) {
        // if it's part of the compiler, also attempt to compile for the compiler world too
        _generate_from_hint(mi, tworld);
    }
}

JL_DLLEXPORT void jl_compile_method_instance(jl_method_instance_t *mi, jl_tupletype_t *types, size_t world)
{
    size_t tworld = jl_typeinf_world;
    uint8_t miflags = jl_atomic_load_relaxed(&mi->flags) | JL_MI_FLAGS_MASK_PRECOMPILED;
    jl_atomic_store_relaxed(&mi->flags, miflags);
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
            miflags = jl_atomic_load_relaxed(&mi2->flags) | JL_MI_FLAGS_MASK_PRECOMPILED;
            jl_atomic_store_relaxed(&mi2->flags, miflags);
            if (jl_rettype_inferred_native(mi2, world, world) == jl_nothing)
                (void)jl_type_infer(mi2, world, SOURCE_MODE_NOT_REQUIRED);
            if (jl_typeinf_func && jl_atomic_load_relaxed(&mi->def.method->primary_world) <= tworld) {
                if (jl_rettype_inferred_native(mi2, tworld, tworld) == jl_nothing)
                    (void)jl_type_infer(mi2, tworld, SOURCE_MODE_NOT_REQUIRED);
            }
        }
    }
    else {
        // Otherwise (this branch), assuming we are at runtime (normal JIT) and
        // we should generate the native code immediately in preparation for use.
        (void)jl_compile_method_internal(mi, world);
    }
}

JL_DLLEXPORT void jl_compile_method_sig(jl_method_t *m, jl_value_t *types, jl_svec_t *env, size_t world)
{
    jl_method_instance_t *mi = jl_specializations_get_linfo(m, types, env);
    jl_compile_method_instance(mi, NULL, world);
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

JL_DLLEXPORT int jl_add_entrypoint(jl_tupletype_t *types)
{
    size_t world = jl_atomic_load_acquire(&jl_world_counter);
    size_t min_valid = 0;
    size_t max_valid = ~(size_t)0;
    jl_method_instance_t *mi = jl_get_compile_hint_specialization(types, world, &min_valid, &max_valid, 1);
    if (mi == NULL)
        return 0;
    JL_GC_PROMISE_ROOTED(mi);
    if (jl_generating_output() && jl_options.trim) {
        arraylist_push(jl_entrypoint_mis, mi);
    }
    return 1;
}

// add type of `f` to front of argument tuple type
jl_value_t *jl_argtype_with_function(jl_value_t *f, jl_value_t *types0)
{
    return jl_argtype_with_function_type(jl_is_type(f) ? (jl_value_t*)jl_wrap_Type(f) : jl_typeof(f), types0);
}

jl_value_t *jl_argtype_with_function_type(jl_value_t *ft JL_MAYBE_UNROOTED, jl_value_t *types0)
{
    jl_value_t *types = jl_unwrap_unionall(types0);
    size_t l = jl_nparams(types);
    jl_value_t *tt = NULL;
    JL_GC_PUSH2(&tt, &ft);
    tt = (jl_value_t*)jl_alloc_svec(1+l);
    jl_svecset(tt, 0, ft);
    for (size_t i = 0; i < l; i++)
        jl_svecset(tt, i+1, jl_tparam(types,i));
    tt = (jl_value_t*)jl_apply_tuple_type((jl_svec_t*)tt, 1);
    tt = jl_rewrap_unionall_(tt, types0);
    JL_GC_POP();
    return tt;
}

// undo jl_argtype_with_function transform
jl_value_t *jl_argtype_without_function(jl_value_t *ftypes)
{
    jl_value_t *types = jl_unwrap_unionall(ftypes);
    size_t l = jl_nparams(types);
    if (l == 1 && jl_is_vararg(jl_tparam0(types)))
        return ftypes;
    jl_value_t *tt = (jl_value_t*)jl_alloc_svec(l - 1);
    JL_GC_PUSH1(&tt);
    for (size_t i = 1; i < l; i++)
        jl_svecset(tt, i - 1, jl_tparam(types, i));
    tt = (jl_value_t*)jl_apply_tuple_type((jl_svec_t*)tt, 0);
    tt = jl_rewrap_unionall_(tt, types);
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
        if (jl_atomic_load_relaxed(&codeinst->min_world) <= world && world <= jl_atomic_load_relaxed(&codeinst->max_world)) {
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

JL_DLLEXPORT jl_value_t *jl_invoke_oc(jl_value_t *F, jl_value_t **args, uint32_t nargs, jl_method_instance_t *mfunc)
{
    jl_opaque_closure_t *oc = (jl_opaque_closure_t*)F;
    jl_task_t *ct = jl_current_task;
    size_t last_age = ct->world_age;
    size_t world = oc->world;
    ct->world_age = world;
    jl_value_t *ret = _jl_invoke(F, args, nargs, mfunc, world);
    ct->world_age = last_age;
    return ret;
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
                world >= jl_atomic_load_relaxed(&entry->min_world) && world <= jl_atomic_load_relaxed(&entry->max_world)) { \
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
    int64_t last_alloc = 0;
    if (i == 4) {
        // if no method was found in the associative cache, check the full cache
        JL_TIMING(METHOD_LOOKUP_FAST, METHOD_LOOKUP_FAST);
        mt = jl_gf_mtable(F);
        jl_genericmemory_t *leafcache = jl_atomic_load_relaxed(&mt->leafcache);
        entry = NULL;
        int cache_entry_count = jl_atomic_load_relaxed(&((jl_datatype_t*)FT)->name->cache_entry_count);
        if (leafcache != (jl_genericmemory_t*)jl_an_empty_memory_any && (cache_entry_count == 0 || cache_entry_count >= 8)) {
            // hashing args is expensive, but so do that only if looking at mc->cache is probably even more expensive
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
        assert(tt);
        // cache miss case
        mfunc = jl_mt_assoc_by_type(mt, tt, world);
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
        // mfunc is about to be dispatched
        uint8_t force_trace_dispatch = jl_atomic_load_relaxed(&jl_force_trace_dispatch_enabled);
        if (force_trace_dispatch || jl_options.trace_dispatch != NULL) {
            uint8_t miflags = jl_atomic_load_relaxed(&mfunc->flags);
            uint8_t was_dispatched = miflags & JL_MI_FLAGS_MASK_DISPATCHED;
            if (!was_dispatched) {
                miflags |= JL_MI_FLAGS_MASK_DISPATCHED;
                jl_atomic_store_relaxed(&mfunc->flags, miflags);
                record_dispatch_statement(mfunc);
            }
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
    if (matches == jl_nothing || jl_array_nrows(matches) != 1)
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
    types = jl_argtype_with_function((jl_value_t*)gf, types0);
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
    uint8_t force_trace_dispatch = jl_atomic_load_relaxed(&jl_force_trace_dispatch_enabled);
    if (force_trace_dispatch || jl_options.trace_dispatch != NULL) {
        uint8_t miflags = jl_atomic_load_relaxed(&mfunc->flags);
        uint8_t was_dispatched = miflags & JL_MI_FLAGS_MASK_DISPATCHED;
        if (!was_dispatched) {
            miflags |= JL_MI_FLAGS_MASK_DISPATCHED;
            jl_atomic_store_relaxed(&mfunc->flags, miflags);
            record_dispatch_statement(mfunc);
        }
    }
    size_t world = jl_current_task->world_age;
    return _jl_invoke(gf, args, nargs - 1, mfunc, world);
}

jl_sym_t *jl_gf_supertype_name(jl_sym_t *name)
{
    size_t l = strlen(jl_symbol_name(name));
    char *prefixed;
    prefixed = (char*)malloc_s(l+2);
    prefixed[0] = '#';
    strcpy(&prefixed[1], jl_symbol_name(name));
    jl_sym_t *tname = jl_symbol(prefixed);
    free(prefixed);
    return tname;
}

// Return value is rooted globally
jl_function_t *jl_new_generic_function_with_supertype(jl_sym_t *name, jl_module_t *module, jl_datatype_t *st, size_t new_world)
{
    // type name is function name prefixed with #
    jl_sym_t *tname = jl_gf_supertype_name(name);
    jl_datatype_t *ftype = (jl_datatype_t*)jl_new_datatype(
            tname, module, st, jl_emptysvec, jl_emptysvec, jl_emptysvec, jl_emptysvec,
            0, 0, 0);
    assert(jl_is_datatype(ftype));
    JL_GC_PUSH1(&ftype);
    ftype->name->mt->name = name;
    jl_gc_wb(ftype->name->mt, name);
    jl_declare_constant_val3(NULL, module, tname, (jl_value_t*)ftype, PARTITION_KIND_CONST, new_world);
    jl_value_t *f = jl_new_struct(ftype);
    ftype->instance = f;
    jl_gc_wb(ftype, f);
    JL_GC_POP();
    return (jl_function_t*)f;
}

jl_function_t *jl_new_generic_function(jl_sym_t *name, jl_module_t *module, size_t new_world)
{
    return jl_new_generic_function_with_supertype(name, module, jl_function_type, new_world);
}

struct ml_matches_env {
    // inputs:
    struct typemap_intersection_env match;
    int intersections;
    size_t world;
    int lim;
    int include_ambiguous;
    // results:
    jl_value_t *t; // array of method matches
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
    size_t min_world = jl_atomic_load_relaxed(&ml->min_world);
    size_t max_world = jl_atomic_load_relaxed(&ml->max_world);
    if (closure->world < min_world) {
        // ignore method table entries that are part of a later world
        if (closure->match.max_valid >= min_world)
            closure->match.max_valid = min_world - 1;
        return 1;
    }
    else if (closure->world > max_world) {
        // ignore method table entries that have been replaced in the current world
        if (closure->match.min_valid <= max_world)
            closure->match.min_valid = max_world + 1;
        return 1;
    }
    if (closure->match.max_valid > max_world)
        closure->match.max_valid = max_world;
    jl_method_t *meth = ml->func.method;
    if (closure->lim >= 0 && jl_is_dispatch_tupletype(meth->sig)) {
        int replaced = 0;
        // check if this is replaced, in which case we need to avoid double-counting it against the limit
        // (although it will figure out later which one to keep and return)
        size_t len = jl_array_nrows(closure->t);
        for (int i = 0; i < len; i++) {
            if (jl_types_equal(((jl_method_match_t*)jl_array_ptr_ref(closure->t, i))->method->sig, meth->sig)) {
                replaced = 1;
                break;
            }
        }
        if (!replaced) {
            if (closure->lim == 0)
                return 0;
            closure->lim--;
        }
    }
    // don't need to consider other similar methods if this ml will always fully intersect with them and dominates all of them
    if (!closure->include_ambiguous || closure->lim != -1)
        typemap_slurp_search(ml, &closure->match);
    closure->matc = make_method_match((jl_tupletype_t*)closure->match.ti,
        closure->match.env, meth,
        closure->match.issubty ? FULLY_COVERS : NOT_FULLY_COVERS);
    size_t len = jl_array_nrows(closure->t);
    if (len == 0) {
        closure->t = (jl_value_t*)jl_alloc_vec_any(1);
        jl_array_ptr_set(closure->t, 0, (jl_value_t*)closure->matc);
    }
    else {
        jl_array_ptr_1d_push((jl_array_t*)closure->t, (jl_value_t*)closure->matc);
    }
    return 1;
}

static int ml_mtable_visitor(jl_methtable_t *mt, void *closure0)
{
    struct typemap_intersection_env* env = (struct typemap_intersection_env*)closure0;
    return jl_typemap_intersection_visitor(jl_atomic_load_relaxed(&mt->defs), jl_cachearg_offset(mt), env);
}

// Visit the candidate methods, starting from t[idx], to determine a possible valid sort ordering,
// where every morespecific method appears before any method which it has a common
// intersection with but is not partly ambiguous with (ambiguity is transitive, particularly
// if lim==-1, although morespecific is not transitive).
// Implements Tarjan's SCC (strongly connected components) algorithm, simplified to remove the count variable
// Inputs:
//  * `t`: the array of vertexes (method matches)
//  * `idx`: the next vertex to add to the output
//  * `visited`: the state of the algorithm for each vertex in `t`: either 1 if we visited it already or 1+depth if we are visiting it now
//  * `stack`: the state of the algorithm for the current vertex (up to length equal to `t`): the list of all vertexes currently in the depth-first path or in the current SCC
//  * `result`: the output of the algorithm, a sorted list of vertexes (up to length `lim`)
//  * `allambig`: a list of all vertexes with an ambiguity (up to length equal to `t`), discovered while running the rest of the algorithm
//  * `lim`: either -1 for unlimited matches, or the maximum length for `result` before returning failure (return -1).
//           If specified as -1, this will return extra matches that would have been elided from the list because they were already covered by an earlier match.
//           This gives a sort of maximal set of matching methods (up to the first minmax method).
//           If specified as -1, the sorting will also include all "weak" edges (every ambiguous pair) which will create much larger ambiguity cycles,
//           resulting in a less accurate sort order and much less accurate `*has_ambiguity` result.
//  * `include_ambiguous`: whether to filter out fully ambiguous matches from `result`
//  * `*has_ambiguity`: whether the algorithm does not need to compute if there is an unresolved ambiguity
//  * `*found_minmax`: whether there is a minmax method already found, so future fully_covers matches should be ignored
// Outputs:
//  * `*has_ambiguity`: whether the caller should check if there remains an unresolved ambiguity (in `allambig`)
// Returns:
//  * -1: too many matches for lim, other outputs are undefined
//  *  0: the child(ren) have been added to the output
//  * 1+: the children are part of this SCC (up to this depth)
// TODO: convert this function into an iterative call, rather than recursive
static int sort_mlmatches(jl_array_t *t, size_t idx, arraylist_t *visited, arraylist_t *stack, arraylist_t *result, arraylist_t *allambig, int lim, int include_ambiguous, int *has_ambiguity, int *found_minmax)
{
    size_t cycle = (size_t)visited->items[idx];
    if (cycle != 0)
        return cycle - 1; // depth remaining
    jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(t, idx);
    jl_method_t *m = matc->method;
    jl_value_t *ti = (jl_value_t*)matc->spec_types;
    int subt = matc->fully_covers != NOT_FULLY_COVERS; // jl_subtype((jl_value_t*)type, (jl_value_t*)m->sig)
    // first check if this new method is actually already fully covered by an
    // existing match and we can just ignore this entry quickly
    size_t result_len = 0;
    if (subt) {
        if (*found_minmax == 2)
            visited->items[idx] = (void*)1;
    }
    else if (lim != -1) {
        for (; result_len < result->len; result_len++) {
            size_t idx2 = (size_t)result->items[result_len];
            jl_method_match_t *matc2 = (jl_method_match_t*)jl_array_ptr_ref(t, idx2);
            jl_method_t *m2 = matc2->method;
            if (jl_subtype(ti, m2->sig)) {
                if (include_ambiguous) {
                    if (!jl_method_morespecific(m2, m))
                        continue;
                }
                visited->items[idx] = (void*)1;
                break;
            }
        }
    }
    if ((size_t)visited->items[idx] == 1)
        return 0;
    arraylist_push(stack, (void*)idx);
    size_t depth = stack->len;
    visited->items[idx] = (void*)(1 + depth);
    cycle = depth;
    int addambig = 0;
    int mayexclude = 0;
    // First visit all "strong" edges where the child is definitely better.
    // This likely won't hit any cycles, but might (because morespecific is not transitive).
    // Along the way, record if we hit any ambiguities-we may need to track those later.
    for (size_t childidx = 0; childidx < jl_array_nrows(t); childidx++) {
        if (childidx == idx)
            continue;
        int child_cycle = (size_t)visited->items[childidx];
        if (child_cycle == 1)
            continue; // already handled
        if (child_cycle != 0 && child_cycle - 1 >= cycle)
            continue; // already part of this cycle
        jl_method_match_t *matc2 = (jl_method_match_t*)jl_array_ptr_ref(t, childidx);
        jl_method_t *m2 = matc2->method;
        int subt2 = matc2->fully_covers == FULLY_COVERS; // jl_subtype((jl_value_t*)type, (jl_value_t*)m2->sig)
        // TODO: we could change this to jl_has_empty_intersection(ti, (jl_value_t*)matc2->spec_types);
        // since we only care about sorting of the intersections the user asked us about
        if (!subt2 && jl_has_empty_intersection(m2->sig, m->sig))
            continue;
        int msp = jl_method_morespecific(m, m2);
        int msp2 = !msp && jl_method_morespecific(m2, m);
        if (!msp) {
            if (subt || !include_ambiguous || (lim != -1 && msp2)) {
                if (subt2 || ((lim != -1 || (!include_ambiguous && !msp2)) && jl_subtype((jl_value_t*)ti, m2->sig))) {
                    // this may be filtered out as fully intersected, if applicable later
                    mayexclude = 1;
                }
            }
            if (!msp2) {
                addambig = 1; // record there is a least one previously-undetected ambiguity that may need to be investigated later (between m and m2)
            }
        }
        if (lim == -1 ? msp : !msp2) // include only strong or also weak edges, depending on whether the result size is limited
            continue;
        // m2 is (lim!=-1 ? better : not-worse), so attempt to visit it first
        // if limited, then we want to visit only better edges, because that results in finding k best matches quickest
        // if not limited, then we want to visit all edges, since that results in finding the largest SCC cycles, which requires doing the fewest intersections
        child_cycle = sort_mlmatches(t, childidx, visited, stack, result, allambig, lim, include_ambiguous, has_ambiguity, found_minmax);
        if (child_cycle == -1)
            return -1;
        if (child_cycle && child_cycle < cycle) {
            // record the cycle will resolve at depth "cycle"
            cycle = child_cycle;
        }
        if (stack->len == depth) {
            // if this child resolved without hitting a cycle, then there is
            // some probability that this method is already fully covered now
            // (same check as before), and we can delete this vertex now without
            // anyone noticing (too much)
            if (subt) {
                if (*found_minmax == 2)
                    visited->items[idx] = (void*)1;
            }
            else if (lim != -1) {
                for (; result_len < result->len; result_len++) {
                    size_t idx2 = (size_t)result->items[result_len];
                    jl_method_match_t *matc2 = (jl_method_match_t*)jl_array_ptr_ref(t, idx2);
                    jl_method_t *m2 = matc2->method;
                    if (jl_subtype(ti, m2->sig)) {
                        if (include_ambiguous) {
                            if (!jl_method_morespecific(m2, m))
                                continue;
                        }
                        visited->items[idx] = (void*)1;
                        break;
                    }
                }
            }
            if ((size_t)visited->items[idx] == 1) {
                // n.b. cycle might be < depth, if we had a cycle with a child
                // idx, but since we are on the top of the stack, nobody
                // observed that and so we are content to ignore this
                size_t childidx = (size_t)arraylist_pop(stack);
                assert(childidx == idx); (void)childidx;
                assert(!subt || *found_minmax == 2);
                return 0;
            }
        }
    }
    if (matc->fully_covers == NOT_FULLY_COVERS && addambig)
        arraylist_push(allambig, (void*)idx);
    if (cycle != depth)
        return cycle;
    result_len = result->len;
    if (stack->len == depth) {
        // Found one "best" method to add right now. But we might exclude it if
        // we determined earlier that we had that option.
        if (mayexclude) {
            if (!subt || *found_minmax == 2)
                visited->items[idx] = (void*)1;
        }
    }
    else {
        // We have a set of ambiguous methods. Record that.
        // This is greatly over-approximated for lim==-1
        *has_ambiguity = 1;
        // If we followed weak edges above, then this also fully closed the ambiguity cycle
        if (lim == -1)
            addambig = 0;
        // If we're only returning possible matches, now filter out this method
        // if its intersection is fully ambiguous in this SCC group.
        // This is a repeat of the "first check", now that we have completed the cycle analysis
        for (size_t i = depth - 1; i < stack->len; i++) {
            size_t childidx = (size_t)stack->items[i];
            jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(t, childidx);
            jl_value_t *ti = (jl_value_t*)matc->spec_types;
            int subt = matc->fully_covers != NOT_FULLY_COVERS; // jl_subtype((jl_value_t*)type, (jl_value_t*)m->sig)
            if ((size_t)visited->items[childidx] == 1) {
                assert(subt);
                continue;
            }
            assert(visited->items[childidx] == (void*)(2 + i));
            // if we only followed strong edges before above
            // check also if this set has an unresolved ambiguity missing from it
            if (lim != -1 && !addambig) {
                for (size_t j = 0; j < allambig->len; j++) {
                    if ((size_t)allambig->items[j] == childidx) {
                        addambig = 1;
                        break;
                    }
                }
            }
            // always remove fully_covers matches after the first minmax ambiguity group is handled
            if (subt) {
                if (*found_minmax)
                    visited->items[childidx] = (void*)1;
                continue;
            }
            else if (lim != -1) {
                // when limited, don't include this match if it was covered by an earlier one
                for (size_t result_len = 0; result_len < result->len; result_len++) {
                    size_t idx2 = (size_t)result->items[result_len];
                    jl_method_match_t *matc2 = (jl_method_match_t*)jl_array_ptr_ref(t, idx2);
                    jl_method_t *m2 = matc2->method;
                    if (jl_subtype(ti, m2->sig)) {
                        if (include_ambiguous) {
                            if (!jl_method_morespecific(m2, m))
                                continue;
                        }
                        visited->items[childidx] = (void*)1;
                        break;
                    }
                }
            }
        }
        if (!include_ambiguous && lim == -1) {
            for (size_t i = depth - 1; i < stack->len; i++) {
                size_t childidx = (size_t)stack->items[i];
                if ((size_t)visited->items[childidx] == 1)
                    continue;
                jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(t, childidx);
                jl_method_t *m = matc->method;
                jl_value_t *ti = (jl_value_t*)matc->spec_types;
                for (size_t j = depth - 1; j < stack->len; j++) {
                    if (i == j)
                        continue;
                    size_t idx2 = (size_t)stack->items[j];
                    jl_method_match_t *matc2 = (jl_method_match_t*)jl_array_ptr_ref(t, idx2);
                    jl_method_t *m2 = matc2->method;
                    int subt2 = matc2->fully_covers == FULLY_COVERS; // jl_subtype((jl_value_t*)type, (jl_value_t*)m2->sig)
                    // if their intersection contributes to the ambiguity cycle
                    // and the contribution of m is fully ambiguous with the portion of the cycle from m2
                    if (subt2 || jl_subtype((jl_value_t*)ti, m2->sig)) {
                        // but they aren't themselves simply ordered (here
                        // we don't consider that a third method might be
                        // disrupting that ordering and just consider them
                        // pairwise to keep this simple).
                        if (!jl_method_morespecific(m, m2) && !jl_method_morespecific(m2, m)) {
                            visited->items[childidx] = (void*)-1;
                            break;
                        }
                    }
                }
            }
        }
    }
    // copy this cycle into the results
    for (size_t i = depth - 1; i < stack->len; i++) {
        size_t childidx = (size_t)stack->items[i];
        if ((size_t)visited->items[childidx] == 1)
            continue;
        if ((size_t)visited->items[childidx] != -1) {
            assert(visited->items[childidx] == (void*)(2 + i));
            visited->items[childidx] = (void*)-1;
            if (lim == -1 || result->len < lim)
                arraylist_push(result, (void*)childidx);
            else
                return -1;
        }
    }
    // now finally cleanup the stack
    while (stack->len >= depth) {
        size_t childidx = (size_t)arraylist_pop(stack);
        // always remove fully_covers matches after the first minmax ambiguity group is handled
        //jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(t, childidx);
        if (matc->fully_covers != NOT_FULLY_COVERS && !addambig)
            *found_minmax = 2;
        if (visited->items[childidx] != (void*)-1)
            continue;
        visited->items[childidx] = (void*)1;
    }
    return 0;
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
    JL_TIMING(METHOD_MATCH, METHOD_MATCH);
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
    struct ml_matches_env env = {{ml_matches_visitor, (jl_value_t*)type, va, /* .search_slurp = */ 0,
            /* .min_valid = */ *min_valid, /* .max_valid = */ *max_valid,
            /* .ti = */ NULL, /* .env = */ jl_emptysvec, /* .issubty = */ 0},
        intersections, world, lim, include_ambiguous, /* .t = */ jl_an_empty_vec_any,
        /* .matc = */ NULL};
    struct jl_typemap_assoc search = {(jl_value_t*)type, world, jl_emptysvec};
    jl_value_t *isect2 = NULL;
    JL_GC_PUSH6(&env.t, &env.matc, &env.match.env, &search.env, &env.match.ti, &isect2);

    if (mt) {
        // check the leaf cache if this type can be in there
        if (((jl_datatype_t*)unw)->isdispatchtuple) {
            jl_genericmemory_t *leafcache = jl_atomic_load_relaxed(&mt->leafcache);
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
                size_t min_world = jl_atomic_load_relaxed(&entry->min_world);
                size_t max_world = jl_atomic_load_relaxed(&entry->max_world);
                if (*min_valid < min_world)
                    *min_valid = min_world;
                if (*max_valid > max_world)
                    *max_valid = max_world;
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
                size_t min_world = jl_atomic_load_relaxed(&entry->min_world);
                size_t max_world = jl_atomic_load_relaxed(&entry->max_world);
                if (*min_valid < min_world)
                    *min_valid = min_world;
                if (*max_valid > max_world)
                    *max_valid = max_world;
                JL_GC_POP();
                return env.t;
            }
        }
        if (!ml_mtable_visitor(mt, &env.match)) {
            JL_GC_POP();
            // if we return early, set only the min/max valid collected from matching
            *min_valid = env.match.min_valid;
            *max_valid = env.match.max_valid;
            return jl_nothing;
        }
    }
    else {
        // else: scan everything
        if (!jl_foreach_reachable_mtable(ml_mtable_visitor, &env.match)) {
            JL_GC_POP();
            // if we return early, set only the min/max valid collected from matching
            *min_valid = env.match.min_valid;
            *max_valid = env.match.max_valid;
            return jl_nothing;
        }
    }
    // if we return early, set only the min/max valid collected from matching
    *min_valid = env.match.min_valid;
    *max_valid = env.match.max_valid;
    // done with many of these values now
    env.match.ti = NULL; env.matc = NULL; env.match.env = NULL; search.env = NULL;
    size_t i, j, len = jl_array_nrows(env.t);
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
                    if (jl_method_morespecific(minmaxm, m))
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
                    if (!jl_method_morespecific(minmaxm, m)) {
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
                    if (jl_method_morespecific(minmaxm, m))
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
        if (minmax && lim == 0) {
            // protect some later algorithms from underflow
            JL_GC_POP();
            return jl_nothing;
        }
    }
    if (len > 1) {
        arraylist_t stack, visited, result, allambig;
        arraylist_new(&result, lim != -1 && lim < len ? lim : len);
        arraylist_new(&stack, 0);
        arraylist_new(&visited, len);
        arraylist_new(&allambig, len);
        arraylist_grow(&visited, len);
        memset(visited.items, 0, len * sizeof(size_t));
        // if we had a minmax method (any subtypes), now may now be able to
        // quickly cleanup some of methods
        int found_minmax = 0;
        if (minmax != NULL)
            found_minmax = 2;
        else if (minmax_ambig && !include_ambiguous)
            found_minmax = 1;
        if (ambig == NULL) // if we don't care about the result, set it now so we won't bother attempting to compute it accurately later
            has_ambiguity = 1;
        for (i = 0; i < len; i++) {
            assert(visited.items[i] == (void*)0 || visited.items[i] == (void*)1);
            jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(env.t, i);
            if (matc->fully_covers != NOT_FULLY_COVERS && found_minmax) {
                // this was already handled above and below, so we won't learn anything new
                // by visiting it and it might be a bit costly
                continue;
            }
            int child_cycle = sort_mlmatches((jl_array_t*)env.t, i, &visited, &stack, &result, &allambig, lim == -1 || minmax == NULL ? lim : lim - 1, include_ambiguous, &has_ambiguity, &found_minmax);
            if (child_cycle == -1) {
                arraylist_free(&allambig);
                arraylist_free(&visited);
                arraylist_free(&stack);
                arraylist_free(&result);
                JL_GC_POP();
                return jl_nothing;
            }
            assert(child_cycle == 0); (void)child_cycle;
            assert(stack.len == 0);
            assert(visited.items[i] == (void*)1);
        }
        // now compute whether there were ambiguities left in this cycle
        if (has_ambiguity == 0 && allambig.len > 0) {
            if (lim == -1) {
                // lim is over-approximated, so has_ambiguities is too
                has_ambiguity = 1;
            }
            else {
                // go back and find the additional ambiguous methods and temporary add them to the stack
                // (potentially duplicating them from lower on the stack to here)
                jl_value_t *ti = NULL;
                jl_value_t *isect2 = NULL;
                JL_GC_PUSH2(&ti, &isect2);
                for (size_t i = 0; i < allambig.len; i++) {
                    size_t idx = (size_t)allambig.items[i];
                    jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(env.t, idx);
                    jl_method_t *m = matc->method;
                    int subt = matc->fully_covers == FULLY_COVERS; // jl_subtype((jl_value_t*)type, (jl_value_t*)m->sig)
                    for (size_t idx2 = 0; idx2 < jl_array_nrows(env.t); idx2++) {
                        if (idx2 == idx)
                            continue;
                        // laborious test, checking for existence and coverage of another method (m3)
                        // outside of the ambiguity group that dominates any ambiguous methods,
                        // and means we can ignore this for has_ambiguity
                        // (has_ambiguity is overestimated for lim==-1, since we don't compute skipped matches either)
                        // n.b. even if we skipped them earlier, they still might
                        // contribute to the ambiguities (due to lock of transitivity of
                        // morespecific over subtyping)
                        // TODO: we could improve this result by checking if the removal of some
                        // edge earlier means that this subgraph is now well-ordered and then be
                        // allowed to ignore these vertexes entirely here
                        jl_method_match_t *matc2 = (jl_method_match_t*)jl_array_ptr_ref(env.t, idx2);
                        jl_method_t *m2 = matc2->method;
                        int subt2 = matc2->fully_covers == FULLY_COVERS; // jl_subtype((jl_value_t*)type, (jl_value_t*)m2->sig)
                        if (subt) {
                            ti = (jl_value_t*)matc2->spec_types;
                            isect2 = NULL;
                        }
                        else if (subt2) {
                            ti = (jl_value_t*)matc->spec_types;
                            isect2 = NULL;
                        }
                        else {
                            jl_type_intersection2((jl_value_t*)matc->spec_types, (jl_value_t*)matc2->spec_types, &ti, &isect2);
                        }
                        // if their intersection contributes to the ambiguity cycle
                        if (ti == jl_bottom_type)
                            continue;
                        // and they aren't themselves simply ordered
                        if (jl_method_morespecific(m, m2) || jl_method_morespecific(m2, m))
                            continue;
                        // now look for a third method m3 that dominated these and that fully covered this intersection already
                        size_t k;
                        for (k = 0; k < result.len; k++) {
                            size_t idx3 = (size_t)result.items[k];
                            if (idx3 == idx || idx3 == idx2) {
                                has_ambiguity = 1;
                                break;
                            }
                            jl_method_match_t *matc3 = (jl_method_match_t*)jl_array_ptr_ref(env.t, idx3);
                            jl_method_t *m3 = matc3->method;
                            if ((jl_subtype(ti, m3->sig) || (isect2 && jl_subtype(isect2, m3->sig)))
                                    && jl_method_morespecific(m3, m) && jl_method_morespecific(m3, m2)) {
                                //if (jl_subtype(matc->spec_types, ti) || jl_subtype(matc->spec_types, matc3->m3->sig))
                                //    // check if it covered not only this intersection, but all intersections with matc
                                //    // if so, we do not need to check all of them separately
                                //    j = len;
                                break;
                            }
                        }
                        if (k == result.len)
                            has_ambiguity = 1;
                        isect2 = NULL;
                        ti = NULL;
                        if (has_ambiguity)
                            break;
                    }
                    if (has_ambiguity)
                        break;
                }
                JL_GC_POP();
            }
        }
        arraylist_free(&allambig);
        arraylist_free(&visited);
        arraylist_free(&stack);
        for (j = 0; j < result.len; j++) {
            i = (size_t)result.items[j];
            jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(env.t, i);
            // remove our sentinel entry markers
            if (matc->fully_covers == SENTINEL)
                matc->fully_covers = NOT_FULLY_COVERS;
            result.items[j] = (void*)matc;
        }
        if (minmax) {
            arraylist_push(&result, minmax);
            j++;
        }
        memcpy(jl_array_data(env.t, jl_method_match_t*), result.items, j * sizeof(jl_method_match_t*));
        arraylist_free(&result);
        if (j != len)
            jl_array_del_end((jl_array_t*)env.t, len - j);
        len = j;
    }
    for (j = 0; j < len; j++) {
        jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(env.t, j);
        jl_method_t *m = matc->method;
        // method applicability is the same as typemapentry applicability
        size_t min_world = jl_atomic_load_relaxed(&m->primary_world);
        // intersect the env valid range with method lookup's inclusive valid range
        if (env.match.min_valid < min_world)
            env.match.min_valid = min_world;
    }
    if (mt && cache_result && ((jl_datatype_t*)unw)->isdispatchtuple) { // cache_result parameter keeps this from being recursive
        if (len == 1 && !has_ambiguity) {
            env.matc = (jl_method_match_t*)jl_array_ptr_ref(env.t, 0);
            jl_method_t *meth = env.matc->method;
            jl_svec_t *tpenv = env.matc->sparams;
            JL_LOCK(&mt->writelock);
            cache_method(mt, &mt->cache, (jl_value_t*)mt, (jl_tupletype_t*)unw, meth, world, env.match.min_valid, env.match.max_valid, tpenv);
            JL_UNLOCK(&mt->writelock);
        }
    }
    *min_valid = env.match.min_valid;
    *max_valid = env.match.max_valid;
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

JL_DLLEXPORT uint64_t jl_typeinf_timing_begin(void)
{
    jl_task_t *ct = jl_current_task;
    if (ct->reentrant_timing & 1)
        return 0;
    ct->reentrant_timing |= 1;
    return jl_hrtime();
}

JL_DLLEXPORT void jl_typeinf_timing_end(uint64_t start, int is_recompile)
{
    if (!start)
        return;
    jl_task_t *ct = jl_current_task;
    ct->reentrant_timing &= ~1u;
    if (jl_atomic_load_relaxed(&jl_measure_compile_time_enabled)) {
        uint64_t inftime = jl_hrtime() - start;
        jl_atomic_fetch_add_relaxed(&jl_cumulative_compile_time, inftime);
        if (is_recompile) {
            jl_atomic_fetch_add_relaxed(&jl_cumulative_recompile_time, inftime);
        }
    }
}

// declare a C-callable entry point; called during code loading from the toplevel
JL_DLLEXPORT void jl_extern_c(jl_value_t *name, jl_value_t *declrt, jl_tupletype_t *sigt)
{
    // validate arguments. try to do as many checks as possible here to avoid
    // throwing errors later during codegen.
    JL_TYPECHK(@ccallable, type, declrt);
    if (!jl_is_tuple_type(sigt))
        jl_type_error("@ccallable", (jl_value_t*)jl_anytuple_type_type, (jl_value_t*)sigt);
    // check that f is a guaranteed singleton type
    jl_datatype_t *ft = (jl_datatype_t*)jl_tparam0(sigt);
    if (!jl_is_datatype(ft) || !jl_is_datatype_singleton(ft))
        jl_error("@ccallable: function object must be a singleton");

    // compute / validate return type
    if (!jl_is_concrete_type(declrt) || jl_is_kind(declrt))
        jl_error("@ccallable: return type must be concrete and correspond to a C type");
    if (!jl_type_mappable_to_c(declrt))
        jl_error("@ccallable: return type doesn't correspond to a C type");

    // validate method signature
    size_t i, nargs = jl_nparams(sigt);
    for (i = 1; i < nargs; i++) {
        jl_value_t *ati = jl_tparam(sigt, i);
        if (!jl_is_concrete_type(ati) || jl_is_kind(ati) || !jl_type_mappable_to_c(ati))
            jl_error("@ccallable: argument types must be concrete");
    }

    // save a record of this so that the alias is generated when we write an object file
    jl_method_t *meth = (jl_method_t*)jl_methtable_lookup(ft->name->mt, (jl_value_t*)sigt, jl_atomic_load_acquire(&jl_world_counter));
    if (!jl_is_method(meth))
        jl_error("@ccallable: could not find requested method");
    JL_GC_PUSH1(&meth);
    if (name == jl_nothing)
        meth->ccallable = jl_svec2(declrt, (jl_value_t*)sigt);
    else
        meth->ccallable = jl_svec3(declrt, (jl_value_t*)sigt, name);
    jl_gc_wb(meth, meth->ccallable);
    JL_GC_POP();
}


#ifdef __cplusplus
}
#endif

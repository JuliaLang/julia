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

// Record of which caches were involved in choosing this compile target.
// To populate after compile for caching.
enum internal_compilation_triggers {
    TRIGGER_NONE, // No cache should be updated
    TRIGGER_FOREIGN, // Unmanageable cache, requires exact updates
    TRIGGER_DISPATCH, // Insert to global dispatch cache
    TRIGGER_INVOKE // Insert to local (invoke) dispatch cache
};

_Atomic(int) allow_new_worlds = 1;
JL_DLLEXPORT _Atomic(size_t) jl_world_counter = 1; // uses atomic acquire/release
jl_mutex_t world_counter_lock;
static _Atomic(size_t) jl_method_cache_insert_generation = 1;

static inline size_t jl_method_cache_insert_generation_load(void) JL_NOTSAFEPOINT
{
    return jl_atomic_load_acquire(&jl_method_cache_insert_generation);
}

static inline void jl_method_cache_inserted(void)
{
    jl_atomic_fetch_add(&jl_method_cache_insert_generation, 1);
}

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
// m->max_varargs (if specified) or a heuristic based on the maximum number of
// non-varargs arguments for the function type of the method signature.
//
// If provided, `may_increase` is set to 1 if the returned value is
// heuristic-based and has a chance of increasing in the future.
static size_t get_max_varargs(
        jl_method_t *m,
        uint8_t *may_increase) JL_NOTSAFEPOINT
{
    size_t max_varargs = 1;
    if (may_increase != NULL)
        *may_increase = 0;

    if (m->max_varargs != UINT8_MAX) {
        max_varargs = m->max_varargs;
    }
    else {
        jl_typename_t *tn1 = jl_nth_argument_datatypename(m->sig, 1);
        jl_typename_t *tn;
        if (jl_kwcall_type && tn1 == jl_kwcall_type->name)
            tn = jl_nth_argument_datatypename(m->sig, 3);
        else
            tn = tn1;
        if (tn != NULL && (!jl_kwcall_type || tn != jl_kwcall_type->name)) {
            if (may_increase != NULL)
                *may_increase = 1; // `max_args` can increase as new methods are inserted

            max_varargs = jl_atomic_load_relaxed(&tn->max_args) + 2;
            if (jl_kwcall_type && tn1 == jl_kwcall_type->name)
                max_varargs += 2;
            if (max_varargs > m->nargs)
                max_varargs -= m->nargs;
            else
                max_varargs = 0;
        }
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
        jl_fprint_backtrace(ios_safe_stderr);
    }
}

/// ----- Definitions for various internal TypeMaps ----- ///

static int8_t jl_cachearg_offset(void)
{
    return 0;
}

/// ----- Insertion logic for special entries ----- ///


uint_t speccache_hash(size_t idx, jl_value_t *data)
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

static int jl_is_builtinfunc(jl_method_t *m)
{
    return m->source == NULL && m->generator == NULL &&
        jl_atomic_load_relaxed(&m->unspecialized) != NULL &&
        m != jl_opaque_closure_method && !m->is_for_opaque_closure;
    // jl_value_t *tt = m->sig;
    // if (!jl_is_datatype(tt) || jl_nparams(tt) != 2)
    //     return 0;
    // jl_datatype_t *t = jl_unwrap_unionall(jl_tparam(tt, 0));
    // if (!jl_is_datatype(t))
    //     return 0;
    // for (; t->super != t; t = t->super)
    //     if (t == jl_builtin_type)
    //         return 1;
    // return 0;
}


// get or create the MethodInstance for a specialization
static jl_method_instance_t *jl_specializations_get_linfo_(jl_method_t *m JL_PROPAGATES_ROOT, jl_value_t *type, jl_svec_t *sparams, jl_method_instance_t *mi_insert)
{
    if (jl_is_builtinfunc(m))
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
        jl_gc_write_atomic(m, m->specializations, jl_value_t, (jl_value_t*)mi, release);
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
            jl_gc_write_atomic(m, m->specializations, jl_value_t, specializations, release);
            if (hv)
                jl_smallintset_insert(&m->speckeyset, (jl_value_t*)m, speccache_hash, 0, specializations);
        }
        if (hv) {
            _Atomic(jl_method_instance_t*) *data = (_Atomic(jl_method_instance_t*)*)jl_svec_data(specializations);
            // Hashed entries fill a contiguous prefix (inserts always land at
            // its boundary), but the array also holds an unhashed suffix, so
            // the boundary cannot be binary-searched. Image loads insert many
            // specializations into the same hot methods back to back, and the
            // linear boundary scan is O(prefix) per insert: remember recent
            // boundaries per method and verify a hint with two loads
            // (data[h-1] filled and data[h] empty is exact, by contiguity).
            // Stale or torn hints fail verification and fall back to the scan.
            static _Atomic(jl_method_t*) bnd_m[4096];
            static _Atomic(size_t) bnd_i[4096];
            size_t slot = ((((uintptr_t)m) >> 4) * 0x9E3779B97F4A7C15ULL >> 52) & 4095;
            i = 0;
            if (jl_atomic_load_relaxed(&bnd_m[slot]) == m) {
                size_t h = jl_atomic_load_relaxed(&bnd_i[slot]);
                if (h > 0 && h <= cl &&
                    (jl_value_t*)jl_atomic_load_relaxed(&data[h - 1]) != jl_nothing &&
                    (h == cl || (jl_value_t*)jl_atomic_load_relaxed(&data[h]) == jl_nothing))
                    i = h;
            }
            for (; i < cl; i++) {
                jl_method_instance_t *mi = jl_atomic_load_relaxed(&data[i]);
                if ((jl_value_t*)mi == jl_nothing)
                    break;
                assert(!jl_types_equal(mi->specTypes, type));
                jl_contrib_stats[31]++; // insert-position scan length
            }
            jl_atomic_store_relaxed(&bnd_m[slot], m);
            jl_atomic_store_relaxed(&bnd_i[slot], i + 1); // next boundary after this insert
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
            jl_gc_write_atomic(m, m->specializations, jl_value_t, specializations, release);
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

JL_DLLEXPORT jl_value_t *jl_methtable_lookup(jl_value_t *type, size_t world)
{
    // TODO: this is sort of an odd lookup strategy (and the only user of
    // jl_typemap_assoc_by_type with subtype=0), while normally jl_gf_invoke_lookup would be
    // expected to be used instead
    struct jl_typemap_assoc search = {type, world, NULL};
    jl_typemap_entry_t *sf = jl_typemap_assoc_by_type(jl_atomic_load_relaxed(&jl_method_table->defs), &search, 0, /*subtype*/0);
    if (!sf)
        return jl_nothing;
    return sf->func.value;
}

// ----- MethodInstance specialization instantiation ----- //

jl_method_t *jl_mk_builtin_func(jl_datatype_t *dt, jl_sym_t *sname, jl_fptr_args_t fptr) JL_GC_DISABLED
{
    jl_value_t *params[2];
    params[0] = dt->name->wrapper;
    params[1] = jl_tparam0(jl_anytuple_type);
    jl_datatype_t *tuptyp = (jl_datatype_t*)jl_apply_tuple_type_v(params, 2);

    jl_typemap_entry_t *newentry = NULL;
    jl_method_t *m = NULL;
    JL_GC_PUSH3(&m, &newentry, &tuptyp);

    m = jl_new_method_uninit(jl_core_module);
    m->name = sname;
    m->module = jl_core_module;
    m->isva = 1;
    m->nargs = 2;
    jl_atomic_store_relaxed(&m->primary_world, 1);
    jl_atomic_store_relaxed(&m->dispatch_status, METHOD_SIG_LATEST_ONLY | METHOD_SIG_LATEST_WHICH);
    m->sig = (jl_value_t*)tuptyp;
    m->slot_syms = jl_an_empty_string;
    m->nospecialize = 0;
    m->nospecialize = ~m->nospecialize;

    jl_method_instance_t *mi = jl_get_specialized(m, (jl_value_t*)tuptyp, jl_emptysvec);
    jl_gc_write_atomic(m, m->unspecialized, jl_method_instance_t, mi, relaxed);

    jl_debuginfo_t *di = NULL;
    jl_svec_t *edges = jl_emptysvec;
    jl_code_instance_t *codeinst = jl_new_codeinst(mi, jl_nothing,
        (jl_value_t*)jl_any_type, (jl_value_t*)jl_any_type, jl_nothing, jl_nothing,
        0, 1, ~(size_t)0, 0, jl_nothing, di, edges);
    jl_atomic_store_relaxed(&codeinst->specptr.fptr1, fptr);
    jl_atomic_store_relaxed(&codeinst->invoke, jl_fptr_args);
    jl_mi_cache_insert(mi, codeinst);

    newentry = jl_typemap_alloc(tuptyp, NULL, jl_emptysvec,
            (jl_value_t*)m, 1, ~(size_t)0);
    jl_typemap_insert(&jl_method_table->defs, (jl_value_t*)jl_method_table, newentry, 0);

    JL_GC_POP();
    return m;
}

// only relevant for bootstrapping. otherwise fairly broken.
static int emit_codeinst_and_edges(jl_code_instance_t *codeinst)
{
    jl_value_t *code = jl_ci_inferred(codeinst);
    if (code) {
        if (jl_atomic_load_relaxed(&codeinst->invoke) != NULL)
            return 1;
        if (code != jl_nothing) {
            JL_GC_PUSH1(&code);
            jl_method_instance_t *mi = jl_get_ci_mi(codeinst);
            jl_method_t *def = mi->def.method;
            if (jl_is_method(def))
                code = (jl_value_t*)jl_uncompress_ir(def, codeinst, (jl_value_t*)code);
            if (jl_is_code_info(code)) {
                jl_emit_codeinsts_to_jit(&codeinst, (jl_code_info_t **)&code, 1);
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
    for (; codeinst; codeinst = jl_ci_next(codeinst)) {
        if (codeinst->owner != jl_nothing)
            continue;
        if (jl_atomic_load_relaxed(&codeinst->min_world) <= world && world <= jl_atomic_load_relaxed(&codeinst->max_world)) {
            if (emit_codeinst_and_edges(codeinst) && jl_atomic_load_relaxed(&codeinst->invoke) != NULL)
                return codeinst;
        }
    }
    return NULL;
}

// run type inference on lambda "mi" for given argument types.
// returns the inferred source, and may cache the result in mi
// if successful, also updates the mi argument to describe the validity of this src
// if inference doesn't occur (or can't finish), returns NULL instead
jl_code_instance_t *jl_type_infer(jl_method_instance_t *mi, size_t world, uint8_t source_mode, uint8_t trim_mode)
{
    if (jl_typeinf_func == NULL)
        return NULL;
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
    JL_GC_PUSHARGS(fargs, 5);
    fargs[0] = (jl_value_t*)jl_typeinf_func;
    fargs[1] = (jl_value_t*)mi;
    fargs[2] = jl_box_ulong(world);
    fargs[3] = jl_box_uint8(source_mode);
    fargs[4] = jl_box_uint8(trim_mode);
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
        ci = (jl_code_instance_t*)jl_apply(fargs, 5);
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
            jl_fprint_backtrace(ios_safe_stderr);
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

    // Record inference entrance backtrace if enabled
    if (ci) {
        JL_GC_PUSH1(&ci);
        jl_push_inference_entrance_backtraces((jl_value_t*)ci);
        JL_GC_POP();
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
    jl_module_t *CC = (jl_module_t*)jl_get_global_value(jl_core_module, jl_symbol("Compiler"), ct->world_age);
    if (CC != NULL && jl_is_module(CC)) {
        JL_GC_PROMISE_ROOTED(CC);
        fargs[0] = jl_get_global_value(CC, jl_symbol("NativeInterpreter"), ct->world_age);
        fargs[1] = jl_box_ulong(world);
        fargs[1] = jl_apply(fargs, 2);
        fargs[0] = jl_get_global_value(CC, jl_symbol("typeinf_code"), ct->world_age);
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

JL_DLLEXPORT jl_code_instance_t *jl_get_method_uninferred(
        jl_method_instance_t *mi JL_PROPAGATES_ROOT, jl_value_t *rettype,
        size_t min_world, size_t max_world, jl_debuginfo_t *di, jl_svec_t *edges)
{
    jl_value_t *owner = jl_nothing; // TODO: owner should be arg
    jl_code_instance_t *codeinst = jl_atomic_load_relaxed(&mi->cache);
    for (; codeinst; codeinst = jl_ci_next(codeinst)) {
        if (jl_atomic_load_relaxed(&codeinst->min_world) == min_world &&
            jl_atomic_load_relaxed(&codeinst->max_world) == max_world &&
            jl_egal(jl_ci_owner(codeinst), owner) &&
            jl_egal(jl_ci_rettype(codeinst), rettype)) {
            if (di == NULL)
                return codeinst;
            jl_debuginfo_t *debuginfo = jl_ci_debuginfo(codeinst);
            if (di != debuginfo) {
                jl_gc_wb(codeinst, di);
                if (!(debuginfo == NULL && jl_atomic_cmpswap_relaxed(&codeinst->debuginfo, &debuginfo, di)))
                    if (!(debuginfo && jl_egal((jl_value_t*)debuginfo, (jl_value_t*)di)))
                        continue;
            }
            // TODO: this is implied by the matching worlds, since it is intrinsic, so do we really need to verify it?
            jl_svec_t *e = jl_atomic_load_relaxed(&codeinst->edges);
            if (e && jl_egal((jl_value_t*)e, (jl_value_t*)edges))
                return codeinst;
        }
    }
    codeinst = jl_new_codeinst(
        mi, owner, rettype, (jl_value_t*)jl_any_type, NULL, NULL,
        0, min_world, max_world, 0, NULL, di, edges);
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
        codeinst = jl_ci_next(codeinst);
    }
    return 0;
}

// look for something with an egal ABI and properties that is already in the JIT for a whole edge (target_world=0) or can be added to the JIT with new source just for target_world.
JL_DLLEXPORT jl_code_instance_t *jl_get_ci_equiv(jl_code_instance_t *ci JL_PROPAGATES_ROOT, size_t target_world) JL_NOTSAFEPOINT
{
    jl_value_t *def = jl_ci_defobj(ci);
    jl_method_instance_t *mi = jl_get_ci_mi(ci);
    jl_value_t *owner = jl_ci_owner(ci);
    jl_value_t *rettype = jl_ci_rettype(ci);
    jl_code_instance_t *codeinst = jl_atomic_load_relaxed(&mi->cache);
    while (codeinst) {
        if (codeinst != ci &&
            jl_ci_inferred(codeinst) != NULL &&
            jl_atomic_load_relaxed(&codeinst->min_world) <= target_world &&
            jl_atomic_load_relaxed(&codeinst->max_world) >= target_world &&
            jl_egal(jl_ci_defobj(codeinst), def) &&
            jl_egal(jl_ci_owner(codeinst), owner) &&
            jl_egal(jl_ci_rettype(codeinst), rettype)) {
            return codeinst;
        }
        codeinst = jl_ci_next(codeinst);
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
    jl_atomic_store_relaxed(&codeinst->flags, 0);
    jl_atomic_store_relaxed(&codeinst->next, NULL);
    jl_atomic_store_relaxed(&codeinst->ipo_purity_bits, effects);
    codeinst->analysis_results = analysis_results;

    jl_jit_register_ci(codeinst);

    return codeinst;
}

JL_DLLEXPORT void jl_fill_codeinst(
        jl_code_instance_t *codeinst,
        jl_value_t *rettype, jl_value_t *exctype,
        jl_value_t *inferred_const,
        jl_value_t *inferred,
        int32_t const_flags, size_t min_world, size_t max_world,
        uint32_t effects, jl_value_t *analysis_results,
        double time_infer_total, double time_infer_cache_saved, double time_infer_self,
        jl_debuginfo_t *di, jl_svec_t *edges /* , int absolute_max*/)
{
    assert(min_world <= max_world && "attempting to set invalid world constraints");
    //assert((!jl_is_method(codeinst->def->def.value) || max_world != ~(size_t)0 || min_world <= 1 || jl_svec_len(edges) != 0) && "missing edges");
    jl_gc_write(codeinst, codeinst->rettype, jl_value_t, rettype);
    jl_gc_write(codeinst, codeinst->exctype, jl_value_t, exctype);
    if ((const_flags & 2) != 0) {
        jl_gc_write(codeinst, codeinst->rettype_const, jl_value_t, inferred_const);
    }
    jl_gc_write(codeinst, codeinst->analysis_results, jl_value_t, analysis_results);
    codeinst->time_infer_total = julia_double_to_half(time_infer_total);
    codeinst->time_infer_cache_saved = julia_double_to_half(time_infer_cache_saved);
    codeinst->time_infer_self = julia_double_to_half(time_infer_self);
    jl_atomic_store_relaxed(&codeinst->ipo_purity_bits, effects);
    jl_gc_write_atomic(codeinst, codeinst->debuginfo, jl_debuginfo_t, di, relaxed);
    jl_gc_write_atomic(codeinst, codeinst->edges, jl_svec_t, edges, relaxed);
    if ((const_flags & 1) != 0) {
        // TODO: may want to follow ordering restrictions here (see jitlayers.cpp)
        assert(const_flags & 2);
        jl_atomic_store_release(&codeinst->invoke, jl_fptr_const_return);
    }
    assert(jl_atomic_load_relaxed(&codeinst->min_world) == 1);
    assert(jl_atomic_load_relaxed(&codeinst->max_world) == 0);
    jl_gc_write_atomic(codeinst, codeinst->inferred, jl_value_t, inferred, relaxed);
    jl_atomic_store_release(&codeinst->min_world, min_world);
    jl_atomic_store_release(&codeinst->max_world, max_world);
}

JL_DLLEXPORT jl_code_instance_t *jl_new_codeinst_uninit(jl_method_instance_t *mi, jl_value_t *owner)
{
    jl_code_instance_t *codeinst = jl_new_codeinst(mi, owner, NULL, NULL, NULL, NULL, 0, 0, 0, 0, NULL, NULL, NULL);
    jl_atomic_store_relaxed(&codeinst->min_world, 1); // sentinel: temporarily invalid so jl_fill_codeinst can assert correct initial state
    return codeinst;
}

JL_DLLEXPORT void jl_mi_cache_insert(jl_method_instance_t *mi,
                                     jl_code_instance_t *ci JL_MAYBE_UNROOTED)
{
    JL_GC_PUSH1(&ci);
    if (jl_is_method(mi->def.method))
        JL_LOCK(&mi->def.method->writelock);
    // Set native_cache_valid bit when inserting into cache
    jl_atomic_fetch_or_relaxed(&ci->flags, JL_CI_FLAGS_NATIVE_CACHE_VALID);
    // find the preferred location for insertion of ci now:
    //   - invoke+inferred group
    //   - inferred group
    //   - others group
    //   - unmoved
    //   - after existing entries with same applicable range
    jl_value_t *parent = (jl_value_t*)mi;
    _Atomic(jl_code_instance_t*) *slot = &mi->cache;
    jl_code_instance_t *oldci = jl_atomic_load_relaxed(slot);
    int hasinferred = jl_ci_inferred(ci) != NULL;
    int hasinvoke = hasinferred && jl_atomic_load_relaxed(&ci->invoke) != NULL;
    size_t max_world = jl_atomic_load_relaxed(&ci->max_world);
    jl_code_instance_t *next = jl_ci_next(ci);
    while (oldci) {
        if (oldci == ci)
            break;
        int old_hasinferred = jl_ci_inferred(oldci) != NULL;
        int old_hasinvoke = old_hasinferred && jl_atomic_load_relaxed(&oldci->invoke) != NULL;
        size_t old_max_world = jl_atomic_load_relaxed(&oldci->max_world);
        if (hasinvoke && !old_hasinvoke)
            break;
        if (hasinferred && !old_hasinferred)
            break;
        if (next == NULL && old_max_world < max_world)
            break;
        parent = (jl_value_t*)oldci;
        slot = &oldci->next;
        oldci = jl_atomic_load_relaxed(slot);
    }
    if (oldci != ci) {
        jl_gc_write_atomic(ci, ci->next, jl_code_instance_t, oldci, relaxed);
        jl_gc_write_atomic(parent, *slot, jl_code_instance_t, ci, release);
        if (oldci != NULL) {
            // list is now potentially circular, need to go find old pointer to ci starting from oldci and insert next there
            do {
                parent = (jl_value_t*)oldci;
                slot = &oldci->next;
                oldci = jl_atomic_load_relaxed(slot);
            } while (oldci && oldci != ci);
            if (oldci) {
                jl_gc_write_atomic(parent, *slot, jl_code_instance_t, next, release);
            }
        }
    }
    if (jl_is_method(mi->def.method))
        JL_UNLOCK(&mi->def.method->writelock);
    JL_GC_POP();
    return;
}

JL_DLLEXPORT int jl_mi_try_insert(jl_method_instance_t *mi,
                                   jl_code_instance_t *expected_ci,
                                   jl_code_instance_t *ci JL_MAYBE_UNROOTED)
{
    JL_GC_PUSH1(&ci);
    if (jl_is_method(mi->def.method))
        JL_LOCK(&mi->def.method->writelock);
    jl_code_instance_t *oldci = jl_atomic_load_relaxed(&mi->cache);
    int ret = 0;
    if (oldci == expected_ci) {
        jl_gc_write_atomic(ci, ci->next, jl_code_instance_t, oldci, relaxed);
        jl_gc_write_atomic(mi, mi->cache, jl_code_instance_t, ci, release);
        ret = 1;
    }
    if (jl_is_method(mi->def.method))
        JL_UNLOCK(&mi->def.method->writelock);
    JL_GC_POP();
    return ret;
}

enum top_typename_facts {
    EXACTLY_ANY = 1 << 0,
    HAVE_TYPE = 1 << 1,
    EXACTLY_TYPE = 1 << 2,
    HAVE_FUNCTION = 1 << 3,
    EXACTLY_FUNCTION = 1 << 4,
    HAVE_KWCALL = 1 << 5,
    EXACTLY_KWCALL = 1 << 6,
    SHORT_TUPLE = 1 << 7,
};

static void foreach_top_nth_typename(void (*f)(jl_typename_t*, int, void*), jl_value_t *a JL_PROPAGATES_ROOT, int n, unsigned *facts, void *env)
{
    arraylist_t workqueue;
    arraylist_new(&workqueue, 0);

    // Push initial work item as jl_value_t* then int (cast to void*)
    arraylist_push(&workqueue, a);
    arraylist_push(&workqueue, (void*)(uintptr_t)n);

    while (workqueue.len > 0) {
        // Pop int n then jl_value_t* a (reverse order)
        int current_n = (int)(uintptr_t)arraylist_pop(&workqueue);
        jl_value_t *current_a = (jl_value_t*)arraylist_pop(&workqueue);
        JL_GC_PROMISE_ROOTED(current_a);

        if (jl_is_some_Type(current_a)) {
            *facts |= HAVE_TYPE;
            arraylist_push(&workqueue, jl_some_Type_T(current_a));
            arraylist_push(&workqueue, (void*)(uintptr_t)-1);
        }
        else if (jl_is_datatype(current_a)) {
            if (current_n <= 0) {
                jl_datatype_t *dt = ((jl_datatype_t*)current_a);
                if (dt == jl_function_type) {
                    if (current_n == -1) // key Type{>:Function} as Type instead of Function
                        *facts |= EXACTLY_TYPE; // HAVE_TYPE is already set
                    else
                        *facts |= HAVE_FUNCTION | EXACTLY_FUNCTION;
                }
                else if (dt == jl_any_type) {
                    if (current_n == -1) // key Type{>:Any} and kinds as Type instead of Any
                        *facts |= EXACTLY_TYPE; // HAVE_TYPE is already set
                    else
                        *facts |= EXACTLY_ANY;
                }
                else if (dt == jl_kwcall_type) {
                    if (current_n == -1) // key Type{>:typeof(kwcall)} as exactly kwcall
                        *facts |= EXACTLY_KWCALL;
                    else
                        *facts |= HAVE_KWCALL;
                }
                else {
                    while (1) {
                        jl_datatype_t *super = dt->super;
                        if (super == jl_function_type) {
                            *facts |= HAVE_FUNCTION;
                            break;
                        }
                        if (super == jl_any_type || super->super == dt)
                            break;
                        dt = super;
                    }
                    f(dt->name, 1, env);
                }
            }
            else if (jl_is_tuple_type(current_a)) {
                if (jl_nparams(current_a) >= current_n) {
                    arraylist_push(&workqueue, jl_tparam(current_a, current_n - 1));
                    arraylist_push(&workqueue, (void*)(uintptr_t)0);
                }
                else
                    *facts |= SHORT_TUPLE;
            }
        }
        else if (jl_is_typevar(current_a)) {
            arraylist_push(&workqueue, ((jl_tvar_t*)current_a)->ub);
            arraylist_push(&workqueue, (void*)(uintptr_t)current_n);
        }
        else if (jl_is_unionall(current_a)) {
            arraylist_push(&workqueue, ((jl_unionall_t*)current_a)->body);
            arraylist_push(&workqueue, (void*)(uintptr_t)current_n);
        }
        else if (jl_is_uniontype(current_a)) {
            jl_uniontype_t *u = (jl_uniontype_t*)current_a;
            // Add both union branches to workqueue (push a second to visit first)
            arraylist_push(&workqueue, u->b);
            arraylist_push(&workqueue, (void*)(uintptr_t)current_n);
            arraylist_push(&workqueue, u->a);
            arraylist_push(&workqueue, (void*)(uintptr_t)current_n);
        }
    }

    arraylist_free(&workqueue);
}

// Inspect type `argtypes` for all backedge keys that might be relevant to it, splitting it
// up on some commonly observed patterns to make a better distribution.
// (It could do some of that balancing automatically, but for now just hard-codes kwcall.)
// Along the way, record some facts about what was encountered, so that those additional
// calls can be added later if needed for completeness.
// The `int explct` argument instructs the caller if the callback is due to an exactly
// encountered type or if it rather encountered a subtype.
// This is not capable of walking to all top-typenames for an explicitly encountered
// Function or Any, so the caller has a fallback that can scan the entire table in that case.
// We do not de-duplicate calls when encountering a Union.
int jl_foreach_top_typename_for(void (*f)(jl_typename_t*, int, void*), jl_value_t *argtypes JL_PROPAGATES_ROOT, int all_subtypes, void *env)
{
    unsigned facts = 0;
    foreach_top_nth_typename(f, argtypes, 1, &facts, env);
    if (facts & HAVE_KWCALL) {
        // split kwcall on the 3rd argument instead, using the same logic
        unsigned kwfacts = 0;
        foreach_top_nth_typename(f, argtypes, 3, &kwfacts, env);
        // copy kwfacts to original facts
        if (kwfacts & SHORT_TUPLE)
            kwfacts |= (all_subtypes ? EXACTLY_ANY : EXACTLY_KWCALL);
        facts |= kwfacts;
    }
    if (all_subtypes && (facts & (EXACTLY_FUNCTION | EXACTLY_TYPE | EXACTLY_ANY)))
        // flag that we have an explicit match that is necessitating a full table scan
        return 0;
    // or inform caller of only which supertypes are applicable
    if (facts & HAVE_FUNCTION)
        f(jl_function_type->name, facts & EXACTLY_FUNCTION ? 1 : 0, env);
    if (facts & HAVE_TYPE)
        f(jl_type_typename, facts & EXACTLY_TYPE ? 1 : 0, env);
    if (facts & (HAVE_KWCALL | EXACTLY_KWCALL))
        f(jl_kwcall_type->name, facts & EXACTLY_KWCALL ? 1 : 0, env);
    f(jl_any_type->name, facts & EXACTLY_ANY ? 1 : 0, env);
    return 1;
}


int jl_foreach_reachable_mtable(int (*visit)(jl_methtable_t *mt, void *env), jl_array_t *mod_array, void *env)
{
    if (!visit(jl_method_table, env))
        return 0;

    if (!mod_array)
        return 1;

    arraylist_t workqueue;
    arraylist_new(&workqueue, 0);

    // Add initial toplevel modules to workqueue
    for (size_t i = 0; i < jl_array_nrows(mod_array); i++) {
        jl_module_t *m = (jl_module_t*)jl_array_ptr_ref(mod_array, i);
        assert(jl_is_module(m));
        if (m->parent == m) // some toplevel modules (really just Base) aren't actually
            arraylist_push(&workqueue, m);
    }

    int result = 1;

    while (workqueue.len > 0) {
        jl_module_t *current_m = (jl_module_t*)arraylist_pop(&workqueue);
        JL_GC_PROMISE_ROOTED(current_m);

        jl_svec_t *table = jl_atomic_load_relaxed(&current_m->bindings);
        for (size_t i = 0; i < jl_svec_len(table); i++) {
            jl_binding_t *b = (jl_binding_t*)jl_svecref(table, i);
            if ((void*)b == jl_nothing)
                break;
            jl_sym_t *name = b->globalref->name;
            jl_value_t *v = jl_get_latest_binding_value_if_const(b);
            if (v) {
                if (jl_is_module(v)) {
                    jl_module_t *child = (jl_module_t*)v;
                    if (child != current_m && child->parent == current_m && child->name == name) {
                        // this is the original/primary binding for the submodule
                        arraylist_push(&workqueue, child);
                    }
                }
                else if (jl_is_mtable(v)) {
                    jl_methtable_t *mt = (jl_methtable_t*)v;
                    if (mt && mt != jl_method_table && mt->module == current_m && mt->name == name) {
                        if (!visit(mt, env)) {
                            result = 0;
                            goto cleanup;
                        }
                    }
                }
            }
            table = jl_atomic_load_relaxed(&current_m->bindings);
        }
    }

cleanup:
    arraylist_free(&workqueue);
    return result;
}

jl_value_t *jl_typeinf_func JL_GLOBALLY_ROOTED = NULL;
jl_value_t *jl_compile_and_emit_func JL_GLOBALLY_ROOTED = NULL;
JL_DLLEXPORT size_t jl_typeinf_world = 1;

// Force Compiler (and staticdata serialization) not to throw away Julia IR,
// even when it is not needed for inlining, etc. - intended for debugging only
static _Atomic(int8_t) jl_type_infer_preserve_ir = 0;

JL_DLLEXPORT int8_t jl_get_type_infer_preserve_ir(void)
{
    return jl_atomic_load_relaxed(&jl_type_infer_preserve_ir);
}

JL_DLLEXPORT void jl_set_type_infer_preserve_ir(int8_t v)
{
    jl_atomic_store_relaxed(&jl_type_infer_preserve_ir, v);
}

// Set by precompile workers around `Base.include` so non-inlineable inferred
// IR is retained on `CodeInstance.inferred` through the irgen phase instead of
// being discarded. `jl_finalize_precompile_inferred` nulls them before save
// (with a backstop in jl_queue_for_serialization).
static _Atomic(int8_t) jl_precompile_keep_ir = 0;

JL_DLLEXPORT int8_t jl_get_precompile_keep_ir(void)
{
    return jl_atomic_load_relaxed(&jl_precompile_keep_ir);
}

JL_DLLEXPORT void jl_set_precompile_keep_ir(int8_t v)
{
    jl_atomic_store_relaxed(&jl_precompile_keep_ir, v);
}

static int invalidate_all_entries(jl_typemap_entry_t *entry, void *env)
{
    jl_atomic_store_relaxed(&entry->max_world, 0);
    return 1;
}

static void drop_all_methcache(jl_methcache_t *mc)
{
    JL_LOCK(&mc->writelock);
    jl_typemap_visitor(jl_atomic_load_relaxed(&mc->cache), invalidate_all_entries, NULL);
    jl_genericmemory_t *leafcache = jl_atomic_load_relaxed(&mc->leafcache);
    size_t i, l = leafcache->length;
    for (i = 1; i < l; i += 2) {
        jl_typemap_entry_t *oldentry = (jl_typemap_entry_t*)jl_genericmemory_ptr_ref(leafcache, i);
        if (oldentry) {
            while ((jl_value_t*)oldentry != jl_nothing) {
                invalidate_all_entries(oldentry, NULL);
                oldentry = jl_atomic_load_relaxed(&oldentry->next);
            }
        }
    }
    jl_atomic_store_relaxed(&mc->cache, jl_nothing);
    jl_atomic_store_relaxed(&mc->leafcache, (jl_genericmemory_t*)jl_an_empty_memory_any);
    JL_UNLOCK(&mc->writelock);
}

JL_DLLEXPORT void jl_set_typeinf_func(jl_value_t *f)
{
    if (jl_typeinf_func == NULL) {
        // drop the major caches, so that their structure can now be inferred
        drop_all_methcache(jl_method_table->cache);
    }
    jl_typeinf_func = (jl_value_t*)f;
    jl_typeinf_world = jl_get_tls_world_age();
}

JL_DLLEXPORT void jl_set_compile_and_emit_func(jl_value_t *f)
{
    jl_compile_and_emit_func = (jl_value_t*)f;
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
        jl_value_t *Nroot = NULL;
        JL_GC_PUSH2(&vm, &Nroot);
        assert(jl_subtype_env_size(decl) == nsp);
        vm = jl_instantiate_type_in_env(vm, (jl_unionall_t*)decl, jl_svec_data(sparams));
        assert(jl_is_vararg(vm));
        // rewrap_unionall(lastdeclt, sparams) if any sparams isa TypeVar
        // for example, `Tuple{Vararg{Union{Nothing,Int,Val{T}}}} where T`
        // and the user called it with `Tuple{Vararg{Union{Nothing,Int},N}}`, then T is unbound
        jl_value_t **sp = jl_svec_data(sparams);
        while (jl_is_unionall(decl)) {
            jl_tvar_t *v = NULL;
            if (jl_is_svec(*sp))
                v = (jl_tvar_t*)jl_svecref(*sp, 0);
            if (v && jl_is_typevar(v)) {
                // must unwrap and re-wrap Vararg object explicitly here since jl_type_unionall handles it differently
                jl_value_t *T = ((jl_vararg_t*)vm)->T;
                Nroot = ((jl_vararg_t*)vm)->N;
                int T_has_tv = T && jl_has_typevar(T, v);
                // n.b. JL_VARARG_UNBOUND check means this should be false
                int N_has_tv = Nroot && jl_has_typevar(Nroot, v);
                assert(!N_has_tv || Nroot == (jl_value_t*)v);
                vm = T_has_tv ? jl_type_unionall(v, T) : T;
                if (N_has_tv)
                    Nroot = NULL;
                vm = (jl_value_t*)jl_wrap_vararg(vm, Nroot, 1, 0); // this cannot throw for these inputs
                Nroot = NULL;
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

static jl_value_t *ml_matches(jl_methtable_t *mt, jl_methcache_t *mc,
                              jl_tupletype_t *type, int lim, int include_ambiguous,
                              int intersections, size_t world, int cache_result_recursion,
                              size_t *min_valid, size_t *max_valid, int *ambig);

// Widen an egality-keyed slot `TypeEgal{A}` to `Type{A}` when the method
// declares the slot as a *concrete* `Type{X}`: there a single `Type{A}`-keyed
// specialization soundly covers all `==`-equal argument values, since no static
// parameter can distinguish them (#61323). The reverse (narrowing a `Type{A}`
// slot of a by-type signature to `TypeEgal{A}`) is not legal here; by-type
// requests that mean the runtime calls narrow at their entry point instead
// (see `jl_get_compile_hint_specialization`).
static void egal_normalize_slot(jl_tupletype_t *tt, size_t i, jl_value_t *decl_i,
                                jl_svec_t **newparams JL_REQUIRE_ROOTED_SLOT)
{
    jl_value_t *elt = jl_tparam(tt, i);
    if (jl_is_typeegal(elt) && !jl_has_free_typevars(elt) &&
        jl_is_typeeq(decl_i) && !jl_has_free_typevars(decl_i)) {
        if (!*newparams) *newparams = jl_svec_copy(tt->parameters);
        jl_svecset(*newparams, i, jl_wrap_Type(jl_some_Type_T(elt)));
    }
}

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

    if (jl_is_builtinfunc(definition)) {
        assert(jl_is_datatype(decl));
        *newparams = ((jl_datatype_t*)decl)->parameters; // handle builtin methods
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
        jl_value_t *decl_i = jl_nth_slot_type(decl, i);
        type_i = jl_rewrap_unionall(decl_i, decl);
        size_t i_arg = (i < nargs - 1 ? i : nargs - 1);

        if (jl_is_vararg(elt)) {
            elt = jl_unwrap_vararg(elt);
        }
        else {
            egal_normalize_slot(tt, i, decl_i, newparams);
            if (*newparams)
                elt = jl_svecref(*newparams, i);
        }

        if (jl_is_kind(type_i)) {
            // if we can prove the match was against the kind (not a Type)
            // we want to put that in the cache instead
            if (!*newparams) *newparams = jl_svec_copy(tt->parameters);
            elt = type_i;
            jl_svecset(*newparams, i, elt);
        }
        else if (jl_is_some_Type(elt)) {
            // if the declared type was not Any or Union{Type, ...},
            // then the match must been with the kind (e.g. UnionAll or DataType)
            // and the result of matching the type signature
            // needs to be restricted to the concrete type 'kind'
            jl_value_t *kind = jl_typeof(jl_some_Type_T(elt));
            if (!(jl_is_typeeq(elt) && jl_is_bottom_singleton_class(jl_typeeq_T(elt))) &&
                    !jl_has_free_typevars(decl_i) &&
                    jl_subtype(kind, type_i) && !jl_subtype((jl_value_t*)jl_type_type, type_i)) {
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
            if (elt == (jl_value_t*)jl_typeofbottom_type) {
                // Preserve the singleton `Type{Union{}}` dispatch key. Widening it to
                // `Type` loses static parameters for compiled calls to `::Type{T}`.
            }
            else if (!(jl_subtype(elt, type_i) && !jl_subtype((jl_value_t*)jl_type_type, type_i))) {
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
        else if (!jl_is_some_Type(elt) && !jl_is_datatype(elt) && jl_subtype(elt, (jl_value_t*)jl_type_type)) { // elt <: Type{T}
            // not triggered for isdispatchtuple(tt), this attempts to handle
            // some cases of adapting a random signature into a compilation signature
            if (!*newparams) *newparams = jl_svec_copy(tt->parameters);
            jl_svecset(*newparams, i, jl_type_type);
        }
        else if (jl_is_some_Type(elt)) { // elt isa Type{T} / TypeEgal{T}
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
            else if (jl_is_some_Type(jl_some_Type_T(elt)) &&
                     // try to give up on specializing type parameters for Type{Type{Type{...}}}
                     (jl_is_some_Type(jl_some_Type_T(jl_some_Type_T(elt))) || !jl_has_free_typevars(decl_i))) {
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
        if (notcalled_func && (jl_subtype((jl_value_t*)jl_function_type, type_i))) {
            // and attempt to despecialize types marked as a supertype of Function (i.e.
            // Function, Callable, Any, or a Union{Function, T})
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
        if (max_varargs > 0 && nspec >= 2) {
            type_i = jl_svecref(*newparams, nspec - 2);
        }
        else {
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
            if (jl_is_some_Type(type_i) && jl_is_some_Type(jl_some_Type_T(type_i)))
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

    if (!jl_is_datatype(type) || jl_has_free_typevars((jl_value_t*)type)) {
        return 0;
    }
    if (jl_is_builtinfunc(definition))
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
        // try to refine estimate of min and max
        uint8_t heuristic_used = 0;
        nspec_max = nspec_min = nargs + get_max_varargs(definition, &heuristic_used);
        if (heuristic_used)
            nspec_max = INT32_MAX; // new methods may be added, increasing nspec_min later
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
            if (jl_is_some_Type(elt) && jl_is_some_Type(jl_some_Type_T(elt))) {
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

        // a closed type-valued dispatch slot is spelled by egality (`TypeEgal{A}`),
        // or by equality (`Type{A}`) iff the method declares the slot as concrete
        // `Type{X}` (see `egal_normalize_slot`); the other spellings are not
        // compileable (an `==`-keyed slot admits non-egal argument values).
        // `Type{Union{}}` is exempt: the bottom object is the unique instance of
        // its `Type`, so the equality spelling is exact (and `TypeEgal{Union{}}`
        // cannot be spelled; it normalizes to `typeof(Union{})`).
        if (!jl_is_vararg(jl_tparam(type, i)) && !jl_has_free_typevars(elt)) {
            int decl_concrete = jl_is_typeeq(decl_i) && !jl_has_free_typevars(decl_i);
            if ((jl_is_typeeq(elt) && !decl_concrete && jl_typeeq_T(elt) != jl_bottom_type) ||
                (jl_is_typeegal(elt) && decl_concrete)) {
                JL_GC_POP();
                return 0;
            }
        }

        if (jl_is_kind(elt)) {
            if (elt == (jl_value_t*)jl_typeofbottom_type && jl_subtype(elt, type_i))
                continue;
            // kind slots always get guard entries (checking for subtypes of Type)
            if (jl_subtype(elt, type_i) && !jl_subtype((jl_value_t*)jl_type_type, type_i))
                continue;
            // jl_compilation_sig keeps a slot declared as a concrete kind (e.g.
            // `::DataType`) equal to that kind, making it the canonical form
            if (jl_is_kind(type_i) && jl_egal(elt, type_i))
                continue;
            // TODO: other code paths that could reach here?
            JL_GC_POP();
            return 0;
        }
        else if (jl_is_kind(type_i)) {
            JL_GC_POP();
            return 0;
        }

        // `elt` can be either equal representation of `Type` (specializations are
        // deduplicated by type-equality): both take the `jl_types_equal(elt,
        // jl_type_type)` path; an `AnyType` elt must not reach `jl_some_Type_T` below
        if (jl_is_some_Type(jl_unwrap_unionall(elt)) || elt == (jl_value_t*)jl_anytype_type) {
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
            if (!jl_is_datatype(elt) && !jl_is_some_Type(elt)) {
                JL_GC_POP();
                return 0;
            }

            // if the declared type was not Any or Union{Type, ...},
            // then the match must been with kind, such as UnionAll or DataType,
            // and the result of matching the type signature
            // needs to be corrected to the concrete type 'kind' (and not to Type)
            jl_value_t *kind = jl_typeof(jl_some_Type_T(elt));
            if (kind == jl_bottom_type ||
                (jl_is_typeeq(elt) && jl_is_bottom_singleton_class(jl_typeeq_T(elt)))) {
                JL_GC_POP();
                return 0; // the bottom singleton class is under no single kind
            }
            if (!jl_has_free_typevars(decl_i) &&
                    jl_subtype(kind, type_i) && !jl_subtype((jl_value_t*)jl_type_type, type_i)) {
                JL_GC_POP();
                return 0; // gets turned into a kind
            }

            else if (jl_is_some_Type(jl_some_Type_T(elt)) &&
                     // give up on specializing static parameters for Type{Type{Type{...}}}
                     (jl_is_some_Type(jl_some_Type_T(jl_some_Type_T(elt))) || !jl_has_free_typevars(decl_i))) {
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
        if (notcalled_func && jl_subtype((jl_value_t*)jl_function_type, type_i)) {
            // and attempt to despecialize types marked as a supertype of Function (i.e.
            // Function, Callable, Any, or a Union{Function, T})
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
            if (!(jl_is_some_Type(a) && jl_typeof(jl_some_Type_T(a)) == decl))
                return 0;
        }
    }
    return 1;
}

// if available, returns a TypeMapEntry in the "leafcache" that matches `tt` (by type-equality) and is valid during `world`
static inline jl_typemap_entry_t *lookup_leafcache(jl_genericmemory_t *leafcache JL_PROPAGATES_ROOT, jl_value_t *tt, size_t world) JL_NOTSAFEPOINT
{
    jl_typemap_entry_t *entry = (jl_typemap_entry_t*)jl_eqtable_get(leafcache, (jl_value_t*)tt, NULL);
    if (entry) {
        // search tail of the linked-list (including the returned entry) for an entry intersecting world
        //
        // n.b. this entire chain is type-equal to tt (by construction), so it is unnecessary to call `tt<:entry->sig`
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

static jl_typemap_entry_t *mt_find_cache_entry(_Atomic(jl_typemap_t*) *cache JL_PROPAGATES_ROOT, _Atomic(jl_genericmemory_t*) *leafcache JL_PROPAGATES_ROOT, jl_datatype_t *tt, size_t world, int offs)
{
    if (leafcache) {
        jl_typemap_entry_t *entry = lookup_leafcache(jl_atomic_load_relaxed(leafcache), (jl_value_t*)tt, world);
        if (entry)
            return entry;
    }
    struct jl_typemap_assoc search = {(jl_value_t*)tt, world, NULL};
    assert(cache);
    jl_typemap_entry_t *entry = jl_typemap_assoc_by_type(jl_atomic_load_relaxed(cache), &search, offs, /*subtype*/1);
    return entry;
}

JL_DLLEXPORT jl_typemap_entry_t *jl_mt_find_cache_entry(jl_methcache_t *cache, jl_datatype_t *tt, size_t world)
{ // exported only for debugging purposes, not for casual use
    return mt_find_cache_entry(&cache->cache, &cache->leafcache, tt, world, jl_cachearg_offset());
}

jl_value_t *compute_simplett(jl_tupletype_t *cachett)
{
    // now scan `cachett` and ensure that `Type{T}` in the cache will be matched exactly by `typeof(T)`
    // and also reduce the complexity of rejecting this entry in the cache
    // by replacing non-simple types with jl_any_type to build a new `type`
    // (for example, if the signature contains jl_function_type)
    // TODO: this is also related to how we should handle partial matches
    //       (which currently might miss detection of a MethodError)
    jl_value_t *simplett = jl_nothing;
    cachett = (jl_tupletype_t*) jl_unwrap_unionall((jl_value_t*)cachett);
    if (!jl_is_datatype(cachett))
        return simplett;
    size_t i, np = jl_nparams(cachett);
    jl_svec_t *newparams = NULL;
    JL_GC_PUSH1(&newparams);
    for (i = 0; i < np; i++) {
        jl_value_t *elt = jl_svecref(cachett->parameters, i);
        if (jl_is_vararg(elt)) {
        }
        else if (jl_is_some_Type(elt)) {
            // TODO: if (!jl_is_singleton(elt)) ...
            jl_value_t *kind = jl_typeof(jl_some_Type_T(elt));
            if (!newparams) newparams = jl_svec_copy(cachett->parameters);
            jl_svecset(newparams, i, kind);
        }
        else if (!jl_is_concrete_type(elt)) { // for example, jl_function_type or jl_tuple_type
            if (!newparams) newparams = jl_svec_copy(cachett->parameters);
            jl_svecset(newparams, i, jl_any_type);
        }
    }
    if (newparams)
        simplett = jl_apply_tuple_type(newparams, 1);
    JL_GC_POP();
    return simplett;
}

static void cache_insert(
        jl_methtable_t *mt, jl_methcache_t *mc, _Atomic(jl_typemap_t*) *cache, jl_value_t *parent JL_PROPAGATES_ROOT,
        jl_method_t *definition,
        jl_tupletype_t *tt, // the original tupletype of the signature
        size_t min_valid, size_t max_valid, size_t current_world,
        jl_tupletype_t *cachett,
        jl_svec_t *guardsigs,
        jl_method_instance_t *newmeth,
        int offs)
{
    // exact-dispatch lookups (`jl_typemap_entry_assoc_exact`) require datatype sigs
    assert(jl_is_datatype(cachett));
    int unconstrained_max = max_valid == ~(size_t)0;
    if (max_valid > current_world)
        max_valid = current_world;
    jl_datatype_t *simplett = NULL;
    jl_typemap_entry_t *newentry = NULL;
    JL_GC_PUSH2(&simplett, &newentry);
    simplett = (jl_datatype_t*)compute_simplett(cachett);
    newentry = jl_typemap_alloc(cachett, simplett, guardsigs, (jl_value_t*)newmeth, min_valid, max_valid);
    if (mc && tt && cachett == tt && tt->hash && !tt->hasfreetypevars) {
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
        jl_genericmemory_t *oldcache = jl_atomic_load_relaxed(&mc->leafcache);
        jl_typemap_entry_t *old = (jl_typemap_entry_t*)jl_eqtable_get(oldcache, (jl_value_t*)tt, jl_nothing);
        jl_gc_write_atomic(newentry, newentry->next, jl_typemap_entry_t, old, relaxed);
        jl_genericmemory_t *newcache = jl_eqtable_put(jl_atomic_load_relaxed(&mc->leafcache), (jl_value_t*)tt, (jl_value_t*)newentry, NULL);
        if (newcache != oldcache) {
            jl_gc_write_atomic(mc, mc->leafcache, jl_genericmemory_t, newcache, release);
        }
    }
    else {
         jl_typemap_insert(cache, parent, newentry, offs);
         if (mt) {
             jl_typename_t *tn = jl_nth_argument_datatypename((jl_value_t*)(tt ? tt : cachett), 1);
             if (tn) {
                 int cache_entry_count = jl_atomic_load_relaxed(&tn->cache_entry_count);
                 if (cache_entry_count < 31)
                     jl_atomic_store_relaxed(&tn->cache_entry_count, cache_entry_count + 1);
             }
         }
    }
    if (mc) {
        jl_method_cache_inserted();
        JL_UNLOCK(&mc->writelock); // before acquiring world_counter_lock

        // Only set METHOD_SIG_LATEST_ONLY on method instance if method does NOT have the bit, no guards required, and min_valid == primary_world
        int should_set_dispatch_status = !(jl_atomic_load_relaxed(&definition->dispatch_status) & METHOD_SIG_LATEST_ONLY) &&
            (jl_value_t*)cachett == newmeth->specTypes && jl_svec_len(guardsigs) == 0 &&
            min_valid == jl_atomic_load_relaxed(&definition->primary_world) &&
            !(jl_atomic_load_relaxed(&newmeth->dispatch_status) & METHOD_SIG_LATEST_ONLY);

        // Combined trylock for both dispatch_status setting and max_world restoration
        if ((should_set_dispatch_status || unconstrained_max) &&
            jl_atomic_load_relaxed(&jl_world_counter) == current_world) {
            JL_LOCK(&world_counter_lock);
            if (jl_atomic_load_relaxed(&jl_world_counter) == current_world) {
                if (should_set_dispatch_status) {
                    jl_atomic_store_relaxed(&newmeth->dispatch_status, METHOD_SIG_LATEST_ONLY);
                }
                if (unconstrained_max) {
                    jl_atomic_store_relaxed(&newentry->max_world, ~(size_t)0);
                }
            }
            JL_UNLOCK(&world_counter_lock);
        }
    }

    JL_GC_POP();
}

static jl_method_instance_t *cache_result(
        jl_methtable_t *mt, jl_methcache_t *mc, _Atomic(jl_typemap_t*) *cache, jl_value_t *parent JL_PROPAGATES_ROOT,
        jl_tupletype_t *tt, // the original tupletype of the signature
        jl_method_t *definition,
        size_t world, size_t min_valid, size_t max_valid, size_t current_world,
        jl_svec_t *sparams,
        // set by callers that have already proven `tt` is absent from the cache
        // immediately before acquiring the lock and that no insertion can have
        // happened since, allowing the redundant re-check below to be skipped
        int tt_known_absent)
{
    // caller must hold the parent->writelock, which this releases
    int8_t offs = mc ? jl_cachearg_offset() : 1;
    // short-circuit (now that we hold the lock) if this entry is already present
    if (!tt_known_absent) {
        jl_typemap_entry_t *entry = mt_find_cache_entry(cache, mc ? &mc->leafcache : NULL, tt, world, offs);
        if (entry) {
            if (mc) JL_UNLOCK(&mc->writelock);
            return entry->func.linfo;
        }
    }

    jl_method_instance_t *newmeth = NULL;
    if (jl_is_builtinfunc(definition)) {
        newmeth = jl_atomic_load_relaxed(&definition->unspecialized);
        assert(newmeth != NULL); // handle builtin methods de-specialization (for invoke, or if the global cache entry somehow gets lost)
        jl_tupletype_t *cachett = (jl_tupletype_t*)definition->sig;
        jl_datatype_t *simplett = NULL;
        jl_typemap_entry_t *newentry = jl_typemap_alloc(cachett, simplett, jl_emptysvec, (jl_value_t*)newmeth, min_valid, max_valid);
        JL_GC_PUSH1(&newentry);
        jl_typemap_insert(cache, parent, newentry, offs);
        if (mc)
            jl_method_cache_inserted();
        JL_GC_POP();
        if (mc) JL_UNLOCK(&mc->writelock);
        return newmeth;
    }

    newmeth = jl_specializations_get_linfo(definition, (jl_value_t*)tt, sparams);
    JL_GC_PUSH1(&newmeth);
    cache_insert(mt, mc, cache, parent, definition, tt, min_valid, max_valid, current_world, tt,
        jl_emptysvec, newmeth, offs);
    JL_GC_POP();
    return newmeth;
}

static void recache_method(
        jl_methtable_t *mt, jl_methcache_t *mc, _Atomic(jl_typemap_t*) *cache, jl_value_t *parent JL_PROPAGATES_ROOT,
        jl_tupletype_t *tt, // the original tupletype of the signature
        jl_method_t *definition,
        size_t world, size_t min_valid, size_t max_valid, size_t current_world,
        jl_svec_t *sparams,
        jl_method_instance_t *newmeth,
        jl_value_t *compilationsig)
{
    // caller must hold the parent->writelock, which this releases
    int8_t offs = mc ? jl_cachearg_offset() : 1;
    // check each cache this might be present in, and update it there
    // TODO: should/how do we check min/max valid on the previous entry before updating to newmeth?
    int orig_in_cache = 0;
    if (mc && tt != NULL) {
        jl_typemap_entry_t *entry = lookup_leafcache(jl_atomic_load_relaxed(&mc->leafcache), (jl_value_t*)tt, world);
        if (entry) {
            jl_gc_write(entry, entry->func.linfo, jl_method_instance_t, newmeth);
            orig_in_cache = 1;
            if (jl_egal((jl_value_t*)tt, (jl_value_t*)newmeth->specTypes)) {
                if (mc) JL_UNLOCK(&mc->writelock);
                return; // cache entry already sufficient
            }
        }
    }
    { // scope block
        struct jl_typemap_assoc search = {tt ? (jl_value_t*)tt : compilationsig, world, NULL};
        assert(cache);
        jl_typemap_entry_t *entry = jl_typemap_assoc_by_type(jl_atomic_load_relaxed(cache), &search, offs, /*subtype*/1);
        if (entry && jl_subtype((jl_value_t*)entry->sig, (jl_value_t*)newmeth->specTypes)) {
            jl_gc_write(entry, entry->func.linfo, jl_method_instance_t, newmeth);
            if (entry->simplesig == (void*)jl_nothing || jl_egal((jl_value_t*)entry->simplesig, compute_simplett((jl_tupletype_t*)newmeth->specTypes))) {
                if (mc) JL_UNLOCK(&mc->writelock);
                return; // cache entry already sufficient
            }
        }
    }

    // cache it generically too, if valid
    int cache_with_orig = newmeth->cache_with_orig;
    if (!cache_with_orig && !jl_egal((jl_value_t*)sparams, (jl_value_t*)newmeth->sparam_vals))
        cache_with_orig = 1;
    if (!cache_with_orig && !jl_subtype(compilationsig, (jl_value_t*)definition->sig))
        // TODO: use (compilationsig = definition->sig; cache_with_orig = jl_is_unionall(definition->sig);) here instead?
        cache_with_orig = 1;
    if (!cache_with_orig && (!jl_is_datatype(compilationsig) || ((jl_datatype_t*)compilationsig)->hasfreetypevars))
        // a UnionAll or free-typevar signature (e.g. the specTypes of a sig-widened
        // MethodInstance) can never serve as an exact-dispatch cache key
        cache_with_orig = 1;
    if (cache_with_orig && (orig_in_cache || tt == NULL)) {
        if (mc) JL_UNLOCK(&mc->writelock);
        return; // leafcache entry alone is sufficient (or no orig tt to cache with)
    }

    jl_value_t *matches = NULL;
    jl_svec_t *guardsigs = jl_emptysvec;
    JL_GC_PUSH2(&matches, &guardsigs);

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
    jl_value_t *cachett = (jl_value_t*)tt;
    if (!cache_with_orig && mt) {
        // now examine what will happen if we chose to use this sig in the cache
        size_t min_valid2 = 1;
        size_t max_valid2 = ~(size_t)0;
        // TODO: check if inferences is empty, before doing the filtered lookup
        matches = ml_matches(mt, mc, (jl_tupletype_t*)compilationsig, MAX_UNSPECIALIZED_CONFLICTS, 1, 1, world, 0, &min_valid2, &max_valid2, NULL);
        int guards = 0;
        if (matches == jl_nothing) {
            cache_with_orig = 1;
        }
        else {
            int unmatched_tvars = 0;
            size_t i, l = jl_array_nrows(matches);
            for (i = 0; i < l; i++) {
                jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(matches, i);
                if (matc->method == definition)
                    continue;
                jl_svec_t *env = matc->sparams;
                int k, l;
                for (k = 0, l = jl_svec_len(env); k < l; k++) {
                    jl_value_t *env_k = jl_svecref(env, k);
                    if (jl_is_svec(env_k) || jl_has_free_typevars(env_k) || jl_is_vararg(env_k)) {
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
            guards = 0;
            for (i = 0, l = jl_array_nrows(matches); i < l; i++) {
                jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(matches, i);
                jl_method_t *other = matc->method;
                if (other != definition) {
                    jl_svecset(guardsigs, guards, matc->spec_types);
                    guards++;
                    // alternative approach: insert sentinel entry
                    //jl_typemap_insert(cache, parent, (jl_tupletype_t*)matc->spec_types,
                    //        NULL, jl_emptysvec, /*guard*/NULL, jl_cachearg_offset(), other->min_world, other->max_world);
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
            if (orig_in_cache || tt == NULL) {
                if (mc) JL_UNLOCK(&mc->writelock);
                JL_GC_POP();
                return; // leafcache entry alone is sufficient (or no orig tt to cache with)
            }
        }
    }

    cache_insert(mt, mc, cache, parent, definition, tt, min_valid, max_valid, current_world, (jl_tupletype_t*)cachett,
        guardsigs, newmeth, offs);
    JL_GC_POP();
    return;
}

static jl_method_match_t *_gf_invoke_lookup(jl_value_t *types JL_PROPAGATES_ROOT, jl_methtable_t *mt, size_t world, int cache_result_recursion, size_t *min_valid, size_t *max_valid);

static void promote_cache_method(jl_value_t *F, jl_value_t **args, uint32_t nargs, size_t world,
    jl_method_instance_t *newmeth, jl_value_t *compilationsig,
    enum internal_compilation_triggers cause)
{
    if (F == NULL)
        return;
    if (cause == TRIGGER_DISPATCH) {
        jl_tupletype_t *tt = arg_type_tuple(F, args, nargs + 1);
        jl_method_match_t *matc = NULL;
        JL_GC_PUSH2(&tt, &matc);
        jl_methtable_t *mt = jl_method_table;
        size_t current_world = jl_atomic_load_acquire(&jl_world_counter);
        size_t min_valid = 0;
        size_t max_valid = ~(size_t)0;
        matc = _gf_invoke_lookup((jl_value_t*)tt, mt, world, 0, &min_valid, &max_valid);
        jl_method_t *definition = newmeth->def.method;
        if (matc && matc->method == definition) {
            jl_methcache_t *mc = mt->cache;
            JL_LOCK(&mc->writelock);
            recache_method(
                mt, mc, &mc->cache, (jl_value_t*)mc, tt, definition, world,
                min_valid, max_valid, current_world, matc->sparams, newmeth, compilationsig);
        }
        JL_GC_POP();
    }
    else if (cause == TRIGGER_INVOKE) {
        jl_tupletype_t *tt = arg_type_tuple(F, args, nargs + 1);
        JL_GC_PUSH1(&tt);
        jl_method_t *definition = newmeth->def.method;
        JL_LOCK(&definition->writelock);
        recache_method(
            NULL, NULL, &definition->invokes, (jl_value_t*)definition, tt, definition,
            1, 1, 1, 1, newmeth->sparam_vals, newmeth, compilationsig);
        JL_UNLOCK(&definition->writelock);
        JL_GC_POP();
    }
}

// Like promote_cache_method(TRIGGER_DISPATCH) but takes the type tuple directly.
// Called from the abstract interpreter after inferring a compilation signature,
// to record in the dispatch/ml_lookup cache that dispatching/lookup of `tt` should use
// `newmeth` to avoid making unnecessary new types for cache_result.
JL_DLLEXPORT void jl_recache_method_by_type(jl_value_t *tt,
    jl_method_instance_t *newmeth, jl_value_t *compilationsig, size_t world,
    size_t min_valid, size_t max_valid, size_t current_world)
{
    if (newmeth->cache_with_orig)
        return;
    jl_method_t *definition = newmeth->def.method;
    if (!jl_is_dispatch_tupletype(tt))
        tt = NULL;
    jl_methtable_t *mt = jl_method_table;
    jl_methcache_t *mc = mt->cache;
    JL_LOCK(&mc->writelock);
    recache_method(mt, mc, &mc->cache, (jl_value_t*)mc, (jl_tupletype_t*)tt, definition, world,
        min_valid, max_valid, current_world,
        newmeth->sparam_vals, newmeth, compilationsig);
}

JL_DLLEXPORT void jl_promote_cis_to_current(jl_code_instance_t **cis, size_t n, size_t validated_world)
{
    size_t current_world = jl_atomic_load_relaxed(&jl_world_counter);
    // No need to acquire the lock if we've been invalidated anyway
    if (current_world > validated_world)
        return;
    JL_LOCK(&world_counter_lock);
    current_world = jl_atomic_load_relaxed(&jl_world_counter);
    if (current_world == validated_world) {
        arraylist_t workqueue;
        arraylist_new(&workqueue, 0);
        for (size_t i = 0; i < n; i++)
            arraylist_push(&workqueue, cis[i]);
        while (workqueue.len > 0) {
            jl_code_instance_t *current_ci = (jl_code_instance_t *)arraylist_pop(&workqueue);
            if (jl_atomic_load_relaxed(&current_ci->max_world) != validated_world)
                continue;
            jl_atomic_store_relaxed(&current_ci->max_world, ~(size_t)0);
            jl_value_t *edges = (jl_value_t*)jl_atomic_load_relaxed(&current_ci->edges);
            jl_interned_code_instance_t *iedges =
                edges != NULL && jl_typetagis(edges, jl_interned_code_instance_type) ?
                (jl_interned_code_instance_t*)edges : NULL;
            size_t nedges = edges == NULL ? 0 : iedges ? iedges->nedges : jl_svec_len(edges);
            for (size_t i = 0; i < nedges; i++) {
                jl_value_t *edge = iedges ? jl_ici_ref_nobox(iedges, i) : jl_svecref(edges, i);
                if (edge == NULL || !jl_is_code_instance(edge))
                    continue;
                arraylist_push(&workqueue, edge);
            }
        }
        arraylist_free(&workqueue);
    }
    JL_UNLOCK(&world_counter_lock);
}

JL_DLLEXPORT void jl_promote_ci_to_current(jl_code_instance_t *ci, size_t validated_world)
{
    jl_promote_cis_to_current(&ci, 1, validated_world);
}

JL_DLLEXPORT void jl_promote_mi_to_current(jl_method_instance_t *mi, size_t min_world, size_t validated_world)
{
    size_t current_world = jl_atomic_load_relaxed(&jl_world_counter);
    // No need to acquire the lock if we've been invalidated anyway
    if (current_world > validated_world)
        return;
    // Only set METHOD_SIG_LATEST_ONLY on method instance if method does NOT have the bit and min_valid == primary_world
    jl_method_t *definition = mi->def.method;
    if ((jl_atomic_load_relaxed(&definition->dispatch_status) & METHOD_SIG_LATEST_ONLY) ||
        min_world != jl_atomic_load_relaxed(&definition->primary_world) ||
        (jl_atomic_load_relaxed(&mi->dispatch_status) & METHOD_SIG_LATEST_ONLY))
        return;
    JL_LOCK(&world_counter_lock);
    current_world = jl_atomic_load_relaxed(&jl_world_counter);
    if (current_world == validated_world) {
        jl_atomic_store_relaxed(&mi->dispatch_status, METHOD_SIG_LATEST_ONLY);
    }
    JL_UNLOCK(&world_counter_lock);
}

static jl_method_instance_t *jl_mt_assoc_by_type(
    jl_methtable_t *mt, jl_methcache_t *mc JL_PROPAGATES_ROOT, jl_datatype_t *tt, size_t world)
{
    size_t cache_insert_generation = jl_method_cache_insert_generation_load();
    jl_typemap_entry_t *entry = mt_find_cache_entry(&mc->cache,
         tt->isdispatchtuple ? &mc->leafcache : NULL,
         tt, world, jl_cachearg_offset());
    if (entry)
        return entry->func.linfo;
    assert(tt->isdispatchtuple || tt->hasfreetypevars);
    JL_TIMING(METHOD_LOOKUP_SLOW, METHOD_LOOKUP_SLOW);
    jl_method_match_t *matc = NULL;
    JL_LOCK(&mc->writelock);
    jl_method_instance_t *mi = NULL;
    // No need to re-check if no method cache insertion happened since the
    // lock-free probe above.
    int tt_known_absent = jl_method_cache_insert_generation_load() == cache_insert_generation;
    if (!tt_known_absent) {
        entry = mt_find_cache_entry(&mc->cache,
         tt->isdispatchtuple ? &mc->leafcache : NULL,
         tt, world, jl_cachearg_offset());
        if (entry)
            mi = entry->func.linfo;
    }
    if (!mi) {
        size_t current_world = jl_atomic_load_acquire(&jl_world_counter);
        size_t min_valid = 0;
        size_t max_valid = ~(size_t)0;
        matc = _gf_invoke_lookup((jl_value_t*)tt, mt, world, 0, &min_valid, &max_valid);
        if (matc) {
            JL_GC_PUSH1(&matc);
            jl_method_t *m = matc->method;
            jl_svec_t *env = matc->sparams;
            // TODO: get mi from jl_specializations_get_linfo?
            mi = cache_result(mt, mc, &mc->cache, (jl_value_t*)mc, tt, m, world, min_valid, max_valid, current_world, env, tt_known_absent);
            JL_GC_POP();
            return mi;
        }
    }
    JL_UNLOCK(&mc->writelock);
    return mi;
}

struct matches_env {
    struct typemap_intersection_env match;
    jl_typemap_entry_t *newentry;
    jl_value_t *shadowed;
    jl_typemap_entry_t *replaced;
};

// nonzero while scanning for an activation whose typename contributors were
// all within the loading image's dependency closure (measurement)
static int current_activation_clean = 0;
static int method_in_loading_closure(jl_method_t *m);
static void record_backedge_log(jl_value_t *target, jl_value_t *invokesig, jl_value_t *caller);
static void jl_method_table_add_backedge_batch(jl_value_t *typ, jl_value_t **callers, size_t n);
static int sig_tns_enabled(void);

static int get_intersect_visitor(jl_typemap_entry_t *oldentry, struct typemap_intersection_env *closure0)
{
    struct matches_env *closure = container_of(closure0, struct matches_env, match);
    jl_method_t *oldmethod = oldentry->func.method;
    if (current_activation_clean) {
        jl_contrib_stats[4]++;
        if (!method_in_loading_closure(oldmethod))
            jl_contrib_stats[3]++; // invariant violation: clean typename, foreign method
    }
    assert(oldentry != closure->newentry && "entry already added");
    assert(jl_atomic_load_relaxed(&oldentry->min_world) <= jl_atomic_load_relaxed(&closure->newentry->min_world) && "old method cannot be newer than new method");
    //assert(jl_atomic_load_relaxed(&oldentry->max_world) != jl_atomic_load_relaxed(&closure->newentry->min_world) && "method cannot be added at the same time as method deleted");
    assert((jl_atomic_load_relaxed(&oldentry->max_world) == ~(size_t)0));
    // don't need to consider other similar methods if this oldentry will always fully intersect with them and dominates all of them
    if (closure->match.issubty // e.g. jl_subtype(closure->newentry.sig, oldentry->sig)
        && jl_subtype(oldmethod->sig, (jl_value_t*)closure->newentry->sig)) { // e.g. jl_type_equal(closure->newentry->sig, oldentry->sig)
        if (closure->replaced == NULL || jl_atomic_load_relaxed(&closure->replaced->min_world) < jl_atomic_load_relaxed(&oldentry->min_world))
            closure->replaced = oldentry; // must pick the newest insertion (both are still valid)
    }
    if (closure->shadowed == NULL)
        closure->shadowed = (jl_value_t*)jl_alloc_vec_any(0);
    // This should be rarely true (in fact, get_intersect_visitor should be
    // rarely true), but might as well skip the rest of the scan fast anyways
    // since we can.
    if (closure->match.issubty) {
        int only = jl_atomic_load_relaxed(&oldmethod->dispatch_status) & METHOD_SIG_LATEST_ONLY;
        if (only) {
            size_t len = jl_array_nrows(closure->shadowed);
            if (len > 0)
                jl_array_del_end((jl_array_t*)closure->shadowed, len);
            jl_array_ptr_1d_push((jl_array_t*)closure->shadowed, (jl_value_t*)oldmethod);
            return 0;
        }
    }
    jl_array_ptr_1d_push((jl_array_t*)closure->shadowed, (jl_value_t*)oldmethod);
    typemap_slurp_search(oldentry, &closure->match);
    return 1;
}

static int intersect_entry_foreign(jl_typemap_entry_t *ml, struct typemap_intersection_env *closure)
{
    // hybrid certificate scan: only look at methods the precompile worker
    // could not have seen (outside the loading image's dependency closure)
    return !method_in_loading_closure(ml->func.method);
}

static jl_value_t *get_intersect_matches(jl_typemap_t *defs, jl_typemap_entry_t *newentry, jl_typemap_entry_t **replaced, size_t world, int foreign_only)
{
    JL_TIMING(ADD_METHOD, ACTIVATE_IsectScan);
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
            /* .ti = */ NULL, /* .env = */ NULL, /* .issubty = */ 0,
            /* .emptiness_only = */ 1,
            /* .entry_filter = */ foreign_only ? intersect_entry_foreign : NULL},
        /* .newentry = */ newentry, /* .shadowed */ NULL, /* .replaced */ NULL};
    JL_GC_PUSH3(&env.match.env, &env.match.ti, &env.shadowed);
    jl_typemap_intersection_visitor(defs, 0, &env.match);
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
    jl_typename_t *tn = jl_nth_argument_datatypename(oldvalue->sig, 1);
    if (jl_kwcall_type && tn == jl_kwcall_type->name)
        tn = jl_nth_argument_datatypename(oldvalue->sig, 3);
    int anon = tn && is_anonfn_typename(jl_symbol_name(tn->name));
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

static void update_max_args(jl_value_t *type)
{
    type = jl_unwrap_unionall(type);
    jl_typename_t *tn = jl_nth_argument_datatypename(type, 1);
    if (tn == NULL || (jl_kwcall_type && tn == jl_kwcall_type->name))
        return;
    assert(jl_is_datatype(type));
    size_t na = jl_nparams(type);
    if (jl_va_tuple_kind((jl_datatype_t*)type) == JL_VARARG_UNBOUND)
        na--;
    // update occurs inside global writelock
    if (na > jl_atomic_load_relaxed(&tn->max_args))
        jl_atomic_store_relaxed(&tn->max_args, na);
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
        // TODO: should we visit all forward edges now and delete ourself from all of those lists too?
    }
    else {
        assert(jl_atomic_load_relaxed(&replaced->max_world) <= max_world);
    }
    JL_UNLOCK(&replaced_mi->def.method->writelock);
}

JL_DLLEXPORT void jl_invalidate_code_instance(jl_code_instance_t *replaced, size_t max_world)
{
    invalidate_code_instance(replaced, max_world, 1);
}

JL_DLLEXPORT void jl_maybe_log_binding_invalidation(jl_value_t *replaced)
{
    if (_jl_debug_method_invalidation) {
        if (replaced) {
            jl_array_ptr_1d_push(_jl_debug_method_invalidation, replaced);
        }
        jl_value_t *loctag = jl_cstr_to_string("jl_maybe_log_binding_invalidation");
        JL_GC_PUSH1(&loctag);
        jl_array_ptr_1d_push(_jl_debug_method_invalidation, loctag);
        JL_GC_POP();
    }
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
        jl_gc_wb(replaced_mi, NULL);
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
        if (replaced_ci) {
            // If we're invalidating a particular codeinstance, only invalidate
            // this backedge it actually has an edge for our codeinstance.
            jl_value_t *edges = (jl_value_t*)jl_atomic_load_relaxed(&replaced->edges);
            jl_interned_code_instance_t *iedges =
                edges != NULL && jl_typetagis(edges, jl_interned_code_instance_type) ?
                (jl_interned_code_instance_t*)edges : NULL;
            size_t nedges = edges == NULL ? 0 : iedges ? iedges->nedges : jl_svec_len(edges);
            for (size_t j = 0; j < nedges; ++j) {
                jl_value_t *edge = iedges ? jl_ici_ref_nobox(iedges, j) : jl_svecref(edges, j);
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

static int jl_type_intersection2(jl_value_t *t1, jl_value_t *t2, jl_value_t **isect JL_REQUIRE_ROOTED_SLOT, jl_value_t **isect2 JL_REQUIRE_ROOTED_SLOT)
{
    *isect2 = NULL;
    // Fast path: a dispatch tuple is a concrete leaf type, so its intersection with any
    // other type is just itself (when it is a subtype) or empty. This avoids full type
    // intersection for the common case of concrete specialization/backedge signatures.
    if (jl_is_dispatch_tupletype(t2)) {
        if (jl_subtype(t2, t1)) {
            *isect = t2;
            return 1;
        }
        *isect = jl_bottom_type;
        return 0;
    }
    if (jl_is_dispatch_tupletype(t1)) {
        if (jl_subtype(t1, t2)) {
            *isect = t1;
            return 1;
        }
        *isect = jl_bottom_type;
        return 0;
    }
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
    if (jl_types_struct_equiv(*isect2, *isect)) {
        *isect2 = NULL;
    }
    return 1;
}


// check if `type` is replacing `m` with an ambiguity here, given other methods in `d` that already match it
static int is_replacing(char ambig, jl_value_t *type, jl_method_t *m, jl_method_t *const *d, size_t n, jl_value_t *isect, jl_value_t *isect2, char *morespec)
{
    size_t k;
    for (k = 0; k < n; k++) {
        jl_method_t *m2 = d[k];
        // see if m2 also fully covered this intersection
        if (m == m2 || !(jl_subtype(isect, m2->sig) || (isect2 && jl_subtype(isect2, m2->sig))))
            continue;
        if (morespec[k])
            // not actually shadowing this--m2 will still be better
            return 0;
        // if type is not more specific than m (thus now dominating it)
        // then there is a new ambiguity here,
        // since m2 was also a previous match over isect,
        // see if m was previously dominant over all m2
        // or if this was already ambiguous before
        if (ambig && !jl_type_morespecific(m->sig, m2->sig)) {
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
    jl_value_t *invokeTypes = NULL;
    jl_code_instance_t *caller = NULL;
    JL_GC_PUSH2(&caller, &invokeTypes);
    int invalidated_any = 0;
    while (mi->backedges && ib < nb) {
        ib = get_next_edge(backedges, ib, &invokeTypes, &caller);
        if (!caller) {
            insb = ib;
            continue;
        }
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
    JL_GC_POP();
    jl_mi_done_backedges(mi, backedge_recursion_flags);
    return invalidated_any;
}

// invalidate cached methods that overlap this definition
static void invalidate_backedges(jl_method_instance_t *replaced_mi, size_t max_world, const char *why)
{
    JL_TIMING(ADD_METHOD, INVALIDATE_Backedges);
    // Reset dispatch_status when method instance is replaced
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
    jl_atomic_store_relaxed(&replaced_mi->dispatch_status, 0);
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
        jl_array_t *backedges = jl_mi_get_backedges(callee);
        // TODO: use jl_cache_type_(invokesig) like cache_insert does to save memory
        if (!backedges) {
            // lazy-init the backedges array
            backedges = jl_alloc_vec_any(0);
            jl_gc_write(callee, callee->backedges, jl_array_t, backedges);
        }
        push_edge(backedges, invokesig, caller);
        record_backedge_log((jl_value_t*)callee, invokesig, (jl_value_t*)caller);
    }
    JL_UNLOCK(&callee->def.method->writelock);
}


int jl_foreach_top_typename_for(void (*f)(jl_typename_t*, int, void*), jl_value_t *argtypes JL_PROPAGATES_ROOT, int all_subtypes, void *env);

struct _typename_add_backedge {
    jl_value_t *typ;
    jl_value_t *caller;
};

static void _typename_add_backedge(jl_typename_t *tn, int explct, void *env0)
{
    struct _typename_add_backedge *env = (struct _typename_add_backedge*)env0;
    JL_GC_PROMISE_ROOTED(env->typ);
    JL_GC_PROMISE_ROOTED(env->caller);
    if (!explct)
        return;
    jl_genericmemory_t *allbackedges = jl_method_table->backedges;
    jl_array_t *backedges = (jl_array_t*)jl_eqtable_get(allbackedges, (jl_value_t*)tn, NULL);
    if (backedges == NULL) {
        backedges = jl_alloc_vec_any(2);
        JL_GC_PUSH1(&backedges);
        jl_array_del_end(backedges, 2);
        jl_genericmemory_t *newtable = jl_eqtable_put(allbackedges, (jl_value_t*)tn, (jl_value_t*)backedges, NULL);
        JL_GC_POP();
        if (newtable != allbackedges) {
            jl_gc_write(jl_method_table, jl_method_table->backedges, jl_genericmemory_t, newtable);
        }
    }
    // check if the edge is already present and avoid adding a duplicate
    size_t i, l = jl_array_nrows(backedges);
    // reuse an already cached instance of this type, if possible
    // TODO: use jl_cache_type_(tt) like cache_insert does, instead of this linear scan?
    // TODO: use as_global_root and de-dup edges array too
    for (i = 1; i < l; i += 2) {
        if (jl_array_ptr_ref(backedges, i) == env->caller) {
            if (jl_types_equal(jl_array_ptr_ref(backedges, i - 1), env->typ)) {
                env->typ = jl_array_ptr_ref(backedges, i - 1);
                return; // this edge already recorded
            }
        }
    }
    for (i = 1; i < l; i += 2) {
        if (jl_array_ptr_ref(backedges, i) != env->caller) {
            if (jl_types_equal(jl_array_ptr_ref(backedges, i - 1), env->typ)) {
                env->typ = jl_array_ptr_ref(backedges, i - 1);
                break;
            }
        }
    }
    jl_array_ptr_1d_push(backedges, env->typ);
    jl_array_ptr_1d_push(backedges, env->caller);
}

// ---- method-table contributor tracking ----
// For every generic function (keyed by the same top-typename decomposition the
// mt-backedge table uses), track which sources have contributed (or deleted)
// methods under it: the linkage blob index for pkgimage-owned methods, -1 for
// run-time sources (eval, deletion). A pkgimage activation whose typenames'
// contributors all lie within the image's dependency closure faces exactly the
// prior world its precompile worker analyzed.
JL_DLLEXPORT jl_genericmemory_t *jl_method_contributors = NULL;
JL_DLLEXPORT jl_genericmemory_t *jl_activation_certs = NULL; // method -> certificate (precompile worker)
JL_DLLEXPORT uint64_t jl_contrib_stats[32];

static int activate_replay_mode(void)
{
    static int mode = -1;
    if (mode == -1) {
        const char *e = getenv("JULIA_ACTIVATE_REPLAY");
        mode = e == NULL ? 0 : (e[0] == '2' ? 2 : (e[0] == '1' ? 1 : 0));
    }
    return mode;
}

static void record_activation_cert(jl_method_t *method, jl_svec_t *cert)
{
    if (jl_activation_certs == NULL) {
        if (jl_an_empty_memory_any == NULL)
            return;
        jl_activation_certs = (jl_genericmemory_t*)jl_an_empty_memory_any;
    }
    jl_genericmemory_t *newtable = jl_eqtable_put(jl_activation_certs, (jl_value_t*)method, (jl_value_t*)cert, NULL);
    if (newtable != jl_activation_certs)
        jl_activation_certs = newtable;
}

JL_DLLEXPORT jl_value_t *jl_get_activation_cert(jl_method_t *method)
{
    if (jl_activation_certs == NULL)
        return jl_nothing;
    jl_value_t *cert = jl_eqtable_get(jl_activation_certs, (jl_value_t*)method, NULL);
    return cert == NULL ? jl_nothing : cert;
}

// Log of backedge registrations performed by this session's own
// CodeInstances, recorded during incremental precompile as flat
// (target, invokesig, caller) triples: target is a MethodInstance, a Binding,
// or `nothing` (a method-table edge, with the signature in the second slot).
// Serialized with the image and bulk-replayed at load for the callers that
// re-validate, replacing the per-CodeInstance edge-list decoding walk.
JL_DLLEXPORT jl_array_t *jl_backedge_log JL_GLOBALLY_ROOTED;
static jl_mutex_t backedge_log_lock;

static void record_backedge_log(jl_value_t *target, jl_value_t *invokesig, jl_value_t *caller)
{
    if (!jl_generating_output() || !jl_options.incremental)
        return;
    if (jl_object_in_image(caller))
        return; // image-owned caller: replayed by its own image's log
    JL_LOCK(&backedge_log_lock);
    if (jl_backedge_log == NULL)
        jl_backedge_log = jl_alloc_vec_any(0);
    jl_array_ptr_1d_push(jl_backedge_log, target);
    jl_array_ptr_1d_push(jl_backedge_log, invokesig == NULL ? jl_nothing : invokesig);
    jl_array_ptr_1d_push(jl_backedge_log, caller);
    JL_UNLOCK(&backedge_log_lock);
}

void jl_record_binding_backedge(jl_binding_t *b, jl_value_t *edge)
{
    // Method-edge binding backedges arise from source scanning, which loads
    // re-run themselves; only CodeInstance callers replay from the log.
    if (jl_is_code_instance(edge))
        record_backedge_log((jl_value_t*)b, NULL, edge);
}

// Reserve capacity on a callee's backedge list ahead of a known burst of
// registrations (grow-then-shrink keeps the buffer).
static void mi_backedges_reserve(jl_method_instance_t *callee, size_t extra)
{
    if (!jl_atomic_load_relaxed(&allow_new_worlds))
        return;
    JL_LOCK(&callee->def.method->writelock);
    if (jl_atomic_load_relaxed(&allow_new_worlds)) {
        jl_array_t *backedges = jl_mi_get_backedges(callee);
        if (!backedges) {
            backedges = jl_alloc_vec_any(0);
            jl_gc_write(callee, callee->backedges, jl_array_t, backedges);
        }
        jl_array_grow_end(backedges, extra);
        jl_array_del_end(backedges, extra);
    }
    JL_UNLOCK(&callee->def.method->writelock);
}

// Re-apply a loaded image's backedge log for the callers that survived
// re-validation (see store_backedges, whose per-CodeInstance work this
// replaces when a log is present). The serialized form is compact:
// log = [unique_objects::Vector{Any}, idxstream::Vector{UInt8}], where the
// stream holds varint-encoded (target, invokesig, caller) index triples —
// this keeps the log's relocation footprint at one entry per unique object.
// The counting pass pre-sizes the backedge arrays: appending edge-by-edge
// re-grows them repeatedly and dominates load-time allocation churn.
JL_DLLEXPORT void jl_apply_backedge_log(jl_array_t *log)
{
    assert(jl_array_nrows(log) == 2);
    jl_array_t *uobjs = (jl_array_t*)jl_array_ptr_ref(log, 0);
    jl_array_t *idxb = (jl_array_t*)jl_array_ptr_ref(log, 1);
    jl_value_t **uo = jl_array_ptr_data(uobjs);
    size_t nuniq = jl_array_nrows(uobjs);
    uint8_t *bytes = jl_array_data(idxb, uint8_t);
    size_t nbytes = jl_array_nrows(idxb);
#define BELOG_NEXT(out) do { \
        size_t v_ = 0; \
        int shift_ = 0; \
        uint8_t c_; \
        do { \
            assert(bp < nbytes); \
            c_ = bytes[bp++]; \
            v_ |= (size_t)(c_ & 0x7f) << shift_; \
            shift_ += 7; \
        } while (c_ & 0x80); \
        assert(v_ < nuniq); \
        (out) = uo[v_]; \
    } while (0)
    // stream = groups of (target, count, count x (invokesig, caller)), sorted
    // by target then invokesig at save; decode each group's live pairs into a
    // scratch, then register with one lock/pre-size per callee and one
    // signature decomposition per method-table run
    size_t scratchcap = 256;
    jl_value_t **scratch = (jl_value_t**)malloc_s(scratchcap * sizeof(jl_value_t*));
    uint64_t tstart = jl_hrtime();
    size_t bp = 0;
    while (bp < nbytes) {
        jl_value_t *target;
        size_t n;
        BELOG_NEXT(target);
        {
            size_t v_ = 0;
            int shift_ = 0;
            uint8_t c_;
            do {
                assert(bp < nbytes);
                c_ = bytes[bp++];
                v_ |= (size_t)(c_ & 0x7f) << shift_;
                shift_ += 7;
            } while (c_ & 0x80);
            n = v_;
        }
        if (2 * n > scratchcap) {
            while (2 * n > scratchcap)
                scratchcap *= 2;
            scratch = (jl_value_t**)realloc_s(scratch, scratchcap * sizeof(jl_value_t*));
        }
        // decode the group, keeping only callers that survived re-validation
        size_t nlive = 0;
        for (size_t k = 0; k < n; k++) {
            jl_value_t *invokesig, *caller;
            BELOG_NEXT(invokesig);
            BELOG_NEXT(caller);
            if (jl_atomic_load_relaxed(&((jl_code_instance_t*)caller)->max_world) != ~(size_t)0) {
                jl_contrib_stats[19]++;
                continue; // did not survive re-validation
            }
            scratch[2 * nlive] = invokesig;
            scratch[2 * nlive + 1] = caller;
            nlive++;
        }
        if (nlive == 0)
            continue;
        if (target == jl_nothing) {
            // runs of identical invokesig are contiguous: one decomposition per run
            uint64_t t1 = jl_hrtime();
            for (size_t k = 0; k < nlive; ) {
                jl_value_t *invokesig = scratch[2 * k];
                size_t e = k;
                while (e < nlive && scratch[2 * e] == invokesig)
                    e++;
                // compact the run's callers in place (pairs -> pointer list)
                for (size_t i = k; i < e; i++)
                    scratch[2 * k + (i - k)] = scratch[2 * i + 1];
                jl_method_table_add_backedge_batch(invokesig, &scratch[2 * k], e - k);
                jl_contrib_stats[17] += e - k;
                k = e;
            }
            jl_contrib_stats[21] += jl_hrtime() - t1;
        }
        else if (jl_is_method_instance(target)) {
            jl_method_instance_t *callee = (jl_method_instance_t*)target;
            uint64_t t1 = jl_hrtime();
            JL_LOCK(&callee->def.method->writelock);
            if (jl_atomic_load_relaxed(&allow_new_worlds)) {
                jl_array_t *backedges = jl_mi_get_backedges(callee);
                if (!backedges) {
                    backedges = jl_alloc_vec_any(0);
                    jl_gc_write(callee, callee->backedges, jl_array_t, backedges);
                }
                // pre-size for the group (grow-then-shrink keeps the buffer)
                jl_array_grow_end(backedges, 2 * nlive);
                jl_array_del_end(backedges, 2 * nlive);
                for (size_t k = 0; k < nlive; k++) {
                    jl_value_t *invokesig = scratch[2 * k];
                    jl_value_t *caller = scratch[2 * k + 1];
                    push_edge(backedges, invokesig == jl_nothing ? NULL : invokesig,
                              (jl_code_instance_t*)caller);
                    record_backedge_log((jl_value_t*)callee,
                                        invokesig == jl_nothing ? NULL : invokesig, caller);
                }
                jl_contrib_stats[16] += nlive;
            }
            JL_UNLOCK(&callee->def.method->writelock);
            jl_contrib_stats[20] += jl_hrtime() - t1;
        }
        else {
            uint64_t t1 = jl_hrtime();
            for (size_t k = 0; k < nlive; k++) {
                jl_value_t *caller = scratch[2 * k + 1];
                jl_contrib_stats[18]++;
                jl_maybe_add_binding_backedge((jl_binding_t*)target, caller,
                                              jl_get_ci_mi((jl_code_instance_t*)caller)->def.method);
            }
            jl_contrib_stats[21] += jl_hrtime() - t1;
        }
    }
    jl_contrib_stats[22] += jl_hrtime() - tstart;
#undef BELOG_NEXT
    free(scratch);
}

static size_t *jl_loading_closure_bits = NULL; // bitset over linkage blobs
static size_t jl_loading_closure_nblobs = 0;
// Generation counter for the per-typename cleanliness memo stored in the
// contributor tag arrays; bumped whenever the closure changes so stale
// verdicts are recomputed. 0 never matches (arrays start at generation 0).
static int32_t jl_loading_closure_gen = 0;

JL_DLLEXPORT void jl_set_loading_closure_blobs(size_t *bits, size_t nblobs)
{
    jl_loading_closure_bits = bits;
    jl_loading_closure_nblobs = nblobs;
    if (++jl_loading_closure_gen <= 0)
        jl_loading_closure_gen = 1;
}

static int blob_in_loading_closure(size_t idx)
{
    if (idx >= jl_loading_closure_nblobs)
        return 0;
    return (jl_loading_closure_bits[idx / (8 * sizeof(size_t))] >>
            (idx % (8 * sizeof(size_t)))) & 1;
}

static int object_in_loading_closure(jl_value_t *v)
{
    if (!jl_object_in_image(v))
        return 0;
    return blob_in_loading_closure(jl_external_blob_index(v));
}

static int method_in_loading_closure(jl_method_t *m)
{
    return object_in_loading_closure((jl_value_t*)m);
}

static void contributor_add_tag(jl_typename_t *tn, int32_t tag)
{
    if (jl_method_contributors == NULL) {
        if (jl_an_empty_memory_any == NULL)
            return; // too early in bootstrap to track (table is per-session anyway)
        jl_method_contributors = (jl_genericmemory_t*)jl_an_empty_memory_any;
    }
    jl_array_t *tags = (jl_array_t*)jl_eqtable_get(jl_method_contributors, (jl_value_t*)tn, NULL);
    if (tags == NULL) {
        // slots [0]=memo generation, [1]=memo verdict; tags start at [2]
        tags = jl_alloc_array_1d(jl_array_int32_type, 2);
        jl_array_data(tags, int32_t)[0] = 0;
        jl_array_data(tags, int32_t)[1] = 0;
        JL_GC_PUSH1(&tags);
        jl_genericmemory_t *newtable = jl_eqtable_put(jl_method_contributors, (jl_value_t*)tn, (jl_value_t*)tags, NULL);
        JL_GC_POP();
        if (newtable != jl_method_contributors)
            jl_method_contributors = newtable;
    }
    size_t l = jl_array_nrows(tags);
    int32_t *d = jl_array_data(tags, int32_t);
    for (size_t i = 2; i < l; i++)
        if (d[i] == tag)
            return;
    jl_array_grow_end(tags, 1);
    d = jl_array_data(tags, int32_t);
    d[l] = tag;
    d[0] = 0; // invalidate the cleanliness memo
}

// Parallel to the tag table: per typename, the session-contributed methods
// themselves (jl_nothing marks a deletion, which poisons the typename). Used
// by the dirty-signature second chance in jl_edge_sig_replayable: when every
// foreign contributor method on a signature's typenames is provably disjoint
// from it, the signature's match set is still unchanged relative to the
// precompile worker. Array layout: [0] boxed closure generation for the memo,
// [1] memoized foreign-method subset (jl_nothing when poisoned or invalid),
// methods from [2].
JL_DLLEXPORT jl_genericmemory_t *jl_method_contributor_methods JL_GLOBALLY_ROOTED;

static void contributor_add_method(jl_typename_t *tn, jl_value_t *m)
{
    if (jl_method_contributor_methods == NULL) {
        if (jl_an_empty_memory_any == NULL)
            return;
        jl_method_contributor_methods = (jl_genericmemory_t*)jl_an_empty_memory_any;
    }
    jl_array_t *ms = (jl_array_t*)jl_eqtable_get(jl_method_contributor_methods, (jl_value_t*)tn, NULL);
    if (ms == NULL) {
        ms = jl_alloc_vec_any(2);
        JL_GC_PUSH1(&ms);
        jl_array_ptr_set(ms, 0, jl_nothing);
        jl_array_ptr_set(ms, 1, jl_nothing);
        jl_genericmemory_t *newtable = jl_eqtable_put(jl_method_contributor_methods, (jl_value_t*)tn, (jl_value_t*)ms, NULL);
        JL_GC_POP();
        if (newtable != jl_method_contributor_methods)
            jl_method_contributor_methods = newtable;
    }
    // methods insert here once per definition, so no dedup scan is needed
    jl_array_ptr_1d_push(ms, m);
    jl_array_ptr_set(ms, 0, jl_nothing); // invalidate the foreign-subset memo
}

struct _contrib_tag {
    int32_t tag;
    jl_value_t *method; // jl_nothing poisons (method deletion)
};

static void _typename_tag_contributor(jl_typename_t *tn, int explct, void *env0)
{
    // like the mt-backedge table: store only under explicitly encountered
    // typenames; checks consult every callback. Constructor signatures get a
    // strict lower bound on their Type var (see jl_strictify_ctor_var), so
    // Type{Union{}} no longer makes unrelated constructor families intersect
    // and the per-family top typename is a complete key.
    if (!explct)
        return;
    struct _contrib_tag *env = (struct _contrib_tag*)env0;
    contributor_add_tag(tn, env->tag);
    contributor_add_method(tn, env->method);
}

static void _typename_check_contributor(jl_typename_t *tn, int explct, void *env0)
{
    int *clean = (int*)env0;
    (void)explct;
    if (!*clean || jl_method_contributors == NULL)
        return;
    jl_array_t *tags = (jl_array_t*)jl_eqtable_get(jl_method_contributors, (jl_value_t*)tn, NULL);
    if (tags == NULL)
        return; // only (pre-session) sysimage contributions
    size_t l = jl_array_nrows(tags);
    int32_t *d = jl_array_data(tags, int32_t);
    if (d[0] == jl_loading_closure_gen) { // memoized for the current closure
        if (!d[1])
            *clean = 0;
        return;
    }
    int tnclean = 1;
    for (size_t i = 2; i < l; i++) {
        if (d[i] < 0 || !blob_in_loading_closure((size_t)d[i])) {
            jl_contrib_stats[d[i] < 0 ? 5 : 6]++; // dirty cause: session / foreign blob
            tnclean = 0;
            break;
        }
    }
    d[0] = jl_loading_closure_gen;
    d[1] = tnclean;
    if (!tnclean)
        *clean = 0;
}

// Is `sig`'s method-matching world provably unchanged relative to the loading
// image's precompile worker? (all contributors to its typenames lie within the
// dependency closure). Returns the replay mode when so, 0 otherwise.
// Bucket key for a tuple slot type: a typename such that two slots with
// different (non-NULL) keys are provably value-disjoint. Non-abstract,
// non-kind datatypes key by their typename (instances carry exactly that
// typename); TypeEq{X} keys by X's typename (its instances are the types
// X{...}, which only kind-typed slots can otherwise contain, and kinds are
// unfilterable). Everything else — abstract, unions, typevars, varargs,
// kinds — returns NULL (unfilterable).
static jl_typename_t *fdisj_slot_key(jl_value_t *t)
{
    while (jl_is_unionall(t))
        t = ((jl_unionall_t*)t)->body;
    if (jl_is_typeeq(t)) {
        jl_value_t *x = jl_typeeq_T(t);
        while (jl_is_unionall(x))
            x = ((jl_unionall_t*)x)->body;
        if (!jl_is_datatype(x) || ((jl_datatype_t*)x)->name->abstract)
            return NULL;
        return ((jl_datatype_t*)x)->name;
    }
    if (!jl_is_datatype(t))
        return NULL;
    jl_datatype_t *dt = (jl_datatype_t*)t;
    if (dt->name == jl_type_typename) {
        // Type{X} is invariant: Type{X} ∩ Type{Y} needs X == Y, so X's base
        // typename partitions soundly even for abstract or UnionAll-valued X.
        // Typevar-valued X stays unkeyed; kind-typed slots (which do intersect
        // Type{X}) never take this branch and stay in the residue.
        jl_value_t *x = jl_tparam0(dt);
        while (jl_is_unionall(x))
            x = ((jl_unionall_t*)x)->body;
        if (!jl_is_datatype(x))
            return NULL;
        jl_contrib_stats[3]++; // DEBUG: Type{X} keying fired
        return ((jl_datatype_t*)x)->name;
    }
    if (dt->name->abstract || jl_is_kind((jl_value_t*)dt))
        return NULL;
    return dt->name;
}

static jl_typename_t *fdisj_sig_key(jl_value_t *sig)
{
    jl_value_t *usig = jl_unwrap_unionall(sig);
    if (!jl_is_datatype(usig) || jl_nparams(usig) < 2)
        return NULL;
    return fdisj_slot_key(jl_tparam(usig, 1));
}

struct _foreign_disjoint {
    jl_value_t *sig;
    int ok;
};

// debug histogram (JULIA_ICI_DEBUG): second-chance load per typename
typedef struct { jl_typename_t *tn; uint64_t tries; uint64_t isects; } fdisj_stat_t;
#define FDISJ_STAT_SZ 4096
static fdisj_stat_t *fdisj_stats = NULL;
static int fdisj_stats_on(void) JL_NOTSAFEPOINT
{
    static int on = -1;
    if (on == -1)
        on = getenv("JULIA_ICI_DEBUG") != NULL;
    return on;
}
static void fdisj_stat_note(jl_typename_t *tn, uint64_t isects) JL_NOTSAFEPOINT
{
    if (!fdisj_stats_on())
        return;
    if (fdisj_stats == NULL)
        fdisj_stats = (fdisj_stat_t*)calloc_s(FDISJ_STAT_SZ * sizeof(fdisj_stat_t));
    size_t idx = (((uintptr_t)tn) * 0x9E3779B97F4A7C15ULL >> 32) & (FDISJ_STAT_SZ - 1);
    for (int probe = 0; probe < 16; probe++) {
        fdisj_stat_t *e = &fdisj_stats[(idx + probe) & (FDISJ_STAT_SZ - 1)];
        if (e->tn == tn || e->tn == NULL) {
            e->tn = tn;
            e->tries++;
            e->isects += isects;
            return;
        }
    }
}
JL_DLLEXPORT void jl_fdisj_stats_dump(void)
{
    if (fdisj_stats == NULL) {
        jl_safe_printf("fdisj stats: none\n");
        return;
    }
    for (size_t i = 0; i < FDISJ_STAT_SZ; i++) {
        fdisj_stat_t *e = &fdisj_stats[i];
        if (e->tn != NULL && e->tries > 200)
            jl_safe_printf("FDISJ %s.%s tries=%zd isects=%zd\n",
                           jl_symbol_name(e->tn->module->name),
                           jl_symbol_name(e->tn->name),
                           (size_t)e->tries, (size_t)e->isects);
    }
}

// key for the function slot: the function type's typename, or for a
// constructor signature the constructed type's base typename (Type and
// TypeEq parameters are invariant, so values under distinct names cannot
// intersect); NULL when the slot is typevar/union/abstract-callable-shaped
static jl_typename_t *fdisj_slot0_key(jl_value_t *sig)
{
    jl_value_t *usig = jl_unwrap_unionall(sig);
    if (!jl_is_datatype(usig) || jl_nparams(usig) < 1)
        return NULL;
    jl_value_t *t = jl_tparam(usig, 0);
    while (jl_is_unionall(t))
        t = ((jl_unionall_t*)t)->body;
    if (jl_is_typeeq(t)) {
        jl_value_t *x = jl_typeeq_T(t);
        while (jl_is_unionall(x))
            x = ((jl_unionall_t*)x)->body;
        if (!jl_is_datatype(x) || ((jl_datatype_t*)x)->name->abstract)
            return NULL;
        return ((jl_datatype_t*)x)->name;
    }
    if (!jl_is_datatype(t))
        return NULL;
    jl_datatype_t *dt = (jl_datatype_t*)t;
    if (dt->name == jl_type_typename) {
        jl_value_t *x = jl_tparam0(dt);
        while (jl_is_unionall(x))
            x = ((jl_unionall_t*)x)->body;
        if (!jl_is_datatype(x))
            return NULL;
        return ((jl_datatype_t*)x)->name; // constructor family (abstract X fine: invariant)
    }
    if (dt->name->abstract || jl_is_kind((jl_value_t*)dt))
        return NULL;
    return dt->name;
}

// nominal upper-bound typename of a signature's second slot, for cheap
// disjointness prefiltering: when a query's second slot has a concrete
// typename Q, a method whose second slot is bounded by nominal abstract A can
// only intersect it if A's name appears on Q's supertype chain. Returns NULL
// when no sound nominal bound exists (union/kind/Type-shaped/Any slots).
static jl_typename_t *fdisj_slot1_nominal_ub(jl_value_t *sig, int *typeside)
{
    *typeside = 0;
    jl_value_t *usig = jl_unwrap_unionall(sig);
    if (!jl_is_datatype(usig) || jl_nparams(usig) < 2)
        return NULL;
    jl_value_t *t = jl_tparam(usig, 1);
    if (jl_is_typevar(t))
        t = ((jl_tvar_t*)t)->ub;
    while (jl_is_unionall(t))
        t = ((jl_unionall_t*)t)->body;
    if (jl_is_typevar(t))
        t = ((jl_tvar_t*)t)->ub;
    if (!jl_is_datatype(t))
        return NULL;
    jl_datatype_t *dt = (jl_datatype_t*)t;
    if (jl_is_typeeq((jl_value_t*)dt))
        return NULL;
    if (dt->name == jl_type_typename) {
        // Type{X} with non-concrete X (concrete X would have been bucketed):
        // the nominal bound of X still prefilters typelike queries, whose
        // slot values are types on X's nominal chain
        jl_value_t *x = jl_tparam0(dt);
        if (jl_is_typevar(x))
            x = ((jl_tvar_t*)x)->ub;
        while (jl_is_unionall(x))
            x = ((jl_unionall_t*)x)->body;
        if (jl_is_typevar(x))
            x = ((jl_tvar_t*)x)->ub;
        if (!jl_is_datatype(x))
            return NULL;
        jl_datatype_t *xdt = (jl_datatype_t*)x;
        if ((jl_value_t*)xdt == (jl_value_t*)jl_any_type || jl_is_kind((jl_value_t*)xdt) ||
            xdt->name == jl_type_typename || jl_is_typeeq((jl_value_t*)xdt))
            return NULL;
        *typeside = 1;
        return xdt->name;
    }
    if ((jl_value_t*)dt == (jl_value_t*)jl_any_type || jl_is_kind((jl_value_t*)dt))
        return NULL;
    return dt->name;
}

// does the query's second slot have a Type/TypeEq shape (whose supertype
// chain is kind-side, not the constructed type's chain)?
static int fdisj_slot1_is_typelike(jl_value_t *sig)
{
    jl_value_t *usig = jl_unwrap_unionall(sig);
    if (!jl_is_datatype(usig) || jl_nparams(usig) < 2)
        return 1;
    jl_value_t *t = jl_tparam(usig, 1);
    while (jl_is_unionall(t))
        t = ((jl_unionall_t*)t)->body;
    if (!jl_is_datatype(t))
        return 1;
    jl_datatype_t *dt = (jl_datatype_t*)t;
    return jl_is_typeeq((jl_value_t*)dt) || dt->name == jl_type_typename ||
           jl_is_kind((jl_value_t*)dt);
}

JL_DLLEXPORT uint64_t jl_isect_memo_hits = 0, jl_isect_memo_misses = 0;

// cheap sound disproofs before the full intersection lambda:
// (1) plain non-vararg datatype-tuples of different arity never intersect;
// (2) a concrete query slot C is disjoint from the method slot M unless
//     C <: ∃W.M (concrete types have no proper subtypes besides ⊥), and one
//     empty slot empties the whole covariant tuple. Per-slot ∃-rewrapping of
//     the method's unionall vars over-approximates, which only retains.
static int fdisj_fast(void) JL_NOTSAFEPOINT
{
    static int on = -1;
    if (on == -1) {
        char *e = getenv("JULIA_FDISJ_FAST");
        on = e == NULL || strcmp(e, "0") != 0;
    }
    return on;
}

static int fdisj_isect_empty(jl_value_t *msig0, jl_value_t *qsig)
{
    jl_value_t *msig = jl_unwrap_unionall(msig0);
    jl_value_t *uq = jl_unwrap_unionall(qsig);
    if (fdisj_fast() && jl_is_datatype(msig) && jl_is_datatype(uq) && !jl_is_unionall(qsig)) {
        size_t np = jl_nparams(msig);
        size_t nq = jl_nparams(uq);
        int mva = np > 0 && jl_is_vararg(jl_tparam(msig, np - 1));
        int qva = nq > 0 && jl_is_vararg(jl_tparam(uq, nq - 1));
        if (!mva && !qva) {
            if (np != nq) {
                jl_isect_memo_hits++;
                return 1; // arity mismatch: disjoint
            }
            for (size_t i = 0; i < np; i++) {
                jl_value_t *qi = jl_tparam(uq, i);
                jl_value_t *mi = jl_tparam(msig, i);
                if (qi == mi)
                    continue;
                if (!jl_is_concrete_type(qi))
                    continue;
                int empty;
                if (!jl_has_free_typevars(mi)) {
                    empty = !jl_subtype(qi, mi);
                }
                else {
                    jl_value_t *mre = jl_rewrap_unionall(mi, msig0);
                    JL_GC_PUSH1(&mre);
                    empty = !jl_subtype(qi, mre);
                    JL_GC_POP();
                }
                if (empty) {
                    jl_isect_memo_hits++;
                    return 1;
                }
            }
        }
    }
    jl_isect_memo_misses++; // full-intersection fallback
    return jl_has_empty_intersection(msig0, qsig);
}

static int sc_oracle(void) JL_NOTSAFEPOINT
{
    static int on = -1;
    if (on == -1) {
        char *e = getenv("JULIA_SC_ORACLE");
        on = e != NULL && strcmp(e, "1") == 0;
    }
    return on;
}

static int fdisj_v2(void) JL_NOTSAFEPOINT
{
    static int on = -1;
    if (on == -1) {
        char *e = getenv("JULIA_FDISJ_V2");
        on = e == NULL || strcmp(e, "0") != 0;
    }
    return on;
}

// does `target` appear on the nominal supertype chain of `n`'s wrapper?
static int tn_chain_contains(jl_typename_t *n, jl_typename_t *target) JL_NOTSAFEPOINT
{
    jl_datatype_t *w = (jl_datatype_t*)jl_unwrap_unionall(n->wrapper);
    for (int d = 0; d < 24 && w != NULL; d++) {
        if (w->name == target)
            return 1;
        if (w == jl_any_type)
            return 0;
        w = w->super;
    }
    return 1; // overflow: assume present (conservative)
}

static void _typename_check_foreign_disjoint(jl_typename_t *tn, int explct, void *env0)
{
    struct _foreign_disjoint *env = (struct _foreign_disjoint*)env0;
    (void)explct;
    if (!env->ok || jl_method_contributor_methods == NULL)
        return;
    jl_array_t *ms = (jl_array_t*)jl_eqtable_get(jl_method_contributor_methods, (jl_value_t*)tn, NULL);
    if (ms == NULL)
        return; // only (pre-session) sysimage contributions
    JL_GC_PROMISE_ROOTED(ms); // held by the rooted contributor table
    jl_value_t **d = jl_array_ptr_data(ms);
    // memo = (residue, famtab, nforeign): foreign methods grouped first by the
    // function-slot key (function typename / constructed typename), then by
    // the second-argument slot key; residue holds methods with no slot-0 key
    jl_svec_t *memo = NULL;
    if (d[0] != jl_nothing && jl_unbox_int32(d[0]) == jl_loading_closure_gen) {
        if (d[1] == jl_nothing) { // memoized: poisoned
            env->ok = 0;
            return;
        }
        memo = (jl_svec_t*)d[1];
    }
    else if (d[0] != jl_nothing) {
        // stale generation, but the contributor list itself is unchanged
        // (adds and deletion tombstones clear d[0] in place): the memo is
        // still valid if no contributor blob's closure membership changed
        if (d[1] == jl_nothing) {
            // poisoned by a deletion tombstone: closure-independent verdict
            jl_array_ptr_set(ms, 0, jl_box_int32(jl_loading_closure_gen));
            env->ok = 0;
            return;
        }
        jl_svec_t *cand = (jl_svec_t*)d[1];
        JL_GC_PROMISE_ROOTED(cand);
        jl_value_t *snapv = jl_svecref(cand, 3);
        if (snapv != jl_nothing) {
            jl_array_t *snap = (jl_array_t*)snapv;
            JL_GC_PROMISE_ROOTED(snap);
            int32_t *sd = jl_array_data(snap, int32_t);
            size_t ns = jl_array_nrows(snap);
            int still = 1;
            for (size_t i = 0; i < ns; i++) {
                if (blob_in_loading_closure((size_t)(sd[i] >> 1)) != (sd[i] & 1)) {
                    still = 0;
                    break;
                }
            }
            if (still) {
                jl_array_ptr_set(ms, 0, jl_box_int32(jl_loading_closure_gen));
                memo = cand;
            }
        }
    }
    if (memo == NULL) {
        // (re)compute the foreign subset for the current closure
        memo = jl_alloc_svec(4);
        JL_GC_PUSH1(&memo);
        jl_svecset(memo, 0, jl_alloc_vec_any(0));
        jl_svecset(memo, 1, jl_an_empty_memory_any);
        int poisoned = 0;
        size_t nforeign = 0;
        size_t l = jl_array_nrows(ms);
        // closure-membership snapshot of the contributor blobs (session
        // methods are always foreign, so they need no entry)
        int32_t snapbuf[96];
        size_t nsnap = 0;
        int snapok = 1;
        for (size_t i = 2; i < l; i++) {
            jl_value_t *m = jl_array_ptr_data(ms)[i];
            JL_GC_PROMISE_ROOTED(m); // held by `ms`
            if (m == jl_nothing) { // deletion: poisoned
                poisoned = 1;
                break;
            }
            if (snapok && jl_object_in_image(m)) {
                size_t blob = jl_external_blob_index(m);
                int32_t ent = (int32_t)((blob << 1) | (blob_in_loading_closure(blob) ? 1 : 0));
                size_t si = 0;
                while (si < nsnap && snapbuf[si] != ent)
                    si++;
                if (si == nsnap) {
                    if (nsnap == 96 || blob >= ((size_t)1 << 30))
                        snapok = 0;
                    else
                        snapbuf[nsnap++] = ent;
                }
            }
            if (object_in_loading_closure(m))
                continue;
            nforeign++;
            jl_typename_t *k0 = fdisj_slot0_key(((jl_method_t*)m)->sig);
            JL_GC_PROMISE_ROOTED(k0); // typenames are rooted by their types
            if (k0 == NULL) {
                static int fdisj_dump_left = 60;
                const char *dumptarget = getenv("JULIA_FDISJ_DUMP");
                if (dumptarget && fdisj_dump_left > 0 &&
                    strstr(jl_symbol_name(tn->name), dumptarget) != NULL) {
                    fdisj_dump_left--;
                    jl_method_t *dm = (jl_method_t*)m;
                    jl_safe_printf("RESIDUE %s: %s.%s ", jl_symbol_name(tn->name),
                                   jl_symbol_name(dm->module->name), jl_symbol_name(dm->name));
                    jl_static_show(JL_STDERR, dm->sig);
                    jl_safe_printf("\n");
                }
                jl_array_ptr_1d_push((jl_array_t*)jl_svecref(memo, 0), m);
                continue;
            }
            jl_genericmemory_t *famtab = (jl_genericmemory_t*)jl_svecref(memo, 1);
            JL_GC_PROMISE_ROOTED(famtab);
            jl_svec_t *fam = (jl_svec_t*)jl_eqtable_get(famtab, (jl_value_t*)k0, NULL);
            if (fam == NULL) {
                fam = jl_alloc_svec(2);
                JL_GC_PUSH1(&fam);
                jl_svecset(fam, 0, jl_alloc_vec_any(0));
                jl_svecset(fam, 1, jl_an_empty_memory_any);
                jl_genericmemory_t *nt = jl_eqtable_put(famtab, (jl_value_t*)k0, (jl_value_t*)fam, NULL);
                JL_GC_POP();
                if (nt != famtab)
                    jl_svecset(memo, 1, nt);
            }
            JL_GC_PROMISE_ROOTED(fam); // held by the family table
            jl_typename_t *k1 = fdisj_sig_key(((jl_method_t*)m)->sig);
            JL_GC_PROMISE_ROOTED(k1);
            if (k1 == NULL) {
                int typeside = 0;
                jl_typename_t *pf = fdisj_slot1_nominal_ub(((jl_method_t*)m)->sig, &typeside);
                // allocation-free side encoding: value-side = the typename
                // object, type-side = the wrapper type (chain-walkable at
                // scan time; legacy name-symbol form is still decoded)
                jl_value_t *pfv = pf == NULL ? jl_nothing :
                    typeside ? (fdisj_v2() ? pf->wrapper : (jl_value_t*)pf->name)
                             : (jl_value_t*)pf;
                // rooted: typenames by their types (reachable from m->sig,
                // held by `ms`), symbols permanently
                JL_GC_PROMISE_ROOTED(pfv);
                jl_array_t *unk = (jl_array_t*)jl_svecref(fam, 0);
                JL_GC_PROMISE_ROOTED(unk);
                jl_array_ptr_1d_push(unk, m);
                jl_array_ptr_1d_push(unk, pfv);
            }
            else {
                jl_genericmemory_t *k1tab = (jl_genericmemory_t*)jl_svecref(fam, 1);
                JL_GC_PROMISE_ROOTED(k1tab);
                jl_array_t *bucket = (jl_array_t*)jl_eqtable_get(k1tab, (jl_value_t*)k1, NULL);
                if (bucket == NULL) {
                    bucket = jl_alloc_vec_any(0);
                    JL_GC_PUSH1(&bucket);
                    jl_genericmemory_t *nt = jl_eqtable_put(k1tab, (jl_value_t*)k1, (jl_value_t*)bucket, NULL);
                    JL_GC_POP();
                    if (nt != k1tab)
                        jl_svecset(fam, 1, nt);
                }
                JL_GC_PROMISE_ROOTED(bucket);
                jl_array_ptr_1d_push(bucket, m);
            }
        }
        jl_svecset(memo, 2, jl_box_long((ssize_t)nforeign));
        if (snapok && !poisoned) {
            jl_array_t *snap = jl_alloc_array_1d(jl_array_int32_type, nsnap);
            memcpy(jl_array_data(snap, int32_t), snapbuf, nsnap * sizeof(int32_t));
            jl_svecset(memo, 3, snap);
        }
        else {
            jl_svecset(memo, 3, jl_nothing);
        }
        jl_array_ptr_set(ms, 1, poisoned ? jl_nothing : (jl_value_t*)memo);
        jl_value_t *boxedgen = jl_box_int32(jl_loading_closure_gen);
        jl_array_ptr_set(ms, 0, boxedgen);
        JL_GC_POP();
        if (poisoned) {
            env->ok = 0;
            return;
        }
    }
    // fail fast when the number of candidate intersections is too large for
    // this to beat ordinary verification
    // TODO: temporary tuning knob for evaluation
    static size_t cap = (size_t)-1;
    if (cap == (size_t)-1) {
        char *ev = getenv("JULIA_EDGE_FDISJ_CAP");
        cap = ev ? (size_t)atol(ev) : 64;
    }
    JL_GC_PROMISE_ROOTED(memo); // held by `ms`
    jl_array_t *residue = (jl_array_t*)jl_svecref(memo, 0);
    JL_GC_PROMISE_ROOTED(residue);
    size_t nf = jl_array_nrows(residue);
    jl_genericmemory_t *famtab0 = (jl_genericmemory_t*)jl_svecref(memo, 1);
    JL_GC_PROMISE_ROOTED(famtab0);
    jl_typename_t *q0 = fdisj_slot0_key(env->sig);
    JL_GC_PROMISE_ROOTED(q0);
    jl_typename_t *q1 = fdisj_sig_key(env->sig);
    JL_GC_PROMISE_ROOTED(q1);
    // supertype-chain names of the query's second slot: sound rejection set
    // for nominally-bounded unkeyed methods (only when the slot is a plain
    // concrete-named type, not Type/TypeEq-shaped)
    jl_typename_t *qchain[24];
    int nqchain = 0;
    int qtypeside = 0;
    // when the second slot has no concrete key, its nominal upper bound (if
    // any) still soundly prunes concrete-marked entries: an entry with
    // concrete-named marker N (same sidedness) can only intersect if the
    // bound's name lies on N's supertype chain
    jl_typename_t *qub = NULL;
    int qubts = 0;
    if (fdisj_v2() && q1 == NULL)
        qub = fdisj_slot1_nominal_ub(env->sig, &qubts);
    JL_GC_PROMISE_ROOTED(qub);
    if (q1 != NULL) {
        qtypeside = fdisj_slot1_is_typelike(env->sig);
        jl_datatype_t *w = (jl_datatype_t*)jl_unwrap_unionall(q1->wrapper);
        while (w != NULL && w != jl_any_type && nqchain < 24) {
            qchain[nqchain++] = w->name;
            w = w->super;
        }
        if (w != jl_any_type)
            nqchain = 0; // overflow: disable the prefilter
    }
    uint64_t nisect = 0;
#define FDISJ_TEST(arr_, idx0_, stride_) do { \
        jl_method_t *m_ = (jl_method_t*)jl_array_ptr_ref((arr_), (idx0_)); \
        if (jl_get_ici_debug_enabled()) { \
            static _Atomic(int) dumped_; \
            if (jl_atomic_fetch_add_relaxed(&dumped_, 1) < 400) { \
                jl_safe_printf("FDTEST %s.%s: ", \
                               jl_symbol_name(tn->module->name), jl_symbol_name(tn->name)); \
                jl_static_show((JL_STREAM*)STDERR_FILENO, (jl_value_t*)m_->sig); \
                jl_safe_printf("\n"); \
            } \
        } \
        nisect++; \
        if (!fdisj_isect_empty((jl_value_t*)m_->sig, env->sig)) { \
            /* move-to-front: repeated dirty patterns hit on the first test */ \
            if (fdisj_v2() && (size_t)(idx0_) >= (size_t)(stride_)) { \
                for (int s_ = 0; s_ < (stride_); s_++) { \
                    jl_value_t *tmp_ = jl_array_ptr_ref((arr_), s_); \
                    jl_array_ptr_set((arr_), s_, jl_array_ptr_ref((arr_), (idx0_) + s_)); \
                    jl_array_ptr_set((arr_), (idx0_) + s_, tmp_); \
                } \
            } \
            fdisj_stat_note(tn, nisect); \
            env->ok = 0; \
            return; \
        } \
    } while (0)
#define FDISJ_SCAN_FAM(famv) do { \
        jl_svec_t *fam_ = (jl_svec_t*)(famv); \
        JL_GC_PROMISE_ROOTED(fam_); \
        jl_array_t *unk_ = (jl_array_t*)jl_svecref(fam_, 0); \
        JL_GC_PROMISE_ROOTED(unk_); \
        size_t nu_ = jl_array_nrows(unk_) / 2; \
        if (nisect + nu_ > cap) { \
            env->ok = 0; \
            return; \
        } \
        for (size_t i_ = 0; i_ < nu_; i_++) { \
            jl_value_t *pf_ = jl_array_ptr_ref(unk_, 2 * i_ + 1); \
            if (pf_ != jl_nothing) { \
                int pfts_; \
                jl_typename_t *pftn_; \
                if (jl_is_symbol(pf_)) { \
                    pfts_ = 1; pftn_ = NULL; /* legacy: side known, chain not */ \
                } \
                else if (jl_typetagis(pf_, jl_typename_type)) { \
                    pfts_ = 0; pftn_ = (jl_typename_t*)pf_; \
                } \
                else { \
                    pfts_ = 1; pftn_ = ((jl_datatype_t*)jl_unwrap_unionall(pf_))->name; \
                } \
                if (nqchain > 0) { \
                    if (pfts_ != qtypeside) \
                        continue; /* Type-shaped and plain slots cannot intersect */ \
                    int hit_ = 0; \
                    for (int c_ = 0; c_ < nqchain; c_++) { \
                        if (pftn_ ? qchain[c_] == pftn_ \
                                  : (jl_value_t*)qchain[c_]->name == pf_) { hit_ = 1; break; } \
                    } \
                    if (!hit_) \
                        continue; \
                } \
                else if (qub != NULL) { \
                    if (pfts_ != qubts) \
                        continue; /* sides cannot intersect */ \
                    if (pftn_ != NULL && !pftn_->abstract && !tn_chain_contains(pftn_, qub)) \
                        continue; /* concrete marker off the bound's chain */ \
                } \
            } \
            FDISJ_TEST(unk_, 2 * i_, 2); \
        } \
        jl_genericmemory_t *k1tab_ = (jl_genericmemory_t*)jl_svecref(fam_, 1); \
        JL_GC_PROMISE_ROOTED(k1tab_); \
        if (q1 != NULL) { \
            jl_array_t *bucket_ = (jl_array_t*)jl_eqtable_get(k1tab_, (jl_value_t*)q1, NULL); \
            if (bucket_ != NULL) { \
                JL_GC_PROMISE_ROOTED(bucket_); \
                size_t nb_ = jl_array_nrows(bucket_); \
                if (nisect + nb_ > cap) { \
                    env->ok = 0; \
                    return; \
                } \
                for (size_t i_ = 0; i_ < nb_; i_++) \
                    FDISJ_TEST(bucket_, i_, 1); \
            } \
        } \
        else { \
            for (size_t j_ = 1; j_ < k1tab_->length; j_ += 2) { \
                jl_array_t *bucket_ = (jl_array_t*)jl_genericmemory_ptr_ref(k1tab_, j_); \
                if (bucket_ == NULL) \
                    continue; \
                if (qub != NULL) { \
                    jl_typename_t *kb_ = (jl_typename_t*)jl_genericmemory_ptr_ref(k1tab_, j_ - 1); \
                    if (kb_ != NULL && !kb_->abstract && !tn_chain_contains(kb_, qub)) \
                        continue; /* concrete-keyed bucket off the bound's chain */ \
                } \
                JL_GC_PROMISE_ROOTED(bucket_); \
                size_t nb_ = jl_array_nrows(bucket_); \
                if (nisect + nb_ > cap) { \
                    env->ok = 0; \
                    return; \
                } \
                for (size_t i_ = 0; i_ < nb_; i_++) \
                    FDISJ_TEST(bucket_, i_, 1); \
            } \
        } \
    } while (0)
    if (nf > cap) {
        env->ok = 0;
        return;
    }
    if (q0 == NULL) {
        // unkeyed signature: candidate set is the whole foreign subset
        size_t nforeign = (size_t)jl_unbox_long(jl_svecref(memo, 2));
        if (nforeign + nf > cap) {
            env->ok = 0;
            return;
        }
    }
    for (size_t i = 0; i < nf; i++)
        FDISJ_TEST(residue, i, 1);
    if (q0 != NULL) {
        jl_svec_t *fam = (jl_svec_t*)jl_eqtable_get(famtab0, (jl_value_t*)q0, NULL);
        if (fam != NULL)
            FDISJ_SCAN_FAM(fam);
    }
    else {
        // unkeyed signature: every family may intersect
        for (size_t j = 1; j < famtab0->length; j += 2) {
            jl_value_t *fam = jl_genericmemory_ptr_ref(famtab0, j);
            if (fam == NULL)
                continue;
            FDISJ_SCAN_FAM(fam);
        }
    }
#undef FDISJ_SCAN_FAM
#undef FDISJ_TEST
    fdisj_stat_note(tn, nisect);
}

// save-side typename decompositions of call/method signatures, registered per
// image at load: sig -> svec(mask::Int, tn...), bit i of mask = explicitness
JL_DLLEXPORT jl_genericmemory_t *jl_sig_tn_table JL_GLOBALLY_ROOTED;

JL_DLLEXPORT void jl_register_sig_tns(jl_array_t *tab)
{
    if (tab == NULL || jl_array_nrows(tab) == 0 || !sig_tns_enabled())
        return;
    if (jl_sig_tn_table == NULL)
        jl_sig_tn_table = (jl_genericmemory_t*)jl_an_empty_memory_any;
    size_t n = jl_array_nrows(tab);
    for (size_t i = 0; i + 1 < n; i += 2) {
        jl_value_t *sig = jl_array_ptr_ref(tab, i);
        jl_value_t *tns = jl_array_ptr_ref(tab, i + 1);
        if (sig == NULL || tns == NULL)
            continue;
        jl_genericmemory_t *nt = jl_eqtable_put(jl_sig_tn_table, sig, tns, NULL);
        if (nt != jl_sig_tn_table)
            jl_sig_tn_table = nt;
    }
}

// iterate a stored decomposition exactly as jl_foreach_top_typename_for would;
// returns -1 when the signature has no stored entry
static int sig_tns_enabled(void)
{
    static int on = -1;
    if (on == -1) {
        // default off: one global eqtable across all images costs more in
        // registration and probes than the decomposition it replaces; a
        // per-image table design could revisit this
        char *e = getenv("JULIA_SIG_TNS");
        on = e != NULL && strcmp(e, "1") == 0;
    }
    return on;
}

static int sig_tns_foreach(void (*f)(jl_typename_t*, int, void*), jl_value_t *sig, void *env)
{
    if (jl_sig_tn_table == NULL || !sig_tns_enabled())
        return -1;
    jl_svec_t *tns = (jl_svec_t*)jl_eqtable_get(jl_sig_tn_table, sig, NULL);
    if (tns == NULL)
        return -1;
    JL_GC_PROMISE_ROOTED(tns); // held by the rooted table
    size_t l = jl_svec_len(tns);
    assert(l >= 1);
    uint64_t mask = (uint64_t)jl_unbox_long(jl_svecref(tns, 0));
    for (size_t i = 1; i < l; i++) {
        jl_typename_t *tn = (jl_typename_t*)jl_svecref(tns, i);
        JL_GC_PROMISE_ROOTED(tn);
        f(tn, (mask >> (i - 1)) & 1, env);
    }
    return 1;
}

// Per-signature verdict memo: edge signatures repeat heavily both within and
#define TN_COLLECT_MAX 15
struct _tn_collect {
    size_t n;
    jl_typename_t *tns[TN_COLLECT_MAX + 1];
};

static void _typename_collect_for_verdict(jl_typename_t *tn, int explct, void *env0)
{
    struct _tn_collect *c = (struct _tn_collect*)env0;
    (void)explct;
    if (c->n <= TN_COLLECT_MAX)
        c->tns[c->n] = tn;
    c->n++;
}

// across CodeInstance edge lists. Pointer-keyed and direct-mapped (collisions
// simply recompute); signatures are kept alive by the edge lists being
// verified, and entries self-invalidate via the closure generation.
// N.B.: assumes a non-moving GC.
typedef struct {
    jl_value_t *sig;
    int32_t gen;
    int32_t verdict;
} edge_sig_memo_ent_t;
static edge_sig_memo_ent_t *edge_sig_memo = NULL;
#define EDGE_SIG_MEMO_SZ (1 << 19)

JL_DLLEXPORT int jl_edge_sig_replayable(jl_value_t *sig)
{
    int mode = activate_replay_mode();
    if (mode < 1 || jl_loading_closure_bits == NULL)
        return 0;
    if (edge_sig_memo == NULL)
        edge_sig_memo = (edge_sig_memo_ent_t*)calloc_s(EDGE_SIG_MEMO_SZ * sizeof(edge_sig_memo_ent_t));
    size_t memoidx = (((uintptr_t)sig) * 0x9E3779B97F4A7C15ULL >> 32) & (EDGE_SIG_MEMO_SZ - 1);
    edge_sig_memo_ent_t *ment = &edge_sig_memo[memoidx];
    for (int probe = 0; probe < 8; probe++) {
        edge_sig_memo_ent_t *e = &edge_sig_memo[(memoidx + probe) & (EDGE_SIG_MEMO_SZ - 1)];
        if (e->sig == sig && e->gen == jl_loading_closure_gen)
            return e->verdict ? mode : 0;
        if (e->sig == NULL || e->gen != jl_loading_closure_gen) {
            ment = e; // first free/stale slot in the window receives the store
            break;
        }
        ment = e; // window full: overwrite the last probed slot
    }
    uint64_t t0 = jl_hrtime();
    int clean = 1;
    jl_contrib_stats[8]++;
    int decomposed;
    static int sc_fuse = -1;
    if (sc_fuse == -1) {
        char *e = getenv("JULIA_SC_FUSE");
        sc_fuse = e == NULL || strcmp(e, "0") != 0;
    }
    if (sc_fuse) {
        // single decomposition: collect the typenames once, then run the
        // contributor check and (only if needed) the foreign-disjoint second
        // chance over the collected list instead of re-walking the signature
        struct _tn_collect coll = { 0, 0 };
        decomposed = sig_tns_foreach(_typename_collect_for_verdict, sig, &coll);
        if (decomposed < 0)
            decomposed = jl_foreach_top_typename_for(_typename_collect_for_verdict, sig, 1, &coll);
        if (coll.n > TN_COLLECT_MAX)
            decomposed = 0; // overflow: treat as undecomposable (dirty)
        if (decomposed) {
            for (size_t i = 0; i < coll.n && clean; i++)
                _typename_check_contributor(coll.tns[i], 1, &clean);
            if (!clean && sc_oracle()) {
                // ceiling probe: assume disjoint (UNSOUND, measurement only)
                jl_contrib_stats[12]++;
                jl_contrib_stats[13]++;
                clean = 1;
            }
            if (!clean) {
                // second chance: the closure does not cover all contributors
                // to these typenames, but if no foreign contributor method
                // intersects this signature, its match set is still provably
                // unchanged relative to the precompile worker (deletions
                // poison; an exact replacement's new method carries the
                // replaced signature, so it covers the removal too)
                struct _foreign_disjoint fenv = { sig, 1 };
                jl_contrib_stats[12]++; // second-chance attempts
                uint64_t t1 = jl_hrtime();
                for (size_t i = 0; i < coll.n && fenv.ok; i++)
                    _typename_check_foreign_disjoint(coll.tns[i], 1, &fenv);
                if (fenv.ok) {
                    clean = 1;
                    jl_contrib_stats[13]++; // second-chance successes
                }
                jl_contrib_stats[23] += jl_hrtime() - t1;
            }
        }
    }
    else {
        decomposed = sig_tns_foreach(_typename_check_contributor, sig, &clean);
        if (decomposed < 0)
            decomposed = jl_foreach_top_typename_for(_typename_check_contributor, sig, 1, &clean);
        if (decomposed && !clean && sc_oracle()) {
            jl_contrib_stats[12]++;
            jl_contrib_stats[13]++;
            clean = 1;
        }
        if (decomposed && !clean) {
            struct _foreign_disjoint fenv = { sig, 1 };
            jl_contrib_stats[12]++; // second-chance attempts
            uint64_t t1 = jl_hrtime();
            int fdec = sig_tns_foreach(_typename_check_foreign_disjoint, sig, &fenv);
            if (fdec < 0)
                fdec = jl_foreach_top_typename_for(_typename_check_foreign_disjoint, sig, 1, &fenv);
            if (fdec && fenv.ok) {
                clean = 1;
                jl_contrib_stats[13]++; // second-chance successes
            }
            jl_contrib_stats[23] += jl_hrtime() - t1;
        }
    }
    jl_contrib_stats[11] += jl_hrtime() - t0;
    ment->sig = sig;
    ment->gen = jl_loading_closure_gen;
    ment->verdict = decomposed && clean;
    if (!decomposed || !clean) {
        jl_value_t *usig = jl_unwrap_unionall(sig);
        if (jl_is_datatype(usig) && jl_nparams(usig) > 0 && jl_is_typeeq(jl_tparam(usig, 0)))
            jl_contrib_stats[10]++; // dirty with Type{...} first arg (constructor call)
        return 0;
    }
    jl_contrib_stats[9]++;
    return mode;
}

// Resolve the all-clean majority of a loaded image's revalidation worklist in
// one C pass, so the Julia verification graph walk only runs for the residue.
// A sentinel CodeInstance is clean when every edge in its list is provably
// unchanged relative to the precompile worker (jl_edge_sig_replayable on call
// signatures, un-invalidated full-range binding partitions) and every callee
// CodeInstance it depends on is clean too (optimistic for cycles: only
// dirt propagates, along reverse dependency edges, so a strongly connected
// clean component stays clean). Survivors get their worlds stamped directly —
// the Julia walk's sentinel short-circuit then skips them — and their
// native-cache pokes are performed here since the Julia cleanup that would
// have done it is skipped. The caller gates on: backedge log present
// (store_backedges is otherwise required per CodeInstance), debug-invalidation
// logging off, and coverage/malloc instrumentation off.
static int verify_plan_enabled(void)
{
    static int on = -1;
    if (on == -1) {
        char *e = getenv("JULIA_VERIFY_PLAN");
        on = e == NULL || strcmp(e, "0") != 0;
    }
    return on;
}

JL_DLLEXPORT jl_value_t *jl_preverify_clean_cis(jl_array_t *worklist, size_t validation_world)
{
    if (activate_replay_mode() < 1 || jl_loading_closure_bits == NULL)
        return jl_nothing;
    if (jl_options.code_coverage || jl_options.malloc_log)
        return jl_nothing;
    size_t nitems = jl_array_nrows(worklist);
    if (nitems == 0)
        return jl_nothing;
    // collect the sentinel CodeInstances
    jl_code_instance_t **cis = (jl_code_instance_t**)malloc_s(nitems * sizeof(void*));
    size_t n = 0;
    for (size_t i = 0; i < nitems; i++) {
        jl_value_t *obj = jl_array_ptr_ref(worklist, i);
        if (jl_is_code_instance(obj) &&
            jl_atomic_load_relaxed(&((jl_code_instance_t*)obj)->max_world) == 1) // WORLD_AGE_REVALIDATION_SENTINEL
            cis[n++] = (jl_code_instance_t*)obj;
    }
    if (n == 0) {
        free(cis);
        return jl_nothing;
    }
    // pointer-keyed slot table
    size_t tabsz = 1;
    while (tabsz < 2 * n)
        tabsz *= 2;
    size_t *tab = (size_t*)malloc_s(tabsz * sizeof(size_t)); // slot+1, 0 = empty
    memset(tab, 0, tabsz * sizeof(size_t));
    for (size_t i = 0; i < n; i++) {
        size_t idx = (((uintptr_t)cis[i]) * 0x9E3779B97F4A7C15ULL >> 32) & (tabsz - 1);
        while (tab[idx] != 0)
            idx = (idx + 1) & (tabsz - 1);
        tab[idx] = i + 1;
    }
#define PREVERIFY_LOOKUP(ci, out) do { \
        size_t idx_ = (((uintptr_t)(ci)) * 0x9E3779B97F4A7C15ULL >> 32) & (tabsz - 1); \
        (out) = (size_t)-1; \
        while (tab[idx_] != 0) { \
            if (cis[tab[idx_] - 1] == (jl_code_instance_t*)(ci)) { (out) = tab[idx_] - 1; break; } \
            idx_ = (idx_ + 1) & (tabsz - 1); \
        } \
    } while (0)
    char *dirty = (char*)malloc_s(n);       // cannot be stamped by the prepass
    char *hard = (char*)malloc_s(n);        // needs the full Julia graph walk
    memset(dirty, 0, n);
    memset(hard, 0, n);
    size_t *minw = (size_t*)malloc_s(n * sizeof(size_t));
    // per-CI recorded edge-word indices (verification-relevant residue):
    // dirty match-group headers, dirty invoke signatures, and sentinel-callee
    // references (for world folding in the plan sweep)
    uint32_t *wstart = (uint32_t*)malloc_s(n * sizeof(uint32_t));
    uint32_t *wcount = (uint32_t*)malloc_s(n * sizeof(uint32_t));
    uint32_t *words = NULL;
    size_t nwords = 0, capwords = 0;
#define PLAN_WORD(j) do { \
        if (nwords == capwords) { \
            capwords = capwords ? capwords * 2 : 4096; \
            words = (uint32_t*)realloc_s(words, capwords * sizeof(uint32_t)); \
        } \
        words[nwords++] = (uint32_t)(j); \
    } while (0)
    // forward dependency pairs (from, to) among in-worklist sentinels
    size_t *deps = NULL, ndeps = 0, capdeps = 0;
    for (size_t i = 0; i < n; i++) {
        jl_code_instance_t *ci = cis[i];
        minw[i] = jl_require_world;
        wstart[i] = (uint32_t)nwords;
        wcount[i] = 0;
        jl_method_instance_t *mi = jl_get_ci_mi(ci);
        if (!jl_is_method(mi->def.value)) {
            hard[i] = 1;
            continue;
        }
        jl_method_t *def = mi->def.method;
        uint8_t scanned = jl_atomic_load_relaxed(&def->did_scan_source);
        if ((scanned & 0x1) == 0 || (scanned & 0x4) != 0) {
            hard[i] = 1; // needs the Julia-side source scan / invalidation log
            continue;
        }
        jl_value_t *edges = (jl_value_t*)jl_atomic_load_relaxed(&ci->edges);
        if (edges == NULL || edges == jl_nothing) {
            continue;
        }
        // image CodeInstances may carry their edges as an InternedCodeInstance
        // (relocation-free words) instead of a SimpleVector
        jl_interned_code_instance_t *iedges = NULL;
        size_t nedges;
        if (jl_typetagis(edges, jl_interned_code_instance_type)) {
            iedges = (jl_interned_code_instance_t*)edges;
            nedges = iedges->nedges;
        }
        else {
            nedges = jl_svec_len(edges);
        }
        int sigdirty = 0;
        for (size_t j = 0; j < nedges && !hard[i]; j++) {
            jl_value_t *item;
            intptr_t litval;
            int is_lit = 0;
            if (iedges) {
                if (jl_ici_literal(iedges, j, &litval)) {
                    is_lit = 1;
                    item = NULL;
                }
                else {
                    item = jl_ici_ref_nobox(iedges, j);
                    JL_GC_PROMISE_ROOTED(item); // decoded words are image objects
                }
            }
            else {
                item = jl_svecref(edges, j);
                if (jl_is_long(item)) {
                    is_lit = 1;
                    litval = jl_unbox_long(item);
                }
            }
            if (is_lit) {
                // (n, sig, targets...) match group
                ssize_t ntargets = litval;
                if (ntargets < 0)
                    ntargets = -ntargets;
                if (j + 1 >= nedges) {
                    hard[i] = 1;
                    break;
                }
                jl_value_t *sig = iedges ? jl_ici_ref_nobox(iedges, j + 1) : jl_svecref(edges, j + 1);
                if (sig == NULL) {
                    hard[i] = 1; // malformed: literal where a sig belongs
                    break;
                }
                JL_GC_PROMISE_ROOTED(sig);
                if (jl_edge_sig_replayable(sig) != 1) {
                    PLAN_WORD(j);
                    wcount[i]++;
                    sigdirty = 1;
                }
                j += 1 + (size_t)ntargets;
            }
            else if (jl_is_code_instance(item)) {
                jl_code_instance_t *callee = (jl_code_instance_t*)item;
                int calleesigdirty = jl_edge_sig_replayable(jl_get_ci_mi(callee)->specTypes) != 1;
                size_t cmax = jl_atomic_load_relaxed(&callee->max_world);
                if (cmax == 1) { // sentinel: depends on its (pre)verification
                    size_t slot;
                    PREVERIFY_LOOKUP(callee, slot);
                    if (slot == (size_t)-1) {
                        hard[i] = 1; // sentinel outside this worklist
                        break;
                    }
                    if (ndeps == capdeps) {
                        capdeps = capdeps ? capdeps * 2 : 1024;
                        deps = (size_t*)realloc_s(deps, capdeps * 2 * sizeof(size_t));
                    }
                    deps[2 * ndeps] = slot;
                    deps[2 * ndeps + 1] = i;
                    ndeps++;
                    PLAN_WORD(j); // fold the callee's stamped world in the sweep
                    wcount[i]++;
                    if (calleesigdirty)
                        sigdirty = 1;
                }
                else if (calleesigdirty) {
                    PLAN_WORD(j);
                    wcount[i]++;
                    sigdirty = 1;
                    if (cmax < validation_world && minw[i] <= cmax)
                        minw[i] = jl_atomic_load_relaxed(&callee->min_world);
                }
                else {
                    // already-settled callee: fully valid or this one bails
                    if (cmax < validation_world) {
                        hard[i] = 1;
                        break;
                    }
                    size_t cmin = jl_atomic_load_relaxed(&callee->min_world);
                    if (minw[i] < cmin)
                        minw[i] = cmin;
                }
            }
            else if (jl_is_method_instance(item)) {
                if (jl_edge_sig_replayable(((jl_method_instance_t*)item)->specTypes) != 1) {
                    PLAN_WORD(j);
                    wcount[i]++;
                    sigdirty = 1;
                }
            }
            else if (jl_is_binding(item)) {
                jl_binding_t *b = (jl_binding_t*)item;
                jl_binding_partition_t *bp = jl_atomic_load_relaxed(&b->partitions);
                if (bp != NULL) {
                    size_t bmin = jl_atomic_load_relaxed(&bp->min_world);
                    size_t bmax = jl_atomic_load_relaxed(&bp->max_world);
                    if (bmin > jl_require_world || bmax < validation_world) {
                        hard[i] = 1; // invalidated binding (or stale partition)
                        break;
                    }
                    if (minw[i] < bmin)
                        minw[i] = bmin;
                }
            }
            else if (jl_is_method(item)) {
                hard[i] = 1; // corrupt edge list; let the Julia walk assert
            }
            else {
                // (invokesig, target) pair
                if (j + 1 >= nedges) {
                    hard[i] = 1;
                    break;
                }
                jl_value_t *target = iedges ? jl_ici_ref_nobox(iedges, j + 1) : jl_svecref(edges, j + 1);
                if (target == NULL) {
                    hard[i] = 1; // malformed: literal where a target belongs
                    break;
                }
                JL_GC_PROMISE_ROOTED(target);
                if (!jl_is_mtable(target)) {
                    if (jl_edge_sig_replayable(item) != 1) {
                        // invoke re-verification needs the pair form; the
                        // sweep cannot reuse the group path: keep it hard
                        hard[i] = 1;
                    }
                }
                j += 1;
            }
        }
        if (hard[i]) {
            // discard recorded words for hard CIs (the Julia walk redoes them)
            nwords = wstart[i];
            wcount[i] = 0;
        }
        dirty[i] = hard[i] | (char)sigdirty;
    }
#undef PREVERIFY_LOOKUP
    // reverse adjacency (CSR over dep pairs, keyed by dependency source)
    size_t *radj_off = (size_t*)malloc_s((n + 1) * sizeof(size_t));
    memset(radj_off, 0, (n + 1) * sizeof(size_t));
    for (size_t k = 0; k < ndeps; k++)
        radj_off[deps[2 * k] + 1]++;
    for (size_t i = 0; i < n; i++)
        radj_off[i + 1] += radj_off[i];
    size_t *radj = (size_t*)malloc_s((ndeps ? ndeps : 1) * sizeof(size_t));
    size_t *fill = (size_t*)malloc_s((n + 1) * sizeof(size_t));
    memcpy(fill, radj_off, (n + 1) * sizeof(size_t));
    for (size_t k = 0; k < ndeps; k++)
        radj[fill[deps[2 * k]]++] = deps[2 * k + 1];
    // propagate dirt from dependencies to dependents
    size_t *worklist2 = (size_t*)malloc_s(n * sizeof(size_t));
    size_t wl = 0;
    for (size_t i = 0; i < n; i++)
        if (dirty[i])
            worklist2[wl++] = i;
    while (wl > 0) {
        size_t d = worklist2[--wl];
        for (size_t k = radj_off[d]; k < radj_off[d + 1]; k++) {
            size_t r = radj[k];
            if (!dirty[r]) {
                dirty[r] = 1;
                worklist2[wl++] = r;
            }
        }
    }
    // propagate minworld forward along the same edges to a fixpoint (bounded
    // sweeps; chains are shallow and cycles take the component max)
    for (int sweep = 0; sweep < 100; sweep++) {
        int changed = 0;
        for (size_t k = 0; k < ndeps; k++) {
            size_t from = deps[2 * k], to = deps[2 * k + 1];
            if (!dirty[from] && !dirty[to] && minw[to] < minw[from]) {
                minw[to] = minw[from];
                changed = 1;
            }
        }
        if (!changed)
            break;
        if (sweep == 99) {
            // did not converge (should not happen); bail the remainder
            for (size_t k = 0; k < ndeps; k++)
                dirty[deps[2 * k + 1]] = 1;
        }
    }
    // stamp the clean survivors and perform the native-cache pokes the Julia
    // cleanup would have done
    size_t nclean = 0;
    for (size_t i = 0; i < n; i++) {
        if (dirty[i])
            continue;
        jl_code_instance_t *ci = cis[i];
        jl_atomic_store_release(&ci->min_world, minw[i]);
        jl_atomic_store_release(&ci->max_world, validation_world);
        if ((jl_atomic_load_relaxed(&ci->flags) & 0b1000) != 0) // CI_FLAGS_NATIVE_CACHE_VALID
            jl_mi_cache_insert(jl_get_ci_mi(ci), ci);
        nclean++;
    }
    jl_contrib_stats[14] += nclean;
    jl_contrib_stats[15] += n - nclean;
    if (!verify_plan_enabled()) {
        // gate: fall back to the old Julia graph walk for all dirty CIs
        free(worklist2); free(fill); free(radj); free(radj_off); free(deps);
        free(words); free(wcount); free(wstart); free(minw); free(hard);
        free(dirty); free(tab); free(cis);
        return jl_nothing;
    }
    // Kahn order over the plan set (dirty, not hard): a plan entry may only
    // depend on clean (stamped above), hard (residue walk runs first), or
    // earlier plan entries; unresolvable cycles fall back to the residue
    uint32_t *indeg = (uint32_t*)calloc_s(n * sizeof(uint32_t));
    for (size_t k = 0; k < ndeps; k++) {
        size_t from = deps[2 * k], to = deps[2 * k + 1];
        if (dirty[from] && !hard[from] && dirty[to] && !hard[to])
            indeg[to]++;
    }
    size_t *order = (size_t*)malloc_s(n * sizeof(size_t));
    size_t nord = 0, qhead = 0;
    for (size_t i = 0; i < n; i++)
        if (dirty[i] && !hard[i] && indeg[i] == 0)
            order[nord++] = i;
    while (qhead < nord) {
        size_t d = order[qhead++];
        for (size_t k = radj_off[d]; k < radj_off[d + 1]; k++) {
            size_t r = radj[k];
            if (dirty[r] && !hard[r] && indeg[r] > 0 && --indeg[r] == 0)
                order[nord++] = r;
        }
    }
    for (size_t i = 0; i < n; i++)
        if (dirty[i] && !hard[i] && indeg[i] > 0)
            hard[i] = 1; // cycle member: residue
    // assemble the plan for the Julia sweep
    jl_value_t *result = jl_nothing;
    jl_array_t *ordered = NULL, *spans = NULL, *wordsv = NULL, *residue = NULL, *minws = NULL;
    JL_GC_PUSH5(&ordered, &spans, &wordsv, &residue, &minws);
    size_t nplan = 0, nres = 0;
    for (size_t k = 0; k < nord; k++)
        if (!hard[order[k]])
            nplan++;
    for (size_t i = 0; i < n; i++)
        if (dirty[i] && hard[i])
            nres++;
    if (nplan > 0 || nres > 0) {
        ordered = jl_alloc_vec_any(nplan);
        spans = jl_alloc_array_1d(jl_array_int32_type, 2 * nplan);
        residue = jl_alloc_vec_any(nres);
        static jl_value_t *uint64_vec_type = NULL;
        if (uint64_vec_type == NULL)
            uint64_vec_type = jl_apply_array_type((jl_value_t*)jl_uint64_type, 1);
        JL_GC_PROMISE_ROOTED(uint64_vec_type); // cached array type, rooted by the type cache
        minws = jl_alloc_array_1d(uint64_vec_type, nplan);
        size_t np = 0, totw = 0;
        for (size_t k = 0; k < nord; k++) {
            size_t i = order[k];
            if (hard[i])
                continue;
            jl_array_ptr_set(ordered, np, (jl_value_t*)cis[i]);
            jl_array_data(spans, int32_t)[2 * np] = (int32_t)totw;
            jl_array_data(spans, int32_t)[2 * np + 1] = (int32_t)wcount[i];
            jl_array_data(minws, uint64_t)[np] = (uint64_t)minw[i];
            totw += wcount[i];
            np++;
        }
        wordsv = jl_alloc_array_1d(jl_array_int32_type, totw);
        size_t tw = 0;
        for (size_t k = 0; k < nord; k++) {
            size_t i = order[k];
            if (hard[i])
                continue;
            if (wcount[i] > 0)
                memcpy(jl_array_data(wordsv, int32_t) + tw, words + wstart[i], wcount[i] * sizeof(int32_t));
            tw += wcount[i];
        }
        size_t nr = 0;
        for (size_t i = 0; i < n; i++)
            if (dirty[i] && hard[i])
                jl_array_ptr_set(residue, nr++, (jl_value_t*)cis[i]);
        jl_svec_t *sv = jl_alloc_svec(5);
        jl_svecset(sv, 0, ordered);
        jl_svecset(sv, 1, spans);
        jl_svecset(sv, 2, wordsv);
        jl_svecset(sv, 3, residue);
        jl_svecset(sv, 4, minws);
        result = (jl_value_t*)sv;
    }
    JL_GC_POP();
    free(order);
    free(indeg);
    free(worklist2);
    free(fill);
    free(radj);
    free(radj_off);
    free(deps);
    free(words);
    free(wcount);
    free(wstart);
    free(minw);
    free(hard);
    free(dirty);
    free(tab);
    free(cis);
    return result;
}

JL_DLLEXPORT int jl_get_force_load_scan(void) JL_NOTSAFEPOINT
{
    char *e = getenv("JULIA_LOAD_SCAN");
    return e == NULL ? 1 : atoi(e); // measurement gate: JULIA_LOAD_SCAN=0 skips
}

static void contributor_tag_method(jl_method_t *method)
{
    struct _contrib_tag env;
    env.tag = jl_object_in_image((jl_value_t*)method) ?
        (int32_t)jl_external_blob_index((jl_value_t*)method) : -1;
    env.method = (jl_value_t*)method;
    jl_foreach_top_typename_for(_typename_tag_contributor, (jl_value_t*)method->sig, 0, &env);
}

struct _typename_add_backedge_batch {
    jl_value_t *typ;
    jl_value_t **callers;
    size_t n;
};

static void _typename_add_backedge_batch(jl_typename_t *tn, int explct, void *env0)
{
    struct _typename_add_backedge_batch *env = (struct _typename_add_backedge_batch*)env0;
    JL_GC_PROMISE_ROOTED(env->typ);
    if (!explct)
        return;
    jl_genericmemory_t *allbackedges = jl_method_table->backedges;
    jl_array_t *backedges = (jl_array_t*)jl_eqtable_get(allbackedges, (jl_value_t*)tn, NULL);
    if (backedges == NULL) {
        backedges = jl_alloc_vec_any(0);
        JL_GC_PUSH1(&backedges);
        jl_genericmemory_t *newtable = jl_eqtable_put(allbackedges, (jl_value_t*)tn, (jl_value_t*)backedges, NULL);
        JL_GC_POP();
        if (newtable != allbackedges)
            jl_gc_write(jl_method_table, jl_method_table->backedges, jl_genericmemory_t, newtable);
    }
    JL_GC_PROMISE_ROOTED(backedges); // held by the method-table backedge table
    // bulk append without the duplicate/type-reuse scans: replay callers are
    // freshly loaded CodeInstances that cannot already be present, and the
    // signature is an image object whose reuse would save nothing
    size_t base = jl_array_nrows(backedges);
    jl_array_grow_end(backedges, 2 * env->n);
    for (size_t i = 0; i < env->n; i++) {
        jl_array_ptr_set(backedges, base + 2 * i, env->typ);
        jl_array_ptr_set(backedges, base + 2 * i + 1, env->callers[i]);
    }
}

// bulk variant of jl_method_table_add_backedge for backedge-log replay:
// registers the same signature for n callers with one decomposition pass
static void jl_method_table_add_backedge_batch(jl_value_t *typ, jl_value_t **callers, size_t n)
{
    if (!jl_atomic_load_relaxed(&allow_new_worlds))
        return;
    jl_methtable_t *mt = jl_method_table;
    jl_methcache_t *mc = mt->cache;
    JL_LOCK(&mc->writelock);
    if (jl_atomic_load_relaxed(&allow_new_worlds)) {
        struct _typename_add_backedge_batch env = {typ, callers, n};
        jl_foreach_top_typename_for(_typename_add_backedge_batch, typ, 0, &env);
        for (size_t i = 0; i < n; i++)
            record_backedge_log(jl_nothing, typ, callers[i]);
    }
    JL_UNLOCK(&mc->writelock);
}

// add a backedge from a non-existent signature to caller
JL_DLLEXPORT void jl_method_table_add_backedge(jl_value_t *typ, jl_code_instance_t *caller)
{
    assert(jl_is_code_instance(caller));
    if (!jl_atomic_load_relaxed(&allow_new_worlds))
        return;
    // try to pick the best cache(s) for this typ edge
    jl_methtable_t *mt = jl_method_table;
    jl_methcache_t *mc = mt->cache;
    JL_LOCK(&mc->writelock);
    if (jl_atomic_load_relaxed(&allow_new_worlds)) {
        struct _typename_add_backedge env = {typ, (jl_value_t*)caller};
        jl_foreach_top_typename_for(_typename_add_backedge, typ, 0, &env);
        record_backedge_log(jl_nothing, typ, (jl_value_t*)caller);
    }
    JL_UNLOCK(&mc->writelock);
}

struct _typename_invalidate_backedge {
    jl_value_t *type;
    jl_value_t **isect;
    jl_value_t **isect2;
    jl_method_t *const *d;
    size_t n;
    size_t max_world;
    int invalidated;
    // certificate support: when `record` is set (precompile worker),
    // invalidated callers are appended to it; when `foreign_only` is set
    // (certificate replay), entries whose caller lies in the loading closure
    // are not re-derived — their invalidations were replayed from the
    // certificate already, so dead ones are dropped and live ones kept.
    // `verify` counts closure-owned invalidations the certificate missed.
    jl_array_t *record;
    int foreign_only;
    int verify;
};

static void _typename_invalidate_backedges(jl_typename_t *tn, int explct, void *env0)
{
    struct _typename_invalidate_backedge *env = (struct _typename_invalidate_backedge*)env0;
    JL_GC_PROMISE_ROOTED(env->type);
    JL_GC_PROMISE_ROOTED(env->isect); // isJuliaType considers jl_value_t** to be a julia object too
    JL_GC_PROMISE_ROOTED(env->isect2); // isJuliaType considers jl_value_t** to be a julia object too
    jl_array_t *backedges = (jl_array_t*)jl_eqtable_get(jl_method_table->backedges, (jl_value_t*)tn, NULL);
    if (backedges == NULL)
        return;
    JL_TIMING(ADD_METHOD, ACTIVATE_TnScan);
    jl_value_t **d = jl_array_ptr_data(backedges);
    size_t i, na = jl_array_nrows(backedges);
    size_t ins = 0;
    for (i = 1; i < na; i += 2) {
        jl_value_t *backedgetyp = d[i - 1];
        JL_GC_PROMISE_ROOTED(backedgetyp);
        jl_code_instance_t *backedge = (jl_code_instance_t*)d[i];
        JL_GC_PROMISE_ROOTED(backedge);
        if (env->foreign_only && object_in_loading_closure((jl_value_t*)backedge)) {
            // the precompile worker saw this entry (its caller is owned by the
            // dependency closure) and its invalidation, if any, was replayed
            // from the certificate: drop it if the caller is dead, keep it
            // otherwise, but do not re-derive the intersection
            if (jl_atomic_load_relaxed(&backedge->max_world) != ~(size_t)0)
                continue;
            d[ins++] = d[i - 1];
            d[ins++] = d[i - 0];
            continue;
        }
        int missing = 0;
        if (jl_type_intersection2(backedgetyp, (jl_value_t*)env->type, env->isect, env->isect2)) {
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
            for (size_t j = 0; j < env->n; j++) {
                jl_method_t *m = env->d[j];
                JL_GC_PROMISE_ROOTED(m);
                if (jl_subtype(*env->isect, m->sig) || (*env->isect2 && jl_subtype(*env->isect2, m->sig))) {
                    // We now know that there actually was a previous
                    // method for this part of the type intersection.
                    if (!jl_type_morespecific(env->type, m->sig)) {
                        missing = 0;
                        break;
                    }
                }
            }
        }
        *env->isect = *env->isect2 = NULL;
        if (missing) {
            if (env->verify && object_in_loading_closure((jl_value_t*)backedge) &&
                jl_atomic_load_relaxed(&backedge->max_world) == ~(size_t)0) {
                // certificate replay would have clamped this closure-owned
                // caller already; reaching here live means the worker missed it
                jl_contrib_stats[4]++;
                jl_method_instance_t *cmi = jl_get_ci_mi(backedge);
                jl_method_t *cm = (jl_method_t*)cmi->def.method;
                jl_safe_printf("TN-CERT VERIFY MISMATCH: caller %s.%s (blob %d) via typename %s\n",
                               jl_is_method(cm) ? jl_symbol_name(cm->module->name) : "?",
                               jl_is_method(cm) ? jl_symbol_name(cm->name) : "?",
                               (int)jl_external_blob_index((jl_value_t*)backedge),
                               jl_symbol_name(tn->name));
            }
            invalidate_code_instance(backedge, env->max_world, 0);
            env->invalidated = 1;
            if (env->record)
                jl_array_ptr_1d_push(env->record, (jl_value_t*)backedge);
            if (_jl_debug_method_invalidation)
                jl_array_ptr_1d_push(_jl_debug_method_invalidation, (jl_value_t*)backedgetyp);
        }
        else {
            d[ins++] = d[i - 1];
            d[ins++] = d[i - 0];
        }
    }
    if (ins == 0)
        jl_eqtable_pop(jl_method_table->backedges, (jl_value_t*)tn, NULL, NULL);
    else if (na != ins)
        jl_array_del_end(backedges, na - ins);
}

struct invalidate_mt_env {
    jl_value_t *newentry_sig;
    jl_array_t *shadowed;
    size_t max_world;
};
static int invalidate_mt_cache(jl_typemap_entry_t *oldentry, void *closure0)
{
    struct invalidate_mt_env *env = (struct invalidate_mt_env*)closure0;
    JL_GC_PROMISE_ROOTED(env->newentry_sig);
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
            intersects = !jl_has_empty_intersection((jl_value_t*)oldentry->sig, env->newentry_sig);
        }
        if (intersects && oldentry->guardsigs != jl_emptysvec) {
            // similarly, if it already matches an existing guardsigs, this is already safe to keep
            size_t i, l;
            for (i = 0, l = jl_svec_len(oldentry->guardsigs); i < l; i++) {
                // see corresponding code in jl_typemap_entry_assoc_exact
                if (jl_subtype(env->newentry_sig, jl_svecref(oldentry->guardsigs, i))) {
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

static void _method_table_invalidate(jl_methcache_t *mc, void *env0)
{
    // drop this method from mc->cache
    jl_typemap_visitor(jl_atomic_load_relaxed(&mc->cache), disable_mt_cache, env0);
    jl_genericmemory_t *leafcache = jl_atomic_load_relaxed(&mc->leafcache);
    size_t i, l = leafcache->length;
    for (i = 1; i < l; i += 2) {
        jl_typemap_entry_t *oldentry = (jl_typemap_entry_t*)jl_genericmemory_ptr_ref(leafcache, i);
        if (oldentry) {
            while ((jl_value_t*)oldentry != jl_nothing) {
                disable_mt_cache(oldentry, env0);
                oldentry = jl_atomic_load_relaxed(&oldentry->next);
            }
        }
    }
}

static void jl_method_table_invalidate(jl_method_t *replaced, size_t max_world)
{
    if (jl_options.incremental && jl_generating_output())
        jl_error("Method deletion is not possible during Module precompile.");
    assert(!replaced->is_for_opaque_closure);
    assert(jl_atomic_load_relaxed(&jl_world_counter) == max_world);
    // Invalidate the backedges
    int invalidated = 0;
    jl_value_t *specializations = jl_atomic_load_relaxed(&replaced->specializations);
    JL_GC_PUSH1(&specializations);
    if (!jl_is_svec(specializations))
        specializations = (jl_value_t*)jl_svec1(specializations);
    size_t i, l = jl_svec_len(specializations);
    for (i = 0; i < l; i++) {
        jl_method_instance_t *mi = (jl_method_instance_t*)jl_svecref(specializations, i);
        if ((jl_value_t*)mi != jl_nothing) {
            invalidated = 1;
            invalidate_backedges(mi, max_world, "jl_method_table_disable");
        }
    }

    jl_methtable_t *mt = jl_method_get_table(replaced);
    struct disable_mt_env mt_cache_env;
    mt_cache_env.max_world = max_world;
    mt_cache_env.replaced = replaced;
    _method_table_invalidate(mt->cache, &mt_cache_env);
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
                jl_gc_wb(mi, NULL);
                mi->backedges = 0;
            }
        }
    }
    else {
        jl_method_instance_t *mi = (jl_method_instance_t*)specializations;
        jl_gc_wb(mi, NULL);
        mi->backedges = 0;
    }
    JL_UNLOCK(&method->writelock);
    return 1;
}

static int erase_all_backedges(jl_methtable_t *mt, void *env)
{
    return jl_typemap_visitor(jl_atomic_load_relaxed(&mt->defs), erase_method_backedges, env);
}

JL_DLLEXPORT void jl_disable_new_worlds(void)
{
    if (jl_generating_output())
        jl_error("Disabling Method changes is not possible when generating output.");
    JL_LOCK(&world_counter_lock);
    jl_atomic_store_relaxed(&allow_new_worlds, 0);
    JL_UNLOCK(&world_counter_lock);
    jl_array_t *mod_array = jl_get_loaded_modules();
    JL_GC_PUSH1(&mod_array);
    jl_foreach_reachable_mtable(erase_all_backedges, mod_array, (void*)NULL);

    JL_LOCK(&jl_method_table->cache->writelock);
    jl_gc_write(jl_method_table, jl_method_table->backedges, jl_genericmemory_t, (jl_genericmemory_t*)jl_an_empty_memory_any);
    JL_UNLOCK(&jl_method_table->cache->writelock);
    JL_GC_POP();
}

JL_DLLEXPORT void jl_method_table_disable(jl_method_t *method)
{
    jl_methtable_t *mt = jl_method_get_table(method);
    jl_typemap_entry_t *methodentry = do_typemap_search(mt, method);
    JL_LOCK(&world_counter_lock);
    if (!jl_atomic_load_relaxed(&allow_new_worlds))
        jl_error("Method changes have been disabled via a call to disable_new_worlds.");
    int enabled = jl_atomic_load_relaxed(&methodentry->max_world) == ~(size_t)0;
    if (enabled) {
        // Narrow the world age on the method to make it uncallable
        size_t world = jl_atomic_load_relaxed(&jl_world_counter);
        assert(method == methodentry->func.method);
        jl_atomic_store_relaxed(&method->dispatch_status, 0);
        assert(jl_atomic_load_relaxed(&methodentry->max_world) == ~(size_t)0);
        jl_atomic_store_relaxed(&methodentry->max_world, world);
        jl_method_table_invalidate(method, world);
        if (jl_method_get_table(method) == jl_method_table) {
            // deletions poison the typename for certificate replay
            struct _contrib_tag env = { -1, jl_nothing };
            jl_foreach_top_typename_for(_typename_tag_contributor, (jl_value_t*)method->sig, 0, &env);
        }
        jl_atomic_store_release(&jl_world_counter, world + 1);
    }
    JL_UNLOCK(&world_counter_lock);
    if (!enabled)
        jl_errorf("Method of %s already disabled", jl_symbol_name(method->name));
}

jl_typemap_entry_t *jl_method_table_add(jl_methtable_t *mt, jl_method_t *method, jl_tupletype_t *simpletype)
{
    JL_TIMING(ADD_METHOD, ADD_METHOD);
    assert(jl_is_method(method));
    assert(jl_is_mtable(mt));
    // n.b. no jl_timing_show_method here: formatting the signature dominates
    // the zone's self time under a connected profiler
    jl_typemap_entry_t *newentry = NULL;
    JL_GC_PUSH1(&newentry);
    // add our new entry
    assert(jl_atomic_load_relaxed(&method->primary_world) == ~(size_t)0); // min-world
    assert((jl_atomic_load_relaxed(&method->dispatch_status) & METHOD_SIG_LATEST_WHICH) == 0);
    assert((jl_atomic_load_relaxed(&method->dispatch_status) & METHOD_SIG_LATEST_ONLY) == 0);
    JL_LOCK(&mt->cache->writelock);
    newentry = jl_typemap_alloc((jl_tupletype_t*)method->sig, simpletype, jl_emptysvec, (jl_value_t*)method, ~(size_t)0, 1);
    {
        JL_TIMING(ADD_METHOD, ACTIVATE_TmapIns);
        jl_typemap_insert(&mt->defs, (jl_value_t*)mt, newentry, 0);
    }

    if (mt == jl_method_table) {
        JL_TIMING(ADD_METHOD, ACTIVATE_Tag);
        update_max_args(method->sig);
        contributor_tag_method(method);
    }
    JL_UNLOCK(&mt->cache->writelock);
    JL_GC_POP();
    return newentry;
}

static int has_key(jl_genericmemory_t *keys, jl_value_t *key)
{
    for (size_t l = keys->length, i = 0; i < l; i++) {
        jl_value_t *k = jl_genericmemory_ptr_ref(keys, i);
        if (k == NULL)
            return 0;
        if (jl_genericmemory_ptr_ref(keys, i) == key)
            return 1;
    }
    return 0;
}

// Check if m2 is in m1's interferences set, which means !morespecific(m1, m2)
static int method_in_interferences(jl_method_t *m2, jl_method_t *m1)
{
    return has_key(jl_atomic_load_relaxed(&m1->interferences), (jl_value_t*)m2);
}

// Find the index of a method in the method match array
static int find_method_in_matches(jl_array_t *t, jl_method_t *method)
{
    size_t len = jl_array_nrows(t);
    for (size_t i = 0; i < len; i++) {
        jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(t, i);
        if (matc->method == method)
            return i;
    }
    return -1;
}

// Recursively check if any method in interferences covers the given type signature
static int check_interferences_covers(jl_method_t *m, jl_value_t *ti, jl_array_t *t, arraylist_t *visited, arraylist_t *seen)
{
    arraylist_t workqueue;
    arraylist_new(&workqueue, 0);
    arraylist_push(&workqueue, m);
    arraylist_push(seen, (void*)m);
    int result = 0;
    while (workqueue.len > 0) {
        jl_method_t *current_m = (jl_method_t*)arraylist_pop(&workqueue);
        JL_GC_PROMISE_ROOTED(current_m);
        jl_genericmemory_t *interferences = jl_atomic_load_relaxed(&current_m->interferences);
        for (size_t i = 0; i < interferences->length; i++) {
            jl_method_t *m2 = (jl_method_t*)jl_genericmemory_ptr_ref(interferences, i);
            if (m2 == NULL)
                continue;
            // Check if we already visited this method
            int in_seen = 0;
            for (size_t i = 0; i < seen->len; i++) {
                if (seen->items[i] == (void*)m2) {
                    in_seen = 1;
                    break;
                }
            }
            if (in_seen)
                continue;
            arraylist_push(seen, (void*)m2);
            int idx = find_method_in_matches(t, m2);
            if (idx < 0)
                continue;
            if (method_in_interferences(current_m, m2))
                continue; // ambiguous
            assert(visited->items[idx] != (void*)0);
            if (visited->items[idx] != (void*)1)
                continue; // part of the same SCC cycle (handled by ambiguity later)
            if (jl_subtype(ti, m2->sig)) {
                result = 1;
                goto cleanup;
            }
            arraylist_push(&workqueue, m2);
        }
    }
cleanup:
    seen->len = 0;
    arraylist_free(&workqueue);
    return result;
}

static int check_fully_ambiguous(jl_method_t *m, jl_value_t *ti, jl_array_t *t, int include_ambiguous, int *has_ambiguity)
{
    jl_genericmemory_t *interferences = jl_atomic_load_relaxed(&m->interferences);
    for (size_t i = 0; i < interferences->length; i++) {
        jl_method_t *m2 = (jl_method_t*)jl_genericmemory_ptr_ref(interferences, i);
        if (m2 == NULL)
            continue;
        int idx = find_method_in_matches(t, m2);
        if (idx < 0)
            continue;
        if (!method_in_interferences(m, m2))
            continue;
        *has_ambiguity = 1;
        if (!include_ambiguous && jl_subtype(ti, m2->sig))
            return 1;
    }
    return 0;
}

// Recursively check if target_method is in the interferences of (morespecific than) start_method, but not the reverse
static int method_morespecific_via_interferences(jl_method_t *target_method, jl_method_t *start_method)
{
    if (target_method == start_method)
        return 0;
    // Check direct interferences first
    if (method_in_interferences(start_method, target_method))
        return 0;
    if (method_in_interferences(target_method, start_method))
        return 1;
    arraylist_t seen;
    arraylist_t workqueue;
    arraylist_new(&seen, 0);
    arraylist_push(&seen, (void*)start_method);
    arraylist_new(&workqueue, 0);
    arraylist_push(&workqueue, start_method);
    int result = 0;
    while (workqueue.len > 0) {
        jl_method_t *current = (jl_method_t*)arraylist_pop(&workqueue);
        JL_GC_PROMISE_ROOTED(current);
        jl_genericmemory_t *interferences = jl_atomic_load_relaxed(&current->interferences);
        for (size_t i = 0; i < interferences->length; i++) {
            jl_method_t *interference_method = (jl_method_t*)jl_genericmemory_ptr_ref(interferences, i);
            if (interference_method == NULL)
                continue;
            // Check if we're already visiting this interference method (cycle prevention)
            int already_seen = 0;
            for (size_t j = 0; j < seen.len; j++) {
                if (seen.items[j] == (void*)interference_method) {
                    already_seen = 1;
                    break;
                }
            }
            if (already_seen)
                continue;
            arraylist_push(&seen, interference_method);
            if (method_in_interferences(current, interference_method))
                continue; // only follow edges to morespecific methods in search of morespecific target (skip ambiguities)
            // Check direct interferences for this interference method
            if (method_in_interferences(interference_method, target_method))
                continue; // return 0 for this path
            if (method_in_interferences(target_method, interference_method)) {
                result = 1;
                goto cleanup;
            }
            arraylist_push(&workqueue, interference_method);
        }
    }
cleanup:
    arraylist_free(&workqueue);
    arraylist_free(&seen);
    //assert(result == jl_method_morespecific(target_method, start_method) || jl_has_empty_intersection(target_method->sig, start_method->sig) || jl_has_empty_intersection(start_method->sig, target_method->sig));
    return result;
}


void jl_method_table_activate(jl_typemap_entry_t *newentry)
{
    jl_method_table_activate_with_cert(newentry, NULL);
}

void jl_method_table_activate_with_cert(jl_typemap_entry_t *newentry, jl_svec_t *cert)
{
    JL_TIMING(ADD_METHOD, ACTIVATE);
    jl_method_t *method = newentry->func.method;
    jl_methtable_t *mt = jl_method_get_table(method);
    assert(jl_is_mtable(mt));
    assert(jl_is_method(method));
    // n.b. no jl_timing_show_method here (see jl_method_table_add)
    jl_value_t *type = (jl_value_t*)newentry->sig;
    jl_value_t *oldvalue = NULL;
    jl_array_t *oldmi = NULL;
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
    jl_genericmemory_t *interferences = NULL;
    jl_svec_t *newcert = NULL;
    jl_array_t *tnrecord = NULL; // missing-edge invalidations for the certificate
    JL_GC_PUSH8(&oldvalue, &oldmi, &loctag, &isect, &isect2, &interferences, &newcert, &tnrecord);
    int record_cert = jl_generating_output() && jl_options.incremental && mt == jl_method_table;
    int closure_clean = 0;
    if (jl_loading_closure_bits != NULL && mt == jl_method_table) {
        int clean = 1;
        int cdec = sig_tns_foreach(_typename_check_contributor, type, &clean);
        if (cdec < 0)
            cdec = jl_foreach_top_typename_for(_typename_check_contributor, type, 1, &clean);
        if (cdec) {
            closure_clean = clean;
        }
        else {
            jl_contrib_stats[2]++; // cannot decompose: full-table semantics
            closure_clean = 0;
        }
        jl_contrib_stats[closure_clean ? 0 : 1]++;
    }
    jl_typemap_entry_t *replaced = NULL;
    int replaying = cert != NULL && jl_svec_len(cert) == 5 && activate_replay_mode() >= 1;
    // With a certificate, the dependency-closure part of the prior world is
    // replayed below and the scan is restricted to foreign entries (methods
    // the precompile worker could not have seen). The two passes each use
    // their partial view of the intersecting set: is_replacing and the
    // missing-backedge checks are monotone in that set, so a partial view
    // can only over-invalidate, never under-invalidate.
    current_activation_clean = closure_clean;
    if (replaying && closure_clean && activate_replay_mode() != 2) {
        // contributor completeness (the invariant edge replay already relies
        // on): a closure-clean signature has no foreign contributor to any
        // typename it can reach, so the foreign-only scan is provably empty
        oldvalue = NULL;
        jl_contrib_stats[27]++;
    }
    else {
        oldvalue = get_intersect_matches(jl_atomic_load_relaxed(&mt->defs), newentry, &replaced, max_world, replaying);
    }
    current_activation_clean = 0;
    record_cert = record_cert && !replaying;
    if (replaying && replaced != NULL) {
        // a foreign method replaces this one exactly: certificate context is
        // unusable, redo the full scan
        replaying = 0;
        oldvalue = get_intersect_matches(jl_atomic_load_relaxed(&mt->defs), newentry, &replaced, max_world, 0);
    }
    jl_method_t *const *d;
    size_t j, n;
    if (oldvalue == NULL) {
        d = NULL;
        n = 0;
    }
    else {
        assert(jl_is_array(oldvalue));
        d = (jl_method_t**)jl_array_ptr_data((jl_array_t*)oldvalue);
        n = jl_array_nrows(oldvalue);
    }
    // oldmi collects invalidated instances for the dispatch-cache flush; it
    // is usually empty, so it allocates lazily at the push sites
#define OLDMI_PUSH(v) do { \
        if (oldmi == NULL) \
            oldmi = jl_alloc_vec_any(0); \
        jl_array_ptr_1d_push(oldmi, (jl_value_t*)(v)); \
    } while (0)

    // These get updated from their state stored in the caches files, since content in cache files gets added "all at once".
    int invalidated = 0;
    int dispatch_bits = METHOD_SIG_LATEST_WHICH; // Always set LATEST_WHICH
    // Check precompiled dispatch status bits
    int precompiled_status = jl_atomic_load_relaxed(&method->dispatch_status);
    if (!(precompiled_status & METHOD_SIG_PRECOMPILE_MANY))
        // This will store if this method will be currently the only result that would returned from `ml_matches` given `sig`.
        dispatch_bits |= METHOD_SIG_LATEST_ONLY; // Tentatively set, will be cleared if not applicable
    // Holds the set of all intersecting methods not more specific than this one.
    // Note: this set may be incomplete (may exclude methods whose intersection
    // is covered by another method that is morespecific than both, causing them
    // to have no relevant type intersection for sorting).
    interferences = (jl_genericmemory_t*)jl_atomic_load_relaxed(&method->interferences);
    if (replaying) {
        jl_value_t *foreign_oldvalue = oldvalue;
        // Replay the precompile worker's scan results: same prior world for
        // this method's typenames (contributor check), so the intersecting
        // set, specificity flags, and image-mi invalidations are as recorded.
        jl_value_t *cd = jl_svecref(cert, 0);
        jl_array_t *cflags = (jl_array_t*)jl_svecref(cert, 1);
        jl_value_t *cmis = jl_svecref(cert, 2);
        dispatch_bits = jl_unbox_int32(jl_svecref(cert, 3)) | METHOD_SIG_LATEST_WHICH;
        if (cd != jl_nothing) {
            oldvalue = cd;
            d = (jl_method_t**)jl_array_ptr_data(oldvalue);
            n = jl_array_nrows(oldvalue);
            int32_t *fl = jl_array_data(cflags, int32_t);
            char *morespec = (char*)alloca(n);
            for (j = 0; j < n; j++)
                morespec[j] = (char)(fl[j] & 1);
            for (j = 0; j < n; j++) {
                jl_method_t *m = d[j];
                char ambig = (char)((fl[j] >> 1) & 1);
                int m_dispatch = jl_atomic_load_relaxed(&m->dispatch_status);
                if (morespec[j] || ambig) {
                    ssize_t idx;
                    if (!has_key(interferences, (jl_value_t*)m))
                        interferences = jl_idset_put_key(interferences, (jl_value_t*)m, &idx);
                }
                if (!morespec[j]) {
                    m_dispatch &= ~METHOD_SIG_LATEST_ONLY;
                    jl_genericmemory_t *m_interferences = jl_atomic_load_relaxed(&m->interferences);
                    ssize_t idx;
                    m_interferences = jl_idset_put_key(m_interferences, (jl_value_t*)method, &idx);
                    jl_gc_write_atomic(m, m->interferences, jl_genericmemory_t, m_interferences, release);
                }
                jl_atomic_store_relaxed(&m->dispatch_status, m_dispatch);
                if (morespec[j])
                    continue;
                jl_method_instance_t *unspec = jl_atomic_load_relaxed(&m->unspecialized);
                if (unspec)
                    OLDMI_PUSH(unspec);
                // live-scan only the specializations the worker could not have
                // seen (not owned by the dependency closure)
                JL_TIMING(ADD_METHOD, ACTIVATE_SpecScan);
                loctag = jl_atomic_load_relaxed(&m->specializations);
                _Atomic(jl_method_instance_t*) *data;
                size_t l;
                if (jl_is_svec(loctag)) {
                    data = (_Atomic(jl_method_instance_t*)*)jl_svec_data(loctag);
                    l = jl_svec_len(loctag);
                }
                else {
                    data = (_Atomic(jl_method_instance_t*)*)&loctag;
                    l = 1;
                }
                jl_contrib_stats[24] += l;
                // slot-1 discriminators of the activating signature: a
                // specialization whose (concrete) first-argument key differs,
                // or whose key is off the nominal chain of an abstract bound,
                // provably cannot intersect `type` (same partition rules as
                // the foreign-disjoint bucketing)
                jl_typename_t *tkey = fdisj_sig_key(type);
                int tub_typeside = 0;
                jl_typename_t *tub = tkey != NULL ? NULL :
                    fdisj_slot1_nominal_ub(type, &tub_typeside);
                for (size_t i = 0; i < l; i++) {
                    jl_method_instance_t *mi = jl_atomic_load_relaxed(&data[i]);
                    if ((jl_value_t*)mi == jl_nothing)
                        continue;
                    if (method_in_loading_closure((jl_method_t*)mi)) {
                        jl_contrib_stats[25]++;
                        continue; // covered by the certificate's mi list
                    }
                    if (tkey != NULL || tub != NULL) {
                        jl_typename_t *skey = fdisj_sig_key(mi->specTypes);
                        if (skey != NULL) {
                            if (tkey != NULL) {
                                if (skey != tkey) {
                                    jl_contrib_stats[28]++;
                                    continue; // distinct concrete/invariant keys
                                }
                            }
                            else if (!tub_typeside) {
                                // abstract nominal bound: reject when the spec's
                                // concrete name is off its supertype chain
                                jl_datatype_t *w = (jl_datatype_t*)jl_unwrap_unionall(skey->wrapper);
                                int hit = 0, depth = 0;
                                while (w != NULL && w != jl_any_type && depth++ < 24) {
                                    if (w->name == tub) {
                                        hit = 1;
                                        break;
                                    }
                                    w = w->super;
                                }
                                if (!hit && depth < 24) {
                                    jl_contrib_stats[28]++;
                                    continue;
                                }
                            }
                        }
                    }
                    jl_contrib_stats[26]++;
                    if (jl_type_intersection2(type, mi->specTypes, &isect, &isect2)) {
                        int replaced_dispatch = is_replacing(ambig, type, m, d, n, isect, isect2, morespec);
                        int invalidatedmi = _invalidate_dispatch_backedges(mi, type, m, d, n, replaced_dispatch, ambig, max_world, morespec);
                        if (replaced_dispatch) {
                            jl_atomic_store_relaxed(&mi->dispatch_status, 0);
                            OLDMI_PUSH(mi);
                        }
                        invalidated |= invalidatedmi;
                    }
                    isect = NULL;
                    isect2 = NULL;
                }
            }
            // recorded image-mi invalidations
            if (cmis != jl_nothing) {
                jl_array_t *cma = (jl_array_t*)cmis;
                for (size_t i = 0, lc = jl_array_nrows(cma); i < lc; i += 2) {
                    jl_method_instance_t *mi = (jl_method_instance_t*)jl_array_ptr_ref(cma, i);
                    int replaced_dispatch = jl_unbox_int32(jl_array_ptr_ref(cma, i + 1));
                    jl_method_t *m = mi->def.method;
                    char ambig = 0;
                    for (j = 0; j < n; j++) {
                        if (d[j] == m) {
                            ambig = (char)((fl[j] >> 1) & 1);
                            break;
                        }
                    }
                    int invalidatedmi = _invalidate_dispatch_backedges(mi, type, m, d, n, replaced_dispatch, ambig, max_world, morespec);
                    if (replaced_dispatch) {
                        jl_atomic_store_relaxed(&mi->dispatch_status, 0);
                        OLDMI_PUSH(mi);
                    }
                    invalidated |= invalidatedmi;
                }
            }
        }
        // recorded missing-edge (mt-backedge) invalidations: closure-owned
        // callers the worker's scan of these typenames' backedge buckets
        // invalidated; the scan below then skips closure-owned entries
        jl_value_t *ctn = jl_svecref(cert, 4);
        if (ctn != jl_nothing) {
            jl_array_t *cta = (jl_array_t*)ctn;
            for (size_t i = 0, lc = jl_array_nrows(cta); i < lc; i++) {
                jl_code_instance_t *ci = (jl_code_instance_t*)jl_array_ptr_ref(cta, i);
                invalidate_code_instance(ci, max_world, 0);
                invalidated = 1;
            }
        }
        jl_contrib_stats[7]++; // replayed activations
        if (activate_replay_mode() == 2) {
            // verify: recompute the full intersecting set and check that it is
            // exactly certificate ∪ foreign scan, with matching flags
            jl_typemap_entry_t *vrepl = NULL;
            loctag = get_intersect_matches(jl_atomic_load_relaxed(&mt->defs), newentry, &vrepl, max_world, 0);
            size_t vn = loctag == NULL ? 0 : jl_array_nrows(loctag);
            jl_method_t **vdd = loctag == NULL ? NULL : (jl_method_t**)jl_array_ptr_data(loctag);
            int mism = 0;
            for (size_t i = 0; i < vn; i++) {
                jl_method_t *m = vdd[i];
                int found = 0;
                for (j = 0; j < n && !found; j++)
                    found = d[j] == m;
                if (!found && foreign_oldvalue) {
                    jl_method_t **fdd = (jl_method_t**)jl_array_ptr_data(foreign_oldvalue);
                    for (size_t k = 0, fn = jl_array_nrows(foreign_oldvalue); k < fn && !found; k++)
                        found = fdd[k] == m;
                }
                if (!found && !method_in_loading_closure(m)) {
                    // a closure-owned extra only reflects the worker's legal
                    // domination truncation (interference sets are documented
                    // under-approximations); a foreign extra is a real hole
                    mism++;
                    jl_safe_printf("  vd-extra (foreign!): %s.%s\n",
                                   jl_symbol_name(m->module->name), jl_symbol_name(m->name));
                }
            }
            if (cd != jl_nothing) {
                int32_t *vfl = jl_array_data(cflags, int32_t);
                for (j = 0; j < n; j++) {
                    jl_method_t *m = d[j];
                    int ms = jl_type_morespecific(m->sig, type);
                    int am = !ms && !jl_type_morespecific(type, m->sig);
                    if (((vfl[j] & 1) != (ms ? 1 : 0)) || (((vfl[j] >> 1) & 1) != (am ? 1 : 0))) {
                        mism++;
                        if (jl_contrib_stats[4] < 20)
                            jl_safe_printf("  flag-mismatch: %s.%s cert=%d live=(%d,%d)\n",
                                           jl_symbol_name(m->module->name), jl_symbol_name(m->name),
                                           (int)vfl[j], ms, am);
                    }
                }
            }
            if (mism) {
                jl_contrib_stats[4] += mism;
                jl_safe_printf("CERT VERIFY MISMATCH (%d) for %s.%s\n", mism,
                               jl_symbol_name(method->module->name), jl_symbol_name(method->name));
            }
            loctag = NULL;
        }
        // dispatch bits from the certificate; the foreign pass below may
        // clear LATEST_ONLY further
        oldvalue = foreign_oldvalue;
    }
    if (oldvalue) {
        // when replaying, this is the live pass over the foreign-only
        // intersecting set (replaced was handled above)
        if (replaying) {
            d = (jl_method_t**)jl_array_ptr_data(oldvalue);
            n = jl_array_nrows(oldvalue);
        }
        assert(n > 0);
        if (replaced) {
            oldvalue = (jl_value_t*)replaced;
            jl_method_t *m = replaced->func.method;
            invalidated = 1;
            method_overwrite(newentry, m);
            // This is an optimized version of below, given we know the type-intersection is exact
            jl_method_table_invalidate(m, max_world);
            int m_dispatch = jl_atomic_load_relaxed(&m->dispatch_status);
            // Clear METHOD_SIG_LATEST_ONLY and METHOD_SIG_LATEST_WHICH bits
            jl_atomic_store_relaxed(&m->dispatch_status, 0);
            if (!(m_dispatch & METHOD_SIG_LATEST_ONLY))
                dispatch_bits &= ~METHOD_SIG_LATEST_ONLY;
            // Take over the interference list from the replaced method
            jl_genericmemory_t *m_interferences = jl_atomic_load_relaxed(&m->interferences);
            if (interferences->length == 0) {
                interferences = jl_genericmemory_copy(m_interferences);
            }
            else {
                for (size_t i = 0; i < m_interferences->length; i++) {
                    jl_value_t *k = jl_genericmemory_ptr_ref(m_interferences, i);
                    if (k && !has_key(interferences, (jl_value_t*)k)) {
                        ssize_t idx;
                        interferences = jl_idset_put_key(interferences, (jl_value_t*)k, &idx);
                    }
                }
            }
            ssize_t idx;
            m_interferences = jl_idset_put_key(m_interferences, (jl_value_t*)method, &idx);
            jl_gc_write_atomic(m, m->interferences, jl_genericmemory_t, m_interferences, release);
            for (j = 0; j < n; j++) {
                jl_method_t *m2 = d[j];
                if (m2 && method_in_interferences(m, m2)) {
                    jl_genericmemory_t *m2_interferences = jl_atomic_load_relaxed(&m2->interferences);
                    ssize_t idx;
                    m2_interferences = jl_idset_put_key(m2_interferences, (jl_value_t*)method, &idx);
                    jl_gc_write_atomic(m2, m2->interferences, jl_genericmemory_t, m2_interferences, release);
                }
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
            for (size_t i = 0; i < l; i++) {
                jl_method_instance_t *mi = jl_atomic_load_relaxed(&data[i]);
                if ((jl_value_t*)mi == jl_nothing)
                    continue;
                OLDMI_PUSH(mi);
            }
            jl_method_instance_t *unspec = jl_atomic_load_relaxed(&m->unspecialized);
            if (unspec)
                OLDMI_PUSH(unspec);
            d = NULL;
            n = 0;
        }
        else {
            char *morespec = (char*)alloca(n);
            // Compute all morespec values upfront
            for (j = 0; j < n; j++)
                morespec[j] = (char)jl_type_morespecific(d[j]->sig, type);
            if (record_cert) {
                newcert = jl_alloc_svec(5);
                jl_svecset(newcert, 0, oldvalue);
                jl_value_t *cf = (jl_value_t*)jl_alloc_array_1d(jl_array_int32_type, n);
                jl_svecset(newcert, 1, cf);
                jl_svecset(newcert, 2, jl_alloc_vec_any(0));
            }
            for (j = 0; j < n; j++) {
                jl_method_t *m = d[j];
                // Compute ambig state: is there an ambiguity between new method and old m?
                char ambig = !morespec[j] && !jl_type_morespecific(type, m->sig);
                if (newcert)
                    jl_array_data((jl_array_t*)jl_svecref(newcert, 1), int32_t)[j] =
                        (int32_t)((morespec[j] ? 1 : 0) | (ambig ? 2 : 0));
                // Compute updates to the dispatch state bits
                int m_dispatch = jl_atomic_load_relaxed(&m->dispatch_status);
                if (morespec[j] || ambig) {
                    // !morespecific(new, old)
                    dispatch_bits &= ~METHOD_SIG_LATEST_ONLY;
                    // Add the old method to this interference set
                    ssize_t idx;
                    if (!has_key(interferences, (jl_value_t*)m))
                        interferences = jl_idset_put_key(interferences, (jl_value_t*)m, &idx);
                }
                if (!morespec[j]) {
                    // !morespecific(old, new)
                    m_dispatch &= ~METHOD_SIG_LATEST_ONLY;
                    // Add the new method to its interference set
                    jl_genericmemory_t *m_interferences = jl_atomic_load_relaxed(&m->interferences);
                    ssize_t idx;
                    m_interferences = jl_idset_put_key(m_interferences, (jl_value_t*)method, &idx);
                    jl_gc_write_atomic(m, m->interferences, jl_genericmemory_t, m_interferences, release);
                }
                // Add methods that intersect but are not more specific to interference list
                jl_atomic_store_relaxed(&m->dispatch_status, m_dispatch);
                if (morespec[j])
                    continue;

                // Now examine if this caused any invalidations.
                jl_method_instance_t *unspec = jl_atomic_load_relaxed(&m->unspecialized);
                if (unspec)
                    OLDMI_PUSH(unspec);
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
                for (size_t i = 0; i < l; i++) {
                    jl_method_instance_t *mi = jl_atomic_load_relaxed(&data[i]);
                    if ((jl_value_t*)mi == jl_nothing)
                        continue;
                    if (jl_type_intersection2(type, mi->specTypes, &isect, &isect2)) {
                        // Replacing a method--see if this really was the selected method previously
                        // over the intersection (not ambiguous) and the new method will be selected now (morespec).
                        // TODO: this only checks pair-wise for ambiguities, but the ambiguities could arise from the interaction of multiple methods
                        // and thus might miss a case where we introduce an ambiguity between`.u two existing methods
                        // We could instead work to sort this into 3 groups `morespecific .. ambiguous .. lesspecific`, with `type` in ambiguous,
                        // such that everything in `morespecific` dominates everything in `ambiguous`, and everything in `ambiguous` dominates everything in `lessspecific`
                        // And then compute where each isect falls, and whether it changed group--necessitating invalidation--or not.
                        int replaced_dispatch = is_replacing(ambig, type, m, d, n, isect, isect2, morespec);
                        if (newcert) {
                            jl_array_t *cmis = (jl_array_t*)jl_svecref(newcert, 2);
                            jl_array_ptr_1d_push(cmis, (jl_value_t*)mi);
                            loctag = jl_box_int32(replaced_dispatch);
                            jl_array_ptr_1d_push(cmis, loctag);
                        }
                        // found that this specialization dispatch got replaced by m
                        // call invalidate_backedges(mi, max_world, "jl_method_table_insert");
                        // but ignore invoke-type edges
                        int invalidatedmi = _invalidate_dispatch_backedges(mi, type, m, d, n, replaced_dispatch, ambig, max_world, morespec);
                        if (replaced_dispatch) {
                            jl_atomic_store_relaxed(&mi->dispatch_status, 0);
                            OLDMI_PUSH(mi);
                        }
                        if (_jl_debug_method_invalidation && invalidatedmi) {
                            jl_array_ptr_1d_push(_jl_debug_method_invalidation, (jl_value_t*)mi);
                            loctag = jl_cstr_to_string("jl_method_table_insert");
                            jl_array_ptr_1d_push(_jl_debug_method_invalidation, loctag);
                        }
                        invalidated |= invalidatedmi;
                    }
                    // TODO: do we have any interesting cases left where isect3 is useful
                    //jl_value_t *isect3 = NULL;
                    //jl_value_t *isect4 = NULL;
                    //jl_value_t *isect5 = NULL;
                    //JL_GC_PUSH3(&isec3, &isect4, &isect5);
                    //isect3 = jl_type_intersection(m->sig, (jl_value_t*)mi->specTypes);
                    //jl_type_intersection2(type, isect3, &isect4, &isect5);
                    //if (!jl_types_equal(isect, isect4) && (!isect2 || !jl_types_equal(isect2, isect4)) &&
                    //    (!isect5 || (!jl_types_equal(isect, isect5) && (!isect2 || !jl_types_equal(isect2, isect5))))) {
                    //    jl_(type);
                    //    jl_(mi->specTypes);
                    //    jl_(m->sig);
                    //}
                    //JL_GC_POP();
                    isect = NULL;
                    isect2 = NULL;
                }
            }
        }
    }

    jl_methcache_t *mc = jl_method_table->cache;
    if (record_cert && !replaced)
        tnrecord = jl_alloc_vec_any(0);
    JL_LOCK(&mc->writelock);
    struct _typename_invalidate_backedge typename_env = {type, &isect, &isect2, d, n, max_world, invalidated,
                                                         tnrecord,
                                                         /* foreign_only */ replaying && activate_replay_mode() != 2,
                                                         /* verify */ replaying && activate_replay_mode() == 2 && closure_clean};
    if (!jl_foreach_top_typename_for(_typename_invalidate_backedges, type, 1, &typename_env)) {
        // if the new method cannot be split into exact backedges, scan the whole table for anything that might be affected
        jl_genericmemory_t *allbackedges = jl_method_table->backedges;
        for (size_t i = 0, n = allbackedges->length; i < n; i += 2) {
            jl_value_t *tn = jl_genericmemory_ptr_ref(allbackedges, i);
            jl_value_t *backedges = jl_genericmemory_ptr_ref(allbackedges, i+1);
            if (tn && tn != jl_nothing && backedges)
                _typename_invalidate_backedges((jl_typename_t*)tn, 0, &typename_env);
        }
    }
    invalidated |= typename_env.invalidated;
    if (oldmi && jl_array_nrows(oldmi)) {
        // drop leafcache and search mc->cache and drop anything that might overlap with the new method
        // this is very cheap, so we don't mind being very conservative at over-approximating this
        JL_TIMING(ADD_METHOD, ACTIVATE_MCache);
        struct invalidate_mt_env mt_cache_env;
        mt_cache_env.max_world = max_world;
        mt_cache_env.shadowed = oldmi;
        mt_cache_env.newentry_sig = (jl_value_t*)newentry->sig;

        jl_typemap_visitor(jl_atomic_load_relaxed(&mc->cache), invalidate_mt_cache, (void*)&mt_cache_env);
        jl_genericmemory_t *leafcache = jl_atomic_load_relaxed(&mc->leafcache);
        size_t i, l = leafcache->length;
        for (i = 1; i < l; i += 2) {
            jl_value_t *entry = jl_genericmemory_ptr_ref(leafcache, i);
            if (entry) {
                while (entry != jl_nothing) {
                    jl_atomic_store_relaxed(&((jl_typemap_entry_t*)entry)->max_world, max_world);
                    entry = (jl_value_t*)jl_atomic_load_relaxed(&((jl_typemap_entry_t*)entry)->next);
                }
            }
        }
        jl_gc_write_atomic(mc, mc->leafcache, jl_genericmemory_t, (jl_genericmemory_t*)jl_an_empty_memory_any, relaxed);
    }
    JL_UNLOCK(&mc->writelock);
    if (invalidated && _jl_debug_method_invalidation) {
        jl_array_ptr_1d_push(_jl_debug_method_invalidation, (jl_value_t*)method);
        loctag = jl_cstr_to_string("jl_method_table_insert");
        jl_array_ptr_1d_push(_jl_debug_method_invalidation, loctag);
    }
    jl_atomic_store_relaxed(&newentry->max_world, ~(size_t)0);
    jl_atomic_store_relaxed(&method->dispatch_status, dispatch_bits); // TODO: this should be sequenced fully after the world counter store
    jl_gc_write_atomic(method, method->interferences, jl_genericmemory_t, interferences, release);
#undef OLDMI_PUSH
    if (record_cert && !replaced) {
        if (newcert == NULL) { // n == 0: record just the dispatch bits
            newcert = jl_alloc_svec(5);
            jl_svecset(newcert, 0, jl_nothing);
            jl_svecset(newcert, 1, jl_nothing);
            jl_svecset(newcert, 2, jl_nothing);
        }
        loctag = jl_box_int32(dispatch_bits);
        jl_svecset(newcert, 3, loctag);
        jl_svecset(newcert, 4, tnrecord != NULL && jl_array_nrows(tnrecord) > 0 ?
                               (jl_value_t*)tnrecord : jl_nothing);
        record_activation_cert(method, newcert);
    }
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
    jl_method_table_activate(newentry);
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
        jl_fprint_critical_error(ios_safe_stderr, 0, 0, NULL, jl_current_task);
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

// hook provided for caching-enhanced fast method instance lookup
// for when the expensive caching cost is justified for some reason
JL_DLLEXPORT jl_value_t *jl_method_lookup_by_tt(jl_tupletype_t *tt, size_t world, jl_value_t *_mt)
{
    jl_methtable_t *mt;
    if (_mt == jl_nothing) {
        mt = jl_method_table;
    }
    else {
        assert(jl_is_mtable(_mt));
        mt = (jl_methtable_t*) _mt;
    }
    jl_methcache_t *mc = mt->cache;
    jl_method_instance_t *mi = jl_mt_assoc_by_type(mt, mc, tt, world);
    if (!mi)
        return jl_nothing;
    return (jl_value_t*)mi;
}

// hook provided for legacy staticdata lookups
JL_DLLEXPORT jl_value_t *jl_gf_invoke_lookup(jl_value_t *types, jl_value_t *mt, size_t world);
jl_method_instance_t *jl_builtin_method_lookup(jl_value_t *builtin)
{
    jl_datatype_t *dt = (jl_datatype_t*)jl_typeof(builtin);
    jl_value_t *params[2];
    params[0] = dt->name->wrapper;
    params[1] = jl_tparam0(jl_anytuple_type);
    jl_tupletype_t *tt = (jl_datatype_t*)jl_apply_tuple_type_v(params, 2);
    JL_GC_PUSH1(&tt);
    jl_method_t *m = (jl_method_t*)jl_gf_invoke_lookup((jl_value_t*)tt, (jl_value_t*)jl_method_table, 1);
    assert(jl_is_method(m) && jl_atomic_load_relaxed(&m->unspecialized));
    JL_GC_POP();
    return jl_atomic_load_relaxed(&m->unspecialized);
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
        mt = (jl_value_t*)jl_method_table;
    jl_methcache_t *mc = ((jl_methtable_t*)mt)->cache;
    return ml_matches((jl_methtable_t*)mt, mc, types, lim, include_ambiguous, 1, world, 1, min_valid, max_valid, ambig);
}

JL_DLLEXPORT jl_method_instance_t *jl_get_unspecialized(jl_method_t *def JL_PROPAGATES_ROOT)
{
    // one unspecialized version of a function can be shared among all cached specializations
    if (!jl_is_method(def)) {
        // generated functions might instead randomly just never get inferred, sorry
        return (jl_method_instance_t*)jl_nothing;
    }
    jl_method_instance_t *unspec = jl_atomic_load_relaxed(&def->unspecialized);
    if (unspec == NULL) {
        if (def->source == NULL)
            return (jl_method_instance_t*)jl_nothing;
        JL_LOCK(&def->writelock);
        unspec = jl_atomic_load_relaxed(&def->unspecialized);
        if (unspec == NULL) {
            unspec = jl_get_specialized(def, def->sig, jl_emptysvec);
            jl_gc_write_atomic(def, def->unspecialized, jl_method_instance_t, unspec, release);
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
            jl_egal(jl_ci_owner(codeinst), owner)) {

            jl_value_t *code = jl_ci_inferred(codeinst);
            if (code)
                return (jl_value_t*)codeinst;
        }
        codeinst = jl_ci_next(codeinst);
    }
    return (jl_value_t*)jl_nothing;
}

JL_DLLEXPORT jl_value_t *jl_rettype_inferred(jl_value_t *owner, jl_method_instance_t *mi, size_t min_world, size_t max_world)
{
    return (jl_value_t*)_jl_rettype_inferred(owner, mi, min_world, max_world);
}

JL_DLLEXPORT jl_value_t *jl_rettype_inferred_native(jl_method_instance_t *mi, size_t min_world, size_t max_world) JL_NOTSAFEPOINT
{
    return (jl_value_t*)_jl_rettype_inferred(jl_nothing, mi, min_world, max_world);
}

JL_DLLEXPORT jl_value_t *(*const jl_rettype_inferred_addr)(jl_method_instance_t *mi, size_t min_world, size_t max_world) JL_NOTSAFEPOINT = jl_rettype_inferred_native;

STATIC_INLINE jl_callptr_t jl_method_compiled_callptr(jl_method_instance_t *mi, size_t world, jl_code_instance_t **codeinst_out) JL_NOTSAFEPOINT
{
    jl_code_instance_t *codeinst = jl_atomic_load_relaxed(&mi->cache);
    for (; codeinst; codeinst = jl_ci_next(codeinst)) {
        if (codeinst->owner != jl_nothing)
            continue;
        if (jl_atomic_load_relaxed(&codeinst->min_world) <= world && world <= jl_atomic_load_relaxed(&codeinst->max_world)) {
            jl_callptr_t invoke = jl_atomic_load_acquire(&codeinst->invoke);
            if (!invoke)
                continue;
            *codeinst_out = codeinst;
            return invoke;
        }
    }
    return NULL;
}

jl_code_instance_t *jl_method_compiled(jl_method_instance_t *mi, size_t world) JL_NOTSAFEPOINT
{
    jl_code_instance_t *codeinst = NULL;
    jl_method_compiled_callptr(mi, world, &codeinst);
    return codeinst;
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
    // Decrement the flag to allow reentrant callers to `@trace_compile`.
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
    if (!jl_is_method(def) || jl_is_builtinfunc(def))
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
    // Decrement the flag to allow reentrant callers to `@trace_dispatch`.
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
    // NOTE: For builtin functions, the specTypes is just `Tuple`, which is not useful to print.
    if (!jl_has_free_typevars(mi->specTypes) && (jl_datatype_t*)mi->specTypes != jl_tuple_type) {
        jl_printf(s_dispatch, "precompile(");
        jl_static_show(s_dispatch, mi->specTypes);
        jl_printf(s_dispatch, ")\n");
        if (s_dispatch != JL_STDERR)
            ios_flush(&f_dispatch);
    }
    JL_UNLOCK(&dispatch_statement_out_lock);
}

static void record_dispatch_statement_on_first_dispatch(jl_method_instance_t *mfunc) {
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

// If waitcompile is 0, this will return NULL if compiling is on-going in the JIT. This is
// useful for the JIT itself, since it just doesn't cause redundant work or missed updates,
// but merely causes it to look into the current JIT worklist.
void jl_read_codeinst_invoke(jl_code_instance_t *ci, uint8_t *specsigflags, jl_callptr_t *invoke, void **specptr, int waitcompile)
{
    uint8_t flags = jl_atomic_load_acquire(&ci->flags); // happens-before for subsequent read of fptr
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
        while (!(flags & JL_CI_FLAGS_INVOKE_MATCHES_SPECPTR)) {
            jl_cpu_pause();
            flags = jl_atomic_load_acquire(&ci->flags);
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

JL_DLLEXPORT jl_method_instance_t *jl_normalize_to_compilable_mi(jl_method_instance_t *mi JL_PROPAGATES_ROOT);

JL_DLLEXPORT void jl_add_codeinsts_to_jit(jl_array_t *codeinsts, jl_array_t *srcs)
{
    assert(jl_array_dim0(codeinsts) == jl_array_dim0(srcs));
    size_t ncodeinsts = jl_array_dim0(codeinsts);
    jl_emit_codeinsts_to_jit((jl_code_instance_t **)jl_array_ptr_data(codeinsts),
                             (jl_code_info_t **)jl_array_ptr_data(srcs),
                             ncodeinsts);
    // since the user just injected new code for mi,
    // drop any currently unspecialized caches for mi,
    // this ensures they can be recomputed on the next dispatch
    jl_array_t *shadowed = NULL;
    JL_GC_PUSH1(&shadowed);
    jl_methcache_t *mc = jl_method_table->cache;
    JL_LOCK(&mc->writelock);
    for (size_t i = 0; i < ncodeinsts; i++) {
        jl_code_instance_t *codeinst = (jl_code_instance_t*)jl_array_ptr_ref(codeinsts, i);
        jl_method_instance_t *mi = jl_get_ci_mi(codeinst);
        if (!jl_is_method(mi->def.method))
            continue;
        jl_method_t *m = mi->def.method;
        jl_method_instance_t *unspecialized = jl_atomic_load_relaxed(&m->unspecialized);
        if (unspecialized == NULL)
            continue;
        if (!shadowed)
            shadowed = jl_alloc_vec_any(1);
        jl_array_ptr_set(shadowed, 0, (jl_value_t*)unspecialized);
        struct invalidate_mt_env mt_cache_env;
        mt_cache_env.max_world = 0;
        mt_cache_env.shadowed = shadowed;
        mt_cache_env.newentry_sig = mi->specTypes;
        jl_typemap_visitor(jl_atomic_load_relaxed(&mc->cache), invalidate_mt_cache, (void*)&mt_cache_env);
        jl_genericmemory_t *leafcache = jl_atomic_load_relaxed(&mc->leafcache);
        size_t i, l = leafcache->length;
        for (i = 1; i < l; i += 2) {
            jl_value_t *entry = jl_genericmemory_ptr_ref(leafcache, i);
            if (entry) {
                while (entry != jl_nothing) {
                    jl_method_instance_t *cacheli = ((jl_typemap_entry_t*)entry)->func.linfo;
                    if (cacheli == unspecialized)
                        jl_atomic_store_relaxed(&((jl_typemap_entry_t*)entry)->max_world, 0);
                    entry = (jl_value_t*)jl_atomic_load_relaxed(&((jl_typemap_entry_t*)entry)->next);
                }
            }
        }
    }
    JL_UNLOCK(&mc->writelock);
    JL_GC_POP();
}

JL_DLLEXPORT int jl_method_is_macro(jl_method_t *m)
{
    return jl_symbol_name(m->name)[0] == '@';
}

int need_copy_to_mi_cache(jl_method_instance_t *mi, jl_method_instance_t *mi2,
    enum internal_compilation_triggers cause)
{
    return cause == TRIGGER_FOREIGN ||
        !jl_egal((jl_value_t*)mi->sparam_vals, (jl_value_t*)mi2->sparam_vals);
}

jl_code_instance_t *copy_to_mi_cache(jl_method_instance_t *mi JL_PROPAGATES_ROOT, jl_code_instance_t *codeinst2)
{
    jl_code_instance_t *codeinst = jl_get_method_uninferred(
            mi, jl_ci_rettype(codeinst2),
            jl_atomic_load_relaxed(&codeinst2->min_world),
            jl_atomic_load_relaxed(&codeinst2->max_world), // TODO: use min(max_world, current_world) here
            jl_atomic_load_relaxed(&codeinst2->debuginfo),
            jl_atomic_load_relaxed(&codeinst2->edges));
    if (jl_atomic_load_relaxed(&codeinst->invoke) == NULL) {
        // TODO: add edges and jl_promote_ci_to_current here
        jl_gc_write(codeinst, codeinst->rettype_const, jl_value_t, codeinst2->rettype_const);
        uint8_t specsigflags;
        jl_callptr_t invoke;
        void *fptr;
        jl_read_codeinst_invoke(codeinst2, &specsigflags, &invoke, &fptr, 1);
        if (fptr != NULL && (specsigflags & JL_CI_FLAGS_SPECPTR_SPECIALIZED)) {
            // A specsig specptr is ABI'd to codeinst2's MethodInstance, not to
            // `mi`, so it must not be adopted here (consumers such as
            // `linkCISymbol` and `jl_jit_abi_converter` would compute the
            // specsig ABI from the wrong specTypes). Adopt only the boxed-ABI
            // invoke wrapper, which is self-contained (it ignores its
            // CodeInstance argument).
            fptr = NULL;
        }
        if (fptr != NULL) {
            void *prev_fptr = NULL;
            // see jitlayers.cpp for the ordering restrictions here
            if (jl_atomic_cmpswap_acqrel(&codeinst->specptr.fptr, &prev_fptr, fptr)) {
                jl_atomic_store_release(&codeinst->invoke, invoke);
                // unspec is not specsig (that is checked above), but might be
                // using specptr in a compatible way (jl_fptr_args passes the
                // arguments through unmodified and jl_fptr_sparam substitutes
                // this MethodInstance's own sparam_vals)
                jl_atomic_fetch_or_relaxed(&codeinst->flags, JL_CI_FLAGS_INVOKE_MATCHES_SPECPTR);
            }
            else {
                // someone else already compiled it
                while (!(jl_atomic_load_acquire(&codeinst->flags) & JL_CI_FLAGS_INVOKE_MATCHES_SPECPTR)) {
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
    return codeinst;
}

// a cacheable sig is normally the same as a compileable sig
// except in the case where we can't execute the compileable sig without copying
// (because of jl_fptr_sparam environment usage)
static jl_value_t *normalize_to_cacheable_sig(jl_method_instance_t *mi JL_PROPAGATES_ROOT)
{
    jl_method_instance_t *mi2 = jl_normalize_to_compilable_mi(mi);
    if (mi != mi2 && need_copy_to_mi_cache(mi, mi2, TRIGGER_NONE)) // rarely true
        mi2 = mi;
    return mi2->specTypes;
}

static jl_code_instance_t *jl_compile_method_very_internal(jl_method_instance_t *mi JL_PROPAGATES_ROOT, size_t world,
    jl_value_t *F, jl_value_t **args, uint32_t nargs,
    enum internal_compilation_triggers cause)
{
    // Quick check if we already have a compiled result
    // (which also catches any builtin functions).
    jl_code_instance_t *codeinst = jl_method_compiled(mi, world);
    if (codeinst) {
        promote_cache_method(F, args, nargs, world, mi, normalize_to_cacheable_sig(mi), cause);
        return codeinst;
    }

    // And additionally we want to catch OpaqueClosure explicitly, since it is not a Builtin subtype,
    // but many of the code paths here would be invalid if we reached them.
    jl_method_t *def = mi->def.method;
    if (def == jl_opaque_closure_method) {
        codeinst = jl_method_compiled(jl_atomic_load_relaxed(&def->unspecialized), world);
        promote_cache_method(F, args, nargs, world, mi, def->sig, cause);
        return codeinst;
    }

    // We don't really want to compile (or infer) unspecialized, since it confuses various heuristics and caches,
    // so re-acquire the specialized MethodInstance, and work forward with that
    if (jl_is_method(def) && mi == jl_atomic_load_relaxed(&def->unspecialized)) {
        if (F) {
            jl_tupletype_t *tt = arg_type_tuple(F, args, nargs + 1);
            jl_svec_t *env = NULL;
            JL_GC_PUSH2(&tt, &env);
            // this just calls jl_subtype_env (since we know that `tt <: def->sig`)
            jl_value_t *ti = jl_type_intersection_env((jl_value_t*)tt, (jl_value_t*)def->sig, &env);
            assert(ti != jl_bottom_type); (void)ti;
            mi = jl_specializations_get_linfo(def, (jl_value_t*)tt, env);
            JL_GC_POP();
        }
        else {
            mi = jl_specializations_get_linfo(def, mi->specTypes, mi->sparam_vals);
        }
    }

    jl_method_instance_t *mi2 = mi;
    int compile_option = jl_options.compile_enabled;
    // disabling compilation per-module can override global setting
    if (jl_is_method(def)) {
        int mod_setting = jl_get_module_compile(((jl_method_t*)def)->module);
        if (mod_setting == JL_OPTIONS_COMPILE_OFF ||
            mod_setting == JL_OPTIONS_COMPILE_MIN)
            compile_option = ((jl_method_t*)def)->module->compile;
    }

    // if compilation is disabled or source is unavailable, try calling unspecialized version
    if (jl_is_method(def)) {
        if (compile_option == JL_OPTIONS_COMPILE_OFF ||
            compile_option == JL_OPTIONS_COMPILE_MIN ||
            def->source == jl_nothing) {
            // copy fptr from the template method definition, if present
            jl_method_instance_t *unspec = jl_atomic_load_relaxed(&def->unspecialized);
            if (unspec) {
                codeinst = jl_atomic_load_relaxed(&unspec->cache);
                if (codeinst && jl_atomic_load_acquire(&codeinst->invoke) != NULL) {
                    if (need_copy_to_mi_cache(mi, unspec, cause)) {
                        codeinst = copy_to_mi_cache(mi, codeinst);
                        mi2 = mi;
                    }
                    else {
                        mi2 = unspec;
                    }
                    promote_cache_method(F, args, nargs, world, mi2, mi == mi2 ? mi->specTypes : normalize_to_cacheable_sig(mi), cause);
                    return codeinst;
                }
                codeinst = NULL;
            }
        }
    }

    // if that didn't work and compilation is off, try running in the interpreter
    if (compile_option == JL_OPTIONS_COMPILE_OFF ||
        compile_option == JL_OPTIONS_COMPILE_MIN) {
        jl_code_info_t *src = jl_code_for_interpreter(mi, world);
        if (!jl_code_requires_compiler(src, 0)) {
            jl_debuginfo_t *di = NULL;
            jl_svec_t *edges = jl_emptysvec;
            jl_code_instance_t *codeinst = jl_new_codeinst(mi, jl_nothing,
                (jl_value_t*)jl_any_type, (jl_value_t*)jl_any_type, NULL, NULL,
                0, 1, ~(size_t)0, 0, jl_nothing, di, edges);
            jl_atomic_store_release(&codeinst->invoke, jl_fptr_interpret_call);
            jl_mi_cache_insert(mi, codeinst);
            promote_cache_method(F, args, nargs, world, mi, mi == mi2 ? mi->specTypes : normalize_to_cacheable_sig(mi), cause);
            return codeinst;
        }
    }

    // Ok, compilation is enabled. We'll need to try to compile something (probably).
    jl_atomic_store_relaxed(&mi->precompile, 1);

    // Everything from here on is considered (user facing) compile time
    uint64_t compilation_start = jl_hrtime();
    uint64_t inference_start = jl_typeinf_timing_begin(); // Special-handling for reentrancy

    // Is a recompile if there is cached code, and it was compiled (not only inferred) before
    int is_recompile = 0;
    jl_code_instance_t *codeinst_old = jl_atomic_load_relaxed(&mi->cache);
    while (codeinst_old != NULL) {
        if (jl_atomic_load_relaxed(&codeinst_old->invoke) != NULL) {
            is_recompile = 1;
            break;
        }
        codeinst_old = jl_ci_next(codeinst_old);
    }

    // jl_type_infer will internally do a cache lookup and jl_engine_reserve call
    // to synchronize this across threads
    assert(!codeinst);
    // Don't bother inferring toplevel thunks or macros - the performance cost of inference is likely
    // to significantly exceed the actual runtime.
    int should_skip_inference = !jl_is_method(mi->def.method) || jl_method_is_macro(mi->def.method);
    if (!should_skip_inference)
        codeinst = jl_type_infer(mi, world, SOURCE_MODE_ABI, jl_options.trim);

    if (codeinst) {
        mi2 = jl_get_ci_mi(codeinst);
        if (mi2 != mi) {
            if (need_copy_to_mi_cache(mi, mi2, cause)) {
                codeinst = copy_to_mi_cache(mi, codeinst);
                mi2 = mi;
            }
        }
        if (jl_is_compiled_codeinst(codeinst)) {
            promote_cache_method(F, args, nargs, world, mi2, mi2->specTypes, cause);
            jl_typeinf_timing_end(inference_start, is_recompile);
            // Already compiled - e.g. constabi, or compiled by a different thread while we were waiting.
            return codeinst;
        }
        if (compile_option == JL_OPTIONS_COMPILE_OFF) {
            jl_printf(JL_STDERR, "No compiled code available for ");
            jl_static_show(JL_STDERR, (jl_value_t*)mi);
            jl_printf(JL_STDERR, " : sysimg may not have been built with --compile=all\n");
        }

        JL_GC_PUSH1(&codeinst);
        int did_compile = jl_compile_codeinst(codeinst);
        double compile_time = jl_hrtime() - compilation_start;

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
        // primarily a bootstrapping fallback--use default heuristics
        // and try to populate some caches
        mi2 = jl_normalize_to_compilable_mi(mi);
        if (mi != mi2) {
            codeinst = jl_compile_method_very_internal(mi2, world, F, args, nargs, cause);
            if (need_copy_to_mi_cache(mi, mi2, cause)) {
                codeinst = copy_to_mi_cache(mi, codeinst);
                mi2 = mi;
            }
            else {
                jl_typeinf_timing_end(inference_start, is_recompile);
                return codeinst;
            }
        }
        else {
            codeinst = jl_method_inferred_with_abi(mi, world);
            if (!codeinst) {
                jl_method_instance_t *unspec = jl_get_unspecialized(def);
                if ((jl_value_t*)unspec == jl_nothing)
                    unspec = mi;
                else
                    codeinst = jl_method_compiled(unspec, world);
                if (!codeinst || jl_atomic_load_relaxed(&codeinst->invoke) == NULL) {
                    codeinst = jl_get_method_uninferred(unspec, (jl_value_t*)jl_any_type, 1, ~(size_t)0, NULL, NULL);
                    // ask codegen to make the fptr for unspec
                    jl_callptr_t ucache_invoke = jl_atomic_load_acquire(&codeinst->invoke);
                    if (ucache_invoke == NULL) {
                        if ((!jl_is_method(def) || def->source == jl_nothing) &&
                            !jl_cached_uninferred(jl_atomic_load_relaxed(&jl_get_ci_mi(codeinst)->cache), world)) {
                            // end the timing region before escaping, so the task's
                            // reentrant_timing bit is not left set if this is caught
                            jl_typeinf_timing_end(inference_start, is_recompile);
                            jl_throw(jl_new_struct(jl_missingcodeerror_type, (jl_value_t*)mi));
                        }
                        jl_generate_fptr_for_unspecialized(codeinst);
                    }
                }
                if (need_copy_to_mi_cache(mi, unspec, cause)) {
                    // only these care about the exact specTypes (actually sparam_vals), otherwise we can use it directly
                    codeinst = copy_to_mi_cache(mi, codeinst);
                }
                else {
                    mi2 = unspec;
                }
            }
        }
    }
    promote_cache_method(F, args, nargs, world, mi2, mi == mi2 ? mi->specTypes : normalize_to_cacheable_sig(mi), cause);
    jl_typeinf_timing_end(inference_start, is_recompile);
    return codeinst;
}

jl_code_instance_t *jl_compile_method_internal(jl_method_instance_t *mi, size_t world)
{
    return jl_compile_method_very_internal(mi, world, NULL, NULL, 0, TRIGGER_FOREIGN);
}

jl_value_t *jl_fptr_const_return(jl_value_t *f, jl_value_t **args, uint32_t nargs, jl_code_instance_t *m)
{
    return jl_ci_rettype_const(m);
}

jl_value_t *jl_fptr_args(jl_value_t *f, jl_value_t **args, uint32_t nargs, jl_code_instance_t *m)
{
    jl_fptr_args_t invoke = jl_atomic_load_relaxed(&m->specptr.fptr1);
    assert(invoke && "Forgot to set specptr for jl_fptr_args!");
    return invoke(f, args, nargs);
}

// Out-of-line entry point for `jl_sparam_defined_value`, for reads of runtime
// static-parameter env slots from generated code: returns the slot's defined
// value (a pinned uncertainty marker reads as its `==`-representative), or
// NULL when the slot is genuinely undefined.
JL_DLLEXPORT jl_value_t *jl_sparam_slot_value(jl_value_t *sp JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT
{
    return jl_sparam_defined_value(sp);
}

jl_value_t *jl_fptr_sparam(jl_value_t *f, jl_value_t **args, uint32_t nargs, jl_code_instance_t *m)
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

JL_CALLABLE(jl_f_opaque_closure_call);
JL_DLLEXPORT const jl_callptr_t jl_f_opaque_closure_call_addr = (jl_callptr_t)&jl_f_opaque_closure_call;

JL_DLLEXPORT const jl_callptr_t jl_fptr_wait_for_compiled_addr = &jl_fptr_wait_for_compiled;

// Return the index of the invoke api, if known
JL_DLLEXPORT int32_t jl_invoke_api(jl_code_instance_t *codeinst)
{
    jl_callptr_t f = jl_atomic_load_relaxed(&codeinst->invoke);
    if (f == NULL)
        return 0;
    jl_invoke_api_t t = jl_callptr_invoke_api(f);
    return t == JL_INVOKE_SPECSIG ? -1 : (int32_t)t;
}

JL_DLLEXPORT jl_value_t *jl_normalize_to_compilable_sig(jl_tupletype_t *ti, jl_svec_t *env, jl_method_t *m,
                                                        int return_if_compileable)
{
    jl_tupletype_t *tt = NULL;
    jl_svec_t *newparams = NULL;
    JL_GC_PUSH2(&tt, &newparams);
    intptr_t max_varargs = get_max_varargs(m, NULL);
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

JL_DLLEXPORT jl_method_instance_t *jl_normalize_to_compilable_mi(jl_method_instance_t *mi JL_PROPAGATES_ROOT)
{
    jl_method_t *def = mi->def.method;
    if (!jl_is_method(def) || !jl_is_datatype(mi->specTypes) || def->is_for_opaque_closure)
        return mi;
    jl_value_t *compilationsig = jl_normalize_to_compilable_sig((jl_datatype_t*)mi->specTypes, mi->sparam_vals, def, 1);
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

// return a MethodInstance for a compileable method_match, if valid
static jl_value_t *jl_method_match_to_mi(jl_method_match_t *match, size_t world, size_t min_valid, size_t max_valid)
{
    jl_method_t *m = match->method;
    JL_GC_PROMISE_ROOTED(m);
    jl_svec_t *env = match->sparams;
    jl_tupletype_t *ti = match->spec_types;
    jl_value_t *mi = jl_nothing;
    if (jl_is_datatype(ti)) {
        jl_value_t *tt = jl_normalize_to_compilable_sig(ti, env, m, 1);
        if (tt != jl_nothing) {
            JL_GC_PUSH2(&tt, &env);
            if (!jl_egal(tt, (jl_value_t*)ti)) {
                jl_value_t *ti = jl_type_intersection_env((jl_value_t*)tt, (jl_value_t*)m->sig, &env);
                assert(ti != jl_bottom_type); (void)ti;
            }
            mi = (jl_value_t*)jl_specializations_get_linfo(m, (jl_value_t*)tt, env);
            JL_GC_POP();
        }
    }
    return mi;
}

// compile-time method lookup
// intersect types with the MT, and return a single compileable specialization that covers the intersection.
jl_value_t *jl_get_specialization1(jl_tupletype_t *types, size_t world)
{
    if (jl_has_free_typevars((jl_value_t*)types))
        return jl_nothing; // don't poison the cache due to a malformed query
    if (!jl_has_concrete_subtype((jl_value_t*)types))
        return jl_nothing;

    // find if exactly 1 method matches (issue #7302)
    size_t min_valid2 = 1;
    size_t max_valid2 = ~(size_t)0;
    int ambig = 0;
    jl_value_t *matches = jl_matching_methods(types, jl_nothing, 1, 1, world, &min_valid2, &max_valid2, &ambig);
    if (matches == jl_nothing || jl_array_nrows(matches) != 1 || ambig)
        return jl_nothing;
    JL_GC_PUSH1(&matches);
    jl_method_match_t *match = (jl_method_match_t*)jl_array_ptr_ref(matches, 0);
    jl_value_t *mi = jl_method_match_to_mi(match, world, min_valid2, max_valid2);
    JL_GC_POP();
    return mi;
}

// A compile request like `precompile(f, (Type{A},))` means the runtime calls it
// denotes, and a closed type-valued argument keys runtime dispatch by egality:
// narrow such slots to the dispatch spelling `TypeEgal{A}` (#61323). `Type{Union{}}`
// stays: the bottom object is the unique instance of its `Type`.
static jl_tupletype_t *egal_normalize_hint_types(jl_tupletype_t *types JL_PROPAGATES_ROOT)
{
    jl_svec_t *newparams = NULL;
    JL_GC_PUSH1(&newparams);
    size_t i, np = jl_nparams(types);
    for (i = 0; i < np; i++) {
        jl_value_t *elt = jl_tparam(types, i);
        if (!jl_is_vararg(elt)) {
            if (elt == (jl_value_t*)jl_typeofbottom_type) {
                if (!newparams)
                    newparams = jl_svec_copy(types->parameters);
                jl_svecset(newparams, i, jl_wrap_Type(jl_bottom_type));
            }
            else if (jl_is_typeeq(elt) && !jl_has_free_typevars(elt) &&
                     jl_typeeq_T(elt) != jl_bottom_type) {
                if (!newparams)
                    newparams = jl_svec_copy(types->parameters);
                jl_svecset(newparams, i, jl_wrap_TypeEgal(jl_typeeq_T(elt)));
            }
        }
    }
    if (newparams)
        types = (jl_tupletype_t*)jl_apply_tuple_type(newparams, 1);
    JL_GC_POP();
    return types;
}

// The canonical per-method spelling of a by-type request: apply the
// egality-slot widening of `jl_compilation_sig` (egality-keyed slots that the
// method declares as concrete `Type{X}` are keyed by equality instead).
// The caller must root the result.
static jl_tupletype_t *egal_canonical_sig(jl_tupletype_t *types JL_PROPAGATES_ROOT, jl_method_t *m)
{
    jl_svec_t *newparams = NULL;
    JL_GC_PUSH1(&newparams);
    size_t i, np = jl_nparams(types);
    for (i = 0; i < np; i++) {
        if (jl_is_vararg(jl_tparam(types, i)))
            continue;
        jl_value_t *decl_i = jl_nth_slot_type(m->sig, i);
        egal_normalize_slot(types, i, decl_i, &newparams);
    }
    if (newparams)
        types = (jl_tupletype_t*)jl_apply_tuple_type(newparams, 1);
    JL_GC_POP();
    return types;
}

// Try to get a MethodInstance for a precompile() call. This uses a special kind of lookup that
// tries to find a method for which the requested signature is compileable.
JL_DLLEXPORT jl_value_t *jl_get_compile_hint_specialization(jl_tupletype_t *types JL_PROPAGATES_ROOT, size_t world)
{
    if (jl_has_free_typevars((jl_value_t*)types))
        return jl_nothing; // don't poison the cache due to a malformed query
    if (!jl_has_concrete_subtype((jl_value_t*)types))
        return jl_nothing;

    size_t min_valid2 = 1;
    size_t max_valid2 = ~(size_t)0;
    int ambig = 0;
    jl_value_t *matches = NULL;
    jl_tupletype_t *normtypes = NULL;
    JL_GC_PUSH3(&types, &matches, &normtypes);
    types = egal_normalize_hint_types(types);
    matches = jl_matching_methods(types, jl_nothing, -1, 0, world, &min_valid2, &max_valid2, &ambig);
    size_t i, n = jl_array_nrows(matches);
    if (n == 0) {
        JL_GC_POP();
        return jl_nothing;
    }
    jl_method_match_t *match = NULL;
    if (n == 1) {
        match = (jl_method_match_t*)jl_array_ptr_ref(matches, 0);
    }
    else if (jl_is_datatype(types)) {
        // first, select methods for which `types` (in its canonical per-method
        // spelling, see `egal_canonical_sig`) is compileable
        size_t count = 0;
        for (i = 0; i < n; i++) {
            jl_method_match_t *match1 = (jl_method_match_t*)jl_array_ptr_ref(matches, i);
            normtypes = egal_canonical_sig(types, match1->method);
            if (jl_isa_compileable_sig(normtypes, match1->sparams, match1->method))
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
    jl_value_t *mi = jl_nothing;
    if (match != NULL)
        mi = jl_method_match_to_mi(match, world, min_valid2, max_valid2);
    JL_GC_POP();
    return mi;
}

JL_DLLEXPORT void jl_compile_method_instance(jl_method_instance_t *mi, jl_tupletype_t *types, size_t world)
{
    uint8_t miflags = jl_atomic_load_relaxed(&mi->flags) | JL_MI_FLAGS_MASK_PRECOMPILED;
    jl_atomic_store_relaxed(&mi->flags, miflags);
    if (jl_generating_output()) {
        jl_atomic_store_relaxed(&mi->precompile, 1);
        jl_push_newly_inferred((jl_value_t*)mi);
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

JL_DLLEXPORT int jl_is_compilable(jl_tupletype_t *types)
{
    size_t world = jl_atomic_load_acquire(&jl_world_counter);
    jl_value_t *mi = jl_get_compile_hint_specialization(types, world);
    return mi == jl_nothing ? 0 : 1;
}

JL_DLLEXPORT int jl_compile_hint(jl_tupletype_t *types)
{
    size_t world = jl_atomic_load_acquire(&jl_world_counter);
    jl_value_t *mi = jl_get_compile_hint_specialization(types, world);
    if (mi == jl_nothing)
        return 0;
    JL_GC_PROMISE_ROOTED(mi);
    jl_compile_method_instance((jl_method_instance_t*)mi, types, world);
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

STATIC_INLINE jl_value_t *_jl_invoke(jl_value_t *F, jl_value_t **args, uint32_t nargs, jl_method_instance_t *mfunc, size_t world,
   enum internal_compilation_triggers cause)
{
    jl_code_instance_t *codeinst = NULL;
    jl_callptr_t invoke = jl_method_compiled_callptr(mfunc, world, &codeinst);
    if (invoke) {
        jl_value_t *res = invoke(F, args, nargs, codeinst);
        return verify_type(res);
    }
    int64_t last_alloc = jl_options.malloc_log ? jl_gc_diff_total_bytes() : 0;
    int last_errno = errno;
#ifdef _OS_WINDOWS_
    DWORD last_error = GetLastError();
#endif
    codeinst = jl_compile_method_very_internal(mfunc, world, F, args, nargs, cause);
#ifdef _OS_WINDOWS_
    SetLastError(last_error);
#endif
    errno = last_errno;
    if (jl_options.malloc_log)
        jl_gc_sync_total_bytes(last_alloc); // discard allocation count from compilation
    invoke = jl_atomic_load_acquire(&codeinst->invoke);
    jl_value_t *res = invoke(F, args, nargs, codeinst);
    return verify_type(res);
}

JL_DLLEXPORT jl_value_t *jl_invoke(jl_value_t *F, jl_value_t **args, uint32_t nargs, jl_method_instance_t *mfunc)
{
    size_t world = jl_current_task->world_age;
    return _jl_invoke(F, args, nargs, mfunc, world, TRIGGER_FOREIGN);
}

// Used by jl_eval_thunk to invoke top-level thunks.  They will be
// garbage-collectable as soon as they are invoked, so their ORC symbols must be
// unregistered before we enter invoke, which may never return.
JL_DLLEXPORT jl_value_t *jl_invoke_oneshot(jl_value_t *F, jl_value_t **args, uint32_t nargs, jl_method_instance_t *mfunc)
{
    size_t world = jl_current_task->world_age;

    int64_t last_alloc = jl_options.malloc_log ? jl_gc_diff_total_bytes() : 0;
    int last_errno = errno;
#ifdef _OS_WINDOWS_
    DWORD last_error = GetLastError();
#endif
    jl_code_instance_t *codeinst = jl_compile_method_very_internal(mfunc, world, F, args, nargs, TRIGGER_NONE);
    if (jl_options.malloc_log)
        jl_gc_sync_total_bytes(last_alloc); // discard allocation count from compilation
    uint8_t specsigflags;
    jl_callptr_t invoke;
    void *specptr;
    jl_read_codeinst_invoke(codeinst, &specsigflags, &invoke, &specptr, 1);
    jl_jit_unregister_ci(codeinst);
#ifdef _OS_WINDOWS_
    SetLastError(last_error);
#endif
    errno = last_errno;

    jl_value_t *res = invoke(F, args,  nargs, codeinst);
    return verify_type(res);
}

JL_DLLEXPORT jl_value_t *jl_invoke_oc(jl_value_t *F, jl_value_t **args, uint32_t nargs, jl_method_instance_t *mfunc)
{
    jl_opaque_closure_t *oc = (jl_opaque_closure_t*)F;
    jl_task_t *ct = jl_current_task;
    size_t last_age = ct->world_age;
    size_t world = oc->world;
    ct->world_age = world;
    jl_value_t *ret = _jl_invoke(F, args, nargs, mfunc, world, TRIGGER_NONE);
    ct->world_age = last_age;
    return ret;
}

STATIC_INLINE int sig_match_fast(jl_value_t *arg1t, jl_value_t **args, jl_value_t **sig, size_t n) JL_NOTSAFEPOINT
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
                                                       uint32_t callsite, size_t world, int for_call)
{
#ifdef JL_GF_PROFILE
    ncalls++;
#endif
#ifdef JL_TRACE
    int traceen = trace_en; //&& ((char*)&mt < jl_stack_hi-6000000);
    if (traceen && for_call)
        show_call(F, args, nargs);
#endif
    nargs++; // add f to argument count
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
    int i;
    jl_tupletype_t *tt = NULL;
    int64_t last_alloc = 0;
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
    if (i == 4) {
        // if no method was found in the associative cache, check the full cache
        JL_TIMING(METHOD_LOOKUP_FAST, METHOD_LOOKUP_FAST);
        jl_methcache_t *mc = jl_method_table->cache;
        jl_genericmemory_t *leafcache = jl_atomic_load_relaxed(&mc->leafcache);
        entry = NULL;
        int cache_entry_count = jl_atomic_load_relaxed(&((jl_datatype_t*)FT)->name->cache_entry_count);
        if (leafcache != (jl_genericmemory_t*)jl_an_empty_memory_any && (cache_entry_count == 0 || cache_entry_count >= 8)) {
            // hashing args is expensive, but so do that only if looking at mc->cache is probably even more expensive
            tt = lookup_arg_type_tuple(F, args, nargs);
            if (tt != NULL)
                entry = lookup_leafcache(leafcache, (jl_value_t*)tt, world);
        }
        if (entry == NULL) {
            jl_typemap_t *cache = jl_atomic_load_relaxed(&mc->cache); // XXX: gc root required?
            entry = jl_typemap_assoc_exact(cache, F, args, nargs, jl_cachearg_offset(), world);
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
        if (entry && for_call) {
            // mfunc was found in slow path, so log --trace-dispatch
            jl_method_instance_t *mfunc = entry->func.linfo;
            record_dispatch_statement_on_first_dispatch(mfunc);
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
        jl_methcache_t *mc = jl_method_table->cache;
        JL_GC_PUSH1(&tt);
        mfunc = jl_mt_assoc_by_type(jl_method_table, mc, tt, world);
        JL_GC_POP();
        if (jl_options.malloc_log)
            jl_gc_sync_total_bytes(last_alloc); // discard allocation count from compilation
        if (for_call) {
            if (mfunc == NULL) {
#ifdef JL_TRACE
                if (error_en)
                    show_call(F, args, nargs);
#endif
                jl_method_error(F, args, nargs, world);
                // unreachable
            }
            // mfunc was found in slow path, so log --trace-dispatch
            record_dispatch_statement_on_first_dispatch(mfunc);
        }
    }

#ifdef JL_TRACE
    if (traceen && for_call)
        jl_printf(JL_STDOUT, " at %s:%d\n", jl_symbol_name(mfunc->def.method->file), mfunc->def.method->line);
#endif

    return mfunc;
}

// introspect the expected result of jl_apply_generic (e.g. for applicable and macro expand)
jl_method_instance_t *jl_apply_lookup(jl_value_t **args, size_t nargs, size_t world)
{
    assert(nargs);
    return jl_lookup_generic_(args[0], &args[1], nargs - 1,
            jl_int32hash_fast(jl_return_address()), world, 0);
}

JL_DLLEXPORT jl_value_t *jl_apply_generic(jl_value_t *F, jl_value_t **args, uint32_t nargs)
{
    size_t world = jl_current_task->world_age;
    jl_method_instance_t *mfunc = jl_lookup_generic_(F, args, nargs,
                                                     jl_int32hash_fast(jl_return_address()),
                                                     world, 1);
    JL_GC_PROMISE_ROOTED(mfunc);
    return _jl_invoke(F, args, nargs, mfunc, world, TRIGGER_DISPATCH);
}

// buggy way to lookup a method given a list of arguments
JL_DLLEXPORT jl_method_instance_t *jl_method_lookup(jl_value_t **args, size_t nargs, size_t world)
{
    assert(nargs > 0 && "expected caller to handle this case");
    jl_methcache_t *mc = jl_method_table->cache;
    jl_typemap_t *cache = jl_atomic_load_relaxed(&mc->cache); // XXX: gc root for this?
    jl_typemap_entry_t *entry = jl_typemap_assoc_exact(cache, args[0], &args[1], nargs, jl_cachearg_offset(), world);
    if (entry)
        return entry->func.linfo;
    jl_tupletype_t *tt = arg_type_tuple(args[0], &args[1], nargs);
    JL_GC_PUSH1(&tt);
    jl_method_instance_t *mi = jl_mt_assoc_by_type(jl_method_table, mc, tt, world);
    JL_GC_POP();
    return mi;
}

static jl_method_match_t *_gf_invoke_lookup(jl_value_t *types JL_PROPAGATES_ROOT, jl_methtable_t *mt, size_t world, int cache_result_recursion, size_t *min_valid, size_t *max_valid)
{
    jl_value_t *unw = jl_unwrap_unionall((jl_value_t*)types);
    if (!jl_is_tuple_type(unw))
        return NULL;
    if (jl_tparam0(unw) == jl_bottom_type)
        return NULL;
    jl_methcache_t *mc = ((jl_methtable_t*)mt)->cache;
    jl_value_t *matches = ml_matches((jl_methtable_t*)mt, mc, (jl_tupletype_t*)types, 1, 0, 0, world, cache_result_recursion, min_valid, max_valid, NULL);
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
    if (mt == jl_nothing)
        mt = (jl_value_t*)jl_method_table;
    jl_method_match_t *matc = _gf_invoke_lookup(types, (jl_methtable_t*)mt, world, 1, &min_valid, &max_valid);
    if (matc == NULL)
        return jl_nothing;
    return (jl_value_t*)matc->method;
}


JL_DLLEXPORT jl_value_t *jl_gf_invoke_lookup_worlds(jl_value_t *types, jl_value_t *mt, size_t world, size_t *min_world, size_t *max_world)
{
    if (mt == jl_nothing)
        mt = (jl_value_t*)jl_method_table;
    jl_method_match_t *matc = _gf_invoke_lookup(types, (jl_methtable_t*)mt, world, 1, min_world, max_world);
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
            // TODO: get this mi from jl_specializations_get_linfo instead?
            mfunc = cache_result(NULL, NULL, &method->invokes, (jl_value_t*)method, tt, method, 1, 1, 1, 1, tpenv, /*tt_known_absent*/0);
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
    return _jl_invoke(gf, args, nargs - 1, mfunc, world, TRIGGER_INVOKE);
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
jl_value_t *jl_new_generic_function_with_supertype(jl_sym_t *name, jl_module_t *module, jl_datatype_t *st, size_t new_world)
{
    // type name is function name prefixed with #
    jl_sym_t *tname = jl_gf_supertype_name(name);
    jl_datatype_t *ftype = (jl_datatype_t*)jl_new_datatype(
            tname, module, st, jl_emptysvec, jl_emptysvec, jl_emptysvec, jl_emptysvec,
            0, 0, 0);
    assert(jl_is_datatype(ftype));
    JL_GC_PUSH1(&ftype);
    jl_gc_write(ftype->name, ftype->name->singletonname, jl_sym_t, name);
    jl_declare_constant_val3(NULL, module, tname, (jl_value_t*)ftype, PARTITION_KIND_CONST, new_world);
    jl_value_t *f = jl_new_struct(ftype);
    jl_gc_write(ftype, ftype->instance, jl_value_t, f);
    JL_GC_POP();
    return (jl_value_t*)f;
}

jl_value_t *jl_new_generic_function(jl_sym_t *name, jl_module_t *module, size_t new_world)
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

// callback for typemap_visitor
//
// This will exit the search early (by returning 0 / false) if the match limit is proven to be
// exceeded early. This is only best-effort, since specificity means that many matched methods
// may be sorted and removed in the output processing for ml_matches and therefore we can only
// conservatively under-approximate the matches during the search.
static int ml_matches_visitor(jl_typemap_entry_t *ml, struct typemap_intersection_env *closure0)
{
    struct ml_matches_env *closure = container_of(closure0, struct ml_matches_env, match);
    if (closure->intersections == 0 && !closure0->issubty)
        return 1;

    // First, check the world range of the typemap entry to ensure that it intersects
    // the query world. If it does not, narrow the result world range to guarantee
    // excluding it from the results is valid for the full span.
    size_t min_world = jl_atomic_load_relaxed(&ml->min_world);
    size_t max_world = jl_atomic_load_relaxed(&ml->max_world);
    if (closure->world < min_world) {
        // exclude method table entries that are part of a later world
        if (closure->match.max_valid >= min_world)
            closure->match.max_valid = min_world - 1;
        return 1;
    }
    else if (closure->world > max_world) {
        // exclude method table entries that have been replaced in the current world
        if (closure->match.min_valid <= max_world)
            closure->match.min_valid = max_world + 1;
        return 1;
    }
    if (closure->match.max_valid > max_world)
        closure->match.max_valid = max_world;
    jl_method_t *meth = ml->func.method;
    int only = jl_atomic_load_relaxed(&meth->dispatch_status) & METHOD_SIG_LATEST_ONLY;
    if (closure->lim >= 0 && only) {
        if (closure->lim == 0) {
            closure->t = jl_an_empty_vec_any;
            return 0;
        }
        closure->lim--;
    }
    closure->matc = make_method_match((jl_tupletype_t*)closure->match.ti,
        closure->match.env, meth,
        closure->match.issubty ? FULLY_COVERS : NOT_FULLY_COVERS);
    size_t len = jl_array_nrows(closure->t);
    if (closure->match.issubty && only) {
        if (len == 0)
            closure->t = (jl_value_t*)jl_alloc_vec_any(1);
        else if (len > 1)
            jl_array_del_end((jl_array_t*)closure->t, len - 1);
        jl_array_ptr_set(closure->t, 0, (jl_value_t*)closure->matc);
        return 0;
    }
    if (len == 0) {
        closure->t = (jl_value_t*)jl_alloc_vec_any(1);
        jl_array_ptr_set(closure->t, 0, (jl_value_t*)closure->matc);
    }
    else {
        jl_array_ptr_1d_push((jl_array_t*)closure->t, (jl_value_t*)closure->matc);
    }
    // don't need to consider other similar methods if this ml will always fully intersect with them and dominates all of them
    if (!closure->include_ambiguous || closure->lim != -1)
        typemap_slurp_search(ml, &closure->match);
    return 1;
}

// Visit the candidate methods, starting from t[idx], to determine a possible valid sort ordering,
// where every morespecific method appears before any method which it has a common
// intersection with but is not partly ambiguous with (ambiguity is not transitive, since morespecific is not transitive).
// Implements Tarjan's SCC (strongly connected components) algorithm, simplified to remove the count variable
// Inputs:
//  * `t`: the array of vertexes (method matches)
//  * `idx`: the next vertex to add to the output
//  * `visited`: the state of the algorithm for each vertex in `t`: either 1 if we visited it already or 1+depth if we are visiting it now
//  * `stack`: the state of the algorithm for the current vertex (up to length equal to `t`): the list of all vertexes currently in the depth-first path or in the current SCC
//  * `result`: the output of the algorithm, a sorted list of vertexes (up to length `lim`)
//  * `recursion_stack`: an array for temporary use
//  * `lim`: either -1 for unlimited matches, or the maximum length for `result` before returning failure (return -1).
//  * `include_ambiguous`: whether to filter out fully ambiguous matches from `result`
//  * `*has_ambiguity`: whether the algorithm does not need to compute if there is an unresolved ambiguity
//  * `*found_minmax`: whether there is a minmax method already found, so future fully_covers matches should be ignored
// Outputs:
//  * `*has_ambiguity`: whether there are any ambiguities that mean the sort order is not exact
// Stack frame for iterative sort_mlmatches implementation
enum sort_state {
    STATE_VISITING,            // Initial visit and setup
    STATE_PROCESSING_INTERFERENCES, // Processing interference loop
    STATE_CHECK_COVERS,        // Check coverage conditions
    STATE_FINALIZE_SCC         // SCC processing and cleanup
};

typedef struct {
    size_t idx;                    // Current method match index
    size_t interference_index;     // Current position in interferences loop
    size_t interference_count;     // Total interferences count
    size_t depth;                  // Stack depth when frame created
    size_t cycle;                  // Cycle depth tracking
    jl_method_match_t *matc;       // Current method match
    jl_method_t *m;                // Current method
    jl_value_t *ti;                // Type intersection
    int subt;                      // Subtype flag
    jl_genericmemory_t *interferences; // Method interferences
    int child_result;              // Result from child recursive call
    enum sort_state state;
} sort_stack_frame_t;

// Returns:
//  * -1: too many matches for lim, other outputs are undefined
//  *  0: the child(ren) have been added to the output
//  * 1+: the children are part of this SCC (up to this depth)
static int sort_mlmatches(jl_array_t *t, size_t idx, arraylist_t *visited, arraylist_t *stack, arraylist_t *result, arraylist_t *recursion_stack, int lim, int include_ambiguous, int *has_ambiguity, int *found_minmax)
{
    // Use arraylist_t for explicit stack of processing frames
    arraylist_t frame_stack;
    arraylist_new(&frame_stack, 0);

    // Push initial frame
    sort_stack_frame_t initial_frame = {
        .idx = idx,
        .interference_index = 0,
        .interference_count = 0,
        .depth = 0,
        .cycle = 0,
        .matc = NULL,
        .m = NULL,
        .ti = NULL,
        .subt = 0,
        .interferences = NULL,
        .child_result = 0,
        .state = STATE_VISITING
    };
    arraylist_push(&frame_stack, memcpy(malloc(sizeof(sort_stack_frame_t)), &initial_frame, sizeof(sort_stack_frame_t)));

    int final_result = 0;

    while (1) {
        sort_stack_frame_t *current = (sort_stack_frame_t*)frame_stack.items[frame_stack.len - 1];
        JL_GC_PROMISE_ROOTED(current->m);
        JL_GC_PROMISE_ROOTED(current->interferences);
        JL_GC_PROMISE_ROOTED(current->ti);

        switch (current->state) {
            case STATE_VISITING: {
                size_t cycle = (size_t)visited->items[current->idx];
                if (cycle != 0) {
                    final_result = cycle - 1;
                    goto propagate_to_parent;
                }

                arraylist_push(stack, (void*)current->idx);
                current->depth = stack->len;
                visited->items[current->idx] = (void*)(1 + current->depth);
                current->matc = (jl_method_match_t*)jl_array_ptr_ref(t, current->idx);
                current->m = current->matc->method;
                current->ti = (jl_value_t*)current->matc->spec_types;
                current->subt = current->matc->fully_covers != NOT_FULLY_COVERS;
                current->interferences = jl_atomic_load_relaxed(&current->m->interferences);
                current->cycle = current->depth;
                current->interference_count = current->interferences->length;
                current->interference_index = 0;
                current->state = STATE_PROCESSING_INTERFERENCES;
                break;
            }

            case STATE_PROCESSING_INTERFERENCES: {
                // If we have a child result to process, handle it first
                if (current->child_result != 0) {
                    if (current->child_result == -1) {
                        final_result = -1;
                        goto propagate_to_parent;
                    }
                    // record the cycle will resolve at depth "cycle"
                    if (current->child_result && current->child_result < current->cycle)
                        current->cycle = current->child_result;
                    current->child_result = 0; // Clear after processing
                }

                // Process interferences iteratively
                while (current->interference_index < current->interference_count) {
                    jl_method_t *m2 = (jl_method_t*)jl_genericmemory_ptr_ref(current->interferences, current->interference_index);
                    current->interference_index++;

                    if (m2 == NULL)
                        continue;

                    int childidx = find_method_in_matches(t, m2);
                    if (childidx < 0 || (size_t)childidx == current->idx)
                        continue;

                    int child_cycle = (size_t)visited->items[childidx];
                    if (child_cycle == 1)
                        continue; // already handled
                    if (child_cycle != 0 && child_cycle - 1 >= current->cycle)
                        continue; // already part of this cycle
                    if (method_in_interferences(current->m, m2))
                        continue;

                    // m2 is morespecific, so attempt to visit it first
                    if (child_cycle != 0) {
                        // Child already being processed, use cached result
                        int child_result = child_cycle - 1;
                        if (child_result == -1) {
                            final_result = -1;
                            goto propagate_to_parent;
                        }
                        if (child_result && child_result < current->cycle)
                            current->cycle = child_result;
                    }
                    else {
                        // Need to process child - push new frame and pause current processing
                        sort_stack_frame_t child_frame = {
                            .idx = childidx,
                            .interference_index = 0,
                            .interference_count = 0,
                            .depth = 0,
                            .cycle = 0,
                            .matc = NULL,
                            .m = NULL,
                            .ti = NULL,
                            .subt = 0,
                            .interferences = NULL,
                            .child_result = 0,
                            .state = STATE_VISITING
                        };
                        arraylist_push(&frame_stack, memcpy(malloc(sizeof(sort_stack_frame_t)), &child_frame, sizeof(sort_stack_frame_t)));
                        goto continue_main_loop; // Resume processing after child completes
                    }
                }

                current->state = STATE_CHECK_COVERS;
                break;
            }

            case STATE_CHECK_COVERS: {
                // There is some probability that this method is already fully covered
                // now, and we can delete this vertex now without anyone noticing.
                if (current->subt && *found_minmax) {
                    if (*found_minmax == 2)
                        visited->items[current->idx] = (void*)1;
                }
                else if (check_interferences_covers(current->m, current->ti, t, visited, recursion_stack)) {
                    visited->items[current->idx] = (void*)1;
                }
                else if (check_fully_ambiguous(current->m, current->ti, t, include_ambiguous, has_ambiguity)) {
                    visited->items[current->idx] = (void*)1;
                }

                // If there were no cycles hit either, then we can potentially delete all of its edges too.
                if ((size_t)visited->items[current->idx] == 1 && stack->len == current->depth) {
                    // n.b. cycle might be < depth, if we had a cycle with a child
                    // idx, but since we are on the top of the stack, nobody
                    // observed that and so we are content to ignore this
                    size_t childidx = (size_t)arraylist_pop(stack);
                    assert(childidx == current->idx); (void)childidx;
                    final_result = 0;
                    goto propagate_to_parent;
                }

                if (current->cycle != current->depth) {
                    final_result = current->cycle;
                    goto propagate_to_parent;
                }

                current->state = STATE_FINALIZE_SCC;
                break;
            }

            case STATE_FINALIZE_SCC: {
                // If this is in an SCC group, do some additional checks before returning or setting has_ambiguity
                if (current->depth != stack->len) {
                    int scc_count = 0;
                    for (size_t i = current->depth - 1; i < stack->len; i++) {
                        size_t childidx = (size_t)stack->items[i];
                        if (visited->items[childidx] == (void*)1)
                            continue;
                        scc_count++;
                    }
                    if (scc_count > 1)
                        *has_ambiguity = 1;
                }

                // copy this cycle into the results
                for (size_t i = current->depth - 1; i < stack->len; i++) {
                    size_t childidx = (size_t)stack->items[i];
                    jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(t, childidx);
                    int subt = matc->fully_covers != NOT_FULLY_COVERS;
                    if (subt && *found_minmax)
                        visited->items[childidx] = (void*)1;
                    if ((size_t)visited->items[childidx] == 1)
                        continue;
                    assert(visited->items[childidx] == (void*)(2 + i));
                    visited->items[childidx] = (void*)1;
                    if (lim == -1 || result->len < lim)
                        arraylist_push(result, (void*)childidx);
                    else {
                        final_result = -1;
                        goto propagate_to_parent;
                    }
                }

                // now finally cleanup the stack
                while (stack->len >= current->depth) {
                    size_t childidx = (size_t)arraylist_pop(stack);
                    // always remove fully_covers matches after the first minmax ambiguity group is handled
                    jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(t, childidx);
                    int subt = matc->fully_covers == FULLY_COVERS;
                    if (subt && *found_minmax == 1)
                        *found_minmax = 2;
                    assert(visited->items[childidx] == (void*)1);
                }

                final_result = 0;
                goto propagate_to_parent;
            }
        }

        continue_main_loop:
            continue;

        propagate_to_parent:
            // Propagate result to parent if exists
            free(arraylist_pop(&frame_stack));
            if (frame_stack.len == 0)
                break;
            sort_stack_frame_t *parent = (sort_stack_frame_t*)frame_stack.items[frame_stack.len - 1];
            parent->child_result = final_result;
    }
    assert(frame_stack.len == 0);
    arraylist_free(&frame_stack);
    return final_result;
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
static jl_value_t *ml_matches(jl_methtable_t *mt, jl_methcache_t *mc,
                              jl_tupletype_t *type, int lim, int include_ambiguous,
                              int intersections, size_t world, int cache_result_recursion,
                              size_t *min_valid, size_t *max_valid, int *ambig)
{
    size_t current_world = jl_atomic_load_acquire(&jl_world_counter);
    if (world > current_world)
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
    struct ml_matches_env env = {
        /* match */ {
            /* inputs */
            /* fptr / callback */ ml_matches_visitor,
            /* sig */ (jl_value_t*)type,
            /* vararg type / tparam0 */ va,

            /* temporaries */
            /* .search_slurp = */ 0,

            /* outputs */
            /* .min_valid = */ *min_valid,
            /* .max_valid = */ *max_valid,
            /* .ti = */ NULL,
            /* .env = */ jl_emptysvec,
            /* .issubty = */ 0
        },
        /* inputs */
        intersections,
        world,
        lim,
        include_ambiguous,

        /* outputs */
        /* .t = */ jl_an_empty_vec_any,

        /* temporaries */
        /* .matc = */ NULL
    };
    struct jl_typemap_assoc search = {(jl_value_t*)type, world, jl_emptysvec};
    jl_value_t *isect2 = NULL;
    JL_GC_PUSH6(&env.t, &env.matc, &env.match.env, &search.env, &env.match.ti, &isect2);

    if (mc) {
        // first check the leaf cache if the type might have been put in there
        if (((jl_datatype_t*)unw)->isdispatchtuple) {
            jl_genericmemory_t *leafcache = jl_atomic_load_relaxed(&mc->leafcache);
            jl_typemap_entry_t *entry = lookup_leafcache(leafcache, (jl_value_t*)type, world);
            if (entry) {
                // leafcache found a match, construct the MethodMatch by computing the effective
                // types + sparams and the world bounds
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
            jl_typemap_entry_t *entry = jl_typemap_assoc_by_type(jl_atomic_load_relaxed(&mc->cache), &search, jl_cachearg_offset(), /*subtype*/1);
            if (entry && (((jl_datatype_t*)unw)->isdispatchtuple || entry->guardsigs == jl_emptysvec)) {
                // full cache found a match, construct the MethodMatch by computing the effective
                // types + sparams and the world bounds
                jl_method_instance_t *mi = entry->func.linfo;
                jl_method_t *meth = mi->def.method;
                size_t min_world = jl_atomic_load_relaxed(&entry->min_world);
                // only return this if it appears min_would is fully computed, otherwise do the full lookup to compute min_world exactly
                if (min_world == jl_atomic_load_relaxed(&meth->primary_world)) {
                    size_t max_world = jl_atomic_load_relaxed(&entry->max_world);
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
                    if (*min_valid < min_world)
                        *min_valid = min_world;
                    if (*max_valid > max_world)
                        *max_valid = max_world;
                    JL_GC_POP();
                    return env.t;
                }
            }
        }
    }
    // then scan everything
    if (!jl_typemap_intersection_visitor(jl_atomic_load_relaxed(&mt->defs), 0, &env.match) && env.t == jl_an_empty_vec_any) {
        JL_GC_POP();
        // if we return early without returning methods, lim was proven to be exceeded
        // during the search set only the min/max valid collected from matching
        *min_valid = env.match.min_valid;
        *max_valid = env.match.max_valid;
        return jl_nothing;
    }
    // if we return early, set only the min/max valid collected from matching
    *min_valid = env.match.min_valid;
    *max_valid = env.match.max_valid;
    // done with many of these values now
    env.match.ti = NULL; env.matc = NULL; env.match.env = NULL; search.env = NULL;

    // all intersecting methods have been collected now. the remaining work is to sort
    // these and apply specificity to determine a list of dispatch-possible call targets
    size_t i, j, len = jl_array_nrows(env.t);

    // the 'minmax' method is a method that (1) fully-covers the queried type, and (2) is
    // more-specific than any other fully-covering method (but if !all_subtypes, there are
    // non-fully-covering methods to which it is _likely_ not more specific)
    jl_method_match_t *minmax = NULL;
    int any_subtypes = 0;
    if (len > 1) {
        // first try to pre-process the results to find the most specific option
        // among the fully-covering methods, since we can do this in O(n^2)
        // time, and the rest is O(n^3)
        //   - first find a candidate for the best of these method results
        for (i = 0; i < len; i++) {
            jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(env.t, i);
            if (matc->fully_covers == FULLY_COVERS) {
                any_subtypes = 1;
                jl_method_t *m = matc->method;
                for (j = 0; j < len; j++) {
                    if (i == j)
                        continue;
                    jl_method_match_t *matc2 = (jl_method_match_t*)jl_array_ptr_ref(env.t, j);
                    if (matc2->fully_covers == FULLY_COVERS) {
                        jl_method_t *m2 = matc2->method;
                        if (!method_morespecific_via_interferences(m, m2))
                            break;
                    }
                }
                if (j == len) {
                    // Found the minmax method
                    minmax = matc;
                    break;
                }
            }
        }
        //   - it may even dominate (be more specific than) some choices that are not fully-covering!
        //     move those into the subtype group, where we'll filter them out shortly after
        //     (potentially avoiding reporting these as an ambiguity, and
        //     potentially allowing us to hit the next fast path)
        //   - we could always check here if *any* FULLY_COVERS method is
        //     more-specific (instead of just considering minmax), but that may
        //     cost much extra and is less likely to help us hit a fast path
        //     (we will look for this later, when we compute ambig_groupid, for
        //     correctness)
        int all_subtypes = any_subtypes;
        if (any_subtypes) {
            jl_method_t *minmaxm = NULL;
            if (minmax != NULL)
                minmaxm = minmax->method;
            // scan through all the non-fully-matching methods and count them as "fully-covering" (ish)
            // (i.e. in the 'subtype' group) if `minmax` is more-specific
            for (i = 0; i < len; i++) {
                jl_method_match_t *matc = (jl_method_match_t*)jl_array_ptr_ref(env.t, i);
                if (matc->fully_covers != FULLY_COVERS) {
                    jl_method_t *m = matc->method;
                    if (minmaxm) {
                        if (method_morespecific_via_interferences(minmaxm, m)) {
                            matc->fully_covers = SENTINEL; // put a sentinel value here for sorting
                            continue;
                        }
                        if (method_in_interferences(minmaxm, m)) // !morespecific(m, minmaxm)
                            has_ambiguity = 1;
                    }
                    all_subtypes = 0;
                }
            }
        }
        //    - now we might have a fast-return here, if we see that
        //      we've already processed all of the possible outputs
        if (all_subtypes) {
            if (minmax == NULL) {
                // all intersecting methods are fully-covering, but there is no unique most-specific method
                if (!include_ambiguous) {
                    // there no unambiguous choice of method
                    len = 0;
                    env.t = jl_an_empty_vec_any;
                }
                else if (lim == 1) {
                    // we'd have to return >1 method due to the ambiguity, so bail early
                    JL_GC_POP();
                    return jl_nothing;
                }
            }
            else {
                // `minmax` is more-specific than all other matches and is fully-covering
                // we can return it as our only result
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
        arraylist_t stack, visited, result, recursion_stack;
        arraylist_new(&result, lim != -1 && lim < len ? lim : len);
        arraylist_new(&stack, 0);
        arraylist_new(&visited, len);
        arraylist_new(&recursion_stack, len);
        arraylist_grow(&visited, len);
        memset(visited.items, 0, len * sizeof(size_t));
        // if we had a minmax method (any subtypes), now may now be able to
        // quickly cleanup some of methods
        int found_minmax = 0;
        if (has_ambiguity)
            found_minmax = 1;
        else if (minmax != NULL)
            found_minmax = 2;
        else if (any_subtypes && !include_ambiguous)
            found_minmax = 1;
        has_ambiguity = 0;
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
            int child_cycle = sort_mlmatches((jl_array_t*)env.t, i, &visited, &stack, &result, &recursion_stack, lim == -1 || minmax == NULL ? lim : lim - 1, include_ambiguous, &has_ambiguity, &found_minmax);
            if (child_cycle == -1) {
                arraylist_free(&recursion_stack);
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
        arraylist_free(&recursion_stack);
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
    if (mc && cache_result_recursion && ((jl_datatype_t*)unw)->isdispatchtuple) { // cache_result_recursion prevents lock confusion and unnecessary work
        if (len == 1 && !has_ambiguity) {
            env.matc = (jl_method_match_t*)jl_array_ptr_ref(env.t, 0);
            jl_method_t *meth = env.matc->method;
            jl_svec_t *tpenv = env.matc->sparams;
            JL_LOCK(&mc->writelock);
            // a matching entry may already be present here (e.g. ml_matches fell
            // through the full-cache check above to recompute min_world exactly),
            // so cache_method must keep its own presence check to avoid a
            // duplicate insertion
            cache_result(mt, mc, &mc->cache, (jl_value_t*)mc, (jl_tupletype_t*)unw, meth, world, env.match.min_valid, env.match.max_valid, current_world, tpenv, /*tt_known_absent*/0);
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
JL_DLLEXPORT int jl_has_concrete_subtype(jl_value_t *typ)
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
    jl_method_t *meth = (jl_method_t*)jl_methtable_lookup((jl_value_t*)sigt, jl_atomic_load_acquire(&jl_world_counter));
    if (!jl_is_method(meth))
        jl_error("@ccallable: could not find requested method");
    JL_GC_PUSH1(&meth);
    if (name == jl_nothing)
        jl_gc_write(meth, meth->ccallable, jl_svec_t, jl_svec2(declrt, (jl_value_t*)sigt));
    else
        jl_gc_write(meth, meth->ccallable, jl_svec_t, jl_svec3(declrt, (jl_value_t*)sigt, name));
    JL_GC_POP();
}

// Drop all method caches and increment world age as if adding a method that intersects everything
static void invalidate_method_instance_caches(jl_method_instance_t *mi, size_t world)
{
    if ((jl_value_t*)mi == jl_nothing)
        return;

    // Walk through all code instances for this method instance
    jl_code_instance_t *ci = jl_atomic_load_relaxed(&mi->cache);
    while (ci != NULL) {
        // Invalidate this code instance by setting max_world to current world
        if (jl_atomic_load_relaxed(&ci->max_world) == ~(size_t)0) {
            jl_atomic_store_release(&ci->max_world, world);
        }
        ci = jl_ci_next(ci);
    }
}

static int invalidate_all_specializations(jl_typemap_entry_t *def, void *closure)
{
    size_t world = *(size_t*)closure;
    jl_method_t *method = def->func.method;
    JL_LOCK(&method->writelock);
    jl_value_t *specializations = jl_atomic_load_relaxed(&method->specializations);
    if (jl_is_svec(specializations)) {
        size_t i, l = jl_svec_len(specializations);
        for (i = 0; i < l; i++) {
            jl_method_instance_t *mi = (jl_method_instance_t*)jl_svecref(specializations, i);
            invalidate_method_instance_caches(mi, world);
        }
    }
    else if (specializations != NULL) {
        jl_method_instance_t *mi = (jl_method_instance_t*)specializations;
        invalidate_method_instance_caches(mi, world);
    }
    JL_UNLOCK(&method->writelock);
    return 1;
}

static void invalidate_all_caches(jl_methtable_t *mt, size_t current_world)
{
    jl_typemap_visitor(jl_atomic_load_relaxed(&mt->defs), invalidate_all_specializations, &current_world);
    drop_all_methcache(mt->cache);
}

JL_DLLEXPORT void jl_drop_all_caches(void)
{
    JL_LOCK(&world_counter_lock);

    // Get current world age - we'll invalidate everything at this world
    size_t current_world = jl_atomic_load_relaxed(&jl_world_counter);

    invalidate_all_caches(jl_method_table, current_world);

    // Increment world age - this forces all subsequent compilation to happen in the new world
    size_t new_world = current_world + 1;
    jl_atomic_store_release(&jl_world_counter, new_world);

    JL_UNLOCK(&world_counter_lock);
}


#ifdef __cplusplus
}
#endif

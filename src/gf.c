// This file is a part of Julia. License is MIT: http://julialang.org/license

/*
  Generic Functions
  . method table and lookup
  . GF constructor, add_method
  . dispatch
  . static parameter inference
  . method specialization, invoking type inference
*/
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "julia.h"
#include "julia_internal.h"
#ifndef _OS_WINDOWS_
#include <unistd.h>
#endif

// ::ANY has no effect if the number of overlapping methods is greater than this
#define MAX_UNSPECIALIZED_CONFLICTS 32

#ifdef __cplusplus
extern "C" {
#endif

static int cache_match_by_type(jl_value_t **types, size_t n, jl_tupletype_t *sig, int va)
{
    if (!va && n > jl_datatype_nfields(sig))
        return 0;
    if (jl_datatype_nfields(sig) > n) {
        if (!(n == jl_datatype_nfields(sig)-1 && va))
            return 0;
    }
    size_t i;
    for(i=0; i < n; i++) {
        jl_value_t *decl = jl_field_type(sig, i);
        if (i == jl_datatype_nfields(sig)-1) {
            if (va) {
                jl_value_t *t = jl_tparam0(decl);
                for(; i < n; i++) {
                    if (!jl_subtype(types[i], t, 0))
                        return 0;
                }
                return 1;
            }
        }
        jl_value_t *a = types[i];
        if (jl_is_type_type(a) && jl_is_type_type(decl)) {
            jl_value_t *tp0 = jl_tparam0(decl);
            if (tp0 == (jl_value_t*)jl_typetype_tvar) {
                // in the case of Type{T}, the types don't have
                // to match exactly either. this is cached as Type{T}.
                // analogous to the situation with tuples.
            }
            else {
                if (!jl_types_equal(jl_tparam0(a), tp0))
                    return 0;
            }
        }
        else if (decl == (jl_value_t*)jl_any_type) {
        }
        else {
            if (!jl_types_equal(a, decl))
                return 0;
        }
    }
    return 1;
}

static inline int cache_match(jl_value_t **args, size_t n, jl_value_t **sig,
                              int va, size_t lensig)
{
    // NOTE: This function is a huge performance hot spot!!
    for(size_t i=0; i < n; i++) {
        jl_value_t *decl = sig[i];
        if (i == lensig-1) {
            if (va) {
                jl_value_t *t = jl_tparam0(decl);
                for(; i < n; i++) {
                    if (!jl_subtype(args[i], t, 1))
                        return 0;
                }
                return 1;
            }
        }
        jl_value_t *a = args[i];
        if (decl == (jl_value_t*)jl_any_type) {
        }
        else if ((jl_value_t*)jl_typeof(a) == decl) {
            /*
              we know there are only concrete types here, and types are
              hash-consed, so pointer comparison should work.
            */
        }
        else if (jl_is_type_type(decl) && jl_is_type(a)) {
            jl_value_t *tp0 = jl_tparam0(decl);
            if (tp0 == (jl_value_t*)jl_typetype_tvar) {
                // in the case of Type{T}, the types don't have
                // to match exactly either. this is cached as Type{T}.
                // analogous to the situation with tuples.
            }
            else {
                if (a!=tp0 && !jl_types_equal(a,tp0))
                    return 0;
            }
        }
        else {
            return 0;
        }
    }
    return 1;
}

static inline
jl_methlist_t *mtcache_hash_lookup(jl_array_t *a, jl_value_t *ty, int8_t tparam, int8_t offs)
{
    uintptr_t uid = ((jl_datatype_t*)ty)->uid;
    jl_methlist_t *ml = (jl_methlist_t*)jl_cellref(a, uid & (a->nrows-1));
    if (ml && ml!=(void*)jl_nothing) {
        jl_value_t *t = jl_field_type(ml->sig, offs);
        if (tparam) t = jl_tparam0(t);
        if (t == ty)
            return ml;
    }
    return (jl_methlist_t*)jl_nothing;
}

static void mtcache_rehash(jl_array_t **pa, jl_value_t *parent, int8_t offs)
{
    size_t len = (*pa)->nrows;
    jl_value_t **d = (jl_value_t**)(*pa)->data;
    jl_array_t *n = jl_alloc_cell_1d(len*2);
    jl_value_t **nd = (jl_value_t**)n->data;
    size_t i;
    for(i=0; i < len; i++) {
        jl_methlist_t *ml = (jl_methlist_t*)d[i];
        if (ml && ml!=(jl_methlist_t*)jl_nothing) {
            jl_value_t *t = jl_field_type(ml->sig, offs);
            if (jl_is_type_type(t))
                t = jl_tparam0(t);
            uintptr_t uid = ((jl_datatype_t*)t)->uid;
            nd[uid & (len*2-1)] = (jl_value_t*)ml;
        }
    }
    jl_gc_wb(parent, n);
    *pa = n;
}

static jl_methlist_t **mtcache_hash_bp(jl_array_t **pa, jl_value_t *ty,
                                       int8_t tparam, int8_t offs, jl_value_t *parent)
{
    uintptr_t uid;
    if (jl_is_datatype(ty) && (uid = ((jl_datatype_t*)ty)->uid)) {
        while (1) {
            jl_methlist_t **pml = &((jl_methlist_t**)jl_array_data(*pa))[uid & ((*pa)->nrows-1)];
            if (*pml == NULL || *pml == (jl_methlist_t*)jl_nothing) {
                *pml = (jl_methlist_t*)jl_nothing;
                return pml;
            }
            jl_value_t *t = jl_field_type((*pml)->sig, offs);
            if (tparam) t = jl_tparam0(t);
            if (t == ty)
                return pml;
            mtcache_rehash(pa, parent, offs);
        }
    }
    return NULL;
}

/*
  Method caches are divided into three parts: one for signatures where
  the first argument is a singleton kind (Type{Foo}), one indexed by the
  UID of the first argument's type in normal cases, and a fallback
  table of everything else.

  Note that the "primary key" is the type of the first *argument*, since
  there tends to be lots of variation there. The type of the 0th argument
  (the function) is always the same for most functions.
*/
static jl_lambda_info_t *jl_method_table_assoc_exact_by_type(jl_methtable_t *mt, jl_tupletype_t *types)
{
    // called object is the primary key for constructors, otherwise first argument
    int8_t offs = (mt == jl_type_type->name->mt) ? 0 : 1;
    jl_methlist_t *ml = (jl_methlist_t*)jl_nothing;
    if (jl_datatype_nfields(types) > offs) {
        jl_value_t *ty = jl_tparam(types, offs);
        if (jl_is_type_type(ty)) {
            jl_value_t *a0 = jl_tparam0(ty);
            if (mt->cache_targ != (void*)jl_nothing && jl_is_datatype(a0)) {
                ml = mtcache_hash_lookup(mt->cache_targ, a0, 1, offs);
                if (ml!=(jl_methlist_t*)jl_nothing)
                    goto mt_assoc_bt_lkup;
            }
        }
        if (mt->cache_arg1 != (void*)jl_nothing && jl_is_datatype(ty)) {
            ml = mtcache_hash_lookup(mt->cache_arg1, ty, 0, offs);
        }
    }
    if (ml == (void*)jl_nothing)
        ml = mt->cache;
 mt_assoc_bt_lkup:
    while (ml != (void*)jl_nothing) {
        if (cache_match_by_type(jl_svec_data(types->parameters),
                                jl_datatype_nfields(types),
                                ml->sig, ml->va)) {
            return ml->func;
        }
        // see corresponding code in jl_method_table_assoc_exact
        if (ml->func == NULL && jl_subtype((jl_value_t*)types, (jl_value_t*)ml->sig, 0))
            return NULL;
        ml = ml->next;
    }
    return NULL;
}

static jl_lambda_info_t *jl_method_table_assoc_exact(jl_methtable_t *mt, jl_value_t **args, size_t n)
{
    // NOTE: This function is a huge performance hot spot!!
    int8_t offs = (mt == jl_type_type->name->mt) ? 0 : 1;
    jl_methlist_t *ml = (jl_methlist_t*)jl_nothing;
    if (n > offs) {
        jl_value_t *a1 = args[offs];
        jl_value_t *ty = (jl_value_t*)jl_typeof(a1);
        if (ty == (jl_value_t*)jl_datatype_type && mt->cache_targ != (void*)jl_nothing) {
            ml = mtcache_hash_lookup(mt->cache_targ, a1, 1, offs);
            if (ml != (void*)jl_nothing)
                goto mt_assoc_lkup;
        }
        assert(jl_is_datatype(ty));
        if (mt->cache_arg1 != (void*)jl_nothing) {
            ml = mtcache_hash_lookup(mt->cache_arg1, ty, 0, offs);
            if (ml != (void*)jl_nothing) {
                jl_value_t *a0 = args[1-offs];
                jl_value_t *t0 = (jl_value_t*)jl_typeof(a0);
                if (ml->next==(void*)jl_nothing && n==2 && jl_datatype_nfields(ml->sig)==2 &&
                    jl_tparam(ml->sig,1-offs)==t0)
                    return ml->func;
                if (n==3) {
                    // some manually-unrolled common special cases
                    jl_value_t *a2 = args[2];
                    if (!jl_is_tuple(a2)) {  // issue #6426
                        jl_methlist_t *mn = ml;
                        if (jl_datatype_nfields(mn->sig)==3 &&
                            jl_tparam(mn->sig,1-offs)==t0 &&
                            jl_tparam(mn->sig,2)==(jl_value_t*)jl_typeof(a2))
                            return mn->func;
                        mn = mn->next;
                        if (mn!=(void*)jl_nothing && jl_datatype_nfields(mn->sig)==3 &&
                            jl_tparam(mn->sig,1-offs)==t0 &&
                            jl_tparam(mn->sig,2)==(jl_value_t*)jl_typeof(a2))
                            return mn->func;
                    }
                }
            }
        }
    }
    if (ml == (void*)jl_nothing)
        ml = mt->cache;
 mt_assoc_lkup:
    while (ml != (void*)jl_nothing) {
        size_t lensig = jl_datatype_nfields(ml->sig);
        if (lensig == n || (ml->va && lensig <= n+1)) {
            if (cache_match(args, n, jl_svec_data(ml->sig->parameters), ml->va, lensig)) {
                return ml->func;
            }
            // if we hit a guard entry (ml->func == NULL), do a more
            // expensive subtype check, since guard entries added for ANY might be
            // abstract. this fixed issue #12967.
            if (ml->func == NULL && jl_tuple_subtype(args, n, ml->sig, 1))
                return NULL;
        }
        ml = ml->next;
    }
    return NULL;
}

// return a new lambda-info that has some extra static parameters merged in.
static jl_lambda_info_t *jl_add_static_parameters(jl_lambda_info_t *l, jl_svec_t *sp, jl_tupletype_t *types)
{
    JL_GC_PUSH1(&sp);
    assert(jl_svec_len(l->sparam_syms) == jl_svec_len(sp) || sp == jl_emptysvec);
    assert(l->sparam_vals == jl_emptysvec);
    assert(l->specTypes == NULL);
    jl_lambda_info_t *nli = jl_copy_lambda_info(l);
    nli->sparam_vals = sp; // no gc_wb needed
    nli->tfunc = jl_nothing;
    nli->specializations = NULL;
    nli->specTypes = types;
    if (types) jl_gc_wb(nli, types);

    /* "method" itself should never get compiled,
      for example, if an unspecialized method is needed,
      the slow compiled code should be associated with
      method->unspecialized, not method */
    assert(!nli->code ||
           (nli->fptr == NULL &&
            nli->jlcall_api == 0 &&
            nli->functionObjects.functionObject == NULL &&
            nli->functionObjects.specFunctionObject == NULL &&
            nli->functionObjects.cFunctionList == NULL &&
            nli->functionID == 0 &&
            nli->specFunctionID == 0));
    if (jl_options.compile_enabled == JL_OPTIONS_COMPILE_OFF || jl_options.compile_enabled == JL_OPTIONS_COMPILE_MIN) {
        // copy fptr from the unspecialized method definition
        jl_lambda_info_t *unspec = l->unspecialized;
        if (unspec != NULL) {
            nli->fptr = unspec->fptr;
            nli->jlcall_api = unspec->jlcall_api;
            nli->functionObjects.functionObject = unspec->functionObjects.functionObject;
            nli->functionObjects.specFunctionObject = unspec->functionObjects.specFunctionObject;
            nli->functionID = unspec->functionID;
            nli->specFunctionID = unspec->functionID;
        }
        if (jl_options.compile_enabled == JL_OPTIONS_COMPILE_OFF && nli->fptr == NULL) {
            jl_printf(JL_STDERR,"code missing for ");
            jl_static_show(JL_STDERR, (jl_value_t*)nli);
            jl_printf(JL_STDERR, "  sysimg may not have been built with --compile=all\n");
        }
    }
    JL_GC_POP();
    return nli;
}

jl_lambda_info_t *jl_get_unspecialized(jl_lambda_info_t *method)
{
    // one unspecialized version of a function can be shared among all cached specializations
    jl_lambda_info_t *def = method->def;
    if (method->unspecialized != NULL)
        return method->unspecialized;
    if (method->specTypes && def->sparam_syms != jl_emptysvec) {
        if (def->needs_sparam_vals_ducttape == 2) {
            jl_array_t *code = method->code;
            JL_GC_PUSH1(&code);
            if (!jl_typeis(code, jl_array_any_type))
                code = jl_uncompress_ast(def, code);
            size_t i, l = jl_array_len(code);
            def->needs_sparam_vals_ducttape = 0;
            for(i=0; i < l; i++) {
                if (jl_has_intrinsics(method, jl_cellref(code,i), method->module)) {
                    def->needs_sparam_vals_ducttape = 1;
                    break;
                }
            }
            JL_GC_POP();
        }
        if (method->needs_sparam_vals_ducttape) {
            // a method is a specialization iff it has specTypes
            // but method->unspecialized points to the definition in that case
            // and definition->unspecialized points to the thing to call
            method->unspecialized = jl_add_static_parameters(def, method->sparam_vals, method->specTypes);
            jl_gc_wb(method, method->unspecialized);
            method->unspecialized->unspecialized = method->unspecialized;
            return method->unspecialized;
        }
    }
    if (def->unspecialized == NULL) {
        def->unspecialized = jl_add_static_parameters(def, jl_emptysvec, jl_anytuple_type);
        jl_gc_wb(def, def->unspecialized);
    }
    return def->unspecialized;
}

static jl_methlist_t *jl_method_list_insert(jl_methlist_t **pml, jl_tupletype_t *type,
                                            jl_lambda_info_t *method, jl_svec_t *tvars,
                                            int check_amb, int8_t isstaged, jl_value_t *parent);

jl_lambda_info_t *jl_method_cache_insert(jl_methtable_t *mt, jl_tupletype_t *type,
                                         jl_lambda_info_t *method)
{
    int8_t offs = (mt == jl_type_type->name->mt) ? 0 : 1;
    jl_methlist_t **pml = &mt->cache;
    jl_value_t *cache_array = NULL;
    if (jl_datatype_nfields(type) > offs) {
        jl_value_t *t1 = jl_tparam(type, offs);
        uintptr_t uid=0;
        // if t1 != jl_typetype_type and the argument is Type{...}, this
        // method has specializations for singleton kinds and we use
        // the table indexed for that purpose.
        if (t1 != (jl_value_t*)jl_typetype_type && jl_is_type_type(t1)) {
            jl_value_t *a0 = jl_tparam0(t1);
            if (jl_is_datatype(a0))
                uid = ((jl_datatype_t*)a0)->uid;
            if (uid > 0) {
                if (mt->cache_targ == (void*)jl_nothing) {
                    mt->cache_targ = jl_alloc_cell_1d(16);
                    jl_gc_wb(mt, mt->cache_targ);
                }
                pml = mtcache_hash_bp(&mt->cache_targ, a0, 1, offs, (jl_value_t*)mt);
                cache_array = (jl_value_t*)mt->cache_targ;
                goto ml_do_insert;
            }
        }
        if (jl_is_datatype(t1))
            uid = ((jl_datatype_t*)t1)->uid;
        if (uid > 0) {
            if (mt->cache_arg1 == (void*)jl_nothing) {
                mt->cache_arg1 = jl_alloc_cell_1d(16);
                jl_gc_wb(mt, mt->cache_arg1);
            }
            pml = mtcache_hash_bp(&mt->cache_arg1, t1, 0, offs, (jl_value_t*)mt);
            cache_array = (jl_value_t*)mt->cache_arg1;
        }
    }
 ml_do_insert:
    return jl_method_list_insert(pml, type, method, jl_emptysvec, 0, 0, cache_array ? cache_array : (jl_value_t*)mt)->func;
}

/*
  run type inference on lambda "li" in-place, for given argument types.
  "def" is the original method definition of which this is an instance;
  can be equal to "li->def" if not applicable.
*/
void jl_type_infer(jl_lambda_info_t *li, jl_value_t *toplevel)
{
#ifdef ENABLE_INFERENCE
    if (jl_typeinf_func != NULL && li->module != jl_gf_mtable(jl_typeinf_func)->module) {
        JL_LOCK(codegen); // Might GC
        assert(li->inInference == 0);
        li->inInference = 1;
        jl_value_t *fargs[3];
        fargs[0] = (jl_value_t*)jl_typeinf_func;
        fargs[1] = (jl_value_t*)li;
        fargs[2] = (jl_value_t*)toplevel;
#ifdef TRACE_INFERENCE
        jl_printf(JL_STDERR,"inference on ");
        jl_static_show_func_sig(JL_STDERR, (jl_value_t*)li->specTypes);
        jl_printf(JL_STDERR, "\n");
#endif
        jl_value_t *info = jl_apply(fargs, 3); (void)info;
        assert(toplevel == jl_false || li->inInference == 0);
        JL_UNLOCK(codegen);
    }
#endif
}

jl_value_t *jl_nth_slot_type(jl_tupletype_t *sig, size_t i)
{
    size_t len = jl_datatype_nfields(sig);
    if (len == 0)
        return NULL;
    if (i < len-1)
        return jl_tparam(sig, i);
    if (jl_is_vararg_type(jl_tparam(sig,len-1)))
        return jl_tparam0(jl_tparam(sig,len-1));
    if (i == len-1)
        return jl_tparam(sig, i);
    return NULL;
}

static int very_general_type(jl_value_t *t)
{
    return (t && (t==(jl_value_t*)jl_any_type || t == (jl_value_t*)jl_type_type ||
                  (jl_is_typevar(t) &&
                   ((jl_tvar_t*)t)->ub==(jl_value_t*)jl_any_type)));
}

static int is_kind(jl_value_t *v)
{
    return (v==(jl_value_t*)jl_uniontype_type ||
            v==(jl_value_t*)jl_datatype_type ||
            v==(jl_value_t*)jl_typector_type);
}

static jl_value_t *ml_matches(jl_methlist_t *ml, jl_value_t *type,
                              jl_sym_t *name, int lim);

static jl_lambda_info_t *cache_method(jl_methtable_t *mt, jl_tupletype_t *type,
                                      jl_lambda_info_t *method, jl_methlist_t *m,
                                      jl_svec_t *sparams)
{
    JL_LOCK(codegen); // Might GC
    size_t i;
    jl_tupletype_t *decl = m->sig;
    int8_t isstaged = m->isstaged;
    int need_guard_entries = 0;
    int cache_as_orig = 0;
    jl_value_t *temp=NULL;
    jl_value_t *temp2=NULL;
    jl_lambda_info_t *newmeth=NULL;
    jl_svec_t *newparams=NULL;
    jl_svec_t *limited=NULL;
    jl_tupletype_t *origtype = type;  // TODO: root?
    JL_GC_PUSH5(&temp, &temp2, &newmeth, &newparams, &limited);
    size_t np = jl_nparams(type);
    newparams = jl_svec_copy(type->parameters);

    for (i=0; i < np; i++) {
        jl_value_t *elt = jl_tparam(type,i);
        jl_value_t *decl_i = jl_nth_slot_type(decl,i);
        if (!isstaged && jl_is_type_type(elt) && jl_is_tuple_type(jl_tparam0(elt)) &&
            !(jl_subtype(decl_i, (jl_value_t*)jl_type_type, 0) && !is_kind(decl_i))) {
            jl_methlist_t *curr = mt->defs;
            int ok=1;
            while (curr != (void*)jl_nothing) {
                jl_value_t *slottype = jl_nth_slot_type(curr->sig, i);
                if (slottype && curr->func!=method) {
                    if (jl_is_type_type(slottype) &&
                        jl_type_intersection(slottype, decl_i) != jl_bottom_type) {
                        ok=0;
                        break;
                    }
                }
                curr = curr->next;
            }
            if (ok) {
                elt = jl_typeof(jl_tparam0(elt));
                jl_svecset(newparams, i, elt);
            }
        }

        int set_to_any = 0;
        int notcalled_func = (i>0 && i<=8 && !(method->called&(1<<(i-1))) &&
                              jl_subtype(elt,(jl_value_t*)jl_function_type,0));
        if (decl_i == jl_ANY_flag ||
            (notcalled_func && (decl_i == (jl_value_t*)jl_any_type ||
                                decl_i == (jl_value_t*)jl_function_type ||
                                (!jl_is_typevar(decl_i) &&
                                 jl_subtype((jl_value_t*)jl_function_type, decl_i, 0) &&
                                 jl_subtype((jl_value_t*)jl_datatype_type, decl_i, 0) &&
                                 jl_is_uniontype(decl_i) && jl_svec_len(((jl_uniontype_t*)decl_i)->types)==2)))) {
            // don't specialize on slots marked ANY
            jl_svecset(newparams, i, (jl_value_t*)jl_any_type);
            temp2 = (jl_value_t*)jl_svec_copy(newparams);
            temp2 = (jl_value_t*)jl_apply_tuple_type((jl_svec_t*)temp2);
            int nintr=0;
            jl_methlist_t *curr = mt->defs;
            int specific_decl =
                decl_i != (jl_value_t*)jl_any_type && decl_i != (jl_value_t*)jl_ANY_flag;
            // if this method is the only match even with the current slot
            // set to Any, then it is safe to cache it that way.
            while (curr != (void*)jl_nothing && (curr->func != method || specific_decl)) {
                if (jl_type_intersection((jl_value_t*)curr->sig, (jl_value_t*)temp2) !=
                    (jl_value_t*)jl_bottom_type) {
                    nintr++;
                    if (specific_decl || notcalled_func) {
                        // ignore MAX_UNSPECIALIZED_CONFLICTS in cache_as_orig mode
                        // fixes issue #15190
                        break;
                    }
                    if (nintr > MAX_UNSPECIALIZED_CONFLICTS) break;
                }
                curr = curr->next;
            }
            if (nintr > MAX_UNSPECIALIZED_CONFLICTS) {
                // TODO: even if different specializations of this slot need
                // separate cache entries, have them share code.
                jl_svecset(newparams, i, jl_tparam(type, i));
            }
            else {
                set_to_any = 1;
                if (nintr > 0) {
                    if (specific_decl || notcalled_func)
                        cache_as_orig = 1;
                    else
                        need_guard_entries = 1;
                }
            }
        }
        if (set_to_any || isstaged) {
        }
        else if (jl_is_type_type(elt) && jl_is_typector(jl_tparam0(elt)) &&
                 decl_i == (jl_value_t*)jl_typector_type) {
            // TypeConstructors are problematic because they can be alternate
            // representations of any type. If we matched this method because
            // it matched the leaf type TypeConstructor, then don't
            // cache something different since that doesn't necessarily actually apply
            jl_svecset(newparams, i, jl_typector_type);
        }
        else if (jl_is_type_type(elt) && decl_i == (jl_value_t*)jl_datatype_type) {
            // similarly, if we matched Type{T<:Any}::DataType,
            // then we don't want to cache it that way
            // since lookup will think we matched ::Type{T}
            // and that is quite a different thing
            jl_svecset(newparams, i, jl_datatype_type);
            need_guard_entries = 1; // DataType has a UID so its precedence in the cache may be too high
        }
        else if (jl_is_type_type(elt) && jl_is_type_type(jl_tparam0(elt)) &&
                 // give up on specializing static parameters for Type{Type{Type{...}}}
                 (jl_is_type_type(jl_tparam0(jl_tparam0(elt))) ||
                  decl_i==NULL || !jl_has_typevars(decl_i))) {
            /*
              actual argument was Type{...}, we computed its type as
              Type{Type{...}}. we must avoid unbounded nesting here, so
              cache the signature as Type{T}, unless something more
              specific like Type{Type{Int32}} was actually declared.
              this can be determined using a type intersection.
            */
            if (i < jl_nparams(decl)) {
                jl_value_t *declt = jl_tparam(decl,i);
                // for T..., intersect with T
                if (jl_is_vararg_type(declt))
                    declt = jl_tparam0(declt);
                jl_value_t *di = jl_type_intersection(declt, (jl_value_t*)jl_typetype_type);
                if (is_kind(di))
                    // issue #11355: DataType has a UID and so takes precedence in the cache
                    jl_svecset(newparams, i, (jl_value_t*)jl_typetype_type);
                else
                    jl_svecset(newparams, i, di);
                // TODO: recompute static parameter values, so in extreme cases we
                // can give `T=Type` instead of `T=Type{Type{Type{...`.
            }
            else {
                jl_svecset(newparams, i, (jl_value_t*)jl_typetype_type);
            }
            need_guard_entries = 1;
            assert(jl_svecref(newparams,i) != (jl_value_t*)jl_bottom_type);
        }
        else if (jl_is_type_type(elt) && very_general_type(decl_i) &&
                 !jl_has_typevars(decl_i)) {
            /*
              here's a fairly complex heuristic: if this argument slot's
              declared type is Any, and no definition overlaps with Type
              for this slot, then don't specialize for every Type that
              might be passed.
              Since every type x has its own type Type{x}, this would be
              excessive specialization for an Any slot.

              TypeConstructors are problematic because they can be alternate
              representations of any type. Extensionally, TC == TC.body, but
              typeof(TC) != typeof(TC.body). This creates an ambiguity:
              Type{TC} is type-equal to Type{TC.body}, yet a slot
              x::TypeConstructor matches the first but not the second, while
              also matching all other TypeConstructors. This means neither
              Type{TC} nor TypeConstructor is more specific.

              To solve this, we identify "kind slots", which are slots
              for which some definition specifies a kind (e.g. DataType).
              Those tend to be in reflective functions that look at types
              themselves. For these slots we specialize on jl_typeof(T) instead
              of Type{T}, i.e. the kind of the type rather than the specific
              type.
            */
            int ok=1, kindslot=0;
            jl_methlist_t *curr = mt->defs;
            jl_value_t *kind = (jl_value_t*)jl_typeof(jl_tparam0(elt));
            while (curr != (void*)jl_nothing) {
                jl_value_t *slottype = jl_nth_slot_type(curr->sig, i);
                if (slottype && curr->func!=method) {
                    if (slottype == kind) {
                        ok=0;
                        break;
                    }
                    if (is_kind(slottype))
                        kindslot=1;
                }
                curr = curr->next;
            }
            if (ok) {
                if (kindslot) {
                    jl_svecset(newparams, i, kind);
                }
                else {
                    curr = mt->defs;
                    while (curr != (void*)jl_nothing) {
                        jl_value_t *slottype = jl_nth_slot_type(curr->sig, i);
                        if (slottype && curr->func!=method) {
                            if (!very_general_type(slottype) &&
                                jl_type_intersection(slottype, (jl_value_t*)jl_type_type) !=
                                (jl_value_t*)jl_bottom_type) {
                                ok=0;
                                break;
                            }
                        }
                        curr = curr->next;
                    }
                    if (ok) {
                        jl_svecset(newparams, i, jl_typetype_type);
                        need_guard_entries = 1;
                    }
                }
            }
        }
        else if (is_kind(decl_i)) {
            // if a slot is specialized for a particular kind, it can be
            // considered a reflective method and so only needs to be
            // specialized for type representation, not type extent.
            jl_methlist_t *curr = mt->defs;
            int ok=1;
            while (curr != (void*)jl_nothing) {
                jl_value_t *slottype = jl_nth_slot_type(curr->sig, i);
                if (slottype && curr->func!=method) {
                    if (jl_is_type_type(slottype) &&
                        jl_type_intersection(slottype, decl_i) != jl_bottom_type) {
                        ok=0;
                        break;
                    }
                }
                curr = curr->next;
            }
            if (ok)
                jl_svecset(newparams, i, decl_i);
        }
    }

    // for varargs methods, only specialize up to max_args.
    // in general, here we want to find the biggest type that's not a
    // supertype of any other method signatures. so far we are conservative
    // and the types we find should be bigger.
    if (!isstaged && jl_nparams(type) > mt->max_args && jl_is_va_tuple(decl)) {
        size_t nspec = mt->max_args + 2;
        limited = jl_alloc_svec(nspec);
        for(i=0; i < nspec-1; i++) {
            jl_svecset(limited, i, jl_svecref(newparams, i));
        }
        jl_value_t *lasttype = jl_svecref(newparams,i-1);
        // if all subsequent arguments are subtypes of lasttype, specialize
        // on that instead of decl. for example, if decl is
        // (Any...)
        // and type is
        // (Symbol, Symbol, Symbol)
        // then specialize as (Symbol...), but if type is
        // (Symbol, Int32, Expr)
        // then specialize as (Any...)
        size_t j = i;
        int all_are_subtypes=1;
        for(; j < jl_svec_len(newparams); j++) {
            if (!jl_subtype(jl_svecref(newparams,j), lasttype, 0)) {
                all_are_subtypes = 0;
                break;
            }
        }
        if (all_are_subtypes) {
            // avoid Type{Type{...}...}...
            if (jl_is_type_type(lasttype) && jl_is_type_type(jl_tparam0(lasttype)))
                lasttype = (jl_value_t*)jl_type_type;
            jl_svecset(limited, i, jl_wrap_vararg(lasttype));
        }
        else {
            jl_value_t *lastdeclt = jl_tparam(decl,jl_nparams(decl)-1);
            int nsp = jl_svec_len(sparams);
            if (nsp > 0) {
                temp2 = (jl_value_t*)jl_alloc_svec_uninit(2*nsp);
                for(j=0; j < nsp; j++) {
                    if (j==0 && jl_is_typevar(m->tvars))
                        jl_svecset(temp2, 0, m->tvars);
                    else
                        jl_svecset(temp2, j*2, jl_svecref(m->tvars, j));
                    jl_svecset(temp2, j*2+1, jl_svecref(sparams,j));
                }
                lastdeclt = (jl_value_t*)jl_instantiate_type_with((jl_value_t*)lastdeclt,
                                                                  jl_svec_data(temp2), nsp);
            }
            jl_svecset(limited, i, lastdeclt);
        }
        type = jl_apply_tuple_type(limited);
        temp2 = (jl_value_t*)type;
        // now there is a problem: the widened signature is more
        // general than just the given arguments, so it might conflict
        // with another definition that doesn't have cache instances yet.
        // to fix this, we insert guard cache entries for all intersections
        // of this signature and definitions. those guard entries will
        // supersede this one in conflicted cases, alerting us that there
        // should actually be a cache miss.
        need_guard_entries = 1;
    }
    else {
        type = jl_apply_tuple_type(newparams);
        temp2 = (jl_value_t*)type;
    }

    if (need_guard_entries) {
        temp = ml_matches(mt->defs, (jl_value_t*)type, lambda_sym, -1);
        int unmatched_tvars = 0;
        for(i=0; i < jl_array_len(temp); i++) {
            jl_value_t *m = jl_cellref(temp, i);
            jl_value_t *env = jl_svecref(m,1);
            for(int k=0; k < jl_svec_len(env); k++) {
                if (jl_is_typevar(jl_svecref(env,k))) {
                    unmatched_tvars = 1; break;
                }
            }
            if (unmatched_tvars) {
                // if distinguishing a guard entry from the generalized signature
                // would require matching type vars then bail out, since the
                // method cache matching algorithm cannot do that.
                type = origtype; break;
            }
        }
        if (!unmatched_tvars) {
            for(i=0; i < jl_array_len(temp); i++) {
                jl_value_t *m = jl_cellref(temp, i);
                if (((jl_methlist_t*)jl_svecref(m,2))->func != method) {
                    jl_method_cache_insert(mt, (jl_tupletype_t*)jl_svecref(m, 0), NULL);
                }
            }
        }
    }

    // here we infer types and specialize the method
    jl_array_t *lilist=NULL;
    jl_lambda_info_t *li=NULL;
    if (method->specializations != NULL) {
        // reuse code already generated for this combination of lambda and
        // arguments types. this happens for inner generic functions where
        // a new closure is generated on each call to the enclosing function.
        lilist = method->specializations;
        int k;
        for(k=0; k < lilist->nrows; k++) {
            li = (jl_lambda_info_t*)jl_cellref(lilist, k);
            if (jl_types_equal((jl_value_t*)li->specTypes, (jl_value_t*)type))
                break;
        }
        if (k == lilist->nrows) lilist=NULL;
    }
    if (lilist != NULL && !li->inInference) {
        assert(li);
        newmeth = li;
        if (cache_as_orig)
            (void)jl_method_cache_insert(mt, origtype, newmeth);
        else
            (void)jl_method_cache_insert(mt, type, newmeth);
        JL_GC_POP();
        JL_UNLOCK(codegen);
        return newmeth;
    }

    newmeth = jl_add_static_parameters(method, sparams, type);

    if (cache_as_orig)
        (void)jl_method_cache_insert(mt, origtype, newmeth);
    else
        (void)jl_method_cache_insert(mt, type, newmeth);

    if (newmeth->code != NULL) {
        jl_array_t *spe = method->specializations;
        if (spe == NULL) {
            spe = jl_alloc_cell_1d(1);
            jl_cellset(spe, 0, newmeth);
        }
        else {
            jl_cell_1d_push(spe, (jl_value_t*)newmeth);
        }
        method->specializations = spe;
        jl_gc_wb(method, method->specializations);
        if (jl_options.compile_enabled != JL_OPTIONS_COMPILE_OFF) // don't bother with typeinf if compile is off
            if (jl_symbol_name(newmeth->name)[0] != '@')  // don't bother with typeinf on macros
                jl_type_infer(newmeth, jl_false);
    }
    JL_GC_POP();
    JL_UNLOCK(codegen);
    return newmeth;
}

static jl_value_t *lookup_match(jl_value_t *a, jl_value_t *b, jl_svec_t **penv,
                                jl_svec_t *tvars)
{
    jl_value_t *ti = jl_type_intersection_matching(a, b, penv, tvars);
    if (ti == (jl_value_t*)jl_bottom_type)
        return ti;
    JL_GC_PUSH1(&ti);
    assert(jl_is_svec(*penv));
    int l = jl_svec_len(*penv);
    for(int i=0; i < l; i++) {
        jl_value_t *val = jl_svecref(*penv,i);
        /*
          since "a" is a concrete type, we assume that
          (a∩b != Union{}) => a<:b. However if a static parameter is
          forced to equal Union{}, then part of "b" might become Union{},
          and therefore a subtype of "a". For example
          (Type{Union{}},Int) ∩ (Type{T},T)
          issue #5254
        */
        if (val == (jl_value_t*)jl_bottom_type) {
            if (!jl_subtype(a, ti, 0)) {
                JL_GC_POP();
                return (jl_value_t*)jl_bottom_type;
            }
        }
    }
    JL_GC_POP();
    return ti;
}

// invoke (compiling if necessary) the jlcall function pointer for an unspecialized method
static jl_value_t *jl_call_unspecialized(jl_svec_t *sparam_vals, jl_lambda_info_t *meth,
                                         jl_value_t **args, uint32_t nargs)
{
    if (__unlikely(meth->fptr == NULL)) {
        jl_compile_linfo(meth);
        jl_generate_fptr(meth);
    }
    assert(jl_svec_len(meth->sparam_syms) == jl_svec_len(sparam_vals));
    if (__likely(meth->jlcall_api == 0))
        return meth->fptr(args[0], &args[1], nargs-1);
    else
        return ((jl_fptr_sparam_t)meth->fptr)(sparam_vals, args[0], &args[1], nargs-1);
}

JL_DLLEXPORT jl_lambda_info_t *jl_instantiate_staged(jl_lambda_info_t *generator, jl_tupletype_t *tt, jl_svec_t *env)
{
    jl_expr_t *ex = NULL;
    jl_value_t *linenum = NULL;
    jl_svec_t *sparam_vals = env;
    JL_GC_PUSH3(&ex, &linenum, &sparam_vals);

    assert(generator->sparam_vals == jl_emptysvec);
    assert(jl_svec_len(generator->sparam_syms) == jl_svec_len(sparam_vals));
    assert(generator->unspecialized == NULL && generator->specTypes == jl_anytuple_type);
    //if (!generated->inferred)
    //    jl_type_infer(generator, jl_false);  // this doesn't help all that much

    ex = jl_exprn(lambda_sym, 2);

    int nargs = generator->nargs;
    jl_array_t *argnames = jl_alloc_cell_1d(nargs);
    jl_cellset(ex->args, 0, argnames);
    size_t i;
    for(i=0; i < nargs; i++)
        jl_cellset(argnames, i, jl_cellref(generator->slotnames,i));

    jl_expr_t *scopeblock = jl_exprn(jl_symbol("scope-block"), 1);
    jl_cellset(ex->args, 1, scopeblock);

    jl_expr_t *body = jl_exprn(jl_symbol("block"), 2);
    jl_cellset(((jl_expr_t*)jl_exprarg(ex,1))->args, 0, body);

    linenum = jl_box_long(generator->line);
    jl_value_t *linenode = jl_new_struct(jl_linenumbernode_type, generator->file, linenum);
    jl_cellset(body->args, 0, linenode);

    // invoke code generator
    assert(jl_nparams(tt) == jl_array_len(argnames) ||
           (generator->isva && (jl_nparams(tt) >= jl_array_len(argnames) - 1)));
    jl_cellset(body->args, 1, jl_call_unspecialized(sparam_vals, generator, jl_svec_data(tt->parameters), jl_nparams(tt)));

    if (generator->sparam_syms != jl_emptysvec) {
        // mark this function as having the same static parameters as the generator
        size_t i, nsp = jl_svec_len(generator->sparam_syms);
        jl_expr_t *newast = jl_exprn(jl_symbol("with-static-parameters"), nsp + 1);
        jl_exprarg(newast, 0) = (jl_value_t*)ex;
        // (with-static-parameters func_expr sp_1 sp_2 ...)
        for (i = 0; i < nsp; i++)
            jl_exprarg(newast, i+1) = jl_svecref(generator->sparam_syms, i);
        ex = newast;
    }
    // need to eval macros in the right module, but not give a warning for the `eval` call unless that results in a call to `eval`
    jl_lambda_info_t *func = (jl_lambda_info_t*)jl_toplevel_eval_in_warn(generator->module, (jl_value_t*)ex, 1);
    func->name = generator->name;
    if (generator->isva)
        func->isva = 1;
    JL_GC_POP();
    return func;
}

static jl_lambda_info_t *jl_mt_assoc_by_type(jl_methtable_t *mt, jl_datatype_t *tt, int cache, int inexact)
{
    jl_methlist_t *m = mt->defs;
    size_t nargs = jl_nparams(tt);
    size_t i;
    jl_value_t *ti=(jl_value_t*)jl_bottom_type;
    jl_tupletype_t *newsig=NULL;
    jl_svec_t *env = jl_emptysvec;
    jl_lambda_info_t *func = NULL;
    JL_GC_PUSH3(&env, &newsig, &func);

    while (m != (void*)jl_nothing) {
        if (m->tvars!=jl_emptysvec) {
            ti = lookup_match((jl_value_t*)tt, (jl_value_t*)m->sig, &env, m->tvars);
            if (ti != (jl_value_t*)jl_bottom_type) {
                // parametric methods only match if all typevars are matched by
                // non-typevars.
                for(i=0; i < jl_svec_len(env); i++) {
                    if (jl_is_typevar(jl_svecref(env,i))) {
                        if (inexact) {
                            // "inexact" means the given type is compile-time,
                            // where a failure to determine the value of a
                            // static parameter is inconclusive.
                            // this is issue #3182, see test/core.jl
                            JL_GC_POP();
                            return NULL;
                        }
                        break;
                    }
                }
                if (i >= jl_svec_len(env))
                    break;
                ti = (jl_value_t*)jl_bottom_type;
            }
        }
        else if (jl_tuple_subtype(jl_svec_data(tt->parameters), nargs, m->sig, 0)) {
            break;
        }
        m = m->next;
    }

    if (ti == (jl_value_t*)jl_bottom_type) {
        if (m != (void*)jl_nothing) {
            func = m->func;
            if (m->isstaged)
                func = jl_instantiate_staged(func, tt, env);
            if (!cache) {
                JL_GC_POP();
                return func;
            }
            // make sure the argument is rooted in `cache_method`
            // in case another thread changed it.
            newsig = m->sig;
            jl_lambda_info_t *res = cache_method(mt, tt, func, m, jl_emptysvec);
            JL_GC_POP();
            return res;
        }
        JL_GC_POP();
        return NULL;
    }

    assert(jl_is_svec(env));
    func = m->func;

    if (inexact && !jl_types_equal(ti, (jl_value_t*)tt)) {
        // the compiler might attempt jl_get_specialization on e.g.
        // convert(::Type{Type{Int}}, ::DataType), which is concrete but might not
        // equal the run time type. in this case ti would be {Type{Type{Int}}, Type{Int}}
        // but tt would be {Type{Type{Int}}, DataType}.
        JL_GC_POP();
        return NULL;
    }

    if (m->isstaged)
        func = jl_instantiate_staged(func, tt, env);

    newsig = m->sig;
    assert(jl_is_tuple_type(newsig));
    jl_lambda_info_t *nf;
    if (!cache)
        nf = func;
    else
        nf = cache_method(mt, tt, func, m, env);
    JL_GC_POP();
    return nf;
}

jl_datatype_t *jl_wrap_Type(jl_value_t *t);

static int sigs_eq(jl_value_t *a, jl_value_t *b, int useenv)
{
    if (jl_has_typevars(a) || jl_has_typevars(b)) {
        return jl_types_equal_generic(a,b,useenv);
    }
    return jl_subtype(a, b, 0) && jl_subtype(b, a, 0);
}

JL_DLLEXPORT int jl_args_morespecific(jl_value_t *a, jl_value_t *b)
{
    int msp = jl_type_morespecific(a,b);
    int btv = jl_has_typevars(b);
    if (btv) {
        if (jl_type_match_morespecific(a,b) == (jl_value_t*)jl_false) {
            if (jl_has_typevars(a))
                return 0;
            return msp;
        }
        if (jl_has_typevars(a)) {
            type_match_invariance_mask = 0;
            //int result = jl_type_match_morespecific(b,a) == (jl_value_t*)jl_false);
            // this rule seems to work better:
            int result = jl_type_match(b,a) == (jl_value_t*)jl_false;
            type_match_invariance_mask = 1;
            if (result)
                return 1;
        }
        int nmsp = jl_type_morespecific(b,a);
        if (nmsp == msp)
            return 0;
    }
    if (jl_has_typevars((jl_value_t*)a)) {
        int nmsp = jl_type_morespecific(b,a);
        if (nmsp && msp)
            return 1;
        if (!btv && jl_types_equal(a,b))
            return 1;
        if (jl_type_match_morespecific(b,a) != (jl_value_t*)jl_false)
            return 0;
    }
    return msp;
}

void print_func_loc(JL_STREAM *s, jl_lambda_info_t *li);

/*
  warn about ambiguous method priorities

  the relative priority of A and B is ambiguous if
  !subtype(A,B) && !subtype(B,A) && no corresponding tuple
  elements are disjoint.

  for example, (AbstractArray, AbstractMatrix) and (AbstractMatrix, AbstractArray) are ambiguous.
  however, (AbstractArray, AbstractMatrix, Foo) and (AbstractMatrix, AbstractArray, Bar) are fine
  since Foo and Bar are disjoint, so there would be no confusion over
  which one to call.

  There is also this kind of ambiguity: foo{T,S}(T, S) vs. foo(Any,Any)
  In this case jl_types_equal() is true, but one is jl_type_morespecific
  or jl_type_match_morespecific than the other.
  To check this, jl_types_equal_generic needs to be more sophisticated
  so (T,T) is not equivalent to (Any,Any). (TODO)
*/
static void check_ambiguous(jl_methlist_t *ml, jl_tupletype_t *type,
                            jl_methlist_t *oldmeth, jl_sym_t *fname,
                            jl_lambda_info_t *linfo)
{
    jl_tupletype_t *sig = oldmeth->sig;
    size_t tl = jl_nparams(type);
    size_t sl = jl_nparams(sig);
    // we know !jl_args_morespecific(type, sig)
    if ((tl==sl ||
         (tl==sl+1 && jl_is_va_tuple(type)) ||
         (tl+1==sl && jl_is_va_tuple(sig))) &&
        !jl_args_morespecific((jl_value_t*)sig, (jl_value_t*)type)) {
        jl_value_t *isect = jl_type_intersection((jl_value_t*)type,
                                                 (jl_value_t*)sig);
        JL_GC_PUSH1(&isect);
        if (isect == (jl_value_t*)jl_bottom_type ||
            // we're ok if the new definition is actually the one we just
            // inferred to be required (see issue #3609). ideally this would
            // never happen, since if New ⊓ Old == New then we should have
            // considered New more specific, but jl_args_morespecific is not
            // perfect, so this is a useful fallback.
            sigs_eq(isect, (jl_value_t*)type, 1)) {
            JL_GC_POP();
            return;
        }
        jl_methlist_t *l = ml;
        JL_STREAM *s;
        while (l != (void*)jl_nothing) {
            if (sigs_eq(isect, (jl_value_t*)l->sig, 0))
                goto done_chk_amb;  // ok, intersection is covered
            l = l->next;
        }
        s = JL_STDERR;
        jl_printf(s, "WARNING: New definition \n    ");
        jl_static_show_func_sig(s, (jl_value_t*)type);
        print_func_loc(s, linfo);
        jl_printf(s, "\nis ambiguous with: \n    ");
        jl_static_show_func_sig(s, (jl_value_t*)sig);
        print_func_loc(s, oldmeth->func);
        jl_printf(s, ".\nTo fix, define \n    ");
        jl_static_show_func_sig(s, isect);
        jl_printf(s, "\nbefore the new definition.\n");
    done_chk_amb:
        JL_GC_POP();
    }
}

static int has_unions(jl_tupletype_t *type)
{
    int i;
    for(i=0; i < jl_nparams(type); i++) {
        jl_value_t *t = jl_tparam(type,i);
        if (jl_is_uniontype(t) ||
            (jl_is_vararg_type(t) && jl_is_uniontype(jl_tparam0(t))))
            return 1;
    }
    return 0;
}

static
jl_methlist_t *jl_method_list_insert(jl_methlist_t **pml, jl_tupletype_t *type,
                                     jl_lambda_info_t *method, jl_svec_t *tvars,
                                     int check_amb, int8_t isstaged, jl_value_t *parent)
{
    jl_methlist_t *l, **pl;

    assert(jl_is_tuple_type(type));
    l = *pml;
    while (l != (void*)jl_nothing) {
        if (((l->tvars==jl_emptysvec) == (tvars==jl_emptysvec)) &&
            sigs_eq((jl_value_t*)type, (jl_value_t*)l->sig, 1)) {
            if (method == NULL)  // don't overwrite with guard entries
                return l;
            // method overwritten
            if (check_amb && l->func && method) {
                jl_module_t *newmod = method->module;
                jl_module_t *oldmod = l->func->module;
                JL_STREAM *s = JL_STDERR;
                jl_printf(s, "WARNING: Method definition ");
                jl_static_show_func_sig(s, (jl_value_t*)type);
                jl_printf(s, " in module %s", jl_symbol_name(oldmod->name));
                print_func_loc(s, l->func);
                jl_printf(s, " overwritten");
                if (oldmod != newmod)
                    jl_printf(s, " in module %s", jl_symbol_name(newmod->name));
                print_func_loc(s, method);
                jl_printf(s, ".\n");
            }
            JL_SIGATOMIC_BEGIN();
            l->sig = type;
            jl_gc_wb(l, l->sig);
            l->tvars = tvars;
            jl_gc_wb(l, l->tvars);
            l->va = jl_is_va_tuple(type);
            l->isstaged = isstaged;
            l->invokes = (struct _jl_methtable_t *)jl_nothing;
            // TODO: `l->func` or `l->func->roots` might need to be rooted
            l->func = method;
            if (l->func)
                jl_gc_wb(l, l->func);
            JL_SIGATOMIC_END();
            return l;
        }
        l = l->next;
    }
    pl = pml;
    l = *pml;
    jl_value_t *pa = parent;
    while (l != (void*)jl_nothing) {
        if (jl_args_morespecific((jl_value_t*)type, (jl_value_t*)l->sig))
            break;
        if (check_amb) {
            check_ambiguous(*pml, type, l, method->name, method);
        }
        pl = &l->next;
        pa = (jl_value_t*)l;
        l = l->next;
    }
    jl_methlist_t *newrec = (jl_methlist_t*)jl_gc_allocobj(sizeof(jl_methlist_t));
    jl_set_typeof(newrec, jl_method_type);
    newrec->sig = type;
    newrec->tvars = tvars;
    newrec->va = jl_is_va_tuple(type);
    newrec->isstaged = isstaged;
    newrec->func = method;
    newrec->invokes = (struct _jl_methtable_t*)jl_nothing;
    newrec->next = l;
    JL_SIGATOMIC_BEGIN();
    JL_GC_PUSH1(&newrec);
    *pl = newrec;
    jl_gc_wb(pa, newrec);
    // if this contains Union types, methods after it might actually be
    // more specific than it. we need to re-sort them.
    if (has_unions(type)) {
        jl_value_t *item_parent = (jl_value_t*)newrec;
        jl_value_t *next_parent = 0;
        jl_methlist_t *item = newrec->next, *next;
        jl_methlist_t **pitem = &newrec->next, **pnext;
        while (item != (void*)jl_nothing) {
            pl = pml;
            l = *pml;
            pa = parent;
            next = item->next;
            pnext = &item->next;
            next_parent = (jl_value_t*)item;
            while (l != newrec->next) {
                if (jl_args_morespecific((jl_value_t*)item->sig,
                                         (jl_value_t*)l->sig)) {
                    // reinsert item earlier in the list
                    *pitem = next;
                    jl_gc_wb(item_parent, next);
                    item->next = l;
                    jl_gc_wb(item, item->next);
                    *pl = item;
                    jl_gc_wb(pa, item);
                    pnext = pitem;
                    next_parent = item_parent;
                    break;
                }
                pl = &l->next;
                pa = (jl_value_t*)l;
                l = l->next;
            }
            item = next;
            pitem = pnext;
            item_parent = next_parent;
        }
    }
    JL_GC_POP();
    JL_SIGATOMIC_END();
    return newrec;
}

static void remove_conflicting(jl_methlist_t **pl, jl_value_t *type)
{
    jl_methlist_t *l = *pl;
    while (l != (void*)jl_nothing) {
        if (jl_type_intersection(type, (jl_value_t*)l->sig) !=
            (jl_value_t*)jl_bottom_type) {
            *pl = l->next;
        }
        else {
            pl = &l->next;
        }
        l = l->next;
    }
}

static void update_max_args(jl_methtable_t *mt, jl_tupletype_t *type)
{
    size_t na = jl_nparams(type);
    if (jl_is_va_tuple(type))
        na--;
    if (na > mt->max_args)
        mt->max_args = na;
}

jl_methlist_t *jl_method_table_insert(jl_methtable_t *mt, jl_tupletype_t *type,
                                      jl_lambda_info_t *method, jl_svec_t *tvars,
                                      int8_t isstaged)
{
    if (jl_svec_len(tvars) == 1)
        tvars = (jl_svec_t*)jl_svecref(tvars,0);
    if (isstaged) // staged function definitions will be compiled directly. mark them accordingly.
        method->specTypes = jl_anytuple_type;
    else // otherwise, the method will be compiled via unspecialized or the method cache, never directly
        assert(!method->specTypes);
    JL_SIGATOMIC_BEGIN();
    jl_methlist_t *ml = jl_method_list_insert(&mt->defs,type,method,tvars,1,isstaged,(jl_value_t*)mt);
    // invalidate cached methods that overlap this definition
    remove_conflicting(&mt->cache, (jl_value_t*)type);
    jl_gc_wb(mt, mt->cache);
    if (mt->cache_arg1 != (void*)jl_nothing) {
        for(int i=0; i < jl_array_len(mt->cache_arg1); i++) {
            jl_methlist_t **pl = &((jl_methlist_t**)jl_array_data(mt->cache_arg1))[i];
            if (*pl && *pl != (void*)jl_nothing) {
                remove_conflicting(pl, (jl_value_t*)type);
                jl_gc_wb(mt->cache_arg1, jl_cellref(mt->cache_arg1,i));
            }
        }
    }
    if (mt->cache_targ != (void*)jl_nothing) {
        for(int i=0; i < jl_array_len(mt->cache_targ); i++) {
            jl_methlist_t **pl = &((jl_methlist_t**)jl_array_data(mt->cache_targ))[i];
            if (*pl && *pl != (void*)jl_nothing) {
                remove_conflicting(pl, (jl_value_t*)type);
                jl_gc_wb(mt->cache_targ, jl_cellref(mt->cache_targ,i));
            }
        }
    }
    update_max_args(mt, type);
    JL_SIGATOMIC_END();
    return ml;
}

void JL_NORETURN jl_no_method_error_bare(jl_function_t *f, jl_value_t *args)
{
    jl_value_t *fargs[3] = {
        (jl_value_t*)jl_methoderror_type,
        (jl_value_t*)f,
        args
    };
    if (jl_base_module) {
        jl_throw(jl_apply_generic(fargs, 3));
    }
    else {
        jl_printf((JL_STREAM*)STDERR_FILENO, "A method error occurred before the base module was defined. Aborting...\n");
        jl_static_show((JL_STREAM*)STDERR_FILENO,(jl_value_t*)f); jl_printf((JL_STREAM*)STDERR_FILENO,"\n");
        jl_static_show((JL_STREAM*)STDERR_FILENO,args); jl_printf((JL_STREAM*)STDERR_FILENO,"\n");
        jl_bt_size = rec_backtrace(jl_bt_data, JL_MAX_BT_SIZE);
        jl_critical_error(0, NULL, jl_bt_data, &jl_bt_size);
        abort();
    }
    // not reached
}

void JL_NORETURN jl_no_method_error(jl_function_t *f, jl_value_t **args, size_t na)
{
    jl_value_t *argtup = jl_f_tuple(NULL, args+1, na-1);
    JL_GC_PUSH1(&argtup);
    jl_no_method_error_bare(f, argtup);
    // not reached
}

jl_tupletype_t *arg_type_tuple(jl_value_t **args, size_t nargs)
{
    jl_tupletype_t *tt;
    size_t i;
    if (nargs < jl_page_size/sizeof(jl_value_t*)) {
        jl_value_t **types;
        JL_GC_PUSHARGS(types, nargs);
        for(i=0; i < nargs; i++) {
            jl_value_t *ai = args[i];
            if (jl_is_type(ai))
                types[i] = (jl_value_t*)jl_wrap_Type(ai);
            else
                types[i] = jl_typeof(ai);
        }
        tt = (jl_tupletype_t*)jl_inst_concrete_tupletype_v(types, nargs);
        JL_GC_POP();
    }
    else {
        jl_svec_t *types = jl_alloc_svec(nargs);
        JL_GC_PUSH1(&types);
        for(i=0; i < nargs; i++) {
            jl_value_t *ai = args[i];
            if (jl_is_type(ai))
                jl_svecset(types, i, (jl_value_t*)jl_wrap_Type(ai));
            else
                jl_svecset(types, i, jl_typeof(ai));
        }
        tt = (jl_tupletype_t*)jl_inst_concrete_tupletype(types);
        JL_GC_POP();
    }
    return tt;
}

jl_lambda_info_t *jl_method_lookup_by_type(jl_methtable_t *mt, jl_tupletype_t *types,
                                           int cache, int inexact)
{
    jl_lambda_info_t *sf = jl_method_table_assoc_exact_by_type(mt, types);
    if (sf == NULL) {
        if (jl_is_leaf_type((jl_value_t*)types)) cache=1;
        sf = jl_mt_assoc_by_type(mt, types, cache, inexact);
    }
    return sf;
}

JL_DLLEXPORT int jl_method_exists(jl_methtable_t *mt, jl_tupletype_t *types)
{
    return jl_method_lookup_by_type(mt, types, 0, 0) != NULL;
}

jl_lambda_info_t *jl_method_lookup(jl_methtable_t *mt, jl_value_t **args, size_t nargs, int cache)
{
    jl_lambda_info_t *sf = jl_method_table_assoc_exact(mt, args, nargs);
    if (sf == NULL) {
        jl_tupletype_t *tt = arg_type_tuple(args, nargs);
        JL_GC_PUSH1(&tt);
        sf = jl_mt_assoc_by_type(mt, tt, cache, 0);
        JL_GC_POP();
    }
    return sf;
}

JL_DLLEXPORT jl_value_t *jl_matching_methods(jl_value_t *types, int lim);

// compile-time method lookup
jl_lambda_info_t *jl_get_specialization1(jl_tupletype_t *types)
{
    assert(jl_nparams(types) > 0);
    if (!jl_is_leaf_type((jl_value_t*)types))
        return NULL;
    assert(jl_is_datatype(jl_tparam0(types)));

    // make sure exactly 1 method matches (issue #7302).
    int i;
    for(i=0; i < jl_nparams(types); i++) {
        jl_value_t *ti = jl_tparam(types, i);
        // if one argument type is DataType, multiple Type{} definitions
        // might match. also be conservative with tuples rather than trying
        // to analyze them in detail.
        if (ti == (jl_value_t*)jl_datatype_type || jl_is_tuple_type(ti)) {
            jl_value_t *matches = jl_matching_methods((jl_value_t*)types, 1);
            if (matches == jl_false)
                return NULL;
            break;
        }
    }

    jl_methtable_t *mt = ((jl_datatype_t*)jl_tparam0(types))->name->mt;
    jl_lambda_info_t *sf = NULL;
    // most of the time sf is rooted in mt, but if the method is staged it may
    // not be the case
    JL_GC_PUSH1(&sf);
    JL_TRY {
        sf = jl_method_lookup_by_type(mt, types, 1, 1);
    } JL_CATCH {
        goto not_found;
    }
    if (sf == NULL || sf->code == NULL || sf->inInference)
        goto not_found;
    if (sf->functionObjects.functionObject == NULL) {
        if (sf->fptr != NULL)
            goto not_found;
        jl_compile_linfo(sf);
    }
    JL_GC_POP();
    return sf;
 not_found:
    JL_GC_POP();
    return NULL;
}

// add type of `f` to front of argument tuple type
jl_tupletype_t *jl_argtype_with_function(jl_function_t *f, jl_tupletype_t *types)
{
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
    JL_GC_POP();
    return (jl_tupletype_t*)tt;
}

static int tupletype_any_bottom(jl_value_t *sig)
{
    jl_svec_t *types = ((jl_tupletype_t*)sig)->types;
    size_t i, l = jl_svec_len(types);
    for (i = 0; i < l; i++) {
        if (jl_svecref(types, i) == jl_bottom_type)
            return 1;
    }
    return 0;
}

static int _compile_all_tvar_union(jl_methlist_t *meth, jl_tupletype_t *methsig)
{
    // f{<:Union{...}}(...) is a common pattern
    // and expanding the Union may give a leaf function
    jl_tvar_t **tvs;
    int tvarslen;
    if (jl_is_typevar(meth->tvars)) {
        tvs = (jl_tvar_t**)&meth->tvars;
        tvarslen = 1;
    }
    else {
        tvs = (jl_tvar_t**)jl_svec_data(meth->tvars);
        tvarslen = jl_svec_len(meth->tvars);
        if (tvarslen == 0) {
            if (jl_is_leaf_type((jl_value_t*)methsig)) {
                // usually can create a specialized version of the function,
                // if the signature is already a leaftype
                jl_lambda_info_t *spec = jl_get_specialization1(methsig);
                if (spec) {
                    if (methsig == meth->sig) {
                        // replace unspecialized func with newly specialized version
                        meth->func->unspecialized = spec;
                        jl_gc_wb(meth->func, spec);
                    }
                    return 1;
                }
            }
            return 0;
        }
    }

    int complete = 1;
    jl_value_t **env;
    JL_GC_PUSHARGS(env, 2 * tvarslen);
    int *idx = (int*)alloca(sizeof(int) * tvarslen);
    int i;
    for (i = 0; i < tvarslen; i++) {
        idx[i] = 0;
        env[2 * i] = (jl_value_t*)tvs[i];
        env[2 * i + 1] = jl_bottom_type; // initialize the list with Union{}, since T<:Union{} is always a valid option
    }

    for (i = 0; i < tvarslen; /* incremented by inner loop */) {
        jl_value_t *sig;
        JL_TRY {
            sig = (jl_value_t*)
                jl_instantiate_type_with((jl_value_t*)methsig, env, tvarslen);
        }
        JL_CATCH {
            goto getnext; // sigh, we found an invalid type signature. should we warn the user?
        }
        assert(jl_is_tuple_type(sig));
        if (sig == jl_bottom_type || tupletype_any_bottom(sig)) {
            goto getnext; // signature wouldn't be callable / is invalid -- skip it
        }
        if (jl_is_leaf_type(sig)) {
            if (jl_get_specialization1((jl_tupletype_t*)sig)) {
                if (!jl_has_typevars((jl_value_t*)sig)) goto getnext; // success
            }
        }
        complete = 0;

getnext:
        for (i = 0; i < tvarslen; i++) {
            jl_tvar_t *tv = tvs[i];
            if (jl_is_uniontype(tv->ub)) {
                jl_uniontype_t *ub = (jl_uniontype_t*)tv->ub;
                size_t l = jl_svec_len(ub->types);
                size_t j = idx[i];
                if (j == l) {
                    env[2 * i + 1] = jl_bottom_type;
                    idx[i] = 0;
                }
                else {
                    jl_value_t *ty = jl_svecref(ub->types, j);
                    if (!jl_is_leaf_type(ty))
                        ty = (jl_value_t*)jl_new_typevar(tv->name, tv->lb, ty);
                    env[2 * i + 1] = ty;
                    idx[i] = j + 1;
                    break;
                }
            }
            else {
                env[2 * i + 1] = (jl_value_t*)tv;
                complete = 0;
            }
        }
    }
    JL_GC_POP();
    return complete;
}

static int _compile_all_union(jl_methlist_t *meth)
{
    // f(::Union{...}, ...) is a common pattern
    // and expanding the Union may give a leaf function
    jl_tupletype_t *sig = meth->sig;
    int complete = 1;
    size_t count_unions = 0;
    size_t i, l = jl_svec_len(sig->parameters);
    jl_svec_t *p = NULL;
    jl_tupletype_t *methsig = NULL;

    for (i = 0; i < l; i++) {
        jl_value_t *ty = jl_svecref(sig->parameters, i);
        if (jl_is_uniontype(ty)) {
            jl_svec_t *utypes = ((jl_uniontype_t*)ty)->types;
            size_t l = jl_svec_len(utypes);
            if (l == 0)
                return 1; // why does this method exist?
            ++count_unions;
        }
    }

    if (count_unions == 0)
        return _compile_all_tvar_union(meth, sig);

    int *idx = (int*)alloca(sizeof(int) * count_unions);
    for (i = 0; i < count_unions; i++) {
        idx[i] = 0;
    }

    JL_GC_PUSH2(&p, &methsig);
    int idx_ctr = 0, incr = 0;
    while (!incr) {
        jl_svec_t *p = jl_alloc_svec_uninit(l);
        for (i = 0, idx_ctr = 0, incr = 1; i < l; i++) {
            jl_value_t *ty = jl_svecref(sig->parameters, i);
            if (jl_is_uniontype(ty)) {
                jl_svec_t *utypes = ((jl_uniontype_t*)ty)->types;
                size_t l = jl_svec_len(utypes);
                size_t j = idx[idx_ctr];
                jl_svecset(p, i, jl_svecref(utypes, j));
                ++j;
                if (incr) {
                    if (j == l) {
                        idx[idx_ctr] = 0;
                    }
                    else {
                        idx[idx_ctr] = j;
                        incr = 0;
                    }
                }
                ++idx_ctr;
            }
            else {
                jl_svecset(p, i, ty);
            }
        }
        methsig = jl_apply_tuple_type(p);
        if (!_compile_all_tvar_union(meth, methsig))
            complete = 0;
    }

    JL_GC_POP();
    return complete;
}

static void _compile_all_deq(jl_array_t *found)
{
    size_t found_i, found_l = jl_array_len(found);
    jl_printf(JL_STDERR, "found %d uncompiled methods for compile-all\n", (int)found_l);
    for (found_i = 0; found_i < found_l; found_i++) {
        jl_printf(JL_STDERR, " %zd / %zd\r", found_i + 1, found_l);
        jl_methlist_t *meth = (jl_methlist_t*)jl_cellref(found, found_i);
        jl_lambda_info_t *linfo = meth->func;

        if (!linfo)
            return; // XXX: how does this happen
        if (!linfo->specTypes)
            linfo = jl_get_unspecialized(linfo);
        if (!linfo->inferred) {
            // force this function to be recompiled
            jl_type_infer(linfo, jl_false);
            linfo->functionObjects.functionObject = NULL;
            linfo->functionObjects.specFunctionObject = NULL;
            linfo->functionObjects.cFunctionList = NULL;
            linfo->functionID = 0;
            linfo->specFunctionID = 0;
            linfo->jlcall_api = 0;
        }

        // keep track of whether all possible signatures have been cached (and thus whether it can skip trying to compile the unspecialized function)
        // this is necessary because many intrinsics try to call static_eval and thus are not compilable unspecialized
        int complete = _compile_all_union(meth);
        if (complete) {
            if (!linfo->functionID)
                // indicate that this method doesn't need a functionID because it was fully covered above
                linfo->functionID = -1;
        }
        else {
            jl_compile_linfo(linfo);
            assert(linfo->functionID > 0);
        }
    }
    jl_printf(JL_STDERR, "\n");
}

static void _compile_all_enq_mt(jl_methtable_t *mt, jl_array_t *found);
static void _compile_all_enq_ml(jl_methlist_t *ml, jl_array_t *found)
{
    while (ml != NULL && (jl_value_t*)ml != jl_nothing) {
        if (ml->func != NULL) {
            // found a method (not a placeholder guard)
            jl_lambda_info_t *linfo = ml->func;
            if (!linfo->specTypes) {
                // method definition -- compile via unspecialized field
                if (linfo->fptr != NULL)
                    return; // builtin function
                linfo = jl_get_unspecialized(linfo);
            }
            if (!linfo->functionID) {
                // and it still needs to be compiled
                if (!ml->isstaged)
                    jl_cell_1d_push(found, (jl_value_t*)ml);
            }
        }
        _compile_all_enq_mt(ml->invokes, found);
        ml = ml->next;
    }
}

static void _compile_all_enq_mt(jl_methtable_t *mt, jl_array_t *found)
{
    jl_array_t *a;
    if (mt == NULL || (jl_value_t*)mt == jl_nothing) return;
    _compile_all_enq_ml(mt->defs, found);
    _compile_all_enq_ml(mt->cache, found);

    a = mt->cache_arg1;
    if ((jl_value_t*)a != jl_nothing) {
        size_t i, l = jl_array_len(a);
        for (i = 0; i < l; i++) {
            _compile_all_enq_ml((jl_methlist_t*)jl_cellref(a, i), found);
        }
    }

    a = mt->cache_targ;
    if ((jl_value_t*)a != jl_nothing) {
        size_t i, l = jl_array_len(a);
        for (i = 0; i < l; i++) {
            _compile_all_enq_ml((jl_methlist_t*)jl_cellref(a, i), found);
        }
    }
}

static void _compile_all_enq_module(jl_module_t *m, jl_array_t *found)
{
    // scan through all types reachable from 'v' and
    // record all jl_lambda_info_t objects and signatures in their method tables
    size_t i, sz = m->bindings.size;
    for(i=1; i < sz; i+=2) {
        if (m->bindings.table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)m->bindings.table[i];
            if (b->owner == m && b->value && b->constp) {
                jl_value_t *v = b->value;
                if (jl_is_datatype(v)) {
                    jl_typename_t *tn = ((jl_datatype_t*)v)->name;
                    if (tn->module == m && tn->name == b->name) {
                        _compile_all_enq_mt(tn->mt, found);
                    }
                }
                else if (jl_is_module(v)) {
                    jl_module_t *child = (jl_module_t*)b->value;
                    if (child != m && child->parent == m && child->name == b->name) {
                        // this is the original/primary binding for the submodule
                        _compile_all_enq_module(child, found);
                    }
                }
            }
        }
    }
}

void jl_compile_all(void)
{
    // this "found" array will contain
    // LambdaInfos that need to be compiled
    // and (generic-function, method) pairs that may be optimized (and need to be compiled)
    jl_array_t *m = jl_alloc_cell_1d(0);
    JL_GC_PUSH1(&m);
    while (1) {
        _compile_all_enq_module(jl_main_module, m);
        size_t changes = jl_array_len(m);
        if (!changes)
            break;
        _compile_all_deq(m);
        jl_array_del_end(m, changes);
    }
    JL_GC_POP();
}

JL_DLLEXPORT void jl_compile_hint(jl_tupletype_t *types)
{
    (void)jl_get_specialization1(types);
}

#ifdef JL_TRACE
static int trace_en = 0;
static int error_en = 1;
static void __attribute__ ((unused)) enable_trace(int x) { trace_en=x; }
static void show_call(jl_value_t *F, jl_value_t **args, uint32_t nargs)
{
    jl_printf(JL_STDOUT, "%s(",  jl_symbol_name(jl_gf_name(F)));
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

JL_DLLEXPORT jl_value_t *jl_apply_generic(jl_value_t **args, uint32_t nargs)
{
    jl_value_t *F = args[0];
    jl_methtable_t *mt = jl_gf_mtable(F);
#ifdef JL_GF_PROFILE
    mt->ncalls++;
#endif
#ifdef JL_TRACE
    int traceen = trace_en; //&& ((char*)&mt < jl_stack_hi-6000000);
    if (traceen)
        show_call(F, &args[1], nargs-1);
#endif
    /*
      search order:
      look at concrete signatures
      if there is an exact match, return it
      otherwise look for a matching generic signature
      if no concrete or generic match, raise error
      if no generic match, use the concrete one even if inexact
      otherwise instantiate the generic method and use it
    */
    jl_lambda_info_t *mfunc = jl_method_table_assoc_exact(mt, args, nargs);

    jl_tupletype_t *tt = NULL;
    if (mfunc == NULL) {
        // cache miss case
        tt = arg_type_tuple(args, nargs);
        // if running inference overwrites this particular method, it becomes
        // unreachable from the method table, so root mfunc.
        JL_GC_PUSH2(&tt, &mfunc);
        mfunc = jl_mt_assoc_by_type(mt, tt, 1, 0);

        if (mfunc == NULL) {
#ifdef JL_TRACE
            if (error_en)
                show_call(F, args, nargs);
#endif
            JL_GC_POP();
            jl_no_method_error((jl_function_t*)F, args, nargs);
            // unreachable
        }
    }
#ifdef JL_TRACE
    if (traceen)
        jl_printf(JL_STDOUT, " at %s:%d\n", jl_symbol_name(mfunc->file), mfunc->line);
#endif
    jl_value_t *res;
    if (mfunc->inInference || mfunc->inCompile) {
        // if inference is running on this function, return a copy
        // of the function to be compiled without inference and run.
        res = jl_call_unspecialized(mfunc->sparam_vals, jl_get_unspecialized(mfunc), args, nargs);
    }
    else {
        res = jl_call_method_internal(mfunc, args, nargs);
    }
    if (tt) JL_GC_POP();
    return verify_type(res);
}

JL_DLLEXPORT jl_value_t *jl_gf_invoke_lookup(jl_datatype_t *types)
{
    jl_methtable_t *mt = ((jl_datatype_t*)jl_tparam0(types))->name->mt;
    jl_methlist_t *m = mt->defs;
    size_t typelen = jl_nparams(types);

    while (m != (void*)jl_nothing) {
        if (m->tvars!=jl_emptysvec) {
            if (jl_type_match((jl_value_t*)types,
                              (jl_value_t*)m->sig) != (jl_value_t*)jl_false)
                break;
        }
        else if (jl_tuple_subtype(jl_svec_data(types->parameters), typelen, m->sig, 0)) {
            break;
        }
        m = m->next;
    }

    if (m == (void*)jl_nothing)
        return jl_nothing;
    return (jl_value_t*)m;
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
jl_value_t *jl_gf_invoke(jl_tupletype_t *types0, jl_value_t **args, size_t nargs)
{
    jl_svec_t *tpenv=jl_emptysvec;
    jl_tupletype_t *newsig=NULL;
    jl_tupletype_t *tt=NULL;
    jl_tupletype_t *types=NULL;
    JL_GC_PUSH4(&types, &tpenv, &newsig, &tt);
    jl_value_t *gf = args[0];
    types = (jl_datatype_t*)jl_argtype_with_function(gf, (jl_tupletype_t*)types0);
    jl_methtable_t *mt = jl_gf_mtable(gf);
    jl_methlist_t *m = (jl_methlist_t*)jl_gf_invoke_lookup(types);

    if ((jl_value_t*)m == jl_nothing) {
        jl_no_method_error_bare(gf, (jl_value_t*)types0);
        // unreachable
    }

    // now we have found the matching definition.
    // next look for or create a specialization of this definition.

    jl_lambda_info_t *mfunc;
    if (m->invokes == (void*)jl_nothing)
        mfunc = NULL;
    else
        mfunc = jl_method_table_assoc_exact(m->invokes, args, nargs);
    if (mfunc != NULL) {
        if (mfunc->inInference || mfunc->inCompile) {
            // if inference is running on this function, return a copy
            // of the function to be compiled without inference and run.
            JL_GC_POP();
            return jl_call_unspecialized(mfunc->sparam_vals, jl_get_unspecialized(mfunc), args, nargs);
        }
    }
    else {
        tt = arg_type_tuple(args, nargs);
        if (m->invokes == (void*)jl_nothing) {
            m->invokes = jl_new_method_table(mt->name, mt->module);
            jl_gc_wb(m, m->invokes);
            update_max_args(m->invokes, tt);
            // this private method table has just this one definition
            jl_method_list_insert(&m->invokes->defs,m->sig,m->func,m->tvars,0,m->isstaged,(jl_value_t*)m->invokes);
        }

        if (m->tvars != jl_emptysvec) {
            jl_value_t *ti =
                lookup_match((jl_value_t*)tt, (jl_value_t*)m->sig, &tpenv, m->tvars);
            assert(ti != (jl_value_t*)jl_bottom_type);
            (void)ti;
        }
        jl_lambda_info_t *func = m->func;
        if (m->isstaged)
            func = jl_instantiate_staged(func, tt, tpenv);

        mfunc = cache_method(m->invokes, tt, func, m, tpenv);
    }

    JL_GC_POP();
    return jl_call_method_internal(mfunc, args, nargs);
}

void print_func_loc(JL_STREAM *s, jl_lambda_info_t *li)
{
    long lno = li->line;
    if (lno > 0) {
        char *fname = jl_symbol_name((jl_sym_t*)li->file);
        jl_printf(s, " at %s:%ld", fname, lno);
    }
}

JL_DLLEXPORT jl_function_t *jl_new_generic_function_with_supertype(jl_sym_t *name, jl_module_t *module, jl_datatype_t *st, int iskw)
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
    jl_datatype_t *ftype = jl_new_datatype(tname, st, jl_emptysvec, jl_emptysvec, jl_emptysvec, 0, 0, 0);
    JL_GC_PUSH1(&ftype);
    ftype->name->mt->name = name; jl_gc_wb(ftype->name->mt, name);
    ftype->name->module = module; jl_gc_wb(ftype->name, module);
    ftype->name->mt->module = module; jl_gc_wb(ftype->name->mt, module);
    jl_set_const(module, tname, (jl_value_t*)ftype);
    jl_value_t *f = jl_new_struct(ftype);
    ftype->instance = f; jl_gc_wb(ftype, f);
    JL_GC_POP();
    return (jl_function_t*)f;
}

JL_DLLEXPORT jl_function_t *jl_get_kwsorter(jl_typename_t *tn)
{
    jl_methtable_t *mt = tn->mt;
    if (!mt->kwsorter) {
        mt->kwsorter = jl_new_generic_function_with_supertype(tn->name, mt->module, jl_function_type, 1);
        jl_gc_wb(mt, mt->kwsorter);
    }
    return mt->kwsorter;
}

JL_DLLEXPORT jl_function_t *jl_new_generic_function(jl_sym_t *name, jl_module_t *module)
{
    return jl_new_generic_function_with_supertype(name, module, jl_function_type, 0);
}

void jl_add_method_to_table(jl_methtable_t *mt, jl_tupletype_t *types, jl_lambda_info_t *meth,
                            jl_svec_t *tvars, int8_t isstaged)
{
    assert(jl_is_tuple_type(types));
    assert(jl_is_lambda_info(meth));
    assert(jl_is_mtable(mt));
    JL_GC_PUSH1(&meth);
    jl_sym_t *n = mt->name;
    if (meth->name != anonymous_sym && meth->name != n) {
        // already used by another GF; make a copy (issue #10373)
        assert(meth->sparam_vals == jl_emptysvec);
        meth = jl_add_static_parameters(meth, jl_emptysvec, NULL);
        meth->unspecialized = NULL;
    }
    meth->name = n;
    (void)jl_method_table_insert(mt, types, meth, tvars, isstaged);
    JL_GC_POP();
}

void jl_add_method(jl_function_t *gf, jl_tupletype_t *types, jl_lambda_info_t *meth,
                   jl_svec_t *tvars, int8_t isstaged)
{
    jl_add_method_to_table(jl_gf_mtable(gf), types, meth, tvars, isstaged);
}

JL_DLLEXPORT jl_svec_t *jl_match_method(jl_value_t *type, jl_value_t *sig,
                                        jl_svec_t *tvars)
{
    jl_svec_t *env = jl_emptysvec;
    jl_value_t *ti=NULL;
    JL_GC_PUSH2(&env, &ti);
    ti = lookup_match(type, (jl_value_t*)sig, &env, tvars);
    jl_svec_t *result = jl_svec2(ti, env);
    JL_GC_POP();
    return result;
}

// Determine whether a typevar exists inside at most one DataType.
// These are the typevars that will always be matched by any matching
// arguments.
static int tvar_exists_at_top_level(jl_value_t *tv, jl_tupletype_t *sig, int attop)
{
    int i, l=jl_nparams(sig);
    for(i=0; i < l; i++) {
        jl_value_t *a = jl_tparam(sig, i);
        if (jl_is_vararg_type(a))
            a = jl_tparam0(a);
        if (a == tv)
            return 1;
        if (attop && jl_is_datatype(a)) {
            jl_svec_t *p = ((jl_datatype_t*)a)->parameters;
            int j;
            for(j=0; j < jl_svec_len(p); j++) {
                if (jl_svecref(p,j) == tv)
                    return 1;
            }
        }
    }
    return 0;
}

// returns a match as (argtypes, static_params, Method)
static jl_value_t *ml_matches(jl_methlist_t *ml, jl_value_t *type,
                              jl_sym_t *name, int lim)
{
    jl_array_t *t = (jl_array_t*)jl_an_empty_cell;
    jl_svec_t *matc=NULL;
    jl_svec_t *env = jl_emptysvec;
    jl_value_t *ti=NULL;
    JL_GC_PUSH4(&t, &matc, &env, &ti);
    int len=0, i;
    while (ml != (void*)jl_nothing) {
        // a method is shadowed if type <: S <: m->sig where S is the
        // signature of another applicable method
        /*
          more generally, we can stop when the type is a subtype of the
          union of all the signatures examined so far.
        */
        env = jl_emptysvec;
        ti = lookup_match(type, (jl_value_t*)ml->sig, &env, ml->tvars);
        if (ti != (jl_value_t*)jl_bottom_type) {
            assert(ml->func);
            assert(jl_is_svec(env));

            int skip = 0;
            if (lim >= 0) {
                // we can skip this match if the types are already covered
                // by a prior (more specific) match. but only do this in
                // the "limited" mode used by type inference.
                size_t l = jl_array_len(t);
                for(i=0; i < l; i++) {
                    jl_value_t *prior_ti = jl_svecref(jl_cellref(t,i),0);
                    // in issue #13007 we incorrectly set skip=1 here, due to
                    // Type{_<:T} ∩ (UnionAll S Type{T{S}}) = Type{T{S}}
                    // Instead we should have computed the intersection as (UnionAll S Type{T{S}}),
                    // which is a bigger type that would not have been a subtype of the prior
                    // match (prior_ti). We simulate that for now by checking jl_has_typevars.
                    if (jl_is_leaf_type(prior_ti) && !jl_has_typevars(ti) && !jl_has_typevars(prior_ti) &&
                        jl_subtype(ti, prior_ti, 0)) {
                        skip = 1;
                        break;
                    }
                }
                // don't analyze slots declared with ANY
                // TODO
                /*
                l = jl_nparams(ml->sig);
                size_t m = jl_nparams(ti);
                for(i=0; i < l && i < m; i++) {
                    if (jl_tparam(ml->sig, i) == jl_ANY_flag)
                        jl_tupleset(ti, i, jl_any_type);
                }
                */
            }
            if (!skip) {
                len++;
                if (lim >= 0 && len > lim) {
                    JL_GC_POP();
                    return jl_false;
                }
                matc = jl_svec(3, ti, env, ml);
                /*
                  Check whether all static parameters matched. If not, then we
                  have an argument type like Vector{T{Int,_}}, and a signature like
                  f{A,B}(::Vector{T{A,B}}). If "_" turns out to be a non-typevar
                  at runtime then this method matches, otherwise it doesn't. So we
                  have to look for more matches. This caused issue #4731.
                */
                int matched_all_typevars = 1;
                size_t l = jl_svec_len(env);
                for(i=0; i < l; i++) {
                    jl_value_t *tv;
                    if (jl_is_typevar(ml->tvars))
                        tv = (jl_value_t*)ml->tvars;
                    else
                        tv = jl_svecref(ml->tvars, i);
                    if (jl_is_typevar(jl_svecref(env,i)) &&
                        // if tvar is at the top level it will definitely be matched.
                        // see issue #5575
                        !tvar_exists_at_top_level(tv, ml->sig, 1)) {
                        matched_all_typevars = 0;
                        break;
                    }
                }
                if (len == 1) {
                    t = jl_alloc_cell_1d(1);
                    jl_cellset(t, 0, (jl_value_t*)matc);
                }
                else {
                    jl_cell_1d_push(t, (jl_value_t*)matc);
                }
                // (type ∩ ml->sig == type) ⇒ (type ⊆ ml->sig)
                // NOTE: jl_subtype check added in case the intersection is
                // over-approximated.
                if (matched_all_typevars && jl_types_equal(jl_svecref(matc,0), type) &&
                    jl_subtype(type, (jl_value_t*)ml->sig, 0)) {
                    JL_GC_POP();
                    return (jl_value_t*)t;
                }
            }
        }
        ml = ml->next;
    }
    JL_GC_POP();
    return (jl_value_t*)t;
}

// return a cell array of svecs, each describing a method match:
// {svec(t, spvals, li, cenv), ...}
// t is the intersection of the type argument and the method signature,
// spvals is any matched static parameter values, li is the LambdaInfo,
// and cenv is the closure environment or ().
//
// lim is the max # of methods to return. if there are more return jl_false.
// -1 for no limit.
JL_DLLEXPORT jl_value_t *jl_matching_methods(jl_value_t *types, int lim)
{
    assert(jl_nparams(types) > 0);
    if (jl_tparam0(types) == jl_bottom_type)
        return (jl_value_t*)jl_alloc_cell_1d(0);
    assert(jl_is_datatype(jl_tparam0(types)));
    jl_methtable_t *mt = ((jl_datatype_t*)jl_tparam0(types))->name->mt;
    if (mt == NULL)
        return (jl_value_t*)jl_alloc_cell_1d(0);
    return ml_matches(mt->defs, types, mt->name, lim);
}

#ifdef __cplusplus
}
#endif

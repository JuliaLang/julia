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
#ifdef _OS_WINDOWS_
#include <malloc.h>
#endif
#include "julia.h"
#include "julia_internal.h"

// ::ANY has no effect if the number of overlapping methods is greater than this
#define MAX_UNSPECIALIZED_CONFLICTS 10

#ifdef __cplusplus
extern "C" {
#endif

static jl_value_t *jl_apply_unspecialized(jl_function_t *meth, jl_value_t **args, uint32_t nargs)
{
    jl_function_t *unspecialized = meth->linfo->unspecialized;
    assert(unspecialized != jl_bottom_func);
    if (meth->env == (jl_value_t*)jl_emptysvec) {
        return jl_apply(unspecialized, args, nargs);
    }
    else {
        jl_function_t *closuremeth = jl_new_closure(unspecialized->fptr, meth->env, unspecialized->linfo);
        JL_GC_PUSH1(&closuremeth);
        jl_value_t *v = jl_apply(closuremeth, args, nargs);
        JL_GC_POP();
        return v;
    }
}


static jl_methtable_t *new_method_table(jl_sym_t *name, jl_module_t *module)
{
    jl_methtable_t *mt = (jl_methtable_t*)jl_gc_allocobj(sizeof(jl_methtable_t));
    jl_set_typeof(mt, jl_methtable_type);
    mt->name = name;
    mt->module = module;
    mt->defs = (jl_methlist_t*)jl_nothing;
    mt->cache = (jl_methlist_t*)jl_nothing;
    mt->cache_arg1 = (jl_array_t*)jl_nothing;
    mt->cache_targ = (jl_array_t*)jl_nothing;
    mt->max_args = 0;
    mt->kwsorter = NULL;
#ifdef JL_GF_PROFILE
    mt->ncalls = 0;
#endif
    return mt;
}

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

static inline int cache_match(jl_value_t **args, size_t n, jl_tupletype_t *sig,
                              int va, size_t lensig)
{
    // NOTE: This function is a huge performance hot spot!!
    for(size_t i=0; i < n; i++) {
        jl_value_t *decl = jl_field_type(sig, i);
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
jl_methlist_t *mtcache_hash_lookup(jl_array_t *a, jl_value_t *ty, int tparam)
{
    uptrint_t uid = ((jl_datatype_t*)ty)->uid;
    jl_methlist_t *ml = (jl_methlist_t*)jl_cellref(a, uid & (a->nrows-1));
    if (ml && ml!=(void*)jl_nothing) {
        jl_value_t *t = jl_field_type(ml->sig, 0);
        if (tparam) t = jl_tparam0(t);
        if (t == ty)
            return ml;
    }
    return (jl_methlist_t*)jl_nothing;
}

static void mtcache_rehash(jl_array_t **pa, jl_value_t* parent)
{
    size_t len = (*pa)->nrows;
    jl_value_t **d = (jl_value_t**)(*pa)->data;
    jl_array_t *n = jl_alloc_cell_1d(len*2);
    jl_value_t **nd = (jl_value_t**)n->data;
    size_t i;
    for(i=0; i < len; i++) {
        jl_methlist_t *ml = (jl_methlist_t*)d[i];
        if (ml && ml!=(jl_methlist_t*)jl_nothing) {
            jl_value_t *t = jl_field_type(ml->sig,0);
            if (jl_is_type_type(t))
                t = jl_tparam0(t);
            uptrint_t uid = ((jl_datatype_t*)t)->uid;
            nd[uid & (len*2-1)] = (jl_value_t*)ml;
        }
    }
    jl_gc_wb(parent, n);
    *pa = n;
}

static jl_methlist_t **mtcache_hash_bp(jl_array_t **pa, jl_value_t *ty,
                                       int tparam, jl_value_t* parent)
{
    uptrint_t uid;
    if (jl_is_datatype(ty) && (uid = ((jl_datatype_t*)ty)->uid)) {
        while (1) {
            jl_methlist_t **pml = &((jl_methlist_t**)jl_array_data(*pa))[uid & ((*pa)->nrows-1)];
            if (*pml == NULL || *pml == (jl_methlist_t*)jl_nothing) {
                *pml = (jl_methlist_t*)jl_nothing;
                return pml;
            }
            jl_value_t *t = jl_field_type((*pml)->sig,0);
            if (tparam) t = jl_tparam0(t);
            if (t == ty)
                return pml;
            mtcache_rehash(pa, parent);
        }
    }
    return NULL;
}

/*
  Method caches are divided into three parts: one for signatures where
  the first argument is a singleton kind (Type{Foo}), one indexed by the
  UID of the first argument's type in normal cases, and a fallback
  table of everything else.
*/
static jl_function_t *jl_method_table_assoc_exact_by_type(jl_methtable_t *mt, jl_tupletype_t *types)
{
    jl_methlist_t *ml = (jl_methlist_t*)jl_nothing;
    if (jl_datatype_nfields(types) > 0) {
        jl_value_t *ty = jl_tparam0(types);
        if (jl_is_type_type(ty)) {
            jl_value_t *a0 = jl_tparam0(ty);
            if (mt->cache_targ != (void*)jl_nothing && jl_is_datatype(a0)) {
                ml = mtcache_hash_lookup(mt->cache_targ, a0, 1);
                if (ml!=(jl_methlist_t*)jl_nothing)
                    goto mt_assoc_bt_lkup;
            }
        }
        if (mt->cache_arg1 != (void*)jl_nothing && jl_is_datatype(ty)) {
            ml = mtcache_hash_lookup(mt->cache_arg1, ty, 0);
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
        if (ml->func == jl_bottom_func && jl_subtype((jl_value_t*)types, (jl_value_t*)ml->sig, 0))
            return jl_bottom_func;
        ml = ml->next;
    }
    return jl_bottom_func;
}

static jl_function_t *jl_method_table_assoc_exact(jl_methtable_t *mt, jl_value_t **args, size_t n)
{
    // NOTE: This function is a huge performance hot spot!!
    jl_methlist_t *ml = (jl_methlist_t*)jl_nothing;
    if (n > 0) {
        jl_value_t *a0 = args[0];
        jl_value_t *ty = (jl_value_t*)jl_typeof(a0);
        if (mt->cache_targ != (void*)jl_nothing && ty == (jl_value_t*)jl_datatype_type) {
            ml = mtcache_hash_lookup(mt->cache_targ, a0, 1);
            if (ml != (void*)jl_nothing)
                goto mt_assoc_lkup;
        }
        assert(jl_is_datatype(ty));
        if (mt->cache_arg1 != (void*)jl_nothing) {
            ml = mtcache_hash_lookup(mt->cache_arg1, ty, 0);
            if (ml != (void*)jl_nothing) {
                if (ml->next==(void*)jl_nothing && n==1 && jl_datatype_nfields(ml->sig)==1)
                    return ml->func;
                if (n==2) {
                    // some manually-unrolled common special cases
                    jl_value_t *a1 = args[1];
                    if (!jl_is_tuple(a1)) {  // issue #6426
                        jl_methlist_t *mn = ml;
                        if (jl_datatype_nfields(mn->sig)==2 &&
                            jl_tparam(mn->sig,1)==(jl_value_t*)jl_typeof(a1))
                            return mn->func;
                        mn = mn->next;
                        if (mn!=(void*)jl_nothing && jl_datatype_nfields(mn->sig)==2 &&
                            jl_tparam(mn->sig,1)==(jl_value_t*)jl_typeof(a1))
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
            if (cache_match(args, n, ml->sig, ml->va, lensig)) {
                return ml->func;
            }
            // if we hit a guard entry (ml->func == jl_bottom_func), do a more
            // expensive subtype check, since guard entries added for ANY might be
            // abstract. this fixed issue #12967.
            if (ml->func == jl_bottom_func && jl_tuple_subtype(args, n, ml->sig, 1))
                return jl_bottom_func;
        }
        ml = ml->next;
    }
    return jl_bottom_func;
}

// return a new lambda-info that has some extra static parameters merged in.
jl_lambda_info_t *jl_add_static_parameters(jl_lambda_info_t *l, jl_svec_t *sp, jl_tupletype_t *types)
{
    JL_GC_PUSH1(&sp);
    if (jl_svec_len(l->sparams) > 0)
        sp = jl_svec_append(sp, l->sparams);
    jl_lambda_info_t *nli = jl_copy_lambda_info(l);
    nli->sparams = sp; // no gc_wb needed
    nli->tfunc = jl_nothing;
    nli->capt = NULL;
    nli->specializations = NULL;
    nli->unspecialized = NULL;
    nli->specTypes = types;
    if (types) jl_gc_wb(nli, types);
    if (jl_options.compile_enabled != JL_OPTIONS_COMPILE_OFF) {
        // make sure this marked as needing to be (re)compiled
        // since the sparams might be providing better type information
        // this might happen if an inner lambda was compiled as part
        // of running an unspecialized function
        nli->fptr = jl_trampoline;
        nli->functionObject = NULL;
        nli->specFunctionObject = NULL;
        nli->functionID = 0;
        nli->specFunctionID = 0;
    }
    else {
        if (nli->fptr == jl_trampoline) {
            jl_printf(JL_STDERR,"code missing for ");
            jl_static_show(JL_STDERR, (jl_value_t*)nli);
            jl_printf(JL_STDERR, "  sysimg may not have been built with --compile=all\n");
        }
    }
    JL_GC_POP();
    return nli;
}

static jl_function_t *jl_instantiate_method(jl_function_t *f, jl_svec_t *sp, jl_tupletype_t *types)
{
    if (f->linfo == NULL)
        return f;
    jl_function_t *nf = jl_new_closure(f->fptr, f->env, NULL);
    JL_GC_PUSH1(&nf);
    nf->linfo = jl_add_static_parameters(f->linfo, sp, types);
    jl_gc_wb(nf, nf->linfo);
    JL_GC_POP();
    return nf;
}

// append values of static parameters to closure environment
static jl_function_t *with_appended_env(jl_function_t *meth, jl_svec_t *sparams)
{
    if (sparams == jl_emptysvec)
        return meth;
    jl_value_t *temp = (jl_value_t*)jl_alloc_svec(jl_svec_len(sparams)/2);
    JL_GC_PUSH1(&temp);
    size_t i;
    for(i=0; i < jl_svec_len(temp); i++) {
        jl_svecset(temp, i, jl_svecref(sparams,i*2+1));
    }
    temp = (jl_value_t*)jl_svec_append((jl_svec_t*)meth->env, (jl_svec_t*)temp);
    meth = jl_new_closure(meth->fptr, temp, meth->linfo);
    JL_GC_POP();
    return meth;
}

// make a new method that calls the generated code from the given linfo
jl_function_t *jl_reinstantiate_method(jl_function_t *f, jl_lambda_info_t *li)
{
    return jl_new_closure(NULL, f->env, li);
}

static
jl_methlist_t *jl_method_list_insert(jl_methlist_t **pml, jl_tupletype_t *type,
                                     jl_function_t *method, jl_svec_t *tvars,
                                     int check_amb, int8_t isstaged, jl_value_t *parent);

jl_function_t *jl_method_cache_insert(jl_methtable_t *mt, jl_tupletype_t *type,
                                      jl_function_t *method)
{
    jl_methlist_t **pml = &mt->cache;
    jl_value_t* cache_array = NULL;
    if (jl_datatype_nfields(type) > 0) {
        jl_value_t *t0 = jl_tparam0(type);
        uptrint_t uid=0;
        // if t0 != jl_typetype_type and the argument is Type{...}, this
        // method has specializations for singleton kinds and we use
        // the table indexed for that purpose.
        if (t0 != (jl_value_t*)jl_typetype_type && jl_is_type_type(t0)) {
            jl_value_t *a0 = jl_tparam0(t0);
            if (jl_is_datatype(a0))
                uid = ((jl_datatype_t*)a0)->uid;
            if (uid > 0) {
                if (mt->cache_targ == (void*)jl_nothing) {
                    mt->cache_targ = jl_alloc_cell_1d(16);
                    jl_gc_wb(mt, mt->cache_targ);
                }
                pml = mtcache_hash_bp(&mt->cache_targ, a0, 1, (jl_value_t*)mt);
                cache_array = (jl_value_t*)mt->cache_targ;
                goto ml_do_insert;
            }
        }
        if (jl_is_datatype(t0))
            uid = ((jl_datatype_t*)t0)->uid;
        if (uid > 0) {
            if (mt->cache_arg1 == (void*)jl_nothing) {
                mt->cache_arg1 = jl_alloc_cell_1d(16);
                jl_gc_wb(mt, mt->cache_arg1);
            }
            pml = mtcache_hash_bp(&mt->cache_arg1, t0, 0, (jl_value_t*)mt);
            cache_array = (jl_value_t*)mt->cache_arg1;
        }
    }
 ml_do_insert:
    return jl_method_list_insert(pml, type, method, jl_emptysvec, 0, 0, cache_array ? cache_array : (jl_value_t*)mt)->func;
}

/*
  run type inference on lambda "li" in-place, for given argument types.
  "def" is the original method definition of which this is an instance;
  can be equal to "li" if not applicable.
*/
int jl_in_inference = 0;
void jl_type_infer(jl_lambda_info_t *li, jl_tupletype_t *argtypes, jl_lambda_info_t *def)
{
    JL_LOCK(codegen);
    int last_ii = jl_in_inference;
    jl_in_inference = 1;
    if (jl_typeinf_func != NULL) {
        // TODO: this should be done right before code gen, so if it is
        // interrupted we can try again the next time the function is
        // called
        assert(li->inInference == 0);
        li->inInference = 1;
        jl_value_t *fargs[4];
        fargs[0] = (jl_value_t*)li;
        fargs[1] = (jl_value_t*)argtypes;
        fargs[2] = (jl_value_t*)jl_emptysvec;
        fargs[3] = (jl_value_t*)def;
#ifdef TRACE_INFERENCE
        jl_printf(JL_STDERR,"inference on %s", jl_symbol_name(li->name));
        jl_static_show_func_sig(JL_STDERR, (jl_value_t*)argtypes);
        jl_printf(JL_STDERR, "\n");
#endif
#ifdef ENABLE_INFERENCE
        jl_value_t *newast = jl_apply(jl_typeinf_func, fargs, 4);
        li->ast = jl_fieldref(newast, 0);
        jl_gc_wb(li, li->ast);
        li->inferred = 1;
#endif
        li->inInference = 0;
    }
    jl_in_inference = last_ii;
    JL_UNLOCK(codegen);
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

static jl_function_t *cache_method(jl_methtable_t *mt, jl_tupletype_t *type,
                                   jl_function_t *method, jl_tupletype_t *decl,
                                   jl_svec_t *sparams, int8_t isstaged)
{
    JL_LOCK(codegen);
    size_t i;
    int need_guard_entries = 0;
    jl_value_t *temp=NULL;
    jl_value_t *temp2=NULL;
    jl_function_t *newmeth=NULL;
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
        if (decl_i == jl_ANY_flag) {
            // don't specialize on slots marked ANY
            jl_svecset(newparams, i, (jl_value_t*)jl_any_type);
            temp2 = (jl_value_t*)jl_svec_copy(newparams);
            temp2 = (jl_value_t*)jl_apply_tuple_type((jl_svec_t*)temp2);
            int nintr=0;
            jl_methlist_t *curr = mt->defs;
            // if this method is the only match even with the current slot
            // set to Any, then it is safe to cache it that way.
            while (curr != (void*)jl_nothing && curr->func!=method) {
                if (jl_type_intersection((jl_value_t*)curr->sig,
                                         (jl_value_t*)temp2) !=
                    (jl_value_t*)jl_bottom_type) {
                    nintr++;
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
                if (nintr > 0)
                    need_guard_entries = 1;
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
            if (jl_svec_len(sparams) > 0) {
                lastdeclt = (jl_value_t*)
                    jl_instantiate_type_with((jl_value_t*)lastdeclt,
                                             jl_svec_data(sparams),
                                             jl_svec_len(sparams)/2);
            }
            jl_svecset(limited, i, lastdeclt);
        }
        type = jl_apply_tuple_type(limited);
        temp2 = (jl_value_t*)type;
        // now there is a problem: the computed signature is more
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
            for(int k=1; k < jl_svec_len(env); k+=2) {
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
                    jl_method_cache_insert(mt, (jl_tupletype_t*)jl_svecref(m, 0), jl_bottom_func);
                }
            }
        }
    }

    // here we infer types and specialize the method
    jl_array_t *lilist=NULL;
    jl_lambda_info_t *li=NULL;
    if (method->linfo && method->linfo->specializations!=NULL) {
        // reuse code already generated for this combination of lambda and
        // arguments types. this happens for inner generic functions where
        // a new closure is generated on each call to the enclosing function.
        lilist = method->linfo->specializations;
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
        newmeth = jl_reinstantiate_method(method, li);
        (void)jl_method_cache_insert(mt, type, newmeth);
        JL_GC_POP();
        JL_UNLOCK(codegen);
        return newmeth;
    }
    else {
        if (jl_options.compile_enabled == JL_OPTIONS_COMPILE_OFF &&
                method->linfo->unspecialized != NULL) {
            jl_function_t *unspec = method->linfo->unspecialized;
            if (method->env == (jl_value_t*)jl_emptysvec)
                newmeth = unspec;
            else
                newmeth = jl_new_closure(unspec->fptr, method->env, unspec->linfo);

            if (sparams != jl_emptysvec)
                newmeth = with_appended_env(newmeth, sparams);

            (void)jl_method_cache_insert(mt, type, newmeth);
            JL_GC_POP();
            return newmeth;
        }
        newmeth = jl_instantiate_method(method, sparams, type);
    }

    /* "method" itself should never get compiled,
      for example, if an unspecialized method is needed,
      the slow compiled code should be associated with
      method->linfo->unspecialized, not method */
    assert(!(newmeth->linfo && newmeth->linfo->ast) ||
           (newmeth->linfo->specTypes == method->linfo->specTypes) ||
           (newmeth->fptr == &jl_trampoline &&
            newmeth->linfo->fptr == &jl_trampoline &&
            newmeth->linfo->functionObject == NULL &&
            newmeth->linfo->specFunctionObject == NULL &&
            newmeth->linfo->functionID == 0 &&
            newmeth->linfo->specFunctionID == 0));

    (void)jl_method_cache_insert(mt, type, newmeth);

    if (newmeth->linfo != NULL && newmeth->linfo->sparams == jl_emptysvec) {
        // when there are no static parameters, one unspecialized version
        // of a function can be shared among all cached specializations.
        if (method->linfo->unspecialized == NULL) {
            method->linfo->unspecialized =
                jl_instantiate_method(method, jl_emptysvec, decl);
            if (method->env != (jl_value_t*)jl_emptysvec)
                method->linfo->unspecialized->env = NULL;
            jl_gc_wb(method->linfo, method->linfo->unspecialized);
        }
        newmeth->linfo->unspecialized = method->linfo->unspecialized;
        jl_gc_wb(newmeth->linfo, newmeth->linfo->unspecialized);
    }

    if (newmeth->linfo != NULL && newmeth->linfo->ast != NULL) {
        jl_array_t *spe = method->linfo->specializations;
        if (spe == NULL) {
            spe = jl_alloc_cell_1d(1);
            jl_cellset(spe, 0, newmeth->linfo);
        }
        else {
            jl_cell_1d_push(spe, (jl_value_t*)newmeth->linfo);
        }
        method->linfo->specializations = spe;
        jl_gc_wb(method->linfo, method->linfo->specializations);
        jl_type_infer(newmeth->linfo, type, method->linfo);
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
    jl_value_t **ee = (jl_value_t**)alloca(sizeof(void*) * jl_svec_len(*penv));
    int n=0;
    // only keep vars in tvars list
    jl_value_t **tvs;
    int tvarslen;
    if (jl_is_typevar(tvars)) {
        tvs = (jl_value_t**)&tvars;
        tvarslen = 1;
    }
    else {
        tvs = jl_svec_data(tvars);
        tvarslen = jl_svec_len(tvars);
    }
    int l = jl_svec_len(*penv);
    for(int i=0; i < l; i+=2) {
        jl_value_t *v = jl_svecref(*penv,i);
        jl_value_t *val = jl_svecref(*penv,i+1);
        for(int j=0; j < tvarslen; j++) {
            if (v == tvs[j]) {
                ee[n++] = v;
                ee[n++] = val;
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
        }
    }
    if (n != l) {
        jl_svec_t *en = jl_alloc_svec_uninit(n);
        memcpy(jl_svec_data(en), ee, n*sizeof(void*));
        *penv = en;
    }
    JL_GC_POP();
    return ti;
}

JL_DLLEXPORT jl_function_t *jl_instantiate_staged(jl_methlist_t *m,
                                                  jl_tupletype_t *tt,
                                                  jl_svec_t *env)
{
    jl_expr_t *ex = NULL;
    jl_expr_t *oldast = NULL;
    jl_function_t *func = NULL;
    jl_value_t *linenum = NULL;
    JL_GC_PUSH4(&ex, &oldast, &func, &linenum);
    if (jl_is_expr(m->func->linfo->ast))
        oldast = (jl_expr_t*)m->func->linfo->ast;
    else
        oldast = (jl_expr_t*)jl_uncompress_ast(m->func->linfo, m->func->linfo->ast);
    assert(oldast->head == lambda_sym);
    ex = jl_exprn(arrow_sym, 2);
    jl_array_t *oldargnames = jl_lam_args(oldast);
    jl_expr_t *argnames = jl_exprn(tuple_sym, jl_array_len(oldargnames));
    jl_cellset(ex->args, 0, argnames);
    for (size_t i = 0; i < jl_array_len(oldargnames); ++i) {
        jl_value_t *arg = jl_cellref(oldargnames,i);
        if (jl_is_expr(arg)) {
            assert(((jl_expr_t*)arg)->head == colons_sym);
            arg = jl_cellref(((jl_expr_t*)arg)->args,0);
            assert(jl_is_symbol(arg));
            jl_expr_t *dd_expr = jl_exprn(dots_sym,1);
            jl_cellset(dd_expr->args,0,arg);
            jl_cellset(argnames->args,i,dd_expr);
        }
        else {
            assert(jl_is_symbol(arg));
            jl_cellset(argnames->args,i,arg);
        }
    }
    func = with_appended_env(m->func, env); // fulfills the expectations of the all_p2c pass
    jl_expr_t *body = jl_exprn(jl_symbol("block"), 2);
    jl_cellset(ex->args, 1, body);
    linenum = jl_box_long(m->func->linfo->line);
    jl_value_t *linenode = jl_new_struct(jl_linenumbernode_type,
                                         m->func->linfo->file,
                                         linenum
                                         );
    jl_cellset(body->args, 0, linenode);
    jl_cellset(body->args, 1, // can call jl_apply here because this isn't a normal gf and has had all_p2c called on the ast earlier
            jl_apply(func, jl_svec_data(tt->parameters), jl_nparams(tt)));
    if (m->tvars != jl_emptysvec) {
        // mark this function as having the same static parameters as the generator
        size_t nsp = jl_is_typevar(m->tvars) ? 1 : jl_svec_len(m->tvars);
        oldast = jl_exprn(jl_symbol("with-static-parameters"), nsp+1);
        jl_exprarg(oldast,0) = (jl_value_t*)ex;
        // (with-static-parameters func_expr sp_1 sp_2 ...)
        if (jl_is_typevar(m->tvars)) {
            jl_exprarg(oldast,1) = (jl_value_t*)((jl_tvar_t*)m->tvars)->name;
        }
        else {
            for(size_t i=0; i < nsp; i++)
                jl_exprarg(oldast,i+1) = (jl_value_t*)((jl_tvar_t*)jl_svecref(m->tvars,i))->name;
        }
        ex = oldast;
    }
    func = (jl_function_t*)jl_toplevel_eval_in_warn(m->func->linfo->module, (jl_value_t*)ex, 1); // need to eval macros in the right module, but not give a warning for the `eval` call unless that results in a call to `eval`
    func->linfo->name = m->func->linfo->name;
    JL_GC_POP();
    return func;
}

static jl_function_t *jl_mt_assoc_by_type(jl_methtable_t *mt, jl_datatype_t *tt, int cache, int inexact)
{
    jl_methlist_t *m = mt->defs;
    size_t nargs = jl_nparams(tt);
    size_t i;
    jl_value_t *ti=(jl_value_t*)jl_bottom_type;
    jl_tupletype_t *newsig=NULL;
    jl_svec_t *env = jl_emptysvec;
    jl_function_t *func = NULL;
    JL_GC_PUSH3(&env, &newsig, &func);

    while (m != (void*)jl_nothing) {
        if (m->tvars!=jl_emptysvec) {
            ti = lookup_match((jl_value_t*)tt, (jl_value_t*)m->sig, &env, m->tvars);
            if (ti != (jl_value_t*)jl_bottom_type) {
                // parametric methods only match if all typevars are matched by
                // non-typevars.
                for(i=1; i < jl_svec_len(env); i+=2) {
                    if (jl_is_typevar(jl_svecref(env,i))) {
                        if (inexact) {
                            // "inexact" means the given type is compile-time,
                            // where a failure to determine the value of a
                            // static parameter is inconclusive.
                            // this is issue #3182, see test/core.jl
                            JL_GC_POP();
                            return jl_bottom_func;
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
                func = jl_instantiate_staged(m,tt,env);
            if (!cache) {
                JL_GC_POP();
                return func;
            }
            jl_function_t *res = cache_method(mt, tt, func, m->sig, jl_emptysvec, m->isstaged);
            JL_GC_POP();
            return res;
        }
        JL_GC_POP();
        return jl_bottom_func;
    }

    assert(jl_is_svec(env));
    func = m->func;

    if (m->isstaged)
        func = jl_instantiate_staged(m,tt,env);

    // don't bother computing this if no arguments are tuples
    for(i=0; i < jl_nparams(tt); i++) {
        if (jl_is_tuple_type(jl_tparam(tt,i)))
            break;
    }
    if (i < jl_nparams(tt)) {
        newsig = (jl_tupletype_t*)jl_instantiate_type_with((jl_value_t*)m->sig,
                                                           jl_svec_data(env),
                                                           jl_svec_len(env)/2);
    }
    else {
        newsig = m->sig;
    }
    assert(jl_is_tuple_type(newsig));
    jl_function_t *nf;
    if (!cache)
        nf = func;
    else
        nf = cache_method(mt, tt, func, newsig, env, m->isstaged);
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
        char *n;
        JL_STREAM *s;
        while (l != (void*)jl_nothing) {
            if (sigs_eq(isect, (jl_value_t*)l->sig, 0))
                goto done_chk_amb;  // ok, intersection is covered
            l = l->next;
        }
        n = jl_symbol_name(fname);
        s = JL_STDERR;
        jl_printf(s, "WARNING: New definition \n    %s", n);
        jl_static_show_func_sig(s, (jl_value_t*)type);
        print_func_loc(s, linfo);
        jl_printf(s, "\nis ambiguous with: \n    %s", n);
        jl_static_show_func_sig(s, (jl_value_t*)sig);
        print_func_loc(s, oldmeth->func->linfo);
        jl_printf(s, ".\nTo fix, define \n    %s", n);
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
                                     jl_function_t *method, jl_svec_t *tvars,
                                     int check_amb, int8_t isstaged, jl_value_t *parent)
{
    jl_methlist_t *l, **pl;

    assert(jl_is_tuple_type(type));
    l = *pml;
    while (l != (void*)jl_nothing) {
        if (((l->tvars==jl_emptysvec) == (tvars==jl_emptysvec)) &&
            sigs_eq((jl_value_t*)type, (jl_value_t*)l->sig, 1)) {
            // method overwritten
            if (check_amb && l->func->linfo && method->linfo &&
                (l->func->linfo->module != method->linfo->module)) {
                jl_module_t *newmod = method->linfo->module;
                JL_STREAM *s = JL_STDERR;
                jl_printf(s, "WARNING: Method definition %s",
                          jl_symbol_name(method->linfo->name));
                jl_static_show_func_sig(s, (jl_value_t*)type);
                jl_printf(s, " in module %s",
                          jl_symbol_name(l->func->linfo->module->name));
                print_func_loc(s, l->func->linfo);
                jl_printf(s, " overwritten in module %s",
                          jl_symbol_name(newmod->name));
                print_func_loc(s, method->linfo);
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
            l->func = method;
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
            check_ambiguous(*pml, type, l,
                            method->linfo ? method->linfo->name :
                            anonymous_sym, method->linfo);
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
        jl_value_t* item_parent = (jl_value_t*)newrec;
        jl_value_t* next_parent = 0;
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
                                      jl_function_t *method, jl_svec_t *tvars,
                                      int8_t isstaged)
{
    if (jl_svec_len(tvars) == 1)
        tvars = (jl_svec_t*)jl_svecref(tvars,0);
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
    jl_throw(jl_apply(jl_module_call_func(jl_base_module), fargs, 3));
    // not reached
}

void JL_NORETURN jl_no_method_error(jl_function_t *f, jl_value_t **args,
                                    size_t na)
{
    jl_value_t *argtup = jl_f_tuple(NULL, args, na);
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

jl_function_t *jl_method_lookup_by_type(jl_methtable_t *mt, jl_tupletype_t *types,
                                        int cache, int inexact)
{
    jl_function_t *sf = jl_method_table_assoc_exact_by_type(mt, types);
    if (sf == jl_bottom_func) {
        if (jl_is_leaf_type((jl_value_t*)types)) cache=1;
        sf = jl_mt_assoc_by_type(mt, types, cache, inexact);
    }
    return sf;
}

JL_DLLEXPORT int jl_method_exists(jl_methtable_t *mt, jl_tupletype_t *types)
{
    return jl_method_lookup_by_type(mt, types, 0, 0) != jl_bottom_func;
}

jl_function_t *jl_method_lookup(jl_methtable_t *mt, jl_value_t **args, size_t nargs, int cache)
{
    jl_function_t *sf = jl_method_table_assoc_exact(mt, args, nargs);
    if (sf == jl_bottom_func) {
        jl_tupletype_t *tt = arg_type_tuple(args, nargs);
        JL_GC_PUSH1(&tt);
        sf = jl_mt_assoc_by_type(mt, tt, cache, 0);
        JL_GC_POP();
    }
    return sf;
}

JL_DLLEXPORT jl_value_t *jl_matching_methods(jl_function_t *gf,
                                             jl_value_t *type, int lim);

// compile-time method lookup
jl_function_t *jl_get_specialization(jl_function_t *f, jl_tupletype_t *types)
{
    if (!jl_is_leaf_type((jl_value_t*)types))
        return NULL;
    assert(jl_is_gf(f));

    // make sure exactly 1 method matches (issue #7302).
    int i;
    for(i=0; i < jl_nparams(types); i++) {
        jl_value_t *ti = jl_tparam(types, i);
        // if one argument type is DataType, multiple Type{} definitions
        // might match. also be conservative with tuples rather than trying
        // to analyze them in detail.
        if (ti == (jl_value_t*)jl_datatype_type || jl_is_tuple_type(ti)) {
            jl_value_t *matches = jl_matching_methods(f, (jl_value_t*)types, 1);
            if (matches == jl_false)
                return NULL;
            break;
        }
    }

    jl_methtable_t *mt = jl_gf_mtable(f);
    jl_function_t *sf = NULL;
    // most of the time sf is rooted in mt, but if the method is staged it may
    // not be the case
    JL_GC_PUSH1(&sf);
    JL_TRY {
        sf = jl_method_lookup_by_type(mt, types, 1, 1);
    } JL_CATCH {
        goto not_found;
    }
    if (sf == jl_bottom_func) {
        goto not_found;
    }
    if (sf->linfo == NULL || sf->linfo->ast == NULL) {
        goto not_found;
    }
    if (sf->linfo->inInference) goto not_found;
    if (sf->linfo->functionObject == NULL) {
        if (sf->fptr != &jl_trampoline)
            goto not_found;
        jl_compile_linfo(sf->linfo);
    }
    JL_GC_POP();
    return sf;
 not_found:
    JL_GC_POP();
    return NULL;
}

void jl_trampoline_compile_linfo(jl_lambda_info_t *linfo, int always_infer);

static void parameters_to_closureenv(jl_value_t *ast, jl_svec_t *tvars)
{
    jl_array_t *closed = jl_lam_capt((jl_expr_t*)ast);
    jl_array_t *splist = jl_lam_staticparams((jl_expr_t*)ast);
    jl_value_t **tvs;
    int tvarslen;
    if (jl_is_typevar(tvars)) {
        tvs = (jl_value_t**)&tvars;
        tvarslen = 1;
    }
    else {
        tvs = jl_svec_data(tvars);
        tvarslen = jl_svec_len(tvars);
    }

    size_t i, j;
    jl_array_t *vi=NULL;
    JL_GC_PUSH1(&vi);
    for(i=0; i < tvarslen; i++) {
        jl_tvar_t *tv = (jl_tvar_t*)tvs[i];
        jl_sym_t *sp = tv->name;
        int found = 0;
        // add this item to closed
        assert(!jl_in_vinfo_array(closed, sp));
        vi = jl_alloc_cell_1d(3);
        jl_cellset(vi, 0, sp);
        jl_cellset(vi, 1, (tv->ub == (jl_value_t*)jl_any_type ? jl_any_type :
                           jl_wrap_Type((jl_value_t*)tv)));
        jl_cellset(vi, 2, jl_box_long(1));
        jl_cell_1d_push(closed, (jl_value_t*)vi);
        // delete this item from the list of static parameters
        for (j = 0; j < jl_array_len(splist); j++) {
            if (jl_cellref(splist, j) == (jl_value_t*)sp) {
                jl_cellset(splist, j, jl_cellref(splist, jl_array_len(splist) - 1));
                jl_array_del_end(splist, 1);
                found = 1;
                break;
            }
        }
        assert(found); (void)found;
    }

    JL_GC_POP();
}

// modifies ast to replace any lambda info with an unspecialized version
// that removes tvars from the sparams. instead adds these static parameter
// names to end of closure env; the compile assumes they are there.
// the method cache will fill them in when it constructs closures
// for new "specializations".
static jl_value_t *all_p2c(jl_value_t *ast, jl_svec_t *tvars)
{
    if (tvars == jl_emptysvec)
        return ast;
    if (jl_is_lambda_info(ast)) {
        jl_lambda_info_t *li = (jl_lambda_info_t*)ast;
        // use linfo->unspecialized to store the modified ast
        jl_function_t *unspec = li->unspecialized;
        if (unspec)
            return (jl_value_t*)unspec->linfo;
        unspec = jl_new_closure(NULL, NULL, li);
        JL_GC_PUSH1(&unspec);
        unspec->linfo = jl_add_static_parameters(li, jl_emptysvec, NULL); // copy linfo
        jl_gc_wb(unspec, unspec->linfo);
        unspec->linfo->ast = jl_prepare_ast(unspec->linfo, jl_emptysvec); // copy ast
        jl_gc_wb(unspec->linfo, unspec->linfo->ast);
        parameters_to_closureenv(unspec->linfo->ast, tvars); // move sparams to closure env
        unspec->linfo->ast = all_p2c(unspec->linfo->ast, tvars);
        jl_gc_wb(unspec->linfo, unspec->linfo->ast);
        if (jl_array_len(jl_lam_staticparams((jl_expr_t*)unspec->linfo->ast)) == 0)
            // mark this as compilable; otherwise, would need to make an unspecialized version of
            // this unspecialized function to handle all of the static parameters
            unspec->linfo->specTypes = jl_anytuple_type; // no gc_wb needed
        li->unspecialized = unspec; // record result for reuse
        jl_gc_wb(li, unspec);
        JL_GC_POP();
        return (jl_value_t*)unspec->linfo;
    }
    else if (jl_is_expr(ast)) {
        jl_expr_t *e = (jl_expr_t*)ast;
        jl_array_t *a = e->args;
        for(size_t i=0; i < jl_array_len(a); i++)
            jl_cellset(a, i, all_p2c(jl_cellref(a, i), tvars));
    }
    return ast;
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

static void dbg_print_progress(uv_stream_t *s, size_t i, size_t l)
{
    char meter[81] = "###############################################################################\r";
    int z, pct = 80 * i / l;
    if (pct > 79) pct = 79;
    for (z = 0; z < pct; z++)
        meter[z] = '#';
    for (; z < 79; z++)
        meter[z] = '-';
    jl_printf(s, "%s", meter);
}

static void _compile_all_deq(jl_array_t *found)
{
    size_t found_i, found_l = jl_array_len(found);
    jl_printf(JL_STDERR, "found %d uncompiled methods for compile-all\n", (int)(found_l / 2));
    for (found_i = 0; found_i < found_l; found_i += 2) {
        dbg_print_progress(JL_STDERR, found_i + 2, found_l);
        jl_value_t *thunk = jl_cellref(found, found_i);

        if (jl_is_lambda_info(thunk)) {
            // anonymous or specialized function
            jl_lambda_info_t *li = (jl_lambda_info_t*)thunk;
            assert(!jl_cellref(found, found_i + 1));
            jl_trampoline_compile_linfo(li, 1);
            assert(li->functionID > 0);
            continue;
        }

        jl_function_t *func = (jl_function_t*)thunk;
        jl_methlist_t *meth = (jl_methlist_t*)jl_cellref(found, found_i + 1);
        assert(!func->linfo);

        if (jl_is_leaf_type((jl_value_t*)meth->sig)) {
            // usually can create a specialized version of the function,
            // if the signature is already a leaftype
            jl_function_t *spec = jl_get_specialization(func, meth->sig);
            if (spec && !jl_has_typevars((jl_value_t*)meth->sig)) {
                // replace unspecialized func with specialized version
                // if there are no bound type vars (e.g. `call{K,V}(Dict{K,V})` vs `call(Dict)`)
                // that might cause a different method to match at runtime
                meth->func = spec;
                jl_gc_wb(meth, spec);
                continue;
            }
        }

        if (jl_is_typevar(meth->tvars)) {
            // f{T<:Union{...}}(...) is a common pattern
            // and expanding the Union may give a leaf function
            jl_tvar_t *tv = (jl_tvar_t*)meth->tvars;
            if (jl_is_uniontype(tv->ub)) {
                int complete = 1; // keep track of whether all possible signatures have been cached (and thus whether it can skip trying to compile the unspecialized function)
                // TODO: remove the "complete" check once runtime-intrinsics are fully active
                jl_uniontype_t *ub = (jl_uniontype_t*)tv->ub;
                size_t i, l = jl_svec_len(ub->types);
                for (i = 0; i < l + 1; i++) { // add Union{} to the end of the list, since T<:Union{} is always a valid option
                    jl_value_t *ty = (i == l ? jl_bottom_type : jl_svecref(ub->types, i));
                    if (i == l || jl_is_leaf_type(ty)) {
                        jl_value_t *env[2] = {(jl_value_t*)tv, ty};
                        jl_value_t *sig;
                        JL_TRY {
                            sig = (jl_value_t*)
                                jl_instantiate_type_with((jl_value_t*)meth->sig, env, 1);
                        }
                        JL_CATCH {
                            continue; // sigh, we found an invalid type signature. should we warn the user?
                        }
                        assert(jl_is_tuple_type(sig));
                        if (sig == jl_bottom_type || tupletype_any_bottom(sig)) {
                            continue; // signature wouldn't be callable / is invalid -- skip it
                        }
                        if (jl_is_leaf_type(sig)) {
                            if (jl_get_specialization(func, (jl_tupletype_t*)sig)) {
                                if (!jl_has_typevars((jl_value_t*)sig)) continue;
                            }
                        }
                    }
                    complete = 0;
                }
                if (complete) {
                    meth->func->linfo->functionID = -1; // indicate that this method doesn't need a functionID
                    continue;
                }
            }
        }

        jl_function_t *unspec = meth->func->linfo->unspecialized;
        if (unspec == NULL) {
            unspec = jl_instantiate_method(meth->func, jl_emptysvec, meth->sig);
            if (unspec->env != (jl_value_t*)jl_emptysvec)
                unspec->env = NULL;
            meth->func->linfo->unspecialized = unspec;
            jl_gc_wb(meth->func->linfo, unspec);
            unspec->linfo = (jl_lambda_info_t*)all_p2c((jl_value_t*)unspec->linfo, meth->tvars);
            jl_gc_wb(unspec, unspec->linfo);
        }
        jl_trampoline_compile_linfo(unspec->linfo, 1);
        assert(unspec->linfo->functionID > 0);
        meth->func->linfo->functionID = -1; // indicate that this method doesn't need a functionID
    }
    jl_printf(JL_STDERR, "\n");
}

static void _compile_all_enq(jl_value_t *v, htable_t *h, jl_array_t *found, jl_function_t *in_gf)
{
    // scan through all content reachable from 'v' and record all jl_function_t objects found
    // with awareness of whether the reachability path indicates that the object is  gf
    if (v == NULL) return;
    if (!ptrhash_has(h, v)) {
        ptrhash_put(h, v, v);
        if (jl_is_gf(v)) {
            jl_function_t *gf = (jl_function_t*)v;
            _compile_all_enq(gf->env, h, found, gf);
            // fast return and skip linfo, it is supposed to be null here
            return;
        }
        else if (jl_is_function(v)) {
            jl_function_t *f = (jl_function_t*)v;
            _compile_all_enq(f->env, h, found, 0);
            _compile_all_enq((jl_value_t*)f->linfo, h, found, in_gf);
            return;
        }
        else if (jl_is_mtable(v)) {
            if (!in_gf) {
                // found an methtable not in a gf, skip it for now -- hopefully we see this again in the gf
                ptrhash_remove(h, v);
                return;
            }
            jl_methtable_t *mt = (jl_methtable_t*)v;
            _compile_all_enq((jl_value_t*)mt->defs, h, found, in_gf);
            _compile_all_enq((jl_value_t*)mt->cache, h, found, in_gf);
            _compile_all_enq((jl_value_t*)mt->cache_arg1, h, found, in_gf);
            _compile_all_enq((jl_value_t*)mt->cache_targ, h, found, in_gf);
        }
        else if (jl_typeis(v, jl_method_type)) {
            if (!in_gf) {
                // found an methtable not in a gf, skip it for now -- hopefully we see this again in the gf
                ptrhash_remove(h, v);
                return;
            }
            jl_methlist_t *meth = (jl_methlist_t*)v;
            int use_mtable = (meth->func->linfo && !meth->func->linfo->specTypes) && !(meth->isstaged);
            _compile_all_enq((jl_value_t*)meth->invokes, h, found, in_gf);
            _compile_all_enq((jl_value_t*)meth->next, h, found, in_gf);
            _compile_all_enq((jl_value_t*)meth->func, h, found, in_gf);
            if (use_mtable && meth->func->fptr == jl_trampoline && !meth->func->linfo->functionID) {
                jl_cell_1d_push(found, (jl_value_t*)in_gf);
                jl_cell_1d_push(found, (jl_value_t*)meth);
            }
        }
        else if (jl_is_lambda_info(v)) {
            jl_lambda_info_t *li = (jl_lambda_info_t*)v;
            // if in_gf and !specTypes, the lambda will be compiled from the method table
            if (!in_gf && !li->specTypes && li->unspecialized) {
                // naked lambda from a gf: compile via unspecialized
                li = li->unspecialized->linfo;
            }
            if (li->specTypes && li->fptr == jl_trampoline && !li->functionID) {
                jl_cell_1d_push(found, (jl_value_t*)li);
                jl_cell_1d_push(found, (jl_value_t*)NULL);
            }
        }
        else if (jl_is_array(v)) {
            jl_array_t *a = (jl_array_t*)v;
            if (a->ptrarray) {
                size_t i, l = jl_array_len(a);
                for (i = 0; i < l; i++) {
                    _compile_all_enq(jl_cellref(a, i), h, found, in_gf);
                }
            }
        }
        else if (jl_is_module(v)) {
            jl_module_t *m = (jl_module_t*)v;
            size_t i;
            size_t sz = m->bindings.size;
            for(i=1; i < sz; i+=2) {
                if (m->bindings.table[i] != HT_NOTFOUND) {
                    jl_binding_t *b = (jl_binding_t*)m->bindings.table[i];
                    jl_value_t *v = b->value;
                    if (v != NULL) {
                        _compile_all_enq(v, h, found, 0);
                    }
                }
            }
            if (m->constant_table != NULL) {
                for(i=0; i < jl_array_len(m->constant_table); i++) {
                    jl_value_t *v = jl_cellref(m->constant_table, i);
                    _compile_all_enq(v, h, found, 0);
                }
            }
        }
        jl_datatype_t *dt = (jl_datatype_t*)jl_typeof(v);
        size_t i, nf = jl_datatype_nfields(dt);
        for (i = 0; i < nf; i++) {
            if (jl_field_isptr(dt, i)) {
                jl_value_t **slot = (jl_value_t**)
                    ((char*)v + jl_field_offset(dt, i));
                jl_value_t *fld = *slot;
                _compile_all_enq(fld, h, found, 0);
            }
        }
    }
}

void jl_compile_all(void)
{
    htable_t h;
    htable_new(&h, 0);
    // this "found" array will contain
    // LambdaStaticDatas that need to be compiled
    // and (generic-function, method) pairs that may be optimized (and need to be compiled)
    jl_array_t *m = jl_alloc_cell_1d(0);
    JL_GC_PUSH1(&m);
    while (1) {
        _compile_all_enq((jl_value_t*)jl_bottom_func, &h, m, 0); // not a gf, but appears in the mtable
        _compile_all_enq((jl_value_t*)jl_main_module, &h, m, 0);
        size_t changes = jl_array_len(m);
        if (!changes)
            break;
        _compile_all_deq(m);
        htable_reset(&h, h.size);
        jl_array_del_end(m, changes);
    }
    JL_GC_POP();
    htable_free(&h);
}

JL_DLLEXPORT void jl_compile_hint(jl_function_t *f, jl_tupletype_t *types)
{
    (void)jl_get_specialization(f, types);
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


JL_CALLABLE(jl_apply_generic)
{
    assert(jl_is_gf(F));
    jl_methtable_t *mt = jl_gf_mtable(F);
#ifdef JL_GF_PROFILE
    mt->ncalls++;
#endif
#ifdef JL_TRACE
    int traceen = trace_en; //&& ((char*)&mt < jl_stack_hi-6000000);
    if (traceen)
        show_call(F, args, nargs);
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
    jl_function_t *mfunc = jl_method_table_assoc_exact(mt, args, nargs);

    if (mfunc != jl_bottom_func) {
#ifdef JL_TRACE
        if (traceen)
            jl_printf(JL_STDOUT, " at %s:%d\n", jl_symbol_name(mfunc->linfo->file), mfunc->linfo->line);
#endif
        if (mfunc->linfo != NULL &&
            (mfunc->linfo->inInference || mfunc->linfo->inCompile)) {
            // if inference is running on this function, return a copy
            // of the function to be compiled without inference and run.
            jl_lambda_info_t *li = mfunc->linfo;
            if (li->unspecialized == NULL) {
                li->unspecialized = jl_instantiate_method(mfunc, li->sparams, jl_anytuple_type);
                if (mfunc->env != (jl_value_t*)jl_emptysvec)
                    li->unspecialized->env = NULL;
                jl_gc_wb(li, li->unspecialized);
            }
            return jl_apply_unspecialized(mfunc, args, nargs);
        }
        assert(!mfunc->linfo || !mfunc->linfo->inInference);
        return jl_apply(mfunc, args, nargs);
    }

    // cache miss case
    jl_tupletype_t *tt = arg_type_tuple(args, nargs);
    // if running inference overwrites this particular method, it becomes
    // unreachable from the method table, so root mfunc.
    JL_GC_PUSH2(&tt, &mfunc);
    mfunc = jl_mt_assoc_by_type(mt, tt, 1, 0);

    if (mfunc == jl_bottom_func) {
#ifdef JL_TRACE
        if (error_en)
            show_call(F, args, nargs);
#endif
        JL_GC_POP();
        jl_no_method_error((jl_function_t*)F, args, nargs);
        // unreachable
    }
#ifdef JL_TRACE
    if (traceen)
        jl_printf(JL_STDOUT, " at %s:%d\n", jl_symbol_name(mfunc->linfo->file), mfunc->linfo->line);
#endif
    assert(!mfunc->linfo || !mfunc->linfo->inInference);
    jl_value_t *res = jl_apply(mfunc, args, nargs);
    JL_GC_POP();
    return res;
}

JL_DLLEXPORT jl_value_t *jl_gf_invoke_lookup(jl_function_t *gf,
                                             jl_datatype_t *types)
{
    assert(jl_is_gf(gf));
    jl_methtable_t *mt = jl_gf_mtable(gf);
    jl_methlist_t *m = mt->defs;
    size_t typelen = jl_nparams(types);
    jl_value_t *env = (jl_value_t*)jl_false;

    while (m != (void*)jl_nothing) {
        if (m->tvars!=jl_emptysvec) {
            env = jl_type_match((jl_value_t*)types, (jl_value_t*)m->sig);
            if (env != (jl_value_t*)jl_false) break;
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
jl_value_t *jl_gf_invoke(jl_function_t *gf, jl_tupletype_t *types,
                         jl_value_t **args, size_t nargs)
{
    assert(jl_is_gf(gf));
    jl_methtable_t *mt = jl_gf_mtable(gf);
    jl_methlist_t *m = (jl_methlist_t*)jl_gf_invoke_lookup(gf, types);
    size_t i;

    if ((jl_value_t*)m == jl_nothing) {
        jl_no_method_error_bare(gf, (jl_value_t*)types);
        // unreachable
    }

    // now we have found the matching definition.
    // next look for or create a specialization of this definition.

    jl_function_t *mfunc;
    if (m->invokes == (void*)jl_nothing)
        mfunc = jl_bottom_func;
    else
        mfunc = jl_method_table_assoc_exact(m->invokes, args, nargs);
    if (mfunc != jl_bottom_func) {
        if (mfunc->linfo != NULL &&
            (mfunc->linfo->inInference || mfunc->linfo->inCompile)) {
            // if inference is running on this function, return a copy
            // of the function to be compiled without inference and run.
            jl_lambda_info_t *li = mfunc->linfo;
            if (li->unspecialized == NULL) {
                li->unspecialized = jl_instantiate_method(mfunc, li->sparams, jl_anytuple_type);
                if (mfunc->env != (jl_value_t*)jl_emptysvec)
                    li->unspecialized->env = NULL;
                jl_gc_wb(li, li->unspecialized);
            }
            return jl_apply_unspecialized(mfunc, args, nargs);
        }
    }
    else {
        jl_svec_t *tpenv=jl_emptysvec;
        jl_tupletype_t *newsig=NULL;
        jl_tupletype_t *tt=NULL;
        JL_GC_PUSH3(&tpenv, &newsig, &tt);
        tt = arg_type_tuple(args, nargs);
        if (m->invokes == (void*)jl_nothing) {
            m->invokes = new_method_table(mt->name, mt->module);
            jl_gc_wb(m, m->invokes);
            update_max_args(m->invokes, tt);
            // this private method table has just this one definition
            jl_method_list_insert(&m->invokes->defs,m->sig,m->func,m->tvars,0,0,(jl_value_t*)m->invokes);
        }

        newsig = m->sig;

        if (m->tvars != jl_emptysvec) {
            jl_value_t *ti =
                lookup_match((jl_value_t*)tt, (jl_value_t*)m->sig, &tpenv, m->tvars);
            assert(ti != (jl_value_t*)jl_bottom_type);
            (void)ti;
            // don't bother computing this if no arguments are tuples
            for(i=0; i < jl_nparams(tt); i++) {
                if (jl_is_tuple_type(jl_tparam(tt,i)))
                    break;
            }
            if (i < jl_nparams(tt)) {
                newsig = (jl_tupletype_t*)jl_instantiate_type_with((jl_value_t*)m->sig,
                                                                   jl_svec_data(tpenv),
                                                                   jl_svec_len(tpenv)/2);
            }
        }
        mfunc = cache_method(m->invokes, tt, m->func, newsig, tpenv, m->isstaged);
        JL_GC_POP();
    }

    return jl_apply(mfunc, args, nargs);
}

void print_func_loc(JL_STREAM *s, jl_lambda_info_t *li)
{
    long lno = li->line;
    if (lno > 0) {
        char *fname = jl_symbol_name((jl_sym_t*)li->file);
        jl_printf(s, " at %s:%ld", fname, lno);
    }
}

jl_function_t *jl_new_generic_function(jl_sym_t *name, jl_module_t *module)
{
    jl_function_t *f = jl_new_closure(jl_apply_generic, NULL, NULL);
    JL_GC_PUSH1(&f);
    f->fptr = jl_apply_generic;
    f->env = (jl_value_t*)new_method_table(name, module);
    jl_gc_wb(f, f->env);
    JL_GC_POP();
    return f;
}

JL_DLLEXPORT jl_function_t *jl_new_gf_internal(jl_value_t *env)
{
    return jl_new_closure(jl_apply_generic, env, NULL);
}

void jl_add_method(jl_function_t *gf, jl_tupletype_t *types, jl_function_t *meth,
                   jl_svec_t *tvars, int8_t isstaged)
{
    assert(jl_is_function(gf));
    assert(jl_is_tuple_type(types));
    assert(jl_is_func(meth));
    assert(jl_is_mtable(jl_gf_mtable(gf)));
    JL_GC_PUSH1(&meth);
    if (meth->linfo != NULL) {
        jl_sym_t *n = jl_gf_name(gf);
        if (meth->linfo->name != anonymous_sym && meth->linfo->name != n) {
            // already used by another GF; make a copy (issue #10373)
            meth = jl_instantiate_method(meth, jl_emptysvec, NULL);
        }
        meth->linfo->name = n;
    }
    if (isstaged && tvars != jl_emptysvec) {
        // copy meth before modifying meth->linfo
        meth = jl_new_closure(NULL, meth->env, meth->linfo);
        // copy linfo and move static parameters to closure where jl_instantiate_staged expects to put them
        meth->linfo = (jl_lambda_info_t*)all_p2c((jl_value_t*)meth->linfo, tvars);
        jl_gc_wb(meth, meth->linfo);
    }
    (void)jl_method_table_insert(jl_gf_mtable(gf), types, meth, tvars, isstaged);
    JL_GC_POP();
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
            assert(ml->func->linfo);  // no builtin methods
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
                for(i=1; i < l; i+=2) {
                    if (jl_is_typevar(jl_svecref(env,i)) &&
                        // if tvar is at the top level it will definitely be matched.
                        // see issue #5575
                        !tvar_exists_at_top_level(jl_svecref(env,i-1), ml->sig, 1)) {
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
// spvals is any matched static parameter values, li is the LambdaStaticData,
// and cenv is the closure environment or ().
//
// lim is the max # of methods to return. if there are more return jl_false.
// -1 for no limit.
JL_DLLEXPORT
jl_value_t *jl_matching_methods(jl_function_t *gf, jl_value_t *type, int lim)
{
    assert(jl_is_func(gf));
    if (!jl_is_gf(gf)) {
        return (jl_value_t*)jl_an_empty_cell;
    }
    jl_methtable_t *mt = jl_gf_mtable(gf);
    return ml_matches(mt->defs, type, jl_gf_name(gf), lim);
}

#ifdef __cplusplus
}
#endif

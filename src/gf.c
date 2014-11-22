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

#ifdef __cplusplus
extern "C" {
#endif

static jl_methtable_t *new_method_table(jl_sym_t *name)
{
    jl_methtable_t *mt = (jl_methtable_t*)allocobj(sizeof(jl_methtable_t));
    mt->type = (jl_value_t*)jl_methtable_type;
    mt->name = name;
    mt->defs = (jl_methlist_t*)JL_NULL;
    mt->cache = (jl_methlist_t*)JL_NULL;
    mt->cache_arg1 = (jl_array_t*)JL_NULL;
    mt->cache_targ = (jl_array_t*)JL_NULL;
    mt->max_args = 0;
    mt->kwsorter = NULL;
#ifdef JL_GF_PROFILE
    mt->ncalls = 0;
#endif
    return mt;
}

static int cache_match_by_type(jl_value_t **types, size_t n, jl_tuple_t *sig, int va)
{
    if (!va && n > jl_tuple_len(sig))
        return 0;
    if (jl_tuple_len(sig) > n) {
        if (!(n == jl_tuple_len(sig)-1 && va))
            return 0;
    }
    size_t i;
    for(i=0; i < n; i++) {
        jl_value_t *decl = jl_tupleref(sig, i);
        if (i == jl_tuple_len(sig)-1) {
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
        if (jl_is_tuple(decl)) {
            // tuples don't have to match exactly, to avoid caching
            // signatures for tuples of every length
            if (!jl_subtype(a, decl, 0))
                return 0;
        }
        else if (jl_is_datatype(a) && jl_is_datatype(decl) &&
                 ((jl_datatype_t*)decl)->name == jl_type_type->name &&
                 ((jl_datatype_t*)a   )->name == jl_type_type->name) {
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

static inline int cache_match(jl_value_t **args, size_t n, jl_tuple_t *sig,
                              int va, size_t lensig)
{
    // NOTE: This function is a huge performance hot spot!!
    for(size_t i=0; i < n; i++) {
        jl_value_t *decl = jl_tupleref(sig, i);
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
        else if (jl_is_tuple(decl)) {
            // tuples don't have to match exactly, to avoid caching
            // signatures for tuples of every length
            if (!jl_is_tuple(a) || //!jl_subtype(a, decl, 1))
                !jl_tuple_subtype(((jl_tuple_t*)a)->data, jl_tuple_len(a),
                                  ((jl_tuple_t*)decl)->data, jl_tuple_len(decl), 1))
                return 0;
        }
        else if (jl_is_type_type(decl) &&
                 (jl_is_nontuple_type(a) ||
                  (jl_is_tuple(a)&&jl_is_type(a)))) {
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
    if (ml && ml!=JL_NULL) {
        jl_value_t *t = jl_tupleref(ml->sig, 0);
        if (tparam) t = jl_tparam0(t);
        if (t == ty)
            return ml;
    }
    return (jl_methlist_t*)JL_NULL;
}

static void mtcache_rehash(jl_array_t **pa)
{
    size_t len = (*pa)->nrows;
    jl_value_t **d = (jl_value_t**)(*pa)->data;
    jl_array_t *n = jl_alloc_cell_1d(len*2);
    jl_value_t **nd = (jl_value_t**)n->data;
    size_t i;
    for(i=0; i < len; i++) {
        jl_methlist_t *ml = (jl_methlist_t*)d[i];
        if (ml && ml!=JL_NULL) {
            jl_value_t *t = jl_tupleref(ml->sig,0);
            if (jl_is_type_type(t))
                t = jl_tparam0(t);
            uptrint_t uid = ((jl_datatype_t*)t)->uid;
            nd[uid & (len*2-1)] = (jl_value_t*)ml;
        }
    }
    *pa = n;
}

static jl_methlist_t **mtcache_hash_bp(jl_array_t **pa, jl_value_t *ty,
                                       int tparam)
{
    uptrint_t uid;
    if (jl_is_datatype(ty) && (uid = ((jl_datatype_t*)ty)->uid)) {
        while (1) {
            jl_methlist_t **pml = (jl_methlist_t**)&jl_cellref(*pa, uid & ((*pa)->nrows-1));
            if (*pml == NULL || *pml == JL_NULL) {
                *pml = (jl_methlist_t*)JL_NULL;
                return pml;
            }
            jl_value_t *t = jl_tupleref((*pml)->sig,0);
            if (tparam) t = jl_tparam0(t);
            if (t == ty)
                return pml;
            mtcache_rehash(pa);
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
static jl_function_t *jl_method_table_assoc_exact_by_type(jl_methtable_t *mt,
                                                          jl_tuple_t *types)
{
    jl_methlist_t *ml = (jl_methlist_t*)JL_NULL;
    if (jl_tuple_len(types) > 0) {
        jl_value_t *ty = jl_t0(types);
        if (jl_is_type_type(ty)) {
            jl_value_t *a0 = jl_tparam0(ty);
            if (mt->cache_targ != JL_NULL && jl_is_datatype(a0)) {
                ml = mtcache_hash_lookup(mt->cache_targ, a0, 1);
                if (ml!=JL_NULL)
                    goto mt_assoc_bt_lkup;
            }
        }
        if (mt->cache_arg1 != JL_NULL && jl_is_datatype(ty)) {
            ml = mtcache_hash_lookup(mt->cache_arg1, ty, 0);
        }
    }
    if (ml == JL_NULL)
        ml = mt->cache;
 mt_assoc_bt_lkup:
    while (ml != JL_NULL) {
        if (cache_match_by_type(&jl_tupleref(types,0), jl_tuple_len(types),
                                (jl_tuple_t*)ml->sig, ml->va)) {
            return ml->func;
        }
        ml = ml->next;
    }
    return jl_bottom_func;
}

static jl_function_t *jl_method_table_assoc_exact(jl_methtable_t *mt,
                                                  jl_value_t **args, size_t n)
{
    // NOTE: This function is a huge performance hot spot!!
    jl_methlist_t *ml = (jl_methlist_t*)JL_NULL;
    if (n > 0) {
        jl_value_t *a0 = args[0];
        jl_value_t *ty = (jl_value_t*)jl_typeof(a0);
        if (mt->cache_targ != JL_NULL && ty == (jl_value_t*)jl_datatype_type) {
            ml = mtcache_hash_lookup(mt->cache_targ, a0, 1);
            if (ml != JL_NULL)
                goto mt_assoc_lkup;
        }
        if (mt->cache_arg1 != JL_NULL && jl_is_datatype(ty)) {
            ml = mtcache_hash_lookup(mt->cache_arg1, ty, 0);
            if (ml != JL_NULL) {
                if (ml->next==JL_NULL && n==1 && jl_tuple_len(ml->sig)==1)
                    return ml->func;
                if (n==2) {
                    // some manually-unrolled common special cases
                    jl_value_t *a1 = args[1];
                    if (!jl_is_tuple(a1)) {  // issue #6426
                        jl_methlist_t *mn = ml;
                        if (jl_tuple_len(mn->sig)==2 &&
                            jl_tupleref(mn->sig,1)==(jl_value_t*)jl_typeof(a1))
                            return mn->func;
                        mn = mn->next;
                        if (mn!=JL_NULL && jl_tuple_len(mn->sig)==2 &&
                            jl_tupleref(mn->sig,1)==(jl_value_t*)jl_typeof(a1))
                            return mn->func;
                    }
                }
            }
        }
    }
    if (ml == JL_NULL)
        ml = mt->cache;
 mt_assoc_lkup:
    while (ml != JL_NULL) {
        size_t lensig = jl_tuple_len(ml->sig);
        if (lensig == n || (ml->va && lensig <= n+1)) {
            if (cache_match(args, n, (jl_tuple_t*)ml->sig, ml->va, lensig)) {
                return ml->func;
            }
        }
        ml = ml->next;
    }
    return jl_bottom_func;
}

// return a new lambda-info that has some extra static parameters
// merged in.
jl_lambda_info_t *jl_add_static_parameters(jl_lambda_info_t *l, jl_tuple_t *sp)
{
    JL_GC_PUSH1(&sp);
    if (jl_tuple_len(l->sparams) > 0)
        sp = jl_tuple_append(sp, l->sparams);
    jl_lambda_info_t *nli = jl_new_lambda_info(l->ast, sp);
    nli->name = l->name;
    nli->fptr = l->fptr;
    nli->module = l->module;
    nli->file = l->file;
    nli->line = l->line;
    nli->def  = l->def;
    JL_GC_POP();
    return nli;
}

jl_function_t *jl_instantiate_method(jl_function_t *f, jl_tuple_t *sp)
{
    if (f->linfo == NULL)
        return f;
    jl_function_t *nf = jl_new_closure(f->fptr, f->env, NULL);
    JL_GC_PUSH1(&nf);
    nf->linfo = jl_add_static_parameters(f->linfo, sp);
    JL_GC_POP();
    return nf;
}

// append values of static parameters to closure environment
static jl_function_t *with_appended_env(jl_function_t *meth, jl_tuple_t *sparams)
{
    if (sparams == jl_null)
        return meth;
    jl_value_t *temp = (jl_value_t*)jl_alloc_tuple(jl_tuple_len(sparams)/2);
    JL_GC_PUSH1(&temp);
    size_t i;
    for(i=0; i < jl_tuple_len(temp); i++) {
        jl_tupleset(temp, i, jl_tupleref(sparams,i*2+1));
    }
    temp = (jl_value_t*)jl_tuple_append((jl_tuple_t*)meth->env, (jl_tuple_t*)temp);
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
jl_methlist_t *jl_method_list_insert(jl_methlist_t **pml, jl_tuple_t *type,
                                     jl_function_t *method, jl_tuple_t *tvars,
                                     int check_amb, int8_t isstaged);

jl_function_t *jl_method_cache_insert(jl_methtable_t *mt, jl_tuple_t *type,
                                      jl_function_t *method)
{
    jl_methlist_t **pml = &mt->cache;
    if (jl_tuple_len(type) > 0) {
        jl_value_t *t0 = jl_t0(type);
        uptrint_t uid=0;
        // if t0 != jl_typetype_type and the argument is Type{...}, this
        // method has specializations for singleton kinds and we use
        // the table indexed for that purpose.
        if (t0 != (jl_value_t*)jl_typetype_type && jl_is_type_type(t0)) {
            jl_value_t *a0 = jl_tparam0(t0);
            if (jl_is_datatype(a0))
                uid = ((jl_datatype_t*)a0)->uid;
            if (uid > 0) {
                if (mt->cache_targ == JL_NULL)
                    mt->cache_targ = jl_alloc_cell_1d(16);
                pml = mtcache_hash_bp(&mt->cache_targ, a0, 1);
                goto ml_do_insert;
            }
        }
        if (jl_is_datatype(t0))
            uid = ((jl_datatype_t*)t0)->uid;
        if (uid > 0) {
            if (mt->cache_arg1 == JL_NULL)
                mt->cache_arg1 = jl_alloc_cell_1d(16);
            pml = mtcache_hash_bp(&mt->cache_arg1, t0, 0);
        }
    }
 ml_do_insert:
    return jl_method_list_insert(pml, type, method, jl_null, 0, 0)->func;
}

extern jl_function_t *jl_typeinf_func;

/*
  run type inference on lambda "li" in-place, for given argument types.
  "def" is the original method definition of which this is an instance;
  can be equal to "li" if not applicable.
*/
int jl_in_inference = 0;
void jl_type_infer(jl_lambda_info_t *li, jl_tuple_t *argtypes,
                   jl_lambda_info_t *def)
{
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
        fargs[2] = (jl_value_t*)jl_null;
        fargs[3] = (jl_value_t*)def;
#ifdef TRACE_INFERENCE
        JL_PRINTF(JL_STDERR,"inference on %s", li->name->name);
        jl_static_show(JL_STDERR, (jl_value_t*)argtypes);
        JL_PRINTF(JL_STDERR, "\n");
#endif
#ifdef ENABLE_INFERENCE
        jl_value_t *newast = jl_apply(jl_typeinf_func, fargs, 4);
        li->ast = jl_tupleref(newast, 0);
        li->inferred = 1;
#endif
        li->inInference = 0;
    }
    jl_in_inference = last_ii;
}

static jl_value_t *nth_slot_type(jl_tuple_t *sig, size_t i)
{
    size_t len = jl_tuple_len(sig);
    if (len == 0)
        return NULL;
    if (i < len-1)
        return jl_tupleref(sig, i);
    if (jl_is_vararg_type(jl_tupleref(sig,len-1))) {
        return jl_tparam0(jl_tupleref(sig,len-1));
    }
    if (i == len-1)
        return jl_tupleref(sig, i);
    return NULL;
}

static int very_general_type(jl_value_t *t)
{
    return (t && (t==(jl_value_t*)jl_any_type ||
                  (jl_is_typevar(t) &&
                   ((jl_tvar_t*)t)->ub==(jl_value_t*)jl_any_type)));
}

static int tuple_all_Any(jl_tuple_t *t)
{
    for(int i=0; i < jl_tuple_len(t); i++) {
        if (jl_tupleref(t,i) != (jl_value_t*)jl_any_type)
            return 0;
    }
    return 1;
}

static int is_kind(jl_value_t *v)
{
    return (v==(jl_value_t*)jl_uniontype_type ||
            v==(jl_value_t*)jl_datatype_type ||
            v==(jl_value_t*)jl_typector_type);
}

static int jl_is_specializable_tuple(jl_tuple_t *t)
{
    if (t == jl_null) return 1;
    jl_value_t *e0 = jl_tupleref(t,0);
    if (jl_is_tuple(e0) || e0 == (jl_value_t*)jl_datatype_type) return 0;
    size_t i, l=jl_tuple_len(t);
    // allow specialization on homogeneous tuples
    for(i=1; i < l; i++) {
        if (jl_tupleref(t,i) != e0) return 0;
    }
    return 1;
}

static jl_value_t *ml_matches(jl_methlist_t *ml, jl_value_t *type,
                              jl_sym_t *name, int lim);

static jl_function_t *cache_method(jl_methtable_t *mt, jl_tuple_t *type,
                                   jl_function_t *method, jl_tuple_t *decl,
                                   jl_tuple_t *sparams)
{
    size_t i;
    int need_guard_entries = 0;
    jl_value_t *temp=NULL;
    jl_function_t *newmeth=NULL;
    JL_GC_PUSH3(&type, &temp, &newmeth);

    for (i=0; i < jl_tuple_len(type); i++) {
        jl_value_t *elt = jl_tupleref(type,i);
        jl_value_t *decl_i = nth_slot_type(decl,i);
        if (jl_is_type_type(elt) && jl_is_tuple(jl_tparam0(elt)) &&
            /*
              NOTE: without this, () is sometimes specialized as () and
              sometimes as Type{()}. In #6624, this caused a
                TypeError(func=:tuplelen, context="", expected=(Any...,), got=Type{()}())
              inside ==, inside isstructtype. Not quite clear why, however.
            */
            jl_tparam0(elt) != (jl_value_t*)jl_null &&

            !jl_is_type_type(decl_i)) {
            jl_methlist_t *curr = mt->defs;
            int ok=1;
            while (curr != JL_NULL) {
                jl_value_t *slottype = nth_slot_type(curr->sig, i);
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
                elt = jl_full_type(jl_tparam0(elt));
                jl_tupleset(type, i, elt);
            }
        }

        int set_to_any = 0;
        if (decl_i == jl_ANY_flag) {
            // don't specialize on slots marked ANY
            temp = jl_tupleref(type, i);
            jl_tupleset(type, i, (jl_value_t*)jl_any_type);
            int nintr=0;
            jl_methlist_t *curr = mt->defs;
            // if this method is the only match even with the current slot
            // set to Any, then it is safe to cache it that way.
            while (curr != JL_NULL && curr->func!=method) {
                if (jl_type_intersection((jl_value_t*)curr->sig,
                                         (jl_value_t*)type) !=
                    (jl_value_t*)jl_bottom_type) {
                    nintr++;
                    break;
                }
                curr = curr->next;
            }
            if (nintr) {
                // TODO: even if different specializations of this slot need
                // separate cache entries, have them share code.
                jl_tupleset(type, i, temp);
            }
            else {
                set_to_any = 1;
            }
        }
        if (set_to_any) {
        }
        else if (jl_is_tuple(elt) && !jl_is_specializable_tuple((jl_tuple_t*)elt)) {
            /*
              don't cache tuple type exactly; just remember that it was
              a tuple, unless the declaration asks for something more
              specific. determined with a type intersection.
            */
            int might_need_guard=0;
            temp = jl_tupleref(type, i);
            if (i < jl_tuple_len(decl)) {
                jl_value_t *declt = jl_tupleref(decl,i);
                // for T..., intersect with T
                if (jl_is_vararg_type(declt))
                    declt = jl_tparam0(declt);
                if (!jl_has_typevars(declt)) {
                    if (declt == (jl_value_t*)jl_tuple_type ||
                        jl_subtype((jl_value_t*)jl_tuple_type, declt, 0)) {
                        // don't specialize args that matched (Any...) or Any
                        jl_tupleset(type, i, (jl_value_t*)jl_tuple_type);
                        might_need_guard = 1;
                    }
                    else {
                        declt = jl_type_intersection(declt,
                                                     (jl_value_t*)jl_tuple_type);
                        if (jl_tuple_len(elt) > 3 ||
                            tuple_all_Any((jl_tuple_t*)declt)) {
                            jl_tupleset(type, i, declt);
                            might_need_guard = 1;
                        }
                    }
                }
            }
            else {
                jl_tupleset(type, i, (jl_value_t*)jl_tuple_type);
                might_need_guard = 1;
            }
            assert(jl_tupleref(type,i) != (jl_value_t*)jl_bottom_type);
            if (might_need_guard) {
                jl_methlist_t *curr = mt->defs;
                // can't generalize type if there's an overlapping definition
                // with typevars.
                // TODO: it seems premature to take these intersections
                // before the whole signature has been generalized.
                // example ((T...,),S,S,S,S,S,S,S,S,S,S,S,S,S,S,S,S,...)
                while (curr != JL_NULL && curr->func!=method) {
                    if (curr->tvars!=jl_null &&
                        jl_type_intersection((jl_value_t*)curr->sig,
                                             (jl_value_t*)type) !=
                        (jl_value_t*)jl_bottom_type) {
                        jl_tupleset(type, i, temp);
                        might_need_guard = 0;
                        break;
                    }
                    curr = curr->next;
                }
            }
            if (might_need_guard) {
                jl_methlist_t *curr = mt->defs;
                while (curr != JL_NULL && curr->func!=method) {
                    jl_tuple_t *sig = curr->sig;
                    if (jl_tuple_len(sig) > i &&
                        (jl_is_tuple(jl_tupleref(sig,i)) ||
                         // tuples can also be Types (issue #5577)
                         jl_subtype(jl_tupleref(sig,i), (jl_value_t*)jl_type_type, 0))) {
                        need_guard_entries = 1;
                        break;
                    }
                    curr = curr->next;
                }
            }
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
            if (i < jl_tuple_len(decl)) {
                jl_value_t *declt = jl_tupleref(decl,i);
                // for T..., intersect with T
                if (jl_is_vararg_type(declt))
                    declt = jl_tparam0(declt);
                jl_tupleset(type, i,
                            jl_type_intersection(declt, (jl_value_t*)jl_typetype_type));
                // TODO: recompute static parameter values, so in extreme cases we
                // can give `T=Type` instead of `T=Type{Type{Type{...`.
            }
            else {
                jl_tupleset(type, i, (jl_value_t*)jl_typetype_type);
            }
            assert(jl_tupleref(type,i) != (jl_value_t*)jl_bottom_type);
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
            jl_value_t *kind = (jl_value_t*)jl_full_type(jl_tparam0(elt));
            while (curr != JL_NULL) {
                jl_value_t *slottype = nth_slot_type(curr->sig, i);
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
                    jl_tupleset(type, i, kind);
                }
                else {
                    curr = mt->defs;
                    while (curr != JL_NULL) {
                        jl_value_t *slottype = nth_slot_type(curr->sig, i);
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
                        jl_tupleset(type, i, jl_typetype_type);
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
            while (curr != JL_NULL) {
                jl_value_t *slottype = nth_slot_type(curr->sig, i);
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
                jl_tupleset(type, i, decl_i);
        }
    }

    // for varargs methods, only specialize up to max_args.
    // in general, here we want to find the biggest type that's not a
    // supertype of any other method signatures. so far we are conservative
    // and the types we find should be bigger.
    if (!mt->defs->isstaged && jl_tuple_len(type) > mt->max_args &&
        jl_is_vararg_type(jl_tupleref(decl,jl_tuple_len(decl)-1))) {
        size_t nspec = mt->max_args + 2;
        jl_tuple_t *limited = jl_alloc_tuple(nspec);
        for(i=0; i < nspec-1; i++) {
            jl_tupleset(limited, i, jl_tupleref(type, i));
        }
        jl_value_t *lasttype = jl_tupleref(type,i-1);
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
        for(; j < jl_tuple_len(type); j++) {
            if (!jl_subtype(jl_tupleref(type,j), lasttype, 0)) {
                all_are_subtypes = 0;
                break;
            }
        }
        type = limited;
        if (all_are_subtypes) {
            // avoid Type{Type{...}...}...
            if (jl_is_type_type(lasttype) && jl_is_type_type(jl_tparam0(lasttype)))
                lasttype = (jl_value_t*)jl_type_type;
            temp = (jl_value_t*)jl_tuple1(lasttype);
            jl_tupleset(type, i, jl_apply_type((jl_value_t*)jl_vararg_type,
                                               (jl_tuple_t*)temp));
        }
        else {
            jl_value_t *lastdeclt = jl_tupleref(decl,jl_tuple_len(decl)-1);
            if (jl_tuple_len(sparams) > 0) {
                lastdeclt = (jl_value_t*)
                    jl_instantiate_type_with((jl_value_t*)lastdeclt,
                                             sparams->data,
                                             jl_tuple_len(sparams)/2);
            }
            jl_tupleset(type, i, lastdeclt);
        }
        // now there is a problem: the computed signature is more
        // general than just the given arguments, so it might conflict
        // with another definition that doesn't have cache instances yet.
        // to fix this, we insert guard cache entries for all intersections
        // of this signature and definitions. those guard entries will
        // supersede this one in conflicted cases, alerting us that there
        // should actually be a cache miss.
        need_guard_entries = 1;
    }

    if (need_guard_entries) {
        temp = ml_matches(mt->defs, (jl_value_t*)type, lambda_sym, -1);
        for(i=0; i < jl_array_len(temp); i++) {
            jl_value_t *m = jl_cellref(temp, i);
            if (jl_tupleref(m,2) != (jl_value_t*)method->linfo) {
                jl_method_cache_insert(mt, (jl_tuple_t*)jl_tupleref(m, 0),
                                       jl_bottom_func);
            }
        }
    }

    // here we infer types and specialize the method
    /*
    if (sparams==jl_null)
        newmeth = method;
    else
    */
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
        return newmeth;
    }
    else {
        if (jl_compileropts.compile_enabled == 0) {
            if (method->linfo->unspecialized == NULL) {
                JL_PRINTF(JL_STDERR,"code missing for %s", method->linfo->name->name);
                jl_static_show(JL_STDERR, (jl_value_t*)type);
                JL_PRINTF(JL_STDERR, "\n");
                exit(1);
            }
            jl_function_t *unspec = method->linfo->unspecialized;
            if (method->env == (jl_value_t*)jl_null)
                newmeth = unspec;
            else
                newmeth = jl_new_closure(unspec->fptr, method->env, unspec->linfo);

            if (sparams != jl_null) {
                newmeth = with_appended_env(newmeth, sparams);
            }

            (void)jl_method_cache_insert(mt, type, newmeth);
            JL_GC_POP();
            return newmeth;
        }
        else {
            newmeth = jl_instantiate_method(method, sparams);
        }
    }
    /*
      if "method" itself can ever be compiled, for example for use as
      an unspecialized method (see below), then newmeth->fptr might point
      to some slow compiled code instead of jl_trampoline, meaning our
      type-inferred code would never get compiled. this can be fixed with
      the commented-out snippet below.

      NOTE: this is now needed when we start with a system image compiled
      with --compile=all.
    */
    /*
    assert(!(newmeth->linfo && newmeth->linfo->ast) ||
           newmeth->fptr == &jl_trampoline);
    */
    if (newmeth->linfo && newmeth->linfo->ast && newmeth->fptr != &jl_trampoline) {
        newmeth->fptr = &jl_trampoline;
    }

    (void)jl_method_cache_insert(mt, type, newmeth);

    if (newmeth->linfo != NULL && newmeth->linfo->sparams == jl_null) {
        // when there are no static parameters, one unspecialized version
        // of a function can be shared among all cached specializations.
        if (method->linfo->unspecialized == NULL) {
            method->linfo->unspecialized =
                jl_instantiate_method(method, jl_null);
        }
        newmeth->linfo->unspecialized = method->linfo->unspecialized;
    }

    if (newmeth->linfo != NULL && newmeth->linfo->ast != NULL) {
        newmeth->linfo->specTypes = type;
        jl_array_t *spe = method->linfo->specializations;
        if (spe == NULL) {
            spe = jl_alloc_cell_1d(1);
            jl_cellset(spe, 0, newmeth->linfo);
        }
        else {
            jl_cell_1d_push(spe, (jl_value_t*)newmeth->linfo);
        }
        method->linfo->specializations = spe;
        jl_type_infer(newmeth->linfo, type, method->linfo);
    }
    JL_GC_POP();
    return newmeth;
}

static jl_value_t *lookup_match(jl_value_t *a, jl_value_t *b, jl_tuple_t **penv,
                                jl_tuple_t *tvars)
{
    jl_value_t *ti = jl_type_intersection_matching(a, b, penv, tvars);
    if (ti == (jl_value_t*)jl_bottom_type)
        return ti;
    JL_GC_PUSH1(&ti);
    assert(jl_is_tuple(*penv));
    jl_value_t **ee = (jl_value_t**)alloca(sizeof(void*) * jl_tuple_len(*penv));
    int n=0;
    // only keep vars in tvars list
    jl_value_t **tvs;
    int tvarslen;
    if (jl_is_typevar(tvars)) {
        tvs = (jl_value_t**)&tvars;
        tvarslen = 1;
    }
    else {
        tvs = &jl_t0(tvars);
        tvarslen = jl_tuple_len(tvars);
    }
    int l = jl_tuple_len(*penv);
    for(int i=0; i < l; i+=2) {
        jl_value_t *v = jl_tupleref(*penv,i);
        jl_value_t *val = jl_tupleref(*penv,i+1);
        for(int j=0; j < tvarslen; j++) {
            if (v == tvs[j]) {
                ee[n++] = v;
                ee[n++] = val;
                /*
                  since "a" is a concrete type, we assume that
                  (a∩b != Union()) => a<:b. However if a static parameter is
                  forced to equal Union(), then part of "b" might become Union(),
                  and therefore a subtype of "a". For example
                  (Type{Union()},Int) ∩ (Type{T},T)
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
        jl_tuple_t *en = jl_alloc_tuple_uninit(n);
        memcpy(en->data, ee, n*sizeof(void*));
        *penv = en;
    }
    JL_GC_POP();
    return ti;
}

DLLEXPORT jl_function_t *jl_instantiate_staged(jl_methlist_t *m, jl_tuple_t *tt, jl_tuple_t *env)
{
    jl_expr_t *ex = NULL;
    jl_expr_t *oldast = NULL;
    jl_function_t *func = NULL;
    JL_GC_PUSH3(&ex, &oldast, &func);
    if (jl_is_expr(m->func->linfo->ast))
        oldast = (jl_expr_t*)m->func->linfo->ast;
    else
        oldast = (jl_expr_t*)jl_uncompress_ast(m->func->linfo, m->func->linfo->ast);
    assert(oldast->head == lambda_sym);
    ex = jl_exprn(arrow_sym, 2);
    jl_array_t *oldargnames = (jl_array_t*)jl_cellref(oldast->args,0);
    jl_expr_t *argnames = jl_exprn(tuple_sym, jl_array_len(oldargnames));
    jl_cellset(ex->args, 0, argnames);
    for (size_t i = 0; i < jl_array_len(oldargnames); ++i) {
        jl_value_t *arg = jl_cellref(oldargnames,i);
        if (jl_is_expr(arg)) {
            assert(((jl_expr_t*)arg)->head == colons_sym);
            arg = jl_cellref(((jl_expr_t*)arg)->args,0);
            assert(jl_is_symbol(arg));
            jl_expr_t *dd_expr = jl_exprn(ldots_sym,1);
            jl_cellset(dd_expr->args,0,arg);
            jl_cellset(argnames->args,i,dd_expr);
        }
        else {
            assert(jl_is_symbol(arg));
            jl_cellset(argnames->args,i,arg);
        }
    }
    func = with_appended_env(m->func, env);
    jl_cellset(ex->args, 1, jl_apply(func, tt->data, jl_tuple_len(tt)));
    func = (jl_function_t*)jl_toplevel_eval_in(m->func->linfo->module, (jl_value_t*)ex);
    JL_GC_POP();
    return func;
}

static jl_function_t *jl_mt_assoc_by_type(jl_methtable_t *mt, jl_tuple_t *tt, int cache, int inexact)
{
    jl_methlist_t *m = mt->defs;
    size_t nargs = jl_tuple_len(tt);
    size_t i;
    jl_value_t *ti=(jl_value_t*)jl_bottom_type;
    jl_tuple_t *newsig=NULL, *env = jl_null;
    jl_function_t *func = NULL;
    JL_GC_PUSH3(&env, &newsig, &func);

    while (m != JL_NULL) {
        if (m->tvars!=jl_null) {
            ti = lookup_match((jl_value_t*)tt, (jl_value_t*)m->sig, &env, m->tvars);
            if (ti != (jl_value_t*)jl_bottom_type) {
                // parametric methods only match if all typevars are matched by
                // non-typevars.
                for(i=1; i < jl_tuple_len(env); i+=2) {
                    if (jl_is_typevar(jl_tupleref(env,i))) {
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
                if (i >= jl_tuple_len(env))
                    break;
                ti = (jl_value_t*)jl_bottom_type;
            }
        }
        else if (jl_tuple_subtype(&jl_tupleref(tt,0), nargs,
                                  &jl_tupleref(m->sig,0),
                                  jl_tuple_len(m->sig), 0)) {
            break;
        }
        m = m->next;
    }

    if (ti == (jl_value_t*)jl_bottom_type) {
        if (m != JL_NULL) {
            func = m->func;
            if (m->isstaged)
                func = jl_instantiate_staged(m,tt,env);
            JL_GC_POP();
            if (!cache)
                return func;
            return cache_method(mt, tt, func, (jl_tuple_t*)m->sig, jl_null);
        }
        JL_GC_POP();
        return jl_bottom_func;
    }

    assert(jl_is_tuple(env));
    func = m->func;

    if (m->isstaged)
        func = jl_instantiate_staged(m,tt,env);

    // don't bother computing this if no arguments are tuples
    for(i=0; i < jl_tuple_len(tt); i++) {
        if (jl_is_tuple(jl_tupleref(tt,i)))
            break;
    }
    if (i < jl_tuple_len(tt)) {
        newsig = (jl_tuple_t*)jl_instantiate_type_with((jl_value_t*)m->sig,
                                                       &jl_tupleref(env,0),
                                                       jl_tuple_len(env)/2);
    }
    else {
        newsig = (jl_tuple_t*)m->sig;
    }
    assert(jl_is_tuple(newsig));
    jl_function_t *nf;
    if (!cache)
        nf = func;
    else
        nf = cache_method(mt, tt, func, newsig, env);
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

DLLEXPORT int jl_args_morespecific(jl_value_t *a, jl_value_t *b)
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
            //if (jl_type_match_morespecific(b,a) == (jl_value_t*)jl_false)
            //    return 1;
            // this rule seems to work better:
            if (jl_type_match(b,a) == (jl_value_t*)jl_false)
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

static int is_va_tuple(jl_tuple_t *t)
{
    return (jl_tuple_len(t)>0 && jl_is_vararg_type(jl_tupleref(t,jl_tuple_len(t)-1)));
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
static void check_ambiguous(jl_methlist_t *ml, jl_tuple_t *type,
                            jl_methlist_t *oldmeth, jl_sym_t *fname,
                            jl_lambda_info_t *linfo)
{
    jl_tuple_t *sig = oldmeth->sig;
    size_t tl = jl_tuple_len(type);
    size_t sl = jl_tuple_len(sig);
    // we know !jl_args_morespecific(type, sig)
    if ((tl==sl ||
         (tl==sl+1 && is_va_tuple(type)) ||
         (tl+1==sl && is_va_tuple(sig))) &&
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
        jl_value_t *errstream;
        JL_STREAM *s;
        while (l != JL_NULL) {
            if (sigs_eq(isect, (jl_value_t*)l->sig, 0))
                goto done_chk_amb;  // ok, intersection is covered
            l = l->next;
        }
        n = fname->name;
        errstream = jl_stderr_obj();
        s = JL_STDERR;
        JL_PRINTF(s, "Warning: New definition \n    %s", n);
        jl_show(errstream, (jl_value_t*)type);
        print_func_loc(s, linfo);
        JL_PRINTF(s, "\nis ambiguous with: \n    %s", n);
        jl_show(errstream, (jl_value_t*)sig);
        print_func_loc(s, oldmeth->func->linfo);
        JL_PRINTF(s, ".\nTo fix, define \n    %s", n);
        jl_show(errstream, isect);
        JL_PRINTF(s, "\nbefore the new definition.\n");
    done_chk_amb:
        JL_GC_POP();
    }
}

static int has_unions(jl_tuple_t *type)
{
    int i;
    for(i=0; i < jl_tuple_len(type); i++) {
        jl_value_t *t = jl_tupleref(type,i);
        if (jl_is_uniontype(t) ||
            (jl_is_vararg_type(t) && jl_is_uniontype(jl_tparam0(t))))
            return 1;
    }
    return 0;
}

static
jl_methlist_t *jl_method_list_insert(jl_methlist_t **pml, jl_tuple_t *type,
                                     jl_function_t *method, jl_tuple_t *tvars,
                                     int check_amb, int8_t isstaged)
{
    jl_methlist_t *l, **pl;

    assert(jl_is_tuple(type));
    l = *pml;
    while (l != JL_NULL) {
        if (((l->tvars==jl_null) == (tvars==jl_null)) &&
            sigs_eq((jl_value_t*)type, (jl_value_t*)l->sig, 1)) {
            // method overwritten
            if (check_amb && l->func->linfo && method->linfo &&
                (l->func->linfo->module != method->linfo->module)) {
                jl_module_t *newmod = method->linfo->module;
                jl_value_t *errstream = jl_stderr_obj();
                JL_STREAM *s = JL_STDERR;
                JL_PRINTF(s, "Warning: Method definition %s", method->linfo->name->name);
                jl_show(errstream, (jl_value_t*)type);
                JL_PRINTF(s, " in module %s", l->func->linfo->module->name->name);
                print_func_loc(s, l->func->linfo);
                JL_PRINTF(s, " overwritten in module %s", newmod->name->name);
                print_func_loc(s, method->linfo);
                JL_PRINTF(s, ".\n");
            }
            JL_SIGATOMIC_BEGIN();
            l->sig = type;
            l->tvars = tvars;
            l->va = (jl_tuple_len(type) > 0 &&
                     jl_is_vararg_type(jl_tupleref(type,jl_tuple_len(type)-1))) ?
                1 : 0;
            l->isstaged = isstaged;
            l->invokes = (struct _jl_methtable_t *)JL_NULL;
            l->func = method;
            JL_SIGATOMIC_END();
            return l;
        }
        l = l->next;
    }
    pl = pml;
    l = *pml;
    while (l != JL_NULL) {
        if (jl_args_morespecific((jl_value_t*)type, (jl_value_t*)l->sig))
            break;
        if (check_amb) {
            check_ambiguous(*pml, type, l,
                            method->linfo ? method->linfo->name :
                            anonymous_sym, method->linfo);
        }
        pl = &l->next;
        l = l->next;
    }
    jl_methlist_t *newrec = (jl_methlist_t*)allocobj(sizeof(jl_methlist_t));
    newrec->type = (jl_value_t*)jl_method_type;
    newrec->sig = type;
    newrec->tvars = tvars;
    newrec->va = (jl_tuple_len(type) > 0 &&
                  jl_is_vararg_type(jl_tupleref(type,jl_tuple_len(type)-1))) ?
        1 : 0;
    newrec->isstaged = isstaged;
    newrec->func = method;
    newrec->invokes = (struct _jl_methtable_t*)JL_NULL;
    newrec->next = l;
    JL_SIGATOMIC_BEGIN();
    *pl = newrec;
    // if this contains Union types, methods after it might actually be
    // more specific than it. we need to re-sort them.
    if (has_unions(type)) {
        jl_methlist_t *item = newrec->next, *next;
        jl_methlist_t **pitem = &newrec->next, **pnext;
        while (item != JL_NULL) {
            pl = pml;
            l = *pml;
            next = item->next;
            pnext = &item->next;
            while (l != newrec->next) {
                if (jl_args_morespecific((jl_value_t*)item->sig,
                                         (jl_value_t*)l->sig)) {
                    // reinsert item earlier in the list
                    *pitem = next;
                    item->next = l;
                    *pl = item;
                    pnext = pitem;
                    break;
                }
                pl = &l->next;
                l = l->next;
            }
            item = next;
            pitem = pnext;
        }
    }
    JL_SIGATOMIC_END();
    return newrec;
}

static void remove_conflicting(jl_methlist_t **pl, jl_value_t *type)
{
    jl_methlist_t *l = *pl;
    while (l != JL_NULL) {
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

jl_methlist_t *jl_method_table_insert(jl_methtable_t *mt, jl_tuple_t *type,
                                      jl_function_t *method, jl_tuple_t *tvars,
                                      int8_t isstaged)
{
    if (jl_tuple_len(tvars) == 1)
        tvars = (jl_tuple_t*)jl_t0(tvars);
    JL_SIGATOMIC_BEGIN();
    jl_methlist_t *ml = jl_method_list_insert(&mt->defs,type,method,tvars,1,isstaged);
    // invalidate cached methods that overlap this definition
    remove_conflicting(&mt->cache, (jl_value_t*)type);
    if (mt->cache_arg1 != JL_NULL) {
        for(int i=0; i < jl_array_len(mt->cache_arg1); i++) {
            jl_methlist_t **pl = (jl_methlist_t**)&jl_cellref(mt->cache_arg1,i);
            if (*pl && *pl != JL_NULL)
                remove_conflicting(pl, (jl_value_t*)type);
        }
    }
    if (mt->cache_targ != JL_NULL) {
        for(int i=0; i < jl_array_len(mt->cache_targ); i++) {
            jl_methlist_t **pl = (jl_methlist_t**)&jl_cellref(mt->cache_targ,i);
            if (*pl && *pl != JL_NULL)
                remove_conflicting(pl, (jl_value_t*)type);
        }
    }
    // update max_args
    jl_tuple_t *t = (jl_tuple_t*)type;
    size_t na = jl_tuple_len(t);
    if (is_va_tuple(t))
        na--;
    if (na > mt->max_args) {
        mt->max_args = na;
    }
    JL_SIGATOMIC_END();
    return ml;
}

jl_value_t *jl_no_method_error(jl_function_t *f, jl_value_t **args, size_t na)
{
    jl_value_t *argtup = jl_f_tuple(NULL, args, na);
    JL_GC_PUSH1(&argtup);
    jl_value_t *fargs[3] = { (jl_value_t*)jl_methoderror_type, (jl_value_t*)f, argtup };
    jl_throw(jl_apply(jl_module_call_func(jl_base_module), fargs, 3));
    // not reached
    return jl_nothing;
}

static jl_tuple_t *arg_type_tuple(jl_value_t **args, size_t nargs)
{
    jl_tuple_t *tt = jl_alloc_tuple(nargs);
    JL_GC_PUSH1(&tt);
    size_t i;
    for(i=0; i < nargs; i++) {
        jl_value_t *ai = args[i];
        jl_value_t *a;
        if (!jl_is_typevar(ai) && jl_is_type(ai)) {
            a = (jl_value_t*)jl_wrap_Type(ai);
        }
        else if (!jl_is_tuple(ai)) {
            a = jl_typeof(ai);
        }
        else {
            a = (jl_value_t*)arg_type_tuple(&jl_tupleref(ai,0), jl_tuple_len(ai));
        }
        jl_tupleset(tt, i, a);
    }
    JL_GC_POP();
    return tt;
}

jl_function_t *jl_method_lookup_by_type(jl_methtable_t *mt, jl_tuple_t *types,
                                        int cache, int inexact)
{
    jl_function_t *sf = jl_method_table_assoc_exact_by_type(mt, types);
    if (sf == jl_bottom_func) {
        if (jl_is_leaf_type((jl_value_t*)types)) cache=1;
        sf = jl_mt_assoc_by_type(mt, types, cache, inexact);
    }
    return sf;
}

jl_function_t *jl_method_lookup(jl_methtable_t *mt, jl_value_t **args, size_t nargs, int cache)
{
    jl_function_t *sf = jl_method_table_assoc_exact(mt, args, nargs);
    if (sf == jl_bottom_func) {
        jl_tuple_t *tt = arg_type_tuple(args, nargs);
        JL_GC_PUSH1(&tt);
        sf = jl_mt_assoc_by_type(mt, tt, cache, 0);
        JL_GC_POP();
    }
    return sf;
}

DLLEXPORT jl_value_t *jl_matching_methods(jl_function_t *gf, jl_value_t *type, int lim);

// compile-time method lookup
jl_function_t *jl_get_specialization(jl_function_t *f, jl_tuple_t *types)
{
    if (!jl_is_leaf_type((jl_value_t*)types))
        return NULL;
    assert(jl_is_gf(f));

    // make sure exactly 1 method matches (issue #7302).
    int i;
    for(i=0; i < jl_tuple_len(types); i++) {
        jl_value_t *ti = jl_tupleref(types, i);
        // if one argument type is DataType, multiple Type{} definitions
        // might match. also be conservative with tuples rather than trying
        // to analyze them in detail.
        if (ti == (jl_value_t*)jl_datatype_type || jl_is_tuple(ti)) {
            jl_value_t *matches = jl_matching_methods(f, (jl_value_t*)types, 1);
            if (matches == jl_false)
                return NULL;
            break;
        }
    }

    jl_methtable_t *mt = jl_gf_mtable(f);
    jl_function_t *sf = NULL;
    JL_TRY {
        sf = jl_method_lookup_by_type(mt, types, 1, 1);
    } JL_CATCH {
        return NULL;
    }
    if (sf == jl_bottom_func) {
        return NULL;
    }
    if (sf->linfo == NULL || sf->linfo->ast == NULL) {
        return NULL;
    }
    if (sf->linfo->inInference) return NULL;
    if (sf->linfo->functionObject == NULL) {
        if (sf->fptr != &jl_trampoline)
            return NULL;
        jl_compile(sf);
    }
    return sf;
}

void jl_trampoline_compile_function(jl_function_t *f, int always_infer, jl_tuple_t *sig);

static void parameters_to_closureenv(jl_value_t *ast, jl_tuple_t *tvars)
{
    jl_array_t *closed = jl_lam_capt((jl_expr_t*)ast);
    jl_value_t **tvs;
    int tvarslen;
    if (jl_is_typevar(tvars)) {
        tvs = (jl_value_t**)&tvars;
        tvarslen = 1;
    }
    else {
        tvs = &jl_t0(tvars);
        tvarslen = jl_tuple_len(tvars);
    }
    size_t i;
    jl_array_t *vi=NULL;
    JL_GC_PUSH1(&vi);
    for(i=0; i < tvarslen; i++) {
        vi = jl_alloc_cell_1d(3);
        jl_cellset(vi, 0, ((jl_tvar_t*)tvs[i])->name);
        jl_cellset(vi, 1, jl_any_type);
        jl_cellset(vi, 2, jl_box_long(1));
        jl_cell_1d_push(closed, (jl_value_t*)vi);
    }
    JL_GC_POP();
}

static void all_p2c(jl_value_t *ast, jl_tuple_t *tvars)
{
    if (jl_is_lambda_info(ast)) {
        jl_lambda_info_t *li = (jl_lambda_info_t*)ast;
        li->ast = jl_prepare_ast(li, jl_null);
        parameters_to_closureenv(li->ast, tvars);
        all_p2c(li->ast, tvars);
    }
    else if (jl_is_expr(ast)) {
        jl_expr_t *e = (jl_expr_t*)ast;
        for(size_t i=0; i < jl_array_len(e->args); i++)
            all_p2c(jl_exprarg(e,i), tvars);
    }
}

static void precompile_unspecialized(jl_function_t *func, jl_tuple_t *sig, jl_tuple_t *tvars)
{
    func->linfo->specTypes = sig;
    if (tvars != jl_null) {
        // add static parameter names to end of closure env; compile
        // assuming they are there. method cache will fill them in when
        // it constructs closures for new "specializations".
        all_p2c((jl_value_t*)func->linfo, tvars);
    }
    jl_trampoline_compile_function(func, 1, sig ? sig : jl_tuple_type);
}

void jl_compile_all_defs(jl_function_t *gf)
{
    assert(jl_is_gf(gf));
    jl_methtable_t *mt = jl_gf_mtable(gf);
    if (mt->kwsorter != NULL)
        jl_compile_all_defs(mt->kwsorter);
    jl_methlist_t *m = mt->defs;
    while (m != JL_NULL) {
        if (jl_is_leaf_type((jl_value_t*)m->sig)) {
            jl_get_specialization(gf, m->sig);
        }
        else if (m->func->linfo->unspecialized == NULL) {
            jl_function_t *func = jl_instantiate_method(m->func, jl_null);
            m->func->linfo->unspecialized = func;
            precompile_unspecialized(func, m->sig, m->tvars);
        }
        m = m->next;
    }
}

static void _compile_all(jl_module_t *m, htable_t *h)
{
    size_t i;
    size_t sz = m->bindings.size;
    void **table = (void**) malloc(sz * sizeof(void*));
    memcpy(table, m->bindings.table, sz*sizeof(void*));
    ptrhash_put(h, m, m);
    for(i=1; i < sz; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            if (b->value != NULL) {
                jl_value_t *v = b->value;
                if (jl_is_gf(v)) {
                    jl_compile_all_defs((jl_function_t*)v);
                }
                else if (jl_is_module(v)) {
                    if (!ptrhash_has(h, v)) {
                        _compile_all((jl_module_t*)v, h);
                    }
                }
            }
        }
    }
    free(table);

    if (m->constant_table != NULL) {
        for(i=0; i < jl_array_len(m->constant_table); i++) {
            jl_value_t *el = jl_cellref(m->constant_table,i);
            if (jl_is_lambda_info(el)) {
                jl_lambda_info_t *li = (jl_lambda_info_t*)el;
                jl_function_t *func = jl_new_closure(li->fptr, (jl_value_t*)jl_null, li);
                li->unspecialized = func;
                precompile_unspecialized(func, NULL, jl_null);
            }
        }
    }
}

void jl_compile_all(void)
{
    htable_t h;
    htable_new(&h, 0);
    _compile_all(jl_main_module, &h);
}

DLLEXPORT void jl_compile_hint(jl_function_t *f, jl_tuple_t *types)
{
    (void)jl_get_specialization(f, types);
}

#ifdef JL_TRACE
static int trace_en = 0;
static int error_en = 1;
static void __attribute__ ((unused)) enable_trace(int x) { trace_en=x; }
static void show_call(jl_value_t *F, jl_value_t **args, uint32_t nargs)
{
    JL_PRINTF(JL_STDOUT, "%s(",  jl_gf_name(F)->name);
    for(size_t i=0; i < nargs; i++) {
        if (i > 0) JL_PRINTF(JL_STDOUT, ", ");
        jl_static_show(JL_STDOUT, jl_typeof(args[i]));
    }
    JL_PRINTF(JL_STDOUT, ")\n");
}
#endif

JL_CALLABLE(jl_apply_generic)
{
    jl_methtable_t *mt = jl_gf_mtable(F);
#ifdef JL_GF_PROFILE
    mt->ncalls++;
#endif
#ifdef JL_TRACE
    if (trace_en)
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
        if (mfunc->linfo != NULL &&
            (mfunc->linfo->inInference || mfunc->linfo->inCompile)) {
            // if inference is running on this function, return a copy
            // of the function to be compiled without inference and run.
            jl_lambda_info_t *li = mfunc->linfo;
            if (li->unspecialized == NULL) {
                li->unspecialized = jl_instantiate_method(mfunc, li->sparams);
            }
            mfunc = li->unspecialized;
            assert(mfunc != jl_bottom_func);
        }
        assert(!mfunc->linfo || !mfunc->linfo->inInference);
        return jl_apply(mfunc, args, nargs);
    }

    // cache miss case
    jl_tuple_t *tt = arg_type_tuple(args, nargs);
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
        return jl_no_method_error((jl_function_t*)F, args, nargs);
    }
    assert(!mfunc->linfo || !mfunc->linfo->inInference);
    jl_value_t *res = jl_apply(mfunc, args, nargs);
    JL_GC_POP();
    return res;
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
jl_value_t *jl_gf_invoke(jl_function_t *gf, jl_tuple_t *types,
                         jl_value_t **args, size_t nargs)
{
    assert(jl_is_gf(gf));
    jl_methtable_t *mt = jl_gf_mtable(gf);

    jl_methlist_t *m = mt->defs;
    size_t typelen = jl_tuple_len(types);
    size_t i;
    jl_value_t *env = (jl_value_t*)jl_false;

    while (m != JL_NULL) {
        if (m->tvars!=jl_null) {
            env = jl_type_match((jl_value_t*)types, (jl_value_t*)m->sig);
            if (env != (jl_value_t*)jl_false) break;
        }
        else if (jl_tuple_subtype(&jl_tupleref(types,0), typelen,
                                  &jl_tupleref(m->sig,0),
                                  jl_tuple_len(m->sig), 0)) {
            break;
        }
        m = m->next;
    }

    if (m == JL_NULL) {
        return jl_no_method_error(gf, args, nargs);
    }

    // now we have found the matching definition.
    // next look for or create a specialization of this definition.

    jl_function_t *mfunc;
    if (m->invokes == JL_NULL)
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
                li->unspecialized = jl_instantiate_method(mfunc, li->sparams);
            }
            mfunc = li->unspecialized;
        }
    }
    else {
        jl_tuple_t *tpenv=jl_null;
        jl_tuple_t *newsig=NULL;
        jl_tuple_t *tt=NULL;
        JL_GC_PUSH3(&env, &newsig, &tt);

        if (m->invokes == JL_NULL) {
            m->invokes = new_method_table(mt->name);
            // this private method table has just this one definition
            jl_method_list_insert(&m->invokes->defs,m->sig,m->func,m->tvars,0,0);
        }

        tt = arg_type_tuple(args, nargs);

        newsig = (jl_tuple_t*)m->sig;

        if (env != (jl_value_t*)jl_false) {
            jl_value_t *ti =
                lookup_match((jl_value_t*)tt, (jl_value_t*)m->sig, &tpenv, m->tvars);
            assert(ti != (jl_value_t*)jl_bottom_type);
            (void)ti;
            // don't bother computing this if no arguments are tuples
            for(i=0; i < jl_tuple_len(tt); i++) {
                if (jl_is_tuple(jl_tupleref(tt,i)))
                    break;
            }
            if (i < jl_tuple_len(tt)) {
                newsig =
                    (jl_tuple_t*)jl_instantiate_type_with((jl_value_t*)m->sig,
                                                          &jl_tupleref(tpenv,0),
                                                          jl_tuple_len(tpenv)/2);
            }
        }
        mfunc = cache_method(m->invokes, tt, m->func, newsig, tpenv);
        JL_GC_POP();
    }

    return jl_apply(mfunc, args, nargs);
}

void print_func_loc(JL_STREAM *s, jl_lambda_info_t *li)
{
    long lno = li->line;
    if (lno > 0) {
        char *fname = ((jl_sym_t*)li->file)->name;
        JL_PRINTF(s, " at %s:%d", fname, lno);
    }
}

void jl_initialize_generic_function(jl_function_t *f, jl_sym_t *name)
{
    f->fptr = jl_apply_generic;
    f->env = (jl_value_t*)new_method_table(name);
}

jl_function_t *jl_new_generic_function(jl_sym_t *name)
{
    jl_function_t *f = jl_new_closure(jl_apply_generic, NULL, NULL);
    JL_GC_PUSH1(&f);
    jl_initialize_generic_function(f, name);
    JL_GC_POP();
    return f;
}

DLLEXPORT jl_function_t *jl_new_gf_internal(jl_value_t *env)
{
    return jl_new_closure(jl_apply_generic, env, NULL);
}

void jl_add_method(jl_function_t *gf, jl_tuple_t *types, jl_function_t *meth,
                   jl_tuple_t *tvars, int8_t isstaged)
{
    assert(jl_is_function(gf));
    assert(jl_is_tuple(types));
    assert(jl_is_func(meth));
    assert(jl_is_mtable(jl_gf_mtable(gf)));
    if (meth->linfo != NULL)
        meth->linfo->name = jl_gf_name(gf);
    if (isstaged && tvars != jl_null) {
        all_p2c((jl_value_t*)meth->linfo, tvars);
    }
    (void)jl_method_table_insert(jl_gf_mtable(gf), types, meth, tvars, isstaged);
}

DLLEXPORT jl_tuple_t *jl_match_method(jl_value_t *type, jl_value_t *sig,
                                      jl_tuple_t *tvars)
{
    jl_tuple_t *env = jl_null;
    jl_value_t *ti=NULL;
    JL_GC_PUSH2(&env, &ti);
    ti = lookup_match(type, (jl_value_t*)sig, &env, tvars);
    jl_tuple_t *result = jl_tuple2(ti, env);
    JL_GC_POP();
    return result;
}

// Determine whether a typevar exists inside at most one DataType.
// These are the typevars that will always be matched by any matching
// arguments.
static int tvar_exists_at_top_level(jl_value_t *tv, jl_tuple_t *sig, int attop)
{
    int i, l=jl_tuple_len(sig);
    for(i=0; i < l; i++) {
        jl_value_t *a = jl_tupleref(sig, i);
        if (jl_is_vararg_type(a))
            a = jl_tparam0(a);
        if (a == tv)
            return 1;
        if (jl_is_tuple(a) && tvar_exists_at_top_level(tv, (jl_tuple_t*)a, attop))
            return 1;
        if (attop && jl_is_datatype(a)) {
            if (tvar_exists_at_top_level(tv, ((jl_datatype_t*)a)->parameters, 0))
                return 1;
        }
    }
    return 0;
}

// returns a match as (argtypes, static_params, Method)
static jl_value_t *ml_matches(jl_methlist_t *ml, jl_value_t *type,
                              jl_sym_t *name, int lim)
{
    jl_array_t *t = (jl_array_t*)jl_an_empty_cell;
    jl_tuple_t *matc=NULL;
    jl_tuple_t *env = jl_null;
    jl_value_t *ti=NULL;
    JL_GC_PUSH4(&t, &matc, &env, &ti);
    int len=0, i;
    while (ml != JL_NULL) {
        // a method is shadowed if type <: S <: m->sig where S is the
        // signature of another applicable method
        /*
          more generally, we can stop when the type is a subtype of the
          union of all the signatures examined so far.
        */
        env = jl_null;
        ti = lookup_match(type, (jl_value_t*)ml->sig, &env, ml->tvars);
        if (ti != (jl_value_t*)jl_bottom_type) {
            assert(ml->func->linfo);  // no builtin methods
            assert(jl_is_tuple(env));

            int skip = 0;
            if (lim >= 0) {
                // we can skip this match if the types are already covered
                // by a prior (more specific) match. but only do this in
                // the "limited" mode used by type inference.
                size_t l = jl_array_len(t);
                for(i=0; i < l; i++) {
                    jl_value_t *prior_ti = jl_t0(jl_cellref(t,i));
                    if (jl_is_leaf_type(prior_ti) && jl_subtype(ti, prior_ti, 0)) {
                        skip = 1;
                        break;
                    }
                }
                // don't analyze slots declared with ANY
                l = jl_tuple_len(ml->sig);
                size_t m = jl_tuple_len(ti);
                for(i=0; i < l && i < m; i++) {
                    if (jl_tupleref(ml->sig, i) == jl_ANY_flag)
                        jl_tupleset(ti, i, jl_any_type);
                }
            }
            if (!skip) {
                len++;
                if (lim >= 0 && len > lim) {
                    JL_GC_POP();
                    return jl_false;
                }
                matc = jl_tuple(3, ti, env, ml);
                /*
                  Check whether all static parameters matched. If not, then we
                  have an argument type like Vector{T{Int,_}}, and a signature like
                  f{A,B}(::Vector{T{A,B}}). If "_" turns out to be a non-typevar
                  at runtime then this method matches, otherwise it doesn't. So we
                  have to look for more matches. This caused issue #4731.
                */
                int matched_all_typevars = 1;
                size_t l = jl_tuple_len(env);
                for(i=1; i < l; i+=2) {
                    if (jl_is_typevar(jl_tupleref(env,i)) &&
                        // if tvar is at the top level it will definitely be matched.
                        // see issue #5575
                        !tvar_exists_at_top_level(jl_tupleref(env,i-1), ml->sig, 1)) {
                        matched_all_typevars = 0;
                        break;
                    }
                }
                if (len == 1) {
                    t = jl_alloc_cell_1d(1);
                    jl_cellref(t,0) = (jl_value_t*)matc;
                }
                else {
                    jl_cell_1d_push(t, (jl_value_t*)matc);
                }
                // (type ∩ ml->sig == type) ⇒ (type ⊆ ml->sig)
                // NOTE: jl_subtype check added in case the intersection is
                // over-approximated.
                if (matched_all_typevars && jl_types_equal(jl_t0(matc), type) &&
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

// return a cell array of tuples, each describing a method match:
// {(t, spvals, li, cenv), ...}
// t is the intersection of the type argument and the method signature,
// spvals is any matched static parameter values, li is the LambdaStaticData,
// and cenv is the closure environment or ().
//
// lim is the max # of methods to return. if there are more return jl_false.
// -1 for no limit.
DLLEXPORT
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

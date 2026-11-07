// This file is a part of Julia. License is MIT: https://julialang.org/license

// Admission for the datatype and linear fragments. This walks expression
// occurrences rather than datatype-family metadata. Inherited supertype links
// are checked but must not multiply the occurrence count of a parameter.
typedef struct scorpus_linear_scope {
    jl_tvar_t *var;
    unsigned uses;
    struct scorpus_linear_scope *outer;
} scorpus_linear_scope;

static int scorpus_extended_admit(jl_value_t *v, scorpus_linear_scope *scope,
                                  unsigned depth, unsigned *visits, int count) JL_NOTSAFEPOINT
{
    if (!v || depth > SCORPUS_MAX_DEPTH || ++*visits > SCORPUS_MAX_NODES)
        return 0;
    if (v == (jl_value_t*)jl_any_type || v == jl_bottom_type)
        return 1;
    if (jl_is_typevar(v)) {
        for (scorpus_linear_scope *s = scope; s; s = s->outer) {
            if ((jl_value_t*)s->var == v) {
                if (count && s->uses < 2)
                    s->uses++;
                return 1;
            }
        }
        return 0;
    }
    if (jl_is_unionall(v)) {
        if (scorpus_grammar != 4)
            return 0;
        jl_unionall_t *ua = (jl_unionall_t*)v;
        if (!ua->var || !jl_is_typevar(ua->var) ||
            !scorpus_extended_admit(ua->var->lb, scope, depth + 1, visits, count) ||
            !scorpus_extended_admit(ua->var->ub, scope, depth + 1, visits, count))
            return 0;
        scorpus_linear_scope inner = {ua->var, 0, scope};
        return scorpus_extended_admit(ua->body, &inner, depth + 1, visits, count) &&
            (!count || inner.uses == 1);
    }
    if (jl_is_uniontype(v) || jl_is_intersecttype(v)) {
        jl_value_t *a = jl_is_uniontype(v) ? ((jl_uniontype_t*)v)->a : ((jl_intersecttype_t*)v)->a;
        jl_value_t *b = jl_is_uniontype(v) ? ((jl_uniontype_t*)v)->b : ((jl_intersecttype_t*)v)->b;
        return scorpus_extended_admit(a, scope, depth + 1, visits, count) &&
            scorpus_extended_admit(b, scope, depth + 1, visits, count);
    }
    if (jl_is_nottype(v))
        return scorpus_extended_admit(((jl_nottype_t*)v)->body, scope, depth + 1, visits, count);
    if (jl_is_typeeq(v))
        return scorpus_extended_admit(jl_typeeq_T(v), scope, depth + 1, visits, count);
    if (!jl_is_datatype(v))
        return 0;
    jl_datatype_t *dt = (jl_datatype_t*)v;
    if (!dt->name || !dt->parameters || dt == jl_anytuple_type)
        return 0;
    for (size_t i = 0; i < jl_nparams(dt); i++) {
        jl_value_t *p = jl_tparam(dt, i);
        if (!p || jl_is_vararg(p) || jl_is_typeegal(p) || jl_is_typeapp(p))
            return 0;
        if (jl_is_type(p) || jl_is_typevar(p) || jl_is_nottype(p) || jl_is_intersecttype(p)) {
            if (!scorpus_extended_admit(p, scope, depth + 1, visits, count))
                return 0;
        }
        else if (dt->name == jl_tuple_typename)
            return 0;
        // Remaining non-type parameters are literal values; v3 records them.
    }
    return dt->name == jl_tuple_typename ||
        scorpus_extended_admit((jl_value_t*)dt->super, scope, depth + 1, visits, 0);
}

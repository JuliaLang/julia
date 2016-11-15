// This file is a part of Julia. License is MIT: http://julialang.org/license

/*
  subtyping predicate

  Uses the algorithm described in section 4.2.2 of https://github.com/JeffBezanson/phdthesis/
  This code adds the following features to the core algorithm:

  - Type variables can be restricted to range over only concrete types.
    This is done by returning false if such a variable's lower bound is not concrete.
  - Diagonal rule: a type variable is concrete if it occurs more than once in
    covariant position, and never in invariant position. This sounds like a syntactic
    property, but actually isn't since it depends on which occurrences of a type
    variable the algorithm actually uses.
  - Unconstrained type vars (Bottom<:T<:Any) can match non-type values.
  - Vararg types have an int-valued length parameter N (in `Vararg{T,N}`).
  - Type{T}<:S if isa(T,S). Existing code assumes this, but it's not strictly
    correct since a type can equal `T` without having the same representation.
  - Free type variables are tolerated. This can hopefully be removed after a
    deprecation period.
*/
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#ifdef _OS_WINDOWS_
#include <malloc.h>
#endif
#include "julia.h"
#include "julia_internal.h"

// stack of bits to keep track of which combination of Union components we are
// looking at (0 for Union.a, 1 for Union.b). forall_exists_subtype and
// exists_subtype loop over all combinations by updating a binary count in
// this structure.
// Union type decision points are discovered while the algorithm works.
// If a new Union decision is encountered, the `more` flag is set to tell
// the forall/exists loop to grow the stack.
typedef struct {
    int depth;
    int8_t more;
    int stacksize;
    uint32_t stack[10];  // stack of bits represented as a bit vector
} jl_unionstate_t;

// Linked list storing the type variable environment. A new jl_varbinding_t
// is pushed for each UnionAll type we encounter. `lb` and `ub` are updated
// during the computation.
// Most of the complexity is due to the "diagonal rule", requiring us to
// identify which type vars range over only concrete types.
typedef struct _varbinding {
    jl_tvar_t *var;
    jl_value_t *lb;
    jl_value_t *ub;
    int8_t right;       // whether this variable came from the right side of `A <: B`
    // if another var that this one depends on is found to be concrete, store it
    // here for reference in case that var is popped from the environment before this one.
    // TODO: generalize this to multiple variables
    jl_tvar_t *concretevar;
    int8_t occurs_inv;  // occurs in invariant position
    int8_t occurs_cov;  // # of occurrences in covariant position
    int8_t concrete;    // 1 if another variable has a constraint forcing this one to be concrete
    // in covariant position, we need to try constraining a variable in different ways:
    // 0 - unconstrained
    // 1 - less than
    // 2 - greater than
    // 3 - inexpressible - occurs when the var has non-trivial overlap with another type,
    //                     and we would need to return `intersect(var,other)`. in this case
    //                     we choose to over-estimate the intersection by returning the var.
    int8_t constraintkind;
    int depth0;         // # of invariant constructors nested around the UnionAll type for this var
    // when this variable's integer value is compared to that of another,
    // it equals `other + offset`. used by vararg length parameters.
    int offset;
    struct _varbinding *prev;
} jl_varbinding_t;

// subtype algorithm state
typedef struct {
    jl_varbinding_t *vars;    // type variable environment
    jl_unionstate_t Lunions;  // union state for unions on the left of A <: B
    jl_unionstate_t Runions;  // union state for unions on the right
    jl_value_t **envout;      // for passing caller the computed bounds of right-side variables
    int envsz;                // length of envout
    int envidx;               // current index in envout
    int invdepth;             // current number of invariant constructors we're nested in
    int ignore_free;          // treat free vars as black boxes; used during intersection
    int intersection;         // true iff subtype is being called from intersection
} jl_stenv_t;

// state manipulation utilities

// look up a type variable in an environment
static jl_varbinding_t *lookup(jl_stenv_t *e, jl_tvar_t *v)
{
    jl_varbinding_t *b = e->vars;
    while (b != NULL) {
        if (b->var == v) return b;
        b = b->prev;
    }
    return b;
}

static int statestack_get(jl_unionstate_t *st, int i)
{
    assert(i < st->stacksize);
    // get the `i`th bit in an array of 32-bit words
    return (st->stack[i>>5] & (1<<(i&31))) != 0;
}

static void statestack_set(jl_unionstate_t *st, int i, int val)
{
    assert(i < st->stacksize);
    if (val)
        st->stack[i>>5] |= (1<<(i&31));
    else
        st->stack[i>>5] &= ~(1<<(i&31));
}

static void statestack_push(jl_unionstate_t *st, int val)
{
    st->stacksize++;
    assert(st->stacksize <= sizeof(st->stack)*8);
    statestack_set(st, st->stacksize-1, val);
}

static void statestack_pop(jl_unionstate_t *st)
{
    assert(st->stacksize > 0);
    st->stacksize--;
}

typedef struct {
    int8_t *buf;
    int rdepth;
} jl_savedenv_t;

static void save_env(jl_stenv_t *e, jl_value_t **root, jl_savedenv_t *se)
{
    jl_varbinding_t *v = e->vars;
    int len=0;
    while (v != NULL) {
        len++;
        v = v->prev;
    }
    *root = (jl_value_t*)jl_alloc_svec(len*2);
    se->buf = (int8_t*)(len ? malloc(len*2) : NULL);
    int i=0; v = e->vars;
    while (v != NULL) {
        jl_svecset(*root, i, v->lb); se->buf[i] = v->occurs_inv;
        i++;
        jl_svecset(*root, i, v->ub); se->buf[i] = v->occurs_cov;
        i++;
        v = v->prev;
    }
    se->rdepth = e->Runions.depth;
}

static void restore_env(jl_stenv_t *e, jl_value_t *root, jl_savedenv_t *se)
{
    jl_varbinding_t *v = e->vars;
    int i = 0;
    while (v != NULL) {
        if (root) v->lb = jl_svecref(root, i);
        v->occurs_inv = se->buf[i];
        i++;
        if (root) v->ub = jl_svecref(root, i);
        v->occurs_cov = se->buf[i];
        i++;
        v = v->prev;
    }
    e->Runions.depth = se->rdepth;
}

// type utilities

// quickly test that two types are identical
static int obviously_egal(jl_value_t *a, jl_value_t *b)
{
    if (a == b) return 1;
    if (jl_typeof(a) != jl_typeof(b)) return 0;
    if (jl_is_datatype(a)) {
        jl_datatype_t *ad = (jl_datatype_t*)a, *bd = (jl_datatype_t*)b;
        if (ad->name != bd->name) return 0;
        size_t i, np = jl_nparams(ad);
        if (np != jl_nparams(bd)) return 0;
        for(i=0; i < np; i++) {
            if (!obviously_egal(jl_tparam(ad,i), jl_tparam(bd,i)))
                return 0;
        }
        return 1;
    }
    if (jl_is_uniontype(a)) {
        return obviously_egal(((jl_uniontype_t*)a)->a, ((jl_uniontype_t*)b)->a) &&
            obviously_egal(((jl_uniontype_t*)a)->b, ((jl_uniontype_t*)b)->b);
    }
    if (jl_is_unionall(a)) {
        return ((jl_unionall_t*)a)->var == ((jl_unionall_t*)b)->var &&
            obviously_egal(((jl_unionall_t*)a)->body, ((jl_unionall_t*)b)->body);
    }
    if (jl_is_typevar(a)) return 0;
    return !jl_is_type(a) && jl_egal(a,b);
}

static int obviously_unequal(jl_value_t *a, jl_value_t *b)
{
    if (jl_is_unionall(a)) a = jl_unwrap_unionall(a);
    if (jl_is_unionall(b)) b = jl_unwrap_unionall(b);
    if (jl_is_datatype(a)) {
        if (b == jl_bottom_type) return 1;
        if (jl_is_datatype(b)) {
            jl_datatype_t *ad = (jl_datatype_t*)a, *bd = (jl_datatype_t*)b;
            if (ad->name != bd->name)
                return 1;
            size_t i, np = jl_nparams(ad);
            if (np != jl_nparams(bd)) return 1;
            for(i=0; i < np; i++) {
                if (obviously_unequal(jl_tparam(ad,i), jl_tparam(bd,i)))
                    return 1;
            }
        }
    }
    else if (a == jl_bottom_type && jl_is_datatype(b)) {
        return 1;
    }
    if (jl_is_typevar(a) && jl_is_typevar(b) && obviously_unequal(((jl_tvar_t*)a)->ub, ((jl_tvar_t*)b)->ub))
        return 1;
    if (jl_is_long(a)) {
        if (jl_is_long(b) && jl_unbox_long(a) != jl_unbox_long(b))
            return 1;
    }
    else if (jl_is_long(b)) return 1;
    if ((jl_is_symbol(a) || jl_is_symbol(b)) && a != b)
        return 1;
    return 0;
}

static int in_union(jl_value_t *u, jl_value_t *x)
{
    if (u == x) return 1;
    if (!jl_is_uniontype(u)) return 0;
    return in_union(((jl_uniontype_t*)u)->a, x) || in_union(((jl_uniontype_t*)u)->b, x);
}

// compute a least upper bound of `a` and `b`
static jl_value_t *simple_join(jl_value_t *a, jl_value_t *b)
{
    if (a == jl_bottom_type || b == (jl_value_t*)jl_any_type || obviously_egal(a,b))
        return b;
    if (b == jl_bottom_type || a == (jl_value_t*)jl_any_type)
        return a;
    if (!(jl_is_type(a) || jl_is_typevar(a)) || !(jl_is_type(b) || jl_is_typevar(b)))
        return (jl_value_t*)jl_any_type;
    if (jl_is_uniontype(a) && in_union(a, b))
        return a;
    if (jl_is_uniontype(b) && in_union(b, a))
        return b;
    if (is_kind(a) && jl_is_type_type(b) && jl_typeof(jl_tparam0(b)) == a)
        return a;
    if (is_kind(b) && jl_is_type_type(a) && jl_typeof(jl_tparam0(a)) == b)
        return b;
    if (jl_is_type_type(a) && jl_is_type_type(b) && !jl_is_typevar(jl_tparam0(a)) &&
        jl_typeof(jl_tparam0(a)) == jl_typeof(jl_tparam0(b)))
        return jl_typeof(jl_tparam0(a));
    if (!jl_has_free_typevars(a) && !jl_has_free_typevars(b)) {
        if (jl_subtype(a, b)) return b;
        if (jl_subtype(b, a)) return a;
    }
    return jl_new_struct(jl_uniontype_type, a, b);
}

static jl_unionall_t *rename_unionall(jl_unionall_t *u)
{
    jl_tvar_t *v = jl_new_typevar(u->var->name, u->var->lb, u->var->ub);
    jl_value_t *t = NULL;
    JL_GC_PUSH2(&v, &t);
    t = jl_instantiate_unionall(u, (jl_value_t*)v);
    t = jl_new_struct(jl_unionall_type, v, t);
    JL_GC_POP();
    return (jl_unionall_t*)t;
}

// main subtyping algorithm

static int subtype(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int param);

// compare the current component of `u` to `t`. `R==1` means `u` came from the right side.
static int subtype_union(jl_value_t *t, jl_uniontype_t *u, jl_stenv_t *e, int8_t R, int param)
{
    jl_unionstate_t *state = R ? &e->Runions : &e->Lunions;
    if (state->depth >= state->stacksize) {
        state->more = 1;
        return 1;
    }
    int ui = statestack_get(state, state->depth);
    state->depth++;
    jl_value_t *choice = ui==0 ? u->a : u->b;
    return R ? subtype(t, choice, e, param) : subtype(choice, t, e, param);
}

// subtype(), but taking apart unions before handling vars
static int subtype_ufirst(jl_value_t *x, jl_value_t *y, jl_stenv_t *e)
{
    if (jl_is_uniontype(x) && jl_is_typevar(y))
        return subtype_union(y, (jl_uniontype_t*)x, e, 0, 0);
    if (jl_is_typevar(x) && jl_is_uniontype(y))
        return (x == ((jl_uniontype_t*)y)->a || x == ((jl_uniontype_t*)y)->b ||
                subtype_union(x, (jl_uniontype_t*)y, e, 1, 0));
    return subtype(x, y, e, 0);
}

// use the current context to record where a variable occurred, for the purpose
// of determining whether the variable is concrete.
static void record_var_occurrence(jl_varbinding_t *vb, jl_stenv_t *e, int param)
{
    if (vb != NULL && param) {
        if (param == 2 && e->invdepth > vb->depth0)
            vb->occurs_inv++;
        else
            vb->occurs_cov++;
    }
}

// is var x's quantifier outside y's in nesting order
static int var_outside(jl_stenv_t *e, jl_tvar_t *x, jl_tvar_t *y)
{
    jl_varbinding_t *btemp = e->vars;
    while (btemp != NULL) {
        if (btemp->var == x) return 0;
        if (btemp->var == y) return 1;
        btemp = btemp->prev;
    }
    return 0;
}

static jl_value_t *intersect_ufirst(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int depth);

// check that type var `b` is <: `a`, and update b's upper bound.
static int var_lt(jl_tvar_t *b, jl_value_t *a, jl_stenv_t *e, int param)
{
    jl_varbinding_t *bb = lookup(e, b);
    if (bb == NULL)
        return e->ignore_free || subtype_ufirst(b->ub, a, e);
    record_var_occurrence(bb, e, param);
    if (!bb->right)  // check ∀b . b<:a
        return subtype_ufirst(bb->ub, a, e);
    if (bb->ub == a)
        return 1;
    if (!((bb->lb == jl_bottom_type && !jl_is_type(a) && !jl_is_typevar(a)) || subtype_ufirst(bb->lb, a, e)))
        return 0;
    // for contravariance we would need to compute a meet here, but
    // because of invariance bb.ub ⊓ a == a here always. however for this
    // to work we need to compute issub(left,right) before issub(right,left),
    // since otherwise the issub(a, bb.ub) check in var_gt becomes vacuous.
    if (e->intersection) {
        jl_value_t *ub = intersect_ufirst(bb->ub, a, e, bb->depth0);
        if (ub != (jl_value_t*)b)
            bb->ub = ub;
    }
    else {
        bb->ub = a;  // meet(bb->ub, a)
    }
    assert(bb->ub != (jl_value_t*)b);
    if (jl_is_typevar(a)) {
        jl_varbinding_t *aa = lookup(e, (jl_tvar_t*)a);
        if (aa && !aa->right && in_union(bb->lb, a) && bb->depth0 != aa->depth0 && var_outside(e, b, (jl_tvar_t*)a)) {
            // an "exists" var cannot equal a "forall" var inside it unless the forall
            // var has equal bounds.
            return subtype_ufirst(aa->ub, aa->lb, e);
        }
    }
    return 1;
}

// check that type var `b` is >: `a`, and update b's lower bound.
static int var_gt(jl_tvar_t *b, jl_value_t *a, jl_stenv_t *e, int param)
{
    jl_varbinding_t *bb = lookup(e, b);
    if (bb == NULL)
        return e->ignore_free || subtype_ufirst(a, b->lb, e);
    record_var_occurrence(bb, e, param);
    if (!bb->right)  // check ∀b . b>:a
        return subtype_ufirst(a, bb->lb, e);
    if (!((bb->ub == (jl_value_t*)jl_any_type && !jl_is_type(a) && !jl_is_typevar(a)) || subtype_ufirst(a, bb->ub, e)))
        return 0;
    bb->lb = simple_join(bb->lb, a);
    assert(bb->lb != (jl_value_t*)b);
    return 1;
}

// check that a type is concrete. this is used to check concrete typevars;
// issubtype is false if the lower bound of a concrete type var is not concrete.
static int is_leaf_bound(jl_value_t *v)
{
    if (v == jl_bottom_type) return 1;
    if (jl_is_datatype(v)) {
        if (((jl_datatype_t*)v)->isleaftype) return 1;
        if (((jl_datatype_t*)v)->abstract) {
            if (jl_is_type_type(v))
                return 1;//!jl_has_free_typevars(jl_tparam0(v));
            return 0;
        }
        jl_svec_t *t = ((jl_datatype_t*)v)->parameters;
        size_t l = jl_svec_len(t);
        if (((jl_datatype_t*)v)->name == jl_tuple_typename) {
            for(int i=0; i < l; i++) {
                if (!is_leaf_bound(jl_svecref(t,i)))
                    return 0;
            }
        }
        return 1;
    }
    return 0;
}

// compare UnionAll type `u` to `t`. `R==1` if `u` came from the right side of A <: B.
static int subtype_unionall(jl_value_t *t, jl_unionall_t *u, jl_stenv_t *e, int8_t R, int param)
{
    jl_varbinding_t *btemp = e->vars;
    // if the var for this unionall (based on identity) already appears somewhere
    // in the environment, rename to get a fresh var.
    while (btemp != NULL) {
        if (btemp->var == u->var || jl_has_typevar(btemp->lb, u->var) ||
            jl_has_typevar(btemp->ub, u->var)) {
            u = rename_unionall(u);
            break;
        }
        btemp = btemp->prev;
    }
    jl_varbinding_t vb = { u->var, u->var->lb, u->var->ub, R, NULL, 0, 0, 0, 0, e->invdepth, 0, e->vars };
    JL_GC_PUSH2(&u, &vb.lb);
    e->vars = &vb;
    int ans;
    if (R) {
        e->envidx++;
        ans = subtype(t, u->body, e, param);
        e->envidx--;
        // fill variable values into `envout` up to `envsz`
        if (e->envidx < e->envsz) {
            jl_value_t *val;
            if (!vb.occurs_inv && vb.lb != jl_bottom_type)
                val = is_leaf_bound(vb.lb) ? vb.lb : (jl_value_t*)jl_new_typevar(u->var->name, jl_bottom_type, vb.lb);
            else if (vb.lb == vb.ub)
                val = vb.lb;
            else if (vb.lb != jl_bottom_type)
                // TODO: for now return the least solution, which is what
                // method parameters expect.
                val = vb.lb;
            else if (vb.lb == u->var->lb && vb.ub == u->var->ub)
                val = (jl_value_t*)u->var;
            else
                val = (jl_value_t*)jl_new_typevar(u->var->name, vb.lb, vb.ub);
            // widen Type{x} to typeof(x) in argument position
            if (jl_is_type_type(val) && !vb.occurs_inv && !jl_is_typevar(jl_tparam0(val)))
                val = jl_typeof(jl_tparam0(val));
            e->envout[e->envidx] = val;
        }
    }
    else {
        ans = subtype(u->body, t, e, param);
    }

    // handle the "diagonal dispatch" rule, which says that a type var occurring more
    // than once, and only in covariant position, is constrained to concrete types. E.g.
    //  ( Tuple{Int, Int}    <: Tuple{T, T} where T) but
    // !( Tuple{Int, String} <: Tuple{T, T} where T)
    // Then check concreteness by checking that the lower bound is not an abstract type.
    if (ans && (vb.concrete || (!vb.occurs_inv && vb.occurs_cov > 1))) {
        if (jl_is_typevar(vb.lb)) {
            // TODO test case that demonstrates the need for this?
            /*
            jl_tvar_t *v = (jl_tvar_t*)vb.lb;
            jl_varbinding_t *vlb = lookup(e, v);
            if (vlb)
                vlb->concrete = 1;
            else  // TODO handle multiple variables in vb.concretevar
                ans = (v == vb.concretevar);
            */
        }
        else if (!is_leaf_bound(vb.lb)) {
            ans = 0;
        }
        if (ans) {
            // if we occur as another var's lower bound, record the fact that we
            // were concrete so that subtype can return true for that var.
            /*
            btemp = vb.prev;
            while (btemp != NULL) {
                if (btemp->lb == (jl_value_t*)u->var)
                    btemp->concretevar = u->var;
                btemp = btemp->prev;
            }
            */
        }
    }

    e->vars = vb.prev;

    btemp = e->vars;
    while (btemp != NULL) {
        jl_value_t *vi = btemp->ub;
        if (vi != (jl_value_t*)vb.var && jl_has_typevar(vi, vb.var)) {
            btemp->ub = jl_new_struct(jl_unionall_type, vb.var, vi);
            btemp->lb = jl_bottom_type;
        }
        btemp = btemp->prev;
    }

    JL_GC_POP();
    return ans;
}

// unwrap <=2 layers of UnionAlls, leaving the vars in *p1 and *p2 and returning the body
static jl_value_t *unwrap_2_unionall(jl_value_t *t, jl_tvar_t **p1, jl_tvar_t **p2)
{
    if (jl_is_unionall(t)) {
        *p1 = ((jl_unionall_t*)t)->var;
        t = ((jl_unionall_t*)t)->body;
        if (jl_is_unionall(t)) {
            *p2 = ((jl_unionall_t*)t)->var;
            t = ((jl_unionall_t*)t)->body;
        }
    }
    return t;
}

// check n <: (length of vararg type v)
static int check_vararg_length(jl_value_t *v, ssize_t n, jl_stenv_t *e)
{
    jl_tvar_t *va_p1=NULL, *va_p2=NULL;
    jl_value_t *tail = unwrap_2_unionall(v, &va_p1, &va_p2);
    assert(jl_is_datatype(tail));
    jl_value_t *N = jl_tparam1(tail);
    // only do the check if N is free in the tuple type's last parameter
    if (N != (jl_value_t*)va_p1 && N != (jl_value_t*)va_p2) {
        jl_value_t *nn = jl_box_long(n);
        JL_GC_PUSH1(&nn);
        e->invdepth++;
        int ans = subtype(nn, N, e, 2) && subtype(N, nn, e, 0);
        e->invdepth--;
        JL_GC_POP();
        if (!ans)
            return 0;
    }
    return 1;
}

static int subtype_tuple(jl_datatype_t *xd, jl_datatype_t *yd, jl_stenv_t *e, int param)
{
    size_t lx = jl_nparams(xd), ly = jl_nparams(yd);
    if (lx == 0 && ly == 0)
        return 1;
    if (ly == 0)
        return 0;
    size_t i=0, j=0;
    int vx=0, vy=0;
    param = (param == 0 ? 1 : param);
    while (i < lx) {
        if (j >= ly) return 0;
        jl_value_t *xi = jl_tparam(xd, i), *yi = jl_tparam(yd, j);
        if (jl_is_vararg_type(xi)) vx = 1;
        if (jl_is_vararg_type(yi)) vy = 1;
        if (vx && !vy)
            return 0;
        if (!vx && vy) {
            jl_tvar_t *p1=NULL, *p2=NULL;
            yi = jl_tparam0(unwrap_2_unionall(yi, &p1, &p2));
            if (yi == (jl_value_t*)p1 || yi == (jl_value_t*)p2)
                yi = ((jl_tvar_t*)yi)->ub;
            if (!subtype(xi, yi, e, param))
                return 0;
        }
        else {
            if (!subtype(xi, yi, e, param))
                return 0;
        }
        i++;
        if (j < ly-1 || !vy)
            j++;
    }
    // TODO: handle Vararg with explicit integer length parameter
    vy = vy || (j < ly && jl_is_vararg_type(jl_tparam(yd,j)));
    if (vy && !vx && lx+1 >= ly) {
        // in Tuple{...,tn} <: Tuple{...,Vararg{T,N}}, check (lx+1-ly) <: N
        if (!check_vararg_length(jl_tparam(yd,ly-1), lx+1-ly, e))
            return 0;
    }
    return (lx==ly && vx==vy) || (vy && (lx >= (vx ? ly : (ly-1))));
}

// `param` means we are currently looking at a parameter of a type constructor
// (as opposed to being outside any type constructor, or comparing variable bounds).
// this is used to record the positions where type variables occur for the
// diagonal rule (record_var_occurrence).
static int subtype(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int param)
{
    if (x == jl_ANY_flag) x = (jl_value_t*)jl_any_type;
    if (y == jl_ANY_flag) y = (jl_value_t*)jl_any_type;
    if (jl_is_typevar(x)) {
        if (jl_is_typevar(y)) {
            if (x == y) return 1;
            jl_varbinding_t *xx = lookup(e, (jl_tvar_t*)x);
            jl_varbinding_t *yy = lookup(e, (jl_tvar_t*)y);
            int xr = xx && xx->right;  // treat free variables as "forall" (left)
            int yr = yy && yy->right;
            if (xr) {
                if (yy) record_var_occurrence(yy, e, param);
                if (yr) {
                    if (xx) record_var_occurrence(xx, e, param);
                    return subtype(xx->lb, yy->ub, e, 0);
                }
                return var_lt((jl_tvar_t*)x, y, e, param);
            }
            else if (yr) {
                if (xx) record_var_occurrence(xx, e, param);
                return var_gt((jl_tvar_t*)y, x, e, param);
            }
            jl_value_t *xub = xx ? xx->ub : ((jl_tvar_t*)x)->ub;
            jl_value_t *ylb = yy ? yy->lb : ((jl_tvar_t*)y)->lb;
            // check ∀x,y . x<:y
            // the bounds of left-side variables never change, and can only lead
            // to other left-side variables, so using || here is safe.
            return subtype(xub, y, e, param) || subtype(x, ylb, e, param);
        }
        return var_lt((jl_tvar_t*)x, y, e, param);
    }
    if (jl_is_typevar(y))
        return var_gt((jl_tvar_t*)y, x, e, param);
    if (jl_is_uniontype(x)) {
        if (x == y) return 1;
        if (param == 2) {
            // in invariant context both parts must match in the same environment.
            // TODO: don't double-count `y` with `param`
            return subtype(((jl_uniontype_t*)x)->a, y, e, param) &&
                subtype(((jl_uniontype_t*)x)->b, y, e, param);
        }
        return subtype_union(y, (jl_uniontype_t*)x, e, 0, param);
    }
    if (jl_is_uniontype(y)) {
        if (x == ((jl_uniontype_t*)y)->a || x == ((jl_uniontype_t*)y)->b)
            return 1;
        if (jl_is_unionall(x))
            return subtype_unionall(y, (jl_unionall_t*)x, e, 0, param);
        return subtype_union(x, (jl_uniontype_t*)y, e, 1, param);
    }
    // handle forall ("left") vars first
    if (jl_is_unionall(x)) {
        if (x == y && !(e->envidx < e->envsz))
            return 1;
        return subtype_unionall(y, (jl_unionall_t*)x, e, 0, param);
    }
    if (jl_is_unionall(y))
        return subtype_unionall(x, (jl_unionall_t*)y, e, 1, param);
    if (jl_is_datatype(x) && jl_is_datatype(y)) {
        if (x == y) return 1;
        if (y == (jl_value_t*)jl_any_type) return 1;
        jl_datatype_t *xd = (jl_datatype_t*)x, *yd = (jl_datatype_t*)y;
        if (jl_is_type_type(x) && !jl_is_type_type(y)) {
            if (!jl_is_typevar(jl_tparam0(xd))) {
                // TODO this is not strictly correct, but we don't yet have any other way for
                // e.g. the argument `Int` to match a `::DataType` slot. Most correct would be:
                // Int isa DataType, Int isa Type{Int}, Type{Int} more specific than DataType,
                // !(Type{Int} <: DataType), !isleaftype(Type{Int}), because non-DataTypes can
                // be type-equal to `Int`.
                return jl_typeof(jl_tparam0(xd)) == (jl_value_t*)yd;
            }
            return 0;
        }
        if (jl_is_type_type(y) && !jl_is_type_type(x)) {
            jl_value_t *tp0 = jl_tparam0(yd);
            if (!jl_is_typevar(tp0))
                return 0;
            if (!is_kind(x)) return 0;
            jl_varbinding_t *yy = lookup(e, (jl_tvar_t*)tp0);
            jl_value_t *ub = yy ? yy->ub : ((jl_tvar_t*)tp0)->ub;
            int ans;
            if (ub == (jl_value_t*)jl_any_type) {
                ans = subtype((jl_value_t*)jl_type_type, y, e, param);
            }
            else {
                e->invdepth++;
                ans = subtype(x, jl_tparam0(yd), e, 2) && subtype(jl_tparam0(yd), x, e, 0);
                e->invdepth--;
            }
            return ans;
        }
        while (xd != jl_any_type && xd->name != yd->name) {
            if (xd->super == NULL)
                jl_errorf("circular type parameter constraint in definition of %s", jl_symbol_name(xd->name->name));
            xd = xd->super;
        }
        if (xd == jl_any_type) return 0;
        if (jl_is_tuple_type(xd))
            return subtype_tuple(xd, yd, e, param);
        if (jl_is_vararg_type((jl_value_t*)xd)) {
            // Vararg: covariant in first parameter, invariant in second
            jl_value_t *xp1=jl_tparam0(xd), *xp2=jl_tparam1(xd), *yp1=jl_tparam0(yd), *yp2=jl_tparam1(yd);
            // in Vararg{T1} <: Vararg{T2}, need to check subtype twice to
            // simulate the possibility of multiple arguments, which is needed
            // to implement the diagonal rule correctly.
            if (!subtype(xp1, yp1, e, 1)) return 0;
            if (!subtype(xp1, yp1, e, 1)) return 0;
            // Vararg{T,N} <: Vararg{T2,N2}; equate N and N2
            e->invdepth++;
            int ans = subtype(xp2, yp2, e, 2) && subtype(yp2, xp2, e, 0);
            e->invdepth--;
            return ans;
        }
        size_t i, np = jl_nparams(xd);
        int ans = 1;
        e->invdepth++;
        for (i=0; i < np; i++) {
            jl_value_t *xi = jl_tparam(xd, i), *yi = jl_tparam(yd, i);
            if (!(xi == yi || (subtype(xi, yi, e, 2) && subtype(yi, xi, e, 0)))) {
                ans = 0; break;
            }
        }
        e->invdepth--;
        return ans;
    }
    if (jl_is_type(y))
        return x == jl_bottom_type;
    return x == y || jl_egal(x, y);
}

static int exists_subtype(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int8_t anyunions, jl_value_t *saved, jl_savedenv_t *se)
{
    int exists;
    for (exists=0; exists <= anyunions; exists++) {
        if (e->Runions.stacksize > 0)
            statestack_set(&e->Runions, e->Runions.stacksize-1, exists);
        e->Lunions.depth = e->Runions.depth = 0;
        e->Lunions.more = e->Runions.more = 0;
        int found = subtype(x, y, e, 0);
        if (e->Lunions.more) {
            // If another "forall" decision is found while inside the "exists"
            // loop, return up to forall_exists_subtype to add it to the "forall"
            // loop. This gives the recursion the following shape, instead of
            // simply nesting on each new decision point:
            // ∀₁         ∀₁
            //   ∃₁  =>     ∀₂
            //                ...
            //                ∃₁
            //                  ∃₂
            return 1;
        }
        if (e->Runions.more) {
            statestack_push(&e->Runions, 0);
            found = exists_subtype(x, y, e, 1, saved, se);
            statestack_pop(&e->Runions);
        }
        else {
            if (!found) restore_env(e, saved, se);
        }
        if (found) return 1;
    }
    return 0;
}

static int forall_exists_subtype(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int8_t anyunions)
{
    jl_value_t *saved=NULL; jl_savedenv_t se;
    JL_GC_PUSH1(&saved);
    int forall;
    for (forall=0; forall <= anyunions; forall++) {
        if (e->Lunions.stacksize > 0)
            statestack_set(&e->Lunions, e->Lunions.stacksize-1, forall);
        save_env(e, &saved, &se);
        if (!exists_subtype(x, y, e, 0, saved, &se)) {
            free(se.buf);
            JL_GC_POP();
            return 0;
        }
        if (e->Lunions.more) {
            restore_env(e, saved, &se);
            statestack_push(&e->Lunions, 0);
            int sub = forall_exists_subtype(x, y, e, 1);
            statestack_pop(&e->Lunions);
            if (!sub) {
                free(se.buf);
                JL_GC_POP();
                return 0;
            }
        }
        free(se.buf);
    }
    JL_GC_POP();
    return 1;
}

static void init_stenv(jl_stenv_t *e, jl_value_t **env, int envsz)
{
    e->vars = NULL;
    assert(env != NULL || envsz == 0);
    e->envsz = envsz;
    e->envout = env;
    e->envidx = 0;
    e->invdepth = 0;
    e->ignore_free = 0;
    e->intersection = 0;
    e->Lunions.depth = 0;      e->Runions.depth = 0;
    e->Lunions.more = 0;       e->Runions.more = 0;
    e->Lunions.stacksize = 0;  e->Runions.stacksize = 0;
}

// subtyping entry points

JL_DLLEXPORT int jl_subtype_env_size(jl_value_t *t)
{
    int sz = 0;
    while (jl_is_unionall(t)) {
        sz++;
        t = ((jl_unionall_t*)t)->body;
    }
    return sz;
}

// `env` is NULL if no typevar information is requested, or otherwise
// points to a rooted array of length `jl_subtype_env_size(y)`.
// This will be populated with the values of variables from unionall
// types at the outer level of `y`.
JL_DLLEXPORT int jl_subtype_env(jl_value_t *x, jl_value_t *y, jl_value_t **env, int envsz)
{
    jl_stenv_t e;
    init_stenv(&e, env, envsz);
    return forall_exists_subtype(x, y, &e, 0);
}

static int subtype_in_env(jl_value_t *x, jl_value_t *y, jl_stenv_t *e)
{
    jl_stenv_t e2;
    init_stenv(&e2, NULL, 0);
    e2.vars = e->vars;
    e2.intersection = e->intersection;
    e2.ignore_free = e->ignore_free;
    e2.envsz = e->envsz;
    e2.envout = e->envout;
    return forall_exists_subtype(x, y, &e2, 0);
}

JL_DLLEXPORT int jl_subtype(jl_value_t *x, jl_value_t *y)
{
    if (y == (jl_value_t*)jl_any_type || x == jl_bottom_type || x == y)
        return 1;
    return jl_subtype_env(x, y, NULL, 0);
}

JL_DLLEXPORT int jl_types_equal(jl_value_t *a, jl_value_t *b)
{
    if (obviously_egal(a, b))    return 1;
    if (obviously_unequal(a, b)) return 0;
    return jl_subtype(a, b) && jl_subtype(b, a);
}

int jl_tuple_isa(jl_value_t **child, size_t cl, jl_datatype_t *pdt)
{
    // TODO jb/subtype avoid allocation
    jl_value_t *tu = (jl_value_t*)arg_type_tuple(child, cl);
    int ans;
    JL_GC_PUSH1(&tu);
    ans = jl_subtype(tu, (jl_value_t*)pdt);
    JL_GC_POP();
    return ans;
}

JL_DLLEXPORT int jl_isa(jl_value_t *x, jl_value_t *t)
{
    if (jl_typeis(x,t) || t == (jl_value_t*)jl_any_type)
        return 1;
    if (jl_is_type(x)) {
        if (t == (jl_value_t*)jl_type_type)
            return 1;
        if (!jl_has_free_typevars(x)) {
            if (jl_is_leaf_type(t)) {
                if (jl_is_type_type(t))
                    return jl_types_equal(x, jl_tparam0(t));
                return 0;
            }
            JL_GC_PUSH1(&x);
            x = (jl_value_t*)jl_wrap_Type(x);  // TODO jb/subtype avoid jl_wrap_Type
            int ans = jl_subtype(x, t);
            JL_GC_POP();
            return ans;
        }
    }
    if (jl_is_leaf_type(t))
        return 0;
    return jl_subtype(jl_typeof(x), t);
}

// type intersection

static jl_value_t *intersect(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int param);

static jl_value_t *intersect_union(jl_value_t *x, jl_uniontype_t *u, jl_stenv_t *e, int8_t R, int param)
{
    if (param == 2) {
        jl_value_t *a=NULL, *b=NULL, *save=NULL; jl_savedenv_t se;
        JL_GC_PUSH3(&a, &b, &save);
        save_env(e, &save, &se);
        a = R ? intersect(x, u->a, e, param) : intersect(u->a, x, e, param);
        restore_env(e, NULL, &se);
        b = R ? intersect(x, u->b, e, param) : intersect(u->b, x, e, param);
        free(se.buf);
        jl_value_t *i = simple_join(a,b);
        JL_GC_POP();
        return i;
    }
    jl_unionstate_t *state = &e->Runions;
    if (state->depth >= state->stacksize) {
        state->more = 1;
        return jl_bottom_type;
    }
    int ui = statestack_get(state, state->depth);
    state->depth++;
    jl_value_t *choice = ui==0 ? u->a : u->b;
    // try all possible choices in covariant position; union them all together at the top level
    return R ? intersect(x, choice, e, param) : intersect(choice, x, e, param);
}

static jl_value_t *intersect_ufirst(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int depth)
{
    jl_value_t *res;
    int savedepth = e->invdepth;
    e->invdepth = depth;
    if (jl_is_uniontype(x) && jl_is_typevar(y))
        res = intersect_union(y, (jl_uniontype_t*)x, e, 0, 0);
    else if (jl_is_typevar(x) && jl_is_uniontype(y))
        res = intersect_union(x, (jl_uniontype_t*)y, e, 1, 0);
    else
        res = intersect(x, y, e, 0);
    e->invdepth = savedepth;
    return res;
}

// set a variable to a non-type constant
static jl_value_t *set_var_to_const(jl_varbinding_t *bb, jl_value_t *v, jl_varbinding_t *othervar)
{
    int offset = bb->offset;
    if (othervar && offset == 0)
        offset = -othervar->offset;
    assert(!othervar || othervar->offset == -offset);
    if (bb->lb == jl_bottom_type && bb->ub == (jl_value_t*)jl_any_type) {
        if (jl_is_long(v))
            v = jl_box_long(jl_unbox_long(v) + offset);
        bb->lb = bb->ub = v;
    }
    else if (jl_is_long(v) && jl_is_long(bb->lb)) {
        if (jl_unbox_long(v) + offset != jl_unbox_long(bb->lb))
            return jl_bottom_type;
    }
    else if (!jl_egal(v, bb->lb)) {
        return jl_bottom_type;
    }
    return v;
}

static jl_value_t *intersect_var(jl_tvar_t *b, jl_value_t *a, jl_stenv_t *e, int8_t R, int param)
{
    jl_varbinding_t *bb = lookup(e, b);
    if (bb == NULL)
        return R ? intersect_ufirst(a, b->ub, e, 0) : intersect_ufirst(b->ub, a, e, 0);
    if (!jl_is_type(a) && !jl_is_typevar(a))
        return set_var_to_const(bb, a, NULL);
    int d = bb->depth0;
    if (param == 2) {
        if (bb->lb == bb->ub && jl_is_typevar(bb->lb))
            return intersect(a, bb->ub, e, param);
        jl_value_t *ub = R ? intersect_ufirst(a, bb->ub, e, d) : intersect_ufirst(bb->ub, a, e, d);
        if (!subtype_in_env(bb->lb, a, e))
            return jl_bottom_type;
        if (ub != (jl_value_t*)b) {
            bb->ub = ub;
            bb->lb = ub;
        }
        return ub;
    }
    else if (bb->constraintkind == 0) {
        return R ? intersect_ufirst(a, bb->ub, e, d) : intersect_ufirst(bb->ub, a, e, d);
    }
    else if (bb->concrete || bb->constraintkind == 1) {
        jl_value_t *ub = R ? intersect_ufirst(a, bb->ub, e, d) : intersect_ufirst(bb->ub, a, e, d);
        if (ub == jl_bottom_type || !subtype_in_env(bb->lb, a, e))
            return jl_bottom_type;
        if (ub != (jl_value_t*)b)
            bb->ub = ub;
        return (jl_value_t*)b;
    }
    else if (bb->constraintkind == 2) {
        if (!subtype_in_env(a, bb->ub, e))
            return jl_bottom_type;
        jl_value_t *lb = simple_join(bb->lb, a);
        if (lb != (jl_value_t*)b)
            bb->lb = lb;
        return a;
    }
    assert(bb->constraintkind == 3);
    jl_value_t *ub = R ? intersect_ufirst(a, bb->ub, e, d) : intersect_ufirst(bb->ub, a, e, d);
    if (ub == jl_bottom_type)
        return jl_bottom_type;
    if (jl_is_typevar(a))
        return (jl_value_t*)b;
    if (ub == a) {
        bb->ub = ub;
        return (jl_value_t*)b;
    }
    jl_value_t *root; jl_savedenv_t se;
    JL_GC_PUSH2(&root, &ub);
    save_env(e, &root, &se);
    jl_value_t *ii = R ? intersect_ufirst(a, bb->lb, e, d) : intersect_ufirst(bb->lb, a, e, d);
    if (ii == jl_bottom_type) {
        restore_env(e, root, &se);
        ii = (jl_value_t*)b;
        if (ub != (jl_value_t*)b)
            bb->ub = ub;
    }
    free(se.buf);
    JL_GC_POP();
    return ii;
}

static int var_occurs_invariant(jl_value_t *v, jl_tvar_t *var, int inv)
{
    if (v == (jl_value_t*)var) {
        return inv;
    }
    else if (jl_is_uniontype(v)) {
        return var_occurs_invariant(((jl_uniontype_t*)v)->a, var, inv) ||
            var_occurs_invariant(((jl_uniontype_t*)v)->b, var, inv);
    }
    else if (jl_is_unionall(v)) {
        jl_unionall_t *ua = (jl_unionall_t*)v;
        if (ua->var == var)
            return 0;
        if (var_occurs_invariant(ua->var->lb, var, inv) || var_occurs_invariant(ua->var->ub, var, inv))
            return 1;
        return var_occurs_invariant(ua->body, var, inv);
    }
    else if (jl_is_datatype(v)) {
        size_t i;
        int invar = inv || !jl_is_tuple_type(v);
        for (i=0; i < jl_nparams(v); i++) {
            if (var_occurs_invariant(jl_tparam(v,i), var, invar))
                return 1;
        }
    }
    return 0;
}

static jl_value_t *finish_unionall(jl_value_t *res, jl_varbinding_t *vb, jl_stenv_t *e)
{
    jl_value_t *varval = NULL, *root = NULL;
    JL_GC_PUSH2(&res, &root);
    // try to reduce var to a single value
    if (obviously_egal(vb->lb, vb->ub)) {
        // given x<:T<:x, substitute x for T
        varval = vb->ub;
    }
    else if (!var_occurs_invariant(res, vb->var, 0) && is_leaf_bound(vb->ub)) {
        // replace T<:x with x in covariant position when possible
        varval = vb->ub;
    }

    // remove/replace/rewrap free occurrences of this var in the environment
    jl_varbinding_t *btemp = e->vars;
    while (btemp != NULL) {
        if (jl_has_typevar(btemp->lb, vb->var)) {
            if (varval)
                btemp->lb = jl_substitute_var(btemp->lb, vb->var, varval);
            else if (btemp->lb == (jl_value_t*)vb->var)
                btemp->lb = vb->lb;
            else
                btemp->lb = jl_new_struct(jl_unionall_type, vb->var, btemp->lb);
            assert((jl_value_t*)btemp->var != btemp->lb);
        }
        if (jl_has_typevar(btemp->ub, vb->var)) {
            if (varval)
                btemp->ub = jl_substitute_var(btemp->ub, vb->var, varval);
            else if (btemp->ub == (jl_value_t*)vb->var)
                btemp->ub = vb->ub;
            else
                btemp->ub = jl_new_struct(jl_unionall_type, vb->var, btemp->ub);
            assert((jl_value_t*)btemp->var != btemp->ub);
        }
        btemp = btemp->prev;
    }

    // if `v` still occurs, re-wrap body in `UnionAll v` or eliminate the UnionAll
    if (jl_has_typevar(res, vb->var)) {
        res = jl_new_struct(jl_unionall_type, vb->var, res);
        if (varval) {
            JL_TRY {
                // you can construct `T{x} where x` even if T's parameter is actually
                // limited. in that case we might get an invalid instantiation here.
                res = jl_instantiate_unionall((jl_unionall_t*)res, varval);
            }
            JL_CATCH {
                res = jl_bottom_type;
            }
        }
        else {
            root = (jl_value_t*)jl_new_typevar(vb->var->name, vb->lb, vb->ub);
            res = jl_instantiate_unionall((jl_unionall_t*)res, root);
            res = jl_new_struct(jl_unionall_type, (jl_tvar_t*)root, res);
        }
    }
    JL_GC_POP();
    return res;
}

static jl_value_t *intersect_unionall_(jl_value_t *t, jl_unionall_t *u, jl_stenv_t *e, int8_t R, int param, jl_varbinding_t *vb)
{
    jl_varbinding_t *btemp = e->vars;
    // if the var for this unionall (based on identity) already appears somewhere
    // in the environment, rename to get a fresh var.
    // TODO: might need to look inside types in btemp->lb and btemp->ub
    while (btemp != NULL) {
        if (btemp->var == u->var || btemp->lb == (jl_value_t*)u->var ||
            btemp->ub == (jl_value_t*)u->var) {
            u = rename_unionall(u);
            break;
        }
        btemp = btemp->prev;
    }
    JL_GC_PUSH1(&u);
    vb->var = u->var;
    e->vars = vb;
    jl_value_t *res = R ? intersect(t, u->body, e, param) : intersect(u->body, t, e, param);
    vb->concrete |= (!vb->occurs_inv && vb->occurs_cov > 1);
    /*
    if (R) {
        e->envidx++;
        ans = subtype(t, u->body, e, param);
        e->envidx--;
        // fill variable values into `envout` up to `envsz`
        if (e->envidx < e->envsz) {
            jl_value_t *val;
            if (vb->lb == vb->ub)
                val = vb->lb;
            else if (vb->lb != jl_bottom_type)
                // TODO: for now return the least solution, which is what
                // method parameters expect.
                val = vb->lb;
            else if (vb->lb == u->var->lb && vb->ub == u->var->ub)
                val = (jl_value_t*)u->var;
            else
                val = (jl_value_t*)jl_new_typevar(u->var->name, vb->lb, vb->ub);
            e->envout[e->envidx] = val;
        }
    }
    else {
        ans = subtype(u->body, t, e, param);
    }
    */

    // handle the "diagonal dispatch" rule, which says that a type var occurring more
    // than once, and only in covariant position, is constrained to concrete types. E.g.
    //  ( Tuple{Int, Int}    <: Tuple{T, T} where T) but
    // !( Tuple{Int, String} <: Tuple{T, T} where T)
    // Then check concreteness by checking that the lower bound is not an abstract type.
    if (res != jl_bottom_type && (vb->concrete || (!vb->occurs_inv && vb->occurs_cov > 1))) {
        if (jl_is_typevar(vb->lb)) {
        }
        else if (!is_leaf_bound(vb->lb)) {
            res = jl_bottom_type;
        }
    }

    e->vars = vb->prev;

    if (res != jl_bottom_type) {
        // fail on circular constraints
        if (jl_has_typevar(vb->lb, u->var) || jl_has_typevar(vb->ub, u->var))
            res = jl_bottom_type;
        // T=Bottom in covariant position
        if (vb->ub == jl_bottom_type && vb->occurs_cov)
            res = jl_bottom_type;
    }
    if (res != jl_bottom_type)
        res = finish_unionall(res, vb, e);
    JL_GC_POP();
    return res;
}

static jl_value_t *intersect_unionall(jl_value_t *t, jl_unionall_t *u, jl_stenv_t *e, int8_t R, int param)
{
    jl_value_t *res=NULL, *res2=NULL, *save=NULL, *save2=NULL;
    jl_savedenv_t se, se2;
    jl_varbinding_t vb = { u->var, u->var->lb, u->var->ub, R, NULL, 0, 0, 0, 0, e->invdepth, 0, e->vars };
    JL_GC_PUSH5(&res, &save2, &vb.lb, &vb.ub, &save);
    save_env(e, &save, &se);
    res = intersect_unionall_(t, u, e, R, param, &vb);
    if (res != jl_bottom_type && !e->Runions.more) {
        if (vb.concrete || vb.occurs_inv>1 || (vb.occurs_inv && vb.occurs_cov)) {
            restore_env(e, NULL, &se);
            vb.occurs_cov = vb.occurs_inv = 0;
            vb.constraintkind = 3;
            res = intersect_unionall_(t, u, e, R, param, &vb);
        }
        else if (vb.occurs_cov) {
            save_env(e, &save2, &se2);
            restore_env(e, save, &se);
            vb.occurs_cov = vb.occurs_inv = 0;
            vb.lb = u->var->lb; vb.ub = u->var->ub;
            vb.constraintkind = 2;
            res2 = intersect_unionall_(t, u, e, R, param, &vb);
            if (res2 == jl_bottom_type && !e->Runions.more) {
                restore_env(e, save, &se);
                vb.occurs_cov = vb.occurs_inv = 0;
                vb.lb = u->var->lb; vb.ub = u->var->ub;
                vb.constraintkind = 1;
                res2 = intersect_unionall_(t, u, e, R, param, &vb);
                if (res2 == jl_bottom_type)
                    restore_env(e, save2, &se2);
            }
            if (res2 != jl_bottom_type)
                res = res2;
            free(se2.buf);
        }
    }
    free(se.buf);
    JL_GC_POP();
    return res;
}

// check n = (length of vararg type v)
static int intersect_vararg_length(jl_value_t *v, ssize_t n, jl_stenv_t *e, int8_t R)
{
    jl_tvar_t *va_p1=NULL, *va_p2=NULL;
    jl_value_t *tail = unwrap_2_unionall(v, &va_p1, &va_p2);
    assert(jl_is_datatype(tail));
    jl_value_t *N = jl_tparam1(tail);
    // only do the check if N is free in the tuple type's last parameter
    if (jl_is_typevar(N) && N != (jl_value_t*)va_p1 && N != (jl_value_t*)va_p2) {
        jl_value_t *len = jl_box_long(n);
        jl_value_t *il = R ? intersect(len, N, e, 2) : intersect(N, len, e, 2);
        if (il == jl_bottom_type)
            return 0;
    }
    return 1;
}

static jl_value_t *intersect_tuple(jl_datatype_t *xd, jl_datatype_t *yd, jl_stenv_t *e, int param)
{
    size_t lx = jl_nparams(xd), ly = jl_nparams(yd);
    if (lx == 0 && ly == 0)
        return (jl_value_t*)yd;
    jl_svec_t *params = jl_alloc_svec(lx > ly ? lx : ly);
    jl_value_t *res=NULL;
    JL_GC_PUSH1(&params);
    size_t i=0, j=0;
    int vx=0, vy=0;
    jl_value_t *xi, *yi;
    while (1) {
        xi = i < lx ? jl_tparam(xd, i) : NULL;
        yi = j < ly ? jl_tparam(yd, j) : NULL;
        if (xi == NULL && yi == NULL) {
            assert(i == j && i == jl_svec_len(params));
            break;
        }
        if (xi && jl_is_vararg_type(xi)) vx = 1;
        if (yi && jl_is_vararg_type(yi)) vy = 1;
        if (xi == NULL || yi == NULL) {
            res = jl_bottom_type;
            if (vx && intersect_vararg_length(xi, ly+1-lx, e, 0))
                res = (jl_value_t*)jl_apply_tuple_type_v(jl_svec_data(params), j);
            if (vy && intersect_vararg_length(yi, lx+1-ly, e, 1))
                res = (jl_value_t*)jl_apply_tuple_type_v(jl_svec_data(params), i);
            break;
        }
        if (vx && !vy)
            xi = jl_unwrap_vararg(xi);
        if (vy && !vx)
            yi = jl_unwrap_vararg(yi);
        jl_varbinding_t *xb=NULL, *yb=NULL;
        if (vx && vy && lx != ly) {
            // {A^n...,Vararg{T,N}} ∩ {Vararg{S,M}} = {(A∩S)^n...,Vararg{T∩S,N}} plus N = M-n
            jl_value_t *xlen = jl_tparam1(jl_unwrap_unionall(xi));
            if (jl_is_typevar(xlen)) {
                xb = lookup(e, (jl_tvar_t*)xlen);
                if (xb)
                    xb->offset = ly-lx;
            }
            jl_value_t *ylen = jl_tparam1(jl_unwrap_unionall(yi));
            if (jl_is_typevar(ylen)) {
                yb = lookup(e, (jl_tvar_t*)ylen);
                if (yb)
                    yb->offset = lx-ly;
            }
        }
        jl_value_t *ii = intersect(xi, yi, e, param == 0 ? 1 : param);
        if (xb) xb->offset = 0;
        if (yb) yb->offset = 0;
        if (ii == jl_bottom_type) {
            if (vx && vy) {
                int len = i > j ? i : j;
                if ((xb && jl_is_long(xb->lb) && lx-1+jl_unbox_long(xb->lb) != len) ||
                    (yb && jl_is_long(yb->lb) && ly-1+jl_unbox_long(yb->lb) != len))
                    res = jl_bottom_type;
                else
                    res = (jl_value_t*)jl_apply_tuple_type_v(jl_svec_data(params), len);
            }
            else {
                res = jl_bottom_type;
            }
            break;
        }
        jl_svecset(params, (i > j ? i : j), ii);
        if (vx && vy)
            break;
        if (i < lx-1 || !vx) i++;
        if (j < ly-1 || !vy) j++;
    }
    // TODO: handle Vararg with explicit integer length parameter
    if (res == NULL)
        res = (jl_value_t*)jl_apply_tuple_type(params);
    JL_GC_POP();
    return res;
}

static void flip_vars(jl_stenv_t *e)
{
    jl_varbinding_t *btemp = e->vars;
    while (btemp != NULL) {
        btemp->right = !btemp->right;
        btemp = btemp->prev;
    }
}

// intersection where xd nominally inherits from yd
static jl_value_t *intersect_sub_datatype(jl_datatype_t *xd, jl_datatype_t *yd, jl_stenv_t *e, int R, int param)
{
    jl_value_t *isuper = R ? intersect((jl_value_t*)yd, (jl_value_t*)xd->super, e, param) :
                             intersect((jl_value_t*)xd->super, (jl_value_t*)yd, e, param);
    if (isuper == jl_bottom_type) return jl_bottom_type;
    if (jl_nparams(xd) == 0 || jl_nparams(xd->super) == 0)
        return (jl_value_t*)xd;
    jl_value_t *super_pattern=NULL;
    JL_GC_PUSH2(&isuper, &super_pattern);
    jl_value_t *wrapper = xd->name->wrapper;
    super_pattern = jl_rewrap_unionall((jl_value_t*)((jl_datatype_t*)jl_unwrap_unionall(wrapper))->super,
                                       wrapper);
    int envsz = jl_subtype_env_size(super_pattern);
    jl_value_t *ii = jl_bottom_type;
    {
        jl_value_t **env;
        JL_GC_PUSHARGS(env, envsz);
        jl_stenv_t tempe;
        init_stenv(&tempe, env, envsz);
        tempe.ignore_free = 1;
        if (subtype_in_env(isuper, super_pattern, &tempe)) {
            jl_value_t *wr = wrapper;
            int i;
            for(i=0; i<envsz; i++) {
                // if a parameter is not constrained by the supertype, use the original
                // parameter value from `x`. this is detected by the value in `env` being
                // the exact typevar from the type's `wrapper`.
                if (env[i] == (jl_value_t*)((jl_unionall_t*)wr)->var)
                    env[i] = jl_tparam(xd,i);
                wr = ((jl_unionall_t*)wr)->body;
            }
            ii = jl_apply_type(wrapper, env, envsz);
        }
        JL_GC_POP();
    }
    JL_GC_POP();
    return ii;
}

static jl_value_t *intersect_invariant(jl_value_t *x, jl_value_t *y, jl_stenv_t *e)
{
    if (!jl_has_free_typevars(x) && !jl_has_free_typevars(y)) {
        return (jl_subtype(x,y) && jl_subtype(y,x)) ? y : NULL;
    }
    e->invdepth++;
    jl_value_t *ii = intersect(x, y, e, 2);
    e->invdepth--;
    if (jl_is_typevar(x) && jl_is_typevar(y) && (jl_is_typevar(ii) || !jl_is_type(ii)))
        return ii;
    if (e->Runions.more)
        return NULL;
    if (ii == jl_bottom_type) {
        if (!subtype_in_env(x, ii, e))
            return NULL;
        flip_vars(e);
        if (!subtype_in_env(y, ii, e))
            ii = NULL;
        flip_vars(e);
        return ii;
    }
    jl_value_t *root=NULL;
    jl_savedenv_t se;
    JL_GC_PUSH2(&ii, &root);
    save_env(e, &root, &se);
    if (!subtype_in_env(x, y, e)) {
        ii = NULL;
    }
    else {
        flip_vars(e);
        if (!subtype_in_env(y, x, e))
            ii = NULL;
        flip_vars(e);
    }
    restore_env(e, root, &se);
    free(se.buf);
    JL_GC_POP();
    return ii;
}

// intersection where x == Type{...} and y is not
static jl_value_t *intersect_type_type(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int8_t R)
{
    jl_value_t *p0 = jl_tparam0(x);
    if (!jl_is_typevar(p0))
        return (jl_typeof(p0) == y) ? x : jl_bottom_type;
    if (!is_kind(y)) return jl_bottom_type;
    if (((jl_tvar_t*)p0)->ub == (jl_value_t*)jl_any_type)
        return y;
    return x;
    /*
    jl_value_t *ii = R ? intersect_invariant(y, jl_tparam0(x), e) : intersect_invariant(jl_tparam0(x), y, e);
    // NOTE: we cannot express e.g. DataType ∩ (UnionAll T<:Integer Type{T}), so returning `x`
    // here is a conservative over-estimate.
    if (ii == NULL || ii == jl_bottom_type) return x;
    if (ii == y) return ii;
    return (jl_value_t*)jl_wrap_Type(ii);
    */
}

// `param` means we are currently looking at a parameter of a type constructor
// (as opposed to being outside any type constructor, or comparing variable bounds).
// this is used to record the positions where type variables occur for the
// diagonal rule (record_var_occurrence).
static jl_value_t *intersect(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int param)
{
    if (x == y) return y;
    if (x == jl_ANY_flag) x = (jl_value_t*)jl_any_type;
    if (y == jl_ANY_flag) y = (jl_value_t*)jl_any_type;
    if (jl_is_typevar(x)) {
        if (jl_is_typevar(y)) {
            jl_varbinding_t *xx = lookup(e, (jl_tvar_t*)x);
            jl_varbinding_t *yy = lookup(e, (jl_tvar_t*)y);
            int R = 0;
            if (xx && yy && var_outside(e, (jl_tvar_t*)x, (jl_tvar_t*)y)) {
                // to preserve variable identities correctly, always accumulate bounds
                // on the outer variable, return the outer variable, and set the inner
                // variable equal to the outer variable.
                jl_value_t *temp; jl_varbinding_t *tvb;
                temp = x; x = y; y = temp;
                tvb = xx; xx = yy; yy = tvb;
                R = 1;
            }
            if (param == 2) {
                jl_value_t *xlb = xx ? xx->lb : ((jl_tvar_t*)x)->lb;
                jl_value_t *xub = xx ? xx->ub : ((jl_tvar_t*)x)->ub;
                jl_value_t *ylb = yy ? yy->lb : ((jl_tvar_t*)y)->lb;
                jl_value_t *yub = yy ? yy->ub : ((jl_tvar_t*)y)->ub;
                record_var_occurrence(xx, e, param);
                if (xx && yy && xx->depth0 != yy->depth0) {
                    record_var_occurrence(yy, e, param);
                    return subtype_in_env(yy->ub, yy->lb, e) ? y : jl_bottom_type;
                }
                if (xub == xlb && jl_is_typevar(xub)) {
                    if (y == xub) {
                        record_var_occurrence(yy, e, param);
                        return y;
                    }
                    return intersect(y, xub, e, param);
                }
                record_var_occurrence(yy, e, param);
                if (!jl_is_type(ylb) && !jl_is_typevar(ylb)) {
                    if (xx)
                        return set_var_to_const(xx, ylb, yy);
                    if ((xlb == jl_bottom_type && xub == (jl_value_t*)jl_any_type) || jl_egal(xlb, ylb))
                        return ylb;
                    return jl_bottom_type;
                }
                if (!jl_is_type(xlb) && !jl_is_typevar(xlb)) {
                    if (yy)
                        return set_var_to_const(yy, xlb, xx);
                    if (ylb == jl_bottom_type && yub == (jl_value_t*)jl_any_type)
                        return xlb;
                    return jl_bottom_type;
                }
                if (!(subtype_in_env(xlb, yub, e) && subtype_in_env(ylb, xub, e)))
                    return jl_bottom_type;
                jl_value_t *ub=NULL, *lb=NULL;
                JL_GC_PUSH2(&lb, &ub);
                ub = intersect_ufirst(xub, yub, e, xx ? xx->depth0 : 0);
                lb = simple_join(xlb, ylb);
                if (yy) {
                    if (lb != y)
                        yy->lb = lb;
                    if (ub != y)
                        yy->ub = ub;
                    assert(yy->ub != y);
                    assert(yy->lb != y);
                }
                if (xx) {
                    xx->lb = y;
                    xx->ub = y;
                    assert(xx->ub != x);
                }
                JL_GC_POP();
                return y;
            }
            record_var_occurrence(xx, e, param);
            record_var_occurrence(yy, e, param);
            if (xx && yy && xx->concrete && !yy->concrete) {
                return intersect_var((jl_tvar_t*)x, y, e, R, param);
            }
            return intersect_var((jl_tvar_t*)y, x, e, !R, param);
        }
        record_var_occurrence(lookup(e, (jl_tvar_t*)x), e, param);
        return intersect_var((jl_tvar_t*)x, y, e, 0, param);
    }
    if (jl_is_typevar(y)) {
        record_var_occurrence(lookup(e, (jl_tvar_t*)y), e, param);
        return intersect_var((jl_tvar_t*)y, x, e, 1, param);
    }
    if (!jl_has_free_typevars(x) && !jl_has_free_typevars(y)) {
        if (jl_subtype(x, y))
            return x;
        if (jl_subtype(y, x))
            return y;
    }
    if (jl_is_uniontype(x)) {
        if (y == ((jl_uniontype_t*)x)->a || y == ((jl_uniontype_t*)x)->b)
            return y;
        return intersect_union(y, (jl_uniontype_t*)x, e, 0, param);
    }
    if (jl_is_uniontype(y)) {
        if (x == ((jl_uniontype_t*)y)->a || x == ((jl_uniontype_t*)y)->b)
            return x;
        if (jl_is_unionall(x))
            return intersect_unionall(y, (jl_unionall_t*)x, e, 0, param);
        return intersect_union(x, (jl_uniontype_t*)y, e, 1, param);
    }
    if (jl_is_unionall(x)) {
        if (jl_is_unionall(y)) {
            jl_value_t *a=NULL, *b=jl_bottom_type, *res=NULL;
            JL_GC_PUSH2(&a,&b);
            jl_value_t *unused; jl_savedenv_t se;
            save_env(e, &unused, &se);
            a = intersect_unionall(y, (jl_unionall_t*)x, e, 0, param);
            if (jl_is_unionall(a)) {
                jl_unionall_t *ua = (jl_unionall_t*)a;
                if (jl_is_unionall(ua->body)) {
                    jl_unionall_t *ub = (jl_unionall_t*)ua->body;
                    if (jl_has_typevar(ub->var->ub, ua->var) ||
                        jl_has_typevar(ub->var->lb, ua->var)) {
                        restore_env(e, NULL, &se); // restore counts
                        b = intersect_unionall(x, (jl_unionall_t*)y, e, 1, param);
                    }
                }
            }
            free(se.buf);
            if (!jl_has_free_typevars(a) && !jl_has_free_typevars(b)) {
                if (jl_subtype(a, b))
                    res = b;
                else if (jl_subtype(b, a))
                    res = a;
            }
            if (!res) res = simple_join(a, b);
            JL_GC_POP();
            return res;
        }
        return intersect_unionall(y, (jl_unionall_t*)x, e, 0, param);
    }
    if (jl_is_unionall(y))
        return intersect_unionall(x, (jl_unionall_t*)y, e, 1, param);
    if (jl_is_datatype(x) && jl_is_datatype(y)) {
        jl_datatype_t *xd = (jl_datatype_t*)x, *yd = (jl_datatype_t*)y;
        if (param < 2) {
            if (x == (jl_value_t*)jl_any_type) return y;
            if (y == (jl_value_t*)jl_any_type) return x;
            if (jl_is_type_type(x)) {
                if (!jl_is_type_type(y))
                    return intersect_type_type(x, y, e, 0);
            }
            else if (jl_is_type_type(y)) {
                return intersect_type_type(y, x, e, 1);
            }
        }
        if (xd->name == yd->name) {
            if (jl_is_tuple_type(xd))
                return intersect_tuple(xd, yd, e, param);
            if (jl_is_vararg_type(x)) {
                // Vararg: covariant in first parameter, invariant in second
                jl_value_t *xp1=jl_tparam0(xd), *xp2=jl_tparam1(xd), *yp1=jl_tparam0(yd), *yp2=jl_tparam1(yd);
                // in Vararg{T1} <: Vararg{T2}, need to check subtype twice to
                // simulate the possibility of multiple arguments, which is needed
                // to implement the diagonal rule correctly.
                if (intersect(xp1, yp1, e, param==0 ? 1 : param) == jl_bottom_type)
                    return jl_bottom_type;
                jl_value_t *i2=NULL, *ii = intersect(xp1, yp1, e, 1);
                if (ii == jl_bottom_type) return jl_bottom_type;
                JL_GC_PUSH2(&ii, &i2);
                // Vararg{T,N} <: Vararg{T2,N2}; equate N and N2
                i2 = intersect_invariant(xp2, yp2, e);
                if (i2 == NULL || i2 == jl_bottom_type || (jl_is_long(i2) && jl_unbox_long(i2) < 0))
                    ii = jl_bottom_type;
                else
                    ii = jl_apply_type2((jl_value_t*)jl_vararg_type, ii, i2);
                JL_GC_POP();
                return ii;
            }
            size_t i, np = jl_nparams(xd);
            jl_value_t **newparams;
            JL_GC_PUSHARGS(newparams, np);
            for (i=0; i < np; i++) {
                jl_value_t *xi = jl_tparam(xd, i), *yi = jl_tparam(yd, i);
                jl_value_t *ii = intersect_invariant(xi, yi, e);
                if (ii == NULL)
                    break;
                newparams[i] = ii;
            }
            jl_value_t *res;
            if (i < np)
                res = jl_bottom_type;
            else
                res = jl_apply_type(xd->name->wrapper, newparams, np);
            JL_GC_POP();
            return res;
        }
        if (param == 2) return jl_bottom_type;
        while (xd != jl_any_type && xd->name != yd->name)
            xd = xd->super;
        if (xd == jl_any_type) {
            xd = (jl_datatype_t*)x;
            while (yd != jl_any_type && yd->name != xd->name)
                yd = yd->super;
            if (yd == jl_any_type)
                return jl_bottom_type;
            return intersect_sub_datatype((jl_datatype_t*)y, xd, e, 1, param);
        }
        return intersect_sub_datatype((jl_datatype_t*)x, yd, e, 0, param);
    }
    if (jl_egal(x, y)) return y;
    return jl_bottom_type;
}

static jl_value_t *intersect_all(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int8_t anyunions)
{
    int i;
    jl_value_t *is[2] = {jl_bottom_type,jl_bottom_type};
    JL_GC_PUSH2(&is[0], &is[1]);
    for (i=0; i <= anyunions; i++) {
        if (e->Runions.stacksize > 0)
            statestack_set(&e->Runions, e->Runions.stacksize-1, i);
        e->Runions.depth = 0;
        e->Runions.more = 0;
        jl_value_t *ii = intersect(x, y, e, 0);
        if (e->Runions.more) {
            statestack_push(&e->Runions, 0);
            ii = intersect_all(x, y, e, 1);
            statestack_pop(&e->Runions);
        }
        is[i] = ii;
    }
    jl_value_t *res = jl_type_union(is, 2);
    JL_GC_POP();
    return res;
}

// type intersection entry points

JL_DLLEXPORT jl_value_t *jl_intersect_types(jl_value_t *x, jl_value_t *y)
{
    jl_stenv_t e;
    init_stenv(&e, NULL, 0);
    e.intersection = 1;
    return intersect_all(x, y, &e, 0);
}

jl_value_t *jl_type_intersection_matching(jl_value_t *a, jl_value_t *b, jl_svec_t **penv)
{
    int szb = jl_subtype_env_size(b);
    int sz = 0, i = 0;
    jl_value_t **env, **ans;
    JL_GC_PUSHARGS(env, szb+1);
    ans = &env[szb]; *ans = jl_bottom_type;
    if (jl_subtype_env(a, b, env, szb)) {
        *ans = a; sz = szb;
    }
    else if (jl_subtype(b, a)) {
        *ans = b;
    }
    else {
        int lta = jl_is_leaf_type(a), ltb = jl_is_leaf_type(b);
        if (lta && ltb)
            goto bot;
        jl_stenv_t e;
        init_stenv(&e, NULL, 0);
        e.intersection = 1;
        *ans = intersect_all(a, b, &e, 0);
        if (*ans == jl_bottom_type) goto bot;
        // TODO: compute `env` directly during intersection
        //if (szb > 0 && jl_subtype_env(ans, b, env, szb))
        //    sz = szb;
        // TODO: don't yet use the types returned by `intersect`, since it returns
        // Unions of Tuples and other code can only handle direct Tuples.
        *ans = b;
    }
    if (sz == 0 && szb > 0) {
        while (jl_is_unionall(b)) {
            env[i++] = (jl_value_t*)((jl_unionall_t*)b)->var;
            b = ((jl_unionall_t*)b)->body;
        }
        sz = szb;
    }
    if (penv) {
        jl_svec_t *e = jl_alloc_svec(sz);
        *penv = e;
        for(i=0; i < sz; i++)
            jl_svecset(e, i, env[i]);
    }
 bot:
    JL_GC_POP();
    return *ans;
}

JL_DLLEXPORT jl_value_t *jl_type_intersection(jl_value_t *a, jl_value_t *b)
{
    return jl_type_intersection_matching(a, b, NULL);
}

JL_DLLEXPORT jl_svec_t *jl_type_intersection_env(jl_value_t *a, jl_value_t *b)
{
    jl_svec_t *env = jl_emptysvec;
    JL_GC_PUSH1(&env);
    jl_value_t *ti = jl_type_intersection_matching(a, b, &env);
    jl_svec_t *pair = jl_svec2(ti, env);
    JL_GC_POP();
    return pair;
}

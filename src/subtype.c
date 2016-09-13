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
    int depth0;         // # of invariant constructors nested around the UnionAll type for this var
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
        return subtype_union(y, x, e, 0, 0);
    if (jl_is_typevar(x) && jl_is_uniontype(y))
        return (x == ((jl_uniontype_t*)y)->a || x == ((jl_uniontype_t*)y)->b ||
                subtype_union(x, y, e, 1, 0));
    return subtype(x, y, e, 0);
}

// use the current context to record where a variable occurred, for the purpose
// of determining whether the variable is concrete.
static void record_var_occurrence(jl_varbinding_t *vb, jl_stenv_t *e, int param)
{
    if (param) {
        if (e->invdepth > vb->depth0)
            vb->occurs_inv = 1;
        else
            vb->occurs_cov++;
    }
}

// check that type var `b` is <: `a`, and update b's upper bound.
static int var_lt(jl_tvar_t *b, jl_value_t *a, jl_stenv_t *e, int param)
{
    jl_varbinding_t *bb = lookup(e, b);
    if (bb == NULL)
        return subtype_ufirst(b->ub, a, e);
    record_var_occurrence(bb, e, param);
    if (!bb->right)  // check ∀b . b<:a
        return subtype_ufirst(bb->ub, a, e);
    if (!((bb->lb == jl_bottom_type && !jl_is_type(a) && !jl_is_typevar(a)) || subtype_ufirst(bb->lb, a, e)))
        return 0;
    // for contravariance we would need to compute a meet here, but
    // because of invariance bb.ub ⊓ a == a here always. however for this
    // to work we need to compute issub(left,right) before issub(right,left),
    // since otherwise the issub(a, bb.ub) check in var_gt becomes vacuous.
    bb->ub = a;  // meet(bb->ub, a)
    return 1;
}

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
    if (jl_is_typevar(a)) return 0;
    return !jl_is_type(a) && jl_egal(a,b);
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
    return jl_new_struct(jl_uniontype_type, a, b);
}

// check that type var `b` is >: `a`, and update b's lower bound.
static int var_gt(jl_tvar_t *b, jl_value_t *a, jl_stenv_t *e, int param)
{
    jl_varbinding_t *bb = lookup(e, b);
    if (bb == NULL)
        return subtype_ufirst(a, b->lb, e);
    record_var_occurrence(bb, e, param);
    if (!bb->right)  // check ∀b . b>:a
        return subtype_ufirst(a, bb->lb, e);
    if (!((bb->ub == (jl_value_t*)jl_any_type && !jl_is_type(a) && !jl_is_typevar(a)) || subtype_ufirst(a, bb->ub, e)))
        return 0;
    bb->lb = simple_join(bb->lb, a);
    return 1;
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
    // TODO: might need to look inside types in btemp->lb and btemp->ub
    while (btemp != NULL) {
        if (btemp->var == u->var || btemp->lb == (jl_value_t*)u->var ||
            btemp->ub == (jl_value_t*)u->var) {
            u = rename_unionall(u);
            break;
        }
        btemp = btemp->prev;
    }
    jl_varbinding_t vb = { u->var, u->var->lb, u->var->ub, R, NULL, 0, 0, 0, e->invdepth, e->vars };
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
            if (vb.lb == vb.ub)
                val = vb.lb;
            else if (vb.lb != jl_bottom_type)
                // TODO: for now return the least solution, which is what
                // method parameters expect.
                val = vb.lb;
            else if (vb.lb == u->var->lb && vb.ub == u->var->ub)
                val = (jl_value_t*)u->var;
            else
                val = (jl_value_t*)jl_new_typevar(u->var->name, vb.lb, vb.ub);
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
            jl_tvar_t *v = (jl_tvar_t*)vb.lb;
            jl_varbinding_t *vlb = lookup(e, v);
            if (vlb)
                vlb->concrete = 1;
            else  // TODO handle multiple variables in vb.concretevar
                ans = (v == vb.concretevar);
        }
        else if (!is_leaf_bound(vb.lb)) {
            ans = 0;
        }
        if (ans) {
            // if we occur as another var's lower bound, record the fact that we
            // were concrete so that subtype can return true for that var.
            btemp = vb.prev;
            while (btemp != NULL) {
                if (btemp->lb == (jl_value_t*)u->var)
                    btemp->concretevar = u->var;
                btemp = btemp->prev;
            }
        }
    }

    e->vars = vb.prev;
    JL_GC_POP();
    return ans;
}

static jl_value_t *unwrap_2_unionall(jl_value_t *t, jl_value_t **p1, jl_value_t **p2)
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
    // in Tuple{...,tn} <: Tuple{...,Vararg{T,N}}, check (lx+1-ly) <: N
    jl_value_t *N = jl_tparam1(tail);
    // only do the check if N is free in the tuple type's last parameter
    if (N != (jl_value_t*)va_p1 && N != (jl_value_t*)va_p2) {
        if (!subtype(jl_box_long(n), N, e, 1))
            return 0;
    }
    return 1;
}

static int subtype_tuple(jl_datatype_t *xd, jl_datatype_t *yd, jl_stenv_t *e)
{
    size_t lx = jl_nparams(xd), ly = jl_nparams(yd);
    if (lx == 0 && ly == 0)
        return 1;
    if (ly == 0)
        return 0;
    size_t i=0, j=0;
    int vx=0, vy=0;
    while (i < lx) {
        if (j >= ly) return 0;
        jl_value_t *xi = jl_tparam(xd, i), *yi = jl_tparam(yd, j);
        if (jl_is_vararg_type(xi)) vx = 1;
        if (jl_is_vararg_type(yi)) vy = 1;
        if (vx && !vy)
            return 0;
        if (!vx && vy) {
            if (!subtype(xi, jl_unwrap_vararg(yi), e, 1))
                return 0;
        }
        else {
            if (!subtype(xi, yi, e, 1))
                return 0;
        }
        i++;
        if (j < ly-1 || !vy)
            j++;
    }
    // TODO: handle Vararg with explicit integer length parameter
    vy = vy || (j < ly && jl_is_vararg_type(jl_tparam(yd,j)));
    if (vy && !vx && lx+1 >= ly) {
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
                    // this is a bit odd, but seems necessary to make this case work:
                    // (UnionAll x<:T<:x Ref{Ref{T}}) == Ref{UnionAll x<:T<:x Ref{T}}
                    return subtype(yy->ub, yy->lb, e, 0);
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
    if (jl_is_uniontype(y)) {
        if (x == y || x == ((jl_uniontype_t*)y)->a || x == ((jl_uniontype_t*)y)->b)
            return 1;
        if (jl_is_unionall(x))
            return subtype_unionall(y, (jl_unionall_t*)x, e, 0, param);
        return subtype_union(x, y, e, 1, param);
    }
    if (jl_is_uniontype(x)) {
        if (jl_is_unionall(y))
            return subtype_unionall(x, (jl_unionall_t*)y, e, 1, param);
        return subtype_union(y, x, e, 0, param);
    }
    if (jl_is_unionall(y)) {
        if (x == y && !(e->envidx < e->envsz))
            return 1;
        return subtype_unionall(x, (jl_unionall_t*)y, e, 1, param);
    }
    if (jl_is_unionall(x))
        return subtype_unionall(y, (jl_unionall_t*)x, e, 0, param);
    if (jl_is_datatype(x) && jl_is_datatype(y)) {
        if (x == y) return 1;
        if (y == (jl_value_t*)jl_any_type) return 1;
        jl_datatype_t *xd = (jl_datatype_t*)x, *yd = (jl_datatype_t*)y;
        if (jl_is_type_type(xd) && !jl_is_typevar(jl_tparam0(xd)) && jl_typeof(jl_tparam0(xd)) == yd)
            // TODO this is not strictly correct, but we don't yet have any other way for
            // e.g. the argument `Int` to match a `::DataType` slot. Most correct would be:
            // Int isa DataType, Int isa Type{Int}, Type{Int} more specific than DataType,
            // !(Type{Int} <: DataType), !isleaftype(Type{Int}), because non-DataTypes can
            // be type-equal to `Int`.
            return 1;
        while (xd != jl_any_type && xd->name != yd->name)
            xd = xd->super;
        if (xd == (jl_value_t*)jl_any_type) return 0;
        if (jl_is_tuple_type(xd))
            return subtype_tuple(xd, yd, e);
        if (jl_is_vararg_type(xd)) {
            // Vararg: covariant in first parameter, invariant in second
            jl_value_t *xp1=jl_tparam0(xd), *xp2=jl_tparam1(xd), *yp1=jl_tparam0(yd), *yp2=jl_tparam1(yd);
            // in Vararg{T1} <: Vararg{T2}, need to check subtype twice to
            // simulate the possibility of multiple arguments, which is needed
            // to implement the diagonal rule correctly.
            if (!subtype(xp1, yp1, e, 1)) return 0;
            if (!subtype(xp1, yp1, e, 1)) return 0;
            // Vararg{T,N} <: Vararg{T2,N2}; equate N and N2
            e->invdepth++;
            int ans = subtype(xp2, yp2, e, 1) && subtype(yp2, xp2, e, 0);
            e->invdepth--;
            return ans;
        }
        size_t i, np = jl_nparams(xd);
        int ans = 1;
        e->invdepth++;
        for (i=0; i < np; i++) {
            jl_value_t *xi = jl_tparam(xd, i), *yi = jl_tparam(yd, i);
            if (!(xi == yi || (subtype(xi, yi, e, 1) && subtype(yi, xi, e, 0)))) {
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

static int exists_subtype(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int8_t anyunions)
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
            found = exists_subtype(x, y, e, 1);
            statestack_pop(&e->Runions);
        }
        if (found) return 1;
    }
    return 0;
}

static int forall_exists_subtype(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int8_t anyunions)
{
    int forall;
    for (forall=0; forall <= anyunions; forall++) {
        if (e->Lunions.stacksize > 0)
            statestack_set(&e->Lunions, e->Lunions.stacksize-1, forall);
        if (!exists_subtype(x, y, e, 0))
            return 0;
        if (e->Lunions.more) {
            statestack_push(&e->Lunions, 0);
            int sub = forall_exists_subtype(x, y, e, 1);
            statestack_pop(&e->Lunions);
            if (!sub) return 0;
        }
    }
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
    e->Lunions.depth = 0;      e->Runions.depth = 0;
    e->Lunions.more = 0;       e->Runions.more = 0;
    e->Lunions.stacksize = 0;  e->Runions.stacksize = 0;
}

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

JL_DLLEXPORT int jl_subtype(jl_value_t *x, jl_value_t *y)
{
    return jl_subtype_env(x, y, NULL, 0);
}

JL_DLLEXPORT int jl_types_equal(jl_value_t *a, jl_value_t *b)
{
    if (obviously_egal(a, b))
        return 1;
    return jl_subtype(a, b) && jl_subtype(b, a);
}

int jl_tuple_isa(jl_value_t **child, size_t cl, jl_datatype_t *pdt)
{
    // TODO jb/subtype avoid allocation
    jl_value_t *tu = arg_type_tuple(child, cl);
    int ans;
    JL_GC_PUSH1(&tu);
    ans = jl_subtype(tu, (jl_value_t*)pdt);
    JL_GC_POP();
    return ans;
}

JL_DLLEXPORT int jl_isa(jl_value_t *x, jl_value_t *t)
{
    if (jl_typeis(x,t))
        return 1;
    if (jl_is_type(x)) {
        if (jl_is_leaf_type(t)) {
            if (jl_is_type_type(t))
                return jl_subtype(x, jl_tparam0(t)) && jl_subtype(jl_tparam0(t), x);
            return 0;
        }
        JL_GC_PUSH1(&x);
        x = (jl_value_t*)jl_wrap_Type(x);
        int ans = jl_subtype(x, t);
        JL_GC_POP();
        return ans;
    }
    if (jl_is_leaf_type(t))
        return 0;
    return jl_subtype(jl_typeof(x), t);
}

// This file is a part of Julia. License is MIT: https://julialang.org/license

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
#ifdef _OS_WINDOWS_
#include <malloc.h>
#endif
#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

// stack of bits to keep track of which combination of Union components we are
// looking at (0 for Union.a, 1 for Union.b). forall_exists_subtype and
// exists_subtype loop over all combinations by updating a binary count in
// this structure.
// Union type decision points are discovered while the algorithm works.
// If a new Union decision is encountered, the `more` flag is set to tell
// the forall/exists loop to grow the stack.
// TODO: the stack probably needs to be artificially large because of some
// deeper problem (see #21191) and could be shrunk once that is fixed
typedef struct {
    int depth;
    int more;
    uint32_t stack[100];  // stack of bits represented as a bit vector
} jl_unionstate_t;

// Linked list storing the type variable environment. A new jl_varbinding_t
// is pushed for each UnionAll type we encounter. `lb` and `ub` are updated
// during the computation.
// Most of the complexity is due to the "diagonal rule", requiring us to
// identify which type vars range over only concrete types.
typedef struct jl_varbinding_t {
    jl_tvar_t *var;
    jl_value_t *lb;
    jl_value_t *ub;
    int8_t right;       // whether this variable came from the right side of `A <: B`
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
    // array of typevars that our bounds depend on, whose UnionAlls need to be
    // moved outside ours.
    jl_array_t *innervars;
    int intvalued;      // must be integer-valued; i.e. occurs as N in Vararg{_,N}
    struct jl_varbinding_t *prev;
} jl_varbinding_t;

// subtype algorithm state
typedef struct jl_stenv_t {
    // N.B.: varbindings are created on the stack and rooted there
    jl_varbinding_t *vars;    // type variable environment
    jl_unionstate_t Lunions;  // union state for unions on the left of A <: B
    jl_unionstate_t Runions;  // union state for unions on the right
    // N.B.: envout is gc-rooted
    jl_value_t **envout;      // for passing caller the computed bounds of right-side variables
    int envsz;                // length of envout
    int envidx;               // current index in envout
    int invdepth;             // # of invariant constructors we're nested in on the left
    int Rinvdepth;            // # of invariant constructors we're nested in on the right
    int ignore_free;          // treat free vars as black boxes; used during intersection
    int intersection;         // true iff subtype is being called from intersection
    int emptiness_only;       // true iff intersection only needs to test for emptiness
} jl_stenv_t;

// state manipulation utilities

// look up a type variable in an environment
#ifdef __clang_analyzer__
static jl_varbinding_t *lookup(jl_stenv_t *e, jl_tvar_t *v) JL_GLOBALLY_ROOTED JL_NOTSAFEPOINT;
#else
static jl_varbinding_t *lookup(jl_stenv_t *e, jl_tvar_t *v) JL_GLOBALLY_ROOTED JL_NOTSAFEPOINT
{
    jl_varbinding_t *b = e->vars;
    while (b != NULL) {
        if (b->var == v) return b;
        b = b->prev;
    }
    return b;
}
#endif

static int statestack_get(jl_unionstate_t *st, int i) JL_NOTSAFEPOINT
{
    assert(i >= 0 && i < sizeof(st->stack) * 8);
    // get the `i`th bit in an array of 32-bit words
    return (st->stack[i>>5] & (1<<(i&31))) != 0;
}

static void statestack_set(jl_unionstate_t *st, int i, int val) JL_NOTSAFEPOINT
{
    assert(i >= 0 && i < sizeof(st->stack) * 8);
    if (val)
        st->stack[i>>5] |= (1<<(i&31));
    else
        st->stack[i>>5] &= ~(1<<(i&31));
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
    *root = (jl_value_t*)jl_alloc_svec(len * 3);
    se->buf = (int8_t*)(len ? malloc_s(len * 2) : NULL);
#ifdef __clang_analyzer__
    if (len)
        memset(se->buf, 0, len * 2);
#endif
    int i=0, j=0; v = e->vars;
    while (v != NULL) {
        jl_svecset(*root, i++, v->lb);
        jl_svecset(*root, i++, v->ub);
        jl_svecset(*root, i++, (jl_value_t*)v->innervars);
        se->buf[j++] = v->occurs_inv;
        se->buf[j++] = v->occurs_cov;
        v = v->prev;
    }
    se->rdepth = e->Runions.depth;
}

static void restore_env(jl_stenv_t *e, jl_value_t *root, jl_savedenv_t *se) JL_NOTSAFEPOINT
{
    jl_varbinding_t *v = e->vars;
    int i = 0, j = 0;
    while (v != NULL) {
        if (root) v->lb = jl_svecref(root, i);
        i++;
        if (root) v->ub = jl_svecref(root, i);
        i++;
        if (root) v->innervars = (jl_array_t*)jl_svecref(root, i);
        i++;
        assert(se->buf);
        v->occurs_inv = se->buf[j++];
        v->occurs_cov = se->buf[j++];
        v = v->prev;
    }
    e->Runions.depth = se->rdepth;
    if (e->envout && e->envidx < e->envsz)
        memset(&e->envout[e->envidx], 0, (e->envsz - e->envidx)*sizeof(void*));
}

// type utilities

// quickly test that two types are identical
static int obviously_egal(jl_value_t *a, jl_value_t *b)
{
    if (a == (jl_value_t*)jl_typeofbottom_type->super)
        a = (jl_value_t*)jl_typeofbottom_type; // supertype(typeof(Union{})) is equal to, although distinct from, itself
    if (b == (jl_value_t*)jl_typeofbottom_type->super)
        b = (jl_value_t*)jl_typeofbottom_type; // supertype(typeof(Union{})) is equal to, although distinct from, itself
    if (a == b) return 1;
    if (jl_typeof(a) != jl_typeof(b)) return 0;
    if (jl_is_datatype(a)) {
        jl_datatype_t *ad = (jl_datatype_t*)a;
        jl_datatype_t *bd = (jl_datatype_t*)b;
        if (ad->name != bd->name) return 0;
        if (ad->isconcretetype || bd->isconcretetype) return 0;
        size_t i, np = jl_nparams(ad);
        if (np != jl_nparams(bd)) return 0;
        for (i = 0; i < np; i++) {
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
    if (a == (jl_value_t*)jl_typeofbottom_type->super)
        a = (jl_value_t*)jl_typeofbottom_type; // supertype(typeof(Union{})) is equal to, although distinct from, itself
    if (b == (jl_value_t*)jl_typeofbottom_type->super)
        b = (jl_value_t*)jl_typeofbottom_type; // supertype(typeof(Union{})) is equal to, although distinct from, itself
    if (a == b)
        return 0;
    if (jl_is_unionall(a))
        a = jl_unwrap_unionall(a);
    if (jl_is_unionall(b))
        b = jl_unwrap_unionall(b);
    if (jl_is_datatype(a)) {
        if (b == jl_bottom_type)
            return 1;
        if (jl_is_datatype(b)) {
            jl_datatype_t *ad = (jl_datatype_t*)a;
            jl_datatype_t *bd = (jl_datatype_t*)b;
            if (ad->name != bd->name)
                return 1;
            int istuple = (ad->name == jl_tuple_typename);
            if (jl_is_concrete_type(a) || jl_is_concrete_type(b)) {
                if (!istuple && ad->name != jl_type_typename) // HACK: can't properly normalize Tuple{Float64} == Tuple{<:Float64} like types or Type{T} types
                    return 1;
            }
            size_t i, np;
            if (istuple) {
                size_t na = jl_nparams(ad), nb = jl_nparams(bd);
                if (jl_is_va_tuple(ad)) {
                    na -= 1;
                    if (jl_is_va_tuple(bd))
                        nb -= 1;
                }
                else if (jl_is_va_tuple(bd)) {
                    nb -= 1;
                }
                else if (na != nb) {
                    return 1;
                }
                np = na < nb ? na : nb;
            }
            else {
                np = jl_nparams(ad);
                if (np != jl_nparams(bd))
                    return 1;
            }
            for (i = 0; i < np; i++) {
                if (obviously_unequal(jl_tparam(ad, i), jl_tparam(bd, i)))
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
    else if (jl_is_long(b)) {
        return 1;
    }
    if ((jl_is_symbol(a) || jl_is_symbol(b)) && a != b)
        return 1;
    return 0;
}

int jl_obviously_unequal(jl_value_t *a, jl_value_t *b)
{
    return obviously_unequal(a, b);
}

static int in_union(jl_value_t *u, jl_value_t *x) JL_NOTSAFEPOINT
{
    if (u == x) return 1;
    if (!jl_is_uniontype(u)) return 0;
    return in_union(((jl_uniontype_t*)u)->a, x) || in_union(((jl_uniontype_t*)u)->b, x);
}

static int obviously_disjoint(jl_value_t *a, jl_value_t *b, int specificity)
{
    if (a == b || a == (jl_value_t*)jl_any_type || b == (jl_value_t*)jl_any_type)
        return 0;
    if (specificity && a == (jl_value_t*)jl_typeofbottom_type)
        return 0;
    // TODO: this would be a nice fast-path to have, unfortuanately,
    //       datatype allocation fails to correctly hash-cons them
    //       and the subtyping tests include tests for this case
    //if (jl_is_concrete_type(a) && jl_is_concrete_type(b) &&
    //    (((jl_datatype_t*)a)->name != jl_tuple_typename ||
    //     ((jl_datatype_t*)b)->name != jl_tuple_typename))
    //    return 1;
    if (jl_is_unionall(a)) a = jl_unwrap_unionall(a);
    if (jl_is_unionall(b)) b = jl_unwrap_unionall(b);
    if (jl_is_datatype(a) && jl_is_datatype(b)) {
        jl_datatype_t *ad = (jl_datatype_t*)a, *bd = (jl_datatype_t*)b;
        if (ad->name != bd->name) {
            jl_datatype_t *temp = ad;
            while (temp != jl_any_type && temp->name != bd->name)
                temp = temp->super;
            if (temp == jl_any_type) {
                temp = bd;
                while (temp != jl_any_type && temp->name != ad->name)
                    temp = temp->super;
                if (temp == jl_any_type)
                    return 1;
                bd = temp;
            }
            else {
                ad = temp;
            }
            if (specificity) {
                // account for declared subtypes taking priority (issue #21710)
                return 0;
            }
        }
        int istuple = (ad->name == jl_tuple_typename);
        size_t np;
        if (istuple) {
            size_t na = jl_nparams(ad), nb = jl_nparams(bd);
            if (jl_is_va_tuple(ad)) {
                na -= 1;
                if (jl_is_va_tuple(bd))
                    nb -= 1;
            }
            else if (jl_is_va_tuple(bd)) {
                nb -= 1;
            }
            else if (!specificity && na != nb) {
                // note: some disjoint types (e.g. tuples of different lengths) can be more specific
                return 1;
            }
            np = na < nb ? na : nb;
        }
        else {
            np = jl_nparams(ad);
        }
        size_t i;
        for (i = 0; i < np; i++) {
            jl_value_t *ai = jl_tparam(ad, i);
            jl_value_t *bi = jl_tparam(bd, i);
            if (jl_is_typevar(ai) || jl_is_typevar(bi))
                continue; // it's possible that Union{} is in this intersection
            if (jl_is_type(ai)) {
                if (jl_is_type(bi)) {
                    if (istuple && (ai == jl_bottom_type || bi == jl_bottom_type))
                        ; // TODO: this can return 1 if and when Tuple{Union{}} === Union{}
                    else if (obviously_disjoint(ai, bi, specificity))
                        return 1;
                }
                else if (ai != (jl_value_t*)jl_any_type) {
                    return 1;
                }
            }
            else if (jl_is_type(bi)) {
                if (bi != (jl_value_t*)jl_any_type)
                    return 1;
            }
            else if (!jl_egal(ai, bi)) {
                return 1;
            }
        }
    }
    else if (a == jl_bottom_type || b == jl_bottom_type) {
        return 1;
    }
    return 0;
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
    if (jl_is_kind(a) && jl_is_type_type(b) && jl_typeof(jl_tparam0(b)) == a)
        return a;
    if (jl_is_kind(b) && jl_is_type_type(a) && jl_typeof(jl_tparam0(a)) == b)
        return b;
    if (jl_is_typevar(a) && obviously_egal(b, ((jl_tvar_t*)a)->lb))
        return a;
    if (jl_is_typevar(b) && obviously_egal(a, ((jl_tvar_t*)b)->lb))
        return b;
    if (!jl_has_free_typevars(a) && !jl_has_free_typevars(b) &&
        // issue #24521: don't merge Type{T} where typeof(T) varies
        !(jl_is_type_type(a) && jl_is_type_type(b) && jl_typeof(jl_tparam0(a)) != jl_typeof(jl_tparam0(b)))) {
        if (jl_subtype(a, b)) return b;
        if (jl_subtype(b, a)) return a;
    }
    return jl_new_struct(jl_uniontype_type, a, b);
}

// compute a greatest lower bound of `a` and `b`
// in many cases, we need to over-estimate this by returning `b`.
static jl_value_t *simple_meet(jl_value_t *a, jl_value_t *b)
{
    if (a == (jl_value_t*)jl_any_type || b == jl_bottom_type || obviously_egal(a,b))
        return b;
    if (b == (jl_value_t*)jl_any_type || a == jl_bottom_type)
        return a;
    if (!(jl_is_type(a) || jl_is_typevar(a)) || !(jl_is_type(b) || jl_is_typevar(b)))
        return jl_bottom_type;
    if (jl_is_uniontype(a) && in_union(a, b))
        return b;
    if (jl_is_uniontype(b) && in_union(b, a))
        return a;
    if (jl_is_kind(a) && jl_is_type_type(b) && jl_typeof(jl_tparam0(b)) == a)
        return b;
    if (jl_is_kind(b) && jl_is_type_type(a) && jl_typeof(jl_tparam0(a)) == b)
        return a;
    if (jl_is_typevar(a) && obviously_egal(b, ((jl_tvar_t*)a)->ub))
        return a;
    if (jl_is_typevar(b) && obviously_egal(a, ((jl_tvar_t*)b)->ub))
        return b;
    if (obviously_disjoint(a, b, 0))
        return jl_bottom_type;
    if (!jl_has_free_typevars(a) && !jl_has_free_typevars(b)) {
        if (jl_subtype(a, b)) return a;
        if (jl_subtype(b, a)) return b;
    }
    return b;
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

static jl_value_t *pick_union_element(jl_value_t *u JL_PROPAGATES_ROOT, jl_stenv_t *e, int8_t R) JL_NOTSAFEPOINT
{
    jl_unionstate_t *state = R ? &e->Runions : &e->Lunions;
    do {
        int ui = statestack_get(state, state->depth);
        state->depth++;
        if (ui == 0) {
            state->more = state->depth; // memorize that this was the deepest available choice
            u = ((jl_uniontype_t*)u)->a;
        }
        else {
            u = ((jl_uniontype_t*)u)->b;
        }
    } while (jl_is_uniontype(u));
    return u;
}

static int forall_exists_subtype(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int param);

// subtype for variable bounds consistency check. needs its own forall/exists environment.
static int subtype_ccheck(jl_value_t *x, jl_value_t *y, jl_stenv_t *e)
{
    if (x == y)
        return 1;
    if (x == jl_bottom_type && jl_is_type(y))
        return 1;
    if (y == (jl_value_t*)jl_any_type && jl_is_type(x))
        return 1;
    if (jl_is_uniontype(x) && jl_is_uniontype(y) && jl_egal(x,y))
        return 1;
    if (x == (jl_value_t*)jl_any_type && jl_is_datatype(y))
        return 0;
    jl_unionstate_t oldLunions = e->Lunions;
    jl_unionstate_t oldRunions = e->Runions;
    int sub;
    memset(e->Lunions.stack, 0, sizeof(e->Lunions.stack));
    memset(e->Runions.stack, 0, sizeof(e->Runions.stack));
    e->Runions.depth = 0;
    e->Runions.more = 0;
    e->Lunions.depth = 0;
    e->Lunions.more = 0;

    sub = forall_exists_subtype(x, y, e, 0);

    e->Runions = oldRunions;
    e->Lunions = oldLunions;
    return sub;
}

static int subtype_left_var(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int param)
{
    if (x == y)
        return 1;
    if (x == jl_bottom_type && jl_is_type(y))
        return 1;
    if (y == (jl_value_t*)jl_any_type && jl_is_type(x))
        return 1;
    if (jl_is_uniontype(x) && jl_is_uniontype(y) && jl_egal(x,y))
        return 1;
    if (x == (jl_value_t*)jl_any_type && jl_is_datatype(y))
        return 0;
    return subtype(x, y, e, param);
}

// use the current context to record where a variable occurred, for the purpose
// of determining whether the variable is concrete.
static void record_var_occurrence(jl_varbinding_t *vb, jl_stenv_t *e, int param) JL_NOTSAFEPOINT
{
    if (vb != NULL && param) {
        // saturate counters at 2; we don't need values bigger than that
        if (param == 2 && (vb->right ? e->Rinvdepth : e->invdepth) > vb->depth0) {
            if (vb->occurs_inv < 2)
                vb->occurs_inv++;
        }
        else if (vb->occurs_cov < 2) {
            vb->occurs_cov++;
        }
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

static jl_value_t *intersect_aside(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int R, int d);

// check that type var `b` is <: `a`, and update b's upper bound.
static int var_lt(jl_tvar_t *b, jl_value_t *a, jl_stenv_t *e, int param)
{
    jl_varbinding_t *bb = lookup(e, b);
    if (bb == NULL)
        return e->ignore_free || subtype_left_var(b->ub, a, e, param);
    record_var_occurrence(bb, e, param);
    if (!bb->right)  // check ∀b . b<:a
        return subtype_left_var(bb->ub, a, e, param);
    if (bb->ub == a)
        return 1;
    if (!((bb->lb == jl_bottom_type && !jl_is_type(a) && !jl_is_typevar(a)) || subtype_ccheck(bb->lb, a, e)))
        return 0;
    // for this to work we need to compute issub(left,right) before issub(right,left),
    // since otherwise the issub(a, bb.ub) check in var_gt becomes vacuous.
    if (e->intersection) {
        jl_value_t *ub = intersect_aside(bb->ub, a, e, 0, bb->depth0);
        if (ub != (jl_value_t*)b)
            bb->ub = ub;
    }
    else {
        bb->ub = simple_meet(bb->ub, a);
    }
    assert(bb->ub != (jl_value_t*)b);
    if (jl_is_typevar(a)) {
        jl_varbinding_t *aa = lookup(e, (jl_tvar_t*)a);
        if (aa && !aa->right && in_union(bb->lb, a) && bb->depth0 != aa->depth0 && var_outside(e, b, (jl_tvar_t*)a)) {
            // an "exists" var cannot equal a "forall" var inside it unless the forall
            // var has equal bounds.
            return subtype_left_var(aa->ub, aa->lb, e, param);
        }
    }
    return 1;
}

// check that type var `b` is >: `a`, and update b's lower bound.
static int var_gt(jl_tvar_t *b, jl_value_t *a, jl_stenv_t *e, int param)
{
    jl_varbinding_t *bb = lookup(e, b);
    if (bb == NULL)
        return e->ignore_free || subtype_left_var(a, b->lb, e, param);
    record_var_occurrence(bb, e, param);
    if (!bb->right)  // check ∀b . b>:a
        return subtype_left_var(a, bb->lb, e, param);
    if (bb->lb == bb->ub) {
        if (jl_is_typevar(bb->lb) && !jl_is_type(a) && !jl_is_typevar(a))
            return var_gt((jl_tvar_t*)bb->lb, a, e, param);
        if (jl_is_typevar(a) && !jl_is_type(bb->lb) && !jl_is_typevar(bb->lb))
            return var_lt((jl_tvar_t*)a, bb->lb, e, param);
    }
    if (!((bb->ub == (jl_value_t*)jl_any_type && !jl_is_type(a) && !jl_is_typevar(a)) || subtype_ccheck(a, bb->ub, e)))
        return 0;
    bb->lb = simple_join(bb->lb, a);
    assert(bb->lb != (jl_value_t*)b);
    if (jl_is_typevar(a)) {
        jl_varbinding_t *aa = lookup(e, (jl_tvar_t*)a);
        if (aa && !aa->right && bb->depth0 != aa->depth0 && param == 2 && var_outside(e, b, (jl_tvar_t*)a))
            return subtype_left_var(aa->ub, aa->lb, e, param);
    }
    return 1;
}

// check that a type is concrete or quasi-concrete (Type{T}).
// this is used to check concrete typevars:
// issubtype is false if the lower bound of a concrete type var is not concrete.
static int is_leaf_bound(jl_value_t *v) JL_NOTSAFEPOINT
{
    if (v == jl_bottom_type)
        return 1;
    if (jl_is_datatype(v)) {
        if (((jl_datatype_t*)v)->abstract) {
            if (jl_is_type_type(v))
                return 1;//!jl_has_free_typevars(jl_tparam0(v));
            return 0;
        }
        return ((jl_datatype_t*)v)->isconcretetype;
    }
    return !jl_is_type(v) && !jl_is_typevar(v);
}

static int is_leaf_typevar(jl_tvar_t *v) JL_NOTSAFEPOINT
{
    return is_leaf_bound(v->lb);
}

static jl_value_t *widen_Type(jl_value_t *t JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT
{
    if (jl_is_type_type(t) && !jl_is_typevar(jl_tparam0(t)))
        return jl_typeof(jl_tparam0(t));
    if (jl_is_uniontype(t)) {
        jl_value_t *a = widen_Type(((jl_uniontype_t*)t)->a);
        jl_value_t *b = widen_Type(((jl_uniontype_t*)t)->b);
        if (a == b)
            return a;
    }
    return t;
}

JL_DLLEXPORT jl_array_t *jl_find_free_typevars(jl_value_t *v);

// convert a type with free variables to a typevar bounded by a UnionAll-wrapped
// version of that type.
// TODO: This loses some inference precision. For example in a case where a
// variable bound is `Vector{_}`, we could potentially infer `Type{Vector{_}} where _`,
// but this causes us to infer the larger `Type{T} where T<:Vector` instead.
// However this is needed because many contexts check `isa(sp, TypeVar)` to determine
// when a static parameter value is not known exactly.
static jl_value_t *fix_inferred_var_bound(jl_tvar_t *var, jl_value_t *ty JL_MAYBE_UNROOTED)
{
    if (!jl_is_typevar(ty) && jl_has_free_typevars(ty)) {
        jl_value_t *ans = ty;
        jl_array_t *vs = NULL;
        JL_GC_PUSH2(&ans, &vs);
        vs = jl_find_free_typevars(ty);
        int i;
        for (i = 0; i < jl_array_len(vs); i++) {
            ans = jl_type_unionall((jl_tvar_t*)jl_array_ptr_ref(vs, i), ans);
        }
        ans = (jl_value_t*)jl_new_typevar(var->name, jl_bottom_type, ans);
        JL_GC_POP();
        return ans;
    }
    return ty;
}

static int var_occurs_inside(jl_value_t *v, jl_tvar_t *var, int inside, int want_inv) JL_NOTSAFEPOINT;

typedef int (*tvar_callback)(void*, int8_t, jl_stenv_t *, int);

static int var_occurs_invariant(jl_value_t *v, jl_tvar_t *var, int inv) JL_NOTSAFEPOINT
{
    return var_occurs_inside(v, var, 0, 1);
}

static int with_tvar(tvar_callback callback, void *context, jl_unionall_t *u, int8_t R, jl_stenv_t *e, int param)
{
    jl_varbinding_t vb = { u->var, u->var->lb, u->var->ub, R, 0, 0, 0, 0,
                           R ? e->Rinvdepth : e->invdepth, 0, NULL, 0, e->vars };
    JL_GC_PUSH4(&u, &vb.lb, &vb.ub, &vb.innervars);
    e->vars = &vb;
    int ans;
    if (R) {
        e->envidx++;
        ans = callback(context, R, e, param);
        e->envidx--;
        // widen Type{x} to typeof(x) in argument position
        if (!vb.occurs_inv)
            vb.lb = widen_Type(vb.lb);
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
            jl_value_t *oldval = e->envout[e->envidx];
            // if we try to assign different variable values (due to checking
            // multiple union members), consider the value unknown.
            if (oldval && !jl_egal(oldval, val))
                e->envout[e->envidx] = (jl_value_t*)u->var;
            else
                e->envout[e->envidx] = fix_inferred_var_bound(u->var, val);
            // TODO: substitute the value (if any) of this variable into previous envout entries
        }
    }
    else {
        ans = callback(context, R, e, param);
    }

    // handle the "diagonal dispatch" rule, which says that a type var occurring more
    // than once, and only in covariant position, is constrained to concrete types. E.g.
    //  ( Tuple{Int, Int}    <: Tuple{T, T} where T) but
    // !( Tuple{Int, String} <: Tuple{T, T} where T)
    // Then check concreteness by checking that the lower bound is not an abstract type.
    int diagonal = vb.occurs_cov > 1 && !var_occurs_invariant(u->body, u->var, 0);
    if (ans && (vb.concrete || (diagonal && is_leaf_typevar(u->var)))) {
        if (vb.concrete && !diagonal && !is_leaf_bound(vb.ub)) {
            // a non-diagonal var can only be a subtype of a diagonal var if its
            // upper bound is concrete.
            ans = 0;
        }
        else if (jl_is_typevar(vb.lb)) {
            jl_tvar_t *v = (jl_tvar_t*)vb.lb;
            jl_varbinding_t *vlb = lookup(e, v);
            if (vlb)
                vlb->concrete = 1;
        }
        else if (!is_leaf_bound(vb.lb)) {
            ans = 0;
        }
    }

    e->vars = vb.prev;

    if (!ans) {
        JL_GC_POP();
        return 0;
    }

    jl_varbinding_t *btemp = e->vars;
    if (vb.lb != vb.ub) {
        while (btemp != NULL) {
            jl_value_t *vu = btemp->ub;
            jl_value_t *vl = btemp->lb;
            // TODO: this takes a significant amount of time
            if (btemp->depth0 != vb.depth0 &&
                ((vu != (jl_value_t*)vb.var && btemp->var->ub != vu && var_occurs_inside(vu, vb.var, 0, 1)) ||
                 (vl != (jl_value_t*)vb.var && btemp->var->lb != vl && var_occurs_inside(vl, vb.var, 0, 1)))) {
                ans = 0; break;
            }
            btemp = btemp->prev;
        }
    }

    JL_GC_POP();
    return ans;
}

static jl_unionall_t *unalias_unionall(jl_unionall_t *u, jl_stenv_t *e)
{
    jl_varbinding_t *btemp = e->vars;
    // if the var for this unionall (based on identity) already appears somewhere
    // in the environment, rename to get a fresh var.
    JL_GC_PUSH1(&u);
    while (btemp != NULL) {
        if (btemp->var == u->var ||
            // outer var can only refer to inner var if bounds changed
            (btemp->lb != btemp->var->lb && jl_has_typevar(btemp->lb, u->var)) ||
            (btemp->ub != btemp->var->ub && jl_has_typevar(btemp->ub, u->var))) {
            u = rename_unionall(u);
            break;
        }
        btemp = btemp->prev;
    }
    JL_GC_POP();
    return u;
}

struct subtype_unionall_env {
    jl_value_t *t;
    jl_value_t *ubody;
};

static int subtype_unionall_callback(struct subtype_unionall_env *env, int8_t R, jl_stenv_t *s, int param) {
    JL_GC_PROMISE_ROOTED(env->t);
    JL_GC_PROMISE_ROOTED(env->ubody);
    if (R) {
        return subtype(env->t, env->ubody, s, param);
    }
    else {
        return subtype(env->ubody, env->t, s, param);
    }
}

// compare UnionAll type `u` to `t`. `R==1` if `u` came from the right side of A <: B.
static int subtype_unionall(jl_value_t *t, jl_unionall_t *u, jl_stenv_t *e, int8_t R, int param)
{
    u = unalias_unionall(u, e);
    struct subtype_unionall_env env = {t, u->body};
    JL_GC_PUSH1(&u);
    int res = with_tvar((tvar_callback)subtype_unionall_callback, (void*)&env, u, R, e, param);
    JL_GC_POP();
    return res;
}

// unwrap <=1 layers of UnionAlls, leaving the var in *p1 the body
static jl_datatype_t *unwrap_1_unionall(jl_value_t *t, jl_tvar_t **p1) JL_NOTSAFEPOINT
{
    assert(t);
    if (jl_is_unionall(t)) {
        *p1 = ((jl_unionall_t*)t)->var;
        t = ((jl_unionall_t*)t)->body;
    }
    assert(jl_is_datatype(t));
    return (jl_datatype_t*)t;
}

// check n <: (length of vararg type v)
static int check_vararg_length(jl_value_t *v, ssize_t n, jl_stenv_t *e)
{
    jl_tvar_t *va_p1=NULL;
    jl_datatype_t *tail = unwrap_1_unionall(v, &va_p1);
    jl_value_t *N = jl_tparam1(tail);
    // only do the check if N is free in the tuple type's last parameter
    if (N != (jl_value_t*)va_p1) {
        jl_value_t *nn = jl_box_long(n);
        JL_GC_PUSH1(&nn);
        e->invdepth++;
        e->Rinvdepth++;
        int ans = subtype(nn, N, e, 2) && subtype(N, nn, e, 0);
        e->invdepth--;
        e->Rinvdepth--;
        JL_GC_POP();
        if (!ans)
            return 0;
    }
    return 1;
}

static int forall_exists_equal(jl_value_t *x, jl_value_t *y, jl_stenv_t *e);

struct subtype_tuple_env {
    jl_datatype_t *xd, *yd;
    jl_value_t *lastx, *lasty;
    size_t lx, ly;
    size_t i, j;
    int vx, vy;
    jl_value_t *vtx;
    jl_value_t *vty;
    jl_vararg_kind_t vvx, vvy;
} JL_ROOTED_VALUE_COLLECTION;

static int subtype_tuple_varargs(struct subtype_tuple_env *env, jl_stenv_t *e, int param)
{
    jl_tvar_t *yv1=NULL;
    jl_datatype_t *yva = unwrap_1_unionall(env->vty, &yv1);
    jl_tvar_t *xv1=NULL;
    jl_datatype_t *xva = unwrap_1_unionall(env->vtx, &xv1);

    jl_value_t *xp0 = jl_tparam0(xva); jl_value_t *xp1 = jl_tparam1(xva);
    jl_value_t *yp0 = jl_tparam0(yva); jl_value_t *yp1 = jl_tparam1(yva);

    if (!jl_is_datatype(env->vtx)) {
        // Unconstrained on the left, constrained on the right
        jl_value_t *yl = yp1;
        if (jl_is_typevar(yl)) {
            jl_varbinding_t *ylv = lookup(e, (jl_tvar_t*)yl);
            if (ylv)
                yl = ylv->lb;
        }
        if (jl_is_long(yl)) {
            return 0;
        }
    }
    else {
        jl_value_t *xl = jl_tparam1(env->vtx);
        if (jl_is_typevar(xl)) {
            jl_varbinding_t *xlv = lookup(e, (jl_tvar_t*)xl);
            if (xlv)
                xl = xlv->lb;
        }
        if (jl_is_long(xl)) {
            if (jl_unbox_long(xl) + 1 == env->vx) {
                // LHS is exhausted. We're a subtype if the RHS is either
                // exhausted as well or unbounded (in which case we need to
                // set it to 0).
                if (jl_is_datatype(env->vty)) {
                    jl_value_t *yl = jl_tparam1(env->vty);
                    if (jl_is_typevar(yl)) {
                        jl_varbinding_t *ylv = lookup(e, (jl_tvar_t*)yl);
                        if (ylv)
                            yl = ylv->lb;
                    }
                    if (jl_is_long(yl)) {
                        return jl_unbox_long(yl) + 1 == env->vy;
                    }
                }
                else {
                    // We can skip the subtype check, but we still
                    // need to make sure to constrain the length of y
                    // to 0.
                    goto constrain_length;
                }
            }
        }
    }

    // in Vararg{T1} <: Vararg{T2}, need to check subtype twice to
    // simulate the possibility of multiple arguments, which is needed
    // to implement the diagonal rule correctly.
    if (!subtype(xp0, yp0, e, param)) return 0;
    if (!subtype(xp0, yp0, e, 1)) return 0;

constrain_length:
    if (!jl_is_datatype(env->vtx)) {
        jl_value_t *yl = yp1;
        if (jl_is_typevar(yl)) {
            jl_varbinding_t *ylv = lookup(e, (jl_tvar_t*)yl);
            if (ylv)
                yl = ylv->lb;
        }
        if (jl_is_long(yl)) {
            // The length of the x tuple is unconstrained, but the
            // length of the y tuple is now fixed (this could have happened
            // as a result of the subtype call above).
            return 0;
        }
    }

    // Vararg{T,N} <: Vararg{T2,N2}; equate N and N2
    e->invdepth++;
    e->Rinvdepth++;
    JL_GC_PUSH2(&xp1, &yp1);
    if (jl_is_long(xp1) && env->vx != 1)
        xp1 = jl_box_long(jl_unbox_long(xp1) - env->vx + 1);
    if (jl_is_long(yp1) && env->vy != 1)
        yp1 = jl_box_long(jl_unbox_long(yp1) - env->vy + 1);
    int ans = forall_exists_equal(xp1, yp1, e);
    JL_GC_POP();
    e->invdepth--;
    e->Rinvdepth--;
    return ans;
}

static int subtype_tuple_tail(struct subtype_tuple_env *env, int8_t R, jl_stenv_t *e, int param)
{
    int x_reps = 1;
loop: // while (i <= lx) {
        if (env->i >= env->lx)
            goto done;

        /* Get the type in the current index. If necessary introduce tvars for
           varargs */
        jl_value_t *xi = NULL;
        if (env->i == env->lx-1 && env->vvx) {
            if (!env->vtx) {
                xi = jl_tparam(env->xd, env->i);
                // Unbounded vararg on the LHS without vararg on the RHS should
                // have been caught earlier.
                assert(env->vvy || !jl_is_unionall(xi));
                if (jl_is_unionall(xi)) {
                    // TODO: If !var_occurs_inside(jl_tparam0(xid), p1, 0, 1),
                    // we could avoid introducing the tvar into the environment
                    jl_unionall_t *u = (jl_unionall_t*)xi;
                    u = unalias_unionall(u, e);
                    env->vtx = (jl_value_t*)u;
                    // goto loop, but with the tvar introduced
                    JL_GC_PUSH1(&u);
                    int res = with_tvar((tvar_callback)subtype_tuple_tail, env, u, 0, e, param);
                    JL_GC_POP();
                    return res;
                }
                env->vtx = xi;
            }
            xi = env->vtx;
        }
        else {
            xi = jl_tparam(env->xd, env->i);
        }

        jl_value_t *yi = NULL;
        if (env->j < env->ly) {
            if (env->j == env->ly-1 && env->vvy) {
                if (!env->vty) {
                    yi = jl_tparam(env->yd, env->j);
                    if (jl_is_unionall(yi)) {
                        jl_unionall_t *u = (jl_unionall_t*)yi;
                        u = unalias_unionall(u, e);
                        env->vty = (jl_value_t*)u;
                        // goto loop, but with the tvar introduced
                        JL_GC_PUSH1(&u);
                        int res = with_tvar((tvar_callback)subtype_tuple_tail, env, u, 1, e, param);
                        JL_GC_POP();
                        return res;
                    }
                    env->vty = yi;
                }
                yi = env->vty;
            }
            else {
                yi = jl_tparam(env->yd, env->j);
            }
        }

        if (env->vtx)
            env->vx += 1;
        if (env->vty)
            env->vy += 1;

        if (env->vx && env->vy) {
            return subtype_tuple_varargs(env, e, param);
        }

        if (env->vx) {
            xi = jl_tparam0(jl_unwrap_unionall(env->vtx));
            if (env->j >= env->ly)
               return 1;
        }
        else if (env->j >= env->ly) {
            return 0;
        }
        int x_same = env->lastx && jl_egal(xi, env->lastx);
        if (env->vy) {
            yi = jl_tparam0(jl_unwrap_unionall(env->vty));
            if (!env->vvx && yi == (jl_value_t*)jl_any_type)
                goto done;  // if y ends in `Vararg{Any}` skip checking everything
            // keep track of number of consecutive identical types compared to Vararg
            if (x_same)
                x_reps++;
            else
                x_reps = 1;
        }
        if (x_reps > 2) {
            // an identical type on the left doesn't need to be compared to a Vararg
            // element type on the right more than twice.
        }
        else if (x_same &&
            ((yi == env->lasty && !jl_has_free_typevars(xi) && !jl_has_free_typevars(yi)) ||
             (yi == env->lasty && !env->vx && env->vy && jl_is_concrete_type(xi)))) {
            // fast path for repeated elements
        }
        else if (e->Runions.depth == 0 && e->Lunions.depth == 0 && !jl_has_free_typevars(xi) && !jl_has_free_typevars(yi)) {
            // fast path for separable sub-formulas
            if (!jl_subtype(xi, yi))
                return 0;
        }
        else if (!subtype(xi, yi, e, param)) {
            return 0;
        }
        env->lastx = xi; env->lasty = yi;
        if (env->i < env->lx-1 || !env->vx)
            env->i++;
        if (env->j < env->ly-1 || !env->vy)
            env->j++;

        goto loop;
    // } (from loop:)

done:
    if (!env->vy && env->j < env->ly && jl_is_vararg_type(jl_tparam(env->yd, env->j)))
        env->vy += 1;
    if (env->vy && !env->vx && env->lx+1 >= env->ly) {
        // in Tuple{...,tn} <: Tuple{...,Vararg{T,N}}, check (lx+1-ly) <: N
        if (!check_vararg_length(jl_tparam(env->yd,env->ly-1), env->lx+1-env->ly, e))
            return 0;
    }
    return (env->lx + env->vx == env->ly + env->vy) || (env->vy && (env->lx >= (env->vx ? env->ly : (env->ly-1))));
}

static int subtype_tuple(jl_datatype_t *xd, jl_datatype_t *yd, jl_stenv_t *e, int param)
{
    struct subtype_tuple_env env;
    env.xd = xd;
    env.yd = yd;
    env.lx = jl_nparams(xd);
    env.ly = jl_nparams(yd);
    if (env.lx == 0 && env.ly == 0)
        return 1;
    env.i = env.j = 0;
    env.vx = env.vy = 0;
    env.vvx = env.vvy = JL_VARARG_NONE;
    jl_varbinding_t *xbb = NULL;
    if (env.lx > 0) {
        env.vvx = jl_vararg_kind(jl_tparam(env.xd, env.lx-1));
        if (env.vvx == JL_VARARG_BOUND)
            xbb = lookup(e, (jl_tvar_t *)jl_tparam1(jl_tparam(env.xd, env.lx - 1)));
    }
    if (env.ly > 0)
        env.vvy = jl_vararg_kind(jl_tparam(env.yd, env.ly-1));
    if (env.vvx != JL_VARARG_NONE && env.vvx != JL_VARARG_INT &&
        (!xbb || !jl_is_long(xbb->lb))) {
        if (env.vvx == JL_VARARG_UNBOUND || (xbb && !xbb->right)) {
            // Unbounded on the LHS, bounded on the RHS
            if (env.vvy == JL_VARARG_NONE || env.vvy == JL_VARARG_INT)
                return 0;
            else if (env.lx < env.ly) // Unbounded includes N == 0
                return 0;
        }
        else if (env.vvy == JL_VARARG_NONE && !check_vararg_length(jl_tparam(env.xd, env.lx-1), env.ly+1-env.lx, e)) {
            return 0;
        }
    }
    else {
        size_t nx = env.lx;
        if (env.vvx == JL_VARARG_INT)
            nx += jl_vararg_length(jl_tparam(env.xd, env.lx-1)) - 1;
        else if (xbb && jl_is_long(xbb->lb))
            nx += jl_unbox_long(xbb->lb) - 1;
        else
            assert(env.vvx == JL_VARARG_NONE);
        size_t ny = env.ly;
        if (env.vvy == JL_VARARG_INT)
            ny += jl_vararg_length(jl_tparam(env.yd, env.ly-1)) - 1;
        else if (env.vvy != JL_VARARG_NONE)
            ny -= 1;
        if (env.vvy == JL_VARARG_NONE || env.vvy == JL_VARARG_INT) {
            if (nx != ny)
                return 0;
        }
        else {
            if (ny > nx)
                return 0;
        }
    }

    param = (param == 0 ? 1 : param);
    env.lastx = env.lasty = NULL;
    env.vtx = env.vty = NULL;
    JL_GC_PUSH2(&env.vtx, &env.vty);
    int ans = subtype_tuple_tail(&env, 0, e, param);
    JL_GC_POP();
    return ans;
}

static int subtype_naked_vararg(jl_datatype_t *xd, jl_datatype_t *yd, jl_stenv_t *e, int param)
{
    // Vararg: covariant in first parameter, invariant in second
    jl_value_t *xp1=jl_tparam0(xd), *xp2=jl_tparam1(xd), *yp1=jl_tparam0(yd), *yp2=jl_tparam1(yd);
    // in Vararg{T1} <: Vararg{T2}, need to check subtype twice to
    // simulate the possibility of multiple arguments, which is needed
    // to implement the diagonal rule correctly.
    if (!subtype(xp1, yp1, e, param)) return 0;
    if (!subtype(xp1, yp1, e, 1)) return 0;
    e->invdepth++;
    e->Rinvdepth++;
    // Vararg{T,N} <: Vararg{T2,N2}; equate N and N2
    int ans = forall_exists_equal(xp2, yp2, e);
    e->invdepth--;
    e->Rinvdepth--;
    return ans;
}

// `param` means we are currently looking at a parameter of a type constructor
// (as opposed to being outside any type constructor, or comparing variable bounds).
// this is used to record the positions where type variables occur for the
// diagonal rule (record_var_occurrence).
static int subtype(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int param)
{
    if (jl_is_uniontype(x)) {
        if (x == y) return 1;
        x = pick_union_element(x, e, 0);
    }
    if (jl_is_uniontype(y)) {
        if (x == ((jl_uniontype_t*)y)->a || x == ((jl_uniontype_t*)y)->b)
            return 1;
        if (jl_is_unionall(x))
            return subtype_unionall(y, (jl_unionall_t*)x, e, 0, param);
        int ui = 1;
        if (jl_is_typevar(x)) {
            // The `convert(Type{T},T)` pattern, where T is a Union, required changing priority
            // of unions and vars: if matching `typevar <: union`, first try to match the whole
            // union against the variable before trying to take it apart to see if there are any
            // variables lurking inside.
            jl_unionstate_t *state = &e->Runions;
            ui = statestack_get(state, state->depth);
            state->depth++;
            if (ui == 0)
                state->more = state->depth; // memorize that this was the deepest available choice
        }
        if (ui == 1)
            y = pick_union_element(y, e, 1);
    }
    if (jl_is_typevar(x)) {
        if (jl_is_typevar(y)) {
            if (x == y) return 1;
            jl_varbinding_t *xx = lookup(e, (jl_tvar_t*)x);
            jl_varbinding_t *yy = lookup(e, (jl_tvar_t*)y);
            jl_value_t *xub = xx ? xx->ub : ((jl_tvar_t*)x)->ub;
            jl_value_t *ylb = yy ? yy->lb : ((jl_tvar_t*)y)->lb;
            if (e->intersection) {
                jl_value_t *xlb = xx ? xx->lb : ((jl_tvar_t*)x)->lb;
                jl_value_t *yub = yy ? yy->ub : ((jl_tvar_t*)y)->ub;
                // find equivalence class for typevars during intersection
                if (xub == xlb && jl_is_typevar(xub))
                    return subtype(xub, y, e, param);
                if (yub == ylb && jl_is_typevar(yub))
                    return subtype(x, yub, e, param);
            }
            int xr = xx && xx->right;  // treat free variables as "forall" (left)
            int yr = yy && yy->right;
            if (xr) {
                if (yy) record_var_occurrence(yy, e, param);
                if (yr) {
                    record_var_occurrence(xx, e, param);
                    return subtype(xx->lb, yy->ub, e, 0);
                }
                return var_lt((jl_tvar_t*)x, y, e, param);
            }
            else if (yr) {
                if (xx) record_var_occurrence(xx, e, param);
                return var_gt((jl_tvar_t*)y, x, e, param);
            }
            // check ∀x,y . x<:y
            // the bounds of left-side variables never change, and can only lead
            // to other left-side variables, so using || here is safe.
            return subtype(xub, y, e, param) || subtype(x, ylb, e, param);
        }
        return var_lt((jl_tvar_t*)x, y, e, param);
    }
    if (jl_is_typevar(y))
        return var_gt((jl_tvar_t*)y, x, e, param);
    if (y == (jl_value_t*)jl_any_type && !jl_has_free_typevars(x))
        return 1;
    if (x == jl_bottom_type && !jl_has_free_typevars(y))
        return 1;
    jl_value_t *ux = jl_unwrap_unionall(x);
    jl_value_t *uy = jl_unwrap_unionall(y);
    if ((x != ux || y != uy) && y != (jl_value_t*)jl_any_type && jl_is_datatype(ux) && jl_is_datatype(uy) &&
        !jl_is_type_type(ux)) {
        assert(ux);
        if (uy == (jl_value_t*)jl_any_type)
            return 1;
        jl_datatype_t *xd = (jl_datatype_t*)ux, *yd = (jl_datatype_t*)uy;
        while (xd != NULL && xd != jl_any_type && xd->name != yd->name) {
            xd = xd->super;
        }
        if (xd == jl_any_type)
            return 0;
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
            jl_value_t *tp0 = jl_tparam0(xd);
            if (!jl_is_typevar(tp0)) {
                // TODO this is not strictly correct, but we don't yet have any other way for
                // e.g. the argument `Int` to match a `::DataType` slot. Most correct would be:
                // Int isa DataType, Int isa Type{Int}, Type{Int} more specific than DataType,
                // !(Type{Int} <: DataType), !isleaftype(Type{Int}), because non-DataTypes can
                // be type-equal to `Int`.
                return jl_typeof(tp0) == (jl_value_t*)yd;
            }
            return 0;
        }
        if (jl_is_type_type(y) && !jl_is_type_type(x) && x != (jl_value_t*)jl_typeofbottom_type) {
            jl_value_t *tp0 = jl_tparam0(yd);
            if (!jl_is_typevar(tp0) || !jl_is_kind(x))
                return 0;
            // DataType.super is special, so `DataType <: Type{T}` (T free) needs special handling.
            // The answer is true iff `T` has full bounds (as in `Type`), but this needs to
            // be checked at the same depth where `Type{T}` occurs --- the depth of the LHS
            // doesn't matter because it (e.g. `DataType`) doesn't actually contain the variable.
            int saved = e->invdepth;
            e->invdepth = e->Rinvdepth;
            int issub = subtype((jl_value_t*)jl_type_type, y, e, param);
            e->invdepth = saved;
            return issub;
        }
        while (xd != jl_any_type && xd->name != yd->name) {
            if (xd->super == NULL)
                jl_errorf("circular type parameter constraint in definition of %s", jl_symbol_name(xd->name->name));
            xd = xd->super;
        }
        if (xd == jl_any_type) return 0;
        if (xd->name == jl_tuple_typename)
            return subtype_tuple(xd, yd, e, param);
        if (xd->name == jl_vararg_typename) {
            // N.B.: This case is only used for raw varargs that are not part
            // of a tuple (those that are have special handling in subtype_tuple).
            // Vararg isn't really a proper type, but it does sometimes show up
            // as e.g. Type{Vararg}, so we'd like to handle that correctly.
            return subtype_naked_vararg(xd, yd, e, param);
        }
        size_t i, np = jl_nparams(xd);
        int ans = 1;
        e->invdepth++;
        e->Rinvdepth++;
        for (i=0; i < np; i++) {
            jl_value_t *xi = jl_tparam(xd, i), *yi = jl_tparam(yd, i);
            if (!(xi == yi || forall_exists_equal(xi, yi, e))) {
                ans = 0; break;
            }
        }
        e->invdepth--;
        e->Rinvdepth--;
        return ans;
    }
    if (jl_is_type(y))
        return x == jl_bottom_type;
    return x == y || jl_egal(x, y);
}

static int is_indefinite_length_tuple_type(jl_value_t *x)
{
    x = jl_unwrap_unionall(x);
    if (!jl_is_tuple_type(x))
        return 0;
    size_t n = jl_nparams(x);
    return n > 0 && jl_vararg_kind(jl_tparam(x, n-1)) == JL_VARARG_UNBOUND;
}

static int is_definite_length_tuple_type(jl_value_t *x)
{
    if (jl_is_typevar(x))
        x = ((jl_tvar_t*)x)->ub;
    x = jl_unwrap_unionall(x);
    if (!jl_is_tuple_type(x))
        return 0;
    size_t n = jl_nparams(x);
    if (n == 0)
        return 1;
    jl_vararg_kind_t k = jl_vararg_kind(jl_tparam(x, n-1));
    return k == JL_VARARG_NONE || k == JL_VARARG_INT;
}

static int forall_exists_equal(jl_value_t *x, jl_value_t *y, jl_stenv_t *e)
{
    if (obviously_egal(x, y)) return 1;

    if ((is_indefinite_length_tuple_type(x) && is_definite_length_tuple_type(y)) ||
        (is_definite_length_tuple_type(x) && is_indefinite_length_tuple_type(y)))
        return 0;

    jl_unionstate_t oldLunions = e->Lunions;
    memset(e->Lunions.stack, 0, sizeof(e->Lunions.stack));
    int sub;

    if (!jl_has_free_typevars(x) || !jl_has_free_typevars(y)) {
        jl_unionstate_t oldRunions = e->Runions;
        memset(e->Runions.stack, 0, sizeof(e->Runions.stack));
        e->Runions.depth = 0;
        e->Runions.more = 0;
        e->Lunions.depth = 0;
        e->Lunions.more = 0;

        sub = forall_exists_subtype(x, y, e, 2);

        e->Runions = oldRunions;
    }
    else {
        int lastset = 0;
        while (1) {
            e->Lunions.more = 0;
            e->Lunions.depth = 0;
            sub = subtype(x, y, e, 2);
            int set = e->Lunions.more;
            if (!sub || !set)
                break;
            for (int i = set; i <= lastset; i++)
                statestack_set(&e->Lunions, i, 0);
            lastset = set - 1;
            statestack_set(&e->Lunions, lastset, 1);
        }
    }

    e->Lunions = oldLunions;
    return sub && subtype(y, x, e, 0);
}

static int exists_subtype(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, jl_value_t *saved, jl_savedenv_t *se, int param)
{
    memset(e->Runions.stack, 0, sizeof(e->Runions.stack));
    int lastset = 0;
    while (1) {
        e->Runions.depth = 0;
        e->Runions.more = 0;
        e->Lunions.depth = 0;
        e->Lunions.more = 0;
        if (subtype(x, y, e, param))
            return 1;
        restore_env(e, saved, se);
        int set = e->Runions.more;
        if (!set)
            return 0;
        for (int i = set; i <= lastset; i++)
            statestack_set(&e->Runions, i, 0);
        lastset = set - 1;
        statestack_set(&e->Runions, lastset, 1);
    }
}

static int forall_exists_subtype(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int param)
{
    // The depth recursion has the following shape, after simplification:
    // ∀₁
    //   ∃₁
    assert(e->Runions.depth == 0);
    assert(e->Lunions.depth == 0);
    jl_value_t *saved=NULL; jl_savedenv_t se;
    JL_GC_PUSH1(&saved);
    save_env(e, &saved, &se);

    memset(e->Lunions.stack, 0, sizeof(e->Lunions.stack));
    int lastset = 0;
    int sub;
    while (1) {
        sub = exists_subtype(x, y, e, saved, &se, param);
        int set = e->Lunions.more;
        if (!sub || !set)
            break;
        free(se.buf);
        save_env(e, &saved, &se);
        for (int i = set; i <= lastset; i++)
            statestack_set(&e->Lunions, i, 0);
        lastset = set - 1;
        statestack_set(&e->Lunions, lastset, 1);
    }

    free(se.buf);
    JL_GC_POP();
    return sub;
}

static void init_stenv(jl_stenv_t *e, jl_value_t **env, int envsz)
{
    e->vars = NULL;
    assert(env != NULL || envsz == 0);
    e->envsz = envsz;
    e->envout = env;
    if (envsz)
        memset(env, 0, envsz*sizeof(void*));
    e->envidx = 0;
    e->invdepth = e->Rinvdepth = 0;
    e->ignore_free = 0;
    e->intersection = 0;
    e->emptiness_only = 0;
    e->Lunions.depth = 0;      e->Runions.depth = 0;
    e->Lunions.more = 0;       e->Runions.more = 0;
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

// compute the minimum bound on the number of concrete types that are subtypes of `t`
// returns 0, 1, or many (2+)
static int concrete_min(jl_value_t *t)
{
    if (jl_is_unionall(t))
        t = jl_unwrap_unionall(t);
    if (jl_is_datatype(t)) {
        if (jl_is_type_type(t))
            return 0; // Type{T} may have the concrete supertype `typeof(T)`, so don't try to handle them here
        return jl_is_concrete_type(t) ? 1 : 2;
    }
    if (jl_is_typevar(t))
        return 0; // could be 0 or more, since we didn't track if it was unbound
    if (jl_is_uniontype(t)) {
        int count = concrete_min(((jl_uniontype_t*)t)->a);
        if (count > 1)
            return count;
        return count + concrete_min(((jl_uniontype_t*)t)->b);
    }
    return 2; // up to infinite
}

static jl_value_t *find_var_body(jl_value_t *t, jl_tvar_t *v)
{
    if (jl_is_unionall(t)) {
        if (((jl_unionall_t*)t)->var == v)
            return ((jl_unionall_t*)t)->body;
        jl_value_t *b = find_var_body(((jl_unionall_t*)t)->var->lb, v);
        if (b) return b;
        b = find_var_body(((jl_unionall_t*)t)->var->ub, v);
        if (b) return b;
        return find_var_body(((jl_unionall_t*)t)->body, v);
    }
    else if (jl_is_uniontype(t)) {
        jl_value_t *b = find_var_body(((jl_uniontype_t*)t)->a, v);
        if (b) return b;
        return find_var_body(((jl_uniontype_t*)t)->b, v);
    }
    else if (jl_is_datatype(t)) {
        size_t i;
        for (i=0; i < jl_nparams(t); i++) {
            jl_value_t *b = find_var_body(jl_tparam(t, i), v);
            if (b)
                return b;
        }
    }
    return NULL;
}

// quickly compute if x seems like a possible subtype of y
// especially optimized for x isa concrete type
// returns true if it could be easily determined, with the result in subtype
// the approximation widens typevar bounds under the assumption they are bound
// in the immediate caller--the caller must be conservative in handling the result
static int obvious_subtype(jl_value_t *x, jl_value_t *y, jl_value_t *y0, int *subtype)
{
    if (x == y || y == (jl_value_t*)jl_any_type) {
        *subtype = 1;
        return 1;
    }
    if (jl_is_unionall(x))
        x = jl_unwrap_unionall(x);
    if (jl_is_unionall(y))
        y = jl_unwrap_unionall(y);
    if (x == (jl_value_t*)jl_typeofbottom_type->super)
        x = (jl_value_t*)jl_typeofbottom_type; // supertype(typeof(Union{})) is equal to, although distinct from, itself
    if (y == (jl_value_t*)jl_typeofbottom_type->super)
        y = (jl_value_t*)jl_typeofbottom_type; // supertype(typeof(Union{})) is equal to, although distinct from, itself
    if (x == y || y == (jl_value_t*)jl_any_type) {
        *subtype = 1;
        return 1;
    }
    if (jl_is_typevar(x)) {
        if (((jl_tvar_t*)x)->lb != (jl_value_t*)jl_bottom_type)
            return 0;
        if (obvious_subtype(((jl_tvar_t*)x)->ub, y, y0, subtype) && *subtype)
            return 1;
        return 0;
    }
    if (jl_is_typevar(y)) {
        if (((jl_tvar_t*)y)->lb != (jl_value_t*)jl_bottom_type)
            return 0;
        if (obvious_subtype(x, ((jl_tvar_t*)y)->ub, y0, subtype) && !*subtype)
            return 1;
        return 0;
    }
    if (x == (jl_value_t*)jl_bottom_type) {
        *subtype = 1;
        return 1;
    }
    if (y == (jl_value_t*)jl_bottom_type) {
        *subtype = 0;
        return 1;
    }
    if (!jl_is_type(x) || !jl_is_type(y)) {
        *subtype = jl_egal(x, y);
        return 1;
    }
    if (jl_is_uniontype(x)) {
        // TODO: consider handling more LHS unions, being wary of covariance
        if (obvious_subtype(((jl_uniontype_t*)x)->a, y, y0, subtype) && *subtype) {
            if (obvious_subtype(((jl_uniontype_t*)x)->b, y, y0, subtype) && *subtype)
                return 1;
        }
        //if (obvious_subtype(((jl_uniontype_t*)x)->a, y, y0, subtype)) {
        //    if (!*subtype)
        //        return 1;
        //    if (obvious_subtype(((jl_uniontype_t*)x)->b, y, y0, subtype))
        //        return 1;
        //}
        //else if (obvious_subtype(((jl_uniontype_t*)x)->b, y, y0, subtype)) {
        //    if (!*subtype)
        //        return 1;
        //}
        return 0;
    }
    if (jl_is_uniontype(y)) {
        if (obvious_subtype(x, ((jl_uniontype_t*)y)->a, y0, subtype)) {
            if (*subtype)
                return 1;
            if (obvious_subtype(x, ((jl_uniontype_t*)y)->b, y0, subtype))
                return 1;
        }
        else if (obvious_subtype(x, ((jl_uniontype_t*)y)->b, y0, subtype)) {
            if (*subtype)
                return 1;
        }
        return 0;
    }
    if (x == (jl_value_t*)jl_any_type) {
        *subtype = 0;
        return 1;
    }
    if (jl_is_datatype(y)) {
        int istuple = (((jl_datatype_t*)y)->name == jl_tuple_typename);
        int iscov = istuple || (((jl_datatype_t*)y)->name == jl_vararg_typename);
        // TODO: this would be a nice fast-path to have, unfortuanately,
        //       datatype allocation fails to correctly hash-cons them
        //       and the subtyping tests include tests for this case
        //if (!iscov && ((jl_datatype_t*)y)->isconcretetype && !jl_is_type_type(x)) {
        //    *subtype = 0;
        //    return 1;
        //}
        if (jl_is_datatype(x)) {
            // Weaker version of above, but runs into the same problem
            //if (((jl_datatype_t*)x)->isconcretetype && ((jl_datatype_t*)y)->isconcretetype && (!istuple || !istuple_x)) {
            //    *subtype = 0;
            //    return 1;
            //}
            int uncertain = 0;
            if (((jl_datatype_t*)x)->name != ((jl_datatype_t*)y)->name) {
                if (jl_is_type_type(x) && jl_is_kind(y)) {
                    jl_value_t *t0 = jl_tparam0(x);
                    if (jl_is_typevar(t0))
                        return 0;
                    *subtype = jl_typeof(t0) == y;
                    return 1;
                }
                if (jl_is_type_type(y)) {
                    jl_value_t *t0 = jl_tparam0(y);
                    assert(!jl_is_type_type(x));
                    if (jl_is_kind(x) && jl_is_typevar(t0))
                        return 0;
                    *subtype = 0;
                    return 1;
                }
                jl_datatype_t *temp = (jl_datatype_t*)x;
                while (temp->name != ((jl_datatype_t*)y)->name) {
                    temp = temp->super;
                    if (temp == NULL) // invalid state during type declaration
                        return 0;
                    if (temp == jl_any_type) {
                        *subtype = 0;
                        return 1;
                    }
                }
                if (obvious_subtype((jl_value_t*)temp, y, y0, subtype) && *subtype)
                    return 1;
                return 0;
            }
            if (!iscov && !((jl_datatype_t*)x)->hasfreetypevars) {
                // by transitivity, if `wrapper <: y`, then `x <: y` if x is a leaf type of its name
                if (obvious_subtype(((jl_datatype_t*)x)->name->wrapper, y, y0, subtype) && *subtype)
                    return 1;
            }
            int i, npx = jl_nparams(x), npy = jl_nparams(y);
            jl_vararg_kind_t vx = JL_VARARG_NONE;
            jl_vararg_kind_t vy = JL_VARARG_NONE;
            jl_value_t *vxt = NULL;
            int nparams_expanded_x = npx;
            int nparams_expanded_y = npy;
            if (istuple) {
                if (npx > 0) {
                    jl_value_t *xva = jl_tparam(x, npx - 1);
                    vx = jl_vararg_kind(xva);
                    if (vx != JL_VARARG_NONE) {
                        vxt = jl_unwrap_vararg(xva);
                        nparams_expanded_x -= 1;
                        if (vx == JL_VARARG_INT)
                            nparams_expanded_x += jl_vararg_length(xva);
                    }
                }
                if (npy > 0) {
                    jl_value_t *yva = jl_tparam(y, npy - 1);
                    vy = jl_vararg_kind(yva);
                    if (vy != JL_VARARG_NONE) {
                        nparams_expanded_y -= 1;
                        if (vy == JL_VARARG_INT)
                            nparams_expanded_y += jl_vararg_length(yva);
                    }
                }
                // if the nparams aren't equal, or at least one of them is a typevar (uncertain), they may be obviously disjoint
                if (nparams_expanded_x != nparams_expanded_y || (vx != JL_VARARG_NONE && vx != JL_VARARG_INT) || (vy != JL_VARARG_NONE && vy != JL_VARARG_INT)) {
                    // we have a stronger bound on x if:
                    if (vy == JL_VARARG_NONE || vy == JL_VARARG_INT) { // the bound on y is certain
                        if (vx == JL_VARARG_NONE || vx == JL_VARARG_INT || vx == JL_VARARG_UNBOUND || // and the bound on x is also certain
                            nparams_expanded_x > nparams_expanded_y || npx > nparams_expanded_y) { // or x is unknown, but definitely longer than y
                            *subtype = 0;
                            return 1; // number of fixed parameters in x are more than declared in y
                        }
                    }
                    if (nparams_expanded_x < nparams_expanded_y) {
                        *subtype = 0;
                        return 1; // number of fixed parameters in x could be fewer than in y
                    }
                    uncertain = 1;
                }
            }
            else if (npx != npy) {
                *subtype = 0;
                return 1;
            }

            // inspect the fixed parameters in y against x
            for (i = 0; i < npy - (vy == JL_VARARG_NONE ? 0 : 1); i++) {
                jl_value_t *a = i >= (npx - (vx == JL_VARARG_NONE ? 0 : 1)) ? vxt : jl_tparam(x, i);
                jl_value_t *b = jl_tparam(y, i);
                if (iscov || jl_is_typevar(b)) {
                    if (obvious_subtype(a, b, y0, subtype)) {
                        if (!*subtype)
                            return 1;
                        if (jl_has_free_typevars(b)) // b is actually more constrained that this
                            uncertain = 1;
                    }
                    else {
                        uncertain = 1;
                    }
                }
                else {
                    if (!obviously_egal(a, b)) {
                        if (obvious_subtype(a, b, y0, subtype)) {
                            if (!*subtype)
                                return 1;
                            if (jl_has_free_typevars(b)) // b is actually more constrained that this
                                uncertain = 1;
                        }
                        else {
                            uncertain = 1;
                        }
                        if (!jl_has_free_typevars(b) && obvious_subtype(b, a, y0, subtype)) {
                            if (!*subtype)
                                return 1;
                            if (jl_has_free_typevars(a)) // a is actually more constrained that this
                                uncertain = 1;
                        }
                        else {
                            uncertain = 1;
                        }
                    }
                }
            }
            if (i < nparams_expanded_x) {
                // there are elements left in x (possibly just a Vararg), check them against the Vararg tail of y too
                assert(vy != JL_VARARG_NONE && istuple && iscov);
                jl_value_t *a1 = (vx != JL_VARARG_NONE && i >= npx - 1) ? vxt : jl_tparam(x, i);
                jl_value_t *b = jl_unwrap_vararg(jl_tparam(y, i));
                if (jl_is_typevar(b)) {
                    jl_value_t *body = find_var_body(y0, (jl_tvar_t*)b);
                    if (body == NULL)
                        body = y0;
                    if (var_occurs_invariant(body, (jl_tvar_t*)b, 0))
                        return 0;
                }
                if (nparams_expanded_x > npy && jl_is_typevar(b) && concrete_min(a1) > 1) {
                    // diagonal rule for 2 or more elements: they must all be concrete on the LHS
                    *subtype = 0;
                    return 1;
                }
                if (jl_is_type_type(a1) && jl_is_type(jl_tparam0(a1))) {
                    a1 = jl_typeof(jl_tparam0(a1));
                }
                for (; i < nparams_expanded_x; i++) {
                    jl_value_t *a = (vx != JL_VARARG_NONE && i >= npx - 1) ? vxt : jl_tparam(x, i);
                    if (i > npy && jl_is_typevar(b)) { // i == npy implies a == a1
                        // diagonal rule: all the later parameters are also constrained to be type-equal to the first
                        jl_value_t *a2 = a;
                        if (jl_is_type_type(a) && jl_is_type(jl_tparam0(a))) {
                            // if a is exactly Type{T}, then use the concrete typeof(T) instead here
                            a2 = jl_typeof(jl_tparam0(a));
                        }
                        if (!obviously_egal(a1, a2)) {
                            if (obvious_subtype(a2, a1, y0, subtype)) {
                                if (!*subtype)
                                    return 1;
                                if (jl_has_free_typevars(a1)) // a1 is actually more constrained that this
                                    uncertain = 1;
                            }
                            else {
                                uncertain = 1;
                            }
                            if (obvious_subtype(a1, a2, y0, subtype)) {
                                if (!*subtype)
                                    return 1;
                                if (jl_has_free_typevars(a2)) // a2 is actually more constrained that this
                                    uncertain = 1;
                            }
                            else {
                                uncertain = 1;
                            }
                        }
                    }
                    if (obvious_subtype(a, b, y0, subtype)) {
                        if (!*subtype)
                            return 1;
                        if (jl_has_free_typevars(b)) // b is actually more constrained that this
                            uncertain = 1;
                    }
                    else {
                        uncertain = 1;
                    }
                }
            }
            if (uncertain)
                return 0;
            *subtype = 1;
            return 1;
        }
    }
    return 0;
}

JL_DLLEXPORT int jl_obvious_subtype(jl_value_t *x, jl_value_t *y, int *subtype)
{
    return obvious_subtype(x, y, y, subtype);
}

// `env` is NULL if no typevar information is requested, or otherwise
// points to a rooted array of length `jl_subtype_env_size(y)`.
// This will be populated with the values of variables from unionall
// types at the outer level of `y`.
JL_DLLEXPORT int jl_subtype_env(jl_value_t *x, jl_value_t *y, jl_value_t **env, int envsz)
{
    jl_stenv_t e;
    if (y == (jl_value_t*)jl_any_type || x == jl_bottom_type)
        return 1;
    if (x == y ||
        (jl_typeof(x) == jl_typeof(y) &&
         (jl_is_unionall(y) || jl_is_uniontype(y)) &&
         jl_egal(x, y))) {
        if (envsz != 0) { // quickly copy env from x
            jl_unionall_t *ua = (jl_unionall_t*)x;
            int i;
            for (i = 0; i < envsz; i++) {
                assert(jl_is_unionall(ua));
                env[i] = (jl_value_t*)ua->var;
                ua = (jl_unionall_t*)ua->body;
            }
        }
        return 1;
    }
    int obvious_subtype = 2;
    if (jl_obvious_subtype(x, y, &obvious_subtype)) {
#ifdef NDEBUG
        if (obvious_subtype == 0)
            return obvious_subtype;
        else if (envsz == 0)
            return obvious_subtype;
#endif
    }
    else {
        obvious_subtype = 3;
    }
    init_stenv(&e, env, envsz);
    int subtype = forall_exists_subtype(x, y, &e, 0);
    assert(obvious_subtype == 3 || obvious_subtype == subtype || jl_has_free_typevars(x) || jl_has_free_typevars(y));
#ifndef NDEBUG
    if (obvious_subtype == 0 || (obvious_subtype == 1 && envsz == 0))
        subtype = obvious_subtype; // this ensures that running in a debugger doesn't change the result
#endif
    return subtype;
}

static int subtype_in_env_(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int invdepth, int Rinvdepth)
{
    jl_stenv_t e2;
    init_stenv(&e2, NULL, 0);
    e2.vars = e->vars;
    e2.intersection = e->intersection;
    e2.ignore_free = e->ignore_free;
    e2.invdepth = invdepth;
    e2.Rinvdepth = Rinvdepth;
    e2.envsz = e->envsz;
    e2.envout = e->envout;
    e2.envidx = e->envidx;
    return forall_exists_subtype(x, y, &e2, 0);
}

static int subtype_in_env(jl_value_t *x, jl_value_t *y, jl_stenv_t *e)
{
    return subtype_in_env_(x, y, e, e->invdepth, e->Rinvdepth);
}

static int subtype_bounds_in_env(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int R, int d)
{
    return subtype_in_env_(x, y, e, R ? e->invdepth : d, R ? d : e->Rinvdepth);
}

JL_DLLEXPORT int jl_subtype(jl_value_t *x, jl_value_t *y)
{
    return jl_subtype_env(x, y, NULL, 0);
}

JL_DLLEXPORT int jl_types_equal(jl_value_t *a, jl_value_t *b)
{
    if (obviously_egal(a, b))
        return 1;
    if (obviously_unequal(a, b))
        return 0;
    // the following is an interleaved version of:
    //   return jl_subtype(a, b) && jl_subtype(b, a)
    // where we try to do the fast checks before the expensive ones
    if (jl_is_datatype(a) && !jl_is_concrete_type(b)) {
        // if one type looks simpler, check it on the right
        // first in order to reject more quickly.
        jl_value_t *temp = a;
        a = b;
        b = temp;
    }
    // first check if a <: b has an obvious answer
    int subtype_ab = 2;
    if (b == (jl_value_t*)jl_any_type || a == jl_bottom_type) {
        subtype_ab = 1;
    }
    else if (jl_typeof(a) == jl_typeof(b) &&
        (jl_is_unionall(b) || jl_is_uniontype(b)) &&
        jl_egal(a, b)) {
        subtype_ab = 1;
    }
    else if (jl_obvious_subtype(a, b, &subtype_ab)) {
#ifdef NDEBUG
        if (subtype_ab == 0)
            return 0;
#endif
    }
    else {
        subtype_ab = 3;
    }
    // next check if b <: a has an obvious answer
    int subtype_ba = 2;
    if (a == (jl_value_t*)jl_any_type || b == jl_bottom_type) {
        subtype_ba = 1;
    }
    else if (jl_typeof(b) == jl_typeof(a) &&
        (jl_is_unionall(a) || jl_is_uniontype(a)) &&
        jl_egal(b, a)) {
        subtype_ba = 1;
    }
    else if (jl_obvious_subtype(b, a, &subtype_ba)) {
#ifdef NDEBUG
        if (subtype_ba == 0)
            return 0;
#endif
    }
    else {
        subtype_ba = 3;
    }
    // finally, do full subtyping for any inconclusive test
    jl_stenv_t e;
#ifdef NDEBUG
    if (subtype_ab != 1)
#endif
    {
        init_stenv(&e, NULL, 0);
        int subtype = forall_exists_subtype(a, b, &e, 0);
        assert(subtype_ab == 3 || subtype_ab == subtype || jl_has_free_typevars(a) || jl_has_free_typevars(b));
#ifndef NDEBUG
        if (subtype_ab != 0 && subtype_ab != 1) // ensures that running in a debugger doesn't change the result
#endif
        subtype_ab = subtype;
#ifdef NDEBUG
        if (subtype_ab == 0)
            return 0;
#endif
    }
#ifdef NDEBUG
    if (subtype_ba != 1)
#endif
    {
        init_stenv(&e, NULL, 0);
        int subtype = forall_exists_subtype(b, a, &e, 0);
        assert(subtype_ba == 3 || subtype_ba == subtype || jl_has_free_typevars(a) || jl_has_free_typevars(b));
#ifndef NDEBUG
        if (subtype_ba != 0 && subtype_ba != 1) // ensures that running in a debugger doesn't change the result
#endif
        subtype_ba = subtype;
    }
    // all tests successful
    return subtype_ab && subtype_ba;
}

JL_DLLEXPORT int jl_is_not_broken_subtype(jl_value_t *a, jl_value_t *b)
{
    // TODO: the final commented out check here isn't correct; it should be closer to the
    // `issingletype` check used by `isnotbrokensubtype` in `base/compiler/typeutils.jl`
    return !jl_is_kind(b) || !jl_is_type_type(a); // || jl_is_datatype_singleton((jl_datatype_t*)jl_tparam0(a));
}

int jl_tuple1_isa(jl_value_t *child1, jl_value_t **child, size_t cl, jl_datatype_t *pdt)
{
    if (jl_is_tuple_type(pdt) && !jl_is_va_tuple(pdt)) {
        if (cl != jl_nparams(pdt))
            return 0;
        size_t i;
        if (!jl_isa(child1, jl_tparam(pdt, 0)))
            return 0;
        for (i = 1; i < cl; i++) {
            if (!jl_isa(child[i - 1], jl_tparam(pdt, i)))
                return 0;
        }
        return 1;
    }
    jl_value_t *tu = (jl_value_t*)arg_type_tuple(child1, child, cl);
    int ans;
    JL_GC_PUSH1(&tu);
    ans = jl_subtype(tu, (jl_value_t*)pdt);
    JL_GC_POP();
    return ans;
}

int jl_tuple_isa(jl_value_t **child, size_t cl, jl_datatype_t *pdt)
{
    if (cl == 0) {
        if (pdt == jl_emptytuple_type)
            return 1;
        if (jl_is_tuple_type(pdt) && (jl_nparams(pdt) != 1 || !jl_is_va_tuple(pdt)))
            return 0;
        return jl_isa(jl_emptytuple, (jl_value_t*)pdt);
    }
    return jl_tuple1_isa(child[0], &child[1], cl, pdt);
}

// returns true if the intersection of `t` and `Type` is non-empty and not a kind
// this is sufficient to determine if `isa(x, T)` can instead simply check for `typeof(x) <: T`
int jl_has_intersect_type_not_kind(jl_value_t *t)
{
    t = jl_unwrap_unionall(t);
    if (t == (jl_value_t*)jl_any_type)
        return 1;
    if (jl_is_uniontype(t)) {
        return jl_has_intersect_type_not_kind(((jl_uniontype_t*)t)->a) ||
               jl_has_intersect_type_not_kind(((jl_uniontype_t*)t)->b);
    }
    if (jl_is_typevar(t)) {
        return jl_has_intersect_type_not_kind(((jl_tvar_t*)t)->ub);
    }
    if (jl_is_datatype(t)) {
        if (((jl_datatype_t*)t)->name == jl_type_typename)
            return 1;
    }
    return 0;
}

JL_DLLEXPORT int jl_isa(jl_value_t *x, jl_value_t *t)
{
    if (jl_typeis(x,t) || t == (jl_value_t*)jl_any_type)
        return 1;
    if (jl_is_type(x)) {
        if (t == (jl_value_t*)jl_type_type)
            return 1;
        if (!jl_has_free_typevars(x)) {
            if (jl_is_concrete_type(t))
                return 0;
            if (jl_is_type_type(t))
                return jl_types_equal(x, jl_tparam0(t));
            jl_value_t *t2 = jl_unwrap_unionall(t);
            if (jl_is_datatype(t2)) {
                if (((jl_datatype_t*)t2)->name == jl_type_typename) {
                    jl_value_t *tp = jl_tparam0(t2);
                    if (jl_is_typevar(tp)) {
                        if (((jl_tvar_t*)tp)->lb == jl_bottom_type) {
                            while (jl_is_typevar(tp))
                                tp = ((jl_tvar_t*)tp)->ub;
                            if (!jl_has_free_typevars(tp))
                                return jl_subtype(x, tp);
                        }
                        else if (((jl_tvar_t*)tp)->ub == (jl_value_t*)jl_any_type) {
                            while (jl_is_typevar(tp))
                                tp = ((jl_tvar_t*)tp)->lb;
                            if (!jl_has_free_typevars(tp))
                                return jl_subtype(tp, x);
                        }
                    }
                }
                else {
                    return 0;
                }
            }
            if (jl_subtype(jl_typeof(x), t))
                return 1;
            if (jl_has_intersect_type_not_kind(t2)) {
                JL_GC_PUSH1(&x);
                x = (jl_value_t*)jl_wrap_Type(x);  // TODO jb/subtype avoid jl_wrap_Type
                int ans = jl_subtype(x, t);
                JL_GC_POP();
                return ans;
            }
            return 0;
        }
    }
    if (jl_is_concrete_type(t))
        return 0;
    return jl_subtype(jl_typeof(x), t);
}

// type intersection

static jl_value_t *intersect(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int param);

static jl_value_t *intersect_all(jl_value_t *x, jl_value_t *y, jl_stenv_t *e);

// intersect in nested union environment, similar to subtype_ccheck
static jl_value_t *intersect_aside(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int R, int d)
{
    // band-aid for #30335
    if (x == (jl_value_t*)jl_any_type && !jl_is_typevar(y))
        return y;
    if (y == (jl_value_t*)jl_any_type && !jl_is_typevar(x))
        return x;

    jl_unionstate_t oldRunions = e->Runions;
    int savedepth = e->invdepth, Rsavedepth = e->Rinvdepth;
    // TODO: this doesn't quite make sense
    e->invdepth = e->Rinvdepth = d;

    jl_value_t *res = intersect_all(x, y, e);

    e->Runions = oldRunions;
    e->invdepth = savedepth;
    e->Rinvdepth = Rsavedepth;
    return res;
}

static jl_value_t *intersect_union(jl_value_t *x, jl_uniontype_t *u, jl_stenv_t *e, int8_t R, int param)
{
    if (param == 2 || (!jl_has_free_typevars(x) && !jl_has_free_typevars((jl_value_t*)u))) {
        jl_value_t *a=NULL, *b=NULL;
        JL_GC_PUSH2(&a, &b);
        jl_unionstate_t oldRunions = e->Runions;
        a = R ? intersect_all(x, u->a, e) : intersect_all(u->a, x, e);
        b = R ? intersect_all(x, u->b, e) : intersect_all(u->b, x, e);
        e->Runions = oldRunions;
        jl_value_t *i = simple_join(a,b);
        JL_GC_POP();
        return i;
    }
    jl_value_t *choice = pick_union_element((jl_value_t*)u, e, 1);
    // try all possible choices in covariant position; union them all together at the top level
    return R ? intersect(x, choice, e, param) : intersect(choice, x, e, param);
}

// set a variable to a non-type constant
static jl_value_t *set_var_to_const(jl_varbinding_t *bb, jl_value_t *v JL_MAYBE_UNROOTED, jl_varbinding_t *othervar)
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
        if (jl_unbox_long(v) != jl_unbox_long(bb->lb))
            return jl_bottom_type;
    }
    else if (!jl_egal(v, bb->lb)) {
        return jl_bottom_type;
    }
    return v;
}

static int try_subtype_in_env(jl_value_t *a, jl_value_t *b, jl_stenv_t *e, int R, int d)
{
    jl_value_t *root=NULL; jl_savedenv_t se;
    JL_GC_PUSH1(&root);
    save_env(e, &root, &se);
    int ret = subtype_bounds_in_env(a, b, e, R, d);
    restore_env(e, root, &se);
    free(se.buf);
    JL_GC_POP();
    return ret;
}

static void set_bound(jl_value_t **bound, jl_value_t *val, jl_tvar_t *v, jl_stenv_t *e) JL_NOTSAFEPOINT
{
    if (in_union(val, (jl_value_t*)v))
        return;
    jl_varbinding_t *btemp = e->vars;
    while (btemp != NULL) {
        if (btemp->lb == (jl_value_t*)v && btemp->ub == (jl_value_t*)v &&
            in_union(val, (jl_value_t*)btemp->var))
            return;
        btemp = btemp->prev;
    }
    *bound = val;
}

// subtype, treating all vars as existential
static int subtype_in_env_existential(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int R, int d)
{
    jl_varbinding_t *v = e->vars;
    int len = 0;
    if (x == jl_bottom_type || y == (jl_value_t*)jl_any_type)
        return 1;
    while (v != NULL) {
        len++;
        v = v->prev;
    }
    int8_t *rs = (int8_t*)malloc_s(len);
    int n = 0;
    v = e->vars;
    while (n < len) {
        assert(v != NULL);
        rs[n++] = v->right;
        v->right = 1;
        v = v->prev;
    }
    int issub = subtype_bounds_in_env(x, y, e, R, d);
    n = 0; v = e->vars;
    while (n < len) {
        assert(v != NULL);
        v->right = rs[n++];
        v = v->prev;
    }
    free(rs);
    return issub;
}

static jl_value_t *intersect_var(jl_tvar_t *b, jl_value_t *a, jl_stenv_t *e, int8_t R, int param)
{
    jl_varbinding_t *bb = lookup(e, b);
    if (bb == NULL)
        return R ? intersect_aside(a, b->ub, e, 1, 0) : intersect_aside(b->ub, a, e, 0, 0);
    if (bb->lb == bb->ub && jl_is_typevar(bb->lb) && bb->lb != (jl_value_t*)b)
        return intersect(a, bb->lb, e, param);
    if (!jl_is_type(a) && !jl_is_typevar(a))
        return set_var_to_const(bb, a, NULL);
    int d = bb->depth0;
    jl_value_t *root=NULL; jl_savedenv_t se;
    if (param == 2) {
        jl_value_t *ub = NULL;
        JL_GC_PUSH2(&ub, &root);
        if (!jl_has_free_typevars(a)) {
            save_env(e, &root, &se);
            int issub = subtype_in_env_existential(bb->lb, a, e, 0, d) && subtype_in_env_existential(a, bb->ub, e, 1, d);
            restore_env(e, root, &se);
            free(se.buf);
            if (!issub) {
                JL_GC_POP();
                return jl_bottom_type;
            }
            ub = a;
        }
        else {
            ub = R ? intersect_aside(a, bb->ub, e, 1, d) : intersect_aside(bb->ub, a, e, 0, d);
            // TODO: we should probably check `bb->lb <: ub` here; find a test case for that
        }
        if (ub != (jl_value_t*)b) {
            if (jl_has_free_typevars(ub)) {
                // constraint X == Ref{X} is unsatisfiable. also check variables set equal to X.
                if (var_occurs_inside(ub, b, 0, 0)) {
                    JL_GC_POP();
                    return jl_bottom_type;
                }
                jl_varbinding_t *btemp = e->vars;
                while (btemp != NULL) {
                    if (btemp->lb == (jl_value_t*)b && btemp->ub == (jl_value_t*)b &&
                        var_occurs_inside(ub, btemp->var, 0, 0)) {
                        JL_GC_POP();
                        return jl_bottom_type;
                    }
                    btemp = btemp->prev;
                }
            }
            bb->ub = ub;
            bb->lb = ub;
        }
        JL_GC_POP();
        return ub;
    }
    else if (bb->constraintkind == 0) {
        if (!jl_is_typevar(bb->ub) && !jl_is_typevar(a)) {
            if (try_subtype_in_env(bb->ub, a, e, 0, d))
                return (jl_value_t*)b;
        }
        return R ? intersect_aside(a, bb->ub, e, 1, d) : intersect_aside(bb->ub, a, e, 0, d);
    }
    else if (bb->concrete || bb->constraintkind == 1) {
        jl_value_t *ub = R ? intersect_aside(a, bb->ub, e, 1, d) : intersect_aside(bb->ub, a, e, 0, d);
        if (ub == jl_bottom_type)
            return jl_bottom_type;
        JL_GC_PUSH1(&ub);
        if (!R && !subtype_bounds_in_env(bb->lb, a, e, 0, d)) {
            // this fixes issue #30122. TODO: better fix for R flag.
            JL_GC_POP();
            return jl_bottom_type;
        }
        JL_GC_POP();
        set_bound(&bb->ub, ub, b, e);
        return (jl_value_t*)b;
    }
    else if (bb->constraintkind == 2) {
        // TODO: removing this case fixes many test_brokens in test/subtype.jl
        // but breaks other tests.
        if (!subtype_bounds_in_env(a, bb->ub, e, 1, d)) {
            // mark var as unsatisfiable by making it circular
            bb->lb = (jl_value_t*)b;
            return jl_bottom_type;
        }
        jl_value_t *lb = simple_join(bb->lb, a);
        set_bound(&bb->lb, lb, b, e);
        return a;
    }
    assert(bb->constraintkind == 3);
    jl_value_t *ub = R ? intersect_aside(a, bb->ub, e, 1, d) : intersect_aside(bb->ub, a, e, 0, d);
    if (ub == jl_bottom_type)
        return jl_bottom_type;
    if (jl_is_typevar(a))
        return (jl_value_t*)b;
    if (ub == a) {
        if (bb->lb == jl_bottom_type) {
            set_bound(&bb->ub, a, b, e);
            return (jl_value_t*)b;
        }
        return ub;
    }
    else if (bb->ub == bb->lb) {
        return ub;
    }
    root = NULL;
    JL_GC_PUSH2(&root, &ub);
    save_env(e, &root, &se);
    jl_value_t *ii = R ? intersect_aside(a, bb->lb, e, 1, d) : intersect_aside(bb->lb, a, e, 0, d);
    if (ii == jl_bottom_type) {
        restore_env(e, root, &se);
        ii = (jl_value_t*)b;
        set_bound(&bb->ub, ub, b, e);
    }
    free(se.buf);
    JL_GC_POP();
    return ii;
}

// test whether `var` occurs inside constructors. `want_inv` tests only inside
// invariant constructors. `inside` means we are currently inside a constructor of the
// requested kind.
static int var_occurs_inside(jl_value_t *v, jl_tvar_t *var, int inside, int want_inv) JL_NOTSAFEPOINT
{
    if (v == (jl_value_t*)var) {
        return inside;
    }
    else if (jl_is_uniontype(v)) {
        return var_occurs_inside(((jl_uniontype_t*)v)->a, var, inside, want_inv) ||
            var_occurs_inside(((jl_uniontype_t*)v)->b, var, inside, want_inv);
    }
    else if (jl_is_unionall(v)) {
        jl_unionall_t *ua = (jl_unionall_t*)v;
        if (ua->var == var)
            return 0;
        if (var_occurs_inside(ua->var->lb, var, inside, want_inv) || var_occurs_inside(ua->var->ub, var, inside, want_inv))
            return 1;
        return var_occurs_inside(ua->body, var, inside, want_inv);
    }
    else if (jl_is_datatype(v)) {
        size_t i;
        int istuple = jl_is_tuple_type(v);
        int isva = jl_is_vararg_type(v);
        for (i=0; i < jl_nparams(v); i++) {
            int invar = isva ? i == 1 : !istuple;
            int ins_i = inside || !want_inv || invar;
            if (var_occurs_inside(jl_tparam(v,i), var, ins_i, want_inv))
                return 1;
        }
    }
    return 0;
}

// Caller might not have rooted `res`
static jl_value_t *finish_unionall(jl_value_t *res JL_MAYBE_UNROOTED, jl_varbinding_t *vb, jl_stenv_t *e)
{
    jl_value_t *varval = NULL;
    jl_tvar_t *newvar = vb->var;
    JL_GC_PUSH2(&res, &newvar);
    // try to reduce var to a single value
    if (jl_is_long(vb->ub) && jl_is_typevar(vb->lb)) {
        varval = vb->ub;
    }
    else if (obviously_egal(vb->lb, vb->ub)) {
        // given x<:T<:x, substitute x for T
        varval = vb->ub;
    }
    else if (!vb->occurs_inv && is_leaf_bound(vb->ub)) {
        // replace T<:x with x in covariant position when possible
        varval = vb->ub;
    }

    if (vb->intvalued) {
        if ((varval && jl_is_long(varval)) ||
            (vb->lb == jl_bottom_type && vb->ub == (jl_value_t*)jl_any_type) ||
            (jl_is_typevar(vb->lb) && vb->ub == vb->lb)) {
            // int-valued typevar must either be an Int, or have Bottom-Any bounds,
            // or be set equal to another typevar.
        }
        else {
            JL_GC_POP();
            return jl_bottom_type;
        }
    }

    if (!varval && (vb->lb != vb->var->lb || vb->ub != vb->var->ub))
        newvar = jl_new_typevar(vb->var->name, vb->lb, vb->ub);

    // remove/replace/rewrap free occurrences of this var in the environment
    jl_varbinding_t *btemp = e->vars;
    int wrap = 1;
    while (btemp != NULL) {
        if (jl_has_typevar(btemp->lb, vb->var)) {
            if (vb->lb == (jl_value_t*)btemp->var) {
                JL_GC_POP();
                return jl_bottom_type;
            }
            if (varval) {
                JL_TRY {
                    btemp->lb = jl_substitute_var(btemp->lb, vb->var, varval);
                }
                JL_CATCH {
                    res = jl_bottom_type;
                }
            }
            else if (btemp->lb == (jl_value_t*)vb->var)
                btemp->lb = vb->lb;
            else if (btemp->depth0 == vb->depth0 && !jl_has_typevar(vb->lb, btemp->var) &&
                     !jl_has_typevar(vb->ub, btemp->var) && jl_has_typevar(btemp->ub, vb->var)) {
                // if our variable is T, and some outer variable has constraint S = Ref{T},
                // move the `where T` outside `where S` instead of putting it here. issue #21243.
                if (btemp->innervars == NULL)
                    btemp->innervars = jl_alloc_array_1d(jl_array_any_type, 0);
                if (newvar != vb->var) {
                    btemp->lb = jl_substitute_var(btemp->lb, vb->var, (jl_value_t*)newvar);
                    btemp->ub = jl_substitute_var(btemp->ub, vb->var, (jl_value_t*)newvar);
                }
                jl_array_ptr_1d_push(btemp->innervars, (jl_value_t*)newvar);
                wrap = 0;
                btemp = btemp->prev;
                continue;
            }
            else
                btemp->lb = jl_new_struct(jl_unionall_type, vb->var, btemp->lb);
            assert((jl_value_t*)btemp->var != btemp->lb);
        }
        if (jl_has_typevar(btemp->ub, vb->var)) {
            if (vb->ub == (jl_value_t*)btemp->var) {
                JL_GC_POP();
                return jl_bottom_type;
            }
            if (varval) {
                JL_TRY {
                    btemp->ub = jl_substitute_var(btemp->ub, vb->var, varval);
                }
                JL_CATCH {
                    res = jl_bottom_type;
                }
            }
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
        if (varval) {
            JL_TRY {
                // you can construct `T{x} where x` even if T's parameter is actually
                // limited. in that case we might get an invalid instantiation here.
                res = jl_substitute_var(res, vb->var, varval);
                // simplify chains of UnionAlls where bounds become equal
                while (jl_is_unionall(res) && obviously_egal(((jl_unionall_t*)res)->var->lb,
                                                             ((jl_unionall_t*)res)->var->ub))
                    res = jl_instantiate_unionall((jl_unionall_t*)res, ((jl_unionall_t*)res)->var->lb);
            }
            JL_CATCH {
                res = jl_bottom_type;
            }
        }
        else {
            if (newvar != vb->var)
                res = jl_substitute_var(res, vb->var, (jl_value_t*)newvar);
            varval = (jl_value_t*)newvar;
            if (wrap)
                res = jl_new_struct(jl_unionall_type, (jl_tvar_t*)newvar, res);
        }
    }

    if (res != jl_bottom_type && vb->innervars != NULL) {
        int i;
        for(i=0; i < jl_array_len(vb->innervars); i++) {
            jl_tvar_t *var = (jl_tvar_t*)jl_array_ptr_ref(vb->innervars, i);
            if (jl_has_typevar(res, var))
                res = jl_new_struct(jl_unionall_type, (jl_tvar_t*)var, res);
        }
    }

    if (vb->right && e->envidx < e->envsz) {
        jl_value_t *oldval = e->envout[e->envidx];
        if (!varval || (!is_leaf_bound(varval) && !vb->occurs_inv))
            e->envout[e->envidx] = (jl_value_t*)vb->var;
        else if (!(oldval && jl_is_typevar(oldval) && jl_is_long(varval)))
            e->envout[e->envidx] = fix_inferred_var_bound(vb->var, varval);
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
    jl_value_t *res;
    if (R) {
        e->envidx++;
        res = intersect(t, u->body, e, param);
        e->envidx--;
    }
    else {
        res = intersect(u->body, t, e, param);
    }
    vb->concrete |= (vb->occurs_cov > 1 && is_leaf_typevar(u->var) &&
                     !var_occurs_invariant(u->body, u->var, 0));

    // handle the "diagonal dispatch" rule, which says that a type var occurring more
    // than once, and only in covariant position, is constrained to concrete types. E.g.
    //  ( Tuple{Int, Int}    <: Tuple{T, T} where T) but
    // !( Tuple{Int, String} <: Tuple{T, T} where T)
    // Then check concreteness by checking that the lower bound is not an abstract type.
    if (res != jl_bottom_type && vb->concrete) {
        if (jl_is_typevar(vb->lb)) {
        }
        else if (!is_leaf_bound(vb->lb)) {
            res = jl_bottom_type;
        }
    }

    e->vars = vb->prev;

    if (res != jl_bottom_type) {
        if (vb->ub == jl_bottom_type && vb->occurs_cov) {
            // T=Bottom in covariant position
            res = jl_bottom_type;
        }
        else if (jl_has_typevar(vb->lb, u->var) || jl_has_typevar(vb->ub, u->var)) {
            // fail on circular constraints
            res = jl_bottom_type;
        }
    }
    if (res != jl_bottom_type)
        // res is rooted by callee
        res = finish_unionall(res, vb, e);
    JL_GC_POP();
    return res;
}

static jl_value_t *intersect_unionall(jl_value_t *t, jl_unionall_t *u, jl_stenv_t *e, int8_t R, int param)
{
    jl_value_t *res=NULL, *res2=NULL, *save=NULL, *save2=NULL;
    jl_savedenv_t se, se2;
    jl_varbinding_t vb = { u->var, u->var->lb, u->var->ub, R, 0, 0, 0, 0,
                           R ? e->Rinvdepth : e->invdepth, 0, NULL, 0, e->vars };
    JL_GC_PUSH6(&res, &save2, &vb.lb, &vb.ub, &save, &vb.innervars);
    save_env(e, &save, &se);
    res = intersect_unionall_(t, u, e, R, param, &vb);
    if (res != jl_bottom_type) {
        if (vb.concrete || vb.occurs_inv>1 || u->var->lb != jl_bottom_type || (vb.occurs_inv && vb.occurs_cov)) {
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
            vb.constraintkind = 1;
            res2 = intersect_unionall_(t, u, e, R, param, &vb);
            if (res2 == jl_bottom_type) {
                restore_env(e, save, &se);
                vb.occurs_cov = vb.occurs_inv = 0;
                vb.lb = u->var->lb; vb.ub = u->var->ub;
                vb.constraintkind = 2;
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
    jl_tvar_t *va_p1=NULL;
    jl_datatype_t *tail = unwrap_1_unionall(v, &va_p1);
    jl_value_t *N = jl_tparam1(tail);
    // only do the check if N is free in the tuple type's last parameter
    if (jl_is_typevar(N) && N != (jl_value_t*)va_p1) {
        jl_value_t *len = jl_box_long(n);
        JL_GC_PUSH1(&len);
        jl_value_t *il = R ? intersect(len, N, e, 2) : intersect(N, len, e, 2);
        JL_GC_POP();
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
    int vx=0, vy=0, vvx = (lx > 0 && jl_is_vararg_type(jl_tparam(xd, lx-1)));
    int vvy = (ly > 0 && jl_is_vararg_type(jl_tparam(yd, ly-1)));
    if (!vvx && !vvy && lx != ly)
        return jl_bottom_type;
    jl_svec_t *params = jl_alloc_svec(lx > ly ? lx : ly);
    jl_value_t *res=NULL;
    JL_GC_PUSH1(&params);
    size_t i=0, j=0;
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
        if (vx && vy) {
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
                    (yb && jl_is_long(yb->lb) && ly-1+jl_unbox_long(yb->lb) != len)) {
                    res = jl_bottom_type;
                }
                else if (param == 2 && jl_is_unionall(xi) != jl_is_unionall(yi)) {
                    res = jl_bottom_type;
                }
                else {
                    if (xb) set_var_to_const(xb, jl_box_long(len-lx+1), yb);
                    if (yb) set_var_to_const(yb, jl_box_long(len-ly+1), xb);
                    res = (jl_value_t*)jl_apply_tuple_type_v(jl_svec_data(params), len);
                }
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
    if (jl_nparams(xd) == 0 || jl_nparams(xd->super) == 0 || !jl_has_free_typevars(xd))
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
                // the exact typevar from the type's `wrapper`, or a free typevar.
                jl_value_t *ei = env[i];
                if (ei == (jl_value_t*)((jl_unionall_t*)wr)->var ||
                    (jl_is_typevar(ei) && lookup(e, (jl_tvar_t*)ei) == NULL))
                    env[i] = jl_tparam(xd,i);
                wr = ((jl_unionall_t*)wr)->body;
            }
            JL_TRY {
                ii = jl_apply_type(wrapper, env, envsz);
            }
            JL_CATCH {
                ii = jl_bottom_type;
            }
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
    e->Rinvdepth++;
    jl_value_t *ii = intersect(x, y, e, 2);
    e->invdepth--;
    e->Rinvdepth--;
    if (jl_is_typevar(x) && jl_is_typevar(y) && (jl_is_typevar(ii) || !jl_is_type(ii)))
        return ii;
    if (ii == jl_bottom_type) {
        if (!subtype_in_env(x, jl_bottom_type, e))
            return NULL;
        flip_vars(e);
        if (!subtype_in_env(y, jl_bottom_type, e)) {
            flip_vars(e);
            return NULL;
        }
        flip_vars(e);
        return jl_bottom_type;
    }
    jl_value_t *root=NULL;
    jl_savedenv_t se;
    JL_GC_PUSH2(&ii, &root);
    save_env(e, &root, &se);
    if (!subtype_in_env_existential(x, y, e, 0, e->invdepth)) {
        ii = NULL;
    }
    else {
        if (!subtype_in_env_existential(y, x, e, 0, e->invdepth))
            ii = NULL;
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
    if (!jl_is_kind(y)) return jl_bottom_type;
    if (y == (jl_value_t*)jl_typeofbottom_type && ((jl_tvar_t*)p0)->lb == jl_bottom_type)
        return (jl_value_t*)jl_wrap_Type(jl_bottom_type);
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
                if (R) flip_vars(e);
                int ccheck = subtype_in_env(xlb, yub, e) && subtype_in_env(ylb, xub, e);
                if (R) flip_vars(e);
                if (!ccheck)
                    return jl_bottom_type;
                if (var_occurs_inside(xub, (jl_tvar_t*)y, 0, 0) && var_occurs_inside(yub, (jl_tvar_t*)x, 0, 0)) {
                    // circular constraint. the result will be Bottom, but in the meantime
                    // we need to avoid computing intersect(xub, yub) since it won't terminate.
                    return y;
                }
                jl_value_t *ub=NULL, *lb=NULL;
                JL_GC_PUSH2(&lb, &ub);
                ub = intersect_aside(xub, yub, e, 0, xx ? xx->depth0 : 0);
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
    if (!jl_has_free_typevars(x) && !jl_has_free_typevars(y) && !jl_is_vararg_type(x) && !jl_is_vararg_type(y)) {
        if (jl_subtype(x, y)) return x;
        if (jl_subtype(y, x)) return y;
    }
    if (jl_is_uniontype(x)) {
        if (y == ((jl_uniontype_t*)x)->a || y == ((jl_uniontype_t*)x)->b)
            return y;
        return intersect_union(y, (jl_uniontype_t*)x, e, 0, param);
    }
    if (jl_is_uniontype(y)) {
        if (x == ((jl_uniontype_t*)y)->a || x == ((jl_uniontype_t*)y)->b)
            return x;
        if (jl_is_unionall(x) && (jl_has_free_typevars(x) || jl_has_free_typevars(y)))
            return intersect_unionall(y, (jl_unionall_t*)x, e, 0, param);
        return intersect_union(x, (jl_uniontype_t*)y, e, 1, param);
    }
    if (y == (jl_value_t*)jl_any_type) return x;
    if (x == (jl_value_t*)jl_any_type) return y;
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
                if (jl_is_typevar(xp2)) {
                    jl_varbinding_t *xb = lookup(e, (jl_tvar_t*)xp2);
                    if (xb) xb->intvalued = 1;
                }
                if (jl_is_typevar(yp2)) {
                    jl_varbinding_t *yb = lookup(e, (jl_tvar_t*)yp2);
                    if (yb) yb->intvalued = 1;
                }
                // Vararg{T,N} <: Vararg{T2,N2}; equate N and N2
                i2 = intersect_invariant(xp2, yp2, e);
                if (i2 == NULL || i2 == jl_bottom_type || (jl_is_long(i2) && jl_unbox_long(i2) < 0) ||
                    !((jl_is_typevar(i2) && ((jl_tvar_t*)i2)->lb == jl_bottom_type &&
                       ((jl_tvar_t*)i2)->ub == (jl_value_t*)jl_any_type) || jl_is_long(i2)))
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
            jl_value_t *res = jl_bottom_type;
            if (i >= np) {
                JL_TRY {
                    res = jl_apply_type(xd->name->wrapper, newparams, np);
                }
                JL_CATCH {
                    res = jl_bottom_type;
                }
            }
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

static jl_value_t *intersect_all(jl_value_t *x, jl_value_t *y, jl_stenv_t *e)
{
    e->Runions.depth = 0;
    e->Runions.more = 0;
    memset(e->Runions.stack, 0, sizeof(e->Runions.stack));
    jl_value_t **is;
    JL_GC_PUSHARGS(is, 3);
    jl_value_t **saved = &is[2];
    jl_savedenv_t se;
    save_env(e, saved, &se);
    int lastset = 0, niter = 0, total_iter = 0;
    jl_value_t *ii = intersect(x, y, e, 0);
    is[0] = ii;  // root
    if (ii == jl_bottom_type) {
        restore_env(e, *saved, &se);
    }
    else {
        free(se.buf);
        save_env(e, saved, &se);
    }
    while (e->Runions.more) {
        if (e->emptiness_only && ii != jl_bottom_type) {
            free(se.buf);
            JL_GC_POP();
            return ii;
        }
        e->Runions.depth = 0;
        int set = e->Runions.more - 1;
        e->Runions.more = 0;
        statestack_set(&e->Runions, set, 1);
        for (int i = set + 1; i <= lastset; i++)
            statestack_set(&e->Runions, i, 0);
        lastset = set;

        is[0] = ii;
        is[1] = intersect(x, y, e, 0);
        if (is[1] == jl_bottom_type) {
            restore_env(e, *saved, &se);
        }
        else {
            free(se.buf);
            save_env(e, saved, &se);
        }
        if (is[0] == jl_bottom_type)
            ii = is[1];
        else if (is[1] == jl_bottom_type)
            ii = is[0];
        else {
            // TODO: the repeated subtype checks in here can get expensive
            ii = jl_type_union(is, 2);
            niter++;
        }
        total_iter++;
        if (niter > 3 || total_iter > 400000) {
            free(se.buf);
            JL_GC_POP();
            return y;
        }
    }
    free(se.buf);
    JL_GC_POP();
    return ii;
}

// type intersection entry points

static jl_value_t *intersect_types(jl_value_t *x, jl_value_t *y, int emptiness_only)
{
    jl_stenv_t e;
    if (obviously_disjoint(x, y, 0))
        return jl_bottom_type;
    if (jl_is_dispatch_tupletype(x) || jl_is_dispatch_tupletype(y)) {
        if (jl_subtype(x, y))
            return x;
        else if (jl_subtype(y, x))
            return y;
        else
            return jl_bottom_type;
    }
    init_stenv(&e, NULL, 0);
    e.intersection = e.ignore_free = 1;
    e.emptiness_only = emptiness_only;
    return intersect_all(x, y, &e);
}

JL_DLLEXPORT jl_value_t *jl_intersect_types(jl_value_t *x, jl_value_t *y)
{
    return intersect_types(x, y, 0);
}

// TODO: this can probably be done more efficiently
JL_DLLEXPORT int jl_has_empty_intersection(jl_value_t *x, jl_value_t *y)
{
    return intersect_types(x, y, 1) == jl_bottom_type;
}

// return a SimpleVector of all vars from UnionAlls wrapping a given type
jl_svec_t *jl_outer_unionall_vars(jl_value_t *u)
{
    int ntvars = jl_subtype_env_size((jl_value_t*)u);
    jl_svec_t *vec = jl_alloc_svec_uninit(ntvars);
    jl_unionall_t *ua = (jl_unionall_t*)u;
    int i;
    for (i = 0; i < ntvars; i++) {
        assert(jl_is_unionall(ua));
        jl_svecset(vec, i, ua->var);
        ua = (jl_unionall_t*)ua->body;
    }
    return vec;
}

// For (possibly unions or unionalls of) tuples `a` and `b`, return the tuple of
// pointwise unions. Note that this may in general be wider than `Union{a,b}`.
// If `a` and `b` are not (non va-)tuples of equal length (or unions or unionalls
// of such), return NULL.
jl_value_t *switch_union_tuple(jl_value_t *a, jl_value_t *b)
{
    if (jl_is_unionall(a)) {
        jl_value_t *ans = switch_union_tuple(((jl_unionall_t*)a)->body, b);
        if (ans == NULL)
            return NULL;
        JL_GC_PUSH1(&ans);
        ans = jl_type_unionall(((jl_unionall_t*)a)->var, ans);
        JL_GC_POP();
        return ans;
    }
    if (jl_is_unionall(b)) {
        jl_value_t *ans = switch_union_tuple(a, ((jl_unionall_t*)b)->body);
        if (ans == NULL)
            return NULL;
        JL_GC_PUSH1(&ans);
        ans = jl_type_unionall(((jl_unionall_t*)b)->var, ans);
        JL_GC_POP();
        return ans;
    }
    if (jl_is_uniontype(a)) {
        a = switch_union_tuple(((jl_uniontype_t*)a)->a, ((jl_uniontype_t*)a)->b);
        if (a == NULL)
            return NULL;
        JL_GC_PUSH1(&a);
        jl_value_t *ans = switch_union_tuple(a, b);
        JL_GC_POP();
        return ans;
    }
    if (jl_is_uniontype(b)) {
        b = switch_union_tuple(((jl_uniontype_t*)b)->a, ((jl_uniontype_t*)b)->b);
        if (b == NULL)
            return NULL;
        JL_GC_PUSH1(&b);
        jl_value_t *ans = switch_union_tuple(a, b);
        JL_GC_POP();
        return ans;
    }
    if (!jl_is_tuple_type(a) || !jl_is_tuple_type(b)) {
        return NULL;
    }
    if (jl_nparams(a) != jl_nparams(b) || jl_is_va_tuple((jl_datatype_t*)a) ||
            jl_is_va_tuple((jl_datatype_t*)b)) {
        return NULL;
    }
    jl_svec_t *vec = jl_alloc_svec(jl_nparams(a));
    JL_GC_PUSH1(&vec);
    for (int i = 0; i < jl_nparams(a); i++) {
        jl_value_t *ts[2];
        ts[0] = jl_tparam(a, i);
        ts[1] = jl_tparam(b, i);
        jl_svecset(vec, i, jl_type_union(ts, 2));
    }
    jl_value_t *ans = (jl_value_t*)jl_apply_tuple_type(vec);
    JL_GC_POP();
    return ans;
}

// `a` might have a non-empty intersection with some concrete type b even if !(a<:b) and !(b<:a)
// For example a=`Tuple{Type{<:Vector}}` and b=`Tuple{DataType}`
int might_intersect_concrete(jl_value_t *a)
{
    if (jl_is_unionall(a))
        a = jl_unwrap_unionall(a);
    if (jl_is_typevar(a))
        return 1; // (maybe)
    if (jl_is_uniontype(a))
        return might_intersect_concrete(((jl_uniontype_t*)a)->a) ||
               might_intersect_concrete(((jl_uniontype_t*)a)->b);
    if (jl_is_vararg_type(a))
        return might_intersect_concrete(jl_tparam0(a));
    if (jl_is_type_type(a))
        return 1;
    if (jl_is_datatype(a)) {
        int tpl = jl_is_tuple_type(a);
        int i, n = jl_nparams(a);
        for (i = 0; i < n; i++) {
            jl_value_t *p = jl_tparam(a, i);
            if (jl_is_typevar(p))
                return 1;
            if (tpl && p == jl_bottom_type)
                return 1;
            if (tpl && might_intersect_concrete(p))
                return 1;
        }
    }
    return 0;
}

// sets *issubty to 1 iff `a` is a subtype of `b`
jl_value_t *jl_type_intersection_env_s(jl_value_t *a, jl_value_t *b, jl_svec_t **penv, int *issubty)
{
    if (issubty) *issubty = 0;
    if (obviously_disjoint(a, b, 0)) {
        if (issubty && a == jl_bottom_type) *issubty = 1;
        return jl_bottom_type;
    }
    int szb = jl_subtype_env_size(b);
    int sz = 0, i = 0;
    jl_value_t **env, **ans;
    JL_GC_PUSHARGS(env, szb+1);
    ans = &env[szb];
    *ans = jl_bottom_type;
    int lta = jl_is_concrete_type(a);
    int ltb = jl_is_concrete_type(b);
    if (jl_subtype_env(a, b, env, szb)) {
        *ans = a; sz = szb;
        if (issubty) *issubty = 1;
    }
    else if (lta && ltb) {
        goto bot;
    }
    else if (jl_subtype(b, a)) {
        *ans = b;
    }
    else {
        // TODO: these tests could probably be ordered better with above
        if (lta && !might_intersect_concrete(b))
            goto bot;
        if (ltb && !might_intersect_concrete(a))
            goto bot;
        jl_stenv_t e;
        init_stenv(&e, NULL, 0);
        e.intersection = e.ignore_free = 1;
        e.envout = env;
        if (szb)
            memset(env, 0, szb*sizeof(void*));
        e.envsz = szb;
        *ans = intersect_all(a, b, &e);
        if (*ans == jl_bottom_type) goto bot;
        // TODO: code dealing with method signatures is not able to handle unions, so if
        // `a` and `b` are both tuples, we need to be careful and may not return a union,
        // even if `intersect` produced one
        int env_from_subtype = 1;
        if (jl_is_tuple_type(jl_unwrap_unionall(a)) && jl_is_tuple_type(jl_unwrap_unionall(b)) &&
            !jl_is_datatype(jl_unwrap_unionall(*ans))) {
            jl_value_t *ans_unwrapped = jl_unwrap_unionall(*ans);
            JL_GC_PUSH1(&ans_unwrapped);
            if (jl_is_uniontype(ans_unwrapped)) {
                ans_unwrapped = switch_union_tuple(((jl_uniontype_t*)ans_unwrapped)->a, ((jl_uniontype_t*)ans_unwrapped)->b);
                if (ans_unwrapped != NULL) {
                    *ans = jl_rewrap_unionall(ans_unwrapped, *ans);
                }
            }
            JL_GC_POP();
            if (!jl_is_datatype(jl_unwrap_unionall(*ans))) {
                *ans = b;
                env_from_subtype = 0;
            }
        }
        if (env_from_subtype) {
            sz = szb;
            // TODO: compute better `env` directly during intersection.
            // for now, we attempt to compute env by using subtype on the intersection result
            if (szb > 0 && !jl_types_equal(b, (jl_value_t*)jl_type_type)) {
                if (!jl_subtype_env(*ans, b, env, szb)) {
                    sz = 0;
                }
            }
        }
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

jl_value_t *jl_type_intersection_env(jl_value_t *a, jl_value_t *b, jl_svec_t **penv)
{
    return jl_type_intersection_env_s(a, b, penv, NULL);
}

JL_DLLEXPORT jl_value_t *jl_type_intersection(jl_value_t *a, jl_value_t *b)
{
    return jl_type_intersection_env(a, b, NULL);
}

JL_DLLEXPORT jl_svec_t *jl_type_intersection_with_env(jl_value_t *a, jl_value_t *b)
{
    jl_svec_t *env = jl_emptysvec;
    jl_value_t *ti = NULL;
    JL_GC_PUSH2(&env, &ti);
    ti = jl_type_intersection_env(a, b, &env);
    jl_svec_t *pair = jl_svec2(ti, env);
    JL_GC_POP();
    return pair;
}

int jl_subtype_matching(jl_value_t *a, jl_value_t *b, jl_svec_t **penv)
{
    int szb = penv ? jl_subtype_env_size(b) : 0;
    if (szb == 0)
        return jl_subtype_env(a, b, NULL, szb);

    jl_value_t **env;
    JL_GC_PUSHARGS(env, szb);
    int sub = jl_subtype_env(a, b, env, szb);
    if (sub) {
        // copy env to svec for return
        int i = 0;
        jl_svec_t *e = jl_alloc_svec(szb);
        *penv = e;
        for (i = 0; i < szb; i++)
            jl_svecset(e, i, env[i]);
    }
    JL_GC_POP();
    return sub;
}


// specificity comparison

static int eq_msp(jl_value_t *a, jl_value_t *b, jl_typeenv_t *env)
{
    if (!(jl_is_type(a) || jl_is_typevar(a)) ||
        !(jl_is_type(b) || jl_is_typevar(b)))
        return jl_egal(a, b);
    JL_GC_PUSH2(&a, &b);
    jl_typeenv_t *e = env;
    while (e != NULL) {
        a = jl_type_unionall(e->var, a);
        b = jl_type_unionall(e->var, b);
        e = e->prev;
    }
    int eq = jl_types_equal(a, b);
    JL_GC_POP();
    return eq;
}

static int sub_msp(jl_value_t *a, jl_value_t *b, jl_typeenv_t *env)
{
    JL_GC_PUSH2(&a, &b);
    while (env != NULL) {
        if (jl_is_type(a) || jl_is_typevar(a))
            a = jl_type_unionall(env->var, a);
        if (jl_is_type(b) || jl_is_typevar(b))
            b = jl_type_unionall(env->var, b);
        env = env->prev;
    }
    int sub = jl_subtype(a, b);
    JL_GC_POP();
    return sub;
}

static int type_morespecific_(jl_value_t *a, jl_value_t *b, int invariant, jl_typeenv_t *env);

static int num_occurs(jl_tvar_t *v, jl_typeenv_t *env);

static jl_value_t *nth_tuple_elt(jl_datatype_t *t JL_PROPAGATES_ROOT, size_t i) JL_NOTSAFEPOINT
{
    size_t len = jl_nparams(t);
    if (len == 0)
        return NULL;
    if (i < len-1)
        return jl_tparam(t, i);
    jl_value_t *last = jl_unwrap_unionall(jl_tparam(t, len-1));
    if (jl_is_vararg_type(last)) {
        jl_value_t *n = jl_tparam1(last);
        if (jl_is_long(n) && i >= len-1+jl_unbox_long(n))
            return NULL;
        return jl_tparam0(last);
    }
    if (i == len-1)
        return jl_tparam(t, i);
    return NULL;
}

static int tuple_morespecific(jl_datatype_t *cdt, jl_datatype_t *pdt, int invariant, jl_typeenv_t *env)
{
    size_t plen = jl_nparams(pdt);
    if (plen == 0) return 0;
    size_t clen = jl_nparams(cdt);
    if (clen == 0) return 1;
    int i = 0;
    jl_value_t *clast = jl_tparam(cdt,clen-1);
    jl_vararg_kind_t ckind = jl_vararg_kind(clast);
    int cva = ckind > JL_VARARG_INT;
    int pva = jl_vararg_kind(jl_tparam(pdt,plen-1)) > JL_VARARG_INT;
    int cdiag = 0, pdiag = 0;
    int some_morespecific = 0;
    while (1) {
        if (cva && pva && i >= clen && i >= plen)
            break;

        jl_value_t *ce = nth_tuple_elt(cdt, i);
        jl_value_t *pe = nth_tuple_elt(pdt, i);

        if (ce == NULL) {
            if (pe == NULL) break;
            return 1;
        }
        if (pe == NULL) {
            if (!cva && !some_morespecific)
                return 0;
            break;
        }

        if (type_morespecific_(pe, ce, invariant, env)) {
            assert(!type_morespecific_(ce, pe, invariant, env));
            return 0;
        }

        if (!cdiag && jl_is_typevar(ce) && num_occurs((jl_tvar_t*)ce,env) > 1)
            cdiag = 1;
        if (!pdiag && jl_is_typevar(pe) && num_occurs((jl_tvar_t*)pe,env) > 1)
            pdiag = 1;

        // in Tuple{a,b...} and Tuple{c,d...} allow b and d to be disjoint
        if (cva && pva && i >= clen-1 && i >= plen-1 && (some_morespecific || (cdiag && !pdiag)))
            return 1;

        int cms = type_morespecific_(ce, pe, invariant, env);

        if (!cms && !sub_msp(ce, pe, env)) {
            /*
              A bound vararg tuple can be more specific despite disjoint elements in order to
              preserve transitivity. For example in
              A = Tuple{Array{T,N}, Vararg{Int,N}} where {T,N}
              B = Tuple{Array, Int}
              C = Tuple{AbstractArray, Int, Array}
              we need A < B < C and A < C.
            */
            return some_morespecific && cva && ckind == JL_VARARG_BOUND && num_occurs((jl_tvar_t*)jl_tparam1(jl_unwrap_unionall(clast)), env) > 1;
        }

        // Tuple{..., T} not more specific than Tuple{..., Vararg{S}} if S is diagonal
        if (!cms && i == clen-1 && clen == plen && !cva && pva && eq_msp(ce, pe, env) &&
            jl_is_typevar(ce) && jl_is_typevar(pe) && !cdiag && pdiag)
            return 0;

        if (cms) some_morespecific = 1;
        i++;
    }
    if (cva && pva && clen > plen && (!pdiag || cdiag))
        return 1;
    if (cva && !pva && !some_morespecific)
        return 0;
    return some_morespecific || (cdiag && !pdiag);
}

static size_t tuple_full_length(jl_value_t *t)
{
    size_t n = jl_nparams(t);
    if (n == 0) return 0;
    jl_value_t *last = jl_unwrap_unionall(jl_tparam(t,n-1));
    if (jl_is_vararg_type(last)) {
        jl_value_t *N = jl_tparam1(last);
        if (jl_is_long(N))
            n += jl_unbox_long(N)-1;
    }
    return n;
}

// Called when a is a bound-vararg and b is not a vararg. Sets the vararg length
// in a to match b, as long as this makes some earlier argument more specific.
static int args_morespecific_fix1(jl_value_t *a, jl_value_t *b, int swap, jl_typeenv_t *env)
{
    size_t n = jl_nparams(a);
    int taillen = tuple_full_length(b)-n+1;
    if (taillen <= 0)
        return -1;
    assert(jl_is_va_tuple((jl_datatype_t*)a));
    jl_datatype_t *new_a = NULL;
    jl_value_t *e[2] = { jl_tparam1(jl_unwrap_unionall(jl_tparam(a, n-1))), jl_box_long(taillen) };
    JL_GC_PUSH2(&new_a, &e[1]);
    new_a = (jl_datatype_t*)jl_instantiate_type_with((jl_value_t*)a, e, 1);
    int changed = 0;
    for (size_t i = 0; i < n-1; i++) {
        if (jl_tparam(a, i) != jl_tparam(new_a, i)) {
            changed = 1;
            break;
        }
    }
    int ret = -1;
    if (changed) {
        if (eq_msp(b, (jl_value_t*)new_a, env))
            ret = swap;
        else if (swap)
            ret = type_morespecific_(b, (jl_value_t*)new_a, 0, env);
        else
            ret = type_morespecific_((jl_value_t*)new_a, b, 0, env);
    }
    JL_GC_POP();
    return ret;
}

static int count_occurs(jl_value_t *t, jl_tvar_t *v)
{
    if (t == (jl_value_t*)v)
        return 1;
    if (jl_is_uniontype(t)) {
        int a = count_occurs(((jl_uniontype_t*)t)->a, v);
        int b = count_occurs(((jl_uniontype_t*)t)->b, v);
        return a > b ? a : b;
    }
    if (jl_is_unionall(t)) {
        if (((jl_unionall_t*)t)->var == v)
            return 0;
        return count_occurs(((jl_unionall_t*)t)->body, v);
    }
    if (jl_is_datatype(t)) {
        int i, c=0;
        for(i=0; i < jl_nparams(t); i++)
            c += count_occurs(jl_tparam(t,i), v);
        return c;
    }
    return 0;
}

static int num_occurs(jl_tvar_t *v, jl_typeenv_t *env)
{
    while (env != NULL) {
        if (env->var == v)
            return (int)(ssize_t)env->val;
        env = env->prev;
    }
    return 0;
}

#define HANDLE_UNIONALL_A                                               \
    jl_unionall_t *ua = (jl_unionall_t*)a;                              \
    jl_typeenv_t newenv = { ua->var, 0x0, env };                        \
    newenv.val = (jl_value_t*)(intptr_t)count_occurs(ua->body, ua->var); \
    return type_morespecific_(ua->body, b, invariant, &newenv)

#define HANDLE_UNIONALL_B                                               \
    jl_unionall_t *ub = (jl_unionall_t*)b;                              \
    jl_typeenv_t newenv = { ub->var, 0x0, env };                        \
    newenv.val = (jl_value_t*)(intptr_t)count_occurs(ub->body, ub->var); \
    return type_morespecific_(a, ub->body, invariant, &newenv)

static int type_morespecific_(jl_value_t *a, jl_value_t *b, int invariant, jl_typeenv_t *env)
{
    if (a == b)
        return 0;

    if (jl_is_tuple_type(a) && jl_is_tuple_type(b)) {
        // When one is JL_VARARG_BOUND and the other has fixed length,
        // allow the argument length to fix the tvar
        jl_vararg_kind_t akind = jl_va_tuple_kind((jl_datatype_t*)a);
        jl_vararg_kind_t bkind = jl_va_tuple_kind((jl_datatype_t*)b);
        int ans = -1;
        if (akind == JL_VARARG_BOUND && bkind < JL_VARARG_BOUND) {
            ans = args_morespecific_fix1(a, b, 0, env);
            if (ans == 1) return 1;
        }
        if (bkind == JL_VARARG_BOUND && akind < JL_VARARG_BOUND) {
            ans = args_morespecific_fix1(b, a, 1, env);
            if (ans == 0) return 0;
        }
        return tuple_morespecific((jl_datatype_t*)a, (jl_datatype_t*)b, invariant, env);
    }

    if (!invariant) {
        if ((jl_datatype_t*)a == jl_any_type) return 0;
        if ((jl_datatype_t*)b == jl_any_type && !jl_is_typevar(a)) return 1;
    }

    if (jl_is_uniontype(a)) {
        if (jl_is_unionall(b)) {
            HANDLE_UNIONALL_B;
        }
        // Union a is more specific than b if some element of a is more specific than b, but
        // not vice-versa.
        if (sub_msp(b, a, env))
            return 0;
        jl_uniontype_t *u = (jl_uniontype_t*)a;
        if (type_morespecific_(u->a, b, invariant, env) || type_morespecific_(u->b, b, invariant, env)) {
            if (jl_is_uniontype(b)) {
                jl_uniontype_t *v = (jl_uniontype_t*)b;
                if (type_morespecific_(v->a, a, invariant, env) || type_morespecific_(v->b, a, invariant, env))
                    return 0;
            }
            return 1;
        }
        return 0;
    }

    if (jl_is_type_type(a) && !invariant) {
        if (b == (jl_value_t*)jl_typeofbottom_type)
            return 0;
        jl_value_t *tp0a = jl_tparam0(a);
        if (jl_is_typevar(tp0a)) {
            jl_value_t *ub = ((jl_tvar_t*)tp0a)->ub;
            if (jl_is_kind(b) && !sub_msp((jl_value_t*)jl_any_type, ub, env))
                return 1;
        }
        else if (tp0a == jl_bottom_type) {
            if (sub_msp(b, (jl_value_t*)jl_type_type, env))
                return 1;
        }
        else if (b == (jl_value_t*)jl_datatype_type || b == (jl_value_t*)jl_unionall_type ||
                 b == (jl_value_t*)jl_uniontype_type) {
            return 1;
        }
    }

    if (jl_is_uniontype(b)) {
        if (jl_is_unionall(a)) {
            HANDLE_UNIONALL_A;
        }
        jl_uniontype_t *u = (jl_uniontype_t*)b;
        if (type_morespecific_(a, u->a, invariant, env) || type_morespecific_(a, u->b, invariant, env))
            return !type_morespecific_(b, a, invariant, env);
        return 0;
    }

    if (jl_is_datatype(a) && jl_is_datatype(b)) {
        jl_datatype_t *tta = (jl_datatype_t*)a, *ttb = (jl_datatype_t*)b;
        // Type{Union{}} is more specific than other types, so TypeofBottom must be too
        if (tta == jl_typeofbottom_type && (jl_is_kind(b) || jl_is_type_type(b)))
            return 1;
        int super = 0;
        while (tta != jl_any_type) {
            if (tta->name == ttb->name) {
                if (super) {
                    if (tta->name != jl_type_typename) return 1;
                    jl_value_t *tp0 = jl_tparam0(b);
                    if (jl_is_typevar(tp0)) {
                        if (sub_msp((jl_value_t*)jl_any_type, ((jl_tvar_t*)tp0)->ub, env))
                            return 1;
                    }
                }
                assert(jl_nparams(tta) == jl_nparams(ttb));
                int ascore=0, bscore=0, ascore1=0, bscore1=0, adiag=0, bdiag=0;
                for(size_t i=0; i < jl_nparams(tta); i++) {
                    jl_value_t *apara = jl_tparam(tta,i);
                    jl_value_t *bpara = jl_tparam(ttb,i);
                    int afree = jl_has_free_typevars(apara);
                    int bfree = jl_has_free_typevars(bpara);
                    if (!afree && !bfree && !jl_types_equal(apara, bpara))
                        return 0;
                    if (type_morespecific_(apara, bpara, 1, env) && (jl_is_typevar(apara) || !afree || bfree))
                        ascore += 1;
                    else if (type_morespecific_(bpara, apara, 1, env) && (jl_is_typevar(bpara) || !bfree || afree))
                        bscore += 1;
                    else if (eq_msp(apara, bpara, env)) {
                        if (!afree && bfree)
                            ascore += 1;
                        else if (afree && !bfree)
                            bscore += 1;
                    }
                    if (jl_is_typevar(bpara) && !jl_is_typevar(apara) && !jl_is_type(apara))
                        ascore1 = 1;
                    else if (jl_is_typevar(apara) && !jl_is_typevar(bpara) && !jl_is_type(bpara))
                        bscore1 = 1;
                    if (!adiag && jl_is_typevar(apara)) {
                        for(int j=i+1; j < jl_nparams(tta); j++) {
                            if (jl_has_typevar(jl_tparam(tta,j), (jl_tvar_t*)apara)) {
                                adiag = 1; break;
                            }
                        }
                    }
                    if (!bdiag && jl_is_typevar(bpara)) {
                        for(int j=i+1; j < jl_nparams(ttb); j++) {
                            if (jl_has_typevar(jl_tparam(ttb,j), (jl_tvar_t*)bpara)) {
                                bdiag = 1; break;
                            }
                        }
                    }
                }
                if (ascore1 > bscore1)
                    return 1;
                if (bscore1 > ascore1 || bscore > ascore || bdiag > adiag)
                    return 0;
                return ascore > bscore || adiag > bdiag;
            }
            tta = tta->super; super = 1;
        }
        return 0;
    }

    if (jl_is_typevar(a)) {
        if (jl_is_typevar(b)) {
            return (( type_morespecific_((jl_value_t*)((jl_tvar_t*)a)->ub,
                                         (jl_value_t*)((jl_tvar_t*)b)->ub, 0, env) &&
                     !type_morespecific_((jl_value_t*)((jl_tvar_t*)a)->lb,
                                         (jl_value_t*)((jl_tvar_t*)b)->lb, 0, env)) ||
                    ( type_morespecific_((jl_value_t*)((jl_tvar_t*)b)->lb,
                                         (jl_value_t*)((jl_tvar_t*)a)->lb, 0, env) &&
                     !type_morespecific_((jl_value_t*)((jl_tvar_t*)b)->ub,
                                         (jl_value_t*)((jl_tvar_t*)a)->ub, 0, env)));
        }
        if (!jl_is_type(b))
            return 0;
        if (invariant) {
            if (((jl_tvar_t*)a)->ub == jl_bottom_type)
                return 1;
            if (!jl_has_free_typevars(b))
                return 0;
            if (eq_msp(((jl_tvar_t*)a)->ub, b, env))
                return num_occurs((jl_tvar_t*)a, env) >= 2;
        }
        else {
            // need `{T,T} where T` more specific than `{Any, Any}`
            if (b == (jl_value_t*)jl_any_type && ((jl_tvar_t*)a)->ub == (jl_value_t*)jl_any_type &&
                num_occurs((jl_tvar_t*)a, env) >= 2)
                return 1;
        }
        return type_morespecific_(((jl_tvar_t*)a)->ub, b, 0, env);
    }
    if (jl_is_typevar(b)) {
        if (!jl_is_type(a))
            return 1;
        if (invariant) {
            if (((jl_tvar_t*)b)->ub == jl_bottom_type)
                return 0;
            if (jl_has_free_typevars(a)) {
                if (type_morespecific_(a, ((jl_tvar_t*)b)->ub, 0, env))
                    return 1;
                if (eq_msp(a, ((jl_tvar_t*)b)->ub, env))
                    return num_occurs((jl_tvar_t*)b, env) < 2;
                return 0;
            }
            else {
                if (obviously_disjoint(a, ((jl_tvar_t*)b)->ub, 1))
                    return 0;
                if (type_morespecific_(((jl_tvar_t*)b)->ub, a, 0, env))
                    return 0;
                return 1;
            }
        }
        return type_morespecific_(a, ((jl_tvar_t*)b)->ub, 0, env);
    }

    if (jl_is_unionall(a)) {
        HANDLE_UNIONALL_A;
    }
    if (jl_is_unionall(b)) {
        HANDLE_UNIONALL_B;
    }

    return 0;
}

JL_DLLEXPORT int jl_type_morespecific(jl_value_t *a, jl_value_t *b)
{
    if (obviously_disjoint(a, b, 1))
        return 0;
    if (jl_has_free_typevars(a) || jl_has_free_typevars(b))
        return 0;
    if (jl_subtype(b, a))
        return 0;
    if (jl_subtype(a, b))
        return 1;
    return type_morespecific_(a, b, 0, NULL);
}

JL_DLLEXPORT int jl_type_morespecific_no_subtype(jl_value_t *a, jl_value_t *b)
{
    return type_morespecific_(a, b, 0, NULL);
}

#ifdef __cplusplus
}
#endif

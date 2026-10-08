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

typedef struct jl_bits_stack_t {
    uint32_t data[16];
    struct jl_bits_stack_t *next;
} jl_bits_stack_t;

typedef struct {
    int16_t depth;
    int16_t more;
    int16_t used;
    jl_bits_stack_t stack;
} jl_unionstate_t;

typedef struct {
    int16_t depth;
    int16_t more;
    int16_t used;
    uint8_t *stack;
} jl_saved_unionstate_t;

// How certain a lower-bound contribution to an existential variable is, for
// the purposes of the `envout` it computes (#61323). Context transitions only
// ever lower the channel (see `jl_stenv_t.bound_channel`); per-variable, the
// strongest contribution wins (see `jl_varbinding_t.lb_certainty`).
typedef enum {
    BOUND_NONE  = 0, // no (non-Bottom) lower-bound contribution yet
    BOUND_PROXY = 1, // derives from another variable's declared bounds: a
                     // `==`-equal rep of the query need not bind this var at all
    BOUND_EQ    = 2, // derives from a query value reached through an `==`
                     // equality wrapper (`Type{A}`): every `==`-equal rep of the
                     // query also binds this var, but only to an `==`-equal value
    BOUND_EGAL  = 3, // derives from an egality-pinned position (`TypeEgal`/type
                     // tag): the value is `===`-certain
} jl_bound_certainty_t;

struct jl_varbinding_t;

// A located term: a raw term of the query together with the chain of native
// binders under which its escaping references resolve (NULL for a closed
// term). A binding's bounds are persistent lists of these: no walk fragment
// is ever re-expressed in variable form to be stored, it is kept as a
// reference into the input structure and walked in place under its chain
// where it is compared. The lists live in the query's arena and are
// immutable (prepend only), so an environment save is a pointer copy.
// `cached` memoizes the materialized (variable-form) type of the suffix
// starting at this cell, for the consumers that need a real type.
typedef struct jl_lterm_t {
    jl_value_t *t;
    struct jl_varbinding_t *frame;
    struct jl_lterm_t *next;
    jl_value_t *cached;
    int8_t detached; // a reference of `t` escapes the chain (a binder outside the query)
} jl_lterm_t;

// Linked list storing the type variable environment. A new jl_varbinding_t
// is pushed for each UnionAll type we encounter. `lbs` and `ubs` are updated
// during the computation.
// Most of the complexity is due to the "diagonal rule", requiring us to
// identify which type vars range over only concrete types.
// Bindings are allocated in the query's arena (see `stenv_push_binding`) and
// outlive their pop: a located term stored into an enclosing binding's bound
// may refer to them, and such a reference then denotes the popped binding's
// final (frozen) bounds, like an inner variable.
typedef struct jl_varbinding_t {
    jl_tvar_t *var; // store NULL to "delete" this from env (temporarily)
    // the binder this binding was pushed for (rooted as part of the walked
    // terms); its raw declared bounds classify the binding without
    // materializing anything
    jl_unionall_t *u;
    // the bounds: `lbs` denotes the join of its entries (empty: `Union{}`),
    // `ubs` the meet of its entries (empty: `Any`)
    jl_lterm_t *lbs;
    jl_lterm_t *ubs;
    // the variable a reference to this binding materializes as once it has
    // been popped: a fresh variable carrying the final bounds (or, for a
    // pinned binding, the pinned value itself)
    jl_value_t *final_var;
    int8_t live;        // currently in the environment
    int8_t popped;      // was in the environment and has been popped
    int8_t referenced;  // a bound of an enclosing binding referred to it when
                        // it was popped: it cannot be reused for a later
                        // crossing of its binder
    int8_t final_pending; // `binding_ref_value` is computing `final_var`
    int8_t existential; // whether this variable should be treated as existential
    int8_t occurs;      // occurs in any position
    int8_t occurs_inv;  // occurs in invariant position
    int8_t pinned;      // an invariant occurrence matched a term free of existential vars
    int8_t occurs_cov;  // # of occurrences in covariant position within the
                        // current consistency-check scope (reset on entry to
                        // `subtype_ccheck` / `intersect_aside`, restored on
                        // exit). Saturates at 2. Covariant occurrences inside
                        // nested Tuple{} accumulate into this counter as long
                        // as no consistency check is entered.
    int8_t cov_diag;    // max value `occurs_cov` reached in any (already-closed)
                        // consistency-check scope. The diagonal-rule test is
                        // `max(occurs_cov, cov_diag) > 1`, so a variable is
                        // diagonal iff it occurred >= 2 times in some single
                        // scope (the outer scope or any consistency check),
                        // rather than summed across consistency checks.
    int8_t concrete;    // 1 if another variable has a constraint forcing this one to be concrete
    int8_t max_offset;  // record the maximum positive offset of the variable (up to 32)
                        // max_offset < 0 if this variable occurs outside VarargNum.
    // constraintkind: in covariant position, we try three different ways to compute var ∩ type:
    // let ub = var.ub ∩ type
    // 0 - var.ub <: type ? var : ub
    // 1 - var.ub = ub; return var
    // 2 - var.lb = lb; return ub
    int8_t constraintkind;
    int8_t intvalued; // intvalued: must be integer-valued; i.e. occurs as N in Vararg{_,N}
    int8_t limited;
    int8_t intersected; // whether this variable has been intersected
    int8_t widened_to_kind;   // Type{X} was widened to a union of kinds
    int8_t lb_certainty; // strongest channel (jl_bound_certainty_t) through which a
                         // lower-bound contribution arrived; a type-valued binding
                         // below BOUND_EGAL is only known up to `==` (#61323) and is
                         // wrapped as an uncertainty marker in `envout`, with the
                         // marker `constrained` (defined for every `==`-equal query
                         // rep) iff the channel is at least BOUND_EQ
    int8_t lb_required;  // a lower-bound contribution came from a covariant tuple
                         // element that is present in every concrete member of
                         // the current left-side branch
    int8_t lb_spell;     // spelling authority (`jl_stenv_t.spell_channel`) of the
                         // contribution that supplied the current `lb` OBJECT.
                         // Among `==`-equal spellings the runtime binding takes
                         // whichever object won the join, so a spelling recorded
                         // through an equality wrapper (a bare argument value,
                         // `==`-authoritative only) must not displace a canonical
                         // type-tag-derived spelling: that keeps the binding agreed
                         // between by-type queries and the runtime MethodInstances
                         // they cover (#61323)
    int8_t tainted_inner; // 1 if this var's bounds reference a TypeVar from a vb that
                          // was pushed at depth0 *strictly greater* than this var's
                          // depth0 and has since been popped. Such "inner" tvars
                          // would not be substituted by going to a more concrete LHS
                          // (they live inside Type{...}/etc value positions), so the
                          // binding is leaky regardless of what `constrained` says.
    int8_t body_occurs_inv; // cached `var_occurs_invariant(u->body, u->var)` — the
                            // static "occurs invariantly" check used by the diagonal
                            // rule (since #34272). Unlike the dynamic `occurs_inv`
                            // counter, this is a pure structural property of the
                            // UnionAll body and does not change during traversal.
    int8_t in_ccheck;   // a bound-consistency check for this binding is in flight.
                        // Under intersection the accumulated bounds can reach the
                        // variable itself through pinned variables (a graph the
                        // per-write guards cannot rule out); a re-entrant check is
                        // then answered coinductively instead of recursing forever.
    int16_t depth0;         // # of invariant constructors nested around the UnionAll type for this var
    // array of typevars that our bounds depend on, whose UnionAlls need to be
    // moved outside ours.
    jl_array_t *innervars;
    struct jl_varbinding_t *prev;
    // the chain of native (positionally-resolved) binders enclosing this one
    // on its own side of the relation, innermost first; a bound-variable
    // reference of depth d in that side's term resolves to the d-th entry
    struct jl_varbinding_t *frame_prev;
    uint32_t serial;    // identifies this push of the binding within the query (see `frame_substitute`)
    // intersection only: the term on the other side of the crossing (used to
    // avoid conflating a memoized variable already visible there; borrowed
    // reference, rooted by the crossing's caller)
    jl_value_t *other_t;
    struct jl_varbinding_t *next_alloc; // all bindings of the query (arena list)
} jl_varbinding_t;

// an entry of `finish_unionall`'s flattened variable list: either an inner
// variable (its bounds in the `var`/`lb`/`ub` slots) or a binding of the
// environment (`b`, whose bounds are read and written through the accessors)
typedef struct jl_ivarbinding_t {
    jl_tvar_t **var;
    jl_value_t **lb;
    jl_value_t **ub;
    jl_varbinding_t *b;
    jl_varbinding_t *root;
    struct jl_ivarbinding_t *next;
} jl_ivarbinding_t;

// bump allocator for the bindings and located-term lists of one query; the
// chunks are released when the query's outermost environment is freed
typedef struct jl_starena_chunk_t {
    struct jl_starena_chunk_t *next;
    size_t used;
    size_t cap;
    char data[];
} jl_starena_chunk_t;

typedef struct {
    jl_starena_chunk_t *chunks;
    jl_varbinding_t *bindings; // every binding allocated in this query
} jl_starena_t;

// subtype algorithm state
typedef struct JL_GC_TRACKED_TYPE jl_stenv_t {
    // N.B.: varbindings are created on the stack and rooted there
    jl_varbinding_t *vars;    // type variable environment
    jl_varbinding_t *Lframe;  // native binder chain of the left term's position
    jl_varbinding_t *Rframe;  // ... and of the right term's position
    int8_t frames_flipped;    // parity of `flip_frames` (is Lframe the original right chain?)
    jl_array_t *substmemo;    // frame_substitute memo (triples); rooted like `opened`
    // every value the walk computes and stores into a binding (a joined or
    // met bound, a boxed length, a variable, an innervars array) is kept
    // alive here for the query's lifetime: the bindings and their lists are
    // arena memory the GC does not see. Rooted like `opened` (lazily allocated)
    jl_array_t *roots;
    jl_array_t *finalvars;    // the variables of popped bindings (inner variables); rooted like `opened`
    jl_value_t *ref1;         // the (permanent) depth-1 reference, for canonical entries
    jl_starena_t *arena;      // the query's arena (owned by the outermost env)
    uint32_t nbindings;       // bindings pushed so far in this query (their `serial`s)
    jl_unionstate_t Lunions;  // union state for unions on the left of A <: B
    jl_unionstate_t Runions;  // union state for unions on the right
    // memo of binding variables for the binder crossings: flat triples
    // [u, prevframe-var-or-nothing, var, ...]. No bodies are materialized;
    // the memo keeps each binding's variable identity stable across the ∀∃
    // and re-intersection passes of one query (see `stenv_binding_var`).
    // Rooted by the caller of init_stenv (lazily allocated).
    // N.B.: this caches pure identities; no binding state lives here.
    jl_array_t *opened;
    // N.B.: envout is gc-rooted
    jl_value_t **envout;      // for passing caller the computed bounds of right-side variables
    int envsz;                // length of envout
    int envidx;               // current index in envout
    int invdepth;             // current number of invariant constructors we're nested in
    int bound_channel;        // certainty (jl_bound_certainty_t) of lower-bound
                              // contributions recorded in the current context;
                              // starts at BOUND_EGAL and is only ever lowered:
                              // to BOUND_EQ inside an x-side equality wrapper
                              // (`Type{A}` matched by `==`), to BOUND_PROXY inside
                              // a bounds-consistency check on a typevar-containing
                              // x-term (whose bindings derive from another var's
                              // declared bounds rather than from a query value)
    int value_descent;        // true inside a bounds-consistency check on a closed
                              // x-term: the x-term is then a concrete type OBJECT
                              // (a candidate variable bound), so structural descent
                              // into it preserves the identity certainty carried by
                              // `bound_channel` and the covariant equality-wrapper
                              // demotion (which encodes that an argument-slot
                              // *spelling* only pins the runtime value up to `==`)
                              // does not apply
    int spell_channel;        // authority (jl_bound_certainty_t) of the *spelling*
                              // a lower-bound contribution carries in the current
                              // context. Mirrors `bound_channel`, but is also
                              // lowered to BOUND_EQ when descending from an
                              // egality-pinned value into an equality wrapper
                              // (`TypeEgal{A} <: Type{B}`): the value `A` is
                              // egal-known, yet `Type{B}` constrains `B` only up
                              // to `==`, so `A`'s spelling must not displace a
                              // canonical (type-tag-derived) spelling of the same
                              // binding (see `jl_varbinding_t.lb_spell`)
    int intersection;         // true iff subtype is being called from intersection
    int emptiness_only;       // true iff intersection only needs to test for emptiness
    int triangular;           // when intersecting Ref{X} with Ref{<:Y}
    int ignore_lb_required;   // true while checking a variable's declared bound
    // Used to represent the length difference between 2 vararg.
    // intersect(X, Y) ==> X = Y + Loffset
    int Loffset;
} jl_stenv_t;

// --- query arena, roots and located-term lists ---

static void *starena_alloc(jl_starena_t *a, size_t sz) JL_NOTSAFEPOINT
{
    sz = (sz + 15) & ~(size_t)15;
    jl_starena_chunk_t *c = a->chunks;
    if (c == NULL || c->used + sz > c->cap) {
        size_t cap = 4096 - sizeof(jl_starena_chunk_t);
        if (cap < sz)
            cap = sz;
        c = (jl_starena_chunk_t*)malloc_s(sizeof(jl_starena_chunk_t) + cap);
        c->next = a->chunks;
        c->used = 0;
        c->cap = cap;
        a->chunks = c;
    }
    void *p = c->data + c->used;
    c->used += sz;
    return p;
}

static void starena_free(jl_starena_t *a) JL_NOTSAFEPOINT
{
    jl_starena_chunk_t *c = a->chunks;
    while (c != NULL) {
        jl_starena_chunk_t *next = c->next;
        free(c);
        c = next;
    }
    a->chunks = NULL;
    a->bindings = NULL;
}

// keep a value the walk computed alive for the rest of the query (the
// bindings and lists that refer to it are arena memory)
static void stenv_root(jl_stenv_t *e, jl_value_t *v JL_MAYBE_UNROOTED) JL_CANSAFEPOINT
{
    JL_GC_PUSH1(&v);
    if (e->roots == NULL)
        e->roots = jl_alloc_array_1d(jl_array_any_type, 0);
    jl_array_ptr_1d_push(e->roots, v);
    JL_GC_POP();
}

#ifndef __clang_gcanalyzer__
static jl_varbinding_t *frame_lookup(jl_varbinding_t *frame, size_t d) JL_GLOBALLY_ROOTED JL_NOTSAFEPOINT;
#else
extern jl_varbinding_t *frame_lookup(jl_varbinding_t *frame, size_t d) JL_GLOBALLY_ROOTED JL_NOTSAFEPOINT;
#endif

// prepend a located term. A closed term gets no frame: its position is
// irrelevant, and entries then compare by content alone. A bare reference
// is canonicalized to the binding it denotes (a depth-1 reference under
// that binding's chain), so that the same binding referred to from
// different positions compares equal.
static jl_lterm_t *lterm_cons(jl_stenv_t *e, jl_value_t *t, jl_varbinding_t *frame, jl_lterm_t *next) JL_NOTSAFEPOINT
{
    jl_lterm_t *c = (jl_lterm_t*)starena_alloc(e->arena, sizeof(jl_lterm_t));
    c->detached = 0;
    if (!jl_has_dangling_tvarrefs(t)) {
        frame = NULL;
    }
    else if (jl_is_tvarref(t)) {
        jl_varbinding_t *b = frame_lookup(frame, jl_tvarref_depth(t));
        if (b != NULL) {
            t = e->ref1;
            frame = b;
        }
        else {
            c->detached = 1;
        }
    }
    else {
        size_t n = 0;
        for (jl_varbinding_t *f = frame; f != NULL; f = f->frame_prev)
            n++;
        c->detached = jl_has_refs_above(t, n);
    }
    c->t = t;
    c->frame = frame;
    c->next = next;
    c->cached = NULL;
    return c;
}

// the type of a single frame-free entry (a closed type, or one in variable
// form), NULL for an empty, located or longer list
static jl_value_t *lterm_closed1(jl_lterm_t *l) JL_GLOBALLY_ROOTED JL_NOTSAFEPOINT
{
    if (l == NULL || l->next != NULL || l->frame != NULL)
        return NULL;
    return l->t;
}

static int lterm_entry_egal(jl_stenv_t *e, jl_lterm_t *c, jl_value_t *t, jl_varbinding_t *frame) JL_CANSAFEPOINT;

static int lterm_eq(jl_stenv_t *e, jl_lterm_t *a, jl_lterm_t *b) JL_CANSAFEPOINT
{
    while (a != NULL && b != NULL) {
        if (a == b)
            return 1;
        if (!lterm_entry_egal(e, a, b->t, b->frame))
            return 0;
        a = a->next;
        b = b->next;
    }
    return a == b;
}

// is `(t, frame)` among the entries?
static jl_lterm_t *lterm_find(jl_stenv_t *e, jl_lterm_t *l, jl_value_t *t, jl_varbinding_t *frame) JL_CANSAFEPOINT
{
    for (; l != NULL; l = l->next) {
        if (lterm_entry_egal(e, l, t, frame))
            return l;
    }
    return NULL;
}

// is the binding pinned (`lb === ub`)? An empty list denotes `Union{}`
// as a lower bound and `Any` as an upper bound.
static int binding_pinned(jl_stenv_t *e, jl_varbinding_t *vb) JL_CANSAFEPOINT
{
    if (vb->lbs == NULL)
        return vb->ubs != NULL && vb->ubs->next == NULL && vb->ubs->t == jl_bottom_type;
    if (vb->ubs == NULL)
        return vb->lbs->next == NULL && vb->lbs->t == (jl_value_t*)jl_any_type;
    return lterm_eq(e, vb->lbs, vb->ubs);
}

// does the variable-form content of a bound contain `v`? (a raw located
// entry contains no variable)
static int lterm_has_typevar(jl_lterm_t *l, jl_tvar_t *v) JL_NOTSAFEPOINT
{
    for (; l != NULL; l = l->next) {
        if (l->frame == NULL && jl_has_typevar(l->t, v))
            return 1;
    }
    return 0;
}

// a bound that is a single closed `Int` (a known Vararg length)
static jl_value_t *lterm_long(jl_lterm_t *l) JL_GLOBALLY_ROOTED JL_NOTSAFEPOINT
{
    jl_value_t *t = lterm_closed1(l);
    return t != NULL && jl_is_long(t) ? t : NULL;
}

static jl_value_t *simple_join(jl_value_t *a, jl_value_t *b) JL_CANSAFEPOINT;
static jl_value_t *simple_meet(jl_value_t *a, jl_value_t *b, int overesi) JL_CANSAFEPOINT;
static jl_value_t *frame_substitute(jl_value_t *t, jl_varbinding_t *frame, jl_stenv_t *e) JL_GLOBALLY_ROOTED JL_CANSAFEPOINT;
static int obviously_egal(jl_value_t *a, jl_value_t *b) JL_NOTSAFEPOINT;

// the materialized (variable-form) type a list denotes: the join of its
// entries, or their meet (`meet`, as an `Intersect` spine where it cannot be
// resolved). Memoized on the list cell, and rooted (an input subterm, a
// memoized substitution, or a value kept in `e->roots`).
static jl_value_t *lterm_type(jl_stenv_t *e, jl_lterm_t *l, int meet) JL_GLOBALLY_ROOTED JL_CANSAFEPOINT
{
    if (l == NULL)
        return meet ? (jl_value_t*)jl_any_type : jl_bottom_type;
    if (l->cached != NULL)
        return l->cached;
    jl_value_t *t = l->frame != NULL ? frame_substitute(l->t, l->frame, e) : l->t;
    if (l->next != NULL) {
        JL_GC_PUSH1(&t);
        // the entries were added newest-first; combine in insertion order
        jl_value_t *rest = lterm_type(e, l->next, meet);
        t = meet ? simple_meet(rest, t, 1) : simple_join(rest, t);
        stenv_root(e, t);
        JL_GC_POP();
    }
    l->cached = t;
    return t;
}

static int egal_frames(jl_value_t *x, jl_varbinding_t *xframe, jl_value_t *y, jl_varbinding_t *yframe,
                       size_t d, jl_stenv_t *e) JL_CANSAFEPOINT;

// do the entry and `(t, frame)` denote the same term? Located terms under
// different chains are compared by the bindings their references resolve
// to; a bare reference compares as the binding it denotes.
static int lterm_entry_egal(jl_stenv_t *e, jl_lterm_t *c, jl_value_t *t, jl_varbinding_t *frame) JL_CANSAFEPOINT
{
    if (c->t == t && c->frame == frame)
        return 1;
    int closed = !jl_has_dangling_tvarrefs(t);
    if (c->frame == NULL && closed)
        return obviously_egal(c->t, t);
    if (c->frame == NULL || closed)
        return 0;
    if (jl_is_tvarref(t)) {
        jl_varbinding_t *b = frame_lookup(frame, jl_tvarref_depth(t));
        return b != NULL && c->frame == b && jl_is_tvarref(c->t) && jl_tvarref_depth(c->t) == 1;
    }
    if (jl_is_tvarref(c->t))
        return 0;
    return egal_frames(c->t, c->frame, t, frame, 0, e);
}

// replace a bound by a single (frame-free) type. The intersection code
// computes its bounds as types; this is also the form the boundary
// consumers write back.
static void binding_set_lb(jl_stenv_t *e, jl_varbinding_t *vb, jl_value_t *t JL_MAYBE_UNROOTED) JL_CANSAFEPOINT
{
    assert(!jl_has_dangling_tvarrefs(t));
    if (t == jl_bottom_type) {
        vb->lbs = NULL;
        return;
    }
    jl_value_t *cur = lterm_closed1(vb->lbs);
    if (cur == t)
        return;
    JL_GC_PUSH1(&t);
    stenv_root(e, t);
    vb->lbs = lterm_cons(e, t, NULL, NULL);
    JL_GC_POP();
}

static void binding_set_ub(jl_stenv_t *e, jl_varbinding_t *vb, jl_value_t *t JL_MAYBE_UNROOTED) JL_CANSAFEPOINT
{
    assert(!jl_has_dangling_tvarrefs(t));
    if (t == (jl_value_t*)jl_any_type) {
        vb->ubs = NULL;
        return;
    }
    jl_value_t *cur = lterm_closed1(vb->ubs);
    if (cur == t)
        return;
    JL_GC_PUSH1(&t);
    stenv_root(e, t);
    vb->ubs = lterm_cons(e, t, NULL, NULL);
    JL_GC_POP();
}

// the bounds as types (for the intersection code and the boundaries)
static jl_value_t *binding_lb(jl_stenv_t *e, jl_varbinding_t *vb) JL_GLOBALLY_ROOTED JL_CANSAFEPOINT
{
    return lterm_type(e, vb->lbs, 0);
}

static jl_value_t *binding_ub(jl_stenv_t *e, jl_varbinding_t *vb) JL_GLOBALLY_ROOTED JL_CANSAFEPOINT
{
    return lterm_type(e, vb->ubs, 1);
}

// write the materialized bounds back as single entries. Only legal once the
// binding can no longer appear in an environment save/merge (it has been
// popped): the boundary consumers (envout, the intersection result
// construction) read the fields as types and may update them in place.
static void binding_force_bounds(jl_stenv_t *e, jl_varbinding_t *vb) JL_CANSAFEPOINT
{
    jl_value_t *lb = binding_lb(e, vb);
    JL_GC_PUSH1(&lb);
    jl_value_t *ub = binding_ub(e, vb);
    binding_set_lb(e, vb, lb);
    binding_set_ub(e, vb, ub);
    JL_GC_POP();
}

// does the located term refer to the binding `P` (resolve a reference to it
// through its chain)? Through a popped binding's frozen bounds as well.
static int lterm_refs_binding(jl_lterm_t *c, jl_varbinding_t *P, int depth) JL_NOTSAFEPOINT
{
    if (c->frame == NULL)
        return 0;
    size_t d = 1;
    for (jl_varbinding_t *f = c->frame; f != NULL; f = f->frame_prev, d++) {
        if (f == P)
            return jl_tvarref_occurs(c->t, d);
        if (f->popped && depth < 8 && jl_tvarref_occurs(c->t, d)) {
            for (jl_lterm_t *l = f->lbs; l != NULL; l = l->next)
                if (lterm_refs_binding(l, P, depth + 1))
                    return 1;
            for (jl_lterm_t *l = f->ubs; l != NULL; l = l->next)
                if (lterm_refs_binding(l, P, depth + 1))
                    return 1;
        }
    }
    return 0;
}

static int binding_refs_binding(jl_varbinding_t *b, jl_varbinding_t *P) JL_NOTSAFEPOINT
{
    for (jl_lterm_t *l = b->lbs; l != NULL; l = l->next)
        if (lterm_refs_binding(l, P, 0) || (P->var != NULL && l->frame == NULL && jl_has_typevar(l->t, P->var)))
            return 1;
    for (jl_lterm_t *l = b->ubs; l != NULL; l = l->next)
        if (lterm_refs_binding(l, P, 0) || (P->var != NULL && l->frame == NULL && jl_has_typevar(l->t, P->var)))
            return 1;
    return 0;
}

// a popped binding that some enclosing binding's bound refers to keeps its
// identity for the rest of the query (it is never reused for another
// crossing of its binder), and so does every binding its own bounds refer to
static void binding_mark_referenced(jl_varbinding_t *P) JL_NOTSAFEPOINT
{
    if (P->referenced)
        return;
    P->referenced = 1;
    for (int which = 0; which < 2; which++) {
        for (jl_lterm_t *c = which ? P->ubs : P->lbs; c != NULL; c = c->next) {
            size_t d = 1;
            for (jl_varbinding_t *f = c->frame; f != NULL; f = f->frame_prev, d++) {
                // (a live binding is referred to as the binder it still is:
                // it decides for itself at its own pop)
                if (f->popped && jl_tvarref_occurs(c->t, d))
                    binding_mark_referenced(f);
            }
        }
    }
}

// allocate (or reuse) the binding for a crossing of `u` on side `R`. In pure
// subtyping, a crossing of the same binder under the same chain, once popped
// and not referred to by any bound, is the same binding again: this keeps
// the binding's identity (and its variable) stable across the ∀∃ passes of
// a query, as the `opened` memo does for the variables in intersection.
static jl_varbinding_t *stenv_push_binding(jl_stenv_t *e, jl_unionall_t *u, int R, jl_value_t *other_t) JL_NOTSAFEPOINT
{
    jl_varbinding_t *frame_prev = R ? e->Rframe : e->Lframe;
    jl_varbinding_t *vb = NULL;
    if (!e->intersection) {
        for (jl_varbinding_t *b = e->arena->bindings; b != NULL; b = b->next_alloc) {
            if (b->u == u && b->frame_prev == frame_prev && !b->live && !b->referenced) {
                vb = b;
                break;
            }
        }
    }
    if (vb == NULL) {
        vb = (jl_varbinding_t*)starena_alloc(e->arena, sizeof(jl_varbinding_t));
        memset(vb, 0, sizeof(jl_varbinding_t));
        vb->serial = ++e->nbindings;
        vb->next_alloc = e->arena->bindings;
        e->arena->bindings = vb;
    }
    else {
        jl_tvar_t *var = vb->var;
        uint32_t serial = vb->serial;
        jl_varbinding_t *next_alloc = vb->next_alloc;
        memset(vb, 0, sizeof(jl_varbinding_t));
        vb->var = var;
        vb->serial = serial;
        vb->next_alloc = next_alloc;
    }
    vb->u = u;
    vb->frame_prev = frame_prev;
    vb->existential = R;
    vb->depth0 = e->invdepth;
    vb->other_t = other_t;
    vb->body_occurs_inv = (u->flags & JL_UNIONALL_OCCURSINV) != 0;
    // the bounds start as the binder's declared bounds, located under the
    // enclosing chain
    vb->lbs = u->lb == jl_bottom_type ? NULL : lterm_cons(e, u->lb, frame_prev, NULL);
    vb->ubs = u->ub == (jl_value_t*)jl_any_type ? NULL : lterm_cons(e, u->ub, frame_prev, NULL);
    return vb;
}

// link the binding into the environment and its side's chain
static void stenv_enter_binding(jl_stenv_t *e, jl_varbinding_t *vb, int R) JL_NOTSAFEPOINT
{
    vb->live = 1;
    vb->popped = 0;
    vb->prev = e->vars;
    e->vars = vb;
    if (R)
        e->Rframe = vb;
    else
        e->Lframe = vb;
}

static void stenv_leave_binding(jl_stenv_t *e, jl_varbinding_t *vb, int R) JL_NOTSAFEPOINT
{
    assert(e->vars == vb);
    e->vars = vb->prev;
    if (R)
        e->Rframe = vb->frame_prev;
    else
        e->Lframe = vb->frame_prev;
    vb->live = 0;
    vb->popped = 1;
}

// state manipulation utilities

// look up a type variable in an environment
static int binding_has_innervar(jl_varbinding_t *b, jl_tvar_t *v) JL_NOTSAFEPOINT
{
    if (b->innervars == NULL)
        return 0;
    for (size_t i = 0; i < jl_array_len(b->innervars); i++) {
        if ((jl_tvar_t*)jl_array_ptr_ref(b->innervars, i) == v)
            return 1;
    }
    return 0;
}

#ifndef __clang_gcanalyzer__
static jl_varbinding_t *lookup_binding(jl_stenv_t *e, jl_tvar_t *v, int *innervar) JL_GLOBALLY_ROOTED JL_NOTSAFEPOINT
{
    jl_varbinding_t *b = e->vars;
    while (b != NULL) {
        if (b->var == v) {
            if (innervar)
                *innervar = 0;
            return b;
        }
        b = b->prev;
    }
    if (innervar) {
        b = e->vars;
        while (b != NULL) {
            if (binding_has_innervar(b, v)) {
                *innervar = 1;
                return NULL;
            }
            b = b->prev;
        }
        // the variable of a popped binding (see `binding_ref_value`)
        if (e->finalvars != NULL) {
            for (size_t i = 0; i < jl_array_nrows(e->finalvars); i++) {
                if ((jl_tvar_t*)jl_array_ptr_ref(e->finalvars, i) == v) {
                    *innervar = 1;
                    return NULL;
                }
            }
        }
        *innervar = 0;
    }
    return NULL;
}
#else
extern jl_varbinding_t *lookup_binding(jl_stenv_t *e, jl_tvar_t *v, int *innervar) JL_GLOBALLY_ROOTED JL_NOTSAFEPOINT;
#endif
jl_varbinding_t *lookup_binding(jl_stenv_t *e, jl_tvar_t *v, int *innervar) JL_GLOBALLY_ROOTED JL_NOTSAFEPOINT;

static jl_varbinding_t *lookup(jl_stenv_t *e, jl_tvar_t *v) JL_GLOBALLY_ROOTED JL_NOTSAFEPOINT
{
    return lookup_binding(e, v, NULL);
}

// whether the variable of a previous opening is still visible somewhere in the
// current environment (directly bound, referenced by some changed bound, or
// registered as an innervar), so reusing it would conflate two bindings
static int opened_var_aliased(jl_stenv_t *e, jl_tvar_t *v)
{
    jl_varbinding_t *b = e->vars;
    while (b != NULL) {
        if (b->var == v)
            return 1;
        b = b->prev;
    }
    return 0;
}

static int has_free_or_dangling_typevars(jl_value_t *v) JL_NOTSAFEPOINT;

// memoized binding variable for a native (positional) binder crossing: no
// body is materialized, but repeated ∀∃ and re-intersection passes must agree
// on the variable's identity -- the envout merging and the identity-based
// cycle breakers rely on `jl_egal` across passes. Entries are additionally
// keyed by the enclosing frame's variable: the same (interned) binder object
// can be crossed at positions under different frames, where its re-expressed
// bounds differ. `t`, if given, is the term on the other side of the relation;
// a variable already visible there would conflate two bindings, so such
// entries are skipped (intersection leaks binding variables into terms).
static jl_tvar_t *binding_var(jl_stenv_t *e, jl_varbinding_t *vb) JL_GLOBALLY_ROOTED JL_CANSAFEPOINT;
static int egal_frames(jl_value_t *x, jl_varbinding_t *xframe, jl_value_t *y, jl_varbinding_t *yframe,
                       size_t d, jl_stenv_t *e) JL_CANSAFEPOINT;

// master's rule for reusing a binder's variable: not if it is already in use
// in the environment (directly bound, in a binding's updated bounds, or an
// inner variable of one) or in the term on the other side
static int canonical_var_aliased(jl_stenv_t *e, jl_tvar_t *v, jl_value_t *t) JL_NOTSAFEPOINT
{
    if (t != NULL && jl_has_typevar(t, v))
        return 1;
    for (jl_varbinding_t *b = e->vars; b != NULL; b = b->prev) {
        if (b->var == v)
            return 1;
        // (a raw located entry contains no variable)
        for (jl_lterm_t *l = b->lbs; l != NULL; l = l->next) {
            if (l->frame == NULL && jl_has_typevar(l->t, v))
                return 1;
        }
        for (jl_lterm_t *l = b->ubs; l != NULL; l = l->next) {
            if (l->frame == NULL && jl_has_typevar(l->t, v))
                return 1;
        }
        if (b->innervars != NULL) {
            for (size_t i = 0; i < jl_array_nrows(b->innervars); i++) {
                if (jl_array_ptr_ref(b->innervars, i) == (jl_value_t*)v)
                    return 1;
            }
        }
    }
    return 0;
}

// the returned variable is memoized in (and so rooted by) `e->opened`
static jl_tvar_t *stenv_binding_var(jl_stenv_t *e, jl_unionall_t *u, jl_value_t *lb, jl_value_t *ub,
                                    jl_varbinding_t *frame_prev, jl_value_t *t) JL_GLOBALLY_ROOTED JL_CANSAFEPOINT
{
    if (lb == u->lb && ub == u->ub && !jl_has_dangling_tvarrefs(lb) && !jl_has_dangling_tvarrefs(ub)) {
        // the bounds do not depend on the enclosing chain: share the binder's
        // canonical variable when it is free to use (the binder keeps it, so
        // every pass of the query finds the same one)
        jl_tvar_t *cv = jl_unionall_var(u);
        if (!canonical_var_aliased(e, cv, t))
            return cv;
    }
    // the key must identify the whole enclosing chain, so the previous
    // frame's variable is forced transitively: repeated passes then agree on
    // every variable an already-memoized bound substitution can contain
    jl_value_t *prevkey = frame_prev == NULL ? jl_nothing : (jl_value_t*)binding_var(e, frame_prev);
    if (e->opened != NULL) {
        size_t i, l = jl_array_nrows(e->opened);
        for (i = 0; i < l; i += 3) {
            if ((jl_unionall_t*)jl_array_ptr_ref(e->opened, i) == u &&
                jl_array_ptr_ref(e->opened, i + 1) == prevkey) {
                jl_tvar_t *v = (jl_tvar_t*)jl_array_ptr_ref(e->opened, i + 2);
                if (!opened_var_aliased(e, v) && !(t != NULL && jl_has_typevar(t, v)))
                    return v;
                // still visible: fall through to look for (or make) another entry
            }
        }
    }
    else {
        e->opened = jl_alloc_array_1d(jl_array_any_type, 0);
    }
    // a detached fragment's binder can carry unresolvable references in its
    // bounds; they support no bound reasoning downstream (see `var_lt_`), but
    // the variable must still exist to carry the binding's identity
    jl_tvar_t *v = (jl_has_dangling_tvarrefs(lb) || jl_has_dangling_tvarrefs(ub)) ?
        jl_new_typevar_raw(u->name, lb, ub) : jl_new_typevar(u->name, lb, ub);
    JL_GC_PUSH1(&v);
    jl_array_ptr_1d_push(e->opened, (jl_value_t*)u);
    jl_array_ptr_1d_push(e->opened, prevkey);
    jl_array_ptr_1d_push(e->opened, (jl_value_t*)v);
    JL_GC_POP();
    return v;
}

// --- positional (native) binder machinery ---

// resolve a bound-variable reference of depth `d` against a side's chain of
// native binders (innermost first); NULL means the reference escapes the
// binders this walk has crossed (a detached fragment of the query itself).
// Like `lookup_binding`, the returned binding is a stack object of an active
// walk frame, whose GC-value fields are rooted by their pushers (and the
// variable additionally by `e->opened`).
#ifndef __clang_gcanalyzer__
static jl_varbinding_t *frame_lookup(jl_varbinding_t *frame, size_t d) JL_GLOBALLY_ROOTED JL_NOTSAFEPOINT
{
    while (frame != NULL && d > 1) {
        d--;
        frame = frame->frame_prev;
    }
    return d == 1 ? frame : NULL;
}
#else
extern jl_varbinding_t *frame_lookup(jl_varbinding_t *frame, size_t d) JL_GLOBALLY_ROOTED JL_NOTSAFEPOINT;
#endif
jl_varbinding_t *frame_lookup(jl_varbinding_t *frame, size_t d) JL_GLOBALLY_ROOTED JL_NOTSAFEPOINT;

static jl_value_t *binding_ref_value(jl_stenv_t *e, jl_varbinding_t *vb) JL_GLOBALLY_ROOTED JL_CANSAFEPOINT;

// resolve a bare bound-variable reference to its binding's variable,
// materializing it on this first use (a still-dangling reference is returned
// unchanged). The result is as rooted as `t`: a binding's variable is kept
// alive by its pusher's frame and by the `e->opened` memo.
static jl_value_t *resolve_tvarref(jl_value_t *t JL_PROPAGATES_ROOT, jl_varbinding_t *frame, jl_stenv_t *e) JL_CANSAFEPOINT
{
    if (jl_is_tvarref(t)) {
        jl_varbinding_t *b = frame_lookup(frame, jl_tvarref_depth(t));
        if (b != NULL)
            return binding_ref_value(e, b);
    }
    return t;
}

// exchange the two sides' binder chains, for the calls that pass a right
// term in a left position (or vice versa)
static void flip_frames(jl_stenv_t *e) JL_NOTSAFEPOINT
{
    jl_varbinding_t *tmp = e->Lframe;
    e->Lframe = e->Rframe;
    e->Rframe = tmp;
    e->frames_flipped ^= 1;
}

// re-express a term in variable form: replace the references escaping `t` by
// the variables of the binders they resolve to in `frame` (deeper unresolved
// references stay). Only the boundaries need this: the environment output,
// the intersection code (whose bounds are types), and walked terms that
// become results. A reference to a popped binding becomes the variable
// carrying that binding's final bounds (see `binding_ref_value`).
// Substitutions are memoized in `e->substmemo` (triples [t, frame-serial,
// result]): repeated queries must yield the IDENTICAL object, or the
// identity-based cycle breakers of the intersection code never fire and
// bound-consistency recursions diverge. The frame's serial (unique per
// binding push within the query) identifies the whole chain without forcing
// any variable. The memo also keeps every result reachable for the query's
// lifetime (the entry points root the `substmemo` slot), so the result is
// rooted.
static jl_value_t *frame_substitute(jl_value_t *t, jl_varbinding_t *frame, jl_stenv_t *e) JL_GLOBALLY_ROOTED JL_CANSAFEPOINT
{
    if (frame == NULL)
        return t;
    if (jl_is_tvarref(t)) {
        // a bare reference re-expresses as its binding's (memoized, so
        // stable) variable, or stays as it is if it escapes the chain
        jl_varbinding_t *b = frame_lookup(frame, jl_tvarref_depth(t));
        return b != NULL ? binding_ref_value(e, b) : t;
    }
    if (e->substmemo != NULL) {
        size_t i, l = jl_array_nrows(e->substmemo);
        for (i = 0; i < l; i += 3) {
            if (jl_array_ptr_ref(e->substmemo, i) == t &&
                jl_unbox_uint32(jl_array_ptr_ref(e->substmemo, i + 1)) == frame->serial)
                return jl_array_ptr_ref(e->substmemo, i + 2);
        }
    }
    else {
        e->substmemo = jl_alloc_array_1d(jl_array_any_type, 0);
    }
    jl_value_t *t0 = t;
    jl_value_t *fbox = NULL;
    JL_GC_PUSH3(&t0, &t, &fbox);
    size_t consumed = 0;
    for (jl_varbinding_t *f = frame; f != NULL; f = f->frame_prev) {
        if (!jl_has_dangling_tvarrefs(t))
            break;
        // each substitution consumes the innermost escaping level and shifts
        // the deeper ones down, so the target is always root-index 1.
        // Invalid `Union` bound arms drop under the substitution; a term
        // invalid beyond that keeps its references (the dangling-binder
        // rules then answer conservatively).
        // Only a level that actually occurs forces its binding's variable;
        // a non-occurring level is a pure shift and any value works.
        jl_value_t *v = jl_tvarref_occurs(t, 1) ? binding_ref_value(e, f) : jl_bottom_type;
        // a variable is a renaming; a pinned binding's value needs the
        // checked instantiation
        jl_value_t *t2 = jl_is_typevar(v) || v == jl_bottom_type ? jl_rename_tvarref(t, 1, v)
                                                                 : jl_substitute_tvarref_nothrow(t, 1, v);
        if (t2 == NULL)
            break;
        t = t2;
        consumed++;
    }
    // references that escape the whole chain (into binders no walk crossed)
    // must keep their original depths -- each substitution above consumed a
    // level, so shift the leftovers back. A stranded shifted reference would
    // later resolve against whatever frame the stored term is walked under.
    if (consumed > 0 && jl_has_dangling_tvarrefs(t))
        t = jl_shift_dangling_refs(t, (ssize_t)consumed);
    fbox = jl_box_uint32(frame->serial);
    jl_array_ptr_1d_push(e->substmemo, t0);
    jl_array_ptr_1d_push(e->substmemo, fbox);
    jl_array_ptr_1d_push(e->substmemo, t);
    JL_GC_POP();
    return t;
}

static jl_value_t *widen_intersect(jl_value_t *t) JL_CANSAFEPOINT;

// force the binding's bookkeeping variable: most binders' variables never
// occur at a compared position, so the variable materializes on first use.
// It carries the binder's declared bounds (re-expressed in variable form) --
// NOT the binding's accumulated state: the walk may have updated the fields
// before the first use (e.g. a bound pinned to a value), and the variable's
// bounds are read as the pristine declaration downstream.
// In pure subtyping the binding object itself is the stable identity (it is
// reused across the passes of a query, see `stenv_push_binding`), so the
// variable needs no memo: the binder's canonical variable where the bounds
// are closed and it is not in use, else a fresh one kept on the binding.
static jl_tvar_t *binding_var(jl_stenv_t *e, jl_varbinding_t *vb) JL_GLOBALLY_ROOTED JL_CANSAFEPOINT
{
    if (vb->var == NULL) {
        jl_value_t *lb = vb->u->lb, *ub = vb->u->ub; // rooted by the memo/binder
        JL_GC_PUSH2(&lb, &ub);
        if (jl_has_dangling_tvarrefs(lb))
            lb = frame_substitute(lb, vb->frame_prev, e);
        if (jl_has_dangling_tvarrefs(ub))
            ub = frame_substitute(ub, vb->frame_prev, e);
        if (e->intersection) {
            vb->var = stenv_binding_var(e, vb->u, lb, ub, vb->frame_prev, vb->other_t);
        }
        else {
            jl_tvar_t *v = NULL;
            if (lb == vb->u->lb && ub == vb->u->ub && !jl_has_dangling_tvarrefs(lb) && !jl_has_dangling_tvarrefs(ub)) {
                jl_tvar_t *cv = jl_unionall_var(vb->u);
                if (!canonical_var_aliased(e, cv, NULL))
                    v = cv;
            }
            if (v == NULL) {
                v = (jl_has_dangling_tvarrefs(lb) || jl_has_dangling_tvarrefs(ub)) ?
                    jl_new_typevar_raw(vb->u->name, lb, ub) : jl_new_typevar(vb->u->name, lb, ub);
                stenv_root(e, (jl_value_t*)v);
            }
            vb->var = v;
        }
        JL_GC_POP();
    }
    return vb->var;
}

// what a reference to the binding re-expresses as: its variable while it is
// live; once popped, a variable carrying its final (frozen) bounds -- or the
// pinned value itself for a binding pinned to one -- created once. This is
// the pop-time renaming of the variable-form representation (an inner
// variable re-owned by the outermost binding), applied lazily.
static jl_value_t *binding_ref_value(jl_stenv_t *e, jl_varbinding_t *vb) JL_GLOBALLY_ROOTED JL_CANSAFEPOINT
{
    if (!vb->popped || e->intersection)
        return (jl_value_t*)binding_var(e, vb);
    if (vb->final_var == NULL) {
        if (vb->final_pending) // a cycle through the bounds: the declared variable
            return (jl_value_t*)binding_var(e, vb);
        vb->final_pending = 1;
        if (binding_pinned(e, vb)) {
            vb->final_var = binding_lb(e, vb); // rooted by the list memo
        }
        else {
            jl_value_t *lb = binding_lb(e, vb);
            jl_value_t *ub = NULL;
            jl_tvar_t *v = NULL;
            JL_GC_PUSH3(&lb, &ub, &v);
            ub = widen_intersect(binding_ub(e, vb));
            v = jl_new_typevar_raw(vb->u->name, lb, ub);
            vb->final_var = (jl_value_t*)v;
            if (e->finalvars == NULL)
                e->finalvars = jl_alloc_array_1d(jl_array_any_type, 0);
            jl_array_ptr_1d_push(e->finalvars, (jl_value_t*)v);
            JL_GC_POP();
        }
        vb->final_pending = 0;
    }
    return vb->final_var;
}

// does `x` contain a reference escaping it that resolves to an existential
// binding in `frame`? (`nested` counts the binders crossed inside `x`)
static int frame_has_existential_ref(jl_value_t *x, jl_varbinding_t *frame, size_t nested) JL_NOTSAFEPOINT
{
    if (frame == NULL)
        return 0;
    if (jl_is_tvarref(x)) {
        size_t d = jl_tvarref_depth(x);
        if (d > nested) {
            jl_varbinding_t *b = frame_lookup(frame, d - nested);
            return b != NULL && b->existential;
        }
        return 0;
    }
    else if (jl_is_uniontype(x) || jl_is_intersecttype(x)) {
        return frame_has_existential_ref(((jl_uniontype_t*)x)->a, frame, nested) ||
               frame_has_existential_ref(((jl_uniontype_t*)x)->b, frame, nested);
    }
    else if (jl_is_unionall(x)) {
        jl_unionall_t *ua = (jl_unionall_t*)x;
        if (!(ua->flags & JL_UNIONALL_ESCAPINGREFS))
            return 0;
        return frame_has_existential_ref(ua->lb, frame, nested) ||
               frame_has_existential_ref(ua->ub, frame, nested) ||
               frame_has_existential_ref(ua->body, frame, nested + 1);
    }
    else if (jl_is_vararg(x)) {
        jl_vararg_t *vm = (jl_vararg_t*)x;
        return (vm->T && frame_has_existential_ref(vm->T, frame, nested)) ||
               (vm->N && frame_has_existential_ref(vm->N, frame, nested));
    }
    else if (jl_is_some_Type(x)) {
        return frame_has_existential_ref(jl_some_Type_T(x), frame, nested);
    }
    else if (jl_is_datatype(x)) {
        if (!((jl_datatype_t*)x)->hasescapingrefs)
            return 0;
        for (size_t i = 0; i < jl_nparams(x); i++) {
            if (frame_has_existential_ref(jl_tparam(x, i), frame, nested))
                return 1;
        }
    }
    return 0;
}

// positional twin of `var_occurs_inside` below: does the binder `d` levels
// out occur in `v` (in invariant position, for want_inv)?
static int tvarref_occurs_inside(jl_value_t *v, size_t d, int inside, int want_inv) JL_NOTSAFEPOINT
{
    if (jl_is_tvarref(v)) {
        return jl_tvarref_depth(v) == d ? inside : 0;
    }
    else if (jl_is_uniontype(v) || jl_is_intersecttype(v)) {
        return tvarref_occurs_inside(((jl_uniontype_t*)v)->a, d, inside, want_inv) ||
            tvarref_occurs_inside(((jl_uniontype_t*)v)->b, d, inside, want_inv);
    }
    else if (jl_is_unionall(v)) {
        jl_unionall_t *ua = (jl_unionall_t*)v;
        if (!(ua->flags & JL_UNIONALL_ESCAPINGREFS))
            return 0; // closed: no reference reaches out to the binder
        // the bounds live outside the binder (same frame as `v`), the body
        // one frame further in
        if (tvarref_occurs_inside(ua->lb, d, inside, want_inv) ||
            tvarref_occurs_inside(ua->ub, d, inside, want_inv))
            return 1;
        return tvarref_occurs_inside(ua->body, d + 1, inside, want_inv);
    }
    else if (jl_is_vararg(v)) {
        jl_vararg_t *vm = (jl_vararg_t*)v;
        if (vm->T) {
            if (tvarref_occurs_inside(vm->T, d, inside || !want_inv, want_inv))
                return 1;
            return vm->N && tvarref_occurs_inside(vm->N, d, 1, want_inv);
        }
    }
    else if (jl_is_some_Type(v)) {
        return tvarref_occurs_inside(jl_some_Type_T(v), d, 1, want_inv);
    }
    else if (jl_is_datatype(v)) {
        if (!((jl_datatype_t*)v)->hasescapingrefs)
            return 0; // closed: no reference reaches out to the binder
        size_t i;
        int istuple = jl_is_tuple_type(v);
        for (i=0; i < jl_nparams(v); i++) {
            int ins_i = inside || !want_inv || !istuple;
            if (tvarref_occurs_inside(jl_tparam(v,i), d, ins_i, want_inv))
                return 1;
        }
    }
    return 0;
}

static int tvarref_occurs_invariant(jl_value_t *v, size_t d) JL_NOTSAFEPOINT
{
    return tvarref_occurs_inside(v, d, 0, 1);
}

// positional twin of `var_occurs_covariant_only` below
static int tvarref_occurs_covariant_only(jl_value_t *t, size_t d, int covariant) JL_NOTSAFEPOINT
{
    if (jl_is_tvarref(t))
        return jl_tvarref_depth(t) == d ? covariant : 1;
    else if (jl_is_uniontype(t)) {
        return tvarref_occurs_covariant_only(((jl_uniontype_t*)t)->a, d, covariant) &&
               tvarref_occurs_covariant_only(((jl_uniontype_t*)t)->b, d, covariant);
    }
    else if (jl_is_unionall(t)) {
        return !jl_tvarref_occurs(t, d);
    }
    else if (jl_is_vararg(t)) {
        jl_vararg_t *vm = (jl_vararg_t*)t;
        if (vm->N && jl_tvarref_occurs(vm->N, d))
            return 0;
        return vm->T == NULL || tvarref_occurs_covariant_only(vm->T, d, covariant);
    }
    else if (jl_is_datatype(t)) {
        if (!((jl_datatype_t*)t)->hasescapingrefs)
            return 1; // closed: the binder does not occur
        int incov = covariant && jl_is_tuple_type(t);
        for (size_t i = 0; i < jl_nparams(t); i++) {
            if (!tvarref_occurs_covariant_only(jl_tparam(t, i), d, incov))
                return 0;
        }
        return 1;
    }
    // conservative for internal nodes (TypeEq, TypeApp, Intersect); plain
    // values contain no references
    return !jl_tvarref_occurs(t, d);
}

// union-stack tools

static int statestack_get(jl_unionstate_t *st, int i) JL_NOTSAFEPOINT
{
    assert(i >= 0 && i < 32767); // limited by the depth bit.
    // get the `i`th bit in an array of 32-bit words
    jl_bits_stack_t *stack = &st->stack;
    while (i >= sizeof(stack->data) * 8) {
        // We should have set this bit.
        assert(stack->next);
        stack = stack->next;
        i -= sizeof(stack->data) * 8;
    }
    return (stack->data[i>>5] & (1u<<(i&31))) != 0;
}

static void statestack_set(jl_unionstate_t *st, int i, int val) JL_NOTSAFEPOINT
{
    assert(i >= 0 && i < 32767); // limited by the depth bit.
    jl_bits_stack_t *stack = &st->stack;
    while (i >= sizeof(stack->data) * 8) {
        if (__unlikely(stack->next == NULL)) {
            stack->next = (jl_bits_stack_t *)malloc(sizeof(jl_bits_stack_t));
            stack->next->next = NULL;
        }
        stack = stack->next;
        i -= sizeof(stack->data) * 8;
    }
    if (val)
        stack->data[i>>5] |= (1u<<(i&31));
    else
        stack->data[i>>5] &= ~(1u<<(i&31));
}

#define has_next_union_state(e, R) ((((R) ? &(e)->Runions : &(e)->Lunions)->more) != 0)

static int next_union_state(jl_stenv_t *e, int8_t R) JL_NOTSAFEPOINT
{
    jl_unionstate_t *state = R ? &e->Runions : &e->Lunions;
    if (state->more == 0)
        return 0;
    // reset `used` and let `pick_union_decision` clean the stack.
    state->used = state->more;
    statestack_set(state, state->used - 1, 1);
    return 1;
}

static int pick_union_decision(jl_stenv_t *e, int8_t R) JL_NOTSAFEPOINT
{
    jl_unionstate_t *state = R ? &e->Runions : &e->Lunions;
    if (state->depth >= state->used) {
        statestack_set(state, state->used, 0);
        state->used++;
    }
    int ui = statestack_get(state, state->depth);
    state->depth++;
    if (ui == 0)
        state->more = state->depth; // memorize that this was the deepest available choice
    return ui;
}

static jl_value_t *pick_union_element(jl_value_t *u JL_PROPAGATES_ROOT, jl_stenv_t *e, int8_t R) JL_NOTSAFEPOINT
{
    do {
        if (pick_union_decision(e, R))
            u = ((jl_uniontype_t*)u)->b;
        else
            u = ((jl_uniontype_t*)u)->a;
    } while (jl_is_uniontype(u));
    return u;
}

#define push_unionstate(saved, src)                                  \
    do {                                                             \
        (saved)->depth = (src)->depth;                               \
        (saved)->more = (src)->more;                                 \
        (saved)->used = (src)->used;                                 \
        jl_bits_stack_t *srcstack = &(src)->stack;                   \
        int pushbits = ((saved)->used+7)/8;                          \
        (saved)->stack = (uint8_t *)alloca(pushbits);                \
        for (int n = 0; n < pushbits; n += sizeof(srcstack->data)) { \
            assert(srcstack != NULL);                                \
            int rest = pushbits - n;                                 \
            if (rest > sizeof(srcstack->data))                       \
                rest = sizeof(srcstack->data);                       \
            memcpy(&(saved)->stack[n], &srcstack->data, rest);       \
            srcstack = srcstack->next;                               \
        }                                                            \
    } while (0);

#define pop_unionstate(dst, saved)                                  \
    do {                                                            \
        (dst)->depth = (saved)->depth;                              \
        (dst)->more = (saved)->more;                                \
        (dst)->used = (saved)->used;                                \
        jl_bits_stack_t *dststack = &(dst)->stack;                  \
        int popbits = ((saved)->used+7)/8;                          \
        for (int n = 0; n < popbits; n += sizeof(dststack->data)) { \
            assert(dststack != NULL);                               \
            int rest = popbits - n;                                 \
            if (rest > sizeof(dststack->data))                      \
                rest = sizeof(dststack->data);                      \
            memcpy(&dststack->data, &(saved)->stack[n], rest);      \
            dststack = dststack->next;                              \
        }                                                           \
    } while (0);

static int current_env_length(jl_stenv_t *e) JL_NOTSAFEPOINT
{
    jl_varbinding_t *v = e->vars;
    int len = 0;
    while (v) {
        len++;
        v = v->prev;
    }
    return len;
}


// Combined covariance count used for diagonal-rule decisions: the max of the
// counter for the current consistency-check scope and the largest count
// observed in any already-closed scope. A variable is diagonal iff
// `cov_count(vb) > 1`.
static inline int8_t cov_count(const jl_varbinding_t *vb) JL_NOTSAFEPOINT
{
    return vb->occurs_cov > vb->cov_diag ? vb->occurs_cov : vb->cov_diag;
}

// the saved state of one binding. The bounds are persistent lists and the
// innervars array is kept alive by `e->roots`, so a save is a plain copy.
typedef struct {
    jl_lterm_t *lbs;
    jl_lterm_t *ubs;
    jl_array_t *innervars;
    int8_t occurs_inv;
    int8_t occurs_cov;
    int8_t cov_diag;
    int8_t max_offset;
    int8_t lb_certainty;
    int8_t lb_required;
    int8_t lb_spell;
    int8_t pinned;
    int8_t occurs;
} jl_savedvar_t;

typedef struct {
    jl_savedvar_t *buf;
    int rdepth;
    int len;
    jl_savedvar_t _space[8];
} jl_savedenv_t;

// Position of a subtype/intersect call within a type's structure. Determines
// whether (and how) a typevar occurrence at this position counts toward the
// diagonal rule (see record_var_occurrence).
typedef enum {
    PARAM_NONE      = 0,  // not inside a covariant/invariant context (top-level
                          // entry, UnionAll body before any constructor, or
                          // bound consistency recheck) — no occurrence recorded
    PARAM_COVARIANT = 1,  // inside a covariant parameter (Tuple/Vararg element)
    PARAM_INVARIANT = 2,  // inside an invariant parameter (most DataType
                          // parameters, Vararg length)
} jl_param_pos_t;

static void re_save_env(jl_stenv_t *e, jl_savedenv_t *se, int root) JL_NOTSAFEPOINT
{
    jl_varbinding_t *v = e->vars;
    int i = 0;
    while (v != NULL) {
        assert(i < se->len);
        jl_savedvar_t *sv = &se->buf[i++];
        if (root) {
            sv->lbs = v->lbs;
            sv->ubs = v->ubs;
            sv->innervars = v->innervars;
        }
        sv->occurs = v->occurs;
        sv->occurs_inv = v->occurs_inv;
        sv->occurs_cov = v->occurs_cov;
        sv->cov_diag = v->cov_diag;
        sv->max_offset = v->max_offset;
        sv->lb_certainty = v->lb_certainty;
        sv->lb_required = v->lb_required;
        sv->lb_spell = v->lb_spell;
        sv->pinned = v->pinned;
        v = v->prev;
    }
    assert(i == se->len);
    se->rdepth = e->Runions.depth;
}

static void alloc_env(jl_stenv_t *e, jl_savedenv_t *se, int root) JL_NOTSAFEPOINT
{
    int len = current_env_length(e);
    se->len = len;
    se->buf = (len > 8 ? (jl_savedvar_t*)malloc_s(len * sizeof(jl_savedvar_t)) : se->_space);
    (void)root;
}

static void save_env(jl_stenv_t *e, jl_savedenv_t *se, int root) JL_NOTSAFEPOINT
{
    alloc_env(e, se, root);
    re_save_env(e, se, root);
}

static void free_env(jl_savedenv_t *se) JL_NOTSAFEPOINT
{
    if (se->buf != se->_space)
        free(se->buf);
    se->buf = NULL;
}

static void free_stenv(jl_stenv_t *e) JL_NOTSAFEPOINT
{
    if (e->arena != NULL)
        starena_free(e->arena);
    for (int R = 0; R < 2; R++) {
        jl_bits_stack_t *temp = R ? e->Runions.stack.next : e->Lunions.stack.next;
        while (temp != NULL) {
            jl_bits_stack_t *next = temp->next;
            free(temp);
            temp = next;
        }
    }
}

static void restore_env(jl_stenv_t *e, jl_savedenv_t *se, int root) JL_NOTSAFEPOINT
{
    jl_varbinding_t *v = e->vars;
    int i = 0;
    while (v != NULL) {
        assert(i < se->len);
        jl_savedvar_t *sv = &se->buf[i++];
        if (root) {
            v->lbs = sv->lbs;
            v->ubs = sv->ubs;
            v->innervars = sv->innervars;
        }
        v->occurs = sv->occurs;
        v->occurs_inv = sv->occurs_inv;
        v->occurs_cov = sv->occurs_cov;
        v->cov_diag = sv->cov_diag;
        v->max_offset = sv->max_offset;
        v->lb_certainty = sv->lb_certainty;
        v->lb_required = sv->lb_required;
        v->lb_spell = sv->lb_spell;
        v->pinned = sv->pinned;
        v = v->prev;
    }
    assert(i == se->len);
    e->Runions.depth = se->rdepth;
    if (e->envout && e->envidx < e->envsz)
        memset(&e->envout[e->envidx], 0, (e->envsz - e->envidx)*sizeof(void*));
}

#define flip_offset(e) ((e)->Loffset *= -1)

// type utilities

static jl_uaenv_t *uaenv_lookup(jl_uaenv_t *env, size_t depth) JL_NOTSAFEPOINT
{
    while (env != NULL && depth > 1) {
        env = env->prev;
        depth--;
    }
    return depth == 1 ? env : NULL; // NULL for a dangling reference
}

// Return true if `T == Union{}` (`T` located under `env`; a dangling reference
// is not known to be)
static int is_bottom_bound(jl_value_t *T, jl_uaenv_t *env) JL_NOTSAFEPOINT
{
    while (1) {
        if (jl_is_typevar(T)) {
            T = ((jl_tvar_t*)T)->ub;
        }
        else if (jl_is_tvarref(T)) {
            jl_uaenv_t *b = uaenv_lookup(env, jl_tvarref_depth(T));
            if (b == NULL)
                return 0;
            T = b->ua->ub;
            env = b->prev;
        }
        else if (jl_is_uniontype(T)) {
            if (!is_bottom_bound(((jl_uniontype_t*)T)->a, env))
                return 0;
            T = ((jl_uniontype_t*)T)->b;
        }
        else {
            return T == jl_bottom_type;
        }
    }
}

// Return true if `t == TypeofBottom` (`t` located under `env`)
static int is_typeofbottom_typealias_env(jl_value_t *t, jl_uaenv_t *env) JL_NOTSAFEPOINT
{
    if (t == NULL)
        return 0;
    if (jl_typeofbottom_type == NULL)
        return 0;
    if (t == (jl_value_t*)jl_typeofbottom_type)
        return 1;
    return jl_is_typeeq(t) && is_bottom_bound(jl_typeeq_T(t), env);
}

static int is_typeofbottom_typealias(jl_value_t *t) JL_NOTSAFEPOINT
{
    return is_typeofbottom_typealias_env(t, NULL);
}

static jl_value_t *normalize_typeofbottom_typealias(jl_value_t *t) JL_NOTSAFEPOINT
{
    return is_typeofbottom_typealias(t) ? (jl_value_t*)jl_typeofbottom_type : t;
}

// quickly test that two types are identical (egal, `===`)
static int obviously_egal(jl_value_t *a, jl_value_t *b) JL_NOTSAFEPOINT
{
    if (a == b) return 1;
    // NB: do NOT normalize the `Type{Union{}}`/`TypeofBottom` typealias here.
    // Those two are `==` but not `===`, so conflating them is unsound for an
    // egality test — in particular for egality-keyed `TypeEgal{...}` slots,
    // where `TypeEgal{Type{Union{}}}` and `TypeEgal{TypeofBottom}` are distinct
    // types with different subtype behavior (#61323). (`obviously_unequal`
    // keeps the normalization: under `==` the pair is equal, hence not unequal.)
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
    if (jl_is_uniontype(a) || jl_is_intersecttype(a)) {
        return obviously_egal(((jl_uniontype_t*)a)->a, ((jl_uniontype_t*)b)->a) &&
            obviously_egal(((jl_uniontype_t*)a)->b, ((jl_uniontype_t*)b)->b);
    }
    if (jl_is_unionall(a)) {
        return ((jl_unionall_t*)a)->name == ((jl_unionall_t*)b)->name &&
            obviously_egal(((jl_unionall_t*)a)->lb, ((jl_unionall_t*)b)->lb) &&
            obviously_egal(((jl_unionall_t*)a)->ub, ((jl_unionall_t*)b)->ub) &&
            obviously_egal(((jl_unionall_t*)a)->body, ((jl_unionall_t*)b)->body);
    }
    if (jl_is_tvarref(a))
        return jl_is_tvarref(b) && jl_tvarref_depth(a) == jl_tvarref_depth(b);
    if (jl_is_vararg(a)) {
        jl_vararg_t *vma = (jl_vararg_t *)a;
        jl_vararg_t *vmb = (jl_vararg_t *)b;
        return obviously_egal(jl_unwrap_vararg(vma), jl_unwrap_vararg(vmb)) &&
            ((!vma->N && !vmb->N) || (vma->N && vmb->N && obviously_egal(vma->N, vmb->N)));
    }
    if (jl_is_some_Type(a))
        return obviously_egal(jl_some_Type_T(a), jl_some_Type_T(b));
    if (jl_is_typevar(a)) return 0;
    return !jl_is_type(a) && jl_egal(a,b);
}

static int obviously_unequal(jl_value_t *a, jl_value_t *b) JL_NOTSAFEPOINT
{
    if (a == b)
        return 0;
    a = normalize_typeofbottom_typealias(a);
    b = normalize_typeofbottom_typealias(b);
    if (a == b)
        return 0;
    if (jl_is_unionall(a))
        a = jl_unwrap_unionall(a);
    if (jl_is_unionall(b))
        b = jl_unwrap_unionall(b);
    // detached bound-variable references stand for unknown types (their
    // binders' constraints are not visible here), so no verdict is obvious
    if (jl_has_dangling_tvarrefs(a) || jl_has_dangling_tvarrefs(b))
        return 0;
    if (jl_is_datatype(a)) {
        if (b == jl_bottom_type)
            return 1;
        if (jl_is_datatype(b)) {
            jl_datatype_t *ad = (jl_datatype_t*)a;
            jl_datatype_t *bd = (jl_datatype_t*)b;
            if (a == (jl_value_t*)jl_typeofbottom_type && jl_is_typeeq(b))
                return obviously_unequal(jl_bottom_type, jl_tparam(bd, 0));
            if (jl_is_typeeq(a) && b == (jl_value_t*)jl_typeofbottom_type)
                return obviously_unequal(jl_tparam(ad, 0), jl_bottom_type);
            if (ad->name != bd->name)
                return 1;
            int istuple = (ad->name == jl_tuple_typename);
            if (jl_type_equality_is_identity(a, b))
                return 1;
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
    if (jl_is_typeegal(a) && jl_is_typeegal(b))
        return obviously_unequal(jl_typeegal_T(a), jl_typeegal_T(b));
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

static int obviously_in_union(jl_value_t *u, jl_value_t *x)
{
    jl_value_t *a = NULL, *b = NULL;
    if (jl_is_uniontype(x)) {
        a = ((jl_uniontype_t*)x)->a;
        b = ((jl_uniontype_t*)x)->b;
        JL_GC_PUSH2(&a, &b);
        int res = obviously_in_union(u, a) && obviously_in_union(u, b);
        JL_GC_POP();
        return res;
    }
    if (jl_is_uniontype(u)) {
        a = ((jl_uniontype_t*)u)->a;
        b = ((jl_uniontype_t*)u)->b;
        JL_GC_PUSH2(&a, &b);
        int res = obviously_in_union(a, x) || obviously_in_union(b, x);
        JL_GC_POP();
        return res;
    }
    return obviously_egal(u, x);
}

// the types whose instances are all themselves types: the concrete kinds plus the
// abstract kind `AnyType` (`== Type`, though not `===`)
STATIC_INLINE int is_kind_or_anytype(jl_value_t *t) JL_NOTSAFEPOINT
{
    return jl_is_kind(t) || t == (jl_value_t*)jl_anytype_type;
}

int obviously_disjoint(jl_value_t *a, jl_value_t *b, int specificity) JL_NOTSAFEPOINT
{
    if (a == b || a == (jl_value_t*)jl_any_type || b == (jl_value_t*)jl_any_type)
        return 0;
    if (specificity && a == (jl_value_t*)jl_typeofbottom_type)
        return 0;
    if (jl_is_concrete_type(a) && jl_is_concrete_type(b) && jl_type_equality_is_identity(a, b))
        return 1;
    if (jl_is_unionall(a)) a = jl_unwrap_unionall(a);
    if (jl_is_unionall(b)) b = jl_unwrap_unionall(b);
    if (jl_is_uniontype(a))
        return obviously_disjoint(((jl_uniontype_t *)a)->a, b, specificity) &&
               obviously_disjoint(((jl_uniontype_t *)a)->b, b, specificity);
    if (jl_is_uniontype(b))
        return obviously_disjoint(a, ((jl_uniontype_t *)b)->a, specificity) &&
               obviously_disjoint(a, ((jl_uniontype_t *)b)->b, specificity);
    if (jl_is_datatype(a) && jl_is_datatype(b)) {
        jl_datatype_t *ad = (jl_datatype_t*)a, *bd = (jl_datatype_t*)b;
        if (ad->name != bd->name) {
            jl_datatype_t *temp = ad;
            while (temp != NULL && temp != jl_any_type && temp->name != bd->name)
                temp = temp->super;
            if (temp == NULL) // deferred supertype: not obviously disjoint
                return 0;
            if (temp == jl_any_type) {
                temp = bd;
                while (temp != NULL && temp != jl_any_type && temp->name != ad->name)
                    temp = temp->super;
                if (temp == NULL)
                    return 0;
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
            if (jl_is_typevar(ai) || jl_is_typevar(bi) || jl_is_tvarref(ai) || jl_is_tvarref(bi))
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
static jl_value_t *simple_join(jl_value_t *a, jl_value_t *b) JL_CANSAFEPOINT
{
    if (a == jl_bottom_type || b == (jl_value_t*)jl_any_type || obviously_egal(a, b))
        return b;
    if (b == jl_bottom_type || a == (jl_value_t*)jl_any_type)
        return a;
    if (!(jl_is_type(a) || jl_is_typevar(a)) || !(jl_is_type(b) || jl_is_typevar(b)))
        return (jl_value_t*)jl_any_type;
    // a kind absorbs a `TypeEgal{T}` with that tag (its sole member is `T`
    // itself) and `Type{Union{}}` (`== TypeofBottom`); it does not absorb other
    // `Type{T}`s, whose members straddle several kinds (#33136)
    if (jl_is_kind(a) && jl_is_typeegal(b) && jl_typeof(jl_typeegal_T(b)) == a)
        return a;
    if (jl_is_kind(b) && jl_is_typeegal(a) && jl_typeof(jl_typeegal_T(a)) == b)
        return b;
    if (a == (jl_value_t*)jl_typeofbottom_type && jl_is_typeeq(b) && jl_typeeq_T(b) == jl_bottom_type)
        return a;
    if (b == (jl_value_t*)jl_typeofbottom_type && jl_is_typeeq(a) && jl_typeeq_T(a) == jl_bottom_type)
        return b;
    if (jl_is_typevar(a) && obviously_egal(b, ((jl_tvar_t*)a)->lb))
        return a;
    if (jl_is_typevar(b) && obviously_egal(a, ((jl_tvar_t*)b)->lb))
        return b;
    return simple_union(a, b);
}

// Compute a greatest lower bound of `a` and `b`
// For the subtype path, we need to over-estimate this by returning `b` in many cases.
// But for `merge_env`, we'd better under-estimate and return a `Union{}`
static jl_value_t *simple_meet(jl_value_t *a, jl_value_t *b, int overesi) JL_CANSAFEPOINT
{
    if (a == (jl_value_t*)jl_any_type || b == jl_bottom_type || obviously_egal(a,b))
        return b;
    if (b == (jl_value_t*)jl_any_type || a == jl_bottom_type)
        return a;
    if (overesi == 1 && (jl_is_intersecttype(a) || jl_is_intersecttype(b)))
        // one operand is already an internal `Intersect` meet node.
        // Represent the combined meet exactly by nesting.
        return jl_new_struct(jl_intersect_type, a, b);
    if (!(jl_is_type(a) || jl_is_typevar(a)) || !(jl_is_type(b) || jl_is_typevar(b)))
        return jl_bottom_type;
    // as in `simple_join`: a kind contains a `TypeEgal{T}` with that tag and
    // `Type{Union{}}` (`== TypeofBottom`), but not other `Type{T}`s (#33136)
    if (jl_is_kind(a) && jl_is_typeegal(b) && jl_typeof(jl_typeegal_T(b)) == a)
        return b;
    if (jl_is_kind(b) && jl_is_typeegal(a) && jl_typeof(jl_typeegal_T(a)) == b)
        return a;
    if (a == (jl_value_t*)jl_typeofbottom_type && jl_is_typeeq(b) && jl_typeeq_T(b) == jl_bottom_type)
        return b;
    if (b == (jl_value_t*)jl_typeofbottom_type && jl_is_typeeq(a) && jl_typeeq_T(a) == jl_bottom_type)
        return a;
    if (jl_is_typevar(a) && obviously_egal(b, ((jl_tvar_t*)a)->ub))
        return a;
    if (jl_is_typevar(b) && obviously_egal(a, ((jl_tvar_t*)b)->ub))
        return b;
    return simple_intersect(a, b, overesi);
}

// Over-approximate an internal `Intersect` meet node (see #61917) by a real
// type, so it cannot escape subtyping into a result type or static parameter.
// An `Intersect` only ever occurs as the top layer of a varbinding's `ub`
// (possibly as a spine of nested `Intersect`s, but never under another type
// constructor), so it suffices to peel that spine here. `Intersect{a, b}`
// denotes `a ∩ b`, which `simple_meet` with `overesi==2` over-approximates by a
// real supertype. `typeintersect` may over-approximate, so this is sound.
static jl_value_t *widen_intersect(jl_value_t *t) JL_CANSAFEPOINT
{
    if (t == NULL || !jl_is_intersecttype(t))
        return t;
    jl_value_t *a = NULL, *b = NULL, *res = NULL;
    JL_GC_PUSH2(&a, &b);
    a = widen_intersect(((jl_intersecttype_t*)t)->a);
    b = widen_intersect(((jl_intersecttype_t*)t)->b);
    res = simple_meet(a, b, 2);
    JL_GC_POP();
    return res;
}

// main subtyping algorithm

static int subtype(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, jl_param_pos_t param) JL_CANSAFEPOINT;

static int local_forall_exists_subtype(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, jl_param_pos_t param, int limit_slow) JL_CANSAFEPOINT;

static int is_leaf_typevar(jl_tvar_t *v) JL_NOTSAFEPOINT;
static int is_leaf_binder(jl_varbinding_t *vb) JL_NOTSAFEPOINT;

// Check whether env (variable bounds & diagonality) changed compared to saved env.
static int env_unchanged(jl_stenv_t *e, jl_savedenv_t *se) JL_NOTSAFEPOINT
{
    jl_varbinding_t *v = e->vars;
    int i = 0;
    while (v != NULL) {
        assert(i < se->len);
        jl_savedvar_t *sv = &se->buf[i++];
        if (v->existential) {
            if (v->lbs != sv->lbs || v->ubs != sv->ubs)
                return 0; // check if bounds changed
            int8_t saved_cov = sv->occurs_cov;
            int8_t saved_diag = sv->cov_diag;
            int8_t saved_max = saved_cov > saved_diag ? saved_cov : saved_diag;
            if (is_leaf_binder(v) && v->body_occurs_inv == 0 && cov_count(v) > 1 && saved_max <= 1)
                return 0; // check if a variable became diagonal from non-diagonal
            if (v->lb_required != sv->lb_required)
                return 0; // check if envout constrainedness changed
        }
        v = v->prev;
    }
    return 1;
}

static int push_consistency_scope(jl_stenv_t *e, int8_t *saved) JL_NOTSAFEPOINT;
static void pop_consistency_scope(jl_stenv_t *e, const int8_t *saved, int nsaved) JL_NOTSAFEPOINT;

// subtype for variable bounds consistency check. needs its own forall/exists environment.
static int subtype_ccheck_(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int chain_relative) JL_CANSAFEPOINT
{
    if (jl_is_long(x) && jl_is_long(y))
        return jl_unbox_long(x) == jl_unbox_long(y) + e->Loffset;
    // the structural fast paths compare content without consulting the
    // frames: correct for canonical operands, whose leftover dangling
    // references are query-global constants (cf. the bare-reference leaf
    // rule), but not for chain-relative raw operands under differing
    // chains, where an identical spelling can denote different binders
    int structural = !chain_relative ||
        (!jl_has_dangling_tvarrefs(x) && !jl_has_dangling_tvarrefs(y));
    if (x == y && structural)
        return 1;
    if (x == jl_bottom_type && jl_is_type(y))
        return 1;
    if (y == (jl_value_t*)jl_any_type && jl_is_type(x))
        return 1;
    if (jl_is_uniontype(x) && structural && jl_egal(x, y))
        return 1;
    if (x == (jl_value_t*)jl_any_type && jl_is_datatype(y))
        return 0;
    if (structural && obviously_in_union(y, x))
        return 1;
    jl_saved_unionstate_t oldLunions; push_unionstate(&oldLunions, &e->Lunions);
    // Consistency check for a typevar bound: covariant occurrences inside this
    // call should not accumulate into the surrounding scope's diagonality
    // counter. Save & reset the counters, then fold the local max into
    // cov_diag on exit.
    int8_t *saved_cov = (int8_t*)alloca(current_env_length(e));
    int nsaved_cov = push_consistency_scope(e, saved_cov);
    // A check on a closed x-term checks an actual value of the query, so bounds
    // recorded inside keep the current certainty channel; descent into that
    // value is structural (`value_descent`), so it also preserves identity
    // across equality wrappers. A typevar-containing x checks a typevar bound
    // proxy, whose bindings need not exist for every call. NOTE: tuple-prefix
    // `lb_required` marking stays active here on purpose — a var reached only
    // through another var's declared tuple bound (e.g. `E` via
    // `S <: Tuple{Vararg{E}}`) is pinned by every member exactly when the
    // x-term supplies a fixed prefix element (see `mark_required_tuple_element`).
    int saved_channel = e->bound_channel;
    int saved_spell = e->spell_channel;
    int saved_descent = e->value_descent;
    if (has_free_or_dangling_typevars(x)) {
        // (a raw bound-variable reference is var-dependence all the same)
        if (e->bound_channel > BOUND_PROXY)
            e->bound_channel = BOUND_PROXY;
        if (e->spell_channel > BOUND_PROXY)
            e->spell_channel = BOUND_PROXY;
    }
    else {
        e->value_descent = 1;
    }
    int sub = local_forall_exists_subtype(x, y, e, PARAM_COVARIANT, 1);
    e->bound_channel = saved_channel;
    e->spell_channel = saved_spell;
    e->value_descent = saved_descent;
    pop_consistency_scope(e, saved_cov, nsaved_cov);
    pop_unionstate(&e->Lunions, &oldLunions);
    return sub;
}

static int subtype_ccheck(jl_value_t *x, jl_value_t *y, jl_stenv_t *e) JL_CANSAFEPOINT
{
    return subtype_ccheck_(x, y, e, 0);
}

// consistency check with explicit per-operand frames: used to walk a
// still-raw declared bound (under its binding's own chain, which outlives
// the binding) against a term at the current position, without
// materializing anything
static int subtype_ccheck_frames(jl_value_t *x, jl_varbinding_t *xframe,
                                 jl_value_t *y, jl_varbinding_t *yframe, jl_stenv_t *e) JL_CANSAFEPOINT
{
    jl_varbinding_t *saveL = e->Lframe, *saveR = e->Rframe;
    e->Lframe = xframe;
    e->Rframe = yframe;
    int sub = subtype_ccheck_(x, y, e, xframe != yframe);
    e->Lframe = saveL;
    e->Rframe = saveR;
    return sub;
}

static int subtype_left_var(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, jl_param_pos_t param) JL_CANSAFEPOINT
{
    if (jl_is_long(x) && jl_is_long(y))
        return jl_unbox_long(x) == jl_unbox_long(y) + e->Loffset;
    if (x == y && !(jl_is_unionall(y)))
        return 1;
    if (x == jl_bottom_type && jl_is_type(y))
        return 1;
    if (y == (jl_value_t*)jl_any_type && jl_is_type(x))
        return 1;
    if (jl_is_uniontype(x) && jl_egal(x, y))
        return 1;
    if (x == (jl_value_t*)jl_any_type && jl_is_datatype(y))
        return 0;
    return subtype(x, y, e, param);
}

// pick an entry of a list walked as a union of alternatives (a right-nested
// `Union` of its entries): the enclosing ∀∃ loops enumerate the choices
static jl_lterm_t *lterm_pick(jl_lterm_t *l, jl_stenv_t *e, int8_t R) JL_NOTSAFEPOINT
{
    while (l->next != NULL && pick_union_decision(e, R))
        l = l->next;
    return l;
}

// `subtype(c, y)` for a located term in the left position: the term is
// walked under its own chain. `subtype_left_var`'s identity fast paths
// compare content, which is only meaningful when at most one operand is
// chain-relative (they decide the trivial cases without registering any
// union decision, which matters: a `Union` bound walked against `Any`
// must not multiply the enclosing ∀∃ enumeration)
static int subtype_located_left(jl_stenv_t *e, jl_lterm_t *c, jl_value_t *y, jl_param_pos_t param) JL_CANSAFEPOINT
{
    jl_varbinding_t *saveL = e->Lframe;
    e->Lframe = c->frame;
    int sub = !jl_has_dangling_tvarrefs(y) ? subtype_left_var(c->t, y, e, param)
                                           : subtype(c->t, y, e, param);
    e->Lframe = saveL;
    return sub;
}

// `subtype(x, c)` for a located term in the right position
static int subtype_located_right(jl_stenv_t *e, jl_value_t *x, jl_lterm_t *c, jl_param_pos_t param) JL_CANSAFEPOINT
{
    jl_varbinding_t *saveR = e->Rframe;
    e->Rframe = c->frame;
    int sub = !jl_has_dangling_tvarrefs(x) ? subtype_left_var(x, c->t, e, param)
                                           : subtype(x, c->t, e, param);
    e->Rframe = saveR;
    return sub;
}

// `ub(vb) <: y`: the meet of the entries lies in `y` if some entry does (a
// universal binding has a single entry: its declared bound or an arm of it)
static int subtype_binding_ub(jl_stenv_t *e, jl_varbinding_t *vb, jl_value_t *y, jl_param_pos_t param) JL_CANSAFEPOINT
{
    if (vb->ubs == NULL)
        return subtype_left_var((jl_value_t*)jl_any_type, y, e, param);
    jl_lterm_t *c = vb->ubs->next != NULL ? lterm_pick(vb->ubs, e, 0) : vb->ubs;
    return subtype_located_left(e, c, y, param);
}

// `x <: lb(vb)`: `x` lies in the join of the entries if it lies in some entry
static int subtype_binding_lb(jl_stenv_t *e, jl_value_t *x, jl_varbinding_t *vb, jl_param_pos_t param) JL_CANSAFEPOINT
{
    if (vb->lbs == NULL)
        return subtype_left_var(x, jl_bottom_type, e, param);
    jl_lterm_t *c = vb->lbs->next != NULL ? lterm_pick(vb->lbs, e, 1) : vb->lbs;
    return subtype_located_right(e, x, c, param);
}

// consistency checks of a bound against a term at the current position: the
// join of the lower bound's entries lies in `a` iff every entry does, and `a`
// lies in the meet of the upper bound's entries iff it lies in every entry
static int ccheck_lbs_le(jl_stenv_t *e, jl_lterm_t *lbs, jl_value_t *a, jl_varbinding_t *aframe) JL_CANSAFEPOINT
{
    for (jl_lterm_t *c = lbs; c != NULL; c = c->next) {
        if (!subtype_ccheck_frames(c->t, c->frame, a, aframe, e))
            return 0;
    }
    return 1;
}

static int ccheck_le_ubs(jl_stenv_t *e, jl_value_t *a, jl_varbinding_t *aframe, jl_lterm_t *ubs) JL_CANSAFEPOINT
{
    for (jl_lterm_t *c = ubs; c != NULL; c = c->next) {
        if (!subtype_ccheck_frames(a, aframe, c->t, c->frame, e))
            return 0;
    }
    return 1;
}

// does some entry of the binding's bounds refer to a binder outside the
// query (a detached fragment)? Such a binding supports no bound reasoning.
static int binding_detached(jl_varbinding_t *vb) JL_NOTSAFEPOINT
{
    for (jl_lterm_t *c = vb->lbs; c != NULL; c = c->next)
        if (c->detached)
            return 1;
    for (jl_lterm_t *c = vb->ubs; c != NULL; c = c->next)
        if (c->detached)
            return 1;
    return 0;
}

// the live binding a bare reference at `frame` denotes, if any
static jl_varbinding_t *ref_binding(jl_value_t *a, jl_varbinding_t *frame) JL_NOTSAFEPOINT
{
    if (!jl_is_tvarref(a))
        return NULL;
    jl_varbinding_t *b = frame_lookup(frame, jl_tvarref_depth(a));
    return b != NULL && b->live ? b : NULL;
}

// is the binding's declared bound (closed) egal to `t`?
static int binding_declared_egal(jl_varbinding_t *X, int ub, jl_value_t *t) JL_NOTSAFEPOINT
{
    jl_value_t *b = ub ? X->u->ub : X->u->lb;
    return !jl_has_dangling_tvarrefs(b) && obviously_egal(b, t);
}

// `simple_subtype` for located terms: can `a` (at `af`) be shown to lie in
// `b` (at `bf`) from the declared bounds of the bindings they refer to? This
// is the subsumption the variable-form representation gets from
// `simple_union`/`simple_intersect` when a bound is updated.
static int lsimple_subtype(jl_stenv_t *e, jl_value_t *a, jl_varbinding_t *af, jl_value_t *b, jl_varbinding_t *bf,
                           int isUnion, int depth) JL_CANSAFEPOINT
{
    assert(a != NULL && b != NULL);
    if (depth > 32)
        return 0;
    if (a == jl_bottom_type || b == (jl_value_t*)jl_any_type)
        return 1;
    int adang = jl_has_dangling_tvarrefs(a), bdang = jl_has_dangling_tvarrefs(b);
    if (!adang)
        af = NULL;
    if (!bdang)
        bf = NULL;
    if (af == bf ? jl_egal(a, b) : (adang && bdang && egal_frames(a, af, b, bf, 0, e)))
        return 1;
    int afree = adang || jl_has_free_typevars(a), bfree = bdang || jl_has_free_typevars(b);
    if (!afree && !bfree) {
        int mergeable = isUnion;
        if (!mergeable) // issue #24521: don't merge Type{T} where typeof(T) varies
            mergeable = !(jl_is_typeeq(a) && jl_is_typeeq(b) &&
             jl_typeof(jl_typeeq_T(a)) != jl_typeof(jl_typeeq_T(b)));
        return mergeable && jl_subtype(a, b);
    }
    // the components of a union are compared one by one
    if (jl_is_uniontype(a))
        return lsimple_subtype(e, ((jl_uniontype_t*)a)->a, af, b, bf, isUnion, depth + 1) &&
               lsimple_subtype(e, ((jl_uniontype_t*)a)->b, af, b, bf, isUnion, depth + 1);
    if (jl_is_uniontype(b))
        return lsimple_subtype(e, a, af, ((jl_uniontype_t*)b)->a, bf, isUnion, depth + 1) ||
               lsimple_subtype(e, a, af, ((jl_uniontype_t*)b)->b, bf, isUnion, depth + 1);
    if (jl_is_typevar(a))
        return lsimple_subtype(e, ((jl_tvar_t*)a)->ub, NULL, b, bf, isUnion, depth + 1);
    if (jl_is_typevar(b)) {
        jl_value_t *nb = ((jl_tvar_t*)b)->lb;
        // This branch is not valid if `b` obeys diagonal rule,
        // as it might normalize `Union` into a single `TypeVar`, e.g.
        // Tuple{Union{Int,T},T} where {T>:Int} != Tuple{T,T} where {T>:Int}
        if (is_leaf_bound(nb))
            return 0;
        return lsimple_subtype(e, a, af, nb, NULL, isUnion, depth + 1);
    }
    if (jl_is_tvarref(a)) {
        // a reference behaves like the variable it resolves to: chase its
        // declared upper bound (located under the binding's chain)
        jl_varbinding_t *X = af != NULL ? frame_lookup(af, jl_tvarref_depth(a)) : NULL;
        if (X == NULL)
            return 0; // detached: no bound information
        return lsimple_subtype(e, X->u->ub, X->frame_prev, b, bf, isUnion, depth + 1);
    }
    if (jl_is_tvarref(b)) {
        jl_varbinding_t *Y = bf != NULL ? frame_lookup(bf, jl_tvarref_depth(b)) : NULL;
        if (Y == NULL)
            return 0;
        jl_value_t *nb = Y->u->lb;
        if (is_leaf_bound(nb))
            return 0;
        return lsimple_subtype(e, a, af, nb, Y->frame_prev, isUnion, depth + 1);
    }
    if (b == (jl_value_t*)jl_typeofbottom_type)
        return jl_is_typeeq(a) && jl_typeeq_T(a) == jl_bottom_type;
    return 0;
}

// rebuild a list without the entries `a` subsumes (`sub`: entries c with
// c <: a; else entries with a <: c)
static jl_lterm_t *lterm_drop_subsumed(jl_stenv_t *e, jl_lterm_t *l, jl_value_t *a, jl_varbinding_t *frame, int sub, int isUnion) JL_CANSAFEPOINT
{
    int drop = 0;
    for (jl_lterm_t *c = l; c != NULL; c = c->next) {
        if (sub ? lsimple_subtype(e, c->t, c->frame, a, frame, isUnion, 0)
                : lsimple_subtype(e, a, frame, c->t, c->frame, isUnion, 0)) {
            drop = 1;
            break;
        }
    }
    if (!drop)
        return l;
    jl_lterm_t *res = NULL;
    for (jl_lterm_t *c = l; c != NULL; c = c->next) {
        if (!(sub ? lsimple_subtype(e, c->t, c->frame, a, frame, isUnion, 0)
                  : lsimple_subtype(e, a, frame, c->t, c->frame, isUnion, 0)))
            res = lterm_cons(e, c->t, c->frame, res);
    }
    return res;
}

// rebuild a list without the entries a reference to `X` absorbs (closed
// entries egal to X's declared bound, the variable-form `T ∩ T.ub == T` rule)
static jl_lterm_t *lterm_drop_absorbed(jl_stenv_t *e, jl_lterm_t *l, jl_varbinding_t *X, int ub) JL_NOTSAFEPOINT
{
    int drop = 0;
    for (jl_lterm_t *c = l; c != NULL; c = c->next) {
        if (c->frame == NULL && binding_declared_egal(X, ub, c->t)) {
            drop = 1;
            break;
        }
    }
    if (!drop)
        return l;
    jl_lterm_t *res = NULL;
    for (jl_lterm_t *c = l; c != NULL; c = c->next) {
        if (!(c->frame == NULL && binding_declared_egal(X, ub, c->t)))
            res = lterm_cons(e, c->t, c->frame, res);
    }
    return res;
}

// does a reference entry to a binding whose declared bound is egal to the
// closed `a` exist? (then `a` adds nothing)
static int lterm_has_absorbing_ref(jl_lterm_t *l, jl_value_t *a, int ub) JL_NOTSAFEPOINT
{
    for (jl_lterm_t *c = l; c != NULL; c = c->next) {
        jl_varbinding_t *Y = c->frame != NULL ? ref_binding(c->t, c->frame) : NULL;
        if (Y != NULL && binding_declared_egal(Y, ub, a))
            return 1;
    }
    return 0;
}

// record `a` (located at `frame`) as a new upper bound: the meet with the
// current bound. Two closed bounds combine as `simple_meet` combines them in
// variable form; an unresolved meet keeps its operands as separate entries
// (the list is the `Intersect` spine), and located entries just accumulate.
static void binding_meet_ub(jl_stenv_t *e, jl_varbinding_t *bb, jl_value_t *a, jl_varbinding_t *frame) JL_CANSAFEPOINT
{
    if (a == (jl_value_t*)jl_any_type)
        return;
    if (!jl_has_dangling_tvarrefs(a))
        frame = NULL;
    jl_lterm_t *ubs = bb->ubs;
    if (ubs == NULL) {
        bb->ubs = lterm_cons(e, a, frame, NULL);
        return;
    }
    if (lterm_find(e, ubs, a, frame) != NULL)
        return;
    jl_value_t *cur = lterm_closed1(ubs);
    if (cur == jl_bottom_type)
        return;
    if (a == jl_bottom_type) {
        bb->ubs = lterm_cons(e, a, NULL, NULL);
        return;
    }
    if (cur != NULL && frame == NULL) {
        jl_value_t *m = simple_meet(cur, a, 1);
        if (m == cur)
            return;
        if (!jl_is_intersecttype(m)) {
            binding_set_ub(e, bb, m);
            return;
        }
        bb->ubs = lterm_cons(e, a, NULL, ubs);
        return;
    }
    jl_varbinding_t *X = frame != NULL ? ref_binding(a, frame) : NULL;
    if (X != NULL)
        ubs = lterm_drop_absorbed(e, ubs, X, 1);
    else if (frame == NULL && lterm_has_absorbing_ref(ubs, a, 1))
        return;
    // the subsumption and disjointness `simple_intersect` resolves
    for (jl_lterm_t *c = ubs; c != NULL; c = c->next) {
        if (lsimple_subtype(e, c->t, c->frame, a, frame, 0, 0))
            return; // the meet is the existing entry
        if (obviously_disjoint(c->t, a, 0)) {
            bb->ubs = lterm_cons(e, jl_bottom_type, NULL, NULL);
            return;
        }
    }
    ubs = lterm_drop_subsumed(e, ubs, a, frame, 0, 0);
    bb->ubs = lterm_cons(e, a, frame, ubs);
}

// record `a` (located at `frame`) as a new lower bound: the join with the
// current bound. Returns whether the bound changed.
static int binding_join_lb(jl_stenv_t *e, jl_varbinding_t *bb, jl_value_t *a, jl_varbinding_t *frame) JL_CANSAFEPOINT
{
    if (a == jl_bottom_type)
        return 0;
    if (!jl_has_dangling_tvarrefs(a))
        frame = NULL;
    jl_lterm_t *lbs = bb->lbs;
    if (lbs == NULL) {
        bb->lbs = lterm_cons(e, a, frame, NULL);
        return 1;
    }
    if (lterm_find(e, lbs, a, frame) != NULL)
        return 0;
    jl_value_t *cur = lterm_closed1(lbs);
    if (cur == (jl_value_t*)jl_any_type)
        return 0;
    if (a == (jl_value_t*)jl_any_type) {
        bb->lbs = lterm_cons(e, a, NULL, NULL);
        return 1;
    }
    if (cur != NULL && frame == NULL) {
        jl_value_t *m = simple_join(cur, a);
        if (m == cur)
            return 0;
        binding_set_lb(e, bb, m);
        return 1;
    }
    jl_varbinding_t *X = frame != NULL ? ref_binding(a, frame) : NULL;
    if (X != NULL)
        lbs = lterm_drop_absorbed(e, lbs, X, 0);
    else if (frame == NULL && lterm_has_absorbing_ref(lbs, a, 0))
        return 0;
    // the subsumption `simple_union` resolves
    for (jl_lterm_t *c = lbs; c != NULL; c = c->next) {
        if (lsimple_subtype(e, a, frame, c->t, c->frame, 1, 0))
            return 0; // `a` adds nothing
    }
    lbs = lterm_drop_subsumed(e, lbs, a, frame, 1, 1);
    bb->lbs = lterm_cons(e, a, frame, lbs);
    return 1;
}

// use the current context to record where a variable occurred, for the purpose
// of determining whether the variable is concrete.
static int has_existential_typevar(jl_value_t *x, jl_stenv_t *e) JL_NOTSAFEPOINT;

// `a` is the matched term in positional form against `frame`: it is free of
// existential variables when neither a free variable nor a dangling reference
// of it stands for an existential binding (no spelling is materialized here)
static void record_var_pin(jl_varbinding_t *vb, jl_value_t *a, jl_varbinding_t *frame, jl_stenv_t *e, jl_param_pos_t param) JL_NOTSAFEPOINT
{
    if (!(e->envsz > 0 && param == PARAM_INVARIANT && e->invdepth > vb->depth0 && !vb->pinned))
        return;
    if (jl_has_free_typevars(a) && has_existential_typevar(a, e))
        return;
    if (jl_has_dangling_tvarrefs(a)) {
        size_t d = 1;
        for (jl_varbinding_t *fb = frame; fb != NULL; fb = fb->frame_prev, d++) {
            if (fb->existential && jl_tvarref_occurs(a, d))
                return;
        }
    }
    vb->pinned = 1;
}

static void record_var_occurrence(jl_varbinding_t *vb, jl_stenv_t *e, jl_param_pos_t param) JL_NOTSAFEPOINT
{
    if (vb != NULL) {
        // saturate counters at 2; we don't need values bigger than that
        if (vb->occurs < 2)
            vb->occurs++;
        if (param == PARAM_INVARIANT && e->invdepth > vb->depth0 && vb->occurs_inv < 2)
            vb->occurs_inv++;
        else if (param == PARAM_COVARIANT && vb->occurs_cov < 2)
            vb->occurs_cov++;
        // Always set `max_offset` to `-1` during the 1st round intersection.
        // Would be recovered in `intersect_varargs`/`subtype_tuple_varargs` if needed.
        if (!vb->intersected)
            vb->max_offset = -1;
    }
}

// Scope the diagonal-rule's covariance counter to the surrounding
// covariant-position context, so that occurrences inside a consistency check
// (`subtype_ccheck` / `intersect_aside`) of a typevar's bound do not
// contaminate the outer covariance count. Covariant positions in covariant
// position tuples within the same scope still accumulate as before.
//
// `push_consistency_scope` saves the current `occurs_cov` of every live var
// into `saved` and resets it to 0; `pop_consistency_scope` folds the in-scope
// value into `cov_diag` (via max) and restores `occurs_cov` from `saved`.
// The diagonal-rule test then becomes `max(occurs_cov, cov_diag) > 1`: a
// variable is diagonal iff it occurred >= 2 times in some single scope (the
// outer scope or any consistency check), rather than summed across all
// consistency checks.
static int push_consistency_scope(jl_stenv_t *e, int8_t *saved) JL_NOTSAFEPOINT
{
    jl_varbinding_t *v = e->vars;
    int i = 0;
    while (v != NULL) {
        saved[i++] = v->occurs_cov;
        v->occurs_cov = 0;
        v = v->prev;
    }
    return i;
}

static void pop_consistency_scope(jl_stenv_t *e, const int8_t *saved, int nsaved) JL_NOTSAFEPOINT
{
    jl_varbinding_t *v = e->vars;
    int i = 0;
    while (v != NULL && i < nsaved) {
        if (v->occurs_cov > v->cov_diag)
            v->cov_diag = v->occurs_cov;
        v->occurs_cov = saved[i++];
        v = v->prev;
    }
}

// When expanding a universal variable's declared upper/lower bound during
// `var_lt` / `var_gt`, occurrences contributed by the expanded bound (which
// can only mention forall-side vars) must not combine with occurrences in the
// enclosing tuple body. We push a separate evidence frame for forall vars
// only: their counts are reset before the recursive subtype call and folded
// into `cov_diag` afterward, while exists-side vars continue accumulating in
// the current scope (their occurrences in the call's right-hand structure are
// still part of the surrounding pattern).
static int push_forall_bound_scope(jl_stenv_t *e, int8_t *saved) JL_NOTSAFEPOINT
{
    jl_varbinding_t *v = e->vars;
    int i = 0;
    while (v != NULL) {
        saved[i++] = v->occurs_cov;
        if (!v->existential)
            v->occurs_cov = 0;
        v = v->prev;
    }
    return i;
}

static void pop_forall_bound_scope(jl_stenv_t *e, const int8_t *saved, int nsaved) JL_NOTSAFEPOINT
{
    jl_varbinding_t *v = e->vars;
    int i = 0;
    while (v != NULL && i < nsaved) {
        if (!v->existential) {
            if (v->occurs_cov > v->cov_diag)
                v->cov_diag = v->occurs_cov;
            v->occurs_cov = saved[i];
        }
        i++;
        v = v->prev;
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

// `var_outside` for bindings (which need not have materialized variables)
static int binding_outside(jl_stenv_t *e, jl_varbinding_t *x, jl_varbinding_t *y) JL_NOTSAFEPOINT
{
    jl_varbinding_t *btemp = e->vars;
    while (btemp != NULL) {
        if (btemp == x) return 0;
        if (btemp == y) return 1;
        btemp = btemp->prev;
    }
    return 0;
}

static jl_value_t *intersect_aside(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int depth) JL_CANSAFEPOINT;

static int reachable_var(jl_value_t *x, jl_tvar_t *y, jl_stenv_t *e) JL_CANSAFEPOINT;

static int singleton_typevar_subtype(jl_tvar_t *v, jl_value_t *a) JL_NOTSAFEPOINT
{
    if (a == (jl_value_t*)v || a == (jl_value_t*)jl_any_type)
        return 1;
    if (jl_is_uniontype(a))
        return singleton_typevar_subtype(v, ((jl_uniontype_t*)a)->a) ||
               singleton_typevar_subtype(v, ((jl_uniontype_t*)a)->b);
    return 0;
}

static int subtype_singleton_typevar(jl_value_t *a, jl_tvar_t *v) JL_NOTSAFEPOINT
{
    if (a == (jl_value_t*)v || a == jl_bottom_type)
        return 1;
    if (jl_is_uniontype(a))
        return subtype_singleton_typevar(((jl_uniontype_t*)a)->a, v) &&
               subtype_singleton_typevar(((jl_uniontype_t*)a)->b, v);
    return 0;
}

// check that type var `b` is <: `a`, and update b's upper bound.
// `a` is a fragment of the right term: it is walked in place through the
// frames, and stored as a located term; nothing is re-expressed.
static int var_lt(jl_tvar_t *b, jl_value_t *a, jl_stenv_t *e, jl_param_pos_t param, jl_varbinding_t *bb, int innervar) JL_CANSAFEPOINT
{
    if (bb == NULL) {
        assert(b != NULL); // only a real (free or inner) variable has no binding
        if (innervar && e->intersection)
            return 1;
        if (innervar)
            return subtype_left_var(b->ub, a, e, param);
        return singleton_typevar_subtype(b, a);
    }
    assert(bb->live);
    record_var_occurrence(bb, e, param);
    if (binding_detached(bb)) {
        // the binder of a detached fragment: its bounds reference binders
        // outside the query, so they support no bound reasoning -- only the
        // trivial relations hold (cf. the bare-reference leaf rule)
        return a == (jl_value_t*)jl_any_type || (b != NULL && a == (jl_value_t*)b);
    }
    assert(!jl_is_long(a) || e->Loffset == 0);
    if (e->Loffset != 0 && !jl_is_typevar(a) && !jl_is_tvarref(a) &&
        a != jl_bottom_type && a != (jl_value_t *)jl_any_type)
        return 0;
    if (!bb->existential) {  // check ∀b . b<:a
        // The expanded bound `bb->ub` lives in the forall-side context;
        // its covariant typevar occurrences must not combine with the
        // surrounding tuple body's occurrences.
        int8_t *saved_fb = (int8_t*)alloca(current_env_length(e));
        int nsaved_fb = push_forall_bound_scope(e, saved_fb);
        int sub = subtype_binding_ub(e, bb, a, param);
        pop_forall_bound_scope(e, saved_fb, nsaved_fb);
        return sub;
    }
    record_var_pin(bb, a, e->Rframe, e, param);
    // the identity fast path: `a` is already an entry of the upper bound
    if (lterm_find(e, bb->ubs, a, e->Rframe) != NULL)
        return 1;
    int lb_ok = (bb->lbs == NULL && !jl_is_type(a) && !jl_is_typevar(a));
    if (!lb_ok) {
        if (e->intersection && bb->in_ccheck) {
            // this binding's consistency check is already pending above us
            // (its bounds reach the variable itself through pinned variables);
            // assume it holds coinductively — the outer check decides
            lb_ok = 1;
        }
        else {
            bb->in_ccheck = 1;
            lb_ok = ccheck_lbs_le(e, bb->lbs, a, e->Rframe);
            bb->in_ccheck = 0;
        }
    }
    if (!lb_ok)
        return 0;
    // for this to work we need to compute issub(left,right) before issub(right,left),
    // since otherwise the issub(a, bb.ub) check in var_gt becomes vacuous.
    if (e->intersection) {
        // the intersection code keeps its bounds as types in variable form
        jl_value_t *av = jl_has_dangling_tvarrefs(a) ? frame_substitute(a, e->Rframe, e) : a;
        if (b == NULL)
            b = bb->var;
        jl_value_t *ub = intersect_aside(av, binding_ub(e, bb), e, bb->depth0);
        JL_GC_PUSH1(&ub);
        if ((b == NULL || ub != (jl_value_t*)b) &&
            (!jl_is_typevar(ub) || b == NULL || !reachable_var(ub, b, e)))
            binding_set_ub(e, bb, ub);
        JL_GC_POP();
        assert(lterm_closed1(bb->ubs) != (jl_value_t*)b);
    }
    else {
        binding_meet_ub(e, bb, a, e->Rframe);
    }
    return 1;
}

// check that type var `b` is >: `a`, and update b's lower bound.
// `a` is a fragment of the left term: it is walked in place through the
// frames, and stored as a located term.
static int var_gt(jl_tvar_t *b, jl_value_t *a, jl_stenv_t *e, jl_param_pos_t param, jl_varbinding_t *bb, int innervar) JL_CANSAFEPOINT
{
    if (bb == NULL) {
        assert(b != NULL); // only a real (free or inner) variable has no binding
        if (innervar && e->intersection)
            return 1;
        if (innervar)
            return subtype_left_var(a, b->lb, e, param);
        return subtype_singleton_typevar(a, b);
    }
    assert(bb->live);
    record_var_occurrence(bb, e, param);
    if (binding_detached(bb)) {
        // see var_lt: a detached fragment's binder supports no bound reasoning
        return a == jl_bottom_type || (b != NULL && a == (jl_value_t*)b);
    }
    assert(!jl_is_long(a) || e->Loffset == 0);
    if (e->Loffset != 0 && !jl_is_typevar(a) && !jl_is_tvarref(a) &&
        a != jl_bottom_type && a != (jl_value_t *)jl_any_type)
        return 0;
    if (!bb->existential) {  // check ∀b . b>:a
        // Symmetric to var_lt: scope forall-side occurrences from the expanded
        // lower bound away from the enclosing tuple body.
        int8_t *saved_fb = (int8_t*)alloca(current_env_length(e));
        int nsaved_fb = push_forall_bound_scope(e, saved_fb);
        int sub = subtype_binding_lb(e, a, bb, param);
        pop_forall_bound_scope(e, saved_fb, nsaved_fb);
        return sub;
    }
    if (a != jl_bottom_type && bb->lb_certainty < e->bound_channel)
        bb->lb_certainty = e->bound_channel;
    record_var_pin(bb, a, e->Lframe, e, param);
    // the identity fast path: `a` is already an entry of the lower bound
    if (lterm_find(e, bb->lbs, a, e->Lframe) != NULL) {
        if (bb->lb_spell < e->spell_channel)
            bb->lb_spell = e->spell_channel;
        return 1;
    }
    if (!(bb->ubs == NULL && !jl_is_type(a) && !jl_is_typevar(a))) {
        int ub_ok;
        if (e->intersection && bb->in_ccheck) {
            // see var_lt: answer a re-entrant consistency check coinductively
            ub_ok = 1;
        }
        else {
            int saved = e->ignore_lb_required;
            e->ignore_lb_required = 1;
            bb->in_ccheck = 1;
            ub_ok = ccheck_le_ubs(e, a, e->Lframe, bb->ubs);
            bb->in_ccheck = 0;
            e->ignore_lb_required = saved;
        }
        if (!ub_ok)
            return 0;
    }
    // when the var is pinned (`lb === ub`), `a <= ub` was just checked and a
    // join picking `a` proves `lb <= a`, i.e. `a` respells the same type: keep
    // the existing spelling unless `a`'s is more authoritative (see `lb_spell`)
    int pinned = binding_pinned(e, bb);
    if (e->intersection) {
        jl_value_t *av = jl_has_dangling_tvarrefs(a) ? frame_substitute(a, e->Lframe, e) : a;
        if (b == NULL)
            b = bb->var;
        jl_value_t *bb_lb = binding_lb(e, bb);
        jl_value_t *lb = simple_join(bb_lb, av);
        JL_GC_PUSH1(&lb);
        if (pinned && lb == av && e->spell_channel <= bb->lb_spell) {
            // keep bb->lb (and bb->ub) as-is
        }
        else if (!jl_is_typevar(lb) || b == NULL || !reachable_var(lb, b, e)) {
            if (bb_lb != lb) {
                binding_set_lb(e, bb, lb);
                bb->lb_spell = e->spell_channel;
            }
        }
        JL_GC_POP();
        // this bound should not be directly circular
        assert(lterm_closed1(bb->lbs) != (jl_value_t*)b);
        return 1;
    }
    if (pinned && e->spell_channel <= bb->lb_spell) {
        jl_value_t *cur = lterm_closed1(bb->lbs);
        if (cur != NULL && !jl_has_dangling_tvarrefs(a)) {
            jl_value_t *lb = simple_join(cur, a);
            if (lb == a)
                return 1; // keep the existing spelling
            if (lb != cur) {
                binding_set_lb(e, bb, lb);
                bb->lb_spell = e->spell_channel;
            }
            return 1;
        }
    }
    if (binding_join_lb(e, bb, a, e->Lframe))
        bb->lb_spell = e->spell_channel;
    return 1;
}

// `b` may be NULL when the binding's variable has not been materialized
// (a bound-variable reference dispatched directly on `bb`); the variable is
// only an identity token here, and an unmaterialized one cannot occur in any
// bound or operand.
static int subtype_var(jl_tvar_t *b, jl_value_t *a, jl_stenv_t *e, int R, jl_param_pos_t param, jl_varbinding_t *bb, int innervar) JL_CANSAFEPOINT
{
    assert(b != NULL || bb != NULL);
    if (e->intersection) {
        jl_value_t *bub = bb ? binding_ub(e, bb) : innervar ? ((jl_tvar_t*)b)->ub : (jl_value_t*)b;
        jl_value_t *blb = bb ? binding_lb(e, bb) : innervar ? ((jl_tvar_t*)b)->lb : (jl_value_t*)b;
        if (bub == blb && jl_is_typevar(bub) && (b == NULL || bub != (jl_value_t*)b)) {
            int bubinner = 0;
            jl_varbinding_t *bubb = lookup_binding(e, (jl_tvar_t*)bub, &bubinner);
            int sub = subtype_var((jl_tvar_t *)bub, a, e, R, param, bubb, bubinner);
            return sub;
        }
    }
    if (e->Loffset != 0 && jl_is_long(a)) {
        int old_offset = R ? -e->Loffset : e->Loffset;
        jl_value_t *na = jl_box_long(jl_unbox_long(a) + old_offset);
        JL_GC_PUSH1(&na);
        stenv_root(e, na); // it may be stored
        e->Loffset = 0;
        int sub = R ? var_gt(b, na, e, param, bb, innervar) : var_lt(b, na, e, param, bb, innervar);
        e->Loffset = R ? -old_offset : old_offset;
        JL_GC_POP();
        return sub;
    }
    return R ? var_gt(b, a, e, param, bb, innervar) : var_lt(b, a, e, param, bb, innervar);
}

// check that a type is concrete or quasi-concrete (Type{T}).
// this is used to check concrete typevars:
// issubtype is false if the lower bound of a concrete type var is not concrete.
int is_leaf_bound(jl_value_t *v) JL_NOTSAFEPOINT
{
    if (v == jl_bottom_type)
        return 1;
    if (jl_is_intersecttype(v)) // internal meet node (see #61917), not a concrete leaf
        return 0;
    if (jl_is_some_Type(v))
        return 1;
    if (jl_is_datatype(v)) {
        if (((jl_datatype_t*)v)->name->abstract) {
            return 0;
        }
        return ((jl_datatype_t*)v)->isconcretetype;
    }
    // a bound-variable reference classifies like the variable it stands for
    return !jl_is_type(v) && !jl_is_typevar(v) && !jl_is_tvarref(v);
}

static int is_leaf_typevar(jl_tvar_t *v) JL_NOTSAFEPOINT
{
    return is_leaf_bound(v->lb);
}

// the diagonal-rule concreteness classification of a binding, from the
// binder's raw declared lower bound (no variable needs to be materialized)
static int is_leaf_binder(jl_varbinding_t *vb) JL_NOTSAFEPOINT
{
    return is_leaf_bound(vb->u->lb);
}

// One entry per binder walked past, innermost first: a bound-variable
// reference with de Bruijn index k refers to the k-th entry. The entry
// carries the binder's bounds (expressed outside its own scope, i.e. under
// `prev`) since a reference cannot reach them itself.
typedef struct _typeeq_varctx_t {
    jl_value_t *lb;
    jl_value_t *ub;
    int pinned; // lb === ub: occurrences behave like that closed bound
    struct _typeeq_varctx_t *prev;
} typeeq_varctx_t;

static typeeq_varctx_t *typeeq_lookup_ref(typeeq_varctx_t *env, size_t idx) JL_NOTSAFEPOINT
{
    while (env != NULL && idx > 1) {
        idx--;
        env = env->prev;
    }
    return idx == 1 ? env : NULL;
}

static int typeeq_vars_bound_in_env(jl_value_t *t, jl_stenv_t *e, typeeq_varctx_t *wenv, jl_varbinding_t *frame, size_t nintro) JL_NOTSAFEPOINT;
static int typeeq_kind_mask(jl_value_t *t) JL_NOTSAFEPOINT;
static int typeeq_mask_le(int mask, jl_value_t *y) JL_NOTSAFEPOINT;

// The concrete tag containing `t` when `t` pins one object: `typeof(T)` for a
// `TypeEgal{T}` (sole member `T`), for `Type{Union{}}` (`== TypeofBottom`),
// and for a dangling-var dispatch key (see `typeeq_vars_bound_in_env`). Any
// other `Type{T}` has members of several tags and no concrete supertype, so
// there is nothing to widen to (#33136).
static jl_value_t *widen_pinned_Type(jl_value_t *t JL_PROPAGATES_ROOT, jl_stenv_t *e, typeeq_varctx_t *wenv, jl_varbinding_t *frame) JL_NOTSAFEPOINT
{
    if (jl_is_typeegal(t))
        return jl_typeof(jl_typeegal_T(t));
    if (jl_is_typeeq(t) && !jl_is_typevar(jl_typeeq_T(t)) && !jl_is_tvarref(jl_typeeq_T(t))) {
        jl_value_t *T = jl_typeeq_T(t);
        if (T == jl_bottom_type)
            return (jl_value_t*)jl_typeofbottom_type;
        if ((jl_has_free_typevars(T) || jl_has_dangling_tvarrefs(T)) &&
            !typeeq_vars_bound_in_env(T, e, wenv, frame, 0))
            return jl_typeof(T);
    }
    return NULL;
}

// Widen a `Type{X}` lower bound to a type tag for the diagonal-concreteness
// check. In the universal (subtype) direction this is only valid when the tag
// really contains all of `Type{X}` -- a pinned single object, or a class whose
// whole kind cover is that one tag (`Type{Vector} <: UnionAll`). In the
// existential (intersection) direction the tag instead selects the (nonempty)
// tag-homogeneous slice of the members as the witness for the diagonal
// variable, so the unconditional tag is a valid choice there.
static jl_value_t *widen_Type_if_concrete(jl_value_t *t JL_PROPAGATES_ROOT, jl_stenv_t *e, typeeq_varctx_t *wenv, jl_varbinding_t *frame, int existential) JL_CANSAFEPOINT
{
    jl_value_t *w = widen_pinned_Type(t, e, wenv, frame);
    if (w == NULL && jl_is_typeeq(t) && !jl_is_typevar(jl_typeeq_T(t)) && !jl_is_tvarref(jl_typeeq_T(t))) {
        jl_value_t *tag = jl_typeof(jl_typeeq_T(t));
        if (existential || typeeq_mask_le(typeeq_kind_mask(jl_typeeq_T(t)), tag))
            w = tag;
    }
    if (w != NULL)
        return w;
    if (jl_is_uniontype(t)) {
        jl_value_t *a = widen_Type_if_concrete(((jl_uniontype_t*)t)->a, e, wenv, frame, existential);
        JL_GC_PUSH1(&a);
        jl_value_t *b = widen_Type_if_concrete(((jl_uniontype_t*)t)->b, e, wenv, frame, existential);
        JL_GC_POP();
        if (a == b)
            return a;
    }
    if (jl_is_unionall(t)) {
        // vars bound by binders we walk past are not dangling
        jl_unionall_t *u = (jl_unionall_t*)t;
        typeeq_varctx_t ctx = { u->lb, u->ub, 0, wenv };
        jl_value_t *body = widen_Type_if_concrete(u->body, e, &ctx, frame, existential);
        if (body != u->body && !jl_tvarref_occurs(body, 1)) {
            JL_GC_PUSH1(&body);
            jl_value_t *r = jl_shift_dangling_refs(body, -1);
            JL_GC_POP();
            return r;
        }
    }
    return t;
}

static int try_subtype_in_env_existential(jl_value_t *a, jl_value_t *b, jl_stenv_t *e) JL_CANSAFEPOINT;

// Map Type{X} to kind type (DataType, UnionAll, Union, TypeofBottom) over union
// only if the widened kind satisfies `bound` , otherwise leave unchanged
static jl_value_t *widen_Type_to_union(jl_value_t *t, jl_value_t *bound, jl_stenv_t *e) JL_CANSAFEPOINT
{
    if (jl_is_some_Type(t) && !jl_is_typevar(jl_some_Type_T(t)) && !jl_is_tvarref(jl_some_Type_T(t))) {
        // This runs in the existential (intersection) direction only, where
        // the tag selects the (nonempty) tag-homogeneous slice of `Type{X}`'s
        // members as the witness for the variable, so widening the bound to
        // the tag remains a valid choice under `==`-class semantics (#33136);
        // the result may then under-represent members of other tags, as
        // intersection always could for diagonal variables.
        jl_value_t *w = jl_typeof(jl_some_Type_T(t));
        if (!try_subtype_in_env_existential(w, bound, e))
            return t;
        return w;
    }
    if (jl_is_uniontype(t)) {
        jl_value_t *wa = NULL, *wb = NULL;
        JL_GC_PUSH2(&wa, &wb);
        wa = widen_Type_to_union(((jl_uniontype_t*)t)->a, bound, e);
        wb = widen_Type_to_union(((jl_uniontype_t*)t)->b, bound, e);
        if (wa != ((jl_uniontype_t*)t)->a || wb != ((jl_uniontype_t*)t)->b)
            wa = simple_join(wa, wb);
        else
            wa = t;
        JL_GC_POP();
        return wa;
    }
    if (jl_is_unionall(t)) {
        jl_unionall_t *u = (jl_unionall_t*)t;
        jl_value_t *body = NULL;
        JL_GC_PUSH1(&body);
        body = widen_Type_to_union(u->body, bound, e);
        if (body != u->body && !jl_tvarref_occurs(body, 1)) {
            body = jl_shift_dangling_refs(body, -1);
            JL_GC_POP();
            return body;
        }
        JL_GC_POP();
    }
    return t;
}

static int var_occurs_inside(jl_value_t *v, jl_tvar_t *var, int inside, int want_inv) JL_NOTSAFEPOINT;

// wrap a TypeVar env entry as svec(tvar, constrained): preserves TypeVar
// identity while carrying the "constrained by any concrete subtype" bit.
// `tvar` is the uncertain value (something with has_free_typevars)
// `constrained` is 1 if all concrete subtypes of the LHS will pin this var to a definite value.
static jl_value_t *wrap_tvar_env(jl_value_t *tvar, int constrained) JL_CANSAFEPOINT
{
    return (jl_value_t*)jl_svec2(tvar, constrained ? jl_true : jl_false);
}

// Positional counterpart of `pins_typeof_static` below: is the binder that
// `t` references at `depth` (1 = the binder whose body `t` was taken from)
// pinned to a `typeof`-produced argument type by every call matching `t`?
// Walks the raw (unopened) body, so entering a nested binder increments the
// depth of the reference being tracked. A false return is always conservative.
static int pins_typeof_static_ref(size_t depth, jl_value_t *t) JL_NOTSAFEPOINT
{
    if (jl_is_tvarref(t) && jl_tvarref_depth(t) == depth)
        return 1;
    while (jl_is_unionall(t)) {
        jl_unionall_t *ua = (jl_unionall_t*)t;
        // the binder's bounds sit outside its own scope: `depth` is unchanged
        if (jl_is_tvarref(ua->ub) && jl_tvarref_depth(ua->ub) == depth &&
            ua->lb == jl_bottom_type && pins_typeof_static_ref(1, ua->body))
            // the least solution for the tracked binder is this binder's value
            return 1;
        t = ua->body;
        depth++;
    }
    if (jl_is_uniontype(t)) {
        return pins_typeof_static_ref(depth, ((jl_uniontype_t*)t)->a) &&
               pins_typeof_static_ref(depth, ((jl_uniontype_t*)t)->b);
    }
    if (!jl_is_datatype(t) || jl_is_abstracttype(t))
        return 0;
    jl_datatype_t *dt = (jl_datatype_t*)t;
    if (dt->name == jl_tuple_typename) {
        size_t fc = jl_nparams(dt);
        for (size_t i = 0; i < fc; i++) {
            jl_value_t *p = jl_tparam(dt, i);
            // a `Vararg` tail may match zero arguments
            if (!jl_is_vararg(p) && pins_typeof_static_ref(depth, p))
                return 1;
        }
    }
    return 0;
}


// Static check mirroring a conservative subset of
// Core.Compiler.constrains_var: is `var` guaranteed to be pinned by any
// concrete leaftype subtype of `typ`? A false return is always safe; a true
// return must also hold under `constrains_var`. Used where a path-independent
// answer is wanted: the env-copy fast paths in `jl_subtype_env`/`intersect`,
// which have no dynamic varbinding state to draw from, and
// `mark_required_tuple_element`, whose answer must not depend on the
// in-progress dynamic bounds even though `e->vars` is available there.
static int constrains_param_static(jl_tvar_t *var, jl_value_t *typ, int covariant) JL_NOTSAFEPOINT
{
    if (typ == (jl_value_t*)var)
        // a covariant occurrence contributes `typeof` of an argument as a
        // lower bound, which determines the least solution only when the
        // declared lower bound does not also union into it
        return !covariant || var->lb == jl_bottom_type;
    while (jl_is_unionall(typ)) {
        jl_unionall_t *ua = (jl_unionall_t*)typ;
        // occurrences in the inner binder's declared bound pin `var` only
        // when every call pins that binder to a `typeof`-produced argument
        // type, satisfying its bounds without constraining them. (The bounds
        // sit on the UnionAll, outside its own scope; positional references
        // mean `var` can never be rebound by an inner binder.)
        if (covariant && ua->lb == jl_bottom_type &&
            jl_has_typevar(ua->ub, var) &&
            pins_typeof_static_ref(1, ua->body) &&
            constrains_param_static(var, ua->ub, 1))
            return 1;
        typ = ua->body;
    }
    if (jl_is_uniontype(typ)) {
        // conservatively, both alternatives must constrain var
        return constrains_param_static(var, ((jl_uniontype_t*)typ)->a, covariant) &&
               constrains_param_static(var, ((jl_uniontype_t*)typ)->b, covariant);
    }
    else if (jl_is_typeeq(typ)) {
        return constrains_param_static(var, jl_typeeq_T(typ), 0);
    }
    else if (jl_is_datatype(typ)) {
        jl_datatype_t *dt = (jl_datatype_t*)typ;
        size_t fc = jl_nparams(dt);
        if (fc > 0) {
            if (dt->name == jl_tuple_typename) {
                for (size_t i = 0; i < fc - 1; i++) {
                    if (constrains_param_static(var, jl_tparam(dt, i), covariant))
                        return 1;
                }
                jl_value_t *lastp = jl_tparam(dt, fc - 1);
                jl_value_t *vararg = jl_unwrap_unionall(lastp);
                if (jl_is_vararg(vararg)) {
                    jl_value_t *vN = jl_unwrap_vararg_num(vararg);
                    if (vN) {
                        if (constrains_param_static(var, vN, covariant))
                            return 1;
                    }
                    else if (constrains_param_static(var, lastp, covariant)) {
                        return 1;
                    }
                }
                else if (constrains_param_static(var, lastp, covariant)) {
                    return 1;
                }
            }
            else {
                for (size_t i = 0; i < fc; i++) {
                    if (constrains_param_static(var, jl_tparam(dt, i), 0))
                        return 1;
                }
            }
        }
    }
    return 0;
}

// positional twin of `constrains_param_static`: does the binder `d` levels
// out (whose declared lower bound is `Union{}` iff `lb_bottom` — the bounds
// live outside the walked body) get pinned by `typ`?
static int constrains_ref_static(size_t d, int lb_bottom, jl_value_t *typ, int covariant) JL_NOTSAFEPOINT
{
    if (jl_is_tvarref(typ))
        // a covariant occurrence contributes `typeof` of an argument as a
        // lower bound, which determines the least solution only when the
        // declared lower bound does not also union into it
        return jl_tvarref_depth(typ) == d && (!covariant || lb_bottom);
    while (jl_is_unionall(typ)) {
        jl_unionall_t *ua = (jl_unionall_t*)typ;
        // occurrences in the inner binder's declared bound pin the tracked
        // binder only when every call pins that inner binder to a
        // `typeof`-produced argument type, satisfying its bounds without
        // constraining them (see `constrains_param_static`)
        if (covariant && ua->lb == jl_bottom_type &&
            jl_tvarref_occurs(ua->ub, d) &&
            pins_typeof_static_ref(1, ua->body) &&
            constrains_ref_static(d, lb_bottom, ua->ub, 1))
            return 1;
        typ = ua->body;
        d++;
    }
    if (jl_is_uniontype(typ)) {
        // conservatively, both alternatives must constrain the binder
        return constrains_ref_static(d, lb_bottom, ((jl_uniontype_t*)typ)->a, covariant) &&
               constrains_ref_static(d, lb_bottom, ((jl_uniontype_t*)typ)->b, covariant);
    }
    else if (jl_is_typeeq(typ)) {
        return constrains_ref_static(d, lb_bottom, jl_typeeq_T(typ), 0);
    }
    else if (jl_is_datatype(typ)) {
        jl_datatype_t *dt = (jl_datatype_t*)typ;
        size_t fc = jl_nparams(dt);
        if (fc > 0) {
            if (dt->name == jl_tuple_typename) {
                for (size_t i = 0; i < fc - 1; i++) {
                    if (constrains_ref_static(d, lb_bottom, jl_tparam(dt, i), covariant))
                        return 1;
                }
                jl_value_t *lastp = jl_tparam(dt, fc - 1);
                jl_value_t *vararg = lastp;
                size_t dv = d;
                while (jl_is_unionall(vararg)) {
                    // count the stripped binders so the reference depth is
                    // still relative to its position
                    vararg = ((jl_unionall_t*)vararg)->body;
                    dv++;
                }
                if (jl_is_vararg(vararg)) {
                    jl_value_t *vN = jl_unwrap_vararg_num(vararg);
                    if (vN) {
                        if (constrains_ref_static(dv, lb_bottom, vN, covariant))
                            return 1;
                    }
                    else if (constrains_ref_static(d, lb_bottom, lastp, covariant)) {
                        return 1;
                    }
                }
                else if (constrains_ref_static(d, lb_bottom, lastp, covariant)) {
                    return 1;
                }
            }
            else {
                for (size_t i = 0; i < fc; i++) {
                    if (constrains_ref_static(d, lb_bottom, jl_tparam(dt, i), 0))
                        return 1;
                }
            }
        }
    }
    return 0;
}

static void mark_required_tuple_element(jl_stenv_t *e, jl_value_t *rhs) JL_NOTSAFEPOINT
{
    if (e->ignore_lb_required)
        return;
    // bindings occur in the (raw) walked term as positional references: check
    // each frame entry at its depth from `rhs`'s position
    size_t d = 1;
    for (jl_varbinding_t *b = e->Rframe; b != NULL; b = b->frame_prev, d++) {
        if (b->existential && b->lbs != NULL && !b->lb_required &&
            constrains_ref_static(d, b->u->lb == jl_bottom_type, rhs, 1))
            b->lb_required = 1;
    }
    // re-expressed (variable-form) content still carries variables
    for (jl_varbinding_t *v = e->vars; v != NULL; v = v->prev) {
        if (v->existential && v->lbs != NULL && !v->lb_required && v->var != NULL &&
            constrains_param_static(v->var, rhs, 1))
            v->lb_required = 1;
    }
}

typedef int (*tvar_callback)(void*, int8_t, jl_stenv_t *, int);

static int var_occurs_invariant(jl_value_t *v, jl_tvar_t *var) JL_NOTSAFEPOINT
{
    return var_occurs_inside(v, var, 0, 1);
}

static int has_existential_typevar(jl_value_t *x, jl_stenv_t *e) JL_NOTSAFEPOINT
{
    jl_typeenv_t *env = NULL;
    jl_varbinding_t *v = e->vars;
    while (v != NULL) {
        if (v->existential) {
            jl_typeenv_t *newenv = (jl_typeenv_t*)alloca(sizeof(jl_typeenv_t));
            newenv->var = v->var;
            newenv->val = NULL;
            newenv->prev = env;
            env = newenv;
        }
        v = v->prev;
    }
    return env != NULL && jl_has_bound_typevars(x, env);
}

// does a bound refer to an existential binding (through an entry's chain) or
// contain an existential variable (in its variable-form content)?
static int lterm_has_existential(jl_stenv_t *e, jl_lterm_t *l) JL_NOTSAFEPOINT
{
    for (; l != NULL; l = l->next) {
        if (l->frame != NULL ? frame_has_existential_ref(l->t, l->frame, 0)
                             : has_existential_typevar(l->t, e))
            return 1;
    }
    return 0;
}

static int has_universal_typevar(jl_value_t *x, jl_stenv_t *e) JL_NOTSAFEPOINT
{
    jl_typeenv_t *env = NULL;
    jl_varbinding_t *v = e->vars;
    while (v != NULL) {
        if (!v->existential) {
            jl_typeenv_t *newenv = (jl_typeenv_t*)alloca(sizeof(jl_typeenv_t));
            newenv->var = v->var;
            newenv->val = NULL;
            newenv->prev = env;
            env = newenv;
        }
        if (v->innervars != NULL) {
            for (size_t i = 0; i < jl_array_len(v->innervars); i++) {
                jl_typeenv_t *newenv = (jl_typeenv_t*)alloca(sizeof(jl_typeenv_t));
                newenv->var = (jl_tvar_t*)jl_array_ptr_ref(v->innervars, i);
                newenv->val = NULL;
                newenv->prev = env;
                env = newenv;
            }
        }
        v = v->prev;
    }
    if (e->finalvars != NULL) {
        for (size_t i = 0; i < jl_array_len(e->finalvars); i++) {
            jl_typeenv_t *newenv = (jl_typeenv_t*)alloca(sizeof(jl_typeenv_t));
            newenv->var = (jl_tvar_t*)jl_array_ptr_ref(e->finalvars, i);
            newenv->val = NULL;
            newenv->prev = env;
            env = newenv;
        }
    }
    return env != NULL && jl_has_bound_typevars(x, env);
}

static int has_env_typevar(jl_value_t *x, jl_stenv_t *e) JL_NOTSAFEPOINT
{
    return jl_has_free_typevars(x) &&
        (has_universal_typevar(x, e) || has_existential_typevar(x, e));
}

// A (closed) type value bound only through equality (`Type{X}`) positions is
// only known up to `==` (#61323); record it as a pinned (lb == ub) typevar
// marker. A BOUND_EQ channel still marks it defined (constrained) for every
// `==`-equal call. Returns NULL for other values: free-typevar values keep the
// legacy plain binding (#61242), egality-certain values stay unwrapped.
static jl_value_t *eq_pinned_envout_marker(jl_unionall_t *u, jl_varbinding_t *vb, jl_value_t *lb,
                                           jl_value_t **new_tvar JL_REQUIRE_ROOTED_SLOT,
                                           int constrained) JL_CANSAFEPOINT
{
    if (jl_is_type(lb) && lb != jl_bottom_type && vb->lb_certainty < BOUND_EGAL &&
        !jl_has_free_typevars(lb)) {
        *new_tvar = (jl_value_t*)jl_new_typevar(u->name, lb, lb);
        return wrap_tvar_env(*new_tvar, constrained || vb->lb_certainty == BOUND_EQ);
    }
    return NULL;
}

// `lb` is the (widened) lower bound, `vlb`/`vub` the materialized bounds and
// `var` the binding's variable
static jl_value_t *subtype_unionall_envout_value(jl_value_t *t, jl_unionall_t *u, jl_stenv_t *e,
                                                 jl_varbinding_t *vb, jl_value_t *lb,
                                                 jl_value_t *vlb, jl_value_t *vub, jl_tvar_t *var,
                                                 jl_value_t **new_tvar JL_REQUIRE_ROOTED_SLOT,
                                                 int constrained) JL_CANSAFEPOINT
{
    if (vb->intvalued && lb == (jl_value_t*)jl_any_type)
        return (jl_value_t*)jl_wrap_vararg(NULL, NULL, 0, 0); // special token result that represents N::Int in the envout
    if (!vb->pinned && lb != jl_bottom_type) {
        if (vb->occurs_inv && (vb->tainted_inner || has_env_typevar(lb, e)))
            return wrap_tvar_env(lb, constrained && !has_existential_typevar(lb, e));
        if (is_leaf_bound(lb) && !has_env_typevar(lb, e)) {
            jl_value_t *marker = eq_pinned_envout_marker(u, vb, lb, new_tvar, constrained);
            if (marker)
                return marker;
            return lb;
        }
        if (constrained && !jl_has_free_typevars(t) && !jl_has_free_typevars(lb) &&
            (jl_is_concrete_type(t) ||
             (jl_is_datatype(t) && ((jl_datatype_t*)t)->isdispatchtuple))) {
            // If the LHS is concrete, e.g. Type{Tuple{Ref}} vs Type{Tuple{S}} where {S<:T}, we'd like to still
            // choose the least solution like below, so that our `constrained` logic below is correct.
            // Also accept dispatchtuples, which cover singleton-like LHSes such as
            // `Tuple{typeof(f), Type{X}}` where the Type{} parameter pins to one runtime value.
            // Refuse when `lb` references universally-quantified vars from the
            // current subtype environment: exposing it directly would leak sibling
            // `where`-bound typevars (e.g. `where {S, T>:S}` would expose `S`).
            return lb;
        }
        if (jl_is_typevar(lb)) {
            // The path below would produce `T_new <: T`. This is redundant for bounds purposes,
            // although it could affect diagonality in downstream uses. However, it is problematic
            // to introduce a new tvar for safety here, because intersection can blow up on that
            // pattern.
            return wrap_tvar_env(lb, constrained);
        }
        *new_tvar = (jl_value_t*)jl_new_typevar(u->name, jl_bottom_type, lb);
        return wrap_tvar_env(*new_tvar, constrained);
    }
    if (lb == vub || lb != jl_bottom_type) {
        // TODO (lb != jl_bottom_type): for now return the least solution, which is what
        // method parameters expect.
        if (vb->tainted_inner || has_env_typevar(lb, e))
            return wrap_tvar_env(lb, constrained && !has_existential_typevar(lb, e));
        jl_value_t *marker = eq_pinned_envout_marker(u, vb, lb, new_tvar, constrained);
        if (marker)
            return marker;
        return lb;
    }
    if (lb == u->lb && vub == u->ub && !*new_tvar)
        // the opened variable carries exactly the binder's name and bounds
        return wrap_tvar_env((jl_value_t*)var, constrained);
    if (!*new_tvar) {
        *new_tvar = (jl_value_t*)jl_new_typevar(u->name, vlb, vub);
        return wrap_tvar_env(*new_tvar, constrained);
    }
    return wrap_tvar_env(*new_tvar, constrained);
}

// widen the `Type{X}` entries of a lower bound for the diagonal-concreteness
// check (each under its own chain); the list is shared where nothing changes
static jl_lterm_t *widen_lterms(jl_stenv_t *e, jl_lterm_t *l, int existential) JL_CANSAFEPOINT
{
    if (l == NULL)
        return NULL;
    jl_lterm_t *rest = widen_lterms(e, l->next, existential);
    jl_value_t *w = widen_Type_if_concrete(l->t, e, NULL, l->frame, existential);
    if (w == l->t && rest == l->next)
        return l;
    if (w != l->t)
        stenv_root(e, w);
    return lterm_cons(e, w, l->frame, rest);
}

// the diagonal rule's leafness of a bound: `Union{}` (no entry) and a single
// leaf entry are leaves; several (distinct) entries form a union
static int lterm_is_leaf(jl_lterm_t *l) JL_NOTSAFEPOINT
{
    if (l == NULL)
        return 1;
    if (l->next != NULL)
        return 0;
    return is_leaf_bound(l->t);
}

static int subtype_unionall(jl_value_t *t, jl_unionall_t *u, jl_stenv_t *e, int8_t R, jl_param_pos_t param) JL_CANSAFEPOINT
{
    jl_value_t *new_tvar = NULL;
    // the binding starts fully lazy: the bounds hold the binder's raw
    // declared bounds (located under the enclosing chain), and no
    // bookkeeping variable exists. Both materialize on first use -- a
    // reference stored past the binding's pop, or the envout -- and most
    // bindings are never used (see `binding_var`).
    jl_varbinding_t *vb = stenv_push_binding(e, u, R, NULL);
    // the body is walked natively: occurrences stay de Bruijn references and
    // resolve positionally through the side's frame chain
    jl_value_t *body = u->body;
    JL_GC_PUSH2(&u, &new_tvar);
    int body_occurs_inv = vb->body_occurs_inv;
    stenv_enter_binding(e, vb, R);
    int ans;
    if (R) {
        e->envidx++;
        ans = subtype(t, body, e, param);
        e->envidx--;
    }
    else {
        // ∀ path: a variable with a trivial lower bound, a union upper bound,
        // and only covariant occurrences in the body ranges over each arm of
        // its upper bound independently, i.e. the UnionAll distributes over
        // the arms:
        //   (Tuple{T,T} where T<:Union{A,B}) ==
        //       Union{Tuple{T,T} where T<:A, Tuple{T,T} where T<:B}
        // (diagonality, if any, is preserved: each value of the variable is
        // concrete and therefore lies entirely within a single arm).
        // Split the bound here by registering one ordinary left-union decision
        // per Union node, so that the enclosing ∀∃ loop enumerates all arms.
        if (!e->intersection && vb->lbs == NULL && vb->ubs != NULL && vb->ubs->next == NULL &&
            jl_is_uniontype(vb->ubs->t) && !body_occurs_inv && tvarref_occurs_covariant_only(body, 1, 1)) {
            jl_value_t *arm = pick_union_element(vb->ubs->t, e, 0);
            // the arm is a subterm of the declared bound, at the same position
            vb->ubs = lterm_cons(e, arm, vb->frame_prev, NULL);
        }
        ans = subtype(body, t, e, param);
    }

    // handle the "diagonal dispatch" rule, which says that a type var occurring more
    // than once, and only in covariant position, is constrained to concrete types. E.g.
    //  ( Tuple{Int, Int}    <: Tuple{T, T} where T) but
    // !( Tuple{Int, String} <: Tuple{T, T} where T)
    // Then check concreteness by checking that the lower bound is not an abstract type.
    int diagonal = cov_count(vb) > 1 && !vb->body_occurs_inv;
    // Widen Type{x} to typeof(x) for ordinary argument-slot occurrences and
    // diagonal constraints, but not invariant matches. This is only a local
    // view for checks and envout; keep `vb->lbs` structurally precise.
    int widen_lb = !vb->occurs_inv && (diagonal || (vb->occurs_cov == 1 && vb->cov_diag == 0));
    jl_lterm_t *widened_lbs = (ans && widen_lb) ? widen_lterms(e, vb->lbs, e->intersection) : vb->lbs;
    if (ans && (vb->concrete || (diagonal && is_leaf_binder(vb)))) {
        jl_lterm_t *concrete_lbs = diagonal ? widened_lbs : vb->lbs;
        jl_lterm_t *l = vb->lbs;
        if (vb->concrete && !diagonal && (vb->ubs == NULL || !lterm_is_leaf(vb->ubs))) {
            // a non-diagonal var can only be a subtype of a diagonal var if its
            // upper bound is concrete.
            ans = 0;
        }
        else if (l != NULL && l->next == NULL && jl_is_typevar(l->t)) {
            jl_varbinding_t *vlb = lookup(e, (jl_tvar_t*)l->t);
            if (vlb)
                vlb->concrete = 1;
        }
        else if (l != NULL && l->next == NULL && jl_is_tvarref(l->t)) {
            // a bound that is a reference to an enclosing binder: the
            // reference is already the positional pointer
            jl_varbinding_t *vlb = frame_lookup(l->frame, jl_tvarref_depth(l->t));
            if (vlb && vlb->live)
                vlb->concrete = 1;
        }
        else if (!lterm_is_leaf(concrete_lbs)) {
            ans = 0;
        }
    }
    stenv_leave_binding(e, vb, R);

    if (!ans) {
        JL_GC_POP();
        return 0;
    }

    int pinned = binding_pinned(e, vb);

    // It is possible for bounds of outer existential vars to refer to universally qualified
    // inner vars. In this case, we should treat this variable as universally qualified over
    // the bounds at this point in future subtype queries. However, we need to do some work
    // to keep track that this situation happened to distinguish it from the case where
    // we have a free typevar in the input.
    // A reference to this binding in an enclosing bound keeps denoting it:
    // the binding stays addressable, with its bounds frozen, and
    // re-expresses as a variable carrying them where a type is needed
    // (`binding_ref_value`).
    int referenced = 0;
    for (jl_varbinding_t *btemp = vb->prev; btemp; btemp = btemp->prev) {
        if (!btemp->existential)
            continue;
        if (!binding_refs_binding(btemp, vb))
            continue;
        referenced = 1;
        if (btemp->depth0 != vb->depth0) {
            // If we've passed through an invariant constructor, the bounds of the outer var can never
            // be satisfied. Consider (ignoring normalization) Ref{T where T} <: Ref{S} where S. This ends
            // up as T<:S<:T. Since `T` is universally qualified over its bounds, this would require `S` to
            // take the full range. However, the `∃` qualifier needs a single value, so unless `T` is similarly
            // constrained, this is unsatisfiable.
            if (!pinned) {
                JL_GC_POP();
                return 0;
            }
        }
        btemp->tainted_inner = 1;
    }
    if (referenced)
        binding_mark_referenced(vb);

    // fill variable values into `envout` up to `envsz`
    if (R && ans && e->envidx < e->envsz) {
        // the envout consumes the materialized view; the binding is popped
        jl_value_t *vlb = NULL, *vub = NULL, *lb = NULL;
        JL_GC_PUSH3(&vlb, &vub, &lb);
        vlb = binding_lb(e, vb);
        // An internal `Intersect` meet node (see #61917) is exact for subtyping but
        // must not appear in a result type or static parameter (it is not a real
        // type). Over-approximate it now, before `vub` is used to build any result typevar.
        vub = pinned ? vlb : widen_intersect(binding_ub(e, vb));
        stenv_root(e, vub);
        lb = widened_lbs == vb->lbs ? vlb : lterm_type(e, widened_lbs, 0);
        if (vb->pinned && has_existential_typevar(vlb, e) && !has_env_typevar(vub, e))
            lb = vub;
        jl_tvar_t *var = binding_var(e, vb);
        // If this variable was resolved to something concrete, just use that value for the
        // substitution below; a referenced binding re-expresses as its final variable
        if (pinned)
            new_tvar = vlb;
        else if (referenced)
            new_tvar = binding_ref_value(e, vb);
        // A var bound only through another variable's declared bounds (BOUND_PROXY)
        // need not be pinned by every call: matching `Type{<:Tuple{Vararg{E}}}`
        // against `Type{S} where S<:NInt` reaches `E` through `S`'s bound, but the
        // `S = Tuple{}` member leaves `E` unbound. So its retained covariant
        // occurrence count must not mark it defined. A fixed prefix on the left,
        // however, is present in every concrete member even when the tuple tail
        // length is free, so a statically constraining right-side element at that
        // position records `lb_required` while matching that tuple element.
        int eff_constrained = (vb->occurs_inv ||
            (cov_count(vb) && u->lb == jl_bottom_type &&
             (vb->lb_certainty > BOUND_PROXY || vb->lb_required)));
        jl_value_t *val = subtype_unionall_envout_value(t, u, e, vb, lb, vlb, vub, var, &new_tvar,
                                                        eff_constrained);
        assert(val != NULL);
        jl_value_t *oldval = e->envout[e->envidx];
        // if we try to assign different variable values (due to checking
        // multiple union members), consider the value unknown. Use AND
        // semantics on the `constrained` flag across iterations: the var is
        // constrained only if every iteration (i.e., every LHS union branch)
        // pinned it.
        if (oldval && !jl_egal(oldval, val)) {
            // Distinct spellings may still pin the same value: a plain value in
            // one branch, a pinned uncertainty marker (with its own fresh
            // typevar) in another. When both branches pin egal values, keep
            // the weaker (`==`-pinned marker) spelling rather than degrading
            // the variable to unbound.
            jl_value_t *oldrep = jl_sparam_defined_value(oldval);
            jl_value_t *newrep = jl_sparam_defined_value(val);
            if (oldrep != NULL && newrep != NULL && jl_egal(oldrep, newrep)) {
                if (!jl_is_svec(oldval))
                    e->envout[e->envidx] = val;
                // else keep oldval, which is already the marker spelling
            }
            else {
                int old_iter_constrained;
                if (jl_is_svec(oldval) && jl_svec_len((jl_svec_t*)oldval) == 2)
                    old_iter_constrained = jl_svecref(oldval, 1) == jl_true;
                else
                    old_iter_constrained = 1; // oldval is a concrete value: iter pinned var
                int new_iter_constrained;
                if (jl_is_svec(val) && jl_svec_len((jl_svec_t*)val) == 2)
                    new_iter_constrained = jl_svecref(val, 1) == jl_true;
                else
                    new_iter_constrained = 1;
                e->envout[e->envidx] = wrap_tvar_env((jl_value_t*)var,
                                                     old_iter_constrained && new_iter_constrained);
            }
        }
        else
            e->envout[e->envidx] = val;
        // TODO: substitute the value (if any) of this variable into previous envout entries
        JL_GC_POP();
    }

    JL_GC_POP();
    return ans;
}

// check n <: (length of vararg type v)
static int check_vararg_length(jl_value_t *v, ssize_t n, jl_stenv_t *e, jl_varbinding_t *frame) JL_CANSAFEPOINT
{
    jl_value_t *N = jl_unwrap_vararg_num(v);
    // only do the check if N is free in the tuple type's last parameter
    if (N) {
        jl_value_t *nn = jl_box_long(n);
        JL_GC_PUSH1(&nn);
        e->invdepth++;
        int ans;
        if (jl_is_tvarref(N) && frame_lookup(frame, jl_tvarref_depth(N)) != NULL) {
            // the length reference is walked raw, under the vararg's own
            // side, in both check directions
            jl_varbinding_t *saveL = e->Lframe, *saveR = e->Rframe;
            e->Rframe = frame;
            ans = subtype(nn, N, e, PARAM_INVARIANT);
            e->Rframe = saveR;
            if (ans) {
                e->Lframe = frame;
                ans = subtype(N, nn, e, PARAM_NONE);
                e->Lframe = saveL;
            }
        }
        else {
            ans = subtype(nn, N, e, PARAM_INVARIANT) && subtype(N, nn, e, PARAM_NONE);
        }
        e->invdepth--;
        JL_GC_POP();
        if (!ans)
            return 0;
    }
    return 1;
}

static int forall_exists_equal(jl_value_t *x, jl_value_t *y, jl_stenv_t *e) JL_CANSAFEPOINT;

static int subtype_tuple_varargs(
    jl_vararg_t *vtx, jl_vararg_t *vty,
    jl_value_t *lastx, jl_value_t *lasty,
    size_t vx, size_t vy, size_t x_reps,
    jl_stenv_t *e, jl_param_pos_t param) JL_CANSAFEPOINT
{
    jl_value_t *xp0 = jl_unwrap_vararg(vtx); jl_value_t *xp1 = jl_unwrap_vararg_num(vtx);
    jl_value_t *yp0 = jl_unwrap_vararg(vty); jl_value_t *yp1 = jl_unwrap_vararg_num(vty);
    // a length reference is dispatched on its binding and stays raw for the
    // walk (cf. the classification at the top of `subtype`); a reference to
    // a popped binding is an inner variable
    jl_varbinding_t *xlv = NULL, *ylv = NULL;
    if (xp1 && jl_is_tvarref(xp1)) {
        xlv = frame_lookup(e->Lframe, jl_tvarref_depth(xp1));
        if (xlv != NULL && xlv->popped) {
            xp1 = binding_ref_value(e, xlv);
            xlv = NULL;
        }
    }
    if (yp1 && jl_is_tvarref(yp1)) {
        ylv = frame_lookup(e->Rframe, jl_tvarref_depth(yp1));
        if (ylv != NULL && ylv->popped) {
            yp1 = binding_ref_value(e, ylv);
            ylv = NULL;
        }
    }
    if (xp1 && jl_is_typevar(xp1))
        xlv = lookup(e, (jl_tvar_t*)xp1);
    if (yp1 && jl_is_typevar(yp1))
        ylv = lookup(e, (jl_tvar_t*)yp1);

    int8_t max_offsetx = xlv ? xlv->max_offset : 0;
    int8_t max_offsety = ylv ? ylv->max_offset : 0;

    jl_value_t *xl = xlv ? lterm_long(xlv->lbs) : xp1;
    jl_value_t *yl = ylv ? lterm_long(ylv->lbs) : yp1;

    if (!xp1) {
        // Unconstrained on the left, constrained on the right
        if (yl && jl_is_long(yl))
            return 0;
    }
    else {
        if (xl && jl_is_long(xl)) {
            if (jl_unbox_long(xl) + 1 == vx) {
                // LHS is exhausted. We're a subtype if the RHS is either
                // exhausted as well or unbounded (in which case we need to
                // set it to 0).
                if (yl) {
                    if (jl_is_long(yl)) {
                        return jl_unbox_long(yl) + 1 == vy;
                    }
                } else {
                    // We can skip the subtype check, but we still
                    // need to make sure to constrain the length of y
                    // to 0.
                    goto constrain_length;
                }
            }
        }
    }
    {
        int x_same = vx > 1 || (lastx && obviously_egal(xp0, lastx));
        int y_same = vy > 1 || (lasty && obviously_egal(yp0, lasty));
        // keep track of number of consecutive identical subtyping
        x_reps = y_same && x_same ? x_reps + 1 : 1;
        if (x_reps > 2) {
            // an identical type on the left doesn't need to be compared to the same
            // element type on the right more than twice.
        }
        else if (x_same && e->Runions.depth == 0 && y_same &&
            !has_free_or_dangling_typevars(xp0) && !has_free_or_dangling_typevars(yp0)) {
            // fast path for repeated elements (cf. subtype_tuple_tail)
        }
        else if ((e->Runions.depth == 0 ? !has_free_or_dangling_typevars(xp0) : jl_is_concrete_type(xp0)) &&
                 !has_free_or_dangling_typevars(yp0)) {
            // fast path for separable sub-formulas (cf. subtype_tuple_tail)
            if (!jl_subtype(xp0, yp0))
                return 0;
        }
        else {
            // in Vararg{T1} <: Vararg{T2}, need to check subtype twice to
            // simulate the possibility of multiple arguments, which is needed
            // to implement the diagonal rule correctly.
            if (!subtype(xp0, yp0, e, param)) return 0;
            if (x_reps < 2 && !subtype(xp0, yp0, e, PARAM_COVARIANT)) return 0;
        }
    }
constrain_length:
    if (!yp1) {
        return 1;
    }
    if (!xp1) {
        jl_value_t *yl = ylv ? lterm_long(ylv->lbs) : yp1;
        if (yl && jl_is_long(yl)) {
            // The length of the x tuple is unconstrained, but the
            // length of the y tuple is now fixed (this could have happened
            // as a result of the subtype call above).
            return 0;
        }

        if (ylv) {
            if (ylv->depth0 != e->invdepth ||
                ylv->lbs != NULL ||
                ylv->ubs != NULL)
                return 0;
            ylv->intvalued = 1;
        }
        // set lb to Any. Since `intvalued` is set, we'll interpret that
        // appropriately.
        e->invdepth++;
        int ans = subtype((jl_value_t*)jl_any_type, yp1, e, PARAM_INVARIANT);
        if (ylv && !ylv->intersected)
            ylv->max_offset = max_offsety;
        e->invdepth--;
        return ans;
    }

    // Vararg{T,N} <: Vararg{T2,N2}; equate N and N2
    e->invdepth++;
    JL_GC_PUSH2(&xp1, &yp1);
    int ans;
    jl_varbinding_t *bxp1 = xlv;
    jl_varbinding_t *byp1 = ylv;
    if (bxp1) {
        if (bxp1->intvalued == 0)
            bxp1->intvalued = 1;
        jl_value_t *l = lterm_long(bxp1->lbs);
        if (l != NULL)
            xp1 = l;
    }
    if (byp1) {
        if (byp1->intvalued == 0)
            byp1->intvalued = 1;
        jl_value_t *l = lterm_long(byp1->lbs);
        if (l != NULL)
            yp1 = l;
    }
    if (jl_is_long(xp1) && jl_is_long(yp1))
        ans = jl_unbox_long(xp1) - vx == jl_unbox_long(yp1) - vy;
    else {
        if (jl_is_long(xp1) && vx != vy) {
            xp1 = jl_box_long(jl_unbox_long(xp1) + vy - vx);
            vx = vy;
        }
        if (jl_is_long(yp1) && vy != vx) {
            yp1 = jl_box_long(jl_unbox_long(yp1) + vx - vy);
            vy = vx;
        }
        assert(e->Loffset == 0);
        e->Loffset = vx - vy;
        ans = forall_exists_equal(xp1, yp1, e);
        assert(e->Loffset == vx - vy);
        e->Loffset = 0;
    }
    JL_GC_POP();
    if (ylv && !ylv->intersected)
        ylv->max_offset = max_offsety;
    if (xlv && !xlv->intersected)
        xlv->max_offset = max_offsetx;
    e->invdepth--;
    return ans;
}

static int subtype_tuple_tail(jl_datatype_t *xd, jl_datatype_t *yd, int8_t R, jl_stenv_t *e, jl_param_pos_t param) JL_CANSAFEPOINT
{
    size_t lx = jl_nparams(xd);
    size_t ly = jl_nparams(yd);
    size_t i = 0, j = 0, vx = 0, vy = 0, x_reps = 1;
    jl_value_t *lastx = NULL, *lasty = NULL;
    jl_value_t *xi = NULL, *yi = NULL;

    for (;;) {
        if (i < lx) {
            xi = jl_tparam(xd, i);
            if (i == lx-1 && (vx || jl_is_vararg(xi))) {
                vx += 1;
            }
        }

        if (j < ly) {
            yi = jl_tparam(yd, j);
            if (j == ly-1 && (vy || jl_is_vararg(yi))) {
                vy += 1;
            }
        }

        if (i >= lx)
            break;

        int all_varargs = vx && vy;
        if (!all_varargs && vy == 1) {
            if (jl_unwrap_vararg(yi) == (jl_value_t*)jl_any_type) {
                // Tuple{...} <: Tuple{..., Vararg{Any, _}}
                // fast path all the type checks away
                xi = jl_tparam(xd, lx-1);
                if (jl_is_vararg(xi)) {
                    all_varargs = 1;
                    // count up to lx-2 rather than lx-1.
                    vy += lx - i - 1;
                    vx = 1;
                } else {
                    break;
                }
            }
        }

        if (all_varargs) {
            // Tuple{..., Vararg{xi, _}} <: Tuple{..., Vararg{yi, _}}
            return subtype_tuple_varargs(
                (jl_vararg_t*)xi,
                (jl_vararg_t*)yi,
                lastx, lasty,
                vx, vy, x_reps, e, param);
        }

        if (j >= ly)
            return !!vx;

        xi = vx ? jl_unwrap_vararg(xi) : xi;
        yi = vy ? jl_unwrap_vararg(yi) : yi;
        int required_lhs_element = !vx && param == PARAM_COVARIANT;
        int x_same = vx > 1 || (lastx && obviously_egal(xi, lastx));
        int y_same = vy > 1 || (lasty && obviously_egal(yi, lasty));
        // keep track of number of consecutive identical subtyping
        x_reps = y_same && x_same ? x_reps + 1 : 1;
        if (x_reps > 2) {
            // an identical type on the left doesn't need to be compared to the same
            // element type on the right more than twice.
        }
        else if (x_same && e->Runions.depth == 0 &&
            ((y_same && !has_free_or_dangling_typevars(xi) && !has_free_or_dangling_typevars(yi)) ||
             (yi == lastx && !jl_has_dangling_tvarrefs(yi) && !vx && vy && jl_is_concrete_type(xi)))) {
            // fast path for repeated elements (a bound-variable reference
            // still records occurrences, so it cannot be skipped)
        }
        else if ((e->Runions.depth == 0 ? !has_free_or_dangling_typevars(xi) : jl_is_concrete_type(xi)) &&
                 !has_free_or_dangling_typevars(yi)) {
            // fast path for separable sub-formulas (a bound-variable
            // reference disqualifies it: a fresh query would lose the frames)
            int sub = jl_subtype(xi, yi);
            if (!sub)
                return 0;
        }
        else {
            int sub = subtype(xi, yi, e, param);
            if (!sub)
                return 0;
        }
        if (required_lhs_element)
            mark_required_tuple_element(e, yi);
        lastx = xi; lasty = yi;
        if (i < lx-1 || !vx)
            i++;
        if (j < ly-1 || !vy)
            j++;
    }

    if (vy && !vx && lx+1 >= ly) {
        // in Tuple{...,tn} <: Tuple{...,Vararg{T,N}}, check (lx+1-ly) <: N
        if (!check_vararg_length(yi, lx+1-ly, e, e->Rframe))
            return 0;
    }
    assert((lx + vx == ly + vy) || (vy && (lx >= (vx ? ly : (ly-1)))));
    return 1;
}

static int subtype_tuple(jl_datatype_t *xd, jl_datatype_t *yd, jl_stenv_t *e, jl_param_pos_t param) JL_CANSAFEPOINT
{
    // Check tuple compatibility based on tuple length only (fastpath)
    size_t lx = jl_nparams(xd);
    size_t ly = jl_nparams(yd);

    if (lx == 0 && ly == 0)
        return 1;

    jl_vararg_kind_t vvx = JL_VARARG_NONE;
    jl_vararg_kind_t vvy = JL_VARARG_NONE;
    jl_varbinding_t *xbb = NULL;
    jl_value_t *xva = NULL, *yva = NULL;
    if (lx > 0) {
        xva = jl_tparam(xd, lx-1);
        vvx = jl_vararg_kind(xva);
        if (vvx == JL_VARARG_BOUND) {
            jl_value_t *xn = jl_unwrap_vararg_num(xva);
            if (jl_is_tvarref(xn))
                // the length is a bound-variable reference of the left term
                xbb = frame_lookup(e->Lframe, jl_tvarref_depth(xn));
            else
                xbb = lookup(e, (jl_tvar_t *)xn);
        }
    }
    if (ly > 0) {
        yva = jl_tparam(yd, ly-1);
        vvy = jl_vararg_kind(yva);
    }
    if (xbb != NULL && !xbb->live)
        xbb = NULL;
    jl_value_t *xbb_len = xbb ? lterm_long(xbb->lbs) : NULL;
    if (vvx != JL_VARARG_NONE && vvx != JL_VARARG_INT && xbb_len == NULL) {
        if (vvx == JL_VARARG_UNBOUND || (xbb && !xbb->existential)) {
            // Unbounded on the LHS, bounded on the RHS
            if (vvy == JL_VARARG_NONE || vvy == JL_VARARG_INT)
                return 0;
            else if (lx < ly) // Unbounded includes N == 0
                return 0;
        }
        else if (vvy == JL_VARARG_NONE && !check_vararg_length(xva, ly+1-lx, e, e->Lframe)) {
            return 0;
        }
    }
    else {
        size_t nx = lx;
        if (vvx == JL_VARARG_INT)
            nx += jl_vararg_length(xva) - 1;
        else if (xbb_len != NULL)
            nx += jl_unbox_long(xbb_len) - 1;
        else
            assert(vvx == JL_VARARG_NONE);
        size_t ny = ly;
        if (vvy == JL_VARARG_INT)
            ny += jl_vararg_length(yva) - 1;
        else if (vvy != JL_VARARG_NONE)
            ny -= 1;
        if (vvy == JL_VARARG_NONE || vvy == JL_VARARG_INT) {
            if (nx != ny)
                return 0;
        }
        else {
            if (ny > nx)
                return 0;
        }
    }

    if (param == PARAM_NONE) param = PARAM_COVARIANT;
    int ans = subtype_tuple_tail(xd, yd, 0, e, param);
    return ans;
}

static int try_subtype_by_bounds(jl_value_t *a, jl_value_t *b, jl_stenv_t *e) JL_CANSAFEPOINT;

// --- kind cover of a `Type{T}` (`TypeEq`) parameter -------------------------
//
// `Type{T}` denotes every type `U` with `U == T` (mutual subtyping), lifted
// into the type domain, so `Type{T} <: S` iff `isa(U, S)` for every such `U`
// (#33136, #62141). Distinct representatives of the same `==`-class generally
// carry different type tags: `Tuple{S} where S<:Int == Tuple{Int}` is a
// `UnionAll`, `Union{Tuple{Int},Tuple{String}} == Tuple{Union{Int,String}}` is
// a `Union`, and bound-pinned spellings like `Vector{S} where Int<:S<:Int`
// exist for every class (whether a particular constructor normalizes them away
// is incidental, so we treat `UnionAll` representatives as present in every
// class). `typeeq_kind_mask` computes a superset of the type tags of the
// members of `T`'s `==`-class; `Type{T} <: y` then requires every kind in the
// mask to be a subtype of `y`.
//
// The one exemption is `Union{}` itself: the runtime globally normalizes every
// spelling of the empty bottom type to the unique bottom object (`Tuple` types
// with `Union{}` parameters collapse, `T where T<:S` returns its bound, union
// components absorb), so `{U : U == Union{}}` is `{Union{}}` exactly and
// `Type{Union{}} == TypeofBottom` remains true. Consequently `TypeofBottom`
// and `Type{Union{}}` are two spellings of one class, and each one's tag
// appears in the mask of the other.
//
// The mask over-approximates: a spurious kind only makes `Type{T} <: y`
// (soundly) fail more often. For exotic spellings this can reject subtypings
// that hold for the canonical spelling of the same class; subtyping was
// already incomplete for such spellings.

#define TYPEEQ_KIND_BOTTOM   1  // TypeofBottom
#define TYPEEQ_KIND_DATATYPE 2
#define TYPEEQ_KIND_UNION    4
#define TYPEEQ_KIND_UNIONALL 8
#define TYPEEQ_KIND_TYPEEQ   16
#define TYPEEQ_KIND_TYPEEGAL 32
#define TYPEEQ_KIND_ALL      63

// count the occurrences of the bound-variable reference with index `idx`
// (adjusted as binders are crossed); union branches count as alternatives
static int count_ref_occurs(jl_value_t *t, size_t idx) JL_NOTSAFEPOINT
{
    if (jl_is_tvarref(t))
        return jl_tvarref_depth(t) == idx ? 1 : 0;
    if (jl_is_uniontype(t)) {
        int a = count_ref_occurs(((jl_uniontype_t*)t)->a, idx);
        int b = count_ref_occurs(((jl_uniontype_t*)t)->b, idx);
        return a > b ? a : b;
    }
    if (jl_is_unionall(t))
        return count_ref_occurs(((jl_unionall_t*)t)->body, idx + 1);
    if (jl_is_vararg(t)) {
        jl_vararg_t *vm = (jl_vararg_t*)t;
        if (vm->T)
            return count_ref_occurs(vm->T, idx) + (vm->N ? count_ref_occurs(vm->N, idx) : 0);
        return 0;
    }
    if (jl_is_some_Type(t))
        return count_ref_occurs(jl_some_Type_T(t), idx);
    if (jl_is_datatype(t)) {
        int c = 0;
        for (size_t i = 0; i < jl_nparams(t); i++)
            c += count_ref_occurs(jl_tparam(t, i), idx);
        return c;
    }
    return 0;
}

// typevars bound by unionalls inside the parameter itself, as opposed to free
// typevars from the outer environment (which range over instantiations)
// may a value of covariant position `p` be `==` to a union of multiple
// incomparable components, so that a `Tuple` around it splits into a `Union`
// (`Tuple{Union{Int,String}} == Union{Tuple{Int},Tuple{String}}`)?
static int typeeq_splittable(jl_value_t *p, typeeq_varctx_t *env) JL_NOTSAFEPOINT
{
    if (jl_is_uniontype(p))
        return 1;
    if (jl_is_vararg(p))
        // `Tuple{Vararg{T}} == Union{Tuple{}, Tuple{T, Vararg{T}}}` (a length
        // split); fixed-length varargs were expanded at construction time
        return 1;
    if (jl_is_tvarref(p)) {
        typeeq_varctx_t *ctx = typeeq_lookup_ref(env, jl_tvarref_depth(p));
        if (ctx == NULL)
            return 1; // dangling reference: one fixed but unknown value
        if (ctx->pinned)
            return typeeq_splittable(ctx->lb, ctx->prev);
        jl_value_t *ub = ctx->ub;
        // a var can range over (or instantiate to) union values unless its
        // upper bound is concrete
        if (jl_has_free_typevars(ub) || jl_has_dangling_tvarrefs(ub) || !jl_is_concrete_type(ub))
            return 1;
        return typeeq_splittable(ub, ctx->prev);
    }
    if (jl_is_typevar(p)) {
        // a free environment var ranges over its instantiations
        jl_value_t *ub = ((jl_tvar_t*)p)->ub;
        if (jl_has_free_typevars(ub) || jl_has_dangling_tvarrefs(ub) || !jl_is_concrete_type(ub))
            return 1;
        return typeeq_splittable(ub, NULL);
    }
    if (jl_is_unionall(p)) {
        jl_unionall_t *u = (jl_unionall_t*)p;
        typeeq_varctx_t ctx = { u->lb, u->ub, u->lb == u->ub, env };
        return typeeq_splittable(u->body, &ctx);
    }
    if (jl_is_datatype(p)) {
        if (((jl_datatype_t*)p)->name == jl_tuple_typename) {
            size_t i, np = jl_nparams(p);
            for (i = 0; i < np; i++) {
                if (typeeq_splittable(jl_tparam(p, i), env))
                    return 1;
            }
        }
        return 0;
    }
    // `TypeEq`/`TypeEgal` wrappers are invariant in their parameter; remaining
    // values (numbers, symbols) are not types
    return 0;
}

// may some instantiation of `p` be the empty bottom type? Only typevars from
// the outer environment can cause this: a `Union{}` instantiation of a
// parameter-local existential var contributes an empty piece to a fixed type
// rather than changing which type it is.
static int typeeq_bottomable(jl_value_t *p, typeeq_varctx_t *env) JL_NOTSAFEPOINT
{
    if (p == jl_bottom_type)
        return 1;
    if (jl_is_tvarref(p)) {
        typeeq_varctx_t *ctx = typeeq_lookup_ref(env, jl_tvarref_depth(p));
        if (ctx != NULL)
            return ctx->pinned ? typeeq_bottomable(ctx->lb, ctx->prev) : 0;
        return 1; // dangling reference: conservatively may be `Union{}`
    }
    if (jl_is_typevar(p)) {
        jl_value_t *lb = ((jl_tvar_t*)p)->lb;
        return lb == jl_bottom_type || jl_is_typevar(lb) || jl_is_tvarref(lb);
    }
    if (jl_is_uniontype(p)) {
        return typeeq_bottomable(((jl_uniontype_t*)p)->a, env) &&
               typeeq_bottomable(((jl_uniontype_t*)p)->b, env);
    }
    if (jl_is_unionall(p)) {
        jl_unionall_t *u = (jl_unionall_t*)p;
        typeeq_varctx_t ctx = { u->lb, u->ub, u->lb == u->ub, env };
        return typeeq_bottomable(u->body, &ctx);
    }
    if (jl_is_datatype(p) && ((jl_datatype_t*)p)->name == jl_tuple_typename) {
        size_t i, np = jl_nparams(p);
        for (i = 0; i < np; i++) {
            jl_value_t *pi = jl_tparam(p, i);
            // a `Union{}` instantiation of a `Vararg` element admits length 0
            // instead of collapsing the tuple
            if (!jl_is_vararg(pi) && typeeq_bottomable(pi, env))
                return 1;
        }
    }
    return 0;
}

// are all components of `t` (a union) datatypes or `Type` wrappers? A typevar
// or unionall component absorbs differently per instantiation, making the
// class of the union too unstable to bound (e.g. `Union{Int,String,T}` is
// `==` to the DataType `Any` when `T == Any`).
static int typeeq_union_arms_stable(jl_value_t *t) JL_NOTSAFEPOINT
{
    if (jl_is_uniontype(t))
        return typeeq_union_arms_stable(((jl_uniontype_t*)t)->a) &&
               typeeq_union_arms_stable(((jl_uniontype_t*)t)->b);
    return jl_is_datatype(t) || jl_is_typeeq(t) || jl_is_typeegal(t);
}

// do all components of `t` (a union) unwrap to `Tuple` datatypes? Unions of
// tuples may refold into a single `Tuple` DataType representative
// (`Union{Tuple{Int},Tuple{String}} == Tuple{Union{Int,String}}`).
static int typeeq_all_tuplish(jl_value_t *t) JL_NOTSAFEPOINT
{
    while (jl_is_unionall(t))
        t = ((jl_unionall_t*)t)->body;
    if (jl_is_uniontype(t))
        return typeeq_all_tuplish(((jl_uniontype_t*)t)->a) &&
               typeeq_all_tuplish(((jl_uniontype_t*)t)->b);
    return jl_is_datatype(t) && ((jl_datatype_t*)t)->name == jl_tuple_typename;
}

static int typeeq_kind_mask1(jl_value_t *t, typeeq_varctx_t *env) JL_NOTSAFEPOINT
{
    if (t == jl_bottom_type)
        return TYPEEQ_KIND_BOTTOM;
    if (jl_is_tvarref(t)) {
        typeeq_varctx_t *ctx = typeeq_lookup_ref(env, jl_tvarref_depth(t));
        if (ctx != NULL) {
            if (ctx->pinned)
                return typeeq_kind_mask1(ctx->lb, ctx->prev);
            // a parameter-local `S where lb<:S<:ub` with a bare-var body
            // denotes the class of `ub`
            return typeeq_kind_mask1(ctx->ub, ctx->prev);
        }
        return TYPEEQ_KIND_ALL; // dangling reference
    }
    if (jl_is_typevar(t)) {
        // a bare environment var is handled by the typevar rules instead
        return TYPEEQ_KIND_ALL;
    }
    if (jl_is_typeeq(t)) {
        // members are `Type{X}` objects; if the parameter can instantiate to
        // `Union{}`, the class also contains the `TypeofBottom` DataType
        // (`Type{Union{}} == TypeofBottom`, the bottom object being unique).
        // A typevar parameter additionally admits the nominal `AnyType`
        // DataType: `(Type{S} where S) == AnyType` when `S` spans all types.
        int mask = TYPEEQ_KIND_TYPEEQ;
        jl_value_t *tp = jl_typeeq_T(t);
        if (jl_is_typevar(tp) || jl_is_tvarref(tp) || typeeq_bottomable(tp, env))
            mask |= TYPEEQ_KIND_DATATYPE;
        return mask;
    }
    if (jl_is_typeegal(t))
        return TYPEEQ_KIND_TYPEEGAL;
    if (jl_is_datatype(t)) {
        jl_datatype_t *dt = (jl_datatype_t*)t;
        if (dt == jl_typeofbottom_type)
            // `TypeofBottom == Type{Union{}}`, whose object is `TypeEq`-kinded
            return TYPEEQ_KIND_DATATYPE | TYPEEQ_KIND_TYPEEQ;
        int mask = TYPEEQ_KIND_DATATYPE;
        if (dt->name == jl_tuple_typename) {
            size_t i, np = jl_nparams(t);
            for (i = 0; i < np; i++) {
                jl_value_t *pi = jl_tparam(t, i);
                if (typeeq_splittable(pi, env))
                    mask |= TYPEEQ_KIND_UNION;
                if (!jl_is_vararg(pi) && typeeq_bottomable(pi, env))
                    mask |= TYPEEQ_KIND_BOTTOM;
            }
        }
        return mask;
    }
    if (jl_is_uniontype(t)) {
        jl_uniontype_t *u = (jl_uniontype_t*)t;
        if (!typeeq_union_arms_stable(t))
            return TYPEEQ_KIND_ALL;
        int mask = TYPEEQ_KIND_UNION;
        if (typeeq_all_tuplish(t))
            mask |= TYPEEQ_KIND_DATATYPE;
        // instantiating a free var can collapse the union: a component may
        // become empty or absorb into another (`Union{Ref{T},Ref{Int}}` is the
        // DataType `Ref{Int}` when `T == Int`), leaving the class of any
        // subset of the components (of `Union{}` itself if all become empty)
        int collapse = jl_has_free_typevars(t) || jl_has_dangling_tvarrefs(t);
        int abot = typeeq_bottomable(u->a, env);
        int bbot = typeeq_bottomable(u->b, env);
        if (abot || collapse)
            mask |= typeeq_kind_mask1(u->b, env);
        if (bbot || collapse)
            mask |= typeeq_kind_mask1(u->a, env);
        if (abot && bbot)
            mask |= TYPEEQ_KIND_BOTTOM;
        return mask;
    }
    if (jl_is_unionall(t)) {
        jl_unionall_t *u = (jl_unionall_t*)t;
        if (u->lb == u->ub) {
            // a pinned var is equivalent to substituting its bound
            typeeq_varctx_t ctx = { u->lb, u->ub, 1, env };
            return typeeq_kind_mask1(u->body, &ctx);
        }
        // Try the "wrapper-like" shape: every non-pinned var has `lb === Union{}`
        // and closed bounds and occurs exactly once, directly as a parameter of
        // a nominal (non-Tuple) core -- e.g. `Vector` = `Array{T,1} where T`.
        // No `DataType` or `Union` can be `==` to such a type: a mutual-subtype
        // candidate would have to match that invariant parameter for every
        // instantiation of a var that ranges over at least two distinct
        // classes. Its class then consists of `UnionAll` representatives only.
        jl_value_t *core = t;
        int wrapperlike = 1;
        size_t nbinders = 0;
        while (jl_is_unionall(core)) {
            jl_unionall_t *w = (jl_unionall_t*)core;
            if (w->lb != w->ub &&
                (w->lb != jl_bottom_type || jl_has_free_typevars(w->ub) ||
                 jl_has_dangling_tvarrefs(w->ub) || count_ref_occurs(w->body, 1) != 1)) {
                wrapperlike = 0;
                break;
            }
            nbinders++;
            core = w->body;
        }
        if (wrapperlike && jl_is_datatype(core) &&
            ((jl_datatype_t*)core)->name != jl_tuple_typename &&
            (jl_datatype_t*)core != jl_typeofbottom_type) {
            // each non-pinned var must appear directly as a parameter of the
            // core; a binder `depth` levels above the core is referenced there
            // by index `depth`
            jl_value_t *w = t;
            size_t i, np = jl_nparams(core);
            size_t depth = nbinders;
            while (jl_is_unionall(w)) {
                jl_unionall_t *uw = (jl_unionall_t*)w;
                if (uw->lb != uw->ub) {
                    for (i = 0; i < np; i++) {
                        jl_value_t *pi = jl_tparam(core, i);
                        if (jl_is_tvarref(pi) && jl_tvarref_depth(pi) == depth)
                            break;
                    }
                    if (i == np) {
                        wrapperlike = 0;
                        break;
                    }
                }
                depth--;
                w = uw->body;
            }
            if (wrapperlike)
                return 0; // `UnionAll` representatives only
        }
        // conservative: analyze the body with the var ranging over its bounds
        typeeq_varctx_t ctx = { u->lb, u->ub, 0, env };
        return typeeq_kind_mask1(u->body, &ctx);
    }
    return TYPEEQ_KIND_ALL; // non-type (defensive)
}

// superset of the type tags (kinds) of the members of the `==`-class of `t`,
// the parameter of a `Type{t}`; free typevars of `t` range over their
// instantiations, but `t` itself must not be a bare typevar
static int typeeq_kind_mask(jl_value_t *t) JL_NOTSAFEPOINT
{
    if (t == jl_bottom_type)
        return TYPEEQ_KIND_BOTTOM; // the bottom object is unique (see above)
    return TYPEEQ_KIND_UNIONALL | typeeq_kind_mask1(t, NULL);
}

// is the kind datatype `k` (whose supertype chain is `k <: AnyType <: Any` and
// which has no parameters or subtypes) a subtype of the datatype `y`?
static int typeeq_kind_le(jl_datatype_t *k, jl_value_t *y) JL_NOTSAFEPOINT
{
    if (!jl_is_datatype(y))
        return 0;
    jl_datatype_t *yd = (jl_datatype_t*)y;
    while (k != jl_any_type) {
        if (k == yd)
            return 1;
        k = k->super;
    }
    return yd == jl_any_type;
}

static const int typeeq_kind_bits[6] = {
    TYPEEQ_KIND_BOTTOM, TYPEEQ_KIND_DATATYPE, TYPEEQ_KIND_UNION,
    TYPEEQ_KIND_UNIONALL, TYPEEQ_KIND_TYPEEQ, TYPEEQ_KIND_TYPEEGAL
};

static jl_datatype_t *typeeq_kind_datatype(int bit) JL_NOTSAFEPOINT
{
    switch (bit) {
    case TYPEEQ_KIND_BOTTOM:   return jl_typeofbottom_type;
    case TYPEEQ_KIND_DATATYPE: return jl_datatype_type;
    case TYPEEQ_KIND_UNION:    return jl_uniontype_type;
    case TYPEEQ_KIND_UNIONALL: return jl_unionall_type;
    case TYPEEQ_KIND_TYPEEQ:   return jl_typeeq_type;
    default:                   assert(bit == TYPEEQ_KIND_TYPEEGAL);
                               return jl_typeegal_type;
    }
}

// Resolve a `Type{T}` typevar parameter whose bounds pin it to a single
// `==`-class: `T where lb<:T<:ub` with `lb == ub` ranges over exactly the
// class of `lb`, so `Type{T} where DataType<:T<:DataType` answers like
// `Type{DataType}` (the pair from #33136). Bounds equal only up to `==` (not
// `===`) pin just the same. A genuinely non-collapsing range instead admits
// `Union` values strictly between the bounds, so nothing sharper than the
// conservative all-kinds answer is sound for it.
static jl_value_t *typeeq_unpin_tvar(jl_value_t *tp0 JL_PROPAGATES_ROOT) JL_CANSAFEPOINT
{
    while (jl_is_typevar(tp0)) {
        jl_value_t *lb = ((jl_tvar_t*)tp0)->lb;
        jl_value_t *ub = ((jl_tvar_t*)tp0)->ub;
        if (lb != ub) {
            if (jl_has_free_typevars(lb) || jl_has_free_typevars(ub) ||
                !jl_types_equal(lb, ub))
                break;
        }
        tp0 = lb;
    }
    return tp0;
}

// `typeeq_unpin_tvar(resolve_tvarref(t, frame, e))` without materializing the
// variable of a binding `t` refers to: a binding's variable carries the
// binder's declared bounds, so whether it is pinned can be read off the
// binder. Returns NULL for a (non-pinned) variable, where the materialized
// form would have been a typevar. Bounds that refer to enclosing binders take
// the materializing path.
static jl_value_t *typeeq_unpin_ref(jl_value_t *t JL_PROPAGATES_ROOT, jl_varbinding_t *frame, jl_stenv_t *e) JL_CANSAFEPOINT
{
    if (jl_is_tvarref(t)) {
        jl_varbinding_t *b = frame_lookup(frame, jl_tvarref_depth(t));
        if (b != NULL) {
            jl_value_t *lb = b->u->lb, *ub = b->u->ub;
            if (!jl_has_free_or_dangling_typevars(lb) && !jl_has_free_or_dangling_typevars(ub)) {
                if (lb == ub || jl_types_equal(lb, ub))
                    return lb; // pinned: a closed bound unpins no further
                return NULL;
            }
        }
    }
    t = typeeq_unpin_tvar(resolve_tvarref(t, frame, e));
    return jl_is_typevar(t) ? NULL : t;
}

// does `t` refer to a bound or free variable (as seen through `frame`)? and if
// so, is it unbounded? (without materializing a binding's variable)
static int typeeq_param_var(jl_value_t *t, jl_varbinding_t *frame, int *unbounded) JL_NOTSAFEPOINT
{
    jl_value_t *lb, *ub;
    if (jl_is_tvarref(t)) {
        jl_varbinding_t *b = frame_lookup(frame, jl_tvarref_depth(t));
        if (b == NULL)
            return 0;
        lb = b->u->lb; ub = b->u->ub;
    }
    else if (jl_is_typevar(t)) {
        lb = ((jl_tvar_t*)t)->lb; ub = ((jl_tvar_t*)t)->ub;
    }
    else {
        return 0;
    }
    if (unbounded)
        *unbounded = lb == jl_bottom_type && ub == (jl_value_t*)jl_any_type;
    return 1;
}

// do all kinds in `mask` lie in the datatype `y`? (the `Type{T} <: y` rule)
static int typeeq_mask_le(int mask, jl_value_t *y) JL_NOTSAFEPOINT
{
    int i;
    for (i = 0; i < 6; i++) {
        if ((mask & typeeq_kind_bits[i]) &&
            !typeeq_kind_le(typeeq_kind_datatype(typeeq_kind_bits[i]), y))
            return 0;
    }
    return 1;
}

// does some kind in `mask` lie in `y`? (`Type{T} ∩ y` nonemptiness)
static int typeeq_mask_meets(int mask, jl_value_t *y) JL_NOTSAFEPOINT
{
    int i;
    for (i = 0; i < 6; i++) {
        if ((mask & typeeq_kind_bits[i]) &&
            typeeq_kind_le(typeeq_kind_datatype(typeeq_kind_bits[i]), y))
            return 1;
    }
    return 0;
}

// collect the components of the union `y` that have no free typevars;
// components beyond `cap` are dropped (making the caller's check conservative)
static void typeeq_collect_closed_components(jl_value_t *y, jl_value_t **out, size_t *n, size_t cap) JL_NOTSAFEPOINT
{
    if (jl_is_uniontype(y)) {
        typeeq_collect_closed_components(((jl_uniontype_t*)y)->a, out, n, cap);
        typeeq_collect_closed_components(((jl_uniontype_t*)y)->b, out, n, cap);
        return;
    }
    if (*n < cap && !jl_has_free_typevars(y))
        out[(*n)++] = y;
}

// `Type{tp0} <: y` for a union `y`: check the kind cover against the union of
// the closed components of `y`. The members of a `Type{T}` straddle several
// kinds, so a subtyping like `Type{Int} <: Union{DataType,UnionAll}` can hold
// without holding for any single branch. Components with free typevars are
// left out: the cover holding over the closed components alone proves the
// subtyping without constraining any variable, just as the per-branch
// decomposition proves it by choosing a var-free branch.
static int typeeq_subtype_kind_cover(jl_value_t *tp0, jl_value_t *y) JL_CANSAFEPOINT
{
    int mask = typeeq_kind_mask(tp0);
    jl_value_t *kinds[6];
    size_t n = 0;
    int i;
    for (i = 0; i < 6; i++) {
        if (mask & typeeq_kind_bits[i])
            kinds[n++] = (jl_value_t*)typeeq_kind_datatype(typeeq_kind_bits[i]);
    }
    jl_value_t *closed[32];
    size_t nc = 0;
    typeeq_collect_closed_components(y, closed, &nc, sizeof(closed) / sizeof(closed[0]));
    if (nc == 0)
        return 0;
    jl_value_t *cover = NULL, *target = NULL;
    JL_GC_PUSH2(&cover, &target);
    cover = jl_type_union(kinds, n);
    target = jl_type_union(closed, nc);
    int ans = jl_subtype(cover, target);
    JL_GC_POP();
    return ans;
}

// quick scan: does the union `y` contain a kind (or `AnyType`) component, so
// that the kind-cover check above can possibly succeed?
static int union_has_kind_component(jl_value_t *y) JL_NOTSAFEPOINT
{
    if (jl_is_uniontype(y))
        return union_has_kind_component(((jl_uniontype_t*)y)->a) ||
               union_has_kind_component(((jl_uniontype_t*)y)->b);
    return is_kind_or_anytype(y);
}

// Does `t` contain a typevar with a binding in the environment `e` (i.e. one
// introduced by a `where` enclosing this query)? A `Type{T}` parameter whose
// free typevars are all dangling instead is an internal dispatch key for one
// specific (open) type object -- `jl_inst_arg_tuple_type` keys type values
// this way when free typevars preclude a `TypeEgal` slot -- and is matched
// like that value: pinned to its type tag, `==`-compared as a `Type` (the same
// hybrid `typekeyvalue_eq` uses). Method signatures always bind their vars, so
// they still get the sound `==`-class semantics.
static int typeeq_vars_bound_in_env(jl_value_t *t, jl_stenv_t *e, typeeq_varctx_t *wenv, jl_varbinding_t *frame, size_t nintro) JL_NOTSAFEPOINT
{
    if (jl_is_tvarref(t)) {
        size_t d = jl_tvarref_depth(t);
        if (d <= nintro)
            return 0; // bound inside `t` itself
        if (typeeq_lookup_ref(wenv, d - nintro) != NULL)
            return 1;
        // a reference is only bound by the binder chain of the side the term
        // came from (`frame`); a binder on the other side of the query must
        // not capture it (it belongs to an unrelated chain)
        return frame_lookup(frame, d - nintro) != NULL;
    }
    if (jl_is_typevar(t)) {
        int inner = 0;
        return lookup_binding(e, (jl_tvar_t*)t, &inner) != NULL || inner;
    }
    if (jl_is_uniontype(t))
        return typeeq_vars_bound_in_env(((jl_uniontype_t*)t)->a, e, wenv, frame, nintro) ||
               typeeq_vars_bound_in_env(((jl_uniontype_t*)t)->b, e, wenv, frame, nintro);
    if (jl_is_unionall(t)) {
        jl_unionall_t *u = (jl_unionall_t*)t;
        return typeeq_vars_bound_in_env(u->lb, e, wenv, frame, nintro) ||
               typeeq_vars_bound_in_env(u->ub, e, wenv, frame, nintro) ||
               typeeq_vars_bound_in_env(u->body, e, wenv, frame, nintro + 1);
    }
    if (jl_is_vararg(t)) {
        jl_vararg_t *vm = (jl_vararg_t*)t;
        return (vm->T && typeeq_vars_bound_in_env(vm->T, e, wenv, frame, nintro)) ||
               (vm->N && typeeq_vars_bound_in_env(vm->N, e, wenv, frame, nintro));
    }
    if (jl_is_some_Type(t))
        return typeeq_vars_bound_in_env(jl_some_Type_T(t), e, wenv, frame, nintro);
    if (jl_is_datatype(t)) {
        if (!((jl_datatype_t*)t)->hasfreetypevars && !((jl_datatype_t*)t)->hasescapingrefs)
            return 0;
        size_t i, np = jl_nparams(t);
        for (i = 0; i < np; i++) {
            if (typeeq_vars_bound_in_env(jl_tparam(t, i), e, wenv, frame, nintro))
                return 1;
        }
    }
    return 0;
}

// is `t` an internal single-object dispatch key: an open type whose free
// typevars are all dangling in this query (see `typeeq_vars_bound_in_env`)?
static int typeeq_is_dangling_key(jl_value_t *t, jl_stenv_t *e, typeeq_varctx_t *wenv, jl_varbinding_t *frame) JL_NOTSAFEPOINT
{
    return (jl_has_free_typevars(t) || jl_has_dangling_tvarrefs(t)) &&
           !typeeq_vars_bound_in_env(t, e, wenv, frame, 0);
}

static int subtype(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, jl_param_pos_t param)
{
    // classify bound-variable references against their side's binder chain.
    // An already-materialized variable takes the ordinary rules; otherwise
    // the reference dispatches on its binding directly (`xrb`/`yrb`) and no
    // variable is created for it here.
    // A reference to a popped binding (reached through a bound stored past
    // its pop) is an inner variable: it re-expresses as a variable carrying
    // the binding's final bounds and takes the ordinary rules.
    jl_varbinding_t *xrb = NULL, *yrb = NULL;
    if (jl_is_tvarref(x)) {
        xrb = frame_lookup(e->Lframe, jl_tvarref_depth(x));
        if (xrb != NULL && (xrb->popped || (e->intersection && xrb->var != NULL))) {
            x = binding_ref_value(e, xrb);
            xrb = NULL;
        }
    }
    if (jl_is_tvarref(y)) {
        yrb = frame_lookup(e->Rframe, jl_tvarref_depth(y));
        if (yrb != NULL && (yrb->popped || (e->intersection && yrb->var != NULL))) {
            y = binding_ref_value(e, yrb);
            yrb = NULL;
        }
    }
    if ((jl_is_tvarref(x) && xrb == NULL) || (jl_is_tvarref(y) && yrb == NULL)) {
        // truly dangling (detached subterms passed in by the user);
        // tolerated like free typevars: only comparable structurally
        if (jl_is_tvarref(x) && xrb == NULL && jl_is_tvarref(y) && yrb == NULL)
            return jl_tvarref_depth(x) == jl_tvarref_depth(y);
        return (jl_is_tvarref(x) && xrb == NULL) ? y == (jl_value_t*)jl_any_type : x == jl_bottom_type;
    }
    if (jl_is_uniontype(x)) {
        if (obviously_egal(x, y) && !jl_has_dangling_tvarrefs(x))
            return 1;
        if (e->Runions.depth == 0 && (jl_is_typevar(y) || yrb != NULL) && !has_free_or_dangling_typevars(x)) {
            // Similar to fast path for repeated elements: if there have been no outer
            // unions on the right, and the right side is a typevar, then we can handle the
            // typevar first before picking a union element, under the theory that it may
            // be easy to match or reject this whole union in comparing and setting the lb
            // and ub of the variable binding, without needing to examine each element.
            // However, if x contains any free typevars, then each element with a free
            // typevar must be handled separately from the union of all elements without
            // free typevars, since the typevars presence might lead to those elements
            // getting eliminated (omit_bad_union) or degenerate (Union{Ptr{T}, Ptr}) or
            // combined (Union{T, S} where {T, S <: T}).
            jl_tvar_t *yvar = yrb != NULL ? yrb->var : (jl_tvar_t *)y; // may be NULL (unmaterialized)
            int yinner = 0;
            jl_varbinding_t *yb = yrb != NULL ? yrb : lookup_binding(e, yvar, &yinner);
            while (e->intersection && yb != NULL) {
                jl_value_t *yb_lb = binding_lb(e, yb);
                if (!(yb_lb == binding_ub(e, yb) && jl_is_typevar(yb_lb)))
                    break;
                yvar = (jl_tvar_t *)yb_lb;
                yb = lookup_binding(e, yvar, &yinner);
            }
            // Note: `x <: ∃y` performs a local ∀-∃ check between `x` and `yb->ub`.
            // We need to ensure that there's no ∃ typevar as otherwise that check
            // might cause false alarm due to the accumulated env change.
            if (yb == NULL || !yb->existential || !lterm_has_existential(e, yb->ubs))
                return subtype_var(yvar, x, e, 1, param, yb, yinner);
        }
        x = pick_union_element(x, e, 0);
        if (jl_is_tvarref(x)) {
            // an arm may itself be a reference: re-classify it
            xrb = frame_lookup(e->Lframe, jl_tvarref_depth(x));
            if (xrb == NULL)
                return y == (jl_value_t*)jl_any_type; // detached: structural rules
            if (xrb->popped || (e->intersection && xrb->var != NULL)) {
                x = binding_ref_value(e, xrb);
                xrb = NULL;
            }
        }
    }
    if (jl_is_uniontype(y)) {
        if (obviously_in_union(y, x) && !jl_has_dangling_tvarrefs(x))
            return 1;
        // The members of a `Type{T}` straddle several kinds, so e.g.
        // `Type{Int} <: Union{DataType,UnionAll}` holds without holding for
        // either branch alone; check the kind cover against the whole union
        // first. This is a closed side query (the cover and `y` have no free
        // vars), so no bindings are recorded and free vars of `T` are not
        // descended into -- matching the kind leaf rule below; a `Type{T'}`
        // branch with `T' == T` -- which covers all of `Type{T}` or nothing --
        // is still found by the per-branch decomposition below.
        if (jl_is_typeeq(x) && union_has_kind_component(y)) {
            jl_value_t *xp0 = typeeq_unpin_tvar(resolve_tvarref(jl_typeeq_T(x), e->Lframe, e));
            // a dangling-var dispatch key has a single tag, found by the
            // per-branch decomposition instead
            if (!jl_is_typevar(xp0) && !typeeq_is_dangling_key(xp0, e, NULL, e->Lframe) &&
                typeeq_subtype_kind_cover(xp0, y))
                return 1;
        }
        if (jl_is_unionall(x))
            return subtype_unionall(y, (jl_unionall_t*)x, e, 0, param);
        int ui = 1;
        if (jl_is_typevar(x) || xrb != NULL) {
            // The `convert(Type{T},T)` pattern, where T is a Union, required changing priority
            // of unions and vars: if matching `typevar <: union`, first try to match the whole
            // union against the variable before trying to take it apart to see if there are any
            // variables lurking inside.
            // note: for forall var, there's no need to split y if it has no free typevars.
            jl_varbinding_t *xx = xrb != NULL ? xrb : lookup(e, (jl_tvar_t *)x);
            // a reference bound in the walk behaves like a free typevar here
            ui = ((xx && xx->existential) || has_free_or_dangling_typevars(y)) && pick_union_decision(e, 1);
        }
        if (ui == 1) {
            y = pick_union_element(y, e, 1);
            if (jl_is_tvarref(y)) {
                // an arm may itself be a reference: re-classify it
                yrb = frame_lookup(e->Rframe, jl_tvarref_depth(y));
                if (yrb == NULL)
                    return x == jl_bottom_type; // detached: structural rules
                if (yrb->popped || (e->intersection && yrb->var != NULL)) {
                    y = binding_ref_value(e, yrb);
                    yrb = NULL;
                }
            }
        }
    }
    // An internal `Intersect` meet node is only ever produced as an existential
    // upper bound, so it can appear on the right (`x <: a ∩ b`) but never on the
    // left. Handling the left case (`a ∩ b <: y`) precisely needs the
    // intersection machinery, so for now we assert it does not arise.
    assert(!jl_is_intersecttype(x));
    if (jl_is_intersecttype(y)) {
        // `x <: a ∩ b`  iff  `x <: a` and `x <: b` (dual to `Union` on the left).
        jl_intersecttype_t *iy = (jl_intersecttype_t*)y;
        return subtype(x, iy->a, e, param) && subtype(x, iy->b, e, param);
    }
    // N.B. on the remaining variable lookups below: bound variables still
    // reach walked positions through accumulated (updated) bound content,
    // which is stored in variable form on purpose — a stored raw term would
    // carry a frame chain that pops (deepest-first) while the bound lives on,
    // so the variable is the pop-safe spelling; the pop boundary that
    // re-owns leaking inner variables is the outer-existential rename in
    // `subtype_unionall`. Still-raw declared bounds, by contrast, are walked
    // in place (see `binding_ub_walkable`) and inject nothing.
    if (jl_is_typevar(x) || xrb != NULL) {
        if (jl_is_typevar(y) || yrb != NULL) {
            // a variable-variable relation reasons with the bindings: no
            // variable is materialized here. A binding's variable is only
            // needed where a reference to it is stored past its frame, which
            // `var_lt`/`var_gt` do on success (an unmaterialized variable
            // occurs nowhere, so it is not identical to anything yet).
            if (xrb != NULL && xrb == yrb)
                return 1;
            jl_tvar_t *xv = xrb != NULL ? xrb->var : (jl_tvar_t*)x;
            jl_tvar_t *yv = yrb != NULL ? yrb->var : (jl_tvar_t*)y;
            if (xv == yv && (xrb == NULL || xv != NULL)) return 1; // (only a binding's variable can be missing)
            int xinner = 0, yinner = 0;
            jl_varbinding_t *xx = xrb != NULL ? xrb : lookup_binding(e, (jl_tvar_t*)x, &xinner);
            jl_varbinding_t *yy = yrb != NULL ? yrb : lookup_binding(e, (jl_tvar_t*)y, &yinner);
            int xfree_singleton = xx == NULL && !xinner;
            int yfree_singleton = yy == NULL && !yinner;
            if (xfree_singleton && yfree_singleton)
                return 0;
            if (e->intersection) {
                jl_value_t *xub = xx ? binding_ub(e, xx) : xinner ? ((jl_tvar_t*)x)->ub : x;
                jl_value_t *ylb = yy ? binding_lb(e, yy) : yinner ? ((jl_tvar_t*)y)->lb : y;
                jl_value_t *xlb = xx ? binding_lb(e, xx) : xinner ? ((jl_tvar_t*)x)->lb : x;
                jl_value_t *yub = yy ? binding_ub(e, yy) : yinner ? ((jl_tvar_t*)y)->ub : y;
                // find equivalence class for typevars during intersection
                if (xub != x && ((jl_is_typevar(xub) && xub == xlb) ||
                                  !jl_is_type(xub)))
                    return subtype(xub, y, e, param);
                if (yub != y && ((jl_is_typevar(yub) && yub == ylb) ||
                                  !jl_is_type(yub)))
                    return subtype(x, yub, e, param);
            }
            int xr = xx && xx->existential;  // treat free variables as "forall" (left)
            int yr = yy && yy->existential;
            if (xr) {
                if (yr) {
                    // TODO: Why is this sound?
                    if (e->intersection && try_subtype_by_bounds(binding_lb(e, xx), binding_ub(e, yy), e))
                        return 1;

                    // Both variables are existential. We need to annotate the constraint
                    // on the inner-most variable, so check which one that is.
                    if (binding_outside(e, xx, yy)) {
                        record_var_occurrence(xx, e, param);
                        return var_gt(yv, x, e, param, yy, yinner);
                    }
                }
                if (yy) record_var_occurrence(yy, e, param);
                return var_lt(xv, y, e, param, xx, xinner);
            }
            else if (yr) {
                if (xx) {
                    record_var_occurrence(xx, e, param);
                    // This encodes the following:
                    //
                    // When we have `∀A ∃B, A <: B`, then the existence of
                    // `B` depends on every particular choice of `A`
                    // (in particular each choice of `A` may have a different
                    // choice of `B`. We encode this as `B->lb = A`.
                    //
                    // However, `∃B ∀A` is different: This requires a single `B`
                    // that works for all `A`. We encode this as `B->lb = A->ub`
                    // (note that A's ub cannot change during the course of the
                    // algorithm). Semantically, at each invariant depth, we push
                    // all universal quantifiers before all existential qualifiers,
                    // so asking which of these cases we're in is equivalent to
                    // asking whether `B`'s depth is greater than `A`'s depth.
                    if (yy && yy->depth0 < xx->depth0)
                        return var_gt(yv, binding_ub(e, xx), e, param, yy, yinner);
                }
                return var_gt(yv, x, e, param, yy, yinner);
            }
            // check ∀x,y . x<:y
            // the bounds of left-side variables never change, and can only lead
            // to other left-side variables, so using || here is safe.
            if (xfree_singleton)
                return singleton_typevar_subtype((jl_tvar_t*)x, y);
            if (yfree_singleton)
                return subtype_singleton_typevar(xx ? binding_ub(e, xx) : ((jl_tvar_t*)x)->ub, (jl_tvar_t*)y);
            // skip <: check during intersection if we meet 2 inner vars,
            // just like var_lt/var_gt.
            if (xinner && yinner && e->intersection)
                return 1;
            // (the bounds are walked in place where they are still raw)
            if (xx ? subtype_binding_ub(e, xx, y, param) : subtype(xinner ? ((jl_tvar_t*)x)->ub : x, y, e, param))
                return 1;
            return yy ? subtype_binding_lb(e, x, yy, param) : subtype(x, yinner ? ((jl_tvar_t*)y)->lb : y, e, param);
        }
        int xinner = 0;
        jl_varbinding_t *xb = xrb != NULL ? xrb : lookup_binding(e, (jl_tvar_t*)x, &xinner);
        if (jl_is_unionall(y)) {
            // n.b. the raw bound is compared here: a bound that IS `y` is the
            // identical (interned) object in either form
            jl_value_t *xub = xb == NULL ? (xinner ? ((jl_tvar_t *)x)->ub : x)
                                         : (xb->ubs != NULL && xb->ubs->next == NULL ? xb->ubs->t : NULL);
            if ((xb == NULL ? !xinner || !e->intersection : !xb->existential) && xub != y) {
                // We'd better unwrap `y::UnionAll` eagerly if `x` isa ∀-var.
                // This makes sure the following cases work correct:
                // 1) `∀T <: Union{∃S, SomeType{P}} where {P}`: `S == Any` ==> `S >: T`
                // 2) `∀T <: Union{∀T, SomeType{P}} where {P}`:
                // note: if xub == y we'd better try `subtype_var` as `subtype_left_var`
                // hit `==` based fast path.
                return subtype_unionall(x, (jl_unionall_t*)y, e, 1, param);
            }
        }
        return subtype_var(xrb != NULL ? xrb->var : (jl_tvar_t*)x, y, e, 0, param, xb, xinner);
    }
    if (jl_is_typevar(y) || yrb != NULL) {
        int yinner = 0;
        jl_varbinding_t *yb = yrb != NULL ? yrb : lookup_binding(e, (jl_tvar_t*)y, &yinner);
        return subtype_var(yrb != NULL ? yrb->var : (jl_tvar_t*)y, x, e, 1, param, yb, yinner);
    }
    if (y == (jl_value_t*)jl_any_type && !jl_has_free_typevars(x))
        return 1;
    if (x == jl_bottom_type && !jl_has_free_typevars(y))
        return 1;
    jl_value_t *ux = jl_unwrap_unionall(x);
    jl_value_t *uy = jl_unwrap_unionall(y);
    if ((x != ux || y != uy) && y != (jl_value_t*)jl_any_type && jl_is_datatype(ux) && jl_is_datatype(uy) &&
        !jl_is_typeeq(ux)) {
        assert(ux);
        if (uy == (jl_value_t*)jl_any_type)
            return 1;
        jl_datatype_t *xd = (jl_datatype_t*)ux, *yd = (jl_datatype_t*)uy;
        while (xd != NULL && xd != jl_any_type && xd->name != yd->name) {
            xd = jl_datatype_compute_super(xd);
        }
        if (xd == jl_any_type)
            return 0;
    }
    // handle forall ("left") vars first
    if (jl_is_unionall(x)) {
        // (identity is only meaningful for frame-free terms: raw ones from
        // different chains can denote different binders)
        if (x == y && !(e->envidx < e->envsz) && !jl_has_dangling_tvarrefs(x))
            return 1;
        return subtype_unionall(y, (jl_unionall_t*)x, e, 0, param);
    }
    // fast path: every member of a closed `Type{T}`/`TypeEgal{T}` is a type,
    // so bare `Type` (`Type{T} where T`) on the right always holds -- without
    // opening `Type`'s var. Only when no envout slot remains to be filled
    // (`Type` as the top-level RHS of `jl_subtype_env` must still bind its
    // var into the environment) and `x` is closed (nothing to record).
    if (y == (jl_value_t*)jl_type_type && !(e->envidx < e->envsz) &&
        jl_is_some_Type(x) && !jl_has_free_typevars((jl_value_t*)x))
        return 1;
    if (jl_is_unionall(y))
        return subtype_unionall(x, (jl_unionall_t*)y, e, 1, param);
    if (jl_is_typeegal(x)) {
        // the sole (closed) instance `A` lies in `y` iff `A === B` for `TypeEgal{B}`,
        // iff `A == B` for `Type{B}` (egal implies equal), and otherwise iff the
        // singleton `typeof(A)` is a subtype of `y`
        jl_value_t *A = jl_typeegal_T(x);
        if (jl_is_typeegal(y))
            return jl_egal(A, jl_typeegal_T(y));
        if (jl_is_typeeq(y)) {
            // `A` is egal-known, but `Type{B}` constrains `B` only up to `==`:
            // bindings keep their certainty (the descent of the one object `A`
            // is deterministic), while `A`'s spelling is only `==`-authoritative
            int saved_spell = e->spell_channel;
            if (e->spell_channel > BOUND_EQ)
                e->spell_channel = BOUND_EQ;
            e->invdepth++;
            int ans = forall_exists_equal(A, jl_typeeq_T(y), e);
            e->invdepth--;
            e->spell_channel = saved_spell;
            return ans;
        }
        return subtype(jl_typeof(A), y, e, param);
    }
    if (jl_is_typeegal(y)) {
        // nothing else (besides `Union{}`, already handled) is a subset of `{B}`
        return 0;
    }
    if (x == (jl_value_t*)jl_typeofbottom_type && jl_is_typeeq(y)) {
        jl_value_t *tp0 = jl_typeeq_T(y);
        e->invdepth++;
        int ans = forall_exists_equal(jl_bottom_type, tp0, e);
        e->invdepth--;
        return ans;
    }
    if (jl_is_typeeq(x) && jl_is_typeeq(y)) {
        // Bounds recorded under a covariant (argument-slot) x-side equality wrapper
        // only pin variables up to `==` (an egality-pinned slot arrives as `TypeEgal`
        // above instead). An invariant occurrence is a parameter of a type tag, whose
        // identity pins its parameters exactly, and inside a bounds-consistency
        // check on a closed x-term (`value_descent`) the x-side is a concrete
        // type object rather than an argument-slot spelling, so descent keeps
        // the incoming certainty.
        int saved_channel = e->bound_channel;
        int saved_spell = e->spell_channel;
        if (param != PARAM_INVARIANT && !e->value_descent && e->bound_channel > BOUND_EQ)
            e->bound_channel = BOUND_EQ;
        if (param != PARAM_INVARIANT && !e->value_descent && e->spell_channel > BOUND_EQ)
            e->spell_channel = BOUND_EQ;
        e->invdepth++;
        int ans = forall_exists_equal(jl_typeeq_T(x), jl_typeeq_T(y), e);
        e->invdepth--;
        e->bound_channel = saved_channel;
        e->spell_channel = saved_spell;
        return ans;
    }
    if (jl_is_typeeq(x) && jl_is_datatype(y)) {
        // fast path: every member of a `Type{T}` is a type, so `AnyType` (and
        // `Any`, reachable here when `T` has free vars) need no classification
        if (y == (jl_value_t*)jl_anytype_type || y == (jl_value_t*)jl_any_type)
            return 1;
        jl_value_t *tp0 = typeeq_unpin_ref(jl_typeeq_T(x), e->Lframe, e);
        if (tp0 != NULL) {
            // a dispatch key for one specific open type object (dangling free
            // typevars, see `typeeq_vars_bound_in_env`) is pinned to that
            // object's type tag
            if (typeeq_is_dangling_key(tp0, e, NULL, e->Lframe))
                return subtype(jl_typeof(tp0), y, e, param);
            // `Type{T} <: y` iff `isa(U, y)` for every `U == T`, i.e. iff every
            // possible type tag of such members lies in `y` (#33136, #62141).
            // For example `Type{Int} <: Union{DataType,UnionAll}` but
            // `Type{Int} <: DataType` does not hold: spellings like
            // `Union{Int,S} where Int<:S<:Int` are `==` to `Int` but are not
            // `DataType`s.
            return typeeq_mask_le(typeeq_kind_mask(tp0), y);
        }
        // `TypeEq(T)` for a free typevar `T` is the kind of all types matching
        // `T`'s bounds; every such instance is itself a type, i.e. a `Kind`. So
        // `Type{T} <: y` reduces to `Kind <: y` (in particular `Type === Kind`).
        return subtype((jl_value_t*)jl_anytype_type, y, e, param);
    }
    if (jl_is_datatype(x) && jl_is_typeeq(y) && x != (jl_value_t*)jl_typeofbottom_type) {
        jl_value_t *tp0 = jl_typeeq_T(y);
        if (typeeq_param_var(tp0, e->Rframe, NULL)) {
            // kinds and `AnyType` are subtypes of `Type` but of no narrower `Type{T'}`,
            // and no `TypeEq` appears in their supertype chains to derive this from; so
            // answer as for `Type <: Type{T}`, at the depth where `Type{T}` occurs (the
            // depth of `x` doesn't matter: it doesn't contain the variable)
            if (!is_kind_or_anytype(x))
                return 0;
            return subtype((jl_value_t*)jl_type_type, y, e, param);
        }
        // `Type{Type{T}}` with an unbounded `T` contains every `Type{X}` value
        // (among others), so a kind contained in `TypeEq` is a subtype
        int unbounded = 0;
        if (jl_is_typeeq(tp0) && typeeq_param_var(jl_typeeq_T(tp0), e->Rframe, &unbounded) && unbounded)
            return subtype(x, (jl_value_t*)jl_typeeq_type, e, param);
        return 0;
    }
    if (jl_is_datatype(x) && jl_is_datatype(y)) {
        // identical reference-form bodies reached under different binder
        // chains still need the walk: their references resolve to different
        // bindings, whose bounds must be recorded and checked
        if (x == y && !jl_has_dangling_tvarrefs(x)) return 1;
        if (y == (jl_value_t*)jl_any_type) return 1;
        jl_datatype_t *xd = (jl_datatype_t*)x, *yd = (jl_datatype_t*)y;
        while (xd != jl_any_type && xd->name != yd->name) {
            // an instantiation whose supertype was deferred (a fragment of a
            // self-referential definition, or one created while its definition
            // was still in progress) is completed on demand — always through
            // `jl_datatype_compute_super`, whose acquiring fast path also
            // synchronizes with a concurrent lazy publication; only a
            // still-incomplete definition remains an error
            jl_datatype_t *xsuper = jl_datatype_compute_super(xd);
            if (xsuper == NULL) {
                assert(xd->parameters && jl_is_typename(xd->name));
                jl_errorf("circular type parameter constraint in definition of %s", jl_symbol_name(xd->name->name));
            }
            xd = xsuper;
        }
        if (xd == jl_any_type) return 0;
        if (xd->name == jl_tuple_typename)
            return subtype_tuple(xd, yd, e, param);
        size_t i, np = jl_nparams(xd);
        int ans = 1;
        e->invdepth++;
        for (i=0; i < np; i++) {
            jl_value_t *xi = jl_tparam(xd, i), *yi = jl_tparam(yd, i);
            // identical parameters shortcut equality -- except identical
            // reference-form parameters, which resolve through their own
            // sides' binder chains and must still be walked
            if (!((xi == yi && !jl_has_dangling_tvarrefs(xi)) || forall_exists_equal(xi, yi, e))) {
                ans = 0; break;
            }
        }
        e->invdepth--;
        return ans;
    }
    if (jl_is_type(y))
        return x == jl_bottom_type;
    if (jl_is_long(x) && jl_is_long(y))
        return jl_unbox_long(x) == jl_unbox_long(y) + e->Loffset;
    return jl_egal(x, y);
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

static int is_existential_typevar(jl_value_t *x, jl_stenv_t *e)
{
    if (!jl_is_typevar(x))
        return 0;
    jl_varbinding_t *vb = lookup(e, (jl_tvar_t *)x);
    return vb && vb->existential;
}

static int forall_exists_subtype(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, jl_param_pos_t param) JL_CANSAFEPOINT;

static int local_forall_exists_subtype(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, jl_param_pos_t param, int limit_slow)
{
    int16_t oldRmore = e->Runions.more;
    int sub;
    // fast-path for #49857
    if (obviously_in_union(y, x) && !jl_has_dangling_tvarrefs(x) && !jl_has_dangling_tvarrefs(y))
        return 1;
    // a bound-variable reference behaves like the free variable it resolves
    // to: it disqualifies the closed fast path (a fresh query would lose the
    // frames) and carries its binding's existential-ness
    int kindx = !jl_has_free_typevars(x) && !jl_has_dangling_tvarrefs(x);
    int kindy = !jl_has_free_typevars(y) && !jl_has_dangling_tvarrefs(y);
    if (kindx && kindy)
        return jl_subtype(x, y);
    // with the frames in their home orientation, pure subtyping keeps the
    // left operand free of existential material: a universal binding is never
    // flipped to an existential one, and universal bounds never accumulate
    // content. The scan is needed when the frames are reversed (the invariant
    // reverse direction walks a right term in the left slot) or when
    // intersection re-enters terms on the other side.
    int has_exists = (!kindx && (e->intersection || e->frames_flipped) &&
                                (has_existential_typevar(x, e) ||
                                 frame_has_existential_ref(x, e->Lframe, 0))) ||
                     (!kindy && (has_existential_typevar(y, e) ||
                                 frame_has_existential_ref(y, e->Rframe, 0)));
    if (!has_exists) {
        // We can use ∀_∃_subtype safely for ∃ free inputs.
        // This helps to save some bits in union stack.
        jl_saved_unionstate_t oldRunions; push_unionstate(&oldRunions, &e->Runions);
        e->Lunions.used = e->Runions.used = 0;
        e->Lunions.depth = e->Runions.depth = 0;
        e->Lunions.more = e->Runions.more = 0;
        sub = forall_exists_subtype(x, y, e, param);
        pop_unionstate(&e->Runions, &oldRunions);
        return sub;
    }
    if (is_existential_typevar(x, e) != is_existential_typevar(y, e)) {
        e->Lunions.used = 0;
        while (1) {
            e->Lunions.more = 0;
            e->Lunions.depth = 0;
            sub = subtype(x, y, e, param);
            if (!sub || !next_union_state(e, 0))
                break;
        }
        return sub;
    }
    if (limit_slow == -1)
        limit_slow = kindx || kindy;
    jl_savedenv_t se;
    save_env(e, &se, 1);
    int count, limited = 0, ini_count = 0;
    jl_saved_unionstate_t latestLunions = {0, 0, 0, NULL};
    while (1) {
        count = ini_count;
        if (ini_count == 0)
            e->Lunions.used = 0;
        else
            pop_unionstate(&e->Lunions, &latestLunions);
        while (1) {
            e->Lunions.more = 0;
            e->Lunions.depth = 0;
            if (count < 4) count++;
            sub = subtype(x, y, e, param);
            if (limit_slow && count == 4)
                limited = 1;
            if (!sub || !next_union_state(e, 0))
                break;
            if (limited || e->Runions.more == oldRmore) {
                // re-save env and freeze the ∃decision for previous ∀Union
                ini_count = count;
                push_unionstate(&latestLunions, &e->Lunions);
                re_save_env(e, &se, 1);
                e->Runions.more = oldRmore;
            }
        }
        if (sub || e->Runions.more == oldRmore)
            break;
        assert(e->Runions.more > oldRmore);
        next_union_state(e, 1);
        restore_env(e, &se, 1); // also restore Rdepth here
        e->Runions.more = oldRmore;
    }
    if (!sub)
        assert(e->Runions.more == oldRmore);
    else if (e->Runions.more > oldRmore && (limited || env_unchanged(e, &se)))
        // Ignore the rest ∃Union decisions if env is unchanged/limited.
        // As otherwise it might cause combinatorial explosion without making any difference to the result.
        e->Runions.more = oldRmore;
    free_env(&se);
    return sub;
}

// `x` may be a raw fragment of the left term: the subtype checks walk it in
// place through the frames, and its canonical (variable-form) spelling is
// computed only where an identity, a cross-position use, or a stored value
// is needed.
static int equal_var(jl_tvar_t *v, jl_value_t *x, jl_stenv_t *e) JL_CANSAFEPOINT;

// `v` may be NULL when the binding has never been materialized (the callers
// classify a positional reference on its binding directly)
// `local_forall_exists_subtype(x, c)` / `(c, y)` for a located term on the
// right / left, walked under its own chain
static int lfe_located_right(jl_stenv_t *e, jl_value_t *x, jl_lterm_t *c, jl_param_pos_t param, int limit_slow) JL_CANSAFEPOINT
{
    jl_varbinding_t *saveR = e->Rframe;
    e->Rframe = c->frame;
    int sub = local_forall_exists_subtype(x, c->t, e, param, limit_slow);
    e->Rframe = saveR;
    return sub;
}

static int lfe_located_left(jl_stenv_t *e, jl_lterm_t *c, jl_value_t *y, jl_param_pos_t param, int limit_slow) JL_CANSAFEPOINT
{
    jl_varbinding_t *saveL = e->Lframe;
    e->Lframe = c->frame;
    int sub = local_forall_exists_subtype(c->t, y, e, param, limit_slow);
    e->Lframe = saveL;
    return sub;
}

static int equal_var_(jl_tvar_t *v, jl_varbinding_t *vb, int innervar, jl_value_t *x, jl_stenv_t *e) JL_CANSAFEPOINT
{
    assert(e->Loffset == 0);
    // Theoretically bounds change would be merged for union inputs.
    // But intersection is not happy as splitting helps to avoid circular env.
    assert(!e->intersection || !jl_is_uniontype(x));
    assert(v != NULL || vb != NULL);
    if (e->intersection && vb != NULL) {
        jl_value_t *vb_lb = binding_lb(e, vb);
        if (binding_pinned(e, vb) && jl_is_typevar(vb_lb))
            // pinned to a variable
            return equal_var((jl_tvar_t *)vb_lb, x, e);
    }
    record_var_occurrence(vb, e, PARAM_INVARIANT);
    if (vb != NULL && binding_detached(vb)) {
        // see var_lt: a detached fragment's binder supports no bound reasoning
        // (an unmaterialized variable cannot occur in `x`)
        return v != NULL && x == (jl_value_t*)v;
    }
    if (vb == NULL) {
        assert(v != NULL);
        if (innervar && e->intersection)
            return 1;
        if (innervar) {
            if (!local_forall_exists_subtype(x, v->lb, e, PARAM_INVARIANT, !has_free_or_dangling_typevars(x)))
                return 0;
            // `x` enters a right position here; this is a pure check, so
            // rather than re-spelling `x`, give the right side its chain (the
            // left operand is in variable form: it gets no chain, so that any
            // accidental resolution against `x`'s chain traps as detached)
            jl_varbinding_t *saveL = e->Lframe, *saveR = e->Rframe;
            e->Rframe = e->Lframe;
            e->Lframe = NULL;
            int sub = local_forall_exists_subtype(v->ub, x, e, PARAM_NONE, 0);
            e->Lframe = saveL;
            e->Rframe = saveR;
            return sub;
        }
        return x == (jl_value_t*)v;
    }
    assert(vb->live);
    if (!vb->existential) {
        // a universal binding has a single entry per bound (its declared
        // bound, or an arm of it): walk it in place
        int limit = !has_free_or_dangling_typevars(x);
        if (vb->lbs == NULL) {
            if (!local_forall_exists_subtype(x, jl_bottom_type, e, PARAM_INVARIANT, limit))
                return 0;
        }
        else {
            jl_lterm_t *c = vb->lbs->next != NULL ? lterm_pick(vb->lbs, e, 1) : vb->lbs;
            if (!lfe_located_right(e, x, c, PARAM_INVARIANT, limit))
                return 0;
        }
        // `x` enters a right position here; this is a pure check, so the
        // right side gets its chain
        jl_varbinding_t *saveL = e->Lframe, *saveR = e->Rframe;
        e->Rframe = e->Lframe;
        int sub;
        if (vb->ubs == NULL) {
            e->Lframe = NULL;
            sub = local_forall_exists_subtype((jl_value_t*)jl_any_type, x, e, PARAM_NONE, 0);
        }
        else {
            jl_lterm_t *c = vb->ubs->next != NULL ? lterm_pick(vb->ubs, e, 0) : vb->ubs;
            sub = lfe_located_left(e, c, x, PARAM_NONE, 0);
        }
        e->Lframe = saveL;
        e->Rframe = saveR;
        return sub;
    }
    if (x != jl_bottom_type && vb->lb_certainty < e->bound_channel)
        vb->lb_certainty = e->bound_channel;
    record_var_pin(vb, x, e->Lframe, e, PARAM_INVARIANT);
    if (lterm_find(e, vb->lbs, x, e->Lframe) != NULL) {
        if (vb->lb_spell < e->spell_channel)
            vb->lb_spell = e->spell_channel;
        // var_lt locates its operand at the right position; `x` is a left
        // term, so hand it the left chain
        jl_varbinding_t *saveR = e->Rframe;
        e->Rframe = e->Lframe;
        int sub = var_lt(v, x, e, PARAM_NONE, vb, innervar);
        e->Rframe = saveR;
        return sub;
    }
    if (!ccheck_le_ubs(e, x, e->Lframe, vb->ubs))
        return 0;
    // when the var is pinned (`lb === ub`), `x <= ub` was just checked and a
    // join picking `x` proves `lb <= x`, i.e. `x` respells the same type: keep
    // the existing spelling unless `x`'s is more authoritative (see `lb_spell`)
    int pinned = binding_pinned(e, vb);
    if (e->intersection) {
        jl_value_t *xv = jl_has_dangling_tvarrefs(x) ? frame_substitute(x, e->Lframe, e) : x;
        jl_value_t *vb_lb = binding_lb(e, vb);
        jl_value_t *lb = simple_join(vb_lb, xv);
        JL_GC_PUSH1(&lb);
        if (pinned && lb == xv && e->spell_channel <= vb->lb_spell) {
            JL_GC_POP();
            // validate the inclusion the respell path would have checked below,
            // then keep both existing spellings
            if (!subtype_ccheck(vb_lb, xv, e))
                return 0;
            return 1;
        }
        if (!jl_is_typevar(lb) || v == NULL || !reachable_var(lb, v, e)) {
            if (vb_lb != lb) {
                binding_set_lb(e, vb, lb);
                vb->lb_spell = e->spell_channel;
            }
        }
        JL_GC_POP();
        if (binding_ub(e, vb) == xv)
            return 1;
        // reload through the binding: the joined bound is rooted by its field now
        if (!subtype_ccheck(binding_lb(e, vb), xv, e))
            return 0;
        // skip `simple_meet` here as we have proven `x <: vb->ub`
        if (v == NULL || !reachable_var(xv, v, e))
            binding_set_ub(e, vb, xv);
        return 1;
    }
    if (pinned && e->spell_channel <= vb->lb_spell) {
        jl_value_t *cur = lterm_closed1(vb->lbs);
        if (cur != NULL && !jl_has_dangling_tvarrefs(x)) {
            jl_value_t *lb = simple_join(cur, x);
            if (lb == x)
                // validate the inclusion the respell path would have checked
                // below, then keep both existing spellings
                return ccheck_lbs_le(e, vb->lbs, x, e->Lframe);
        }
    }
    if (binding_join_lb(e, vb, x, e->Lframe))
        vb->lb_spell = e->spell_channel;
    if (vb->ubs != NULL && vb->ubs->next == NULL && lterm_entry_egal(e, vb->ubs, x, e->Lframe))
        return 1;
    if (!ccheck_lbs_le(e, vb->lbs, x, e->Lframe))
        return 0;
    // `x <: ub` was just checked: the meet with `x` is `x`
    vb->ubs = lterm_cons(e, x, e->Lframe, NULL);
    return 1;
}

static int equal_var(jl_tvar_t *v, jl_value_t *x, jl_stenv_t *e)
{
    int innervar = 0;
    jl_varbinding_t *vb = lookup_binding(e, v, &innervar);
    return equal_var_(v, vb, innervar, x, e);
}

static int forall_exists_equal(jl_value_t *x, jl_value_t *y, jl_stenv_t *e)
{
    if (obviously_egal(x, y) && !jl_has_dangling_tvarrefs(x)) return 1;

    if ((is_indefinite_length_tuple_type(x) && is_definite_length_tuple_type(y)) ||
        (is_definite_length_tuple_type(x) && is_indefinite_length_tuple_type(y)))
        return 0;

    if (jl_is_datatype(x) && jl_is_datatype(y)) {
        // Fastpath for nested constructor. Skip the unneeded `>:` check.
        // Note: since there is no changes to the environment or union stack implied by `x` or `y`, this will simply forward to calling
        // `forall_exists_equal(xi, yi, e)` on each parameter `(xi, yi)` of `(x, y)`,
        // which means this subtype call will give the same result for `subtype(x, y)` and `subtype(y, x)`.
        jl_datatype_t *xd = (jl_datatype_t*)x, *yd = (jl_datatype_t*)y;
        if (xd->name != yd->name)
            return 0;
        if (xd->name != jl_tuple_typename)
            return subtype(x, y, e, PARAM_INVARIANT);
    }

    if ((jl_is_uniontype(x) && jl_is_uniontype(y))) {
        // For 2 unions, first try a more efficient greedy algorithm that compares the unions
        // componentwise. If failed, `exists_subtype` would memorize that this branch should be skipped.
        // Note: this is valid because the normal path checks `>:` locally.
        if (pick_union_decision(e, 1) == 0) {
            return forall_exists_equal(((jl_uniontype_t *)x)->a, ((jl_uniontype_t *)y)->a, e) &&
                   forall_exists_equal(((jl_uniontype_t *)x)->b, ((jl_uniontype_t *)y)->b, e);
        }
    }

    if (e->Loffset == 0 && jl_is_type(x) && (!e->intersection || !jl_is_uniontype(x))) {
        // Fastpath for Type == TypeVar.
        // Avoid duplicated `<:` check between adjacent `var_gt` and `var_lt`
        if (jl_is_typevar(y))
            return equal_var((jl_tvar_t *)y, x, e);
        if (jl_is_tvarref(y)) {
            // an unmaterialized binding is classified directly; a detached
            // reference supports no bound reasoning and takes the generic path
            jl_varbinding_t *yrb = frame_lookup(e->Rframe, jl_tvarref_depth(y));
            if (yrb != NULL && yrb->popped) {
                // an inner variable (or a pinned value)
                jl_value_t *yv = binding_ref_value(e, yrb);
                if (jl_is_typevar(yv))
                    return equal_var((jl_tvar_t*)yv, x, e);
                y = yv;
            }
            else if (yrb != NULL)
                return equal_var_(yrb->var, yrb, 0, x, e);
        }
    }

    jl_saved_unionstate_t oldLunions; push_unionstate(&oldLunions, &e->Lunions);

    int sub = local_forall_exists_subtype(x, y, e, PARAM_INVARIANT, -1);
    if (sub) {
        // the terms swap sides, and their reference frames with them
        flip_offset(e); flip_frames(e);
        sub = local_forall_exists_subtype(y, x, e, PARAM_NONE, 0);
        flip_offset(e); flip_frames(e);
    }
    pop_unionstate(&e->Lunions, &oldLunions);
    return sub;
}

static int exists_subtype(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, jl_savedenv_t *se, jl_param_pos_t param) JL_CANSAFEPOINT
{
    e->Runions.used = 0;
    while (1) {
        e->Runions.depth = 0;
        e->Runions.more = 0;
        e->Lunions.depth = 0;
        e->Lunions.more = 0;
        if (subtype(x, y, e, param))
            return 1;
        if (next_union_state(e, 1)) {
            // We preserve `envout` here as `subtype_unionall` needs previous assigned env values.
            int oldidx = e->envidx;
            e->envidx = e->envsz;
            restore_env(e, se, 1);
            e->envidx = oldidx;
        }
        else {
            restore_env(e, se, 1);
            return 0;
        }
    }
}

static int forall_exists_subtype(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, jl_param_pos_t param)
{
    // The depth recursion has the following shape, after simplification:
    // ∀₁
    //   ∃₁
    assert(e->Runions.depth == 0);
    assert(e->Lunions.depth == 0);
    jl_savedenv_t se;
    save_env(e, &se, 1);

    e->Lunions.used = 0;
    int sub;
    while (1) {
        sub = exists_subtype(x, y, e, &se, param);
        if (!sub || !next_union_state(e, 0))
            break;
        re_save_env(e, &se, 1);
    }

    free_env(&se);
    return sub;
}

static void init_stenv(jl_stenv_t *e, jl_value_t **env, int envsz)
{
    e->vars = NULL;
    e->Lframe = NULL;
    e->Rframe = NULL;
    e->frames_flipped = 0;
    e->substmemo = NULL; // rooted with `opened` at the entry points
    e->roots = NULL;
    e->finalvars = NULL;
    e->ref1 = jl_new_tvarref(1); // (a permanent object)
    e->arena = NULL; // the entry points provide one
    e->nbindings = 0;
    e->opened = NULL; // n.b. the caller must root this slot before any opening
    e->envsz = envsz;
    e->envout = env;
    if (envsz) {
        assert(env != NULL);
        memset(env, 0, envsz*sizeof(void*));
    }
    e->envidx = 0;
    e->invdepth = 0;
    e->bound_channel = BOUND_EGAL;
    e->spell_channel = BOUND_EGAL;
    e->value_descent = 0;
    e->intersection = 0;
    e->emptiness_only = 0;
    e->triangular = 0;
    e->ignore_lb_required = 0;
    e->Loffset = 0;
    e->Lunions.depth = 0;      e->Runions.depth = 0;
    e->Lunions.more = 0;       e->Runions.more = 0;
    e->Lunions.used = 0;       e->Runions.used = 0;
    e->Lunions.stack.next = NULL;
    e->Runions.stack.next = NULL;
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
    if (t == (jl_value_t*)jl_bottom_type)
        return 1;
    if (jl_is_some_Type(t))
        return 0; // Type{T}/TypeEgal{T} may have the concrete supertype `typeof(T)`, so don't try to handle them here
    if (jl_is_datatype(t)) {
        return jl_is_concrete_type(t) ? 1 : 2;
    }
    if (jl_is_vararg(t))
        return 0;
    if (jl_is_typevar(t))
        return 0; // could be 0 or more, since we didn't track if it was unbound
    if (jl_is_uniontype(t)) {
        int count = concrete_min(((jl_uniontype_t*)t)->a);
        if (count > 1)
            return count;
        return count + concrete_min(((jl_uniontype_t*)t)->b);
    }
    assert(!jl_is_kind(t));
    return 1; // a non-Type is also considered concrete
}

// "not structurally ground" for obvious_subtype purposes: a dangling de Bruijn
// reference constrains a parameter just like a free typevar does, but is not
// seen by `jl_has_free_typevars`
static int has_free_or_dangling_typevars(jl_value_t *v) JL_NOTSAFEPOINT
{
    return jl_has_free_typevars(v) || jl_has_dangling_tvarrefs(v);
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
    if (jl_is_unionall(x) && jl_is_unionall(y)) {
        while (jl_is_unionall(x)) {
            if (!jl_is_unionall(y)) {
                if (obvious_subtype(jl_unwrap_unionall(x), y, y0, subtype) && !*subtype)
                    return 1;
                return 0;
            }
            // strip in lockstep only while the binders' bounds agree; positional
            // references are only comparable across sides under equal bounds
            if (!obviously_egal(((jl_unionall_t*)x)->lb, ((jl_unionall_t*)y)->lb) ||
                !obviously_egal(((jl_unionall_t*)x)->ub, ((jl_unionall_t*)y)->ub))
                return 0;
            x = ((jl_unionall_t*)x)->body;
            y = ((jl_unionall_t*)y)->body;
        }
        // positional reference identity cannot prove subtyping: the sides'
        // binders may differ in diagonality (e.g. an invariant occurrence on
        // one side only), which this structural walk does not see. Only a
        // definite "no" may be propagated.
        if (obvious_subtype(x, y, y0, subtype) && !*subtype)
            return 1;
        return 0;
    }
    if (jl_is_unionall(x)) {
        // (`y` is not a unionall here: the lockstep block above returned)
        if (obvious_subtype(jl_unwrap_unionall(x), y, y0, subtype) && !*subtype)
            return 1;
        return 0;
    }
    if (jl_is_unionall(y))
        y = jl_unwrap_unionall(y);
    if (is_typeofbottom_typealias(x))
        x = (jl_value_t*)jl_typeofbottom_type;
    if (is_typeofbottom_typealias(y))
        y = (jl_value_t*)jl_typeofbottom_type;
    if (x == y || y == (jl_value_t*)jl_any_type) {
        *subtype = 1;
        return 1;
    }
    if (jl_is_typevar(x) || jl_is_tvarref(x)) {
        return 0;
    }
    if (jl_is_typevar(y) || jl_is_tvarref(y)) {
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
    if (jl_is_vararg(x)) {
        if (!jl_is_vararg(y)) {
            *subtype = 0;
            return 1;
        }
        return 0;
    }
    // `Intersect` is an internal meet node, not a real type tag. Handle it
    // before the non-type fallback, which would otherwise treat it as egal-only.
    if (jl_is_intersecttype(y)) {
        jl_intersecttype_t *iy = (jl_intersecttype_t*)y;
        int sub_a, sub_b;
        int known_a = obvious_subtype(x, iy->a, y0, &sub_a);
        if (known_a && !sub_a) {
            *subtype = 0;
            return 1;
        }
        int known_b = obvious_subtype(x, iy->b, y0, &sub_b);
        if (known_b && !sub_b) {
            *subtype = 0;
            return 1;
        }
        if (known_a && sub_a && known_b && sub_b) {
            *subtype = 1;
            return 1;
        }
        return 0;
    }
    if (jl_is_intersecttype(x))
        return 0;
    if (!jl_is_type(x) || !jl_is_type(y)) {
        *subtype = jl_egal(x, y);
        return 1;
    }
    if (jl_is_typeegal(x)) {
        jl_value_t *A = jl_typeegal_T(x);
        if (jl_is_typeegal(y)) {
            *subtype = jl_egal(A, jl_typeegal_T(y)); // `TypeEgal{A} <: TypeEgal{B}` iff `A === B`
            return 1;
        }
        // `Type{B}` (equality) and unions are left for the full subtype check
        if (jl_is_datatype(y))
            return obvious_subtype(jl_typeof(A), y, y0, subtype);
        return 0;
    }
    if (jl_is_typeegal(y)) {
        // nothing but `Union{}` (handled above) and egal `TypeEgal`s is a subtype
        *subtype = 0;
        return 1;
    }
    if (jl_is_uniontype(x)) {
        // TODO: consider handling more LHS unions, being wary of covariance
        jl_value_t *xa = ((jl_uniontype_t*)x)->a;
        jl_value_t *xb = ((jl_uniontype_t*)x)->b;
        if (obvious_subtype(xa, y, y0, subtype) && *subtype) {
            if (obvious_subtype(xb, y, y0, subtype) && *subtype)
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
        jl_value_t *ya = ((jl_uniontype_t*)y)->a;
        jl_value_t *yb = ((jl_uniontype_t*)y)->b;
        if (obvious_subtype(x, ya, y0, subtype)) {
            if (*subtype)
                return 1;
            if (obvious_subtype(x, yb, y0, subtype))
                return 1;
        }
        else if (obvious_subtype(x, yb, y0, subtype)) {
            if (*subtype)
                return 1;
        }
        return 0;
    }
    if (x == (jl_value_t*)jl_any_type) {
        *subtype = 0;
        return 1;
    }
    // `Type{T}` is a `TypeEq`, not a `DataType`, so the `jl_is_datatype` cases
    // below miss it; decide the obvious `X <: Type{T}` rejections here.
    if (jl_is_typeeq(y) && !jl_is_typeeq(x) && jl_is_datatype(x) &&
            x != (jl_value_t*)jl_typeofbottom_type) {
        jl_value_t *t0 = jl_typeeq_T(y);
        if (jl_is_typevar(t0) || jl_is_tvarref(t0)) {
            // a bound-variable reference (dangling here after unwrapping) acts
            // like a typevar for this purpose
            if (!is_kind_or_anytype(x)) {
                *subtype = 0;    // an ordinary type value is never a subtype of `Type{T}`
                return 1;
            }
            return 0;            // a kind may be: `Type <: Type{T}` is handled by `subtype`
        }
        if (!jl_is_typeeq(t0)) {
            if (jl_has_dangling_tvarrefs(t0))
                return 0;        // references into the stripped binders: not obvious
            *subtype = 0;        // `X <: Type{ConcreteType}` (X not a `Type{}`) is never true
            return 1;
        }
        // `Type{Type{...}}`: leave to `subtype`
    }
    if (jl_is_datatype(y)) {
        int istuple = (((jl_datatype_t*)y)->name == jl_tuple_typename);
        int iscov = istuple;
        // TODO: this would be a nice fast-path to have, unfortunately,
        //       datatype allocation fails to correctly hash-cons them
        //       and the subtyping tests include tests for this case
        //if (!iscov && ((jl_datatype_t*)y)->isconcretetype && !jl_is_typeeq(x)) {
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
                jl_datatype_t *temp = (jl_datatype_t*)x;
                while (temp->name != ((jl_datatype_t*)y)->name) {
                    // raw read: this heuristic must not reach a safepoint, so
                    // a deferred (unset) supertype stays undecided and the
                    // full algorithm resolves it
                    temp = temp->super;
                    if (temp == NULL)
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
            if (!iscov && !((jl_datatype_t*)x)->hasfreetypevars &&
                !((jl_datatype_t*)x)->hasescapingrefs) {
                // by transitivity, if `wrapper <: y`, then `x <: y` if x is a leaf type of its name
                jl_value_t *wrapper = ((jl_datatype_t*)x)->name->wrapper;
                int wrapper_sub = 0;
                JL_GC_PUSH1(&wrapper);
                wrapper_sub = obvious_subtype(wrapper, y, y0, subtype);
                JL_GC_POP();
                if (wrapper_sub && *subtype)
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
                jl_value_t *a;
                if (i >= (npx - (vx == JL_VARARG_NONE ? 0 : 1))) {
                    a = vxt;
                    assert(a != NULL);
                }
                else {
                    a = jl_tparam(x, i);
                }
                jl_value_t *b = jl_tparam(y, i);
                if (iscov || jl_is_typevar(b)) {
                    if (obvious_subtype(a, b, y0, subtype)) {
                        if (!*subtype)
                            return 1;
                        if (has_free_or_dangling_typevars(b)) // b is actually more constrained that this
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
                            if (has_free_or_dangling_typevars(b)) // b is actually more constrained that this
                                uncertain = 1;
                        }
                        else {
                            uncertain = 1;
                        }
                        if (!has_free_or_dangling_typevars(b) && obvious_subtype(b, a, y0, subtype)) {
                            if (!*subtype)
                                return 1;
                            if (has_free_or_dangling_typevars(a)) // a is actually more constrained that this
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
                if (jl_is_tvarref(b))
                    // a bound variable without its binder's bounds at hand
                    return 0;
                if (jl_is_typevar(b)) {
                    if (var_occurs_invariant(y0, (jl_tvar_t*)b))
                        return 0;
                }
                if (nparams_expanded_x > npy && jl_is_typevar(b) && is_leaf_typevar((jl_tvar_t *)b) && concrete_min(a1) > 1) {
                    // diagonal rule for 2 or more elements: they must all be concrete on the LHS
                    *subtype = 0;
                    return 1;
                }
                jl_value_t *a1u = jl_unwrap_unionall(a1);
                // only `TypeEgal{T}` (a tag-exact singleton) and `Type{Union{}}`
                // (`== TypeofBottom`) lie in their `typeof`; other `Type{T}`
                // elements straddle kinds (#33136) and are left to the full
                // algorithm below (conservatively marked uncertain)
                if (jl_is_typeegal(a1u) ||
                    (jl_is_typeeq(a1u) && jl_typeeq_T(a1u) == jl_bottom_type)) {
                    a1 = jl_typeof(jl_some_Type_T(a1u));
                }
                for (; i < nparams_expanded_x; i++) {
                    jl_value_t *a = (vx != JL_VARARG_NONE && i >= npx - 1) ? vxt : jl_tparam(x, i);
                    if (i > npy && jl_is_typevar(b) && is_leaf_typevar((jl_tvar_t *)b)) { // i == npy implies a == a1
                        // diagonal rule: all the later parameters are also constrained to be type-equal to the first
                        jl_value_t *a2 = a;
                        jl_value_t *au = jl_unwrap_unionall(a);
                        if (jl_is_typeegal(au) ||
                            (jl_is_typeeq(au) && jl_typeeq_T(au) == jl_bottom_type)) {
                            // a `TypeEgal{T}` (or `Type{Union{}}`) element lies
                            // exactly in the concrete typeof(T); see above
                            a2 = jl_typeof(jl_some_Type_T(au));
                        }
                        if (!obviously_egal(a1, a2)) {
                            if (obvious_subtype(a2, a1, y0, subtype)) {
                                if (!*subtype)
                                    return 1;
                                if (has_free_or_dangling_typevars(a1)) // a1 is actually more constrained that this
                                    uncertain = 1;
                            }
                            else {
                                uncertain = 1;
                            }
                            if (obvious_subtype(a1, a2, y0, subtype)) {
                                if (!*subtype)
                                    return 1;
                                if (has_free_or_dangling_typevars(a2)) // a2 is actually more constrained that this
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
                        if (has_free_or_dangling_typevars(b)) // b is actually more constrained that this
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
         (jl_is_unionall(y) || jl_is_uniontype(y) || jl_is_some_Type(y)) &&
         jl_types_struct_equiv(x, y))) {
        if (envsz != 0) { // quickly copy env from x
            jl_unionall_t *ua = (jl_unionall_t*)x;
            int i;
            jl_svec_t *vars = NULL;
            jl_tvar_t *v = NULL;
            JL_GC_PUSH2(&vars, &v);
            vars = jl_alloc_svec(envsz);
            for (i = 0; i < envsz; i++) {
                assert(jl_is_unionall(ua));
                // materialize only the binder's variable; the body is walked
                // in place and the occurrence check is positional
                v = jl_unionall_bind_var(ua, vars, i);
                jl_svecset(vars, i, v);
                int constrained = constrains_ref_static(1, ua->lb == jl_bottom_type, ua->body, 1);
                env[i] = wrap_tvar_env((jl_value_t*)v, constrained);
                ua = (jl_unionall_t*)ua->body;
            }
            JL_GC_POP();
        }
        return 1;
    }
    if (jl_is_typeapp(x) || jl_is_typeapp(y))
        jl_error("internal error: TypeApp in subtyping");
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
    jl_starena_t arena = {NULL, NULL};
    e.arena = &arena;
    JL_GC_PUSH4(&e.opened, &e.substmemo, &e.roots, &e.finalvars);
    int subtype = forall_exists_subtype(x, y, &e, PARAM_NONE);
    free_stenv(&e);
    JL_GC_POP();
    assert(obvious_subtype == 3 || obvious_subtype == subtype || jl_has_free_typevars(x) || jl_has_free_typevars(y) || jl_has_dangling_tvarrefs(x) || jl_has_dangling_tvarrefs(y));
#ifndef NDEBUG
    if (obvious_subtype == 0 || (obvious_subtype == 1 && envsz == 0))
        subtype = obvious_subtype; // this ensures that running in a debugger doesn't change the result
#endif
    return subtype;
}

static int subtype_in_env(jl_value_t *x, jl_value_t *y, jl_stenv_t *e) JL_CANSAFEPOINT
{
    jl_stenv_t e2;
    init_stenv(&e2, NULL, 0);
    e2.vars = e->vars;
    e2.Lframe = e->Lframe; // operands may be raw walk fragments of `e`'s terms
    e2.Rframe = e->Rframe;
    e2.frames_flipped = e->frames_flipped;
    e2.opened = e->opened; // share the binding-variable memo (and its rooting)
    e2.substmemo = e->substmemo;
    e2.roots = e->roots;
    e2.finalvars = e->finalvars;
    e2.arena = e->arena; // the bindings and lists of the nested query live on
    e2.nbindings = e->nbindings; // keep the binding serials unique across the shared memo
    e2.intersection = e->intersection;
    e2.invdepth = e->invdepth;
    e2.envsz = e->envsz;
    e2.envout = e->envout;
    e2.envidx = e->envidx;
    e2.ignore_lb_required = e->ignore_lb_required;
    e2.Loffset = e->Loffset;
    JL_GC_PUSH4(&e2.opened, &e2.substmemo, &e2.roots, &e2.finalvars);
    int sub = forall_exists_subtype(x, y, &e2, PARAM_NONE);
    // the nested query may have (lazily) allocated the shared memo arrays;
    // keep sharing them (the caller roots these slots)
    e->opened = e2.opened;
    e->substmemo = e2.substmemo;
    e->roots = e2.roots;
    e->finalvars = e2.finalvars;
    e->nbindings = e2.nbindings;
    e2.arena = NULL; // (owned by the caller)
    free_stenv(&e2);
    JL_GC_POP();
    return sub;
}

JL_DLLEXPORT int jl_subtype(jl_value_t *x, jl_value_t *y)
{
    return jl_subtype_env(x, y, NULL, 0);
}

JL_DLLEXPORT int jl_types_equal(jl_value_t *a, jl_value_t *b)
{
    if (a == b)
        return 1;
    if (jl_typeof(a) == jl_typeof(b) && jl_types_struct_equiv(a, b))
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
        jl_starena_t arena = {NULL, NULL};
        e.arena = &arena;
        JL_GC_PUSH4(&e.opened, &e.substmemo, &e.roots, &e.finalvars);
        int subtype = forall_exists_subtype(a, b, &e, PARAM_NONE);
        free_stenv(&e);
        JL_GC_POP();
        assert(subtype_ab == 3 || subtype_ab == subtype || jl_has_free_typevars(a) || jl_has_free_typevars(b) || jl_has_dangling_tvarrefs(a) || jl_has_dangling_tvarrefs(b));
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
        jl_starena_t arena = {NULL, NULL};
        e.arena = &arena;
        JL_GC_PUSH4(&e.opened, &e.substmemo, &e.roots, &e.finalvars);
        int subtype = forall_exists_subtype(b, a, &e, PARAM_NONE);
        free_stenv(&e);
        JL_GC_POP();
        assert(subtype_ba == 3 || subtype_ba == subtype || jl_has_free_typevars(a) || jl_has_free_typevars(b) || jl_has_dangling_tvarrefs(a) || jl_has_dangling_tvarrefs(b));
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
    return !jl_is_kind(b) || !jl_is_some_Type(a); // || jl_is_datatype_singleton((jl_datatype_t*)jl_tparam0(a));
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

// Return true if `t` may meet a type object by its identity (a `TypeEq{...}` or
// `TypeEgal{...}` component), rather than only through a kind it contains in full;
// if false, `isa(x, t)` can instead simply check for `typeof(x) <: t`
int jl_has_intersect_type_not_kind(jl_value_t *t)
{
    t = jl_unwrap_unionall(t);
    if (t == (jl_value_t*)jl_any_type)
        return 1;
    assert(!jl_is_vararg(t));
    if (jl_is_uniontype(t))
        return jl_has_intersect_type_not_kind(((jl_uniontype_t*)t)->a) ||
               jl_has_intersect_type_not_kind(((jl_uniontype_t*)t)->b);
    if (jl_is_some_Type(t))
        return 1;
    if (jl_is_typevar(t))
        return jl_has_intersect_type_not_kind(((jl_tvar_t*)t)->ub);
    return 0;
}


JL_DLLEXPORT int jl_isa(jl_value_t *x, jl_value_t *t)
{
    if (t == (jl_value_t*)jl_any_type || jl_typetagis(x,t))
        return 1;
    if (jl_typetagof(x) < (jl_max_tags << 4) && jl_is_datatype(t) && jl_typetagis(x,((jl_datatype_t*)t)->smalltag << 4))
        return 1;
    if (jl_is_typeapp(t))
        jl_error("internal error: TypeApp in jl_isa");
    if (jl_is_type(x)) {
        if (t == (jl_value_t*)jl_type_type)
            return 1;
        if (!jl_has_free_typevars(x)) {
            if (jl_is_concrete_type(t))
                return 0;
            if (jl_is_typeeq(t))
                return jl_types_equal(x, jl_typeeq_T(t));
            if (jl_is_typeegal(t))
                return jl_egal(x, jl_typeegal_T(t));
            jl_value_t *t2 = jl_unwrap_unionall(t);
            if (jl_is_typeeq(t2)) {
                jl_value_t *tp = jl_typeeq_T(t2);
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
            else if (jl_is_datatype(t2)) {
                return jl_subtype(jl_typeof(x), t);
            }
            if (jl_subtype(jl_typeof(x), t))
                return 1;
            if (jl_has_intersect_type_not_kind(t2)) {
                // wrap as the egality kind: `TypeEgal{x} <: Type{B}` also holds
                // whenever `x == B`, so this covers both wrapper kinds in `t`
                jl_value_t *wrapped = jl_wrap_TypeEgal(x);  // TODO jb/subtype avoid jl_wrap_TypeEgal
                JL_GC_PUSH1(&wrapped);
                int ans = jl_subtype(wrapped, t);
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

static jl_value_t *intersect(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, jl_param_pos_t param) JL_CANSAFEPOINT;

static jl_value_t *intersect_all(jl_value_t *x, jl_value_t *y, jl_stenv_t *e) JL_CANSAFEPOINT;

// intersect in nested union environment, similar to subtype_ccheck
static jl_value_t *intersect_aside(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int depth)
{
    // band-aid for #30335
    if (x == (jl_value_t*)jl_any_type && !jl_is_typevar(y))
        return y;
    if (y == (jl_value_t*)jl_any_type && !jl_is_typevar(x))
        return x;
    // band-aid for #46736 #56040 (structural checks: meaningless between raw
    // references from different chains)
    if (!jl_has_dangling_tvarrefs(x) && !jl_has_dangling_tvarrefs(y)) {
        if (obviously_in_union(x, y))
            return y;
        if (obviously_in_union(y, x))
            return x;
    }

    // Consistency check for a typevar bound: covariant occurrences inside this
    // call should not accumulate into the surrounding scope's diagonality
    // counter. Save & reset the counters before any env truncation so all
    // vars are captured, and restore after the env is put back.
    int8_t *saved_cov = (int8_t*)alloca(current_env_length(e));
    int nsaved_cov = push_consistency_scope(e, saved_cov);

    jl_varbinding_t *vars = NULL;
    jl_varbinding_t *bbprev = NULL;
    jl_varbinding_t *saveL = e->Lframe, *saveR = e->Rframe;
    int xinner = 0, yinner = 0;
    jl_varbinding_t *xb = jl_is_typevar(x) ? lookup_binding(e, (jl_tvar_t *)x, &xinner) : NULL;
    jl_varbinding_t *yb = jl_is_typevar(y) ? lookup_binding(e, (jl_tvar_t *)y, &yinner) : NULL;
    int simple_x = jl_is_typevar(x) ? (xb ? !has_free_or_dangling_typevars(binding_ub(e, xb)) : !xinner) : !has_free_or_dangling_typevars(x);
    int simple_y = jl_is_typevar(y) ? (yb ? !has_free_or_dangling_typevars(binding_ub(e, yb)) : !yinner) : !has_free_or_dangling_typevars(y);
    if (simple_x && simple_y && !(xb && yb)) {
        vars = e->vars;
        e->vars = xb ? xb : yb;
        if (e->vars != NULL) {
            bbprev = e->vars->prev;
            e->vars->prev = NULL;
        }
        // the operands are self-contained; the truncated scope must not
        // resolve deeper references against the enclosing walk's binders
        e->Lframe = e->Rframe = NULL;
    }
    jl_saved_unionstate_t oldRunions; push_unionstate(&oldRunions, &e->Runions);
    int savedepth = e->invdepth;
    e->invdepth = depth;
    jl_value_t *res = intersect_all(x, y, e);
    e->invdepth = savedepth;
    pop_unionstate(&e->Runions, &oldRunions);
    if (bbprev) e->vars->prev = bbprev;
    if (vars) {
        e->vars = vars;
        e->Lframe = saveL;
        e->Rframe = saveR;
    }

    pop_consistency_scope(e, saved_cov, nsaved_cov);
    return res;
}

static jl_value_t *intersect_union(jl_value_t *x, jl_uniontype_t *u, jl_stenv_t *e, int8_t R, jl_param_pos_t param) JL_CANSAFEPOINT
{
    int no_free = !has_free_or_dangling_typevars(x) && !has_free_or_dangling_typevars((jl_value_t*)u);
    if (param == PARAM_INVARIANT || no_free) {
        jl_value_t *a=NULL, *b=NULL;
        JL_GC_PUSH2(&a, &b);
        jl_varbinding_t *vars = NULL;
        jl_varbinding_t *saveL = e->Lframe, *saveR = e->Rframe;
        if (no_free) {
            // ground operands: the walk below is self-contained
            vars = e->vars;
            e->vars = NULL;
            e->Lframe = e->Rframe = NULL;
        }
        jl_saved_unionstate_t oldRunions; push_unionstate(&oldRunions, &e->Runions);
        a = R ? intersect_all(x, u->a, e) : intersect_all(u->a, x, e);
        b = R ? intersect_all(x, u->b, e) : intersect_all(u->b, x, e);
        pop_unionstate(&e->Runions, &oldRunions);
        if (vars) {
            e->vars = vars;
            e->Lframe = saveL;
            e->Rframe = saveR;
        }
        jl_value_t *i = simple_join(a,b);
        JL_GC_POP();
        return i;
    }
    jl_value_t *choice = pick_union_element((jl_value_t*)u, e, 1);
    // try all possible choices in covariant position; union them all together at the top level
    return R ? intersect(x, choice, e, param) : intersect(choice, x, e, param);
}

// set a variable to a non-type constant
static jl_value_t *set_var_to_const(jl_varbinding_t *bb, jl_value_t *v JL_MAYBE_UNROOTED, jl_stenv_t *e, int R) JL_CANSAFEPOINT
{
    int offset = R ? -e->Loffset : e->Loffset;
    if (bb->lbs == NULL && bb->ubs == NULL) {
        if (offset == 0) {
            JL_GC_PUSH1(&v);
            binding_set_lb(e, bb, v);
            bb->ubs = bb->lbs;
            JL_GC_POP();
        }
        else if (jl_is_long(v)) {
            size_t iv = jl_unbox_long(v);
            v = jl_box_long(iv + offset);
            JL_GC_PUSH1(&v);
            binding_set_lb(e, bb, v);
            bb->ubs = bb->lbs;
            JL_GC_POP();
            // Here we always return the shorter `Vararg`'s length.
            if (offset > 0)
                return jl_box_long(iv);
        }
        else
            return jl_bottom_type;
        return v;
    }
    jl_value_t *bb_lb = binding_lb(e, bb);
    if (jl_is_long(v) && jl_is_long(bb_lb)) {
        if (jl_unbox_long(v) + offset != jl_unbox_long(bb_lb))
            return jl_bottom_type;
        // Here we always return the shorter `Vararg`'s length.
        if (offset < 0) return bb_lb;
    }
    else if (!jl_egal(v, bb_lb)) {
        return jl_bottom_type;
    }
    return v;
}

static jl_value_t *bound_var_below(jl_tvar_t *tv, jl_varbinding_t *bb, jl_stenv_t *e, int R) JL_CANSAFEPOINT {
    if (!bb)
        return (jl_value_t*)tv;
    if (bb->depth0 != e->invdepth)
        return jl_bottom_type;
    e->invdepth++;
    record_var_occurrence(bb, e, PARAM_INVARIANT);
    e->invdepth--;
    int offset = R ? -e->Loffset : e->Loffset;
    jl_value_t *bb_lb = lterm_long(bb->lbs);
    if (bb_lb != NULL) {
        ssize_t blb = jl_unbox_long(bb_lb);
        if (blb < offset || blb < 0)
            return jl_bottom_type;
        // Here we always return the shorter `Vararg`'s length.
        if (offset <= 0)
            return bb_lb;
        return jl_box_long(blb - offset);
    }
    if (offset > 0) {
        if (bb->innervars == NULL) {
            bb->innervars = jl_alloc_array_1d(jl_array_any_type, 0);
            stenv_root(e, (jl_value_t*)bb->innervars);
        }
        jl_value_t *ntv = NULL;
        JL_GC_PUSH1(&ntv);
        ntv = (jl_value_t *)jl_new_typevar(tv->name, jl_bottom_type, (jl_value_t *)jl_any_type);
        jl_array_ptr_1d_push(bb->innervars, ntv);
        JL_GC_POP();
        return ntv;
    }
    return (jl_value_t*)tv;
}

static int subtype_by_bounds(jl_value_t *x, jl_value_t *y, jl_stenv_t *e) JL_NOTSAFEPOINT;

// similar to `subtype_by_bounds`, used to avoid stack-overflow caused by circular constraints.
static int try_subtype_by_bounds(jl_value_t *a, jl_value_t *b, jl_stenv_t *e) JL_CANSAFEPOINT
{
    if (jl_is_uniontype(a))
        return try_subtype_by_bounds(((jl_uniontype_t *)a)->a, b, e) &&
               try_subtype_by_bounds(((jl_uniontype_t *)a)->b, b, e);
    else if (jl_is_uniontype(b))
        return try_subtype_by_bounds(a, ((jl_uniontype_t *)b)->a, e) ||
               try_subtype_by_bounds(a, ((jl_uniontype_t *)b)->b, e);
    else if (a == jl_bottom_type || b == (jl_value_t *)jl_any_type || obviously_egal(a, b))
        return 1;
    else if (!jl_is_typevar(b))
        return 0;
    else if (jl_is_typevar(a) && subtype_by_bounds(a, b, e))
        return 1;
    // check if `Union{a, ...} <: b`.
    int innervar = 0;
    jl_varbinding_t *vb = lookup_binding(e, (jl_tvar_t *)b, &innervar);
    if (vb == NULL && !innervar)
        return subtype_singleton_typevar(a, (jl_tvar_t*)b);
    jl_value_t *blb = vb ? binding_lb(e, vb) : ((jl_tvar_t *)b)->lb;
    return obviously_in_union(a, blb);
}

static int subtype_in_env_existential(jl_value_t *x, jl_value_t *y, jl_stenv_t *e) JL_CANSAFEPOINT;

static int try_subtype_in_env_existential(jl_value_t *a, jl_value_t *b, jl_stenv_t *e) JL_CANSAFEPOINT
{
    if (try_subtype_by_bounds(a, b, e))
        return 1;
    jl_savedenv_t se;
    save_env(e, &se, 1);
    int ret = subtype_in_env_existential(a, b, e);
    restore_env(e, &se, 1);
    free_env(&se);
    return ret;
}

static int subtype_in_env_nonexistential(jl_value_t *x, jl_value_t *y, jl_stenv_t *e) JL_CANSAFEPOINT;

static int try_subtype_in_env_nonexistential(jl_value_t *a, jl_value_t *b, jl_stenv_t *e) JL_CANSAFEPOINT
{
    if (try_subtype_by_bounds(a, b, e))
        return 1;
    jl_savedenv_t se;
    save_env(e, &se, 0); // Set root=0 as this is a nonexistential subtype check (no bounds change in env)
    int ret = subtype_in_env_nonexistential(a, b, e);
    restore_env(e, &se, 0);
    free_env(&se);
    return ret;
}

static void set_bound(jl_stenv_t *e, jl_varbinding_t *bb, int ub, jl_value_t *val JL_MAYBE_UNROOTED, jl_tvar_t *v) JL_CANSAFEPOINT
{
    if (in_union(val, (jl_value_t*)v))
        return;
    jl_varbinding_t *btemp = e->vars;
    while (btemp != NULL) {
        if ((lterm_closed1(btemp->lbs) == (jl_value_t*)v || lterm_closed1(btemp->ubs) == (jl_value_t*)v) &&
            in_union(val, (jl_value_t*)btemp->var))
            return;
        btemp = btemp->prev;
    }
    if (ub)
        binding_set_ub(e, bb, val);
    else
        binding_set_lb(e, bb, val);
}

// subtype, treating all vars as existential
static int subtype_in_env_existential(jl_value_t *x, jl_value_t *y, jl_stenv_t *e) JL_CANSAFEPOINT
{
    if (x == jl_bottom_type || y == (jl_value_t*)jl_any_type ||
        (!jl_has_dangling_tvarrefs(x) && !jl_has_dangling_tvarrefs(y) && obviously_in_union(y, x)))
        return 1;
    int8_t *rs = (int8_t*)alloca(current_env_length(e));
    jl_varbinding_t *v = e->vars;
    int n = 0;
    while (v != NULL) {
        rs[n++] = v->existential;
        v->existential = 1;
        v = v->prev;
    }
    int issub = subtype_in_env(x, y, e);
    n = 0; v = e->vars;
    while (v != NULL) {
        v->existential = rs[n++];
        v = v->prev;
    }
    return issub;
}

// subtype, treating all vars as nonexistential
static int subtype_in_env_nonexistential(jl_value_t *x, jl_value_t *y, jl_stenv_t *e) JL_CANSAFEPOINT
{
    if (x == jl_bottom_type || y == (jl_value_t*)jl_any_type || obviously_in_union(y, x))
        return 1;
    int8_t *rs = (int8_t*)alloca(current_env_length(e));
    jl_varbinding_t *v = e->vars;
    int n = 0;
    while (v != NULL) {
        rs[n++] = v->existential;
        v->existential = 0;
        v = v->prev;
    }
    int issub = subtype_in_env(x, y, e);
    n = 0; v = e->vars;
    while (v != NULL) {
        v->existential = rs[n++];
        v = v->prev;
    }
    return issub;
}

// See if var y is reachable from x via bounds; used to avoid cycles.
static int _reachable_var(jl_value_t *x, jl_tvar_t *y, jl_stenv_t *e, jl_typeenv_t *log) JL_CANSAFEPOINT
{
    if (in_union(x, (jl_value_t*)y))
        return 1;
    if (jl_is_uniontype(x) || jl_is_intersecttype(x))
        return _reachable_var(((jl_uniontype_t *)x)->a, y, e, log) ||
               _reachable_var(((jl_uniontype_t *)x)->b, y, e, log);
    if (!jl_is_typevar(x))
        return 0;
    jl_typeenv_t *t = log;
    while (t != NULL) {
        if (x == (jl_value_t *)t->var)
            return 0;
        t = t->prev;
    }
    int innervar = 0;
    jl_varbinding_t *xv = lookup_binding(e, (jl_tvar_t*)x, &innervar);
    if (xv == NULL && !innervar)
        return 0;
    // cycles can run through a binder's declared bound, so the substituted
    // view must be chased (a raw reference would hide the edge)
    jl_value_t *lb = xv == NULL ? ((jl_tvar_t*)x)->lb : binding_lb(e, xv);
    jl_value_t *ub = xv == NULL ? ((jl_tvar_t*)x)->ub : binding_ub(e, xv);
    jl_typeenv_t newlog = { (jl_tvar_t*)x, NULL, log };
    return _reachable_var(ub, y, e, &newlog) || _reachable_var(lb, y, e, &newlog);
}

static int reachable_var(jl_value_t *x, jl_tvar_t *y, jl_stenv_t *e) JL_CANSAFEPOINT
{
    return _reachable_var(x, y, e, NULL);
}

// check whether setting v == t implies v == SomeType{v}, which is unsatisfiable.
static int check_unsat_bound(jl_value_t *t, jl_tvar_t *v, jl_stenv_t *e) JL_NOTSAFEPOINT
{
    if (var_occurs_inside(t, v, 0, 0))
        return 1;
    jl_varbinding_t *btemp = e->vars;
    while (btemp != NULL) {
        if (lterm_closed1(btemp->lbs) == (jl_value_t*)v && lterm_closed1(btemp->ubs) == (jl_value_t*)v &&
            var_occurs_inside(t, btemp->var, 0, 0))
            return 1;
        btemp = btemp->prev;
    }
    return 0;
}


static int intersect_var_ccheck_in_env(jl_value_t *xlb, jl_value_t *xub, jl_value_t *ylb, jl_value_t *yub, jl_stenv_t *e, int flip) JL_CANSAFEPOINT;

static jl_value_t *intersect_var(jl_tvar_t *b, jl_value_t *a, jl_stenv_t *e, int8_t R, jl_param_pos_t param) JL_CANSAFEPOINT
{
    // `a` is a fragment of the other side's term; it must be re-expressed in
    // variable form before it can be compared against (or stored into)
    // variable bounds, which outlive its position
    if (jl_has_dangling_tvarrefs(a))
        a = frame_substitute(a, R ? e->Lframe : e->Rframe, e);
    int innervar = 0;
    jl_varbinding_t *bb = lookup_binding(e, b, &innervar);
    if (bb == NULL) {
        if (innervar)
            return R ? intersect_aside(a, b->ub, e, 0) : intersect_aside(b->ub, a, e, 0);
        if (singleton_typevar_subtype(b, a))
            return (jl_value_t*)b;
        if (subtype_singleton_typevar(a, b))
            return a;
        return jl_bottom_type;
    }
    // the accumulating (variable-form) view of the bounds; writes below go to
    // the fields directly
    jl_value_t *bb_lb = binding_lb(e, bb), *bb_ub = binding_ub(e, bb);
    if (jl_has_dangling_tvarrefs(bb_lb) || jl_has_dangling_tvarrefs(bb_ub)) {
        // the binder of a detached fragment: its bounds reference binders
        // outside the query, so they support no bound reasoning -- over-
        // approximate the meet by the other side (cf. `var_lt_`)
        return a;
    }
    if (reachable_var(bb_lb, b, e) || reachable_var(bb_ub, b, e))
        return a;
    if (bb_lb == bb_ub && jl_is_typevar(bb_lb))
        return R ? intersect(a, bb_lb, e, param) : intersect(bb_lb, a, e, param);
    if (!jl_is_type(a) && !jl_is_typevar(a))
        return set_var_to_const(bb, a, e, R);
    if (param == PARAM_INVARIANT) {
        jl_value_t *ub = NULL;
        JL_GC_PUSH1(&ub);
        if (!jl_has_free_typevars(a)) {
            if (R) flip_offset(e);
            int ccheck = intersect_var_ccheck_in_env(bb_lb, bb_ub, a, a, e, !R);
            if (R) flip_offset(e);
            if (!ccheck) {
                JL_GC_POP();
                return jl_bottom_type;
            }
            ub = a;
        }
        else {
            if (jl_subtype(a, bb_ub)) {
                ub = a;
            }
            else {
                int old_triangular = e->triangular;
                // Memorize the side we meet the triangular intersection on.
                e->triangular = R + 1;
                ub = R ? intersect_aside(a, bb_ub, e, bb->depth0) : intersect_aside(bb_ub, a, e, bb->depth0);
                e->triangular = old_triangular;
            }
            jl_savedenv_t se;
            save_env(e, &se, 1);
            int issub = subtype_in_env_existential(bb_lb, ub, e);
            restore_env(e, &se, 1);
            free_env(&se);
            if (!issub) {
                JL_GC_POP();
                return jl_bottom_type;
            }
        }
        if (ub != (jl_value_t*)b) {
            if (jl_has_free_typevars(ub)) {
                if (check_unsat_bound(ub, b, e)) {
                    JL_GC_POP();
                    return jl_bottom_type;
                }
            }
            binding_set_ub(e, bb, ub);
            if ((jl_is_uniontype(ub) && !jl_is_uniontype(a)) ||
                (jl_is_unionall(ub) && !jl_is_unionall(a)))
                ub = (jl_value_t*)b;
            else
                binding_set_lb(e, bb, ub);
        }
        JL_GC_POP();
        return ub;
    }
    jl_value_t *ub = R ? intersect_aside(a, bb_ub, e, bb->depth0) : intersect_aside(bb_ub, a, e, bb->depth0);
    if (ub == jl_bottom_type)
        return jl_bottom_type;
    if (e->triangular && ((e->triangular - 1) != R)) {
        if (check_unsat_bound(ub, b, e))
            return jl_bottom_type;
        set_bound(e, bb, 1, ub, b);
        return (jl_value_t*)b;
    }
    if (bb->constraintkind == 1) {
        if (!jl_is_some_Type(ub) && !jl_is_uniontype(ub) && !jl_is_unionall(ub)) {
            // this branch is a fast path if there are no `Type`s and not needed for correctness
            set_bound(e, bb, 1, ub, b);
            return (jl_value_t*)b;
        }
        jl_value_t *ub2 = NULL;
        JL_GC_PUSH2(&ub, &ub2);
        ub2 = widen_Type_to_union(ub, bb_ub, e);
        if (ub2 != ub) {
            set_bound(e, bb, 1, ub2, b);
            if (jl_is_concrete_type(ub2)) {
                // all members widened to the same concrete kind
                JL_GC_POP();
                return ub;
            }
            bb->widened_to_kind = 1;
            JL_GC_POP();
            return (jl_value_t*)b;
        }
        set_bound(e, bb, 1, ub, b);
        JL_GC_POP();
        return (jl_value_t*)b;
    }
    else if (bb->constraintkind == 0) {
        JL_GC_PUSH1(&ub);
        if (!jl_is_typevar(a) && try_subtype_in_env_nonexistential(bb_ub, a, e)) {
            JL_GC_POP();
            return (jl_value_t*)b;
        }
        JL_GC_POP();
        return ub;
    }
    assert(bb->constraintkind == 2);
    if (ub == a && bb_lb != jl_bottom_type)
        return ub;
    if (jl_egal(bb_ub, bb_lb))
        return ub;
    if (is_leaf_bound(ub)) {
        JL_GC_PUSH1(&ub);
        set_bound(e, bb, 0, ub, b);
        JL_GC_POP();
    }
    // TODO: can we improve this bound by pushing a new variable into the environment
    // and adding that to the lower bound of our variable?
    //jl_value_t *ntv = NULL;
    //JL_GC_PUSH2(&ntv, &ub);
    //if (bb->innervars == NULL)
    //    bb->innervars = jl_alloc_array_1d(jl_array_any_type, 0);
    //ntv = (jl_value_t*)jl_new_typevar(b->name, bb->lb, ub);
    //jl_array_ptr_1d_push(bb->innervars, ntv);
    //jl_value_t *lb = simple_join(b->lb, ntv);
    //JL_GC_POP();
    //bb->lb = lb;
    return ub;
}

// test whether `var` occurs inside constructors. `want_inv` tests only inside
// invariant constructors. `inside` means we are currently inside a constructor of the
// requested kind.
static int var_occurs_inside(jl_value_t *v, jl_tvar_t *var, int inside, int want_inv) JL_NOTSAFEPOINT
{
    if (v == (jl_value_t*)var) {
        return inside;
    }
    else if (jl_is_uniontype(v) || jl_is_intersecttype(v)) {
        return var_occurs_inside(((jl_uniontype_t*)v)->a, var, inside, want_inv) ||
            var_occurs_inside(((jl_uniontype_t*)v)->b, var, inside, want_inv);
    }
    else if (jl_is_unionall(v)) {
        jl_unionall_t *ua = (jl_unionall_t*)v;
        if (var_occurs_inside(ua->lb, var, inside, want_inv) || var_occurs_inside(ua->ub, var, inside, want_inv))
            return 1;
        return var_occurs_inside(ua->body, var, inside, want_inv);
    }
    else if (jl_is_vararg(v)) {
        jl_vararg_t *vm = (jl_vararg_t*)v;
        if (vm->T) {
            if (var_occurs_inside(vm->T, var, inside || !want_inv, want_inv))
                return 1;
            return vm->N && var_occurs_inside(vm->N, var, 1, want_inv);
        }
    }
    else if (jl_is_some_Type(v)) {
        return var_occurs_inside(jl_some_Type_T(v), var, 1, want_inv);
    }
    else if (jl_is_datatype(v)) {
        size_t i;
        int istuple = jl_is_tuple_type(v);
        for (i=0; i < jl_nparams(v); i++) {
            int ins_i = inside || !want_inv || !istuple;
            if (var_occurs_inside(jl_tparam(v,i), var, ins_i, want_inv))
                return 1;
        }
    }
    return 0;
}

static jl_value_t *omit_bad_union(jl_value_t *u, jl_tvar_t *t) JL_CANSAFEPOINT
{
    if (!jl_has_typevar(u, t))
        return u; // return u if possible as many checks use `==`.
    jl_value_t *res = NULL;
    if (jl_is_unionall(u)) {
        jl_unionall_t *ua = (jl_unionall_t *)u;
        jl_value_t *ub = ua->ub, *body = ua->body;
        JL_GC_PUSH2(&ub, &body);
        body = omit_bad_union(body, t);
        if (!jl_tvarref_occurs(body, 1)) {
            res = jl_shift_dangling_refs(body, -1);
        }
        else if (jl_has_typevar(ua->lb, t)) {
            res = jl_bottom_type;
        }
        else {
            ub = omit_bad_union(ub, t);
            if (ub == jl_bottom_type && ua->lb != ub) {
                res = jl_bottom_type;
            }
            else if (obviously_egal(ua->lb, ub)) {
                // the binder is pinned; substitute its bound for it
                jl_value_t *tmp = jl_new_unionall_raw(ua->name, ua->lb, ub, body);
                JL_GC_PUSH1(&tmp);
                res = jl_instantiate_unionall_nothrow((jl_unionall_t*)tmp, ub, 2);
                JL_GC_POP();
                if (res == NULL)
                    res = jl_bottom_type;
            }
            else {
                // only the bound changed; the body's references are unaffected
                res = jl_new_unionall_raw(ua->name, ua->lb, ub, body);
            }
        }
        JL_GC_POP();
    }
    else if (jl_is_uniontype(u)) {
        jl_value_t *a = ((jl_uniontype_t *)u)->a;
        jl_value_t *b = ((jl_uniontype_t *)u)->b;
        JL_GC_PUSH2(&a, &b);
        a = omit_bad_union(a, t);
        b = omit_bad_union(b, t);
        res = simple_join(a, b);
        JL_GC_POP();
    }
    else {
        res = jl_bottom_type;
    }
    assert(res != NULL);
    return res;
}

// TODO: fuse with reachable_var?
static int has_typevar_via_flatten_env(jl_value_t *x, jl_tvar_t *t, jl_ivarbinding_t *allvars, int8_t *checked) JL_NOTSAFEPOINT
{
    if (jl_is_unionall(x)) {
        if (has_typevar_via_flatten_env(((jl_unionall_t *)x)->lb, t, allvars, checked) ||
            has_typevar_via_flatten_env(((jl_unionall_t *)x)->ub, t, allvars, checked))
            return 1;
        return has_typevar_via_flatten_env(((jl_unionall_t *)x)->body, t, allvars, checked);
    }
    else if (jl_is_uniontype(x)) {
        return has_typevar_via_flatten_env(((jl_uniontype_t *)x)->a, t, allvars, checked) ||
            has_typevar_via_flatten_env(((jl_uniontype_t *)x)->b, t, allvars, checked);
    }
    else if (jl_is_vararg(x)) {
        jl_vararg_t *v = (jl_vararg_t *)x;
        return (v->T && has_typevar_via_flatten_env(v->T, t, allvars, checked)) ||
            (v->N && has_typevar_via_flatten_env(v->N, t, allvars, checked));
    }
    else if (jl_is_datatype(x)) {
        for (size_t i = 0; i < jl_nparams(x); i++) {
            if (has_typevar_via_flatten_env(jl_tparam(x, i), t, allvars, checked))
                return 1;
        }
        return 0;
    }
    else if (jl_is_typevar(x)) {
        if (t == (jl_tvar_t *)x)
            return 1;
        size_t ind = 0;
        jl_ivarbinding_t *itemp = allvars;
        while (itemp && *itemp->var != (jl_tvar_t *)x)
        {
            ind++;
            itemp = itemp->next;
        }
        if (itemp == NULL || checked[ind])
            return 0;
        checked[ind] = 1;
        return has_typevar_via_flatten_env(*itemp->lb, t, allvars, checked) ||
            has_typevar_via_flatten_env(*itemp->ub, t, allvars, checked);
    }
    return 0;
}

// Caller might not have rooted `res`
static jl_value_t *finish_unionall(jl_value_t *res JL_MAYBE_UNROOTED, jl_varbinding_t *vb, jl_unionall_t *u, jl_stenv_t *e) JL_CANSAFEPOINT
{
    jl_value_t *varval = NULL, *ilb = NULL, *iub = NULL, *nivar = NULL;
    jl_tvar_t *newvar = vb->var, *ivar = NULL;
    // the binding's (forced) bounds, as types; written back at the end
    jl_value_t *vlb = NULL, *vub = NULL;
    JL_GC_PUSH8(&res, &newvar, &ivar, &nivar, &ilb, &iub, &vlb, &vub);
    vlb = binding_lb(e, vb);
    vub = binding_ub(e, vb);
    // Note: `Intersect` is subtype accounting only and is widened away before
    // it leaves the subtype path (see `subtype_unionall`), so the intersection
    // result here never contains one.
    assert(!jl_is_intersecttype(vub));
    // try to reduce var to a single value
    if (jl_is_long(vub) && jl_is_typevar(vlb)) {
        varval = vub;
    }
    else if (obviously_egal(vlb, vub)) {
        // given x<:T<:x, substitute x for T
        varval = vub;
    }
    // TODO: `vb.occurs_cov == 1`, we could also substitute Tuple{<:X} => Tuple{X},
    // but it may change some ambiguity errors so we don't need to do it yet.
    else if (cov_count(vb) && is_leaf_bound(vub) && !jl_has_free_typevars(vub)) {
        // replace T<:x with x in covariant position when possible
        varval = vub;
    }

    if (vb->intvalued) {
        if ((varval && jl_is_long(varval)) ||
            (vlb == jl_bottom_type && vub == (jl_value_t*)jl_any_type) ||
            (jl_is_typevar(vlb) && vub == vlb)) {
            // int-valued typevar must either be an Int, or have Bottom-Any bounds,
            // or be set equal to another typevar.
        }
        else {
            JL_GC_POP();
            return jl_bottom_type;
        }
    }

    // TODO: this can prevent us from matching typevar identities later
    if (!varval && (vlb != vb->var->lb || vub != vb->var->ub))
        newvar = jl_new_typevar(vb->var->name, vlb, vub);
    // n.b. vb->var is the opened variable, whose lb/ub are the binder's pristine bounds

    // flatten all innervar into a (reversed) list; the environment's
    // bindings get rooted slots for their bounds as types too
    size_t icount = 0, nslots = 0;
    if (vb->innervars)
        icount += jl_array_nrows(vb->innervars);
    for (jl_varbinding_t *btemp = e->vars; btemp != NULL; btemp = btemp->prev) {
        if (btemp->innervars != NULL)
            icount += jl_array_nrows(btemp->innervars);
        nslots += 2;
    }
    nslots += 3*icount;
    jl_svec_t *p = NULL;
    jl_value_t **iparams;
    jl_value_t **roots;
    JL_GC_PUSHARGS(roots, nslots < 66 ? nslots : 1);
    if (nslots < 66) {
        iparams = roots;
    }
    else {
        p = jl_alloc_svec(nslots);
        roots[0] = (jl_value_t*)p;
        iparams = jl_svec_data(p);
    }
    jl_ivarbinding_t *allvars = NULL;
    size_t niparams = 0;
    if (vb->innervars) {
        for (size_t i = 0; i < jl_array_nrows(vb->innervars); i++) {
            jl_tvar_t *ivar = (jl_tvar_t *)jl_array_ptr_ref(vb->innervars, i);
            jl_ivarbinding_t *inew = (jl_ivarbinding_t *)alloca(sizeof(jl_ivarbinding_t));
            inew->var = (jl_tvar_t **)&iparams[niparams++]; *inew->var = ivar;
            inew->lb = &iparams[niparams++]; *inew->lb = ivar->lb;
            inew->ub = &iparams[niparams++]; *inew->ub = ivar->ub;
            inew->b = NULL;
            inew->root = vb;
            inew->next = allvars;
            allvars = inew;
        }
    }
    for (jl_varbinding_t *btemp = e->vars; btemp != NULL; btemp = btemp->prev) {
        jl_ivarbinding_t *inew = (jl_ivarbinding_t *)alloca(sizeof(jl_ivarbinding_t));
        inew->var = &btemp->var;
        inew->lb = &iparams[niparams++]; *inew->lb = binding_lb(e, btemp);
        inew->ub = &iparams[niparams++]; *inew->ub = binding_ub(e, btemp);
        inew->b = btemp;
        inew->root = btemp;
        inew->next = allvars;
        allvars = inew;
        if (btemp->innervars) {
            for (size_t i = 0; i < jl_array_nrows(btemp->innervars); i++) {
                jl_tvar_t *ivar = (jl_tvar_t *)jl_array_ptr_ref(btemp->innervars, i);
                jl_ivarbinding_t *inew = (jl_ivarbinding_t *)alloca(sizeof(jl_ivarbinding_t));
                inew->var = (jl_tvar_t **)&iparams[niparams++]; *inew->var = ivar;
                inew->lb = &iparams[niparams++]; *inew->lb = ivar->lb;
                inew->ub = &iparams[niparams++]; *inew->ub = ivar->ub;
                inew->b = NULL;
                inew->root = btemp;
                inew->next = allvars;
                allvars = inew;
            }
        }
    }

    // remove/replace/rewrap free occurrences of this var in the environment
    int wrapped = 0;
    jl_ivarbinding_t *pwrap = NULL;
    int vcount = icount + current_env_length(e);
    int8_t *checked = (int8_t *)alloca(vcount);
    for (jl_ivarbinding_t *btemp = allvars, *pbtemp = NULL; btemp != NULL; btemp = btemp->next) {
        int bdepth0 = btemp->root->depth0;
        int innerflag = 0;
        ivar = *btemp->var;
        ilb = *btemp->lb;
        iub = *btemp->ub;
        if (jl_has_typevar(ilb, vb->var)) {
            assert(btemp->root->var == ivar || bdepth0 == vb->depth0);
            if (vlb == (jl_value_t*)ivar) {
                JL_GC_POP();
                JL_GC_POP();
                return jl_bottom_type;
            }
            if (varval) {
                JL_TRY {
                    *btemp->lb = jl_substitute_var(ilb, vb->var, varval);
                }
                JL_CATCH {
                    res = jl_bottom_type;
                }
            }
            else if (ilb == (jl_value_t*)vb->var) {
                *btemp->lb = vlb;
            }
            else {
                innerflag |= 1;
            }
        }
        if (jl_has_typevar(iub, vb->var)) {
            assert(btemp->root->var == ivar || bdepth0 == vb->depth0);
            if (vub == (jl_value_t*)ivar) {
                *btemp->ub = omit_bad_union(iub, vb->var);
                if (*btemp->ub == jl_bottom_type && *btemp->ub != *btemp->lb) {
                    JL_GC_POP();
                    JL_GC_POP();
                    return jl_bottom_type;
                }
            }
            if (varval) {
                iub = jl_substitute_var_nothrow(iub, vb->var, varval, 2);
                if (iub == NULL)
                    res = jl_bottom_type;
                else
                    *btemp->ub = iub;
            }
            else if (iub == (jl_value_t*)vb->var) {
                // TODO: this loses some constraints, such as in this test, where we replace T4<:S3 (e.g. T4==S3 since T4 only appears covariantly once) with T4<:Any
                // a = Tuple{Float64,T3,T4} where T4 where T3
                // b = Tuple{S2,Tuple{S3},S3} where S2 where S3
                // Tuple{Float64, T3, T4} where {S3, T3<:Tuple{S3}, T4<:S3}
                *btemp->ub = vub;
            }
            else {
                innerflag |= 2;
            }
        }
        if (innerflag) {
            memset(checked, 0, vcount);
            if (btemp->root == vb || bdepth0 != vb->depth0 ||
                has_typevar_via_flatten_env(vlb, ivar, allvars, checked) ||
                has_typevar_via_flatten_env(vub, ivar, allvars, checked)) {
                if (innerflag & 1)
                    *btemp->lb = jl_type_unionall(vb->var, ilb);
                if (innerflag & 2)
                    *btemp->ub = jl_type_unionall(vb->var, iub);
            }
            else {
                assert(btemp->root != vb);
                // if our variable is T, and some outer variable has constraint S = Ref{T},
                // move the `where T` outside `where S` instead of putting it here. issue #21243.
                if (newvar != vb->var) {
                    if (innerflag & 1)
                        *btemp->lb = jl_substitute_var(ilb, vb->var, (jl_value_t*)newvar);
                    if (innerflag & 2)
                        *btemp->ub = jl_substitute_var(iub, vb->var, (jl_value_t*)newvar);
                }
                if (!wrapped)
                    pwrap = pbtemp;
                wrapped = 1;
            }
            assert((jl_value_t*)ivar != *btemp->lb);
            assert((jl_value_t*)ivar != *btemp->ub);
        }
        pbtemp = btemp;
    }

    // Insert the newvar into the (reversed) var list if needed.
    if (wrapped) {
        jl_ivarbinding_t *wrap = pwrap == NULL ? allvars : pwrap->next;
        jl_ivarbinding_t *inew = (jl_ivarbinding_t *)alloca(sizeof(jl_ivarbinding_t));
        inew->var = &newvar;
        inew->lb = &newvar->lb;
        inew->ub = &newvar->ub;;
        inew->b = NULL;
        inew->root = wrap->root;
        inew->next = wrap;
        if (pwrap != NULL)
            pwrap->next = inew;
        else
            allvars = inew;
        vcount++;
    }

    // Re-sort the innervar inside the (reversed) var list.
    // `jl_has_typevar` is used as the partial-ordering predicate.
    // If this is slow, we could possibly switch to a simpler graph sort, such as Tarjan's SCC.
    if (icount > 0) {
        jl_ivarbinding_t *pib1 = NULL;
#ifndef NDEBUG
        size_t sort_count = 0;
#endif
        while (1) {
            jl_ivarbinding_t *ib1 = pib1 == NULL ? allvars : pib1->next;
            if (ib1 == NULL) break;
            assert((++sort_count) <= (vcount * (vcount + 1)) >> 1);
            int lbfree = jl_has_free_typevars(*ib1->lb);
            int ubfree = jl_has_free_typevars(*ib1->ub);
            if (lbfree || ubfree) {
                int changed = 0;
                jl_ivarbinding_t *pib2 = ib1, *ib2 = ib1->next;
                while (ib2 != NULL) {
                    int isinnervar = ib2->root->var != *ib2->var;
                    if (isinnervar && ib1->root->depth0 == ib2->root->depth0 &&
                        ((lbfree && jl_has_typevar(*ib1->lb, *ib2->var)) ||
                         (ubfree && jl_has_typevar(*ib1->ub, *ib2->var)))) {
                        pib2->next = ib2->next;
                        ib2->next = ib1;
                        ib2->root = ib1->root;
                        if (pib1)
                            pib1->next = ib2;
                        else
                            allvars = ib2;
                        changed = 1;
                        break;
                    }
                    pib2 = ib2;
                    ib2 = ib2->next;
                }
                if (changed) continue;
            }
            pib1 = ib1;
        }
    }

    // Freeze the innervars' lb/ub and perform substitution if needed.
    for (jl_ivarbinding_t *btemp1 = allvars; btemp1 != NULL; btemp1 = btemp1->next) {
        ivar = *btemp1->var;
        ilb = *btemp1->lb;
        iub = *btemp1->ub;
        int isinnervar = btemp1->root->var != ivar;
        if (isinnervar && (ivar->lb != ilb || ivar->ub != iub)) {
            nivar = (jl_value_t *)jl_new_typevar(ivar->name, ilb, iub);
            if (jl_has_typevar(res, ivar))
                res = jl_substitute_var(res, ivar, nivar);
            for (jl_ivarbinding_t *btemp2 = btemp1->next; btemp2 != NULL; btemp2 = btemp2->next) {
                ilb = *btemp2->lb;
                iub = *btemp2->ub;
                if (jl_has_typevar(ilb, ivar))
                    *btemp2->lb = jl_substitute_var(ilb, ivar, nivar);
                if (jl_has_typevar(iub, ivar))
                    *btemp2->ub = jl_substitute_var(iub, ivar, nivar);
            }
            if (!wrapped && !varval) {
                // newvar also needs bounds substitution.
                if (jl_has_typevar(vlb, ivar))
                    vlb = jl_substitute_var(vlb, ivar, nivar);
                if (jl_has_typevar(vub, ivar))
                    vub = jl_substitute_var(vub, ivar, nivar);
            }
            *btemp1->var = (jl_tvar_t *)nivar;
        }
    }

    // write the (possibly rewritten) bounds back into the environment's bindings
    for (jl_ivarbinding_t *btemp = allvars; btemp != NULL; btemp = btemp->next) {
        if (btemp->b != NULL) {
            binding_set_lb(e, btemp->b, *btemp->lb);
            binding_set_ub(e, btemp->b, *btemp->ub);
        }
    }

    // Switch back the innervars' storage.
    while (1) {
        jl_ivarbinding_t *btemp = allvars;
        jl_varbinding_t *root = btemp ? btemp->root : vb;
        size_t icount = 0;
        while (btemp && btemp->root == root) {
            btemp = btemp->next;
            icount++;
        }
        if (root != vb) icount--;
        if (root->innervars != NULL) {
            jl_array_t *rinnervars = root->innervars;
            JL_GC_PROMISE_ROOTED(rinnervars);
            size_t len = jl_array_nrows(rinnervars);
            if (icount > len)
                jl_array_grow_end(rinnervars, icount - len);
            if (icount < len)
                jl_array_del_end(rinnervars, len - icount);
        }
        else if (icount > 0) {
            root->innervars = jl_alloc_array_1d(jl_array_any_type, icount);
            stenv_root(e, (jl_value_t*)root->innervars);
        }
        btemp = allvars;
        for (size_t i = icount; i > 0; i--) {
            jl_array_ptr_set(root->innervars, i - 1, (jl_value_t*)*btemp->var);
            btemp = btemp->next;
        }
        if (root == vb) break;
        assert(*btemp->var == root->var);
        allvars = btemp->next;
        assert(allvars == NULL || allvars->root != root);
    }
    JL_GC_POP();

    // if `v` still occurs, re-wrap body in `UnionAll v` or eliminate the UnionAll
    if (jl_has_typevar(res, vb->var)) {
        if (varval) {
            // you can construct `T{x} where x` even if T's parameter is actually
            // limited. in that case we might get an invalid instantiation here.
            res = jl_substitute_var_nothrow(res, vb->var, varval, 2);
            // simplify chains of UnionAlls where bounds become equal
            while (res != NULL && jl_is_unionall(res) && obviously_egal(((jl_unionall_t*)res)->lb,
                                                         ((jl_unionall_t*)res)->ub)) {
                jl_unionall_t * ures = (jl_unionall_t *)res;
                res = jl_instantiate_unionall_nothrow(ures, ures->lb, 2);
            }
            if (res == NULL)
                res = jl_bottom_type;
        }
        else {
            // re-fresh newvar if bounds changed.
            if (vlb != newvar->lb || vub != newvar->ub)
                newvar = jl_new_typevar(newvar->name, vlb, vub);
            if (newvar != vb->var)
                res = jl_substitute_var(res, vb->var, (jl_value_t*)newvar);
            varval = (jl_value_t*)newvar;
            if (!wrapped)
                res = jl_type_unionall((jl_tvar_t*)newvar, res);
        }
    }

    if (vb->innervars != NULL) {
        for (size_t i = 0; i < jl_array_nrows(vb->innervars); i++) {
            jl_tvar_t *var = (jl_tvar_t*)jl_array_ptr_ref(vb->innervars, i);
            res = jl_type_unionall(var, res);
        }
    }

    binding_set_lb(e, vb, vlb);
    binding_set_ub(e, vb, vub);
    JL_GC_POP();
    return res;
}

static jl_value_t *intersect_unionall_(jl_value_t *t, jl_unionall_t *u, jl_stenv_t *e, int8_t R, jl_param_pos_t param, jl_varbinding_t *vb) JL_CANSAFEPOINT
{
    jl_varbinding_t *btemp = e->vars;
    int envsize = 0;
    while (btemp != NULL) {
        envsize++;
        if (envsize > 120) {
            vb->limited = 1;
            // the operand becomes the (over-approximate) result; it must not
            // carry this walk's raw references
            if (jl_has_dangling_tvarrefs(t))
                t = frame_substitute(t, R ? e->Lframe : e->Rframe, e);
            return t;
        }
        btemp = btemp->prev;
    }
    // the body is walked natively: occurrences stay de Bruijn references and
    // resolve positionally through the side's frame chain
    JL_GC_PUSH1(&u);
    assert(vb->frame_prev == (R ? e->Rframe : e->Lframe));
    stenv_enter_binding(e, vb, R);
    jl_value_t *res;
    if (R) {
        e->envidx++;
        res = intersect(t, u->body, e, param);
        e->envidx--;
    }
    else {
        res = intersect(u->body, t, e, param);
    }
    if (R)
        e->Rframe = vb->frame_prev;
    else
        e->Lframe = vb->frame_prev;
    if (res != jl_bottom_type) {
        // everything below (the diagonal checks, `finish_unionall`, the
        // re-intersection bookkeeping) consumes the materialized view. The
        // walk is over: no environment save can see this binding's raw
        // bounds anymore, so the fields may be written in place.
        binding_var(e, vb);
        binding_force_bounds(e, vb);
    }
    vb->concrete |= (cov_count(vb) > 1 && is_leaf_binder(vb) &&
                     !vb->body_occurs_inv);

    // handle the "diagonal dispatch" rule, which says that a type var occurring more
    // than once, and only in covariant position, is constrained to concrete types. E.g.
    //  ( Tuple{Int, Int}    <: Tuple{T, T} where T) but
    // !( Tuple{Int, String} <: Tuple{T, T} where T)
    // Then check concreteness by checking that the lower bound is not an abstract type.
    if (res != jl_bottom_type && vb->concrete) {
        jl_value_t *vlb = binding_lb(e, vb);
        if (jl_is_typevar(vlb)) {
        }
        else if (!is_leaf_bound(vlb)) {
            // in the existential direction a `Type{X}` member of the bound
            // stands for its (nonempty) tag-homogeneous slice, so check the
            // tag-widened bound before rejecting (see `widen_Type_if_concrete`)
            jl_value_t *wlb = widen_Type_if_concrete(vlb, e, NULL, vb->frame_prev, 1);
            if (!is_leaf_bound(wlb))
                res = jl_bottom_type;
        }
    }

    // Propagate "deeper-popped tvar leaks into outer bounds" taint upward.
    // See the matching block in `subtype_unionall`. (A never-materialized
    // variable cannot occur in any outer bound.)
    for (jl_varbinding_t *btemp = vb->var == NULL ? NULL : vb->prev; btemp; btemp = btemp->prev) {
        if ((vb->depth0 > btemp->depth0 || vb->tainted_inner) &&
            (lterm_has_typevar(btemp->lbs, vb->var) || lterm_has_typevar(btemp->ubs, vb->var))) {
            btemp->tainted_inner = 1;
        }
    }

    stenv_leave_binding(e, vb, R);

    if (res != jl_bottom_type) {
        if (lterm_closed1(vb->ubs) == jl_bottom_type && cov_count(vb)) {
            // T=Bottom in covariant position
            res = jl_bottom_type;
        }
        else if (jl_has_typevar(binding_lb(e, vb), vb->var)) {
            // fail on circular constraints
            res = jl_bottom_type;
        }
        else {
            JL_GC_PUSH1(&res);
            binding_set_ub(e, vb, omit_bad_union(binding_ub(e, vb), vb->var));
            JL_GC_POP();
            if (lterm_closed1(vb->ubs) == jl_bottom_type && vb->lbs != NULL)
                res = jl_bottom_type;
        }
    }
    if (res != jl_bottom_type)
        // res is rooted by callee
        res = finish_unionall(res, vb, u, e);
    JL_GC_POP();
    return res;
}

// positional twin of upstream's `always_occurs_cov`: does the binder `d`
// levels out have a guaranteed covariant occurrence in `v`?
static int tvarref_always_occurs_cov(jl_value_t *v, size_t d, jl_param_pos_t param) JL_NOTSAFEPOINT
{
    if (param == PARAM_INVARIANT) {
        return 0;
    }
    else if (jl_is_tvarref(v)) {
        return jl_tvarref_depth(v) == d && param == PARAM_COVARIANT;
    }
    else if (jl_is_uniontype(v)) {
        return tvarref_always_occurs_cov(((jl_uniontype_t*)v)->a, d, param) &&
               tvarref_always_occurs_cov(((jl_uniontype_t*)v)->b, d, param);
    }
    else if (jl_is_unionall(v)) {
        jl_unionall_t *ua = (jl_unionall_t*)v;
        // the body is one frame further in
        return tvarref_always_occurs_cov(ua->body, d + 1, param);
    }
    else if (jl_is_vararg(v)) {
        jl_vararg_t *vm = (jl_vararg_t*)v;
        return vm->T && tvarref_always_occurs_cov(vm->T, d, param);
    }
    else if (jl_is_some_Type(v)) {
        return tvarref_always_occurs_cov(jl_some_Type_T(v), d, PARAM_INVARIANT);
    }
    else if (jl_is_datatype(v)) {
        jl_param_pos_t nparam = jl_is_tuple_type(v) ? PARAM_COVARIANT : param;
        for (size_t i = 0; i < jl_nparams(v); i++) {
            if (tvarref_always_occurs_cov(jl_tparam(v, i), d, nparam))
                return 1;
        }
    }
    return 0;
}

// positional twin of upstream's `never_occurs_in_inside_ub`: does the binder
// `d` levels out occur nowhere inside an inner binder's upper bound?
static int tvarref_never_occurs_in_inside_ub(jl_value_t *v, size_t d) JL_NOTSAFEPOINT
{
    if (jl_is_uniontype(v)) {
        return tvarref_never_occurs_in_inside_ub(((jl_uniontype_t*)v)->a, d) &&
               tvarref_never_occurs_in_inside_ub(((jl_uniontype_t*)v)->b, d);
    }
    else if (jl_is_unionall(v)) {
        jl_unionall_t *ua = (jl_unionall_t*)v;
        return !jl_tvarref_occurs(ua->ub, d) && tvarref_never_occurs_in_inside_ub(ua->body, d + 1);
    }
    else if (jl_is_vararg(v)) {
        jl_vararg_t *vm = (jl_vararg_t*)v;
        return (vm->T == NULL || tvarref_never_occurs_in_inside_ub(vm->T, d)) &&
               (vm->N == NULL || tvarref_never_occurs_in_inside_ub(vm->N, d));
    }
    else if (jl_is_some_Type(v)) {
        return tvarref_never_occurs_in_inside_ub(jl_some_Type_T(v), d);
    }
    else if (jl_is_datatype(v)) {
        for (size_t i = 0; i < jl_nparams(v); i++) {
            if (!tvarref_never_occurs_in_inside_ub(jl_tparam(v, i), d))
                return 0;
        }
        return 1;
    }
    // conservative for internal nodes (TypeEq, TypeApp, Intersect); plain
    // values contain no references
    return jl_is_tvarref(v) || !jl_tvarref_occurs(v, d);
}

// construction-time entry for the `JL_UNIONALL_ALWAYSCOV` flag (the enum
// `jl_param_pos_t` is private to this file): the binder's variable always
// occurs covariantly, and never inside an inner binder's upper bound
// (setting `constraintkind = 1` for a covariant variable is invalid if it
// also occurs in a triangular position)
int jl_tvarref_always_occurs_cov_top(jl_value_t *body) JL_NOTSAFEPOINT
{
    return tvarref_always_occurs_cov(body, 1, PARAM_COVARIANT) &&
           tvarref_never_occurs_in_inside_ub(body, 1);
}

// construction-time entry for the `JL_UNIONALL_OCCURSINV` flag
int jl_tvarref_occurs_invariant_top(jl_value_t *body) JL_NOTSAFEPOINT
{
    return tvarref_occurs_invariant(body, 1);
}

static jl_value_t *intersect_unionall(jl_value_t *t, jl_unionall_t *u, jl_stenv_t *e, int8_t R, jl_param_pos_t param) JL_CANSAFEPOINT
{
    jl_value_t *res = NULL;
    jl_savedenv_t se;
    JL_GC_PUSH1(&res);
    // the binding starts lazy (raw declared bounds, no variable); the
    // fast-failing crossings never materialize anything, and a successful
    // pass forces the binding before its result is constructed (see
    // `intersect_unionall_`)
    jl_varbinding_t *vb = stenv_push_binding(e, u, R, t);
    int body_occurs_inv = vb->body_occurs_inv;
    save_env(e, &se, 1);
    int noinv = !body_occurs_inv;
    // Setting constraintkind=1 for covariant var might be invalid if it also occurs in triangular positions.
    if (is_leaf_binder(vb) && noinv &&
        (param == PARAM_COVARIANT ? (u->flags & JL_UNIONALL_ALWAYSCOV) != 0
                                  : (tvarref_always_occurs_cov(u->body, 1, param) &&
                                     tvarref_never_occurs_in_inside_ub(u->body, 1))))
        vb->constraintkind = 1;
    res = intersect_unionall_(t, u, e, R, param, vb);
    vb->intersected = 1;
    if (vb->limited) {
        // if the environment got too big, avoid tree recursion and propagate the flag
        if (e->vars)
            e->vars->limited = 1;
    }
    else if (res != jl_bottom_type) {
        int constraint1 = vb->constraintkind;
        if (vb->concrete || vb->occurs_inv>1 || (vb->occurs_inv && cov_count(vb)))
            vb->constraintkind = vb->concrete ? 1 : 2;
        else if (u->lb != jl_bottom_type)
            vb->constraintkind = 2;
        else if (cov_count(vb) && vb->occurs < 2 && noinv)
            vb->constraintkind = 1;
        int reintersection = constraint1 != vb->constraintkind || vb->concrete || (vb->occurs_inv == 1 && vb->occurs > 1);
        if (reintersection) {
            if (constraint1 == 1) {
                binding_set_lb(e, vb, vb->var->lb);
                binding_set_ub(e, vb, vb->var->ub);
            }
            restore_env(e, &se, vb->constraintkind == 1 ? 1 : 0);
            vb->occurs = vb->occurs_cov = vb->occurs_inv = vb->cov_diag = 0;
            res = intersect_unionall_(t, u, e, R, param, vb);
        }
    }
    if (res != jl_bottom_type && vb->constraintkind == 1 && vb->widened_to_kind == 1) {
        // a `Type` was widened to a non-concrete kind union during intersection
        if (cov_count(vb) > 1) {
            // diagonal: reintersect if able to narrow across positions to a leaf bound
            // otherwise use original (possibly non-precise) bound
            if (is_leaf_bound(binding_ub(e, vb))) {
                restore_env(e, &se, 1);
                binding_set_lb(e, vb, vb->var->lb);
                vb->occurs = vb->occurs_cov = vb->occurs_inv = vb->cov_diag = 0;
                res = intersect_unionall_(t, u, e, R, param, vb);
            }
        }
        else {
            // actually non-diagonal: reintersect without widening or constraint
            restore_env(e, &se, 1);
            binding_set_lb(e, vb, vb->var->lb);
            binding_set_ub(e, vb, vb->var->ub);
            vb->constraintkind = 0;
            vb->widened_to_kind = 0;
            vb->occurs = vb->occurs_cov = vb->occurs_inv = vb->cov_diag = 0;
            res = intersect_unionall_(t, u, e, R, param, vb);
        }
    }
    free_env(&se);
    JL_GC_POP();
    return res;
}

static jl_value_t *intersect_invariant(jl_value_t *x, jl_value_t *y, jl_stenv_t *e) JL_CANSAFEPOINT;

// check n = (length of vararg type v)
static int intersect_vararg_length(jl_value_t *v, ssize_t n, jl_stenv_t *e, int8_t R) JL_CANSAFEPOINT
{
    jl_value_t *N = jl_unwrap_vararg_num(v);
    // resolve a length reference against the vararg's own side
    if (N && jl_is_tvarref(N))
        N = resolve_tvarref(N, R ? e->Rframe : e->Lframe, e);
    // only do the check if N is free in the tuple type's last parameter
    if (N && jl_is_typevar(N)) {
        jl_value_t *len = jl_box_long(n);
        JL_GC_PUSH1(&len);
        jl_value_t *il = R ? intersect_invariant(len, N, e) : intersect_invariant(N, len, e);
        JL_GC_POP();
        if (il == NULL || il == jl_bottom_type)
            return 0;
    }
    return 1;
}

static jl_value_t *intersect_varargs(jl_vararg_t *vmx, jl_vararg_t *vmy, ssize_t offset, jl_stenv_t *e, jl_param_pos_t param) JL_CANSAFEPOINT
{
    // Vararg: covariant in first parameter, invariant in second
    jl_value_t *xp1=jl_unwrap_vararg(vmx), *xp2=jl_unwrap_vararg_num(vmx),
                *yp1=jl_unwrap_vararg(vmy), *yp2=jl_unwrap_vararg_num(vmy);
    // resolve length references to their bindings' variables; the raw forms
    // are kept for the result-reuse checks below, so a raw-length vararg is
    // rebuilt (with the variable) rather than reused
    jl_value_t *xp2r = xp2, *yp2r = yp2;
    if (xp2 && jl_is_tvarref(xp2))
        xp2 = resolve_tvarref(xp2, e->Lframe, e);
    if (yp2 && jl_is_tvarref(yp2))
        yp2 = resolve_tvarref(yp2, e->Rframe, e);
    // in Vararg{T1} <: Vararg{T2}, need to check subtype twice to
    // simulate the possibility of multiple arguments, which is needed
    // to implement the diagonal rule correctly.
    if (intersect(xp1, yp1, e, param == PARAM_NONE ? PARAM_COVARIANT : param) == jl_bottom_type)
        return jl_bottom_type;
    jl_value_t *i2=NULL, *ii = intersect(xp1, yp1, e, PARAM_COVARIANT);
    if (ii == jl_bottom_type)
        return jl_bottom_type;
    if (!xp2 && !yp2) {
        if (obviously_egal(xp1, ii))
            ii = (jl_value_t*)vmx;
        else if (obviously_egal(yp1, ii))
            ii = (jl_value_t*)vmy;
        else {
            JL_GC_PUSH1(&ii);
            ii = (jl_value_t*)jl_wrap_vararg(ii, NULL, 1, 0);
            JL_GC_POP();
        }
        return ii;
    }
    JL_GC_PUSH2(&ii, &i2);
    assert(e->Loffset == 0);
    e->Loffset = offset;
    jl_varbinding_t *xb = NULL, *yb = NULL;
    int8_t max_offsetx = 0, max_offsety = 0;
    // a length reference that stayed dangling above (a detached fragment's
    // length) has no binding: leave `i2` as NULL, an unbounded length
    if (xp2 && jl_is_typevar(xp2)) {
        xb = lookup(e, (jl_tvar_t*)xp2);
        if (xb) xb->intvalued = 1;
        if (xb) max_offsetx = xb->max_offset;
        if (!yp2)
            i2 = bound_var_below((jl_tvar_t*)xp2, xb, e, 0);
    }
    if (yp2 && jl_is_typevar(yp2)) {
        yb = lookup(e, (jl_tvar_t*)yp2);
        if (yb) yb->intvalued = 1;
        if (yb) max_offsety = yb->max_offset;
        if (!xp2)
            i2 = bound_var_below((jl_tvar_t*)yp2, yb, e, 1);
    }
    if (xp2 && yp2) {
        // Vararg{T,N} <: Vararg{T2,N2}; equate N and N2
        i2 = intersect_invariant(xp2, yp2, e);
        if (i2 == NULL || i2 == jl_bottom_type || (jl_is_long(i2) && jl_unbox_long(i2) < 0) ||
            !((jl_is_typevar(i2) && ((jl_tvar_t*)i2)->lb == jl_bottom_type &&
                ((jl_tvar_t*)i2)->ub == (jl_value_t*)jl_any_type) || jl_is_long(i2))) {
            i2 = jl_bottom_type;
        }
    }
    assert(e->Loffset == offset);
    e->Loffset = 0;
    if (i2 == jl_bottom_type) {
        ii = (jl_value_t*)jl_bottom_type;
    }
    else {
        if (xb && !xb->intersected) {
            xb->max_offset = max_offsetx;
            if (offset > xb->max_offset && xb->max_offset >= 0)
                xb->max_offset = offset > 32 ? 32 : offset;
        }
        if (yb && !yb->intersected) {
            yb->max_offset = max_offsety;
            if (-offset > yb->max_offset && yb->max_offset >= 0)
                yb->max_offset = -offset > 32 ? 32 : -offset;
        }
        if (xp2 && i2 != NULL && obviously_egal(xp1, ii) && obviously_egal(xp2r, i2))
            ii = (jl_value_t*)vmx;
        else if (yp2 && i2 != NULL && obviously_egal(yp1, ii) && obviously_egal(yp2r, i2))
            ii = (jl_value_t*)vmy;
        else
            ii = (jl_value_t*)jl_wrap_vararg(ii, i2, 1, 0);
    }
    JL_GC_POP();
    return ii;
}


static jl_value_t *intersect_tuple(jl_datatype_t *xd, jl_datatype_t *yd, jl_stenv_t *e, jl_param_pos_t param) JL_CANSAFEPOINT
{
    size_t lx = jl_nparams(xd), ly = jl_nparams(yd);
    size_t llx = lx, lly = ly;
    if (lx == 0 && ly == 0)
        return (jl_value_t*)yd;
    int vx=0, vy=0;
    jl_vararg_kind_t vvx = lx > 0 ? jl_vararg_kind(jl_tparam(xd, lx-1)) : JL_VARARG_NONE;
    jl_vararg_kind_t vvy = ly > 0 ? jl_vararg_kind(jl_tparam(yd, ly-1)) : JL_VARARG_NONE;
    if (vvx == JL_VARARG_INT)
        llx += jl_unbox_long(jl_unwrap_vararg_num((jl_vararg_t *)jl_tparam(xd, lx-1))) - 1;
    if (vvy == JL_VARARG_INT)
        lly += jl_unbox_long(jl_unwrap_vararg_num((jl_vararg_t *)jl_tparam(yd, ly-1))) - 1;
    if (vvx == JL_VARARG_BOUND && (vvy == JL_VARARG_BOUND || vvy == JL_VARARG_UNBOUND)) {
        jl_value_t *xlen = jl_unwrap_vararg_num((jl_vararg_t*)jl_tparam(xd, lx-1));
        assert(xlen != NULL);
        xlen = resolve_tvarref(xlen, e->Lframe, e);
        jl_varbinding_t *xb = jl_is_typevar(xlen) ? lookup(e, (jl_tvar_t*)xlen) : NULL;
        if (xb && xb->intersected && xb->max_offset > 0) {
            assert(xb->max_offset <= 32);
            llx += xb->max_offset;
        }
    }
    if (vvy == JL_VARARG_BOUND && (vvx == JL_VARARG_BOUND || vvx == JL_VARARG_UNBOUND)) {
        jl_value_t *ylen = jl_unwrap_vararg_num((jl_vararg_t*)jl_tparam(yd, ly-1));
        assert(ylen != NULL);
        ylen = resolve_tvarref(ylen, e->Rframe, e);
        jl_varbinding_t *yb = jl_is_typevar(ylen) ? lookup(e, (jl_tvar_t*)ylen) : NULL;
        if (yb && yb->intersected && yb->max_offset > 0) {
            assert(yb->max_offset <= 32);
            lly += yb->max_offset;
        }
    }

    if ((vvx == JL_VARARG_NONE || vvx == JL_VARARG_INT) &&
        (vvy == JL_VARARG_NONE || vvy == JL_VARARG_INT)) {
        if (llx != lly)
            return jl_bottom_type;
    }

    size_t np = llx > lly ? llx : lly;
    jl_value_t *res = NULL;
    jl_svec_t *p = NULL;
    jl_value_t **params;
    jl_value_t **roots;
    JL_GC_PUSHARGS(roots, np < 64 ? np : 1);
    if (np < 64) {
        params = roots;
    }
    else {
        p = jl_alloc_svec(np);
        roots[0] = (jl_value_t*)p;
        params = jl_svec_data(p);
    }
    size_t i=0, j=0;
    jl_value_t *xi, *yi;
    int isx = 1, isy = 1; // try to reuse the object x or y as res whenever we can (e.g. when it is the supertype) instead of allocating a copy
    while (1) {
        vx = vy = 0;
        xi = i < llx ? jl_tparam(xd, i < lx ? i : lx - 1) : NULL;
        yi = j < lly ? jl_tparam(yd, j < ly ? j : ly - 1) : NULL;
        if (xi == NULL && yi == NULL) {
            assert(i == j && i == np);
            break;
        }
        if (xi && jl_is_vararg(xi)) vx = vvx == JL_VARARG_UNBOUND || (vvx == JL_VARARG_BOUND && i == llx - 1);
        if (yi && jl_is_vararg(yi)) vy = vvy == JL_VARARG_UNBOUND || (vvy == JL_VARARG_BOUND && j == lly - 1);
        if (xi == NULL || yi == NULL) {
            if (vx && intersect_vararg_length(xi, lly+1-llx, e, 0)) {
                np = j;
                p = NULL;
            }
            else if (vy && intersect_vararg_length(yi, llx+1-lly, e, 1)) {
                np = i;
                p = NULL;
            }
            else {
                res = jl_bottom_type;
            }
            break;
        }
        jl_value_t *ii = NULL;
        if (vx && vy) {
            ii = intersect_varargs((jl_vararg_t*)xi,
                                   (jl_vararg_t*)yi,
                                   lly - llx, // xi's offset: {A^n...,Vararg{T,N}} ∩ {Vararg{S,M}}
                                            // {(A∩S)^n...,Vararg{T∩S,N}} plus N = M-n
                                   e,
                                   param);
        }
        else {
            ii = intersect(jl_is_vararg(xi) ? jl_unwrap_vararg(xi) : xi,
                           jl_is_vararg(yi) ? jl_unwrap_vararg(yi) : yi,
                           e,
                           param == PARAM_NONE ? PARAM_COVARIANT : param);
        }
        if (ii == jl_bottom_type) {
            if (vx && vy) {
                jl_varbinding_t *xb=NULL, *yb=NULL;
                jl_value_t *xlen = jl_unwrap_vararg_num(xi);
                if (xlen) xlen = resolve_tvarref(xlen, e->Lframe, e);
                if (xlen && jl_is_typevar(xlen)) xb = lookup(e, (jl_tvar_t*)xlen);
                jl_value_t *ylen = jl_unwrap_vararg_num(yi);
                if (ylen) ylen = resolve_tvarref(ylen, e->Rframe, e);
                if (ylen && jl_is_typevar(ylen)) yb = lookup(e, (jl_tvar_t*)ylen);
                int len = i > j ? i : j;
                jl_value_t *xblen = xb ? lterm_long(xb->lbs) : NULL;
                jl_value_t *yblen = yb ? lterm_long(yb->lbs) : NULL;
                if ((xblen != NULL && llx-1+jl_unbox_long(xblen) != len) ||
                    (yblen != NULL && lly-1+jl_unbox_long(yblen) != len)) {
                    res = jl_bottom_type;
                }
                else {
                    assert(e->Loffset == 0);
                    if (xb) set_var_to_const(xb, jl_box_long(len-llx+1), e, 0);
                    if (yb) set_var_to_const(yb, jl_box_long(len-lly+1), e, 1);
                    np = len;
                    p = NULL;
                }
            }
            else {
                res = jl_bottom_type;
            }
            break;
        }
        isx = isx && ii == xi;
        isy = isy && ii == yi;
        if (p)
            jl_svecset(p, (i > j ? i : j), ii);
        else
            params[i > j ? i : j] = ii;
        if (vx && vy)
            break;
        if (!vx) i++;
        if (!vy) j++;
    }
    // TODO: handle Vararg with explicit integer length parameter
    if (res == NULL) {
        assert(!p || np == jl_svec_len(p));
        isx = isx && lx == np;
        isy = isy && ly == np;
        if (!isx && !isy) {
            // do a more careful check now for equivalence
            if (lx == np) {
                isx = 1;
                for (i = 0; i < np; i++)
                    isx = isx && obviously_egal(params[i], jl_tparam(xd, i));
            }
            if (!isx && ly == np) {
                isy = 1;
                for (i = 0; i < np; i++)
                    isy = isy && obviously_egal(params[i], jl_tparam(yd, i));
            }
        }
        if (isx)
            res = (jl_value_t*)xd;
        else if (isy)
            res = (jl_value_t*)yd;
        else if (p)
            res = jl_apply_tuple_type(p, 1);
        else
            res = jl_apply_tuple_type_v(params, np);
    }
    JL_GC_POP();
    return res;
}

static void flip_vars(jl_stenv_t *e)
{
    jl_varbinding_t *btemp = e->vars;
    while (btemp != NULL) {
        btemp->existential = !btemp->existential;
        btemp = btemp->prev;
    }
}

// intersection where xd nominally inherits from yd
static jl_value_t *intersect_sub_datatype(jl_datatype_t *xd, jl_datatype_t *yd, jl_stenv_t *e, int R, jl_param_pos_t param) JL_CANSAFEPOINT
{
    // attempt to populate additional constraints into `e`
    // if that attempt fails, then return bottom
    // otherwise return xd (finish_unionall will later handle propagating those constraints)
    assert(e->Loffset == 0);
    jl_datatype_t *xsuper = jl_datatype_compute_super(xd);
    if (xsuper == NULL)
        return jl_bottom_type; // deferred supertype; definition still in progress
    jl_value_t *isuper = R ? intersect((jl_value_t*)yd, (jl_value_t*)xsuper, e, param) :
                             intersect((jl_value_t*)xsuper, (jl_value_t*)yd, e, param);
    if (isuper == jl_bottom_type)
        return jl_bottom_type;
    // the walked term becomes the result: re-express it in variable form so
    // the constraints just recorded (and the enclosing binder re-wrap) can
    // see its bound-variable occurrences
    if (jl_has_dangling_tvarrefs((jl_value_t*)xd))
        return frame_substitute((jl_value_t*)xd, R ? e->Rframe : e->Lframe, e);
    return (jl_value_t*)xd;
}

static jl_value_t *intersect_invariant(jl_value_t *x, jl_value_t *y, jl_stenv_t *e)
{
    // resolve bound-variable references so the typevar tests below see the
    // bindings' variables (cf. the resolution at the top of `intersect`)
    x = resolve_tvarref(x, e->Lframe, e);
    y = resolve_tvarref(y, e->Rframe, e);
    if (e->Loffset == 0 && !has_free_or_dangling_typevars(x) && !has_free_or_dangling_typevars(y)) {
        return (jl_subtype(x,y) && jl_subtype(y,x)) ? y : NULL;
    }
    e->invdepth++;
    jl_value_t *ii = intersect(x, y, e, PARAM_INVARIANT);
    e->invdepth--;
    if (jl_is_typevar(x) && jl_is_typevar(y) && jl_is_typevar(ii))
        return ii; // skip the following check due to possible circular constraints.
    if (ii == jl_bottom_type) {
        if (!subtype_in_env(x, jl_bottom_type, e))
            return NULL;
        // `y` (a right term) is checked in a left position: give the launched
        // query the matching frame orientation
        flip_vars(e); flip_offset(e); flip_frames(e);
        if (!subtype_in_env(y, jl_bottom_type, e)) {
            flip_vars(e); flip_offset(e); flip_frames(e);
            return NULL;
        }
        flip_vars(e); flip_offset(e); flip_frames(e);
        return jl_bottom_type;
    }
    jl_savedenv_t se;
    JL_GC_PUSH1(&ii);
    save_env(e, &se, 1);
    if (!subtype_in_env_existential(x, y, e))
        ii = NULL;
    else {
        restore_env(e, &se, 1);
        flip_offset(e); flip_frames(e);
        if (!subtype_in_env_existential(y, x, e))
            ii = NULL;
        flip_offset(e); flip_frames(e);
    }
    restore_env(e, &se, 1);
    free_env(&se);
    JL_GC_POP();
    return ii;
}

// intersection where x == Type{...} and y is not
static jl_value_t *intersect_type_type(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int8_t R) JL_CANSAFEPOINT
{
    assert(e->Loffset == 0);
    // a bare reference that stays a variable (see `typeeq_unpin_tvar`: its
    // binder's declared bounds do not pin it) meets no non-kind: decided
    // without materializing the variable, as this is the common outcome
    if (jl_is_tvarref(jl_typeeq_T(x)) && !is_kind_or_anytype(y)) {
        jl_varbinding_t *b = frame_lookup(R ? e->Rframe : e->Lframe, jl_tvarref_depth(jl_typeeq_T(x)));
        if (b != NULL) {
            jl_value_t *lb = b->u->lb, *ub = b->u->ub;
            if (lb != ub && (jl_has_free_or_dangling_typevars(lb) || jl_has_free_or_dangling_typevars(ub) ||
                             !jl_types_equal(lb, ub)))
                return jl_bottom_type;
        }
    }
    // the payload is small: re-express it in variable form, so that the
    // bound-variable tests below see the binding's variable and the
    // `return x` paths are frame-free (`R` says which side `x` came from)
    if (jl_has_dangling_tvarrefs(x))
        x = frame_substitute(x, R ? e->Rframe : e->Lframe, e);
    // fast path: every member of a `Type{T}` is a type
    if (y == (jl_value_t*)jl_anytype_type || y == (jl_value_t*)jl_any_type)
        return x;
    jl_value_t *p0 = typeeq_unpin_tvar(jl_typeeq_T(x));
    if (!jl_is_typevar(p0)) {
        // a dispatch key for one specific open type object (dangling free
        // typevars, see `typeeq_vars_bound_in_env`) is pinned to its type tag
        if (typeeq_is_dangling_key(p0, e, NULL, R ? e->Rframe : e->Lframe))
            return (jl_typeof(p0) == y) ? x : jl_bottom_type;
        // `Type{T}`'s members can carry any type tag in the kind mask, so the
        // intersection with `y` (a kind or a kind's supertype) is nonempty
        // whenever some kind in the mask lies in `y`. `Type{T}` itself is the
        // best expressible bound for the members with that tag (e.g. the
        // `Vector{S} where Int<:S<:Int` member of `Type{Vector{Int}} ∩ UnionAll`).
        return typeeq_mask_meets(typeeq_kind_mask(p0), y) ? x : jl_bottom_type;
    }
    if (!is_kind_or_anytype(y)) return jl_bottom_type;
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

// cmp <= 0: is x already <= y in this environment
// cmp >= 0: is x already >= y in this environment
static int compareto_var(jl_value_t *x, jl_tvar_t *y, jl_stenv_t *e, int cmp) JL_NOTSAFEPOINT
{
    if (x == (jl_value_t*)y)
        return 1;
    if (!jl_is_typevar(x))
        return 0;
    int innervar = 0;
    jl_varbinding_t *xv = lookup_binding(e, (jl_tvar_t*)x, &innervar);
    if (xv == NULL && !innervar)
        return 0;
    int ans = 1;
    if (cmp <= 0) {
        jl_value_t *ub = xv ? lterm_closed1(xv->ubs) : ((jl_tvar_t*)x)->ub;
        ans &= ub != NULL && compareto_var(ub, y, e, cmp);
    }
    if (cmp >= 0) {
        jl_value_t *lb = xv ? lterm_closed1(xv->lbs) : ((jl_tvar_t*)x)->lb;
        ans &= lb != NULL && compareto_var(lb, y, e, cmp);
    }
    return ans;
}

// Check whether the environment already asserts x <: y via recorded bounds.
// This is used to avoid adding redundant constraints that lead to cycles.
// Note this is a semi-predicate: 1 => is a subtype, 0 => unknown
static int subtype_by_bounds(jl_value_t *x, jl_value_t *y, jl_stenv_t *e) JL_NOTSAFEPOINT
{
    if (!jl_is_typevar(x) || !jl_is_typevar(y))
        return 0;
    return compareto_var(x, (jl_tvar_t*)y, e, -1) || compareto_var(y, (jl_tvar_t*)x, e, 1);
}

static int intersect_var_ccheck_in_env(jl_value_t *xlb, jl_value_t *xub, jl_value_t *ylb, jl_value_t *yub, jl_stenv_t *e, int flip)
{
    int easy_check1 = xlb == jl_bottom_type ||
                      yub == (jl_value_t *)jl_any_type ||
                      (e->Loffset == 0 && obviously_in_union(yub, xlb));
    int easy_check2 = ylb == jl_bottom_type ||
                      xub == (jl_value_t *)jl_any_type ||
                      (e->Loffset == 0 && obviously_in_union(xub, ylb));
    int nofree1 = 0, nofree2 = 0;
    if (!easy_check1) {
        nofree1 = !jl_has_free_typevars(xlb) && !jl_has_free_typevars(yub);
        if (nofree1 && e->Loffset == 0) {
            easy_check1 = jl_subtype(xlb, yub);
            if (!easy_check1)
                return 0;
        }
    }
    if (!easy_check2) {
        nofree2 = !jl_has_free_typevars(ylb) && !jl_has_free_typevars(xub);
        if (nofree2 && e->Loffset == 0) {
            easy_check2 = jl_subtype(ylb, xub);
            if (!easy_check2)
                return 0;
        }
    }
    if (easy_check1 && easy_check2)
        return 1;
    int ccheck = 0;
    if ((easy_check1 || nofree1) && (easy_check2 || nofree2)) {
        jl_varbinding_t *vars = e->vars;
        e->vars = NULL;
        ccheck = easy_check1 || subtype_in_env(xlb, yub, e);
        if (ccheck && !easy_check2) {
            flip_offset(e);
            ccheck = subtype_in_env(ylb, xub, e);
            flip_offset(e);
        }
        e->vars = vars;
        return ccheck;
    }
    jl_savedenv_t se;
    save_env(e, &se, 1);
    // first try normal flip.
    if (flip) flip_vars(e);
    ccheck = easy_check1 || subtype_in_env(xlb, yub, e);
    if (ccheck && !easy_check2) {
        flip_offset(e);
        ccheck = subtype_in_env(ylb, xub, e);
        flip_offset(e);
    }
    if (flip) flip_vars(e);
    if (!ccheck) {
        // then try reverse flip.
        restore_env(e, &se, 1);
        if (!flip) flip_vars(e);
        ccheck = easy_check1 || subtype_in_env(xlb, yub, e);
        if (ccheck && !easy_check2) {
            flip_offset(e);
            ccheck = subtype_in_env(ylb, xub, e);
            flip_offset(e);
        }
        if (!flip) flip_vars(e);
    }
    if (!ccheck) {
        // then try existential.
        restore_env(e, &se, 1);
        if (easy_check1)
            ccheck = 1;
        else {
            ccheck = subtype_in_env_existential(xlb, yub, e);
            restore_env(e, &se, 1);
        }
        if (ccheck && !easy_check2) {
            flip_offset(e);
            ccheck = subtype_in_env_existential(ylb, xub, e);
            flip_offset(e);
            restore_env(e, &se, 1);
        }
    }
    free_env(&se);
    return ccheck;
}

static int has_typevar_via_env(jl_value_t *x, jl_tvar_t *t, jl_stenv_t *e) JL_CANSAFEPOINT
{
    if (e->Loffset == 0) {
        jl_varbinding_t *temp = e->vars;
        while (temp != NULL) {
            if (temp->var == t)
                break;
            if (temp->var != NULL) {
                // a pinned binding's edge can live in its declared bound
                jl_value_t *tlb = binding_lb(e, temp);
                if (tlb == binding_ub(e, temp) && tlb == (jl_value_t *)t &&
                    jl_has_typevar(x, temp->var))
                    return 1;
            }
            temp = temp->prev;
        }
    }
    return jl_has_typevar(x, t);
}

// Check if a type variable always occurs as a parameter in a datatype. (e.g. Set{x}, Union{Set{x},Val{x}}, ...)
static int var_always_occurs_as_parameter(jl_value_t *t, jl_tvar_t *var, int param) JL_NOTSAFEPOINT
{
    if (t == (jl_value_t*)var)
        return param;
    else if (jl_is_uniontype(t)) {
        return var_always_occurs_as_parameter(((jl_uniontype_t*)t)->a, var, 0) &&
               var_always_occurs_as_parameter(((jl_uniontype_t*)t)->b, var, 0);
    }
    else if (jl_is_unionall(t)) {
        // (a binder cannot shadow the free variable: its occurrences are references)
        return var_always_occurs_as_parameter(((jl_unionall_t*)t)->body, var, param);
    }
    else if (jl_is_vararg(t)) {
        jl_vararg_t *vm = (jl_vararg_t*)t;
        return (vm->T != NULL && var_always_occurs_as_parameter(vm->T, var, param)) ||
               (vm->N != NULL && var_always_occurs_as_parameter(vm->N, var, param)) ;
    }
    else if (jl_is_datatype(t)) {
        for (size_t i = 0; i < jl_nparams(t); i++) {
            if (var_always_occurs_as_parameter(jl_tparam(t, i), var, 1))
                return 1;
        }
        return 0;
    }
    return 0;
}

static jl_value_t *intersect(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, jl_param_pos_t param)
{
    // resolve bound-variable references through their side's binder chain
    x = resolve_tvarref(x, e->Lframe, e);
    y = resolve_tvarref(y, e->Rframe, e);
    // interned terms carrying raw references are only identical as objects;
    // under different chains their references mean different bindings
    if (x == y && !jl_has_dangling_tvarrefs(x)) return y;
    if (jl_is_tvarref(x) || jl_is_tvarref(y)) {
        // Still-dangling references (detached subterms of the query): only
        // structurally comparable. A reference stands for an unknown type, so
        // over-approximate the meet by the other side (`typeintersect` may
        // return a supertype of the true intersection, but must not claim
        // emptiness for a possibly-inhabited intersection).
        if (jl_is_tvarref(x) && jl_is_tvarref(y) && jl_tvarref_depth(x) == jl_tvarref_depth(y))
            return y;
        if (jl_is_tvarref(x))
            return jl_has_dangling_tvarrefs(y) ? frame_substitute(y, e->Rframe, e) : y;
        return jl_has_dangling_tvarrefs(x) ? frame_substitute(x, e->Lframe, e) : x;
    }
    if (jl_is_typevar(x)) {
        if (jl_is_typevar(y)) {
            int xinner = 0, yinner = 0;
            jl_varbinding_t *xx = lookup_binding(e, (jl_tvar_t*)x, &xinner);
            jl_varbinding_t *yy = lookup_binding(e, (jl_tvar_t*)y, &yinner);
            int xfree_singleton = xx == NULL && !xinner;
            int yfree_singleton = yy == NULL && !yinner;
            if (xfree_singleton && yfree_singleton)
                return jl_bottom_type;
            // the binder of a detached fragment supports no bound reasoning
            // (its bounds reference binders outside the query, cf. `var_lt_`);
            // over-approximate by the (arbitrary) right variable
            if ((xx && (jl_has_dangling_tvarrefs(binding_lb(e, xx)) || jl_has_dangling_tvarrefs(binding_ub(e, xx)))) ||
                (yy && (jl_has_dangling_tvarrefs(binding_lb(e, yy)) || jl_has_dangling_tvarrefs(binding_ub(e, yy)))))
                return y;
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
            if (param == PARAM_INVARIANT) {
                jl_value_t *xlb = xx ? binding_lb(e, xx) : xinner ? ((jl_tvar_t*)x)->lb : x;
                jl_value_t *xub = xx ? binding_ub(e, xx) : xinner ? ((jl_tvar_t*)x)->ub : x;
                jl_value_t *ylb = yy ? binding_lb(e, yy) : yinner ? ((jl_tvar_t*)y)->lb : y;
                jl_value_t *yub = yy ? binding_ub(e, yy) : yinner ? ((jl_tvar_t*)y)->ub : y;
                if (xx && yy && xx->depth0 != yy->depth0) {
                    record_var_occurrence(xx, e, param);
                    record_var_occurrence(yy, e, param);
                    return subtype_in_env(yub, ylb, e) ? y : jl_bottom_type;
                }
                if (xub == xlb && jl_is_typevar(xub) && xub != x) {
                    record_var_occurrence(xx, e, param);
                    if (y == xub) {
                        record_var_occurrence(yy, e, param);
                        return y;
                    }
                    if (R) flip_offset(e);
                    jl_value_t *res = intersect(xub, y, e, param);
                    if (R) flip_offset(e);
                    return res;
                }
                if (yub == ylb && jl_is_typevar(yub) && yub != y) {
                    record_var_occurrence(yy, e, param);
                    if (R) flip_offset(e);
                    jl_value_t *res = intersect(x, yub, e, param);
                    if (R) flip_offset(e);
                    return res;
                }
                record_var_occurrence(xx, e, param);
                record_var_occurrence(yy, e, param);
                int xoffset = R ? -e->Loffset : e->Loffset;
                if (!jl_is_type(ylb) && !jl_is_typevar(ylb)) {
                    if (xx)
                        return set_var_to_const(xx, ylb, e, R);
                    if ((xlb == jl_bottom_type && xub == (jl_value_t*)jl_any_type) || jl_egal(xlb, ylb)) {
                        if (xoffset == 0)
                            return ylb;
                        else if (jl_is_long(ylb)) {
                            if (xoffset > 0)
                                return ylb;
                            else
                                return jl_box_long(jl_unbox_long(ylb) + xoffset);
                        }
                    }
                    return jl_bottom_type;
                }
                if (!jl_is_type(xlb) && !jl_is_typevar(xlb)) {
                    if (yy)
                        return set_var_to_const(yy, xlb, e, !R);
                    if (ylb == jl_bottom_type && yub == (jl_value_t*)jl_any_type) {
                        if (xoffset == 0)
                            return xlb;
                        else if (jl_is_long(xlb)) {
                            if (xoffset < 0)
                                return xlb;
                            else
                                return jl_box_long(jl_unbox_long(ylb) - xoffset);
                        }
                    }
                    return jl_bottom_type;
                }
                int ccheck;
                if (R) flip_offset(e);
                if (xlb == xub && ylb == yub &&
                    jl_has_typevar(xlb, (jl_tvar_t *)y) &&
                    jl_has_typevar(ylb, (jl_tvar_t *)x)) {
                    // special case for e.g.
                    // 1) Val{Y}<:X<:Val{Y} && Val{X}<:Y<:Val{X}
                    // 2) Y<:X<:Y && Val{X}<:Y<:Val{X} => Val{Y}<:Y<:Val{Y}
                    ccheck = 0;
                }
                else if (var_always_occurs_as_parameter(xub, (jl_tvar_t*)y, 0) ||
                         var_always_occurs_as_parameter(yub, (jl_tvar_t*)x, 0)) {
                    // special case for X<:Val{Y} ∩ Y
                    ccheck = 0;
                }
                else if (yub == xub ||
                    (subtype_by_bounds(xlb, yub, e) && subtype_by_bounds(ylb, xub, e))) {
                    ccheck = 1;
                }
                else {
                    // try many subtype check to avoid false `Union{}`
                    ccheck = intersect_var_ccheck_in_env(xlb, xub, ylb, yub, e, R);
                }
                if (R) flip_offset(e);
                if (!ccheck)
                    return jl_bottom_type;
                if ((has_typevar_via_env(xub, (jl_tvar_t*)y, e) || has_typevar_via_env(xub, (jl_tvar_t*)x, e)) &&
                    (has_typevar_via_env(yub, (jl_tvar_t*)x, e) || has_typevar_via_env(yub, (jl_tvar_t*)y, e))) {
                    // TODO: This doesn't make much sense.
                    // circular constraint. the result will be Bottom, but in the meantime
                    // we need to avoid computing intersect(xub, yub) since it won't terminate.
                    return y;
                }
                jl_value_t *ub=NULL, *lb=NULL;
                JL_GC_PUSH2(&lb, &ub);
                int d = xx ? xx->depth0 : yy ? yy->depth0 : 0;
                ub = R ? intersect_aside(yub, xub, e, d) : intersect_aside(xub, yub, e, d);
                if (reachable_var(xlb, (jl_tvar_t*)y, e))
                    lb = ylb;
                else
                    lb = simple_join(xlb, ylb);
                if (yy && xoffset == 0) {
                    binding_set_lb(e, yy, lb);
                    if (!reachable_var(ub, (jl_tvar_t*)y, e))
                        binding_set_ub(e, yy, ub);
                    assert(lterm_closed1(yy->ubs) != y);
                    assert(lterm_closed1(yy->lbs) != y);
                }
                if (xx && xoffset == 0 && !reachable_var(y, (jl_tvar_t*)x, e)) {
                    binding_set_lb(e, xx, y);
                    binding_set_ub(e, xx, y);
                    assert(lterm_closed1(xx->ubs) != x);
                }
                JL_GC_POP();
                // Here we always return the shorter `Vararg`'s length.
                return xoffset < 0 ? x : y;
            }
            assert(e->Loffset == 0);
            record_var_occurrence(xx, e, param);
            record_var_occurrence(yy, e, param);
            if (xx && yy && ((xx->concrete && !yy->concrete) || (xx->lb == xx->ub && xx->lb == y))) {
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
    if (e->Loffset == 0 && !has_free_or_dangling_typevars(x) && !has_free_or_dangling_typevars(y)) {
        if (jl_subtype(x, y)) return x;
        if (jl_subtype(y, x)) return y;
    }
    if (jl_is_uniontype(x)) {
        // the obvious-membership checks compare the sides structurally, which
        // is meaningless for raw references from different chains
        int structural = !jl_has_dangling_tvarrefs(x) && !jl_has_dangling_tvarrefs(y);
        if (structural && obviously_in_union(x, y))
            return y;
        if (structural && jl_is_uniontype(y) && obviously_in_union(y, x))
            return x;
        return intersect_union(y, (jl_uniontype_t*)x, e, 0, param);
    }
    if (jl_is_uniontype(y)) {
        if (!jl_has_dangling_tvarrefs(x) && !jl_has_dangling_tvarrefs(y) &&
            obviously_in_union(y, x))
            return x;
        if (jl_is_unionall(x) && (has_free_or_dangling_typevars(x) || has_free_or_dangling_typevars(y)))
            return intersect_unionall(y, (jl_unionall_t*)x, e, 0, param);
        return intersect_union(x, (jl_uniontype_t*)y, e, 1, param);
    }
    // a walked term returned as the result must be re-expressed in variable
    // form; it does not outlive its frames otherwise
    if (y == (jl_value_t*)jl_any_type)
        return jl_has_dangling_tvarrefs(x) ? frame_substitute(x, e->Lframe, e) : x;
    if (x == (jl_value_t*)jl_any_type)
        return jl_has_dangling_tvarrefs(y) ? frame_substitute(y, e->Rframe, e) : y;
    if (jl_is_unionall(x)) {
        if (jl_is_unionall(y)) {
            jl_value_t *a=NULL, *b=jl_bottom_type, *res=NULL;
            JL_GC_PUSH2(&a, &b);
            jl_savedenv_t se;
            save_env(e, &se, 0);
            a = intersect_unionall(y, (jl_unionall_t*)x, e, 0, param);
            if (jl_is_unionall(a)) {
                jl_unionall_t *ua = (jl_unionall_t*)a;
                if (jl_is_unionall(ua->body)) {
                    jl_unionall_t *ub = (jl_unionall_t*)ua->body;
                    // does the inner binder's bound reference the outer binder?
                    if (jl_tvarref_occurs(ub->ub, 1) ||
                        jl_tvarref_occurs(ub->lb, 1)) {
                        restore_env(e, &se, 0); // restore counts
                        b = intersect_unionall(x, (jl_unionall_t*)y, e, 1, param);
                    }
                }
            }
            free_env(&se);
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
    if (jl_is_typeegal(x) || jl_is_typeegal(y)) {
        // the intersection is `TypeEgal{A}` itself when `A` lies in the other
        // operand (binding its typevars), else `Bottom`
        int8_t R = 0;
        if (!jl_is_typeegal(x)) { jl_value_t *t = x; x = y; y = t; R = 1; }
        // `x` is small and may be the result: re-express it in variable form
        // up front. `y` is re-expressed only for the payload comparison by
        // egality; `intersect_invariant` walks a raw payload in place (it is
        // passed on its own side, which `R` tracks)
        if (jl_has_dangling_tvarrefs(x))
            x = frame_substitute(x, R ? e->Rframe : e->Lframe, e);
        jl_value_t *A = jl_typeegal_T(x);
        if (jl_is_typeegal(y)) {
            if (jl_has_dangling_tvarrefs(y))
                y = frame_substitute(y, R ? e->Lframe : e->Rframe, e);
            return jl_egal(A, jl_typeegal_T(y)) ? x : jl_bottom_type; // intersection is nonempty iff `A === B`
        }
        if (jl_is_typeeq(y)) {
            jl_value_t *yp = jl_typeeq_T(y);
            // as in the subtype rule: `A` is egal-known, but `Type{B}` pins `B`
            // only up to `==`, so `A`'s spelling is only `==`-authoritative
            int saved_spell = e->spell_channel;
            if (e->spell_channel > BOUND_EQ)
                e->spell_channel = BOUND_EQ;
            jl_value_t *ii = R ? intersect_invariant(yp, A, e) : intersect_invariant(A, yp, e);
            e->spell_channel = saved_spell;
            return (ii == NULL || ii == jl_bottom_type) ? jl_bottom_type : x;
        }
        // `A` lies in `y` iff the singleton `typeof(A)` does; `jl_subtype` also
        // covers abstract supertypes (e.g. `AnyType`) when the closed-types
        // fast path above was skipped
        if (param != PARAM_INVARIANT && !jl_has_free_or_dangling_typevars(y) &&
            jl_subtype(jl_typeof(A), y))
            return x;
        return jl_bottom_type;
    }
    if (jl_is_typeeq(x) && jl_is_typeeq(y)) {
        jl_value_t *xp = jl_typeeq_T(x);
        jl_value_t *yp = jl_typeeq_T(y);
        jl_value_t *ii = intersect_invariant(xp, yp, e);
        if (ii == NULL)
            return jl_bottom_type;
        JL_GC_PUSH1(&ii);
        jl_value_t *ans = (jl_value_t*)jl_wrap_Type(ii);
        JL_GC_POP();
        return ans;
    }
    if (param != PARAM_INVARIANT) {
        if (jl_is_typeeq(x))
            return intersect_type_type(x, y, e, 0);
        if (jl_is_typeeq(y))
            return intersect_type_type(y, x, e, 1);
    }
    if (jl_is_datatype(x) && jl_is_datatype(y)) {
        jl_datatype_t *xd = (jl_datatype_t*)x, *yd = (jl_datatype_t*)y;
        if (param != PARAM_INVARIANT) {
            if (jl_is_typeeq(x)) {
                if (!jl_is_typeeq(y))
                    return intersect_type_type(x, y, e, 0);
            }
            else if (jl_is_typeeq(y)) {
                return intersect_type_type(y, x, e, 1);
            }
        }
        if (xd->name == yd->name) {
            if (jl_is_tuple_type(xd))
                return intersect_tuple(xd, yd, e, param);
            size_t i, np = jl_nparams(xd);
            jl_value_t **newparams;
            JL_GC_PUSHARGS(newparams, np);
            int isx = 1, isy = 1; // try to reuse the object x or y as res whenever we can (e.g. when it is the supertype) instead of allocating a copy
            for (i = 0; i < np; i++) {
                jl_value_t *xi = jl_tparam(xd, i), *yi = jl_tparam(yd, i);
                jl_value_t *ii = intersect_invariant(xi, yi, e);
                if (ii == NULL)
                    break;
                isx = isx && ii == xi;
                isy = isy && ii == yi;
                newparams[i] = ii;
            }
            jl_value_t *res = jl_bottom_type;
            if (i == np) {
                if (!isx && !isy) {
                    // do a more careful check now for equivalence
                    isx = 1;
                    for (i = 0; i < np; i++)
                        isx = isx && obviously_egal(newparams[i], jl_tparam(xd, i));
                    if (!isx) {
                        isy = 1;
                        for (i = 0; i < np; i++)
                            isy = isy && obviously_egal(newparams[i], jl_tparam(yd, i));
                    }
                }
                if (isx)
                    res = x;
                else if (isy)
                    res = y;
                else {
                    JL_TRY {
                        res = jl_apply_type(xd->name->wrapper, newparams, np);
                    }
                    JL_CATCH {
                        res = jl_bottom_type;
                    }
                }
            }
            JL_GC_POP();
            return res;
        }
        if (param == PARAM_INVARIANT) return jl_bottom_type;
        // deferred supertypes (self-referential definitions) materialize on
        // demand; `jl_datatype_compute_super`'s fast path is the acquiring read
        while (xd != NULL && xd != jl_any_type && xd->name != yd->name)
            xd = jl_datatype_compute_super(xd);
        if (xd == NULL)
            return jl_bottom_type; // definition still in progress
        if (xd == jl_any_type) {
            xd = (jl_datatype_t*)x;
            while (yd != NULL && yd != jl_any_type && yd->name != xd->name)
                yd = jl_datatype_compute_super(yd);
            if (yd == NULL)
                return jl_bottom_type;
            if (yd == jl_any_type)
                return jl_bottom_type;
            return intersect_sub_datatype((jl_datatype_t*)y, xd, e, 1, param);
        }
        return intersect_sub_datatype((jl_datatype_t*)x, yd, e, 0, param);
    }
    // compare the variable forms (raw references from different chains can
    // spell the same binder differently, or different binders alike); the
    // result is `y`'s variable form
    if (!egal_frames(x, e->Lframe, y, e->Rframe, 0, e))
        return jl_bottom_type;
    return jl_has_dangling_tvarrefs(y) ? frame_substitute(y, e->Rframe, e) : y;
}

// the binding a reference inside a term resolves to through the term's
// chain, with `d` binders of the term entered; NULL for a reference to a
// binder of the term itself or an unresolved one (which substitution leaves
// unchanged, so that it is compared by its index either way)
static jl_varbinding_t *egal_ref_binding(jl_value_t *t, jl_varbinding_t *frame, size_t d) JL_NOTSAFEPOINT
{
    size_t k = jl_tvarref_depth(t);
    return k <= d ? NULL : frame_lookup(frame, k - d);
}

// `jl_egal` of the variable forms of `x` (under the chain `xframe`) and `y`
// (under `yframe`), without building them: references are compared by what
// they resolve to. `d` counts the binders entered inside both terms.
static int egal_frames(jl_value_t *x, jl_varbinding_t *xframe, jl_value_t *y, jl_varbinding_t *yframe,
                       size_t d, jl_stenv_t *e) JL_CANSAFEPOINT
{
    if (x == y && (xframe == yframe || !jl_has_dangling_tvarrefs(x)))
        return 1;
    if (!jl_has_dangling_tvarrefs(x) && !jl_has_dangling_tvarrefs(y))
        return jl_egal(x, y);
    if (jl_is_tvarref(x) || jl_is_tvarref(y)) {
        jl_varbinding_t *xb = jl_is_tvarref(x) ? egal_ref_binding(x, xframe, d) : NULL;
        jl_varbinding_t *yb = jl_is_tvarref(y) ? egal_ref_binding(y, yframe, d) : NULL;
        if (xb != NULL && yb != NULL)
            return xb == yb;
        if (xb == NULL && yb == NULL)
            return jl_is_tvarref(x) && jl_is_tvarref(y) && jl_tvarref_depth(x) == jl_tvarref_depth(y);
        // a binding against anything else: only its variable can be identical
        jl_value_t *other = xb != NULL ? y : x;
        if (!jl_is_typevar(other))
            return 0;
        return (jl_value_t*)binding_var(e, xb != NULL ? xb : yb) == other;
    }
    if (jl_typeof(x) != jl_typeof(y))
        return 0;
    if (jl_is_datatype(x)) {
        if (((jl_datatype_t*)x)->name != ((jl_datatype_t*)y)->name)
            return 0;
        size_t i, np = jl_nparams(x);
        if (jl_nparams(y) != np)
            return 0;
        for (i = 0; i < np; i++) {
            if (!egal_frames(jl_tparam(x, i), xframe, jl_tparam(y, i), yframe, d, e))
                return 0;
        }
        return 1;
    }
    if (jl_is_uniontype(x))
        return egal_frames(((jl_uniontype_t*)x)->a, xframe, ((jl_uniontype_t*)y)->a, yframe, d, e) &&
               egal_frames(((jl_uniontype_t*)x)->b, xframe, ((jl_uniontype_t*)y)->b, yframe, d, e);
    if (jl_is_intersecttype(x))
        return egal_frames(((jl_intersecttype_t*)x)->a, xframe, ((jl_intersecttype_t*)y)->a, yframe, d, e) &&
               egal_frames(((jl_intersecttype_t*)x)->b, xframe, ((jl_intersecttype_t*)y)->b, yframe, d, e);
    if (jl_is_unionall(x)) {
        jl_unionall_t *ux = (jl_unionall_t*)x, *uy = (jl_unionall_t*)y;
        // (the binder name is observable, as in `jl_egal`; the bounds lie
        // outside the binder's own scope)
        return ux->name == uy->name &&
               egal_frames(ux->lb, xframe, uy->lb, yframe, d, e) &&
               egal_frames(ux->ub, xframe, uy->ub, yframe, d, e) &&
               egal_frames(ux->body, xframe, uy->body, yframe, d + 1, e);
    }
    if (jl_is_vararg(x)) {
        jl_vararg_t *vx = (jl_vararg_t*)x, *vy = (jl_vararg_t*)y;
        if ((vx->T == NULL) != (vy->T == NULL) || (vx->N == NULL) != (vy->N == NULL))
            return 0;
        return (vx->T == NULL || egal_frames(vx->T, xframe, vy->T, yframe, d, e)) &&
               (vx->N == NULL || egal_frames(vx->N, xframe, vy->N, yframe, d, e));
    }
    if (jl_is_some_Type(x))
        return egal_frames(jl_some_Type_T(x), xframe, jl_some_Type_T(y), yframe, d, e);
    assert(0 && "unexpected term with dangling references");
    return 0;
}

static int merge_env(jl_stenv_t *e, jl_savedenv_t *me, jl_savedenv_t *se, int count) JL_CANSAFEPOINT
{
    if (count == 0) {
        save_env(e, me, 1);
        return 1;
    }
    assert(se->len == me->len && se->len == current_env_length(e));
    int n = 0;
    jl_varbinding_t *v = e->vars;
    while (v != NULL) {
        jl_savedvar_t *sv = &se->buf[n], *mv = &me->buf[n];
        // merge `lb`: the meet of the branches' bounds (under-estimated);
        // the intersection code keeps its bounds as single types
        if (!(mv->lbs == sv->lbs || v->lbs == sv->lbs) && mv->lbs != v->lbs) {
            jl_value_t *b1 = lterm_type(e, mv->lbs, 0);
            JL_GC_PUSH1(&b1);
            jl_value_t *b2 = lterm_type(e, v->lbs, 0);
            jl_value_t *m = simple_meet(b1, b2, 0);
            JL_GC_POP();
            if (m == jl_bottom_type)
                mv->lbs = NULL;
            else {
                JL_GC_PUSH1(&m);
                stenv_root(e, m);
                mv->lbs = lterm_cons(e, m, NULL, NULL);
                JL_GC_POP();
            }
        }
        else if (mv->lbs != sv->lbs && v->lbs == sv->lbs)
            mv->lbs = sv->lbs;
        // merge `ub`: the join
        if (!(mv->ubs == sv->ubs || v->ubs == sv->ubs) && mv->ubs != v->ubs) {
            jl_value_t *b1 = lterm_type(e, mv->ubs, 1);
            JL_GC_PUSH1(&b1);
            jl_value_t *b2 = lterm_type(e, v->ubs, 1);
            jl_value_t *m = simple_join(b1, b2);
            JL_GC_POP();
            if (m == (jl_value_t*)jl_any_type)
                mv->ubs = NULL;
            else {
                JL_GC_PUSH1(&m);
                stenv_root(e, m);
                mv->ubs = lterm_cons(e, m, NULL, NULL);
                JL_GC_POP();
            }
        }
        else if (mv->ubs != sv->ubs && v->ubs == sv->ubs)
            mv->ubs = sv->ubs;
        // merge `innervars`
        if (v->innervars != NULL && mv->innervars != v->innervars) {
            if (mv->innervars != NULL)
                jl_array_ptr_1d_append(mv->innervars, v->innervars);
            else
                mv->innervars = v->innervars;
        }
        // merge occurs/occurs_inv/cov/cov_diag by max (never decrease)
        if (v->occurs > mv->occurs)
            mv->occurs = v->occurs;
        if (v->occurs_inv > mv->occurs_inv)
            mv->occurs_inv = v->occurs_inv;
        if (v->occurs_cov > mv->occurs_cov)
            mv->occurs_cov = v->occurs_cov;
        if (v->cov_diag > mv->cov_diag)
            mv->cov_diag = v->cov_diag;
        // merge max_offset by min
        if (!v->intersected && v->max_offset < mv->max_offset)
            mv->max_offset = v->max_offset;
        // required lower-bound evidence must hold for every merged branch
        if (!v->lb_required)
            mv->lb_required = 0;
        // the merged binding's spelling is only as authoritative as its
        // weakest contributor
        if (v->lb_spell < mv->lb_spell)
            mv->lb_spell = v->lb_spell;
        if (!v->pinned)
            mv->pinned = 0;
        n++;
        v = v->prev;
    }
    assert(n == se->len);
    return count + 1;
}

static jl_value_t *intersect_all(jl_value_t *x, jl_value_t *y, jl_stenv_t *e)
{
    e->Runions.depth = 0;
    e->Runions.more = 0;
    e->Runions.used = 0;
    jl_value_t **is;
    JL_GC_PUSHARGS(is, 2);
    jl_savedenv_t se, me;
    save_env(e, &se, 1);
    int niter = 0, total_iter = 0;
    is[0] = intersect(x, y, e, PARAM_NONE); // root
    if (is[0] == jl_bottom_type) {
        restore_env(e, &se, 1);
    }
    else if (!e->emptiness_only && has_next_union_state(e, 1)) {
        niter = merge_env(e, &me, &se, niter);
        restore_env(e, &se, 1);
    }
    while (next_union_state(e, 1)) {
        if (e->emptiness_only && is[0] != jl_bottom_type)
            break;
        e->Runions.depth = 0;
        e->Runions.more = 0;

        is[1] = intersect(x, y, e, PARAM_NONE);
        if (is[1] == jl_bottom_type) {
            restore_env(e, &se, 1);
        }
        else if (niter > 0 || (!e->emptiness_only && has_next_union_state(e, 1))) {
            niter = merge_env(e, &me, &se, niter);
            restore_env(e, &se, 1);
        }
        else {
            assert(is[0] == jl_bottom_type);
        }
        if (is[0] == jl_bottom_type)
            is[0] = is[1];
        else if (is[1] != jl_bottom_type) {
            // TODO: the repeated subtype checks in here can get expensive
            is[0] = jl_type_union(is, 2);
        }
        total_iter++;
        if (has_next_union_state(e, 1) && (niter > 4 || total_iter > 400000)) {
            is[0] = y;
            // we give up precise intersection here, just restore the saved env
            restore_env(e, &se, 1);
            if (niter > 0) {
                free_env(&me);
                niter = 0;
            }
            break;
        }
    }
    if (niter) {
        restore_env(e, &me, 1);
        free_env(&me);
    }
    free_env(&se);
    JL_GC_POP();
    return is[0];
}

// type intersection entry points

static jl_value_t *intersect_types(jl_value_t *x, jl_value_t *y, int emptiness_only) JL_CANSAFEPOINT
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
    jl_starena_t arena = {NULL, NULL};
    e.arena = &arena;
    JL_GC_PUSH4(&e.opened, &e.substmemo, &e.roots, &e.finalvars);
    e.intersection = 1;
    e.emptiness_only = emptiness_only;
    jl_value_t *ans = intersect_all(x, y, &e);
    free_stenv(&e);
    JL_GC_POP();
    return ans;
}

JL_DLLEXPORT jl_value_t *jl_intersect_types(jl_value_t *x, jl_value_t *y) JL_CANSAFEPOINT
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
    jl_svec_t *vec = jl_alloc_svec(ntvars);
    jl_unionall_t *ua = (jl_unionall_t*)u;
    int i;
    JL_GC_PUSH1(&vec);
    for (i = 0; i < ntvars; i++) {
        assert(jl_is_unionall(ua));
        // materialize only the binder (bounds re-expressed against the
        // variables already collected); the body is never instantiated
        jl_tvar_t *v = jl_unionall_bind_var(ua, vec, i);
        jl_svecset(vec, i, v);
        ua = (jl_unionall_t*)ua->body;
    }
    JL_GC_POP();
    return vec;
}

// For (possibly unions or unionalls of) tuples `a` and `b`, return the tuple of
// pointwise unions. Note that this may in general be wider than `Union{a,b}`.
// If `a` and `b` are not (non va-)tuples of equal length (or unions or unionalls
// of such), return NULL.
static jl_value_t *switch_union_tuple(jl_value_t *a, jl_value_t *b) JL_CANSAFEPOINT
{
    if (jl_is_unionall(a)) {
        jl_unionall_t *ua = (jl_unionall_t*)a;
        jl_value_t *ans = NULL;
        if (jl_is_unionall(b)) {
            jl_unionall_t *ub = (jl_unionall_t*)b;
            if (ub->lb == ua->lb && ub->ub == ua->ub) {
                // aligned binders: the bodies' references to them already
                // agree positionally, so recurse raw and rebuild the node
                ans = switch_union_tuple(ua->body, ub->body);
                if (ans != NULL) {
                    JL_GC_PUSH1(&ans);
                    ans = jl_new_unionall_raw(ua->name, ua->lb, ua->ub, ans);
                    JL_GC_POP();
                }
                return ans;
            }
        }
        // sole binder: `b` contains no references to it
        ans = switch_union_tuple(ua->body, b);
        if (ans != NULL) {
            JL_GC_PUSH1(&ans);
            ans = jl_new_unionall_raw(ua->name, ua->lb, ua->ub, ans);
            JL_GC_POP();
        }
        return ans;
    }
    if (jl_is_unionall(b)) {
        jl_unionall_t *ub = (jl_unionall_t*)b;
        jl_value_t *ans = switch_union_tuple(a, ub->body);
        if (ans != NULL) {
            JL_GC_PUSH1(&ans);
            ans = jl_new_unionall_raw(ub->name, ub->lb, ub->ub, ans);
            JL_GC_POP();
        }
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
    jl_value_t *ans = jl_apply_tuple_type(vec, 1);
    JL_GC_POP();
    return ans;
}

// `a` might have a non-empty intersection with some concrete type b even if !(a<:b) and !(b<:a)
// For example a=`Tuple{Type{<:Vector}}` and b=`Tuple{DataType}`
// TODO: this query is partly available memoized as jl_type_equality_is_identity
static int might_intersect_concrete(jl_value_t *a) JL_NOTSAFEPOINT
{
    if (jl_is_unionall(a))
        a = jl_unwrap_unionall(a);
    if (jl_is_typevar(a) || jl_is_tvarref(a))
        return 1; // (maybe)
    if (jl_is_uniontype(a))
        return might_intersect_concrete(((jl_uniontype_t*)a)->a) ||
               might_intersect_concrete(((jl_uniontype_t*)a)->b);
    if (jl_is_vararg(a))
        return might_intersect_concrete(jl_unwrap_vararg(a));
    if (jl_is_some_Type(a))
        return 1;
    if (jl_is_datatype(a)) {
        int tpl = jl_is_tuple_type(a);
        int i, n = jl_nparams(a);
        for (i = 0; i < n; i++) {
            jl_value_t *p = jl_tparam(a, i);
            if (jl_is_typevar(p) || jl_is_tvarref(p))
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
    if (jl_is_typeapp(a) || jl_is_typeapp(b))
        jl_error("internal error: TypeApp in type intersection");
    int szb = penv ? jl_subtype_env_size(b) : 0;
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
    // else if (lta && ltb) { // !jl_type_equality_is_identity known in this case because obviously_disjoint returned false
    //     goto bot;
    // }
    else if (jl_subtype(b, a)) {
        *ans = b;
    }
    else {
        // TODO: these tests could probably be ordered better with above
        if (lta && !might_intersect_concrete(b))
            goto bot;
        if (ltb && !might_intersect_concrete(a))
            goto bot;
        // A dispatch tuple is a concrete leaf type, so its intersection with any other
        // type is just itself (when it is a subtype) or empty. The subtype checks above
        // having failed, the intersection must be empty.
        if (jl_is_dispatch_tupletype(a) || jl_is_dispatch_tupletype(b))
            goto bot;
        jl_stenv_t e;
        init_stenv(&e, NULL, 0);
        jl_starena_t arena = {NULL, NULL};
        e.arena = &arena;
        JL_GC_PUSH4(&e.opened, &e.substmemo, &e.roots, &e.finalvars);
        e.intersection = 1;
        e.envout = env;
        if (szb)
            memset(env, 0, szb*sizeof(void*));
        e.envsz = szb;
        *ans = intersect_all(a, b, &e);
        free_stenv(&e);
        JL_GC_POP();
        if (*ans == jl_bottom_type) goto bot;
        // TODO: code dealing with method signatures is not able to handle unions, so if
        // `a` and `b` are both tuples, we need to be careful and may not return a union,
        // even if `intersect` produced one
        if (jl_is_tuple_type(jl_unwrap_unionall(a)) && jl_is_tuple_type(jl_unwrap_unionall(b)) &&
            !jl_is_datatype(jl_unwrap_unionall(*ans))) {
            jl_value_t *ans_unwrapped = jl_unwrap_unionall(*ans);
            JL_GC_PUSH1(&ans_unwrapped);
            if (jl_is_uniontype(ans_unwrapped)) {
                ans_unwrapped = switch_union_tuple(((jl_uniontype_t*)ans_unwrapped)->a, ((jl_uniontype_t*)ans_unwrapped)->b);
                if (ans_unwrapped != NULL) {
                    *ans = jl_rewrap_unionall_(ans_unwrapped, *ans);
                }
            }
            JL_GC_POP();
            if (!jl_is_datatype(jl_unwrap_unionall(*ans))) {
                // Bail: caller can't handle a non-datatype here. The env computed
                // by `intersect` is meaningless after this assignment, but the
                // subtype call below recovers a usable env via the `x == y` fast
                // path in `jl_subtype_env` (typevars from `b`).
                *ans = b;
            }
        }
        sz = szb;
        // TODO: compute better `env` directly during intersection.
        // for now, we attempt to compute env by using subtype on the intersection result
        if (szb > 0 && !jl_types_equal(b, (jl_value_t*)jl_type_type)) {
            if (!jl_subtype_env(*ans, b, env, szb)) {
                sz = 0;
            }
        }
    }
    if (sz > 0 && szb > 0) {
        for (i = 0; i < sz; i++) {
            if (!env[i]) {
                sz = 0;
                break;
            }
        }
    }
    if (sz == 0 && szb > 0) {
        jl_unionall_t *ub = (jl_unionall_t*)b;
        jl_svec_t *vars = jl_alloc_svec(szb);
        JL_GC_PUSH1(&vars);
        size_t nvars = 0;
        while (jl_is_unionall(ub)) {
            // materialize only the binder; the occurrence scan is positional
            jl_tvar_t *v = jl_unionall_bind_var(ub, vars, nvars);
            jl_svecset(vars, nvars, v);
            nvars++;
            int constrained = constrains_ref_static(1, ub->lb == jl_bottom_type, ub->body, 1);
            env[i++] = wrap_tvar_env((jl_value_t*)v, constrained);
            ub = (jl_unionall_t*)ub->body;
        }
        JL_GC_POP();
        sz = szb;
    }
    if (penv) {
        jl_svec_t *e = jl_alloc_svec(sz);
        for (i = 0; i < sz; i++) {
            assert(env[i]);
            jl_svecset(e, i, env[i]);
        }
        *penv = e;
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

JL_DLLEXPORT jl_svec_t *jl_type_intersection_with_env(jl_value_t *a, jl_value_t *b) JL_CANSAFEPOINT
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
        for (i = 0; i < szb; i++) {
            assert(env[i]);
            jl_svecset(e, i, env[i]);
        }
        *penv = e;
    }
    JL_GC_POP();
    return sub;
}

// type utils
static void check_diagonal(jl_value_t *t, jl_varbinding_t *troot, jl_param_pos_t param)
{
    if (jl_is_uniontype(t)) {
        int i, len = 0;
        jl_varbinding_t *v;
        for (v = troot; v != NULL; v = v->prev)
            len++;
        // 3 bytes per var: [occurs_inv, occurs_cov, cov_diag].
        int8_t *occurs = (int8_t *)alloca(len * 3);
        for (v = troot, i = 0; v != NULL; v = v->prev, i++) {
            occurs[i*3]   = v->occurs_inv;
            occurs[i*3+1] = v->occurs_cov;
            occurs[i*3+2] = v->cov_diag;
        }
        jl_value_t *a = ((jl_uniontype_t *)t)->a;
        check_diagonal(a, troot, param);
        for (v = troot, i = 0; v != NULL; v = v->prev, i++) {
            int8_t a_inv = v->occurs_inv;
            int8_t a_cov = v->occurs_cov;
            int8_t a_diag = v->cov_diag;
            v->occurs_inv = occurs[i*3];
            v->occurs_cov = occurs[i*3+1];
            v->cov_diag   = occurs[i*3+2];
            occurs[i*3]   = a_inv;
            occurs[i*3+1] = a_cov;
            occurs[i*3+2] = a_diag;
        }
        jl_value_t *b = ((jl_uniontype_t *)t)->b;
        check_diagonal(b, troot, param);
        for (v = troot, i = 0; v != NULL; v = v->prev, i++) {
            if (v->occurs_inv < occurs[i*3])
                v->occurs_inv = occurs[i*3];
            if (v->occurs_cov < occurs[i*3+1])
                v->occurs_cov = occurs[i*3+1];
            if (v->cov_diag  < occurs[i*3+2])
                v->cov_diag  = occurs[i*3+2];
        }
    }
    else if (jl_is_unionall(t)) {
        // an inner binder cannot shadow the (free) variables being counted;
        // occurrences inside its bounds still count through the reference leaf
        check_diagonal(((jl_unionall_t *)t)->body, troot, param);
    }
    else if (jl_is_some_Type(t)) {
        jl_value_t *T = jl_some_Type_T(t);
        check_diagonal(T, troot, PARAM_INVARIANT);
    }
    else if (jl_is_datatype(t)) {
        jl_param_pos_t nparam = jl_is_tuple_type(t) ? PARAM_COVARIANT : PARAM_INVARIANT;
        if (nparam < param) nparam = param;
        for (size_t i = 0; i < jl_nparams(t); i++) {
            jl_value_t *p = jl_tparam(t, i);
            check_diagonal(p, troot, nparam);
        }
    }
    else if (jl_is_vararg(t)) {
        jl_value_t *T = jl_unwrap_vararg(t);
        jl_value_t *N = jl_unwrap_vararg_num(t);
        int n = (N && jl_is_long(N)) ? jl_unbox_long(N) : 2;
        if (T && n > 0) check_diagonal(T, troot, param);
        if (T && n > 1) check_diagonal(T, troot, param);
        if (N)          check_diagonal(N, troot, PARAM_INVARIANT);
    }
    else if (jl_is_typevar(t)) {
        jl_varbinding_t *v = troot;
        for (; v != NULL; v = v->prev) {
            if (v->var == (jl_tvar_t *)t) {
                if (param == PARAM_COVARIANT && v->occurs_cov < 2) v->occurs_cov++;
                if (param == PARAM_INVARIANT && v->occurs_inv < 2) v->occurs_inv++;
                break;
            }
        }
        if (v == NULL) {
            jl_value_t *ub = ((jl_tvar_t *)t)->ub;
            check_diagonal(ub, troot, PARAM_NONE);
        }
    }
    // n.b. TypeVarRef leaves are bound occurrences of inner binders; the
    // counted variables are free, so they never match
}

static jl_value_t *insert_nondiagonal(jl_value_t *type, jl_varbinding_t *troot, int widen2ub) JL_CANSAFEPOINT
{
    if (jl_is_typevar(type)) {
        int concretekind = widen2ub > 1 ? 0 : 1;
        jl_varbinding_t *v = troot;
        for (; v != NULL; v = v->prev) {
            if (v->occurs_inv == 0 &&
                cov_count(v) > concretekind &&
                v->var == (jl_tvar_t *)type)
                break;
        }
        if (v != NULL) {
            if (widen2ub) {
                jl_value_t *ub = ((jl_tvar_t *)type)->ub;
                type = insert_nondiagonal(ub, troot, 2);
            }
            else {
                // we must replace each covariant occurrence of newvar with a different newvar2<:newvar (diagonal rule)
                if (v->innervars == NULL)
                    v->innervars = jl_alloc_array_1d(jl_array_any_type, 0);
                jl_value_t *newvar = NULL, *lb = v->var->lb, *ub = (jl_value_t *)v->var;
                jl_array_t *innervars = v->innervars;
                JL_GC_PUSH4(&newvar, &lb, &ub, &innervars);
                newvar = (jl_value_t *)jl_new_typevar(v->var->name, lb, ub);
                jl_array_ptr_1d_push(innervars, newvar);
                JL_GC_POP();
                type = newvar;
            }
        }
    }
    else if (jl_is_unionall(type)) {
        jl_unionall_t *ua = (jl_unionall_t*)type;
        jl_value_t *newbody = NULL, *newub = NULL;
        JL_GC_PUSH3(&newbody, &newub, &type);
        // the replaced variables are free, so the binder needs no removal dance;
        // its bound occurrences (references) are unaffected by the substitution
        newbody = insert_nondiagonal(ua->body, troot, widen2ub);
        // n.b. we do not widen lb, since that would be the wrong direction
        newub = insert_nondiagonal(ua->ub, troot, widen2ub);
        if (newbody != ua->body || newub != ua->ub)
            type = jl_new_unionall_raw(ua->name, ua->lb, newub, newbody);
        JL_GC_POP();
    }
    else if (jl_is_uniontype(type)) {
        jl_value_t *a = ((jl_uniontype_t*)type)->a;
        jl_value_t *b = ((jl_uniontype_t*)type)->b;
        jl_value_t *newa = NULL;
        jl_value_t *newb = NULL;
        JL_GC_PUSH2(&newa, &newb);
        newa = insert_nondiagonal(a, troot, widen2ub);
        newb = insert_nondiagonal(b, troot, widen2ub);
        if (newa != a || newb != b)
            type = simple_union(newa, newb);
        JL_GC_POP();
    }
    else if (jl_is_vararg(type)) {
        // As for Vararg we'd better widen its var to ub as otherwise they are still diagonal
        jl_value_t *t = jl_unwrap_vararg(type);
        jl_value_t *n = jl_unwrap_vararg_num(type);
        if (widen2ub == 0)
            widen2ub = !(n && jl_is_long(n)) || jl_unbox_long(n) > 1;
        jl_value_t *newt = insert_nondiagonal(t, troot, widen2ub);
        if (t != newt) {
            JL_GC_PUSH1(&newt);
            type = (jl_value_t *)jl_wrap_vararg(newt, n, 0, 0);
            JL_GC_POP();
        }
    }
    else if (jl_is_typeeq(type)) {
        jl_value_t *T = jl_typeeq_T(type);
        jl_value_t *newT = insert_nondiagonal(T, troot, 1);
        if (T != newT) {
            JL_GC_PUSH1(&newT);
            type = (jl_value_t*)jl_wrap_Type(newT);
            JL_GC_POP();
        }
    }
    else if (jl_is_typeegal(type)) {
        // a no-op for closed `T`; kept parallel to the `TypeEq` case
        jl_value_t *T = jl_typeegal_T(type);
        jl_value_t *newT = insert_nondiagonal(T, troot, 1);
        if (T != newT) {
            JL_GC_PUSH1(&newT);
            type = jl_wrap_TypeEgal(newT);
            JL_GC_POP();
        }
    }
    else if (jl_is_datatype(type)) {
        if (jl_is_tuple_type(type)) {
            jl_svec_t *newparams = NULL;
            jl_value_t *newelt = NULL;
            JL_GC_PUSH2(&newparams, &newelt);
            for (size_t i = 0; i < jl_nparams(type); i++) {
                jl_value_t *elt = jl_tparam(type, i);
                newelt = insert_nondiagonal(elt, troot, widen2ub);
                if (elt != newelt) {
                    if (!newparams)
                        newparams = jl_svec_copy(((jl_datatype_t*)type)->parameters);
                    jl_svecset(newparams, i, newelt);
                }
            }
            if (newparams)
                type = (jl_value_t*)jl_apply_tuple_type(newparams, 1);
            JL_GC_POP();
        }
    }
    return type;
}

static jl_value_t *_widen_diagonal(jl_value_t *t, jl_varbinding_t *troot) JL_CANSAFEPOINT {
    check_diagonal(t, troot, PARAM_NONE);
    int any_concrete = 0;
    for (jl_varbinding_t *v = troot; v != NULL; v = v->prev)
        any_concrete |= cov_count(v) > 1 && v->occurs_inv == 0;
    if (!any_concrete)
        return t; // no diagonal
    return insert_nondiagonal(t, troot, 0);
}

static jl_value_t *widen_diagonal(jl_value_t *t, jl_unionall_t *u, jl_varbinding_t *troot,
                                  jl_svec_t *outervars, size_t nouter) JL_CANSAFEPOINT
{
    // `t` is a term derived from under `u`'s binder chain. The machinery below
    // constructs new binders whose placement (and whose bound fields) refer to
    // this binder from depths that are unknown until every wrap has happened,
    // so it works with position-free tokens: materialize the binder's variable
    // -- bounds only, re-expressed against the outer variables; the body is
    // never instantiated -- rebind `t`'s references to it, process, and
    // translate the variable back into references at the end.
    size_t nremaining = jl_subtype_env_size((jl_value_t*)u);
    jl_varbinding_t vb;
    memset(&vb, 0, sizeof(vb));
    vb.existential = 1;
    vb.prev = troot;
    jl_value_t *nt = NULL;
    JL_GC_PUSH2(&vb.innervars, &nt);
    jl_tvar_t *v = jl_unionall_bind_var(u, outervars, nouter);
    jl_svecset(outervars, nouter, v);
    vb.var = v;
    nt = jl_substitute_tvarref(t, nremaining, (jl_value_t*)v);
    if (jl_is_unionall(u->body))
        nt = widen_diagonal(nt, (jl_unionall_t *)u->body, &vb, outervars, nouter + 1);
    else
        nt = _widen_diagonal(nt, &vb);
    if (vb.innervars != NULL) {
        for (size_t i = 0; i < jl_array_nrows(vb.innervars); i++) {
            jl_tvar_t *var = (jl_tvar_t*)jl_array_ptr_ref(vb.innervars, i);
            nt = jl_type_unionall(var, nt);
        }
    }
    nt = jl_translate_var_to_ref(nt, v, nremaining);
    JL_GC_POP();
    return nt;
}

JL_DLLEXPORT jl_value_t *jl_widen_diagonal(jl_value_t *t, jl_unionall_t *ua) JL_CANSAFEPOINT
{
    jl_svec_t *outervars = jl_alloc_svec(jl_subtype_env_size((jl_value_t*)ua));
    JL_GC_PUSH1(&outervars);
    jl_value_t *nt = widen_diagonal(t, ua, NULL, outervars, 0);
    JL_GC_POP();
    return nt;
}

// specificity comparison
// positional environment of the specificity walk: one node per stripped
// `where` binder (per side, innermost first), carrying the binder and the
// occurrence count of its references in the body it was stripped from
typedef struct jl_specenv_t {
    jl_unionall_t *u;
    int count;
    struct jl_specenv_t *prev;
} jl_specenv_t;

static jl_specenv_t *specenv_lookup(jl_specenv_t *env, size_t depth) JL_NOTSAFEPOINT
{
    while (env != NULL && depth > 1) {
        env = env->prev;
        depth--;
    }
    return env;
}

// how many times does the binder `t` resolves to occur in the body it was
// stripped from? (a free TypeVar was never counted into a chain)
static int spec_num_occurs(jl_value_t *t, jl_specenv_t *env) JL_NOTSAFEPOINT
{
    if (jl_is_tvarref(t)) {
        jl_specenv_t *node = specenv_lookup(env, jl_tvarref_depth(t));
        return node == NULL ? 0 : node->count;
    }
    return 0;
}

static int sub_msp(jl_value_t *x, jl_value_t *y, jl_value_t *y0, jl_specenv_t *xenv, jl_specenv_t *yenv) JL_CANSAFEPOINT;
static int eq_msp(jl_value_t *a, jl_value_t *b, jl_value_t *a0, jl_value_t *b0, jl_specenv_t *aenv, jl_specenv_t *benv) JL_CANSAFEPOINT;



static int eq_msp(jl_value_t *a, jl_value_t *b, jl_value_t *a0, jl_value_t *b0, jl_specenv_t *aenv, jl_specenv_t *benv) JL_CANSAFEPOINT
{
    if (!(jl_is_type(a) || jl_is_typevar(a)) ||
        !(jl_is_type(b) || jl_is_typevar(b)))
        return jl_egal(a, b);
    // close each side over its own chain up front: identical raw spellings
    // under two different chains can denote different binders, so every
    // structural comparison below must see the closed forms
    if (jl_has_dangling_tvarrefs(a) || jl_has_dangling_tvarrefs(b)) {
        JL_GC_PUSH2(&a, &b);
        for (jl_specenv_t *env = aenv; env != NULL; env = env->prev)
            a = jl_rewrap_unionall_one(a, env->u);
        for (jl_specenv_t *env = benv; env != NULL; env = env->prev)
            b = jl_rewrap_unionall_one(b, env->u);
        int ret = eq_msp(a, b, a0, b0, NULL, NULL);
        JL_GC_POP();
        return ret;
    }
    if (a == b)
        return 1;
    if (jl_typeof(a) == jl_typeof(b) && jl_types_struct_equiv(a, b))
        return 1;
    if (obviously_unequal(a, b))
        return 0;
    // the following is an interleaved version of:
    //   return jl_type_equal(a, b)
    // where we try to do the fast checks before the expensive ones
    if (jl_is_datatype(a) && !jl_is_concrete_type(b)) {
        // if one type looks simpler, check it on the right
        // first in order to reject more quickly.
        jl_value_t *temp = a;
        a = b;
        b = temp;
        jl_specenv_t *tempenv = aenv;
        aenv = benv;
        benv = tempenv;
    }
    // first check if a <: b has an obvious answer
    int subtype_ab = 2;
    if (b == (jl_value_t*)jl_any_type || a == jl_bottom_type) {
        subtype_ab = 1;
    }
    else if (obvious_subtype(a, b, b0, &subtype_ab)) {
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
    else if (obvious_subtype(b, a, a0, &subtype_ba)) {
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
    jl_starena_t arena = {NULL, NULL};
    e.opened = NULL;
    e.substmemo = NULL;
    e.roots = NULL;
    e.finalvars = NULL;
    JL_GC_PUSH6(&a, &b, &e.opened, &e.substmemo, &e.roots, &e.finalvars);
#ifdef NDEBUG
    if (subtype_ab != 1)
#endif
    {
        init_stenv(&e, NULL, 0);
        e.arena = &arena;
        int subtype = forall_exists_subtype(a, b, &e, PARAM_NONE);
        free_stenv(&e);
        assert(subtype_ab == 3 || subtype_ab == subtype || jl_has_free_typevars(a) || jl_has_free_typevars(b) || jl_has_dangling_tvarrefs(a) || jl_has_dangling_tvarrefs(b));
#ifndef NDEBUG
        if (subtype_ab != 0 && subtype_ab != 1) // ensures that running in a debugger doesn't change the result
#endif
        subtype_ab = subtype;
#ifdef NDEBUG
        if (subtype_ab == 0) {
            JL_GC_POP();
            return 0;
        }
#endif
    }
#ifdef NDEBUG
    if (subtype_ba != 1)
#endif
    {
        init_stenv(&e, NULL, 0);
        e.arena = &arena;
        int subtype = forall_exists_subtype(b, a, &e, PARAM_NONE);
        free_stenv(&e);
        assert(subtype_ba == 3 || subtype_ba == subtype || jl_has_free_typevars(a) || jl_has_free_typevars(b) || jl_has_dangling_tvarrefs(a) || jl_has_dangling_tvarrefs(b));
#ifndef NDEBUG
        if (subtype_ba != 0 && subtype_ba != 1) // ensures that running in a debugger doesn't change the result
#endif
        subtype_ba = subtype;
    }
    JL_GC_POP();
    // all tests successful
    return subtype_ab && subtype_ba;
}

static int sub_msp(jl_value_t *x, jl_value_t *y, jl_value_t *y0, jl_specenv_t *xenv, jl_specenv_t *yenv) JL_CANSAFEPOINT
{
    jl_stenv_t e;
    if (y == (jl_value_t*)jl_any_type || x == jl_bottom_type)
        return 1;
    // close each side over its own chain up front: identical raw spellings
    // under two different chains can denote different binders, so every
    // structural comparison below must see the closed forms
    if (jl_has_dangling_tvarrefs(x) || jl_has_dangling_tvarrefs(y)) {
        JL_GC_PUSH2(&x, &y);
        for (jl_specenv_t *env = xenv; env != NULL; env = env->prev)
            x = jl_rewrap_unionall_one(x, env->u);
        for (jl_specenv_t *env = yenv; env != NULL; env = env->prev)
            y = jl_rewrap_unionall_one(y, env->u);
        int ret = sub_msp(x, y, y0, NULL, NULL);
        JL_GC_POP();
        return ret;
    }
    if (x == y ||
        (jl_typeof(x) == jl_typeof(y) &&
         (jl_is_unionall(y) || jl_is_uniontype(y)) &&
         jl_types_struct_equiv(x, y))) {
        return 1;
    }
    int obvious_sub = 2;
    if (obvious_subtype(x, y, y0, &obvious_sub)) {
#ifdef NDEBUG
        return obvious_sub;
#endif
    }
    else {
        obvious_sub = 3;
    }
    jl_starena_t arena = {NULL, NULL};
    e.opened = NULL;
    e.substmemo = NULL;
    e.roots = NULL;
    e.finalvars = NULL;
    JL_GC_PUSH6(&x, &y, &e.opened, &e.substmemo, &e.roots, &e.finalvars);
    init_stenv(&e, NULL, 0);
    e.arena = &arena;
    int subtype = forall_exists_subtype(x, y, &e, PARAM_NONE);
    free_stenv(&e);
    assert(obvious_sub == 3 || obvious_sub == subtype || jl_has_free_typevars(x) || jl_has_free_typevars(y) || jl_has_dangling_tvarrefs(x) || jl_has_dangling_tvarrefs(y));
#ifndef NDEBUG
    if (obvious_sub == 0 || obvious_sub == 1)
        subtype = obvious_sub; // this ensures that running in a debugger doesn't change the result
#endif
    JL_GC_POP();
    return subtype;
}

static int type_morespecific_(jl_value_t *a, jl_value_t *b, jl_value_t *a0, jl_value_t *b0, int invariant, jl_specenv_t *aenv, jl_specenv_t *benv) JL_CANSAFEPOINT;

// resolve a possible bound-variable reference to an equivalent (free)
// variable carrying the binder's bounds shifted into the comparison position
// (their own references then resolve through the same chain); other values
// pass through. NULL for a detached reference: no bound information.
static jl_value_t *spec_resolve_ref(jl_value_t *t, jl_specenv_t *env) JL_CANSAFEPOINT
{
    if (!jl_is_tvarref(t))
        return t;
    jl_specenv_t *node = specenv_lookup(env, jl_tvarref_depth(t));
    if (node == NULL)
        return NULL;
    ssize_t d = (ssize_t)jl_tvarref_depth(t);
    jl_value_t *lb = NULL, *ub = NULL;
    JL_GC_PUSH2(&lb, &ub);
    lb = jl_shift_dangling_refs(node->u->lb, d);
    ub = jl_shift_dangling_refs(node->u->ub, d);
    jl_value_t *v = (jl_value_t*)jl_new_typevar_raw(node->u->name, lb, ub);
    JL_GC_POP();
    return v;
}

static jl_value_t *nth_tuple_elt(jl_datatype_t *t JL_PROPAGATES_ROOT, size_t i) JL_NOTSAFEPOINT
{
    size_t len = jl_nparams(t);
    if (len == 0)
        return NULL;
    if (i < len-1)
        return jl_tparam(t, i);
    jl_value_t *last = jl_unwrap_unionall(jl_tparam(t, len-1));
    if (jl_is_vararg(last)) {
        jl_value_t *n = jl_unwrap_vararg_num(last);
        if (n && jl_is_long(n) && i >= len-1+jl_unbox_long(n))
            return NULL;
        return jl_unwrap_vararg(last);
    }
    if (i == len-1)
        return jl_tparam(t, i);
    return NULL;
}

static int tuple_morespecific(jl_datatype_t *cdt, jl_datatype_t *pdt, jl_value_t *c0, jl_value_t *p0, int invariant, jl_specenv_t *cenv, jl_specenv_t *penv) JL_CANSAFEPOINT
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

        if (type_morespecific_(pe, ce, p0, c0, invariant, penv, cenv)) {
            assert(!type_morespecific_(ce, pe, c0, p0, invariant, cenv, penv));
            return 0;
        }

        if (!cdiag && (jl_is_typevar(ce) || jl_is_tvarref(ce)) && spec_num_occurs(ce, cenv) > 1)
            cdiag = 1;
        if (!pdiag && (jl_is_typevar(pe) || jl_is_tvarref(pe)) && spec_num_occurs(pe, penv) > 1)
            pdiag = 1;

        // in Tuple{a,b...} and Tuple{c,d...} allow b and d to be disjoint
        if (cva && pva && i >= clen-1 && i >= plen-1 && (some_morespecific || (cdiag && !pdiag)))
            return 1;

        int cms = type_morespecific_(ce, pe, c0, p0, invariant, cenv, penv);

        if (!cms && !sub_msp(ce, pe, p0, cenv, penv)) {
            /*
              A bound vararg tuple can be more specific despite disjoint elements in order to
              preserve transitivity. For example in
              A = Tuple{Array{T,N}, Vararg{Int,N}} where {T,N}
              B = Tuple{Array, Int}
              C = Tuple{AbstractArray, Int, Array}
              we need A < B < C and A < C.
            */
            return some_morespecific && cva && ckind == JL_VARARG_BOUND && spec_num_occurs(jl_unwrap_vararg_num(clast), cenv) > 1;
        }

        // Tuple{..., T} not more specific than Tuple{..., Vararg{S}} if S is diagonal
        if (!cms && i == clen-1 && clen == plen && !cva && pva && eq_msp(ce, pe, c0, p0, cenv, penv) &&
            (jl_is_typevar(ce) || jl_is_tvarref(ce)) && (jl_is_typevar(pe) || jl_is_tvarref(pe)) && !cdiag && pdiag)
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
    if (jl_is_vararg(last)) {
        jl_value_t *N = jl_unwrap_vararg_num(last);
        if (jl_is_long(N))
            n += jl_unbox_long(N)-1;
    }
    return n;
}

// Called when a is a bound-vararg and b is not a vararg. Sets the vararg length
// in a to match b, as long as this makes some earlier argument more specific.
static int args_morespecific_fix1(jl_value_t *a, jl_value_t *b, jl_value_t *a0, jl_value_t *b0, int swap, jl_specenv_t *aenv, jl_specenv_t *benv) JL_CANSAFEPOINT
{
    size_t n = jl_nparams(a);
    int taillen = tuple_full_length(b)-n+1;
    if (taillen <= 0)
        return -1;
    assert(jl_is_va_tuple((jl_datatype_t*)a));
    jl_value_t *num = jl_unwrap_vararg_num(jl_unwrap_unionall(jl_tparam(a, n-1)));
    jl_datatype_t *new_a = NULL;
    jl_value_t *boxlen = jl_box_long(taillen);
    JL_GC_PUSH2(&new_a, &boxlen);
    jl_specenv_t *new_aenv = aenv;
    int changed = 0;
    if (jl_is_tvarref(num)) {
        // a positional count: does it constrain the fixed prefix?
        size_t d = jl_tvarref_depth(num);
        for (size_t i = 0; i < n-1 && !changed; i++)
            changed = jl_tvarref_occurs(jl_tparam(a, i), d);
        if (changed) {
            new_a = (jl_datatype_t*)jl_substitute_tvarref_nothrow((jl_value_t*)a, d, boxlen);
            if (new_a == NULL)
                changed = 0;
            else {
                // the substitution consumed the binder's level (deeper
                // references shifted down in step), so the result is
                // compared under the chain with that node dropped
                jl_specenv_t *node = specenv_lookup(aenv, d);
                jl_specenv_t *tail = node == NULL ? NULL : node->prev;
                size_t nprefix = d - 1;
                if (nprefix == 0)
                    new_aenv = tail;
                else {
                    jl_specenv_t *copies = (jl_specenv_t*)alloca(nprefix * sizeof(jl_specenv_t));
                    jl_specenv_t *src = aenv;
                    for (size_t i = 0; i < nprefix; i++, src = src->prev) {
                        copies[i] = *src;
                        copies[i].prev = (i + 1 < nprefix) ? &copies[i + 1] : tail;
                    }
                    new_aenv = &copies[0];
                }
            }
        }
    }
    else {
        jl_value_t *e[2] = { num, boxlen };
        new_a = (jl_datatype_t*)jl_instantiate_type_with((jl_value_t*)a, e, 1);
        for (size_t i = 0; i < n-1; i++) {
            if (jl_tparam(a, i) != jl_tparam(new_a, i)) {
                changed = 1;
                break;
            }
        }
    }
    int ret = -1;
    if (changed) {
        if (eq_msp(b, (jl_value_t*)new_a, b0, a0, benv, new_aenv))
            ret = swap;
        else if (swap)
            ret = type_morespecific_(b, (jl_value_t*)new_a, b0, a0, 0, benv, new_aenv);
        else
            ret = type_morespecific_((jl_value_t*)new_a, b, a0, b0, 0, new_aenv, benv);
    }
    JL_GC_POP();
    return ret;
}



// Return true if `t == Type{Union{}}`, so that `morespecific` ranks `t` above any other
// parameter. This must accept every `t` that subtyping proves `<: Type{Union{}}`, since the
// typemap's `Union{}` pruning relies on that ranking. The rule is sound because `Union{}`
// cannot appear covariantly in a tuple, so such a parameter admits exactly the `Union{}` calls.
// `t` is located under `env`: a reference to one of its binders is read through that
// binder's upper bound (a reference escaping `env` is not known to admit only `Union{}`).
int jl_is_typeofbottom_param_env(jl_value_t *t, jl_uaenv_t *env) JL_NOTSAFEPOINT
{
    if (t == NULL)
        return 0;
    while (1) {
        if (jl_is_unionall(t)) {
            jl_uaenv_t node = { (jl_unionall_t*)t, env };
            return jl_is_typeofbottom_param_env(((jl_unionall_t*)t)->body, &node);
        }
        else if (jl_is_typevar(t)) {
            t = ((jl_tvar_t*)t)->ub;
        }
        else if (jl_is_tvarref(t)) {
            jl_uaenv_t *b = uaenv_lookup(env, jl_tvarref_depth(t));
            if (b == NULL)
                return 0;
            t = b->ua->ub;
            env = b->prev;
        }
        else if (jl_is_uniontype(t)) {
            if (!jl_is_typeofbottom_param_env(((jl_uniontype_t*)t)->a, env))
                return 0;
            t = ((jl_uniontype_t*)t)->b;
        }
        else {
            return is_typeofbottom_typealias_env(t, env);
        }
    }
}

int jl_is_typeofbottom_param(jl_value_t *t) JL_NOTSAFEPOINT
{
    return jl_is_typeofbottom_param_env(t, NULL);
}

// the binder chain of a specificity environment, for the predicates above
// (`nodes` has room for the chain)
static jl_uaenv_t *specenv_uaenv(jl_specenv_t *env, jl_uaenv_t *nodes) JL_NOTSAFEPOINT
{
    size_t i = 0;
    for (jl_specenv_t *v = env; v != NULL; v = v->prev, i++) {
        nodes[i].ua = v->u;
        nodes[i].prev = v->prev == NULL ? NULL : &nodes[i + 1];
    }
    return env == NULL ? NULL : nodes;
}

static size_t specenv_length(jl_specenv_t *env) JL_NOTSAFEPOINT
{
    size_t n = 0;
    for (; env != NULL; env = env->prev)
        n++;
    return n;
}

static int tuple_cmp_typeofbottom(jl_datatype_t *a, jl_datatype_t *b, jl_specenv_t *aenv, jl_specenv_t *benv)
{
    jl_uaenv_t *anodes = (jl_uaenv_t*)alloca(sizeof(jl_uaenv_t) * (specenv_length(aenv) + 1));
    jl_uaenv_t *bnodes = (jl_uaenv_t*)alloca(sizeof(jl_uaenv_t) * (specenv_length(benv) + 1));
    jl_uaenv_t *ae = specenv_uaenv(aenv, anodes), *be = specenv_uaenv(benv, bnodes);
    size_t i, la = jl_nparams(a), lb = jl_nparams(b);
    for (i = 0; i < la || i < lb; i++) {
        jl_value_t *pa = i < la ? jl_tparam(a, i) : NULL;
        jl_value_t *pb = i < lb ? jl_tparam(b, i) : NULL;
        assert(jl_typeofbottom_type); // for clang-sa
        int xa = jl_is_typeofbottom_param_env(pa, ae);
        int xb = jl_is_typeofbottom_param_env(pb, be);
        if (xa != xb)
            return xa - xb;
    }
    return 0;
}


#define HANDLE_UNIONALL_A                                               \
    do {                                                                \
        jl_unionall_t *ua_ = (jl_unionall_t*)a;                         \
        jl_specenv_t newenv = { ua_, count_ref_occurs(ua_->body, 1), aenv }; \
        return type_morespecific_(ua_->body, b, a0, b0, invariant, &newenv, benv); \
    } while (0)

#define HANDLE_UNIONALL_B                                               \
    do {                                                                \
        jl_unionall_t *ub_ = (jl_unionall_t*)b;                         \
        jl_specenv_t newenv = { ub_, count_ref_occurs(ub_->body, 1), benv }; \
        return type_morespecific_(a, ub_->body, a0, b0, invariant, aenv, &newenv); \
    } while (0)

// the variable leaves of `type_morespecific_`: both operands are (free)
// variables or plain types here; a reference operand was resolved by the
// caller, with its occurrence count computed from its chain node
static int type_morespecific_var_(jl_value_t *a, jl_value_t *b, jl_value_t *a0, jl_value_t *b0, int invariant,
                                  jl_specenv_t *aenv, jl_specenv_t *benv, int acount, int bcount) JL_CANSAFEPOINT
{
    if (jl_is_typevar(a)) {
        if (jl_is_typevar(b)) {
            return (( type_morespecific_((jl_value_t*)((jl_tvar_t*)a)->ub,
                                         (jl_value_t*)((jl_tvar_t*)b)->ub, a0, b0, 0, aenv, benv) &&
                     !type_morespecific_((jl_value_t*)((jl_tvar_t*)a)->lb,
                                         (jl_value_t*)((jl_tvar_t*)b)->lb, a0, b0, 0, aenv, benv)) ||
                    ( type_morespecific_((jl_value_t*)((jl_tvar_t*)b)->lb,
                                         (jl_value_t*)((jl_tvar_t*)a)->lb, b0, a0, 0, benv, aenv) &&
                     !type_morespecific_((jl_value_t*)((jl_tvar_t*)b)->ub,
                                         (jl_value_t*)((jl_tvar_t*)a)->ub, b0, a0, 0, benv, aenv)));
        }
        if (!jl_is_type(b))
            return 0;
        if (invariant) {
            if (((jl_tvar_t*)a)->ub == jl_bottom_type)
                return 1;
            if (!has_free_or_dangling_typevars(b))
                return 0;
            if (eq_msp(((jl_tvar_t*)a)->ub, b, a0, b0, aenv, benv))
                return acount >= 2;
        }
        else {
            // need `{T,T} where T` more specific than `{Any, Any}`
            if (b == (jl_value_t*)jl_any_type && ((jl_tvar_t*)a)->ub == (jl_value_t*)jl_any_type &&
                acount >= 2)
                return 1;
        }
        return type_morespecific_(((jl_tvar_t*)a)->ub, b, a0, b0, 0, aenv, benv);
    }
    if (jl_is_typevar(b)) {
        if (!jl_is_type(a))
            return 1;
        if (invariant) {
            if (((jl_tvar_t*)b)->ub == jl_bottom_type)
                return 0;
            if (has_free_or_dangling_typevars(a)) {
                if (type_morespecific_(a, ((jl_tvar_t*)b)->ub, a0, b0, 0, aenv, benv))
                    return 1;
                if (eq_msp(a, ((jl_tvar_t*)b)->ub, a0, b0, aenv, benv))
                    return bcount < 2;
                return 0;
            }
            else {
                if (obviously_disjoint(a, ((jl_tvar_t*)b)->ub, 1))
                    return 0;
                if (type_morespecific_(((jl_tvar_t*)b)->ub, a, b0, a0, 0, benv, aenv))
                    return 0;
                return 1;
            }
        }
        return type_morespecific_(a, ((jl_tvar_t*)b)->ub, a0, b0, 0, aenv, benv);
    }

    return 0;
}

static int type_morespecific_(jl_value_t *a, jl_value_t *b, jl_value_t *a0, jl_value_t *b0, int invariant, jl_specenv_t *aenv, jl_specenv_t *benv) JL_CANSAFEPOINT
{
    // (identical raw spellings under two different chains can denote
    // different binders, so identity requires closed operands)
    if (a == b && !jl_has_dangling_tvarrefs(a))
        return 0;

    if (jl_is_tuple_type(a) && jl_is_tuple_type(b)) {
        // compare whether a and b have Type{Union{}} included,
        // which makes them instantly the most specific, regardless of all else,
        // for whichever is left most (the left-to-right behavior here ensures
        // we do not need to keep track of conflicts with multiple methods).
        int msp = tuple_cmp_typeofbottom((jl_datatype_t*)a, (jl_datatype_t*)b, aenv, benv);
        if (msp)
            return msp > 0;
        // When one is JL_VARARG_BOUND and the other has fixed length,
        // allow the argument length to fix the tvar
        jl_vararg_kind_t akind = jl_va_tuple_kind((jl_datatype_t*)a);
        jl_vararg_kind_t bkind = jl_va_tuple_kind((jl_datatype_t*)b);
        int ans = -1;
        if (akind == JL_VARARG_BOUND && bkind < JL_VARARG_BOUND) {
            ans = args_morespecific_fix1(a, b, a0, b0, 0, aenv, benv);
            if (ans == 1) return 1;
        }
        if (bkind == JL_VARARG_BOUND && akind < JL_VARARG_BOUND) {
            ans = args_morespecific_fix1(b, a, b0, a0, 1, benv, aenv);
            if (ans == 0) return 0;
        }
        return tuple_morespecific((jl_datatype_t*)a, (jl_datatype_t*)b, a0, b0, invariant, aenv, benv);
    }

    if (!invariant) {
        if ((jl_datatype_t*)a == jl_any_type) return 0;
        if ((jl_datatype_t*)b == jl_any_type && !jl_is_typevar(a) && !jl_is_tvarref(a)) return 1;
    }

    if (jl_is_uniontype(a)) {
        if (jl_is_unionall(b)) {
            HANDLE_UNIONALL_B;
        }
        // Union a is more specific than b if some element of a is more specific than b, but
        // not vice-versa.
        if (sub_msp(b, a, a0, benv, aenv))
            return 0;
        jl_uniontype_t *u = (jl_uniontype_t*)a;
        if (type_morespecific_(u->a, b, a0, b0, invariant, aenv, benv) || type_morespecific_(u->b, b, a0, b0, invariant, aenv, benv)) {
            if (jl_is_uniontype(b)) {
                jl_uniontype_t *v = (jl_uniontype_t*)b;
                if (type_morespecific_(v->a, a, b0, a0, invariant, benv, aenv) || type_morespecific_(v->b, a, b0, a0, invariant, benv, aenv))
                    return 0;
            }
            return 1;
        }
        return 0;
    }

    if (jl_is_some_Type(a) && !invariant) {
        if (b == (jl_value_t*)jl_typeofbottom_type)
            return 0;
        jl_value_t *tp0a = jl_some_Type_T(a);
        if (jl_is_tvarref(tp0a))
            // resolved to an equivalent free variable (NULL when detached)
            tp0a = spec_resolve_ref(tp0a, aenv);
        if (tp0a != NULL && jl_is_typevar(tp0a)) {
            JL_GC_PUSH1(&tp0a);
            int msp = jl_is_kind(b) && !sub_msp((jl_value_t*)jl_any_type, ((jl_tvar_t*)tp0a)->ub, b0, NULL, aenv);
            JL_GC_POP();
            if (msp)
                return 1;
        }
        else if (tp0a == NULL) {
            // a detached reference supports no bound reasoning
        }
        else if (tp0a == jl_bottom_type) {
            if (sub_msp(b, (jl_value_t*)jl_type_type, (jl_value_t*)jl_type_type, benv, NULL))
                return 1;
        }
        else if (jl_is_kind(b)) {
            return 1;
        }
    }

    if (jl_is_uniontype(b)) {
        if (jl_is_unionall(a)) {
            HANDLE_UNIONALL_A;
        }
        jl_uniontype_t *u = (jl_uniontype_t*)b;
        if (type_morespecific_(a, u->a, a0, b0, invariant, aenv, benv) || type_morespecific_(a, u->b, a0, b0, invariant, aenv, benv))
            return !type_morespecific_(b, a, b0, a0, invariant, benv, aenv);
        return 0;
    }

    if (jl_is_some_Type(a) && jl_is_some_Type(b)) {
        jl_value_t *apara = jl_some_Type_T(a);
        jl_value_t *bpara = jl_some_Type_T(b);
        int afree = has_free_or_dangling_typevars(apara);
        int bfree = has_free_or_dangling_typevars(bpara);
        if (!afree && !bfree && !jl_types_equal(apara, bpara))
            return 0;
        if (type_morespecific_(apara, bpara, a0, b0, 1, aenv, benv) && (jl_is_typevar(apara) || jl_is_tvarref(apara) || !afree || bfree))
            return 1;
        if (type_morespecific_(bpara, apara, b0, a0, 1, benv, aenv) && (jl_is_typevar(bpara) || jl_is_tvarref(bpara) || !bfree || afree))
            return 0;
        if (eq_msp(apara, bpara, a0, b0, aenv, benv))
            return !afree && bfree;
        return 0;
    }

    if (jl_is_kind(a) && jl_is_typeeq(b) && !invariant) {
        // a kind (e.g. `DataType`) is more specific than an unbounded `Type{T}`
        jl_value_t *tp0b = jl_typeeq_T(b);
        if (jl_is_tvarref(tp0b))
            tp0b = spec_resolve_ref(tp0b, benv);
        if (tp0b != NULL && jl_is_typevar(tp0b)) {
            JL_GC_PUSH1(&tp0b);
            int msp = sub_msp((jl_value_t*)jl_any_type, ((jl_tvar_t*)tp0b)->ub, b0, NULL, benv);
            JL_GC_POP();
            if (msp)
                return 1;
        }
    }

    if (jl_is_datatype(a) && jl_is_datatype(b)) {
        jl_datatype_t *tta = (jl_datatype_t*)a, *ttb = (jl_datatype_t*)b;
        // Type{Union{}} is more specific than other types, so TypeofBottom must be too
        if (tta == jl_typeofbottom_type && (is_kind_or_anytype(b) || jl_is_typeeq(b)))
            return 1;
        int super = 0;
        while (tta != jl_any_type) {
            if (tta->name == ttb->name) {
                if (super) {
                    if (!jl_is_typeeq(b)) return 1;
                    jl_value_t *tp0 = jl_typeeq_T(b);
                    if (jl_is_tvarref(tp0))
                        tp0 = spec_resolve_ref(tp0, benv);
                    if (tp0 != NULL && jl_is_typevar(tp0)) {
                        JL_GC_PUSH1(&tp0);
                        int msp = sub_msp((jl_value_t*)jl_any_type, ((jl_tvar_t*)tp0)->ub, b0, NULL, benv);
                        JL_GC_POP();
                        if (msp)
                            return 1;
                    }
                }
                assert(jl_nparams(tta) == jl_nparams(ttb));
                int ascore=0, bscore=0, ascore1=0, bscore1=0, adiag=0, bdiag=0;
                for(size_t i=0; i < jl_nparams(tta); i++) {
                    jl_value_t *apara = jl_tparam(tta,i);
                    jl_value_t *bpara = jl_tparam(ttb,i);
                    int afree = has_free_or_dangling_typevars(apara);
                    int bfree = has_free_or_dangling_typevars(bpara);
                    if (!afree && !bfree && !jl_types_equal(apara, bpara))
                        return 0;
                    if (type_morespecific_(apara, bpara, a0, b0, 1, aenv, benv) && (jl_is_typevar(apara) || jl_is_tvarref(apara) || !afree || bfree))
                        ascore += 1;
                    else if (type_morespecific_(bpara, apara, b0, a0, 1, benv, aenv) && (jl_is_typevar(bpara) || jl_is_tvarref(bpara) || !bfree || afree))
                        bscore += 1;
                    else if (eq_msp(apara, bpara, a0, b0, aenv, benv)) {
                        if (!afree && bfree)
                            ascore += 1;
                        else if (afree && !bfree)
                            bscore += 1;
                    }
                    int avar = jl_is_typevar(apara) || jl_is_tvarref(apara);
                    int bvar = jl_is_typevar(bpara) || jl_is_tvarref(bpara);
                    if (bvar && !avar && !jl_is_type(apara))
                        ascore1 = 1;
                    else if (avar && !bvar && !jl_is_type(bpara))
                        bscore1 = 1;
                    if (!adiag && avar) {
                        for(int j=i+1; j < jl_nparams(tta); j++) {
                            jl_value_t *sib = jl_tparam(tta,j);
                            if (jl_is_typevar(apara) ? jl_has_typevar(sib, (jl_tvar_t*)apara)
                                                     : jl_tvarref_occurs(sib, jl_tvarref_depth(apara))) {
                                adiag = 1; break;
                            }
                        }
                    }
                    if (!bdiag && bvar) {
                        for(int j=i+1; j < jl_nparams(ttb); j++) {
                            jl_value_t *sib = jl_tparam(ttb,j);
                            if (jl_is_typevar(bpara) ? jl_has_typevar(sib, (jl_tvar_t*)bpara)
                                                     : jl_tvarref_occurs(sib, jl_tvarref_depth(bpara))) {
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
            // deferred supertypes (self-referential definitions) materialize on demand
            tta = jl_datatype_compute_super(tta);
            if (tta == NULL)
                return 0; // definition still in progress
            super = 1;
        }
        return 0;
    }

    if (jl_is_typevar(a) || jl_is_typevar(b) || jl_is_tvarref(a) || jl_is_tvarref(b)) {
        int acount = spec_num_occurs(a, aenv);
        int bcount = spec_num_occurs(b, benv);
        JL_GC_PUSH2(&a, &b);
        a = spec_resolve_ref(a, aenv);
        b = b == NULL ? NULL : spec_resolve_ref(b, benv);
        int ret = 0; // a detached reference supports no bound reasoning
        if (a != NULL && b != NULL)
            ret = type_morespecific_var_(a, b, a0, b0, invariant, aenv, benv, acount, bcount);
        JL_GC_POP();
        return ret;
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
    if (has_free_or_dangling_typevars(a) || has_free_or_dangling_typevars(b))
        return 0;
    if (jl_subtype(b, a))
        return 0;
    if (jl_subtype(a, b))
        return 1;
    return type_morespecific_(a, b, a, b, 0, NULL, NULL);
}

JL_DLLEXPORT int jl_type_morespecific_no_subtype(jl_value_t *a, jl_value_t *b)
{
    return type_morespecific_(a, b, a, b, 0, NULL, NULL);
}

// Equivalent to `jl_type_morespecific` of the signatures, except that more recent
// methods are more specific, iff the methods signatures are type-equal
JL_DLLEXPORT int jl_method_morespecific(jl_method_t *ma, jl_method_t *mb)
{
    jl_value_t *a = (jl_value_t*)ma->sig;
    jl_value_t *b = (jl_value_t*)mb->sig;
    if (obviously_disjoint(a, b, 1))
        return 0;
    if (has_free_or_dangling_typevars(a) || has_free_or_dangling_typevars(b))
        return 0;
    if (jl_subtype(b, a)) {
        if (jl_types_equal(a, b))
            return jl_atomic_load_relaxed(&ma->primary_world) > jl_atomic_load_relaxed(&mb->primary_world);
        return 0;
    }
    if (jl_subtype(a, b))
        return 1;
    return type_morespecific_(a, b, a, b, 0, NULL, NULL);
}

#ifdef __cplusplus
}
#endif

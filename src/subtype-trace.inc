// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  Execution transcripts for subtyping and type intersection queries.

  This file is included by subtype.c once its state types are defined; the
  wrappers that instrument the algorithm live in subtype-trace-wrappers.inc,
  included at the end of subtype.c. Everything is compiled only when Julia is
  built with `-DJL_SUBTYPE_TRACE` (e.g. `JCFLAGS += -DJL_SUBTYPE_TRACE` in
  Make.user); otherwise every hook below expands to nothing.

  Tracing is opted into for one explicit query at a time, on the calling thread:

      ccall(:jl_subtype_trace, Any, (Cstring, Cstring, Any, Any), path, kind, a, b)

  where `kind` is one of
      "subtype"                -> jl_subtype(a, b)                    :: Bool
      "subtype_env"            -> jl_subtype_env(a, b, env)           :: svec(Bool, svec(env...))
      "types_equal"            -> jl_types_equal(a, b)                :: Bool
      "isa"                    -> jl_isa(a, b)                        :: Bool
      "intersect"              -> jl_type_intersection(a, b)          :: Type
      "intersect_env"          -> jl_type_intersection_with_env(a, b) :: svec(Type, svec(env...))
      "has_empty_intersection" -> jl_has_empty_intersection(a, b)     :: Bool
  The query's result is returned, and one JSON object is appended to `path` as
  a single line:

      {"version":1, "kind":"subtype", "args":[A,B], "result":R, "atoms":[...], "log":[...]}

  Identity
  --------
  All identities are dense integers that are only meaningful within one
  transcript. None of them is a raw address of stack-allocated state, since
  those are reused:
    - atom ids: heap objects (types, values). Every object that is given an id
      is kept alive until the transcript is written, so an address can never be
      reused for a different object within a transcript.
    - `e`: one per `jl_stenv_t` initialization (each top-level or nested
      subtype/intersection environment).
    - `vb`: one per push of a variable binding onto an environment's `vars`
      stack. A binding re-pushed for the same UnionAll (e.g. when intersection
      retries a variable) gets a new id; its `var` atom is the stable TypeVar.
    - `se`: one per saved environment (`jl_savedenv_t`); kept across re-saves.
    - `c`: one per call of an instrumented function.

  Atoms
  -----
  `atoms[i].id == i`. Every atom has "id", "kind", "addr" (hex address),
  "oid" (hex `objectid`) and "t" (index into `log` of the event that first
  referenced it, or -1 for the query arguments and their components). Atoms
  created during the query (by substitution, meets, intersection results...)
  therefore have `t >= 0`. Child references are atom ids (or null).
    {"kind":"Bottom"}                                   Union{}
    {"kind":"TypeVar", "name":str, "lb":id, "ub":id}
    {"kind":"Union", "a":id, "b":id}
    {"kind":"UnionAll", "var":id, "body":id}
    {"kind":"Vararg", "T":id|null, "N":id|null}
    {"kind":"TypeEq", "T":id}                           Type{T}
    {"kind":"TypeEgal", "T":id}                         Core.TypeEgal{T}
    {"kind":"Intersect", "a":id, "b":id}                internal meet node
    {"kind":"TypeApp", "head":id, "param":id}           internal lazy application
    {"kind":"DataType", "name":id, "params":[id...]}
    {"kind":"TypeName", "name":str, "module":[str...], "wrapper":id, "super":id,
     "abstract":bool, "mutable":bool}                   `super` is the supertype of the
                                                        wrapper's body, in terms of its vars
    {"kind":"Symbol", "name":str}
    {"kind":"Module", "path":[str...]}
    {"kind":"Value", "type":id, "repr":str, "bits":hex} `bits` only for isbits values

  Log
  ---
  Every event has "ev" and "in" (the `c` of the innermost active call, or -1).
    call     {"c", "fn", "d" (call depth), "e"?, "args":[id...], "argvb":[vb|null...]?, ...}
             `argvb` resolves TypeVar arguments to their live binding. Extra keys
             are function specific (R, param, depth, offset, se, vb, ...).
    ret      {"c", "fn", "res"}       res: 0/1 or an atom id (or null)
    catch    {"c":[...]}             an exception was caught inside the algorithm
                                      (the result there becomes Union{}); `c` lists
                                      the calls it abandoned, whose bindings are
                                      unbound first with "unwound":1
    stenv    {"e", "envsz"}
    bind     {"vb", "e", "var", "lb", "ub", "pos" (stack depth, 0 = outermost),
              "existential", "depth0"}
    unbind   {"vb"}
    bound    {"vb", "field":"lb"|"ub"|"var", "old", "new"}
    flags    {"vb", "<field>":[old,new]...}   varbinding counters/flags
    innervars {"vb", "set":[id...]}           the binding's innervars changed
    upick    {"e", "R", "site", "slot", "bit", "fresh", "more", "u", "el"?}
             a union decision: `slot` is the position in the R/L decision stack
             of environment `e`; `fresh` = the slot was newly pushed (so `bit` is
             0); `u` is the Union being decided on and `el` the component chosen
             (site "elem"). Slots are positional: the same slot may refer to a
             different Union in a different iteration, so always read `u`.
    unext    {"e", "R", "ok", "slot", "used", "bits"}   advance to the next
             combination of decisions (`slot` is the decision flipped to 1)
    usave / urestore {"e", "R", "depth", "more", "used", "bits"}
    iter     {"e", "R", "loop"}      start of one iteration of a forall/exists loop
    env_save / env_resave {"se", "e", "vbs":[vb...]}  (innermost first)
    env_restore {"se", "e", "root"}  followed by the bound/flags events it caused
  Binding changes are found by comparing every live binding against its last
  logged state before each event, so they are attributed to the innermost
  active call at the time they are observed.
*/

#ifdef JL_SUBTYPE_TRACE

#include "julia_gcext.h"

#define STRACE_VB_FLAGS(X) \
    X(existential) X(occurs) X(occurs_inv) X(pinned) X(occurs_cov) X(cov_diag) \
    X(concrete) X(max_offset) X(constraintkind) X(intvalued) X(limited) \
    X(intersected) X(widened_to_kind) X(lb_certainty) X(lb_required) X(lb_spell) \
    X(tainted_inner)
#define STRACE_COUNT_FLAG(f) + 1
enum { STRACE_NFLAGS = 0 STRACE_VB_FLAGS(STRACE_COUNT_FLAG) };

typedef struct {
    jl_varbinding_t *vb;
    int64_t id;
    jl_tvar_t *var;
    jl_value_t *lb;
    jl_value_t *ub;
    jl_array_t *innervars;
    size_t ninner;
    int8_t flags[STRACE_NFLAGS];
} strace_vb_t;

typedef struct {
    int64_t cid;
    const char *fn;
} strace_frame_t;

typedef struct {
    size_t nframes;
    size_t nvbs;
} strace_savepoint_t;

typedef struct {
    jl_ptls_t ptls;
    htable_t ids;        // object -> atom id + 2 (1 is HT_NOTFOUND)
    arraylist_t roots;   // atom id -> object; kept alive by `strace_root_scanner`
    arraylist_t spans;   // atom id -> [start, end) offsets into `atoms`
    ios_t atoms;
    ios_t log;
    int64_t nevents;
    int64_t cur_event;   // index of the event being written, -1 before the first
    int64_t ncalls;
    int64_t nvbids;
    int64_t nstenvs;
    int64_t nsavedenvs;
    strace_frame_t *frames;
    size_t nframes, maxframes;
    strace_vb_t *vbs;    // live bindings, in push order
    size_t nvbs, maxvbs;
} strace_t;

static _Atomic(strace_t*) strace_ctx = NULL;

static int statestack_get(jl_unionstate_t *st, int i) JL_NOTSAFEPOINT;

static inline strace_t *strace_cur(void) JL_NOTSAFEPOINT
{
    strace_t *t = jl_atomic_load_relaxed(&strace_ctx);
    if (__likely(t == NULL) || t->ptls != jl_current_task->ptls)
        return NULL;
    return t;
}

static void strace_root_scanner(int full) JL_NOTSAFEPOINT
{
    strace_t *t = jl_atomic_load_relaxed(&strace_ctx);
    if (t == NULL)
        return;
    jl_ptls_t ptls = jl_current_task->ptls;
    for (size_t i = 0; i < t->roots.len; i++)
        jl_gc_mark_queue_obj(ptls, (jl_value_t*)t->roots.items[i]);
}

static void *strace_grow(void *p, size_t *max, size_t n, size_t elsz) JL_NOTSAFEPOINT
{
    if (n < *max)
        return p;
    *max = *max ? *max * 2 : 16;
    return realloc_s(p, *max * elsz);
}

// JSON output

static void strace_str(ios_t *o, const char *s) JL_NOTSAFEPOINT
{
    ios_putc('"', o);
    for (; *s; s++) {
        unsigned char c = *s;
        if (c == '"' || c == '\\') {
            ios_putc('\\', o);
            ios_putc(c, o);
        }
        else if (c < 0x20) {
            ios_printf(o, "\\u%04x", c);
        }
        else {
            ios_putc(c, o);
        }
    }
    ios_putc('"', o);
}

static void strace_id(ios_t *o, int64_t id) JL_NOTSAFEPOINT
{
    if (id < 0)
        ios_puts("null", o);
    else
        ios_printf(o, "%lld", (long long)id);
}

static void strace_modpath(ios_t *o, jl_module_t *m) JL_NOTSAFEPOINT
{
    if (m->parent != NULL && m->parent != m) {
        strace_modpath(o, m->parent);
        ios_putc(',', o);
    }
    strace_str(o, jl_symbol_name(m->name));
}

// Atoms

static int64_t strace_atom(strace_t *t, jl_value_t *v) JL_NOTSAFEPOINT;

static void strace_atom_head(strace_t *t, jl_value_t *v, int64_t id, const char *kind) JL_NOTSAFEPOINT
{
    ios_printf(&t->atoms, "{\"id\":%lld,\"kind\":\"%s\",\"addr\":\"%p\",\"oid\":\"0x%" PRIxPTR "\",\"t\":%lld",
               (long long)id, kind, (void*)v, (uintptr_t)jl_object_id(v), (long long)t->cur_event);
}

// With `emit == 0`, make sure every child of `v` has an atom (which may emit
// those atoms). With `emit == 1`, write the fields of `v`'s atom; its children
// then all have ids already, so nothing else is written to `atoms` meanwhile.
static void strace_atom_fields(strace_t *t, jl_value_t *v, int64_t id, int emit) JL_NOTSAFEPOINT
{
    ios_t *o = &t->atoms;
#define HEAD(kind) do { if (emit) strace_atom_head(t, v, id, kind); } while (0)
#define CHILD(key, c) do {                                              \
        int64_t _cid = strace_atom(t, (jl_value_t*)(c));                \
        if (emit) { ios_printf(o, ",\"%s\":", key); strace_id(o, _cid); } \
    } while (0)
    if (v == jl_bottom_type) {
        HEAD("Bottom");
    }
    else if (jl_is_typevar(v)) {
        jl_tvar_t *tv = (jl_tvar_t*)v;
        HEAD("TypeVar");
        if (emit) {
            ios_puts(",\"name\":", o);
            strace_str(o, jl_symbol_name(tv->name));
        }
        CHILD("lb", tv->lb);
        CHILD("ub", tv->ub);
    }
    else if (jl_is_uniontype(v)) {
        HEAD("Union");
        CHILD("a", ((jl_uniontype_t*)v)->a);
        CHILD("b", ((jl_uniontype_t*)v)->b);
    }
    else if (jl_is_unionall(v)) {
        HEAD("UnionAll");
        CHILD("var", ((jl_unionall_t*)v)->var);
        CHILD("body", ((jl_unionall_t*)v)->body);
    }
    else if (jl_is_vararg(v)) {
        HEAD("Vararg");
        CHILD("T", ((jl_vararg_t*)v)->T);
        CHILD("N", ((jl_vararg_t*)v)->N);
    }
    else if (jl_is_typeeq(v)) {
        HEAD("TypeEq");
        CHILD("T", ((jl_typeeq_t*)v)->T);
    }
    else if (jl_is_typeegal(v)) {
        HEAD("TypeEgal");
        CHILD("T", ((jl_typeeq_t*)v)->T);
    }
    else if (jl_is_intersecttype(v)) {
        HEAD("Intersect");
        CHILD("a", ((jl_intersecttype_t*)v)->a);
        CHILD("b", ((jl_intersecttype_t*)v)->b);
    }
    else if (jl_is_typeapp(v)) {
        HEAD("TypeApp");
        CHILD("head", ((jl_typeapp_t*)v)->head);
        CHILD("param", ((jl_typeapp_t*)v)->param);
    }
    else if (jl_is_datatype(v)) {
        jl_datatype_t *dt = (jl_datatype_t*)v;
        HEAD("DataType");
        CHILD("name", dt->name);
        size_t np = dt->parameters ? jl_svec_len(dt->parameters) : 0;
        if (emit)
            ios_puts(",\"params\":[", o);
        for (size_t i = 0; i < np; i++) {
            int64_t pid = strace_atom(t, jl_svecref(dt->parameters, i));
            if (emit) {
                if (i)
                    ios_putc(',', o);
                strace_id(o, pid);
            }
        }
        if (emit)
            ios_putc(']', o);
    }
    else if (jl_is_typename(v)) {
        jl_typename_t *tn = (jl_typename_t*)v;
        jl_value_t *body = tn->wrapper ? jl_unwrap_unionall(tn->wrapper) : NULL;
        HEAD("TypeName");
        if (emit) {
            ios_puts(",\"name\":", o);
            strace_str(o, jl_symbol_name(tn->name));
            ios_puts(",\"module\":[", o);
            if (tn->module)
                strace_modpath(o, tn->module);
            ios_printf(o, "],\"abstract\":%s,\"mutable\":%s",
                       tn->abstract ? "true" : "false", tn->mutabl ? "true" : "false");
        }
        CHILD("wrapper", tn->wrapper);
        CHILD("super", body && jl_is_datatype(body) ? ((jl_datatype_t*)body)->super : NULL);
    }
    else if (jl_is_symbol(v)) {
        HEAD("Symbol");
        if (emit) {
            ios_puts(",\"name\":", o);
            strace_str(o, jl_symbol_name((jl_sym_t*)v));
        }
    }
    else if (jl_is_module(v)) {
        HEAD("Module");
        if (emit) {
            ios_puts(",\"path\":[", o);
            strace_modpath(o, (jl_module_t*)v);
            ios_putc(']', o);
        }
    }
    else {
        jl_value_t *ty = jl_typeof(v);
        HEAD("Value");
        CHILD("type", ty);
        if (emit) {
            ios_t buf;
            ios_mem(&buf, 0);
            jl_static_show((JL_STREAM*)&buf, v);
            ios_putc('\0', &buf);
            ios_puts(",\"repr\":", o);
            strace_str(o, buf.buf);
            ios_close(&buf);
            if (jl_isbits(ty)) {
                size_t sz = jl_datatype_size(ty);
                ios_puts(",\"bits\":\"", o);
                for (size_t i = 0; i < sz; i++)
                    ios_printf(o, "%02x", ((uint8_t*)v)[i]);
                ios_putc('"', o);
            }
        }
    }
#undef HEAD
#undef CHILD
}

static int64_t strace_atom(strace_t *t, jl_value_t *v) JL_NOTSAFEPOINT
{
    if (v == NULL)
        return -1;
    void **bp = ptrhash_bp(&t->ids, v);
    if (*bp != HT_NOTFOUND)
        return (int64_t)(intptr_t)*bp - 2;
    int64_t id = t->roots.len;
    *bp = (void*)(intptr_t)(id + 2);
    arraylist_push(&t->roots, v);
    arraylist_push(&t->spans, NULL);
    arraylist_push(&t->spans, NULL);
    strace_atom_fields(t, v, id, 0);
    t->spans.items[2 * id] = (void*)(intptr_t)ios_pos(&t->atoms);
    strace_atom_fields(t, v, id, 1);
    ios_putc('}', &t->atoms);
    t->spans.items[2 * id + 1] = (void*)(intptr_t)ios_pos(&t->atoms);
    return id;
}

// Log events

static void strace_ev(strace_t *t, const char *ev) JL_NOTSAFEPOINT
{
    if (t->nevents)
        ios_putc(',', &t->log);
    t->cur_event = t->nevents++;
    int64_t in = t->nframes ? t->frames[t->nframes - 1].cid : -1;
    ios_printf(&t->log, "{\"ev\":\"%s\",\"in\":%lld", ev, (long long)in);
}

static void strace_kint(strace_t *t, const char *key, int64_t v) JL_NOTSAFEPOINT
{
    ios_printf(&t->log, ",\"%s\":%lld", key, (long long)v);
}

static void strace_katom(strace_t *t, const char *key, jl_value_t *v) JL_NOTSAFEPOINT
{
    int64_t id = strace_atom(t, v);
    ios_printf(&t->log, ",\"%s\":", key);
    strace_id(&t->log, id);
}

static void strace_kstr(strace_t *t, const char *key, const char *s) JL_NOTSAFEPOINT
{
    ios_printf(&t->log, ",\"%s\":", key);
    strace_str(&t->log, s);
}

static void strace_end(strace_t *t) JL_NOTSAFEPOINT
{
    ios_putc('}', &t->log);
}

static void strace_kbits(strace_t *t, const char *key, jl_unionstate_t *st) JL_NOTSAFEPOINT
{
    ios_printf(&t->log, ",\"%s\":\"", key);
    for (int i = 0; i < st->used; i++)
        ios_putc(statestack_get(st, i) ? '1' : '0', &t->log);
    ios_putc('"', &t->log);
}

// Variable bindings

static strace_vb_t *strace_find_vb(strace_t *t, jl_varbinding_t *vb) JL_NOTSAFEPOINT
{
    for (size_t i = t->nvbs; i > 0; i--) {
        if (t->vbs[i - 1].vb == vb)
            return &t->vbs[i - 1];
    }
    return NULL;
}

static int64_t strace_vb_id(strace_t *t, jl_varbinding_t *vb) JL_NOTSAFEPOINT
{
    strace_vb_t *r = vb ? strace_find_vb(t, vb) : NULL;
    return r ? r->id : -1;
}

static void strace_snapshot_vb(strace_vb_t *r, jl_varbinding_t *vb) JL_NOTSAFEPOINT
{
    r->var = vb->var;
    r->lb = vb->lb;
    r->ub = vb->ub;
    r->innervars = vb->innervars;
    r->ninner = vb->innervars ? jl_array_nrows(vb->innervars) : 0;
    int i = 0;
#define SNAP_FLAG(f) r->flags[i++] = vb->f;
    STRACE_VB_FLAGS(SNAP_FLAG)
#undef SNAP_FLAG
}

static void strace_bind_vb(strace_t *t, jl_stenv_t *e, jl_varbinding_t *vb) JL_NOTSAFEPOINT
{
    t->vbs = (strace_vb_t*)strace_grow(t->vbs, &t->maxvbs, t->nvbs, sizeof(strace_vb_t));
    strace_vb_t *r = &t->vbs[t->nvbs++];
    r->vb = vb;
    r->id = t->nvbids++;
    strace_snapshot_vb(r, vb);
    int pos = -1;
    for (jl_varbinding_t *b = vb; b != NULL; b = b->prev)
        pos++;
    strace_ev(t, "bind");
    strace_kint(t, "vb", r->id);
    strace_kint(t, "e", e->trace_id);
    strace_katom(t, "var", (jl_value_t*)vb->var);
    strace_katom(t, "lb", vb->lb);
    strace_katom(t, "ub", vb->ub);
    strace_kint(t, "pos", pos);
    strace_kint(t, "existential", vb->existential);
    strace_kint(t, "depth0", vb->depth0);
    strace_end(t);
}

static void strace_sync_vb(strace_t *t, strace_vb_t *r) JL_NOTSAFEPOINT
{
    jl_varbinding_t *vb = r->vb;
#define BOUND_FIELD(f)                                          \
    if ((jl_value_t*)vb->f != (jl_value_t*)r->f) {              \
        strace_ev(t, "bound");                                  \
        strace_kint(t, "vb", r->id);                            \
        strace_kstr(t, "field", #f);                            \
        strace_katom(t, "old", (jl_value_t*)r->f);              \
        strace_katom(t, "new", (jl_value_t*)vb->f);             \
        strace_end(t);                                          \
    }
    BOUND_FIELD(var)
    BOUND_FIELD(lb)
    BOUND_FIELD(ub)
#undef BOUND_FIELD
    size_t ninner = vb->innervars ? jl_array_nrows(vb->innervars) : 0;
    if (vb->innervars != r->innervars || ninner != r->ninner) {
        strace_ev(t, "innervars");
        strace_kint(t, "vb", r->id);
        ios_puts(",\"set\":[", &t->log);
        for (size_t i = 0; i < ninner; i++) {
            int64_t id = strace_atom(t, jl_array_ptr_ref(vb->innervars, i));
            if (i)
                ios_putc(',', &t->log);
            strace_id(&t->log, id);
        }
        ios_putc(']', &t->log);
        strace_end(t);
    }
    int i = 0, changed = 0;
#define CHANGED_FLAG(f) changed |= (r->flags[i++] != vb->f);
    STRACE_VB_FLAGS(CHANGED_FLAG)
#undef CHANGED_FLAG
    if (changed) {
        strace_ev(t, "flags");
        strace_kint(t, "vb", r->id);
        i = 0;
#define LOG_FLAG(f)                                                         \
        if (r->flags[i] != vb->f)                                           \
            ios_printf(&t->log, ",\"" #f "\":[%d,%d]", r->flags[i], vb->f); \
        i++;
        STRACE_VB_FLAGS(LOG_FLAG)
#undef LOG_FLAG
        strace_end(t);
    }
    strace_snapshot_vb(r, vb);
}

// Report every change to a live binding since it was last logged. Bindings in
// `e->vars` that were not pushed through an instrumented push site are bound
// lazily.
static void strace_sync(strace_t *t, jl_stenv_t *e) JL_NOTSAFEPOINT
{
    if (e != NULL) {
        for (jl_varbinding_t *vb = e->vars; vb != NULL; vb = vb->prev) {
            if (strace_find_vb(t, vb) == NULL)
                strace_bind_vb(t, e, vb);
        }
    }
    for (size_t i = 0; i < t->nvbs; i++)
        strace_sync_vb(t, &t->vbs[i]);
}

// Hooks called from subtype.c

static void strace_bind(jl_stenv_t *e, jl_varbinding_t *vb) JL_NOTSAFEPOINT
{
    strace_t *t = strace_cur();
    if (t == NULL)
        return;
    strace_sync(t, NULL);
    strace_bind_vb(t, e, vb);
}

static void strace_unbind(jl_stenv_t *e, jl_varbinding_t *vb) JL_NOTSAFEPOINT
{
    strace_t *t = strace_cur();
    if (t == NULL)
        return;
    strace_sync(t, NULL);
    strace_vb_t *r = strace_find_vb(t, vb);
    if (r == NULL)
        return;
    strace_ev(t, "unbind");
    strace_kint(t, "vb", r->id);
    strace_end(t);
    size_t i = r - t->vbs;
    memmove(&t->vbs[i], &t->vbs[i + 1], (t->nvbs - i - 1) * sizeof(strace_vb_t));
    t->nvbs--;
}

static void strace_stenv(jl_stenv_t *e) JL_NOTSAFEPOINT
{
    strace_t *t = strace_cur();
    if (t == NULL) {
        e->trace_id = -1;
        return;
    }
    e->trace_id = t->nstenvs++;
    e->Lunions.trace_e = e->Runions.trace_e = e->trace_id;
    e->Lunions.trace_R = 0;
    e->Runions.trace_R = 1;
    strace_ev(t, "stenv");
    strace_kint(t, "e", e->trace_id);
    strace_kint(t, "envsz", e->envsz);
    strace_end(t);
}

static void strace_upick(jl_stenv_t *e, int8_t R, jl_value_t *u, const char *site,
                         int slot, int bit, int fresh) JL_NOTSAFEPOINT
{
    strace_t *t = strace_cur();
    if (t == NULL)
        return;
    strace_sync(t, e);
    jl_unionstate_t *state = R ? &e->Runions : &e->Lunions;
    strace_ev(t, "upick");
    strace_kint(t, "e", e->trace_id);
    strace_kint(t, "R", R);
    strace_kstr(t, "site", site);
    strace_kint(t, "slot", slot);
    strace_kint(t, "bit", bit);
    strace_kint(t, "fresh", fresh);
    strace_kint(t, "more", state->more);
    strace_katom(t, "u", u);
    if (u != NULL && jl_is_uniontype(u) && strcmp(site, "elem") == 0)
        strace_katom(t, "el", bit ? ((jl_uniontype_t*)u)->b : ((jl_uniontype_t*)u)->a);
    strace_end(t);
}

static void strace_unext(jl_stenv_t *e, int8_t R, int ok) JL_NOTSAFEPOINT
{
    strace_t *t = strace_cur();
    if (t == NULL)
        return;
    strace_sync(t, e);
    jl_unionstate_t *state = R ? &e->Runions : &e->Lunions;
    strace_ev(t, "unext");
    strace_kint(t, "e", e->trace_id);
    strace_kint(t, "R", R);
    strace_kint(t, "ok", ok);
    strace_kint(t, "slot", ok ? state->used - 1 : -1);
    strace_kint(t, "used", state->used);
    strace_kbits(t, "bits", state);
    strace_end(t);
}

static void strace_ustate(const char *ev, jl_unionstate_t *st) JL_NOTSAFEPOINT
{
    strace_t *t = strace_cur();
    if (t == NULL)
        return;
    strace_sync(t, NULL);
    strace_ev(t, ev);
    strace_kint(t, "e", st->trace_e);
    strace_kint(t, "R", st->trace_R);
    strace_kint(t, "depth", st->depth);
    strace_kint(t, "more", st->more);
    strace_kint(t, "used", st->used);
    strace_kbits(t, "bits", st);
    strace_end(t);
}

static void strace_iter(jl_stenv_t *e, int8_t R, const char *loop) JL_NOTSAFEPOINT
{
    strace_t *t = strace_cur();
    if (t == NULL)
        return;
    strace_sync(t, e);
    strace_ev(t, "iter");
    strace_kint(t, "e", e->trace_id);
    strace_kint(t, "R", R);
    strace_kstr(t, "loop", loop);
    strace_end(t);
}

static void strace_env_save(jl_stenv_t *e, jl_savedenv_t *se) JL_NOTSAFEPOINT
{
    strace_t *t = strace_cur();
    if (t == NULL)
        return;
    strace_sync(t, e);
    int resave = se->trace_id >= 0;
    if (!resave)
        se->trace_id = t->nsavedenvs++;
    strace_ev(t, resave ? "env_resave" : "env_save");
    strace_kint(t, "se", se->trace_id);
    strace_kint(t, "e", e->trace_id);
    ios_puts(",\"vbs\":[", &t->log);
    for (jl_varbinding_t *vb = e->vars; vb != NULL; vb = vb->prev) {
        if (vb != e->vars)
            ios_putc(',', &t->log);
        strace_id(&t->log, strace_vb_id(t, vb));
    }
    ios_putc(']', &t->log);
    strace_end(t);
}

static void strace_env_restore(jl_stenv_t *e, jl_savedenv_t *se, int root) JL_NOTSAFEPOINT
{
    strace_t *t = strace_cur();
    if (t == NULL)
        return;
    strace_ev(t, "env_restore");
    strace_kint(t, "se", se->trace_id);
    strace_kint(t, "e", e->trace_id);
    strace_kint(t, "root", root);
    strace_end(t);
    strace_sync(t, e);
}

static strace_savepoint_t strace_savepoint(void) JL_NOTSAFEPOINT
{
    strace_savepoint_t sp = {0, 0};
    strace_t *t = strace_cur();
    if (t != NULL) {
        sp.nframes = t->nframes;
        sp.nvbs = t->nvbs;
    }
    return sp;
}

// After an exception was caught inside the algorithm: drop the calls and
// bindings whose stack frames are gone, without reading those bindings.
static void strace_unwind(strace_savepoint_t *sp) JL_NOTSAFEPOINT
{
    strace_t *t = strace_cur();
    if (t == NULL)
        return;
    for (size_t i = sp->nvbs; i < t->nvbs; i++) {
        strace_ev(t, "unbind");
        strace_kint(t, "vb", t->vbs[i].id);
        strace_kint(t, "unwound", 1);
        strace_end(t);
    }
    if (t->nvbs > sp->nvbs)
        t->nvbs = sp->nvbs;
    size_t nframes = t->nframes;
    if (t->nframes > sp->nframes)
        t->nframes = sp->nframes;
    strace_ev(t, "catch");
    ios_puts(",\"c\":[", &t->log);
    for (size_t i = sp->nframes; i < nframes; i++)
        ios_printf(&t->log, "%s%lld", i == sp->nframes ? "" : ",", (long long)t->frames[i].cid);
    ios_puts("]}", &t->log);
    strace_sync(t, NULL);
}

// Calls of instrumented functions (see subtype-trace-wrappers.inc)

static strace_t *strace_call(const char *fn, jl_stenv_t *e, int nargs, jl_value_t *a, jl_value_t *b, jl_value_t *c) JL_NOTSAFEPOINT
{
    strace_t *t = strace_cur();
    if (t == NULL)
        return NULL;
    strace_sync(t, e);
    int64_t cid = t->ncalls++;
    strace_ev(t, "call");
    t->frames = (strace_frame_t*)strace_grow(t->frames, &t->maxframes, t->nframes, sizeof(strace_frame_t));
    t->frames[t->nframes].cid = cid;
    t->frames[t->nframes].fn = fn;
    t->nframes++;
    strace_kint(t, "c", cid);
    strace_kstr(t, "fn", fn);
    strace_kint(t, "d", t->nframes - 1);
    if (e != NULL)
        strace_kint(t, "e", e->trace_id);
    jl_value_t *args[3] = {a, b, c};
    int anytv = 0;
    ios_puts(",\"args\":[", &t->log);
    for (int i = 0; i < nargs; i++) {
        int64_t id = strace_atom(t, args[i]);
        if (i)
            ios_putc(',', &t->log);
        strace_id(&t->log, id);
        anytv |= args[i] != NULL && jl_is_typevar(args[i]);
    }
    ios_putc(']', &t->log);
    if (anytv && e != NULL) {
        ios_puts(",\"argvb\":[", &t->log);
        for (int i = 0; i < nargs; i++) {
            if (i)
                ios_putc(',', &t->log);
            jl_varbinding_t *vb = args[i] != NULL && jl_is_typevar(args[i]) ? lookup(e, (jl_tvar_t*)args[i]) : NULL;
            strace_id(&t->log, strace_vb_id(t, vb));
        }
        ios_putc(']', &t->log);
    }
    return t;
}

static void strace_ret_begin(strace_t *t) JL_NOTSAFEPOINT
{
    strace_sync(t, NULL);
    assert(t->nframes > 0);
    strace_frame_t f = t->frames[--t->nframes];
    strace_ev(t, "ret");
    strace_kint(t, "c", f.cid);
    strace_kstr(t, "fn", f.fn);
}

static int strace_ret_int(strace_t *t, int r) JL_NOTSAFEPOINT
{
    if (t != NULL) {
        strace_ret_begin(t);
        strace_kint(t, "res", r);
        strace_end(t);
    }
    return r;
}

static jl_value_t *strace_ret_val(strace_t *t, jl_value_t *r) JL_NOTSAFEPOINT
{
    if (t != NULL) {
        strace_ret_begin(t);
        strace_katom(t, "res", r);
        strace_end(t);
    }
    return r;
}

#define STRACE_IMPL(f) f##__untraced
#define STRACE_STENV(e) strace_stenv(e)
#define STRACE_UPICK(e, R, u, site, slot, bit, fresh) strace_upick(e, R, u, site, slot, bit, fresh)
#define STRACE_UNEXT(e, R, ok) strace_unext(e, R, ok)
#define STRACE_USTATE(ev, st) strace_ustate(ev, st)
#define STRACE_ITER(e, R, loop) strace_iter(e, R, loop)
#define STRACE_ENV_ALLOC(se) ((se)->trace_id = -1)
#define STRACE_ENV_SAVE(e, se) strace_env_save(e, se)
#define STRACE_ENV_RESTORE(e, se, root) strace_env_restore(e, se, root)
#define STRACE_BIND(e, vb) strace_bind(e, vb)
#define STRACE_UNBIND(e, vb) strace_unbind(e, vb)
#define STRACE_SAVEPOINT(sp) strace_savepoint_t sp = strace_savepoint()
#define STRACE_UNWIND(sp) strace_unwind(&sp)

// Forward declarations of the instrumented functions: their definitions in
// subtype.c are renamed to `<name>__untraced` (declared here with the same
// annotations), and the original names refer to the wrappers in
// subtype-trace-wrappers.inc.
static int subtype(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, jl_param_pos_t param) JL_CANSAFEPOINT;
static int subtype__untraced(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, jl_param_pos_t param) JL_CANSAFEPOINT;
static int forall_exists_subtype(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, jl_param_pos_t param) JL_CANSAFEPOINT;
static int forall_exists_subtype__untraced(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, jl_param_pos_t param) JL_CANSAFEPOINT;
static int local_forall_exists_subtype(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, jl_param_pos_t param, int limit_slow) JL_CANSAFEPOINT;
static int local_forall_exists_subtype__untraced(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, jl_param_pos_t param, int limit_slow) JL_CANSAFEPOINT;
static int exists_subtype(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, jl_savedenv_t *se, jl_param_pos_t param) JL_CANSAFEPOINT;
static int exists_subtype__untraced(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, jl_savedenv_t *se, jl_param_pos_t param) JL_CANSAFEPOINT;
static int forall_exists_equal(jl_value_t *x, jl_value_t *y, jl_stenv_t *e) JL_CANSAFEPOINT;
static int forall_exists_equal__untraced(jl_value_t *x, jl_value_t *y, jl_stenv_t *e) JL_CANSAFEPOINT;
static int equal_var(jl_tvar_t *v, jl_value_t *x, jl_stenv_t *e) JL_CANSAFEPOINT;
static int equal_var__untraced(jl_tvar_t *v, jl_value_t *x, jl_stenv_t *e) JL_CANSAFEPOINT;
static int subtype_unionall(jl_value_t *t, jl_unionall_t *u, jl_stenv_t *e, int8_t R, jl_param_pos_t param) JL_CANSAFEPOINT;
static int subtype_unionall__untraced(jl_value_t *t, jl_unionall_t *u, jl_stenv_t *e, int8_t R, jl_param_pos_t param) JL_CANSAFEPOINT;
static int subtype_var(jl_tvar_t *b, jl_value_t *a, jl_stenv_t *e, int R, jl_param_pos_t param, jl_varbinding_t *bb, int innervar) JL_CANSAFEPOINT;
static int subtype_var__untraced(jl_tvar_t *b, jl_value_t *a, jl_stenv_t *e, int R, jl_param_pos_t param, jl_varbinding_t *bb, int innervar) JL_CANSAFEPOINT;
static int var_lt(jl_tvar_t *b, jl_value_t *a, jl_stenv_t *e, jl_param_pos_t param, jl_varbinding_t *bb, int innervar) JL_CANSAFEPOINT;
static int var_lt__untraced(jl_tvar_t *b, jl_value_t *a, jl_stenv_t *e, jl_param_pos_t param, jl_varbinding_t *bb, int innervar) JL_CANSAFEPOINT;
static int var_gt(jl_tvar_t *b, jl_value_t *a, jl_stenv_t *e, jl_param_pos_t param, jl_varbinding_t *bb, int innervar) JL_CANSAFEPOINT;
static int var_gt__untraced(jl_tvar_t *b, jl_value_t *a, jl_stenv_t *e, jl_param_pos_t param, jl_varbinding_t *bb, int innervar) JL_CANSAFEPOINT;
static int subtype_tuple(jl_datatype_t *xd, jl_datatype_t *yd, jl_stenv_t *e, jl_param_pos_t param) JL_CANSAFEPOINT;
static int subtype_tuple__untraced(jl_datatype_t *xd, jl_datatype_t *yd, jl_stenv_t *e, jl_param_pos_t param) JL_CANSAFEPOINT;
static int subtype_tuple_tail(jl_datatype_t *xd, jl_datatype_t *yd, int8_t R, jl_stenv_t *e, jl_param_pos_t param) JL_CANSAFEPOINT;
static int subtype_tuple_tail__untraced(jl_datatype_t *xd, jl_datatype_t *yd, int8_t R, jl_stenv_t *e, jl_param_pos_t param) JL_CANSAFEPOINT;
static int subtype_tuple_varargs(jl_vararg_t *vtx, jl_vararg_t *vty, jl_value_t *lastx, jl_value_t *lasty, size_t vx, size_t vy, size_t x_reps, jl_stenv_t *e, jl_param_pos_t param) JL_CANSAFEPOINT;
static int subtype_tuple_varargs__untraced(jl_vararg_t *vtx, jl_vararg_t *vty, jl_value_t *lastx, jl_value_t *lasty, size_t vx, size_t vy, size_t x_reps, jl_stenv_t *e, jl_param_pos_t param) JL_CANSAFEPOINT;
static int subtype_ccheck(jl_value_t *x, jl_value_t *y, jl_stenv_t *e) JL_CANSAFEPOINT;
static int subtype_ccheck__untraced(jl_value_t *x, jl_value_t *y, jl_stenv_t *e) JL_CANSAFEPOINT;
static int subtype_left_var(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, jl_param_pos_t param) JL_CANSAFEPOINT;
static int subtype_left_var__untraced(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, jl_param_pos_t param) JL_CANSAFEPOINT;
static int obvious_subtype(jl_value_t *x, jl_value_t *y, jl_value_t *y0, int *subtype);
static int obvious_subtype__untraced(jl_value_t *x, jl_value_t *y, jl_value_t *y0, int *subtype);
static int subtype_in_env(jl_value_t *x, jl_value_t *y, jl_stenv_t *e) JL_CANSAFEPOINT;
static int subtype_in_env__untraced(jl_value_t *x, jl_value_t *y, jl_stenv_t *e) JL_CANSAFEPOINT;
static int subtype_in_env_existential(jl_value_t *x, jl_value_t *y, jl_stenv_t *e) JL_CANSAFEPOINT;
static int subtype_in_env_existential__untraced(jl_value_t *x, jl_value_t *y, jl_stenv_t *e) JL_CANSAFEPOINT;
static int subtype_in_env_nonexistential(jl_value_t *x, jl_value_t *y, jl_stenv_t *e) JL_CANSAFEPOINT;
static int subtype_in_env_nonexistential__untraced(jl_value_t *x, jl_value_t *y, jl_stenv_t *e) JL_CANSAFEPOINT;
static int subtype_by_bounds(jl_value_t *x, jl_value_t *y, jl_stenv_t *e) JL_NOTSAFEPOINT;
static int subtype_by_bounds__untraced(jl_value_t *x, jl_value_t *y, jl_stenv_t *e) JL_NOTSAFEPOINT;
static jl_value_t *intersect(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, jl_param_pos_t param) JL_CANSAFEPOINT;
static jl_value_t *intersect__untraced(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, jl_param_pos_t param) JL_CANSAFEPOINT;
static jl_value_t *intersect_all(jl_value_t *x, jl_value_t *y, jl_stenv_t *e) JL_CANSAFEPOINT;
static jl_value_t *intersect_all__untraced(jl_value_t *x, jl_value_t *y, jl_stenv_t *e) JL_CANSAFEPOINT;
static jl_value_t *intersect_aside(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int depth) JL_CANSAFEPOINT;
static jl_value_t *intersect_aside__untraced(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int depth) JL_CANSAFEPOINT;
static jl_value_t *intersect_union(jl_value_t *x, jl_uniontype_t *u, jl_stenv_t *e, int8_t R, jl_param_pos_t param) JL_CANSAFEPOINT;
static jl_value_t *intersect_union__untraced(jl_value_t *x, jl_uniontype_t *u, jl_stenv_t *e, int8_t R, jl_param_pos_t param) JL_CANSAFEPOINT;
static jl_value_t *intersect_var(jl_tvar_t *b, jl_value_t *a, jl_stenv_t *e, int8_t R, jl_param_pos_t param) JL_CANSAFEPOINT;
static jl_value_t *intersect_var__untraced(jl_tvar_t *b, jl_value_t *a, jl_stenv_t *e, int8_t R, jl_param_pos_t param) JL_CANSAFEPOINT;
static jl_value_t *intersect_unionall(jl_value_t *t, jl_unionall_t *u, jl_stenv_t *e, int8_t R, jl_param_pos_t param) JL_CANSAFEPOINT;
static jl_value_t *intersect_unionall__untraced(jl_value_t *t, jl_unionall_t *u, jl_stenv_t *e, int8_t R, jl_param_pos_t param) JL_CANSAFEPOINT;
static jl_value_t *intersect_unionall_(jl_value_t *t, jl_unionall_t *u, jl_stenv_t *e, int8_t R, jl_param_pos_t param, jl_varbinding_t *vb) JL_CANSAFEPOINT;
static jl_value_t *intersect_unionall___untraced(jl_value_t *t, jl_unionall_t *u, jl_stenv_t *e, int8_t R, jl_param_pos_t param, jl_varbinding_t *vb) JL_CANSAFEPOINT;
static jl_value_t *finish_unionall(jl_value_t *res JL_MAYBE_UNROOTED, jl_varbinding_t *vb, jl_unionall_t *u, jl_stenv_t *e) JL_CANSAFEPOINT;
static jl_value_t *finish_unionall__untraced(jl_value_t *res JL_MAYBE_UNROOTED, jl_varbinding_t *vb, jl_unionall_t *u, jl_stenv_t *e) JL_CANSAFEPOINT;
static jl_value_t *intersect_tuple(jl_datatype_t *xd, jl_datatype_t *yd, jl_stenv_t *e, jl_param_pos_t param) JL_CANSAFEPOINT;
static jl_value_t *intersect_tuple__untraced(jl_datatype_t *xd, jl_datatype_t *yd, jl_stenv_t *e, jl_param_pos_t param) JL_CANSAFEPOINT;
static jl_value_t *intersect_varargs(jl_vararg_t *vmx, jl_vararg_t *vmy, ssize_t offset, jl_stenv_t *e, jl_param_pos_t param) JL_CANSAFEPOINT;
static jl_value_t *intersect_varargs__untraced(jl_vararg_t *vmx, jl_vararg_t *vmy, ssize_t offset, jl_stenv_t *e, jl_param_pos_t param) JL_CANSAFEPOINT;
static jl_value_t *intersect_invariant(jl_value_t *x, jl_value_t *y, jl_stenv_t *e) JL_CANSAFEPOINT;
static jl_value_t *intersect_invariant__untraced(jl_value_t *x, jl_value_t *y, jl_stenv_t *e) JL_CANSAFEPOINT;
static jl_value_t *intersect_sub_datatype(jl_datatype_t *xd, jl_datatype_t *yd, jl_stenv_t *e, int R, jl_param_pos_t param) JL_CANSAFEPOINT;
static jl_value_t *intersect_sub_datatype__untraced(jl_datatype_t *xd, jl_datatype_t *yd, jl_stenv_t *e, int R, jl_param_pos_t param) JL_CANSAFEPOINT;
static jl_value_t *intersect_type_type(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int8_t R) JL_CANSAFEPOINT;
static jl_value_t *intersect_type_type__untraced(jl_value_t *x, jl_value_t *y, jl_stenv_t *e, int8_t R) JL_CANSAFEPOINT;
static jl_value_t *set_var_to_const(jl_varbinding_t *bb, jl_value_t *v JL_MAYBE_UNROOTED, jl_stenv_t *e, int R) JL_CANSAFEPOINT;
static jl_value_t *set_var_to_const__untraced(jl_varbinding_t *bb, jl_value_t *v JL_MAYBE_UNROOTED, jl_stenv_t *e, int R) JL_CANSAFEPOINT;
static jl_value_t *bound_var_below(jl_tvar_t *tv, jl_varbinding_t *bb, jl_stenv_t *e, int R) JL_CANSAFEPOINT;
static jl_value_t *bound_var_below__untraced(jl_tvar_t *tv, jl_varbinding_t *bb, jl_stenv_t *e, int R) JL_CANSAFEPOINT;
static jl_value_t *intersect_types(jl_value_t *x, jl_value_t *y, int emptiness_only) JL_CANSAFEPOINT;
static jl_value_t *intersect_types__untraced(jl_value_t *x, jl_value_t *y, int emptiness_only) JL_CANSAFEPOINT;
JL_DLLEXPORT int jl_subtype_env__untraced(jl_value_t *x, jl_value_t *y, jl_value_t **env, int envsz) JL_CANSAFEPOINT;
JL_DLLEXPORT int jl_types_equal__untraced(jl_value_t *a, jl_value_t *b) JL_CANSAFEPOINT;
JL_DLLEXPORT int jl_isa__untraced(jl_value_t *x, jl_value_t *t) JL_CANSAFEPOINT;
jl_value_t *jl_type_intersection_env_s__untraced(jl_value_t *a, jl_value_t *b, jl_svec_t **penv, int *issubty) JL_CANSAFEPOINT;

#else

#define STRACE_IMPL(f) f
#define STRACE_STENV(e)
#define STRACE_UPICK(e, R, u, site, slot, bit, fresh)
#define STRACE_UNEXT(e, R, ok)
#define STRACE_USTATE(ev, st)
#define STRACE_ITER(e, R, loop)
#define STRACE_ENV_ALLOC(se)
#define STRACE_ENV_SAVE(e, se)
#define STRACE_ENV_RESTORE(e, se, root)
#define STRACE_BIND(e, vb)
#define STRACE_UNBIND(e, vb)
#define STRACE_SAVEPOINT(sp)
#define STRACE_UNWIND(sp)

#endif

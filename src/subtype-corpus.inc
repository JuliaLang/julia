// This file is a part of Julia. License is MIT: https://julialang.org/license

/* Opt-in, self-contained corpus of completed public subtype queries.
 * See doc/src/devdocs/subtype-corpus.md. This deliberately inspects only the
 * input grammar and the public Boolean result, not algorithm-internal state.
 * All collection and I/O below is non-safepointing; no Julia code is called.
 */
#include "julia_gcext.h"

#define SCORPUS_MAX_NODES 4096
#define SCORPUS_MAX_DEPTH 256
#define SCORPUS_BUCKETS 4096

typedef enum { SC_ANY, SC_BOTTOM, SC_UNION, SC_INTERSECT, SC_NOT, SC_ABSTRACT, SC_PRIMITIVE, SC_TUPLE, SC_TYPEVAR, SC_UNIONALL } scorpus_kind;
typedef struct {
    scorpus_kind kind;
    int a, b, binder;
    jl_datatype_t *dt;
} scorpus_node;
typedef struct {
    scorpus_node nodes[SCORPUS_MAX_NODES];
    int len;
    jl_value_t *inputs[SCORPUS_MAX_NODES];
    int input_ids[SCORPUS_MAX_NODES];
    int ninputs;
    int edges[SCORPUS_MAX_NODES];
    int nedges, nbinders, extended;
} scorpus_query;
typedef struct scorpus_nominal {
    jl_datatype_t *dt; // retained by scorpus_roots; prevents address reuse
    char *key;
    unsigned ordinal;
    int stable;
    jl_uuid_t build_id, uuid;
    struct scorpus_nominal *next;
} scorpus_nominal;
typedef struct scorpus_seen {
    uint64_t hash;
    size_t len;
    char *key;
    int result;
    struct scorpus_seen *next;
} scorpus_seen;

static uv_once_t scorpus_once = UV_ONCE_INIT;
static uv_mutex_t scorpus_lock;
static _Atomic(int) scorpus_enabled = -1;
// 0: nominal; 1: tuples; 2: UnionAll; 3: datatypes; 4: linear; 5: unrestricted.
static int scorpus_grammar;
static char *scorpus_dir;
static scorpus_nominal *scorpus_nominals;
static scorpus_seen *scorpus_seen_queries[SCORPUS_BUCKETS];
static int scorpus_warned;
static uint64_t scorpus_session;

static void scorpus_roots(int full) JL_NOTSAFEPOINT
{
    jl_ptls_t ptls = jl_current_task->ptls;
    for (scorpus_nominal *n = scorpus_nominals; n; n = n->next)
        jl_gc_mark_queue_obj(ptls, (jl_value_t*)n->dt);
}

static void scorpus_init(void) JL_NOTSAFEPOINT
{
    const char *dir = getenv("JULIA_SUBTYPE_CORPUS_DIR");
    if (!dir || !*dir) {
        jl_atomic_store_release(&scorpus_enabled, 0);
        return;
    }
    const char *grammar = getenv("JULIA_SUBTYPE_CORPUS_GRAMMAR");
    if (grammar && !strcmp(grammar, "tuples"))
        scorpus_grammar = 1;
    else if (grammar && !strcmp(grammar, "unionall"))
        scorpus_grammar = 2;
    else if (grammar && !strcmp(grammar, "datatype"))
        scorpus_grammar = 3;
    else if (grammar && !strcmp(grammar, "linear"))
        scorpus_grammar = 4;
    else if (grammar && !strcmp(grammar, "full"))
        scorpus_grammar = 5;
    else if (grammar && *grammar && strcmp(grammar, "nominal")) {
        fprintf(stderr, "subtype corpus: unknown grammar %s (expected nominal, tuples, unionall, datatype, linear, or full)\n", grammar);
        jl_atomic_store_release(&scorpus_enabled, 0);
        return;
    }
    uv_fs_t req;
    int err = uv_fs_mkdir(unused_uv_loop_arg, &req, dir, 0755, NULL);
    uv_fs_req_cleanup(&req);
    if (err && err != UV_EEXIST) {
        fprintf(stderr, "subtype corpus: cannot create %s: %s\n", dir, uv_strerror(err));
        jl_atomic_store_release(&scorpus_enabled, 0);
        return;
    }
    err = uv_fs_realpath(unused_uv_loop_arg, &req, dir, NULL);
    if (err) {
        fprintf(stderr, "subtype corpus: cannot resolve %s: %s\n", dir, uv_strerror(err));
        uv_fs_req_cleanup(&req);
        jl_atomic_store_release(&scorpus_enabled, 0);
        return;
    }
    scorpus_dir = strdup((char*)req.ptr);
    uv_fs_req_cleanup(&req);
    if (!scorpus_dir || uv_mutex_init(&scorpus_lock))
        abort();
    scorpus_session = uv_hrtime();
    jl_gc_set_cb_root_scanner(scorpus_roots, 1);
    jl_atomic_store_release(&scorpus_enabled, 1);
}

static inline int scorpus_active(void) JL_NOTSAFEPOINT
{
    int enabled = jl_atomic_load_acquire(&scorpus_enabled);
    if (__unlikely(enabled < 0)) {
        uv_once(&scorpus_once, scorpus_init);
        enabled = jl_atomic_load_acquire(&scorpus_enabled);
    }
    return enabled;
}

static void scorpus_string(ios_t *o, const char *s) JL_NOTSAFEPOINT
{
    ios_putc('"', o);
    for (; *s; s++) {
        unsigned char c = *s;
        if (c == '"' || c == '\\') {
            ios_putc('\\', o);
            ios_putc(c, o);
        }
        else if (c < 0x20 || c == 0x7f)
            ios_printf(o, "\\u%04x", c);
        else
            ios_putc(c, o);
    }
    ios_putc('"', o);
}

// Variables are resolved lexically, never by their printed names. A binding
// occurrence owns its TypeVar node; sibling binders remain distinct even when
// raw expressions reuse the same TypeVar pointer.
typedef struct scorpus_scope {
    jl_tvar_t *var;
    int id;
    const struct scorpus_scope *outer;
} scorpus_scope;

static int scorpus_intern(scorpus_query *q, scorpus_node n) JL_NOTSAFEPOINT
{
    for (int i = 0; i < q->len; i++) {
        scorpus_node *p = &q->nodes[i];
        if (n.kind != p->kind || n.b != p->b || n.dt != p->dt || n.binder != p->binder)
            continue;
        if (n.kind == SC_TUPLE) {
            if (memcmp(q->edges + n.a, q->edges + p->a, n.b * sizeof(int)))
                continue;
        }
        else if (n.a != p->a)
            continue;
        return i + 1;
    }
    if (q->len == SCORPUS_MAX_NODES)
        return 0;
    q->nodes[q->len++] = n;
    return q->len;
}

static int scorpus_node_id(scorpus_query *q, jl_value_t *v, unsigned depth,
                          const scorpus_scope *scope) JL_NOTSAFEPOINT
{
    if (!v || depth > SCORPUS_MAX_DEPTH)
        return 0;
    // Pointer memoization is valid for closed nominal expressions. Extended
    // queries must revisit shared expressions in their current lexical scope.
    if (!scorpus_grammar) {
        for (int i = 0; i < q->ninputs; i++)
            if (q->inputs[i] == v)
                return q->input_ids[i];
    }
    if (q->ninputs == SCORPUS_MAX_NODES)
        return 0;
    int input = q->ninputs++;
    q->inputs[input] = v;
    q->input_ids[input] = 0;
    scorpus_node n = {SC_ANY, 0, 0, 0, NULL};
    if (v == (jl_value_t*)jl_any_type)
        n.kind = SC_ANY;
    else if (v == jl_bottom_type)
        n.kind = SC_BOTTOM;
    else if (jl_is_typevar(v)) {
        if (scorpus_grammar < 2)
            return 0;
        for (const scorpus_scope *s = scope; s; s = s->outer)
            if ((jl_value_t*)s->var == v)
                return s->id;
        return 0; // free variables are not standalone closed queries
    }
    else if (jl_is_unionall(v)) {
        if (scorpus_grammar < 2)
            return 0;
        jl_unionall_t *ua = (jl_unionall_t*)v;
        if (!ua->var || !jl_is_typevar(ua->var))
            return 0;
        scorpus_node var = {SC_TYPEVAR, 0, 0, ++q->nbinders, NULL};
        var.a = scorpus_node_id(q, ua->var->lb, depth + 1, scope);
        var.b = scorpus_node_id(q, ua->var->ub, depth + 1, scope);
        if (!var.a || !var.b)
            return 0;
        n.kind = SC_UNIONALL;
        n.a = scorpus_intern(q, var);
        if (!n.a)
            return 0;
        scorpus_scope inner = {ua->var, n.a, scope};
        n.b = scorpus_node_id(q, ua->body, depth + 1, &inner);
        if (!n.b)
            return 0;
        q->extended = 1;
    }
    else if (jl_is_uniontype(v) || jl_is_intersecttype(v)) {
        n.kind = jl_is_uniontype(v) ? SC_UNION : SC_INTERSECT;
        jl_value_t *a = n.kind == SC_UNION ? ((jl_uniontype_t*)v)->a : ((jl_intersecttype_t*)v)->a;
        jl_value_t *b = n.kind == SC_UNION ? ((jl_uniontype_t*)v)->b : ((jl_intersecttype_t*)v)->b;
        n.a = scorpus_node_id(q, a, depth + 1, scope);
        n.b = scorpus_node_id(q, b, depth + 1, scope);
        if (!n.a || !n.b)
            return 0;
    }
    else if (jl_is_nottype(v)) {
        n.kind = SC_NOT;
        n.a = scorpus_node_id(q, ((jl_nottype_t*)v)->body, depth + 1, scope);
        if (!n.a)
            return 0;
    }
    else if (jl_is_datatype(v)) {
        jl_datatype_t *dt = (jl_datatype_t*)v;
        if (!dt->name || !dt->name->module || !dt->parameters)
            return 0;
        if (dt->name == jl_tuple_typename) {
            if (!scorpus_grammar || dt == jl_anytuple_type || jl_nparams(dt) > SCORPUS_MAX_NODES - q->nedges)
                return 0;
            n.kind = SC_TUPLE;
            n.a = q->nedges;
            n.b = jl_nparams(dt);
            q->nedges += n.b;
            for (int i = 0; i < n.b; i++) {
                int id = scorpus_node_id(q, jl_tparam(dt, i), depth + 1, scope);
                if (!id)
                    return 0; // includes Vararg and non-type parameters
                q->edges[n.a + i] = id;
            }
            q->extended = 1;
        }
        else {
            if (jl_nparams(dt) || (!dt->name->abstract && !dt->isprimitivetype) ||
                (dt->isprimitivetype && !dt->layout))
                return 0;
            n.kind = dt->name->abstract ? SC_ABSTRACT : SC_PRIMITIVE;
            n.dt = dt;
            n.a = scorpus_node_id(q, (jl_value_t*)dt->super, depth + 1, scope);
            if (!n.a)
                return 0;
        }
    }
    else
        return 0;
    int id = scorpus_intern(q, n);
    q->input_ids[input] = id;
    return id;
}

static int scorpus_module(ios_t *o, jl_module_t *m) JL_NOTSAFEPOINT
{
    jl_module_t *path[SCORPUS_MAX_DEPTH];
    unsigned len = 0;
    while (m && len < SCORPUS_MAX_DEPTH) {
        path[len++] = m;
        if (m == m->parent)
            break;
        m = m->parent;
    }
    if (len == SCORPUS_MAX_DEPTH)
        return 0;
    ios_putc('[', o);
    for (unsigned i = len; i > 0; i--) {
        if (i != len)
            ios_putc(',', o);
        scorpus_string(o, jl_symbol_name(path[i - 1]->name));
    }
    ios_putc(']', o);
    return 1;
}

// Read existing bindings without resolving imports, allocating partitions, or
// invoking a safepoint. Unbound/ambiguous identities are process-scoped.
static int scorpus_canonical_binding(jl_datatype_t *dt) JL_NOTSAFEPOINT
{
    jl_svec_t *bindings = jl_atomic_load_acquire(&dt->name->module->bindings);
    if (!bindings)
        return 0;
    for (size_t i = 0; i < jl_svec_len(bindings); i++) {
        jl_value_t *v = jl_svecref(bindings, i);
        if (v == jl_nothing || !v)
            continue;
        jl_binding_t *b = (jl_binding_t*)v;
        if (b->globalref->name == dt->name->name)
            return jl_get_latest_binding_value_if_resolved_and_const_debug_only(b) == (jl_value_t*)dt;
    }
    return 0;
}

static scorpus_nominal *scorpus_nominal_id(jl_datatype_t *dt) JL_NOTSAFEPOINT
{
    for (scorpus_nominal *n = scorpus_nominals; n; n = n->next)
        if (n->dt == dt)
            return n;
    ios_t key;
    ios_mem(&key, 128);
    jl_module_t *m = dt->name->module;
    ios_printf(&key, "%016" PRIx64 "%016" PRIx64 ":", m->build_id.hi, m->build_id.lo);
    scorpus_module(&key, m);
    ios_putc(':', &key);
    scorpus_string(&key, jl_symbol_name(dt->name->name));
    size_t size;
    char *s = ios_take_buffer(&key, &size);
    ios_close(&key);
    unsigned ordinal = 0;
    for (scorpus_nominal *n = scorpus_nominals; n; n = n->next)
        if (!strcmp(s, n->key) && n->ordinal >= ordinal)
            ordinal = n->ordinal + 1;
    scorpus_nominal *n = (scorpus_nominal*)malloc_s(sizeof(*n));
    n->dt = dt;
    n->key = s;
    n->ordinal = ordinal;
    n->build_id = m->build_id;
    n->uuid = m->uuid;
    // Image restoration already marks object headers before registering its
    // address ranges. Subtype queries can occur in that interval, so use the
    // header flag rather than jl_object_in_image's registered-range assertion.
    n->stable = scorpus_canonical_binding(dt) &&
        (m == jl_core_module || jl_astaggedvalue(dt)->bits.in_image != 0);
    // A process-scoped clone encountered first must not change the canonical
    // image type's identity. A genuinely replaced canonical binding, however,
    // must not reuse the identity already assigned to its predecessor.
    for (scorpus_nominal *p = scorpus_nominals; n->stable && p; p = p->next)
        if (p->stable && !strcmp(n->key, p->key))
            n->stable = 0;
    n->next = scorpus_nominals;
    scorpus_nominals = n;
    return n;
}

static char *scorpus_encode(scorpus_query *q, int lhs, int rhs, size_t *len) JL_NOTSAFEPOINT
{
    static const char *kinds[] = {"Any", "Bottom", "Union", "Intersect", "Not", "Abstract", "Primitive", "Tuple", "TypeVar", "UnionAll"};
    ios_t o;
    ios_mem(&o, 512);
    ios_printf(&o, "version = %d\nkind = \"subtype\"\nlhs = %d\nrhs = %d\n", q->extended ? 2 : 1, lhs, rhs);
    for (int i = 0; i < q->len; i++) {
        scorpus_node *n = &q->nodes[i];
        ios_printf(&o, "\n[[nodes]]\nid = %d\nkind = \"%s\"\n", i + 1, kinds[n->kind]);
        if (n->kind == SC_UNION || n->kind == SC_INTERSECT)
            ios_printf(&o, "a = %d\nb = %d\n", n->a, n->b);
        else if (n->kind == SC_NOT)
            ios_printf(&o, "child = %d\n", n->a);
        else if (n->kind == SC_TUPLE) {
            ios_puts("elements = [", &o);
            for (int j = 0; j < n->b; j++)
                ios_printf(&o, "%s%d", j ? "," : "", q->edges[n->a + j]);
            ios_puts("]\n", &o);
        }
        else if (n->kind == SC_TYPEVAR)
            ios_printf(&o, "lb = %d\nub = %d\nbinder = %d\n", n->a, n->b, n->binder);
        else if (n->kind == SC_UNIONALL)
            ios_printf(&o, "var = %d\nbody = %d\n", n->a, n->b);
        else if (n->dt) {
            jl_datatype_t *dt = n->dt;
            jl_module_t *m = dt->name->module;
            scorpus_nominal *nominal = scorpus_nominal_id(dt);
            ios_puts("name = ", &o);
            scorpus_string(&o, jl_symbol_name(dt->name->name));
            ios_puts("\nmodule = ", &o);
            if (!scorpus_module(&o, m)) {
                ios_close(&o);
                return NULL;
            }
            ios_printf(&o, "\nmodule_uuid = \"%016" PRIx64 "%016" PRIx64 "\"\nmodule_build_id = \"%016" PRIx64 "%016" PRIx64 "\"\n",
                       nominal->uuid.hi, nominal->uuid.lo, nominal->build_id.hi, nominal->build_id.lo);
            ios_puts("nominal_id = ", &o);
            ios_t id;
            ios_mem(&id, 128);
            if (nominal->stable)
                ios_printf(&id, "%s:canonical", nominal->key);
            else
                ios_printf(&id, "%s:process-%d-%016" PRIx64 ":%u", nominal->key,
                           (int)uv_os_getpid(), scorpus_session, nominal->ordinal);
            ios_putc(0, &id);
            scorpus_string(&o, id.buf);
            ios_close(&id);
            ios_printf(&o, "\nsuper = %d\n", n->a);
            if (n->kind == SC_PRIMITIVE)
                ios_printf(&o, "bits = %u\n", jl_datatype_nbits(dt));
        }
    }
    char *key = ios_take_buffer(&o, len);
    ios_close(&o);
    // ios_take_buffer includes its trailing NUL in the returned size.
    (*len)--;
    return key;
}

#include "subtype-corpus-filter.inc"
#include "subtype-corpus-full.inc"

static uint64_t scorpus_hash(const char *s, size_t len) JL_NOTSAFEPOINT
{
    uint64_t h = UINT64_C(14695981039346656037);
    for (size_t i = 0; i < len; i++) {
        h ^= (unsigned char)s[i];
        h *= UINT64_C(1099511628211);
    }
    return h;
}

// The fixed-width first line is excluded from query identity. No hash is ever
// trusted as proof of equality: compare all key bytes, including nominal IDs.
#define SCORPUS_RESULT_LEN (sizeof("result = false\n") - 1)
static int scorpus_compare(const char *path, const char *key, size_t len, int *result) JL_NOTSAFEPOINT
{
    FILE *f = fopen(path, "rb");
    if (!f)
        return errno == ENOENT ? 0 : -1;
    char header[SCORPUS_RESULT_LEN];
    if (fread(header, 1, sizeof(header), f) != sizeof(header) ||
        (memcmp(header, "result = true \n", sizeof(header)) &&
         memcmp(header, "result = false\n", sizeof(header)))) {
        fclose(f);
        return 2;
    }
    *result = header[9] == 't';
    size_t pos = 0;
    char buf[4096];
    while (pos < len) {
        size_t n = len - pos < sizeof(buf) ? len - pos : sizeof(buf);
        if (fread(buf, 1, n, f) != n || memcmp(buf, key + pos, n)) {
            fclose(f);
            return 2;
        }
        pos += n;
    }
    int match = fgetc(f) == EOF && !ferror(f);
    fclose(f);
    return match ? 1 : 2;
}

// Publish only complete files. Hard-link creation is atomic and cannot replace
// an existing query, even when independent bootstrap processes share a corpus.
static int scorpus_publish(const char *path, const char *key, size_t len, int result) JL_NOTSAFEPOINT
{
    size_t cap = strlen(scorpus_dir) + 32;
    char *pattern = (char*)malloc_s(cap);
    snprintf(pattern, cap, "%s/.query-XXXXXX", scorpus_dir);
    uv_fs_t req;
    int status = uv_fs_mkstemp(unused_uv_loop_arg, &req, pattern, NULL);
    int fd = status < 0 ? status : (int)req.result;
    free(pattern);
    char *temp = fd >= 0 ? strdup(req.path) : NULL;
    uv_fs_req_cleanup(&req);
    if (fd < 0)
        return fd;
    if (!temp)
        abort();
    int err = 0;
    const char *parts[] = {result ? "result = true \n" : "result = false\n", key};
    size_t lengths[] = {SCORPUS_RESULT_LEN, len};
    for (int part = 0; part < 2 && !err; part++) {
        size_t pos = 0;
        while (pos < lengths[part]) {
            uv_buf_t buf = uv_buf_init((char*)parts[part] + pos, lengths[part] - pos);
            int n = uv_fs_write(unused_uv_loop_arg, &req, fd, &buf, 1, -1, NULL);
            uv_fs_req_cleanup(&req);
            if (n <= 0) {
                err = n ? n : UV_EIO;
                break;
            }
            pos += n;
        }
    }
    int close_err = uv_fs_close(unused_uv_loop_arg, &req, fd, NULL);
    uv_fs_req_cleanup(&req);
    if (!err)
        err = close_err;
    if (!err) {
        err = uv_fs_link(unused_uv_loop_arg, &req, temp, path, NULL);
        uv_fs_req_cleanup(&req);
    }
    uv_fs_unlink(unused_uv_loop_arg, &req, temp, NULL);
    uv_fs_req_cleanup(&req);
    free(temp);
    return err;
}

static void scorpus_conflict(const char *path, const char *key, size_t len, int result) JL_NOTSAFEPOINT
{
    size_t n = strlen(path) + 16;
    char *conflict = (char*)malloc_s(n);
    snprintf(conflict, n, "%s.conflict", path);
    scorpus_publish(conflict, key, len, result);
    fprintf(stderr, "subtype corpus: conflicting Boolean results for %s (new result %d)\n", path, result);
    free(conflict);
}

static void scorpus_record(jl_value_t *x, jl_value_t *y, int result) JL_NOTSAFEPOINT
{
    if (!scorpus_active())
        return;
    size_t len;
    char *key;
    if (scorpus_grammar >= 3) {
        unsigned visits = 0;
        if (scorpus_grammar != 5 &&
            (!scorpus_extended_admit(x, NULL, 0, &visits, 1) ||
             !scorpus_extended_admit(y, NULL, 0, &visits, 1)))
            return;
        uv_mutex_lock(&scorpus_lock);
        key = scorpus_encode_full(x, y, &len);
    }
    else {
        scorpus_query *q = (scorpus_query*)malloc_s(sizeof(*q));
        q->len = q->ninputs = q->nedges = q->nbinders = q->extended = 0;
        int lhs = scorpus_node_id(q, x, 0, NULL);
        int rhs = lhs ? scorpus_node_id(q, y, 0, NULL) : 0;
        if (!lhs || !rhs) {
            free(q);
            return;
        }
        uv_mutex_lock(&scorpus_lock);
        key = scorpus_encode(q, lhs, rhs, &len);
        free(q);
    }
    if (!key) {
        uv_mutex_unlock(&scorpus_lock);
        return;
    }
    uint64_t hash = scorpus_hash(key, len);
    unsigned bucket = hash % SCORPUS_BUCKETS;
    for (scorpus_seen *p = scorpus_seen_queries[bucket]; p; p = p->next) {
        if (p->hash == hash && p->len == len && !memcmp(key, p->key, len) && p->result == result) {
            free(key);
            uv_mutex_unlock(&scorpus_lock);
            return;
        }
    }
    size_t cap = strlen(scorpus_dir) + 80;
    char *path = (char*)malloc_s(cap);
    int err = 0, stored_result = result;
    for (unsigned collision = 0; ; collision++) {
        snprintf(path, cap, "%s/%016" PRIx64 "-%u.toml", scorpus_dir, hash, collision);
        int cmp = scorpus_compare(path, key, len, &stored_result);
        if (cmp == 2)
            continue;
        if (cmp < 0) {
            err = UV_EIO;
            break;
        }
        if (cmp == 1) {
            if (stored_result != result)
                scorpus_conflict(path, key, len, result);
            break;
        }
        err = scorpus_publish(path, key, len, result);
        if (err == UV_EEXIST) {
            collision--; // another process won; compare its complete file
            err = 0;
            continue;
        }
        break;
    }
    if (!err) {
        scorpus_seen *p = (scorpus_seen*)malloc_s(sizeof(*p));
        *p = (scorpus_seen){hash, len, key, result, scorpus_seen_queries[bucket]};
        scorpus_seen_queries[bucket] = p;
    }
    else {
        if (!scorpus_warned++)
            fprintf(stderr, "subtype corpus: cannot save %s: %s\n", path, uv_strerror(err));
        free(key);
    }
    free(path);
    uv_mutex_unlock(&scorpus_lock);
}

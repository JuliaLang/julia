
// Forward declarations for private staticdata.c methods
static size_t n_linkage_blobs(void) JL_NOTSAFEPOINT;
static size_t external_blob_index(jl_value_t *v) JL_NOTSAFEPOINT;

// inverse of backedges graph (caller=>callees hash)
jl_array_t *internal_methods JL_GLOBALLY_ROOTED = NULL; // rooted for the duration of our uses of this

static void write_float64(ios_t *s, double x) JL_NOTSAFEPOINT
{
    write_uint64(s, *((uint64_t*)&x));
}

// BUILD_PATH_PREFIX_MAP (https://reproducible-builds.org/specs/build-path-prefix-map/):
// remap source path prefixes when writing images.

typedef struct {
    char *target;
    size_t target_len;
    char *source;
    size_t source_len;
} prefix_map_pair_t;

static prefix_map_pair_t *prefix_map_pairs = NULL;
static size_t prefix_map_npairs = 0;
static enum {
    PREFIX_MAP_UNPARSED = -1,
    PREFIX_MAP_INACTIVE = 0,
    PREFIX_MAP_ACTIVE = 1,
    PREFIX_MAP_MALFORMED = 2,
} prefix_map_state = PREFIX_MAP_UNPARSED;

// decode `%.` `%+` `%#` escapes; NULL if invalid
static char *prefix_map_unescape(const char *s, size_t len, size_t *outlen) JL_NOTSAFEPOINT
{
    char *out = (char*)malloc_s(len + 1);
    size_t j = 0;
    for (size_t i = 0; i < len; i++) {
        char c = s[i];
        if (c == '%') {
            char e = i + 1 < len ? s[i + 1] : '\0';
            c = e == '.' ? ':' : e == '+' ? '=' : e == '#' ? '%' : '\0';
            if (c == '\0') {
                free(out);
                return NULL;
            }
            i++;
        }
        else if (c == '=' || c == ':') {
            free(out);
            return NULL;
        }
        out[j++] = c;
    }
    out[j] = '\0';
    *outlen = j;
    return out;
}

static void prefix_map_free(prefix_map_pair_t *pairs, size_t npairs) JL_NOTSAFEPOINT
{
    for (size_t i = 0; i < npairs; i++) {
        free(pairs[i].target);
        free(pairs[i].source);
    }
    free(pairs);
}

// returns 0 if malformed, which invalidates the whole map (per the spec)
static int prefix_map_parse(const char *spec, prefix_map_pair_t **out_pairs, size_t *out_npairs) JL_NOTSAFEPOINT
{
    prefix_map_pair_t *pairs = NULL;
    size_t n = 0, cap = 0;
    const char *p = spec;
    while (1) {
        const char *end = strchr(p, ':');
        size_t len = end ? (size_t)(end - p) : strlen(p);
        if (len > 0) { // empty subsequences between ':' are ignored
            const char *eq = (const char*)memchr(p, '=', len);
            char *target = NULL, *source = NULL;
            size_t tlen = 0, slen = 0;
            if (eq) {
                target = prefix_map_unescape(p, eq - p, &tlen);
                if (target)
                    source = prefix_map_unescape(eq + 1, p + len - (eq + 1), &slen);
            }
            // reject '@'-prefixed targets, which would collide with the loader's @depot aliases
            if (!source || (target[0] == '@')) {
                free(target);
                free(source);
                prefix_map_free(pairs, n);
                return 0;
            }
            if (n == cap) {
                cap = cap ? 2 * cap : 4;
                pairs = (prefix_map_pair_t*)realloc_s(pairs, cap * sizeof(prefix_map_pair_t));
            }
            pairs[n].target = target;
            pairs[n].target_len = tlen;
            pairs[n].source = source;
            pairs[n].source_len = slen;
            n++;
        }
        if (!end)
            break;
        p = end + 1;
    }
    *out_pairs = pairs;
    *out_npairs = n;
    return 1;
}

static int is_path_sep(char c) JL_NOTSAFEPOINT
{
#ifdef _OS_WINDOWS_
    if (c == '\\')
        return 1;
#endif
    return c == '/';
}

// path-component prefix matching (byte-exact, no case folding); when several of the
// map's `target=source` pairs match, the rightmost one in the variable wins, since
// the spec has producers append new pairs rather than overwrite existing ones.
// Returns a malloc'd string, or NULL when no pair matches.
static char *prefix_map_apply(prefix_map_pair_t *pairs, size_t npairs,
                              const char *path, size_t len, size_t *outlen) JL_NOTSAFEPOINT
{
    if (len == 0)
        return NULL;
    for (size_t i = npairs; i-- > 0; ) {
        const char *source = pairs[i].source;
        size_t slen = pairs[i].source_len;
        while (slen > 1 && is_path_sep(source[slen - 1]))
            slen--;
        if (len < slen || memcmp(path, source, slen))
            continue;
        // component-boundary check: /path/to/a must not match /path/to/aa/b
        if (len > slen && !is_path_sep(path[slen]) && !(slen > 0 && is_path_sep(source[slen - 1])))
            continue;
        size_t tlen = pairs[i].target_len;
        size_t rest = len - slen;
        char *out = (char*)malloc_s(tlen + rest + 1);
        memcpy(out, pairs[i].target, tlen);
        memcpy(out + tlen, path + slen, rest);
        out[tlen + rest] = '\0';
        *outlen = tlen + rest;
        return out;
    }
    return NULL;
}

static void prefix_map_init(void) JL_NOTSAFEPOINT
{
    if (prefix_map_state != PREFIX_MAP_UNPARSED)
        return;
    // uv_os_getenv rather than getenv: on Windows it reads the wide environment
    // and converts to WTF-8, matching the encoding of the paths being mapped
    size_t size = 256;
    char *spec = (char*)malloc_s(size);
    int r = uv_os_getenv("BUILD_PATH_PREFIX_MAP", spec, &size);
    if (r == UV_ENOBUFS) {
        spec = (char*)realloc_s(spec, size);
        r = uv_os_getenv("BUILD_PATH_PREFIX_MAP", spec, &size);
    }
    if (r != 0 || !spec[0]) {
        free(spec);
        prefix_map_state = PREFIX_MAP_INACTIVE;
        return;
    }
    prefix_map_state = prefix_map_parse(spec, &prefix_map_pairs, &prefix_map_npairs) ?
        PREFIX_MAP_ACTIVE : PREFIX_MAP_MALFORMED;
    free(spec);
}

// a malformed map errors rather than silently writing unmapped output; call this
// before serialization starts (jl_maybe_map_build_path_sym cannot throw)
static int prefix_map_active(void)
{
    prefix_map_init();
    if (prefix_map_state == PREFIX_MAP_MALFORMED)
        jl_error("malformed BUILD_PATH_PREFIX_MAP value");
    return prefix_map_state == PREFIX_MAP_ACTIVE;
}

static char *maybe_map_build_path(const char *path, size_t len, size_t *outlen) JL_NOTSAFEPOINT
{
    if (prefix_map_state != PREFIX_MAP_ACTIVE || !jl_generating_output())
        return NULL;
    return prefix_map_apply(prefix_map_pairs, prefix_map_npairs, path, len, outlen);
}

// raw-string variant for codegen debug info (DIFile paths); self-initializing since
// codegen for an output image runs before serialization validates the map (a
// malformed map is inactive here and errors later in prefix_map_active)
JL_DLLEXPORT char *jl_maybe_map_build_path_cstr(const char *path, size_t len, size_t *outlen) JL_NOTSAFEPOINT
{
    prefix_map_init();
    return maybe_map_build_path(path, len, outlen);
}

static jl_sym_t *jl_maybe_map_build_path_sym(jl_sym_t *s) JL_NOTSAFEPOINT
{
    if (s == NULL)
        return s;
    const char *name = jl_symbol_name(s);
    size_t outlen;
    char *mapped = maybe_map_build_path(name, strlen(name), &outlen);
    if (!mapped)
        return s;
    jl_sym_t *r = jl_symbol_n(mapped, outlen);
    free(mapped);
    return r;
}

jl_value_t *jl_maybe_map_build_path_jlstr(jl_value_t *str) JL_CANSAFEPOINT
{
    size_t len = jl_string_len(str);
    const char *data = jl_string_data(str);
    // skip binary-packed `require` records (leading NUL) and @depot aliases
    if (len == 0 || data[0] == '\0' || (len >= 6 && !memcmp(data, "@depot", 6)))
        return str;
    size_t outlen;
    char *mapped = maybe_map_build_path(data, len, &outlen);
    if (!mapped)
        return str;
    jl_value_t *r = jl_pchar_to_string(mapped, outlen);
    free(mapped);
    return r;
}

// remap build paths inside a quoted-AST literal from Method.roots: path symbols and
// strings (anchored prefix match, like gcc's -fmacro-prefix-map for __FILE__), and
// LineNumberNode files inside Expr/QuoteNode trees; returns NULL when unchanged.
// Allocates without rooting, so only call while the GC is disabled (serialization).
static jl_value_t *maybe_map_build_path_in_ast(jl_value_t *v, int depth) JL_CANSAFEPOINT JL_GC_DISABLED
{
    if (v == NULL || depth > 100)
        return NULL;
    if (jl_is_symbol(v)) {
        jl_sym_t *mapped = jl_maybe_map_build_path_sym((jl_sym_t*)v);
        return mapped == (jl_sym_t*)v ? NULL : (jl_value_t*)mapped;
    }
    if (jl_is_string(v)) {
        jl_value_t *mapped = jl_maybe_map_build_path_jlstr(v);
        return mapped == v ? NULL : mapped;
    }
    if (jl_is_linenode(v)) {
        jl_value_t *file = maybe_map_build_path_in_ast(jl_linenode_file(v), depth + 1);
        if (file == NULL)
            return NULL;
        return jl_new_struct(jl_linenumbernode_type, jl_box_long(jl_linenode_line(v)), file);
    }
    if (jl_is_quotenode(v)) {
        jl_value_t *val = maybe_map_build_path_in_ast(jl_quotenode_value(v), depth + 1);
        if (val == NULL)
            return NULL;
        return jl_new_struct(jl_quotenode_type, val);
    }
    if (jl_is_expr(v)) {
        jl_expr_t *e = (jl_expr_t*)v;
        size_t i, n = jl_expr_nargs(e);
        jl_expr_t *ne = NULL;
        for (i = 0; i < n; i++) {
            jl_value_t *mapped = maybe_map_build_path_in_ast(jl_exprarg(e, i), depth + 1);
            if (mapped) {
                if (ne == NULL) {
                    ne = jl_exprn(e->head, n);
                    for (size_t j = 0; j < n; j++)
                        jl_exprargset(ne, j, jl_exprarg(e, j));
                }
                jl_exprargset(ne, i, mapped);
            }
        }
        return (jl_value_t*)ne;
    }
    return NULL;
}

JL_DLLEXPORT jl_value_t *jl_map_build_path(jl_value_t *str) JL_CANSAFEPOINT
{
    JL_TYPECHK(map_build_path, string, str);
    if (!prefix_map_active())
        return str;
    return jl_maybe_map_build_path_jlstr(str);
}

// test entry point: parses `map` fresh on every call (the env value is cached per-process)
JL_DLLEXPORT jl_value_t *jl_test_map_build_path(jl_value_t *map, jl_value_t *path) JL_CANSAFEPOINT
{
    JL_TYPECHK(test_map_build_path, string, map);
    JL_TYPECHK(test_map_build_path, string, path);
    prefix_map_pair_t *pairs = NULL;
    size_t npairs = 0;
    if (!prefix_map_parse(jl_string_data(map), &pairs, &npairs))
        jl_error("malformed BUILD_PATH_PREFIX_MAP value");
    size_t outlen;
    char *mapped = prefix_map_apply(pairs, npairs, jl_string_data(path), jl_string_len(path), &outlen);
    prefix_map_free(pairs, npairs);
    if (!mapped)
        return path;
    jl_value_t *r = jl_pchar_to_string(mapped, outlen);
    free(mapped);
    return r;
}

// Decide if `t` must be new, because it points to something new.
// If it is new, the object (in particular, the super field) might not be entirely
// valid for the cache, so we want to finish transforming it before attempting
// to look in the cache for it
int must_be_new_dt(jl_value_t *t, htable_t *news, char *image_base, size_t sizeof_sysimg)
{
    //if (jl_object_in_image(t))
    //    return 0; // fast-path for rejection
    assert(ptrhash_get(news, (void*)t) != (void*)t);
    if (ptrhash_has(news, (void*)t) || ptrhash_has(news, (void*)jl_typeof(t)))
        return 1;
    if (!(image_base < (char*)t && (char*)t <= image_base + sizeof_sysimg))
        return 0; // fast-path for rejection
    if (jl_is_uniontype(t)) {
        jl_uniontype_t *u = (jl_uniontype_t*)t;
        return must_be_new_dt(u->a, news, image_base, sizeof_sysimg) ||
               must_be_new_dt(u->b, news, image_base, sizeof_sysimg);
    }
    else if (jl_is_unionall(t)) {
        jl_unionall_t *ua = (jl_unionall_t*)t;
        return must_be_new_dt((jl_value_t*)ua->var, news, image_base, sizeof_sysimg) ||
               must_be_new_dt(ua->body, news, image_base, sizeof_sysimg);
    }
    else if (jl_is_typevar(t)) {
        jl_tvar_t *tv = (jl_tvar_t*)t;
        return must_be_new_dt(tv->lb, news, image_base, sizeof_sysimg) ||
               must_be_new_dt(tv->ub, news, image_base, sizeof_sysimg);
    }
    else if (jl_is_some_Type(t)) {
        return must_be_new_dt(jl_some_Type_T(t), news, image_base, sizeof_sysimg);
    }
    else if (jl_is_vararg(t)) {
        jl_vararg_t *tv = (jl_vararg_t*)t;
        if (tv->T && must_be_new_dt(tv->T, news, image_base, sizeof_sysimg))
            return 1;
        if (tv->N && must_be_new_dt(tv->N, news, image_base, sizeof_sysimg))
            return 1;
    }
    else if (jl_is_datatype(t)) {
        jl_datatype_t *dt = (jl_datatype_t*)t;
        assert(jl_astaggedvalue(dt->name)->bits.in_image && "type_in_worklist mistake?");
        jl_datatype_t *super = dt->super;
        // fast-path: check if super is in news, since then we must be new also
        // (it is also possible that super is indeterminate or NULL right now,
        // waiting for `t` to be resolved, then will be determined later as
        // soon as possible afterwards).
        while (super != NULL && super != jl_any_type) {
            if (ptrhash_has(news, (void*)super))
                return 1;
            if (!(image_base < (char*)super && (char*)super <= image_base + sizeof_sysimg))
               break; // the rest must all be non-new
            // otherwise super might be something that was not cached even though a later supertype might be
            // for example while handling `Type{Mask{4, U} where U}`, if we have `Mask{4, U} <: AbstractSIMDVector{4}`
            super = super->super;
        }
        jl_svec_t *tt = dt->parameters;
        size_t i, l = jl_svec_len(tt);
        for (i = 0; i < l; i++)
            if (must_be_new_dt(jl_tparam(dt, i), news, image_base, sizeof_sysimg))
                return 1;
    }
    else {
        return must_be_new_dt(jl_typeof(t), news, image_base, sizeof_sysimg);
    }
    return 0;
}

static uint64_t jl_worklist_key(jl_array_t *worklist) JL_NOTSAFEPOINT
{
    assert(jl_is_array(worklist));
    size_t len = jl_array_nrows(worklist);
    if (len > 0) {
        jl_module_t *topmod = (jl_module_t*)jl_array_ptr_ref(worklist, len-1);
        assert(jl_is_module(topmod));
        return topmod->build_id.lo;
    }
    return 0;
}

static jl_array_t *newly_inferred JL_GLOBALLY_ROOTED /*FIXME*/;
// Mutex for newly_inferred
jl_mutex_t newly_inferred_mutex;
extern jl_mutex_t world_counter_lock;
static _Atomic(uint8_t) jl_tag_newly_inferred_enabled = 0;

/**
 * @brief Enable tagging of all newly inferred CodeInstances.
 */
JL_DLLEXPORT void jl_tag_newly_inferred_enable(void)
{
    jl_atomic_fetch_add(&jl_tag_newly_inferred_enabled, 1);  // FIXME overflow?
}
/**
 * @brief Disable tagging of all newly inferred CodeInstances.
 */
JL_DLLEXPORT void jl_tag_newly_inferred_disable(void)
{
    jl_atomic_fetch_add(&jl_tag_newly_inferred_enabled, -1);  // FIXME underflow?
}


// Register array of newly-inferred MethodInstances
// This gets called as the first step of Base.include_package_for_output
JL_DLLEXPORT void jl_set_newly_inferred(jl_value_t* _newly_inferred)
{
    assert(_newly_inferred == NULL || _newly_inferred == jl_nothing || jl_is_array_any(_newly_inferred));
    if (_newly_inferred == jl_nothing)
        _newly_inferred = NULL;
    JL_LOCK(&newly_inferred_mutex);
    newly_inferred = (jl_array_t*) _newly_inferred;
    JL_UNLOCK(&newly_inferred_mutex);
}

// Null `CodeInstance.inferred` for native-owned CIs where it is still a raw
// `CodeInfo` left by `jl_precompile_keep_ir`. These are non-inlineable methods
// whose IR is otherwise discarded, retained only long enough for
// `jl_create_native` to reuse via the `typeinf_ext` short-circuit. A
// CodeInstance cached racing the end of the include phase can escape this walk;
// see jl_queue_for_serialization.
JL_DLLEXPORT void jl_finalize_precompile_inferred(int8_t cleanup_keep_ir)
{
    jl_set_precompile_keep_ir(0);
    if (!cleanup_keep_ir || newly_inferred == NULL)
        return;
    size_t n = jl_array_nrows(newly_inferred);
    for (size_t i = 0; i < n; i++) {
        jl_code_instance_t *ci = (jl_code_instance_t*)jl_array_ptr_ref(newly_inferred, i);
        if (ci == NULL)
            continue;
        if (ci->owner != jl_nothing)
            continue; // foreign interpreters own their cached IR
        jl_value_t *inferred = jl_atomic_load_relaxed(&ci->inferred);
        if (inferred == NULL || !jl_is_code_info(inferred))
            continue;
        jl_method_instance_t *mi = jl_get_ci_mi(ci);
        if (!jl_is_method(mi->def.value))
            continue; // toplevel code retains its inferred IR
        if (mi->def.method->source == NULL)
            continue; // optimized opaque closures can't reconstruct their IR
        jl_atomic_store_release(&ci->inferred, jl_nothing);
    }
}

static jl_array_t *queue_used(jl_array_t *list, jl_query_cache *query_cache) JL_CANSAFEPOINT;

JL_DLLEXPORT jl_array_t* jl_compute_new_used_ci(void) JL_CANSAFEPOINT
{
    if (newly_inferred == NULL)
        return jl_alloc_vec_any(0);
    jl_query_cache query_cache;
    init_query_cache(&query_cache);
    jl_array_t *new_used = queue_used(newly_inferred, &query_cache);
    destroy_query_cache(&query_cache);
    return new_used;
}

JL_DLLEXPORT void jl_push_newly_inferred(jl_value_t* ci)
{
    if (!newly_inferred)
        return;
    if (jl_is_code_instance(ci)) {
        uint8_t tag_newly_inferred = jl_atomic_load_relaxed(&jl_tag_newly_inferred_enabled);
        if (tag_newly_inferred) {
            jl_method_instance_t *mi = jl_get_ci_mi((jl_code_instance_t*)ci);
            uint8_t miflags = jl_atomic_load_relaxed(&mi->flags);
            jl_atomic_store_relaxed(&mi->flags, miflags | JL_MI_FLAGS_MASK_PRECOMPILED);
        }
    }
    JL_LOCK(&newly_inferred_mutex);
    // re-check under the lock: a concurrent jl_set_newly_inferred may have
    // cleared or replaced the array since the unlocked fast-path check above
    jl_array_t *arr = newly_inferred;
    if (arr != NULL) {
        size_t end = jl_array_nrows(arr);
        jl_array_grow_end(arr, 1);
        jl_array_ptr_set(arr, end, ci);
    }
    JL_UNLOCK(&newly_inferred_mutex);
}


static jl_array_t *inference_entrance_backtraces JL_GLOBALLY_ROOTED /*FIXME*/ = NULL;
// Mutex for inference_entrance_backtraces
jl_mutex_t inference_entrance_backtraces_mutex;

// Register array of inference entrance backtraces
JL_DLLEXPORT void jl_set_inference_entrance_backtraces(jl_value_t* _inference_entrance_backtraces)
{
    assert(_inference_entrance_backtraces == NULL || _inference_entrance_backtraces == jl_nothing || jl_is_array(_inference_entrance_backtraces));
    if (_inference_entrance_backtraces == jl_nothing)
        _inference_entrance_backtraces = NULL;
    JL_LOCK(&inference_entrance_backtraces_mutex);
    inference_entrance_backtraces = (jl_array_t*) _inference_entrance_backtraces;
    JL_UNLOCK(&inference_entrance_backtraces_mutex);
}


JL_DLLEXPORT void jl_push_inference_entrance_backtraces(jl_value_t* ci)
{
    JL_LOCK(&inference_entrance_backtraces_mutex);
    if (inference_entrance_backtraces == NULL) {
        JL_UNLOCK(&inference_entrance_backtraces_mutex);
        return;
    }
    jl_value_t* backtrace = jl_backtrace_from_here(0, 1);
    size_t end = jl_array_nrows(inference_entrance_backtraces);
    jl_array_grow_end(inference_entrance_backtraces, 2);
    jl_array_ptr_set(inference_entrance_backtraces, end, ci);
    jl_array_ptr_set(inference_entrance_backtraces, end + 1, backtrace);
    JL_UNLOCK(&inference_entrance_backtraces_mutex);
}

// compute whether a type references something internal to worklist
// and thus could not have existed before deserialize
// and thus does not need delayed unique-ing
static int type_in_worklist(jl_value_t *v, jl_query_cache *cache) JL_NOTSAFEPOINT
{
    if (jl_object_in_image(v))
        return 0; // fast-path for rejection

    void *cached = HT_NOTFOUND;
    if (cache != NULL)
        cached = ptrhash_get(&cache->type_in_worklist, v);

    // fast-path for memoized results
    if (cached != HT_NOTFOUND)
        return cached == v;

    int result = 0;
    if (jl_is_uniontype(v)) {
        jl_uniontype_t *u = (jl_uniontype_t*)v;
        result = type_in_worklist(u->a, cache) ||
                 type_in_worklist(u->b, cache);
    }
    else if (jl_is_unionall(v)) {
        jl_unionall_t *ua = (jl_unionall_t*)v;
        result = type_in_worklist((jl_value_t*)ua->var, cache) ||
                 type_in_worklist(ua->body, cache);
    }
    else if (jl_is_typevar(v)) {
        jl_tvar_t *tv = (jl_tvar_t*)v;
        result = type_in_worklist(tv->lb, cache) ||
                 type_in_worklist(tv->ub, cache);
    }
    else if (jl_is_vararg(v)) {
        jl_vararg_t *tv = (jl_vararg_t*)v;
        result = ((tv->T && type_in_worklist(tv->T, cache)) ||
                  (tv->N && type_in_worklist(tv->N, cache)));
    }
    else if (jl_is_datatype(v)) {
        jl_datatype_t *dt = (jl_datatype_t*)v;
        if (!jl_object_in_image((jl_value_t*)dt->name)) {
            result = 1;
        }
        else {
            jl_svec_t *tt = dt->parameters;
            size_t i, l = jl_svec_len(tt);
            for (i = 0; i < l; i++) {
                if (type_in_worklist(jl_tparam(dt, i), cache)) {
                    result = 1;
                    break;
                }
            }
        }
    }
    else {
        return type_in_worklist(jl_typeof(v), cache);
    }

    // Memoize result
    if (cache != NULL)
        ptrhash_put(&cache->type_in_worklist, (void*)v, result ? (void*)v : NULL);

    return result;
}

// Stack frame for iterative has_backedge_to_worklist implementation
enum backedge_state {
    STATE_VISITING,                 // Initial visit, setup phase
    STATE_PROCESSING_EDGES,         // Processing backedges loop
    STATE_FINISHING                 // Cleanup and result propagation
};

typedef struct {
    jl_method_instance_t *mi;           // Current method instance
    size_t edge_index;                  // Current position in backedges array
    size_t backedges_len;               // Total backedges count
    jl_array_t *backedges;              // Backedges array
    int depth;                          // Stack depth when this frame was created
    int cycle;                          // Cycle depth tracking
    int found;                          // Result found flag
    int child_result;                   // Result from child recursive call
    enum backedge_state state;
} backedge_stack_frame_t;

// When we infer external method instances, ensure they link back to the
// package. Otherwise they might be, e.g., for external macros.
// Implements Tarjan's SCC (strongly connected components) algorithm, simplified to remove the count variable
static int has_backedge_to_worklist(jl_method_instance_t *mi, htable_t *visited, arraylist_t *stack, jl_query_cache *query_cache)
{
    // Use arraylist_t for explicit stack of processing frames
    arraylist_t frame_stack;
    arraylist_new(&frame_stack, 0);

    // Push initial frame
    backedge_stack_frame_t initial_frame = {
        .mi = mi,
        .edge_index = 0,
        .backedges_len = 0,
        .backedges = NULL,
        .depth = 0,
        .cycle = 0,
        .found = 0,
        .child_result = 0,
        .state = STATE_VISITING
    };
    arraylist_push(&frame_stack, memcpy(malloc(sizeof(backedge_stack_frame_t)), &initial_frame, sizeof(backedge_stack_frame_t)));

    int final_result = 0;
    while (1) {
        backedge_stack_frame_t *current = (backedge_stack_frame_t*)frame_stack.items[frame_stack.len - 1];
        JL_GC_PROMISE_ROOTED(current->mi);
        JL_GC_PROMISE_ROOTED(current->backedges);

        switch (current->state) {
            case STATE_VISITING: {
                jl_module_t *mod = current->mi->def.module;
                if (jl_is_method(mod))
                    mod = ((jl_method_t*)mod)->module;
                assert(jl_is_module(mod));
                uint8_t is_precompiled = jl_atomic_load_relaxed(&current->mi->flags) & JL_MI_FLAGS_MASK_PRECOMPILED;

                if (is_precompiled || !jl_object_in_image((jl_value_t*)mod) || type_in_worklist(current->mi->specTypes, query_cache)) {
                    if (frame_stack.len > 1) {
                        final_result = 1;
                        goto propagate_to_parent;
                    }
                    current->found = 1;
                    // Continue to setup below, then go to finishing
                }
                else if (!current->mi->backedges) {
                    if (frame_stack.len > 1) {
                        final_result = 0;
                        goto propagate_to_parent;
                    }
                    current->found = 0;
                    // Setup minimal state for cleanup, skip backedges processing
                    arraylist_push(stack, (void*)current->mi);
                    current->depth = stack->len;
                    void **bp = ptrhash_bp(visited, current->mi);
                    *bp = (void*)((char*)HT_NOTFOUND + 4 + current->depth);
                    current->cycle = current->depth;
                    current->state = STATE_FINISHING;
                    break;
                }

                void **bp = ptrhash_bp(visited, current->mi);
                // HT_NOTFOUND: not yet analyzed
                // HT_NOTFOUND + 1: no link back
                // HT_NOTFOUND + 2: does link back
                // HT_NOTFOUND + 3: does link back, and included in new_ext_cis already
                // HT_NOTFOUND + 4 + depth: in-progress
                int found = (char*)*bp - (char*)HT_NOTFOUND;
                if (found) {
                    if (frame_stack.len > 1) {
                        final_result = found - 1;
                        goto propagate_to_parent;
                    }
                    current->found = found - 1;
                }

                // Setup for processing
                arraylist_push(stack, (void*)current->mi);
                current->depth = stack->len;
                *bp = (void*)((char*)HT_NOTFOUND + 4 + current->depth); // preliminarily mark as in-progress
                current->backedges = jl_mi_get_backedges(current->mi);
                current->backedges_len = current->backedges ? jl_array_nrows(current->backedges) : 0;
                current->cycle = current->depth;
                current->edge_index = 0;
                // Don't reset current->found if it was already set by early termination logic above
                if (current->found == 0) {
                    current->state = STATE_PROCESSING_EDGES;
                }
                else {
                    // Early termination case - skip processing and go straight to finishing
                    current->state = STATE_FINISHING;
                }
                break;
            }

            case STATE_PROCESSING_EDGES: {
                // If we have a child result to process, handle it first
                if (current->child_result != 0) {
                    if (current->child_result == 1 || current->child_result == 2) {
                        // found what we were looking for, so terminate early
                        current->found = 1;
                        current->state = STATE_FINISHING;
                        break;
                    }
                    else if (current->child_result >= 3 && current->child_result - 3 < current->cycle) {
                        // record the cycle will resolve at depth "cycle"
                        current->cycle = current->child_result - 3;
                        assert(current->cycle);
                    }
                    current->child_result = 0; // Clear after processing
                }

                // Process backedges iteratively
                while (current->edge_index < current->backedges_len && current->backedges) {
                    jl_code_instance_t *be;
                    current->edge_index = get_next_edge(current->backedges, current->edge_index, NULL, &be);
                    if (!be)
                        continue;
                    jl_method_instance_t *child_mi = jl_get_ci_mi(be);

                    // Check if we need to recurse (push new frame) or handle result
                    jl_module_t *child_mod = child_mi->def.module;
                    if (jl_is_method(child_mod))
                        child_mod = ((jl_method_t*)child_mod)->module;
                    assert(jl_is_module(child_mod));
                    uint8_t child_is_precompiled = jl_atomic_load_relaxed(&child_mi->flags) & JL_MI_FLAGS_MASK_PRECOMPILED;

                    // Early termination check for child
                    if (child_is_precompiled || !jl_object_in_image((jl_value_t*)child_mod) || type_in_worklist(child_mi->specTypes, query_cache)) {
                        // found what we were looking for, so terminate early
                        current->found = 1;
                        break;
                    }

                    if (!child_mi->backedges) {
                        // This child returns 0, continue with next edge
                        continue;
                    }

                    void *child_bpval = ptrhash_get(visited, child_mi);
                    int child_found = (char*)child_bpval - (char*)HT_NOTFOUND;
                    if (child_found) {
                        int child_result = child_found - 1;
                        if (child_result == 1 || child_result == 2) {
                            // found what we were looking for, so terminate early
                            current->found = 1;
                            break;
                        }
                        else if (child_result >= 3 && child_result - 3 < current->cycle) {
                            // record the cycle will resolve at depth "cycle"
                            current->cycle = child_result - 3;
                            assert(current->cycle);
                        }
                    }
                    else {
                        // Need to process child - push new frame and pause current processing
                        backedge_stack_frame_t child_frame = {
                            .mi = child_mi,
                            .edge_index = 0,
                            .backedges_len = 0,
                            .backedges = NULL,
                            .depth = 0,
                            .cycle = 0,
                            .found = 0,
                            .child_result = 0,
                            .state = STATE_VISITING
                        };
                        arraylist_push(&frame_stack, memcpy(malloc(sizeof(backedge_stack_frame_t)), &child_frame, sizeof(backedge_stack_frame_t)));
                        goto continue_main_loop; // Resume processing after child completes
                    }
                }

                current->state = STATE_FINISHING;
                break;
            }

            case STATE_FINISHING: {
                if (!current->found && current->cycle != current->depth) {
                    final_result = current->cycle + 3;
                    goto propagate_to_parent;
                }

                // If we are the top of the current cycle, now mark all other parts of
                // our cycle with what we found.
                // Or if we found a backedge, also mark all of the other parts of the
                // cycle as also having a backedge.
                while (stack->len >= current->depth) {
                    void *mi_ptr = arraylist_pop(stack);
                    void **bp = ptrhash_bp(visited, mi_ptr);
                    assert((char*)*bp - (char*)HT_NOTFOUND == 5 + stack->len);
                    *bp = (void*)((char*)HT_NOTFOUND + 1 + current->found);
                }

                final_result = current->found;
                goto propagate_to_parent;
            }
        }

        continue_main_loop:
            continue;

        propagate_to_parent:
            // Propagate result to parent
            free(arraylist_pop(&frame_stack));
            if (frame_stack.len == 0)
                break;
            backedge_stack_frame_t *parent = (backedge_stack_frame_t*)frame_stack.items[frame_stack.len - 1];
            parent->child_result = final_result;
    }
    // Cleanup remaining frames
    assert(frame_stack.len == 0);
    arraylist_free(&frame_stack);
    return final_result;
}

// Given the list of CodeInstances or MethodInstances that were inferred during
// the build, select those that are (1) internal or are inferred to be called
// from the worklist or explicitly added by a `precompile` statement. This will
// be used as roots for exploring what to include in the final image.
static jl_array_t *queue_used(jl_array_t *list, jl_query_cache *query_cache)
{
    if (list == NULL)
        return NULL;
    size_t i;
    htable_t visited;
    arraylist_t stack;
    assert(jl_is_array(list));
    size_t n0 = jl_array_nrows(list);
    htable_new(&visited, n0);
    arraylist_new(&stack, 0);
    jl_array_t *new_used = jl_alloc_vec_any(0);
    JL_GC_PUSH1(&new_used);
    for (i = n0; i-- > 0; ) {
        jl_value_t *v = jl_array_ptr_ref(list, i);
        if (jl_is_code_instance(v)) {
            jl_code_instance_t *ci = (jl_code_instance_t*)v;
            jl_method_instance_t *mi = jl_get_ci_mi(ci);
            jl_method_t *m = mi->def.method;
            int dispatch_status = jl_atomic_load_relaxed(&m->dispatch_status);
            if (!(dispatch_status & METHOD_SIG_LATEST_WHICH))
                continue; // ignore replaced methods
            if (jl_atomic_load_relaxed(&ci->inferred) && jl_is_method(m)) {
                int found = jl_object_in_image((jl_value_t*)m->module) ? has_backedge_to_worklist(mi, &visited, &stack, query_cache) : 1;
                assert(found == 0 || found == 1 || found == 2);
                assert(stack.len == 0);
                if (found == 1) {
                    jl_array_ptr_1d_push(new_used, (jl_value_t*)ci);
                }
            }
        }
        else if (jl_is_method_instance(v)) {
            jl_array_ptr_1d_push(new_used, v);
        }
    }
    htable_free(&visited);
    arraylist_free(&stack);
    JL_GC_POP();
    // reverse new_used
    n0 = jl_array_nrows(new_used);
    jl_value_t **news = jl_array_data(new_used, jl_value_t*);
    for (i = 0; i < n0 / 2; i++) {
        jl_value_t *temp = news[i];
        news[i] = news[n0 - i - 1];
        news[n0 - i - 1] = temp;
    }
    return new_used;
}

// For every method:
// - if the method is owned by a worklist module, add it to the list of things to be
//   verified on reloading
// - if the method is extext, record that it needs to be reinserted later in the method table
static int jl_collect_methcache_from_mod(jl_typemap_entry_t *ml, void *closure) JL_CANSAFEPOINT
{
    jl_array_t *s = (jl_array_t*)closure;
    jl_method_t *m = ml->func.method;
    if (!jl_object_in_image((jl_value_t*)m->module))
        jl_array_ptr_1d_push(s, (jl_value_t*)m); // extext
    return 1;
}

// Collect every currently-valid method of a worklist-owned method table, whose
// contents are dropped from the image (see jl_prune_internal_mtable)
static int jl_collect_methcache_internal(jl_typemap_entry_t *ml, void *closure) JL_CANSAFEPOINT
{
    jl_array_t *s = (jl_array_t*)closure;
    if (jl_atomic_load_relaxed(&ml->max_world) == ~(size_t)0)
        jl_array_ptr_1d_push(s, (jl_value_t*)ml->func.method); // extext
    return 1;
}

static int jl_collect_methtable_from_mod(jl_methtable_t *mt, void *env) JL_CANSAFEPOINT
{
    // Custom method tables owned by the worklist are serialized without their
    // contents (jl_prune_internal_mtable), so all of their methods are treated
    // as extending an "external" table, to be re-added and re-activated on load.
    jl_typemap_visitor(jl_atomic_load_relaxed(&mt->defs),
                       jl_object_in_image((jl_value_t*)mt) ? jl_collect_methcache_from_mod
                                                           : jl_collect_methcache_internal,
                       env);
    return 1;
}

// Collect methods of external functions defined by modules in the worklist
// "extext" = "extending external"
// Also collect relevant backedges
static void jl_collect_extext_methods(jl_array_t *s, jl_array_t *mod_array) JL_CANSAFEPOINT
{
    jl_foreach_reachable_mtable(jl_collect_methtable_from_mod, mod_array, s);
}

// Headers

// serialize information about all loaded modules
static void write_mod_list(ios_t *s, jl_array_t *a)
{
    size_t i;
    size_t len = jl_array_nrows(a);
    for (i = 0; i < len; i++) {
        jl_module_t *m = (jl_module_t*)jl_array_ptr_ref(a, i);
        assert(jl_is_module(m));
        if (jl_object_in_image((jl_value_t*)m)) {
            const char *modname = jl_symbol_name(m->name);
            size_t l = strlen(modname);
            write_int32(s, l);
            ios_write(s, modname, l);
            write_uint64(s, m->uuid.hi);
            write_uint64(s, m->uuid.lo);
            write_uint64(s, m->build_id.hi);
            write_uint64(s, m->build_id.lo);
        }
    }
    write_int32(s, 0);
}

#define OPT_LEVEL 6
#define DEBUG_LEVEL 1

JL_DLLEXPORT uint8_t jl_cache_flags(void)
{
    // OOICCDDP
    uint8_t flags = 0;
    flags |= (jl_options.use_pkgimages & 1); // 0-bit
    flags |= (jl_options.debug_level & 3) << DEBUG_LEVEL; // 1-2 bit
    flags |= (jl_options.check_bounds & 3) << 3; // 3-4 bit
    flags |= (jl_options.can_inline & 1) << 5; // 5-bit
    flags |= (jl_options.opt_level & 3) << OPT_LEVEL; // 6-7 bit
    return flags;
}


JL_DLLEXPORT uint8_t jl_match_cache_flags(uint8_t requested_flags, uint8_t actual_flags)
{
    uint8_t supports_pkgimage = (requested_flags & 1);
    uint8_t is_pkgimage = (actual_flags & 1);

    // For .ji packages ignore other flags
    if (!supports_pkgimage && !is_pkgimage) {
        return 1;
    }

    // If package images are optional, ignore that bit (it will be unset in requested_flags)
    if (jl_options.use_pkgimages == JL_OPTIONS_USE_PKGIMAGES_EXISTING) {
        actual_flags &= ~1;
    }

    // 2. Check all flags, except opt level and debug level must be exact
    uint8_t mask = (~(3u << OPT_LEVEL) & ~(3u << DEBUG_LEVEL)) & 0x7f;
    if ((actual_flags & mask) != (requested_flags & mask))
        return 0;
    // 3. allow for higher optimization and debug level flags in cache to minimize required compile option combinations
    return ((actual_flags >> OPT_LEVEL) & 3) >= ((requested_flags >> OPT_LEVEL) & 3) &&
           ((actual_flags >> DEBUG_LEVEL) & 3) >= ((requested_flags >> DEBUG_LEVEL) & 3);
}

JL_DLLEXPORT uint8_t jl_match_cache_flags_current(uint8_t flags)
{
    return jl_match_cache_flags(jl_cache_flags(), flags);
}

// return char* from String field in Base.GIT_VERSION_INFO
static const char *git_info_string(const char *fld) JL_CANSAFEPOINT
{
    static jl_value_t *GIT_VERSION_INFO = NULL;
    if (!GIT_VERSION_INFO)
        GIT_VERSION_INFO = jl_get_global(jl_base_module, jl_symbol("GIT_VERSION_INFO"));
    jl_value_t *f = jl_get_field(GIT_VERSION_INFO, fld);
    assert(jl_is_string(f));
    return jl_string_data(f);
}

static const char *jl_git_branch(void) JL_CANSAFEPOINT
{
    static const char *branch = NULL;
    if (!branch) branch = git_info_string("branch");
    return branch;
}

static const char *jl_git_commit(void) JL_CANSAFEPOINT
{
    static const char *commit = NULL;
    if (!commit) commit = git_info_string("commit");
    return commit;
}


// "magic" string and version header of .ji file
static const int JI_FORMAT_VERSION = 15;
static const char JI_MAGIC[] = "\373jli\r\n\032\n"; // based on PNG signature
static const uint16_t BOM = 0xFEFF; // byte-order marker

// This .ji is for an incremental image and not a system image.
static const uint32_t JI_FLAG_PKGIMAGE = 1 << 0;
// This .ji must be loaded from the associated native image (.so/.dylib/.dll).
static const uint32_t JI_FLAG_SPLIT = 1 << 1;

static int64_t write_header(ios_t *s, uint32_t flags) JL_CANSAFEPOINT
{
    ios_write(s, JI_MAGIC, strlen(JI_MAGIC));
    write_uint16(s, JI_FORMAT_VERSION);
    ios_write(s, (char *) &BOM, 2);
    write_uint8(s, sizeof(void*));
    ios_write(s, JL_BUILD_UNAME, strlen(JL_BUILD_UNAME)+1);
    ios_write(s, JL_BUILD_ARCH, strlen(JL_BUILD_ARCH)+1);
    ios_write(s, JULIA_VERSION_STRING, strlen(JULIA_VERSION_STRING)+1);
    const char *gc_abi = jl_gc_image_abi();
    ios_write(s, gc_abi, strlen(gc_abi)+1);
    write_uint32(s, flags);
    if (flags & JI_FLAG_PKGIMAGE) {
        const char *branch = jl_git_branch(), *commit = jl_git_commit();
        ios_write(s, branch, strlen(branch)+1);
        ios_write(s, commit, strlen(commit)+1);
    }
    int64_t checksumpos = ios_pos(s);
    write_uint32(s, 0); // eventually will hold checksum for the content portion of this (build_id.hi)
    write_uint64(s, 0); // eventually will hold datastartpos
    write_uint64(s, 0); // eventually will hold dataendpos
    return checksumpos;
}

static int is_serialization_root_module(jl_module_t *mod) JL_NOTSAFEPOINT
{
    return mod->parent == jl_main_module || mod->parent == jl_base_module || mod->parent == mod;
}

// serialize information about the result of deserializing this file
static void write_worklist_for_header(ios_t *s, jl_array_t *worklist)
{
    int i, l = jl_array_nrows(worklist);
    for (i = 0; i < l; i++) {
        jl_module_t *workmod = (jl_module_t*)jl_array_ptr_ref(worklist, i);
        if (is_serialization_root_module(workmod)) {
            size_t l = strlen(jl_symbol_name(workmod->name));
            write_int32(s, l);
            ios_write(s, jl_symbol_name(workmod->name), l);
            write_uint64(s, workmod->uuid.hi);
            write_uint64(s, workmod->uuid.lo);
            write_uint64(s, workmod->build_id.lo);
        }
    }
    write_int32(s, 0);
}

static void write_module_path(ios_t *s, jl_module_t *depmod) JL_NOTSAFEPOINT
{
    if (is_serialization_root_module(depmod))
        return;
    const char *mname = jl_symbol_name(depmod->name);
    size_t slen = strlen(mname);
    write_module_path(s, depmod->parent);
    write_int32(s, slen);
    ios_write(s, mname, slen);
}

// Cache file header
// Serialize the global Base._require_dependencies array of pathnames that
// are include dependencies. Also write Preferences and return
// the location of the srctext "pointer" in the header index.
static int64_t write_dependency_list(ios_t *s, jl_array_t* worklist, jl_array_t **udepsp) JL_CANSAFEPOINT
{
    int64_t initial_pos = 0;
    int64_t pos = 0;
    jl_task_t *ct = jl_current_task;
    size_t last_age = ct->world_age;
    ct->world_age = jl_atomic_load_acquire(&jl_world_counter);
    jl_value_t *depots = NULL, *prefs_blob = NULL;
    jl_value_t *unique_func = NULL;
    jl_value_t *replace_depot_func = NULL;
    jl_value_t *normalize_depots_func = NULL;
    jl_value_t *deppath = NULL;
    JL_GC_PUSH6(&depots, &prefs_blob, &unique_func, &replace_depot_func, &normalize_depots_func, &deppath);

    jl_array_t *udeps = (jl_array_t*)jl_get_global_value(jl_base_module, jl_symbol("_require_dependencies"), ct->world_age);
    *udepsp = udeps;

    // unique(udeps) to eliminate duplicates while preserving order:
    // we preserve order so that the topmost included .jl file comes first
    if (udeps) {
        unique_func = jl_eval_global_var(jl_base_module, jl_symbol("unique"), ct->world_age);
        jl_value_t *uniqargs[2] = {unique_func, (jl_value_t*)udeps};
        udeps = (jl_array_t*)jl_apply(uniqargs, 2);
        *udepsp = udeps;
        JL_TYPECHK(write_dependency_list, array_any, (jl_value_t*)udeps);
    }

    replace_depot_func = jl_get_global_value(jl_base_module, jl_symbol("replace_depot_path"), ct->world_age);
    normalize_depots_func = jl_eval_global_var(jl_base_module, jl_symbol("normalize_depots_for_relocation"), ct->world_age);

    depots = jl_apply(&normalize_depots_func, 1);

    jl_datatype_t *deptuple_p[5] = {jl_module_type, jl_string_type, jl_uint64_type, jl_uint32_type, jl_float64_type};
    jl_value_t *jl_deptuple_type = jl_apply_tuple_type_v((jl_value_t**)deptuple_p, 5);
    JL_GC_PROMISE_ROOTED(jl_deptuple_type);
#define jl_is_deptuple(v) (jl_typeis((v), jl_deptuple_type))

    // write a placeholder for total size so that we can quickly seek past all of the
    // dependencies if we don't need them
    initial_pos = ios_pos(s);
    write_uint64(s, 0);
    size_t i, l = udeps ? jl_array_nrows(udeps) : 0;
    for (i = 0; i < l; i++) {
        jl_value_t *deptuple = jl_array_ptr_ref(udeps, i);
        JL_TYPECHK(write_dependency_list, deptuple, deptuple);
        deppath = jl_fieldref_noalloc(deptuple, 1);

        if (replace_depot_func) {
            jl_value_t *replace_depot_args[3];
            replace_depot_args[0] = replace_depot_func;
            replace_depot_args[1] = deppath;
            replace_depot_args[2] = depots;
            deppath = (jl_value_t*)jl_apply(replace_depot_args, 3);
            JL_TYPECHK(write_dependency_list, string, deppath);
        }
        deppath = jl_maybe_map_build_path_jlstr(deppath);

        size_t slen = jl_string_len(deppath);
        write_int32(s, slen);
        ios_write(s, jl_string_data(deppath), slen);
        write_uint64(s, jl_unbox_uint64(jl_fieldref(deptuple, 2)));    // fsize
        write_uint32(s, jl_unbox_uint32(jl_fieldref(deptuple, 3)));    // hash
        write_float64(s, jl_unbox_float64(jl_fieldref(deptuple, 4)));  // mtime
        jl_module_t *depmod = (jl_module_t*)jl_fieldref_noalloc(deptuple, 0);  // evaluating module
        jl_module_t *depmod_top = depmod;
        while (!is_serialization_root_module(depmod_top))
            depmod_top = depmod_top->parent;
        unsigned provides = 0;
        size_t j, lj = jl_array_nrows(worklist);
        for (j = 0; j < lj; j++) {
            jl_module_t *workmod = (jl_module_t*)jl_array_ptr_ref(worklist, j);
            if (is_serialization_root_module(workmod)) {
                ++provides;
                if (workmod == depmod_top) {
                    write_int32(s, provides);
                    write_module_path(s, depmod);
                    break;
                }
            }
        }
        write_int32(s, 0);
    }
    write_int32(s, 0); // terminator, for ease of reading

    // Serialize any compile-time dependencies on preferences.
    assert(jl_base_module);
    jl_value_t *get_preferences_blob = jl_eval_global_var(
        jl_base_module, jl_symbol("get_preferences_blob"), ct->world_age);
    assert(get_preferences_blob);
    jl_value_t *args[1] = { get_preferences_blob };
    prefs_blob = jl_apply(args, 1);
    JL_TYPECHK(write_dependency_list, string, prefs_blob);
    ct->world_age = last_age;

    // Write out the serialized preferences "blob"
    size_t slen = jl_string_len(prefs_blob);
    write_int32(s, slen);
    ios_write(s, jl_string_data(prefs_blob), slen);

    JL_GC_POP(); // for depots, prefs_blob
#undef jl_is_deptuple

    // write a dummy file position to indicate the beginning of the source-text
    pos = ios_pos(s);
    ios_seek(s, initial_pos);
    write_uint64(s, pos - initial_pos);
    ios_seek(s, pos);
    write_uint64(s, 0);
    return pos;
}


// Deserialization

// Add methods to external (non-worklist-owned) functions
// mutating external to point at the new methodtable entry instead of the new method
static void jl_add_methods(jl_array_t *external) JL_CANSAFEPOINT
{
    size_t i, l = jl_array_nrows(external);
    for (i = 0; i < l; i++) {
        jl_method_t *meth = (jl_method_t*)jl_array_ptr_ref(external, i);
        assert(jl_is_method(meth));
        assert(!meth->is_for_opaque_closure);
        jl_methtable_t *mt = jl_method_get_table(meth);
        assert((jl_value_t*)mt != jl_nothing);
        jl_typemap_entry_t *entry = jl_method_table_add(mt, meth, NULL);
        jl_array_ptr_set(external, i, entry);
    }
}

extern _Atomic(int) allow_new_worlds;
static void jl_activate_methods(jl_array_t *external, jl_array_t *internal, size_t world, const char *pkgname) JL_CANSAFEPOINT
{
    size_t i, l = jl_array_nrows(internal);
    for (i = 0; i < l; i++) {
        // allow_new_worlds doesn't matter here, since we aren't actually changing anything external
        jl_value_t *obj = jl_array_ptr_ref(internal, i);
        if (jl_typetagis(obj, jl_typemap_entry_type)) {
            jl_typemap_entry_t *entry = (jl_typemap_entry_t*)obj;
            assert(jl_atomic_load_relaxed(&entry->min_world) == ~(size_t)0);
            assert(jl_atomic_load_relaxed(&entry->max_world) == WORLD_AGE_REVALIDATION_SENTINEL);
            jl_atomic_store_release(&entry->min_world, world);
            jl_atomic_store_release(&entry->max_world, ~(size_t)0);
        }
        else if (jl_is_method(obj)) {
            jl_method_t *m = (jl_method_t*)obj;
            assert(jl_atomic_load_relaxed(&m->primary_world) == ~(size_t)0);
            jl_atomic_store_release(&m->primary_world, world);
        }
        else if (jl_is_code_instance(obj)) {
            jl_code_instance_t *ci = (jl_code_instance_t*)obj;
            assert(jl_atomic_load_relaxed(&ci->min_world) == ~(size_t)0);
            assert(jl_atomic_load_relaxed(&ci->max_world) == WORLD_AGE_REVALIDATION_SENTINEL);
            jl_atomic_store_relaxed(&ci->min_world, world);
            // n.b. ci->max_world is not updated until edges are verified
        }
        else {
            abort();
        }
    }
    l = jl_array_nrows(external);
    if (l) {
        if (!jl_atomic_load_relaxed(&allow_new_worlds)) {
            jl_printf(JL_STDERR, "WARNING: Method changes for %s have been disabled via a call to disable_new_worlds.\n", pkgname);
            return;
        }
        for (i = 0; i < l; i++) {
            jl_typemap_entry_t *entry = (jl_typemap_entry_t*)jl_array_ptr_ref(external, i);
            //uint64_t t0 = uv_hrtime();
            jl_method_table_activate(entry);
            //jl_printf(JL_STDERR, "%f ", (double)(uv_hrtime() - t0) / 1e6);
            //jl_static_show(JL_STDERR, entry->func.value);
            //jl_printf(JL_STDERR, "\n");
        }
    }
}

static int jl_copy_roots(jl_array_t *method_roots_list, uint64_t key) JL_CANSAFEPOINT
{
    size_t i, l = jl_array_nrows(method_roots_list);
    int failed = 0;
    for (i = 0; i < l; i+=2) {
        jl_method_t *m = (jl_method_t*)jl_array_ptr_ref(method_roots_list, i);
        jl_array_t *roots = (jl_array_t*)jl_array_ptr_ref(method_roots_list, i+1);
        if (roots) {
            assert(jl_is_array(roots));
            if (m->root_blocks) {
                // check for key collision
                uint64_t *blocks = jl_array_data(m->root_blocks, uint64_t);
                size_t nx2 = jl_array_nrows(m->root_blocks);
                for (size_t i = 0; i < nx2; i+=2) {
                    if (blocks[i] == key) {
                        // found duplicate block
                        failed = -1;
                    }
                }
            }

            jl_append_method_roots(m, key, roots);
        }
    }
    return failed;
}

static jl_value_t *read_verify_mod_list(ios_t *s, jl_array_t *depmods) JL_CANSAFEPOINT
{
    if (!jl_main_module->build_id.lo) {
        return jl_get_exceptionf(jl_errorexception_type,
                "Main module uuid state is invalid for module deserialization.");
    }
    size_t i, l = jl_array_nrows(depmods);
    for (i = 0; ; i++) {
        size_t len = read_int32(s);
        if (len == 0 && i == l)
            return NULL; // success
        if (len == 0 || i == l)
            return jl_get_exceptionf(jl_errorexception_type, "Wrong number of entries in module list.");
        char *name = (char*)alloca(len + 1);
        ios_readall(s, name, len);
        name[len] = '\0';
        jl_uuid_t uuid;
        uuid.hi = read_uint64(s);
        uuid.lo = read_uint64(s);
        jl_uuid_t build_id;
        build_id.hi = read_uint64(s);
        build_id.lo = read_uint64(s);
        jl_sym_t *sym = _jl_symbol(name, len);
        jl_module_t *m = (jl_module_t*)jl_array_ptr_ref(depmods, i);
        if (!m || !jl_is_module(m) || m->uuid.hi != uuid.hi || m->uuid.lo != uuid.lo || m->name != sym ||
                m->build_id.hi != build_id.hi || m->build_id.lo != build_id.lo) {
            return jl_get_exceptionf(jl_errorexception_type,
                "Invalid input in module list: expected %s.", name);
        }
    }
}

static int readstr_verify(ios_t *s, const char *str, int include_null)
{
    size_t i, len = strlen(str) + include_null;
    for (i = 0; i < len; ++i)
        if ((char)read_uint8(s) != str[i])
            return 0;
    return 1;
}

JL_DLLEXPORT int jl_read_verify_header(ios_t *s, uint32_t *flags, uint32_t *checksum, int64_t *dataendpos, int64_t *datastartpos) JL_CANSAFEPOINT
{
    uint16_t bom;
    if (!(readstr_verify(s, JI_MAGIC, 0) &&
          read_uint16(s) == JI_FORMAT_VERSION &&
          ios_read(s, (char *) &bom, 2) == 2 && bom == BOM &&
          read_uint8(s) == sizeof(void*) &&
          readstr_verify(s, JL_BUILD_UNAME, 1) &&
          readstr_verify(s, JL_BUILD_ARCH, 1) &&
          readstr_verify(s, JULIA_VERSION_STRING, 1) &&
          readstr_verify(s, jl_gc_image_abi(), 1)))
        return -1;

    *flags = read_uint32(s);

    if ((*flags & JI_FLAG_PKGIMAGE) &&
        !(readstr_verify(s, jl_git_branch(), 1) && readstr_verify(s, jl_git_commit(), 1)))
        return -1;

    *checksum = read_uint32(s);
    *datastartpos = (int64_t)read_uint64(s);
    *dataendpos = (int64_t)read_uint64(s);

    return 0;
}

// Returns `depmodidxs` where `j = depmodidxs[i]` corresponds to the blob `depmods[j]` in `write_mod_list`
static jl_array_t *image_to_depmodidx(jl_array_t *depmods) JL_CANSAFEPOINT
{
    if (!depmods)
        return NULL;
    assert(jl_array_nrows(depmods) < INT32_MAX && "too many dependencies to serialize");
    size_t lbids = n_linkage_blobs();
    size_t ldeps = jl_array_nrows(depmods);
    jl_array_t *depmodidxs = jl_alloc_array_1d(jl_array_int32_type, lbids);
    int32_t *dmidxs = jl_array_data(depmodidxs, int32_t);
    memset(dmidxs, -1, lbids * sizeof(int32_t));
    dmidxs[0] = 0; // the sysimg can also be found at idx 0, by construction
    for (size_t i = 0, j = 0; i < ldeps; i++) {
        jl_value_t *depmod = jl_array_ptr_ref(depmods, i);
        size_t idx = external_blob_index(depmod);
        if (idx < lbids) { // jl_object_in_image
            j++;
            if (dmidxs[idx] == -1)
                dmidxs[idx] = j;
        }
    }
    return depmodidxs;
}

// Returns `imageidxs` where `j = imageidxs[i]` is the blob corresponding to `depmods[j]`
static jl_array_t *depmod_to_imageidx(jl_array_t *depmods) JL_CANSAFEPOINT
{
    if (!depmods)
        return NULL;
    size_t ldeps = jl_array_nrows(depmods);
    jl_array_t *imageidxs = jl_alloc_array_1d(jl_array_int32_type, ldeps + 1);
    int32_t *imgidxs = jl_array_data(imageidxs, int32_t);
    imgidxs[0] = 0;
    for (size_t i = 0; i < ldeps; i++) {
        jl_value_t *depmod = jl_array_ptr_ref(depmods, i);
        size_t j = external_blob_index(depmod);
        assert(j < INT32_MAX);
        imgidxs[i + 1] = (int32_t)j;
    }
    return imageidxs;
}

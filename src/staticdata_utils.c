// inverse of backedges graph (caller=>callees hash)
jl_array_t *internal_methods JL_GLOBALLY_ROOTED = NULL; // rooted for the duration of our uses of this

static void write_float64(ios_t *s, double x) JL_NOTSAFEPOINT
{
    write_uint64(s, *((uint64_t*)&x));
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
    else if (jl_is_vararg(t)) {
        jl_vararg_t *tv = (jl_vararg_t*)t;
        if (tv->T && must_be_new_dt(tv->T, news, image_base, sizeof_sysimg))
            return 1;
        if (tv->N && must_be_new_dt(tv->N, news, image_base, sizeof_sysimg))
            return 1;
    }
    else if (jl_is_datatype(t)) {
        jl_datatype_t *dt = (jl_datatype_t*)t;
        assert(jl_object_in_image((jl_value_t*)dt->name) && "type_in_worklist mistake?");
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

// Register array of newly-inferred MethodInstances
// This gets called as the first step of Base.include_package_for_output
JL_DLLEXPORT void jl_set_newly_inferred(jl_value_t* _newly_inferred)
{
    assert(_newly_inferred == NULL || _newly_inferred == jl_nothing || jl_is_array(_newly_inferred));
    if (_newly_inferred == jl_nothing)
        _newly_inferred = NULL;
    newly_inferred = (jl_array_t*) _newly_inferred;
}

JL_DLLEXPORT void jl_push_newly_inferred(jl_value_t* ci)
{
    if (!newly_inferred)
        return;
    JL_LOCK(&newly_inferred_mutex);
    size_t end = jl_array_nrows(newly_inferred);
    jl_array_grow_end(newly_inferred, 1);
    jl_array_ptr_set(newly_inferred, end, ci);
    JL_UNLOCK(&newly_inferred_mutex);
}


// compute whether a type references something internal to worklist
// and thus could not have existed before deserialize
// and thus does not need delayed unique-ing
static int type_in_worklist(jl_value_t *v) JL_NOTSAFEPOINT
{
    if (jl_object_in_image(v))
        return 0; // fast-path for rejection
    if (jl_is_uniontype(v)) {
        jl_uniontype_t *u = (jl_uniontype_t*)v;
        return type_in_worklist(u->a) ||
               type_in_worklist(u->b);
    }
    else if (jl_is_unionall(v)) {
        jl_unionall_t *ua = (jl_unionall_t*)v;
        return type_in_worklist((jl_value_t*)ua->var) ||
               type_in_worklist(ua->body);
    }
    else if (jl_is_typevar(v)) {
        jl_tvar_t *tv = (jl_tvar_t*)v;
        return type_in_worklist(tv->lb) ||
               type_in_worklist(tv->ub);
    }
    else if (jl_is_vararg(v)) {
        jl_vararg_t *tv = (jl_vararg_t*)v;
        if (tv->T && type_in_worklist(tv->T))
            return 1;
        if (tv->N && type_in_worklist(tv->N))
            return 1;
    }
    else if (jl_is_datatype(v)) {
        jl_datatype_t *dt = (jl_datatype_t*)v;
        if (!jl_object_in_image((jl_value_t*)dt->name))
            return 1;
        jl_svec_t *tt = dt->parameters;
        size_t i, l = jl_svec_len(tt);
        for (i = 0; i < l; i++)
            if (type_in_worklist(jl_tparam(dt, i)))
                return 1;
    }
    else {
        return type_in_worklist(jl_typeof(v));
    }
    return 0;
}

// When we infer external method instances, ensure they link back to the
// package. Otherwise they might be, e.g., for external macros.
// Implements Tarjan's SCC (strongly connected components) algorithm, simplified to remove the count variable
static int has_backedge_to_worklist(jl_method_instance_t *mi, htable_t *visited, arraylist_t *stack)
{
    jl_module_t *mod = mi->def.module;
    if (jl_is_method(mod))
        mod = ((jl_method_t*)mod)->module;
    assert(jl_is_module(mod));
    uint8_t is_precompiled = jl_atomic_load_relaxed(&mi->flags) & JL_MI_FLAGS_MASK_PRECOMPILED;
    if (is_precompiled || !jl_object_in_image((jl_value_t*)mod) || type_in_worklist(mi->specTypes)) {
        return 1;
    }
    if (!mi->backedges) {
        return 0;
    }
    void **bp = ptrhash_bp(visited, mi);
    // HT_NOTFOUND: not yet analyzed
    // HT_NOTFOUND + 1: no link back
    // HT_NOTFOUND + 2: does link back
    // HT_NOTFOUND + 3: does link back, and included in new_ext_cis already
    // HT_NOTFOUND + 4 + depth: in-progress
    int found = (char*)*bp - (char*)HT_NOTFOUND;
    if (found)
        return found - 1;
    arraylist_push(stack, (void*)mi);
    int depth = stack->len;
    *bp = (void*)((char*)HT_NOTFOUND + 4 + depth); // preliminarily mark as in-progress
    size_t i = 0, n = jl_array_nrows(mi->backedges);
    int cycle = depth;
    while (i < n) {
        jl_code_instance_t *be;
        i = get_next_edge(mi->backedges, i, NULL, &be);
        JL_GC_PROMISE_ROOTED(be); // get_next_edge propagates the edge for us here
        int child_found = has_backedge_to_worklist(jl_get_ci_mi(be), visited, stack);
        if (child_found == 1 || child_found == 2) {
            // found what we were looking for, so terminate early
            found = 1;
            break;
        }
        else if (child_found >= 3 && child_found - 3 < cycle) {
            // record the cycle will resolve at depth "cycle"
            cycle = child_found - 3;
            assert(cycle);
        }
    }
    if (!found && cycle != depth)
        return cycle + 3;
    // If we are the top of the current cycle, now mark all other parts of
    // our cycle with what we found.
    // Or if we found a backedge, also mark all of the other parts of the
    // cycle as also having an backedge.
    while (stack->len >= depth) {
        void *mi = arraylist_pop(stack);
        bp = ptrhash_bp(visited, mi);
        assert((char*)*bp - (char*)HT_NOTFOUND == 5 + stack->len);
        *bp = (void*)((char*)HT_NOTFOUND + 1 + found);
    }
    return found;
}

// Given the list of CodeInstances that were inferred during the build, select
// those that are (1) external, (2) still valid, (3) are inferred to be called
// from the worklist or explicitly added by a `precompile` statement, and
// (4) are the most recently computed result for that method.
// These will be preserved in the image.
static jl_array_t *queue_external_cis(jl_array_t *list)
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
    jl_array_t *new_ext_cis = jl_alloc_vec_any(0);
    JL_GC_PUSH1(&new_ext_cis);
    for (i = n0; i-- > 0; ) {
        jl_code_instance_t *ci = (jl_code_instance_t*)jl_array_ptr_ref(list, i);
        assert(jl_is_code_instance(ci));
        jl_method_instance_t *mi = jl_get_ci_mi(ci);
        jl_method_t *m = mi->def.method;
        if (ci->owner == jl_nothing && jl_atomic_load_relaxed(&ci->inferred) && jl_is_method(m) && jl_object_in_image((jl_value_t*)m->module)) {
            int found = has_backedge_to_worklist(mi, &visited, &stack);
            assert(found == 0 || found == 1 || found == 2);
            assert(stack.len == 0);
            if (found == 1 && jl_atomic_load_relaxed(&ci->max_world) == ~(size_t)0) {
                jl_array_ptr_1d_push(new_ext_cis, (jl_value_t*)ci);
            }
        }
    }
    htable_free(&visited);
    arraylist_free(&stack);
    JL_GC_POP();
    // reverse new_ext_cis
    n0 = jl_array_nrows(new_ext_cis);
    jl_value_t **news = jl_array_data(new_ext_cis, jl_value_t*);
    for (i = 0; i < n0; i++) {
        jl_value_t *temp = news[i];
        news[i] = news[n0 - i - 1];
        news[n0 - i - 1] = temp;
    }
    return new_ext_cis;
}

// New roots for external methods
static void jl_collect_new_roots(jl_array_t *roots, jl_array_t *new_ext_cis, uint64_t key)
{
    htable_t mset;
    htable_new(&mset, 0);
    size_t l = new_ext_cis ? jl_array_nrows(new_ext_cis) : 0;
    for (size_t i = 0; i < l; i++) {
        jl_code_instance_t *ci = (jl_code_instance_t*)jl_array_ptr_ref(new_ext_cis, i);
        assert(jl_is_code_instance(ci));
        jl_method_t *m = jl_get_ci_mi(ci)->def.method;
        assert(jl_is_method(m));
        ptrhash_put(&mset, (void*)m, (void*)m);
    }
    int nwithkey;
    void *const *table = mset.table;
    jl_array_t *newroots = NULL;
    JL_GC_PUSH1(&newroots);
    for (size_t i = 0; i < mset.size; i += 2) {
        if (table[i+1] != HT_NOTFOUND) {
            jl_method_t *m = (jl_method_t*)table[i];
            assert(jl_is_method(m));
            nwithkey = nroots_with_key(m, key);
            if (nwithkey) {
                jl_array_ptr_1d_push(roots, (jl_value_t*)m);
                newroots = jl_alloc_vec_any(nwithkey);
                jl_array_ptr_1d_push(roots, (jl_value_t*)newroots);
                rle_iter_state rootiter = rle_iter_init(0);
                uint64_t *rletable = NULL;
                size_t nblocks2 = 0, nroots = jl_array_nrows(m->roots), k = 0;
                if (m->root_blocks) {
                    rletable = jl_array_data(m->root_blocks, uint64_t);
                    nblocks2 = jl_array_nrows(m->root_blocks);
                }
                while (rle_iter_increment(&rootiter, nroots, rletable, nblocks2))
                    if (rootiter.key == key)
                        jl_array_ptr_set(newroots, k++, jl_array_ptr_ref(m->roots, rootiter.i));
                assert(k == nwithkey);
            }
        }
    }
    JL_GC_POP();
    htable_free(&mset);
}


// For every method:
// - if the method is owned by a worklist module, add it to the list of things to be
//   verified on reloading
// - if the method is extext, record that it needs to be reinserted later in the method table
static int jl_collect_methcache_from_mod(jl_typemap_entry_t *ml, void *closure)
{
    jl_array_t *s = (jl_array_t*)closure;
    jl_method_t *m = ml->func.method;
    if (!jl_object_in_image((jl_value_t*)m->module)) {
        jl_array_ptr_1d_push(internal_methods, (jl_value_t*)m);
        if (s)
            jl_array_ptr_1d_push(s, (jl_value_t*)m); // extext
    }
    return 1;
}

static int jl_collect_methtable_from_mod(jl_methtable_t *mt, void *env)
{
    if (!jl_object_in_image((jl_value_t*)mt))
        env = NULL; // mark internal, not extext
    jl_typemap_visitor(jl_atomic_load_relaxed(&mt->defs), jl_collect_methcache_from_mod, env);
    return 1;
}

// Collect methods of external functions defined by modules in the worklist
// "extext" = "extending external"
// Also collect relevant backedges
static void jl_collect_extext_methods_from_mod(jl_array_t *s, jl_module_t *m)
{
    foreach_mtable_in_module(m, jl_collect_methtable_from_mod, s);
}

static void jl_record_edges(jl_method_instance_t *caller, jl_array_t *edges)
{
    jl_code_instance_t *ci = jl_atomic_load_relaxed(&caller->cache);
    while (ci != NULL) {
        if (jl_atomic_load_relaxed(&ci->edges) &&
            jl_atomic_load_relaxed(&ci->edges) != jl_emptysvec &&
            jl_atomic_load_relaxed(&ci->max_world) == ~(size_t)0)
            jl_array_ptr_1d_push(edges, (jl_value_t*)ci);
        ci = jl_atomic_load_relaxed(&ci->next);
    }
}

// Extract `edges` and `ext_targets` from `edges_map`
// `edges` = [caller1, ...], the list of codeinstances internal to methods
static void jl_collect_internal_cis(jl_array_t *edges, size_t world)
{
    for (size_t i = 0; i < jl_array_nrows(internal_methods); i++) {
        jl_method_t *m = (jl_method_t*)jl_array_ptr_ref(internal_methods, i);
        jl_value_t *specializations = jl_atomic_load_relaxed(&m->specializations);
        if (!jl_is_svec(specializations)) {
            jl_method_instance_t *mi = (jl_method_instance_t*)specializations;
            jl_record_edges(mi, edges);
        }
        else {
            size_t j, l = jl_svec_len(specializations);
            for (j = 0; j < l; j++) {
                jl_method_instance_t *mi = (jl_method_instance_t*)jl_svecref(specializations, j);
                if ((jl_value_t*)mi != jl_nothing)
                    jl_record_edges(mi, edges);
            }
        }
    }
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
static const char *git_info_string(const char *fld)
{
    static jl_value_t *GIT_VERSION_INFO = NULL;
    if (!GIT_VERSION_INFO)
        GIT_VERSION_INFO = jl_get_global(jl_base_module, jl_symbol("GIT_VERSION_INFO"));
    jl_value_t *f = jl_get_field(GIT_VERSION_INFO, fld);
    assert(jl_is_string(f));
    return jl_string_data(f);
}

static const char *jl_git_branch(void)
{
    static const char *branch = NULL;
    if (!branch) branch = git_info_string("branch");
    return branch;
}

static const char *jl_git_commit(void)
{
    static const char *commit = NULL;
    if (!commit) commit = git_info_string("commit");
    return commit;
}


// "magic" string and version header of .ji file
static const int JI_FORMAT_VERSION = 12;
static const char JI_MAGIC[] = "\373jli\r\n\032\n"; // based on PNG signature
static const uint16_t BOM = 0xFEFF; // byte-order marker
static int64_t write_header(ios_t *s, uint8_t pkgimage)
{
    ios_write(s, JI_MAGIC, strlen(JI_MAGIC));
    write_uint16(s, JI_FORMAT_VERSION);
    ios_write(s, (char *) &BOM, 2);
    write_uint8(s, sizeof(void*));
    ios_write(s, JL_BUILD_UNAME, strlen(JL_BUILD_UNAME)+1);
    ios_write(s, JL_BUILD_ARCH, strlen(JL_BUILD_ARCH)+1);
    ios_write(s, JULIA_VERSION_STRING, strlen(JULIA_VERSION_STRING)+1);
    const char *branch = jl_git_branch(), *commit = jl_git_commit();
    ios_write(s, branch, strlen(branch)+1);
    ios_write(s, commit, strlen(commit)+1);
    write_uint8(s, pkgimage);
    int64_t checksumpos = ios_pos(s);
    write_uint64(s, 0); // eventually will hold checksum for the content portion of this (build_id.hi)
    write_uint64(s, 0); // eventually will hold dataendpos
    write_uint64(s, 0); // eventually will hold datastartpos
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
static int64_t write_dependency_list(ios_t *s, jl_array_t* worklist, jl_array_t **udepsp)
{
    int64_t initial_pos = 0;
    int64_t pos = 0;
    static jl_array_t *deps = NULL;
    if (!deps)
        deps = (jl_array_t*)jl_get_global(jl_base_module, jl_symbol("_require_dependencies"));

    // unique(deps) to eliminate duplicates while preserving order:
    // we preserve order so that the topmost included .jl file comes first
    static jl_value_t *unique_func = NULL;
    if (!unique_func)
        unique_func = jl_get_global(jl_base_module, jl_symbol("unique"));
    jl_value_t *uniqargs[2] = {unique_func, (jl_value_t*)deps};
    jl_task_t *ct = jl_current_task;
    size_t last_age = ct->world_age;
    ct->world_age = jl_atomic_load_acquire(&jl_world_counter);
    jl_array_t *udeps = (*udepsp = deps && unique_func ? (jl_array_t*)jl_apply(uniqargs, 2) : NULL);
    ct->world_age = last_age;

    static jl_value_t *replace_depot_func = NULL;
    if (!replace_depot_func)
        replace_depot_func = jl_get_global(jl_base_module, jl_symbol("replace_depot_path"));
    static jl_value_t *normalize_depots_func = NULL;
    if (!normalize_depots_func)
        normalize_depots_func = jl_get_global(jl_base_module, jl_symbol("normalize_depots_for_relocation"));

    jl_value_t *depots = NULL, *prefs_hash = NULL, *prefs_list = NULL;
    JL_GC_PUSH2(&depots, &prefs_list);
    last_age = ct->world_age;
    ct->world_age = jl_atomic_load_acquire(&jl_world_counter);
    depots = jl_apply(&normalize_depots_func, 1);
    ct->world_age = last_age;

    // write a placeholder for total size so that we can quickly seek past all of the
    // dependencies if we don't need them
    initial_pos = ios_pos(s);
    write_uint64(s, 0);
    size_t i, l = udeps ? jl_array_nrows(udeps) : 0;
    for (i = 0; i < l; i++) {
        jl_value_t *deptuple = jl_array_ptr_ref(udeps, i);
        jl_value_t *deppath = jl_fieldref(deptuple, 1);

        if (replace_depot_func) {
            jl_value_t **replace_depot_args;
            JL_GC_PUSHARGS(replace_depot_args, 3);
            replace_depot_args[0] = replace_depot_func;
            replace_depot_args[1] = deppath;
            replace_depot_args[2] = depots;
            ct = jl_current_task;
            size_t last_age = ct->world_age;
            ct->world_age = jl_atomic_load_acquire(&jl_world_counter);
            deppath = (jl_value_t*)jl_apply(replace_depot_args, 3);
            ct->world_age = last_age;
            JL_GC_POP();
        }

        size_t slen = jl_string_len(deppath);
        write_int32(s, slen);
        ios_write(s, jl_string_data(deppath), slen);
        write_uint64(s, jl_unbox_uint64(jl_fieldref(deptuple, 2)));    // fsize
        write_uint32(s, jl_unbox_uint32(jl_fieldref(deptuple, 3)));    // hash
        write_float64(s, jl_unbox_float64(jl_fieldref(deptuple, 4)));  // mtime
        jl_module_t *depmod = (jl_module_t*)jl_fieldref(deptuple, 0);  // evaluating module
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

    // Calculate Preferences hash for current package.
    if (jl_base_module) {
        // Toplevel module is the module we're currently compiling, use it to get our preferences hash
        jl_value_t * toplevel = (jl_value_t*)jl_get_global(jl_base_module, jl_symbol("__toplevel__"));
        jl_value_t * prefs_hash_func = jl_get_global(jl_base_module, jl_symbol("get_preferences_hash"));
        jl_value_t * get_compiletime_prefs_func = jl_get_global(jl_base_module, jl_symbol("get_compiletime_preferences"));

        if (toplevel && prefs_hash_func && get_compiletime_prefs_func) {
            // Temporary invoke in newest world age
            size_t last_age = ct->world_age;
            ct->world_age = jl_atomic_load_acquire(&jl_world_counter);

            // call get_compiletime_prefs(__toplevel__)
            jl_value_t *args[3] = {get_compiletime_prefs_func, (jl_value_t*)toplevel, NULL};
            prefs_list = (jl_value_t*)jl_apply(args, 2);

            // Call get_preferences_hash(__toplevel__, prefs_list)
            args[0] = prefs_hash_func;
            args[2] = prefs_list;
            prefs_hash = (jl_value_t*)jl_apply(args, 3);

            // Reset world age to normal
            ct->world_age = last_age;
        }
    }

    // If we successfully got the preferences, write it out, otherwise write `0` for this `.ji` file.
    if (prefs_hash != NULL && prefs_list != NULL) {
        size_t i, l = jl_array_nrows(prefs_list);
        for (i = 0; i < l; i++) {
            jl_value_t *pref_name = jl_array_ptr_ref(prefs_list, i);
            size_t slen = jl_string_len(pref_name);
            write_int32(s, slen);
            ios_write(s, jl_string_data(pref_name), slen);
        }
        write_int32(s, 0); // terminator
        write_uint64(s, jl_unbox_uint64(prefs_hash));
    }
    else {
        // This is an error path, but let's at least generate a valid `.ji` file.
        // We declare an empty list of preference names, followed by a zero-hash.
        // The zero-hash is not what would be generated for an empty set of preferences,
        // and so this `.ji` file will be invalidated by a future non-erroring pass
        // through this function.
        write_int32(s, 0);
        write_uint64(s, 0);
    }
    JL_GC_POP(); // for depots, prefs_list

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
static void jl_add_methods(jl_array_t *external)
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
static void jl_activate_methods(jl_array_t *external, jl_array_t *internal, size_t world, const char *pkgname)
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
            assert(jl_atomic_load_relaxed(&m->deleted_world) == WORLD_AGE_REVALIDATION_SENTINEL);
            jl_atomic_store_release(&m->primary_world, world);
            jl_atomic_store_release(&m->deleted_world, ~(size_t)0);
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
            jl_methtable_t *mt = jl_method_get_table(entry->func.method);
            assert((jl_value_t*)mt != jl_nothing);
            jl_method_table_activate(mt, entry);
        }
    }
}

static void jl_copy_roots(jl_array_t *method_roots_list, uint64_t key)
{
    size_t i, l = jl_array_nrows(method_roots_list);
    for (i = 0; i < l; i+=2) {
        jl_method_t *m = (jl_method_t*)jl_array_ptr_ref(method_roots_list, i);
        jl_array_t *roots = (jl_array_t*)jl_array_ptr_ref(method_roots_list, i+1);
        if (roots) {
            assert(jl_is_array(roots));
            jl_append_method_roots(m, key, roots);
        }
    }
}

static void verify_invokesig(jl_value_t *invokesig, jl_method_t *expected, size_t world, size_t *minworld, size_t *maxworld)
{
    assert(jl_is_type(invokesig));
    assert(jl_is_method(expected));
    if (jl_egal(invokesig, expected->sig)) {
        // the invoke match is `expected` for `expected->sig`, unless `expected` is invalid
        *minworld = jl_atomic_load_relaxed(&expected->primary_world);
        *maxworld = jl_atomic_load_relaxed(&expected->deleted_world);
        assert(*minworld <= world);
        if (*maxworld < world)
            *maxworld = 0;
    }
    else {
        *minworld = 1;
        *maxworld = ~(size_t)0;
        jl_methtable_t *mt = jl_method_get_table(expected);
        if ((jl_value_t*)mt == jl_nothing) {
            *maxworld = 0;
        }
        else {
            jl_value_t *matches = jl_gf_invoke_lookup_worlds(invokesig, (jl_value_t*)mt, world, minworld, maxworld);
            if (matches == jl_nothing) {
                *maxworld = 0;
            }
            else {
                if (((jl_method_match_t*)matches)->method != expected) {
                    *maxworld = 0;
                }
            }
        }
    }
}

static void verify_call(jl_value_t *sig, jl_svec_t *expecteds, size_t i, size_t n, size_t world, size_t *minworld, size_t *maxworld, jl_value_t **matches JL_REQUIRE_ROOTED_SLOT)
{
    // verify that these edges intersect with the same methods as before
    *minworld = 1;
    *maxworld = ~(size_t)0;
    int ambig = 0;
    // TODO: possibly need to included ambiguities too (for the optimizer correctness)?
    jl_value_t *result = jl_matching_methods((jl_tupletype_t*)sig, jl_nothing,
            _jl_debug_method_invalidation ? INT32_MAX : n,
            0, world, minworld, maxworld, &ambig);
    *matches = result;
    if (result == jl_nothing) {
        *maxworld = 0;
    }
    else {
        // setdiff!(result, expected)
        size_t j, k, ins = 0;
        if (jl_array_nrows(result) != n) {
            *maxworld = 0;
        }
        for (k = 0; k < jl_array_nrows(result); k++) {
            jl_method_t *match = ((jl_method_match_t*)jl_array_ptr_ref(result, k))->method;
            for (j = 0; j < n; j++) {
                jl_value_t *t = jl_svecref(expecteds, j + i);
                if (jl_is_code_instance(t))
                    t = (jl_value_t*)((jl_code_instance_t*)t)->def;
                jl_method_t *meth;
                if (jl_is_method(t))
                    meth = (jl_method_t*)t;
                else {
                    assert(jl_is_method_instance(t));
                    meth = ((jl_method_instance_t*)t)->def.method;
                }
                if (match == meth)
                    break;
            }
            if (j == n) {
                // intersection has a new method or a method was
                // deleted--this is now probably no good, just invalidate
                // everything about it now
                *maxworld = 0;
                if (!_jl_debug_method_invalidation)
                    break;
                jl_array_ptr_set(result, ins++, match);
            }
        }
        if (*maxworld != ~(size_t)0 && _jl_debug_method_invalidation)
            jl_array_del_end((jl_array_t*)result, jl_array_nrows(result) - ins);
    }
}

// Test all edges relevant to a method:
//// Visit the entire call graph, starting from edges[idx] to determine if that method is valid
//// Implements Tarjan's SCC (strongly connected components) algorithm, simplified to remove the count variable
//// and slightly modified with an early termination option once the computation reaches its minimum
static int jl_verify_method(jl_code_instance_t *codeinst, size_t *minworld, size_t *maxworld, arraylist_t *stack, htable_t *visiting)
{
    size_t world = jl_atomic_load_relaxed(&codeinst->min_world);
    size_t max_valid2 = jl_atomic_load_relaxed(&codeinst->max_world);
    if (max_valid2 != WORLD_AGE_REVALIDATION_SENTINEL) {
        *minworld = world;
        *maxworld = max_valid2;
        return 0;
    }
    *minworld = 1;
    size_t current_world = jl_atomic_load_relaxed(&jl_world_counter);
    *maxworld = current_world;
    assert(jl_is_method_instance(jl_get_ci_mi(codeinst)) && jl_is_method(jl_get_ci_mi(codeinst)->def.method));
    void **bp = ptrhash_bp(visiting, codeinst);
    if (*bp != HT_NOTFOUND)
        return (char*)*bp - (char*)HT_NOTFOUND; // cycle idx
    arraylist_push(stack, (void*)codeinst);
    size_t depth = stack->len;
    *bp = (char*)HT_NOTFOUND + depth;
    JL_TIMING(VERIFY_IMAGE, VERIFY_Methods);
    jl_svec_t *callees = jl_atomic_load_relaxed(&codeinst->edges);
    assert(jl_is_svec((jl_value_t*)callees));
    // verify current edges
    if (callees == jl_emptysvec) {
        // quick return: no edges to verify (though we probably shouldn't have gotten here from WORLD_AGE_REVALIDATION_SENTINEL)
    }
    else if (*maxworld == jl_require_world) {
        // if no new worlds were allocated since serializing the base module, then no new validation is worth doing right now either
        *minworld = *maxworld;
    }
    else {
        jl_value_t *loctag = NULL;
        jl_value_t *sig = NULL;
        jl_value_t *matches = NULL;
        JL_GC_PUSH3(&loctag, &matches, &sig);
        for (size_t j = 0; j < jl_svec_len(callees); ) {
            jl_value_t *edge = jl_svecref(callees, j);
            size_t min_valid2;
            size_t max_valid2;
            assert(!jl_is_method(edge)); // `Method`-edge isn't allowed for the optimized one-edge format
            if (jl_is_code_instance(edge))
                edge = (jl_value_t*)jl_get_ci_mi((jl_code_instance_t*)edge);
            if (jl_is_method_instance(edge)) {
                jl_method_instance_t *mi = (jl_method_instance_t*)edge;
                sig = jl_type_intersection(mi->def.method->sig, (jl_value_t*)mi->specTypes); // TODO: ??
                verify_call(sig, callees, j, 1, world, &min_valid2, &max_valid2, &matches);
                sig = NULL;
                j += 1;
            }
            else if (jl_is_long(edge)) {
                jl_value_t *sig = jl_svecref(callees, j + 1);
                size_t nedges = jl_unbox_long(edge);
                verify_call(sig, callees, j + 2, nedges, world, &min_valid2, &max_valid2, &matches);
                j += 2 + nedges;
                edge = sig;
            }
            else {
                jl_method_instance_t *callee = (jl_method_instance_t*)jl_svecref(callees, j + 1);
                jl_method_t *meth;
                if (jl_is_mtable(callee)) {
                    // skip the legacy edge (missing backedge)
                    j += 2;
                    continue;
                }
                if (jl_is_code_instance(callee))
                    callee = jl_get_ci_mi((jl_code_instance_t*)callee);
                if (jl_is_method_instance(callee)) {
                    meth = callee->def.method;
                }
                else {
                    assert(jl_is_method(callee));
                    meth = (jl_method_t*)callee;
                }
                verify_invokesig(edge, meth, world, &min_valid2, &max_valid2);
                j += 2;
            }
            if (*minworld < min_valid2)
                *minworld = min_valid2;
            if (*maxworld > max_valid2)
                *maxworld = max_valid2;
            if (max_valid2 != ~(size_t)0 && _jl_debug_method_invalidation) {
                jl_array_ptr_1d_push(_jl_debug_method_invalidation, edge);
                loctag = jl_cstr_to_string("insert_backedges_callee");
                jl_array_ptr_1d_push(_jl_debug_method_invalidation, loctag);
                jl_array_ptr_1d_push(_jl_debug_method_invalidation, (jl_value_t*)codeinst);
                jl_array_ptr_1d_push(_jl_debug_method_invalidation, matches);
            }
            //jl_static_show((JL_STREAM*)ios_stderr, (jl_value_t*)edge);
            //ios_puts(max_valid2 == ~(size_t)0 ? "valid\n" : "INVALID\n", ios_stderr);
            if (max_valid2 == 0 && !_jl_debug_method_invalidation)
                break;
        }
        JL_GC_POP();
    }
    // verify recursive edges (if valid, or debugging)
    size_t cycle = depth;
    jl_code_instance_t *cause = codeinst;
    if (*maxworld != 0 || _jl_debug_method_invalidation) {
        for (size_t j = 0; j < jl_svec_len(callees); j++) {
            jl_value_t *edge = jl_svecref(callees, j);
            if (!jl_is_code_instance(edge))
                continue;
            jl_code_instance_t *callee = (jl_code_instance_t*)edge;
            size_t min_valid2;
            size_t max_valid2;
            size_t child_cycle = jl_verify_method(callee, &min_valid2, &max_valid2, stack, visiting);
            if (*minworld < min_valid2)
                *minworld = min_valid2;
            if (*minworld > max_valid2)
                max_valid2 = 0;
            if (*maxworld > max_valid2) {
                cause = callee;
                *maxworld = max_valid2;
            }
            if (max_valid2 == 0) {
                // found what we were looking for, so terminate early
                break;
            }
            else if (child_cycle && child_cycle < cycle) {
                // record the cycle will resolve at depth "cycle"
                cycle = child_cycle;
            }
        }
    }
    if (*maxworld != 0 && cycle != depth)
        return cycle;
    // If we are the top of the current cycle, now mark all other parts of
    // our cycle with what we found.
    // Or if we found a failed edge, also mark all of the other parts of the
    // cycle as also having a failed edge.
    while (stack->len >= depth) {
        jl_code_instance_t *child = (jl_code_instance_t*)arraylist_pop(stack);
        if (jl_atomic_load_relaxed(&jl_n_threads) == 1) {
            // a different thread might simultaneously come to a different, but equally valid, alternative result
            assert(jl_atomic_load_relaxed(&child->max_world) == WORLD_AGE_REVALIDATION_SENTINEL);
            assert(*minworld <= jl_atomic_load_relaxed(&child->min_world));
        }
        if (*maxworld != 0)
            jl_atomic_store_relaxed(&child->min_world, *minworld);
        jl_atomic_store_relaxed(&child->max_world, *maxworld);
        void **bp = ptrhash_bp(visiting, child);
        assert(*bp == (char*)HT_NOTFOUND + stack->len + 1);
        *bp = HT_NOTFOUND;
        if (_jl_debug_method_invalidation && *maxworld < current_world) {
            jl_value_t *loctag;
            jl_array_ptr_1d_push(_jl_debug_method_invalidation, (jl_value_t*)child);
            loctag = jl_cstr_to_string("verify_methods");
            JL_GC_PUSH1(&loctag);
            jl_array_ptr_1d_push(_jl_debug_method_invalidation, loctag);
            jl_array_ptr_1d_push(_jl_debug_method_invalidation, (jl_value_t*)cause);
            JL_GC_POP();
        }
    }
    //jl_static_show((JL_STREAM*)ios_stderr, (jl_value_t*)codeinst->def);
    //ios_puts(max_valid == ~(size_t)0 ? "valid\n\n" : "INVALID\n\n", ios_stderr);
    return 0;
}

static void jl_verify_method_graph(jl_code_instance_t *codeinst, arraylist_t *stack, htable_t *visiting)
{
    size_t minworld;
    size_t maxworld;
    assert(stack->len == 0);
    for (size_t i = 0, hsz = visiting->size; i < hsz; i++)
        assert(visiting->table[i] == HT_NOTFOUND);
    int child_cycle = jl_verify_method(codeinst, &minworld, &maxworld, stack, visiting);
    assert(child_cycle == 0); (void)child_cycle;
    assert(stack->len == 0);
    for (size_t i = 0, hsz = visiting->size / 2; i < hsz; i++) {
        assert(visiting->table[2 * i + 1] == HT_NOTFOUND);
        visiting->table[2 * i] = HT_NOTFOUND;
    }
    if (jl_atomic_load_relaxed(&jl_n_threads) == 1) { // a different thread might simultaneously come to a different, but equally valid, alternative result
        assert(maxworld == 0 || jl_atomic_load_relaxed(&codeinst->min_world) == minworld);
        assert(jl_atomic_load_relaxed(&codeinst->max_world) == maxworld);
    }
}

// Restore backedges to external targets
// `edges` = [caller1, ...], the list of worklist-owned code instances internally
// `ext_ci_list` = [caller1, ...], the list of worklist-owned code instances externally
static void jl_insert_backedges(jl_array_t *edges, jl_array_t *ext_ci_list)
{
    // determine which CodeInstance objects are still valid in our image
    // to enable any applicable new codes
    arraylist_t stack;
    arraylist_new(&stack, 0);
    htable_t visiting;
    htable_new(&visiting, 0);
    for (size_t external = 0; external < (ext_ci_list ? 2 : 1); external++) {
        if (external)
            edges = ext_ci_list;
        size_t nedges = jl_array_nrows(edges);
        for (size_t i = 0; i < nedges; i++) {
            jl_code_instance_t *codeinst = (jl_code_instance_t*)jl_array_ptr_ref(edges, i);
            jl_svec_t *callees = jl_atomic_load_relaxed(&codeinst->edges);
            jl_method_instance_t *caller = jl_get_ci_mi(codeinst);
            jl_verify_method_graph(codeinst, &stack, &visiting);
            size_t minvalid = jl_atomic_load_relaxed(&codeinst->min_world);
            size_t maxvalid = jl_atomic_load_relaxed(&codeinst->max_world);
            if (maxvalid >= minvalid) {
                if (jl_atomic_load_relaxed(&jl_world_counter) == maxvalid) {
                    // if this callee is still valid, add all the backedges
                    for (size_t j = 0; j < jl_svec_len(callees); ) {
                        jl_value_t *edge = jl_svecref(callees, j);
                        if (jl_is_long(edge)) {
                            j += 2; // skip over signature and count but not methods
                            continue;
                        }
                        else if (jl_is_method(edge)) {
                            j += 1;
                            continue;
                        }
                        if (jl_is_code_instance(edge))
                            edge = (jl_value_t*)((jl_code_instance_t*)edge)->def;
                        if (jl_is_method_instance(edge)) {
                            jl_method_instance_add_backedge((jl_method_instance_t*)edge, NULL, codeinst);
                            j += 1;
                        }
                        else {
                            jl_value_t *callee = jl_svecref(callees, j + 1);
                            if (jl_is_mtable(callee)) {
                                jl_methtable_t *mt = (jl_methtable_t*)callee;
                                jl_method_table_add_backedge(mt, edge, codeinst);
                                j += 2;
                                continue;
                            }
                            else if (jl_is_code_instance(callee)) {
                                callee = (jl_value_t*)((jl_code_instance_t*)callee)->def;
                            }
                            else if (jl_is_method(callee)) {
                                j += 2;
                                continue;
                            }
                            jl_method_instance_add_backedge((jl_method_instance_t*)callee, edge, codeinst);
                            j += 2;
                        }
                    }
                }
                if (jl_atomic_load_relaxed(&jl_world_counter) == maxvalid) {
                    maxvalid = ~(size_t)0;
                    jl_atomic_store_relaxed(&codeinst->max_world, maxvalid);
                }
                if (external) {
                    jl_value_t *owner = codeinst->owner;
                    JL_GC_PROMISE_ROOTED(owner);

                    // See #53586, #53109
                    assert(jl_atomic_load_relaxed(&codeinst->inferred));

                    if (jl_rettype_inferred(owner, caller, minvalid, maxvalid) != jl_nothing) {
                        // We already got a code instance for this world age range from somewhere else - we don't need
                        // this one.
                    }
                    else {
                        jl_mi_cache_insert(caller, codeinst);
                    }
                }
            }
        }
    }

    htable_free(&visiting);
    arraylist_free(&stack);
}

static jl_value_t *read_verify_mod_list(ios_t *s, jl_array_t *depmods)
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

JL_DLLEXPORT uint64_t jl_read_verify_header(ios_t *s, uint8_t *pkgimage, int64_t *dataendpos, int64_t *datastartpos)
{
    uint16_t bom;
    uint64_t checksum = 0;
    if (readstr_verify(s, JI_MAGIC, 0) &&
        read_uint16(s) == JI_FORMAT_VERSION &&
        ios_read(s, (char *) &bom, 2) == 2 && bom == BOM &&
        read_uint8(s) == sizeof(void*) &&
        readstr_verify(s, JL_BUILD_UNAME, 1) &&
        readstr_verify(s, JL_BUILD_ARCH, 1) &&
        readstr_verify(s, JULIA_VERSION_STRING, 1) &&
        readstr_verify(s, jl_git_branch(), 1) &&
        readstr_verify(s, jl_git_commit(), 1))
    {
        *pkgimage = read_uint8(s);
        checksum = read_uint64(s);
        *datastartpos = (int64_t)read_uint64(s);
        *dataendpos = (int64_t)read_uint64(s);
    }
    return checksum;
}

// Returns `depmodidxs` where `j = depmodidxs[i]` corresponds to the blob `depmods[j]` in `write_mod_list`
static jl_array_t *image_to_depmodidx(jl_array_t *depmods)
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
static jl_array_t *depmod_to_imageidx(jl_array_t *depmods)
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

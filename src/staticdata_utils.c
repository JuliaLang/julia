// inverse of backedges graph (caller=>callees hash)
jl_array_t *edges_map JL_GLOBALLY_ROOTED = NULL; // rooted for the duration of our uses of this

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
        // check if super is news, since then we must be new also
        // (it is also possible that super is indeterminate now, wait for `t`
        // to be resolved, then will be determined later and fixed up by the
        // delay_list, for this and any other references to it).
        while (super != jl_any_type) {
            assert(super);
            if (ptrhash_has(news, (void*)super))
                return 1;
            if (!(image_base < (char*)super && (char*)super <= image_base + sizeof_sysimg))
               break; // fast-path for rejection of super
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
    size_t len = jl_array_len(worklist);
    if (len > 0) {
        jl_module_t *topmod = (jl_module_t*)jl_array_ptr_ref(worklist, len-1);
        assert(jl_is_module(topmod));
        return topmod->build_id.lo;
    }
    return 0;
}

static jl_array_t *newly_inferred JL_GLOBALLY_ROOTED /*FIXME*/;
// Mutex for newly_inferred
static jl_mutex_t newly_inferred_mutex;

// Register array of newly-inferred MethodInstances
// This gets called as the first step of Base.include_package_for_output
JL_DLLEXPORT void jl_set_newly_inferred(jl_value_t* _newly_inferred)
{
    assert(_newly_inferred == NULL || jl_is_array(_newly_inferred));
    newly_inferred = (jl_array_t*) _newly_inferred;
}

JL_DLLEXPORT void jl_push_newly_inferred(jl_value_t* ci)
{
    JL_LOCK(&newly_inferred_mutex);
    size_t end = jl_array_len(newly_inferred);
    jl_array_grow_end(newly_inferred, 1);
    jl_arrayset(newly_inferred, ci, end);
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
    if (mi->precompiled || !jl_object_in_image((jl_value_t*)mod) || type_in_worklist(mi->specTypes)) {
        return 1;
    }
    if (!mi->backedges) {
        return 0;
    }
    void **bp = ptrhash_bp(visited, mi);
    // HT_NOTFOUND: not yet analyzed
    // HT_NOTFOUND + 1: no link back
    // HT_NOTFOUND + 2: does link back
    // HT_NOTFOUND + 3: does link back, and included in new_specializations already
    // HT_NOTFOUND + 4 + depth: in-progress
    int found = (char*)*bp - (char*)HT_NOTFOUND;
    if (found)
        return found - 1;
    arraylist_push(stack, (void*)mi);
    int depth = stack->len;
    *bp = (void*)((char*)HT_NOTFOUND + 4 + depth); // preliminarily mark as in-progress
    size_t i = 0, n = jl_array_len(mi->backedges);
    int cycle = 0;
    while (i < n) {
        jl_method_instance_t *be;
        i = get_next_edge(mi->backedges, i, NULL, &be);
        int child_found = has_backedge_to_worklist(be, visited, stack);
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
    if (!found && cycle && cycle != depth)
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
    size_t n0 = jl_array_len(list);
    htable_new(&visited, n0);
    arraylist_new(&stack, 0);
    jl_array_t *new_specializations = jl_alloc_vec_any(0);
    JL_GC_PUSH1(&new_specializations);
    for (i = n0; i-- > 0; ) {
        jl_code_instance_t *ci = (jl_code_instance_t*)jl_array_ptr_ref(list, i);
        assert(jl_is_code_instance(ci));
        jl_method_instance_t *mi = ci->def;
        jl_method_t *m = mi->def.method;
        if (ci->inferred && jl_is_method(m) && jl_object_in_image((jl_value_t*)m->module)) {
            int found = has_backedge_to_worklist(mi, &visited, &stack);
            assert(found == 0 || found == 1 || found == 2);
            assert(stack.len == 0);
            if (found == 1 && ci->max_world == ~(size_t)0) {
                void **bp = ptrhash_bp(&visited, mi);
                if (*bp != (void*)((char*)HT_NOTFOUND + 3)) {
                    *bp = (void*)((char*)HT_NOTFOUND + 3);
                    jl_array_ptr_1d_push(new_specializations, (jl_value_t*)ci);
                }
            }
        }
    }
    htable_free(&visited);
    arraylist_free(&stack);
    JL_GC_POP();
    // reverse new_specializations
    n0 = jl_array_len(new_specializations);
    jl_value_t **news = (jl_value_t**)jl_array_data(new_specializations);
    for (i = 0; i < n0; i++) {
        jl_value_t *temp = news[i];
        news[i] = news[n0 - i - 1];
        news[n0 - i - 1] = temp;
    }
    return new_specializations;
}

// New roots for external methods
static void jl_collect_new_roots(jl_array_t *roots, jl_array_t *new_specializations, uint64_t key)
{
    htable_t mset;
    htable_new(&mset, 0);
    size_t l = new_specializations ? jl_array_len(new_specializations) : 0;
    for (size_t i = 0; i < l; i++) {
        jl_code_instance_t *ci = (jl_code_instance_t*)jl_array_ptr_ref(new_specializations, i);
        assert(jl_is_code_instance(ci));
        jl_method_t *m = ci->def->def.method;
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
                size_t nblocks2 = 0, nroots = jl_array_len(m->roots), k = 0;
                if (m->root_blocks) {
                    rletable = (uint64_t*)jl_array_data(m->root_blocks);
                    nblocks2 = jl_array_len(m->root_blocks);
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

// Create the forward-edge map (caller => callees)
// the intent of these functions is to invert the backedges tree
// for anything that points to a method not part of the worklist
//
// from MethodTables
static void jl_collect_missing_backedges(jl_methtable_t *mt)
{
    jl_array_t *backedges = mt->backedges;
    if (backedges) {
        size_t i, l = jl_array_len(backedges);
        for (i = 1; i < l; i += 2) {
            jl_method_instance_t *caller = (jl_method_instance_t*)jl_array_ptr_ref(backedges, i);
            jl_value_t *missing_callee = jl_array_ptr_ref(backedges, i - 1);  // signature of abstract callee
            jl_array_t *edges = (jl_array_t*)jl_eqtable_get(edges_map, (jl_value_t*)caller, NULL);
            if (edges == NULL) {
                edges = jl_alloc_vec_any(0);
                JL_GC_PUSH1(&edges);
                edges_map = jl_eqtable_put(edges_map, (jl_value_t*)caller, (jl_value_t*)edges, NULL);
                JL_GC_POP();
            }
            jl_array_ptr_1d_push(edges, NULL);
            jl_array_ptr_1d_push(edges, missing_callee);
        }
    }
}


// from MethodInstances
static void collect_backedges(jl_method_instance_t *callee, int internal)
{
    jl_array_t *backedges = callee->backedges;
    if (backedges) {
        size_t i = 0, l = jl_array_len(backedges);
        while (i < l) {
            jl_value_t *invokeTypes;
            jl_method_instance_t *caller;
            i = get_next_edge(backedges, i, &invokeTypes, &caller);
            jl_array_t *edges = (jl_array_t*)jl_eqtable_get(edges_map, (jl_value_t*)caller, NULL);
            if (edges == NULL) {
                edges = jl_alloc_vec_any(0);
                JL_GC_PUSH1(&edges);
                edges_map = jl_eqtable_put(edges_map, (jl_value_t*)caller, (jl_value_t*)edges, NULL);
                JL_GC_POP();
            }
            jl_array_ptr_1d_push(edges, invokeTypes);
            jl_array_ptr_1d_push(edges, (jl_value_t*)callee);
        }
    }
}


// For functions owned by modules not on the worklist, call this on each method.
// - if the method is owned by a worklist module, add it to the list of things to be
//   fully serialized
// - Collect all backedges (may be needed later when we invert this list).
static int jl_collect_methcache_from_mod(jl_typemap_entry_t *ml, void *closure)
{
    jl_array_t *s = (jl_array_t*)closure;
    jl_method_t *m = ml->func.method;
    if (s && !jl_object_in_image((jl_value_t*)m->module)) {
        jl_array_ptr_1d_push(s, (jl_value_t*)m);
    }
    if (edges_map == NULL)
        return 1;
    jl_svec_t *specializations = m->specializations;
    size_t i, l = jl_svec_len(specializations);
    for (i = 0; i < l; i++) {
        jl_method_instance_t *callee = (jl_method_instance_t*)jl_svecref(specializations, i);
        if ((jl_value_t*)callee != jl_nothing)
            collect_backedges(callee, !s);
    }
    return 1;
}

static int jl_collect_methtable_from_mod(jl_methtable_t *mt, void *env)
{
    if (!jl_object_in_image((jl_value_t*)mt))
        env = NULL; // do not collect any methods from here
    jl_typemap_visitor(jl_atomic_load_relaxed(&mt->defs), jl_collect_methcache_from_mod, env);
    if (env && edges_map)
        jl_collect_missing_backedges(mt);
    return 1;
}

// Collect methods of external functions defined by modules in the worklist
// "extext" = "extending external"
// Also collect relevant backedges
static void jl_collect_extext_methods_from_mod(jl_array_t *s, jl_module_t *m)
{
    foreach_mtable_in_module(m, jl_collect_methtable_from_mod, s);
}

static void jl_record_edges(jl_method_instance_t *caller, arraylist_t *wq, jl_array_t *edges)
{
    jl_array_t *callees = NULL;
    JL_GC_PUSH2(&caller, &callees);
    callees = (jl_array_t*)jl_eqtable_pop(edges_map, (jl_value_t*)caller, NULL, NULL);
    if (callees != NULL) {
        jl_array_ptr_1d_push(edges, (jl_value_t*)caller);
        jl_array_ptr_1d_push(edges, (jl_value_t*)callees);
        size_t i, l = jl_array_len(callees);
        for (i = 1; i < l; i += 2) {
            jl_method_instance_t *c = (jl_method_instance_t*)jl_array_ptr_ref(callees, i);
            if (c && jl_is_method_instance(c)) {
                arraylist_push(wq, c);
            }
        }
    }
    JL_GC_POP();
}


// Extract `edges` and `ext_targets` from `edges_map`
// `edges` = [caller1, targets_indexes1, ...], the list of methods and their edges
// `ext_targets` is [invokesig1, callee1, matches1, ...], the edges for each target
static void jl_collect_edges(jl_array_t *edges, jl_array_t *ext_targets, jl_array_t *external_cis)
{
    size_t world = jl_atomic_load_acquire(&jl_world_counter);
    htable_t external_mis;
    htable_new(&external_mis, 0);
    if (external_cis) {
        for (size_t i = 0; i < jl_array_len(external_cis); i++) {
            jl_code_instance_t *ci = (jl_code_instance_t*)jl_array_ptr_ref(external_cis, i);
            jl_method_instance_t *mi = ci->def;
            ptrhash_put(&external_mis, (void*)mi, (void*)mi);
        }
    }
    arraylist_t wq;
    arraylist_new(&wq, 0);
    void **table = (void**)jl_array_data(edges_map);    // edges_map is caller => callees
    size_t table_size = jl_array_len(edges_map);
    for (size_t i = 0; i < table_size; i += 2) {
        assert(table == jl_array_data(edges_map) && table_size == jl_array_len(edges_map) &&
               "edges_map changed during iteration");
        jl_method_instance_t *caller = (jl_method_instance_t*)table[i];
        jl_array_t *callees = (jl_array_t*)table[i + 1];
        if (callees == NULL)
            continue;
        assert(jl_is_method_instance(caller) && jl_is_method(caller->def.method));
        if (!jl_object_in_image((jl_value_t*)caller->def.method->module) ||
            ptrhash_get(&external_mis, caller) != HT_NOTFOUND) {
            jl_record_edges(caller, &wq, edges);
        }
    }
    htable_free(&external_mis);
    while (wq.len) {
        jl_method_instance_t *caller = (jl_method_instance_t*)arraylist_pop(&wq);
        jl_record_edges(caller, &wq, edges);
    }
    arraylist_free(&wq);
    edges_map = NULL;
    htable_t edges_map2;
    htable_new(&edges_map2, 0);
    htable_t edges_ids;
    size_t l = edges ? jl_array_len(edges) : 0;
    htable_new(&edges_ids, l);
    for (size_t i = 0; i < l / 2; i++) {
        jl_method_instance_t *caller = (jl_method_instance_t*)jl_array_ptr_ref(edges, i * 2);
        void *target = (void*)((char*)HT_NOTFOUND + i + 1);
        ptrhash_put(&edges_ids, (void*)caller, target);
    }
    // process target list to turn it into a memoized validity table
    // and compute the old methods list, ready for serialization
    jl_value_t *matches = NULL;
    jl_array_t *callee_ids = NULL;
    JL_GC_PUSH2(&matches, &callee_ids);
    for (size_t i = 0; i < l; i += 2) {
        jl_array_t *callees = (jl_array_t*)jl_array_ptr_ref(edges, i + 1);
        size_t l = jl_array_len(callees);
        callee_ids = jl_alloc_array_1d(jl_array_int32_type, l + 1);
        int32_t *idxs = (int32_t*)jl_array_data(callee_ids);
        idxs[0] = 0;
        size_t nt = 0;
        for (size_t j = 0; j < l; j += 2) {
            jl_value_t *invokeTypes = jl_array_ptr_ref(callees, j);
            jl_value_t *callee = jl_array_ptr_ref(callees, j + 1);
            assert(callee && "unsupported edge");

            if (jl_is_method_instance(callee)) {
                jl_methtable_t *mt = jl_method_get_table(((jl_method_instance_t*)callee)->def.method);
                if (!jl_object_in_image((jl_value_t*)mt))
                    continue;
            }

            // (nullptr, c) => call
            // (invokeTypes, c) => invoke
            // (nullptr, invokeTypes) => missing call
            // (invokeTypes, nullptr) => missing invoke (unused--inferred as Any)
            void *target = ptrhash_get(&edges_map2, invokeTypes ? (void*)invokeTypes : (void*)callee);
            if (target == HT_NOTFOUND) {
                size_t min_valid = 0;
                size_t max_valid = ~(size_t)0;
                if (invokeTypes) {
                    assert(jl_is_method_instance(callee));
                    jl_methtable_t *mt = jl_method_get_table(((jl_method_instance_t*)callee)->def.method);
                    if ((jl_value_t*)mt == jl_nothing) {
                        callee_ids = NULL; // invalid
                        break;
                    }
                    else {
                        matches = jl_gf_invoke_lookup_worlds(invokeTypes, (jl_value_t*)mt, world, &min_valid, &max_valid);
                        if (matches == jl_nothing) {
                            callee_ids = NULL; // invalid
                            break;
                        }
                        matches = (jl_value_t*)((jl_method_match_t*)matches)->method;
                    }
                }
                else {
                    jl_value_t *sig;
                    if (jl_is_method_instance(callee))
                        sig = ((jl_method_instance_t*)callee)->specTypes;
                    else
                        sig = callee;
                    int ambig = 0;
                    matches = jl_matching_methods((jl_tupletype_t*)sig, jl_nothing,
                            -1, 0, world, &min_valid, &max_valid, &ambig);
                    if (matches == jl_nothing) {
                        callee_ids = NULL; // invalid
                        break;
                    }
                    size_t k;
                    for (k = 0; k < jl_array_len(matches); k++) {
                        jl_method_match_t *match = (jl_method_match_t *)jl_array_ptr_ref(matches, k);
                        jl_array_ptr_set(matches, k, match->method);
                    }
                }
                jl_array_ptr_1d_push(ext_targets, invokeTypes);
                jl_array_ptr_1d_push(ext_targets, callee);
                jl_array_ptr_1d_push(ext_targets, matches);
                target = (void*)((char*)HT_NOTFOUND + jl_array_len(ext_targets) / 3);
                ptrhash_put(&edges_map2, (void*)callee, target);
            }
            idxs[++nt] = (char*)target - (char*)HT_NOTFOUND - 1;
        }
        jl_array_ptr_set(edges, i + 1, callee_ids); // swap callees for ids
        if (!callee_ids)
            continue;
        idxs[0] = nt;
        // record place of every method in edges
        // add method edges to the callee_ids list
        for (size_t j = 0; j < l; j += 2) {
            jl_value_t *callee = jl_array_ptr_ref(callees, j + 1);
            if (callee && jl_is_method_instance(callee)) {
                void *target = ptrhash_get(&edges_ids, (void*)callee);
                if (target != HT_NOTFOUND) {
                    idxs[++nt] = (char*)target - (char*)HT_NOTFOUND - 1;
                }
            }
        }
        jl_array_del_end(callee_ids, l - nt);
    }
    JL_GC_POP();
    htable_free(&edges_map2);
}

// Headers

// serialize information about all loaded modules
static void write_mod_list(ios_t *s, jl_array_t *a)
{
    size_t i;
    size_t len = jl_array_len(a);
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

// OPT_LEVEL should always be the upper bits
#define OPT_LEVEL 6

JL_DLLEXPORT uint8_t jl_cache_flags(void)
{
    // OOICCDDP
    uint8_t flags = 0;
    flags |= (jl_options.use_pkgimages & 1); // 0-bit
    flags |= (jl_options.debug_level & 3) << 1; // 1-2 bit
    flags |= (jl_options.check_bounds & 3) << 3; // 3-4 bit
    flags |= (jl_options.can_inline & 1) << 5; // 5-bit
    flags |= (jl_options.opt_level & 3) << OPT_LEVEL; // 6-7 bit
    return flags;
}

JL_DLLEXPORT uint8_t jl_match_cache_flags(uint8_t flags)
{
    // 1. Check which flags are relevant
    uint8_t current_flags = jl_cache_flags();
    uint8_t supports_pkgimage = (current_flags & 1);
    uint8_t is_pkgimage = (flags & 1);

    // For .ji packages ignore other flags
    if (!supports_pkgimage && !is_pkgimage) {
        return 1;
    }

    // 2. Check all flags, execept opt level must be exact
    uint8_t mask = (1 << OPT_LEVEL)-1;
    if ((flags & mask) != (current_flags & mask))
        return 0;
    // 3. allow for higher optimization flags in cache
    flags >>= OPT_LEVEL;
    current_flags >>= OPT_LEVEL;
    return flags >= current_flags;
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

// serialize information about the result of deserializing this file
static void write_worklist_for_header(ios_t *s, jl_array_t *worklist)
{
    int i, l = jl_array_len(worklist);
    for (i = 0; i < l; i++) {
        jl_module_t *workmod = (jl_module_t*)jl_array_ptr_ref(worklist, i);
        if (workmod->parent == jl_main_module || workmod->parent == workmod) {
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
    if (depmod->parent == jl_main_module || depmod->parent == depmod)
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

    // write a placeholder for total size so that we can quickly seek past all of the
    // dependencies if we don't need them
    initial_pos = ios_pos(s);
    write_uint64(s, 0);
    size_t i, l = udeps ? jl_array_len(udeps) : 0;
    for (i = 0; i < l; i++) {
        jl_value_t *deptuple = jl_array_ptr_ref(udeps, i);
        jl_value_t *dep = jl_fieldref(deptuple, 1);              // file abspath
        size_t slen = jl_string_len(dep);
        write_int32(s, slen);
        ios_write(s, jl_string_data(dep), slen);
        write_float64(s, jl_unbox_float64(jl_fieldref(deptuple, 2)));  // mtime
        jl_module_t *depmod = (jl_module_t*)jl_fieldref(deptuple, 0);  // evaluating module
        jl_module_t *depmod_top = depmod;
        while (depmod_top->parent != jl_main_module && depmod_top->parent != depmod_top)
            depmod_top = depmod_top->parent;
        unsigned provides = 0;
        size_t j, lj = jl_array_len(worklist);
        for (j = 0; j < lj; j++) {
            jl_module_t *workmod = (jl_module_t*)jl_array_ptr_ref(worklist, j);
            if (workmod->parent == jl_main_module || workmod->parent == workmod) {
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
    jl_value_t *prefs_hash = NULL;
    jl_value_t *prefs_list = NULL;
    JL_GC_PUSH1(&prefs_list);
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
        size_t i, l = jl_array_len(prefs_list);
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
    JL_GC_POP(); // for prefs_list

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
static void jl_insert_methods(jl_array_t *list)
{
    size_t i, l = jl_array_len(list);
    for (i = 0; i < l; i++) {
        jl_method_t *meth = (jl_method_t*)jl_array_ptr_ref(list, i);
        assert(jl_is_method(meth));
        assert(!meth->is_for_opaque_closure);
        jl_methtable_t *mt = jl_method_get_table(meth);
        assert((jl_value_t*)mt != jl_nothing);
        jl_method_table_insert(mt, meth, NULL);
    }
}

static void jl_copy_roots(jl_array_t *method_roots_list, uint64_t key)
{
    size_t i, l = jl_array_len(method_roots_list);
    for (i = 0; i < l; i+=2) {
        jl_method_t *m = (jl_method_t*)jl_array_ptr_ref(method_roots_list, i);
        jl_array_t *roots = (jl_array_t*)jl_array_ptr_ref(method_roots_list, i+1);
        if (roots) {
            assert(jl_is_array(roots));
            jl_append_method_roots(m, key, roots);
        }
    }
}

// verify that these edges intersect with the same methods as before
static jl_array_t *jl_verify_edges(jl_array_t *targets)
{
    size_t world = jl_atomic_load_acquire(&jl_world_counter);
    size_t i, l = jl_array_len(targets) / 3;
    jl_array_t *valids = jl_alloc_array_1d(jl_array_uint8_type, l);
    memset(jl_array_data(valids), 1, l);
    jl_value_t *loctag = NULL;
    jl_value_t *matches = NULL;
    JL_GC_PUSH3(&valids, &matches, &loctag);
    for (i = 0; i < l; i++) {
        jl_value_t *invokesig = jl_array_ptr_ref(targets, i * 3);
        jl_value_t *callee = jl_array_ptr_ref(targets, i * 3 + 1);
        jl_value_t *expected = jl_array_ptr_ref(targets, i * 3 + 2);
        int valid = 1;
        size_t min_valid = 0;
        size_t max_valid = ~(size_t)0;
        if (invokesig) {
            assert(callee && "unsupported edge");
            jl_methtable_t *mt = jl_method_get_table(((jl_method_instance_t*)callee)->def.method);
            if ((jl_value_t*)mt == jl_nothing) {
                valid = 0;
            }
            else {
                matches = jl_gf_invoke_lookup_worlds(invokesig, (jl_value_t*)mt, world, &min_valid, &max_valid);
                if (matches == jl_nothing) {
                     valid = 0;
                }
                else {
                    matches = (jl_value_t*)((jl_method_match_t*)matches)->method;
                    if (matches != expected) {
                        valid = 0;
                    }
                }
            }
        }
        else {
            jl_value_t *sig;
            if (jl_is_method_instance(callee))
                sig = ((jl_method_instance_t*)callee)->specTypes;
            else
                sig = callee;
            assert(jl_is_array(expected));
            int ambig = 0;
            // TODO: possibly need to included ambiguities too (for the optimizer correctness)?
            matches = jl_matching_methods((jl_tupletype_t*)sig, jl_nothing,
                    -1, 0, world, &min_valid, &max_valid, &ambig);
            if (matches == jl_nothing) {
                valid = 0;
            }
            else {
                // setdiff!(matches, expected)
                size_t j, k, ins = 0;
                if (jl_array_len(matches) != jl_array_len(expected)) {
                    valid = 0;
                }
                for (k = 0; k < jl_array_len(matches); k++) {
                    jl_method_t *match = ((jl_method_match_t*)jl_array_ptr_ref(matches, k))->method;
                    size_t l = jl_array_len(expected);
                    for (j = 0; j < l; j++)
                        if (match == (jl_method_t*)jl_array_ptr_ref(expected, j))
                            break;
                    if (j == l) {
                        // intersection has a new method or a method was
                        // deleted--this is now probably no good, just invalidate
                        // everything about it now
                        valid = 0;
                        if (!_jl_debug_method_invalidation)
                            break;
                        jl_array_ptr_set(matches, ins++, match);
                    }
                }
                if (!valid && _jl_debug_method_invalidation)
                    jl_array_del_end((jl_array_t*)matches, jl_array_len(matches) - ins);
            }
        }
        jl_array_uint8_set(valids, i, valid);
        if (!valid && _jl_debug_method_invalidation) {
            jl_array_ptr_1d_push(_jl_debug_method_invalidation, invokesig ? (jl_value_t*)invokesig : callee);
            loctag = jl_cstr_to_string("insert_backedges_callee");
            jl_array_ptr_1d_push(_jl_debug_method_invalidation, loctag);
            loctag = jl_box_int32((int32_t)i);
            jl_array_ptr_1d_push(_jl_debug_method_invalidation, loctag);
            jl_array_ptr_1d_push(_jl_debug_method_invalidation, matches);
        }
        //jl_static_show((JL_STREAM*)ios_stderr, (jl_value_t*)invokesig);
        //jl_static_show((JL_STREAM*)ios_stderr, (jl_value_t*)callee);
        //ios_puts(valid ? "valid\n" : "INVALID\n", ios_stderr);
    }
    JL_GC_POP();
    return valids;
}

// Combine all edges relevant to a method into the visited table
static void jl_verify_methods(jl_array_t *edges, jl_array_t *valids, htable_t *visited)
{
    jl_value_t *loctag = NULL;
    JL_GC_PUSH1(&loctag);
    size_t i, l = jl_array_len(edges) / 2;
    htable_new(visited, l);
    for (i = 0; i < l; i++) {
        jl_method_instance_t *caller = (jl_method_instance_t*)jl_array_ptr_ref(edges, 2 * i);
        assert(jl_is_method_instance(caller) && jl_is_method(caller->def.method));
        jl_array_t *callee_ids = (jl_array_t*)jl_array_ptr_ref(edges, 2 * i + 1);
        assert(jl_typeis((jl_value_t*)callee_ids, jl_array_int32_type));
        int valid = 1;
        if (callee_ids == NULL) {
            // serializing the edges had failed
            valid = 0;
        }
        else {
            int32_t *idxs = (int32_t*)jl_array_data(callee_ids);
            size_t j;
            for (j = 0; valid && j < idxs[0]; j++) {
                int32_t idx = idxs[j + 1];
                valid = jl_array_uint8_ref(valids, idx);
                if (!valid && _jl_debug_method_invalidation) {
                    jl_array_ptr_1d_push(_jl_debug_method_invalidation, (jl_value_t*)caller);
                    loctag = jl_cstr_to_string("verify_methods");
                    jl_array_ptr_1d_push(_jl_debug_method_invalidation, loctag);
                    loctag = jl_box_int32((int32_t)idx);
                    jl_array_ptr_1d_push(_jl_debug_method_invalidation, loctag);
                }
            }
        }
        ptrhash_put(visited, caller, (void*)(((char*)HT_NOTFOUND) + valid + 1));
        //jl_static_show((JL_STREAM*)ios_stderr, (jl_value_t*)caller);
        //ios_puts(valid ? "valid\n" : "INVALID\n", ios_stderr);
        // HT_NOTFOUND: valid (no invalid edges)
        // HT_NOTFOUND + 1: invalid
        // HT_NOTFOUND + 2: need to scan
        // HT_NOTFOUND + 3 + depth: in-progress
    }
    JL_GC_POP();
}


// Visit the entire call graph, starting from edges[idx] to determine if that method is valid
// Implements Tarjan's SCC (strongly connected components) algorithm, simplified to remove the count variable
static int jl_verify_graph_edge(jl_array_t *edges, int idx, htable_t *visited, arraylist_t *stack)
{
    jl_method_instance_t *caller = (jl_method_instance_t*)jl_array_ptr_ref(edges, idx * 2);
    assert(jl_is_method_instance(caller) && jl_is_method(caller->def.method));
    int found = (char*)ptrhash_get(visited, (void*)caller) - (char*)HT_NOTFOUND;
    if (found == 0)
        return 1; // NOTFOUND == valid
    if (found == 1)
        return 0; // invalid
    if (found != 2)
        return found - 1; // depth
    found = 0;
    jl_value_t *cause = NULL;
    arraylist_push(stack, (void*)caller);
    int depth = stack->len;
    ptrhash_put(visited, (void*)caller, (void*)((char*)HT_NOTFOUND + 3 + depth)); // change 2 to in-progress at depth
    jl_array_t *callee_ids = (jl_array_t*)jl_array_ptr_ref(edges, idx * 2 + 1);
    assert(jl_typeis((jl_value_t*)callee_ids, jl_array_int32_type));
    int32_t *idxs = (int32_t*)jl_array_data(callee_ids);
    int cycle = 0;
    size_t i, n = jl_array_len(callee_ids);
    for (i = idxs[0] + 1; i < n; i++) {
        int32_t idx = idxs[i];
        int child_found = jl_verify_graph_edge(edges, idx, visited, stack);
        if (child_found == 0) {
            // found what we were looking for, so terminate early
            found = 1;
            cause = jl_array_ptr_ref(edges, idx * 2);
            break;
        }
        else if (child_found >= 2 && child_found - 2 < cycle) {
            // record the cycle will resolve at depth "cycle"
            cycle = child_found - 2;
            assert(cycle);
        }
    }
    if (!found && cycle && cycle != depth)
        return cycle + 2;
    // If we are the top of the current cycle, now mark all other parts of
    // our cycle with what we found.
    // Or if we found a backedge, also mark all of the other parts of the
    // cycle as also having an backedge.
    while (stack->len >= depth) {
        void *mi = arraylist_pop(stack);
        assert((char*)ptrhash_get(visited, mi) - (char*)HT_NOTFOUND == 4 + stack->len);
        if (found)
            ptrhash_put(visited, mi, (void*)((char*)HT_NOTFOUND + 1 + found));
        else
            ptrhash_remove(visited, mi); // assign as NOTFOUND in table
        if (_jl_debug_method_invalidation && found) {
            jl_value_t *loctag = NULL;
            JL_GC_PUSH1(&loctag);
            jl_array_ptr_1d_push(_jl_debug_method_invalidation, (jl_value_t*)mi);
            loctag = jl_cstr_to_string("verify_methods");
            jl_array_ptr_1d_push(_jl_debug_method_invalidation, loctag);
            jl_array_ptr_1d_push(_jl_debug_method_invalidation, (jl_value_t*)cause);
            JL_GC_POP();
        }
    }
    return found ? 0 : 1;
}

// Visit all entries in edges, verify if they are valid
static jl_array_t *jl_verify_graph(jl_array_t *edges, htable_t *visited)
{
    arraylist_t stack;
    arraylist_new(&stack, 0);
    size_t i, n = jl_array_len(edges) / 2;
    jl_array_t *valids = jl_alloc_array_1d(jl_array_uint8_type, n);
    JL_GC_PUSH1(&valids);
    int8_t *valids_data = (int8_t*)jl_array_data(valids);
    for (i = 0; i < n; i++)
        valids_data[i] = jl_verify_graph_edge(edges, i, visited, &stack);
    arraylist_free(&stack);
    JL_GC_POP();
    return valids;
}

// Restore backedges to external targets
// `edges` = [caller1, targets_indexes1, ...], the list of worklist-owned methods calling external methods.
// `ext_targets` is [invokesig1, callee1, matches1, ...], the global set of non-worklist callees of worklist-owned methods.
static void jl_insert_backedges(jl_array_t *edges, jl_array_t *ext_targets, jl_array_t *ci_list)
{
    // determine which CodeInstance objects are still valid in our image
    size_t world = jl_atomic_load_acquire(&jl_world_counter);
    jl_array_t *valids = jl_verify_edges(ext_targets);
    JL_GC_PUSH1(&valids);
    htable_t visited;
    htable_new(&visited, 0);
    jl_verify_methods(edges, valids, &visited); // consumes valids, creates visited
    valids = jl_verify_graph(edges, &visited); // consumes visited, creates valids
    size_t i, l;

    // next build a map from external MethodInstances to their CodeInstance for insertion
    l = jl_array_len(ci_list);
    htable_reset(&visited, l);
    for (i = 0; i < l; i++) {
        jl_code_instance_t *ci = (jl_code_instance_t*)jl_array_ptr_ref(ci_list, i);
        assert(ci->min_world == world);
        if (ci->max_world == 1) { // sentinel value: has edges to external callables
            ptrhash_put(&visited, (void*)ci->def, (void*)ci);
        }
        else {
            assert(ci->max_world == ~(size_t)0);
            jl_method_instance_t *caller = ci->def;
            if (ci->inferred && jl_rettype_inferred(caller, world, ~(size_t)0) == jl_nothing) {
                jl_mi_cache_insert(caller, ci);
            }
            //jl_static_show((jl_stream*)ios_stderr, (jl_value_t*)caller);
            //ios_puts("free\n", ios_stderr);
        }
    }

    // next enable any applicable new codes
    l = jl_array_len(edges) / 2;
    for (i = 0; i < l; i++) {
        jl_method_instance_t *caller = (jl_method_instance_t*)jl_array_ptr_ref(edges, 2 * i);
        int valid = jl_array_uint8_ref(valids, i);
        if (!valid)
            continue;
        // if this callee is still valid, add all the backedges
        jl_array_t *callee_ids = (jl_array_t*)jl_array_ptr_ref(edges, 2 * i + 1);
        int32_t *idxs = (int32_t*)jl_array_data(callee_ids);
        for (size_t j = 0; j < idxs[0]; j++) {
            int32_t idx = idxs[j + 1];
            jl_value_t *invokesig = jl_array_ptr_ref(ext_targets, idx * 3);
            jl_value_t *callee = jl_array_ptr_ref(ext_targets, idx * 3 + 1);
            if (callee && jl_is_method_instance(callee)) {
                jl_method_instance_add_backedge((jl_method_instance_t*)callee, invokesig, caller);
            }
            else {
                jl_value_t *sig = callee == NULL ? invokesig : callee;
                jl_methtable_t *mt = jl_method_table_for(sig);
                // FIXME: rarely, `callee` has an unexpected `Union` signature,
                // see https://github.com/JuliaLang/julia/pull/43990#issuecomment-1030329344
                // Fix the issue and turn this back into an `assert((jl_value_t*)mt != jl_nothing)`
                // This workaround exposes us to (rare) 265-violations.
                if ((jl_value_t*)mt != jl_nothing)
                    jl_method_table_add_backedge(mt, sig, (jl_value_t*)caller);
            }
        }
        // then enable any methods associated with it
        void *ci = ptrhash_get(&visited, (void*)caller);
        //assert(ci != HT_NOTFOUND);
        if (ci != HT_NOTFOUND) {
            // have some new external code to use
            assert(jl_is_code_instance(ci));
            jl_code_instance_t *codeinst = (jl_code_instance_t*)ci;
            assert(codeinst->min_world == world && codeinst->inferred);
            codeinst->max_world = ~(size_t)0;
            if (jl_rettype_inferred(caller, world, ~(size_t)0) == jl_nothing) {
                jl_mi_cache_insert(caller, codeinst);
            }
        }
    }

    htable_free(&visited);
    JL_GC_POP();
}

static void classify_callers(htable_t *callers_with_edges, jl_array_t *edges)
{
    size_t l = edges ? jl_array_len(edges) / 2 : 0;
    for (size_t i = 0; i < l; i++) {
        jl_method_instance_t *caller = (jl_method_instance_t*)jl_array_ptr_ref(edges, 2 * i);
        ptrhash_put(callers_with_edges, (void*)caller, (void*)caller);
    }
}

static jl_value_t *read_verify_mod_list(ios_t *s, jl_array_t *depmods)
{
    if (!jl_main_module->build_id.lo) {
        return jl_get_exceptionf(jl_errorexception_type,
                "Main module uuid state is invalid for module deserialization.");
    }
    size_t i, l = jl_array_len(depmods);
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
    assert(jl_array_len(depmods) < INT32_MAX && "too many dependencies to serialize");
    size_t lbids = n_linkage_blobs();
    size_t ldeps = jl_array_len(depmods);
    jl_array_t *depmodidxs = jl_alloc_array_1d(jl_array_int32_type, lbids);
    int32_t *dmidxs = (int32_t*)jl_array_data(depmodidxs);
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
    size_t ldeps = jl_array_len(depmods);
    jl_array_t *imageidxs = jl_alloc_array_1d(jl_array_int32_type, ldeps + 1);
    int32_t *imgidxs = (int32_t*)jl_array_data(imageidxs);
    imgidxs[0] = 0;
    for (size_t i = 0; i < ldeps; i++) {
        jl_value_t *depmod = jl_array_ptr_ref(depmods, i);
        size_t j = external_blob_index(depmod);
        assert(j < INT32_MAX);
        imgidxs[i + 1] = (int32_t)j;
    }
    return imageidxs;
}

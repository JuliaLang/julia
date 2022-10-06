static htable_t new_code_instance_validate;

uint64_t jl_worklist_key(jl_array_t *worklist)
{
    assert(jl_is_array(worklist));
    size_t len = jl_array_len(worklist);
    if (len > 0) {
        jl_module_t *topmod = (jl_module_t*)jl_array_ptr_ref(worklist, len-1);
        assert(jl_is_module(topmod));
        return topmod->build_id;
    }
    return 0;
}

static int method_instance_in_queue(jl_method_instance_t *mi)
{
    return ptrhash_get(&external_mis, mi) != HT_NOTFOUND;
}

// compute whether a type references something internal to worklist
// and thus could not have existed before deserialize
// and thus does not need delayed unique-ing
static int type_in_worklist(jl_datatype_t *dt) JL_NOTSAFEPOINT
{
    if (!jl_object_in_image(dt->name->module))
        return 1;
    int i, l = jl_svec_len(dt->parameters);
    for (i = 0; i < l; i++) {
        jl_value_t *p = jl_unwrap_unionall(jl_tparam(dt, i));
        // TODO: what about Union and TypeVar??
        if (type_in_worklist((jl_datatype_t*)(jl_is_datatype(p) ? p : jl_typeof(p))))
            return 1;
    }
    return 0;
}


static void mark_backedges_in_worklist(jl_method_instance_t *mi, htable_t *visited, int found)
{
    int oldfound = (char*)ptrhash_get(visited, mi) - (char*)HT_NOTFOUND;
    if (oldfound < 3)
        return; // not in-progress
    ptrhash_put(visited, mi, (void*)((char*)HT_NOTFOUND + 1 + found));
#ifndef NDEBUG
    jl_module_t *mod = mi->def.module;
    if (jl_is_method(mod))
        mod = ((jl_method_t*)mod)->module;
    assert(jl_is_module(mod));
    assert(!mi->precompiled && !!jl_object_in_image(mod));
    assert(mi->backedges);
#endif
    size_t i = 0, n = jl_array_len(mi->backedges);
    while (i < n) {
        jl_method_instance_t *be;
        i = get_next_edge(mi->backedges, i, NULL, &be);
        mark_backedges_in_worklist(be, visited, found);
    }
}

// When we infer external method instances, ensure they link back to the
// package. Otherwise they might be, e.g., for external macros
static int has_backedge_to_worklist(jl_method_instance_t *mi, htable_t *visited, int depth)
{
    jl_module_t *mod = mi->def.module;
    if (jl_is_method(mod))
        mod = ((jl_method_t*)mod)->module;
    assert(jl_is_module(mod));
    if (mi->precompiled || !jl_object_in_image(mod)) {
        return 1;
    }
    if (!mi->backedges) {
        return 0;
    }
    void **bp = ptrhash_bp(visited, mi);
    // HT_NOTFOUND: not yet analyzed
    // HT_NOTFOUND + 1: no link back
    // HT_NOTFOUND + 2: does link back
    // HT_NOTFOUND + 3 + depth: in-progress
    int found = (char*)*bp - (char*)HT_NOTFOUND;
    if (found)
        return found - 1;
    *bp = (void*)((char*)HT_NOTFOUND + 3 + depth); // preliminarily mark as in-progress
    size_t i = 0, n = jl_array_len(mi->backedges);
    int cycle = 0;
    while (i < n) {
        jl_method_instance_t *be;
        i = get_next_edge(mi->backedges, i, NULL, &be);
        int child_found = has_backedge_to_worklist(be, visited, depth + 1);
        if (child_found == 1) {
            found = 1;
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
    bp = ptrhash_bp(visited, mi); // re-acquire since rehashing might change the location
    *bp = (void*)((char*)HT_NOTFOUND + 1 + found);
    if (cycle) {
        // If we are the top of the current cycle, now mark all other parts of
        // our cycle by re-walking the backedges graph and marking all WIP
        // items as found.
        // Be careful to only re-walk as far as we had originally scanned above.
        // Or if we found a backedge, also mark all of the other parts of the
        // cycle as also having an backedge.
        n = i;
        i = 0;
        while (i < n) {
            jl_method_instance_t *be;
            i = get_next_edge(mi->backedges, i, NULL, &be);
            mark_backedges_in_worklist(be, visited, found);
        }
    }
    return found;
}

// given the list of MethodInstances that were inferred during the
// build, select those that are external and have at least one
// relocatable CodeInstance and are inferred to be called from the worklist
// or explicitly added by a precompile statement.
// Also prepares external_mis for method_instance_in_queue queries.
static jl_array_t *queue_external_mis(jl_array_t *list)
{
    if (list == NULL)
        return NULL;
    size_t i, n = 0;
    htable_t visited;
    assert(jl_is_array(list));
    size_t n0 = jl_array_len(list);
    htable_new(&visited, n0);
    for (i = 0; i < n0; i++) {
        jl_method_instance_t *mi = (jl_method_instance_t*)jl_array_ptr_ref(list, i);
        assert(jl_is_method_instance(mi));
        if (jl_is_method(mi->def.value)) {
            jl_method_t *m = mi->def.method;
            if (!!jl_object_in_image(m->module)) {
                jl_code_instance_t *ci = mi->cache;
                while (ci) {
                    if (ci->max_world == ~(size_t)0 && ci->relocatability && ci->inferred)
                        break;
                    ci = jl_atomic_load_relaxed(&ci->next);
                }
                if (ci && ptrhash_get(&external_mis, mi) == HT_NOTFOUND) {
                    int found = has_backedge_to_worklist(mi, &visited, 1);
                    assert(found == 0 || found == 1);
                    if (found == 1) {
                        ptrhash_put(&external_mis, mi, ci);
                        n++;
                    }
                }
            }
        }
    }
    htable_free(&visited);
    if (n == 0)
        return NULL;
    jl_array_t *mi_list = jl_alloc_vec_any(n);
    n = 0;
    for (size_t i = 0; i < external_mis.size; i += 2) {
        void *ci = external_mis.table[i+1];
        if (ci != HT_NOTFOUND) {
            jl_array_ptr_set(mi_list, n++, (jl_value_t*)ci);
        }
    }
    assert(n == jl_array_len(mi_list));
    return mi_list;
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
            jl_array_t **edges = (jl_array_t**)ptrhash_bp(&edges_map, (void*)caller);
            if (*edges == HT_NOTFOUND)
                *edges = jl_alloc_vec_any(0);
            jl_array_ptr_1d_push(*edges, NULL);
            jl_array_ptr_1d_push(*edges, missing_callee);
        }
    }
}


// from MethodInstances
static void collect_backedges(jl_method_instance_t *callee, int internal) JL_GC_DISABLED
{
    jl_array_t *backedges = callee->backedges;
    if (backedges) {
        size_t i = 0, l = jl_array_len(backedges);
        while (i < l) {
            jl_value_t *invokeTypes;
            jl_method_instance_t *caller;
            i = get_next_edge(backedges, i, &invokeTypes, &caller);
            jl_array_t **edges = (jl_array_t**)ptrhash_bp(&edges_map, caller);
            if (*edges == HT_NOTFOUND)
                *edges = jl_alloc_vec_any(0);
            jl_array_ptr_1d_push(*edges, invokeTypes);
            jl_array_ptr_1d_push(*edges, (jl_value_t*)callee);
        }
    }
}


// For functions owned by modules not on the worklist, call this on each method.
// - if the method is owned by a worklist module, add it to the list of things to be
//   fully serialized
// - Collect all backedges (may be needed later when we invert this list).
static int jl_collect_methcache_from_mod(jl_typemap_entry_t *ml, void *closure) JL_GC_DISABLED
{
    jl_array_t *s = (jl_array_t*)closure;
    jl_method_t *m = ml->func.method;
    if (s && !jl_object_in_image(m->module)) {
        jl_array_ptr_1d_push(s, (jl_value_t*)m);
    }
    jl_svec_t *specializations = m->specializations;
    size_t i, l = jl_svec_len(specializations);
    for (i = 0; i < l; i++) {
        jl_method_instance_t *callee = (jl_method_instance_t*)jl_svecref(specializations, i);
        if ((jl_value_t*)callee != jl_nothing)
            collect_backedges(callee, !s);
    }
    return 1;
}

static void jl_collect_methtable_from_mod(jl_array_t *s, jl_methtable_t *mt) JL_GC_DISABLED
{
    jl_typemap_visitor(mt->defs, jl_collect_methcache_from_mod, (void*)s);
}

// Collect methods of external functions defined by modules in the worklist
// "extext" = "extending external"
// Also collect relevant backedges
static void jl_collect_extext_methods_from_mod(jl_array_t *s, jl_module_t *m) JL_GC_DISABLED
{
    if (s && !jl_object_in_image(m))
        s = NULL; // do not collect any methods
    size_t i;
    void **table = m->bindings.table;
    for (i = 1; i < m->bindings.size; i += 2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            if (b->owner == m && b->value && b->constp) {
                jl_value_t *bv = jl_unwrap_unionall(b->value);
                if (jl_is_datatype(bv)) {
                    jl_typename_t *tn = ((jl_datatype_t*)bv)->name;
                    if (tn->module == m && tn->name == b->name && tn->wrapper == b->value) {
                        jl_methtable_t *mt = tn->mt;
                        if (mt != NULL &&
                                (jl_value_t*)mt != jl_nothing &&
                                (mt != jl_type_type_mt && mt != jl_nonfunction_mt)) {
                            assert(mt->module == tn->module);
                            jl_collect_methtable_from_mod(s, mt);
                            if (s)
                                jl_collect_missing_backedges(mt);
                        }
                    }
                }
                else if (jl_is_module(b->value)) {
                    jl_module_t *child = (jl_module_t*)b->value;
                    if (child != m && child->parent == m && child->name == b->name) {
                        // this is the original/primary binding for the submodule
                        jl_collect_extext_methods_from_mod(s, (jl_module_t*)b->value);
                    }
                }
                else if (jl_is_mtable(b->value)) {
                    jl_methtable_t *mt = (jl_methtable_t*)b->value;
                    if (mt->module == m && mt->name == b->name) {
                        // this is probably an external method table, so let's assume so
                        // as there is no way to precisely distinguish them,
                        // and the rest of this serializer does not bother
                        // to handle any method tables specially
                        jl_collect_methtable_from_mod(s, (jl_methtable_t*)bv);
                    }
                }
            }
        }
    }
}

static void jl_record_edges(jl_method_instance_t *caller, arraylist_t *wq, jl_array_t *edges) JL_GC_DISABLED
{
    jl_array_t *callees = (jl_array_t*)ptrhash_get(&edges_map, (void*)caller);
    if (callees != HT_NOTFOUND) {
        ptrhash_remove(&edges_map, (void*)caller);
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
}


// Extract `edges` and `ext_targets` from `edges_map`
// `edges` = [caller1, targets_indexes1, ...], the list of methods and their edges
// `ext_targets` is [invokesig1, callee1, matches1, ...], the edges for each target
static void jl_collect_edges(jl_array_t *edges, jl_array_t *ext_targets)
{
    size_t world = jl_atomic_load_acquire(&jl_world_counter);
    arraylist_t wq;
    arraylist_new(&wq, 0);
    void **table = edges_map.table;    // edges is caller => callees
    size_t table_size = edges_map.size;
    for (size_t i = 0; i < table_size; i += 2) {
        assert(table == edges_map.table && table_size == edges_map.size &&
               "edges_map changed during iteration");
        jl_method_instance_t *caller = (jl_method_instance_t*)table[i];
        jl_array_t *callees = (jl_array_t*)table[i + 1];
        if (callees == HT_NOTFOUND)
            continue;
        assert(jl_is_method_instance(caller) && jl_is_method(caller->def.method));
        if (!jl_object_in_image(caller->def.method->module) ||
            method_instance_in_queue(caller)) {
            jl_record_edges(caller, &wq, edges);
        }
    }
    while (wq.len) {
        jl_method_instance_t *caller = (jl_method_instance_t*)arraylist_pop(&wq);
        jl_record_edges(caller, &wq, edges);
    }
    arraylist_free(&wq);
    htable_reset(&edges_map, 0);
    htable_t edges_ids;
    size_t l = jl_array_len(edges);
    htable_new(&edges_ids, l);
    for (size_t i = 0; i < l / 2; i++) {
        jl_method_instance_t *caller = (jl_method_instance_t*)jl_array_ptr_ref(edges, i * 2);
        void *target = (void*)((char*)HT_NOTFOUND + i + 1);
        ptrhash_put(&edges_ids, (void*)caller, target);
    }
    // process target list to turn it into a memoized validity table
    // and compute the old methods list, ready for serialization
    for (size_t i = 0; i < l; i += 2) {
        jl_array_t *callees = (jl_array_t*)jl_array_ptr_ref(edges, i + 1);
        size_t l = jl_array_len(callees);
        jl_array_t *callee_ids = jl_alloc_array_1d(jl_array_int32_type, l + 1);
        int32_t *idxs = (int32_t*)jl_array_data(callee_ids);
        idxs[0] = 0;
        size_t nt = 0;
        for (size_t j = 0; j < l; j += 2) {
            jl_value_t *invokeTypes = jl_array_ptr_ref(callees, j);
            jl_value_t *callee = jl_array_ptr_ref(callees, j + 1);
            assert(callee && "unsupported edge");

            if (jl_is_method_instance(callee)) {
                jl_methtable_t *mt = jl_method_get_table(((jl_method_instance_t*)callee)->def.method);
                if (!jl_object_in_image(mt->module))
                    continue;
            }

            // (nullptr, c) => call
            // (invokeTypes, c) => invoke
            // (nullptr, invokeTypes) => missing call
            // (invokeTypes, nullptr) => missing invoke (unused--inferred as Any)
            void *target = ptrhash_get(&edges_map, invokeTypes ? (void*)invokeTypes : (void*)callee);
            if (target == HT_NOTFOUND) {
                jl_value_t *matches;
                size_t min_valid = 0;
                size_t max_valid = ~(size_t)0;
                if (invokeTypes) {
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
                    if (matches == jl_false) {
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
                ptrhash_put(&edges_map, (void*)callee, target);
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
    htable_reset(&edges_map, 0);
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
        if (!!jl_object_in_image(m)) {
            const char *modname = jl_symbol_name(m->name);
            size_t l = strlen(modname);
            write_int32(s, l);
            ios_write(s, modname, l);
            write_uint64(s, m->uuid.hi);
            write_uint64(s, m->uuid.lo);
            write_uint64(s, m->build_id);
        }
    }
    write_int32(s, 0);
}

// "magic" string and version header of .ji file
static const int JI_FORMAT_VERSION = 11;
static const char JI_MAGIC[] = "\373jli\r\n\032\n"; // based on PNG signature
static const uint16_t BOM = 0xFEFF; // byte-order marker
static void write_header(ios_t *s)
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
}

// serialize information about the result of deserializing this file
static void write_worklist_for_header(ios_t *s)
{
    int i, l = jl_array_len(serializer_worklist);
    for (i = 0; i < l; i++) {
        jl_module_t *workmod = (jl_module_t*)jl_array_ptr_ref(serializer_worklist, i);
        if (workmod->parent == jl_main_module || workmod->parent == workmod) {
            size_t l = strlen(jl_symbol_name(workmod->name));
            write_int32(s, l);
            ios_write(s, jl_symbol_name(workmod->name), l);
            write_uint64(s, workmod->uuid.hi);
            write_uint64(s, workmod->uuid.lo);
            write_uint64(s, workmod->build_id);
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
static int64_t write_dependency_list(ios_t *s, jl_array_t **udepsp)
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
    if (udeps) {
        size_t i, l = jl_array_len(udeps);
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
            size_t j, lj = jl_array_len(serializer_worklist);
            for (j = 0; j < lj; j++) {
                jl_module_t *workmod = (jl_module_t*)jl_array_ptr_ref(serializer_worklist, j);
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
        } else {
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
    }
    return pos;
}


// Deserialization

// Add methods to external (non-worklist-owned) functions
static void jl_insert_methods(jl_array_t *list)
{
    size_t i, l = jl_array_len(list);
    for (i = 0; i < l; i += 2) {
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
    size_t i, j, l;
    for (i = 0; i < jl_array_len(method_roots_list); i+=2) {
        jl_method_t *m = (jl_method_t*)jl_array_ptr_ref(method_roots_list, i);
        jl_array_t *roots = (jl_array_t*)jl_array_ptr_ref(method_roots_list, i+1);
        if (roots) {
            assert(jl_is_array(roots));
            jl_append_method_roots(m, key, roots);
        }
    }
}

int remove_code_instance_from_validation(jl_code_instance_t *codeinst)
{
    return ptrhash_remove(&new_code_instance_validate, codeinst);
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
                break;
            }
            matches = jl_gf_invoke_lookup_worlds(invokesig, (jl_value_t*)mt, world, &min_valid, &max_valid);
            if (matches == jl_nothing) {
                 valid = 0;
                 break;
            }
            matches = (jl_value_t*)((jl_method_match_t*)matches)->method;
            if (matches != expected) {
                valid = 0;
                break;
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
            if (matches == jl_false) {
                valid = 0;
                break;
            }
            // setdiff!(matches, expected)
            size_t j, k, ins = 0;
            if (jl_array_len(matches) != jl_array_len(expected)) {
                valid = 0;
                if (!_jl_debug_method_invalidation)
                    break;
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
                    jl_array_ptr_set(matches, ins++, match);
                }
            }
            jl_array_del_end((jl_array_t*)matches, jl_array_len(matches) - ins);
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
void jl_verify_methods(jl_array_t *edges, jl_array_t *valids, htable_t *visited)
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


// Propagate the result of cycle-resolution to all edges (recursively)
static int mark_edges_in_worklist(jl_array_t *edges, int idx, jl_method_instance_t *cycle, htable_t *visited, int found)
{
    jl_method_instance_t *caller = (jl_method_instance_t*)jl_array_ptr_ref(edges, idx * 2);
    int oldfound = (char*)ptrhash_get(visited, caller) - (char*)HT_NOTFOUND;
    if (oldfound < 3)
        return 0; // not in-progress
    if (!found) {
        ptrhash_remove(visited, (void*)caller);
    }
    else {
        ptrhash_put(visited, (void*)caller, (void*)((char*)HT_NOTFOUND + 1 + found));
    }
    jl_array_t *callee_ids = (jl_array_t*)jl_array_ptr_ref(edges, idx * 2 + 1);
    assert(jl_typeis((jl_value_t*)callee_ids, jl_array_int32_type));
    int32_t *idxs = (int32_t*)jl_array_data(callee_ids);
    size_t i, badidx = 0, n = jl_array_len(callee_ids);
    for (i = idxs[0] + 1; i < n; i++) {
        if (mark_edges_in_worklist(edges, idxs[i], cycle, visited, found) && badidx == 0)
            badidx = i - idxs[0];
    }
    if (_jl_debug_method_invalidation) {
        jl_value_t *loctag = NULL;
        JL_GC_PUSH1(&loctag);
        jl_array_ptr_1d_push(_jl_debug_method_invalidation, (jl_value_t*)caller);
        loctag = jl_cstr_to_string("verify_methods");
        jl_array_ptr_1d_push(_jl_debug_method_invalidation, loctag);
        jl_method_instance_t *callee = cycle;
        if (badidx--)
            callee = (jl_method_instance_t*)jl_array_ptr_ref(edges, 2 * badidx);
        jl_array_ptr_1d_push(_jl_debug_method_invalidation, (jl_value_t*)callee);
        JL_GC_POP();
    }
    return 1;
}


// Visit the entire call graph, starting from edges[idx] to determine if that method is valid
static int jl_verify_graph_edge(jl_array_t *edges, int idx, htable_t *visited, int depth)
{
    jl_method_instance_t *caller = (jl_method_instance_t*)jl_array_ptr_ref(edges, idx * 2);
    assert(jl_is_method_instance(caller) && jl_is_method(caller->def.method));
    int found = (char*)ptrhash_get(visited, (void*)caller) - (char*)HT_NOTFOUND;
    if (found == 0)
        return 1; // valid
    if (found == 1)
        return 0; // invalid
    if (found != 2)
        return found - 1; // depth
    found = 0;
    ptrhash_put(visited, (void*)caller, (void*)((char*)HT_NOTFOUND + 3 + depth)); // change 2 to in-progress at depth
    jl_array_t *callee_ids = (jl_array_t*)jl_array_ptr_ref(edges, idx * 2 + 1);
    assert(jl_typeis((jl_value_t*)callee_ids, jl_array_int32_type));
    int32_t *idxs = (int32_t*)jl_array_data(callee_ids);
    int cycle = 0;
    size_t i, n = jl_array_len(callee_ids);
    for (i = idxs[0] + 1; i < n; i++) {
        int32_t idx = idxs[i];
        int child_found = jl_verify_graph_edge(edges, idx, visited, depth + 1);
        if (child_found == 0) {
            found = 1;
            if (_jl_debug_method_invalidation) {
                jl_value_t *loctag = NULL;
                JL_GC_PUSH1(&loctag);
                jl_array_ptr_1d_push(_jl_debug_method_invalidation, (jl_value_t*)caller);
                loctag = jl_cstr_to_string("verify_methods");
                jl_array_ptr_1d_push(_jl_debug_method_invalidation, loctag);
                jl_array_ptr_1d_push(_jl_debug_method_invalidation, jl_array_ptr_ref(edges, idx * 2));
                JL_GC_POP();
            }
            break;
        }
        else if (child_found >= 2 && child_found - 2 < cycle) {
            // record the cycle will resolve at depth "cycle"
            cycle = child_found - 2;
            assert(cycle);
        }
    }
    if (!found) {
        if (cycle && cycle != depth)
            return cycle + 2;
        ptrhash_remove(visited, (void*)caller);
    }
    else { // found invalid
        ptrhash_put(visited, (void*)caller, (void*)((char*)HT_NOTFOUND + 1 + found));
    }
    if (cycle) {
        // If we are the top of the current cycle, now mark all other parts of
        // our cycle by re-walking the backedges graph and marking all WIP
        // items as found.
        // Be careful to only re-walk as far as we had originally scanned above.
        // Or if we found a backedge, also mark all of the other parts of the
        // cycle as also having an backedge.
        n = i;
        for (i = idxs[0] + 1; i < n; i++) {
            mark_edges_in_worklist(edges, idxs[i], caller, visited, found);
        }
    }
    return found ? 0 : 1;
}

// Visit all entries in edges, verify if they are valid
static jl_array_t *jl_verify_graph(jl_array_t *edges, htable_t *visited)
{
    size_t i, n = jl_array_len(edges) / 2;
    jl_array_t *valids = jl_alloc_array_1d(jl_array_uint8_type, n);
    JL_GC_PUSH1(&valids);
    int8_t *valids_data = (int8_t*)jl_array_data(valids);
    for (i = 0; i < n; i++) {
        valids_data[i] = jl_verify_graph_edge(edges, i, visited, 1);
    }
    JL_GC_POP();
    return valids;
}

// Restore backedges to external targets
// `edges` = [caller1, targets_indexes1, ...], the list of worklist-owned methods calling external methods.
// `ext_targets` is [invokesig1, callee1, matches1, ...], the global set of non-worklist callees of worklist-owned methods.
static void jl_insert_backedges(jl_array_t *edges, jl_array_t *ext_targets, jl_array_t *mi_list)
{
    // determine which CodeInstance objects are still valid in our image
    size_t world = jl_atomic_load_acquire(&jl_world_counter);
    jl_array_t *valids = jl_verify_edges(ext_targets);
    JL_GC_PUSH1(&valids);
    htable_t visited;
    htable_new(&visited, 0);
    jl_verify_methods(edges, valids, &visited);
    valids = jl_verify_graph(edges, &visited);
    size_t i, l = jl_array_len(edges) / 2;

    // next build a map from external_mis to their CodeInstance for insertion
    if (mi_list == NULL) {
        htable_reset(&visited, 0);
    }
    else {
        size_t i, l = jl_array_len(mi_list);
        htable_reset(&visited, l);
        for (i = 0; i < l; i++) {
            jl_code_instance_t *ci = (jl_code_instance_t*)jl_array_ptr_ref(mi_list, i);
            ptrhash_put(&visited, (void*)ci->def, (void*)ci);
        }
    }

    // next disable any invalid codes, so we do not try to enable them
    for (i = 0; i < l; i++) {
        jl_method_instance_t *caller = (jl_method_instance_t*)jl_array_ptr_ref(edges, 2 * i);
        assert(jl_is_method_instance(caller) && jl_is_method(caller->def.method));
        int valid = jl_array_uint8_ref(valids, i);
        if (valid)
            continue;
        void *ci = ptrhash_get(&visited, (void*)caller);
        if (ci != HT_NOTFOUND) {
            assert(jl_is_code_instance(ci));
            remove_code_instance_from_validation((jl_code_instance_t*)ci); // mark it as handled
        }
        else {
            jl_code_instance_t *codeinst = caller->cache;
            while (codeinst) {
                remove_code_instance_from_validation(codeinst); // should be left invalid
                codeinst = jl_atomic_load_relaxed(&codeinst->next);
            }
        }
    }

    // finally enable any applicable new codes
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
        // then enable it
        void *ci = ptrhash_get(&visited, (void*)caller);
        if (ci != HT_NOTFOUND) {
            // have some new external code to use
            assert(jl_is_code_instance(ci));
            jl_code_instance_t *codeinst = (jl_code_instance_t*)ci;
            remove_code_instance_from_validation(codeinst); // mark it as handled
            assert(codeinst->min_world >= world && codeinst->inferred);
            codeinst->max_world = ~(size_t)0;
            if (jl_rettype_inferred(caller, world, ~(size_t)0) == jl_nothing) {
                jl_mi_cache_insert(caller, codeinst);
            }
        }
        else {
            jl_code_instance_t *codeinst = caller->cache;
            while (codeinst) {
                if (remove_code_instance_from_validation(codeinst)) { // mark it as handled
                    assert(codeinst->min_world >= world && codeinst->inferred);
                    codeinst->max_world = ~(size_t)0;
                }
                codeinst = jl_atomic_load_relaxed(&codeinst->next);
            }
        }
    }

    htable_free(&visited);
    JL_GC_POP();
}

static void classify_callers(htable_t *callers_with_edges, jl_array_t *edges)
{
    size_t l = jl_array_len(edges) / 2;
    for (size_t i = 0; i < l; i++) {
        jl_method_instance_t *caller = (jl_method_instance_t*)jl_array_ptr_ref(edges, 2 * i);
        ptrhash_put(callers_with_edges, (void*)caller, (void*)caller);
    }
}

static void validate_new_code_instances(void)
{
    size_t world = jl_atomic_load_acquire(&jl_world_counter);
    size_t i;
    for (i = 0; i < new_code_instance_validate.size; i += 2) {
        if (new_code_instance_validate.table[i+1] != HT_NOTFOUND) {
            jl_code_instance_t *ci = (jl_code_instance_t*)new_code_instance_validate.table[i];
            JL_GC_PROMISE_ROOTED(ci); // TODO: this needs a root (or restructuring to avoid it)
            assert(ci->min_world >= world && ci->inferred);
            ci->max_world = ~(size_t)0;
            jl_method_instance_t *caller = ci->def;
            if (jl_rettype_inferred(caller, world, ~(size_t)0) == jl_nothing) {
                jl_mi_cache_insert(caller, ci);
            }
            //jl_static_show((JL_STREAM*)ios_stderr, (jl_value_t*)caller);
            //ios_puts("FREE\n", ios_stderr);
        }
    }
}

static jl_value_t *read_verify_mod_list(ios_t *s, jl_array_t *mod_list)
{
    if (!jl_main_module->build_id) {
        return jl_get_exceptionf(jl_errorexception_type,
                "Main module uuid state is invalid for module deserialization.");
    }
    size_t i, l = jl_array_len(mod_list);
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
        uint64_t build_id = read_uint64(s);
        jl_sym_t *sym = _jl_symbol(name, len);
        jl_module_t *m = (jl_module_t*)jl_array_ptr_ref(mod_list, i);
        if (!m || !jl_is_module(m) || m->uuid.hi != uuid.hi || m->uuid.lo != uuid.lo || m->name != sym || m->build_id != build_id) {
            return jl_get_exceptionf(jl_errorexception_type,
                "Invalid input in module list: expected %s.", name);
        }
    }
}

static int readstr_verify(ios_t *s, const char *str)
{
    size_t i, len = strlen(str);
    for (i = 0; i < len; ++i)
        if ((char)read_uint8(s) != str[i])
            return 0;
    return 1;
}

JL_DLLEXPORT int jl_read_verify_header(ios_t *s)
{
    uint16_t bom;
    return (readstr_verify(s, JI_MAGIC) &&
            read_uint16(s) == JI_FORMAT_VERSION &&
            ios_read(s, (char *) &bom, 2) == 2 && bom == BOM &&
            read_uint8(s) == sizeof(void*) &&
            readstr_verify(s, JL_BUILD_UNAME) && !read_uint8(s) &&
            readstr_verify(s, JL_BUILD_ARCH) && !read_uint8(s) &&
            readstr_verify(s, JULIA_VERSION_STRING) && !read_uint8(s) &&
            readstr_verify(s, jl_git_branch()) && !read_uint8(s) &&
            readstr_verify(s, jl_git_commit()) && !read_uint8(s));
}

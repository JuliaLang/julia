// This file is a part of Julia. License is MIT: https://julialang.org/license

// f(...) where {T<:Union{...}} is a common pattern
// and expanding the Union may give some leaf functions
static int _compile_all_tvar_union(jl_value_t *methsig)
{
    int tvarslen = jl_subtype_env_size(methsig);
    jl_value_t *sigbody = methsig;
    jl_value_t **roots;
    JL_GC_PUSHARGS(roots, 1 + 2 * tvarslen);
    jl_value_t **env = roots + 1;
    int *idx = (int*)alloca(sizeof(int) * tvarslen);
    int i;
    for (i = 0; i < tvarslen; i++) {
        assert(jl_is_unionall(sigbody));
        idx[i] = 0;
        env[2 * i] = (jl_value_t*)((jl_unionall_t*)sigbody)->var;
        jl_value_t *tv = env[2 * i];
        while (jl_is_typevar(tv))
            tv = ((jl_tvar_t*)tv)->ub;
        if (jl_is_abstracttype(tv) && !jl_is_type_type(tv)) {
            JL_GC_POP();
            return 0; // Any as TypeVar is common and not useful here to try to analyze further
        }
        env[2 * i + 1] = tv;
        sigbody = ((jl_unionall_t*)sigbody)->body;
    }

    int all = 1;
    int incr = 0;
    while (!incr) {
        for (i = 0, incr = 1; i < tvarslen; i++) {
            jl_value_t *tv = env[2 * i];
            while (jl_is_typevar(tv))
                tv = ((jl_tvar_t*)tv)->ub;
            if (jl_is_uniontype(tv)) {
                size_t l = jl_count_union_components(tv);
                size_t j = idx[i];
                env[2 * i + 1] = jl_nth_union_component(tv, j);
                ++j;
                if (incr) {
                    if (j == l) {
                        idx[i] = 0;
                    }
                    else {
                        idx[i] = j;
                        incr = 0;
                    }
                }
            }
        }
        jl_value_t *sig = NULL;
        JL_TRY {
            // TODO: wrap in UnionAll for each tvar in env[2*i + 1] ?
            // currently doesn't matter much, since jl_compile_hint doesn't work on abstract types
            sig = (jl_value_t*)jl_instantiate_type_with(sigbody, env, tvarslen);
        }
        JL_CATCH {
            sig = NULL;
        }
        if (sig) {
            roots[0] = sig;
            if (jl_is_datatype(sig) && jl_has_concrete_subtype(sig))
                all = all && jl_compile_hint((jl_tupletype_t*)sig);
            else
                all = 0;
        }
    }
    JL_GC_POP();
    return all;
}

// f(::Union{...}, ...) is a common pattern
// and expanding the Union may give a leaf function
static int _compile_all_union(jl_value_t *sig)
{
    jl_tupletype_t *sigbody = (jl_tupletype_t*)jl_unwrap_unionall(sig);
    size_t count_unions = 0;
    size_t union_size = 1;
    size_t i, l = jl_svec_len(sigbody->parameters);
    jl_svec_t *p = NULL;
    jl_value_t *methsig = NULL;

    for (i = 0; i < l; i++) {
        jl_value_t *ty = jl_svecref(sigbody->parameters, i);
        if (jl_is_uniontype(ty)) {
            count_unions += 1;
            union_size *= jl_count_union_components(ty);
        }
        else if (jl_is_datatype(ty) &&
                 ((!((jl_datatype_t*)ty)->isconcretetype || jl_is_kind(ty)) &&
                  ((jl_datatype_t*)ty)->name != jl_type_typename))
            return 0; // no amount of union splitting will make this a dispatch signature
    }

    if (union_size <= 1 || union_size > 8) {
        return _compile_all_tvar_union(sig);
    }

    int *idx = (int*)alloca(sizeof(int) * count_unions);
    for (i = 0; i < count_unions; i++) {
        idx[i] = 0;
    }

    int all = 1;
    JL_GC_PUSH2(&p, &methsig);
    int idx_ctr = 0, incr = 0;
    while (!incr) {
        p = jl_alloc_svec_uninit(l);
        for (i = 0, idx_ctr = 0, incr = 1; i < l; i++) {
            jl_value_t *ty = jl_svecref(sigbody->parameters, i);
            if (jl_is_uniontype(ty)) {
                assert(idx_ctr < count_unions);
                size_t l = jl_count_union_components(ty);
                size_t j = idx[idx_ctr];
                jl_svecset(p, i, jl_nth_union_component(ty, j));
                ++j;
                if (incr) {
                    if (j == l) {
                        idx[idx_ctr] = 0;
                    }
                    else {
                        idx[idx_ctr] = j;
                        incr = 0;
                    }
                }
                ++idx_ctr;
            }
            else {
                jl_svecset(p, i, ty);
            }
        }
        methsig = jl_apply_tuple_type(p, 1);
        methsig = jl_rewrap_unionall(methsig, sig);
        if (!_compile_all_tvar_union(methsig))
            all = 0;
    }

    JL_GC_POP();
    return all;
}

static int compile_all_collect__(jl_typemap_entry_t *ml, void *env)
{
    jl_array_t *allmeths = (jl_array_t*)env;
    jl_method_t *m = ml->func.method;
    if (m->external_mt)
        return 1;
    if (m->source) {
        // method has a non-generated definition; can be compiled generically
        jl_array_ptr_1d_push(allmeths, (jl_value_t*)m);
    }
    return 1;
}

static int compile_all_collect_(jl_methtable_t *mt, void *env)
{
    jl_typemap_visitor(jl_atomic_load_relaxed(&mt->defs), compile_all_collect__, env);
    return 1;
}

static void jl_compile_all_defs(jl_array_t *mis, int all)
{
    jl_array_t *allmeths = jl_alloc_vec_any(0);
    JL_GC_PUSH1(&allmeths);

    jl_foreach_reachable_mtable(compile_all_collect_, allmeths);

    size_t world =  jl_atomic_load_acquire(&jl_world_counter);
    size_t i, l = jl_array_nrows(allmeths);
    for (i = 0; i < l; i++) {
        jl_method_t *m = (jl_method_t*)jl_array_ptr_ref(allmeths, i);
        int is_macro_method = jl_symbol_name(m->name)[0] == '@';
        if (is_macro_method && !all)
            continue; // Avoid inference / pre-compilation for macros

        if (jl_is_datatype(m->sig) && jl_isa_compileable_sig((jl_tupletype_t*)m->sig, jl_emptysvec, m)) {
            // method has a single compilable specialization, e.g. its definition
            // signature is concrete. in this case we can just hint it.
            jl_compile_method_sig(m, m->sig, jl_emptysvec, world);
        }
        else {
            // first try to create leaf signatures from the signature declaration and compile those
            _compile_all_union(m->sig);

            if (all) {
                // finally, compile a fully generic fallback that can work for all arguments (even invoke)
                jl_method_instance_t *unspec = jl_get_unspecialized(m);
                if (unspec)
                    jl_array_ptr_1d_push(mis, (jl_value_t*)unspec);
            }
        }
    }

    JL_GC_POP();
}

static int precompile_enq_specialization_(jl_method_instance_t *mi, void *closure)
{
    assert(jl_is_method_instance(mi));
    jl_code_instance_t *codeinst = jl_atomic_load_relaxed(&mi->cache);
    while (codeinst) {
        int do_compile = 0;
        if (codeinst->owner != jl_nothing) {
            // TODO(vchuravy) native code caching for foreign interpreters
        }
        else if (jl_atomic_load_relaxed(&codeinst->invoke) != jl_fptr_const_return) {
            jl_value_t *inferred = jl_atomic_load_relaxed(&codeinst->inferred);
            if (inferred &&
                (jl_options.compile_enabled == JL_OPTIONS_COMPILE_ALL || inferred == jl_nothing ||
                 ((jl_is_string(inferred) || jl_is_code_info(inferred)) && jl_ir_inlining_cost(inferred) == UINT16_MAX))) {
                do_compile = 1;
            }
            else if (jl_atomic_load_relaxed(&codeinst->invoke) != NULL || jl_atomic_load_relaxed(&codeinst->precompile)) {
                do_compile = 1;
            }
        }
        if (do_compile) {
            jl_array_ptr_1d_push((jl_array_t*)closure, (jl_value_t*)mi);
            return 1;
        }
        codeinst = jl_atomic_load_relaxed(&codeinst->next);
    }
    return 1;
}

static int precompile_enq_all_specializations__(jl_typemap_entry_t *def, void *closure)
{
    jl_method_t *m = def->func.method;
    if (m->external_mt)
        return 1;
    if ((m->name == jl_symbol("__init__") || m->ccallable) && jl_is_dispatch_tupletype(m->sig)) {
        // ensure `__init__()` and @ccallables get strongly-hinted, specialized, and compiled
        jl_method_instance_t *mi = jl_specializations_get_linfo(m, m->sig, jl_emptysvec);
        jl_array_ptr_1d_push((jl_array_t*)closure, (jl_value_t*)mi);
    }
    else {
        jl_value_t *specializations = jl_atomic_load_relaxed(&def->func.method->specializations);
        if (!jl_is_svec(specializations)) {
            precompile_enq_specialization_((jl_method_instance_t*)specializations, closure);
        }
        else {
            size_t i, l = jl_svec_len(specializations);
            for (i = 0; i < l; i++) {
                jl_value_t *mi = jl_svecref(specializations, i);
                if (mi != jl_nothing)
                    precompile_enq_specialization_((jl_method_instance_t*)mi, closure);
            }
        }
    }
    if (m->ccallable)
        jl_array_ptr_1d_push((jl_array_t*)closure, (jl_value_t*)m->ccallable);
    return 1;
}

static int precompile_enq_all_specializations_(jl_methtable_t *mt, void *env)
{
    return jl_typemap_visitor(jl_atomic_load_relaxed(&mt->defs), precompile_enq_all_specializations__, env);
}

static void *jl_precompile_(jl_array_t *m, int external_linkage)
{
    jl_array_t *m2 = NULL;
    jl_method_instance_t *mi = NULL;
    JL_GC_PUSH2(&m2, &mi);
    m2 = jl_alloc_vec_any(0);
    for (size_t i = 0; i < jl_array_nrows(m); i++) {
        jl_value_t *item = jl_array_ptr_ref(m, i);
        if (jl_is_method_instance(item)) {
            mi = (jl_method_instance_t*)item;
            if (mi != jl_atomic_load_relaxed(&mi->def.method->unspecialized) && !jl_isa_compileable_sig((jl_tupletype_t*)mi->specTypes, mi->sparam_vals, mi->def.method))
                mi = jl_get_specialization1((jl_tupletype_t*)mi->specTypes, jl_atomic_load_acquire(&jl_world_counter), 0);
            if (mi)
                jl_array_ptr_1d_push(m2, (jl_value_t*)mi);
        }
        else {
            assert(jl_is_simplevector(item));
            assert(jl_svec_len(item) == 2 || jl_svec_len(item) == 3);
            jl_array_ptr_1d_push(m2, item);
        }
    }
    void *native_code = jl_create_native(m2, NULL, 0, external_linkage, jl_atomic_load_acquire(&jl_world_counter));
    JL_GC_POP();
    return native_code;
}

static void *jl_precompile(int all)
{
    // array of MethodInstances and ccallable aliases to include in the output
    jl_array_t *m = jl_alloc_vec_any(0);
    JL_GC_PUSH1(&m);
    jl_compile_all_defs(m, all);
    jl_foreach_reachable_mtable(precompile_enq_all_specializations_, m);
    void *native_code = jl_precompile_(m, 0);
    JL_GC_POP();
    return native_code;
}

static int suppress_precompile = 0;
JL_DLLEXPORT void jl_suppress_precompile(int suppress)
{
    suppress_precompile = suppress;
}

static void *jl_precompile_worklist(jl_array_t *worklist, jl_array_t *extext_methods, jl_array_t *new_ext_cis)
{
    if (!worklist)
        return NULL;
    // this "found" array will contain function
    // type signatures that were inferred but haven't been compiled
    jl_array_t *m = jl_alloc_vec_any(0);
    JL_GC_PUSH1(&m);
    if (!suppress_precompile) {
        size_t i, n = jl_array_nrows(worklist);
        for (i = 0; i < n; i++) {
            jl_module_t *mod = (jl_module_t*)jl_array_ptr_ref(worklist, i);
            assert(jl_is_module(mod));
            foreach_mtable_in_module(mod, precompile_enq_all_specializations_, m);
        }
        n = jl_array_nrows(extext_methods);
        for (i = 0; i < n; i++) {
            jl_method_t *method = (jl_method_t*)jl_array_ptr_ref(extext_methods, i);
            assert(jl_is_method(method));
            jl_value_t *specializations = jl_atomic_load_relaxed(&method->specializations);
            if (!jl_is_svec(specializations)) {
                precompile_enq_specialization_((jl_method_instance_t*)specializations, m);
            }
            else {
                size_t j, l = jl_svec_len(specializations);
                for (j = 0; j < l; j++) {
                    jl_value_t *mi = jl_svecref(specializations, j);
                    if (mi != jl_nothing)
                        precompile_enq_specialization_((jl_method_instance_t*)mi, m);
                }
            }
        }
        if (new_ext_cis) {
            n = jl_array_nrows(new_ext_cis);
            for (i = 0; i < n; i++) {
                jl_code_instance_t *ci = (jl_code_instance_t*)jl_array_ptr_ref(new_ext_cis, i);
                precompile_enq_specialization_(jl_get_ci_mi(ci), m);
            }
        }
    }
    void *native_code = jl_precompile_(m, 1);
    JL_GC_POP();
    return native_code;
}

static int enq_ccallable_entrypoints_(jl_typemap_entry_t *def, void *closure)
{
    jl_method_t *m = def->func.method;
    if (m->external_mt)
        return 1;
    if (m->ccallable)
        jl_add_entrypoint((jl_tupletype_t*)jl_svecref(m->ccallable, 1));
    return 1;
}

static int enq_ccallable_entrypoints(jl_methtable_t *mt, void *env)
{
    return jl_typemap_visitor(jl_atomic_load_relaxed(&mt->defs), enq_ccallable_entrypoints_, env);
}

JL_DLLEXPORT void jl_add_ccallable_entrypoints(void)
{
    jl_foreach_reachable_mtable(enq_ccallable_entrypoints, NULL);
}

static void *jl_precompile_trimmed(size_t world)
{
    // array of MethodInstances and ccallable aliases to include in the output
    jl_array_t *m = jl_alloc_vec_any(0);
    jl_value_t *ccallable = NULL;
    JL_GC_PUSH2(&m, &ccallable);
    jl_method_instance_t *mi;
    while (1) {
        mi = (jl_method_instance_t*)arraylist_pop(jl_entrypoint_mis);
        if (mi == NULL)
            break;
        assert(jl_is_method_instance(mi));

        jl_array_ptr_1d_push(m, (jl_value_t*)mi);
        ccallable = (jl_value_t *)mi->def.method->ccallable;
        if (ccallable)
            jl_array_ptr_1d_push(m, ccallable);
    }

    void *native_code = NULL;
    JL_TRY {
        native_code = jl_create_native(m, NULL, jl_options.trim, 0, world);
    } JL_CATCH {
        jl_value_t *exc = jl_current_exception(jl_current_task);
        if (!jl_isa(exc, (jl_value_t*)jl_trimfailure_type))
            jl_rethrow(); // unexpected exception, expose the stacktrace

        // The verification check failed. The error message should already have
        // been printed, so give up here and exit (w/o a stack trace).
        exit(1);
    }
    JL_GC_POP();
    return native_code;
}

static void jl_rebuild_methtables(arraylist_t* MIs, htable_t* mtables)
{
    size_t i;
    for (i = 0; i < MIs->len; i++) {
        jl_method_instance_t *mi = (jl_method_instance_t*)MIs->items[i];
        jl_method_t *m = mi->def.method;
        jl_methtable_t *old_mt = jl_method_get_table(m);
        if ((jl_value_t *)old_mt == jl_nothing)
            continue;
        jl_sym_t *name = old_mt->name;
        if (!ptrhash_has(mtables, old_mt))
            ptrhash_put(mtables, old_mt, jl_new_method_table(name, m->module));
        jl_methtable_t *mt = (jl_methtable_t*)ptrhash_get(mtables, old_mt);
        size_t world =  jl_atomic_load_acquire(&jl_world_counter);
        jl_value_t *lookup = jl_methtable_lookup(mt, m->sig, world);
        // Check if the method is already in the new table, if not then insert it there
        if (lookup == jl_nothing || (jl_method_t*)lookup != m) {
            //TODO: should this be a function like unsafe_insert_method?
            size_t min_world = jl_atomic_load_relaxed(&m->primary_world);
            size_t max_world = ~(size_t)0;
            assert(min_world == jl_atomic_load_relaxed(&m->primary_world));
            int dispatch_status = jl_atomic_load_relaxed(&m->dispatch_status);
            jl_atomic_store_relaxed(&m->primary_world, ~(size_t)0);
            jl_atomic_store_relaxed(&m->dispatch_status, 0);
            jl_typemap_entry_t *newentry = jl_method_table_add(mt, m, NULL);
            jl_atomic_store_relaxed(&m->primary_world, min_world);
            jl_atomic_store_relaxed(&m->dispatch_status, dispatch_status);
            jl_atomic_store_relaxed(&newentry->min_world, min_world);
            jl_atomic_store_relaxed(&newentry->max_world, max_world); // short-circuit jl_method_table_insert
        }
    }
}

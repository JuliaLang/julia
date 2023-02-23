// f{<:Union{...}}(...) is a common pattern
// and expanding the Union may give a leaf function
static void _compile_all_tvar_union(jl_value_t *methsig)
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
        env[2 * i + 1] = jl_bottom_type; // initialize the list with Union{}, since T<:Union{} is always a valid option
        sigbody = ((jl_unionall_t*)sigbody)->body;
    }

    for (i = 0; i < tvarslen; /* incremented by inner loop */) {
        jl_value_t **sig = &roots[0];
        JL_TRY {
            // TODO: wrap in UnionAll for each tvar in env[2*i + 1] ?
            // currently doesn't matter much, since jl_compile_hint doesn't work on abstract types
            *sig = (jl_value_t*)jl_instantiate_type_with(sigbody, env, tvarslen);
        }
        JL_CATCH {
            goto getnext; // sigh, we found an invalid type signature. should we warn the user?
        }
        if (!jl_has_concrete_subtype(*sig))
            goto getnext; // signature wouldn't be callable / is invalid -- skip it
        if (jl_is_concrete_type(*sig)) {
            if (jl_compile_hint((jl_tupletype_t *)*sig))
                goto getnext; // success
        }

    getnext:
        for (i = 0; i < tvarslen; i++) {
            jl_tvar_t *tv = (jl_tvar_t*)env[2 * i];
            if (jl_is_uniontype(tv->ub)) {
                size_t l = jl_count_union_components(tv->ub);
                size_t j = idx[i];
                if (j == l) {
                    env[2 * i + 1] = jl_bottom_type;
                    idx[i] = 0;
                }
                else {
                    jl_value_t *ty = jl_nth_union_component(tv->ub, j);
                    if (!jl_is_concrete_type(ty))
                        ty = (jl_value_t*)jl_new_typevar(tv->name, tv->lb, ty);
                    env[2 * i + 1] = ty;
                    idx[i] = j + 1;
                    break;
                }
            }
            else {
                env[2 * i + 1] = (jl_value_t*)tv;
            }
        }
    }
    JL_GC_POP();
}

// f(::Union{...}, ...) is a common pattern
// and expanding the Union may give a leaf function
static void _compile_all_union(jl_value_t *sig)
{
    jl_tupletype_t *sigbody = (jl_tupletype_t*)jl_unwrap_unionall(sig);
    size_t count_unions = 0;
    size_t i, l = jl_svec_len(sigbody->parameters);
    jl_svec_t *p = NULL;
    jl_value_t *methsig = NULL;

    for (i = 0; i < l; i++) {
        jl_value_t *ty = jl_svecref(sigbody->parameters, i);
        if (jl_is_uniontype(ty))
            ++count_unions;
        else if (ty == jl_bottom_type)
            return; // why does this method exist?
        else if (jl_is_datatype(ty) && !jl_has_free_typevars(ty) &&
                 ((!jl_is_kind(ty) && ((jl_datatype_t*)ty)->isconcretetype) ||
                  ((jl_datatype_t*)ty)->name == jl_type_typename))
            return; // no amount of union splitting will make this a leaftype signature
    }

    if (count_unions == 0 || count_unions >= 6) {
        _compile_all_tvar_union(sig);
        return;
    }

    int *idx = (int*)alloca(sizeof(int) * count_unions);
    for (i = 0; i < count_unions; i++) {
        idx[i] = 0;
    }

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
        methsig = (jl_value_t*)jl_apply_tuple_type(p);
        methsig = jl_rewrap_unionall(methsig, sig);
        _compile_all_tvar_union(methsig);
    }

    JL_GC_POP();
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

static void jl_compile_all_defs(jl_array_t *mis)
{
    jl_array_t *allmeths = jl_alloc_vec_any(0);
    JL_GC_PUSH1(&allmeths);

    jl_foreach_reachable_mtable(compile_all_collect_, allmeths);

    size_t i, l = jl_array_len(allmeths);
    for (i = 0; i < l; i++) {
        jl_method_t *m = (jl_method_t*)jl_array_ptr_ref(allmeths, i);
        if (jl_is_datatype(m->sig) && jl_isa_compileable_sig((jl_tupletype_t*)m->sig, jl_emptysvec, m)) {
            // method has a single compilable specialization, e.g. its definition
            // signature is concrete. in this case we can just hint it.
            jl_compile_hint((jl_tupletype_t*)m->sig);
        }
        else {
            // first try to create leaf signatures from the signature declaration and compile those
            _compile_all_union(m->sig);

            // finally, compile a fully generic fallback that can work for all arguments
            jl_method_instance_t *unspec = jl_get_unspecialized(m);
            if (unspec)
                jl_array_ptr_1d_push(mis, (jl_value_t*)unspec);
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
        if (jl_atomic_load_relaxed(&codeinst->invoke) != jl_fptr_const_return) {
            jl_value_t *inferred = jl_atomic_load_relaxed(&codeinst->inferred);
            if (inferred &&
                inferred != jl_nothing &&
                jl_ir_flag_inferred((jl_array_t*)inferred) &&
                (jl_ir_inlining_cost((jl_array_t*)inferred) == UINT16_MAX)) {
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
        jl_svec_t *specializations = jl_atomic_load_relaxed(&def->func.method->specializations);
        size_t i, l = jl_svec_len(specializations);
        for (i = 0; i < l; i++) {
            jl_value_t *mi = jl_svecref(specializations, i);
            if (mi != jl_nothing)
                precompile_enq_specialization_((jl_method_instance_t*)mi, closure);
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
    for (size_t i = 0; i < jl_array_len(m); i++) {
        jl_value_t *item = jl_array_ptr_ref(m, i);
        if (jl_is_method_instance(item)) {
            mi = (jl_method_instance_t*)item;
            size_t min_world = 0;
            size_t max_world = ~(size_t)0;
            if (mi != jl_atomic_load_relaxed(&mi->def.method->unspecialized) && !jl_isa_compileable_sig((jl_tupletype_t*)mi->specTypes, mi->sparam_vals, mi->def.method))
                mi = jl_get_specialization1((jl_tupletype_t*)mi->specTypes, jl_atomic_load_acquire(&jl_world_counter), &min_world, &max_world, 0);
            if (mi)
                jl_array_ptr_1d_push(m2, (jl_value_t*)mi);
        }
        else {
            assert(jl_is_simplevector(item));
            assert(jl_svec_len(item) == 2);
            jl_array_ptr_1d_push(m2, item);
        }
    }
    void *native_code = jl_create_native(m2, NULL, NULL, 0, 1, external_linkage,
                                         jl_atomic_load_acquire(&jl_world_counter));
    JL_GC_POP();
    return native_code;
}

static void *jl_precompile(int all)
{
    // array of MethodInstances and ccallable aliases to include in the output
    jl_array_t *m = jl_alloc_vec_any(0);
    JL_GC_PUSH1(&m);
    if (all)
        jl_compile_all_defs(m);
    jl_foreach_reachable_mtable(precompile_enq_all_specializations_, m);
    void *native_code = jl_precompile_(m, 0);
    JL_GC_POP();
    return native_code;
}

static void *jl_precompile_worklist(jl_array_t *worklist, jl_array_t *extext_methods, jl_array_t *new_specializations)
{
    if (!worklist)
        return NULL;
    // this "found" array will contain function
    // type signatures that were inferred but haven't been compiled
    jl_array_t *m = jl_alloc_vec_any(0);
    JL_GC_PUSH1(&m);
    size_t i, n = jl_array_len(worklist);
    for (i = 0; i < n; i++) {
        jl_module_t *mod = (jl_module_t*)jl_array_ptr_ref(worklist, i);
        assert(jl_is_module(mod));
        foreach_mtable_in_module(mod, precompile_enq_all_specializations_, m);
    }
    n = jl_array_len(extext_methods);
    for (i = 0; i < n; i++) {
        jl_method_t *method = (jl_method_t*)jl_array_ptr_ref(extext_methods, i);
        assert(jl_is_method(method));
        jl_svec_t *specializations = jl_atomic_load_relaxed(&method->specializations);
        size_t j, l = jl_svec_len(specializations);
        for (j = 0; j < l; j++) {
            jl_value_t *mi = jl_svecref(specializations, j);
            if (mi != jl_nothing)
                precompile_enq_specialization_((jl_method_instance_t*)mi, m);
        }
    }
    n = jl_array_len(new_specializations);
    for (i = 0; i < n; i++) {
        jl_code_instance_t *ci = (jl_code_instance_t*)jl_array_ptr_ref(new_specializations, i);
        precompile_enq_specialization_(ci->def, m);
    }
    void *native_code = jl_precompile_(m, 1);
    JL_GC_POP();
    return native_code;
}

// This file is a part of Julia. License is MIT: https://julialang.org/license

static int suppress_precompile = 0;
JL_DLLEXPORT void jl_suppress_precompile(int suppress)
{
    suppress_precompile = suppress;
}


static void jl_rebuild_methtables(arraylist_t *MIs, htable_t *mtables) JL_GC_DISABLED
{
    // Rebuild MethodTable to contain only those methods for which we compiled code.
    // This can have significant soundness problems if there previously existed
    // any ambiguous methods, but it would probably be pretty hard to do this
    // fully correctly (with the necessary inserted guard entries).
    htable_t ms;
    htable_new(&ms, 0);
    for (size_t i = 0; i < MIs->len; i++) {
        jl_method_instance_t *mi = (jl_method_instance_t*)MIs->items[i];
        jl_method_t *m = mi->def.method;
        // Check if the method is already in the new table, if not then insert it there
        void **inserted = ptrhash_bp(&ms, m);
        if (*inserted != HT_NOTFOUND)
            continue;
        *inserted = (void*)m;
        jl_methtable_t *old_mt = jl_method_get_table(m);
        if ((jl_value_t *)old_mt == jl_nothing)
            continue;
        if (!ptrhash_has(mtables, old_mt))
            ptrhash_put(mtables, old_mt, jl_new_method_table(old_mt->name, old_mt->module));
        jl_methtable_t *mt = (jl_methtable_t*)ptrhash_get(mtables, old_mt);
        //TODO: should this be a function like unsafe_insert_method, since all that is wanted is the jl_typemap_insert on a copy of the existing entry
        size_t min_world = jl_atomic_load_relaxed(&m->primary_world);
        size_t max_world = ~(size_t)0;
        int dispatch_status = jl_atomic_load_relaxed(&m->dispatch_status);
        jl_atomic_store_relaxed(&m->primary_world, ~(size_t)0);
        jl_atomic_store_relaxed(&m->dispatch_status, 0);
        jl_typemap_entry_t *newentry = jl_method_table_add(mt, m, NULL);
        jl_atomic_store_relaxed(&m->primary_world, min_world);
        jl_atomic_store_relaxed(&m->dispatch_status, dispatch_status);
        jl_atomic_store_relaxed(&newentry->min_world, min_world);
        jl_atomic_store_relaxed(&newentry->max_world, max_world); // short-circuit jl_method_table_insert
    }
    htable_free(&ms);
}

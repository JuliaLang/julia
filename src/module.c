// This file is a part of Julia. License is MIT: https://julialang.org/license
//

/*
  modules and top-level bindings
*/
#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

// In this translation unit and this translation unit only emit this symbol `extern` for use by julia
EXTERN_INLINE_DEFINE uint8_t jl_bpart_get_kind(jl_binding_partition_t *bpart) JL_NOTSAFEPOINT;

static jl_binding_partition_t *new_binding_partition(void)
{
    jl_binding_partition_t *bpart = (jl_binding_partition_t*)jl_gc_alloc(jl_current_task->ptls, sizeof(jl_binding_partition_t), jl_binding_partition_type);
    bpart->restriction = NULL;
    bpart->kind = (size_t)PARTITION_KIND_GUARD;
    bpart->min_world = 0;
    jl_atomic_store_relaxed(&bpart->max_world, (size_t)-1);
    jl_atomic_store_relaxed(&bpart->next, NULL);
    return bpart;
}

struct implicit_search_gap {
    _Atomic(jl_binding_partition_t *) *insert;
    jl_binding_partition_t *replace;
    jl_value_t *parent;

    size_t min_world;
    size_t max_world;
    size_t inherited_flags;
};

STATIC_INLINE jl_binding_partition_t *jl_get_binding_partition__(jl_binding_t *b JL_PROPAGATES_ROOT, size_t world, struct implicit_search_gap *gap) JL_GLOBALLY_ROOTED
{
    // Iterate through the list of binding partitions, keeping track of where to insert a new one for an implicit
    // resolution if necessary.
    while (gap->replace && world < gap->replace->min_world) {
        gap->insert = &gap->replace->next;
        gap->max_world = gap->replace->min_world - 1;
        gap->parent = (jl_value_t*)gap->replace;
        gap->replace = jl_atomic_load_relaxed(gap->insert);
    }
    if (gap->replace && world <= jl_atomic_load_relaxed(&gap->replace->max_world)) {
        return gap->replace;
    }
    gap->min_world = gap->replace ? jl_atomic_load_relaxed(&gap->replace->max_world) + 1 : 0;
    if (gap->replace)
        gap->inherited_flags = gap->replace->kind & PARTITION_MASK_FLAG;
    else
        gap->inherited_flags = 0;
    return NULL;
}

STATIC_INLINE jl_binding_partition_t *jl_get_binding_partition_if_present(jl_binding_t *b JL_PROPAGATES_ROOT, size_t world, struct implicit_search_gap *gap)
{
    gap->parent = (jl_value_t*)b;
    gap->insert = &b->partitions;
    gap->replace = jl_atomic_load_relaxed(gap->insert);
    gap->min_world = 0;
    gap->max_world = ~(size_t)0;
    gap->inherited_flags = 0;
    return jl_get_binding_partition__(b, world, gap);
}

struct implicit_search_resolution {
    enum jl_partition_kind ultimate_kind;
    jl_value_t *binding_or_const;
    size_t min_world;
    size_t max_world;
    int saw_cycle;
    //// Not semantic, but used for reflection.
    // If non-null, the unique module from which this binding was imported
    jl_module_t *debug_only_import_from;
    // If non-null, the unique binding imported. For PARTITION_KIND_IMPLICIT_GLOBAL, always matches binding_or_const.
    // Must have trust_cache = 0.
    jl_binding_t *debug_only_ultimate_binding;
};

static size_t WORLDMAX(size_t a, size_t b) { return a > b ? a : b; }
static size_t WORLDMIN(size_t a, size_t b) { return a > b ? b : a; }

static void update_implicit_resolution(struct implicit_search_resolution *to_update, struct implicit_search_resolution resolution)
{
    to_update->min_world = WORLDMAX(to_update->min_world, resolution.min_world);
    to_update->max_world = WORLDMIN(to_update->max_world, resolution.max_world);
    to_update->saw_cycle |= resolution.saw_cycle;
    if (resolution.ultimate_kind == PARTITION_FAKE_KIND_CYCLE) {
        // Cycles get ignored. This causes the resolution resolution to only be partial, so we can't
        // cache it. This gets tracked in saw_cycle;
        to_update->saw_cycle = 1;
        return;
    }
    if (resolution.ultimate_kind == PARTITION_KIND_GUARD) {
        // Ignore guard imports
        return;
    }
    if (to_update->ultimate_kind == PARTITION_KIND_GUARD) {
        assert(resolution.binding_or_const);
        to_update->ultimate_kind = resolution.ultimate_kind;
        to_update->binding_or_const = resolution.binding_or_const;
        to_update->debug_only_import_from = resolution.debug_only_import_from;
        to_update->debug_only_ultimate_binding = resolution.debug_only_ultimate_binding;
        return;
    }
    if (resolution.ultimate_kind == to_update->ultimate_kind &&
        resolution.binding_or_const == to_update->binding_or_const) {
        if (resolution.debug_only_import_from != to_update->debug_only_import_from) {
            to_update->debug_only_import_from = NULL;
        }
        if (resolution.debug_only_ultimate_binding != to_update->debug_only_ultimate_binding) {
            to_update->debug_only_ultimate_binding = NULL;
        }
        return;
    }
    to_update->ultimate_kind = PARTITION_KIND_FAILED;
    to_update->binding_or_const = NULL;
    to_update->debug_only_import_from = NULL;
    to_update->debug_only_ultimate_binding = NULL;
}

static jl_binding_partition_t *jl_implicit_import_resolved(jl_binding_t *b, struct implicit_search_gap gap, struct implicit_search_resolution resolution)
{
    jl_binding_partition_t *new_bpart = new_binding_partition();
    jl_atomic_store_relaxed(&new_bpart->max_world, gap.max_world < resolution.max_world ? gap.max_world : resolution.max_world);
    new_bpart->min_world = gap.min_world > resolution.min_world ? gap.min_world : resolution.min_world;
    new_bpart->kind = resolution.ultimate_kind | gap.inherited_flags;
    new_bpart->restriction = resolution.binding_or_const;
    jl_gc_wb_fresh(new_bpart, new_bpart->restriction);
    jl_atomic_store_relaxed(&new_bpart->next, gap.replace);
    if (!jl_atomic_cmpswap(gap.insert, &gap.replace, new_bpart))
        return NULL;
    jl_gc_wb(gap.parent, new_bpart);
    return new_bpart;
}

// find a binding from a module's `usings` list
struct implicit_search_resolution jl_resolve_implicit_import(jl_binding_t *b, modstack_t *st, size_t world, int trust_cache)
{
    // First check if we've hit a cycle in this resolution
    {
        modstack_t *tmp = st;
        for (; tmp != NULL; tmp = tmp->prev) {
            if (tmp->b == b) {
                return (struct implicit_search_resolution){ PARTITION_FAKE_KIND_CYCLE, NULL, 0, ~(size_t)0, 1, NULL, NULL };
            }
        }
    }

    jl_module_t *m = b->globalref->mod;
    jl_sym_t *var = b->globalref->name;

    modstack_t top = { b, st };
    struct implicit_search_resolution impstate;
    struct implicit_search_resolution depimpstate;
    size_t min_world = 0;
    size_t max_world = ~(size_t)0;
    impstate = depimpstate = (struct implicit_search_resolution){ PARTITION_KIND_GUARD, NULL, min_world, max_world, 0, NULL, NULL };

    JL_LOCK(&m->lock);
    int i = (int)module_usings_length(m) - 1;
    JL_UNLOCK(&m->lock);
    for (; i >= 0 && impstate.ultimate_kind != PARTITION_KIND_FAILED; --i) {
        JL_LOCK(&m->lock);
        struct _jl_module_using data = *module_usings_getidx(m, i);
        JL_UNLOCK(&m->lock);
        if (data.min_world > world) {
            if (max_world > data.min_world)
                max_world = data.min_world - 1;
            continue;
        }
        if (data.max_world < world) {
            if (min_world < data.max_world)
                min_world = data.max_world + 1;
            continue;
        }

        min_world = WORLDMAX(min_world, data.min_world);
        max_world = WORLDMIN(max_world, data.max_world);

        jl_module_t *imp = data.mod;
        JL_GC_PROMISE_ROOTED(imp);
        jl_binding_t *tempb = jl_get_module_binding(imp, var, 0);
        if (!tempb) {
            // If the binding has never been allocated, it could not have been marked exported, so
            // it is irrelevant for our resolution. We can move on.
            continue;
        }

        struct implicit_search_gap gap;
        jl_binding_partition_t *tempbpart = jl_get_binding_partition_if_present(tempb, world, &gap);
        size_t tempbpart_flags = tempbpart ? (tempbpart->kind & PARTITION_MASK_FLAG) : gap.inherited_flags;

        while (tempbpart && jl_bkind_is_some_explicit_import(jl_binding_kind(tempbpart))) {
            max_world = WORLDMIN(max_world, jl_atomic_load_relaxed(&tempbpart->max_world));
            min_world = WORLDMAX(min_world, tempbpart->min_world);

            tempb = (jl_binding_t*)tempbpart->restriction;
            tempbpart = jl_get_binding_partition_if_present(tempb, world, &gap);
        }

        int tempbpart_valid = tempbpart && (trust_cache || !jl_bkind_is_some_implicit(jl_binding_kind(tempbpart)));
        size_t tembppart_max_world = tempbpart_valid ? jl_atomic_load_relaxed(&tempbpart->max_world) : gap.max_world;
        size_t tembppart_min_world = tempbpart ? WORLDMAX(tempbpart->min_world, gap.min_world) : gap.min_world;

        max_world = WORLDMIN(max_world, tembppart_max_world);
        min_world = WORLDMAX(min_world, tembppart_min_world);

        if (!(tempbpart_flags & PARTITION_FLAG_EXPORTED)) {
            // Partition not exported - skip.
            continue;
        }

        struct implicit_search_resolution *comparison = &impstate;
        if (impstate.ultimate_kind != PARTITION_KIND_GUARD) {
            if (tempbpart_flags & PARTITION_FLAG_DEPRECATED) {
                // Deprecated, but we already have a non-deprecated binding for this - skip.
                continue;
            }
        } else if (tempbpart_flags & PARTITION_FLAG_DEPRECATED) {
            if (depimpstate.ultimate_kind == PARTITION_KIND_FAILED) {
                // We've already decided that the deprecated bindings are ambiguous, so skip this, but
                // keep going to look for non-deprecated bindings.
                continue;
            }
            comparison = &depimpstate;
        }

        struct implicit_search_resolution imp_resolution = { PARTITION_KIND_GUARD, NULL, min_world, max_world, 0, NULL, NULL };
        if (!tempbpart_valid) {
            imp_resolution = jl_resolve_implicit_import(tempb, &top, world, trust_cache);
        } else {
            enum jl_partition_kind kind = jl_binding_kind(tempbpart);
            if (kind == PARTITION_KIND_IMPLICIT_GLOBAL) {
                imp_resolution.binding_or_const = tempbpart->restriction;
                imp_resolution.debug_only_ultimate_binding = (jl_binding_t*)tempbpart->restriction;
                imp_resolution.ultimate_kind = PARTITION_KIND_IMPLICIT_GLOBAL;
            } else if (kind == PARTITION_KIND_GLOBAL || kind == PARTITION_KIND_DECLARED || kind == PARTITION_KIND_BACKDATED_CONST) {
                imp_resolution.binding_or_const = (jl_value_t *)tempb;
                imp_resolution.debug_only_ultimate_binding = tempb;
                imp_resolution.ultimate_kind = PARTITION_KIND_IMPLICIT_GLOBAL;
            } else if (jl_bkind_is_defined_constant(kind)) {
                assert(tempbpart->restriction);
                imp_resolution.binding_or_const = tempbpart->restriction;
                imp_resolution.debug_only_ultimate_binding = tempb;
                imp_resolution.ultimate_kind = PARTITION_KIND_IMPLICIT_CONST;
            }
        }
        imp_resolution.debug_only_import_from = imp;
        update_implicit_resolution(comparison, imp_resolution);

        if (!tempbpart && !imp_resolution.saw_cycle) {
            // Independent of whether or not we trust the cache, we have independently computed the implicit resolution
            // for this import, so we can put it in the cache.
            jl_implicit_import_resolved(tempb, gap, imp_resolution);
        }
    }

    if (impstate.ultimate_kind == PARTITION_KIND_GUARD && depimpstate.ultimate_kind != PARTITION_KIND_GUARD) {
        depimpstate.min_world = WORLDMAX(depimpstate.min_world, min_world);
        depimpstate.max_world = WORLDMIN(depimpstate.max_world, max_world);
        return depimpstate;
    }
    impstate.min_world = WORLDMAX(impstate.min_world, min_world);
    impstate.max_world = WORLDMIN(impstate.max_world, max_world);
    return impstate;
}

JL_DLLEXPORT jl_binding_partition_t *jl_maybe_reresolve_implicit(jl_binding_t *b, size_t new_max_world)
{
    struct implicit_search_gap gap;
    while (1) {
        jl_binding_partition_t *bpart = jl_get_binding_partition_if_present(b, new_max_world+1, &gap);
        assert(bpart == jl_atomic_load_relaxed(&b->partitions));
        assert(bpart);
        struct implicit_search_resolution resolution = jl_resolve_implicit_import(b, NULL, new_max_world+1, 0);
        if (resolution.min_world == bpart->min_world) {
            assert(bpart->restriction == resolution.binding_or_const && jl_binding_kind(bpart) == resolution.ultimate_kind);
            return bpart;
        }
        assert(resolution.min_world == new_max_world+1 && "Missed an invalidation or bad resolution bounds");
        size_t expected_max_world = ~(size_t)0;
        if (jl_atomic_cmpswap(&bpart->max_world, &expected_max_world, new_max_world))
        {
            gap.min_world = new_max_world+1;
            gap.inherited_flags = bpart->kind & PARTITION_MASK_FLAG;
            jl_binding_partition_t *new_bpart = jl_implicit_import_resolved(b, gap, resolution);
            if (new_bpart)
                return new_bpart;
        }
    }
}

JL_DLLEXPORT void jl_update_loaded_bpart(jl_binding_t *b, jl_binding_partition_t *bpart)
{
    struct implicit_search_resolution resolution = jl_resolve_implicit_import(b, NULL, jl_atomic_load_relaxed(&jl_world_counter), 0);
    bpart->min_world = resolution.min_world;
    jl_atomic_store_relaxed(&bpart->max_world, resolution.max_world);
    bpart->restriction = resolution.binding_or_const;
    bpart->kind = resolution.ultimate_kind;
}

STATIC_INLINE jl_binding_partition_t *jl_get_binding_partition_(jl_binding_t *b JL_PROPAGATES_ROOT, jl_value_t *parent, _Atomic(jl_binding_partition_t *)*insert, size_t world, modstack_t *st) JL_GLOBALLY_ROOTED
{
    assert(jl_is_binding(b));
    struct implicit_search_gap gap;
    gap.parent = parent;
    gap.insert = insert;
    gap.inherited_flags = 0;
    gap.min_world = 0;
    gap.max_world = ~(size_t)0;
    while (1) {
        gap.replace = jl_atomic_load_relaxed(gap.insert);
        jl_binding_partition_t *bpart = jl_get_binding_partition__(b, world, &gap);
        if (bpart)
            return bpart;
        struct implicit_search_resolution resolution = jl_resolve_implicit_import(b, NULL, world, 1);
        jl_binding_partition_t *new_bpart = jl_implicit_import_resolved(b, gap, resolution);
        if (new_bpart)
            return new_bpart;
    }
}

jl_binding_partition_t *jl_get_binding_partition(jl_binding_t *b, size_t world) {
    if (!b)
        return NULL;
    // Duplicate the code for the entry frame for branch prediction
    return jl_get_binding_partition_(b, (jl_value_t*)b, &b->partitions, world, NULL);
}

jl_binding_partition_t *jl_get_binding_partition_with_hint(jl_binding_t *b, jl_binding_partition_t *prev, size_t world) JL_GLOBALLY_ROOTED {
    // Helper for getting a binding partition for an older world after we've already looked up the partition for a newer world
    assert(b);
    assert(prev->min_world > world);
    return jl_get_binding_partition_(b, (jl_value_t*)prev, &prev->next, world, NULL);
}

jl_binding_partition_t *jl_get_binding_partition_all(jl_binding_t *b, size_t min_world, size_t max_world) {
    if (!b)
        return NULL;
    jl_binding_partition_t *bpart = jl_get_binding_partition(b, min_world);
    if (!bpart)
        return NULL;
    if (jl_atomic_load_relaxed(&bpart->max_world) < max_world)
        return NULL;
    return bpart;
}

JL_DLLEXPORT int jl_get_binding_leaf_partitions_restriction_kind(jl_binding_t *b JL_PROPAGATES_ROOT, struct restriction_kind_pair *rkp, size_t min_world, size_t max_world) {
    if (!b)
        return 0;

    int first = 1;
    size_t validated_min_world = max_world == ~(size_t)0 ? ~(size_t)0 : max_world + 1;
    jl_binding_partition_t *bpart = NULL;
    int maybe_depwarn = 0;
    while (validated_min_world > min_world) {
        bpart = bpart ? jl_get_binding_partition_with_hint(b, bpart, validated_min_world - 1) :
                        jl_get_binding_partition(b, validated_min_world - 1);
        while (validated_min_world > min_world && validated_min_world > bpart->min_world) {
            jl_binding_t *curb = b;
            jl_binding_partition_t *curbpart = bpart;
            size_t cur_min_world = bpart->min_world;
            size_t cur_max_world = validated_min_world - 1;
            jl_walk_binding_inplace_worlds(&curb, &curbpart, &cur_min_world, &cur_max_world, &maybe_depwarn, cur_max_world);
            enum jl_partition_kind kind = jl_binding_kind(curbpart);
            if (kind == PARTITION_KIND_IMPLICIT_CONST)
                kind = PARTITION_KIND_CONST;
            if (first == 1) {
                rkp->kind = kind;
                rkp->restriction = curbpart->restriction;
                if (rkp->kind == PARTITION_KIND_GLOBAL || rkp->kind == PARTITION_KIND_DECLARED)
                    rkp->binding_if_global = curb;
                first = 0;
            } else {
                if (kind != rkp->kind || curbpart->restriction != rkp->restriction)
                    return 0;
                if ((rkp->kind == PARTITION_KIND_GLOBAL || rkp->kind == PARTITION_KIND_DECLARED) && rkp->binding_if_global != curb)
                    return 0;
            }
            validated_min_world = cur_min_world;
        }
    }
    rkp->maybe_depwarn = maybe_depwarn;
    return 1;
}

JL_DLLEXPORT jl_value_t *jl_get_binding_leaf_partitions_value_if_const(jl_binding_t *b JL_PROPAGATES_ROOT, int *maybe_depwarn, size_t min_world, size_t max_world) {
    struct restriction_kind_pair rkp = { NULL, NULL, PARTITION_KIND_GUARD, 0 };
    if (!jl_get_binding_leaf_partitions_restriction_kind(b, &rkp, min_world, max_world))
        return NULL;
    if (jl_bkind_is_some_constant(rkp.kind) && rkp.kind != PARTITION_KIND_BACKDATED_CONST) {
        *maybe_depwarn = rkp.maybe_depwarn;
        return rkp.restriction;
    }
    return NULL;
}

JL_DLLEXPORT jl_module_t *jl_new_module__(jl_sym_t *name, jl_module_t *parent)
{
    jl_task_t *ct = jl_current_task;
    const jl_uuid_t uuid_zero = {0, 0};
    jl_module_t *m = (jl_module_t*)jl_gc_alloc(ct->ptls, sizeof(jl_module_t),
                                               jl_module_type);
    jl_set_typetagof(m, jl_module_tag, 0);
    assert(jl_is_symbol(name));
    m->name = name;
    m->parent = parent;
    m->istopmod = 0;
    m->uuid = uuid_zero;
    static unsigned int mcounter; // simple counter backup, in case hrtime is not incrementing
    m->build_id.lo = jl_hrtime() + (++mcounter);
    if (!m->build_id.lo)
        m->build_id.lo++; // build id 0 is invalid
    m->build_id.hi = ~(uint64_t)0;
    jl_atomic_store_relaxed(&m->counter, 1);
    m->usings_backedges = jl_nothing;
    m->scanned_methods = jl_nothing;
    m->nospecialize = 0;
    m->optlevel = -1;
    m->compile = -1;
    m->infer = -1;
    m->max_methods = -1;
    m->file = jl_empty_sym;
    m->line = 0;
    m->hash = parent == NULL ? bitmix(name->hash, jl_module_type->hash) :
        bitmix(name->hash, parent->hash);
    JL_MUTEX_INIT(&m->lock, "module->lock");
    jl_atomic_store_relaxed(&m->bindings, jl_emptysvec);
    jl_atomic_store_relaxed(&m->bindingkeyset, (jl_genericmemory_t*)jl_an_empty_memory_any);
    arraylist_new(&m->usings, 0);
    return m;
}

JL_DLLEXPORT void jl_add_default_names(jl_module_t *m, uint8_t default_using_core, uint8_t self_name)
{
    if (jl_core_module) {
        // Bootstrap: Before jl_core_module is defined, we don't have enough infrastructure
        // for bindings, so Core itself gets special handling in jltypes.c
        if (default_using_core) {
            jl_module_using(m, jl_core_module);
        }
        if (self_name) {
            // export own name, so "using Foo" makes "Foo" itself visible
            jl_set_const(m, m->name, (jl_value_t*)m);
            jl_module_public(m, m->name, 1);
        }
    }
}

JL_DLLEXPORT jl_module_t *jl_new_module_(jl_sym_t *name, jl_module_t *parent, uint8_t default_using_core, uint8_t self_name)
{
    jl_module_t *m = jl_new_module__(name, parent);
    JL_GC_PUSH1(&m);
    jl_add_default_names(m, default_using_core, self_name);
    JL_GC_POP();
    return m;
}


// Precondition: world_counter_lock is held
JL_DLLEXPORT jl_binding_partition_t *jl_declare_constant_val3(
    jl_binding_t *b, jl_module_t *mod, jl_sym_t *var, jl_value_t *val,
    enum jl_partition_kind constant_kind, size_t new_world)
{
    jl_binding_partition_t *new_prev_bpart = NULL;
    JL_GC_PUSH2(&val, &new_prev_bpart);
    if (!b) {
        b = jl_get_module_binding(mod, var, 1);
    }
    jl_binding_partition_t *new_bpart = NULL;
    jl_binding_partition_t *bpart = jl_get_binding_partition(b, new_world);
    while (!new_bpart) {
        enum jl_partition_kind kind = jl_binding_kind(bpart);
        if (jl_bkind_is_some_constant(kind) && !jl_bkind_is_some_implicit(kind)) {
            if (!val) {
                new_bpart = bpart;
                break;
            }
            jl_value_t *old = bpart->restriction;
            JL_GC_PROMISE_ROOTED(old);
            if (val == old || (val && old && jl_egal(val, old))) {
                new_bpart = bpart;
                break;
            }
        } else if (jl_bkind_is_some_explicit_import(kind)) {
            jl_errorf("cannot declare %s.%s constant; it was already declared as an import",
                      jl_symbol_name(mod->name), jl_symbol_name(var));
        } else if (kind == PARTITION_KIND_GLOBAL) {
            jl_errorf("cannot declare %s.%s constant; it was already declared global",
                      jl_symbol_name(mod->name), jl_symbol_name(var));
        }
        if (bpart->min_world == new_world) {
            bpart->kind = constant_kind | (bpart->kind & PARTITION_MASK_FLAG);
            bpart->restriction = val;
            if (val)
                jl_gc_wb(bpart, val);
            new_bpart = bpart;
        } else {
            new_bpart = jl_replace_binding_locked(b, bpart, val, constant_kind, new_world);
        }
        int need_backdate = new_world && val;
        if (need_backdate) {
            // We will backdate as long as this partition was never explicitly
            // declared const, global, or imported.
            jl_binding_partition_t *prev_bpart = bpart;
            for (;;) {
                enum jl_partition_kind prev_kind = jl_binding_kind(prev_bpart);
                if (jl_bkind_is_some_constant(prev_kind) || prev_kind == PARTITION_KIND_GLOBAL ||
                    (jl_bkind_is_some_import(prev_kind))) {
                    need_backdate = 0;
                    break;
                }
                if (prev_bpart->min_world == 0)
                    break;
                prev_bpart = jl_get_binding_partition(b, prev_bpart->min_world - 1);
            }
        }
        // If backdate is required, replace each existing partition by a new one.
        // We can't use one binding to cover the entire range, because we need to
        // keep the flags partitioned.
        if (need_backdate) {
            jl_binding_partition_t *prev_bpart = bpart;
            jl_binding_partition_t *backdate_bpart = new_binding_partition();
            new_prev_bpart = backdate_bpart;
            while (1) {
                backdate_bpart->kind = (size_t)PARTITION_KIND_BACKDATED_CONST | (prev_bpart->kind & 0xf0);
                backdate_bpart->restriction = val;
                backdate_bpart->min_world = prev_bpart->min_world;
                jl_gc_wb_fresh(backdate_bpart, val);
                jl_atomic_store_relaxed(&backdate_bpart->max_world,
                    jl_atomic_load_relaxed(&prev_bpart->max_world));
                prev_bpart = jl_atomic_load_relaxed(&prev_bpart->next);
                if (!prev_bpart)
                    break;
                jl_binding_partition_t *next_prev_bpart = new_binding_partition();
                jl_atomic_store_relaxed(&backdate_bpart->next, next_prev_bpart);
                jl_gc_wb(backdate_bpart, next_prev_bpart);
                backdate_bpart = next_prev_bpart;
            }
            jl_atomic_store_release(&new_bpart->next, new_prev_bpart);
            jl_gc_wb(new_bpart, new_prev_bpart);
        }
    }
    JL_GC_POP();
    return new_bpart;
}

JL_DLLEXPORT jl_module_t *jl_new_module(jl_sym_t *name, jl_module_t *parent)
{
    return jl_new_module_(name, parent, 1, 1);
}

uint32_t jl_module_next_counter(jl_module_t *m)
{
    return jl_atomic_fetch_add_relaxed(&m->counter, 1);
}

JL_DLLEXPORT jl_value_t *jl_f_new_module(jl_sym_t *name, uint8_t std_imports, uint8_t default_names)
{
    // TODO: should we prohibit this during incremental compilation?
    // TODO: the parent module is a lie
    jl_module_t *m = jl_new_module_(name, jl_main_module, default_names, default_names);
    JL_GC_PUSH1(&m);
    if (std_imports)
        jl_add_standard_imports(m);
    JL_GC_POP();
    // TODO: should we somehow try to gc-root this correctly?
    return (jl_value_t*)m;
}

JL_DLLEXPORT void jl_set_module_nospecialize(jl_module_t *self, int on)
{
    self->nospecialize = (on ? -1 : 0);
}

JL_DLLEXPORT void jl_set_module_optlevel(jl_module_t *self, int lvl)
{
    self->optlevel = lvl;
}

JL_DLLEXPORT int jl_get_module_optlevel(jl_module_t *m)
{
    int lvl = m->optlevel;
    while (lvl == -1 && m->parent != m && m != jl_base_module) {
        m = m->parent;
        lvl = m->optlevel;
    }
    return lvl;
}

JL_DLLEXPORT void jl_set_module_compile(jl_module_t *self, int value)
{
    self->compile = value;
}

JL_DLLEXPORT int jl_get_module_compile(jl_module_t *m)
{
    int value = m->compile;
    while (value == -1 && m->parent != m && m != jl_base_module) {
        m = m->parent;
        value = m->compile;
    }
    return value;
}

JL_DLLEXPORT void jl_set_module_infer(jl_module_t *self, int value)
{
    self->infer = value;
    // no reason to specialize if inference is off
    if (!value)
        jl_set_module_nospecialize(self, 1);
}

JL_DLLEXPORT int jl_get_module_infer(jl_module_t *m)
{
    int value = m->infer;
    while (value == -1 && m->parent != m && m != jl_base_module) {
        m = m->parent;
        value = m->infer;
    }
    return value;
}

JL_DLLEXPORT void jl_set_module_max_methods(jl_module_t *self, int value)
{
    self->max_methods = value;
}

JL_DLLEXPORT int jl_get_module_max_methods(jl_module_t *m)
{
    int value = m->max_methods;
    while (value == -1 && m->parent != m && m != jl_base_module) {
        m = m->parent;
        value = m->max_methods;
    }
    return value;
}

JL_DLLEXPORT void jl_set_istopmod(jl_module_t *self, uint8_t isprimary)
{
    self->istopmod = 1;
    if (isprimary) {
        jl_top_module = self;
    }
}

JL_DLLEXPORT uint8_t jl_istopmod(jl_module_t *mod)
{
    return mod->istopmod;
}

static jl_globalref_t *jl_new_globalref(jl_module_t *mod, jl_sym_t *name, jl_binding_t *b)
{
    jl_task_t *ct = jl_current_task;
    jl_globalref_t *g = (jl_globalref_t*)jl_gc_alloc(ct->ptls, sizeof(jl_globalref_t), jl_globalref_type);
    g->mod = mod;
    jl_gc_wb_fresh(g, g->mod);
    g->name = name;
    jl_gc_wb_fresh(g, g->name);
    g->binding = b;
    jl_gc_wb_fresh(g, g->binding);
    return g;
}

static jl_binding_t *new_binding(jl_module_t *mod, jl_sym_t *name)
{
    jl_task_t *ct = jl_current_task;
    assert(jl_is_module(mod) && jl_is_symbol(name));
    jl_binding_t *b = (jl_binding_t*)jl_gc_alloc(ct->ptls, sizeof(jl_binding_t), jl_binding_type);
    jl_atomic_store_relaxed(&b->value, NULL);
    jl_atomic_store_relaxed(&b->partitions, NULL);
    b->globalref = NULL;
    b->backedges = NULL;
    jl_atomic_store_relaxed(&b->flags, 0);
    JL_GC_PUSH1(&b);
    b->globalref = jl_new_globalref(mod, name, b);
    jl_gc_wb(b, b->globalref);
    JL_GC_POP();
    return b;
}

extern jl_mutex_t jl_modules_mutex;

static int is_module_open(jl_module_t *m)
{
    JL_LOCK(&jl_modules_mutex);
    int open = ptrhash_has(&jl_current_modules, (void*)m);
    if (!open && jl_module_init_order != NULL) {
        size_t i, l = jl_array_len(jl_module_init_order);
        for (i = 0; i < l; i++) {
            if (m == (jl_module_t*)jl_array_ptr_ref(jl_module_init_order, i)) {
                open = 1;
                break;
            }
        }
    }
    JL_UNLOCK(&jl_modules_mutex);
    return open;
}

extern void check_safe_newbinding(jl_module_t *m, jl_sym_t *var)
{
    if (jl_current_task->ptls->in_pure_callback)
        jl_errorf("new strong globals cannot be created in a generated function. Declare them outside using `global x::Any`.");
    if (jl_options.incremental && jl_generating_output() && !is_module_open(m)) {
        jl_errorf("Creating a new global in closed module `%s` (`%s`) breaks incremental compilation "
                    "because the side effects will not be permanent.",
                    jl_symbol_name(m->name), jl_symbol_name(var));
    }
}

static jl_module_t *jl_binding_dbgmodule(jl_binding_t *b) JL_GLOBALLY_ROOTED;

// Checks that the binding in general is currently writable, but does not perform any checks on the
// value to be written into the binding.
JL_DLLEXPORT void jl_check_binding_currently_writable(jl_binding_t *b, jl_module_t *m, jl_sym_t *s)
{
    jl_binding_partition_t *bpart = jl_get_binding_partition(b, jl_current_task->world_age);
    if (jl_options.depwarn && (bpart->kind & PARTITION_FLAG_DEPWARN)) {
        jl_binding_deprecation_warning(b);
    }
    enum jl_partition_kind kind = jl_binding_kind(bpart);
    if (kind != PARTITION_KIND_GLOBAL && kind != PARTITION_KIND_DECLARED) {
        if (jl_bkind_is_some_guard(kind)) {
            jl_errorf("Global %s.%s does not exist and cannot be assigned.\n"
                        "Note: Julia 1.9 and 1.10 inadvertently omitted this error check (#56933).\n"
                        "Hint: Declare it using `global %s` inside `%s` before attempting assignment.",
                        jl_symbol_name(m->name), jl_symbol_name(s),
                        jl_symbol_name(s), jl_symbol_name(m->name));
        }
        else if (jl_bkind_is_some_constant(kind) && kind != PARTITION_KIND_IMPLICIT_CONST) {
            jl_errorf("invalid assignment to constant %s.%s. This redefinition may be permitted using the `const` keyword.",
                        jl_symbol_name(m->name), jl_symbol_name(s));
        }
        else {
            jl_module_t *from = jl_binding_dbgmodule(b);
            if (from == m || !from)
                jl_errorf("cannot assign a value to imported variable %s.%s",
                          jl_symbol_name(m->name), jl_symbol_name(s));
            else
                jl_errorf("cannot assign a value to imported variable %s.%s from module %s",
                          jl_symbol_name(from->name), jl_symbol_name(s), jl_symbol_name(m->name));
        }
    }
}

JL_DLLEXPORT jl_binding_t *jl_get_binding_wr(jl_module_t *m JL_PROPAGATES_ROOT, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_module_binding(m, var, 1);
    jl_check_binding_currently_writable(b, m, var);
    return b;
}

// return module of binding
JL_DLLEXPORT jl_module_t *jl_get_module_of_binding(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_module_binding(m, var, 1);
    jl_binding_partition_t *bpart = jl_get_binding_partition(b, jl_current_task->world_age);
    jl_walk_binding_inplace(&b, &bpart, jl_current_task->world_age);
    if (jl_binding_kind(bpart) == PARTITION_KIND_IMPLICIT_CONST) {
        struct implicit_search_resolution resolution = jl_resolve_implicit_import(b, NULL, jl_current_task->world_age, 0);
        if (!resolution.debug_only_ultimate_binding)
            jl_error("Constant binding was imported from multiple modules");
        b = resolution.debug_only_ultimate_binding;
    }
    return b ? b->globalref->mod : m;
}

static NOINLINE void print_backdate_admonition(jl_binding_t *b) JL_NOTSAFEPOINT
{
    jl_safe_printf(
        "WARNING: Detected access to binding `%s.%s` in a world prior to its definition world.\n"
        "  Julia 1.12 has introduced more strict world age semantics for global bindings.\n"
        "  !!! This code may malfunction under Revise.\n"
        "  !!! This code will error in future versions of Julia.\n"
        "Hint: Add an appropriate `invokelatest` around the access to this binding.\n",
        jl_symbol_name(b->globalref->mod->name), jl_symbol_name(b->globalref->name));
}

static inline void check_backdated_binding(jl_binding_t *b, enum jl_partition_kind kind) JL_NOTSAFEPOINT
{
    if (__unlikely(kind == PARTITION_KIND_BACKDATED_CONST) &&
        !(jl_atomic_fetch_or_relaxed(&b->flags, BINDING_FLAG_DID_PRINT_BACKDATE_ADMONITION) & BINDING_FLAG_DID_PRINT_BACKDATE_ADMONITION)) {
        print_backdate_admonition(b);
    }
}

JL_DLLEXPORT jl_value_t *jl_get_binding_value(jl_binding_t *b)
{
    return jl_get_binding_value_in_world(b, jl_current_task->world_age);
}

JL_DLLEXPORT jl_value_t *jl_get_binding_value_in_world(jl_binding_t *b, size_t world)
{
    jl_binding_partition_t *bpart = jl_get_binding_partition(b, world);
    jl_walk_binding_inplace(&b, &bpart, world);
    enum jl_partition_kind kind = jl_binding_kind(bpart);
    if (jl_bkind_is_some_guard(kind))
        return NULL;
    if (jl_bkind_is_some_constant(kind)) {
        check_backdated_binding(b, kind);
        return bpart->restriction;
    }
    assert(!jl_bkind_is_some_import(kind));
    return jl_atomic_load_relaxed(&b->value);
}

JL_DLLEXPORT jl_value_t *jl_get_binding_value_depwarn(jl_binding_t *b)
{
    jl_binding_partition_t *bpart = jl_get_binding_partition(b, jl_current_task->world_age);
    if (jl_options.depwarn) {
        int needs_depwarn = 0;
        jl_walk_binding_inplace_depwarn(&b, &bpart, jl_current_task->world_age, &needs_depwarn);
        if (needs_depwarn)
            jl_binding_deprecation_warning(b);
    } else {
        jl_walk_binding_inplace(&b, &bpart, jl_current_task->world_age);
    }
    enum jl_partition_kind kind = jl_binding_kind(bpart);
    if (jl_bkind_is_some_guard(kind))
        return NULL;
    if (jl_bkind_is_some_constant(kind)) {
        check_backdated_binding(b, kind);
        return bpart->restriction;
    }
    assert(!jl_bkind_is_some_import(kind));
    return jl_atomic_load_relaxed(&b->value);
}


JL_DLLEXPORT jl_value_t *jl_get_binding_value_seqcst(jl_binding_t *b)
{
    jl_binding_partition_t *bpart = jl_get_binding_partition(b, jl_current_task->world_age);
    jl_walk_binding_inplace(&b, &bpart, jl_current_task->world_age);
    enum jl_partition_kind kind = jl_binding_kind(bpart);
    if (jl_bkind_is_some_guard(kind))
        return NULL;
    if (jl_bkind_is_some_constant(kind)) {
        check_backdated_binding(b, kind);
        return bpart->restriction;
    }
    assert(!jl_bkind_is_some_import(kind));
    return jl_atomic_load(&b->value);
}

JL_DLLEXPORT jl_value_t *jl_get_binding_value_if_const(jl_binding_t *b)
{
    jl_binding_partition_t *bpart = jl_get_binding_partition(b, jl_current_task->world_age);
    jl_walk_binding_inplace(&b, &bpart, jl_current_task->world_age);
    enum jl_partition_kind kind = jl_binding_kind(bpart);
    if (jl_bkind_is_some_guard(kind))
        return NULL;
    if (!jl_bkind_is_some_constant(kind))
        return NULL;
    check_backdated_binding(b, kind);
    return bpart->restriction;
}

JL_DLLEXPORT jl_value_t *jl_get_binding_value_if_resolved_and_const(jl_binding_t *b)
{
    // Unlike jl_get_binding_value_if_const this doesn't try to allocate new binding partitions if they
    // don't already exist, making this JL_NOTSAFEPOINT.
    if (!b)
        return NULL;
    jl_binding_partition_t *bpart = jl_atomic_load_relaxed(&b->partitions);
    if (!bpart)
        return NULL;
    size_t max_world = jl_atomic_load_relaxed(&bpart->max_world);
    if (bpart->min_world > jl_current_task->world_age || jl_current_task->world_age > max_world)
        return NULL;
    enum jl_partition_kind kind = jl_binding_kind(bpart);
    if (jl_bkind_is_some_guard(kind))
        return NULL;
    if (!jl_bkind_is_some_constant(kind))
        return NULL;
    check_backdated_binding(b, kind);
    return bpart->restriction;
}

JL_DLLEXPORT jl_value_t *jl_get_binding_value_if_resolved(jl_binding_t *b)
{
    // Unlike jl_get_binding_value this doesn't try to allocate new binding partitions if they
    // don't already exist, making this JL_NOTSAFEPOINT.
    if (!b)
        return NULL;
    jl_binding_partition_t *bpart = jl_atomic_load_relaxed(&b->partitions);
    if (!bpart)
        return NULL;
    size_t max_world = jl_atomic_load_relaxed(&bpart->max_world);
    if (bpart->min_world > jl_current_task->world_age || jl_current_task->world_age > max_world)
        return NULL;
    enum jl_partition_kind kind = jl_binding_kind(bpart);
    if (jl_bkind_is_some_guard(kind))
        return NULL;
    if (jl_bkind_is_some_import(kind))
        return NULL;
    if (jl_bkind_is_some_constant(kind)) {
        check_backdated_binding(b, kind);
        return bpart->restriction;
    }
    return jl_atomic_load_relaxed(&b->value);
}

JL_DLLEXPORT jl_value_t *jl_bpart_get_restriction_value(jl_binding_partition_t *bpart)
{
    jl_value_t *v = bpart->restriction;
    if (!v)
        jl_throw(jl_undefref_exception);
    return v;
}

// for error message printing: look up the module that exported a binding to m as var
// this might not be the same as the owner of the binding, since the binding itself may itself have been imported from elsewhere
static jl_module_t *jl_binding_dbgmodule(jl_binding_t *b)
{
    jl_binding_partition_t *bpart = jl_get_binding_partition(b, jl_current_task->world_age);
    enum jl_partition_kind kind = jl_binding_kind(bpart);
    if (jl_bkind_is_some_explicit_import(kind) || kind == PARTITION_KIND_IMPLICIT_GLOBAL) {
        return ((jl_binding_t*)bpart->restriction)->globalref->mod;
    }
    if (kind == PARTITION_KIND_IMPLICIT_CONST) {
        struct implicit_search_resolution resolution = jl_resolve_implicit_import(b, NULL, jl_current_task->world_age, 1);
        return resolution.debug_only_import_from;
    }
    return b->globalref->mod;
}

// Look at the given binding and decide whether to add a new method to an existing generic function
// or ask for the creation of a new generic function (NULL return), checking various error conditions
// along the way.
JL_DLLEXPORT jl_value_t *jl_get_existing_strong_gf(jl_binding_t *b, size_t new_world)
{
    jl_binding_partition_t *bpart = jl_get_binding_partition(b, new_world);
    enum jl_partition_kind kind = jl_binding_kind(bpart);
    if (jl_bkind_is_some_constant(kind) && kind != PARTITION_KIND_IMPLICIT_CONST)
        return bpart->restriction;
    if (jl_bkind_is_some_guard(kind) || kind == PARTITION_KIND_DECLARED) {
        check_safe_newbinding(b->globalref->mod, b->globalref->name);
        return NULL;
    }
    if (!jl_bkind_is_some_import(kind)) {
        jl_errorf("cannot define function %s; it already has a value", jl_symbol_name(b->globalref->name));
    }
    jl_binding_t *ownerb = b;
    jl_walk_binding_inplace(&ownerb, &bpart, new_world);
    jl_value_t *f = NULL;
    if (jl_bkind_is_some_constant(jl_binding_kind(bpart)))
        f = bpart->restriction;
    if (f == NULL) {
        if (jl_bkind_is_some_implicit(kind)) {
            check_safe_newbinding(b->globalref->mod, b->globalref->name);
            return NULL;
        }
        jl_module_t *from = jl_binding_dbgmodule(b);\
        assert(from); // Can only be NULL if implicit, which we excluded above
        jl_errorf("invalid method definition in %s: exported function %s.%s does not exist",
                    jl_module_debug_name(b->globalref->mod), jl_module_debug_name(from), jl_symbol_name(b->globalref->name));
    }
    int istype = f && jl_is_type(f);
    if (!istype) {
        if (jl_bkind_is_some_implicit(kind)) {
            check_safe_newbinding(b->globalref->mod, b->globalref->name);
            return NULL;
        }
        else if (kind != PARTITION_KIND_IMPORTED) {
            // TODO: we might want to require explicitly importing types to add constructors
            //       or we might want to drop this error entirely
            jl_module_t *from = jl_binding_dbgmodule(b);
            assert(from); // Can only be NULL if implicit, which we excluded above
            jl_errorf("invalid method definition in %s: function %s.%s must be explicitly imported to be extended",
                        jl_module_debug_name(b->globalref->mod), jl_module_debug_name(from), jl_symbol_name(b->globalref->name));
        }
    }
    else if (kind != PARTITION_KIND_IMPORTED) {
        int should_error = strcmp(jl_symbol_name(b->globalref->name), "=>") == 0;
        jl_module_t *from = jl_binding_dbgmodule(b);
        if (should_error) {
            jl_errorf("invalid method definition in %s: function %s.%s must be explicitly imported to be extended",
                        jl_module_debug_name(b->globalref->mod), from ? jl_module_debug_name(from) : "<multiple modules>", jl_symbol_name(b->globalref->name));
        }
        else if (!(jl_atomic_fetch_or_relaxed(&b->flags, BINDING_FLAG_DID_PRINT_IMPLICIT_IMPORT_ADMONITION) &
                                              BINDING_FLAG_DID_PRINT_IMPLICIT_IMPORT_ADMONITION)) {
            jl_printf(JL_STDERR, "WARNING: Constructor for type \"%s\" was extended in `%s` without explicit qualification or import.\n"
                                 "  NOTE: Assumed \"%s\" refers to `%s.%s`. This behavior is deprecated and may differ in future versions.\n"
                                 "  NOTE: This behavior may have differed in Julia versions prior to 1.12.\n"
                                 "  Hint: If you intended to create a new generic function of the same name, use `function %s end`.\n"
                                 "  Hint: To silence the warning, qualify `%s` as `%s.%s` in the method signature or explicitly `import %s: %s`.\n",
                jl_symbol_name(b->globalref->name), jl_module_debug_name(b->globalref->mod),
                jl_symbol_name(b->globalref->name), jl_module_debug_name(from), jl_symbol_name(b->globalref->name),
                jl_symbol_name(b->globalref->name), jl_symbol_name(b->globalref->name), jl_module_debug_name(from), jl_symbol_name(b->globalref->name),
                jl_module_debug_name(from), jl_symbol_name(b->globalref->name));
        }
    }
    return f;
}

static void jl_binding_dep_message(jl_binding_t *b);

// get type of binding m.var, without resolving the binding
JL_DLLEXPORT jl_value_t *jl_get_binding_type(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_module_binding(m, var, 0);
    jl_binding_partition_t *bpart = jl_get_binding_partition(b, jl_current_task->world_age);
    if (b == NULL)
        return jl_nothing;
    jl_walk_binding_inplace(&b, &bpart, jl_current_task->world_age);
    enum jl_partition_kind kind = jl_binding_kind(bpart);
    if (jl_bkind_is_some_guard(kind) || kind == PARTITION_KIND_DECLARED)
        return jl_nothing;
    if (jl_bkind_is_some_constant(kind)) {
        // TODO: We would like to return the type of the constant, but
        // currently code relies on this returning any to bypass conversion
        // before an attempted assignment to a constant.
        // return bpart->restriction;
        return (jl_value_t*)jl_any_type;
    }
    return bpart->restriction;
}

JL_DLLEXPORT jl_binding_t *jl_get_binding(jl_module_t *m, jl_sym_t *var)
{
    return jl_get_module_binding(m, var, 1);
}

JL_DLLEXPORT jl_value_t *jl_module_globalref(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_module_binding(m, var, 1);
    jl_globalref_t *globalref = b->globalref;
    assert(globalref != NULL);
    return (jl_value_t*)globalref;
}

// does module m explicitly import s?
JL_DLLEXPORT int jl_is_imported(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_module_binding(m, var, 0);
    jl_binding_partition_t *bpart = jl_get_binding_partition(b, jl_current_task->world_age);
    return b && jl_binding_kind(bpart) == PARTITION_KIND_IMPORTED;
}

extern const char *jl_filename;
extern int jl_lineno;

static char const dep_message_prefix[] = "_dep_message_";

static void jl_binding_dep_message(jl_binding_t *b)
{
    jl_module_t *m = b->globalref->mod;
    jl_sym_t *name = b->globalref->name;
    size_t prefix_len = strlen(dep_message_prefix);
    size_t name_len = strlen(jl_symbol_name(name));
    char *dep_binding_name = (char*)alloca(prefix_len+name_len+1);
    memcpy(dep_binding_name, dep_message_prefix, prefix_len);
    memcpy(dep_binding_name + prefix_len, jl_symbol_name(name), name_len);
    dep_binding_name[prefix_len+name_len] = '\0';
    jl_binding_t *dep_message_binding = jl_get_binding(m, jl_symbol(dep_binding_name));
    jl_value_t *dep_message = NULL;
    if (dep_message_binding != NULL)
        dep_message = jl_get_binding_value(dep_message_binding);
    JL_GC_PUSH1(&dep_message);
    if (dep_message != NULL) {
        if (jl_is_string(dep_message)) {
            jl_uv_puts(JL_STDERR, jl_string_data(dep_message), jl_string_len(dep_message));
        }
        else {
            jl_static_show(JL_STDERR, dep_message);
        }
    }
    else {
        jl_value_t *v = jl_get_binding_value(b);
        dep_message = v; // use as gc-root
        if (v) {
            if (jl_is_type(v) || jl_is_module(v)) {
                jl_printf(JL_STDERR, ", use ");
                jl_static_show(JL_STDERR, v);
                jl_printf(JL_STDERR, " instead.");
            }
            else {
                jl_methtable_t *mt = jl_gf_mtable(v);
                if (mt != NULL) {
                    jl_printf(JL_STDERR, ", use ");
                    if (mt->module != jl_core_module) {
                        jl_static_show(JL_STDERR, (jl_value_t*)mt->module);
                        jl_printf(JL_STDERR, ".");
                    }
                    jl_printf(JL_STDERR, "%s", jl_symbol_name(mt->name));
                    jl_printf(JL_STDERR, " instead.");
                }
            }
        }
    }
    jl_printf(JL_STDERR, "\n");
    JL_GC_POP();
}

JL_DLLEXPORT void check_safe_import_from(jl_module_t *m)
{
    if (jl_options.incremental && jl_generating_output() && m == jl_main_module) {
        jl_errorf("Any `import` or `using` from `Main` is prohibited during incremental compilation.");
    }
}

static int eq_bindings(jl_binding_partition_t *owner, jl_binding_t *alias, size_t world)
{
    jl_binding_t *ownerb = NULL;
    jl_binding_partition_t *alias_bpart = jl_get_binding_partition(alias, world);
    if (owner == alias_bpart)
        return 1;
    jl_walk_binding_inplace(&ownerb, &owner, world);
    jl_walk_binding_inplace(&alias, &alias_bpart, world);
    if (jl_bkind_is_some_constant(jl_binding_kind(owner)) &&
        jl_bkind_is_some_constant(jl_binding_kind(alias_bpart)) &&
        owner->restriction &&
        alias_bpart->restriction == owner->restriction)
        return 1;
    return owner == alias_bpart;
}

// NOTE: we use explici since explicit is a C++ keyword
static void module_import_(jl_task_t *ct, jl_module_t *to, jl_module_t *from, jl_sym_t *asname, jl_sym_t *s, int explici)
{
    check_safe_import_from(from);
    jl_binding_t *b = jl_get_binding(from, s);
    jl_binding_partition_t *bpart = jl_get_binding_partition(b, jl_current_task->world_age);
    if (bpart->kind & PARTITION_FLAG_DEPRECATED) {
        if (jl_get_binding_value(b) == jl_nothing) {
            // silently skip importing deprecated values assigned to nothing (to allow later mutation)
            return;
        }
        else if (to != jl_main_module && to != jl_base_module &&
                    jl_options.depwarn != JL_OPTIONS_DEPWARN_OFF) {
            /* with #22763, external packages wanting to replace
                deprecated Base bindings should simply export the new
                binding */
            jl_printf(JL_STDERR,
                        "WARNING: importing deprecated binding %s.%s into %s%s%s.\n",
                        jl_symbol_name(from->name), jl_symbol_name(s),
                        jl_symbol_name(to->name),
                        asname == s ? "" : " as ",
                        asname == s ? "" : jl_symbol_name(asname));
            jl_binding_dep_message(b);
        }
    }

    jl_binding_t *ownerb = b;
    jl_binding_partition_t *ownerbpart = bpart;
    jl_walk_binding_inplace(&ownerb, &ownerbpart, ct->world_age);

    if (jl_bkind_is_some_guard(jl_binding_kind(ownerbpart))) {
        jl_printf(JL_STDERR,
                  "WARNING: Imported binding %s.%s was undeclared at import time during import to %s.\n",
                  jl_symbol_name(from->name), jl_symbol_name(s),
                  jl_symbol_name(to->name));
    }

    jl_binding_t *bto = jl_get_module_binding(to, asname, 1);
    if (bto == b) {
        // importing a binding on top of itself. harmless.
        return;
    }
    JL_LOCK(&world_counter_lock);
    size_t new_world = jl_atomic_load_acquire(&jl_world_counter)+1;
    jl_binding_partition_t *btopart = jl_get_binding_partition(bto, new_world);
    enum jl_partition_kind btokind = jl_binding_kind(btopart);
    if (jl_bkind_is_some_implicit(btokind)) {
        jl_binding_partition_t *new_bpart = jl_replace_binding_locked(bto, btopart, (jl_value_t*)b, (explici != 0) ? PARTITION_KIND_IMPORTED : PARTITION_KIND_EXPLICIT, new_world);
        if (jl_atomic_load_relaxed(&new_bpart->max_world) == ~(size_t)0)
            jl_add_binding_backedge(b, (jl_value_t*)bto);
        jl_atomic_store_release(&jl_world_counter, new_world);
    }
    else {
        if (eq_bindings(bpart, bto, new_world)) {
            // already imported - potentially upgrade _EXPLICIT to _IMPORTED
            if (btokind == PARTITION_KIND_EXPLICIT && explici != 0) {
                jl_replace_binding_locked(bto, btopart, (jl_value_t*)b, PARTITION_KIND_IMPORTED, new_world);
                jl_atomic_store_release(&jl_world_counter, new_world);
            }
        }
        else if (jl_bkind_is_some_import(btokind)) {
            // already imported from somewhere else
            jl_printf(JL_STDERR,
                        "WARNING: ignoring conflicting import of %s.%s into %s\n",
                        jl_symbol_name(from->name), jl_symbol_name(s),
                        jl_symbol_name(to->name));
        }
        else {
            // conflict with name owned by destination module
            jl_printf(JL_STDERR,
                        "WARNING: import of %s.%s into %s conflicts with an existing identifier; ignored.\n",
                        jl_symbol_name(from->name), jl_symbol_name(s),
                        jl_symbol_name(to->name));
        }
    }
    JL_UNLOCK(&world_counter_lock);
}

JL_DLLEXPORT void jl_module_import(jl_task_t *ct, jl_module_t *to, jl_module_t *from, jl_sym_t *s)
{
    module_import_(ct, to, from, s, s, 1);
}

JL_DLLEXPORT void jl_module_import_as(jl_task_t *ct, jl_module_t *to, jl_module_t *from, jl_sym_t *s, jl_sym_t *asname)
{
    module_import_(ct, to, from, asname, s, 1);
}

JL_DLLEXPORT void jl_module_use(jl_task_t *ct, jl_module_t *to, jl_module_t *from, jl_sym_t *s)
{
    module_import_(ct, to, from, s, s, 0);
}

JL_DLLEXPORT void jl_module_use_as(jl_task_t *ct, jl_module_t *to, jl_module_t *from, jl_sym_t *s, jl_sym_t *asname)
{
    module_import_(ct, to, from, asname, s, 0);
}

void jl_add_usings_backedge(jl_module_t *from, jl_module_t *to)
{
    JL_LOCK(&from->lock);
    if (from->usings_backedges == jl_nothing) {
        from->usings_backedges = (jl_value_t*)jl_alloc_vec_any(0);
        jl_gc_wb(from, from->usings_backedges);
    }
    jl_array_ptr_1d_push((jl_array_t*)from->usings_backedges, (jl_value_t*)to);
    JL_UNLOCK(&from->lock);
}

JL_DLLEXPORT void jl_module_using(jl_module_t *to, jl_module_t *from)
{
    if (to == from)
        return;
    check_safe_import_from(from);
    JL_LOCK(&world_counter_lock);
    JL_LOCK(&to->lock);
    for (size_t i = 0; i < module_usings_length(to); i++) {
        if (from == module_usings_getmod(to, i)) {
            JL_UNLOCK(&to->lock);
            JL_UNLOCK(&world_counter_lock);
            return;
        }
    }

    size_t new_world = jl_atomic_load_acquire(&jl_world_counter)+1;
    struct _jl_module_using new_item = {
        .mod = from,
        .min_world = new_world,
        .max_world = ~(size_t)0
    };
    arraylist_grow(&to->usings, sizeof(struct _jl_module_using)/sizeof(void*));
    memcpy(&to->usings.items[to->usings.len-3], &new_item, sizeof(struct _jl_module_using));
    jl_gc_wb(to, from);

    JL_UNLOCK(&to->lock);

    // Go through all exported bindings. If we have a binding for this in the
    // importing module and it is some import or guard, we need to recompute
    // it.
    jl_svec_t *table = jl_atomic_load_relaxed(&from->bindings);
    for (size_t i = 0; i < jl_svec_len(table); i++) {
        jl_binding_t *b = (jl_binding_t*)jl_svecref(table, i);
        if ((void*)b == jl_nothing)
            break;
        jl_binding_partition_t *frombpart = jl_get_binding_partition(b, new_world);
        if (frombpart->kind & PARTITION_FLAG_EXPORTED) {
            jl_sym_t *var = b->globalref->name;
            jl_binding_t *tob = jl_get_module_binding(to, var, 0);
            if (tob) {
                jl_binding_partition_t *tobpart = jl_atomic_load_relaxed(&tob->partitions);
                if (tobpart) {
                    enum jl_partition_kind kind = jl_binding_kind(tobpart);
                    if (jl_bkind_is_some_implicit(kind)) {
                        jl_replace_binding_locked(tob, tobpart, NULL, PARTITION_FAKE_KIND_IMPLICIT_RECOMPUTE, new_world);
                    }
                }
            }
        }
        table = jl_atomic_load_relaxed(&from->bindings);
    }

    jl_add_usings_backedge(from, to);

    jl_atomic_store_release(&jl_world_counter, new_world);
    JL_UNLOCK(&world_counter_lock);
}

JL_DLLEXPORT jl_value_t *jl_get_module_usings_backedges(jl_module_t *m)
{
    // We assume the caller holds the world_counter_lock, which is the only place we set this
    // TODO: We may want to make this more precise with the module lock
    return m->usings_backedges;
}

JL_DLLEXPORT size_t jl_module_scanned_methods_length(jl_module_t *m)
{
    JL_LOCK(&m->lock);
    size_t len = 0;
    if (m->scanned_methods != jl_nothing)
        len = jl_array_len(m->scanned_methods);
    JL_UNLOCK(&m->lock);
    return len;
}

JL_DLLEXPORT jl_value_t *jl_module_scanned_methods_getindex(jl_module_t *m, size_t i)
{
    JL_LOCK(&m->lock);
    assert(m->scanned_methods != jl_nothing);
    jl_value_t *ret = jl_array_ptr_ref(m->scanned_methods, i-1);
    JL_UNLOCK(&m->lock);
    return ret;
}

JL_DLLEXPORT jl_value_t *jl_get_module_binding_or_nothing(jl_module_t *m, jl_sym_t *s)
{
    jl_binding_t *b = jl_get_module_binding(m, s, 0);
    if (!b)
        return jl_nothing;
    return (jl_value_t*)b;
}

JL_DLLEXPORT void jl_module_public(jl_module_t *from, jl_sym_t *s, int exported)
{
    jl_binding_t *b = jl_get_module_binding(from, s, 1);
    JL_LOCK(&world_counter_lock);
    size_t new_world = jl_atomic_load_acquire(&jl_world_counter)+1;
    jl_binding_partition_t *bpart = jl_get_binding_partition(b, new_world);
    int was_exported = (bpart->kind & PARTITION_FLAG_EXPORTED) != 0;
    if (jl_atomic_load_relaxed(&b->flags) & BINDING_FLAG_PUBLICP) {
        // check for conflicting declarations
        if (was_exported && !exported)
            jl_errorf("cannot declare %s.%s public; it is already declared exported",
                      jl_symbol_name(from->name), jl_symbol_name(s));
        if (!was_exported && exported)
            jl_errorf("cannot declare %s.%s exported; it is already declared public",
                      jl_symbol_name(from->name), jl_symbol_name(s));
    }
    jl_atomic_fetch_or_relaxed(&b->flags, BINDING_FLAG_PUBLICP);
    if (was_exported != exported) {
        jl_replace_binding_locked2(b, bpart, bpart->restriction, bpart->kind | PARTITION_FLAG_EXPORTED, new_world);
        jl_atomic_store_release(&jl_world_counter, new_world);
    }
    JL_UNLOCK(&world_counter_lock);
}

JL_DLLEXPORT int jl_boundp(jl_module_t *m, jl_sym_t *var, int allow_import) // unlike most queries here, this is currently seq_cst
{
    jl_binding_t *b = jl_get_module_binding(m, var, allow_import);
    if (!b)
        return 0;
    jl_binding_partition_t *bpart = jl_get_binding_partition(b, jl_current_task->world_age);
    if (!bpart)
        return 0;
    if (!allow_import) {
        if (!bpart || jl_bkind_is_some_import(jl_binding_kind(bpart)))
            return 0;
    } else {
        jl_walk_binding_inplace(&b, &bpart, jl_current_task->world_age);
    }
    if (jl_bkind_is_some_guard(jl_binding_kind(bpart)))
        return 0;
    if (jl_bkind_is_defined_constant(jl_binding_kind(bpart))) {
        // N.B.: No backdated check for isdefined
        return 1;
    }
    return jl_atomic_load(&b->value) != NULL;
}

JL_DLLEXPORT int jl_defines_or_exports_p(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_module_binding(m, var, 0);
    jl_binding_partition_t *bpart = jl_get_binding_partition(b, jl_current_task->world_age);
    return b && ((bpart->kind & PARTITION_FLAG_EXPORTED) || jl_binding_kind(bpart) == PARTITION_KIND_GLOBAL);
}

JL_DLLEXPORT int jl_module_exports_p(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_module_binding(m, var, 0);
    jl_binding_partition_t *bpart = jl_get_binding_partition(b, jl_current_task->world_age);
    return b && (bpart->kind & PARTITION_FLAG_EXPORTED);
}

JL_DLLEXPORT int jl_module_public_p(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_module_binding(m, var, 0);
    return b && (jl_atomic_load_relaxed(&b->flags) & BINDING_FLAG_PUBLICP);
}

uint_t bindingkey_hash(size_t idx, jl_value_t *data)
{
    jl_binding_t *b = (jl_binding_t*)jl_svecref(data, idx); // This must always happen inside the lock
    jl_sym_t *var = b->globalref->name;
    return var->hash;
}

static int bindingkey_eq(size_t idx, const void *var, jl_value_t *data, uint_t hv)
{
    if (idx >= jl_svec_len(data))
        return 0; // We got a OOB access, probably due to a data race
    jl_binding_t *b = (jl_binding_t*)jl_svecref(data, idx);
    jl_sym_t *name = b->globalref->name;
    return var == name;
}

JL_DLLEXPORT jl_binding_t *jl_get_module_binding(jl_module_t *m, jl_sym_t *var, int alloc)
{
    uint_t hv = var->hash;
    for (int locked = 0; ; locked++) {
        jl_genericmemory_t *bindingkeyset = jl_atomic_load_acquire(&m->bindingkeyset);
        jl_svec_t *bindings = jl_atomic_load_relaxed(&m->bindings);
        ssize_t idx = jl_smallintset_lookup(bindingkeyset, bindingkey_eq, var, (jl_value_t*)bindings, hv, 0); // acquire
        if (idx != -1) {
            jl_binding_t *b = (jl_binding_t*)jl_svecref(bindings, idx); // relaxed
            JL_GC_PROMISE_ROOTED(b);
            if (locked)
                JL_UNLOCK(&m->lock);
            return b;
        }
        if (!alloc) {
            return NULL;
        }
        else if (!locked) {
            JL_LOCK(&m->lock);
        }
        else {
            size_t i, cl = jl_svec_len(bindings);
            for (i = cl; i > 0; i--) {
                jl_value_t *b = jl_svecref(bindings, i - 1);
                if (b != jl_nothing)
                    break;
            }
            if (i == cl) {
                size_t ncl = cl < 8 ? 8 : (cl*3)>>1; // grow 50%
                jl_svec_t *nc = jl_alloc_svec_uninit(ncl);
                if (i > 0)
                    memcpy((char*)jl_svec_data(nc), jl_svec_data(bindings), sizeof(void*) * i);
                for (size_t j = i; j < ncl; j++)
                    jl_svec_data(nc)[j] = jl_nothing;
                jl_atomic_store_release(&m->bindings, nc);
                jl_gc_wb(m, nc);
                bindings = nc;
            }
            jl_binding_t *b = new_binding(m, var);
            assert(jl_svecref(bindings, i) == jl_nothing);
            jl_svecset(bindings, i, b); // relaxed
            jl_smallintset_insert(&m->bindingkeyset, (jl_value_t*)m, bindingkey_hash, i, (jl_value_t*)bindings); // release
            JL_UNLOCK(&m->lock);
            return b;
        }
    }
}


JL_DLLEXPORT jl_value_t *jl_get_globalref_value(jl_globalref_t *gr)
{
    jl_binding_t *b = gr->binding;
    if (!b)
        b = jl_get_module_binding(gr->mod, gr->name, 1);
    return jl_get_binding_value_depwarn(b);
}

JL_DLLEXPORT jl_value_t *jl_get_global(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_module_binding(m, var, 1);
    return jl_get_binding_value_depwarn(b);
}

JL_DLLEXPORT void jl_set_global(jl_module_t *m JL_ROOTING_ARGUMENT, jl_sym_t *var, jl_value_t *val JL_ROOTED_ARGUMENT)
{
    jl_binding_t *bp = jl_get_binding_wr(m, var);
    jl_checked_assignment(bp, m, var, val);
}

JL_DLLEXPORT void jl_set_const(jl_module_t *m JL_ROOTING_ARGUMENT, jl_sym_t *var, jl_value_t *val JL_ROOTED_ARGUMENT)
{
    // this function is mostly only used during initialization, so the data races here are not too important to us
    jl_binding_t *bp = jl_get_module_binding(m, var, 1);
    jl_binding_partition_t *bpart = jl_get_binding_partition(bp, jl_current_task->world_age);
    bpart->min_world = 0;
    jl_atomic_store_release(&bpart->max_world, ~(size_t)0);
    bpart->kind = PARTITION_KIND_CONST | (bpart->kind & PARTITION_MASK_FLAG);
    bpart->restriction = val;
    jl_gc_wb(bpart, val);
}

void jl_invalidate_binding_refs(jl_globalref_t *ref, jl_binding_partition_t *invalidated_bpart, jl_binding_partition_t *new_bpart, size_t new_world)
{
    static jl_value_t *invalidate_code_for_globalref = NULL;
    if (invalidate_code_for_globalref == NULL && jl_base_module != NULL)
        invalidate_code_for_globalref = jl_get_global(jl_base_module, jl_symbol("invalidate_code_for_globalref!"));
    if (!invalidate_code_for_globalref)
        jl_error("Binding invalidation is not permitted during bootstrap.");
    jl_value_t **fargs;
    JL_GC_PUSHARGS(fargs, 5);
    fargs[0] = (jl_function_t*)invalidate_code_for_globalref;
    fargs[1] = (jl_value_t*)ref;
    fargs[2] = (jl_value_t*)invalidated_bpart;
    fargs[3] = (jl_value_t*)new_bpart;
    fargs[4] = jl_box_ulong(new_world);
    jl_apply(fargs, 5);
    JL_GC_POP();
}

JL_DLLEXPORT void jl_add_binding_backedge(jl_binding_t *b, jl_value_t *edge)
{
    if (!b->backedges) {
        b->backedges = jl_alloc_vec_any(0);
        jl_gc_wb(b, b->backedges);
    } else if (jl_array_len(b->backedges) > 0 &&
               jl_array_ptr_ref(b->backedges, jl_array_len(b->backedges)-1) == edge) {
        // Optimization: Deduplicate repeated insertion of the same edge (e.g. during
        // definition of a method that contains many references to the same global)
        return;
    }
    jl_array_ptr_1d_push(b->backedges, edge);
}

// Called for all GlobalRefs found in lowered code. Adds backedges for cross-module
// GlobalRefs.
JL_DLLEXPORT int jl_maybe_add_binding_backedge(jl_binding_t *b, jl_value_t *edge, jl_method_t *for_method)
{
    if (!edge)
        return 0;
    jl_module_t *defining_module = for_method->module;
    // N.B.: The logic for evaluating whether a backedge is required must
    // match the invalidation logic.
    if (b->globalref->mod == defining_module) {
        // No backedge required - invalidation will forward scan
        jl_atomic_fetch_or(&b->flags, BINDING_FLAG_ANY_IMPLICIT_EDGES);
        if (!(jl_atomic_fetch_or(&for_method->did_scan_source, 0x2) & 0x2))
            jl_add_scanned_method(for_method->module, for_method);
        return 1;
    }
    jl_add_binding_backedge(b, (jl_value_t*)edge);
    return 0;
}

JL_DLLEXPORT jl_binding_partition_t *jl_replace_binding_locked(jl_binding_t *b,
    jl_binding_partition_t *old_bpart, jl_value_t *restriction_val, enum jl_partition_kind kind, size_t new_world)
{
    // Copy flags from old bpart
    return jl_replace_binding_locked2(b, old_bpart, restriction_val, (size_t)kind | (size_t)(old_bpart->kind & PARTITION_MASK_FLAG),
        new_world);
}

extern JL_DLLEXPORT _Atomic(size_t) jl_first_image_replacement_world;
JL_DLLEXPORT jl_binding_partition_t *jl_replace_binding_locked2(jl_binding_t *b,
    jl_binding_partition_t *old_bpart, jl_value_t *restriction_val, size_t kind, size_t new_world)
{
    check_safe_newbinding(b->globalref->mod, b->globalref->name);

    // Check if this is a replacing a binding in the system or a package image.
    // Until the first such replacement, we can fast-path validation.
    // For these purposes, we consider the `Main` module to be a non-sysimg module.
    // This is legal, because we special case the `Main` in check_safe_import_from.
    if (jl_object_in_image((jl_value_t*)b) && b->globalref->mod != jl_main_module && jl_atomic_load_relaxed(&jl_first_image_replacement_world) == ~(size_t)0)
        jl_atomic_store_relaxed(&jl_first_image_replacement_world, new_world);

    assert(jl_atomic_load_relaxed(&b->partitions) == old_bpart);
    jl_binding_partition_t *new_bpart = new_binding_partition();
    JL_GC_PUSH1(&new_bpart);
    new_bpart->min_world = new_world;
    if ((kind & PARTITION_MASK_KIND) == PARTITION_FAKE_KIND_IMPLICIT_RECOMPUTE) {
        assert(!restriction_val);
        struct implicit_search_resolution resolution = jl_resolve_implicit_import(b, NULL, new_world, 0);
        new_bpart->kind = resolution.ultimate_kind | (kind & PARTITION_MASK_FLAG);
        new_bpart->restriction = resolution.binding_or_const;
        assert(resolution.min_world <= new_world && resolution.max_world == ~(size_t)0);
        if (new_bpart->kind == old_bpart->kind && new_bpart->restriction == old_bpart->restriction) {
            JL_GC_POP();
            return old_bpart;
        }
    }
    else {
        new_bpart->kind = kind;
        new_bpart->restriction = restriction_val;
        jl_gc_wb_fresh(new_bpart, restriction_val);
    }
    jl_atomic_store_release(&old_bpart->max_world, new_world-1);
    jl_atomic_store_relaxed(&new_bpart->next, old_bpart);
    jl_gc_wb_fresh(new_bpart, old_bpart);

    if (((old_bpart->kind & PARTITION_FLAG_EXPORTED) || (kind & PARTITION_FLAG_EXPORTED)) && jl_require_world != ~(size_t)0) {
        jl_atomic_store_release(&b->globalref->mod->export_set_changed_since_require_world, 1);
    }

    jl_atomic_store_release(&b->partitions, new_bpart);
    jl_gc_wb(b, new_bpart);
    JL_GC_POP();

    if (jl_typeinf_world != 1) {
        jl_task_t *ct = jl_current_task;
        size_t last_world = ct->world_age;
        ct->world_age = jl_typeinf_world;
        jl_invalidate_binding_refs(b->globalref, old_bpart, new_bpart, new_world-1);
        ct->world_age = last_world;
    }

    return new_bpart;
}

JL_DLLEXPORT jl_binding_partition_t *jl_replace_binding(jl_binding_t *b,
        jl_binding_partition_t *old_bpart, jl_value_t *restriction_val, enum jl_partition_kind kind) {

    JL_LOCK(&world_counter_lock);

    if (jl_atomic_load_relaxed(&b->partitions) != old_bpart) {
        JL_UNLOCK(&world_counter_lock);
        return NULL;
    }

    size_t new_world = jl_atomic_load_acquire(&jl_world_counter)+1;
    jl_binding_partition_t *bpart = jl_replace_binding_locked(b, old_bpart, restriction_val, kind, new_world);
    if (bpart && bpart->min_world == new_world)
        jl_atomic_store_release(&jl_world_counter, new_world);

    JL_UNLOCK(&world_counter_lock);
    return bpart;
}

JL_DLLEXPORT int jl_globalref_is_const(jl_globalref_t *gr)
{
    jl_binding_t *b = gr->binding;
    if (!b)
        b = jl_get_module_binding(gr->mod, gr->name, 1);
    jl_binding_partition_t *bpart = jl_get_binding_partition(b, jl_current_task->world_age);
    jl_walk_binding_inplace(&b, &bpart, jl_current_task->world_age);
    return jl_bkind_is_some_constant(jl_binding_kind(bpart));
}

JL_DLLEXPORT void jl_disable_binding(jl_globalref_t *gr)
{
    jl_binding_t *b = gr->binding;
    if (!b)
        b = jl_get_module_binding(gr->mod, gr->name, 1);
    jl_binding_partition_t *bpart = jl_get_binding_partition(b, jl_current_task->world_age);

    if (jl_binding_kind(bpart) == PARTITION_KIND_GUARD) {
        // Already guard
        return;
    }

    for (;;)
        if (jl_replace_binding(b, bpart, NULL, PARTITION_KIND_GUARD))
            break;
}

JL_DLLEXPORT int jl_is_const(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_binding(m, var);
    jl_binding_partition_t *bpart = jl_get_binding_partition(b, jl_current_task->world_age);
    jl_walk_binding_inplace(&b, &bpart, jl_current_task->world_age);
    return b && jl_bkind_is_some_constant(jl_binding_kind(bpart));
}

// set the deprecated flag for a binding:
//   0=not deprecated, 1=renamed, 2=moved to another package
static const size_t DEPWARN_FLAGS = PARTITION_FLAG_DEPRECATED | PARTITION_FLAG_DEPWARN;
JL_DLLEXPORT void jl_deprecate_binding(jl_module_t *m, jl_sym_t *var, int flag)
{
    jl_binding_t *b = jl_get_binding(m, var);
    size_t new_flags = flag == 1 ? PARTITION_FLAG_DEPRECATED | PARTITION_FLAG_DEPWARN :
                       flag == 2 ? PARTITION_FLAG_DEPRECATED :
                                   0;
    JL_LOCK(&world_counter_lock);
    size_t new_world = jl_atomic_load_acquire(&jl_world_counter)+1;
    jl_binding_partition_t *old_bpart = jl_get_binding_partition(b, jl_current_task->world_age);
    if ((old_bpart->kind & DEPWARN_FLAGS) == new_flags) {
        JL_UNLOCK(&world_counter_lock);
        return;
    }
    jl_replace_binding_locked2(b, old_bpart, old_bpart->restriction,
        (old_bpart->kind & ~DEPWARN_FLAGS) | new_flags, new_world);
    jl_atomic_store_release(&jl_world_counter, new_world);
    JL_UNLOCK(&world_counter_lock);
}

static int should_depwarn(jl_binding_t *b, uint8_t flag)
{
    // We consider bindings deprecated, if:
    //
    // 1. The binding itself is deprecated, or
    // 2. We implicitly import any deprecated binding.
    //
    // However, we do not consider the binding deprecated if the import was an explicit
    // (`using` or `import`). The logic here is that the thing that needs to be adjusted
    // is not the use itself, but rather the `using` or `import` (which already prints
    // an appropriate warning).
    jl_binding_partition_t *bpart = jl_get_binding_partition(b, jl_current_task->world_age);
    if (bpart->kind & flag)
        return 1;
    return 0;
}

JL_DLLEXPORT void jl_binding_deprecation_check(jl_binding_t *b)
{
    if (jl_options.depwarn && should_depwarn(b, PARTITION_FLAG_DEPWARN))
        jl_binding_deprecation_warning(b);
}

JL_DLLEXPORT int jl_is_binding_deprecated(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_module_binding(m, var, 0);
    if (!b)
        return 0;
    return should_depwarn(b, PARTITION_FLAG_DEPRECATED);
}

void jl_binding_deprecation_warning(jl_binding_t *b)
{
    if (jl_options.depwarn != JL_OPTIONS_DEPWARN_ERROR)
        jl_printf(JL_STDERR, "WARNING: ");
    jl_printf(JL_STDERR, "Use of ");

    jl_printf(JL_STDERR, "%s.%s is deprecated",
                jl_symbol_name(b->globalref->mod->name), jl_symbol_name(b->globalref->name));
    jl_binding_dep_message(b);

    if (jl_options.depwarn != JL_OPTIONS_DEPWARN_ERROR) {
        if (jl_lineno != 0) {
            jl_printf(JL_STDERR, "  likely near %s:%d\n", jl_filename, jl_lineno);
        }
    }

    if (jl_options.depwarn == JL_OPTIONS_DEPWARN_ERROR) {
        jl_errorf("use of deprecated variable: %s.%s",
                    jl_symbol_name(b->globalref->mod->name),
                    jl_symbol_name(b->globalref->name));
    }
}

// For a generally writable binding (checked using jl_check_binding_currently_writable in this world age), check whether
// we can actually write the value `rhs` to it.
jl_value_t *jl_check_binding_assign_value(jl_binding_t *b JL_PROPAGATES_ROOT, jl_module_t *mod, jl_sym_t *var, jl_value_t *rhs JL_MAYBE_UNROOTED)
{
    JL_GC_PUSH1(&rhs); // callee-rooted
    jl_binding_partition_t *bpart = jl_get_binding_partition(b, jl_current_task->world_age);
    enum jl_partition_kind kind = jl_binding_kind(bpart);
    assert(kind == PARTITION_KIND_DECLARED || kind == PARTITION_KIND_GLOBAL);
    jl_value_t *old_ty = kind == PARTITION_KIND_DECLARED ? (jl_value_t*)jl_any_type : bpart->restriction;
    JL_GC_PROMISE_ROOTED(old_ty);
    if (old_ty != (jl_value_t*)jl_any_type && jl_typeof(rhs) != old_ty) {
        if (!jl_isa(rhs, old_ty))
            jl_errorf("cannot assign an incompatible value to the global %s.%s.",
                        jl_symbol_name(mod->name), jl_symbol_name(var));
    }
    JL_GC_POP();
    return old_ty;
}

JL_DLLEXPORT void jl_checked_assignment(jl_binding_t *b, jl_module_t *mod, jl_sym_t *var, jl_value_t *rhs)
{
    if (jl_check_binding_assign_value(b, mod, var, rhs) != NULL) {
        jl_atomic_store_release(&b->value, rhs);
        jl_gc_wb(b, rhs);
    }
}

JL_DLLEXPORT jl_value_t *jl_checked_swap(jl_binding_t *b, jl_module_t *mod, jl_sym_t *var, jl_value_t *rhs)
{
    jl_check_binding_assign_value(b, mod, var, rhs);
    jl_value_t *old = jl_atomic_exchange(&b->value, rhs);
    jl_gc_wb(b, rhs);
    if (__unlikely(old == NULL))
        jl_undefined_var_error(var, (jl_value_t*)mod);
    return old;
}

JL_DLLEXPORT jl_value_t *jl_checked_replace(jl_binding_t *b, jl_module_t *mod, jl_sym_t *var, jl_value_t *expected, jl_value_t *rhs)
{
    jl_value_t *ty = jl_check_binding_assign_value(b, mod, var, rhs);
    return replace_value(ty, &b->value, (jl_value_t*)b, expected, rhs, 1, mod, var);
}

JL_DLLEXPORT jl_value_t *jl_checked_modify(jl_binding_t *b, jl_module_t *mod, jl_sym_t *var, jl_value_t *op, jl_value_t *rhs)
{
    jl_binding_partition_t *bpart = jl_get_binding_partition(b, jl_current_task->world_age);
    enum jl_partition_kind kind = jl_binding_kind(bpart);
    assert(!jl_bkind_is_some_guard(kind) && !jl_bkind_is_some_import(kind));
    if (jl_bkind_is_some_constant(kind))
        jl_errorf("invalid assignment to constant %s.%s",
                  jl_symbol_name(mod->name), jl_symbol_name(var));
    jl_value_t *ty = bpart->restriction;
    JL_GC_PROMISE_ROOTED(ty);
    return modify_value(ty, &b->value, (jl_value_t*)b, op, rhs, 1, mod, var);
}

JL_DLLEXPORT jl_value_t *jl_checked_assignonce(jl_binding_t *b, jl_module_t *mod, jl_sym_t *var, jl_value_t *rhs )
{
    jl_check_binding_assign_value(b, mod, var, rhs);
    jl_value_t *old = NULL;
    if (jl_atomic_cmpswap(&b->value, &old, rhs))
        jl_gc_wb(b, rhs);
    return old;
}

JL_DLLEXPORT jl_value_t *jl_module_usings(jl_module_t *m)
{
    JL_LOCK(&m->lock);
    int j = module_usings_length(m);
    jl_array_t *a = jl_alloc_array_1d(jl_array_any_type, j);
    JL_GC_PUSH1(&a);
    for (int i = 0; j > 0; i++) {
        j--;
        jl_module_t *imp = module_usings_getmod(m, i);
        jl_array_ptr_set(a, j, (jl_value_t*)imp);
    }
    JL_UNLOCK(&m->lock); // may gc
    JL_GC_POP();
    return (jl_value_t*)a;
}

void _append_symbol_to_bindings_array(jl_array_t* a, jl_sym_t *name) {
    jl_array_grow_end(a, 1);
    //XXX: change to jl_arrayset if array storage allocation for Array{Symbols,1} changes:
    jl_array_ptr_set(a, jl_array_dim0(a)-1, (jl_value_t*)name);
}

void append_module_names(jl_array_t* a, jl_module_t *m, int all, int imported, int usings)
{
    jl_svec_t *table = jl_atomic_load_relaxed(&m->bindings);
    for (size_t i = 0; i < jl_svec_len(table); i++) {
        jl_binding_t *b = (jl_binding_t*)jl_svecref(table, i);
        if ((void*)b == jl_nothing)
            break;
        jl_sym_t *asname = b->globalref->name;
        int hidden = jl_symbol_name(asname)[0]=='#';
        int main_public = (m == jl_main_module && !(asname == jl_eval_sym || asname == jl_include_sym));
        jl_binding_partition_t *bpart = jl_get_binding_partition(b, jl_current_task->world_age);
        enum jl_partition_kind kind = jl_binding_kind(bpart);
        if (((jl_atomic_load_relaxed(&b->flags) & BINDING_FLAG_PUBLICP) ||
             (imported && (kind == PARTITION_KIND_CONST_IMPORT || kind == PARTITION_KIND_IMPORTED)) ||
             (usings && kind == PARTITION_KIND_EXPLICIT) ||
             ((kind == PARTITION_KIND_GLOBAL || kind == PARTITION_KIND_CONST || kind == PARTITION_KIND_DECLARED) && (all || main_public))) &&
            (all || (!(bpart->kind & PARTITION_FLAG_DEPRECATED) && !hidden)))
            _append_symbol_to_bindings_array(a, asname);
    }
}

void append_exported_names(jl_array_t* a, jl_module_t *m, int all)
{
    jl_svec_t *table = jl_atomic_load_relaxed(&m->bindings);
    for (size_t i = 0; i < jl_svec_len(table); i++) {
        jl_binding_t *b = (jl_binding_t*)jl_svecref(table, i);
        if ((void*)b == jl_nothing)
            break;
        jl_binding_partition_t *bpart = jl_get_binding_partition(b, jl_current_task->world_age);
        if ((bpart->kind & PARTITION_FLAG_EXPORTED) && (all || !(bpart->kind & PARTITION_FLAG_DEPRECATED)))
            _append_symbol_to_bindings_array(a, b->globalref->name);
    }
}

JL_DLLEXPORT jl_value_t *jl_module_names(jl_module_t *m, int all, int imported, int usings)
{
    jl_array_t *a = jl_alloc_array_1d(jl_array_symbol_type, 0);
    JL_GC_PUSH1(&a);
    append_module_names(a, m, all, imported, usings);
    if (usings) {
        // If `usings` is specified, traverse the list of `using`-ed modules and incorporate
        // the names exported by those modules into the list.
        for (int i = module_usings_length(m)-1; i >= 0; i--)
            append_exported_names(a, module_usings_getmod(m, i), all);
    }
    JL_GC_POP();
    return (jl_value_t*)a;
}

JL_DLLEXPORT jl_sym_t *jl_module_name(jl_module_t *m) { return m->name; }
JL_DLLEXPORT jl_module_t *jl_module_parent(jl_module_t *m) { return m->parent; }
jl_module_t *jl_module_root(jl_module_t *m)
{
    while (1) {
        if (m->parent == NULL || m->parent == m)
            return m;
        m = m->parent;
    }
}

JL_DLLEXPORT jl_sym_t *jl_module_getloc(jl_module_t *m, int32_t *line)
{
    if (line) {
        *line = m->line;
    }
    return m->file;
}

JL_DLLEXPORT jl_uuid_t jl_module_build_id(jl_module_t *m) { return m->build_id; }
JL_DLLEXPORT jl_uuid_t jl_module_uuid(jl_module_t* m) { return m->uuid; }

// TODO: make this part of the module constructor and read-only?
JL_DLLEXPORT void jl_set_module_uuid(jl_module_t *m, jl_uuid_t uuid) { m->uuid = uuid; }
JL_DLLEXPORT void jl_set_module_parent(jl_module_t *m, jl_module_t *parent) { m->parent = parent; }

int jl_is_submodule(jl_module_t *child, jl_module_t *parent) JL_NOTSAFEPOINT
{
    while (1) {
        if (parent == child)
            return 1;
        if (child == NULL || child == child->parent)
            return 0;
        child = child->parent;
    }
}

// Remove implicitly imported identifiers, effectively resetting all the binding
// resolution decisions for a module. This is dangerous, and should only be
// done for modules that are essentially empty anyway. The only use case for this
// is to leave `Main` as empty as possible in the default system image.
JL_DLLEXPORT void jl_clear_implicit_imports(jl_module_t *m)
{
    JL_LOCK(&m->lock);
    jl_svec_t *table = jl_atomic_load_relaxed(&m->bindings);
    for (size_t i = 0; i < jl_svec_len(table); i++) {
        jl_binding_t *b = (jl_binding_t*)jl_svecref(table, i);
        if ((void*)b == jl_nothing)
            break;
        jl_binding_partition_t *bpart = jl_get_binding_partition(b, jl_current_task->world_age);
        if (jl_bkind_is_some_implicit(jl_binding_kind(bpart))) {
            jl_atomic_store_relaxed(&b->partitions, NULL);
        }
    }
    JL_UNLOCK(&m->lock);
}

JL_DLLEXPORT void jl_add_to_module_init_list(jl_value_t *mod)
{
    if (jl_module_init_order == NULL)
        jl_module_init_order = jl_alloc_vec_any(0);
    jl_array_ptr_1d_push(jl_module_init_order, mod);
}

JL_DLLEXPORT jl_svec_t *jl_module_get_bindings(jl_module_t *m)
{
    return jl_atomic_load_relaxed(&m->bindings);
}

JL_DLLEXPORT void jl_init_restored_module(jl_value_t *mod)
{
    if (!jl_generating_output() || jl_options.incremental) {
        jl_module_run_initializer((jl_module_t*)mod);
    }
    else {
        jl_add_to_module_init_list(mod);
    }
}

#ifdef __cplusplus
}
#endif

// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  modules and top-level bindings
*/
#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

JL_DLLEXPORT jl_module_t *jl_new_module_(jl_sym_t *name, jl_module_t *parent, uint8_t default_names)
{
    jl_task_t *ct = jl_current_task;
    const jl_uuid_t uuid_zero = {0, 0};
    jl_module_t *m = (jl_module_t*)jl_gc_alloc(ct->ptls, sizeof(jl_module_t),
                                               jl_module_type);
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
    m->primary_world = 0;
    jl_atomic_store_relaxed(&m->counter, 1);
    m->nospecialize = 0;
    m->optlevel = -1;
    m->compile = -1;
    m->infer = -1;
    m->max_methods = -1;
    m->hash = parent == NULL ? bitmix(name->hash, jl_module_type->hash) :
        bitmix(name->hash, parent->hash);
    JL_MUTEX_INIT(&m->lock, "module->lock");
    jl_atomic_store_relaxed(&m->bindings, jl_emptysvec);
    jl_atomic_store_relaxed(&m->bindingkeyset, (jl_array_t*)jl_an_empty_vec_any);
    arraylist_new(&m->usings, 0);
    JL_GC_PUSH1(&m);
    if (jl_core_module && default_names) {
        jl_module_using(m, jl_core_module);
    }
    // export own name, so "using Foo" makes "Foo" itself visible
    if (default_names) {
        jl_set_const(m, name, (jl_value_t*)m);
    }
    jl_module_export(m, name);
    JL_GC_POP();
    return m;
}

JL_DLLEXPORT jl_module_t *jl_new_module(jl_sym_t *name, jl_module_t *parent)
{
    return jl_new_module_(name, parent, 1);
}

uint32_t jl_module_next_counter(jl_module_t *m)
{
    return jl_atomic_fetch_add(&m->counter, 1);
}

JL_DLLEXPORT jl_value_t *jl_f_new_module(jl_sym_t *name, uint8_t std_imports, uint8_t default_names)
{
    // TODO: should we prohibit this during incremental compilation?
    // TODO: the parent module is a lie
    jl_module_t *m = jl_new_module_(name, jl_main_module, default_names);
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
    jl_gc_wb(g, g->mod);
    g->name = name;
    g->binding = b;
    return g;
}

static jl_binding_t *new_binding(jl_module_t *mod, jl_sym_t *name)
{
    jl_task_t *ct = jl_current_task;
    assert(jl_is_module(mod) && jl_is_symbol(name));
    jl_binding_t *b = (jl_binding_t*)jl_gc_alloc(ct->ptls, sizeof(jl_binding_t), jl_binding_type);
    jl_atomic_store_relaxed(&b->value, NULL);
    jl_atomic_store_relaxed(&b->owner, NULL);
    jl_atomic_store_relaxed(&b->ty, NULL);
    b->globalref = NULL;
    b->constp = 0;
    b->exportp = 0;
    b->imported = 0;
    b->deprecated = 0;
    b->usingfailed = 0;
    b->padding = 0;
    JL_GC_PUSH1(&b);
    b->globalref = jl_new_globalref(mod, name, b);
    JL_GC_POP();
    return b;
}

static jl_module_t *jl_binding_dbgmodule(jl_binding_t *b, jl_module_t *m, jl_sym_t *var) JL_GLOBALLY_ROOTED;

// get binding for assignment
JL_DLLEXPORT jl_binding_t *jl_get_binding_wr(jl_module_t *m JL_PROPAGATES_ROOT, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_module_binding(m, var, 1);

    if (b) {
        jl_binding_t *b2 = NULL;
        if (!jl_atomic_cmpswap(&b->owner, &b2, b) && b2 != b) {
            jl_module_t *from = jl_binding_dbgmodule(b, m, var);
            if (from == m)
                jl_errorf("cannot assign a value to imported variable %s.%s",
                          jl_symbol_name(from->name), jl_symbol_name(var));
            else
                jl_errorf("cannot assign a value to imported variable %s.%s from module %s",
                          jl_symbol_name(from->name), jl_symbol_name(var), jl_symbol_name(m->name));
        }
    }

    return b;
}

// return module of binding
JL_DLLEXPORT jl_module_t *jl_get_module_of_binding(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_binding(m, var);
    if (b == NULL)
        return NULL;
    return b->globalref->mod; // TODO: deprecate this?
}

// get binding for adding a method
// like jl_get_binding_wr, but has different error paths
JL_DLLEXPORT jl_binding_t *jl_get_binding_for_method_def(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_module_binding(m, var, 1);

    jl_binding_t *b2 = NULL;
    if (!jl_atomic_cmpswap(&b->owner, &b2, b) && b2 != b) {
        jl_value_t *f = jl_atomic_load_relaxed(&b2->value);
        jl_module_t *from = jl_binding_dbgmodule(b, m, var);
        if (f == NULL) {
            // we must have implicitly imported this with using, so call jl_binding_dbgmodule to try to get the name of the module we got this from
            jl_errorf("invalid method definition in %s: exported function %s.%s does not exist",
                      jl_symbol_name(m->name), jl_symbol_name(from->name), jl_symbol_name(var));
        }
        // TODO: we might want to require explicitly importing types to add constructors
        //       or we might want to drop this error entirely
        if (!b->imported && (!b2->constp || !jl_is_type(f))) {
            jl_errorf("invalid method definition in %s: function %s.%s must be explicitly imported to be extended",
                      jl_symbol_name(m->name), jl_symbol_name(from->name), jl_symbol_name(var));
        }
        return b2;
    }

    return b;
}

typedef struct _modstack_t {
    jl_module_t *m;
    jl_sym_t *var;
    struct _modstack_t *prev;
} modstack_t;

static jl_binding_t *jl_resolve_owner(jl_binding_t *b/*optional*/, jl_module_t *m JL_PROPAGATES_ROOT, jl_sym_t *var, modstack_t *st);

static inline jl_module_t *module_usings_getidx(jl_module_t *m JL_PROPAGATES_ROOT, size_t i) JL_NOTSAFEPOINT;

#ifndef __clang_gcanalyzer__
// The analyzer doesn't like looking through the arraylist, so just model the
// access for it using this function
static inline jl_module_t *module_usings_getidx(jl_module_t *m JL_PROPAGATES_ROOT, size_t i) JL_NOTSAFEPOINT {
    return (jl_module_t*)m->usings.items[i];
}
#endif

static int eq_bindings(jl_binding_t *owner, jl_binding_t *alias)
{
    assert(owner == jl_atomic_load_relaxed(&owner->owner));
    if (owner == alias)
        return 1;
    alias = jl_atomic_load_relaxed(&alias->owner);
    if (owner == alias)
        return 1;
    if (owner->constp && alias->constp && jl_atomic_load_relaxed(&owner->value) && jl_atomic_load_relaxed(&alias->value) == jl_atomic_load_relaxed(&owner->value))
        return 1;
    return 0;
}

// find a binding from a module's `usings` list
static jl_binding_t *using_resolve_binding(jl_module_t *m JL_PROPAGATES_ROOT, jl_sym_t *var, jl_module_t **from, modstack_t *st, int warn)
{
    jl_binding_t *b = NULL;
    jl_module_t *owner = NULL;
    JL_LOCK(&m->lock);
    int i = (int)m->usings.len - 1;
    JL_UNLOCK(&m->lock);
    for (; i >= 0; --i) {
        JL_LOCK(&m->lock);
        jl_module_t *imp = module_usings_getidx(m, i);
        JL_UNLOCK(&m->lock);
        jl_binding_t *tempb = jl_get_module_binding(imp, var, 0);
        if (tempb != NULL && tempb->exportp) {
            tempb = jl_resolve_owner(NULL, imp, var, st); // find the owner for tempb
            if (tempb == NULL)
                // couldn't resolve; try next using (see issue #6105)
                continue;
            assert(jl_atomic_load_relaxed(&tempb->owner) == tempb);
            if (b != NULL && !tempb->deprecated && !b->deprecated && !eq_bindings(tempb, b)) {
                if (warn) {
                    // set usingfailed=1 to avoid repeating this warning
                    // the owner will still be NULL, so it can be later imported or defined
                    tempb = jl_get_module_binding(m, var, 1);
                    tempb->usingfailed = 1;
                    jl_printf(JL_STDERR,
                              "WARNING: both %s and %s export \"%s\"; uses of it in module %s must be qualified\n",
                              jl_symbol_name(owner->name),
                              jl_symbol_name(imp->name), jl_symbol_name(var),
                              jl_symbol_name(m->name));
                }
                return NULL;
            }
            if (owner == NULL || !tempb->deprecated) {
                owner = imp;
                b = tempb;
            }
        }
    }
    *from = owner;
    return b;
}

// for error message printing: look up the module that exported a binding to m as var
// this might not be the same as the owner of the binding, since the binding itself may itself have been imported from elsewhere
static jl_module_t *jl_binding_dbgmodule(jl_binding_t *b, jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b2 = jl_atomic_load_relaxed(&b->owner);
    if (b2 != b && !b->imported) {
        // for implicitly imported globals, try to re-resolve it to find the module we got it from most directly
        jl_module_t *from = NULL;
        b = using_resolve_binding(m, var, &from, NULL, 0);
        if (b) {
            if (b2 == NULL || jl_atomic_load_relaxed(&b->owner) == jl_atomic_load_relaxed(&b2->owner))
                return from;
            // if we did not find it (or accidentally found a different one), ignore this
        }
    }
    return m;
}

static void jl_binding_dep_message(jl_module_t *m, jl_sym_t *name, jl_binding_t *b);

// get binding for reading. might return NULL for unbound.
static jl_binding_t *jl_resolve_owner(jl_binding_t *b/*optional*/, jl_module_t *m, jl_sym_t *var, modstack_t *st)
{
    if (b == NULL)
        b = jl_get_module_binding(m, var, 1);
    jl_binding_t *b2 = jl_atomic_load_relaxed(&b->owner);
    if (b2 == NULL) {
        if (b->usingfailed)
            return NULL;
        modstack_t top = { m, var, st };
        modstack_t *tmp = st;
        for (; tmp != NULL; tmp = tmp->prev) {
            if (tmp->m == m && tmp->var == var) {
                // import cycle without finding actual location
                return NULL;
            }
        }
        jl_module_t *from = NULL; // for error message printing
        b2 = using_resolve_binding(m, var, &from, &top, 1);
        if (b2 == NULL)
            return NULL;
        assert(from);
        JL_GC_PROMISE_ROOTED(from); // gc-analysis does not understand output parameters
        if (b2->deprecated) {
            if (jl_atomic_load_relaxed(&b2->value) == jl_nothing) {
                // silently skip importing deprecated values assigned to nothing (to allow later mutation)
                return NULL;
            }
        }
        // do a full import to prevent the result of this lookup from
        // changing, for example if this var is assigned to later.
        jl_binding_t *owner = NULL;
        if (!jl_atomic_cmpswap(&b->owner, &owner, b2)) {
            // concurrent import
            return owner;
        }
        if (b2->deprecated) {
            b->deprecated = 1; // we will warn about this below, but we might want to warn at the use sites too
            if (m != jl_main_module && m != jl_base_module &&
                jl_options.depwarn != JL_OPTIONS_DEPWARN_OFF) {
                /* with #22763, external packages wanting to replace
                   deprecated Base bindings should simply export the new
                   binding */
                jl_printf(JL_STDERR,
                          "WARNING: using deprecated binding %s.%s in %s.\n",
                          jl_symbol_name(from->name), jl_symbol_name(var),
                          jl_symbol_name(m->name));
                jl_binding_dep_message(from, var, b2);
            }
        }
    }
    assert(jl_atomic_load_relaxed(&b2->owner) == b2);
    return b2;
}

JL_DLLEXPORT jl_binding_t *jl_get_binding_if_bound(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_module_binding(m, var, 0);
    return b == NULL ? NULL : jl_atomic_load_relaxed(&b->owner);
}


// get the current likely owner of binding when accessing m.var, without resolving the binding (it may change later)
JL_DLLEXPORT jl_binding_t *jl_binding_owner(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_module_binding(m, var, 0);
    jl_module_t *from = m;
    if (b == NULL || (!b->usingfailed && jl_atomic_load_relaxed(&b->owner) == NULL))
        b = using_resolve_binding(m, var, &from, NULL, 0);
    else
        b = jl_atomic_load_relaxed(&b->owner);
    return b;
}

// get type of binding m.var, without resolving the binding
JL_DLLEXPORT jl_value_t *jl_get_binding_type(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_module_binding(m, var, 0);
    if (b == NULL)
        return jl_nothing;
    b = jl_atomic_load_relaxed(&b->owner);
    if (b == NULL)
        return jl_nothing;
    jl_value_t *ty = jl_atomic_load_relaxed(&b->ty);
    return ty ? ty : jl_nothing;
}

JL_DLLEXPORT jl_binding_t *jl_get_binding(jl_module_t *m, jl_sym_t *var)
{
    return jl_resolve_owner(NULL, m, var, NULL);
}

JL_DLLEXPORT jl_binding_t *jl_get_binding_or_error(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_binding(m, var);
    if (b == NULL)
        jl_undefined_var_error(var);
    // XXX: this only considers if the original is deprecated, not the binding in m
    if (b->deprecated)
        jl_binding_deprecation_warning(m, var, b);
    return b;
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
    return b && b->imported;
}

extern const char *jl_filename;
extern int jl_lineno;

static char const dep_message_prefix[] = "_dep_message_";

static void jl_binding_dep_message(jl_module_t *m, jl_sym_t *name, jl_binding_t *b)
{
    size_t prefix_len = strlen(dep_message_prefix);
    size_t name_len = strlen(jl_symbol_name(name));
    char *dep_binding_name = (char*)alloca(prefix_len+name_len+1);
    memcpy(dep_binding_name, dep_message_prefix, prefix_len);
    memcpy(dep_binding_name + prefix_len, jl_symbol_name(name), name_len);
    dep_binding_name[prefix_len+name_len] = '\0';
    jl_binding_t *dep_message_binding = jl_get_binding(m, jl_symbol(dep_binding_name));
    jl_value_t *dep_message = NULL;
    if (dep_message_binding != NULL)
        dep_message = jl_atomic_load_relaxed(&dep_message_binding->value);
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
        jl_value_t *v = jl_atomic_load_relaxed(&b->value);
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

// NOTE: we use explici since explicit is a C++ keyword
static void module_import_(jl_module_t *to, jl_module_t *from, jl_sym_t *asname, jl_sym_t *s, int explici)
{
    jl_binding_t *b = jl_get_binding(from, s);
    if (b == NULL) {
        jl_printf(JL_STDERR,
                  "WARNING: could not import %s.%s into %s\n",
                  jl_symbol_name(from->name), jl_symbol_name(s),
                  jl_symbol_name(to->name));
    }
    else {
        assert(jl_atomic_load_relaxed(&b->owner) == b);
        if (b->deprecated) {
            if (jl_atomic_load_relaxed(&b->value) == jl_nothing) {
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
                jl_binding_dep_message(from, s, b);
            }
        }

        jl_binding_t *bto = jl_get_module_binding(to, asname, 1);
        if (bto == b) {
            // importing a binding on top of itself. harmless.
            return;
        }
        jl_binding_t *ownerto = NULL;
        if (jl_atomic_cmpswap(&bto->owner, &ownerto, b)) {
            bto->imported |= (explici != 0);
            bto->deprecated |= b->deprecated; // we already warned about this above, but we might want to warn at the use sites too
        }
        else {
            if (eq_bindings(b, bto)) {
                // already imported
                bto->imported |= (explici != 0);
            }
            else if (ownerto != bto) {
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
    }
}

JL_DLLEXPORT void jl_module_import(jl_module_t *to, jl_module_t *from, jl_sym_t *s)
{
    module_import_(to, from, s, s, 1);
}

JL_DLLEXPORT void jl_module_import_as(jl_module_t *to, jl_module_t *from, jl_sym_t *s, jl_sym_t *asname)
{
    module_import_(to, from, asname, s, 1);
}

JL_DLLEXPORT void jl_module_use(jl_module_t *to, jl_module_t *from, jl_sym_t *s)
{
    module_import_(to, from, s, s, 0);
}

JL_DLLEXPORT void jl_module_use_as(jl_module_t *to, jl_module_t *from, jl_sym_t *s, jl_sym_t *asname)
{
    module_import_(to, from, asname, s, 0);
}


JL_DLLEXPORT void jl_module_using(jl_module_t *to, jl_module_t *from)
{
    if (to == from)
        return;
    JL_LOCK(&to->lock);
    for (size_t i = 0; i < to->usings.len; i++) {
        if (from == to->usings.items[i]) {
            JL_UNLOCK(&to->lock);
            return;
        }
    }
    arraylist_push(&to->usings, from);
    jl_gc_wb(to, from);
    JL_UNLOCK(&to->lock);

    // print a warning if something visible via this "using" conflicts with
    // an existing identifier. note that an identifier added later may still
    // silently override a "using" name. see issue #2054.
    jl_svec_t *table = jl_atomic_load_relaxed(&from->bindings);
    for (size_t i = 0; i < jl_svec_len(table); i++) {
        jl_binding_t *b = (jl_binding_t*)jl_svec_ref(table, i);
        if ((void*)b == jl_nothing)
            break;
        if (b->exportp && (jl_atomic_load_relaxed(&b->owner) == b || b->imported)) {
            jl_sym_t *var = b->globalref->name;
            jl_binding_t *tob = jl_get_module_binding(to, var, 0);
            if (tob && jl_atomic_load_relaxed(&tob->owner) != NULL &&
                // don't warn for conflicts with the module name itself.
                // see issue #4715
                var != to->name &&
                !eq_bindings(jl_atomic_load_relaxed(&tob->owner), b)) {
                jl_printf(JL_STDERR,
                          "WARNING: using %s.%s in module %s conflicts with an existing identifier.\n",
                          jl_symbol_name(from->name), jl_symbol_name(var),
                          jl_symbol_name(to->name));
            }
        }
        table = jl_atomic_load_relaxed(&from->bindings);
    }
}

JL_DLLEXPORT void jl_module_export(jl_module_t *from, jl_sym_t *s)
{
    jl_binding_t *b = jl_get_module_binding(from, s, 1);
    b->exportp = 1;
}

JL_DLLEXPORT int jl_boundp(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_binding(m, var);
    return b && (jl_atomic_load_relaxed(&b->value) != NULL);
}

JL_DLLEXPORT int jl_defines_or_exports_p(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_module_binding(m, var, 0);
    return b && (b->exportp || jl_atomic_load_relaxed(&b->owner) == b);
}

JL_DLLEXPORT int jl_module_exports_p(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_module_binding(m, var, 0);
    return b && b->exportp;
}

JL_DLLEXPORT int jl_binding_resolved_p(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_module_binding(m, var, 0);
    return b && jl_atomic_load_relaxed(&b->owner) != NULL;
}

static uint_t bindingkey_hash(size_t idx, jl_svec_t *data)
{
    jl_binding_t *b = (jl_binding_t*)jl_svecref(data, idx);
    jl_sym_t *var = b->globalref->name;
    return var->hash;
}

static int bindingkey_eq(size_t idx, const void *var, jl_svec_t *data, uint_t hv)
{
    jl_binding_t *b = (jl_binding_t*)jl_svecref(data, idx);
    jl_sym_t *name = b->globalref->name;
    return var == name;
}

JL_DLLEXPORT jl_binding_t *jl_get_module_binding(jl_module_t *m, jl_sym_t *var, int alloc)
{
    uint_t hv = var->hash;
    for (int locked = 0; ; locked++) {
        jl_array_t *bindingkeyset = jl_atomic_load_acquire(&m->bindingkeyset);
        jl_svec_t *bindings = jl_atomic_load_relaxed(&m->bindings);
        ssize_t idx = jl_smallintset_lookup(bindingkeyset, bindingkey_eq, var, bindings, hv); // acquire
        if (idx != -1) {
            jl_binding_t *b = (jl_binding_t*)jl_svecref(bindings, idx); // relaxed
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
            jl_smallintset_insert(&m->bindingkeyset, (jl_value_t*)m, bindingkey_hash, i, bindings); // release
            JL_UNLOCK(&m->lock);
            return b;
        }
    }
}


JL_DLLEXPORT jl_value_t *jl_get_globalref_value(jl_globalref_t *gr)
{
    jl_binding_t *b = gr->binding;
    b = jl_resolve_owner(b, gr->mod, gr->name, NULL);
    // ignores b->deprecated
    return b == NULL ? NULL : jl_atomic_load_relaxed(&b->value);
}

JL_DLLEXPORT jl_value_t *jl_get_global(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_binding(m, var);
    if (b == NULL)
        return NULL;
    // XXX: this only considers if the original is deprecated, not the binding in m
    if (b->deprecated)
        jl_binding_deprecation_warning(m, var, b);
    return jl_atomic_load_relaxed(&b->value);
}

JL_DLLEXPORT void jl_set_global(jl_module_t *m JL_ROOTING_ARGUMENT, jl_sym_t *var, jl_value_t *val JL_ROOTED_ARGUMENT)
{
    jl_binding_t *bp = jl_get_binding_wr(m, var);
    jl_checked_assignment(bp, m, var, val);
}

JL_DLLEXPORT void jl_set_const(jl_module_t *m JL_ROOTING_ARGUMENT, jl_sym_t *var, jl_value_t *val JL_ROOTED_ARGUMENT)
{
    // this function is mostly only used during initialization, so the data races here are not too important to us
    jl_binding_t *bp = jl_get_binding_wr(m, var);
    if (jl_atomic_load_relaxed(&bp->value) == NULL) {
        jl_value_t *old_ty = NULL;
        jl_atomic_cmpswap_relaxed(&bp->ty, &old_ty, (jl_value_t*)jl_any_type);
        uint8_t constp = 0;
        // if (jl_atomic_cmpswap(&bp->constp, &constp, 1)) {
        if (constp = bp->constp, bp->constp = 1, constp == 0) {
            jl_value_t *old = NULL;
            if (jl_atomic_cmpswap(&bp->value, &old, val)) {
                jl_gc_wb_binding(bp, val);
                return;
            }
        }
    }
    jl_errorf("invalid redefinition of constant %s", jl_symbol_name(var));
}

JL_DLLEXPORT int jl_globalref_is_const(jl_globalref_t *gr)
{
    jl_binding_t *b = gr->binding;
    b = jl_resolve_owner(b, gr->mod, gr->name, NULL);
    return b && b->constp;
}

JL_DLLEXPORT int jl_globalref_boundp(jl_globalref_t *gr)
{
    jl_binding_t *b = gr->binding;
    b = jl_resolve_owner(b, gr->mod, gr->name, NULL);
    return b && jl_atomic_load_relaxed(&b->value) != NULL;
}

JL_DLLEXPORT int jl_is_const(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_binding(m, var);
    return b && b->constp;
}

// set the deprecated flag for a binding:
//   0=not deprecated, 1=renamed, 2=moved to another package
JL_DLLEXPORT void jl_deprecate_binding(jl_module_t *m, jl_sym_t *var, int flag)
{
    // XXX: this deprecates the original value, which might be imported from elsewhere
    jl_binding_t *b = jl_get_binding(m, var);
    if (b) b->deprecated = flag;
}

JL_DLLEXPORT int jl_is_binding_deprecated(jl_module_t *m, jl_sym_t *var)
{
    if (jl_binding_resolved_p(m, var)) {
        // XXX: this only considers if the original is deprecated, not this precise binding
        jl_binding_t *b = jl_get_binding(m, var);
        return b && b->deprecated;
    }
    return 0;
}

void jl_binding_deprecation_warning(jl_module_t *m, jl_sym_t *s, jl_binding_t *b)
{
    // Only print a warning for deprecated == 1 (renamed).
    // For deprecated == 2 (moved to a package) the binding is to a function
    // that throws an error, so we don't want to print a warning too.
    if (b->deprecated == 1 && jl_options.depwarn) {
        if (jl_options.depwarn != JL_OPTIONS_DEPWARN_ERROR)
            jl_printf(JL_STDERR, "WARNING: ");
        assert(jl_atomic_load_relaxed(&b->owner) == b);
        jl_printf(JL_STDERR, "%s.%s is deprecated",
                  jl_symbol_name(m->name), jl_symbol_name(s));
        jl_binding_dep_message(m, s, b);

        if (jl_options.depwarn != JL_OPTIONS_DEPWARN_ERROR) {
            if (jl_lineno != 0) {
                jl_printf(JL_STDERR, "  likely near %s:%d\n", jl_filename, jl_lineno);
            }
        }

        if (jl_options.depwarn == JL_OPTIONS_DEPWARN_ERROR) {
            jl_errorf("use of deprecated variable: %s.%s",
                      jl_symbol_name(m->name),
                      jl_symbol_name(s));
        }
    }
}

JL_DLLEXPORT void jl_checked_assignment(jl_binding_t *b, jl_module_t *mod, jl_sym_t *var, jl_value_t *rhs)
{
    jl_value_t *old_ty = NULL;
    if (!jl_atomic_cmpswap_relaxed(&b->ty, &old_ty, (jl_value_t*)jl_any_type)) {
        if (old_ty != (jl_value_t*)jl_any_type && jl_typeof(rhs) != old_ty) {
            JL_GC_PUSH1(&rhs); // callee-rooted
            if (!jl_isa(rhs, old_ty))
                jl_errorf("cannot assign an incompatible value to the global %s.%s.",
                          jl_symbol_name(mod->name), jl_symbol_name(var));
            JL_GC_POP();
        }
    }
    if (b->constp) {
        jl_value_t *old = NULL;
        if (jl_atomic_cmpswap(&b->value, &old, rhs)) {
            jl_gc_wb_binding(b, rhs);
            return;
        }
        if (jl_egal(rhs, old))
            return;
        if (jl_typeof(rhs) != jl_typeof(old) || jl_is_type(rhs) || jl_is_module(rhs)) {
            jl_errorf("invalid redefinition of constant %s.%s",
                      jl_symbol_name(mod->name), jl_symbol_name(var));

        }
        jl_safe_printf("WARNING: redefinition of constant %s.%s. This may fail, cause incorrect answers, or produce other errors.\n",
                       jl_symbol_name(mod->name), jl_symbol_name(var));
    }
    jl_atomic_store_release(&b->value, rhs);
    jl_gc_wb_binding(b, rhs);
}

JL_DLLEXPORT void jl_declare_constant(jl_binding_t *b, jl_module_t *mod, jl_sym_t *var)
{
    // n.b. jl_get_binding_wr should have ensured b->owner == b as mod.var
    if (jl_atomic_load_relaxed(&b->owner) != b || (jl_atomic_load_relaxed(&b->value) != NULL && !b->constp)) {
        jl_errorf("cannot declare %s.%s constant; it already has a value",
                  jl_symbol_name(mod->name), jl_symbol_name(var));
    }
    b->constp = 1;
}

JL_DLLEXPORT jl_value_t *jl_module_usings(jl_module_t *m)
{
    JL_LOCK(&m->lock);
    int j = m->usings.len;
    jl_array_t *a = jl_alloc_array_1d(jl_array_any_type, j);
    JL_GC_PUSH1(&a);
    for (int i = 0; j > 0; i++) {
        j--;
        jl_module_t *imp = (jl_module_t*)m->usings.items[i];
        jl_array_ptr_set(a, j, (jl_value_t*)imp);
    }
    JL_UNLOCK(&m->lock); // may gc
    JL_GC_POP();
    return (jl_value_t*)a;
}

JL_DLLEXPORT jl_value_t *jl_module_names(jl_module_t *m, int all, int imported)
{
    jl_array_t *a = jl_alloc_array_1d(jl_array_symbol_type, 0);
    JL_GC_PUSH1(&a);
    jl_svec_t *table = jl_atomic_load_relaxed(&m->bindings);
    for (size_t i = 0; i < jl_svec_len(table); i++) {
        jl_binding_t *b = (jl_binding_t*)jl_svec_ref(table, i);
        if ((void*)b == jl_nothing)
            break;
        jl_sym_t *asname = b->globalref->name;
        int hidden = jl_symbol_name(asname)[0]=='#';
        if ((b->exportp ||
             (imported && b->imported) ||
             (jl_atomic_load_relaxed(&b->owner) == b && !b->imported && (all || m == jl_main_module))) &&
            (all || (!b->deprecated && !hidden))) {
            jl_array_grow_end(a, 1);
            // n.b. change to jl_arrayset if array storage allocation for Array{Symbols,1} changes:
            jl_array_ptr_set(a, jl_array_dim0(a)-1, (jl_value_t*)asname);
        }
        table = jl_atomic_load_relaxed(&m->bindings);
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

JL_DLLEXPORT jl_uuid_t jl_module_build_id(jl_module_t *m) { return m->build_id; }
JL_DLLEXPORT jl_uuid_t jl_module_uuid(jl_module_t* m) { return m->uuid; }

// TODO: make this part of the module constructor and read-only?
JL_DLLEXPORT void jl_set_module_uuid(jl_module_t *m, jl_uuid_t uuid) { m->uuid = uuid; }

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
        jl_binding_t *b = (jl_binding_t*)jl_svec_ref(table, i);
        if ((void*)b == jl_nothing)
            break;
        if (jl_atomic_load_relaxed(&b->owner) && jl_atomic_load_relaxed(&b->owner) != b && !b->imported)
            jl_atomic_store_relaxed(&b->owner, NULL);
    }
    JL_UNLOCK(&m->lock);
}

JL_DLLEXPORT void jl_init_restored_module(jl_value_t *mod)
{
    if (!jl_generating_output() || jl_options.incremental) {
        jl_module_run_initializer((jl_module_t*)mod);
    }
    else {
        if (jl_module_init_order == NULL)
            jl_module_init_order = jl_alloc_vec_any(0);
        jl_array_ptr_1d_push(jl_module_init_order, mod);
    }
}

#ifdef __cplusplus
}
#endif

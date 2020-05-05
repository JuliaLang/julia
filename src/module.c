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

jl_module_t *jl_main_module = NULL;
jl_module_t *jl_core_module = NULL;
jl_module_t *jl_base_module = NULL;
jl_module_t *jl_top_module = NULL;

JL_DLLEXPORT jl_module_t *jl_new_module(jl_sym_t *name)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    const jl_uuid_t uuid_zero = {0, 0};
    jl_module_t *m = (jl_module_t*)jl_gc_alloc(ptls, sizeof(jl_module_t),
                                               jl_module_type);
    JL_GC_PUSH1(&m);
    assert(jl_is_symbol(name));
    m->name = name;
    m->parent = NULL;
    m->istopmod = 0;
    m->uuid = uuid_zero;
    static unsigned int mcounter; // simple counter backup, in case hrtime is not incrementing
    m->build_id = jl_hrtime() + (++mcounter);
    if (!m->build_id)
        m->build_id++; // build id 0 is invalid
    m->primary_world = 0;
    m->counter = 1;
    m->nospecialize = 0;
    m->optlevel = -1;
    JL_MUTEX_INIT(&m->lock);
    htable_new(&m->bindings, 0);
    arraylist_new(&m->usings, 0);
    if (jl_core_module) {
        jl_module_using(m, jl_core_module);
    }
    // export own name, so "using Foo" makes "Foo" itself visible
    jl_set_const(m, name, (jl_value_t*)m);
    jl_module_export(m, name);
    JL_GC_POP();
    return m;
}

uint32_t jl_module_next_counter(jl_module_t *m)
{
    return jl_atomic_fetch_add(&m->counter, 1);
}

JL_DLLEXPORT jl_value_t *jl_f_new_module(jl_sym_t *name, uint8_t std_imports)
{
    // TODO: should we prohibit this during incremental compilation?
    jl_module_t *m = jl_new_module(name);
    JL_GC_PUSH1(&m);
    m->parent = jl_main_module; // TODO: this is a lie
    jl_gc_wb(m, m->parent);
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

static jl_binding_t *new_binding(jl_sym_t *name)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    assert(jl_is_symbol(name));
    jl_binding_t *b = (jl_binding_t*)jl_gc_alloc_buf(ptls, sizeof(jl_binding_t));
    b->name = name;
    b->value = NULL;
    b->owner = NULL;
    b->globalref = NULL;
    b->constp = 0;
    b->exportp = 0;
    b->imported = 0;
    b->deprecated = 0;
    return b;
}

// get binding for assignment
JL_DLLEXPORT jl_binding_t *jl_get_binding_wr(jl_module_t *m, jl_sym_t *var, int error)
{
    JL_LOCK_NOGC(&m->lock);
    jl_binding_t **bp = (jl_binding_t**)ptrhash_bp(&m->bindings, var);
    jl_binding_t *b = *bp;

    if (b != HT_NOTFOUND) {
        if (b->owner != m) {
            if (b->owner == NULL) {
                b->owner = m;
            }
            else if (error) {
                JL_UNLOCK_NOGC(&m->lock);
                jl_errorf("cannot assign a value to variable %s.%s from module %s",
                          jl_symbol_name(b->owner->name), jl_symbol_name(var), jl_symbol_name(m->name));
            }
        }
    }
    else {
        b = new_binding(var);
        b->owner = m;
        *bp = b;
        jl_gc_wb_buf(m, b, sizeof(jl_binding_t));
    }

    JL_UNLOCK_NOGC(&m->lock);
    return b;
}

// Hash tables don't generically root their contents, but they do for bindings.
// Express this to the analyzer.
// NOTE: Must hold m->lock while calling these.
#ifdef __clang_analyzer__
jl_binding_t *_jl_get_module_binding(jl_module_t *m JL_PROPAGATES_ROOT, jl_sym_t *var) JL_NOTSAFEPOINT;
jl_binding_t **_jl_get_module_binding_bp(jl_module_t *m JL_PROPAGATES_ROOT, jl_sym_t *var) JL_NOTSAFEPOINT;
#else
static inline jl_binding_t *_jl_get_module_binding(jl_module_t *m JL_PROPAGATES_ROOT, jl_sym_t *var) JL_NOTSAFEPOINT
{
    return (jl_binding_t*)ptrhash_get(&m->bindings, var);
}
static inline jl_binding_t **_jl_get_module_binding_bp(jl_module_t *m JL_PROPAGATES_ROOT, jl_sym_t *var) JL_NOTSAFEPOINT
{
    return (jl_binding_t**)ptrhash_bp(&m->bindings, var);
}
#endif


// return module of binding
JL_DLLEXPORT jl_module_t *jl_get_module_of_binding(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_binding(m, var);
    if (b == NULL)
        return NULL;
    return b->owner;
}

// get binding for adding a method
// like jl_get_binding_wr, but has different error paths
JL_DLLEXPORT jl_binding_t *jl_get_binding_for_method_def(jl_module_t *m, jl_sym_t *var)
{
    JL_LOCK_NOGC(&m->lock);
    jl_binding_t **bp = _jl_get_module_binding_bp(m, var);
    jl_binding_t *b = *bp;

    if (b != HT_NOTFOUND) {
        if (b->owner != m) {
            if (b->owner == NULL) {
                b->owner = m;
            }
            else {
                JL_UNLOCK_NOGC(&m->lock);
                jl_binding_t *b2 = jl_get_binding(b->owner, var);
                if (b2 == NULL || b2->value == NULL)
                    jl_errorf("invalid method definition: imported function %s.%s does not exist",
                              jl_symbol_name(b->owner->name), jl_symbol_name(var));
                // TODO: we might want to require explicitly importing types to add constructors
                if (!b->imported && !jl_is_type(b2->value)) {
                    jl_errorf("error in method definition: function %s.%s must be explicitly imported to be extended",
                              jl_symbol_name(b->owner->name), jl_symbol_name(var));
                }
                return b2;
            }
        }
    }
    else {
        b = new_binding(var);
        b->owner = m;
        *bp = b;
        jl_gc_wb_buf(m, b, sizeof(jl_binding_t));
    }

    JL_UNLOCK_NOGC(&m->lock);
    return b;
}

static void module_import_(jl_module_t *to, jl_module_t *from, jl_sym_t *s,
                           int explici);

typedef struct _modstack_t {
    jl_module_t *m;
    struct _modstack_t *prev;
} modstack_t;

static jl_binding_t *jl_get_binding_(jl_module_t *m JL_PROPAGATES_ROOT, jl_sym_t *var, modstack_t *st);

// find a binding from a module's `usings` list
// called while holding m->lock
static jl_binding_t *using_resolve_binding(jl_module_t *m JL_PROPAGATES_ROOT, jl_sym_t *var, modstack_t *st, int warn)
{
    jl_binding_t *b = NULL;
    jl_module_t *owner = NULL;
    for(int i=(int)m->usings.len-1; i >= 0; --i) {
        jl_module_t *imp = (jl_module_t*)m->usings.items[i];
        // TODO: make sure this can't deadlock
        JL_LOCK(&imp->lock);
        jl_binding_t *tempb = _jl_get_module_binding(imp, var);
        JL_UNLOCK(&imp->lock);
        if (tempb != HT_NOTFOUND && tempb->exportp) {
            tempb = jl_get_binding_(imp, var, st);
            if (tempb == NULL || tempb->owner == NULL)
                // couldn't resolve; try next using (see issue #6105)
                continue;
            if (owner != NULL && tempb->owner != b->owner &&
                !tempb->deprecated && !b->deprecated &&
                !(tempb->constp && tempb->value && b->constp && b->value == tempb->value)) {
                if (warn) {
                    JL_UNLOCK(&m->lock);
                    jl_printf(JL_STDERR,
                              "WARNING: both %s and %s export \"%s\"; uses of it in module %s must be qualified\n",
                              jl_symbol_name(owner->name),
                              jl_symbol_name(imp->name), jl_symbol_name(var),
                              jl_symbol_name(m->name));
                    // mark this binding resolved, to avoid repeating the warning
                    (void)jl_get_binding_wr(m, var, 0);
                    JL_LOCK(&m->lock);
                }
                return NULL;
            }
            if (owner == NULL || !tempb->deprecated) {
                owner = imp;
                b = tempb;
            }
        }
    }
    return b;
}

// get binding for reading. might return NULL for unbound.
static jl_binding_t *jl_get_binding_(jl_module_t *m, jl_sym_t *var, modstack_t *st)
{
    modstack_t top = { m, st };
    modstack_t *tmp = st;
    while (tmp != NULL) {
        if (tmp->m == m) {
            // import cycle without finding actual location
            return NULL;
        }
        tmp = tmp->prev;
    }
    JL_LOCK(&m->lock);
    jl_binding_t *b = _jl_get_module_binding(m, var);
    if (b == HT_NOTFOUND || b->owner == NULL) {
        b = using_resolve_binding(m, var, &top, 1);
        JL_UNLOCK(&m->lock);
        if (b != NULL) {
            // do a full import to prevent the result of this lookup
            // from changing, for example if this var is assigned to
            // later.
            module_import_(m, b->owner, var, 0);
            return b;
        }
        return NULL;
    }
    JL_UNLOCK(&m->lock);
    if (b->owner != m)
        return jl_get_binding_(b->owner, var, &top);
    return b;
}

// get owner of binding when accessing m.var, without resolving the binding
JL_DLLEXPORT jl_value_t *jl_binding_owner(jl_module_t *m, jl_sym_t *var)
{
    JL_LOCK(&m->lock);
    jl_binding_t *b = (jl_binding_t*)ptrhash_get(&m->bindings, var);
    if (b == HT_NOTFOUND || b->owner == NULL)
        b = using_resolve_binding(m, var, NULL, 0);
    JL_UNLOCK(&m->lock);
    if (b == NULL || b->owner == NULL)
        return jl_nothing;
    return (jl_value_t*)b->owner;
}

JL_DLLEXPORT jl_binding_t *jl_get_binding(jl_module_t *m, jl_sym_t *var)
{
    return jl_get_binding_(m, var, NULL);
}

void jl_binding_deprecation_warning(jl_module_t *m, jl_binding_t *b);

JL_DLLEXPORT jl_binding_t *jl_get_binding_or_error(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_binding(m, var);
    if (b == NULL)
        jl_undefined_var_error(var);
    if (b->deprecated)
        jl_binding_deprecation_warning(m, b);
    return b;
}

JL_DLLEXPORT jl_value_t *jl_module_globalref(jl_module_t *m, jl_sym_t *var)
{
    JL_LOCK(&m->lock);
    jl_binding_t *b = (jl_binding_t*)ptrhash_get(&m->bindings, var);
    if (b == HT_NOTFOUND) {
        JL_UNLOCK(&m->lock);
        return jl_new_struct(jl_globalref_type, m, var);
    }
    if (b->globalref == NULL) {
        b->globalref = jl_new_struct(jl_globalref_type, m, var);
        jl_gc_wb(m, b->globalref);
    }
    JL_UNLOCK(&m->lock);
    return b->globalref;
}

static int eq_bindings(jl_binding_t *a, jl_binding_t *b)
{
    if (a==b) return 1;
    if (a->name == b->name && a->owner == b->owner) return 1;
    if (a->constp && a->value && b->constp && b->value == a->value) return 1;
    return 0;
}

// does module m explicitly import s?
JL_DLLEXPORT int jl_is_imported(jl_module_t *m, jl_sym_t *s)
{
    JL_LOCK(&m->lock);
    jl_binding_t *b = (jl_binding_t*)ptrhash_get(&m->bindings, s);
    JL_UNLOCK(&m->lock);
    return (b != HT_NOTFOUND && b->imported);
}

// NOTE: we use explici since explicit is a C++ keyword
static void module_import_(jl_module_t *to, jl_module_t *from, jl_sym_t *s, int explici)
{
    jl_binding_t *b = jl_get_binding(from, s);
    if (b == NULL) {
        jl_printf(JL_STDERR,
                  "WARNING: could not import %s.%s into %s\n",
                  jl_symbol_name(from->name), jl_symbol_name(s),
                  jl_symbol_name(to->name));
    }
    else {
        if (b->deprecated) {
            if (b->value == jl_nothing) {
                return;
            }
            else if (to != jl_main_module && to != jl_base_module &&
                     jl_options.depwarn != JL_OPTIONS_DEPWARN_OFF) {
                /* with #22763, external packages wanting to replace
                   deprecated Base bindings should simply export the new
                   binding */
                jl_printf(JL_STDERR,
                          "WARNING: importing deprecated binding %s.%s into %s.\n",
                          jl_symbol_name(from->name), jl_symbol_name(s),
                          jl_symbol_name(to->name));
            }
        }

        JL_LOCK(&to->lock);
        jl_binding_t **bp = (jl_binding_t**)ptrhash_bp(&to->bindings, s);
        jl_binding_t *bto = *bp;
        if (bto != HT_NOTFOUND) {
            if (bto == b) {
                // importing a binding on top of itself. harmless.
            }
            else if (bto->owner == b->owner) {
                // already imported
                bto->imported = (explici!=0);
            }
            else if (bto->owner != to && bto->owner != NULL) {
                // already imported from somewhere else
                jl_binding_t *bval = jl_get_binding(to, s);
                if (bval->constp && bval->value && b->constp && b->value == bval->value) {
                    // equivalent binding
                    bto->imported = (explici!=0);
                    JL_UNLOCK(&to->lock);
                }
                else {
                    JL_UNLOCK(&to->lock);
                    jl_printf(JL_STDERR,
                              "WARNING: ignoring conflicting import of %s.%s into %s\n",
                              jl_symbol_name(from->name), jl_symbol_name(s),
                              jl_symbol_name(to->name));
                }
                return;
            }
            else if (bto->constp || bto->value) {
                // conflict with name owned by destination module
                assert(bto->owner == to);
                if (bto->constp && bto->value && b->constp && b->value == bto->value) {
                    // equivalent binding
                    JL_UNLOCK(&to->lock);
                }
                else {
                    JL_UNLOCK(&to->lock);
                    jl_printf(JL_STDERR,
                              "WARNING: import of %s.%s into %s conflicts with an existing identifier; ignored.\n",
                              jl_symbol_name(from->name), jl_symbol_name(s),
                              jl_symbol_name(to->name));
                }
                return;
            }
            else {
                bto->owner = b->owner;
                bto->imported = (explici!=0);
            }
        }
        else {
            jl_binding_t *nb = new_binding(s);
            nb->owner = b->owner;
            nb->imported = (explici!=0);
            nb->deprecated = b->deprecated;
            *bp = nb;
            jl_gc_wb_buf(to, nb, sizeof(jl_binding_t));
        }
        JL_UNLOCK(&to->lock);
    }
}

JL_DLLEXPORT void jl_module_import(jl_module_t *to, jl_module_t *from, jl_sym_t *s)
{
    module_import_(to, from, s, 1);
}

JL_DLLEXPORT void jl_module_use(jl_module_t *to, jl_module_t *from, jl_sym_t *s)
{
    module_import_(to, from, s, 0);
}

JL_DLLEXPORT void jl_module_using(jl_module_t *to, jl_module_t *from)
{
    if (to == from)
        return;
    JL_LOCK(&to->lock);
    for(size_t i=0; i < to->usings.len; i++) {
        if (from == to->usings.items[i]) {
            JL_UNLOCK(&to->lock);
            return;
        }
    }
    // TODO: make sure this can't deadlock
    JL_LOCK(&from->lock);
    // print a warning if something visible via this "using" conflicts with
    // an existing identifier. note that an identifier added later may still
    // silently override a "using" name. see issue #2054.
    void **table = from->bindings.table;
    for(size_t i=1; i < from->bindings.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            if (b->exportp && (b->owner==from || b->imported)) {
                jl_sym_t *var = (jl_sym_t*)table[i-1];
                jl_binding_t **tobp = (jl_binding_t**)ptrhash_bp(&to->bindings, var);
                if (*tobp != HT_NOTFOUND && (*tobp)->owner != NULL &&
                    // don't warn for conflicts with the module name itself.
                    // see issue #4715
                    var != to->name &&
                    !eq_bindings(jl_get_binding(to,var), b)) {
                    // TODO: not ideal to print this while holding module locks
                    jl_printf(JL_STDERR,
                              "WARNING: using %s.%s in module %s conflicts with an existing identifier.\n",
                              jl_symbol_name(from->name), jl_symbol_name(var),
                              jl_symbol_name(to->name));
                }
            }
        }
    }
    JL_UNLOCK(&from->lock);

    arraylist_push(&to->usings, from);
    jl_gc_wb(to, from);
    JL_UNLOCK(&to->lock);
}

JL_DLLEXPORT void jl_module_export(jl_module_t *from, jl_sym_t *s)
{
    JL_LOCK(&from->lock);
    jl_binding_t **bp = (jl_binding_t**)ptrhash_bp(&from->bindings, s);
    if (*bp == HT_NOTFOUND) {
        jl_binding_t *b = new_binding(s);
        // don't yet know who the owner is
        b->owner = NULL;
        *bp = b;
        jl_gc_wb_buf(from, b, sizeof(jl_binding_t));
    }
    assert(*bp != HT_NOTFOUND);
    (*bp)->exportp = 1;
    JL_UNLOCK(&from->lock);
}

JL_DLLEXPORT int jl_boundp(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_binding(m, var);
    return b && (b->value != NULL);
}

JL_DLLEXPORT int jl_defines_or_exports_p(jl_module_t *m, jl_sym_t *var)
{
    JL_LOCK_NOGC(&m->lock);
    jl_binding_t *b = (jl_binding_t*)ptrhash_get(&m->bindings, var);
    JL_UNLOCK_NOGC(&m->lock);
    return b != HT_NOTFOUND && (b->exportp || b->owner==m);
}

JL_DLLEXPORT int jl_module_exports_p(jl_module_t *m, jl_sym_t *var) JL_NOTSAFEPOINT
{
    JL_LOCK_NOGC(&m->lock);
    jl_binding_t *b = _jl_get_module_binding(m, var);
    JL_UNLOCK_NOGC(&m->lock);
    return b != HT_NOTFOUND && b->exportp;
}

JL_DLLEXPORT int jl_binding_resolved_p(jl_module_t *m, jl_sym_t *var) JL_NOTSAFEPOINT
{
    JL_LOCK_NOGC(&m->lock);
    jl_binding_t *b = _jl_get_module_binding(m, var);
    JL_UNLOCK_NOGC(&m->lock);
    return b != HT_NOTFOUND && b->owner != NULL;
}

JL_DLLEXPORT jl_binding_t *jl_get_module_binding(jl_module_t *m JL_PROPAGATES_ROOT, jl_sym_t *var) JL_NOTSAFEPOINT
{
    JL_LOCK_NOGC(&m->lock);
    jl_binding_t *b = _jl_get_module_binding(m, var);
    JL_UNLOCK_NOGC(&m->lock);
    return b == HT_NOTFOUND ? NULL : b;
}

JL_DLLEXPORT jl_value_t *jl_get_global(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_binding(m, var);
    if (b == NULL) return NULL;
    if (b->deprecated) jl_binding_deprecation_warning(m, b);
    return b->value;
}

JL_DLLEXPORT void jl_set_global(jl_module_t *m JL_ROOTING_ARGUMENT, jl_sym_t *var, jl_value_t *val JL_ROOTED_ARGUMENT)
{
    jl_binding_t *bp = jl_get_binding_wr(m, var, 1);
    // In a release build, simply ignore conflicting assignments (for backwards compatibility).
    // However, we want to start asserting that they do not occur, since that can cause `val`
    // not to be rooted when the caller expected it to be.
    assert(!bp->constp);
    if (!bp->constp) {
        bp->value = val;
        jl_gc_wb(m, val);
    }
}

JL_DLLEXPORT void jl_set_const(jl_module_t *m JL_ROOTING_ARGUMENT, jl_sym_t *var, jl_value_t *val JL_ROOTED_ARGUMENT)
{
    jl_binding_t *bp = jl_get_binding_wr(m, var, 1);
    assert(!bp->constp);
    if (jl_atomic_compare_exchange(&bp->constp, 0, 1) == 0) {
        bp->value = val;
        jl_gc_wb(m, val);
    }
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
    jl_binding_t *b = jl_get_binding(m, var);
    if (b) b->deprecated = flag;
}

JL_DLLEXPORT int jl_is_binding_deprecated(jl_module_t *m, jl_sym_t *var)
{
    if (jl_binding_resolved_p(m, var)) {
        jl_binding_t *b = jl_get_binding(m, var);
        return b && b->deprecated;
    }
    return 0;
}

extern const char *jl_filename;
extern int jl_lineno;

char dep_message_prefix[] = "_dep_message_";

jl_binding_t *jl_get_dep_message_binding(jl_module_t *m, jl_binding_t *deprecated_binding)
{
    size_t prefix_len = strlen(dep_message_prefix);
    size_t name_len = strlen(jl_symbol_name(deprecated_binding->name));
    char *dep_binding_name = (char*)alloca(prefix_len+name_len+1);
    memcpy(dep_binding_name, dep_message_prefix, prefix_len);
    memcpy(dep_binding_name + prefix_len, jl_symbol_name(deprecated_binding->name), name_len);
    dep_binding_name[prefix_len+name_len] = '\0';
    return jl_get_binding(m, jl_symbol(dep_binding_name));
}

void jl_binding_deprecation_warning(jl_module_t *m, jl_binding_t *b)
{
    // Only print a warning for deprecated == 1 (renamed).
    // For deprecated == 2 (moved to a package) the binding is to a function
    // that throws an error, so we don't want to print a warning too.
    if (b->deprecated == 1 && jl_options.depwarn) {
        if (jl_options.depwarn != JL_OPTIONS_DEPWARN_ERROR)
            jl_printf(JL_STDERR, "WARNING: ");
        jl_binding_t *dep_message_binding = NULL;
        if (b->owner) {
            jl_printf(JL_STDERR, "%s.%s is deprecated",
                      jl_symbol_name(b->owner->name), jl_symbol_name(b->name));
            dep_message_binding = jl_get_dep_message_binding(b->owner, b);
        }
        else {
            jl_printf(JL_STDERR, "%s is deprecated", jl_symbol_name(b->name));
        }

        if (dep_message_binding && dep_message_binding->value) {
            if (jl_isa(dep_message_binding->value, (jl_value_t*)jl_string_type)) {
                jl_uv_puts(JL_STDERR, jl_string_data(dep_message_binding->value),
                    jl_string_len(dep_message_binding->value));
            }
            else {
                jl_static_show(JL_STDERR, dep_message_binding->value);
            }
        }
        else {
            jl_value_t *v = b->value;
            if (v) {
                if (jl_is_type(v) || jl_is_module(v)) {
                    jl_printf(JL_STDERR, ", use ");
                    jl_static_show(JL_STDERR, v);
                    jl_printf(JL_STDERR, " instead.");
                }
                else {
                    jl_methtable_t *mt = jl_gf_mtable(v);
                    if (mt != NULL && (mt->defs != jl_nothing ||
                                       jl_isa(v, (jl_value_t*)jl_builtin_type))) {
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

        if (jl_options.depwarn != JL_OPTIONS_DEPWARN_ERROR) {
            if (jl_lineno == 0) {
                jl_printf(JL_STDERR, " in module %s\n", jl_symbol_name(m->name));
            }
            else {
                jl_printf(JL_STDERR, "  likely near %s:%d\n", jl_filename, jl_lineno);
            }
        }

        if (jl_options.depwarn == JL_OPTIONS_DEPWARN_ERROR) {
            if (b->owner)
                jl_errorf("deprecated binding: %s.%s",
                          jl_symbol_name(b->owner->name),
                          jl_symbol_name(b->name));
            else
                jl_errorf("deprecated binding: %s", jl_symbol_name(b->name));
        }
    }
}

JL_DLLEXPORT void jl_checked_assignment(jl_binding_t *b, jl_value_t *rhs)
{
    if (b->constp && b->value != NULL) {
        if (!jl_egal(rhs, b->value)) {
            if (jl_typeof(rhs) != jl_typeof(b->value) ||
                jl_is_type(rhs) /*|| jl_is_function(rhs)*/ || jl_is_module(rhs)) {
                jl_errorf("invalid redefinition of constant %s",
                          jl_symbol_name(b->name));
            }
            jl_printf(JL_STDERR, "WARNING: redefinition of constant %s. This may fail, cause incorrect answers, or produce other errors.\n",
                      jl_symbol_name(b->name));
        }
    }
    b->value = rhs;
    jl_gc_wb_binding(b, rhs);
}

JL_DLLEXPORT void jl_declare_constant(jl_binding_t *b)
{
    if (b->value != NULL && !b->constp) {
        jl_errorf("cannot declare %s constant; it already has a value",
                  jl_symbol_name(b->name));
    }
    b->constp = 1;
}

JL_DLLEXPORT jl_value_t *jl_module_usings(jl_module_t *m)
{
    jl_array_t *a = jl_alloc_array_1d(jl_array_any_type, 0);
    JL_GC_PUSH1(&a);
    JL_LOCK(&m->lock);
    for(int i=(int)m->usings.len-1; i >= 0; --i) {
        jl_array_grow_end(a, 1);
        jl_module_t *imp = (jl_module_t*)m->usings.items[i];
        jl_array_ptr_set(a,jl_array_dim0(a)-1, (jl_value_t*)imp);
    }
    JL_UNLOCK(&m->lock);
    JL_GC_POP();
    return (jl_value_t*)a;
}

JL_DLLEXPORT jl_value_t *jl_module_names(jl_module_t *m, int all, int imported)
{
    jl_array_t *a = jl_alloc_array_1d(jl_array_symbol_type, 0);
    JL_GC_PUSH1(&a);
    size_t i;
    JL_LOCK(&m->lock);
    void **table = m->bindings.table;
    for (i = 1; i < m->bindings.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            int hidden = jl_symbol_name(b->name)[0]=='#';
            if ((b->exportp ||
                 (imported && b->imported) ||
                 (b->owner == m && !b->imported && (all || m == jl_main_module))) &&
                (all || (!b->deprecated && !hidden))) {
                jl_array_grow_end(a, 1);
                //XXX: change to jl_arrayset if array storage allocation for Array{Symbols,1} changes:
                jl_array_ptr_set(a, jl_array_dim0(a)-1, (jl_value_t*)b->name);
            }
        }
    }
    JL_UNLOCK(&m->lock);
    JL_GC_POP();
    return (jl_value_t*)a;
}

JL_DLLEXPORT jl_sym_t *jl_module_name(jl_module_t *m) { return m->name; }
JL_DLLEXPORT jl_module_t *jl_module_parent(jl_module_t *m) { return m->parent; }
JL_DLLEXPORT uint64_t jl_module_build_id(jl_module_t *m) { return m->build_id; }
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
    size_t i;
    JL_LOCK(&m->lock);
    void **table = m->bindings.table;
    for (i = 1; i < m->bindings.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            if (b->owner != m && !b->imported)
                table[i] = HT_NOTFOUND;
        }
    }
    JL_UNLOCK(&m->lock);
}

#ifdef __cplusplus
}
#endif

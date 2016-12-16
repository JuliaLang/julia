// This file is a part of Julia. License is MIT: http://julialang.org/license

/*
  modules and top-level bindings
*/
#include <assert.h>
#include "julia.h"
#include "julia_internal.h"

#ifdef __cplusplus
extern "C" {
#endif

jl_module_t *jl_main_module = NULL;
jl_module_t *jl_core_module = NULL;
jl_module_t *jl_base_module = NULL;
jl_module_t *jl_top_module = NULL;
extern jl_function_t *jl_append_any_func;

JL_DLLEXPORT jl_module_t *jl_new_module(jl_sym_t *name)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_module_t *m = (jl_module_t*)jl_gc_alloc(ptls, sizeof(jl_module_t),
                                               jl_module_type);
    JL_GC_PUSH1(&m);
    assert(jl_is_symbol(name));
    m->name = name;
    m->parent = NULL;
    m->istopmod = 0;
    static unsigned int mcounter; // simple counter backup, in case hrtime is not incrementing
    m->uuid = jl_hrtime() + (++mcounter);
    if (!m->uuid) m->uuid++; // uuid 0 is invalid
    m->counter = 0;
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
    return ++(m->counter);
}

JL_DLLEXPORT jl_value_t *jl_f_new_module(jl_sym_t *name, uint8_t std_imports)
{
    jl_module_t *m = jl_new_module(name);
    JL_GC_PUSH1(&m);
    m->parent = jl_main_module;
    jl_gc_wb(m, m->parent);
    if (std_imports) jl_add_standard_imports(m);
    JL_GC_POP();
    return (jl_value_t*)m;
}

JL_DLLEXPORT void jl_set_istopmod(uint8_t isprimary)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    ptls->current_module->istopmod = 1;
    if (isprimary) {
        jl_top_module = ptls->current_module;
        jl_append_any_func = NULL;
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
JL_DLLEXPORT jl_binding_t *jl_get_binding_wr(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t **bp = (jl_binding_t**)ptrhash_bp(&m->bindings, var);
    jl_binding_t *b;

    if (*bp != HT_NOTFOUND) {
        if ((*bp)->owner == NULL) {
            (*bp)->owner = m;
            return *bp;
        }
        else if ((*bp)->owner != m) {
            // TODO: change this to an error soon
            jl_printf(JL_STDERR,
                      "WARNING: imported binding for %s overwritten in module %s\n", jl_symbol_name(var), jl_symbol_name(m->name));
        }
        else {
            return *bp;
        }
    }

    b = new_binding(var);
    b->owner = m;
    *bp = b;
    jl_gc_wb_buf(m, b, sizeof(jl_binding_t));
    return *bp;
}

// return module of binding
JL_DLLEXPORT jl_module_t *jl_get_module_of_binding(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_binding(m, var);
    if (b == NULL)
        return NULL;
    return b->owner;
}

// get binding for adding a method
// like jl_get_binding_wr, but uses existing imports instead of warning
// and overwriting.
JL_DLLEXPORT jl_binding_t *jl_get_binding_for_method_def(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t **bp = (jl_binding_t**)ptrhash_bp(&m->bindings, var);
    jl_binding_t *b = *bp;

    if (b != HT_NOTFOUND) {
        if (b->owner != m && b->owner != NULL) {
            jl_binding_t *b2 = jl_get_binding(b->owner, var);
            if (b2 == NULL)
                jl_errorf("invalid method definition: imported function %s.%s does not exist", jl_symbol_name(b->owner->name), jl_symbol_name(var));
            // TODO: we might want to require explicitly importing types to add constructors
            if (!b->imported && (b2->value == NULL || !jl_is_type(b2->value))) {
                jl_errorf("error in method definition: function %s.%s must be explicitly imported to be extended", jl_symbol_name(b->owner->name),
                          jl_symbol_name(var));
            }
            return b2;
        }
        b->owner = m;
        return b;
    }

    b = new_binding(var);
    b->owner = m;
    *bp = b;
    jl_gc_wb_buf(m, b, sizeof(jl_binding_t));
    return *bp;
}

static void module_import_(jl_module_t *to, jl_module_t *from, jl_sym_t *s,
                           int explici);

typedef struct _modstack_t {
    jl_module_t *m;
    struct _modstack_t *prev;
} modstack_t;

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
    jl_binding_t *b = (jl_binding_t*)ptrhash_get(&m->bindings, var);
    if (b == HT_NOTFOUND || b->owner == NULL) {
        jl_module_t *owner = NULL;
        for(int i=(int)m->usings.len-1; i >= 0; --i) {
            jl_module_t *imp = (jl_module_t*)m->usings.items[i];
            jl_binding_t *tempb = (jl_binding_t*)ptrhash_get(&imp->bindings, var);
            if (tempb != HT_NOTFOUND && tempb->exportp) {
                tempb = jl_get_binding_(imp, var, &top);
                if (tempb == NULL || tempb->owner == NULL)
                    // couldn't resolve; try next using (see issue #6105)
                    continue;
                if (owner != NULL && tempb->owner != b->owner &&
                    !(tempb->constp && tempb->value && b->constp && b->value == tempb->value)) {
                    jl_printf(JL_STDERR,
                              "WARNING: both %s and %s export \"%s\"; uses of it in module %s must be qualified\n",
                              jl_symbol_name(owner->name),
                              jl_symbol_name(imp->name), jl_symbol_name(var),
                              jl_symbol_name(m->name));
                    // mark this binding resolved, to avoid repeating the warning
                    (void)jl_get_binding_wr(m, var);
                    return NULL;
                }
                if (owner == NULL || !tempb->deprecated) {
                    owner = imp;
                    b = tempb;
                }
            }
        }
        if (owner != NULL) {
            // do a full import to prevent the result of this lookup
            // from changing, for example if this var is assigned to
            // later.
            module_import_(m, b->owner, var, 0);
            return b;
        }
        return NULL;
    }
    if (b->owner != m)
        return jl_get_binding_(b->owner, var, &top);
    return b;
}

JL_DLLEXPORT jl_binding_t *jl_get_binding(jl_module_t *m, jl_sym_t *var)
{
    return jl_get_binding_(m, var, NULL);
}

void jl_binding_deprecation_warning(jl_binding_t *b);

JL_DLLEXPORT jl_binding_t *jl_get_binding_or_error(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_binding_(m, var, NULL);
    if (b == NULL)
        jl_undefined_var_error(var);
    if (b->deprecated)
        jl_binding_deprecation_warning(b);
    return b;
}

JL_DLLEXPORT jl_value_t *jl_module_globalref(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = (jl_binding_t*)ptrhash_get(&m->bindings, var);
    if (b == HT_NOTFOUND) {
        return jl_new_struct(jl_globalref_type, m, var);
    }
    if (b->globalref == NULL) {
        b->globalref = jl_new_struct(jl_globalref_type, m, var);
        jl_gc_wb(m, b->globalref);
    }
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
    jl_binding_t **bp = (jl_binding_t**)ptrhash_bp(&m->bindings, s);
    jl_binding_t *bto = *bp;
    return (bto != HT_NOTFOUND && bto->imported);
}

// NOTE: we use explici since explicit is a C++ keyword
static void module_import_(jl_module_t *to, jl_module_t *from, jl_sym_t *s,
                           int explici)
{
    if (to == from)
        return;
    jl_binding_t *b = jl_get_binding(from, s);
    if (b == NULL) {
        jl_printf(JL_STDERR,
                  "WARNING: could not import %s.%s into %s\n",
                  jl_symbol_name(from->name), jl_symbol_name(s),
                  jl_symbol_name(to->name));
    }
    else {
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
                    return;
                }
                jl_printf(JL_STDERR,
                          "WARNING: ignoring conflicting import of %s.%s into %s\n",
                          jl_symbol_name(from->name), jl_symbol_name(s),
                          jl_symbol_name(to->name));
            }
            else if (bto->constp || bto->value) {
                // conflict with name owned by destination module
                assert(bto->owner == to);
                if (bto->constp && bto->value && b->constp && b->value == bto->value) {
                    // equivalent binding
                    return;
                }
                jl_printf(JL_STDERR,
                          "WARNING: import of %s.%s into %s conflicts with an existing identifier; ignored.\n",
                          jl_symbol_name(from->name), jl_symbol_name(s),
                          jl_symbol_name(to->name));
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
    }
}

JL_DLLEXPORT void jl_module_import(jl_module_t *to, jl_module_t *from,
                                   jl_sym_t *s)
{
    module_import_(to, from, s, 1);
}

JL_DLLEXPORT void jl_module_use(jl_module_t *to, jl_module_t *from, jl_sym_t *s)
{
    module_import_(to, from, s, 0);
}

JL_DLLEXPORT void jl_module_importall(jl_module_t *to, jl_module_t *from)
{
    void **table = from->bindings.table;
    for(size_t i=1; i < from->bindings.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            if (b->exportp && (b->owner==from || b->imported))
                jl_module_import(to, from, b->name);
        }
    }
}

JL_DLLEXPORT void jl_module_using(jl_module_t *to, jl_module_t *from)
{
    if (to == from)
        return;
    for(size_t i=0; i < to->usings.len; i++) {
        if (from == to->usings.items[i])
            return;
    }
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
                    jl_printf(JL_STDERR,
                              "WARNING: using %s.%s in module %s conflicts with an existing identifier.\n",
                              jl_symbol_name(from->name), jl_symbol_name(var),
                              jl_symbol_name(to->name));
                }
            }
        }
    }

    arraylist_push(&to->usings, from);
}

JL_DLLEXPORT void jl_module_export(jl_module_t *from, jl_sym_t *s)
{
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
}

JL_DLLEXPORT int jl_boundp(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_binding(m, var);
    return b && (b->value != NULL);
}

JL_DLLEXPORT int jl_defines_or_exports_p(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t **bp = (jl_binding_t**)ptrhash_bp(&m->bindings, var);
    if (*bp == HT_NOTFOUND) return 0;
    return (*bp)->exportp || (*bp)->owner==m;
}

JL_DLLEXPORT int jl_module_exports_p(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t **bp = (jl_binding_t**)ptrhash_bp(&m->bindings, var);
    if (*bp == HT_NOTFOUND) return 0;
    return (*bp)->exportp;
}

JL_DLLEXPORT int jl_binding_resolved_p(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t **bp = (jl_binding_t**)ptrhash_bp(&m->bindings, var);
    if (*bp == HT_NOTFOUND) return 0;
    return (*bp)->owner != NULL;
}

JL_DLLEXPORT jl_value_t *jl_get_global(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_binding(m, var);
    if (b == NULL) return NULL;
    if (b->deprecated) jl_binding_deprecation_warning(b);
    return b->value;
}

JL_DLLEXPORT void jl_set_global(jl_module_t *m, jl_sym_t *var, jl_value_t *val)
{
    jl_binding_t *bp = jl_get_binding_wr(m, var);
    if (!bp->constp) {
        bp->value = val;
        jl_gc_wb(m, val);
    }
}

JL_DLLEXPORT void jl_set_const(jl_module_t *m, jl_sym_t *var, jl_value_t *val)
{
    jl_binding_t *bp = jl_get_binding_wr(m, var);
    if (!bp->constp) {
        bp->value = val;
        bp->constp = 1;
        jl_gc_wb(m, val);
    }
}

JL_DLLEXPORT int jl_is_const(jl_module_t *m, jl_sym_t *var)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (m == NULL) m = ptls->current_module;
    jl_binding_t *b = jl_get_binding(m, var);
    return b && b->constp;
}

JL_DLLEXPORT void jl_deprecate_binding(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_binding(m, var);
    if (b) b->deprecated = 1;
}

JL_DLLEXPORT int jl_is_binding_deprecated(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_binding(m, var);
    return b && b->deprecated;
}

extern const char *jl_filename;
extern int jl_lineno;

void jl_binding_deprecation_warning(jl_binding_t *b)
{
    if (b->deprecated && jl_options.depwarn) {
        if (jl_options.depwarn != JL_OPTIONS_DEPWARN_ERROR)
            jl_printf(JL_STDERR, "WARNING: ");
        if (b->owner)
            jl_printf(JL_STDERR, "%s.%s is deprecated",
                      jl_symbol_name(b->owner->name), jl_symbol_name(b->name));
        else
            jl_printf(JL_STDERR, "%s is deprecated", jl_symbol_name(b->name));
        jl_value_t *v = b->value;
        if (v && (jl_is_type(v) || jl_is_module(v)/* || (jl_is_function(v) && jl_is_gf(v))*/)) {
            jl_printf(JL_STDERR, ", use ");
            jl_static_show(JL_STDERR, v);
            jl_printf(JL_STDERR, " instead");
        }
        jl_printf(JL_STDERR, ".\n");

        if (jl_options.depwarn != JL_OPTIONS_DEPWARN_ERROR)
            jl_printf(JL_STDERR, "  likely near %s:%d\n", jl_filename, jl_lineno);

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
            jl_printf(JL_STDERR, "WARNING: redefining constant %s\n",
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

JL_DLLEXPORT jl_value_t *jl_get_current_module(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    return (jl_value_t*)ptls->current_module;
}

JL_DLLEXPORT void jl_set_current_module(jl_value_t *m)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    assert(jl_typeis(m, jl_module_type));
    ptls->current_module = (jl_module_t*)m;
}

JL_DLLEXPORT jl_value_t *jl_module_usings(jl_module_t *m)
{
    jl_array_t *a = jl_alloc_array_1d(jl_array_any_type, 0);
    JL_GC_PUSH1(&a);
    for(int i=(int)m->usings.len-1; i >= 0; --i) {
        jl_array_grow_end(a, 1);
        jl_module_t *imp = (jl_module_t*)m->usings.items[i];
        jl_array_ptr_set(a,jl_array_dim0(a)-1, (jl_value_t*)imp);
    }
    JL_GC_POP();
    return (jl_value_t*)a;
}

JL_DLLEXPORT jl_value_t *jl_module_names(jl_module_t *m, int all, int imported)
{
    jl_array_t *a = jl_alloc_array_1d(jl_array_symbol_type, 0);
    JL_GC_PUSH1(&a);
    size_t i;
    void **table = m->bindings.table;
    for (i = 1; i < m->bindings.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            int hidden = jl_symbol_name(b->name)[0]=='#';
            if ((b->exportp ||
                 (imported && b->imported) ||
                 ((b->owner == m) && (all || m == jl_main_module))) &&
                (all || (!b->deprecated && !hidden))) {
                jl_array_grow_end(a, 1);
                //XXX: change to jl_arrayset if array storage allocation for Array{Symbols,1} changes:
                jl_array_ptr_set(a, jl_array_dim0(a)-1, (jl_value_t*)b->name);
            }
        }
    }
    JL_GC_POP();
    return (jl_value_t*)a;
}

JL_DLLEXPORT jl_sym_t *jl_module_name(jl_module_t *m) { return m->name; }
JL_DLLEXPORT jl_module_t *jl_module_parent(jl_module_t *m) { return m->parent; }
JL_DLLEXPORT uint64_t jl_module_uuid(jl_module_t *m) { return m->uuid; }

int jl_is_submodule(jl_module_t *child, jl_module_t *parent)
{
    while (1) {
        if (parent == child)
            return 1;
        if (child == NULL || child == child->parent)
            return 0;
        child = child->parent;
    }
}

#ifdef __cplusplus
}
#endif

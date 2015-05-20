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

jl_module_t *jl_main_module=NULL;
jl_module_t *jl_core_module=NULL;
jl_module_t *jl_base_module=NULL;
jl_module_t *jl_top_module=NULL;
jl_module_t *jl_current_module=NULL;

jl_module_t *jl_new_module(jl_sym_t *name)
{
    jl_module_t *m = (jl_module_t*)allocobj(sizeof(jl_module_t));
    jl_set_typeof(m, jl_module_type);
    JL_GC_PUSH1(&m);
    assert(jl_is_symbol(name));
    m->name = name;
    m->parent = NULL;
    m->constant_table = NULL;
    m->call_func = NULL;
    m->istopmod = 0;
    m->uuid = uv_now(uv_default_loop());
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

DLLEXPORT jl_value_t *jl_f_new_module(jl_sym_t *name, uint8_t std_imports)
{
    jl_module_t *m = jl_new_module(name);
    m->parent = jl_main_module;
    gc_wb(m, m->parent);
    if (std_imports) jl_add_standard_imports(m);
    return (jl_value_t*)m;
}

DLLEXPORT void jl_set_istopmod(uint8_t isprimary)
{
    jl_current_module->istopmod = 1;
    if (isprimary)
        jl_top_module = jl_current_module;
}

DLLEXPORT uint8_t jl_istopmod(jl_module_t *mod)
{
    return mod->istopmod;
}

static jl_binding_t *new_binding(jl_sym_t *name)
{
    assert(jl_is_symbol(name));
    jl_binding_t *b = (jl_binding_t*)allocb(sizeof(jl_binding_t));
    b->name = name;
    b->value = NULL;
    b->type = (jl_value_t*)jl_any_type;
    b->owner = NULL;
    b->constp = 0;
    b->exportp = 0;
    b->imported = 0;
    return b;
}

// get binding for assignment
jl_binding_t *jl_get_binding_wr(jl_module_t *m, jl_sym_t *var)
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
                       "Warning: imported binding for %s overwritten in module %s\n", var->name, m->name->name);
        }
        else {
            return *bp;
        }
    }

    b = new_binding(var);
    b->owner = m;
    *bp = b;
    gc_wb_buf(m, b);
    return *bp;
}

// return module of binding
DLLEXPORT jl_module_t *jl_get_module_of_binding(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_binding(m, var);
    if (b == NULL)
        jl_undefined_var_error(var);
    return b->owner;
}

// get binding for adding a method
// like jl_get_binding_wr, but uses existing imports instead of warning
// and overwriting.
jl_binding_t *jl_get_binding_for_method_def(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t **bp = (jl_binding_t**)ptrhash_bp(&m->bindings, var);
    jl_binding_t *b = *bp;

    if (b != HT_NOTFOUND) {
        if (b->owner != m && b->owner != NULL) {
            jl_binding_t *b2 = jl_get_binding(b->owner, var);
            if (b2 == NULL)
                jl_errorf("invalid method definition: imported function %s.%s does not exist", b->owner->name->name, var->name);
            if (!b->imported && (b2->value==NULL || jl_is_function(b2->value))) {
                if (b2->value && !jl_is_gf(b2->value)) {
                    jl_errorf("error in method definition: %s.%s cannot be extended", b->owner->name->name, var->name);
                }
                else {
                    jl_errorf("error in method definition: function %s.%s must be explicitly imported to be extended", b->owner->name->name, var->name);
                }
            }
            return b2;
        }
        b->owner = m;
        return b;
    }

    b = new_binding(var);
    b->owner = m;
    *bp = b;
    gc_wb_buf(m, b);
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
                              "Warning: both %s and %s export \"%s\"; uses of it in module %s must be qualified\n",
                              owner->name->name, imp->name->name, var->name, m->name->name);
                    // mark this binding resolved, to avoid repeating the warning
                    (void)jl_get_binding_wr(m, var);
                    return NULL;
                }
                owner = imp;
                b = tempb;
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

jl_binding_t *jl_get_binding(jl_module_t *m, jl_sym_t *var)
{
    return jl_get_binding_(m, var, NULL);
}

static int eq_bindings(jl_binding_t *a, jl_binding_t *b)
{
    if (a==b) return 1;
    if (a->name == b->name && a->owner == b->owner) return 1;
    if (a->constp && a->value && b->constp && b->value == a->value) return 1;
    return 0;
}

// does module m explicitly import s?
int jl_is_imported(jl_module_t *m, jl_sym_t *s)
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
                  "Warning: could not import %s.%s into %s\n",
                  from->name->name, s->name, to->name->name);
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
                          "Warning: ignoring conflicting import of %s.%s into %s\n",
                          from->name->name, s->name, to->name->name);
            }
            else if (bto->constp || bto->value) {
                // conflict with name owned by destination module
                assert(bto->owner == to);
                if (bto->constp && bto->value && b->constp && b->value == bto->value) {
                    // equivalent binding
                    return;
                }
                jl_printf(JL_STDERR,
                          "Warning: import of %s.%s into %s conflicts with an existing identifier; ignored.\n",
                          from->name->name, s->name, to->name->name);
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
            *bp = nb;
            gc_wb_buf(to, nb);
        }
    }
}

void jl_module_import(jl_module_t *to, jl_module_t *from, jl_sym_t *s)
{
    module_import_(to, from, s, 1);
}

void jl_module_use(jl_module_t *to, jl_module_t *from, jl_sym_t *s)
{
    module_import_(to, from, s, 0);
}

void jl_module_importall(jl_module_t *to, jl_module_t *from)
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

void jl_module_using(jl_module_t *to, jl_module_t *from)
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
                              "Warning: using %s.%s in module %s conflicts with an existing identifier.\n",
                              from->name->name, var->name, to->name->name);
                }
            }
        }
    }

    arraylist_push(&to->usings, from);
}

void jl_module_export(jl_module_t *from, jl_sym_t *s)
{
    jl_binding_t **bp = (jl_binding_t**)ptrhash_bp(&from->bindings, s);
    if (*bp == HT_NOTFOUND) {
        jl_binding_t *b = new_binding(s);
        // don't yet know who the owner is
        b->owner = NULL;
        *bp = b;
        gc_wb_buf(from, b);
    }
    assert(*bp != HT_NOTFOUND);
    (*bp)->exportp = 1;
}

int jl_boundp(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_binding(m, var);
    return b && (b->value != NULL);
}

int jl_defines_or_exports_p(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t **bp = (jl_binding_t**)ptrhash_bp(&m->bindings, var);
    if (*bp == HT_NOTFOUND) return 0;
    return (*bp)->exportp || (*bp)->owner==m;
}

int jl_binding_resolved_p(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t **bp = (jl_binding_t**)ptrhash_bp(&m->bindings, var);
    if (*bp == HT_NOTFOUND) return 0;
    return (*bp)->owner != NULL;
}

jl_value_t *jl_get_global(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_binding(m, var);
    if (b == NULL) return NULL;
    return b->value;
}

void jl_set_global(jl_module_t *m, jl_sym_t *var, jl_value_t *val)
{
    jl_binding_t *bp = jl_get_binding_wr(m, var);
    if (!bp->constp) {
        bp->value = val;
        gc_wb(m, val);
    }
}

void jl_set_const(jl_module_t *m, jl_sym_t *var, jl_value_t *val)
{
    jl_binding_t *bp = jl_get_binding_wr(m, var);
    if (!bp->constp) {
        bp->value = val;
        bp->constp = 1;
        gc_wb(m, val);
    }
}

DLLEXPORT int jl_is_const(jl_module_t *m, jl_sym_t *var)
{
    if (m == NULL) m = jl_current_module;
    jl_binding_t *b = jl_get_binding(m, var);
    return b && b->constp;
}

DLLEXPORT void jl_checked_assignment(jl_binding_t *b, jl_value_t *rhs)
{
    if (b->constp && b->value != NULL) {
        if (!jl_egal(rhs, b->value)) {
            if (jl_typeof(rhs) != jl_typeof(b->value) ||
                jl_is_type(rhs) || jl_is_function(rhs) || jl_is_module(rhs)) {
                jl_errorf("invalid redefinition of constant %s", b->name->name);
            }
            jl_printf(JL_STDERR,"Warning: redefining constant %s\n",b->name->name);
        }
    }
    b->value = rhs;
    gc_wb_binding(b, rhs);
}

DLLEXPORT void jl_declare_constant(jl_binding_t *b)
{
    if (b->value != NULL && !b->constp) {
        jl_errorf("cannot declare %s constant; it already has a value",
                  b->name->name);
    }
    b->constp = 1;
}

DLLEXPORT jl_value_t *jl_get_current_module()
{
    return (jl_value_t*)jl_current_module;
}

DLLEXPORT void jl_set_current_module(jl_value_t *m)
{
    assert(jl_typeis(m, jl_module_type));
    jl_current_module = (jl_module_t*)m;
}

DLLEXPORT jl_value_t *jl_module_usings(jl_module_t *m)
{
    jl_array_t *a = jl_alloc_array_1d(jl_array_any_type, 0);
    JL_GC_PUSH1(&a);
    for(int i=(int)m->usings.len-1; i >= 0; --i) {
        jl_array_grow_end(a, 1);
        jl_module_t *imp = (jl_module_t*)m->usings.items[i];
        jl_cellset(a,jl_array_dim0(a)-1, (jl_value_t*)imp);
    }
    JL_GC_POP();
    return (jl_value_t*)a;
}

DLLEXPORT jl_value_t *jl_module_names(jl_module_t *m, int all, int imported)
{
    jl_array_t *a = jl_alloc_array_1d(jl_array_symbol_type, 0);
    JL_GC_PUSH1(&a);
    size_t i;
    void **table = m->bindings.table;
    for(i=1; i < m->bindings.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            if (b->exportp || ((imported || b->owner == m) && (all || m == jl_main_module))) {
                jl_array_grow_end(a, 1);
                //XXX: change to jl_arrayset if array storage allocation for Array{Symbols,1} changes:
                jl_cellset(a, jl_array_dim0(a)-1, (jl_value_t*)b->name);
            }
        }
    }
    JL_GC_POP();
    return (jl_value_t*)a;
}

DLLEXPORT jl_sym_t *jl_module_name(jl_module_t *m) { return m->name; }
DLLEXPORT jl_module_t *jl_module_parent(jl_module_t *m) { return m->parent; }
DLLEXPORT uint64_t jl_module_uuid(jl_module_t *m) { return m->uuid; }

jl_function_t *jl_module_get_initializer(jl_module_t *m)
{
    jl_value_t *f = jl_get_global(m, jl_symbol("__init__"));
    if (f == NULL || !jl_is_function(f))
        return NULL;
    return (jl_function_t*)f;
}

void jl_module_run_initializer(jl_module_t *m)
{
    jl_function_t *f = jl_module_get_initializer(m);
    if (f == NULL)
        return;
    JL_TRY {
        jl_apply(f, NULL, 0);
    }
    JL_CATCH {
        jl_printf(JL_STDERR, "Warning: error initializing module %s:\n", m->name->name);
        jl_static_show(JL_STDERR, jl_exception_in_transit);
        jl_printf(JL_STDERR, "\n");
    }
}

jl_function_t *jl_module_call_func(jl_module_t *m)
{
    if (m->call_func == NULL) {
        jl_function_t *cf = (jl_function_t*)jl_get_global(m, call_sym);
        if (cf == NULL || !jl_is_function(cf))
            cf = jl_bottom_func;
        m->call_func = cf;
    }
    return m->call_func;
}

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

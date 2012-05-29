/*
  modules and top-level bindings
*/
#include <assert.h>
#include "julia.h"

jl_module_t *jl_core_module=NULL;
jl_module_t *jl_base_module=NULL;
jl_module_t *jl_current_module=NULL;

static jl_binding_t *varlist_binding=NULL;

jl_module_t *jl_new_module(jl_sym_t *name)
{
    jl_module_t *m = (jl_module_t*)allocobj(sizeof(jl_module_t));
    m->type = (jl_type_t*)jl_module_type;
    m->name = name;
    htable_new(&m->bindings, 0);
    htable_new(&m->macros, 0);
    jl_set_const(m, name, (jl_value_t*)m);
    if (jl_current_module)
        jl_set_const(m, jl_current_module->name, (jl_value_t*)jl_current_module);
    arraylist_new(&m->imports, 0);
    if (jl_core_module) {
        jl_module_importall(m, jl_core_module);
    }
    return m;
}

static jl_binding_t *new_binding(jl_sym_t *name)
{
    jl_binding_t *b = (jl_binding_t*)allocb(sizeof(jl_binding_t));
    b->name = name;
    b->value = NULL;
    b->type = (jl_type_t*)jl_any_type;
    b->owner = NULL;
    b->constp = 0;
    b->exportp = 0;
    return b;
}

// get binding for assignment
jl_binding_t *jl_get_binding_wr(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t **bp = (jl_binding_t**)ptrhash_bp(&m->bindings, var);
    jl_binding_t *b;

    if (*bp != HT_NOTFOUND) {
        if ((*bp)->owner != m) {
            ios_printf(JL_STDERR,
                       "Warning: imported binding for %s overwritten in module %s", var->name, m->name->name);
        }
        else {
            return *bp;
        }
    }

    b = new_binding(var);
    b->owner = m;
    *bp = b;

    // keep track of all variables added after the VARIABLES array
    // is defined
    if (jl_base_module) {
        if (varlist_binding == NULL) {
            varlist_binding = jl_get_binding(jl_base_module, jl_symbol("VARIABLES"));
        }
        if (varlist_binding && varlist_binding->value != NULL &&
            jl_typeis(varlist_binding->value, jl_array_any_type)) {
            jl_array_t *a = (jl_array_t*)varlist_binding->value;
            jl_cell_1d_push(a, (jl_value_t*)var);
        }
    }
    return *bp;
}

typedef struct _mod_stack_t {
    jl_module_t *m;
    struct _mod_stack_t *prev;
} mod_stack_t;

// get binding for reading. might return NULL for unbound.
static jl_binding_t *get_binding_(jl_module_t *m, jl_sym_t *var, mod_stack_t *p)
{
    if (m == NULL) return NULL;
    mod_stack_t *pp = p;
    // handle mutual importing
    while (pp != NULL) {
        if (pp->m == m)
            return NULL;
        pp = pp->prev;
    }
    mod_stack_t top = { m, p };
    jl_binding_t *b = (jl_binding_t*)ptrhash_get(&m->bindings, var);
    if (b == HT_NOTFOUND) {
        for(size_t i=0; i < m->imports.len; i++) {
            b = get_binding_((jl_module_t*)m->imports.items[i], var, &top);
            if (b != NULL)
                return b;
        }
        return NULL;
    }
    if (b->owner != m)
        return get_binding_(b->owner, var, &top);
    return b;
}

jl_binding_t *jl_get_binding(jl_module_t *m, jl_sym_t *var)
{
    return get_binding_(m, var, NULL);
}

void jl_module_importall(jl_module_t *to, jl_module_t *from)
{
    if (to == from)
        return;
    for(size_t i=0; i < to->imports.len; i++) {
        if (from == to->imports.items[i])
            return;
    }
    arraylist_push(&to->imports, from);
}

void jl_module_import(jl_module_t *to, jl_module_t *from, jl_sym_t *s)
{
    if (to == from)
        return;
    jl_binding_t *b = jl_get_binding(from, s);
    if (b == NULL) {
        ios_printf(JL_STDERR,
                   "Warning: could not import %s.%s into %s",
                   from->name->name, s->name, to->name->name);
    }
    else {
        jl_binding_t **bp = (jl_binding_t**)ptrhash_bp(&to->bindings, s);
        if (*bp != HT_NOTFOUND) {
            if ((*bp)->owner != to) {
                ios_printf(JL_STDERR,
                           "Warning: ignoring conflicting import of %s into %s",
                           s->name, to->name->name);
            }
            else if ((*bp)->constp || (*bp)->value) {
                ios_printf(JL_STDERR,
                           "Warning: import of %s into %s conflicts with an existing identifier; ignored",
                           s->name, to->name->name);
            }
            else if (*bp == b) {
                // importing a binding on top of itself. harmless.
            }
            else {
                (*bp)->owner = b->owner;
            }
        }
        else {
            jl_binding_t *nb = new_binding(s);
            nb->owner = b->owner;
            *bp = nb;
        }
    }
}

int jl_boundp(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = jl_get_binding(m, var);
    return b && (b->value != NULL);
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
    }
}

void jl_set_const(jl_module_t *m, jl_sym_t *var, jl_value_t *val)
{
    jl_binding_t *bp = jl_get_binding_wr(m, var);
    if (!bp->constp) {
        bp->value = val;
        bp->constp = 1;
    }
}

DLLEXPORT int jl_is_const(jl_module_t *m, jl_sym_t *var)
{
    if (m == NULL) m = jl_current_module;
    jl_binding_t *b = jl_get_binding(m, var);
    return b && b->constp;
}

void jl_checked_assignment(jl_binding_t *b, jl_value_t *rhs)
{
    if (b->constp && b->value != NULL) {
        //jl_errorf("cannot redefine constant %s", b->name->name);
        JL_PRINTF(JL_STDERR, "Warning: redefinition of constant %s ignored\n",
                   b->name->name);
    }
    else {
        b->value = rhs;
    }
}

void jl_declare_constant(jl_binding_t *b)
{
    if (b->value != NULL && !b->constp) {
        jl_errorf("cannot declare %s constant; it already has a value",
                  b->name->name);
    }
    b->constp = 1;
}

jl_function_t *jl_get_expander(jl_module_t *m, jl_sym_t *macroname)
{
    jl_function_t *f = (jl_function_t*)ptrhash_get(&m->macros, macroname);
    if (f == HT_NOTFOUND)
        return NULL;
    return f;
}

void jl_set_expander(jl_module_t *m, jl_sym_t *macroname, jl_function_t *f)
{
    jl_function_t **bp = (jl_function_t**)ptrhash_bp(&m->macros, macroname);
    *bp = f;
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

DLLEXPORT jl_value_t *jl_module_names(jl_module_t *m)
{
    jl_array_t *a = jl_alloc_cell_1d(0);
    JL_GC_PUSH(&a);
    size_t i;
    void **table = m->bindings.table;
    for(i=1; i < m->bindings.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            if (b->value != NULL || b->exportp || b->constp)
                jl_cell_1d_push(a, (jl_value_t*)b->name);
        }
    }
    JL_GC_POP();
    return (jl_value_t*)a;
}

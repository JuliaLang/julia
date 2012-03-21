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
    //arraylist_new(&m->imports, 0);
    return m;
}

// get binding for assignment
jl_binding_t *jl_get_binding_wr(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t **bp = (jl_binding_t**)ptrhash_bp(&m->bindings, var);
    jl_binding_t *b;
    if (*bp == HT_NOTFOUND) {
        b = (jl_binding_t*)allocb(sizeof(jl_binding_t));
        b->name = var;
        b->value = NULL;
        b->type = (jl_type_t*)jl_any_type;
        b->constp = 0;
        b->exportp = 0;
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
    }
    return *bp;
}

// get binding for reading. might return NULL for unbound.
jl_binding_t *jl_get_binding(jl_module_t *m, jl_sym_t *var)
{
    if (m == NULL) return NULL;
    jl_binding_t *b = (jl_binding_t*)ptrhash_get(&m->bindings, var);
    if (b == HT_NOTFOUND) {
        if (jl_core_module && m != jl_core_module) {
            return jl_get_binding(jl_core_module, var);
        }
        return NULL;
    }
    return b;
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
        ios_printf(ios_stderr, "Warning: redefinition of constant %s ignored\n",
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

jl_module_t *jl_add_module(jl_module_t *m, jl_module_t *child);
jl_module_t *jl_get_module(jl_module_t *m, jl_sym_t *name);
jl_module_t *jl_import_module(jl_module_t *to, jl_module_t *from);

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

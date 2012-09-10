/*
  modules and top-level bindings
*/
#include <assert.h>
#include "julia.h"

jl_module_t *jl_root_module=NULL;
jl_module_t *jl_core_module=NULL;
jl_module_t *jl_base_module=NULL;
jl_module_t *jl_main_module=NULL;
jl_module_t *jl_current_module=NULL;

jl_module_t *jl_new_module(jl_sym_t *name)
{
    jl_module_t *m = (jl_module_t*)allocobj(sizeof(jl_module_t));
    m->type = (jl_type_t*)jl_module_type;
    m->name = name;
    htable_new(&m->bindings, 0);
    jl_set_const(m, name, (jl_value_t*)m);
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
                       "Warning: imported binding for %s overwritten in module %s\n", var->name, m->name->name);
        }
        else {
            return *bp;
        }
    }

    b = new_binding(var);
    b->owner = m;
    *bp = b;
    if (m == jl_root_module) {
        b->exportp = 1;  // export everything from Root
    }
    return *bp;
}

// get binding for reading. might return NULL for unbound.
jl_binding_t *jl_get_binding(jl_module_t *m, jl_sym_t *var)
{
    jl_binding_t *b = (jl_binding_t*)ptrhash_get(&m->bindings, var);
    if (b == HT_NOTFOUND) {
        for(size_t i=0; i < m->imports.len; i++) {
            jl_module_t *imp = (jl_module_t*)m->imports.items[i];
            b = (jl_binding_t*)ptrhash_get(&imp->bindings, var);
            if (b != HT_NOTFOUND && b->exportp) {
                if (b->owner != imp)
                    return jl_get_binding(b->owner, var);
                // only import if the source module has resolved the binding;
                // otherwise it might just be marked for re-export.
                if (b->constp || b->value) {
                    return b;
                }
            }
        }
        return NULL;
    }
    if (b->owner != m)
        return jl_get_binding(b->owner, var);
    return b;
}

void jl_module_import(jl_module_t *to, jl_module_t *from, jl_sym_t *s)
{
    if (to == from)
        return;
    jl_binding_t *b = jl_get_binding(from, s);
    if (b == NULL || !b->exportp) {
        ios_printf(JL_STDERR,
                   "Warning: could not import %s.%s into %s\n",
                   from->name->name, s->name, to->name->name);
    }
    else {
        jl_binding_t **bp = (jl_binding_t**)ptrhash_bp(&to->bindings, s);
        if (*bp != HT_NOTFOUND) {
            if ((*bp)->owner != to) {
                ios_printf(JL_STDERR,
                           "Warning: ignoring conflicting import of %s.%s into %s\n",
                           from->name->name, s->name, to->name->name);
            }
            else if (*bp == b) {
                // importing a binding on top of itself. harmless.
            }
            else if ((*bp)->constp || (*bp)->value) {
                if ((*bp)->constp && (*bp)->value && b->constp &&
                    b->value == (*bp)->value) {
                    // import of equivalent binding
                    return;
                }
                ios_printf(JL_STDERR,
                           "Warning: import of %s.%s into %s conflicts with an existing identifier; ignored.\n",
                           from->name->name, s->name, to->name->name);
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

void jl_module_importall(jl_module_t *to, jl_module_t *from)
{
    if (to == from)
        return;
    for(size_t i=0; i < to->imports.len; i++) {
        if (from == to->imports.items[i])
            return;
    }
    arraylist_push(&to->imports, from);
    // go though all "from" module's exports to check for conflicts, and
    // re-resolve symbols if it's not too late.
    // mostly, we want to handle "export x" occurring before the "import M.*"
    // statement that actually provides x.
    void **table = from->bindings.table;
    for(size_t i=1; i < from->bindings.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            if (b->exportp) {
                jl_binding_t **bp =
                    (jl_binding_t**)ptrhash_bp(&to->bindings, b->name);
                if (*bp != HT_NOTFOUND) {
                    jl_module_import(to, from, b->name);
                }
            }
        }
    }
}

void jl_module_export(jl_module_t *from, jl_sym_t *s)
{
    jl_binding_t **bp = (jl_binding_t**)ptrhash_bp(&from->bindings, s);
    if (*bp == HT_NOTFOUND) {
        jl_binding_t *b = jl_get_binding(from, s);
        if (b == NULL) {
            b = jl_get_binding_wr(from, s);
        }
        if (b->owner != from) {
            // create an explicit import so we can mark as re-exported
            jl_module_import(from, b->owner, s);
        }
        bp = (jl_binding_t**)ptrhash_bp(&from->bindings, s);
    }
    assert(*bp != HT_NOTFOUND);
    (*bp)->exportp = 1;
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
        JL_PRINTF(JL_STDERR, "Warning: redefinition of constant %s ignored.\n",
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

DLLEXPORT jl_value_t *jl_get_current_module()
{
    return (jl_value_t*)jl_current_module;
}

DLLEXPORT void jl_set_current_module(jl_value_t *m)
{
    assert(jl_typeis(m, jl_module_type));
    jl_current_module = (jl_module_t*)m;
}

DLLEXPORT jl_value_t *jl_module_names(jl_module_t *m, int all)
{
    jl_array_t *a = jl_alloc_array_1d(jl_array_symbol_type, 0);
    JL_GC_PUSH(&a);
    size_t i;
    void **table = m->bindings.table;
    for(i=1; i < m->bindings.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            if (all || b->exportp || m == jl_main_module) {
                jl_array_grow_end(a, 1);
                //XXX: change to jl_arrayset if array storage allocation for Array{Symbols,1} changes:
                jl_cellset(a, a->length-1, (jl_value_t*)b->name);
            }
        }
    }
    JL_GC_POP();
    return (jl_value_t*)a;
}

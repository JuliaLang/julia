// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  evaluating top-level expressions, loading source files
*/
#include "platform.h"

#include <stdlib.h>
#include <string.h>
#include <setjmp.h>
#include <sys/types.h>
#include <errno.h>
#if defined(_OS_WINDOWS_)
#include <malloc.h>
#else
#include <unistd.h>
#endif
#include "julia.h"
#include "julia_internal.h"
#include "uv.h"
#include "julia_assert.h"
#include "intrinsics.h"

#ifdef __cplusplus
extern "C" {
#endif

// current line number in a file
JL_DLLEXPORT int jl_lineno = 0; // need to update jl_critical_error if this is TLS
// current file name
JL_DLLEXPORT const char *jl_filename = "no file"; // need to update jl_critical_error if this is TLS

// the Main we started with, in case it is switched
jl_module_t *jl_internal_main_module = NULL;

JL_DLLEXPORT void jl_add_standard_imports(jl_module_t *m)
{
    jl_module_t *base_module = jl_base_relative_to(m);
    assert(base_module != NULL);
    // using Base
    jl_module_using(m, base_module);
}

JL_DLLEXPORT jl_module_t *jl_new_main_module(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (jl_generating_output() && jl_options.incremental)
        jl_error("cannot call workspace() in incremental compile mode");

    // switch to a new top-level module
    if (ptls->current_module != jl_main_module &&
        ptls->current_module != NULL && jl_main_module != NULL)
        jl_error("Main can only be replaced from the top level");

    jl_module_t *old_main = jl_main_module;

    jl_main_module = jl_new_module(jl_symbol("Main"));
    jl_main_module->parent = jl_main_module;
    if (old_main) { // don't block continued loading of incremental caches
        jl_main_module->primary_world = old_main->primary_world;
        jl_main_module->build_id = old_main->build_id;
        jl_main_module->uuid = old_main->uuid;
    }
    ptls->current_module = jl_main_module;

    jl_core_module->parent = jl_main_module;
    jl_set_const(jl_main_module, jl_symbol("Core"),
                 (jl_value_t*)jl_core_module);
    jl_set_global(jl_core_module, jl_symbol("Main"),
                  (jl_value_t*)jl_main_module);
    ptls->current_task->current_module = jl_main_module;

    return old_main;
}

static jl_function_t *jl_module_get_initializer(jl_module_t *m)
{
    return (jl_function_t*)jl_get_global(m, jl_symbol("__init__"));
}


void jl_module_run_initializer(jl_module_t *m)
{
    jl_function_t *f = jl_module_get_initializer(m);
    if (f == NULL)
        return;
    size_t last_age = jl_get_ptls_states()->world_age;
    JL_TRY {
        jl_get_ptls_states()->world_age = jl_world_counter;
        jl_apply(&f, 1);
        jl_get_ptls_states()->world_age = last_age;
    }
    JL_CATCH {
        if (jl_initerror_type == NULL) {
            jl_rethrow();
        }
        else {
            jl_rethrow_other(jl_new_struct(jl_initerror_type, m->name,
                                           jl_exception_in_transit));
        }
    }
}

// load time init procedure: in build mode, only record order
static void jl_module_load_time_initialize(jl_module_t *m)
{
    int build_mode = jl_generating_output();
    if (build_mode) {
        if (jl_module_init_order == NULL)
            jl_module_init_order = jl_alloc_vec_any(0);
        jl_array_ptr_1d_push(jl_module_init_order, (jl_value_t*)m);
        jl_function_t *f = jl_module_get_initializer(m);
        if (f != NULL && jl_options.incremental) {
            jl_value_t *tt = jl_is_type(f) ? (jl_value_t*)jl_wrap_Type(f) : jl_typeof(f);
            JL_GC_PUSH1(&tt);
            tt = (jl_value_t*)jl_apply_tuple_type_v(&tt, 1);
            jl_compile_hint((jl_tupletype_t*)tt);
            JL_GC_POP();
        }
    }
    else {
        jl_module_run_initializer(m);
    }
}

void jl_register_root_module(jl_module_t *m)
{
    static jl_value_t *register_module_func = NULL;
    assert(jl_base_module);
    if (register_module_func == NULL)
        register_module_func = jl_get_global(jl_base_module, jl_symbol("register_root_module"));
    assert(register_module_func);
    jl_value_t *args[2];
    args[0] = register_module_func;
    args[1] = (jl_value_t*)m;
    jl_apply(args, 2);
}

jl_array_t *jl_get_loaded_modules(void)
{
    static jl_value_t *loaded_modules_array = NULL;
    if (loaded_modules_array == NULL && jl_base_module != NULL)
        loaded_modules_array = jl_get_global(jl_base_module, jl_symbol("loaded_modules_array"));
    if (loaded_modules_array != NULL)
        return (jl_array_t*)jl_call0((jl_function_t*)loaded_modules_array);
    return NULL;
}

jl_value_t *jl_eval_module_expr(jl_module_t *parent_module, jl_expr_t *ex)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    static arraylist_t module_stack;
    static int initialized=0;
    static jl_module_t *outermost = NULL;
    if (!initialized) {
        arraylist_new(&module_stack, 0);
        initialized = 1;
    }
    assert(ex->head == module_sym);
    jl_module_t *last_module = ptls->current_module;
    if (jl_array_len(ex->args) != 3 || !jl_is_expr(jl_exprarg(ex,2))) {
        jl_error("syntax: malformed module expression");
    }
    int std_imports = (jl_exprarg(ex,0)==jl_true);
    jl_sym_t *name = (jl_sym_t*)jl_exprarg(ex, 1);
    if (!jl_is_symbol(name)) {
        jl_type_error("module", (jl_value_t*)jl_sym_type, (jl_value_t*)name);
    }
    jl_module_t *newm = jl_new_module(name);
    jl_value_t *defaultdefs = NULL, *form = NULL;
    JL_GC_PUSH4(&last_module, &defaultdefs, &form, &newm);
    // copy parent environment info into submodule
    newm->uuid = parent_module->uuid;
    if (jl_base_module &&
            (jl_value_t*)parent_module == jl_get_global(jl_base_module, jl_symbol("__toplevel__"))) {
        newm->parent = newm;
        jl_register_root_module(newm);
    }
    else {
        jl_binding_t *b = jl_get_binding_wr(parent_module, name, 1);
        jl_declare_constant(b);
        if (b->value != NULL) {
            if (!jl_is_module(b->value)) {
                jl_errorf("invalid redefinition of constant %s", jl_symbol_name(name));
            }
            if (jl_generating_output()) {
                jl_errorf("cannot replace module %s during compilation", jl_symbol_name(name));
            }
            jl_printf(JL_STDERR, "WARNING: replacing module %s.\n", jl_symbol_name(name));
        }
        newm->parent = parent_module;
        b->value = (jl_value_t*)newm;
        jl_gc_wb_binding(b, newm);
    }
    // Assume `newm` is globally reachable at this point.

    if (parent_module == jl_main_module && name == jl_symbol("Base")) {
        // pick up Base module during bootstrap
        jl_base_module = newm;
    }

    // add standard imports unless baremodule
    if (std_imports) {
        if (jl_base_module != NULL) {
            jl_add_standard_imports(newm);
        }
    }

    size_t last_age = ptls->world_age;
    jl_module_t *task_last_m = ptls->current_task->current_module;
    ptls->current_task->current_module = ptls->current_module = newm;

    jl_module_t *prev_outermost = outermost;
    size_t stackidx = module_stack.len;
    if (outermost == NULL)
        outermost = newm;

    jl_array_t *exprs = ((jl_expr_t*)jl_exprarg(ex, 2))->args;
    JL_TRY {
        if (std_imports) {
            // add `eval` function
            defaultdefs = jl_call_scm_on_ast("module-default-defs", (jl_value_t*)ex, newm);
            ptls->world_age = jl_world_counter;
            jl_toplevel_eval_flex(newm, defaultdefs, 0, 1);
            defaultdefs = NULL;
        }

        for (int i = 0; i < jl_array_len(exprs); i++) {
            // process toplevel form
            ptls->world_age = jl_world_counter;
            form = jl_expand_stmt(jl_array_ptr_ref(exprs, i), newm);
            ptls->world_age = jl_world_counter;
            (void)jl_toplevel_eval_flex(newm, form, 1, 1);
        }
    }
    JL_CATCH {
        ptls->current_module = last_module;
        ptls->current_task->current_module = task_last_m;
        outermost = prev_outermost;
        module_stack.len = stackidx;
        jl_rethrow();
    }
    JL_GC_POP();
    ptls->world_age = last_age;
    ptls->current_module = last_module;
    ptls->current_task->current_module = task_last_m;
    outermost = prev_outermost;

#if 0
    // some optional post-processing steps
    size_t i;
    void **table = newm->bindings.table;
    for(i=1; i < newm->bindings.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            // remove non-exported macros
            if (jl_symbol_name(b->name)[0]=='@' &&
                !b->exportp && b->owner == newm)
                b->value = NULL;
            // error for unassigned exports
            /*
            if (b->exportp && b->owner==newm && b->value==NULL)
                jl_errorf("identifier %s exported from %s is not initialized",
                          jl_symbol_name(b->name), jl_symbol_name(newm->name));
            */
        }
    }
#endif

    arraylist_push(&module_stack, newm);

    if (outermost == NULL || parent_module == jl_main_module) {
        JL_TRY {
            size_t i, l = module_stack.len;
            for (i = stackidx; i < l; i++) {
                jl_module_load_time_initialize((jl_module_t*)module_stack.items[i]);
            }
            assert(module_stack.len == l);
            module_stack.len = stackidx;
        }
        JL_CATCH {
            module_stack.len = stackidx;
            jl_rethrow();
        }
    }

    return (jl_value_t*)newm;
}

jl_value_t *jl_eval_dot_expr(jl_module_t *m, jl_value_t *x, jl_value_t *f, int fast)
{
    jl_value_t **args;
    JL_GC_PUSHARGS(args, 3);
    args[1] = jl_toplevel_eval_flex(m, x, fast, 0);
    args[2] = jl_toplevel_eval_flex(m, f, fast, 0);
    if (jl_is_module(args[1])) {
        JL_TYPECHK("getfield", symbol, args[2]);
        args[0] = jl_eval_global_var((jl_module_t*)args[1], (jl_sym_t*)args[2]);
    }
    else {
        args[0] = jl_eval_global_var(jl_base_relative_to(m), jl_symbol("getproperty"));
        args[0] = jl_apply(args, 3);
    }
    JL_GC_POP();
    return args[0];
}

// module referenced by (top ...) from within m
// this is only needed because of the bootstrapping process:
// - initially Base doesn't exist and top === Core
// - later, it refers to either old Base or new Base
JL_DLLEXPORT jl_module_t *jl_base_relative_to(jl_module_t *m)
{
    for (;;) {
        if (m->istopmod)
            return m;
        if (m == m->parent)
            break;
        m = m->parent;
    }
    return jl_top_module;
}

static void expr_attributes(jl_value_t *v, int *has_intrinsics, int *has_defs)
{
    if (!jl_is_expr(v))
        return;
    jl_expr_t *e = (jl_expr_t*)v;
    jl_sym_t *head = e->head;
    if (head == toplevel_sym || head == thunk_sym) {
        return;
    }
    else if (head == global_sym) {
        // this could be considered has_defs, but loops that assign to globals
        // might still need to be optimized.
        return;
    }
    else if (head == const_sym || head == copyast_sym) {
        // Note: `copyast` is included here since it indicates the presence of
        // `quote` and probably `eval`.
        *has_defs = 1;
        return;
    }
    else if (head == method_sym || head == abstracttype_sym || head == primtype_sym ||
             head == structtype_sym || jl_is_toplevel_only_expr(v)) {
        *has_defs = 1;
    }
    else if (head == cfunction_sym) {
        *has_intrinsics = 1;
        return;
    }
    else if (head == foreigncall_sym) {
        *has_intrinsics = 1;
        return;
    }
    else if (head == call_sym && jl_expr_nargs(e) > 0) {
        jl_value_t *called = NULL;
        jl_value_t *f = jl_exprarg(e, 0);
        if (jl_is_globalref(f)) {
            jl_module_t *mod = jl_globalref_mod(f);
            jl_sym_t *name = jl_globalref_name(f);
            if (jl_binding_resolved_p(mod, name)) {
                jl_binding_t *b = jl_get_binding(mod, name);
                if (b && b->value && b->constp)
                    called = b->value;
            }
        }
        else if (jl_is_quotenode(f)) {
            called = jl_quotenode_value(f);
        }
        if (called && jl_is_intrinsic(called) && jl_unbox_int32(called) == (int)llvmcall) {
            *has_intrinsics = 1;
            return;
        }
    }
    int i;
    for (i = 0; i < jl_array_len(e->args); i++) {
        jl_value_t *a = jl_exprarg(e, i);
        if (jl_is_expr(a))
            expr_attributes(a, has_intrinsics, has_defs);
    }
}

int jl_code_requires_compiler(jl_code_info_t *src)
{
    jl_array_t *body = src->code;
    assert(jl_typeis(body, jl_array_any_type));
    size_t i;
    int has_intrinsics = 0, has_defs = 0;
    for(i=0; i < jl_array_len(body); i++) {
        jl_value_t *stmt = jl_array_ptr_ref(body,i);
        expr_attributes(stmt, &has_intrinsics, &has_defs);
        if (has_intrinsics)
            return 1;
    }
    return 0;
}

static void body_attributes(jl_array_t *body, int *has_intrinsics, int *has_defs, int *has_loops)
{
    size_t i;
    *has_loops = 0;
    for(i=0; i < jl_array_len(body); i++) {
        jl_value_t *stmt = jl_array_ptr_ref(body,i);
        if (!*has_loops) {
            if (jl_is_gotonode(stmt)) {
                if (jl_gotonode_label(stmt) <= i)
                    *has_loops = 1;
            }
            else if (jl_is_expr(stmt)) {
                if (((jl_expr_t*)stmt)->head == goto_ifnot_sym) {
                    if (jl_unbox_long(jl_exprarg(stmt,1)) <= i)
                        *has_loops = 1;
                }
            }
        }
        expr_attributes(stmt, has_intrinsics, has_defs);
    }
}

static jl_module_t *call_require(jl_module_t *mod, jl_sym_t *var)
{
    static jl_value_t *require_func = NULL;
    static size_t require_world = 0;
    int build_mode = jl_generating_output();
    jl_module_t *m = NULL;
    jl_ptls_t ptls = jl_get_ptls_states();
    if (require_func == NULL && jl_base_module != NULL) {
        require_func = jl_get_global(jl_base_module, jl_symbol("require"));
        if (build_mode)
            require_world = ptls->world_age;
    }
    if (require_func != NULL) {
        size_t last_age = ptls->world_age;
        if (build_mode)
            ptls->world_age = require_world;
        jl_value_t *reqargs[3];
        reqargs[0] = require_func;
        reqargs[1] = (jl_value_t*)mod;
        reqargs[2] = (jl_value_t*)var;
        m = (jl_module_t*)jl_apply(reqargs, 3);
        if (build_mode)
            ptls->world_age = last_age;
    }
    if (m == NULL || !jl_is_module(m)) {
        jl_errorf("failed to load module %s", jl_symbol_name(var));
    }
    return m;
}

// either:
//   - sets *name and returns the module to import *name from
//   - sets *name to NULL and returns a module to import
static jl_module_t *eval_import_path(jl_module_t *where, jl_module_t *from, jl_array_t *args, jl_sym_t **name, const char *keyword)
{
    jl_sym_t *var = (jl_sym_t*)jl_array_ptr_ref(args, 0);
    size_t i = 1;
    jl_module_t *m = NULL;
    *name = NULL;
    if (!jl_is_symbol(var))
        jl_type_error(keyword, (jl_value_t*)jl_sym_type, (jl_value_t*)var);

    if (from != NULL) {
        m = from;
        i = 0;
    }
    else if (var != dot_sym) {
        // `A.B`: call the loader to obtain the root A in the current environment.
        if (jl_core_module && var == jl_core_module->name) {
            m = jl_core_module;
        }
        else if (jl_base_module && var == jl_base_module->name) {
            m = jl_base_module;
        }
        else {
            m = call_require(where, var);
        }
        if (i == jl_array_len(args))
            return m;
    }
    else {
        // `.A.B.C`: strip off leading dots by following parent links
        m = where;
        while (1) {
            if (i >= jl_array_len(args))
                jl_error("invalid module path");
            var = (jl_sym_t*)jl_array_ptr_ref(args, i);
            if (var != dot_sym)
                break;
            i++;
            m = m->parent;
        }
    }

    while (1) {
        var = (jl_sym_t*)jl_array_ptr_ref(args, i);
        if (!jl_is_symbol(var))
            jl_type_error(keyword, (jl_value_t*)jl_sym_type, (jl_value_t*)var);
        if (var == dot_sym)
            jl_errorf("invalid %s path: \".\" in identifier path", keyword);
        if (i == jl_array_len(args)-1)
            break;
        m = (jl_module_t*)jl_eval_global_var(m, var);
        if (!jl_is_module(m))
            jl_errorf("invalid %s path: \"%s\" does not name a module", keyword, jl_symbol_name(var));
        i++;
    }
    *name = var;
    return m;
}

int jl_is_toplevel_only_expr(jl_value_t *e)
{
    return jl_is_expr(e) &&
        (((jl_expr_t*)e)->head == module_sym ||
         ((jl_expr_t*)e)->head == import_sym ||
         ((jl_expr_t*)e)->head == using_sym ||
         ((jl_expr_t*)e)->head == export_sym ||
         ((jl_expr_t*)e)->head == thunk_sym ||
         ((jl_expr_t*)e)->head == global_sym ||
         ((jl_expr_t*)e)->head == toplevel_sym ||
         ((jl_expr_t*)e)->head == error_sym ||
         ((jl_expr_t*)e)->head == jl_incomplete_sym);
}

int jl_needs_lowering(jl_value_t *e) JL_NOTSAFEPOINT
{
    if (!jl_is_expr(e))
        return 0;
    jl_expr_t *ex = (jl_expr_t*)e;
    jl_sym_t *head = ex->head;
    if (head == module_sym || head == import_sym || head == using_sym ||
        head == export_sym || head == thunk_sym || head == toplevel_sym ||
        head == error_sym || head == jl_incomplete_sym || head == method_sym) {
        return 0;
    }
    if (head == global_sym) {
        size_t i, l = jl_array_len(ex->args);
        for (i = 0; i < l; i++) {
            jl_value_t *a = jl_exprarg(ex, i);
            if (!jl_is_symbol(a) && !jl_is_globalref(a))
                return 1;
        }
        return 0;
    }
    return 1;
}

void jl_resolve_globals_in_ir(jl_array_t *stmts, jl_module_t *m, jl_svec_t *sparam_vals,
                              int binding_effects);

static jl_method_instance_t *method_instance_for_thunk(jl_code_info_t *src, jl_module_t *module)
{
    jl_method_instance_t *li = jl_new_method_instance_uninit();
    li->inferred = (jl_value_t*)src;
    li->specTypes = (jl_value_t*)jl_emptytuple_type;
    li->def.module = module;
    return li;
}

static void import_module(jl_module_t *m, jl_module_t *import)
{
    jl_sym_t *name = import->name;
    jl_binding_t *b;
    if (jl_binding_resolved_p(m, name)) {
        b = jl_get_binding(m, name);
        if ((!b->constp && b->owner != m) || (b->value && b->value != (jl_value_t*)import)) {
            jl_errorf("importing %s into %s conflicts with an existing identifier",
                      jl_symbol_name(name), jl_symbol_name(m->name));
        }
    }
    else {
        b = jl_get_binding_wr(m, name, 1);
        b->imported = 1;
    }
    if (!b->constp) {
        b->value = (jl_value_t*)import;
        b->constp = 1;
        jl_gc_wb(m, (jl_value_t*)import);
    }
}

// in `import A.B: x, y, ...`, evaluate the `A.B` part if it exists
static jl_module_t *eval_import_from(jl_module_t *m, jl_expr_t *ex, const char *keyword)
{
    if (jl_expr_nargs(ex) == 1 && jl_is_expr(jl_exprarg(ex, 0))) {
        jl_expr_t *fr = (jl_expr_t*)jl_exprarg(ex, 0);
        if (fr->head == colon_sym) {
            if (jl_expr_nargs(fr) > 0 && jl_is_expr(jl_exprarg(fr, 0))) {
                jl_expr_t *path = (jl_expr_t*)jl_exprarg(fr, 0);
                if (((jl_expr_t*)path)->head == dot_sym) {
                    jl_sym_t *name = NULL;
                    jl_module_t *from = eval_import_path(m, NULL, path->args, &name, "import");
                    if (name != NULL) {
                        from = (jl_module_t*)jl_eval_global_var(from, name);
                        if (!jl_is_module(from))
                            jl_errorf("invalid %s path: \"%s\" does not name a module", keyword, jl_symbol_name(name));
                    }
                    return from;
                }
            }
            jl_errorf("malformed \"%s:\" expression", keyword);
        }
    }
    return NULL;
}

jl_value_t *jl_toplevel_eval_flex(jl_module_t *m, jl_value_t *e, int fast, int expanded)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (!jl_is_expr(e)) {
        if (jl_is_linenode(e)) {
            jl_lineno = jl_linenode_line(e);
            return jl_nothing;
        }
        if (jl_is_symbol(e)) {
            char *n = jl_symbol_name((jl_sym_t*)e);
            while (*n == '_') ++n;
            if (*n == 0)
                jl_error("all-underscore identifier used as rvalue");
        }
        return jl_interpret_toplevel_expr_in(m, e, NULL, NULL);
    }

    jl_expr_t *ex = (jl_expr_t*)e;

    if (ex->head == dot_sym) {
        if (jl_expr_nargs(ex) != 2)
            jl_error("syntax: malformed \".\" expression");
        jl_value_t *lhs = jl_exprarg(ex, 0);
        jl_value_t *rhs = jl_exprarg(ex, 1);
        // only handle `a.b` syntax here, so qualified names can be eval'd in pure contexts
        if (jl_is_quotenode(rhs) && jl_is_symbol(jl_fieldref(rhs,0)))
            return jl_eval_dot_expr(m, lhs, rhs, fast);
    }

    if (ptls->in_pure_callback) {
        jl_error("eval cannot be used in a generated function");
    }

    jl_method_instance_t *li = NULL;
    jl_code_info_t *thk = NULL;
    JL_GC_PUSH3(&li, &thk, &ex);

    if (!expanded && jl_needs_lowering(e)) {
        ex = (jl_expr_t*)jl_expand(e, m);
    }
    jl_sym_t *head = jl_is_expr(ex) ? ex->head : NULL;

    if (head == module_sym) {
        jl_value_t *val = jl_eval_module_expr(m, ex);
        JL_GC_POP();
        return val;
    }
    else if (head == using_sym) {
        size_t last_age = ptls->world_age;
        ptls->world_age = jl_world_counter;
        jl_sym_t *name = NULL;
        jl_module_t *from = eval_import_from(m, ex, "using");
        size_t i = 0;
        if (from) {
            i = 1;
            ex = (jl_expr_t*)jl_exprarg(ex, 0);
        }
        for (; i < jl_expr_nargs(ex); i++) {
            jl_value_t *a = jl_exprarg(ex, i);
            if (jl_is_expr(a) && ((jl_expr_t*)a)->head == dot_sym) {
                name = NULL;
                ptls->world_age = jl_world_counter;
                jl_module_t *import = eval_import_path(m, from, ((jl_expr_t*)a)->args, &name, "using");
                jl_module_t *u = import;
                if (name != NULL)
                    u = (jl_module_t*)jl_eval_global_var(import, name);
                if (from) {
                    // `using A: B` syntax
                    jl_module_use(m, import, name);
                }
                else {
                    if (!jl_is_module(u))
                        jl_errorf("invalid using path: \"%s\" does not name a module", jl_symbol_name(name));
                    // `using A.B` syntax
                    jl_module_using(m, u);
                    if (m == jl_main_module && name == NULL) {
                        // TODO: for now, `using A` in Main also creates an explicit binding for `A`
                        // This will possibly be extended to all modules.
                        import_module(m, u);
                    }
                }
            }
        }
        ptls->world_age = last_age;
        JL_GC_POP();
        return jl_nothing;
    }
    else if (head == import_sym) {
        size_t last_age = ptls->world_age;
        ptls->world_age = jl_world_counter;
        jl_sym_t *name = NULL;
        jl_module_t *from = eval_import_from(m, ex, "import");
        size_t i = 0;
        if (from) {
            i = 1;
            ex = (jl_expr_t*)jl_exprarg(ex, 0);
        }
        for (; i < jl_expr_nargs(ex); i++) {
            jl_value_t *a = jl_exprarg(ex, i);
            if (jl_is_expr(a) && ((jl_expr_t*)a)->head == dot_sym) {
                name = NULL;
                ptls->world_age = jl_world_counter;
                jl_module_t *import = eval_import_path(m, from, ((jl_expr_t*)a)->args, &name, "import");
                if (name == NULL) {
                    import_module(m, import);
                }
                else {
                    jl_module_import(m, import, name);
                }
            }
        }
        ptls->world_age = last_age;
        JL_GC_POP();
        return jl_nothing;
    }
    else if (head == export_sym) {
        for (size_t i = 0; i < jl_array_len(ex->args); i++) {
            jl_sym_t *name = (jl_sym_t*)jl_array_ptr_ref(ex->args, i);
            if (!jl_is_symbol(name))
                jl_error("syntax: malformed \"export\" statement");
            jl_module_export(m, name);
        }
        JL_GC_POP();
        return jl_nothing;
    }
    else if (head == global_sym) {
        // create uninitialized mutable binding for "global x" decl
        size_t i, l = jl_array_len(ex->args);
        for (i = 0; i < l; i++) {
            jl_value_t *arg = jl_exprarg(ex, i);
            jl_module_t *gm;
            jl_sym_t *gs;
            if (jl_is_globalref(arg)) {
                gm = jl_globalref_mod(arg);
                gs = jl_globalref_name(arg);
            }
            else {
                assert(jl_is_symbol(arg));
                gm = m;
                gs = (jl_sym_t*)arg;
            }
            jl_get_binding_wr(gm, gs, 0);
        }
        JL_GC_POP();
        return jl_nothing;
    }
    else if (head == toplevel_sym) {
        size_t last_age = ptls->world_age;
        jl_value_t *res = jl_nothing;
        int i;
        for (i = 0; i < jl_array_len(ex->args); i++) {
            ptls->world_age = jl_world_counter; // eval each statement in the newest world age
            res = jl_toplevel_eval_flex(m, jl_array_ptr_ref(ex->args, i), fast, 0);
        }
        ptls->world_age = last_age;
        JL_GC_POP();
        return res;
    }
    else if (head == error_sym || head == jl_incomplete_sym) {
        if (jl_expr_nargs(ex) == 0)
            jl_errorf("malformed \"%s\" expression", jl_symbol_name(head));
        if (jl_is_string(jl_exprarg(ex,0)))
            jl_errorf("syntax: %s", jl_string_data(jl_exprarg(ex,0)));
        jl_throw(jl_exprarg(ex,0));
    }
    else if (jl_is_symbol(ex)) {
        JL_GC_POP();
        return jl_eval_global_var(m, (jl_sym_t*)ex);
    }
    else if (head == NULL) {
        JL_GC_POP();
        return (jl_value_t*)ex;
    }

    int has_intrinsics = 0, has_defs = 0, has_loops = 0;
    assert(head == thunk_sym);
    thk = (jl_code_info_t*)jl_exprarg(ex,0);
    assert(jl_is_code_info(thk));
    assert(jl_typeis(thk->code, jl_array_any_type));
    body_attributes((jl_array_t*)thk->code, &has_intrinsics, &has_defs, &has_loops);

    jl_value_t *result;
    if (has_intrinsics || (!has_defs && fast && has_loops &&
                           jl_options.compile_enabled != JL_OPTIONS_COMPILE_OFF &&
                           jl_options.compile_enabled != JL_OPTIONS_COMPILE_MIN)) {
        // use codegen
        li = method_instance_for_thunk(thk, m);
        jl_resolve_globals_in_ir((jl_array_t*)thk->code, m, NULL, 0);
        // Don't infer blocks containing e.g. method definitions, since it's probably not
        // worthwhile and also unsound (see #24316).
        // TODO: This is still not correct since an `eval` can happen elsewhere, but it
        // helps in common cases.
        size_t last_age = ptls->world_age;
        size_t world = jl_world_counter;
        ptls->world_age = world;
        if (!has_defs) {
            jl_type_infer(&li, world, 0);
        }
        jl_value_t *dummy_f_arg = NULL;
        result = li->invoke(li, &dummy_f_arg, 1);
        ptls->world_age = last_age;
    }
    else {
        // use interpreter
        assert(thk);
        result = jl_interpret_toplevel_thunk(m, thk);
    }

    JL_GC_POP();
    return result;
}

JL_DLLEXPORT jl_value_t *jl_toplevel_eval(jl_module_t *m, jl_value_t *v)
{
    return jl_toplevel_eval_flex(m, v, 1, 0);
}

JL_DLLEXPORT jl_value_t *jl_infer_thunk(jl_code_info_t *thk, jl_module_t *m)
{
    jl_method_instance_t *li = method_instance_for_thunk(thk, m);
    JL_GC_PUSH1(&li);
    jl_resolve_globals_in_ir((jl_array_t*)thk->code, m, NULL, 0);
    jl_type_infer(&li, jl_get_ptls_states()->world_age, 0);
    JL_GC_POP();
    return li->rettype;
}

JL_DLLEXPORT jl_value_t *jl_load(jl_module_t *module, const char *fname)
{
    if (module->istopmod) {
        jl_printf(JL_STDOUT, "%s\r\n", fname);
#ifdef _OS_WINDOWS_
        jl_uv_flush(JL_STDOUT);
#endif
    }
    uv_stat_t stbuf;
    if (jl_stat(fname, (char*)&stbuf) != 0 || (stbuf.st_mode & S_IFMT) != S_IFREG) {
        jl_errorf("could not open file %s", fname);
    }
    return jl_parse_eval_all(fname, NULL, 0, module);
}

// load from filename given as a String object
JL_DLLEXPORT jl_value_t *jl_load_(jl_module_t *module, jl_value_t *str)
{
    // assume String has a hidden '\0' at the end
    return jl_load(module, (const char*)jl_string_data(str));
}

JL_DLLEXPORT jl_value_t *jl_prepend_cwd(jl_value_t *str)
{
    size_t sz = 1024;
    char path[1024];
    int c = uv_cwd(path, &sz);
    if (c < 0) {
        jl_errorf("could not get current directory");
    }
    path[sz] = '/';  // fix later with normpath if Windows
    const char *fstr = (const char*)jl_string_data(str);
    if (strlen(fstr) + sz >= 1024) {
        jl_errorf("use a bigger buffer for jl_fullpath");
    }
    strcpy(path + sz + 1, fstr);
    return jl_cstr_to_string(path);
}

#ifdef __cplusplus
}
#endif

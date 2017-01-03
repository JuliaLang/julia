// This file is a part of Julia. License is MIT: http://julialang.org/license

/*
  evaluating top-level expressions, loading source files
*/
#include "platform.h"

#include <stdlib.h>
#include <string.h>
#include <setjmp.h>
#include <assert.h>
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
    assert(jl_base_module != NULL);
    // using Base
    jl_module_using(m, jl_base_module);
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
    if (old_main) // don't block continued loading of incremental caches
        jl_main_module->uuid = old_main->uuid;
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
        if (f != NULL) {
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

extern void jl_get_system_hooks(void);
jl_value_t *jl_eval_module_expr(jl_expr_t *ex)
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
    jl_module_t *parent_module = ptls->current_module;
    jl_binding_t *b = jl_get_binding_wr(parent_module, name);
    jl_declare_constant(b);
    if (b->value != NULL) {
        if (!jl_is_module(b->value)) {
            jl_errorf("invalid redefinition of constant %s",
                      jl_symbol_name(name));
        }
        if (jl_generating_output()) {
            jl_errorf("cannot replace module %s during compilation",
                      jl_symbol_name(name));
        }
        jl_printf(JL_STDERR, "WARNING: replacing module %s\n",
                  jl_symbol_name(name));
    }
    jl_module_t *newm = jl_new_module(name);
    newm->parent = parent_module;
    b->value = (jl_value_t*)newm;
    jl_gc_wb_binding(b, newm);

    if (parent_module == jl_main_module && name == jl_symbol("Base")) {
        // pick up Base module during bootstrap
        jl_base_module = newm;
    }
    // export all modules from Main
    if (parent_module == jl_main_module)
        jl_module_export(jl_main_module, name);

    // add standard imports unless baremodule
    if (std_imports) {
        if (jl_base_module != NULL) {
            jl_add_standard_imports(newm);
        }
    }

    jl_value_t *defaultdefs = NULL, *form = NULL;
    JL_GC_PUSH3(&last_module, &defaultdefs, &form);
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
            defaultdefs = jl_call_scm_on_ast("module-default-defs", (jl_value_t*)ex);
            ptls->world_age = jl_world_counter;
            jl_toplevel_eval_flex(defaultdefs, 0, 1);
            defaultdefs = NULL;
        }

        for (int i = 0; i < jl_array_len(exprs); i++) {
            // process toplevel form
            ptls->world_age = jl_world_counter;
            form = jl_expand(jl_array_ptr_ref(exprs, i));
            ptls->world_age = jl_world_counter;
            (void)jl_toplevel_eval_flex(form, 1, 1);
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

    if (outermost == NULL || ptls->current_module == jl_main_module) {
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

// module referenced by (top ...) from within m
// this is only needed because of the bootstrapping process:
// - initially Base doesn't exist and top === Core
// - later, it refers to either old Base or new Base
JL_DLLEXPORT jl_module_t *jl_base_relative_to(jl_module_t *m)
{
    while (m != m->parent) {
        if (m->istopmod)
            return m;
        m = m->parent;
    }
    return jl_top_module;
}

int jl_has_intrinsics(jl_value_t *v)
{
    if (!jl_is_expr(v))
        return 0;
    jl_expr_t *e = (jl_expr_t*)v;
    if (e->head == toplevel_sym || e->head == copyast_sym)
        return 0;
    if (e->head == foreigncall_sym)
        return 1;
    int i;
    for (i = 0; i < jl_array_len(e->args); i++) {
        jl_value_t *a = jl_exprarg(e, i);
        if (jl_is_expr(a) && jl_has_intrinsics(a))
            return 1;
    }
    return 0;
}

// heuristic for whether a top-level input should be evaluated with
// the compiler or the interpreter.
static int jl_eval_with_compiler_p(jl_code_info_t *src, jl_array_t *body, int compileloops, jl_module_t *m)
{
    size_t i, maxlabl=0;
    // compile if there are backwards branches
    for(i=0; i < jl_array_len(body); i++) {
        jl_value_t *stmt = jl_array_ptr_ref(body,i);
        if (jl_is_labelnode(stmt)) {
            int l = jl_labelnode_label(stmt);
            if (l > maxlabl) maxlabl = l;
        }
    }
    size_t sz = (maxlabl+1+7)/8;
    char *labls = (char*)alloca(sz); memset(labls,0,sz);
    for(i=0; i < jl_array_len(body); i++) {
        jl_value_t *stmt = jl_array_ptr_ref(body,i);
        if (jl_is_labelnode(stmt)) {
            int l = jl_labelnode_label(stmt);
            labls[l/8] |= (1<<(l&7));
        }
        else if (compileloops && jl_is_gotonode(stmt)) {
            int l = jl_gotonode_label(stmt);
            if (labls[l/8]&(1<<(l&7))) {
                return 1;
            }
        }
        else if (jl_is_expr(stmt)) {
            if (compileloops && ((jl_expr_t*)stmt)->head==goto_ifnot_sym) {
                int l = jl_unbox_long(jl_exprarg(stmt,1));
                if (labls[l/8]&(1<<(l&7))) {
                    return 1;
                }
            }
        }
        if (jl_has_intrinsics(stmt))
            return 1;
    }
    return 0;
}

static int jl_eval_expr_with_compiler_p(jl_value_t *e, int compileloops, jl_module_t *m)
{
    if (jl_is_expr(e) && ((jl_expr_t*)e)->head == body_sym)
        return jl_eval_with_compiler_p(NULL, ((jl_expr_t*)e)->args, compileloops, m);
    if (jl_has_intrinsics(e))
        return 1;
    return 0;
}

static jl_value_t *require_func=NULL;

static jl_module_t *eval_import_path_(jl_array_t *args, int retrying)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    // in .A.B.C, first find a binding for A in the chain of module scopes
    // following parent links. then evaluate the rest of the path from there.
    // in A.B, look for A in Main first.
    jl_sym_t *var = (jl_sym_t*)jl_array_ptr_ref(args,0);
    size_t i=1;
    if (!jl_is_symbol(var)) jl_type_error("import or using", (jl_value_t*)jl_sym_type, (jl_value_t*)var);
    jl_module_t *m;

    if (var != dot_sym) {
        m = jl_main_module;
    }
    else {
        m = ptls->current_module;
        while (1) {
            var = (jl_sym_t*)jl_array_ptr_ref(args,i);
            if (!jl_is_symbol(var)) jl_type_error("import or using", (jl_value_t*)jl_sym_type, (jl_value_t*)var);
            i++;
            if (var != dot_sym) {
                if (i == jl_array_len(args))
                    return m;
                else
                    break;
            }
            m = m->parent;
        }
    }

    while (1) {
        if (jl_binding_resolved_p(m, var)) {
            jl_binding_t *mb = jl_get_binding(m, var);
            jl_module_t *m0 = m;
            int isimp = jl_is_imported(m, var);
            assert(mb != NULL);
            if (mb->owner == m0 || isimp) {
                m = (jl_module_t*)mb->value;
                if ((mb->owner == m0 && m != NULL && !jl_is_module(m)) ||
                    (isimp && (m == NULL || !jl_is_module(m))))
                    jl_errorf("invalid module path (%s does not name a module)",
                              jl_symbol_name(var));
                // If the binding has been resolved but is (1) undefined, and (2) owned
                // by the module we're importing into, then allow the import into the
                // undefined variable (by setting m back to m0).
                if (m == NULL)
                    m = m0;
                else
                    break;
            }
        }
        if (m == jl_main_module) {
            if (!retrying && i==1) { // (i==1) => no require() for relative imports
                if (require_func == NULL && jl_base_module != NULL)
                    require_func = jl_get_global(jl_base_module, jl_symbol("require"));
                if (require_func != NULL) {
                    jl_value_t *reqargs[2] = {require_func, (jl_value_t*)var};
                    jl_apply(reqargs, 2);
                    return eval_import_path_(args, 1);
                }
            }
        }
        if (retrying && require_func) {
            jl_printf(JL_STDERR, "WARNING: requiring \"%s\" in module \"%s\" did not define a corresponding module.\n", jl_symbol_name(var),
                      jl_symbol_name(ptls->current_module->name));
            return NULL;
        }
        else {
            jl_errorf("in module path: %s not defined", jl_symbol_name(var));
        }
    }

    for(; i < jl_array_len(args)-1; i++) {
        jl_value_t *s = jl_array_ptr_ref(args,i);
        assert(jl_is_symbol(s));
        m = (jl_module_t*)jl_eval_global_var(m, (jl_sym_t*)s);
        if (!jl_is_module(m))
            jl_errorf("invalid import statement");
    }
    return m;
}

static jl_module_t *eval_import_path(jl_array_t *args)
{
    return eval_import_path_(args, 0);
}

jl_value_t *jl_toplevel_eval_body(jl_array_t *stmts);

int jl_is_toplevel_only_expr(jl_value_t *e)
{
    return jl_is_expr(e) &&
        (((jl_expr_t*)e)->head == module_sym ||
         ((jl_expr_t*)e)->head == importall_sym ||
         ((jl_expr_t*)e)->head == import_sym ||
         ((jl_expr_t*)e)->head == using_sym ||
         ((jl_expr_t*)e)->head == export_sym ||
         ((jl_expr_t*)e)->head == thunk_sym ||
         ((jl_expr_t*)e)->head == toplevel_sym);
}

jl_value_t *jl_resolve_globals(jl_value_t *expr, jl_module_t *module, jl_svec_t *sparam_vals);
static jl_method_instance_t *jl_new_thunk(jl_code_info_t *src, jl_module_t *module)
{
    jl_method_instance_t *li = jl_new_method_instance_uninit();
    li->inferred = (jl_value_t*)src;
    li->specTypes = jl_typeof(jl_emptytuple);
    jl_array_t *stmts = (jl_array_t*)src->code;
    size_t i, l;
    for (i = 0, l = jl_array_len(stmts); i < l; i++) {
        jl_array_ptr_set(stmts, i, jl_resolve_globals(jl_array_ptr_ref(stmts, i), module, NULL));
    }
    return li;
}

jl_value_t *jl_toplevel_eval_flex(jl_value_t *e, int fast, int expanded)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    //jl_show(ex);
    //jl_printf(JL_STDOUT, "\n");
    if (!jl_is_expr(e)) {
        if (jl_is_linenode(e)) {
            jl_lineno = jl_linenode_line(e);
            return jl_nothing;
        }
        return jl_interpret_toplevel_expr(e);
    }

    jl_expr_t *ex = (jl_expr_t*)e;
    if (ex->head == error_sym || ex->head == jl_incomplete_sym) {
        // expression types simple enough not to need expansion
        return jl_interpret_toplevel_expr(e);
    }
    else if (ex->head == module_sym) {
        return jl_eval_module_expr(ex);
    }
    else if (ex->head == importall_sym) {
        jl_module_t *m = eval_import_path(ex->args);
        if (m==NULL) return jl_nothing;
        jl_sym_t *name = (jl_sym_t*)jl_array_ptr_ref(ex->args, jl_array_len(ex->args)-1);
        if (!jl_is_symbol(name))
            jl_error("syntax: malformed \"importall\" statement");
        m = (jl_module_t*)jl_eval_global_var(m, name);
        if (!jl_is_module(m))
            jl_errorf("invalid %s statement: name exists but does not refer to a module", jl_symbol_name(ex->head));
        jl_module_importall(ptls->current_module, m);
        return jl_nothing;
    }
    else if (ex->head == using_sym) {
        jl_module_t *m = eval_import_path(ex->args);
        if (m==NULL) return jl_nothing;
        jl_sym_t *name = (jl_sym_t*)jl_array_ptr_ref(ex->args, jl_array_len(ex->args)-1);
        if (!jl_is_symbol(name))
            jl_error("syntax: malformed \"using\" statement");
        jl_module_t *u = (jl_module_t*)jl_eval_global_var(m, name);
        if (jl_is_module(u)) {
            jl_module_using(ptls->current_module, u);
        }
        else {
            jl_module_use(ptls->current_module, m, name);
        }
        return jl_nothing;
    }
    else if (ex->head == import_sym) {
        jl_module_t *m = eval_import_path(ex->args);
        if (m==NULL) return jl_nothing;
        jl_sym_t *name = (jl_sym_t*)jl_array_ptr_ref(ex->args, jl_array_len(ex->args)-1);
        if (!jl_is_symbol(name))
            jl_error("syntax: malformed \"import\" statement");
        jl_module_import(ptls->current_module, m, name);
        return jl_nothing;
    }
    else if (ex->head == export_sym) {
        for(size_t i=0; i < jl_array_len(ex->args); i++) {
            jl_sym_t *name = (jl_sym_t*)jl_array_ptr_ref(ex->args, i);
            if (!jl_is_symbol(name))
                jl_error("syntax: malformed \"export\" statement");
            jl_module_export(ptls->current_module, name);
        }
        return jl_nothing;
    }
    else if (ex->head == line_sym) {
        jl_lineno = jl_unbox_long(jl_exprarg(ex,0));
        return jl_nothing;
    }

    jl_method_instance_t *li = NULL;
    jl_value_t *result;
    jl_code_info_t *thk = NULL;
    int ewc = 0;
    JL_GC_PUSH3(&li, &thk, &ex);

    if (!expanded && ex->head != body_sym && ex->head != thunk_sym && ex->head != return_sym &&
        ex->head != method_sym && ex->head != toplevel_sym) {
        // not yet expanded
        ex = (jl_expr_t*)jl_expand(e);
    }
    jl_sym_t *head = jl_is_expr(ex) ? ex->head : NULL;

    if (head == toplevel_sym) {
        size_t last_age = ptls->world_age;
        jl_value_t *res = jl_nothing;
        int i;
        for (i = 0; i < jl_array_len(ex->args); i++) {
            ptls->world_age = jl_world_counter; // eval each statement in the newest world age
            res = jl_toplevel_eval_flex(jl_array_ptr_ref(ex->args, i), fast, 0);
        }
        ptls->world_age = last_age;
        JL_GC_POP();
        return res;
    }

    if (head == thunk_sym) {
        thk = (jl_code_info_t*)jl_exprarg(ex,0);
        assert(jl_is_code_info(thk));
        assert(jl_typeis(thk->code, jl_array_any_type));
        ewc = jl_eval_with_compiler_p(thk, (jl_array_t*)thk->code, fast, ptls->current_module);
    }
    else {
        if (head && jl_eval_expr_with_compiler_p((jl_value_t*)ex, fast, ptls->current_module)) {
            thk = jl_wrap_expr((jl_value_t*)ex);
            ewc = 1;
        }
        else {
            if (head == body_sym) {
                result = jl_toplevel_eval_body(ex->args);
            }
            else if (jl_is_toplevel_only_expr((jl_value_t*)ex)) {
                result = jl_toplevel_eval((jl_value_t*)ex);
            }
            else {
                result = jl_interpret_toplevel_expr((jl_value_t*)ex);
            }
            JL_GC_POP();
            return result;
        }
    }

    if (ewc) {
        li = jl_new_thunk(thk, ptls->current_module);
        size_t world = jl_get_ptls_states()->world_age;
        jl_type_infer(&li, world, 0);
        jl_value_t *dummy_f_arg = NULL;
        result = jl_call_method_internal(li, &dummy_f_arg, 1);
    }
    else {
        result = jl_interpret_toplevel_thunk(thk);
    }
    JL_GC_POP();
    return result;
}

JL_DLLEXPORT jl_value_t *jl_toplevel_eval(jl_value_t *v)
{
    return jl_toplevel_eval_flex(v, 1, 0);
}

JL_DLLEXPORT jl_value_t *jl_load(const char *fname)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (ptls->current_module->istopmod) {
        jl_printf(JL_STDOUT, "%s\r\n", fname);
#ifdef _OS_WINDOWS_
        uv_run(uv_default_loop(), (uv_run_mode)1);
#endif
    }
    uv_stat_t stbuf;
    if (jl_stat(fname, (char*)&stbuf) != 0 || (stbuf.st_mode & S_IFMT) != S_IFREG) {
        jl_errorf("could not open file %s", fname);
    }
    return jl_parse_eval_all(fname, NULL, 0);
}

// load from filename given as a String object
JL_DLLEXPORT jl_value_t *jl_load_(jl_value_t *str)
{
    // assume String has a hidden '\0' at the end
    return jl_load((const char*)jl_string_data(str));
}

#ifdef __cplusplus
}
#endif

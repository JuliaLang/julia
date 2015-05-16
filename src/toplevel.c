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
DLLEXPORT int jl_lineno = 0;
// current file name
DLLEXPORT const char *jl_filename = "no file";

jl_module_t *jl_old_base_module = NULL;
// the Main we started with, in case it is switched
jl_module_t *jl_internal_main_module = NULL;

jl_value_t *jl_toplevel_eval_flex(jl_value_t *e, int fast);

void jl_add_standard_imports(jl_module_t *m)
{
    assert(jl_base_module != NULL);
    // using Base
    jl_module_using(m, jl_base_module);
    // importall Base.Operators
    jl_module_t *opmod = (jl_module_t*)jl_get_global(jl_base_module, jl_symbol("Operators"));
    if (opmod != NULL)
        jl_module_importall(m, opmod);
}

jl_module_t *jl_new_main_module(void)
{
    // switch to a new top-level module
    if (jl_current_module != jl_main_module && jl_current_module != NULL)
        jl_error("Main can only be replaced from the top level");

    jl_module_t *old_main = jl_main_module;

    jl_main_module = jl_new_module(jl_symbol("Main"));
    jl_main_module->parent = jl_main_module;
    jl_current_module = jl_main_module;

    jl_core_module->parent = jl_main_module;
    jl_set_const(jl_main_module, jl_symbol("Core"),
                 (jl_value_t*)jl_core_module);
    jl_set_global(jl_core_module, jl_symbol("Main"),
                  (jl_value_t*)jl_main_module);
    jl_current_task->current_module = jl_main_module;

    jl_module_import(jl_main_module, jl_core_module, jl_symbol("eval"));

    return old_main;
}

jl_array_t *jl_module_init_order = NULL;

// load time init procedure: in build mode, only record order
void jl_module_load_time_initialize(jl_module_t *m)
{
    int build_mode = (jl_options.build_path != NULL);
    if (build_mode) {
        if (jl_module_init_order == NULL)
            jl_module_init_order = jl_alloc_cell_1d(0);
        jl_cell_1d_push(jl_module_init_order, (jl_value_t*)m);
        jl_function_t *f = jl_module_get_initializer(m);
        if (f) jl_get_specialization(f, (jl_tupletype_t*)jl_typeof(jl_emptytuple));
    }
    else {
        jl_module_run_initializer(m);
    }
}

extern void jl_get_system_hooks(void);
jl_value_t *jl_eval_module_expr(jl_expr_t *ex)
{
    static arraylist_t module_stack;
    static int initialized=0;
    if (!initialized) {
        arraylist_new(&module_stack, 0);
        initialized = 1;
    }
    assert(ex->head == module_sym);
    jl_module_t *last_module = jl_current_module;
    if (jl_array_len(ex->args) != 3 || !jl_is_expr(jl_exprarg(ex,2))) {
        jl_error("syntax: malformed module expression");
    }
    int std_imports = (jl_exprarg(ex,0)==jl_true);
    jl_sym_t *name = (jl_sym_t*)jl_exprarg(ex, 1);
    if (!jl_is_symbol(name)) {
        jl_type_error("module", (jl_value_t*)jl_sym_type, (jl_value_t*)name);
    }
    jl_module_t *parent_module = jl_current_module;
    jl_binding_t *b = jl_get_binding_wr(parent_module, name);
    jl_declare_constant(b);
    if (b->value != NULL) {
        jl_printf(JL_STDERR, "Warning: replacing module %s\n", name->name);
    }
    jl_module_t *newm = jl_new_module(name);
    newm->parent = parent_module;
    b->value = (jl_value_t*)newm;

    gc_wb(parent_module, newm);

    if (parent_module == jl_main_module && name == jl_symbol("Base")) {
        // pick up Base module during bootstrap
        jl_old_base_module = jl_base_module;
        jl_base_module = newm;
        // reinitialize global variables
        // to pick up new types from Base
        jl_errorexception_type = NULL;
        jl_argumenterror_type = NULL;
        jl_typeerror_type = NULL;
        jl_methoderror_type = NULL;
        jl_loaderror_type = NULL;
        jl_current_task->tls = jl_nothing;
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

    JL_GC_PUSH1(&last_module);
    jl_module_t *task_last_m = jl_current_task->current_module;
    jl_current_task->current_module = jl_current_module = newm;

    jl_array_t *exprs = ((jl_expr_t*)jl_exprarg(ex, 2))->args;
    JL_TRY {
        for(int i=0; i < jl_array_len(exprs); i++) {
            // process toplevel form
            jl_value_t *form = jl_cellref(exprs, i);
            (void)jl_toplevel_eval_flex(form, 1);
        }
    }
    JL_CATCH {
        jl_current_module = last_module;
        jl_current_task->current_module = task_last_m;
        jl_rethrow();
    }
    JL_GC_POP();
    jl_current_module = last_module;
    jl_current_task->current_module = task_last_m;

#if 0
    // some optional post-processing steps
    size_t i;
    void **table = newm->bindings.table;
    for(i=1; i < newm->bindings.size; i+=2) {
        if (table[i] != HT_NOTFOUND) {
            jl_binding_t *b = (jl_binding_t*)table[i];
            // remove non-exported macros
            if (b->name->name[0]=='@' && !b->exportp && b->owner==newm)
                b->value = NULL;
            // error for unassigned exports
            /*
            if (b->exportp && b->owner==newm && b->value==NULL)
                jl_errorf("identifier %s exported from %s is not initialized",
                          b->name->name, newm->name->name);
            */
        }
    }
#endif

    arraylist_push(&module_stack, newm);

    if (jl_current_module == jl_main_module) {
        while (module_stack.len > 0) {
            jl_module_load_time_initialize((jl_module_t *) arraylist_pop(&module_stack));
        }
    }

    return jl_nothing;
}

static int is_intrinsic(jl_module_t *m, jl_sym_t *s)
{
    jl_value_t *v = jl_get_global(m, s);
    return (v != NULL && jl_typeof(v)==(jl_value_t*)jl_intrinsic_type);
}

// module referenced by TopNode from within m
// this is only needed because of the bootstrapping process:
// - initially Base doesn't exist and top === Core
// - later, it refers to either old Base or new Base
DLLEXPORT jl_module_t *jl_base_relative_to(jl_module_t *m)
{
    while (m != jl_main_module) {
        if (m->istopmod)
            return m;
        m = m->parent;
    }
    return jl_top_module;
}

int jl_has_intrinsics(jl_expr_t *e, jl_module_t *m)
{
    if (jl_array_len(e->args) == 0)
        return 0;
    if (e->head == static_typeof_sym) return 1;
    jl_value_t *e0 = jl_exprarg(e,0);
    if (e->head == call_sym &&
        ((jl_is_symbol(e0) && is_intrinsic(m,(jl_sym_t*)e0)) ||
         (jl_is_topnode(e0) && is_intrinsic(jl_base_relative_to(m),(jl_sym_t*)jl_fieldref(e0,0)))))
        return 1;
    int i;
    for(i=0; i < jl_array_len(e->args); i++) {
        jl_value_t *a = jl_exprarg(e,i);
        if (jl_is_expr(a) && jl_has_intrinsics((jl_expr_t*)a, m))
            return 1;
    }
    return 0;
}

// heuristic for whether a top-level input should be evaluated with
// the compiler or the interpreter.
int jl_eval_with_compiler_p(jl_expr_t *expr, int compileloops, jl_module_t *m)
{
    assert(jl_is_expr(expr));
    if (expr->head==body_sym && compileloops) {
        jl_array_t *body = expr->args;
        size_t i, maxlabl=0;
        // compile if there are backwards branches
        for(i=0; i < jl_array_len(body); i++) {
            jl_value_t *stmt = jl_cellref(body,i);
            if (jl_is_labelnode(stmt)) {
                int l = jl_labelnode_label(stmt);
                if (l > maxlabl) maxlabl = l;
            }
        }
        size_t sz = (maxlabl+1+7)/8;
        char *labls = (char*)alloca(sz); memset(labls,0,sz);
        for(i=0; i < jl_array_len(body); i++) {
            jl_value_t *stmt = jl_cellref(body,i);
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
                // to compile code that uses exceptions
                /*
                if (((jl_expr_t*)stmt)->head == enter_sym) {
                    return 1;
                }
                */
            }
        }
    }
    if (jl_has_intrinsics(expr, m)) return 1;
    return 0;
}

extern int jl_in_inference;

static jl_value_t *require_func=NULL;

static jl_module_t *eval_import_path_(jl_array_t *args, int retrying)
{
    // in .A.B.C, first find a binding for A in the chain of module scopes
    // following parent links. then evaluate the rest of the path from there.
    // in A.B, look for A in Main first.
    jl_sym_t *var = (jl_sym_t*)jl_cellref(args,0);
    size_t i=1;
    if (!jl_is_symbol(var)) jl_type_error("import or using", (jl_value_t*)jl_sym_type, (jl_value_t*)var);
    jl_module_t *m;

    if (var != dot_sym) {
        m = jl_main_module;
    }
    else {
        m = jl_current_module;
        while (1) {
            var = (jl_sym_t*)jl_cellref(args,i);
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
                    jl_errorf("invalid module path (%s does not name a module)", var->name);
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
                    jl_value_t *str = jl_cstr_to_string(var->name);
                    JL_GC_PUSH1(&str);
                    jl_apply((jl_function_t*)require_func, &str, 1);
                    JL_GC_POP();
                    return eval_import_path_(args, 1);
                }
            }
        }
        if (retrying && require_func) {
            jl_printf(JL_STDERR, "Warning: requiring \"%s\" did not define a corresponding module.\n", var->name);
            return NULL;
        }
        else {
            jl_errorf("in module path: %s not defined", var->name);
        }
    }

    for(; i < jl_array_len(args)-1; i++) {
        jl_value_t *s = jl_cellref(args,i);
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
         ((jl_expr_t*)e)->head == toplevel_sym);
}

jl_value_t *jl_toplevel_eval_flex(jl_value_t *e, int fast)
{
    //jl_show(ex);
    //jl_printf(JL_STDOUT, "\n");
    if (!jl_is_expr(e))
        return jl_interpret_toplevel_expr(e);

    jl_expr_t *ex = (jl_expr_t*)e;
    if (ex->head == null_sym || ex->head == error_sym) {
        // expression types simple enough not to need expansion
        return jl_interpret_toplevel_expr(e);
    }

    if (ex->head == module_sym) {
        return jl_eval_module_expr(ex);
    }

    // handle import, using, importall, export toplevel-only forms
    if (ex->head == importall_sym) {
        jl_module_t *m = eval_import_path(ex->args);
        if (m==NULL) return jl_nothing;
        jl_sym_t *name = (jl_sym_t*)jl_cellref(ex->args, jl_array_len(ex->args)-1);
        if (!jl_is_symbol(name))
            jl_error("syntax: malformed \"importall\" statement");
        m = (jl_module_t*)jl_eval_global_var(m, name);
        if (!jl_is_module(m))
            jl_errorf("invalid %s statement: name exists but does not refer to a module", ex->head->name);
        jl_module_importall(jl_current_module, m);
        return jl_nothing;
    }

    if (ex->head == using_sym) {
        jl_module_t *m = eval_import_path(ex->args);
        if (m==NULL) return jl_nothing;
        jl_sym_t *name = (jl_sym_t*)jl_cellref(ex->args, jl_array_len(ex->args)-1);
        if (!jl_is_symbol(name))
            jl_error("syntax: malformed \"using\" statement");
        jl_module_t *u = (jl_module_t*)jl_eval_global_var(m, name);
        if (jl_is_module(u)) {
            jl_module_using(jl_current_module, u);
        }
        else {
            jl_module_use(jl_current_module, m, name);
        }
        return jl_nothing;
    }

    if (ex->head == import_sym) {
        jl_module_t *m = eval_import_path(ex->args);
        if (m==NULL) return jl_nothing;
        jl_sym_t *name = (jl_sym_t*)jl_cellref(ex->args, jl_array_len(ex->args)-1);
        if (!jl_is_symbol(name))
            jl_error("syntax: malformed \"import\" statement");
        jl_module_import(jl_current_module, m, name);
        return jl_nothing;
    }

    if (ex->head == export_sym) {
        for(size_t i=0; i < jl_array_len(ex->args); i++) {
            jl_sym_t *name = (jl_sym_t*)jl_cellref(ex->args, i);
            if (!jl_is_symbol(name))
                jl_error("syntax: malformed \"export\" statement");
            jl_module_export(jl_current_module, name);
        }
        return jl_nothing;
    }

    if (ex->head == toplevel_sym) {
        int i=0; jl_value_t *res=jl_nothing;
        for(i=0; i < jl_array_len(ex->args); i++) {
            res = jl_toplevel_eval_flex(jl_cellref(ex->args, i), fast);
        }
        return res;
    }

    jl_value_t *thunk=NULL;
    jl_value_t *result;
    jl_lambda_info_t *thk=NULL;
    int ewc = 0;
    JL_GC_PUSH3(&thunk, &thk, &ex);

    if (ex->head != body_sym && ex->head != thunk_sym && ex->head != return_sym &&
        ex->head != method_sym) {
        // not yet expanded
        ex = (jl_expr_t*)jl_expand(e);
    }
    jl_sym_t *head = jl_is_expr(ex) ? ex->head : NULL;

    if (head == toplevel_sym) {
        int i=0; jl_value_t *res=jl_nothing;
        for(i=0; i < jl_array_len(ex->args); i++) {
            res = jl_toplevel_eval_flex(jl_cellref(ex->args, i), fast);
        }
        JL_GC_POP();
        return res;
    }

    if (head == thunk_sym) {
        thk = (jl_lambda_info_t*)jl_exprarg(ex,0);
        assert(jl_is_lambda_info(thk));
        assert(jl_is_expr(thk->ast));
        ewc = jl_eval_with_compiler_p(jl_lam_body((jl_expr_t*)thk->ast), fast, jl_current_module);
        if (!ewc) {
            if (jl_lam_vars_captured((jl_expr_t*)thk->ast)) {
                // interpreter doesn't handle closure environment
                ewc = 1;
            }
        }
    }
    else {
        if (head && jl_eval_with_compiler_p((jl_expr_t*)ex, fast, jl_current_module)) {
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
        thunk = (jl_value_t*)jl_new_closure(NULL, (jl_value_t*)jl_emptysvec, thk);
        if (!jl_in_inference) {
            jl_type_infer(thk, jl_tuple_type, thk);
        }
        result = jl_apply((jl_function_t*)thunk, NULL, 0);
    }
    else {
        result = jl_interpret_toplevel_thunk(thk);
    }
    JL_GC_POP();
    return result;
}

jl_value_t *jl_toplevel_eval(jl_value_t *v)
{
    return jl_toplevel_eval_flex(v, 1);
}

// repeatedly call jl_parse_next and eval everything
jl_value_t *jl_parse_eval_all(const char *fname, size_t len)
{
    //jl_printf(JL_STDERR, "***** loading %s\n", fname);
    int last_lineno = jl_lineno;
    const char *last_filename = jl_filename;
    jl_lineno = 0;
    jl_filename = fname;
    jl_value_t *fn=NULL, *ln=NULL, *form=NULL, *result=jl_nothing;
    JL_GC_PUSH4(&fn, &ln, &form, &result);
    JL_TRY {
        // handle syntax error
        while (1) {
            form = jl_parse_next();
            if (form == NULL)
                break;
            if (jl_is_expr(form)) {
                if (((jl_expr_t*)form)->head == jl_incomplete_sym) {
                    jl_errorf("syntax: %s", jl_string_data(jl_exprarg(form,0)));
                }
                if (((jl_expr_t*)form)->head == error_sym) {
                    jl_interpret_toplevel_expr(form);
                }
            }
            result = jl_toplevel_eval_flex(form, 1);
        }
    }
    JL_CATCH {
        jl_stop_parsing();
        fn = jl_pchar_to_string(fname, strlen(fname));
        ln = jl_box_long(jl_lineno);
        jl_lineno = last_lineno;
        jl_filename = last_filename;
        if (jl_loaderror_type == NULL) {
            jl_rethrow();
        }
        else {
            jl_rethrow_other(jl_new_struct(jl_loaderror_type, fn, ln,
                                           jl_exception_in_transit));
        }
    }
    jl_stop_parsing();
    jl_lineno = last_lineno;
    jl_filename = last_filename;
    JL_GC_POP();
    return result;
}

jl_value_t *jl_load(const char *fname)
{
    if (jl_current_module->istopmod) {
        jl_printf(JL_STDOUT, "%s\r\n", fname);
#ifdef _OS_WINDOWS_
        uv_run(uv_default_loop(), (uv_run_mode)1);
#endif
    }
    char *fpath = (char*)fname;
    uv_stat_t stbuf;
    if (jl_stat(fpath, (char*)&stbuf) != 0 || (stbuf.st_mode & S_IFMT) != S_IFREG) {
        jl_errorf("could not open file %s", fpath);
    }
    if (jl_start_parsing_file(fpath) != 0) {
        jl_errorf("could not open file %s", fpath);
    }
    jl_value_t *result = jl_parse_eval_all(fpath, strlen(fpath));
    if (fpath != fname) free(fpath);
    return result;
}

// load from filename given as a ByteString object
DLLEXPORT jl_value_t *jl_load_(jl_value_t *str)
{
    return jl_load(jl_string_data(str));
}

// type definition ------------------------------------------------------------

void jl_reinstantiate_inner_types(jl_datatype_t *t);

void jl_check_type_tuple(jl_value_t *t, jl_sym_t *name, const char *ctx)
{
    if (!jl_is_tuple_type(t))
        jl_type_error_rt(name->name, ctx, (jl_value_t*)jl_type_type, t);
}

void jl_set_datatype_super(jl_datatype_t *tt, jl_value_t *super)
{
    if (!jl_is_datatype(super) || !jl_is_abstracttype(super) ||
        tt->name == ((jl_datatype_t*)super)->name ||
        jl_subtype(super,(jl_value_t*)jl_vararg_type,0) ||
        jl_is_tuple_type(super) ||
        jl_subtype(super,(jl_value_t*)jl_type_type,0)) {
        jl_errorf("invalid subtyping in definition of %s",tt->name->name->name);
    }
    tt->super = (jl_datatype_t*)super;
    gc_wb(tt, tt->super);
    if (jl_svec_len(tt->parameters) > 0) {
        tt->name->cache = jl_emptysvec;
        tt->name->linearcache = jl_emptysvec;
        jl_reinstantiate_inner_types(tt);
    }
}

// method definition ----------------------------------------------------------

extern int jl_boot_file_loaded;

static int type_contains(jl_value_t *ty, jl_value_t *x);
static int svec_contains(jl_svec_t *svec, jl_value_t *x)
{
    assert(jl_is_svec(svec));
    size_t i, l=jl_svec_len(svec);
    for(i=0; i < l; i++) {
        jl_value_t *e = jl_svecref(svec, i);
        if (e==x || type_contains(e, x))
            return 1;
    }
    return 0;
}

static int type_contains(jl_value_t *ty, jl_value_t *x)
{
    if (ty == x) return 1;
    if (jl_is_uniontype(ty))
        return svec_contains((jl_svec_t*)jl_fieldref(ty,0), x);
    if (jl_is_datatype(ty))
        return svec_contains(((jl_datatype_t*)ty)->parameters, x);
    return 0;
}

void print_func_loc(JL_STREAM *s, jl_lambda_info_t *li);

DLLEXPORT jl_value_t *jl_method_def(jl_sym_t *name, jl_value_t **bp, jl_value_t *bp_owner,
                                    jl_binding_t *bnd,
                                    jl_svec_t *argdata, jl_function_t *f, jl_value_t *isstaged,
                                    jl_value_t *call_func, int iskw)
{
    // argdata is svec({types...}, svec(typevars...))
    jl_tupletype_t *argtypes = (jl_tupletype_t*)jl_svecref(argdata,0);
    jl_svec_t *tvars = (jl_svec_t*)jl_svecref(argdata,1);
    jl_value_t *gf=NULL;
    JL_GC_PUSH3(&gf, &tvars, &argtypes);

    if (bnd && bnd->value != NULL && !bnd->constp) {
        jl_errorf("cannot define function %s; it already has a value", bnd->name->name);
    }

    if (*bp != NULL) {
        gf = *bp;
        if (!jl_is_gf(gf)) {
            if (jl_is_datatype(gf)) {
                // DataType: define `call`, for backwards compat with outer constructors
                if (call_func == NULL)
                    call_func = (jl_value_t*)jl_module_call_func(jl_current_module);
                size_t na = jl_nparams(argtypes);
                jl_svec_t *newargtypes = jl_alloc_svec(1 + na);
                JL_GC_PUSH1(&newargtypes);
                size_t i=0;
                if (iskw) {
                    assert(na > 0);
                    // for kw sorter, keep container argument first
                    jl_svecset(newargtypes, 0, jl_tparam(argtypes, 0));
                    i++;
                }
                jl_svecset(newargtypes, i, jl_wrap_Type(gf));
                i++;
                for(; i < na+1; i++) {
                    jl_svecset(newargtypes, i, jl_tparam(argtypes, i-1));
                }
                argtypes = jl_apply_tuple_type(newargtypes);
                JL_GC_POP();
                gf = call_func;
                name = call_sym;
                // edit args, insert type first
                if (!jl_is_expr(f->linfo->ast)) {
                    f->linfo->ast = jl_uncompress_ast(f->linfo, f->linfo->ast);
                    gc_wb(f->linfo, f->linfo->ast);
                }
                jl_array_t *al = jl_lam_args((jl_expr_t*)f->linfo->ast);
                if (jl_array_len(al) == 0) {
                    al = jl_alloc_cell_1d(1);
                    jl_exprargset(f->linfo->ast, 0, (jl_value_t*)al);
                }
                else {
                    jl_array_grow_beg(al, 1);
                }
                if (iskw) {
                    jl_cellset(al, 0, jl_cellref(al, 1));
                    jl_cellset(al, 1, (jl_value_t*)jl_gensym());
                }
                else {
                    jl_cellset(al, 0, (jl_value_t*)jl_gensym());
                }
            }
            if (!jl_is_gf(gf)) {
                jl_errorf("cannot define function %s; it already has a value", name->name);
            }
        }
        if (iskw) {
            bp = (jl_value_t**)&((jl_methtable_t*)((jl_function_t*)gf)->env)->kwsorter;
            bp_owner = (jl_value_t*)((jl_function_t*)gf)->env;
            gf = *bp;
        }
    }

    // TODO
    size_t na = jl_nparams(argtypes);
    for(size_t i=0; i < na; i++) {
        jl_value_t *elt = jl_tparam(argtypes,i);
        if (!jl_is_type(elt) && !jl_is_typevar(elt)) {
            jl_lambda_info_t *li = f->linfo;
            jl_exceptionf(jl_argumenterror_type, "invalid type for argument %s in method definition for %s at %s:%d",
                          jl_lam_argname(li,i)->name, name->name, li->file->name, li->line);
        }
    }

    int ishidden = !!strchr(name->name, '#');
    for(size_t i=0; i < jl_svec_len(tvars); i++) {
        jl_value_t *tv = jl_svecref(tvars,i);
        if (!jl_is_typevar(tv))
            jl_type_error_rt(name->name, "method definition", (jl_value_t*)jl_tvar_type, tv);
        if (!ishidden && !type_contains((jl_value_t*)argtypes, tv)) {
            jl_printf(JL_STDERR, "Warning: static parameter %s does not occur in signature for %s",
                      ((jl_tvar_t*)tv)->name->name, name->name);
            print_func_loc(JL_STDERR, f->linfo);
            jl_printf(JL_STDERR, ".\nThe method will not be callable.\n");
        }
    }

    if (bnd) {
        bnd->constp = 1;
    }
    if (*bp == NULL) {
        gf = (jl_value_t*)jl_new_generic_function(name);
        *bp = gf;
        if (bp_owner) gc_wb(bp_owner, gf);
    }
    assert(jl_is_function(f));
    assert(jl_is_tuple_type(argtypes));
    assert(jl_is_svec(tvars));

    jl_add_method((jl_function_t*)gf, argtypes, f, tvars, isstaged == jl_true);
    if (jl_boot_file_loaded &&
        f->linfo && f->linfo->ast && jl_is_expr(f->linfo->ast)) {
        jl_lambda_info_t *li = f->linfo;
        li->ast = jl_compress_ast(li, li->ast);
        gc_wb(li, li->ast);
    }
    JL_GC_POP();
    return gf;
}

void jl_check_static_parameter_conflicts(jl_lambda_info_t *li, jl_svec_t *t, jl_sym_t *fname)
{
    jl_array_t *vinfo;
    size_t nvars;

    if (li->ast && jl_is_expr(li->ast)) {
        vinfo = jl_lam_vinfo((jl_expr_t*)li->ast);
        nvars = jl_array_len(vinfo);
        for(size_t i=0; i < jl_svec_len(t); i++) {
            for(size_t j=0; j < nvars; j++) {
                jl_value_t *tv = jl_svecref(t,i);
                if (jl_is_typevar(tv)) {
                    if ((jl_sym_t*)jl_cellref((jl_array_t*)jl_cellref(vinfo,j),0) ==
                        ((jl_tvar_t*)tv)->name) {
                        jl_printf(JL_STDERR,
                                  "Warning: local variable %s conflicts with a static parameter in %s",
                                  ((jl_tvar_t*)tv)->name->name, fname->name);
                        print_func_loc(JL_STDERR, li);
                        jl_printf(JL_STDERR, ".\n");
                    }
                }
            }
        }
    }
}

#ifdef __cplusplus
}
#endif

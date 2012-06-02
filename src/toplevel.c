/*
  evaluating top-level expressions, loading source files
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <setjmp.h>
#include <assert.h>
#include <sys/types.h>
#include <errno.h>
#include <fcntl.h>
#include <unistd.h>
#ifdef __WIN32__
#include <malloc.h>
#endif
#include <ctype.h>
#include <math.h>
#include "julia.h"
#include "builtin_proto.h"

DLLEXPORT char *julia_home = NULL;

jl_value_t *jl_toplevel_eval_flex(jl_value_t *e, int fast, int *plineno);

jl_value_t *jl_eval_module_expr(jl_expr_t *ex, int *plineno)
{
    assert(ex->head == module_sym);
    jl_module_t *last_module = jl_current_module;
    jl_sym_t *name = (jl_sym_t*)jl_exprarg(ex, 0);
    if (!jl_is_symbol(name)) {
        jl_type_error("module", (jl_value_t*)jl_sym_type, (jl_value_t*)name);
    }
    if (name == jl_current_module->name) {
        jl_errorf("module name %s conflicts with enclosing module", name->name);
    }
    jl_binding_t *b = jl_get_binding_wr(jl_current_module, name);
    jl_declare_constant(b);
    if (b->value != NULL) {
        JL_PRINTF(JL_STDERR, "Warning: redefinition of module %s ignored\n",
                   name->name);
        return jl_nothing;
    }
    jl_module_t *newm = jl_new_module(name);
    b->value = (jl_value_t*)newm;
    if (jl_current_module == jl_core_module && name == jl_symbol("Base")) {
        // pick up Base module during bootstrap, and stay within it
        // after loading.
        jl_base_module = last_module = newm;
    }
    JL_GC_PUSH(&last_module);
    jl_current_module = newm;

    jl_array_t *exprs = ((jl_expr_t*)jl_exprarg(ex, 1))->args;
    JL_TRY {
        for(int i=0; i < exprs->length; i++) {
            // process toplevel form
            jl_value_t *form = jl_cellref(exprs, i);
            if (jl_is_linenode(form)) {
                if (plineno)
                    *plineno = jl_linenode_line(form);
            }
            else {
                (void)jl_toplevel_eval_flex(form, 0, plineno);
            }
        }
    }
    JL_CATCH {
        JL_GC_POP();
        jl_current_module = last_module;
        jl_raise(jl_exception_in_transit);
    }
    JL_GC_POP();
    jl_current_module = last_module;
    return jl_nothing;
}

static int is_intrinsic(jl_sym_t *s)
{
    jl_value_t *v = jl_get_global(jl_current_module, s);
    return (v != NULL && jl_typeof(v)==(jl_type_t*)jl_intrinsic_type);
}

static int has_intrinsics(jl_expr_t *e)
{
    if (e->args->length == 0)
        return 0;
    if (e->head == static_typeof_sym) return 1;
    jl_value_t *e0 = jl_exprarg(e,0);
    if (e->head == call_sym &&
        ((jl_is_symbol(e0) && is_intrinsic((jl_sym_t*)e0)) ||
         (jl_is_topnode(e0) && is_intrinsic((jl_sym_t*)jl_fieldref(e0,0)))))
        return 1;
    int i;
    for(i=0; i < e->args->length; i++) {
        jl_value_t *a = jl_exprarg(e,i);
        if (jl_is_expr(a) && has_intrinsics((jl_expr_t*)a))
            return 1;
    }
    return 0;
}

// heuristic for whether a top-level input should be evaluated with
// the compiler or the interpreter.
static int eval_with_compiler_p(jl_expr_t *expr, int compileloops)
{
    assert(jl_is_expr(expr));
    if (expr->head==body_sym && compileloops) {
        jl_array_t *body = expr->args;
        size_t i, maxlabl=0;
        // compile if there are backwards branches
        for(i=0; i < body->length; i++) {
            jl_value_t *stmt = jl_cellref(body,i);
            if (jl_is_labelnode(stmt)) {
                int l = jl_labelnode_label(stmt);
                if (l > maxlabl) maxlabl = l;
            }
        }
        size_t sz = (maxlabl+1+7)/8;
        char *labls = alloca(sz); memset(labls,0,sz);
        for(i=0; i < body->length; i++) {
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
    if (has_intrinsics(expr)) return 1;
    return 0;
}

extern int jl_in_inference;

static jl_module_t *eval_import_path(jl_array_t *args)
{
    jl_module_t *m = jl_current_module;
    for(size_t i=0; i < args->length-1; i++) {
        jl_value_t *s = jl_cellref(args,i);
        assert(jl_is_symbol(s));
        m = (jl_module_t*)jl_eval_global_var(m, (jl_sym_t*)s);
        if (!jl_is_module(m))
            jl_errorf("invalid import statement");
    }
    return m;
}

jl_value_t *jl_toplevel_eval_flex(jl_value_t *e, int fast, int *plineno)
{
    //jl_show(ex);
    //JL_PRINTF(JL_STDOUT, "\n");
    if (!jl_is_expr(e))
        return jl_interpret_toplevel_expr(e);

    jl_expr_t *ex = (jl_expr_t*)e;
    if (ex->head == null_sym || ex->head == error_sym) {
        // expression types simple enough not to need expansion
        return jl_interpret_toplevel_expr(e);
    }

    if (ex->head == module_sym) {
        return jl_eval_module_expr(ex, plineno);
    }

    // handle import, export toplevel-only forms
    if (ex->head == importall_sym) {
        jl_module_t *m = eval_import_path(ex->args);
        jl_sym_t *name = (jl_sym_t*)jl_cellref(ex->args, ex->args->length-1);
        assert(jl_is_symbol(name));
        m = (jl_module_t*)jl_eval_global_var(m, name);
        if (!jl_is_module(m))
            jl_errorf("invalid import statement");
        jl_module_importall(jl_current_module, m);
        return jl_nothing;
    }

    if (ex->head == import_sym) {
        jl_module_t *m = eval_import_path(ex->args);
        jl_sym_t *name = (jl_sym_t*)jl_cellref(ex->args, ex->args->length-1);
        assert(jl_is_symbol(name));
        jl_module_import(jl_current_module, m, name);
        return jl_nothing;
    }

    // TODO: export

    jl_value_t *thunk=NULL;
    jl_value_t *result;
    jl_lambda_info_t *thk=NULL;
    int ewc = 0;
    JL_GC_PUSH(&thunk, &thk, &ex);

    if (ex->head != body_sym && ex->head != thunk_sym) {
        // not yet expanded
        ex = (jl_expr_t*)jl_expand(e);
    }

    if (jl_is_expr(ex) && ex->head == thunk_sym) {
        thk = (jl_lambda_info_t*)jl_exprarg(ex,0);
        assert(jl_is_lambda_info(thk));
        ewc = eval_with_compiler_p(jl_lam_body((jl_expr_t*)thk->ast), fast);
        if (!ewc) {
            jl_array_t *vinfos = jl_lam_vinfo((jl_expr_t*)thk->ast);
            int i;
            for(i=0; i < vinfos->length; i++) {
                if (jl_vinfo_capt((jl_array_t*)jl_cellref(vinfos,i))) {
                    // interpreter doesn't handle closure environment
                    ewc = 1;
                    break;
                }
            }
        }
    }
    else {
        if (jl_is_expr(ex) && eval_with_compiler_p((jl_expr_t*)ex, fast)) {
            thk = jl_wrap_expr((jl_value_t*)ex);
            ewc = 1;
        }
        else {
            result = jl_interpret_toplevel_expr((jl_value_t*)ex);
            JL_GC_POP();
            return result;
        }
    }

    if (ewc) {
        thunk = (jl_value_t*)jl_new_closure(NULL, (jl_value_t*)jl_null, thk);
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
    return jl_toplevel_eval_flex(v, 1, NULL);
}

// repeatedly call jl_parse_next and eval everything
void jl_parse_eval_all(char *fname)
{
    int lineno=0;
    jl_value_t *fn=NULL, *ln=NULL, *form=NULL;
    JL_GC_PUSH(&fn, &ln, &form);
    JL_TRY {
        jl_register_toplevel_eh();
        // handle syntax error
        while (1) {
            form = jl_parse_next(&lineno);
            if (form == NULL)
                break;
            if (jl_is_expr(form)) {
                if (((jl_expr_t*)form)->head == jl_continue_sym) {
                    jl_errorf("syntax error: %s", jl_string_data(jl_exprarg(form,0)));
                }
                if (((jl_expr_t*)form)->head == error_sym) {
                    jl_interpret_toplevel_expr(form);
                }
            }
            (void)jl_toplevel_eval_flex(form, 0, &lineno);
        }
    }
    JL_CATCH {
        jl_stop_parsing();
        fn = jl_pchar_to_string(fname, strlen(fname));
        ln = jl_box_long(lineno);
        jl_raise(jl_new_struct(jl_loaderror_type, fn, ln,
                               jl_exception_in_transit));
    }
    jl_stop_parsing();
    JL_GC_POP();
}

int asprintf(char **strp, const char *fmt, ...);

// fpath needs to be freed if != fname
char *jl_find_file_in_path(const char *fname)
{
    char *fpath = (char*)fname;
    int fid = open (fpath, O_RDONLY);
    // try adding julia home
    if (fid == -1 && julia_home && fname[0] != '/') {
        if (-1 != asprintf(&fpath, "%s/../lib/julia/%s", julia_home, fname))
            fid = open (fpath, O_RDONLY);
    }
    if (fid == -1) {
        if (fpath != fname) free(fpath);
        jl_errorf("could not open file %s", fname);
    }
    close(fid);

    return fpath;
}

void jl_load(const char *fname)
{
    char *fpath = jl_find_file_in_path(fname);
    jl_start_parsing_file(fpath);
    jl_parse_eval_all(fpath);
    if (fpath != fname) free(fpath);
}

// load from filename given as a ByteString object
DLLEXPORT void jl_load_(jl_value_t *str)
{
    jl_load(jl_string_data(str));
}

// type definition ------------------------------------------------------------

void jl_reinstantiate_inner_types(jl_tag_type_t *t);

void jl_check_type_tuple(jl_tuple_t *t, jl_sym_t *name, const char *ctx)
{
    size_t i;
    for(i=0; i < jl_tuple_len(t); i++) {
        jl_value_t *elt = jl_tupleref(t,i);
        if (!jl_is_type(elt) && !jl_is_typevar(elt)) {
            jl_type_error_rt(name->name, ctx, (jl_value_t*)jl_type_type, elt);
        }
    }
}

void jl_set_tag_type_super(jl_tag_type_t *tt, jl_value_t *super)
{
    if (!jl_is_tag_type(super) || super == (jl_value_t*)jl_undef_type ||
        jl_subtype(super,(jl_value_t*)jl_type_type,0)) {
        jl_errorf("invalid subtyping in definition of %s",tt->name->name->name);
    }
    tt->super = (jl_tag_type_t*)super;
    if (jl_tuple_len(tt->parameters) > 0) {
        tt->name->cache = jl_null;
        jl_reinstantiate_inner_types((jl_tag_type_t*)tt);
    }
}

// method definition ----------------------------------------------------------

extern int jl_boot_file_loaded;

jl_value_t *jl_method_def(jl_sym_t *name, jl_value_t **bp, jl_binding_t *bnd,
                          jl_tuple_t *argtypes, jl_function_t *f, jl_tuple_t *t)
{
    jl_value_t *gf;
    if (bnd) {
        jl_declare_constant(bnd);
    }
    if (*bp == NULL) {
        gf = (jl_value_t*)jl_new_generic_function(name);
        *bp = gf;
    }
    else {
        gf = *bp;
        if (!jl_is_gf(gf))
            jl_error("in method definition: not a generic function");
    }
    JL_GC_PUSH(&gf);
    assert(jl_is_function(f));
    assert(jl_is_tuple(argtypes));
    jl_check_type_tuple(argtypes, name, "method definition");
    jl_add_method((jl_function_t*)gf, argtypes, f, t);
    if (jl_boot_file_loaded &&
        f->linfo && f->linfo->ast && jl_is_expr(f->linfo->ast)) {
        jl_lambda_info_t *li = f->linfo;
        li->ast = jl_compress_ast(li, li->ast);
    }
    JL_GC_POP();
    return gf;
}

/*
  AST
  interface to front-end, obtains and translates syntax trees
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>
#ifdef _OS_WINDOWS_
#include <malloc.h>
#endif
#include "julia.h"
#include "julia_internal.h"
#include "flisp.h"

#ifdef __cplusplus
extern "C" {
#endif

// MSVC complains about "julia_flisp.boot.inc : error C4335: Mac file format
// detected: please convert the source file to either DOS or UNIX format"
#ifdef _MSC_VER
#pragma warning(disable:4335)
#endif

static uint8_t flisp_system_image[] = {
#include "julia_flisp.boot.inc"
};

extern fltype_t *iostreamtype;
static fltype_t *jvtype=NULL;

static value_t true_sym;
static value_t false_sym;
static value_t fl_error_sym;
static value_t fl_null_sym;

static jl_value_t *scm_to_julia(value_t e, int expronly);
static value_t julia_to_scm(jl_value_t *v);

value_t fl_defined_julia_global(value_t *args, uint32_t nargs)
{
    // tells whether a var is defined in and *by* the current module
    argcount("defined-julia-global", nargs, 1);
    (void)tosymbol(args[0], "defined-julia-global");
    if (jl_current_module == NULL)
        return FL_F;
    jl_sym_t *var = jl_symbol(symbol_name(args[0]));
    jl_binding_t *b =
        (jl_binding_t*)ptrhash_get(&jl_current_module->bindings, var);
    return (b != HT_NOTFOUND && b->owner==jl_current_module) ? FL_T : FL_F;
}

value_t fl_current_julia_module(value_t *args, uint32_t nargs)
{
    value_t opaque = cvalue(jvtype, sizeof(void*));
    *(jl_value_t**)cv_data((cvalue_t*)ptr(opaque)) = (jl_value_t*)jl_current_module;
    return opaque;
}

value_t fl_invoke_julia_macro(value_t *args, uint32_t nargs)
{
    if (nargs < 1)
        argcount("invoke-julia-macro", nargs, 1);
    jl_function_t *f = NULL;
    jl_value_t **margs;
    JL_GC_PUSHARGS(margs, nargs);
    int i;
    for(i=0; i < nargs; i++) margs[i] = NULL;
    for(i=1; i < nargs; i++) margs[i] = scm_to_julia(args[i], 1);
    jl_value_t *result = NULL;

    JL_TRY {
        margs[0] = scm_to_julia(args[0], 1);
        f = (jl_function_t*)jl_toplevel_eval(margs[0]);
        assert(jl_is_func(f));
        result = jl_apply(f, &margs[1], nargs-1);
    }
    JL_CATCH {
        JL_GC_POP();
        value_t opaque = cvalue(jvtype, sizeof(void*));
        *(jl_value_t**)cv_data((cvalue_t*)ptr(opaque)) = jl_exception_in_transit;
        return fl_list2(fl_error_sym, opaque);
    }
    // protect result from GC, otherwise it could be freed during future
    // macro expansions, since it will be referenced only from scheme and
    // not julia.
    // all calls to invoke-julia-macro happen under a single call to jl_expand,
    // so the preserved value stack is popped there.
    assert(result != NULL);
    jl_gc_preserve(result);
    value_t scm = julia_to_scm(result);
    fl_gc_handle(&scm);
    value_t scmresult;
    jl_module_t *defmod = f->linfo->module;
    if (defmod == jl_current_module) {
        scmresult = fl_cons(scm, FL_F);
    }
    else {
        value_t opaque = cvalue(jvtype, sizeof(void*));
        *(jl_value_t**)cv_data((cvalue_t*)ptr(opaque)) = (jl_value_t*)defmod;
        scmresult = fl_cons(scm, opaque);
    }
    fl_free_gc_handles(1);

    JL_GC_POP();
    return scmresult;
}

static builtinspec_t julia_flisp_ast_ext[] = {
    { "defined-julia-global", fl_defined_julia_global },
    { "invoke-julia-macro", fl_invoke_julia_macro },
    { "current-julia-module", fl_current_julia_module },
    { NULL, NULL }
};

extern int jl_parse_depwarn(int warn);

void jl_init_frontend(void)
{
    fl_init(4*1024*1024);
    value_t img = cvalue(iostreamtype, sizeof(ios_t));
    ios_t *pi = value2c(ios_t*, img);
    ios_static_buffer(pi, (char*)flisp_system_image, sizeof(flisp_system_image));

    if (fl_load_system_image(img)) {
        JL_PRINTF(JL_STDERR, "fatal error loading system image\n");
        jl_exit(1);
    }

    fl_applyn(0, symbol_value(symbol("__init_globals")));

    jvtype = define_opaque_type(symbol("julia_value"), sizeof(void*),
                                NULL, NULL);

    assign_global_builtins(julia_flisp_ast_ext);
    true_sym = symbol("true");
    false_sym = symbol("false");
    fl_error_sym = symbol("error");
    fl_null_sym = symbol("null");

    // Enable / disable syntax deprecation warnings
    jl_parse_depwarn((int)jl_compileropts.depwarn);
}

DLLEXPORT void jl_lisp_prompt(void)
{
    if (jvtype==NULL) jl_init_frontend();
    fl_applyn(1, symbol_value(symbol("__start")), fl_cons(FL_NIL,FL_NIL));
}

static jl_sym_t *scmsym_to_julia(value_t s)
{
    assert(issymbol(s));
    if (fl_isgensym(s)) {
        static char gsname[16];
        char *n = uint2str(&gsname[1], sizeof(gsname)-1,
                           ((gensym_t*)ptr(s))->id, 10);
        *(--n) = '#';
        return jl_symbol(n);
    }
    return jl_symbol(symbol_name(s));
}

static jl_value_t *scm_to_julia_(value_t e, int expronly);

static jl_value_t *full_list(value_t e, int expronly)
{
    size_t ln = llength(e);
    if (ln == 0) return jl_an_empty_cell;
    jl_array_t *ar = jl_alloc_cell_1d(ln);
    size_t i=0;
    while (iscons(e)) {
        jl_cellset(ar, i, scm_to_julia_(car_(e), expronly));
        e = cdr_(e);
        i++;
    }
    return (jl_value_t*)ar;
}

static jl_value_t *full_list_of_lists(value_t e, int expronly)
{
    size_t ln = llength(e);
    if (ln == 0) return jl_an_empty_cell;
    jl_array_t *ar = jl_alloc_cell_1d(ln);
    size_t i=0;
    while (iscons(e)) {
        jl_cellset(ar, i, full_list(car_(e),expronly));
        e = cdr_(e);
        i++;
    }
    return (jl_value_t*)ar;
}

static jl_value_t *scm_to_julia(value_t e, int expronly)
{
#ifdef JL_GC_MARKSWEEP
    int en = jl_gc_is_enabled();
    jl_gc_disable();
#endif
    jl_value_t *v;
    JL_TRY {
        v = scm_to_julia_(e, expronly);
    }
    JL_CATCH {
        // if expression cannot be converted, replace with error expr
        jl_expr_t *ex = jl_exprn(error_sym, 1);
        jl_cellset(ex->args, 0, jl_cstr_to_string("invalid AST"));
        v = (jl_value_t*)ex;
    }
#ifdef JL_GC_MARKSWEEP
    if (en) jl_gc_enable();
#endif
    return v;
}

extern int64_t conv_to_int64(void *data, numerictype_t tag);

static jl_value_t *scm_to_julia_(value_t e, int eo)
{
    if (fl_isnumber(e)) {
        int64_t i64;
        if (isfixnum(e)) {
            i64 = numval(e);
        }
        else {
            assert(iscprim(e));
            cprim_t *cp = (cprim_t*)ptr(e);
            numerictype_t nt = cp_numtype(cp);
            switch (nt) {
            case T_DOUBLE:
                return (jl_value_t*)jl_box_float64(*(double*)cp_data(cp));
            case T_FLOAT:
                return (jl_value_t*)jl_box_float32(*(float*)cp_data(cp));
            case T_UINT8:
                return (jl_value_t*)jl_box_uint8(*(uint8_t*)cp_data(cp));
            case T_UINT16:
                return (jl_value_t*)jl_box_uint16(*(uint16_t*)cp_data(cp));
            case T_UINT32:
                return (jl_value_t*)jl_box_uint32(*(uint32_t*)cp_data(cp));
            case T_UINT64:
                return (jl_value_t*)jl_box_uint64(*(uint64_t*)cp_data(cp));
            default:
                ;
            }
            i64 = conv_to_int64(cp_data(cp), nt);
        }
        if (
#ifdef _P64
            jl_compileropts.int_literals==32
#else
            jl_compileropts.int_literals!=64
#endif
            ) {
            if (i64 > (int64_t)S32_MAX || i64 < (int64_t)S32_MIN)
                return (jl_value_t*)jl_box_int64(i64);
            return (jl_value_t*)jl_box_int32((int32_t)i64);
        }
        else {
            return (jl_value_t*)jl_box_int64(i64);
        }
    }
    if (issymbol(e)) {
        if (e == true_sym)
            return jl_true;
        else if (e == false_sym)
            return jl_false;
        return (jl_value_t*)scmsym_to_julia(e);
    }
    if (fl_isstring(e)) {
        return jl_pchar_to_string((char*)cvalue_data(e), cvalue_len(e));
    }
    if (e == FL_F) {
        return jl_false;
    }
    if (e == FL_T) {
        return jl_true;
    }
    if (e == FL_NIL) {
        return (jl_value_t*)jl_null;
    }
    if (iscons(e)) {
        value_t hd = car_(e);
        if (issymbol(hd)) {
            jl_sym_t *sym = scmsym_to_julia(hd);
            /* tree node types:
               goto  gotoifnot  label  return
               lambda  call  =  quote
               null  top  method
               body  file new
               line  enter  leave
            */
            size_t n = llength(e)-1;
            size_t i;
            if (sym == null_sym && n == 0)
                return jl_nothing;
            if (sym == lambda_sym) {
                jl_expr_t *ex = jl_exprn(lambda_sym, n);
                e = cdr_(e);
                value_t largs = car_(e);
                jl_cellset(ex->args, 0, full_list(largs,eo));
                e = cdr_(e);

                value_t ee = car_(e);
                jl_array_t *vinf = jl_alloc_cell_1d(3);
                jl_cellset(vinf, 0, full_list(car_(ee),eo));
                ee = cdr_(ee);
                jl_cellset(vinf, 1, full_list_of_lists(car_(ee),eo));
                ee = cdr_(ee);
                jl_cellset(vinf, 2, full_list_of_lists(car_(ee),eo));
                assert(!iscons(cdr_(ee)));
                jl_cellset(ex->args, 1, vinf);
                e = cdr_(e);

                for(i=2; i < n; i++) {
                    assert(iscons(e));
                    jl_cellset(ex->args, i, scm_to_julia_(car_(e), eo));
                    e = cdr_(e);
                }
                return
                    (jl_value_t*)jl_new_lambda_info((jl_value_t*)ex, jl_null);
            }

            e = cdr_(e);
            if (!eo) {
                if (sym == line_sym && n==1) {
                    return jl_new_struct(jl_linenumbernode_type,
                                         scm_to_julia_(car_(e),0));
                }
                if (sym == label_sym) {
                    return jl_new_struct(jl_labelnode_type,
                                         scm_to_julia_(car_(e),0));
                }
                if (sym == goto_sym) {
                    return jl_new_struct(jl_gotonode_type,
                                         scm_to_julia_(car_(e),0));
                }
                if (sym == quote_sym) {
                    return jl_new_struct(jl_quotenode_type,
                                         scm_to_julia_(car_(e),0));
                }
                if (sym == top_sym) {
                    return jl_new_struct(jl_topnode_type,
                                         scm_to_julia_(car_(e),0));
                }
                if (sym == newvar_sym) {
                    return jl_new_struct(jl_newvarnode_type,
                                         scm_to_julia_(car_(e),0));
                }
            }
            jl_expr_t *ex = jl_exprn(sym, n);
            // allocate a fresh args array for empty exprs passed to macros
            if (eo && n == 0)
                ex->args = jl_alloc_cell_1d(0);
            for(i=0; i < n; i++) {
                assert(iscons(e));
                jl_cellset(ex->args, i, scm_to_julia_(car_(e),eo));
                e = cdr_(e);
            }
            return (jl_value_t*)ex;
        }
        else {
            jl_error("malformed tree");
        }
    }
    if (iscprim(e) && cp_class((cprim_t*)ptr(e))==wchartype) {
        jl_value_t *wc =
            jl_box32(jl_char_type, *(int32_t*)cp_data((cprim_t*)ptr(e)));
        return wc;
    }
    if (iscvalue(e) && cv_class((cvalue_t*)ptr(e)) == jvtype) {
        return *(jl_value_t**)cv_data((cvalue_t*)ptr(e));
    }
    jl_error("malformed tree");

    return (jl_value_t*)jl_null;
}

static value_t julia_to_scm_(jl_value_t *v);

static value_t julia_to_scm(jl_value_t *v)
{
    value_t temp;
    // need try/catch to reset GC handle stack in case of error
    FL_TRY_EXTERN {
        temp = julia_to_scm_(v);
    }
    FL_CATCH_EXTERN {
        temp = fl_list2(fl_error_sym, cvalue_static_cstring("expression too large"));
    }
    return temp;
}

static void array_to_list(jl_array_t *a, value_t *pv)
{
    if (jl_array_len(a) > 300000)
        lerror(MemoryError, "expression too large");
    value_t temp;
    for(long i=jl_array_len(a)-1; i >= 0; i--) {
        *pv = fl_cons(FL_NIL, *pv);
        temp = julia_to_scm_(jl_cellref(a,i));
        // note: must be separate statement
        car_(*pv) = temp;
    }
}

static value_t julia_to_list2(jl_value_t *a, jl_value_t *b)
{
    value_t sa = julia_to_scm_(a);
    fl_gc_handle(&sa);
    value_t sb = julia_to_scm_(b);
    value_t l = fl_list2(sa, sb);
    fl_free_gc_handles(1);
    return l;
}

static value_t julia_to_scm_(jl_value_t *v)
{
    if (jl_is_symbol(v)) {
        return symbol(((jl_sym_t*)v)->name);
    }
    if (v == jl_true) {
        return FL_T;
    }
    if (v == jl_false) {
        return FL_F;
    }
    if (v == jl_nothing) {
        return fl_cons(fl_null_sym, FL_NIL);
    }
    if (jl_is_expr(v)) {
        jl_expr_t *ex = (jl_expr_t*)v;
        value_t args = FL_NIL;
        fl_gc_handle(&args);
        array_to_list(ex->args, &args);
        value_t hd = julia_to_scm_((jl_value_t*)ex->head);
        value_t scmv = fl_cons(hd, args);
        fl_free_gc_handles(1);
        return scmv;
    }
    if (jl_typeis(v, jl_linenumbernode_type)) {
        return julia_to_list2((jl_value_t*)line_sym, jl_fieldref(v,0));
    }
    if (jl_typeis(v, jl_labelnode_type)) {
        return julia_to_list2((jl_value_t*)label_sym, jl_fieldref(v,0));
    }
    if (jl_typeis(v, jl_gotonode_type)) {
        return julia_to_list2((jl_value_t*)goto_sym, jl_fieldref(v,0));
    }
    if (jl_typeis(v, jl_quotenode_type)) {
        return julia_to_list2((jl_value_t*)quote_sym, jl_fieldref(v,0));
    }
    if (jl_typeis(v, jl_newvarnode_type)) {
        return julia_to_list2((jl_value_t*)newvar_sym, jl_fieldref(v,0));
    }
    if (jl_typeis(v, jl_topnode_type)) {
        return julia_to_list2((jl_value_t*)top_sym, jl_fieldref(v,0));
    }
    if (jl_is_long(v) && fits_fixnum(jl_unbox_long(v))) {
        return fixnum(jl_unbox_long(v));
    }
    value_t opaque = cvalue(jvtype, sizeof(void*));
    *(jl_value_t**)cv_data((cvalue_t*)ptr(opaque)) = v;
    return opaque;
}

// this is used to parse a line of repl input
DLLEXPORT jl_value_t *jl_parse_input_line(const char *str)
{
    value_t s = cvalue_static_cstring(str);
    value_t e = fl_applyn(1, symbol_value(symbol("jl-parse-string")), s);
    if (e == FL_EOF)
        return jl_nothing;

    return scm_to_julia(e,0);
}

// this is for parsing one expression out of a string, keeping track of
// the current position.
DLLEXPORT jl_value_t *jl_parse_string(const char *str, int pos0, int greedy)
{
    value_t s = cvalue_static_cstring(str);
    value_t p = fl_applyn(3, symbol_value(symbol("jl-parse-one-string")),
                          s, fixnum(pos0), greedy?FL_T:FL_F);
    jl_value_t *expr=NULL, *pos1=NULL;
    JL_GC_PUSH2(&expr, &pos1);

    value_t e = car_(p);
    if (e == FL_EOF) {
        expr = (jl_value_t*)jl_null;
    }
    else {
        expr = scm_to_julia(e,0);
    }

    pos1 = jl_box_long(tosize(cdr_(p),"parse"));
    jl_value_t *result = (jl_value_t*)jl_tuple2(expr, pos1);
    JL_GC_POP();
    return result;
}

int jl_start_parsing_file(const char *fname)
{
    value_t s = cvalue_static_cstring(fname);
    if (fl_applyn(1, symbol_value(symbol("jl-parse-file")), s) == FL_F)
        return 1;
    return 0;
}

void jl_stop_parsing(void)
{
    fl_applyn(0, symbol_value(symbol("jl-parser-close-stream")));
}

DLLEXPORT int jl_parse_depwarn(int warn)
{
    value_t prev = fl_applyn(1, symbol_value(symbol("jl-parser-depwarn")), warn? FL_T : FL_F);
    return prev == FL_T ? 1 : 0;
}

extern int jl_lineno;

jl_value_t *jl_parse_next(void)
{
    value_t c = fl_applyn(0, symbol_value(symbol("jl-parser-next")));
    if (c == FL_EOF)
        return NULL;
    if (iscons(c)) {
        if (cdr_(c) == FL_EOF)
            return NULL;
        value_t a = car_(c);
        if (isfixnum(a)) {
            jl_lineno = numval(a);
            //jl_printf(JL_STDERR, "  on line %d\n", jl_lineno);
            c = cdr_(c);
        }
    }
    // for error, get most recent line number
    if (iscons(c) && car_(c) == fl_error_sym)
        jl_lineno = numval(fl_applyn(0, symbol_value(symbol("jl-parser-current-lineno"))));
    return scm_to_julia(c,0);
}

jl_value_t *jl_load_file_string(const char *text, char *filename)
{
    value_t t, f;
    t = cvalue_static_cstring(text);
    fl_gc_handle(&t);
    f = cvalue_static_cstring(filename);
    fl_applyn(2, symbol_value(symbol("jl-parse-string-stream")),
              t, f);
    fl_free_gc_handles(1);
    return jl_parse_eval_all(filename);
}

// returns either an expression or a thunk
jl_value_t *jl_expand(jl_value_t *expr)
{
    int np = jl_gc_n_preserved_values();
    value_t arg = julia_to_scm(expr);
    value_t e = fl_applyn(1, symbol_value(symbol("jl-expand-to-thunk")), arg);
    jl_value_t *result = scm_to_julia(e,0);
    while (jl_gc_n_preserved_values() > np) {
        jl_gc_unpreserve();
    }
    return result;
}

DLLEXPORT jl_value_t *jl_macroexpand(jl_value_t *expr)
{
    int np = jl_gc_n_preserved_values();
    value_t arg = julia_to_scm(expr);
    value_t e = fl_applyn(1, symbol_value(symbol("jl-macroexpand")), arg);
    jl_value_t *result;
    result = scm_to_julia(e,0);
    while (jl_gc_n_preserved_values() > np) {
        jl_gc_unpreserve();
    }
    return result;
}

// wrap expr in a thunk AST
jl_lambda_info_t *jl_wrap_expr(jl_value_t *expr)
{
    // `(lambda () (() () ()) ,expr)
    jl_expr_t *le=NULL, *bo=NULL; jl_value_t *vi=NULL;
    jl_value_t *mt = jl_an_empty_cell;
    JL_GC_PUSH3(&le, &vi, &bo);
    le = jl_exprn(lambda_sym, 3);
    jl_cellset(le->args, 0, mt);
    vi = (jl_value_t*)jl_alloc_cell_1d(3);
    jl_cellset(vi, 0, mt);
    jl_cellset(vi, 1, mt);
    jl_cellset(vi, 2, mt);
    jl_cellset(le->args, 1, vi);
    if (!jl_is_expr(expr) || ((jl_expr_t*)expr)->head != body_sym) {
        bo = jl_exprn(body_sym, 1);
        jl_cellset(bo->args, 0, (jl_value_t*)jl_exprn(return_sym, 1));
        jl_cellset(((jl_expr_t*)jl_exprarg(bo,0))->args, 0, expr);
        expr = (jl_value_t*)bo;
    }
    jl_cellset(le->args, 2, expr);
    jl_lambda_info_t *li = jl_new_lambda_info((jl_value_t*)le, jl_null);
    JL_GC_POP();
    return li;
}

// syntax tree accessors

// get array of formal argument expressions
jl_array_t *jl_lam_args(jl_expr_t *l)
{
    assert(jl_is_expr(l));
    assert(l->head == lambda_sym);
    jl_value_t *ae = jl_exprarg(l,0);
    assert(jl_is_array(ae));
    return (jl_array_t*)ae;
}

jl_sym_t *jl_lam_argname(jl_lambda_info_t *li, int i)
{
    jl_expr_t *ast;
    if (jl_is_expr(li->ast))
        ast = (jl_expr_t*)li->ast;
    else
        ast = (jl_expr_t*)jl_uncompress_ast(li, li->ast);
    return (jl_sym_t*)jl_arrayref(jl_lam_args(ast),i);
}

// get array of local var symbols
jl_array_t *jl_lam_locals(jl_expr_t *l)
{
    assert(jl_is_expr(l));
    jl_value_t *le = jl_exprarg(l, 1);
    assert(jl_is_array(le));
    jl_value_t *ll = jl_cellref(le, 0);
    assert(jl_is_array(ll));
    return (jl_array_t*)ll;
}

// get array of var info records
jl_array_t *jl_lam_vinfo(jl_expr_t *l)
{
    assert(jl_is_expr(l));
    jl_value_t *le = jl_exprarg(l, 1);
    assert(jl_is_array(le));
    jl_value_t *ll = jl_cellref(le, 1);
    assert(jl_is_array(ll));
    return (jl_array_t*)ll;
}

// get array of var info records for captured vars
jl_array_t *jl_lam_capt(jl_expr_t *l)
{
    assert(jl_is_expr(l));
    jl_value_t *le = jl_exprarg(l, 1);
    assert(jl_is_array(le));
    jl_value_t *ll = jl_cellref(le, 2);
    assert(jl_is_array(ll));
    return (jl_array_t*)ll;
}

int jl_lam_vars_captured(jl_expr_t *ast)
{
    jl_array_t *vinfos = jl_lam_vinfo(ast);
    for(int i=0; i < jl_array_len(vinfos); i++) {
        if (jl_vinfo_capt((jl_array_t*)jl_cellref(vinfos,i))) {
            return 1;
        }
    }
    return 0;
}

// get array of body forms
jl_expr_t *jl_lam_body(jl_expr_t *l)
{
    assert(jl_is_expr(l));
    jl_value_t *be = jl_exprarg(l, 2);
    assert(jl_is_expr(be));
    assert(((jl_expr_t*)be)->head == body_sym);
    return (jl_expr_t*)be;
}

jl_sym_t *jl_decl_var(jl_value_t *ex)
{
    if (jl_is_symbol(ex)) return (jl_sym_t*)ex;
    assert(jl_is_expr(ex));
    return (jl_sym_t*)jl_exprarg(ex, 0);
}

int jl_is_rest_arg(jl_value_t *ex)
{
    if (!jl_is_expr(ex)) return 0;
    if (((jl_expr_t*)ex)->head != colons_sym) return 0;
    jl_expr_t *atype = (jl_expr_t*)jl_exprarg(ex,1);
    if (!jl_is_expr(atype)) return 0;
    if (atype->head != call_sym || jl_array_len(atype->args) != 3)
        return 0;
    if ((jl_sym_t*)jl_exprarg(atype,1) != dots_sym)
        return 0;
    return 1;
}

static jl_value_t *copy_ast(jl_value_t *expr, jl_tuple_t *sp, int do_sp)
{
    if (jl_is_symbol(expr)) {
        if (!do_sp) return expr;
        // pre-evaluate certain static parameters to help type inference
        for(int i=0; i < jl_tuple_len(sp); i+=2) {
            assert(jl_is_typevar(jl_tupleref(sp,i)));
            if ((jl_sym_t*)expr == ((jl_tvar_t*)jl_tupleref(sp,i))->name) {
                jl_value_t *spval = jl_tupleref(sp,i+1);
                if (jl_is_long(spval))
                    return spval;
            }
        }
    }
    else if (jl_is_lambda_info(expr)) {
        jl_lambda_info_t *li = (jl_lambda_info_t*)expr;
        /*
        if (sp == jl_null && li->ast &&
            jl_array_len(jl_lam_capt((jl_expr_t*)li->ast)) == 0)
            return expr;
        */
        // TODO: avoid if above condition is true and decls have already
        // been evaluated.
        JL_GC_PUSH1(&li);
        li = jl_add_static_parameters(li, sp);
        // inner lambda does not need the "def" link. it leads to excess object
        // retention, for example pointing to the original uncompressed AST
        // of a top-level thunk that gets type inferred.
        li->def = li;
        li->ast = jl_prepare_ast(li, li->sparams);
        JL_GC_POP();
        return (jl_value_t*)li;
    }
    else if (jl_typeis(expr,jl_array_any_type)) {
        jl_array_t *a = (jl_array_t*)expr;
        jl_array_t *na = jl_alloc_cell_1d(jl_array_len(a));
        JL_GC_PUSH1(&na);
        size_t i;
        for(i=0; i < jl_array_len(a); i++)
            jl_cellset(na, i, copy_ast(jl_cellref(a,i), sp, do_sp));
        JL_GC_POP();
        return (jl_value_t*)na;
    }
    else if (jl_is_expr(expr)) {
        jl_expr_t *e = (jl_expr_t*)expr;
        jl_expr_t *ne = jl_exprn(e->head, jl_array_len(e->args));
        JL_GC_PUSH1(&ne);
        if (e->head == lambda_sym) {
            jl_exprarg(ne, 0) = copy_ast(jl_exprarg(e,0), sp, 0);
            jl_exprarg(ne, 1) = copy_ast(jl_exprarg(e,1), sp, 0);
            jl_exprarg(ne, 2) = copy_ast(jl_exprarg(e,2), sp, 1);
        }
        else if (e->head == assign_sym) {
            jl_exprarg(ne, 0) = copy_ast(jl_exprarg(e,0), sp, 0);
            jl_exprarg(ne, 1) = copy_ast(jl_exprarg(e,1), sp, 1);
        }
        else {
            for(size_t i=0; i < jl_array_len(e->args); i++)
                jl_exprarg(ne, i) = copy_ast(jl_exprarg(e,i), sp, 1);
        }
        JL_GC_POP();
        return (jl_value_t*)ne;
    }
    return expr;
}

DLLEXPORT jl_value_t *jl_copy_ast(jl_value_t *expr)
{
    if (expr == NULL) {
        return NULL;
    }
    else if (jl_is_expr(expr)) {
        jl_expr_t *e = (jl_expr_t*)expr;
        size_t i, l = jl_array_len(e->args);
        jl_expr_t *ne = NULL;
        JL_GC_PUSH2(&ne, &expr);
        ne = jl_exprn(e->head, l);
        if (l == 0) {
            ne->args = jl_alloc_cell_1d(0);
        }
        else {
            for(i=0; i < l; i++)
                jl_exprarg(ne, i) = jl_copy_ast(jl_exprarg(e,i));
        }
        JL_GC_POP();
        return (jl_value_t*)ne;
    }
    else if (jl_typeis(expr,jl_array_any_type)) {
        jl_array_t *a = (jl_array_t*)expr;
        size_t i, l = jl_array_len(a);
        jl_array_t *na = NULL;
        JL_GC_PUSH2(&na, &expr);
        na = jl_alloc_cell_1d(l);
        for(i=0; i < l; i++)
            jl_cellset(na, i, jl_copy_ast(jl_cellref(a,i)));
        JL_GC_POP();
        return (jl_value_t*)na;
    }
    else if (jl_is_quotenode(expr)) {
        if (jl_is_symbol(jl_fieldref(expr,0)))
            return expr;
        jl_value_t *q = NULL;
        JL_GC_PUSH2(&q, &expr);
        q = jl_copy_ast(jl_fieldref(expr,0));
        jl_value_t *v = jl_new_struct(jl_quotenode_type, q);
        JL_GC_POP();
        return v;
    }
    return expr;
}

static jl_value_t *dont_copy_ast(jl_value_t *expr, jl_tuple_t *sp, int do_sp)
{
    if (jl_is_symbol(expr) || jl_is_lambda_info(expr)) {
        return copy_ast(expr, sp, do_sp);
    }
    else if (jl_is_expr(expr)) {
        jl_expr_t *e = (jl_expr_t*)expr;
        if (e->head == lambda_sym) {
            jl_exprarg(e, 0) = dont_copy_ast(jl_exprarg(e,0), sp, 0);
            jl_exprarg(e, 1) = dont_copy_ast(jl_exprarg(e,1), sp, 0);
            jl_exprarg(e, 2) = dont_copy_ast(jl_exprarg(e,2), sp, 1);
        }
        else if (e->head == assign_sym) {
            jl_exprarg(e, 0) = dont_copy_ast(jl_exprarg(e,0), sp, 0);
            jl_exprarg(e, 1) = dont_copy_ast(jl_exprarg(e,1), sp, 1);
        }
        else {
            for(size_t i=0; i < jl_array_len(e->args); i++)
                jl_exprarg(e, i) = dont_copy_ast(jl_exprarg(e,i), sp, 1);
        }
        return (jl_value_t*)e;
    }
    return expr;
}

// TODO: eval decl types for arguments of non-generic functions
static void eval_decl_types(jl_array_t *vi, jl_value_t *ast, jl_tuple_t *spenv)
{
    size_t i, l = jl_array_len(vi);
    for(i=0; i < l; i++) {
        jl_array_t *v = (jl_array_t*)jl_cellref(vi, i);
        assert(jl_array_len(v) > 1);
        jl_value_t *ty = jl_static_eval(jl_cellref(v,1), NULL, jl_current_module,
                                        (jl_value_t*)spenv, (jl_expr_t*)ast, 1, 1);
        if (ty != NULL && (jl_is_type(ty) || jl_is_typevar(ty))) {
            jl_cellref(v, 1) = ty;
        }
        else {
            jl_cellref(v, 1) = (jl_value_t*)jl_any_type;
        }
    }
}

jl_tuple_t *jl_tuple_tvars_to_symbols(jl_tuple_t *t)
{
    jl_tuple_t *s = jl_alloc_tuple_uninit(jl_tuple_len(t));
    size_t i;
    for(i=0; i < jl_tuple_len(s); i+=2) {
        assert(jl_is_typevar(jl_tupleref(t,i)));
        jl_tupleset(s, i,
                    (jl_value_t*)((jl_tvar_t*)jl_tupleref(t,i))->name);
        jl_tupleset(s, i+1, jl_tupleref(t,i+1));
    }
    return s;
}

// given a new lambda_info with static parameter values, make a copy
// of the tree with declared types evaluated and static parameters passed
// on to all enclosed functions.
// this tree can then be further mutated by optimization passes.
DLLEXPORT
jl_value_t *jl_prepare_ast(jl_lambda_info_t *li, jl_tuple_t *sparams)
{
    jl_tuple_t *spenv = NULL;
    jl_value_t *ast = li->ast;
    if (ast == NULL) return NULL;
    JL_GC_PUSH2(&spenv, &ast);
    spenv = jl_tuple_tvars_to_symbols(sparams);
    if (!jl_is_expr(ast)) {
        ast = jl_uncompress_ast(li, ast);
        ast = dont_copy_ast(ast, sparams, 1);
    }
    else {
        ast = copy_ast(ast, sparams, 1);
    }
    jl_module_t *last_m = jl_current_module;
    JL_TRY {
        jl_current_module = li->module;
        eval_decl_types(jl_lam_vinfo((jl_expr_t*)ast), ast, spenv);
        eval_decl_types(jl_lam_capt((jl_expr_t*)ast), ast, spenv);
    }
    JL_CATCH {
        jl_current_module = last_m;
        jl_rethrow();
    }
    jl_current_module = last_m;
    JL_GC_POP();
    return ast;
}

DLLEXPORT int jl_is_operator(char *sym) {
     return fl_applyn(1, symbol_value(symbol("operator?")), symbol(sym))
             == FL_T;
}

DLLEXPORT int jl_operator_precedence(char *sym) {
     return numval(fl_applyn(1, symbol_value(symbol("operator-precedence")),
                             symbol(sym)));
}

#ifdef __cplusplus
}
#endif

// This file is a part of Julia. License is MIT: http://julialang.org/license

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
#include <julia_flisp.boot.inc>
};

static fltype_t *jvtype=NULL;

static value_t true_sym;
static value_t false_sym;
static value_t fl_error_sym;
static value_t fl_null_sym;
static value_t fl_jlgensym_sym;

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
extern int jl_parse_deperror(int err);

void jl_init_frontend(void)
{
    fl_init(4*1024*1024);

    if (fl_load_system_image_str((char*)flisp_system_image,
                                 sizeof(flisp_system_image))) {
        jl_error("fatal error loading system image");
    }

    fl_applyn(0, symbol_value(symbol("__init_globals")));

    jvtype = define_opaque_type(symbol("julia_value"), sizeof(void*),
                                NULL, NULL);

    assign_global_builtins(julia_flisp_ast_ext);
    true_sym = symbol("true");
    false_sym = symbol("false");
    fl_error_sym = symbol("error");
    fl_null_sym = symbol("null");
    fl_jlgensym_sym = symbol("jlgensym");

    // Enable / disable syntax deprecation warnings
    // Disable in imaging mode to avoid i/o errors (#10727)
    if (jl_generating_output())
        jl_parse_depwarn(0);
    else if (jl_options.depwarn == JL_OPTIONS_DEPWARN_ERROR)
        jl_parse_deperror(1);
    else
        jl_parse_depwarn((int)jl_options.depwarn);
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

static jl_value_t *resolve_globals(jl_value_t *expr, jl_lambda_info_t *lam);

static jl_value_t *scm_to_julia(value_t e, int expronly)
{
    int en = jl_gc_enable(0);
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
    jl_gc_enable(en);
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
#ifdef _P64
        return (jl_value_t*)jl_box_int64(i64);
#else
        if (i64 > (int64_t)S32_MAX || i64 < (int64_t)S32_MIN)
            return (jl_value_t*)jl_box_int64(i64);
        else
            return (jl_value_t*)jl_box_int32((int32_t)i64);
#endif
    }
    if (issymbol(e)) {
        if (e == true_sym)
            return jl_true;
        else if (e == false_sym)
            return jl_false;
        return (jl_value_t*)scmsym_to_julia(e);
    }
    if (fl_isstring(e))
        return jl_pchar_to_string((char*)cvalue_data(e), cvalue_len(e));
    if (e == FL_F)
        return jl_false;
    if (e == FL_T)
        return jl_true;
    if (e == FL_NIL) {
        assert(0 && "Jeff doesn't think this is supposed to happen");
    }
    if (iscons(e)) {
        value_t hd = car_(e);
        if (hd == fl_jlgensym_sym) {
            size_t genid = numval(car_(cdr_(e)));
            return jl_box_gensym(genid);
        }
        if (hd == fl_null_sym && llength(e) == 1)
            return jl_nothing;
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
            if (sym == lambda_sym) {
                jl_expr_t *ex = jl_exprn(lambda_sym, n);
                e = cdr_(e);
                value_t largs = car_(e);
                jl_cellset(ex->args, 0, full_list(largs,eo));
                e = cdr_(e);

                value_t ee = car_(e);
                jl_array_t *vinf = jl_alloc_cell_1d(4);
                jl_cellset(vinf, 0, full_list_of_lists(car_(ee),eo));
                ee = cdr_(ee);
                jl_cellset(vinf, 1, full_list_of_lists(car_(ee),eo));
                ee = cdr_(ee);
                jl_cellset(vinf, 2, isfixnum(car_(ee)) ?
                           jl_box_long(numval(car_(ee))) :
                           full_list(car_(ee),eo));
                ee = cdr_(ee);
                jl_cellset(vinf, 3, full_list(car_(ee),eo));
                assert(!iscons(cdr_(ee)));
                jl_cellset(ex->args, 1, vinf);
                e = cdr_(e);

                for(i=2; i < n; i++) {
                    assert(iscons(e));
                    jl_cellset(ex->args, i, scm_to_julia_(car_(e), eo));
                    e = cdr_(e);
                }
                jl_lambda_info_t *nli = jl_new_lambda_info((jl_value_t*)ex, jl_emptysvec, jl_current_module);
                resolve_globals(nli->ast, nli);
                return (jl_value_t*)nli;
            }

            e = cdr_(e);
            if (!eo) {
                if (sym == line_sym && n==2) {
                    // NOTE: n==3 case exists: '(line, linenum, filename, funcname) passes
                    //       the original name through to keyword-arg specializations.
                    //       See 'line handling in julia-syntax.scm:keywords-method-def-expr
                    jl_value_t *filename = NULL, *linenum = NULL;
                    JL_GC_PUSH2(&filename, &linenum);
                    filename = scm_to_julia_(car_(cdr_(e)),0);
                    linenum  = scm_to_julia_(car_(e),0);
                    jl_value_t *temp = jl_new_struct(jl_linenumbernode_type,
                                                     filename, linenum);
                    JL_GC_POP();
                    return temp;
                }
                jl_value_t *scmv = NULL, *temp = NULL;
                JL_GC_PUSH1(&scmv);
                if (sym == label_sym) {
                    scmv = scm_to_julia_(car_(e),0);
                    temp = jl_new_struct(jl_labelnode_type, scmv);
                    JL_GC_POP();
                    return temp;
                }
                if (sym == goto_sym) {
                    scmv = scm_to_julia_(car_(e),0);
                    temp = jl_new_struct(jl_gotonode_type, scmv);
                    JL_GC_POP();
                    return temp;
                }
                if (sym == inert_sym || (sym == quote_sym && (!iscons(car_(e))))) {
                    scmv = scm_to_julia_(car_(e),0);
                    temp = jl_new_struct(jl_quotenode_type, scmv);
                    JL_GC_POP();
                    return temp;
                }
                if (sym == top_sym) {
                    scmv = scm_to_julia_(car_(e),0);
                    temp = jl_new_struct(jl_topnode_type, scmv);
                    JL_GC_POP();
                    return temp;
                }
                if (sym == newvar_sym) {
                    scmv = scm_to_julia_(car_(e),0);
                    temp = jl_new_struct(jl_newvarnode_type, scmv);
                    JL_GC_POP();
                    return temp;
                }
                JL_GC_POP();
            }
            else if (sym == inert_sym && !iscons(car_(e))) {
                sym = quote_sym;
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

    return jl_nothing;
}

static value_t julia_to_scm_(jl_value_t *v);
static arraylist_t jlgensym_to_flisp;

static value_t julia_to_scm(jl_value_t *v)
{
    value_t temp;
    if (jlgensym_to_flisp.len)
        jlgensym_to_flisp.len = 0; // in case we didn't free it last time we got here (for example, if we threw an error)
    else
        arraylist_new(&jlgensym_to_flisp, 0);
    // need try/catch to reset GC handle stack in case of error
    FL_TRY_EXTERN {
        temp = julia_to_scm_(v);
    }
    FL_CATCH_EXTERN {
        temp = fl_list2(fl_error_sym, cvalue_static_cstring("expression too large"));
    }
    arraylist_free(&jlgensym_to_flisp);
    return temp;
}

static void array_to_list(jl_array_t *a, value_t *pv)
{
    if (jl_array_len(a) > 300000)
        lerror(OutOfMemoryError, "expression too large");
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
    if (jl_is_symbol(v))
        return symbol(((jl_sym_t*)v)->name);
    if (jl_is_gensym(v)) {
        size_t idx = ((jl_gensym_t*)v)->id;
        size_t i;
        for (i = 0; i < jlgensym_to_flisp.len; i+=2) {
            if ((ssize_t)jlgensym_to_flisp.items[i] == idx)
                return fl_list2(fl_jlgensym_sym, fixnum((size_t)jlgensym_to_flisp.items[i+1]));
        }
        arraylist_push(&jlgensym_to_flisp, (void*)idx);
        value_t flv = fl_applyn(0, symbol_value(symbol("make-jlgensym")));
        assert(iscons(flv) && car_(flv) == fl_jlgensym_sym);
        arraylist_push(&jlgensym_to_flisp, (void*)(size_t)numval(car_(cdr_(flv))));
        return flv;
    }
    if (v == jl_true)
        return FL_T;
    if (v == jl_false)
        return FL_F;
    if (v == jl_nothing)
        return fl_cons(fl_null_sym, FL_NIL);
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
        // GC Note: jl_fieldref(v, 1) allocates but neither jl_fieldref(v, 0)
        //          or julia_to_list2 should allocate here
        value_t args = julia_to_list2(jl_fieldref(v,1), jl_fieldref(v,0));
        fl_gc_handle(&args);
        value_t hd = julia_to_scm_((jl_value_t*)line_sym);
        value_t scmv = fl_cons(hd, args);
        fl_free_gc_handles(1);
        return scmv;
    }
    // GC Note: jl_fieldref(v, 0) allocate for LabelNode, GotoNode
    //          but we don't need a GC root here because julia_to_list2
    //          shouldn't allocate in this case.
    if (jl_typeis(v, jl_labelnode_type))
        return julia_to_list2((jl_value_t*)label_sym, jl_fieldref(v,0));
    if (jl_typeis(v, jl_gotonode_type))
        return julia_to_list2((jl_value_t*)goto_sym, jl_fieldref(v,0));
    if (jl_typeis(v, jl_quotenode_type))
        return julia_to_list2((jl_value_t*)inert_sym, jl_fieldref(v,0));
    if (jl_typeis(v, jl_newvarnode_type))
        return julia_to_list2((jl_value_t*)newvar_sym, jl_fieldref(v,0));
    if (jl_typeis(v, jl_topnode_type))
        return julia_to_list2((jl_value_t*)top_sym, jl_fieldref(v,0));
    if (jl_is_long(v) && fits_fixnum(jl_unbox_long(v)))
        return fixnum(jl_unbox_long(v));
    value_t opaque = cvalue(jvtype, sizeof(void*));
    *(jl_value_t**)cv_data((cvalue_t*)ptr(opaque)) = v;
    return opaque;
}

// this is used to parse a line of repl input
DLLEXPORT jl_value_t *jl_parse_input_line(const char *str, size_t len)
{
    value_t s = cvalue_static_cstrn(str, len);
    value_t e = fl_applyn(1, symbol_value(symbol("jl-parse-string")), s);
    if (e == FL_EOF)
        return jl_nothing;
    return scm_to_julia(e,0);
}

// this is for parsing one expression out of a string, keeping track of
// the current position.
DLLEXPORT jl_value_t *jl_parse_string(const char *str, size_t len,
                                      int pos0, int greedy)
{
    value_t s = cvalue_static_cstrn(str, len);
    value_t p = fl_applyn(3, symbol_value(symbol("jl-parse-one-string")),
                          s, fixnum(pos0), greedy?FL_T:FL_F);
    jl_value_t *expr=NULL, *pos1=NULL;
    JL_GC_PUSH2(&expr, &pos1);

    value_t e = car_(p);
    if (e == FL_EOF)
        expr = jl_nothing;
    else
        expr = scm_to_julia(e,0);

    pos1 = jl_box_long(tosize(cdr_(p),"parse"));
    jl_value_t *result = (jl_value_t*)jl_svec2(expr, pos1);
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
    value_t prev = fl_applyn(1, symbol_value(symbol("jl-parser-depwarn")),
                             warn ? FL_T : FL_F);
    return prev == FL_T ? 1 : 0;
}

int jl_parse_deperror(int err)
{
    value_t prev = fl_applyn(1, symbol_value(symbol("jl-parser-deperror")),
                             err ? FL_T : FL_F);
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

jl_value_t *jl_load_file_string(const char *text, size_t len,
                                char *filename, size_t namelen)
{
    value_t t, f;
    t = cvalue_static_cstrn(text, len);
    fl_gc_handle(&t);
    f = cvalue_static_cstrn(filename, namelen);
    fl_applyn(2, symbol_value(symbol("jl-parse-string-stream")), t, f);
    fl_free_gc_handles(1);
    return jl_parse_eval_all(filename, namelen);
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

ssize_t jl_max_jlgensym_in(jl_value_t *v)
{
    ssize_t genid = -1;
    if (jl_is_gensym(v)) {
        genid = ((jl_gensym_t*)v)->id;
    }
    else if (jl_is_expr(v)) {
        jl_expr_t *e = (jl_expr_t*)v;
        size_t i, l = jl_array_len(e->args);
        for (i = 0; i < l; i++) {
            ssize_t maxid = jl_max_jlgensym_in(jl_exprarg(e, i));
            if (maxid > genid)
                genid = maxid;
        }
    }
    return genid;
}

// wrap expr in a thunk AST
jl_lambda_info_t *jl_wrap_expr(jl_value_t *expr)
{
    // `(lambda () (() () () ()) ,expr)
    jl_expr_t *le=NULL, *bo=NULL; jl_value_t *vi=NULL;
    jl_value_t *mt = jl_an_empty_cell;
    JL_GC_PUSH3(&le, &vi, &bo);
    le = jl_exprn(lambda_sym, 3);
    jl_cellset(le->args, 0, mt);
    vi = (jl_value_t*)jl_alloc_cell_1d(4);
    jl_cellset(vi, 0, mt);
    jl_cellset(vi, 1, mt);
    jl_cellset(vi, 2, jl_box_long(jl_max_jlgensym_in(expr)+1));
    jl_cellset(vi, 3, mt);
    jl_cellset(le->args, 1, vi);
    if (!jl_is_expr(expr) || ((jl_expr_t*)expr)->head != body_sym) {
        bo = jl_exprn(body_sym, 1);
        jl_cellset(bo->args, 0, (jl_value_t*)jl_exprn(return_sym, 1));
        jl_cellset(((jl_expr_t*)jl_exprarg(bo,0))->args, 0, expr);
        expr = (jl_value_t*)bo;
    }
    jl_cellset(le->args, 2, expr);
    jl_lambda_info_t *li = jl_new_lambda_info((jl_value_t*)le, jl_emptysvec, jl_current_module);
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
    // NOTE (gc root): `ast` is not rooted here, but jl_lam_args and jl_cellref
    // do not allocate.
    return (jl_sym_t*)jl_cellref(jl_lam_args(ast),i);
}

// get array of var info records (for args and locals)
jl_array_t *jl_lam_vinfo(jl_expr_t *l)
{
    assert(jl_is_expr(l));
    jl_value_t *le = jl_exprarg(l, 1);
    assert(jl_is_array(le));
    jl_value_t *ll = jl_cellref(le, 0);
    assert(jl_is_array(ll));
    return (jl_array_t*)ll;
}

// get array of var info records for captured vars
jl_array_t *jl_lam_capt(jl_expr_t *l)
{
    assert(jl_is_expr(l));
    jl_value_t *le = jl_exprarg(l, 1);
    assert(jl_is_array(le));
    jl_value_t *ll = jl_cellref(le, 1);
    assert(jl_is_array(ll));
    return (jl_array_t*)ll;
}

// get array of types for GenSym vars, or its length (if not type-inferred)
jl_value_t *jl_lam_gensyms(jl_expr_t *l)
{
    assert(jl_is_expr(l));
    jl_value_t *le = jl_exprarg(l, 1);
    assert(jl_is_array(le));
    assert(jl_array_len(le) == 4);
    return jl_cellref(le, 2);
}

// get array of static parameter symbols
jl_array_t *jl_lam_staticparams(jl_expr_t *l)
{
    assert(jl_is_expr(l));
    jl_value_t *le = jl_exprarg(l, 1);
    assert(jl_is_array(le));
    assert(jl_array_len(le) == 4);
    assert(jl_is_array(jl_cellref(le, 3)));
    return (jl_array_t*)jl_cellref(le, 3);
}

int jl_lam_vars_captured(jl_expr_t *ast)
{
    jl_array_t *vinfos = jl_lam_vinfo(ast);
    for(int i=0; i < jl_array_len(vinfos); i++) {
        if (jl_vinfo_capt((jl_array_t*)jl_cellref(vinfos,i)))
            return 1;
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
    if (((jl_expr_t*)atype)->head == dots_sym)
        return 1;
    if (atype->head != call_sym || jl_array_len(atype->args) < 3 || jl_array_len(atype->args) > 4)
        return 0;
    return ((jl_sym_t*)jl_exprarg(atype,1)) == vararg_sym;
}

static jl_value_t *copy_ast(jl_value_t *expr, jl_svec_t *sp, int do_sp)
{
    if (jl_is_symbol(expr)) {
        if (!do_sp) return expr;
        // pre-evaluate certain static parameters to help type inference
        for(int i=0; i < jl_svec_len(sp); i+=2) {
            assert(jl_is_typevar(jl_svecref(sp,i)));
            if ((jl_sym_t*)expr == ((jl_tvar_t*)jl_svecref(sp,i))->name) {
                jl_value_t *spval = jl_svecref(sp,i+1);
                if (jl_is_long(spval))
                    return spval;
            }
        }
    }
    else if (jl_is_lambda_info(expr)) {
        jl_lambda_info_t *li = (jl_lambda_info_t*)expr;
        if (sp == jl_emptysvec && li->ast &&
            (jl_is_expr(li->ast) ? jl_array_len(jl_lam_capt((jl_expr_t*)li->ast)) == 0 : li->capt == NULL)) {
            // share the inner function if the outer function has no sparams to insert
            if (!li->specTypes) {
                // if the decl values haven't been evaluated yet (or alternately already compiled by jl_trampoline), do so now
                li->ast = jl_prepare_ast(li, li->sparams);
                jl_gc_wb(li, li->ast);
                if (jl_array_len(jl_lam_staticparams(li->ast)) == 0)
                    // mark this as compilable; otherwise, will need to make an (un)specialized version of
                    // to handle all of the static parameters before compiling
                    li->specTypes = jl_anytuple_type; // no gc_wb needed
            }
            return expr;
        }
        JL_GC_PUSH1(&li);
        li = jl_add_static_parameters(li, sp, li->specTypes);
        // inner lambda does not need the "def" link. it leads to excess object
        // retention, for example pointing to the original uncompressed AST
        // of a top-level thunk that gets type inferred.
        li->def = li;
        li->ast = jl_prepare_ast(li, li->sparams);
        jl_gc_wb(li, li->ast);
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
            jl_exprargset(ne, 0, copy_ast(jl_exprarg(e,0), sp, 0));
            jl_exprargset(ne, 1, copy_ast(jl_exprarg(e,1), sp, 0));
            jl_exprargset(ne, 2, copy_ast(jl_exprarg(e,2), sp, 1));
        }
        else if (e->head == assign_sym) {
            jl_exprargset(ne, 0, copy_ast(jl_exprarg(e,0), sp, 0));
            jl_exprargset(ne, 1, copy_ast(jl_exprarg(e,1), sp, 1));
        }
        else {
            for(size_t i=0; i < jl_array_len(e->args); i++) {
                jl_exprargset(ne, i, copy_ast(jl_exprarg(e,i), sp, 1));
            }
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
            jl_gc_wb(ne, ne->args);
        }
        else {
            for(i=0; i < l; i++) {
                jl_exprargset(ne, i, jl_copy_ast(jl_exprarg(e,i)));
            }
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
        jl_value_t *v = jl_fieldref(expr,0);
        if (jl_is_symbol(v) || jl_is_gensym(v))
            return expr;
        jl_value_t *q = NULL;
        JL_GC_PUSH2(&q, &v);
        q = jl_copy_ast(v);
        v = jl_new_struct(jl_quotenode_type, q);
        JL_GC_POP();
        return v;
    }
    return expr;
}

static jl_value_t *dont_copy_ast(jl_value_t *expr, jl_svec_t *sp, int do_sp)
{
    if (jl_is_symbol(expr) || jl_is_lambda_info(expr)) {
        return copy_ast(expr, sp, do_sp);
    }
    else if (jl_is_expr(expr)) {
        jl_expr_t *e = (jl_expr_t*)expr;
        if (e->head == lambda_sym) {
            jl_exprargset(e, 0, dont_copy_ast(jl_exprarg(e,0), sp, 0));
            jl_exprargset(e, 1, dont_copy_ast(jl_exprarg(e,1), sp, 0));
            jl_exprargset(e, 2, dont_copy_ast(jl_exprarg(e,2), sp, 1));
        }
        else if (e->head == assign_sym) {
            jl_exprargset(e, 0, dont_copy_ast(jl_exprarg(e,0), sp, 0));
            jl_exprargset(e, 1, dont_copy_ast(jl_exprarg(e,1), sp, 1));
        }
        else {
            for(size_t i=0; i < jl_array_len(e->args); i++) {
                jl_exprargset(e, i, dont_copy_ast(jl_exprarg(e,i), sp, 1));
            }
        }
        return (jl_value_t*)e;
    }
    return expr;
}

// TODO: eval decl types for arguments of non-generic functions
static void eval_decl_types(jl_array_t *vi, jl_value_t *ast, jl_svec_t *spenv)
{
    size_t i, l = jl_array_len(vi);
    for(i=0; i < l; i++) {
        jl_array_t *v = (jl_array_t*)jl_cellref(vi, i);
        assert(jl_array_len(v) > 1);
        jl_value_t *ty = jl_static_eval(jl_cellref(v,1), NULL, jl_current_module,
                                        (jl_value_t*)spenv, (jl_expr_t*)ast, 1, 1);
        if (ty != NULL && (jl_is_type(ty) || jl_is_typevar(ty))) {
            jl_cellset(v, 1, ty);
        }
        else {
            jl_cellset(v, 1, (jl_value_t*)jl_any_type);
        }
    }
}

jl_svec_t *jl_svec_tvars_to_symbols(jl_svec_t *t)
{
    jl_svec_t *s = jl_alloc_svec_uninit(jl_svec_len(t));
    size_t i;
    for(i=0; i < jl_svec_len(s); i+=2) {
        assert(jl_is_typevar(jl_svecref(t,i)));
        jl_svecset(s, i, (jl_value_t*)((jl_tvar_t*)jl_svecref(t,i))->name);
        jl_svecset(s, i+1, jl_svecref(t,i+1));
    }
    return s;
}

// given a new lambda_info with static parameter values, make a copy
// of the tree with declared types evaluated and static parameters passed
// on to all enclosed functions.
// this tree can then be further mutated by optimization passes.
DLLEXPORT jl_value_t *jl_prepare_ast(jl_lambda_info_t *li, jl_svec_t *sparams)
{
    jl_svec_t *spenv = NULL;
    jl_value_t *ast = li->ast;
    if (ast == NULL) return NULL;
    JL_GC_PUSH2(&spenv, &ast);
    spenv = jl_svec_tvars_to_symbols(sparams);
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

DLLEXPORT int jl_is_operator(char *sym)
{
    return fl_applyn(1, symbol_value(symbol("operator?")), symbol(sym)) == FL_T;
}

DLLEXPORT int jl_operator_precedence(char *sym)
{
    return numval(fl_applyn(1, symbol_value(symbol("operator-precedence")), symbol(sym)));
}

jl_value_t *skip_meta(jl_array_t *body)
{
    jl_value_t *body1 = jl_cellref(body,0);
    if (jl_is_expr(body1) && ((jl_expr_t*)body1)->head == meta_sym
        && jl_array_len(body) > 1)
        body1 = jl_cellref(body,1);
    return body1;
}

int has_meta(jl_array_t *body, jl_sym_t *sym)
{
    size_t i, l = jl_array_len(body);
    for (i = 0; i < l; i++) {
        jl_expr_t *stmt = (jl_expr_t*)jl_cellref(body, i);
        if (jl_is_expr((jl_value_t*)stmt) && stmt->head == meta_sym) {
            size_t i, l = jl_array_len(stmt->args);
            for (i = 0; i < l; i++)
                if (jl_cellref(stmt->args, i) == (jl_value_t*)sym)
                    return 1;
        }
    }
    return 0;
}


int jl_in_vinfo_array(jl_array_t *a, jl_sym_t *v)
{
    size_t i, l=jl_array_len(a);
    for(i=0; i<l; i++) {
        if (jl_cellref(jl_cellref(a,i),0) == (jl_value_t*)v)
            return 1;
    }
    return 0;
}

int jl_in_sym_array(jl_array_t *a, jl_sym_t *v)
{
    size_t i, l=jl_array_len(a);
    for(i=0; i<l; i++) {
        if (jl_cellref(a,i) == (jl_value_t*)v)
            return 1;
    }
    return 0;
}

int jl_local_in_ast(jl_expr_t *ast, jl_sym_t *sym)
{
    return jl_in_vinfo_array(jl_lam_vinfo(ast), sym) ||
        jl_in_vinfo_array(jl_lam_capt(ast), sym) ||
        jl_in_sym_array(jl_lam_staticparams(ast), sym);
}

JL_CALLABLE(jl_f_get_field);

static jl_value_t *resolve_globals(jl_value_t *expr, jl_lambda_info_t *lam)
{
    if (jl_is_symbol(expr)) {
        if (lam->module == NULL)
            return expr;
        if (!jl_local_in_ast((jl_expr_t*)lam->ast, (jl_sym_t*)expr))
            return jl_module_globalref(lam->module, (jl_sym_t*)expr);
    }
    else if (jl_is_lambda_info(expr)) {
        jl_lambda_info_t *l = (jl_lambda_info_t*)expr;
        (void)resolve_globals(l->ast, l);
    }
    else if (jl_is_expr(expr)) {
        jl_expr_t *e = (jl_expr_t*)expr;
        if (e->head == lambda_sym) {
            (void)resolve_globals(jl_exprarg(e,2), lam);
        }
        else if (jl_is_toplevel_only_expr(expr) || e->head == const_sym || e->head == copyast_sym ||
                 e->head == global_sym || e->head == quote_sym || e->head == inert_sym ||
                 e->head == line_sym || e->head == meta_sym) {
        }
        else {
            if (e->head == call_sym && jl_expr_nargs(e) == 3 && jl_is_quotenode(jl_exprarg(e,2)) &&
                lam->module != NULL) {
                // replace getfield(module_expr, :sym) with GlobalRef
                jl_value_t *s = jl_fieldref(jl_exprarg(e,2),0);
                jl_value_t *fe = jl_exprarg(e,0);
                if (jl_is_symbol(s) && jl_is_topnode(fe)) {
                    jl_value_t *f = jl_static_eval(fe, NULL, lam->module,
                                                   NULL, (jl_expr_t*)lam->ast, 0, 0);
                    if (f && jl_is_func(f) && ((jl_function_t*)f)->fptr == &jl_f_get_field) {
                        jl_value_t *me = jl_exprarg(e,1);
                        if (jl_is_topnode(me) ||
                            (jl_is_symbol(me) && jl_binding_resolved_p(lam->module,(jl_sym_t*)me))) {
                            jl_value_t *m = jl_static_eval(me, NULL, lam->module,
                                                           NULL, (jl_expr_t*)lam->ast, 0, 0);
                            if (m && jl_is_module(m))
                                return jl_module_globalref((jl_module_t*)m, (jl_sym_t*)s);
                        }
                    }
                }
            }
            size_t i = 0;
            if (e->head == method_sym || e->head == abstracttype_sym || e->head == compositetype_sym ||
                e->head == bitstype_sym || e->head == macro_sym || e->head == module_sym)
                i++;
            for(; i < jl_array_len(e->args); i++) {
                jl_exprargset(e, i, resolve_globals(jl_exprarg(e,i), lam));
            }
        }
    }
    return expr;
}

#ifdef __cplusplus
}
#endif

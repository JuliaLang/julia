/*
  AST
  interface to front-end, obtains and translates syntax trees
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include <sys/types.h>
#include <limits.h>
#include <errno.h>
#include <math.h>
#ifndef NO_BOEHM_GC
#include <gc.h>
#endif
#include "llt.h"
#include "julia.h"

#define ___VERSION 405001
#include "gambit.h"

#include "jlfrontend.h"

// gambit boilerplate. just look at all those underscores...
#define SCHEME_LIBRARY_LINKER ____20_jlfrontend__
___BEGIN_C_LINKAGE
extern ___mod_or_lnk SCHEME_LIBRARY_LINKER (___global_state_struct*);
___END_C_LINKAGE

static htable_t gensym_table;

void jl_init_frontend()
{
    ___setup_params_struct setup_params;
    ___setup_params_reset (&setup_params);
    setup_params.version = ___VERSION;
    setup_params.linker  = SCHEME_LIBRARY_LINKER;
    ___setup (&setup_params);

    htable_new(&gensym_table, 0);
}

void jl_shutdown_frontend()
{
    ___cleanup();
}

static jl_sym_t *scmsym_to_julia(___SCMOBJ s)
{
    ___SCMOBJ ___temp;
    assert(___SYMBOLP(s));
    ___SCMOBJ str = ___VECTORREF(s,0);
    if (___NUMBERP(str)) {
        uptrint_t n = (uptrint_t)jl_scm_uint64(str)+100;
        void **bp = ptrhash_bp(&gensym_table, (void*)n);
        if (*bp == HT_NOTFOUND) {
            *bp = jl_gensym();
        }
        return (jl_sym_t*)*bp;
    }
    char *ss;
    ___SCMOBJ_to_CHARSTRING(str, &ss, 0);
    jl_sym_t *sym = jl_symbol(ss);
    ___release_rc(ss);
    return sym;
}

static char *scmsym_to_str(___SCMOBJ s)
{
    ___SCMOBJ ___temp;
    assert(___SYMBOLP(s));
    ___SCMOBJ str = ___VECTORREF(s,0);
    if (___NUMBERP(str)) {
        return NULL;
    }
    char *ss;
    ___SCMOBJ_to_CHARSTRING(str, &ss, 0);
    return ss;
}

static void syntax_error_check(___SCMOBJ e)
{
    ___SCMOBJ ___temp;
    if (___PAIRP(e)) {
        ___SCMOBJ hd = ___CAR(e);
        if (___SYMBOLP(hd)) {
            char *s;
            s = scmsym_to_str(hd);
            if (!strcmp(s,"error")) {
                ___release_rc(s);
                // note: this string should be released
                ___SCMOBJ_to_CHARSTRING(___CADR(e), &s, 0);
                jl_errorf("\nsyntax error: %s", s);
            }
            else {
                ___release_rc(s);
            }
        }
    }
}

static size_t scm_list_length(___SCMOBJ x)
{
    size_t l = 0;
    while (___PAIRP(x)) {
        l++;
        x = ___CDR(x);
    }
    return l;
}

static jl_value_t *scm_to_julia(___SCMOBJ e);

static jl_expr_t *full_list(___SCMOBJ e)
{
    jl_expr_t *ar = jl_exprn(list_sym, scm_list_length(e));
    size_t i=0;
    while (___PAIRP(e)) {
        jl_tupleset(ar->args, i, scm_to_julia(___CAR(e)));
        e = ___CDR(e);
        i++;
    }
    return ar;
}

static jl_value_t *scm_to_julia(___SCMOBJ e)
{
    ___SCMOBJ ___temp;
    if (___NUMBERP(e)) {
        if (jl_scm_integerp(e)) {
            uint64_t n = jl_scm_uint64(e);
            if (n > S64_MAX)
                return (jl_value_t*)jl_box_uint64(n);
            if (n > S32_MAX)
                return (jl_value_t*)jl_box_int64((int64_t)n);
            return (jl_value_t*)jl_box_int32((int32_t)n);
        }
        return (jl_value_t*)jl_box_float64(jl_scm_float64(e));
    }
    if (___SYMBOLP(e)) {
        jl_sym_t *sym = scmsym_to_julia(e);
        if (!strcmp(sym->name,"true"))
            return jl_true;
        else if (!strcmp(sym->name,"false"))
            return jl_false;
        return (jl_value_t*)sym;
    }
    if (___STRINGP(e)) {
        char *ss;
        ___SCMOBJ_to_CHARSTRING(e, &ss, 0);
        jl_value_t *v = (jl_value_t*)jl_cstr_to_array(ss);
        ___release_rc(ss);
        return v;
    }
    if (___BOOLEANP(e)) {
        if (___FALSEP(e))
            return jl_false;
        return jl_true;
    }
    if (___NULLP(e))
        return (jl_value_t*)jl_null;
    if (___PAIRP(e)) {
        ___SCMOBJ hd = ___CAR(e);
        if (___SYMBOLP(hd)) {
            jl_sym_t *sym = scmsym_to_julia(hd);
            char *s = sym->name;
            /* tree node types:
               goto  goto-ifnot  label  return
               lambda  call  =  quote
               null  top  unbound  box-unbound  closure-ref
               body  file  string
               line
            */
            size_t n = scm_list_length(e)-1;
            size_t i;
            if (sym == lambda_sym) {
                jl_expr_t *ex = jl_exprn(lambda_sym, n);
                ___SCMOBJ largs = ___CADR(e);
                jl_tupleset(ex->args, 0, (jl_value_t*)full_list(largs));
                e = ___CDR(___CDR(e));
                for(i=1; i < n; i++) {
                    assert(___PAIRP(e));
                    jl_tupleset(ex->args, i, scm_to_julia(___CAR(e)));
                    e = ___CDR(e);
                }
                return (jl_value_t*)
                    jl_expr(quote_sym, 1,
                            jl_new_lambda_info((jl_value_t*)ex, jl_null));
            }
            if (!strcmp(s, "var-info")) {
                jl_expr_t *ex = jl_exprn(sym, n);
                e = ___CDR(e);
                jl_tupleset(ex->args, 0, scm_to_julia(___CAR(e)));
                e = ___CDR(e);
                jl_tupleset(ex->args, 1, (jl_value_t*)full_list(___CAR(e)));
                e = ___CDR(e);
                jl_tupleset(ex->args, 2, (jl_value_t*)full_list(___CAR(e)));
                e = ___CDR(e);
                for(i=3; i < n; i++) {
                    assert(___PAIRP(e));
                    jl_tupleset(ex->args, i, scm_to_julia(___CAR(e)));
                    e = ___CDR(e);
                }
                return (jl_value_t*)ex;
            }
            jl_expr_t *ex = jl_exprn(sym, n);
            e = ___CDR(e);
            for(i=0; i < n; i++) {
                assert(___PAIRP(e));
                jl_tupleset(ex->args, i, scm_to_julia(___CAR(e)));
                e = ___CDR(e);
            }
            return (jl_value_t*)ex;
        }
        else {
            jl_error("malformed tree");
        }
    }
    jl_error("malformed tree");
    
    return (jl_value_t*)jl_null;
}

jl_value_t *jl_parse_input_line(const char *str)
{
    ___SCMOBJ e = jl_scm_parse_string(str);
    if (___BOOLEANP(e) || ___EOFP(e))
        return NULL;
    syntax_error_check(e);
    
    return scm_to_julia(e);
}

jl_value_t *jl_parse_file(const char *fname)
{
    ___SCMOBJ e = jl_scm_parse_file(fname);
    syntax_error_check(e);
    if (!___PAIRP(e))
        return (jl_value_t*)jl_null;
    return scm_to_julia(e);
}

// syntax tree accessors

// get array of formal argument expressions
jl_tuple_t *jl_lam_args(jl_expr_t *l)
{
    assert(l->head == lambda_sym);
    jl_value_t *ae = jl_exprarg(l,0);
    if (ae == (jl_value_t*)jl_null) return jl_null;
    assert(jl_is_expr(ae));
    assert(((jl_expr_t*)ae)->head == list_sym);
    return ((jl_expr_t*)ae)->args;
}

// get array of local var symbols
jl_tuple_t *jl_lam_locals(jl_expr_t *l)
{
    jl_value_t *le = jl_exprarg(l, 1);
    assert(jl_is_expr(le));
    jl_expr_t *lle = (jl_expr_t*)jl_exprarg(le,0);
    assert(jl_is_expr(lle));
    assert(lle->head == locals_sym);
    return lle->args;
}

// get array of body forms
jl_tuple_t *jl_lam_body(jl_expr_t *l)
{
    jl_value_t *be = jl_exprarg(l, 2);
    assert(jl_is_expr(be));
    assert(((jl_expr_t*)be)->head == body_sym);
    return ((jl_expr_t*)be)->args;
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
    if (atype->head != call_sym ||
        atype->args->length != 3)
        return 0;
    if ((jl_sym_t*)jl_exprarg(atype,1) != dots_sym)
        return 0;
    return 1;
}

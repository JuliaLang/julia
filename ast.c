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

void jl_init_frontend()
{
    ___setup_params_struct setup_params;
    ___setup_params_reset (&setup_params);
    setup_params.version = ___VERSION;
    setup_params.linker  = SCHEME_LIBRARY_LINKER;
    ___setup (&setup_params);
}

void jl_shutdown_frontend()
{
    ___cleanup();
}

static void syntax_error_check(___SCMOBJ e)
{
    ___SCMOBJ ___temp;
    if (___PAIRP(e)) {
        ___SCMOBJ hd = ___CAR(e);
        char *s;
        ___SCMOBJ_to_CHARSTRING(hd, &s, 0);
        if (___STRINGP(hd) && !strcmp(s,"error")) {
            ___release_rc(s);
            // note: this string should be released
            ___SCMOBJ_to_CHARSTRING(___CADR(___CADR(e)), &s, 0);
            jl_errorf("syntax error: %s", s);
        }
        else {
            ___release_rc(s);
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
    if (___STRINGP(e)) {
        char *s;
        ___SCMOBJ_to_CHARSTRING(e, &s, 0);
        jl_value_t *v;
        if (!strcmp(s,"true"))
            v = (jl_value_t*)jl_true;
        else if (!strcmp(s,"false"))
            v = (jl_value_t*)jl_false;
        else
            v = (jl_value_t*)jl_symbol(s);
        ___release_rc(s);
        return v;
    }
    if (___NULLP(e))
        return (jl_value_t*)jl_null;
    if (___PAIRP(e)) {
        ___SCMOBJ hd = ___CAR(e);
        if (___STRINGP(hd)) {
            char *s;
            ___SCMOBJ_to_CHARSTRING(hd, &s, 0);
            /* tree node types:
               goto  goto-ifnot  label  return
               lambda  call  =  quote
               null  top  unbound  box-unbound  closure-ref
               body  file  string
               line
            */
            jl_value_t *v;
            if (!strcmp(s, "string")) {
                char *ss;
                ___SCMOBJ_to_CHARSTRING(___CADR(e), &ss, 0);
                v = (jl_value_t*)jl_cstr_to_buffer(ss);
                ___release_rc(ss);
            }
            else {
                size_t n = scm_list_length(e)-1;
                size_t i;
                jl_expr_t *ex = jl_exprn(jl_symbol(s), n);
                e = ___CDR(e);
                for(i=0; i < n; i++) {
                    assert(___PAIRP(e));
                    ((jl_value_t**)ex->args->data)[i] = scm_to_julia(___CAR(e));
                    e = ___CDR(e);
                }
                if (!strcmp(s, "lambda")) {
                    v = (jl_value_t*)
                        jl_expr(jl_symbol("quote"), 1,
                                jl_new_lambda_info((jl_value_t*)ex, jl_null));
                }
                else {
                    v = (jl_value_t*)ex;
                }
            }
            ___release_rc(s);
            return v;
        }
        else {
            jl_error("malformed tree");
        }
    }
    jl_error("malformed tree");
    
    return (jl_value_t*)jl_null;
}

jl_value_t *jl_parse_input_line(char *str)
{
    ___SCMOBJ e = jl_scm_parse_string(str);
    if (___BOOLEANP(e) || ___EOFP(e))
        return NULL;
    syntax_error_check(e);
    
    return scm_to_julia(e);
}

jl_value_t *jl_parse_file(char *fname)
{
    ___SCMOBJ e = jl_scm_parse_file(fname);
    syntax_error_check(e);
    if (!___PAIRP(e))
        return (jl_value_t*)jl_null;
    return scm_to_julia(e);
}

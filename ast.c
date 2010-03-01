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
#include <gc.h>
#include "llt.h"
#include "julia.h"

#ifdef linux
#define ___VERSION 405001
#else
#define ___VERSION 406000
#endif
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
        if (___STRINGP(hd) && !strcmp(jl_scm_str(hd),"error")) {
            jl_errorf("syntax error: %s", jl_scm_str(___CADR(___CADR(e))));
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
    if (___STRINGP(e))
        return (jl_value_t*)jl_symbol(jl_scm_str(e));
    if (___NULLP(e))
        return (jl_value_t*)jl_null;
    if (___PAIRP(e)) {
        ___SCMOBJ hd = ___CAR(e);
        if (___STRINGP(hd)) {
            char *s = jl_scm_str(hd);
            /* tree node types:
               goto  goto-ifnot  label  return
               lambda  call  =  quote
               null  top  value-or-null  closure-ref
               body  file  string
            */
            if (!strcmp(s, "string"))
                return (jl_value_t*)jl_cstr_to_buffer(jl_scm_str(___CADR(e)));
            if (!strcmp(s, "lambda")) {
                // TODO: make function info record
            }
            size_t n = scm_list_length(e)-1;
            size_t i;
            jl_expr_t *ex = jl_exprn(jl_symbol(s), n);
            e = ___CDR(e);
            for(i=0; i < n; i++) {
                assert(___PAIRP(e));
                ((jl_value_t**)ex->args->data)[i] = scm_to_julia(___CAR(e));
                e = ___CDR(e);
            }
            return (jl_value_t*)ex;
        }
        else {
            jl_error("Malformed tree");
        }
    }
    jl_error("Malformed tree");
    
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

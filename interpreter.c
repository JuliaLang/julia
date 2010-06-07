#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <setjmp.h>
#include <assert.h>
#include <sys/types.h>
#include <limits.h>
#include <errno.h>
#include <math.h>
#ifdef BOEHM_GC
#include <gc.h>
#endif
#include "llt.h"
#include "julia.h"

static jl_value_t *eval(jl_value_t *e);

jl_value_t *jl_interpret_toplevel_expr(jl_value_t *e)
{
    return eval(e);
}

static jl_value_t *do_call(jl_function_t *f, jl_value_t **args, size_t nargs)
{
    jl_value_t **argv = alloca(nargs * sizeof(jl_value_t*));
    size_t i;
    for(i=0; i < nargs; i++)
        argv[i] = eval(args[i]);
    return jl_apply(f, argv, nargs);
}

static jl_value_t *eval(jl_value_t *e)
{
    if (jl_is_symbol(e)) {
        jl_value_t **bp = jl_get_bindingp(jl_system_module, (jl_sym_t*)e);
        if (*bp == NULL)
            jl_errorf("%s not defined", ((jl_sym_t*)e)->name);
        return *bp;
    }
    if (!jl_is_expr(e))
        return e;
    jl_expr_t *ex = (jl_expr_t*)e;
    jl_value_t **args = &jl_cellref(ex->args,0);
    if (ex->head == call_sym) {
        jl_function_t *f = (jl_function_t*)eval(args[0]);
        if (!jl_is_func(f))
            jl_error("apply: expected function");
        return do_call(f, &args[1], ex->args->length-1);
    }
    else if (ex->head == assign_sym) {
        jl_value_t **bp = jl_get_bindingp(jl_system_module, (jl_sym_t*)args[0]);
        *bp = eval(args[1]);
        return (jl_value_t*)jl_null;
    }
    else if (ex->head == top_sym) {
        // we only do toplevel bindings
        return eval(args[0]);
    }
    else if (ex->head == unbound_sym) {
        jl_value_t **bp = jl_get_bindingp(jl_system_module, (jl_sym_t*)args[0]);
        if (*bp == NULL)
            return jl_true;
        return jl_false;
    }
    else if (ex->head == quote_sym) {
        return args[0];
    }
    else if (ex->head == null_sym) {
        return (jl_value_t*)jl_null;
    }
    jl_error("not supported");
    return (jl_value_t*)jl_null;
}

static int label_idx(jl_value_t *tgt, jl_array_t *stmts)
{
    size_t j;
    for(j=0; j < stmts->length; j++) {
        jl_value_t *l = jl_cellref(stmts,j);
        if (jl_is_expr(l) && ((jl_expr_t*)l)->head==label_sym &&
            jl_exprarg(l,0)==tgt)
            break;
    }
    assert(j < stmts->length);
    return j;
}

jl_value_t *jl_interpret_toplevel_thunk(jl_lambda_info_t *lam)
{
    jl_expr_t *ast = (jl_expr_t*)lam->ast;
    jl_array_t *stmts = jl_lam_body(ast);
    size_t i=0;
    while (1) {
        jl_value_t *stmt = jl_cellref(stmts,i);
        if (jl_is_expr(stmt)) {
            jl_sym_t *head = ((jl_expr_t*)stmt)->head;
            if (head == label_sym) {
            }
            else if (head == goto_sym) {
                i = label_idx(jl_exprarg(stmt,0), stmts);
                continue;
            }
            else if (head == goto_ifnot_sym) {
                jl_value_t *cond = eval(jl_exprarg(stmt,0));
                if (cond == jl_false) {
                    i = label_idx(jl_exprarg(stmt,1), stmts);
                    continue;
                }
            }
            else if (head == return_sym) {
                return eval(jl_exprarg(stmt,0));
            }
            else {
                eval(stmt);
            }
        }
        else {
            eval(stmt);
        }
        i++;
    }
    assert(0);
    return (jl_value_t*)jl_null;
}

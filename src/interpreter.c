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
#include "llt.h"
#include "julia.h"
#include "builtin_proto.h"

static jl_value_t *eval(jl_value_t *e, jl_value_t **locals, size_t nl);
static jl_value_t *eval_body(jl_array_t *stmts, jl_value_t **locals, size_t nl,
                             int start);

jl_value_t *jl_interpret_toplevel_expr(jl_value_t *e)
{
    return eval(e, NULL, 0);
}

jl_value_t *jl_interpret_toplevel_expr_with(jl_value_t *e,
                                            jl_value_t **locals, size_t nl)
{
    return eval(e, locals, nl);
}

static jl_value_t *do_call(jl_function_t *f, jl_value_t **args, size_t nargs,
                           jl_value_t **locals, size_t nl)
{
    jl_value_t **argv = alloca((nargs+1) * sizeof(jl_value_t*));
    size_t i;
    argv[0] = (jl_value_t*)f;
    for(i=1; i < nargs+1; i++) argv[i] = NULL;
    JL_GC_PUSHARGS(argv, nargs+1);
    for(i=0; i < nargs; i++)
        argv[i+1] = eval(args[i], locals, nl);
    jl_value_t *result = jl_apply(f, &argv[1], nargs);
    JL_GC_POP();
    return result;
}

static jl_value_t **var_bp(jl_sym_t *s, jl_value_t **locals, size_t nl)
{
    size_t i;
    for(i=0; i < nl; i++) {
        if (locals[i*2] == (jl_value_t*)s) {
            return &locals[i*2+1];
        }
    }
    return jl_get_bindingp(jl_system_module, s);
}

static jl_value_t *eval(jl_value_t *e, jl_value_t **locals, size_t nl)
{
    if (jl_is_symbol(e)) {
        jl_value_t **bp = var_bp((jl_sym_t*)e, locals, nl);
        if (*bp == NULL)
            jl_errorf("%s not defined", ((jl_sym_t*)e)->name);
        return *bp;
    }
    if (!jl_is_expr(e)) {
        if (jl_is_lambda_info(e)) {
            return jl_new_closure_internal((jl_lambda_info_t*)e,
                                           (jl_value_t*)jl_null);
        }
        return e;
    }
    jl_expr_t *ex = (jl_expr_t*)e;
    jl_value_t **args = &jl_cellref(ex->args,0);
    if (ex->head == call_sym) {
        jl_function_t *f = (jl_function_t*)eval(args[0], locals, nl);
        if (!jl_is_func(f))
            jl_type_error("apply", (jl_value_t*)jl_function_type,
                          (jl_value_t*)f);
        return do_call(f, &args[1], ex->args->length-1, locals, nl);
    }
    else if (ex->head == assign_sym) {
        jl_value_t *sym = args[0];
        size_t i;
        for (i=0; i < nl; i++) {
            if (locals[i*2] == sym) {
                locals[i*2+1] = eval(args[1], locals, nl);
                return (jl_value_t*)jl_nothing;
            }
        }
        jl_value_t **bp = jl_get_bindingp(jl_system_module, (jl_sym_t*)sym);
        jl_value_t *rhs = eval(args[1], locals, nl);
        if (*bp==NULL || !jl_is_func(*bp))
            *bp = rhs;
        return (jl_value_t*)jl_nothing;
    }
    else if (ex->head == top_sym) {
        jl_value_t **bp = jl_get_bindingp(jl_system_module, (jl_sym_t*)args[0]);
        if (*bp == NULL)
            jl_errorf("%s not defined", ((jl_sym_t*)args[0])->name);
        return *bp;
    }
    else if (ex->head == symbol_sym) {
        return eval(jl_exprarg(ex,0), locals, nl);
    }
    else if (ex->head == new_sym) {
        jl_value_t *thetype = eval(args[0], locals, nl);
        JL_GC_PUSH(&thetype);
        assert(jl_is_struct_type(thetype));
        jl_value_t *v = jl_new_struct_uninit((jl_struct_type_t*)thetype);
        JL_GC_POP();
        return v;
    }
    else if (ex->head == quote_sym) {
        return args[0];
    }
    else if (ex->head == null_sym) {
        return (jl_value_t*)jl_nothing;
    }
    else if (ex->head == body_sym) {
        return eval_body(ex->args, locals, nl, 0);
    }
    else if (ex->head == isbound_sym) {
        jl_value_t **bp = var_bp((jl_sym_t*)args[0], locals, nl);
        return (*bp == NULL) ? jl_false : jl_true;
    }
    else if (ex->head == exc_sym) {
        return jl_exception_in_transit;
    }
    else if (ex->head == static_typeof_sym) {
        return (jl_value_t*)jl_any_type;
    }
    else if (ex->head == method_sym) {
        jl_sym_t *fname = (jl_sym_t*)args[0];
        jl_value_t **bp = var_bp(fname, locals, nl);
        jl_value_t *atypes=NULL, *meth=NULL;
        JL_GC_PUSH(&atypes, &meth);
        atypes = eval(args[1], locals, nl);
        meth = eval(args[2], locals, nl);
        jl_value_t *gf = jl_method_def(fname, bp, (jl_tuple_t*)atypes,
                                       (jl_function_t*)meth);
        JL_GC_POP();
        return gf;
    }
    else if (ex->head == error_sym) {
        jl_errorf("syntax error: %s", jl_string_data(args[0]));
    }
    else if (ex->head == line_sym) {
        return (jl_value_t*)jl_nothing;
    }
    jl_error("not supported");
    return (jl_value_t*)jl_nothing;
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

static int hand_n_leave = 0;

static jl_value_t *eval_body(jl_array_t *stmts, jl_value_t **locals, size_t nl,
                             int start)
{
    jl_savestate_t __ss;
    jmp_buf __handlr;
    size_t i=start;
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
                jl_value_t *cond = eval(jl_exprarg(stmt,0), locals, nl);
                if (cond == jl_false) {
                    i = label_idx(jl_exprarg(stmt,1), stmts);
                    continue;
                }
            }
            else if (head == return_sym) {
                return eval(jl_exprarg(stmt,0), locals, nl);
            }
            else if (head == enter_sym) {
                jl_enter_handler(&__ss, &__handlr);
                if (!setjmp(__handlr)) {
                    i = (size_t)eval_body(stmts, locals, nl, i+1);
                    continue;
                }
                else {
                    i = label_idx(jl_exprarg(stmt,0), stmts);
                    continue;
                }
            }
            else if (head == leave_sym) {
                if (hand_n_leave == 0) {
                    hand_n_leave = jl_unbox_int32(jl_exprarg(stmt,0));
                }
                jl_pop_handler(1);
                hand_n_leave--;
                if (hand_n_leave==0)
                    return (jl_value_t*)(i+1);
                else
                    return (jl_value_t*)(i);
            }
            else {
                eval(stmt, locals, nl);
            }
        }
        else {
            eval(stmt, locals, nl);
        }
        i++;
    }
    assert(0);
    return NULL;
}

jl_value_t *jl_interpret_toplevel_thunk_with(jl_lambda_info_t *lam,
                                             jl_value_t **loc, size_t nl)
{
    jl_expr_t *ast = (jl_expr_t*)lam->ast;
    jl_array_t *stmts = jl_lam_body(ast)->args;
    jl_array_t *l = jl_lam_locals(ast);
    jl_value_t **names = &((jl_value_t**)l->data)[0];
    nl += l->length;
    jl_value_t **locals = (jl_value_t**)alloca(nl*2*sizeof(void*));
    jl_value_t *r = (jl_value_t*)jl_null;
    size_t i=0;
    for(i=0; i < l->length; i++) {
        locals[i*2]   = names[i];
        locals[i*2+1] = NULL;
    }
    for(; i < nl; i++) {
        locals[i*2]   = loc[(i-l->length)*2];
        locals[i*2+1] = loc[(i-l->length)*2+1];
    }
    JL_GC_PUSHARGS(locals, nl*2);
    r = eval_body(stmts, locals, nl, 0);
    JL_GC_POP();
    return r;
}

jl_value_t *jl_interpret_toplevel_thunk(jl_lambda_info_t *lam)
{
    return jl_interpret_toplevel_thunk_with(lam, NULL, 0);
}

#include <stdlib.h>
#include <setjmp.h>
#include <assert.h>
#ifdef __WIN32__
#include <malloc.h>
#endif
#include "julia.h"
#include "builtin_proto.h"

extern int jl_lineno;

static jl_value_t *eval(jl_value_t *e, jl_value_t **locals, size_t nl);
static jl_value_t *eval_body(jl_array_t *stmts, jl_value_t **locals, size_t nl,
                             int start);
jl_value_t *jl_eval_module_expr(jl_expr_t *ex);

jl_value_t *jl_interpret_toplevel_expr(jl_value_t *e)
{
    return eval(e, NULL, 0);
}

jl_value_t *jl_interpret_toplevel_expr_with(jl_value_t *e,
                                            jl_value_t **locals, size_t nl)
{
    return eval(e, locals, nl);
}

jl_value_t *jl_interpret_toplevel_expr_in(jl_module_t *m, jl_value_t *e,
                                          jl_value_t **locals, size_t nl)
{
    jl_value_t *v=NULL;
    jl_module_t *last_m = jl_current_module;
    JL_TRY {
        jl_current_module = m;
        v = eval(e, locals, nl);
    }
    JL_CATCH {
        jl_current_module = last_m;
        jl_rethrow();
    }
    jl_current_module = last_m;
    assert(v);
    return v;
}

static jl_value_t *do_call(jl_function_t *f, jl_value_t **args, size_t nargs,
                           jl_value_t **locals, size_t nl)
{
    jl_value_t **argv;
    JL_GC_PUSHARGS(argv, nargs+1);
    size_t i;
    argv[0] = (jl_value_t*)f;
    for(i=1; i < nargs+1; i++) argv[i] = NULL;
    for(i=0; i < nargs; i++)
        argv[i+1] = eval(args[i], locals, nl);
    jl_value_t *result = jl_apply(f, &argv[1], nargs);
    JL_GC_POP();
    return result;
}

jl_value_t *jl_eval_global_var(jl_module_t *m, jl_sym_t *e)
{
    jl_value_t *v = jl_get_global(m, e);
    if (v == NULL)
        jl_errorf("%s not defined", e->name);
    return v;
}

extern int jl_boot_file_loaded;
extern int inside_typedef;

static jl_value_t *eval(jl_value_t *e, jl_value_t **locals, size_t nl)
{
    if (jl_is_symbol(e)) {
        jl_value_t *v;
        size_t i;
        for(i=0; i < nl; i++) {
            if (locals[i*2] == e) {
                v = locals[i*2+1];
                break;
            }
        }
        if (i >= nl) {
            v = jl_get_global(jl_current_module, (jl_sym_t*)e);
        }
        if (v == NULL) {
            jl_errorf("%s not defined", ((jl_sym_t*)e)->name);
        }
        return v;
    }
    if (jl_is_symbolnode(e)) {
        return eval((jl_value_t*)jl_symbolnode_sym(e), locals, nl);
    }
    if (jl_is_quotenode(e)) {
        return jl_fieldref(e,0);
    }
    if (jl_is_topnode(e)) {
        jl_sym_t *s = (jl_sym_t*)jl_fieldref(e,0);
        jl_value_t *v = jl_get_global(jl_base_relative_to(jl_current_module),s);
        if (v == NULL)
            jl_errorf("%s not defined", s->name);
        return v;
    }
    if (!jl_is_expr(e)) {
        if (jl_is_getfieldnode(e)) {
            jl_value_t *v = eval(jl_getfieldnode_val(e), locals, nl);
            jl_value_t *gfargs[2] = {v, (jl_value_t*)jl_getfieldnode_name(e)};
            return jl_f_get_field(NULL, gfargs, 2);
        }
        if (jl_is_lambda_info(e)) {
            return (jl_value_t*)jl_new_closure(NULL, (jl_value_t*)jl_null,
                                               (jl_lambda_info_t*)e);
        }
        if (jl_is_linenode(e)) {
            jl_lineno = jl_linenode_line(e);
        }
        return e;
    }
    jl_expr_t *ex = (jl_expr_t*)e;
    jl_value_t **args = &jl_cellref(ex->args,0);
    size_t nargs = jl_array_len(ex->args);
    if (ex->head == call_sym ||  ex->head == call1_sym) {
        jl_function_t *f = (jl_function_t*)eval(args[0], locals, nl);
        if (!jl_is_func(f))
            jl_type_error("apply", (jl_value_t*)jl_function_type,
                          (jl_value_t*)f);
        return do_call(f, &args[1], nargs-1, locals, nl);
    }
    else if (ex->head == assign_sym) {
        jl_value_t *sym = args[0];
        size_t i;
        for (i=0; i < nl; i++) {
            if (locals[i*2] == sym) {
                return (locals[i*2+1] = eval(args[1], locals, nl));
            }
        }
        jl_binding_t *b = jl_get_binding_wr(jl_current_module, (jl_sym_t*)sym);
        jl_value_t *rhs = eval(args[1], locals, nl);
        jl_checked_assignment(b, rhs);
        return rhs;
    }
    else if (ex->head == new_sym) {
        jl_value_t *thetype = eval(args[0], locals, nl);
        jl_value_t *v=NULL;
        JL_GC_PUSH(&thetype, &v);
        assert(jl_is_structtype(thetype));
        v = jl_new_struct_uninit((jl_datatype_t*)thetype);
        for(size_t i=1; i < nargs; i++) {
            jl_set_nth_field(v, i-1, eval(args[i], locals, nl));
        }
        JL_GC_POP();
        return v;
    }
    else if (ex->head == null_sym) {
        return (jl_value_t*)jl_nothing;
    }
    else if (ex->head == body_sym) {
        return eval_body(ex->args, locals, nl, 0);
    }
    else if (ex->head == exc_sym) {
        return jl_exception_in_transit;
    }
    else if (ex->head == static_typeof_sym) {
        return (jl_value_t*)jl_any_type;
    }
    else if (ex->head == method_sym) {
        jl_sym_t *fname = (jl_sym_t*)args[0];
        jl_value_t **bp=NULL;
        jl_binding_t *b=NULL;
        for (size_t i=0; i < nl; i++) {
            if (locals[i*2] == (jl_value_t*)fname) {
                bp = &locals[i*2+1];
                break;
            }
        }
        if (bp == NULL) {
            b = jl_get_binding_for_method_def(jl_current_module, fname);
            bp = &b->value;
        }
        jl_value_t *atypes=NULL, *meth=NULL, *tvars=NULL;
        JL_GC_PUSH(&atypes, &meth, &tvars);
        atypes = eval(args[1], locals, nl);
        meth = eval(args[2], locals, nl);
        tvars = eval(args[3], locals, nl);
        jl_method_def(fname, bp, b, (jl_tuple_t*)atypes,
                      (jl_function_t*)meth, (jl_tuple_t*)tvars);
        JL_GC_POP();
        return *bp;
    }
    else if (ex->head == const_sym) {
        jl_value_t *sym = args[0];
        for (size_t i=0; i < nl; i++) {
            if (locals[i*2] == sym) {
                return (jl_value_t*)jl_nothing;
            }
        }
        jl_binding_t *b = jl_get_binding_wr(jl_current_module, (jl_sym_t*)sym);
        jl_declare_constant(b);
        return (jl_value_t*)jl_nothing;
    }
    else if (ex->head == global_sym) {
        // create uninitialized mutable binding for "global x" decl
        // TODO: handle type decls
        for (size_t i=0; i < jl_array_len(ex->args); i++) {
            assert(jl_is_symbol(args[i]));
            jl_get_binding_wr(jl_current_module, (jl_sym_t*)args[i]);
        }
        return (jl_value_t*)jl_nothing;
    }
    else if (ex->head == abstracttype_sym) {
        jl_value_t *name = args[0];
        jl_value_t *para = eval(args[1], locals, nl);
        jl_value_t *super = NULL;
        JL_GC_PUSH(&para, &super);
        jl_datatype_t *dt =
            jl_new_abstracttype(name, jl_any_type, (jl_tuple_t*)para);
        jl_binding_t *b = jl_get_binding_wr(jl_current_module, (jl_sym_t*)name);
        jl_checked_assignment(b, (jl_value_t*)dt);
        super = eval(args[2], locals, nl);
        jl_set_datatype_super(dt, super);
        JL_GC_POP();
        return (jl_value_t*)jl_nothing;
    }
    else if (ex->head == bitstype_sym) {
        jl_value_t *name = args[0];
        jl_value_t *super = NULL, *para = NULL, *vnb = NULL;
        JL_GC_PUSH(&para, &super, &vnb);
        para = eval(args[1], locals, nl);
        vnb  = eval(args[2], locals, nl);
        if (!jl_is_long(vnb))
            jl_errorf("invalid declaration of bits type %s", ((jl_sym_t*)name)->name);
        int32_t nb = jl_unbox_long(vnb);
        if (nb < 1 || nb>=(1<<23) || (nb&7) != 0)
            jl_errorf("invalid number of bits in type %s",
                      ((jl_sym_t*)name)->name);
        jl_datatype_t *dt =
            jl_new_bitstype(name, jl_any_type, (jl_tuple_t*)para, nb);
        jl_binding_t *b = jl_get_binding_wr(jl_current_module, (jl_sym_t*)name);
        jl_checked_assignment(b, (jl_value_t*)dt);
        super = eval(args[3], locals, nl);
        jl_set_datatype_super(dt, super);
        JL_GC_POP();
        return (jl_value_t*)jl_nothing;
    }
    else if (ex->head == compositetype_sym) {
        void jl_add_constructors(jl_datatype_t *t);
        jl_value_t *name = args[0];
        jl_value_t *para = eval(args[1], locals, nl);
        jl_value_t *fnames = NULL;
        jl_value_t *super = NULL;
        jl_datatype_t *dt = NULL;
        JL_GC_PUSH(&para, &super, &fnames, &dt);
        fnames = eval(args[2], locals, nl);
        dt = jl_new_datatype((jl_sym_t*)name, jl_any_type, (jl_tuple_t*)para,
                             (jl_tuple_t*)fnames, NULL,
                             0, args[6]==jl_true ? 1 : 0);
        dt->fptr = jl_f_ctor_trampoline;
        dt->ctor_factory = eval(args[3], locals, nl);
        jl_binding_t *b = jl_get_binding_wr(jl_current_module, (jl_sym_t*)name);
        jl_checked_assignment(b, (jl_value_t*)dt);
        inside_typedef = 1;
        dt->types = (jl_tuple_t*)eval(args[5], locals, nl);
        inside_typedef = 0;
        jl_check_type_tuple(dt->types, dt->name->name, "type definition");
        super = eval(args[4], locals, nl);
        jl_set_datatype_super(dt, super);
        jl_compute_field_offsets(dt);
        jl_add_constructors(dt);
        JL_GC_POP();
        return (jl_value_t*)jl_nothing;
    }
    else if (ex->head == macro_sym) {
        jl_sym_t *nm = (jl_sym_t*)args[0];
        assert(jl_is_symbol(nm));
        jl_function_t *f = (jl_function_t*)eval(args[1], locals, nl);
        assert(jl_is_function(f));
        if (jl_boot_file_loaded &&
            f->linfo && f->linfo->ast && jl_is_expr(f->linfo->ast)) {
            jl_lambda_info_t *li = f->linfo;
            li->ast = jl_compress_ast(li, li->ast);
            li->name = nm;
        }
        jl_set_global(jl_current_module, nm, (jl_value_t*)f);
        return (jl_value_t*)jl_nothing;
    }
    else if (ex->head == line_sym) {
        jl_lineno = jl_unbox_long(jl_exprarg(ex,0));
        return (jl_value_t*)jl_nothing;
    }
    else if (ex->head == module_sym) {
        return jl_eval_module_expr(ex);
    }
    else if (ex->head == error_sym || ex->head == jl_continue_sym) {
        if (jl_is_byte_string(args[0]))
            jl_errorf("syntax: %s", jl_string_data(args[0]));
        jl_throw(args[0]);
    }
    jl_errorf("unsupported or misplaced expression %s", ex->head->name);
    return (jl_value_t*)jl_nothing;
}

static int label_idx(jl_value_t *tgt, jl_array_t *stmts)
{
    size_t j;
    long ltgt = jl_unbox_long(tgt);
    for(j=0; j < stmts->nrows; j++) {
        jl_value_t *l = jl_cellref(stmts,j);
        if (jl_is_labelnode(l) && jl_labelnode_label(l)==ltgt)
            break;
    }
    assert(j < stmts->nrows);
    return j;
}

static jl_value_t *eval_body(jl_array_t *stmts, jl_value_t **locals, size_t nl,
                             int start)
{
    jl_handler_t __eh;
    size_t i=start;
    while (1) {
        jl_value_t *stmt = jl_cellref(stmts,i);
        if (jl_is_gotonode(stmt)) {
            i = label_idx(jl_fieldref(stmt,0), stmts);
            continue;
        }
        if (jl_is_expr(stmt)) {
            jl_sym_t *head = ((jl_expr_t*)stmt)->head;
            if (head == goto_ifnot_sym) {
                jl_value_t *cond = eval(jl_exprarg(stmt,0), locals, nl);
                if (cond == jl_false) {
                    i = label_idx(jl_exprarg(stmt,1), stmts);
                    continue;
                }
                else if (cond != jl_true) {
                    jl_type_error_rt("toplevel", "if",
                                     (jl_value_t*)jl_bool_type, cond);
                }
            }
            else if (head == return_sym) {
                return eval(jl_exprarg(stmt,0), locals, nl);
            }
            else if (head == enter_sym) {
                jl_enter_handler(&__eh);
                if (!jl_setjmp(__eh.eh_ctx,1)) {
                    return eval_body(stmts, locals, nl, i+1);
                }
                else {
                    i = label_idx(jl_exprarg(stmt,0), stmts);
                    continue;
                }
            }
            else if (head == leave_sym) {
                int hand_n_leave = jl_unbox_long(jl_exprarg(stmt,0));
                jl_pop_handler(hand_n_leave);
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
    size_t llength = jl_array_len(l);
    jl_value_t **names = &((jl_value_t**)l->data)[0];
    nl += llength;
    jl_value_t **locals;
    JL_GC_PUSHARGS(locals, nl*2);
    jl_value_t *r = (jl_value_t*)jl_null;
    size_t i=0;
    for(i=0; i < llength; i++) {
        locals[i*2]   = names[i];
        locals[i*2+1] = NULL;
    }
    for(; i < nl; i++) {
        locals[i*2]   = loc[(i-llength)*2];
        locals[i*2+1] = loc[(i-llength)*2+1];
    }
    r = eval_body(stmts, locals, nl, 0);
    JL_GC_POP();
    return r;
}

jl_value_t *jl_interpret_toplevel_thunk(jl_lambda_info_t *lam)
{
    return jl_interpret_toplevel_thunk_with(lam, NULL, 0);
}

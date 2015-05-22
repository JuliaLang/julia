// This file is a part of Julia. License is MIT: http://julialang.org/license

#include <stdlib.h>
#include <setjmp.h>
#include <assert.h>
#ifdef _OS_WINDOWS_
#include <malloc.h>
#endif
#include "julia.h"
#include "julia_internal.h"
#include "builtin_proto.h"

#ifdef __cplusplus
extern "C" {
#endif

extern int jl_lineno;

static jl_value_t *eval(jl_value_t *e, jl_value_t **locals, size_t nl, size_t ngensym);
static jl_value_t *eval_body(jl_array_t *stmts, jl_value_t **locals, size_t nl, size_t ngensym,
                             int start, int toplevel);
jl_value_t *jl_eval_module_expr(jl_expr_t *ex);
int jl_is_toplevel_only_expr(jl_value_t *e);

jl_value_t *jl_interpret_toplevel_expr(jl_value_t *e)
{
    return eval(e, NULL, 0, 0);
}

DLLEXPORT jl_value_t *jl_interpret_toplevel_expr_in(jl_module_t *m, jl_value_t *e,
                                                    jl_value_t **locals, size_t nl)
{
    jl_value_t *v=NULL;
    jl_module_t *last_m = jl_current_module;
    jl_module_t *task_last_m = jl_current_task->current_module;
    JL_TRY {
        jl_current_task->current_module = jl_current_module = m;
        v = eval(e, locals, nl, 0);
    }
    JL_CATCH {
        jl_current_module = last_m;
        jl_current_task->current_module = task_last_m;
        jl_rethrow();
    }
    jl_current_module = last_m;
    jl_current_task->current_module = task_last_m;
    assert(v);
    return v;
}

static jl_value_t *do_call(jl_function_t *f, jl_value_t **args, size_t nargs,
                           jl_value_t *eval0, jl_value_t **locals, size_t nl, size_t ngensym)
{
    jl_value_t **argv;
    JL_GC_PUSHARGS(argv, nargs+1);
    size_t i;
    argv[0] = (jl_value_t*)f;
    i = 0;
    if (eval0) { /* 0-th argument has already been evaluated */
        argv[1] = eval0; i++;
    }
    for(; i < nargs; i++) {
        argv[i+1] = eval(args[i], locals, nl, ngensym);
    }
    jl_value_t *result = jl_apply(f, &argv[1], nargs);
    JL_GC_POP();
    return result;
}

jl_value_t *jl_eval_global_var(jl_module_t *m, jl_sym_t *e)
{
    jl_value_t *v = jl_get_global(m, e);
    if (v == NULL)
        jl_undefined_var_error(e);
    return v;
}

void jl_check_static_parameter_conflicts(jl_lambda_info_t *li, jl_svec_t *t, jl_sym_t *fname);

int jl_has_intrinsics(jl_expr_t *e, jl_module_t *m);

extern int jl_boot_file_loaded;
extern int inside_typedef;

// this is a heuristic for allowing "redefining" a type to something identical
static int equiv_type(jl_datatype_t *dta, jl_datatype_t *dtb)
{
    return (jl_typeof(dta) == jl_typeof(dtb) &&
            // TODO: can't yet handle parametric types due to how constructors work
            dta->parameters == jl_emptysvec &&
            dta->name->name == dtb->name->name &&
            jl_egal((jl_value_t*)dta->types, (jl_value_t*)dtb->types) &&
            dta->abstract == dtb->abstract &&
            dta->mutabl == dtb->mutabl &&
            dta->size == dtb->size &&
            jl_egal((jl_value_t*)dta->super, (jl_value_t*)dtb->super) &&
            jl_egal((jl_value_t*)dta->name->names, (jl_value_t*)dtb->name->names) &&
            jl_egal((jl_value_t*)dta->parameters, (jl_value_t*)dtb->parameters));
}

static void check_can_assign_type(jl_binding_t *b)
{
    if (b->constp && b->value != NULL && !jl_is_datatype(b->value))
        jl_errorf("invalid redefinition of constant %s", b->name->name);
}

static jl_value_t *eval(jl_value_t *e, jl_value_t **locals, size_t nl, size_t ngensym)
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
            jl_undefined_var_error((jl_sym_t*)e);
        }
        return v;
    }
    if (jl_is_symbolnode(e)) {
        return eval((jl_value_t*)jl_symbolnode_sym(e), locals, nl, ngensym);
    }
    if (jl_is_gensym(e)) {
        ssize_t genid = ((jl_gensym_t*)e)->id;
        if (genid >= ngensym || genid < 0)
            jl_error("access to invalid GenSym location");
        else
            return locals[nl*2 + genid];
    }
    if (jl_is_quotenode(e)) {
        return jl_fieldref(e,0);
    }
    if (jl_is_topnode(e)) {
        jl_sym_t *s = (jl_sym_t*)jl_fieldref(e,0);
        jl_value_t *v = jl_get_global(jl_base_relative_to(jl_current_module),s);
        if (v == NULL)
            jl_undefined_var_error(s);
        return v;
    }
    if (!jl_is_expr(e)) {
        if (jl_is_globalref(e)) {
            jl_value_t *gfargs[2] = {(jl_value_t*)jl_globalref_mod(e), (jl_value_t*)jl_globalref_name(e)};
            return jl_f_get_field(NULL, gfargs, 2);
        }
        if (jl_is_lambda_info(e)) {
            jl_lambda_info_t *li = (jl_lambda_info_t*)e;
            if (jl_boot_file_loaded && li->ast && jl_is_expr(li->ast)) {
                li->ast = jl_compress_ast(li, li->ast);
                gc_wb(li, li->ast);
            }
            return (jl_value_t*)jl_new_closure(NULL, (jl_value_t*)jl_emptysvec, li);
        }
        if (jl_is_linenode(e)) {
            jl_lineno = jl_linenode_line(e);
        }
        if (jl_is_newvarnode(e)) {
            jl_value_t *var = jl_fieldref(e,0);
            assert(!jl_is_gensym(var));
            assert(jl_is_symbol(var));
            for(size_t i=0; i < nl; i++) {
                if (locals[i*2] == var) {
                    locals[i*2+1] = NULL;
                    break;
                }
            }
            return (jl_value_t*)jl_nothing;
        }
        return e;
    }
    jl_expr_t *ex = (jl_expr_t*)e;
    jl_value_t **args = (jl_value_t**)jl_array_data(ex->args);
    size_t nargs = jl_array_len(ex->args);
    if (ex->head == call_sym ||  ex->head == call1_sym) {
        if (jl_is_lambda_info(args[0])) {
            // directly calling an inner function ("let")
            jl_lambda_info_t *li = (jl_lambda_info_t*)args[0];
            if (jl_is_expr(li->ast) && !jl_lam_vars_captured((jl_expr_t*)li->ast) &&
                !jl_has_intrinsics((jl_expr_t*)li->ast, jl_current_module)) {
                size_t na = nargs-1;
                if (na == 0)
                    return jl_interpret_toplevel_thunk(li);
                jl_array_t *formals = jl_lam_args((jl_expr_t*)li->ast);
                size_t nreq = jl_array_len(formals);
                if (nreq==0 || !jl_is_rest_arg(jl_cellref(formals,nreq-1))) {
                    jl_value_t **ar;
                    JL_GC_PUSHARGS(ar, na*2);
                    for(int i=0; i < na; i++) {
                        ar[i*2+1] = eval(args[i+1], locals, nl, ngensym);
                        gc_wb(ex->args, ar[i*2+1]);
                    }
                    if (na != nreq) {
                        jl_error("wrong number of arguments");
                    }
                    for(int i=0; i < na; i++) {
                        jl_value_t *v = jl_cellref(formals, i);
                        ar[i*2] = (jl_is_gensym(v)) ? v : (jl_value_t*)jl_decl_var(v);
                    }
                    jl_value_t *ret = jl_interpret_toplevel_thunk_with(li, ar, na);
                    JL_GC_POP();
                    return ret;
                }
            }
        }
        jl_function_t *f = (jl_function_t*)eval(args[0], locals, nl, ngensym);
        if (jl_is_func(f))
            return do_call(f, &args[1], nargs-1, NULL, locals, nl, ngensym);
        else
            return do_call(jl_module_call_func(jl_current_module), args, nargs, (jl_value_t*)f, locals, nl, ngensym);
    }
    else if (ex->head == assign_sym) {
        jl_value_t *sym = args[0];
        jl_value_t *rhs = eval(args[1], locals, nl, ngensym);
        if (jl_is_gensym(sym)) {
            ssize_t genid = ((jl_gensym_t*)sym)->id;
            if (genid >= ngensym || genid < 0)
                jl_error("assignment to invalid GenSym location");
            locals[nl*2 + genid] = rhs;
            gc_wb(jl_current_module, rhs); // not sure about jl_current_module
            return rhs;
        }
        assert(jl_is_symbol(sym));
        size_t i;
        for (i=0; i < nl; i++) {
            if (locals[i*2] == sym) {
                locals[i*2+1] = rhs;
                gc_wb(jl_current_module, rhs); // not sure about jl_current_module
                return rhs;
            }
        }
        JL_GC_PUSH1(&rhs);
        jl_binding_t *b = jl_get_binding_wr(jl_current_module, (jl_sym_t*)sym);
        jl_checked_assignment(b, rhs);
        JL_GC_POP();
        return rhs;
    }
    else if (ex->head == new_sym) {
        jl_value_t *thetype = eval(args[0], locals, nl, ngensym);
        jl_value_t *v=NULL;
        JL_GC_PUSH2(&thetype, &v);
        assert(jl_is_structtype(thetype));
        v = jl_new_struct_uninit((jl_datatype_t*)thetype);
        for(size_t i=1; i < nargs; i++) {
            jl_set_nth_field(v, i-1, eval(args[i], locals, nl, ngensym));
        }
        JL_GC_POP();
        return v;
    }
    else if (ex->head == null_sym) {
        return (jl_value_t*)jl_nothing;
    }
    else if (ex->head == body_sym) {
        return eval_body(ex->args, locals, nl, ngensym, 0, 0);
    }
    else if (ex->head == exc_sym) {
        return jl_exception_in_transit;
    }
    else if (ex->head == method_sym) {
        jl_sym_t *fname = (jl_sym_t*)args[0];
        jl_value_t **bp=NULL;
        jl_value_t *bp_owner=NULL;
        jl_binding_t *b=NULL;
        jl_value_t *gf=NULL;
        int kw=0;
        if (jl_is_expr(fname)) {
            if (((jl_expr_t*)fname)->head == kw_sym) {
                kw = 1;
                fname = (jl_sym_t*)jl_exprarg(fname, 0);
            }
            gf = eval((jl_value_t*)fname, locals, nl, ngensym);
            if (jl_is_expr(fname))
                fname = (jl_sym_t*)jl_fieldref(jl_exprarg(fname, 2), 0);
            bp = &gf;
            assert(jl_is_symbol(fname));
        }
        else {
            for (size_t i=0; i < nl; i++) {
                if (locals[i*2] == (jl_value_t*)fname) {
                    bp = &locals[i*2+1];
                    break;
                }
            }
            if (bp == NULL) {
                b = jl_get_binding_for_method_def(jl_current_module, fname);
                bp = &b->value;
                bp_owner = (jl_value_t*)jl_current_module;
            }
        }
        jl_value_t *atypes=NULL, *meth=NULL;
        JL_GC_PUSH2(&atypes, &meth);
        atypes = eval(args[1], locals, nl, ngensym);
        if (jl_is_lambda_info(args[2])) {
            jl_check_static_parameter_conflicts((jl_lambda_info_t*)args[2], (jl_svec_t*)jl_svecref(atypes,1), fname);
        }
        meth = eval(args[2], locals, nl, ngensym);
        jl_method_def(fname, bp, bp_owner, b, (jl_svec_t*)atypes, (jl_function_t*)meth, args[3], NULL, kw);
        JL_GC_POP();
        return *bp;
    }
    else if (ex->head == copyast_sym) {
        return jl_copy_ast(eval(args[0], locals, nl, ngensym));
    }
    else if (ex->head == const_sym) {
        jl_value_t *sym = args[0];
        assert(jl_is_symbol(sym));
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
        jl_value_t *para = eval(args[1], locals, nl, ngensym);
        jl_value_t *super = NULL;
        jl_value_t *temp = NULL;
        jl_datatype_t *dt = NULL;
        JL_GC_PUSH4(&para, &super, &temp, &dt);
        assert(jl_is_svec(para));
        assert(jl_is_symbol(name));
        dt = jl_new_abstracttype(name, jl_any_type, (jl_svec_t*)para);
        jl_binding_t *b = jl_get_binding_wr(jl_current_module, (jl_sym_t*)name);
        temp = b->value;
        check_can_assign_type(b);
        b->value = (jl_value_t*)dt;
        gc_wb_binding(b, dt);
        super = eval(args[2], locals, nl, ngensym);
        jl_set_datatype_super(dt, super);
        b->value = temp;
        if (temp==NULL || !equiv_type(dt, (jl_datatype_t*)temp)) {
            jl_checked_assignment(b, (jl_value_t*)dt);
        }
        JL_GC_POP();
        return (jl_value_t*)jl_nothing;
    }
    else if (ex->head == bitstype_sym) {
        jl_value_t *name = args[0];
        jl_value_t *super = NULL, *para = NULL, *vnb = NULL, *temp = NULL;
        jl_datatype_t *dt = NULL;
        JL_GC_PUSH4(&para, &super, &temp, &dt);
        assert(jl_is_symbol(name));
        para = eval(args[1], locals, nl, ngensym);
        assert(jl_is_svec(para));
        vnb  = eval(args[2], locals, nl, ngensym);
        if (!jl_is_long(vnb))
            jl_errorf("invalid declaration of bits type %s", ((jl_sym_t*)name)->name);
        int32_t nb = jl_unbox_long(vnb);
        if (nb < 1 || nb>=(1<<23) || (nb&7) != 0)
            jl_errorf("invalid number of bits in type %s",
                      ((jl_sym_t*)name)->name);
        dt = jl_new_bitstype(name, jl_any_type, (jl_svec_t*)para, nb);
        jl_binding_t *b = jl_get_binding_wr(jl_current_module, (jl_sym_t*)name);
        temp = b->value;
        check_can_assign_type(b);
        b->value = (jl_value_t*)dt;
        gc_wb_binding(b, dt);
        super = eval(args[3], locals, nl, ngensym);
        jl_set_datatype_super(dt, super);
        b->value = temp;
        if (temp==NULL || !equiv_type(dt, (jl_datatype_t*)temp)) {
            jl_checked_assignment(b, (jl_value_t*)dt);
        }
        JL_GC_POP();
        return (jl_value_t*)jl_nothing;
    }
    else if (ex->head == compositetype_sym) {
        jl_value_t *name = args[0];
        assert(jl_is_symbol(name));
        jl_value_t *para = eval(args[1], locals, nl, ngensym);
        assert(jl_is_svec(para));
        jl_value_t *temp = NULL;
        jl_value_t *super = NULL;
        jl_datatype_t *dt = NULL;
        JL_GC_PUSH4(&para, &super, &temp, &dt);
        temp = eval(args[2], locals, nl, ngensym);  // field names
        dt = jl_new_datatype((jl_sym_t*)name, jl_any_type, (jl_svec_t*)para,
                             (jl_svec_t*)temp, NULL,
                             0, args[5]==jl_true ? 1 : 0, jl_unbox_long(args[6]));

        jl_binding_t *b = jl_get_binding_wr(jl_current_module, (jl_sym_t*)name);
        temp = b->value;  // save old value
        // temporarily assign so binding is available for field types
        check_can_assign_type(b);
        b->value = (jl_value_t*)dt;
        gc_wb_binding(b,dt);

        JL_TRY {
            // operations that can fail
            inside_typedef = 1;
            dt->types = (jl_svec_t*)eval(args[4], locals, nl, ngensym);
            gc_wb(dt, dt->types);
            inside_typedef = 0;
            //jl_check_type_tuple(dt->types, dt->name->name, "type definition");
            super = eval(args[3], locals, nl, ngensym);
            jl_set_datatype_super(dt, super);
        }
        JL_CATCH {
            b->value = temp;
            jl_rethrow();
        }
        for(size_t i=0; i < jl_svec_len(para); i++) {
            ((jl_tvar_t*)jl_svecref(para,i))->bound = 0;
        }
        jl_compute_field_offsets(dt);
        if (para == (jl_value_t*)jl_emptysvec && jl_is_datatype_singleton(dt)) {
            dt->instance = newstruct(dt);
            gc_wb(dt, dt->instance);
        }

        b->value = temp;
        if (temp==NULL || !equiv_type(dt, (jl_datatype_t*)temp)) {
            jl_checked_assignment(b, (jl_value_t*)dt);
        }
        else {
            // TODO: remove all old ctors and set temp->name->ctor_factory = dt->name->ctor_factory
        }

        JL_GC_POP();
        return (jl_value_t*)jl_nothing;
    }
    else if (ex->head == macro_sym) {
        jl_sym_t *nm = (jl_sym_t*)args[0];
        assert(jl_is_symbol(nm));
        jl_function_t *f = (jl_function_t*)eval(args[1], locals, nl, ngensym);
        JL_GC_PUSH1(&f);
        assert(jl_is_function(f));
        if (jl_boot_file_loaded &&
            f->linfo && f->linfo->ast && jl_is_expr(f->linfo->ast)) {
            jl_lambda_info_t *li = f->linfo;
            li->ast = jl_compress_ast(li, li->ast);
            gc_wb(li, li->ast);
            li->name = nm;
        }
        jl_set_global(jl_current_module, nm, (jl_value_t*)f);
        JL_GC_POP();
        return (jl_value_t*)jl_nothing;
    }
    else if (ex->head == line_sym) {
        jl_lineno = jl_unbox_long(jl_exprarg(ex,0));
        return (jl_value_t*)jl_nothing;
    }
    else if (ex->head == module_sym) {
        return jl_eval_module_expr(ex);
    }
    else if (ex->head == error_sym || ex->head == jl_incomplete_sym) {
        if (nargs == 0)
            jl_error("malformed \"error\" expression");
        if (jl_is_byte_string(args[0]))
            jl_errorf("syntax: %s", jl_string_data(args[0]));
        jl_throw(args[0]);
    }
    else if (ex->head == boundscheck_sym) {
        return (jl_value_t*)jl_nothing;
    }
    else if (ex->head == fastmath_sym) {
        return (jl_value_t*)jl_nothing;
    }
    else if (ex->head == simdloop_sym) {
        return (jl_value_t*)jl_nothing;
    }
    else if (ex->head == meta_sym) {
        return (jl_value_t*)jl_nothing;
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

jl_value_t *jl_toplevel_eval_body(jl_array_t *stmts)
{
    ssize_t ngensym = 0;
    size_t i, l = jl_array_len(stmts);
    for (i = 0; i < l; i++) {
        ssize_t maxid = jl_max_jlgensym_in(jl_cellref(stmts, i))+1;
        if (maxid > ngensym)
            ngensym = maxid;
    }
    jl_value_t **locals = NULL;
    if (ngensym > 0) {
        JL_GC_PUSHARGS(locals, ngensym);
    }
    jl_value_t *ret = eval_body(stmts, locals, 0, ngensym, 0, 1);
    if (ngensym > 0)
        JL_GC_POP();
    return ret;
}

static jl_value_t *eval_body(jl_array_t *stmts, jl_value_t **locals, size_t nl, size_t ngensym,
                             int start, int toplevel)
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
                jl_value_t *cond = eval(jl_exprarg(stmt,0), locals, nl, ngensym);
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
                jl_value_t *ex = jl_exprarg(stmt,0);
                if (toplevel && jl_is_toplevel_only_expr(ex))
                    return jl_toplevel_eval(ex);
                else
                    return eval(ex, locals, nl, ngensym);
            }
            else if (head == enter_sym) {
                jl_enter_handler(&__eh);
                if (!jl_setjmp(__eh.eh_ctx,1)) {
                    return eval_body(stmts, locals, nl, ngensym, i+1, toplevel);
                }
                else {
#ifdef _OS_WINDOWS_
                    if (jl_exception_in_transit == jl_stackovf_exception)
                        _resetstkoflw();
#endif
                    i = label_idx(jl_exprarg(stmt,0), stmts);
                    continue;
                }
            }
            else if (head == leave_sym) {
                int hand_n_leave = jl_unbox_long(jl_exprarg(stmt,0));
                jl_pop_handler(hand_n_leave);
            }
            else {
                if (toplevel && jl_is_toplevel_only_expr(stmt))
                    jl_toplevel_eval(stmt);
                else
                    eval(stmt, locals, nl, ngensym);
            }
        }
        else {
            if (toplevel && jl_is_toplevel_only_expr(stmt))
                jl_toplevel_eval(stmt);
            else
                eval(stmt, locals, nl, ngensym);
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
    jl_value_t *gensym_types = jl_lam_gensyms(ast);
    size_t ngensym = (jl_is_array(gensym_types) ? jl_array_len(gensym_types) : jl_unbox_gensym(gensym_types));
    JL_GC_PUSHARGS(locals, nl*2+ngensym);
    jl_value_t *r = (jl_value_t*)jl_nothing;
    size_t i=0;
    for(i=0; i < llength; i++) {
        locals[i*2]   = names[i];
        //locals[i*2+1] = NULL;   // init'd by JL_GC_PUSHARGS
    }
    for(; i < nl; i++) {
        locals[i*2]   = loc[(i-llength)*2];
        locals[i*2+1] = loc[(i-llength)*2+1];
    }
    r = eval_body(stmts, locals, nl, ngensym, 0, 1);
    JL_GC_POP();
    return r;
}

jl_value_t *jl_interpret_toplevel_thunk(jl_lambda_info_t *lam)
{
    return jl_interpret_toplevel_thunk_with(lam, NULL, 0);
}

#ifdef __cplusplus
}
#endif

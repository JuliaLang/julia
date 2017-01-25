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

typedef struct {
    jl_code_info_t *src;
    jl_module_t *module;
    jl_value_t **locals;
    jl_svec_t *sparam_vals;
} interpreter_state;

static jl_value_t *eval(jl_value_t *e, interpreter_state *s);
static jl_value_t *eval_body(jl_array_t *stmts, interpreter_state *s, int start, int toplevel);

jl_value_t *jl_eval_module_expr(jl_expr_t *ex);
int jl_is_toplevel_only_expr(jl_value_t *e);

jl_value_t *jl_interpret_toplevel_expr(jl_value_t *e)
{
    size_t last_age = jl_get_ptls_states()->world_age;
    jl_get_ptls_states()->world_age = jl_world_counter;
    jl_value_t *ret = eval(e, NULL);
    jl_get_ptls_states()->world_age = last_age;
    return ret;
}

jl_value_t *jl_interpret_toplevel_expr_in(jl_module_t *m, jl_value_t *e,
                                          jl_code_info_t *src,
                                          jl_svec_t *sparam_vals)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_value_t *v=NULL;
    jl_module_t *last_m = ptls->current_module;
    jl_module_t *task_last_m = ptls->current_task->current_module;
    interpreter_state s;
    s.src = src;
    s.module = m;
    s.locals = NULL;
    s.sparam_vals = sparam_vals;

    JL_TRY {
        ptls->current_task->current_module = ptls->current_module = m;
        v = eval(e, &s);
    }
    JL_CATCH {
        ptls->current_module = last_m;
        ptls->current_task->current_module = task_last_m;
        jl_rethrow();
    }
    ptls->current_module = last_m;
    ptls->current_task->current_module = task_last_m;
    assert(v);
    return v;
}

static jl_value_t *do_call(jl_value_t **args, size_t nargs, interpreter_state *s)
{
    jl_value_t **argv;
    JL_GC_PUSHARGS(argv, nargs);
    size_t i;
    for(i=0; i < nargs; i++)
        argv[i] = eval(args[i], s);
    jl_value_t *result = jl_apply_generic(argv, nargs);
    JL_GC_POP();
    return result;
}

static jl_value_t *do_invoke(jl_value_t **args, size_t nargs, interpreter_state *s)
{
    jl_value_t **argv;
    JL_GC_PUSHARGS(argv, nargs - 1);
    size_t i;
    for (i = 1; i < nargs; i++)
        argv[i - 1] = eval(args[i], s);
    jl_method_instance_t *meth = (jl_method_instance_t*)args[0];
    assert(jl_is_method_instance(meth) && !meth->inInference);
    jl_value_t *result = jl_call_method_internal(meth, argv, nargs - 1);
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

extern int jl_boot_file_loaded;
extern int inside_typedef;

// this is a heuristic for allowing "redefining" a type to something identical
static int equiv_svec_dt(jl_svec_t *sa, jl_svec_t *sb)
{
    size_t i, l = jl_svec_len(sa);
    if (l != jl_svec_len(sb)) return 0;
    for (i = 0; i < l; i++) {
        jl_value_t *a = jl_svecref(sa, i);
        jl_value_t *b = jl_svecref(sb, i);
        if (jl_typeof(a) != jl_typeof(b))
            return 0;
        if (jl_is_typevar(a) && ((jl_tvar_t*)a)->name != ((jl_tvar_t*)b)->name)
            return 0;
        if (!jl_subtype(a, b) || !jl_subtype(b, a))
            return 0;
    }
    return 1;
}
static int equiv_type(jl_datatype_t *dta, jl_datatype_t *dtb)
{
    // TODO: revisit after jb/subtype
    return (jl_typeof(dta) == jl_typeof(dtb) &&
            dta->name->name == dtb->name->name &&
            dta->abstract == dtb->abstract &&
            dta->mutabl == dtb->mutabl &&
            dta->size == dtb->size &&
            dta->ninitialized == dtb->ninitialized &&
            equiv_svec_dt(dta->parameters, dtb->parameters) &&
            equiv_svec_dt(dta->types, dtb->types) &&
            jl_subtype((jl_value_t*)dta->super, (jl_value_t*)dtb->super) &&
            jl_subtype((jl_value_t*)dtb->super, (jl_value_t*)dta->super) &&
            jl_egal((jl_value_t*)dta->name->names, (jl_value_t*)dtb->name->names));
}

static void check_can_assign_type(jl_binding_t *b)
{
    if (b->constp && b->value != NULL && !jl_is_datatype(b->value))
        jl_errorf("invalid redefinition of constant %s",
                  jl_symbol_name(b->name));
}

void jl_reinstantiate_inner_types(jl_datatype_t *t);
void jl_reset_instantiate_inner_types(jl_datatype_t *t);

void jl_set_datatype_super(jl_datatype_t *tt, jl_value_t *super)
{
    if (!jl_is_datatype(super) || !jl_is_abstracttype(super) ||
        tt->name == ((jl_datatype_t*)super)->name ||
        jl_subtype(super,(jl_value_t*)jl_vararg_type) ||
        jl_is_tuple_type(super) ||
        jl_subtype(super,(jl_value_t*)jl_type_type) ||
        super == (jl_value_t*)jl_builtin_type) {
        jl_errorf("invalid subtyping in definition of %s",
                  jl_symbol_name(tt->name->name));
    }
    tt->super = (jl_datatype_t*)super;
    jl_gc_wb(tt, tt->super);
}

static int jl_source_nslots(jl_code_info_t *src)
{
    return jl_array_len(src->slotflags);
}

static int jl_source_nssavalues(jl_code_info_t *src)
{
    return jl_is_long(src->ssavaluetypes) ? jl_unbox_long(src->ssavaluetypes) : jl_array_len(src->ssavaluetypes);
}

static jl_value_t *eval(jl_value_t *e, interpreter_state *s)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_code_info_t *src = s==NULL ? NULL : s->src;
    if (jl_is_ssavalue(e)) {
        ssize_t id = ((jl_ssavalue_t*)e)->id;
        if (id >= jl_source_nssavalues(src) || id < 0 || s->locals == NULL)
            jl_error("access to invalid SSAValue");
        else
            return s->locals[jl_source_nslots(src) + id];
    }
    if (jl_is_slot(e)) {
        ssize_t n = jl_slot_number(e);
        if (n > jl_source_nslots(src) || n < 1 || s->locals == NULL)
            jl_error("access to invalid slot number");
        jl_value_t *v = s->locals[n-1];
        if (v == NULL)
            jl_undefined_var_error((jl_sym_t*)jl_array_ptr_ref(src->slotnames, n - 1));
        return v;
    }
    if (jl_is_globalref(e)) {
        jl_sym_t *s = jl_globalref_name(e);
        jl_value_t *v = jl_get_global(jl_globalref_mod(e), s);
        if (v == NULL)
            jl_undefined_var_error(s);
        return v;
    }
    if (jl_is_quotenode(e))
        return jl_fieldref(e,0);
    jl_module_t *modu = (s == NULL ? ptls->current_module : s->module);
    if (jl_is_symbol(e)) {  // bare symbols appear in toplevel exprs not wrapped in `thunk`
        jl_value_t *v = jl_get_global(modu, (jl_sym_t*)e);
        if (v == NULL)
            jl_undefined_var_error((jl_sym_t*)e);
        return v;
    }
    if (!jl_is_expr(e))
        return e;
    jl_expr_t *ex = (jl_expr_t*)e;
    jl_value_t **args = (jl_value_t**)jl_array_data(ex->args);
    size_t nargs = jl_array_len(ex->args);
    if (ex->head == call_sym) {
        return do_call(args, nargs, s);
    }
    else if (ex->head == invoke_sym) {
        return do_invoke(args, nargs, s);
    }
    else if (ex->head == new_sym) {
        jl_value_t *thetype = eval(args[0], s);
        jl_value_t *v=NULL;
        JL_GC_PUSH2(&thetype, &v);
        assert(jl_is_structtype(thetype));
        v = jl_new_struct_uninit((jl_datatype_t*)thetype);
        for(size_t i=1; i < nargs; i++) {
            jl_set_nth_field(v, i-1, eval(args[i], s));
        }
        JL_GC_POP();
        return v;
    }
    else if (ex->head == static_parameter_sym) {
        ssize_t n = jl_unbox_long(args[0]);
        assert(n > 0);
        if (s->sparam_vals && n <= jl_svec_len(s->sparam_vals)) {
            return jl_svecref(s->sparam_vals, n - 1);
        }
        // static parameter val unknown needs to be an error for ccall
        jl_error("could not determine static parameter value");
    }
    else if (ex->head == inert_sym) {
        return args[0];
    }
    else if (ex->head == copyast_sym) {
        return jl_copy_ast(eval(args[0], s));
    }
    else if (ex->head == exc_sym) {
        return ptls->exception_in_transit;
    }
    else if (ex->head == method_sym) {
        jl_sym_t *fname = (jl_sym_t*)args[0];
        if (jl_is_globalref(fname)) {
            modu = jl_globalref_mod(fname);
            fname = jl_globalref_name(fname);
        }
        assert(jl_expr_nargs(ex) != 1 || jl_is_symbol(fname));

        if (jl_is_symbol(fname)) {
            jl_value_t **bp=NULL;
            jl_value_t *bp_owner=NULL;
            jl_binding_t *b=NULL;
            if (bp == NULL) {
                b = jl_get_binding_for_method_def(modu, fname);
                bp = &b->value;
                bp_owner = (jl_value_t*)modu;
            }
            jl_value_t *gf = jl_generic_function_def(fname, bp, bp_owner, b);
            if (jl_expr_nargs(ex) == 1)
                return gf;
        }

        jl_value_t *atypes=NULL, *meth=NULL;
        JL_GC_PUSH2(&atypes, &meth);
        atypes = eval(args[1], s);
        meth = eval(args[2], s);
        jl_method_def((jl_svec_t*)atypes, (jl_code_info_t*)meth, args[3]);
        JL_GC_POP();
        return jl_nothing;
    }
    else if (ex->head == const_sym) {
        jl_sym_t *sym = (jl_sym_t*)args[0];
        if (jl_is_globalref(sym)) {
            modu = jl_globalref_mod(sym);
            sym = jl_globalref_name(sym);
        }
        assert(jl_is_symbol(sym));
        jl_binding_t *b = jl_get_binding_wr(modu, sym);
        jl_declare_constant(b);
        return (jl_value_t*)jl_nothing;
    }
    else if (ex->head == global_sym) {
        // create uninitialized mutable binding for "global x" decl
        // TODO: handle type decls
        size_t i, l = jl_array_len(ex->args);
        for (i = 0; i < l; i++) {
            jl_sym_t *gsym = (jl_sym_t*)args[i];
            jl_module_t *gmodu = modu;
            if (jl_is_globalref(gsym)) {
                gmodu = jl_globalref_mod(gsym);
                gsym = jl_globalref_name(gsym);
            }
            assert(jl_is_symbol(gsym));
            jl_get_binding_wr(gmodu, gsym);
        }
        return (jl_value_t*)jl_nothing;
    }
    else if (ex->head == abstracttype_sym) {
        if (inside_typedef)
            jl_error("cannot eval a new abstract type definition while defining another type");
        jl_value_t *name = args[0];
        jl_value_t *para = eval(args[1], s);
        jl_value_t *super = NULL;
        jl_value_t *temp = NULL;
        jl_datatype_t *dt = NULL;
        jl_value_t *w = NULL;
        JL_GC_PUSH4(&para, &super, &temp, &w);
        assert(jl_is_svec(para));
        if (jl_is_globalref(name)) {
            modu = jl_globalref_mod(name);
            name = (jl_value_t*)jl_globalref_name(name);
        }
        assert(jl_is_symbol(name));
        dt = jl_new_abstracttype(name, NULL, (jl_svec_t*)para);
        w = dt->name->wrapper;
        jl_binding_t *b = jl_get_binding_wr(modu, (jl_sym_t*)name);
        temp = b->value;
        check_can_assign_type(b);
        b->value = w;
        jl_gc_wb_binding(b, w);
        JL_TRY {
            inside_typedef = 1;
            super = eval(args[2], s);
            jl_set_datatype_super(dt, super);
            jl_reinstantiate_inner_types(dt);
        }
        JL_CATCH {
            jl_reset_instantiate_inner_types(dt);
            b->value = temp;
            jl_rethrow();
        }
        b->value = temp;
        if (temp == NULL || !equiv_type(dt, (jl_datatype_t*)jl_unwrap_unionall(temp))) {
            jl_checked_assignment(b, w);
        }
        JL_GC_POP();
        return (jl_value_t*)jl_nothing;
    }
    else if (ex->head == bitstype_sym) {
        if (inside_typedef)
            jl_error("cannot eval a new bits type definition while defining another type");
        jl_value_t *name = args[0];
        jl_value_t *super = NULL, *para = NULL, *vnb = NULL, *temp = NULL;
        jl_datatype_t *dt = NULL;
        jl_value_t *w = NULL;
        JL_GC_PUSH4(&para, &super, &temp, &w);
        if (jl_is_globalref(name)) {
            modu = jl_globalref_mod(name);
            name = (jl_value_t*)jl_globalref_name(name);
        }
        assert(jl_is_symbol(name));
        para = eval(args[1], s);
        assert(jl_is_svec(para));
        vnb  = eval(args[2], s);
        if (!jl_is_long(vnb))
            jl_errorf("invalid declaration of bits type %s",
                      jl_symbol_name((jl_sym_t*)name));
        ssize_t nb = jl_unbox_long(vnb);
        if (nb < 1 || nb>=(1<<23) || (nb&7) != 0)
            jl_errorf("invalid number of bits in type %s",
                      jl_symbol_name((jl_sym_t*)name));
        dt = jl_new_bitstype(name, NULL, (jl_svec_t*)para, nb);
        w = dt->name->wrapper;
        jl_binding_t *b = jl_get_binding_wr(modu, (jl_sym_t*)name);
        temp = b->value;
        check_can_assign_type(b);
        b->value = w;
        jl_gc_wb_binding(b, w);
        JL_TRY {
            inside_typedef = 1;
            super = eval(args[3], s);
            jl_set_datatype_super(dt, super);
            jl_reinstantiate_inner_types(dt);
        }
        JL_CATCH {
            jl_reset_instantiate_inner_types(dt);
            b->value = temp;
            jl_rethrow();
        }
        b->value = temp;
        if (temp == NULL || !equiv_type(dt, (jl_datatype_t*)jl_unwrap_unionall(temp))) {
            jl_checked_assignment(b, w);
        }
        JL_GC_POP();
        return (jl_value_t*)jl_nothing;
    }
    else if (ex->head == compositetype_sym) {
        if (inside_typedef)
            jl_error("cannot eval a new data type definition while defining another type");
        jl_value_t *name = args[0];
        jl_value_t *para = eval(args[1], s);
        jl_value_t *temp = NULL;
        jl_value_t *super = NULL;
        jl_datatype_t *dt = NULL;
        jl_value_t *w = NULL;
        JL_GC_PUSH4(&para, &super, &temp, &w);
        if (jl_is_globalref(name)) {
            modu = jl_globalref_mod(name);
            name = (jl_value_t*)jl_globalref_name(name);
        }
        assert(jl_is_symbol(name));
        assert(jl_is_svec(para));
        temp = eval(args[2], s);  // field names
        dt = jl_new_datatype((jl_sym_t*)name, NULL, (jl_svec_t*)para,
                             (jl_svec_t*)temp, NULL,
                             0, args[5]==jl_true ? 1 : 0, jl_unbox_long(args[6]));
        w = dt->name->wrapper;

        jl_binding_t *b = jl_get_binding_wr(modu, (jl_sym_t*)name);
        temp = b->value;  // save old value
        // temporarily assign so binding is available for field types
        check_can_assign_type(b);
        b->value = w;
        jl_gc_wb_binding(b,w);

        JL_TRY {
            inside_typedef = 1;
            // operations that can fail
            super = eval(args[3], s);
            jl_set_datatype_super(dt, super);
            dt->types = (jl_svec_t*)eval(args[4], s);
            jl_gc_wb(dt, dt->types);
            for(size_t i=0; i < jl_svec_len(dt->types); i++) {
                jl_value_t *elt = jl_svecref(dt->types, i);
                if (!jl_is_type(elt) && !jl_is_typevar(elt))
                    jl_type_error_rt(jl_symbol_name(dt->name->name),
                                     "type definition",
                                     (jl_value_t*)jl_type_type, elt);
            }
            jl_reinstantiate_inner_types(dt);
        }
        JL_CATCH {
            jl_reset_instantiate_inner_types(dt);
            b->value = temp;
            jl_rethrow();
        }
        if (dt->name->names == jl_emptysvec)
            dt->layout = jl_void_type->layout; // reuse the same layout for all singletons
        else if (jl_is_leaf_type((jl_value_t*)dt))
            jl_compute_field_offsets(dt);
        if (para == (jl_value_t*)jl_emptysvec && jl_is_datatype_make_singleton(dt)) {
            dt->instance = jl_gc_alloc(ptls, 0, dt);
            jl_gc_wb(dt, dt->instance);
        }

        b->value = temp;
        if (temp == NULL || !equiv_type(dt, (jl_datatype_t*)jl_unwrap_unionall(temp))) {
            jl_checked_assignment(b, w);
        }

        JL_GC_POP();
        return (jl_value_t*)jl_nothing;
    }
    else if (ex->head == module_sym) {
        return jl_eval_module_expr(ex);
    }
    else if (ex->head == thunk_sym) {
        return jl_toplevel_eval((jl_value_t*)ex);
    }
    else if (ex->head == error_sym || ex->head == jl_incomplete_sym) {
        if (nargs == 0)
            jl_error("malformed \"error\" expression");
        if (jl_is_string(args[0]))
            jl_errorf("syntax: %s", jl_string_data(args[0]));
        jl_throw(args[0]);
    }
    else if (ex->head == boundscheck_sym || ex->head == inbounds_sym || ex->head == fastmath_sym ||
             ex->head == simdloop_sym || ex->head == meta_sym) {
        return jl_nothing;
    }
    jl_errorf("unsupported or misplaced expression %s", jl_symbol_name(ex->head));
    return (jl_value_t*)jl_nothing;
}

jl_value_t *jl_toplevel_eval_body(jl_array_t *stmts)
{
    size_t last_age = jl_get_ptls_states()->world_age;
    jl_value_t *ret = eval_body(stmts, NULL, 0, 1);
    jl_get_ptls_states()->world_age = last_age;
    return ret;
}

static jl_value_t *eval_body(jl_array_t *stmts, interpreter_state *s, int start, int toplevel)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_handler_t __eh;
    size_t i=start, ns = jl_array_len(stmts);

    while (1) {
        if (i >= ns)
            jl_error("`body` expression must terminate in `return`. Use `block` instead.");
        if (toplevel)
            jl_get_ptls_states()->world_age = jl_world_counter;
        jl_value_t *stmt = jl_array_ptr_ref(stmts, i);
        if (jl_is_gotonode(stmt)) {
            i = jl_gotonode_label(stmt) - 1;
            continue;
        }
        else if (jl_is_expr(stmt)) {
            jl_sym_t *head = ((jl_expr_t*)stmt)->head;
            if (head == return_sym) {
                jl_value_t *ex = jl_exprarg(stmt, 0);
                if (toplevel && jl_is_toplevel_only_expr(ex))
                    return jl_toplevel_eval(ex);
                else
                    return eval(ex, s);
            }
            else if (head == assign_sym) {
                jl_value_t *sym = jl_exprarg(stmt, 0);
                jl_value_t *rhs = eval(jl_exprarg(stmt, 1), s);
                if (jl_is_ssavalue(sym)) {
                    ssize_t genid = ((jl_ssavalue_t*)sym)->id;
                    if (genid >= jl_source_nssavalues(s->src) || genid < 0)
                        jl_error("assignment to invalid GenSym location");
                    s->locals[jl_source_nslots(s->src) + genid] = rhs;
                }
                else if (jl_is_slot(sym)) {
                    ssize_t n = jl_slot_number(sym);
                    assert(n <= jl_source_nslots(s->src) && n > 0);
                    s->locals[n-1] = rhs;
                }
                else {
                    jl_module_t *m;
                    if (jl_is_globalref(sym)) {
                        m = jl_globalref_mod(sym);
                        sym = (jl_value_t*)jl_globalref_name(sym);
                    }
                    else {
                        m = (s == NULL ? ptls->current_module : s->module);
                    }
                    assert(jl_is_symbol(sym));
                    JL_GC_PUSH1(&rhs);
                    jl_binding_t *b = jl_get_binding_wr(m, (jl_sym_t*)sym);
                    jl_checked_assignment(b, rhs);
                    JL_GC_POP();
                }
            }
            else if (head == goto_ifnot_sym) {
                jl_value_t *cond = eval(jl_exprarg(stmt, 0), s);
                if (cond == jl_false) {
                    i = jl_unbox_long(jl_exprarg(stmt, 1)) - 1;
                    continue;
                }
                else if (cond != jl_true) {
                    jl_type_error_rt("toplevel", "if", (jl_value_t*)jl_bool_type, cond);
                }
            }
            else if (head == line_sym) {
                if (toplevel)
                    jl_lineno = jl_unbox_long(jl_exprarg(stmt, 0));
                // TODO: interpreted function line numbers
            }
            else if (head == enter_sym) {
                jl_enter_handler(&__eh);
                if (!jl_setjmp(__eh.eh_ctx,1)) {
                    return eval_body(stmts, s, i + 1, toplevel);
                }
                else {
#ifdef _OS_WINDOWS_
                    if (ptls->exception_in_transit == jl_stackovf_exception)
                        _resetstkoflw();
#endif
                    i = jl_unbox_long(jl_exprarg(stmt, 0)) - 1;
                    continue;
                }
            }
            else if (head == leave_sym) {
                int hand_n_leave = jl_unbox_long(jl_exprarg(stmt,0));
                jl_pop_handler(hand_n_leave);
            }
            else if (toplevel && jl_is_toplevel_only_expr(stmt)) {
                jl_toplevel_eval(stmt);
            }
            else {
                eval(stmt, s);
            }
        }
        else if (jl_is_linenode(stmt)) {
            if (toplevel)
                jl_lineno = jl_linenode_line(stmt);
            // TODO: interpreted function line numbers
        }
        else if (jl_is_newvarnode(stmt)) {
            jl_value_t *var = jl_fieldref(stmt, 0);
            assert(jl_is_slot(var));
            ssize_t n = jl_slot_number(var);
            assert(n <= jl_source_nslots(s->src) && n > 0);
            s->locals[n - 1] = NULL;
        }
        else {
            eval(stmt, s);
        }
        i++;
    }
    assert(0);
    return NULL;
}

jl_value_t *jl_interpret_call(jl_method_instance_t *lam, jl_value_t **args, uint32_t nargs)
{
    if (lam->jlcall_api == 2)
        return lam->inferred;
    jl_code_info_t *src = (jl_code_info_t*)lam->inferred;
    if (src == NULL || !jl_is_code_info(src)) {
        if (lam->def->isstaged) {
            src = jl_code_for_staged(lam);
            lam->inferred = (jl_value_t*)src;
            jl_gc_wb(lam, src);
        }
        else {
            src = lam->def->source;
        }
    }
    jl_array_t *stmts = src->code;
    if (!jl_typeis(stmts, jl_array_any_type)) {
        stmts = jl_uncompress_ast(lam->def, stmts);
        src->code = stmts;
        jl_gc_wb(src, stmts);
    }
    assert(jl_typeis(stmts, jl_array_any_type));
    jl_value_t **locals;
    JL_GC_PUSHARGS(locals, jl_source_nslots(src) + jl_source_nssavalues(src) + 2);
    locals[0] = (jl_value_t*)src;
    locals[1] = (jl_value_t*)stmts;
    interpreter_state s;
    s.src = src;
    s.module = lam->def->module;
    s.locals = locals + 2;
    s.sparam_vals = lam->sparam_vals;
    size_t i;
    for (i = 0; i < lam->def->nargs; i++) {
        if (lam->def->isva && i == lam->def->nargs - 1)
            s.locals[i] = jl_f_tuple(NULL, &args[i], nargs - i);
        else
            s.locals[i] = args[i];
    }
    jl_value_t *r = eval_body(stmts, &s, 0, 0);
    JL_GC_POP();
    return r;
}

jl_value_t *jl_interpret_toplevel_thunk(jl_code_info_t *src)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_array_t *stmts = src->code;
    assert(jl_typeis(stmts, jl_array_any_type));
    jl_value_t **locals;
    JL_GC_PUSHARGS(locals, jl_source_nslots(src) + jl_source_nssavalues(src));
    interpreter_state s;
    s.src = src;
    s.locals = locals;
    s.module = ptls->current_module;
    s.sparam_vals = jl_emptysvec;
    size_t last_age = jl_get_ptls_states()->world_age;
    jl_value_t *r = eval_body(stmts, &s, 0, 1);
    jl_get_ptls_states()->world_age = last_age;
    JL_GC_POP();
    return r;
}

#ifdef __cplusplus
}
#endif

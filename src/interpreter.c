// This file is a part of Julia. License is MIT: https://julialang.org/license

#include <stdlib.h>
#include <setjmp.h>
#ifdef _OS_WINDOWS_
#include <malloc.h>
#endif
#include "julia.h"
#include "julia_internal.h"
#include "builtin_proto.h"
#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

typedef struct {
    jl_code_info_t *src; // contains the names and number of slots
    jl_module_t *module; // context for globals
    jl_value_t **locals; // slots for holding local slots and ssavalues
    jl_svec_t *sparam_vals; // method static parameters, if eval-ing a method body
    int preevaluation; // use special rules for pre-evaluating expressions
} interpreter_state;

static jl_value_t *eval(jl_value_t *e, interpreter_state *s);
static jl_value_t *eval_body(jl_array_t *stmts, interpreter_state *s, int start, int toplevel);

jl_value_t *jl_eval_module_expr(jl_module_t *m, jl_expr_t *ex);
int jl_is_toplevel_only_expr(jl_value_t *e);

// deprecated: do not use this method in new code
// it uses special scoping / evaluation / error rules
// which should instead be handled in lowering
jl_value_t *jl_interpret_toplevel_expr_in(jl_module_t *m, jl_value_t *e,
                                          jl_code_info_t *src,
                                          jl_svec_t *sparam_vals)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_value_t *v=NULL;
    jl_module_t *last_m = ptls->current_module;
    jl_module_t *task_last_m = ptls->current_task->current_module;
    interpreter_state s = {};
    s.src = src;
    s.module = m;
    s.sparam_vals = sparam_vals;
    s.preevaluation = (sparam_vals != NULL);

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
    assert(jl_is_method_instance(meth));
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
static int equiv_type(jl_datatype_t *dta, jl_datatype_t *dtb)
{
    if (!(jl_typeof(dta) == jl_typeof(dtb) &&
          dta->name->name == dtb->name->name &&
          dta->abstract == dtb->abstract &&
          dta->mutabl == dtb->mutabl &&
          dta->size == dtb->size &&
          dta->ninitialized == dtb->ninitialized &&
          jl_egal((jl_value_t*)jl_field_names(dta), (jl_value_t*)jl_field_names(dtb)) &&
          jl_nparams(dta) == jl_nparams(dtb) &&
          jl_field_count(dta) == jl_field_count(dtb)))
        return 0;
    jl_value_t *a=NULL, *b=NULL;
    int ok = 1;
    size_t i, nf = jl_field_count(dta);
    JL_GC_PUSH2(&a, &b);
    a = jl_rewrap_unionall((jl_value_t*)dta->super, dta->name->wrapper);
    b = jl_rewrap_unionall((jl_value_t*)dtb->super, dtb->name->wrapper);
    if (!jl_types_equal(a, b))
        goto no;
    JL_TRY {
        a = jl_apply_type(dtb->name->wrapper, jl_svec_data(dta->parameters), jl_nparams(dta));
    }
    JL_CATCH {
        ok = 0;
    }
    if (!ok) goto no;
    assert(jl_is_datatype(a));
    a = dta->name->wrapper;
    b = dtb->name->wrapper;
    while (jl_is_unionall(a)) {
        jl_unionall_t *ua = (jl_unionall_t*)a;
        jl_unionall_t *ub = (jl_unionall_t*)b;
        if (!jl_egal(ua->var->lb, ub->var->lb) || !jl_egal(ua->var->ub, ub->var->ub) ||
            ua->var->name != ub->var->name)
            goto no;
        a = jl_instantiate_unionall(ua, (jl_value_t*)ub->var);
        b = ub->body;
    }
    assert(jl_is_datatype(a) && jl_is_datatype(b));
    for (i=0; i < nf; i++) {
        jl_value_t *ta = jl_svecref(((jl_datatype_t*)a)->types, i);
        jl_value_t *tb = jl_svecref(((jl_datatype_t*)b)->types, i);
        if (jl_has_free_typevars(ta)) {
            if (!jl_has_free_typevars(tb) || !jl_egal(ta, tb))
                goto no;
        }
        else if (jl_has_free_typevars(tb) || jl_typeof(ta) != jl_typeof(tb) ||
                 !jl_types_equal(ta, tb)) {
            goto no;
        }
    }
    JL_GC_POP();
    return 1;
 no:
    JL_GC_POP();
    return 0;
}

static void check_can_assign_type(jl_binding_t *b, jl_value_t *rhs)
{
    if (b->constp && b->value != NULL && jl_typeof(b->value) != jl_typeof(rhs))
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
    jl_code_info_t *src = s->src;
    if (jl_is_ssavalue(e)) {
        ssize_t id = ((jl_ssavalue_t*)e)->id;
        if (src == NULL || id >= jl_source_nssavalues(src) || id < 0 || s->locals == NULL)
            jl_error("access to invalid SSAValue");
        else
            return s->locals[jl_source_nslots(src) + id];
    }
    if (jl_is_slot(e)) {
        ssize_t n = jl_slot_number(e);
        if (src == NULL || n > jl_source_nslots(src) || n < 1 || s->locals == NULL)
            jl_error("access to invalid slot number");
        jl_value_t *v = s->locals[n - 1];
        if (v == NULL)
            jl_undefined_var_error((jl_sym_t*)jl_array_ptr_ref(src->slotnames, n - 1));
        return v;
    }
    if (jl_is_globalref(e)) {
        return jl_eval_global_var(jl_globalref_mod(e), jl_globalref_name(e));
    }
    if (jl_is_quotenode(e))
        return jl_fieldref(e,0);
    jl_module_t *modu = s->module;
    if (jl_is_symbol(e)) {  // bare symbols appear in toplevel exprs not wrapped in `thunk`
        return jl_eval_global_var(modu, (jl_sym_t*)e);
    }
    if (!jl_is_expr(e))
        return e;
    jl_expr_t *ex = (jl_expr_t*)e;
    jl_value_t **args = (jl_value_t**)jl_array_data(ex->args);
    size_t nargs = jl_array_len(ex->args);
    if (ex->head == isdefined_sym) {
        jl_value_t *sym = args[0];
        int defined = 0;
        if (jl_is_slot(sym)) {
            ssize_t n = jl_slot_number(sym);
            if (src == NULL || n > jl_source_nslots(src) || n < 1 || s->locals == NULL)
                jl_error("access to invalid slot number");
            defined = s->locals[n - 1] != NULL;
        }
        else if (jl_is_globalref(sym)) {
            defined = jl_boundp(jl_globalref_mod(sym), jl_globalref_name(sym));
        }
        else if (jl_is_symbol(sym)) {
            defined = jl_boundp(modu, (jl_sym_t*)sym);
        }
        else if (jl_is_expr(sym) && ((jl_expr_t*)sym)->head == static_parameter_sym) {
            ssize_t n = jl_unbox_long(args[0]);
            assert(n > 0);
            if (s->sparam_vals && n <= jl_svec_len(s->sparam_vals)) {
                jl_value_t *sp = jl_svecref(s->sparam_vals, n - 1);
                defined = !jl_is_typevar(sp);
            }
            else {
                // static parameter val unknown needs to be an error for ccall
                jl_error("could not determine static parameter value");
            }
        }
        else {
            assert(0 && "malformed isdefined expression");
        }
        return defined ? jl_true : jl_false;
    }
    else if (ex->head == call_sym) {
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
        for (size_t i = 1; i < nargs; i++) {
            jl_value_t *ft = jl_field_type(thetype, i - 1);
            jl_value_t *fldv = eval(args[i], s);
            if (!jl_isa(fldv, ft))
                jl_type_error("new", ft, fldv);
            jl_set_nth_field(v, i - 1, fldv);
        }
        JL_GC_POP();
        return v;
    }
    else if (ex->head == static_parameter_sym) {
        ssize_t n = jl_unbox_long(args[0]);
        assert(n > 0);
        if (s->sparam_vals && n <= jl_svec_len(s->sparam_vals)) {
            jl_value_t *sp = jl_svecref(s->sparam_vals, n - 1);
            if (jl_is_typevar(sp) && !s->preevaluation)
                jl_undefined_var_error(((jl_tvar_t*)sp)->name);
            return sp;
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
            jl_value_t *bp_owner = (jl_value_t*)modu;
            jl_binding_t *b = jl_get_binding_for_method_def(modu, fname);
            jl_value_t **bp = &b->value;
            jl_value_t *gf = jl_generic_function_def(b->name, b->owner, bp, bp_owner, b);
            if (jl_expr_nargs(ex) == 1)
                return gf;
        }

        jl_value_t *atypes = NULL, *meth = NULL;
        JL_GC_PUSH2(&atypes, &meth);
        atypes = eval(args[1], s);
        meth = eval(args[2], s);
        jl_method_def((jl_svec_t*)atypes, (jl_code_info_t*)meth, s->module, args[3]);
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
        jl_binding_t *b = jl_get_binding_wr(modu, sym, 1);
        jl_declare_constant(b);
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
        dt = jl_new_abstracttype(name, modu, NULL, (jl_svec_t*)para);
        w = dt->name->wrapper;
        jl_binding_t *b = jl_get_binding_wr(modu, (jl_sym_t*)name, 1);
        temp = b->value;
        check_can_assign_type(b, w);
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
    else if (ex->head == primtype_sym) {
        if (inside_typedef)
            jl_error("cannot eval a new primitive type definition while defining another type");
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
            jl_errorf("invalid declaration of primitive type %s",
                      jl_symbol_name((jl_sym_t*)name));
        ssize_t nb = jl_unbox_long(vnb);
        if (nb < 1 || nb >= (1 << 23) || (nb & 7) != 0)
            jl_errorf("invalid number of bits in primitive type %s",
                      jl_symbol_name((jl_sym_t*)name));
        dt = jl_new_primitivetype(name, modu, NULL, (jl_svec_t*)para, nb);
        w = dt->name->wrapper;
        jl_binding_t *b = jl_get_binding_wr(modu, (jl_sym_t*)name, 1);
        temp = b->value;
        check_can_assign_type(b, w);
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
    else if (ex->head == structtype_sym) {
        if (inside_typedef)
            jl_error("cannot eval a new struct type definition while defining another type");
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
        dt = jl_new_datatype((jl_sym_t*)name, modu, NULL, (jl_svec_t*)para,
                             (jl_svec_t*)temp, NULL,
                             0, args[5]==jl_true ? 1 : 0, jl_unbox_long(args[6]));
        w = dt->name->wrapper;

        jl_binding_t *b = jl_get_binding_wr(modu, (jl_sym_t*)name, 1);
        temp = b->value;  // save old value
        // temporarily assign so binding is available for field types
        check_can_assign_type(b, w);
        b->value = w;
        jl_gc_wb_binding(b,w);

        JL_TRY {
            inside_typedef = 1;
            // operations that can fail
            super = eval(args[3], s);
            jl_set_datatype_super(dt, super);
            dt->types = (jl_svec_t*)eval(args[4], s);
            jl_gc_wb(dt, dt->types);
            for (size_t i = 0; i < jl_svec_len(dt->types); i++) {
                jl_value_t *elt = jl_svecref(dt->types, i);
                if ((!jl_is_type(elt) && !jl_is_typevar(elt)) || jl_is_vararg_type(elt)) {
                    jl_type_error_rt(jl_symbol_name(dt->name->name),
                                     "type definition",
                                     (jl_value_t*)jl_type_type, elt);
                }
            }
            jl_reinstantiate_inner_types(dt);
        }
        JL_CATCH {
            jl_reset_instantiate_inner_types(dt);
            b->value = temp;
            jl_rethrow();
        }
        jl_compute_field_offsets(dt);

        b->value = temp;
        if (temp == NULL || !equiv_type(dt, (jl_datatype_t*)jl_unwrap_unionall(temp))) {
            jl_checked_assignment(b, w);
        }

        JL_GC_POP();
        return (jl_value_t*)jl_nothing;
    }
    else if (ex->head == module_sym) {
        return jl_eval_module_expr(s->module, ex);
    }
    else if (ex->head == thunk_sym) {
        return jl_toplevel_eval(s->module, (jl_value_t*)ex);
    }
    else if (ex->head == error_sym || ex->head == jl_incomplete_sym) {
        if (nargs == 0)
            jl_error("malformed \"error\" expression");
        if (jl_is_string(args[0]))
            jl_errorf("syntax: %s", jl_string_data(args[0]));
        jl_throw(args[0]);
    }
    else if (ex->head == boundscheck_sym) {
        return jl_true;
    }
    else if (ex->head == boundscheck_sym || ex->head == inbounds_sym || ex->head == fastmath_sym ||
             ex->head == simdloop_sym || ex->head == meta_sym) {
        return jl_nothing;
    } else if (ex->head == gc_preserve_begin_sym || ex->head == gc_preserve_end_sym) {
        // The interpreter generally keeps values that were assigned in this scope
        // rooted. If the interpreter learns to be more agressive here, we may
        // want to explicitly root these values.
        return jl_nothing;
    }
    jl_errorf("unsupported or misplaced expression %s", jl_symbol_name(ex->head));
    abort();
}

jl_value_t *jl_toplevel_eval_body(jl_module_t *m, jl_array_t *stmts)
{
    size_t last_age = jl_get_ptls_states()->world_age;
    interpreter_state s = {};
    s.module = m;
    jl_value_t *ret = eval_body(stmts, &s, 0, 1);
    jl_get_ptls_states()->world_age = last_age;
    return ret;
}

static jl_value_t *eval_body(jl_array_t *stmts, interpreter_state *s, int start, int toplevel)
{
    jl_handler_t __eh;
    size_t i = start;
    size_t ns = jl_array_len(stmts);

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
                    return jl_toplevel_eval(s->module, ex);
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
                    jl_module_t *modu = s->module;
                    if (jl_is_globalref(sym)) {
                        modu = jl_globalref_mod(sym);
                        sym = (jl_value_t*)jl_globalref_name(sym);
                    }
                    assert(jl_is_symbol(sym));
                    JL_GC_PUSH1(&rhs);
                    jl_binding_t *b = jl_get_binding_wr(modu, (jl_sym_t*)sym, 1);
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
            else if (head == enter_sym) {
                jl_enter_handler(&__eh);
                if (!jl_setjmp(__eh.eh_ctx,1)) {
                    return eval_body(stmts, s, i + 1, toplevel);
                }
                else {
#ifdef _OS_WINDOWS_
                    if (jl_get_ptls_states()->exception_in_transit == jl_stackovf_exception)
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
                jl_toplevel_eval(s->module, stmt);
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
        return lam->inferred_const;
    jl_code_info_t *src = (jl_code_info_t*)lam->inferred;
    if (!src || (jl_value_t*)src == jl_nothing) {
        if (lam->def.method->source) {
            src = (jl_code_info_t*)lam->def.method->source;
        }
        else {
            assert(lam->def.method->generator);
            src = jl_code_for_staged(lam);
            lam->inferred = (jl_value_t*)src;
            jl_gc_wb(lam, src);
        }
    }
    if (src && (jl_value_t*)src != jl_nothing) {
        src = jl_uncompress_ast(lam->def.method, (jl_array_t*)src);
        lam->inferred = (jl_value_t*)src;
        jl_gc_wb(lam, src);
    }
    if (!src || !jl_is_code_info(src)) {
        jl_error("source missing for method called in interpreter");
    }

    jl_array_t *stmts = src->code;
    assert(jl_typeis(stmts, jl_array_any_type));
    jl_value_t **locals;
    JL_GC_PUSHARGS(locals, jl_source_nslots(src) + jl_source_nssavalues(src) + 2);
    locals[0] = (jl_value_t*)src;
    locals[1] = (jl_value_t*)stmts;
    interpreter_state s = {};
    s.src = src;
    s.module = lam->def.method->module;
    s.locals = locals + 2;
    s.sparam_vals = lam->sparam_vals;
    size_t i;
    for (i = 0; i < lam->def.method->nargs; i++) {
        if (lam->def.method->isva && i == lam->def.method->nargs - 1)
            s.locals[i] = jl_f_tuple(NULL, &args[i], nargs - i);
        else
            s.locals[i] = args[i];
    }
    jl_value_t *r = eval_body(stmts, &s, 0, 0);
    JL_GC_POP();
    return r;
}

jl_value_t *jl_interpret_toplevel_thunk(jl_module_t *m, jl_code_info_t *src)
{
    jl_array_t *stmts = src->code;
    assert(jl_typeis(stmts, jl_array_any_type));
    jl_value_t **locals;
    JL_GC_PUSHARGS(locals, jl_source_nslots(src) + jl_source_nssavalues(src));
    interpreter_state s = {};
    s.src = src;
    s.locals = locals;
    s.module = m;
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

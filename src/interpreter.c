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
    jl_value_t **locals;
    size_t nl;
    size_t ngensym;
    jl_module_t *mod;
} interpreter_state;

static jl_value_t *eval(jl_value_t *e, interpreter_state s);
static jl_value_t *eval_body(jl_array_t *stmts, interpreter_state s, int start, int toplevel);
jl_value_t *jl_eval_module_expr(jl_expr_t *ex);
int jl_is_toplevel_only_expr(jl_value_t *e);

jl_value_t *jl_interpret_toplevel_expr(jl_value_t *e)
{
    interpreter_state s = { .locals = NULL, .nl = 0, .ngensym = 0, .mod = NULL };
    return eval(e, s);
}

JL_DLLEXPORT jl_value_t *jl_interpret_toplevel_expr_in(jl_module_t *m,
                                                       jl_value_t *e,
                                                       jl_svec_t *local_syms,
                                                       jl_svec_t *local_vals)
{
    jl_value_t *v=NULL;
    jl_module_t *last_m = jl_current_module;
    jl_module_t *task_last_m = jl_current_task->current_module;
    size_t i, nl = jl_svec_len(local_syms);
    jl_value_t **locals = (jl_value_t**)alloca(sizeof(jl_value_t*) * 2 * nl);
    for (i = 0; i < nl; i++) {
        locals[2 * i] = jl_svecref(local_syms, i);
        locals[2 * i + 1] = jl_svec_len(local_vals) > 0 ? jl_svecref(local_vals, i) : NULL;
    }
    JL_TRY {
        jl_current_task->current_module = jl_current_module = m;
        interpreter_state s = { .locals = locals,
                                .nl = nl, .ngensym = 0, .mod = NULL};
        v = eval(e, s);
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

static jl_value_t *do_call(jl_value_t **args, size_t nargs,
                           interpreter_state s)
{
    jl_value_t **argv;
    JL_GC_PUSHARGS(argv, nargs);
    size_t i;
    // hack, remove me
    int *ref_index = alloca(sizeof(int)*nargs);
    void **refs = alloca(sizeof(void*)*nargs);
    for(i=0; i < nargs; i++) {
        jl_value_t *arg = args[i];
        /* ugly hack */
        if (jl_is_expr(arg) && ((jl_expr_t*)arg)->head == amp_sym) {
            jl_value_t *var = jl_cellref(((jl_expr_t*)arg)->args, 0);
            jl_value_t *val = NULL;
            if (jl_is_gensym(var)) {
                size_t id = ((jl_gensym_t*)var)->id;
                ref_index[i] = 1 + 2*s.nl + id;
                val = s.locals[2*s.nl + id];
            } else {
                if (!jl_is_symbol(var)) {
                    assert(jl_is_symbolnode(var));
                    var = jl_symbolnode_sym(var);
                }
                assert(jl_is_symbol(var));
                int k;
                for (k = 0; k < s.nl; k++) {
                    if (s.locals[2*k] == var) {
                        ref_index[i] = 1 + 2*k + 1;
                        val = s.locals[2*k + 1];
                        break;
                    }
                }
                assert(k < s.nl);
            }
            jl_value_t *param = NULL, *ty = NULL, *ref = NULL;
            JL_GC_PUSH3(&param, &ty, &ref);
            param = jl_alloc_svec(1);
            jl_svecset(param, 0, jl_typeof(val));
            int sz = jl_datatype_size(jl_typeof(val));
            refs[i] = malloc(sz);
            memcpy(refs[i], val, sz);
            argv[i] = jl_new_struct_uninit(jl_apply_type(jl_pointer_type, param));
            *(void**)argv[i] = refs[i];
            JL_GC_POP();
        } else { /* sane path */
            argv[i] = eval(arg, s);
            ref_index[i] = 0;
        }
    }
    /* nothing to see here */
    jl_value_t *result = jl_apply_generic(argv, nargs);
    for (int k = 0; k < nargs; k++) {
        if (ref_index[k] != 0) {
            int ri = ref_index[k] - 1;
            jl_value_t *ty = jl_typeof(s.locals[ri]);
            s.locals[ri] = jl_new_struct_uninit(ty);
            memcpy(s.locals[ri], refs[k], jl_datatype_size(ty));
            free(refs[k]);
        }
    }
    JL_GC_POP();
    return result;
}

jl_value_t *jl_apply_interpreted(jl_value_t **argv, uint32_t nargs)
{
    if (jl_typeof(argv[0]) == jl_intrinsic_type) {
        int intr_id = *(int*)argv[0];
        if (intr_id == 89) { // ccall implementation
            jl_value_t *gen_ccall = jl_eval_string("Base.generic_ccall");
            argv[0] = gen_ccall;
            if (!jl_is_cpointer(argv[1])) {
                argv[1] = jl_native_sym(argv[1], jl_voidpointer_type);
            }
            argv[3] = jl_inst_concrete_tupletype(argv[3]);
            assert(nargs >= 4);
            int was_enabled = jl_interpreter_enabled;
            jl_interpreter_enabled = 0; // avoid recursing in there while generating generic_ccall's body
            jl_value_t *result;
            if (nargs == 4) {
                jl_value_t **argv2 = alloca(sizeof(jl_value_t*)*(nargs+1));
                memcpy(argv2, argv, nargs*sizeof(jl_value_t*));
                argv2[4] = jl_emptysvec;
                result = jl_apply_generic_ex(argv2, 5, 0);
            } else {
                jl_value_t *ccall_args = jl_alloc_svec(nargs - 4); // not rooted until next assignment
                memcpy(jl_svec_data(ccall_args), &argv[4], (nargs - 4)*sizeof(jl_value_t*));
                argv[4] = ccall_args;
                result = jl_apply_generic_ex(argv, 5, 0);
            }
            jl_interpreter_enabled = was_enabled;
            return result;
        } else if (intr_id == 90) { // cglobal implementation
            jl_value_t *ty = jl_voidpointer_type;
            JL_GC_PUSH1(&ty);
            if (nargs == 3) {
                ty = jl_alloc_svec(1);
                jl_svecset(ty, 0, argv[2]);
                ty = jl_apply_type(jl_pointer_type, ty);
            }
            jl_value_t *result = jl_native_sym(argv[1], ty);
            JL_GC_POP();
            return result;
        }
        else {
            return jl_f_intrinsic_call(NULL, argv, nargs);
        }
    }
    else if (jl_subtype(argv[0], (jl_value_t*)jl_builtin_type, 1)) {
        return ((jl_value_t*(*)(jl_value_t*,jl_value_t**,int))jl_get_builtin_fptr(argv[0]))(NULL, &argv[1], nargs-1);
    }
    else {
        jl_methtable_t *mt = jl_gf_mtable(argv[0]);
        jl_lambda_info_t *mfunc = jl_method_table_assoc_exact(mt, argv, nargs);
        if (!mfunc) {
            jl_tupletype_t *tt = arg_type_tuple(argv, nargs);
            mfunc = jl_mt_assoc_by_type(mt, tt, 1, 0);
        }
        if (!mfunc) {
            jl_no_method_error((jl_function_t*)argv[0], argv, nargs);
            __builtin_unreachable();
        }
        return jl_interpret_call(/*jl_get_unspecialized(*/mfunc/*)*/, argv, nargs, mfunc->sparam_vals, 0);
    }
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
    return (jl_typeof(dta) == jl_typeof(dtb) &&
            // TODO: can't yet handle parametric types due to how constructors work
            dta->parameters == jl_emptysvec &&
            dta->name->name == dtb->name->name &&
            jl_egal((jl_value_t*)dta->types, (jl_value_t*)dtb->types) &&
            dta->abstract == dtb->abstract &&
            dta->mutabl == dtb->mutabl &&
            dta->size == dtb->size &&
            dta->ninitialized == dtb->ninitialized &&
            jl_egal((jl_value_t*)dta->super, (jl_value_t*)dtb->super) &&
            jl_egal((jl_value_t*)dta->name->names, (jl_value_t*)dtb->name->names) &&
            jl_egal((jl_value_t*)dta->parameters, (jl_value_t*)dtb->parameters));
}

static void check_can_assign_type(jl_binding_t *b)
{
    if (b->constp && b->value != NULL && !jl_is_datatype(b->value))
        jl_errorf("invalid redefinition of constant %s",
                  jl_symbol_name(b->name));
}

void jl_reinstantiate_inner_types(jl_datatype_t *t);

void jl_set_datatype_super(jl_datatype_t *tt, jl_value_t *super)
{
    if (!jl_is_datatype(super) || !jl_is_abstracttype(super) ||
        tt->name == ((jl_datatype_t*)super)->name ||
        jl_subtype(super,(jl_value_t*)jl_vararg_type,0) ||
        jl_is_tuple_type(super) ||
        jl_subtype(super,(jl_value_t*)jl_type_type,0) ||
        super == (jl_value_t*)jl_builtin_type) {
        jl_errorf("invalid subtyping in definition of %s",
                  jl_symbol_name(tt->name->name));
    }
    tt->super = (jl_datatype_t*)super;
    jl_gc_wb(tt, tt->super);
}
int jl_interpreter_enabled = 1;
static jl_value_t *eval(jl_value_t *e, interpreter_state s)
{
    jl_module_t *mod = s.mod ? s.mod : jl_current_module;
    if (jl_is_symbol(e)) {
        jl_value_t *v = NULL;
        size_t i;
        for(i=0; i < s.nl; i++) {
            if (s.locals[i*2] == e) {
                v = s.locals[i*2+1];
                break;
            }
        }
        if (i >= s.nl)
            v = jl_get_global(mod, (jl_sym_t*)e);
        if (v == NULL)
            jl_undefined_var_error((jl_sym_t*)e);
        return v;
    }
    if (jl_is_symbolnode(e)) {
        return eval((jl_value_t*)jl_symbolnode_sym(e), s);
    }
    if (jl_is_gensym(e)) {
        ssize_t genid = ((jl_gensym_t*)e)->id;
        if (genid >= s.ngensym || genid < 0)
            jl_error("access to invalid GenSym location");
        else
            return s.locals[s.nl*2 + genid];
    }
    if (jl_is_quotenode(e)) {
        return jl_fieldref(e,0);
    }
    if (jl_is_topnode(e)) {
        jl_sym_t *s = (jl_sym_t*)jl_fieldref(e,0);
        jl_value_t *v = jl_get_global(jl_base_relative_to(mod),s);
        if (v == NULL)
            jl_undefined_var_error(s);
        return v;
    }
    if (!jl_is_expr(e)) {
        if (jl_is_globalref(e)) {
            jl_value_t *gfargs[2] = {(jl_value_t*)jl_globalref_mod(e), (jl_value_t*)jl_globalref_name(e)};
            return jl_f_getfield(NULL, gfargs, 2);
        }
        if (jl_is_linenode(e)) {
            jl_lineno = jl_linenode_line(e);
        }
        if (jl_is_newvarnode(e)) {
            // TODO enable again when frontend bug(?) fixed
            /*jl_value_t *var = jl_fieldref(e,0);
            assert(!jl_is_gensym(var));
            assert(jl_is_symbol(var));
            for(size_t i=0; i < s.nl; i++) {
                if (s.locals[i*2] == var) {
                    s.locals[i*2+1] = NULL;
                    break;
                }
                }*/
            return (jl_value_t*)jl_nothing;
        }
        return e;
    }
    jl_expr_t *ex = (jl_expr_t*)e;
    jl_value_t **args = (jl_value_t**)jl_array_data(ex->args);
    size_t nargs = jl_array_len(ex->args);
    if (ex->head == call_sym) {
        return do_call(args, nargs, s);
    }
    else if (ex->head == assign_sym) {
        jl_value_t *sym = args[0];
        jl_value_t *rhs = eval(args[1], s);
        if (jl_is_gensym(sym)) {
            ssize_t genid = ((jl_gensym_t*)sym)->id;
            if (genid >= s.ngensym || genid < 0)
                jl_error("assignment to invalid GenSym location");
            s.locals[s.nl*2 + genid] = rhs;
            return rhs;
        }
        if (jl_is_symbol(sym)) {
            size_t i;
            for (i=0; i < s.nl; i++) {
                if (s.locals[i*2] == sym) {
                    s.locals[i*2+1] = rhs;
                    return rhs;
                }
            }
        }
        jl_module_t *m = mod;
        if (jl_is_globalref(sym)) {
            m = jl_globalref_mod(sym);
            sym = (jl_value_t*)jl_globalref_name(sym);
        }
        assert(jl_is_symbol(sym));
        JL_GC_PUSH1(&rhs);
        jl_binding_t *b = jl_get_binding_wr(m, (jl_sym_t*)sym);
        jl_checked_assignment(b, rhs);
        JL_GC_POP();
        return rhs;
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
    else if (ex->head == null_sym) {
        return (jl_value_t*)jl_nothing;
    }
    else if (ex->head == body_sym) {
        return eval_body(ex->args, s, 0, 0);
    }
    else if (ex->head == exc_sym) {
        return jl_exception_in_transit;
    }
    else if (ex->head == static_typeof_sym) {
        return (jl_value_t*)jl_any_type;
    }
    else if (ex->head == method_sym) {
        jl_sym_t *fname = (jl_sym_t*)args[0];
        assert(jl_expr_nargs(ex) != 1 || jl_is_symbol(fname));

        if (jl_is_symbol(fname)) {
            jl_value_t **bp=NULL;
            jl_value_t *bp_owner=NULL;
            jl_binding_t *b=NULL;
            for (size_t i=0; i < s.nl; i++) {
                if (s.locals[i*2] == (jl_value_t*)fname) {
                    bp = &s.locals[i*2+1];
                    break;
                }
            }
            if (bp == NULL) {
                b = jl_get_binding_for_method_def(mod, fname);
                bp = &b->value;
                bp_owner = (jl_value_t*)mod;
            }
            jl_value_t *gf = jl_generic_function_def(fname, bp, bp_owner, b);
            if (jl_expr_nargs(ex) == 1)
                return gf;
        }

        jl_value_t *atypes=NULL, *meth=NULL;
        JL_GC_PUSH2(&atypes, &meth);
        atypes = eval(args[1], s);
        meth = eval(args[2], s);
        jl_method_def((jl_svec_t*)atypes, (jl_lambda_info_t*)meth, args[3]);
        JL_GC_POP();
        return jl_nothing;
    }
    else if (ex->head == copyast_sym) {
        return jl_copy_ast(eval(args[0], s));
    }
    else if (ex->head == const_sym) {
        jl_value_t *sym = args[0];
        assert(jl_is_symbol(sym));
        for (size_t i=0; i < s.nl; i++) {
            if (s.locals[i*2] == sym) {
                return (jl_value_t*)jl_nothing;
            }
        }
        jl_binding_t *b = jl_get_binding_wr(mod, (jl_sym_t*)sym);
        jl_declare_constant(b);
        return (jl_value_t*)jl_nothing;
    }
    else if (ex->head == global_sym) {
        // create uninitialized mutable binding for "global x" decl
        // TODO: handle type decls
        for (size_t i=0; i < jl_array_len(ex->args); i++) {
            assert(jl_is_symbol(args[i]));
            jl_get_binding_wr(mod, (jl_sym_t*)args[i]);
        }
        return (jl_value_t*)jl_nothing;
    }
    else if (ex->head == abstracttype_sym) {
        jl_value_t *name = args[0];
        jl_value_t *para = eval(args[1], s);
        jl_value_t *super = NULL;
        jl_value_t *temp = NULL;
        jl_datatype_t *dt = NULL;
        JL_GC_PUSH4(&para, &super, &temp, &dt);
        assert(jl_is_svec(para));
        assert(jl_is_symbol(name));
        dt = jl_new_abstracttype(name, jl_any_type, (jl_svec_t*)para);
        jl_binding_t *b = jl_get_binding_wr(mod, (jl_sym_t*)name);
        temp = b->value;
        check_can_assign_type(b);
        b->value = (jl_value_t*)dt;
        jl_gc_wb_binding(b, dt);
        super = eval(args[2], s);
        jl_set_datatype_super(dt, super);
        jl_reinstantiate_inner_types(dt);
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
        dt = jl_new_bitstype(name, jl_any_type, (jl_svec_t*)para, nb);
        jl_binding_t *b = jl_get_binding_wr(mod, (jl_sym_t*)name);
        temp = b->value;
        check_can_assign_type(b);
        b->value = (jl_value_t*)dt;
        jl_gc_wb_binding(b, dt);
        super = eval(args[3], s);
        jl_set_datatype_super(dt, super);
        jl_reinstantiate_inner_types(dt);
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
        jl_value_t *para = eval(args[1], s);
        assert(jl_is_svec(para));
        jl_value_t *temp = NULL;
        jl_value_t *super = NULL;
        jl_datatype_t *dt = NULL;
        JL_GC_PUSH4(&para, &super, &temp, &dt);
        temp = eval(args[2], s);  // field names
        dt = jl_new_datatype((jl_sym_t*)name, jl_any_type, (jl_svec_t*)para,
                             (jl_svec_t*)temp, NULL,
                             0, args[5]==jl_true ? 1 : 0, jl_unbox_long(args[6]));

        jl_binding_t *b = jl_get_binding_wr(mod, (jl_sym_t*)name);
        temp = b->value;  // save old value
        // temporarily assign so binding is available for field types
        check_can_assign_type(b);
        b->value = (jl_value_t*)dt;
        jl_gc_wb_binding(b,dt);

        JL_TRY {
            super = eval(args[3], s);
            jl_set_datatype_super(dt, super);
            // operations that can fail
            inside_typedef = 1;
            dt->types = (jl_svec_t*)eval(args[4], s);
            jl_gc_wb(dt, dt->types);
            inside_typedef = 0;
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
            b->value = temp;
            jl_rethrow();
        }
        for(size_t i=0; i < jl_svec_len(para); i++) {
            ((jl_tvar_t*)jl_svecref(para,i))->bound = 0;
        }
        jl_compute_field_offsets(dt);
        if (para == (jl_value_t*)jl_emptysvec && jl_is_datatype_singleton(dt)) {
            dt->instance = newstruct(dt);
            jl_gc_wb(dt, dt->instance);
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
    else if (ex->head == inbounds_sym) {
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
    else if (ex->head == inert_sym) {
        return args[0];
    }
    else if (ex->head == thunk_sym) {
        return jl_toplevel_eval((jl_value_t*)ex);
    }
    jl_errorf("unsupported or misplaced expression %s", jl_symbol_name(ex->head));
    return (jl_value_t*)jl_nothing;
}

static int label_idx(long ltgt, jl_array_t *stmts)
{
    size_t j;
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
    interpreter_state s = {.locals = locals, .nl = 0, .ngensym = ngensym, .mod = NULL };
    jl_value_t *ret = eval_body(stmts, s, 0, 1);
    if (ngensym > 0)
        JL_GC_POP();
    return ret;
}

static jl_value_t *eval_body(jl_array_t *stmts, interpreter_state s,
                             int start, int toplevel)
{
    jl_handler_t __eh;
    size_t i=start, ns = jl_array_len(stmts);

    while (1) {
        if (i >= ns)
            jl_error("`body` expression must terminate in `return`. Use `block` instead.");
        jl_value_t *stmt = jl_cellref(stmts,i);
        if (jl_is_gotonode(stmt)) {
            i = label_idx(jl_gotonode_label(stmt), stmts);
            continue;
        }
        if (jl_is_expr(stmt)) {
            jl_sym_t *head = ((jl_expr_t*)stmt)->head;
            if (head == goto_ifnot_sym) {
                jl_value_t *cond = eval(jl_exprarg(stmt,0), s);
                if (cond == jl_false) {
                    i = label_idx(jl_unbox_long(jl_exprarg(stmt, 1)), stmts);
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
                    return eval(ex, s);
            }
            else if (head == enter_sym) {
                jl_enter_handler(&__eh);
                if (!jl_setjmp(__eh.eh_ctx,1)) {
                    return eval_body(stmts, s, i+1, toplevel);
                }
                else {
#ifdef _OS_WINDOWS_
                    if (jl_exception_in_transit == jl_stackovf_exception)
                        _resetstkoflw();
#endif
                    i = label_idx(jl_unbox_long(jl_exprarg(stmt,0)), stmts);
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
                    eval(stmt, s);
            }
        }
        else {
            if (toplevel && jl_is_toplevel_only_expr(stmt))
                jl_toplevel_eval(stmt);
            else
                eval(stmt, s);
        }
        i++;
    }
    assert(0);
    return NULL;
}

jl_value_t *jl_interpret_call(jl_lambda_info_t *lam,
                              jl_value_t **args, size_t nargs,
                              jl_value_t **sparam_vals,
                              int toplevel)
{
    jl_expr_t *ast = (jl_expr_t*)lam->ast;
    JL_GC_PUSH1(&ast)
    if (!jl_is_expr(ast))
        ast = jl_uncompress_ast(lam, ast);
    jl_array_t *stmts = jl_lam_body(ast)->args;
    jl_array_t *formals = jl_lam_args(ast);
    size_t nformals = jl_array_len(formals);
    jl_array_t *l = jl_lam_vinfo(ast);
    size_t nl = jl_array_len(l);
    jl_value_t **locals;
    jl_value_t *gensym_types = jl_lam_gensyms(ast);
    size_t ngensym = (jl_is_array(gensym_types) ? jl_array_len(gensym_types) : jl_unbox_gensym(gensym_types));
    size_t n_sparam = jl_svec_len(lam->sparam_syms);
    JL_GC_PUSHARGS(locals, 2*nl + 2*n_sparam + ngensym);
    jl_value_t *r = (jl_value_t*)jl_nothing;
    size_t i=0;
    size_t used_l = 0;
    // passed arguments
    for(; i < nformals; i++) {
        if (used_l >= nl)
            break;
        jl_value_t *local_name = jl_cellref(jl_cellref(l, used_l),0);
        jl_value_t *formal = jl_cellref(formals, i);
        jl_value_t *formal_name = jl_decl_var(formal);
        assert(jl_is_symbol(local_name) && jl_is_symbol(formal_name));
        if (formal_name != local_name) {
            // parameter unused so has not slot in the frame
            continue;
        }
        locals[used_l*2] = local_name;
        if (jl_is_rest_arg(formal)) {
            assert(i == nformals - 1);
            locals[used_l*2+1] = jl_f_tuple(NULL, &args[i], nargs - i);
        } else {
            locals[used_l*2+1] = args[i];
        }
        used_l++;
    }
    // locals
    for(i = used_l; i < nl; i++) {
        locals[i*2]   = jl_cellref(jl_cellref(l,i),0);
        //locals[i*2+1] = NULL;   // init'd by JL_GC_PUSHARGS
    }
    for(i = 0; i < n_sparam; i++) {
        locals[(nl+i)*2] = jl_svecref(lam->sparam_syms, i);
        locals[(nl+i)*2 + 1] = jl_svecref(sparam_vals, i);
    }
    interpreter_state s = { .locals = locals, .nl = nl + n_sparam, .ngensym = ngensym, .mod = lam->module };
    r = eval_body(stmts, s, 0, toplevel);
    JL_GC_POP();
    JL_GC_POP();
    return r;
}

jl_value_t *jl_interpret_toplevel_thunk(jl_lambda_info_t *lam)
{
    return jl_interpret_call(lam, NULL, 0, NULL, 1);
}

#ifdef __cplusplus
}
#endif

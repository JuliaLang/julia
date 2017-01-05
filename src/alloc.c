// This file is a part of Julia. License is MIT: http://julialang.org/license

/*
  object constructors
*/
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <assert.h>
#include "julia.h"
#include "julia_internal.h"

#ifdef __cplusplus
extern "C" {
#endif

JL_DLLEXPORT jl_value_t *jl_true;
JL_DLLEXPORT jl_value_t *jl_false;

jl_tvar_t     *jl_typetype_tvar;
jl_datatype_t *jl_typetype_type;
jl_value_t    *jl_ANY_flag;

jl_datatype_t *jl_typector_type;

jl_datatype_t *jl_array_type;
jl_typename_t *jl_array_typename;
jl_value_t *jl_array_uint8_type;
jl_value_t *jl_array_any_type=NULL;
jl_value_t *jl_array_symbol_type;
jl_datatype_t *jl_weakref_type;
jl_datatype_t *jl_string_type;
jl_datatype_t *jl_expr_type;
jl_datatype_t *jl_globalref_type;
jl_datatype_t *jl_linenumbernode_type;
jl_datatype_t *jl_labelnode_type;
jl_datatype_t *jl_gotonode_type;
jl_datatype_t *jl_quotenode_type;
jl_datatype_t *jl_newvarnode_type;
jl_datatype_t *jl_intrinsic_type;
jl_datatype_t *jl_method_type;
jl_datatype_t *jl_methtable_type;
jl_datatype_t *jl_typemap_entry_type;
jl_datatype_t *jl_typemap_level_type;
jl_datatype_t *jl_method_instance_type;
jl_datatype_t *jl_code_info_type;
jl_datatype_t *jl_module_type;
jl_datatype_t *jl_errorexception_type=NULL;
jl_datatype_t *jl_argumenterror_type;
jl_datatype_t *jl_typeerror_type;
jl_datatype_t *jl_methoderror_type;
jl_datatype_t *jl_loaderror_type;
jl_datatype_t *jl_initerror_type;
jl_datatype_t *jl_undefvarerror_type;
jl_datatype_t *jl_ref_type;
jl_datatype_t *jl_pointer_type;
jl_datatype_t *jl_void_type;
jl_datatype_t *jl_voidpointer_type;
jl_value_t *jl_an_empty_vec_any=NULL;
jl_value_t *jl_stackovf_exception;
#ifdef SEGV_EXCEPTION
jl_value_t *jl_segv_exception;
#endif
JL_DLLEXPORT jl_value_t *jl_diverror_exception;
JL_DLLEXPORT jl_value_t *jl_domain_exception;
JL_DLLEXPORT jl_value_t *jl_overflow_exception;
JL_DLLEXPORT jl_value_t *jl_inexact_exception;
JL_DLLEXPORT jl_value_t *jl_undefref_exception;
jl_value_t *jl_interrupt_exception;
jl_datatype_t *jl_boundserror_type;
jl_value_t *jl_memory_exception;
jl_value_t *jl_readonlymemory_exception;
union jl_typemap_t jl_cfunction_list;

jl_sym_t *call_sym;    jl_sym_t *invoke_sym;
jl_sym_t *dots_sym;    jl_sym_t *empty_sym;
jl_sym_t *module_sym;  jl_sym_t *slot_sym;
jl_sym_t *export_sym;  jl_sym_t *import_sym;
jl_sym_t *importall_sym; jl_sym_t *toplevel_sym;
jl_sym_t *quote_sym;   jl_sym_t *amp_sym;
jl_sym_t *top_sym;     jl_sym_t *colons_sym;
jl_sym_t *line_sym;    jl_sym_t *jl_incomplete_sym;
// head symbols for each expression type
jl_sym_t *goto_sym;    jl_sym_t *goto_ifnot_sym;
jl_sym_t *label_sym;   jl_sym_t *return_sym;
jl_sym_t *lambda_sym;  jl_sym_t *assign_sym;
jl_sym_t *body_sym;    jl_sym_t *globalref_sym;
jl_sym_t *method_sym;  jl_sym_t *core_sym;
jl_sym_t *enter_sym;   jl_sym_t *leave_sym;
jl_sym_t *exc_sym;     jl_sym_t *error_sym;
jl_sym_t *new_sym;     jl_sym_t *using_sym;
jl_sym_t *const_sym;   jl_sym_t *thunk_sym;
jl_sym_t *anonymous_sym;  jl_sym_t *underscore_sym;
jl_sym_t *abstracttype_sym; jl_sym_t *bitstype_sym;
jl_sym_t *compositetype_sym;
jl_sym_t *global_sym; jl_sym_t *list_sym;
jl_sym_t *dot_sym;    jl_sym_t *newvar_sym;
jl_sym_t *boundscheck_sym; jl_sym_t *inbounds_sym;
jl_sym_t *copyast_sym; jl_sym_t *fastmath_sym;
jl_sym_t *pure_sym; jl_sym_t *simdloop_sym;
jl_sym_t *meta_sym; jl_sym_t *compiler_temp_sym;
jl_sym_t *inert_sym; jl_sym_t *vararg_sym;
jl_sym_t *unused_sym; jl_sym_t *static_parameter_sym;
jl_sym_t *polly_sym; jl_sym_t *inline_sym;
jl_sym_t *propagate_inbounds_sym;

typedef struct {
    int64_t a;
    int64_t b;
} bits128_t;

// Note that this function updates len
static jl_value_t *jl_new_bits_internal(jl_value_t *dt, void *data, size_t *len)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    assert(jl_is_datatype(dt));
    jl_datatype_t *bt = (jl_datatype_t*)dt;
    size_t nb = jl_datatype_size(bt);
    if (nb == 0)
        return jl_new_struct_uninit(bt);
    *len = LLT_ALIGN(*len, bt->layout->alignment);
    data = (char*)data + (*len);
    *len += nb;
    if (bt == jl_uint8_type)   return jl_box_uint8(*(uint8_t*)data);
    if (bt == jl_int64_type)   return jl_box_int64(*(int64_t*)data);
    if (bt == jl_bool_type)    return (*(int8_t*)data) ? jl_true:jl_false;
    if (bt == jl_int32_type)   return jl_box_int32(*(int32_t*)data);
    if (bt == jl_float64_type) return jl_box_float64(*(double*)data);

    jl_value_t *v = jl_gc_alloc(ptls, nb, bt);
    switch (nb) {
    case  1: *(int8_t*)   jl_data_ptr(v) = *(int8_t*)data;    break;
    case  2: *(int16_t*)  jl_data_ptr(v) = *(int16_t*)data;   break;
    case  4: *(int32_t*)  jl_data_ptr(v) = *(int32_t*)data;   break;
    case  8: *(int64_t*)  jl_data_ptr(v) = *(int64_t*)data;   break;
    case 16: *(bits128_t*)jl_data_ptr(v) = *(bits128_t*)data; break;
    default: memcpy(jl_data_ptr(v), data, nb);
    }
    return v;
}

JL_DLLEXPORT jl_value_t *jl_new_bits(jl_value_t *bt, void *data)
{
    size_t len = 0;
    return jl_new_bits_internal(bt, data, &len);
}

void jl_assign_bits(void *dest, jl_value_t *bits)
{
    size_t nb = jl_datatype_size(jl_typeof(bits));
    if (nb == 0) return;
    switch (nb) {
    case  1: *(int8_t*)dest    = *(int8_t*)jl_data_ptr(bits);    break;
    case  2: *(int16_t*)dest   = *(int16_t*)jl_data_ptr(bits);   break;
    case  4: *(int32_t*)dest   = *(int32_t*)jl_data_ptr(bits);   break;
    case  8: *(int64_t*)dest   = *(int64_t*)jl_data_ptr(bits);   break;
    case 16: *(bits128_t*)dest = *(bits128_t*)jl_data_ptr(bits); break;
    default: memcpy(dest, jl_data_ptr(bits), nb);
    }
}

JL_DLLEXPORT int jl_field_index(jl_datatype_t *t, jl_sym_t *fld, int err)
{
    jl_svec_t *fn = t->name->names;
    for(size_t i=0; i < jl_svec_len(fn); i++) {
        if (jl_svecref(fn,i) == (jl_value_t*)fld) {
            return (int)i;
        }
    }
    if (err)
        jl_errorf("type %s has no field %s", jl_symbol_name(t->name->name),
                  jl_symbol_name(fld));
    return -1;
}

JL_DLLEXPORT jl_value_t *jl_get_nth_field(jl_value_t *v, size_t i)
{
    jl_datatype_t *st = (jl_datatype_t*)jl_typeof(v);
    assert(i < jl_datatype_nfields(st));
    size_t offs = jl_field_offset(st,i);
    if (jl_field_isptr(st,i)) {
        return *(jl_value_t**)((char*)v + offs);
    }
    return jl_new_bits(jl_field_type(st,i), (char*)v + offs);
}

JL_DLLEXPORT jl_value_t *jl_get_nth_field_checked(jl_value_t *v, size_t i)
{
    jl_datatype_t *st = (jl_datatype_t*)jl_typeof(v);
    if (i >= jl_datatype_nfields(st))
        jl_bounds_error_int(v, i+1);
    size_t offs = jl_field_offset(st,i);
    if (jl_field_isptr(st,i)) {
        jl_value_t *fval = *(jl_value_t**)((char*)v + offs);
        if (fval == NULL)
            jl_throw(jl_undefref_exception);
        return fval;
    }
    return jl_new_bits(jl_field_type(st,i), (char*)v + offs);
}

JL_DLLEXPORT void jl_set_nth_field(jl_value_t *v, size_t i, jl_value_t *rhs)
{
    jl_datatype_t *st = (jl_datatype_t*)jl_typeof(v);
    size_t offs = jl_field_offset(st,i);
    if (jl_field_isptr(st,i)) {
        *(jl_value_t**)((char*)v + offs) = rhs;
        if (rhs != NULL) jl_gc_wb(v, rhs);
    }
    else {
        jl_assign_bits((char*)v + offs, rhs);
    }
}

JL_DLLEXPORT int jl_field_isdefined(jl_value_t *v, size_t i)
{
    jl_datatype_t *st = (jl_datatype_t*)jl_typeof(v);
    size_t offs = jl_field_offset(st,i);
    if (jl_field_isptr(st,i)) {
        return *(jl_value_t**)((char*)v + offs) != NULL;
    }
    return 1;
}

JL_DLLEXPORT jl_value_t *jl_new_struct(jl_datatype_t *type, ...)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (type->instance != NULL) return type->instance;
    va_list args;
    size_t nf = jl_datatype_nfields(type);
    va_start(args, type);
    jl_value_t *jv = jl_gc_alloc(ptls, jl_datatype_size(type), type);
    for(size_t i=0; i < nf; i++) {
        jl_set_nth_field(jv, i, va_arg(args, jl_value_t*));
    }
    va_end(args);
    return jv;
}

JL_DLLEXPORT jl_value_t *jl_new_structv(jl_datatype_t *type, jl_value_t **args,
                                        uint32_t na)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (type->instance != NULL) return type->instance;
    size_t nf = jl_datatype_nfields(type);
    jl_value_t *jv = jl_gc_alloc(ptls, jl_datatype_size(type), type);
    for(size_t i=0; i < na; i++) {
        jl_set_nth_field(jv, i, args[i]);
    }
    for(size_t i=na; i < nf; i++) {
        if (jl_field_isptr(type, i)) {
            *(jl_value_t**)((char*)jl_data_ptr(jv)+jl_field_offset(type,i)) = NULL;
        }
    }
    return jv;
}

JL_DLLEXPORT jl_value_t *jl_new_struct_uninit(jl_datatype_t *type)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    if (type->instance != NULL) return type->instance;
    size_t size = jl_datatype_size(type);
    jl_value_t *jv = jl_gc_alloc(ptls, size, type);
    if (size > 0)
        memset(jl_data_ptr(jv), 0, size);
    return jv;
}


extern jl_value_t *jl_builtin_getfield;
jl_value_t *jl_resolve_globals(jl_value_t *expr, jl_module_t *module)
{
    if (jl_is_symbol(expr)) {
        if (module == NULL)
            return expr;
        return jl_module_globalref(module, (jl_sym_t*)expr);
    }
    else if (jl_is_expr(expr)) {
        jl_expr_t *e = (jl_expr_t*)expr;
        if (jl_is_toplevel_only_expr(expr) || e->head == const_sym || e->head == copyast_sym ||
            e->head == global_sym || e->head == quote_sym || e->head == inert_sym ||
            e->head == line_sym || e->head == meta_sym || e->head == inbounds_sym ||
            e->head == boundscheck_sym || e->head == simdloop_sym) {
        }
        else {
            if (e->head == call_sym && jl_expr_nargs(e) == 3 &&
                    jl_is_quotenode(jl_exprarg(e, 2)) && module != NULL) {
                // replace getfield(module_expr, :sym) with GlobalRef
                jl_value_t *s = jl_fieldref(jl_exprarg(e, 2), 0);
                jl_value_t *fe = jl_exprarg(e, 0);
                if (jl_is_symbol(s) && jl_is_globalref(fe)) {
                    jl_binding_t *b = jl_get_binding(jl_globalref_mod(fe), jl_globalref_name(fe));
                    jl_value_t *f = NULL;
                    if (b && b->constp) {
                        f = b->value;
                    }
                    if (f == jl_builtin_getfield) {
                        jl_value_t *me = jl_exprarg(e, 1);
                        jl_module_t *me_mod = NULL;
                        jl_sym_t *me_sym = NULL;
                        if (jl_is_globalref(me)) {
                            me_mod = jl_globalref_mod(me);
                            me_sym = jl_globalref_name(me);
                        }
                        else if (jl_is_symbol(me) && jl_binding_resolved_p(module, (jl_sym_t*)me)) {
                            me_mod = module;
                            me_sym = (jl_sym_t*)me;
                        }
                        if (me_mod && me_sym) {
                            jl_binding_t *b = jl_get_binding(me_mod, me_sym);
                            if (b && b->constp) {
                                jl_value_t *m = b->value;
                                if (m && jl_is_module(m)) {
                                    return jl_module_globalref((jl_module_t*)m, (jl_sym_t*)s);
                                }
                            }
                        }
                    }
                }
            }
            size_t i = 0;
            if (e->head == method_sym || e->head == abstracttype_sym || e->head == compositetype_sym ||
                e->head == bitstype_sym || e->head == module_sym)
                i++;
            for (; i < jl_array_len(e->args); i++) {
                // TODO: this should be making a copy, not mutating the source
                jl_exprargset(e, i, jl_resolve_globals(jl_exprarg(e, i), module));
            }
        }
    }
    return expr;
}

// copy a :lambda Expr into its CodeInfo representation,
// including popping of known meta nodes
static void jl_code_info_set_ast(jl_code_info_t *li, jl_expr_t *ast)
{
    assert(jl_is_expr(ast));
    jl_expr_t *bodyex = (jl_expr_t*)jl_exprarg(ast, 2);
    assert(jl_is_expr(bodyex));
    jl_array_t *body = bodyex->args;
    li->code = body;
    jl_gc_wb(li, li->code);
    size_t j, n = jl_array_len(body);
    jl_value_t **bd = (jl_value_t**)jl_array_data((jl_array_t*)li->code);
    for (j = 0; j < n; j++) {
        jl_value_t *st = bd[j];
        if (jl_is_expr(st) && ((jl_expr_t*)st)->head == meta_sym) {
            size_t k, ins = 0, na = jl_expr_nargs(st);
            jl_array_t *meta = ((jl_expr_t*)st)->args;
            for (k = 0; k < na; k++) {
                jl_value_t *ma = jl_array_ptr_ref(meta, k);
                if (ma == (jl_value_t*)pure_sym)
                    li->pure = 1;
                else if (ma == (jl_value_t*)inline_sym)
                    li->inlineable = 1;
                else if (ma == (jl_value_t*)propagate_inbounds_sym)
                    li->propagate_inbounds = 1;
                else
                    jl_array_ptr_set(meta, ins++, ma);
            }
            if (ins == 0)
                bd[j] = jl_nothing;
            else
                jl_array_del_end(meta, na - ins);
        }
    }
    jl_array_t *vinfo = (jl_array_t*)jl_exprarg(ast, 1);
    jl_array_t *vis = (jl_array_t*)jl_array_ptr_ref(vinfo, 0);
    size_t nslots = jl_array_len(vis);
    jl_value_t *ssavalue_types = jl_array_ptr_ref(vinfo, 2);
    assert(jl_is_long(ssavalue_types));
    size_t nssavalue = jl_unbox_long(ssavalue_types);
    li->slotnames = jl_alloc_vec_any(nslots);
    jl_gc_wb(li, li->slotnames);
    li->slottypes = jl_nothing;
    li->slotflags = jl_alloc_array_1d(jl_array_uint8_type, nslots);
    jl_gc_wb(li, li->slotflags);
    li->ssavaluetypes = jl_box_long(nssavalue);
    jl_gc_wb(li, li->ssavaluetypes);
    int i;
    for (i = 0; i < nslots; i++) {
        jl_value_t *vi = jl_array_ptr_ref(vis, i);
        jl_sym_t *name = (jl_sym_t*)jl_array_ptr_ref(vi, 0);
        assert(jl_is_symbol(name));
        char *str = jl_symbol_name(name);
        if (i > 0 && name != unused_sym) {
            if (str[0] == '#') {
                // convention for renamed variables: #...#original_name
                char *nxt = strchr(str + 1, '#');
                if (nxt)
                    name = jl_symbol(nxt+1);
                else if (str[1] == 's')  // compiler-generated temporaries, #sXXX
                    name = compiler_temp_sym;
            }
        }
        jl_array_ptr_set(li->slotnames, i, name);
        jl_array_uint8_set(li->slotflags, i, jl_unbox_long(jl_array_ptr_ref(vi, 2)));
    }
}

JL_DLLEXPORT jl_method_instance_t *jl_new_method_instance_uninit(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_method_instance_t *li =
        (jl_method_instance_t*)jl_gc_alloc(ptls, sizeof(jl_method_instance_t),
                                           jl_method_instance_type);
    li->inferred = NULL;
    li->inferred_const = NULL;
    li->rettype = (jl_value_t*)jl_any_type;
    li->sparam_vals = jl_emptysvec;
    li->backedges = NULL;
    li->fptr = NULL;
    li->unspecialized_ducttape = NULL;
    li->jlcall_api = 0;
    li->compile_traced = 0;
    li->functionObjectsDecls.functionObject = NULL;
    li->functionObjectsDecls.specFunctionObject = NULL;
    li->specTypes = NULL;
    li->inInference = 0;
    li->def = NULL;
    li->min_world = 0;
    li->max_world = 0;
    return li;
}

JL_DLLEXPORT jl_code_info_t *jl_new_code_info_uninit(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_code_info_t *src =
        (jl_code_info_t*)jl_gc_alloc(ptls, sizeof(jl_code_info_t),
                                       jl_code_info_type);
    src->code = NULL;
    src->slotnames = NULL;
    src->slotflags = NULL;
    src->slottypes = NULL;
    src->ssavaluetypes = NULL;
    src->inferred = 0;
    src->pure = 0;
    src->inlineable = 0;
    src->propagate_inbounds = 0;
    return src;
}

jl_code_info_t *jl_new_code_info_from_ast(jl_expr_t *ast)
{
    jl_code_info_t *src = NULL;
    JL_GC_PUSH1(&src);
    src = jl_new_code_info_uninit();
    jl_code_info_set_ast(src, ast);
    JL_GC_POP();
    return src;
}

// invoke (compiling if necessary) the jlcall function pointer for a method template
STATIC_INLINE jl_value_t *jl_call_staged(jl_svec_t *sparam_vals, jl_method_instance_t *generator,
                                         jl_value_t **args, uint32_t nargs)
{
    jl_generic_fptr_t fptr;
    fptr.fptr = generator->fptr;
    fptr.jlcall_api = generator->jlcall_api;
    if (__unlikely(fptr.fptr == NULL || fptr.jlcall_api == 0)) {
        size_t world = generator->def->min_world;
        void *F = jl_compile_linfo(&generator, (jl_code_info_t*)generator->inferred, world, &jl_default_cgparams).functionObject;
        fptr = jl_generate_fptr(generator, F, world);
    }
    assert(jl_svec_len(generator->def->sparam_syms) == jl_svec_len(sparam_vals));
    if (fptr.jlcall_api == 1)
        return fptr.fptr1(args[0], &args[1], nargs-1);
    else if (fptr.jlcall_api == 3)
        return fptr.fptr3(sparam_vals, args[0], &args[1], nargs-1);
    else
        abort(); // shouldn't have inferred any other calling convention
}

// return a newly allocated CodeInfo for the function signature
// effectively described by the tuple (specTypes, env, Method) inside linfo
JL_DLLEXPORT jl_code_info_t *jl_code_for_staged(jl_method_instance_t *linfo)
{
    JL_TIMING(STAGED_FUNCTION);
    jl_tupletype_t *tt = linfo->specTypes;
    jl_svec_t *env = linfo->sparam_vals;
    size_t i, l;
    jl_expr_t *ex = NULL;
    jl_value_t *linenum = NULL;
    jl_svec_t *sparam_vals = env;
    jl_method_instance_t *generator = linfo->def->generator;
    assert(linfo != generator);
    assert(linfo->def->isstaged);
    jl_code_info_t *func = NULL;
    JL_GC_PUSH4(&ex, &linenum, &sparam_vals, &func);
    jl_ptls_t ptls = jl_get_ptls_states();
    int last_lineno = jl_lineno;
    int last_in = ptls->in_pure_callback;
    jl_module_t *last_m = ptls->current_module;
    jl_module_t *task_last_m = ptls->current_task->current_module;
    size_t last_age = jl_get_ptls_states()->world_age;
    assert(jl_svec_len(linfo->def->sparam_syms) == jl_svec_len(sparam_vals));
    JL_TRY {
        ptls->in_pure_callback = 1;
        // need to eval macros in the right module
        ptls->current_task->current_module = ptls->current_module = linfo->def->module;
        // and the right world
        ptls->world_age = generator->def->min_world;

        ex = jl_exprn(lambda_sym, 2);

        int nargs = linfo->def->nargs;
        jl_array_t *argnames = jl_alloc_vec_any(nargs);
        jl_array_ptr_set(ex->args, 0, argnames);
        for (i = 0; i < nargs; i++)
            jl_array_ptr_set(argnames, i, jl_array_ptr_ref(linfo->def->source->slotnames, i));

        jl_expr_t *scopeblock = jl_exprn(jl_symbol("scope-block"), 1);
        jl_array_ptr_set(ex->args, 1, scopeblock);
        jl_expr_t *body = jl_exprn(jl_symbol("block"), 2);
        jl_array_ptr_set(((jl_expr_t*)jl_exprarg(ex,1))->args, 0, body);
        linenum = jl_box_long(linfo->def->line);
        jl_value_t *linenode = jl_new_struct(jl_linenumbernode_type, linenum);
        jl_array_ptr_set(body->args, 0, linenode);

        // invoke code generator
        assert(jl_nparams(tt) == jl_array_len(argnames) ||
               (linfo->def->isva && (jl_nparams(tt) >= jl_array_len(argnames) - 1)));
        jl_array_ptr_set(body->args, 1,
                jl_call_staged(sparam_vals, generator, jl_svec_data(tt->parameters), jl_nparams(tt)));

        if (linfo->def->sparam_syms != jl_emptysvec) {
            // mark this function as having the same static parameters as the generator
            size_t i, nsp = jl_svec_len(linfo->def->sparam_syms);
            jl_expr_t *newast = jl_exprn(jl_symbol("with-static-parameters"), nsp + 1);
            jl_exprarg(newast, 0) = (jl_value_t*)ex;
            // (with-static-parameters func_expr sp_1 sp_2 ...)
            for (i = 0; i < nsp; i++)
                jl_exprarg(newast, i+1) = jl_svecref(linfo->def->sparam_syms, i);
            ex = newast;
        }

        func = (jl_code_info_t*)jl_expand((jl_value_t*)ex);
        if (!jl_is_code_info(func)) {
            if (jl_is_expr(func) && ((jl_expr_t*)func)->head == error_sym)
                jl_interpret_toplevel_expr((jl_value_t*)func);
            jl_error("generated function body is not pure. this likely means it contains a closure or comprehension.");
        }

        jl_array_t *stmts = (jl_array_t*)func->code;
        for (i = 0, l = jl_array_len(stmts); i < l; i++) {
            jl_array_ptr_set(stmts, i, jl_resolve_globals(jl_array_ptr_ref(stmts, i), linfo->def->module));
        }
        ptls->in_pure_callback = last_in;
        jl_lineno = last_lineno;
        ptls->current_module = last_m;
        ptls->current_task->current_module = task_last_m;
        ptls->world_age = last_age;
    }
    JL_CATCH {
        ptls->in_pure_callback = last_in;
        jl_lineno = last_lineno;
        ptls->current_module = last_m;
        ptls->current_task->current_module = task_last_m;
        jl_rethrow();
    }
    JL_GC_POP();
    return func;
}

JL_DLLEXPORT jl_code_info_t *jl_copy_code_info(jl_code_info_t *src)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_code_info_t *newsrc =
        (jl_code_info_t*)jl_gc_alloc(ptls, sizeof(jl_code_info_t),
                                       jl_code_info_type);
    *newsrc = *src;
    return newsrc;
}

// return a new lambda-info that has some extra static parameters merged in
jl_method_instance_t *jl_get_specialized(jl_method_t *m, jl_tupletype_t *types, jl_svec_t *sp)
{
    assert(jl_svec_len(m->sparam_syms) == jl_svec_len(sp) || sp == jl_emptysvec);
    jl_method_instance_t *new_linfo = jl_new_method_instance_uninit();
    new_linfo->def = m;
    new_linfo->specTypes = types;
    new_linfo->sparam_vals = sp;
    new_linfo->min_world = m->min_world;
    new_linfo->max_world = m->max_world;
    return new_linfo;
}

static void jl_method_set_source(jl_method_t *m, jl_code_info_t *src)
{
    uint8_t j;
    uint8_t called = 0;
    for (j = 1; j < m->nargs && j <= 8; j++) {
        jl_value_t *ai = jl_array_ptr_ref(src->slotnames, j);
        if (ai == (jl_value_t*)unused_sym)
            continue;
        if (jl_array_uint8_ref(src->slotflags, j) & 64)
            called |= (1 << (j - 1));
    }
    m->called = called;

    assert(jl_typeis(src->code, jl_array_any_type));
    jl_array_t *stmts = (jl_array_t*)src->code;
    size_t i, n = jl_array_len(stmts);
    jl_array_t *copy = jl_alloc_vec_any(n);
    JL_GC_PUSH1(&copy);
    int set_lineno = 0;
    for (i = 0; i < n; i++) {
        jl_value_t *st = jl_array_ptr_ref(stmts, i);
        if (jl_is_expr(st) && ((jl_expr_t*)st)->head == line_sym) {
            if (!set_lineno) {
                m->line = jl_unbox_long(jl_exprarg(st, 0));
                m->file = (jl_sym_t*)jl_exprarg(st, 1);
                st = jl_nothing;
                set_lineno = 1;
            }
        }
        else {
            st = jl_resolve_globals(st, m->module);
        }
        jl_array_ptr_set(copy, i, st);
    }
    copy = jl_compress_ast(m, copy);
    m->source = jl_copy_code_info(src);
    jl_gc_wb(m, m->source);
    m->source->code = copy;
    JL_GC_POP();
}

JL_DLLEXPORT jl_method_t *jl_new_method_uninit(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_method_t *m =
        (jl_method_t*)jl_gc_alloc(ptls, sizeof(jl_method_t), jl_method_type);
    m->specializations.unknown = jl_nothing;
    m->sig = NULL;
    m->tvars = NULL;
    m->sparam_syms = NULL;
    m->ambig = jl_nothing;
    m->roots = NULL;
    m->module = ptls->current_module;
    m->source = NULL;
    m->unspecialized = NULL;
    m->generator = NULL;
    m->name = NULL;
    m->file = empty_sym;
    m->line = 0;
    m->called = 0xff;
    m->invokes.unknown = NULL;
    m->isstaged = 0;
    m->isva = 0;
    m->nargs = 0;
    m->needs_sparam_vals_ducttape = 2;
    m->traced = 0;
    m->min_world = 1;
    m->max_world = ~(size_t)0;
    JL_MUTEX_INIT(&m->writelock);
    return m;
}

jl_array_t *jl_all_methods;
jl_method_t *jl_new_method(jl_code_info_t *definition,
                           jl_sym_t *name,
                           jl_tupletype_t *sig,
                           size_t nargs,
                           int isva,
                           jl_svec_t *tvars,
                           int isstaged)
{
    size_t i, l = jl_svec_len(tvars);
    jl_svec_t *sparam_syms = jl_alloc_svec_uninit(l);
    for (i = 0; i < l; i++) {
        jl_svecset(sparam_syms, i, ((jl_tvar_t*)jl_svecref(tvars, i))->name);
    }
    jl_value_t *root = (jl_value_t*)sparam_syms;
    JL_GC_PUSH1(&root);

    jl_method_t *m = jl_new_method_uninit();
    m->min_world = ++jl_world_counter;
    m->isstaged = isstaged;
    m->name = name;
    m->sig = sig;
    m->isva = isva;
    m->nargs = nargs;
    if (jl_svec_len(tvars) == 1)
        tvars = (jl_svec_t*)jl_svecref(tvars, 0);
    m->tvars = tvars;
    m->sparam_syms = sparam_syms;
    root = (jl_value_t*)m;
    jl_method_set_source(m, definition);
    if (isstaged) {
        // create and store generator for generated functions
        m->generator = jl_get_specialized(m, jl_anytuple_type, jl_emptysvec);
        jl_gc_wb(m, m->generator);
        m->generator->inferred = (jl_value_t*)m->source;
    }

#ifdef RECORD_METHOD_ORDER
    if (jl_all_methods == NULL)
        jl_all_methods = jl_alloc_vec_any(0);
#endif
    if (jl_all_methods != NULL) {
        while (jl_array_len(jl_all_methods) < jl_world_counter)
            jl_array_ptr_1d_push(jl_all_methods, NULL);
        jl_array_ptr_1d_push(jl_all_methods, (jl_value_t*)m);
    }

    JL_GC_POP();
    return m;
}

// symbols --------------------------------------------------------------------

static jl_sym_t *volatile symtab = NULL;

static uintptr_t hash_symbol(const char *str, size_t len)
{
    return memhash(str, len) ^ ~(uintptr_t)0/3*2;
}

static size_t symbol_nbytes(size_t len)
{
    return (sizeof(jl_taggedvalue_t) + sizeof(jl_sym_t) + len + 1 + 7) & -8;
}

static jl_sym_t *mk_symbol(const char *str, size_t len)
{
    jl_sym_t *sym;
    size_t nb = symbol_nbytes(len);

    jl_taggedvalue_t *tag = (jl_taggedvalue_t*)jl_gc_perm_alloc_nolock(nb);
    sym = (jl_sym_t*)jl_valueof(tag);
    // set to old marked since we don't need write barrier on it.
    tag->header = ((uintptr_t)jl_sym_type) | GC_OLD_MARKED;
    sym->left = sym->right = NULL;
    sym->hash = hash_symbol(str, len);
    memcpy(jl_symbol_name(sym), str, len);
    jl_symbol_name(sym)[len] = 0;
    return sym;
}

static jl_sym_t *symtab_lookup(jl_sym_t *volatile *ptree, const char *str,
                               size_t len, jl_sym_t *volatile **slot)
{
    jl_sym_t *node = jl_atomic_load_acquire(ptree);
    uintptr_t h = hash_symbol(str, len);

    // Tree nodes sorted by major key of (int(hash)) and minor key of (str).
    while (node != NULL) {
        intptr_t x = (intptr_t)(h - node->hash);
        if (x == 0) {
            x = strncmp(str, jl_symbol_name(node), len);
            if (x == 0 && jl_symbol_name(node)[len] == 0) {
                if (slot != NULL)
                    *slot = ptree;
                return node;
            }
        }
        if (x < 0)
            ptree = &node->left;
        else
            ptree = &node->right;
        node = jl_atomic_load_acquire(ptree);
    }
    if (slot != NULL)
        *slot = ptree;
    return node;
}

static jl_sym_t *_jl_symbol(const char *str, size_t len)
{
    jl_sym_t *volatile *slot;
    jl_sym_t *node = symtab_lookup(&symtab, str, len, &slot);
    if (node == NULL) {
        JL_LOCK_NOGC(&gc_perm_lock);
        // Someone might have updated it, check and look up again
        if (*slot != NULL && (node = symtab_lookup(slot, str, len, &slot))) {
            JL_UNLOCK_NOGC(&gc_perm_lock);
            return node;
        }
        node = mk_symbol(str, len);
        jl_atomic_store_release(slot, node);
        JL_UNLOCK_NOGC(&gc_perm_lock);
    }
    return node;
}

JL_DLLEXPORT jl_sym_t *jl_symbol(const char *str)
{
    return _jl_symbol(str, strlen(str));
}

JL_DLLEXPORT jl_sym_t *jl_symbol_lookup(const char *str)
{
    return symtab_lookup(&symtab, str, strlen(str), NULL);
}

JL_DLLEXPORT jl_sym_t *jl_symbol_n(const char *str, size_t len)
{
    if (memchr(str, 0, len))
        jl_exceptionf(jl_argumenterror_type, "Symbol name may not contain \\0");
    return _jl_symbol(str, len);
}

JL_DLLEXPORT jl_sym_t *jl_get_root_symbol(void)
{
    return symtab;
}

static uint32_t gs_ctr = 0;  // TODO: per-thread
uint32_t jl_get_gs_ctr(void) { return gs_ctr; }
void jl_set_gs_ctr(uint32_t ctr) { gs_ctr = ctr; }

JL_DLLEXPORT jl_sym_t *jl_gensym(void)
{
    char name[16];
    char *n;
    n = uint2str(&name[2], sizeof(name)-2, gs_ctr, 10);
    *(--n) = '#'; *(--n) = '#';
    gs_ctr++;
    return jl_symbol(n);
}

JL_DLLEXPORT jl_sym_t *jl_tagged_gensym(const char *str, int32_t len)
{
    char gs_name[14];
    if (memchr(str, 0, len))
        jl_exceptionf(jl_argumenterror_type, "Symbol name may not contain \\0");
    char *name = (char*) (len >= 256 ? malloc(sizeof(gs_name)+len+3) :
                          alloca(sizeof(gs_name)+len+3));
    char *n;
    name[0] = '#'; name[1] = '#'; name[2+len] = '#';
    memcpy(name+2, str, len);
    n = uint2str(gs_name, sizeof(gs_name), gs_ctr, 10);
    memcpy(name+3+len, n, sizeof(gs_name)-(n-gs_name));
    gs_ctr++;
    jl_sym_t *sym = _jl_symbol(name, len+3+sizeof(gs_name)-(n-gs_name)-1);
    if (len >= 256) free(name);
    return sym;
}

// allocating types -----------------------------------------------------------

jl_sym_t *jl_demangle_typename(jl_sym_t *s)
{
    char *n = jl_symbol_name(s);
    if (n[0] != '#')
        return s;
    char *end = strrchr(n, '#');
    int32_t len;
    if (end == n || end == n+1)
        len = strlen(n) - 1;
    else
        len = (end-n) - 1;
    return jl_symbol_n(&n[1], len);
}

JL_DLLEXPORT jl_methtable_t *jl_new_method_table(jl_sym_t *name, jl_module_t *module)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_methtable_t *mt =
        (jl_methtable_t*)jl_gc_alloc(ptls, sizeof(jl_methtable_t),
                                     jl_methtable_type);
    mt->name = jl_demangle_typename(name);
    mt->module = module;
    mt->defs.unknown = jl_nothing;
    mt->cache.unknown = jl_nothing;
    mt->max_args = 0;
    mt->kwsorter = NULL;
    mt->backedges = NULL;
    JL_MUTEX_INIT(&mt->writelock);
    return mt;
}

JL_DLLEXPORT jl_typename_t *jl_new_typename_in(jl_sym_t *name, jl_module_t *module)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_typename_t *tn =
        (jl_typename_t*)jl_gc_alloc(ptls, sizeof(jl_typename_t),
                                    jl_typename_type);
    tn->name = name;
    tn->module = module;
    tn->primary = NULL;
    tn->cache = jl_emptysvec;
    tn->linearcache = jl_emptysvec;
    tn->names = NULL;
    tn->hash = bitmix(bitmix(module ? module->uuid : 0, name->hash), 0xa1ada1da);
    tn->mt = NULL;
    return tn;
}

JL_DLLEXPORT jl_typename_t *jl_new_typename(jl_sym_t *name)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    return jl_new_typename_in(name, ptls->current_module);
}

jl_datatype_t *jl_new_abstracttype(jl_value_t *name, jl_datatype_t *super,
                                   jl_svec_t *parameters)
{
    jl_datatype_t *dt = jl_new_datatype((jl_sym_t*)name, super, parameters, jl_emptysvec, jl_emptysvec, 1, 0, 0);
    return dt;
}

jl_datatype_t *jl_new_uninitialized_datatype(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_datatype_t *t = (jl_datatype_t*)jl_gc_alloc(ptls, sizeof(jl_datatype_t), jl_datatype_type);
    t->depth = 0;
    t->hastypevars = 0;
    t->haswildcard = 0;
    t->isleaftype = 1;
    t->layout = NULL;
    return t;
}

static jl_datatype_layout_t *jl_get_layout(uint32_t nfields,
                                           uint32_t alignment,
                                           int haspadding,
                                           jl_fielddesc32_t desc[])
{
    // compute the smallest fielddesc type that can hold the layout description
    int fielddesc_type = 0;
    if (nfields > 0) {
        uint32_t max_size = 0;
        uint32_t max_offset = desc[nfields - 1].offset;
        for (size_t i = 0; i < nfields; i++) {
            if (desc[i].size > max_size)
                max_size = desc[i].size;
        }
        jl_fielddesc8_t maxdesc8 = { 0, max_size, max_offset };
        jl_fielddesc16_t maxdesc16 = { 0, max_size, max_offset };
        jl_fielddesc32_t maxdesc32 = { 0, max_size, max_offset };
        if (maxdesc8.size != max_size || maxdesc8.offset != max_offset) {
            fielddesc_type = 1;
            if (maxdesc16.size != max_size || maxdesc16.offset != max_offset) {
                fielddesc_type = 2;
                if (maxdesc32.size != max_size || maxdesc32.offset != max_offset) {
                    assert(0); // should have been verified by caller
                }
            }
        }
    }

    // allocate a new descriptor
    uint32_t fielddesc_size = jl_fielddesc_size(fielddesc_type);
    jl_datatype_layout_t *flddesc =
        (jl_datatype_layout_t*)jl_gc_perm_alloc(sizeof(jl_datatype_layout_t) + nfields * fielddesc_size);
    flddesc->nfields = nfields;
    flddesc->alignment = alignment;
    flddesc->haspadding = haspadding;
    flddesc->fielddesc_type = fielddesc_type;

    // fill out the fields of the new descriptor
    jl_fielddesc8_t* desc8 = (jl_fielddesc8_t*)jl_dt_layout_fields(flddesc);
    jl_fielddesc16_t* desc16 = (jl_fielddesc16_t*)jl_dt_layout_fields(flddesc);
    jl_fielddesc32_t* desc32 = (jl_fielddesc32_t*)jl_dt_layout_fields(flddesc);
    int ptrfree = 1;
    for (size_t i = 0; i < nfields; i++) {
        if (fielddesc_type == 0) {
            desc8[i].offset = desc[i].offset;
            desc8[i].size = desc[i].size;
            desc8[i].isptr = desc[i].isptr;
        }
        else if (fielddesc_type == 1) {
            desc16[i].offset = desc[i].offset;
            desc16[i].size = desc[i].size;
            desc16[i].isptr = desc[i].isptr;
        }
        else {
            desc32[i].offset = desc[i].offset;
            desc32[i].size = desc[i].size;
            desc32[i].isptr = desc[i].isptr;
        }
        if (desc[i].isptr)
            ptrfree = 0;
    }
    flddesc->pointerfree = ptrfree;
    return flddesc;
}

// Determine if homogeneous tuple with fields of type t will have
// a special alignment beyond normal Julia rules.
// Return special alignment if one exists, 0 if normal alignment rules hold.
// A non-zero result *must* match the LLVM rules for a vector type <nfields x t>.
// For sake of Ahead-Of-Time (AOT) compilation, this routine has to work
// without LLVM being available.
unsigned jl_special_vector_alignment(size_t nfields, jl_value_t *t)
{
    if (!jl_is_vecelement_type(t))
        return 0;
    // LLVM 3.7 and 3.8 either crash or generate wrong code for many
    // SIMD vector sizes N. It seems the rule is that N can have at
    // most 2 non-zero bits. (This is true at least for N<=100.) See
    // also <https://llvm.org/bugs/show_bug.cgi?id=27708>.
    size_t mask = nfields;
    // See e.g.
    // <https://graphics.stanford.edu/%7Eseander/bithacks.html> for an
    // explanation of this bit-counting algorithm.
    mask &= mask-1;             // clear least-significant 1 if present
    mask &= mask-1;             // clear another 1
    if (mask)
        return 0;               // nfields has more than two 1s
    assert(jl_datatype_nfields(t)==1);
    jl_value_t *ty = jl_field_type(t, 0);
    if (!jl_is_bitstype(ty))
        // LLVM requires that a vector element be a primitive type.
        // LLVM allows pointer types as vector elements, but until a
        // motivating use case comes up for Julia, we reject pointers.
        return 0;
    size_t elsz = jl_datatype_size(ty);
    if (elsz>8 || (1<<elsz & 0x116) == 0)
        // Element size is not 1, 2, 4, or 8.
        return 0;
    size_t size = nfields*elsz;
    // LLVM's alignment rule for vectors seems to be to round up to
    // a power of two, even if that's overkill for the target hardware.
    size_t alignment=1;
    for( ; size>alignment; alignment*=2 )
        continue;
    return alignment;
}

void jl_compute_field_offsets(jl_datatype_t *st)
{
    size_t sz = 0, alignm = 1;
    int homogeneous = 1;
    jl_value_t *lastty = NULL;

    uint64_t max_offset = (((uint64_t)1) << 32) - 1;
    uint64_t max_size = max_offset >> 1;

    uint32_t nfields = jl_svec_len(st->types);
    size_t descsz = nfields * sizeof(jl_fielddesc32_t);
    jl_fielddesc32_t *desc;
    if (descsz < jl_page_size)
        desc = (jl_fielddesc32_t*)alloca(descsz);
    else
        desc = (jl_fielddesc32_t*)malloc(descsz);
    int haspadding = 0;
    assert(st->name == jl_tuple_typename ||
           st == jl_sym_type ||
           st == jl_simplevector_type ||
           nfields != 0);

    for (size_t i = 0; i < nfields; i++) {
        jl_value_t *ty = jl_field_type(st, i);
        size_t fsz, al;
        if (jl_isbits(ty) && jl_is_leaf_type(ty) && ((jl_datatype_t*)ty)->layout) {
            fsz = jl_datatype_size(ty);
            // Should never happen
            if (__unlikely(fsz > max_size))
                goto throw_ovf;
            al = ((jl_datatype_t*)ty)->layout->alignment;
            desc[i].isptr = 0;
            if (((jl_datatype_t*)ty)->layout->haspadding)
                haspadding = 1;
        }
        else {
            fsz = sizeof(void*);
            if (fsz > MAX_ALIGN)
                fsz = MAX_ALIGN;
            al = fsz;
            desc[i].isptr = 1;
        }
        if (al != 0) {
            size_t alsz = LLT_ALIGN(sz, al);
            if (sz & (al - 1))
                haspadding = 1;
            sz = alsz;
            if (al > alignm)
                alignm = al;
        }
        homogeneous &= lastty==NULL || lastty==ty;
        lastty = ty;
        desc[i].offset = sz;
        desc[i].size = fsz;
        if (__unlikely(max_offset - sz < fsz))
            goto throw_ovf;
        sz += fsz;
    }
    if (homogeneous && lastty!=NULL && jl_is_tuple_type(st)) {
        // Some tuples become LLVM vectors with stronger alignment than what was calculated above.
        unsigned al = jl_special_vector_alignment(nfields, lastty);
        assert(al % alignm == 0);
        if (al)
            alignm = al;
    }
    st->size = LLT_ALIGN(sz, alignm);
    if (st->size > sz)
        haspadding = 1;
    st->layout = jl_get_layout(nfields, alignm, haspadding, desc);
    if (descsz >= jl_page_size) free(desc);
    return;
 throw_ovf:
    if (descsz >= jl_page_size) free(desc);
    jl_throw(jl_overflow_exception);
}

extern int jl_boot_file_loaded;

JL_DLLEXPORT jl_datatype_t *jl_new_datatype(jl_sym_t *name, jl_datatype_t *super,
                                            jl_svec_t *parameters,
                                            jl_svec_t *fnames, jl_svec_t *ftypes,
                                            int abstract, int mutabl,
                                            int ninitialized)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_datatype_t *t=NULL;
    jl_typename_t *tn=NULL;
    JL_GC_PUSH2(&t, &tn);

    if (!jl_boot_file_loaded && jl_is_symbol(name)) {
        // hack to avoid making two versions of basic types needed
        // during bootstrapping
        if (!strcmp(jl_symbol_name((jl_sym_t*)name), "Int32"))
            t = jl_int32_type;
        else if (!strcmp(jl_symbol_name((jl_sym_t*)name), "Int64"))
            t = jl_int64_type;
        else if (!strcmp(jl_symbol_name((jl_sym_t*)name), "Bool"))
            t = jl_bool_type;
        else if (!strcmp(jl_symbol_name((jl_sym_t*)name), "UInt8"))
            t = jl_uint8_type;
    }
    if (t == NULL)
        t = jl_new_uninitialized_datatype();
    else
        tn = t->name;
    // init before possibly calling jl_new_typename
    t->super = super;
    if (super != NULL) jl_gc_wb(t, t->super);
    t->parameters = parameters;
    jl_gc_wb(t, t->parameters);
    t->types = ftypes;
    if (ftypes != NULL) jl_gc_wb(t, t->types);
    t->abstract = abstract;
    t->mutabl = mutabl;
    t->ninitialized = ninitialized;
    t->instance = NULL;
    t->struct_decl = NULL;
    t->ditype = NULL;
    t->size = 0;

    if (tn == NULL) {
        t->name = NULL;
        if (jl_is_typename(name)) {
            tn = (jl_typename_t*)name;
        }
        else {
            tn = jl_new_typename((jl_sym_t*)name);
            if (!abstract) {
                tn->mt = jl_new_method_table(name, ptls->current_module);
                jl_gc_wb(tn, tn->mt);
            }
        }
        t->name = tn;
        jl_gc_wb(t, t->name);
    }
    t->name->names = fnames;
    jl_gc_wb(t->name, t->name->names);

    if (t->name->primary == NULL) {
        t->name->primary = (jl_value_t*)t;
        jl_gc_wb(t->name, t);
    }
    jl_precompute_memoized_dt(t);

    if (abstract || jl_svec_len(parameters) > 0) {
        t->uid = 0;
    }
    else {
        t->uid = jl_assign_type_uid();
        if (t->types != NULL && t->isleaftype) {
            static const jl_datatype_layout_t singleton_layout = {0, 1, 0, 1, 0};
            if (fnames == jl_emptysvec)
                t->layout = &singleton_layout;
            else
                jl_compute_field_offsets(t);
        }
    }
    JL_GC_POP();
    return t;
}

JL_DLLEXPORT jl_datatype_t *jl_new_bitstype(jl_value_t *name, jl_datatype_t *super,
                                            jl_svec_t *parameters, size_t nbits)
{
    jl_datatype_t *bt = jl_new_datatype((jl_sym_t*)name, super, parameters,
                                        jl_emptysvec, jl_emptysvec, 0, 0, 0);
    uint32_t nbytes = (nbits + 7) / 8;
    uint32_t alignm = next_power_of_two(nbytes);
    if (alignm > MAX_ALIGN)
        alignm = MAX_ALIGN;
    bt->size = nbytes;
    bt->layout = jl_get_layout(0, alignm, 0, NULL);
    return bt;
}

// type constructor -----------------------------------------------------------

JL_DLLEXPORT jl_value_t *jl_new_type_constructor(jl_svec_t *p, jl_value_t *body)
{
    jl_ptls_t ptls = jl_get_ptls_states();
#ifndef NDEBUG
    size_t i, np = jl_svec_len(p);
    for (i = 0; i < np; i++) {
        jl_tvar_t *tv = (jl_tvar_t*)jl_svecref(p, i);
        assert(jl_is_typevar(tv) && !tv->bound);
    }
#endif
    jl_typector_t *tc =
        (jl_typector_t*)jl_gc_alloc(ptls, sizeof(jl_typector_t),
                                    jl_typector_type);
    tc->parameters = p;
    tc->body = body;
    return (jl_value_t*)tc;
}


// bits constructors ----------------------------------------------------------

#define BOXN_FUNC(nb,nw)                                                \
    JL_DLLEXPORT jl_value_t *jl_box##nb(jl_datatype_t *t, int##nb##_t x) \
    {                                                                   \
        jl_ptls_t ptls = jl_get_ptls_states();                          \
        assert(jl_isbits(t));                                           \
        assert(jl_datatype_size(t) == sizeof(x));                       \
        jl_value_t *v = jl_gc_alloc(ptls, nw * sizeof(void*), t);       \
        *(int##nb##_t*)jl_data_ptr(v) = x;                              \
        return v;                                                       \
    }
BOXN_FUNC(8,  1)
BOXN_FUNC(16, 1)
BOXN_FUNC(32, 1)
#ifdef _P64
BOXN_FUNC(64, 1)
#else
BOXN_FUNC(64, 2)
#endif

#define UNBOX_FUNC(j_type,c_type)                                       \
    JL_DLLEXPORT c_type jl_unbox_##j_type(jl_value_t *v)                \
    {                                                                   \
        assert(jl_is_bitstype(jl_typeof(v)));                           \
        assert(jl_datatype_size(jl_typeof(v)) == sizeof(c_type));       \
        return *(c_type*)jl_data_ptr(v);                                \
    }
UNBOX_FUNC(int8,   int8_t)
UNBOX_FUNC(uint8,  uint8_t)
UNBOX_FUNC(int16,  int16_t)
UNBOX_FUNC(uint16, uint16_t)
UNBOX_FUNC(int32,  int32_t)
UNBOX_FUNC(uint32, uint32_t)
UNBOX_FUNC(int64,  int64_t)
UNBOX_FUNC(uint64, uint64_t)
UNBOX_FUNC(bool,   int8_t)
UNBOX_FUNC(float32, float)
UNBOX_FUNC(float64, double)
UNBOX_FUNC(voidpointer, void*)

#define BOX_FUNC(typ,c_type,pfx,nw)                             \
    JL_DLLEXPORT jl_value_t *pfx##_##typ(c_type x)              \
    {                                                           \
        jl_ptls_t ptls = jl_get_ptls_states();                  \
        jl_value_t *v = jl_gc_alloc(ptls, nw * sizeof(void*),   \
                                    jl_##typ##_type);           \
        *(c_type*)jl_data_ptr(v) = x;                           \
        return v;                                               \
    }
BOX_FUNC(float32, float,  jl_box, 1)
BOX_FUNC(voidpointer, void*,  jl_box, 1)
#ifdef _P64
BOX_FUNC(float64, double, jl_box, 1)
#else
BOX_FUNC(float64, double, jl_box, 2)
#endif

#define NBOX_C 1024

#define SIBOX_FUNC(typ,c_type,nw)\
    static jl_value_t *boxed_##typ##_cache[NBOX_C];             \
    JL_DLLEXPORT jl_value_t *jl_box_##typ(c_type x)             \
    {                                                           \
        jl_ptls_t ptls = jl_get_ptls_states();                  \
        c_type idx = x+NBOX_C/2;                                \
        if ((u##c_type)idx < (u##c_type)NBOX_C)                 \
            return boxed_##typ##_cache[idx];                    \
        jl_value_t *v = jl_gc_alloc(ptls, nw * sizeof(void*),   \
                                    jl_##typ##_type);           \
        *(c_type*)jl_data_ptr(v) = x;                           \
        return v;                                               \
    }
#define UIBOX_FUNC(typ,c_type,nw)                               \
    static jl_value_t *boxed_##typ##_cache[NBOX_C];             \
    JL_DLLEXPORT jl_value_t *jl_box_##typ(c_type x)             \
    {                                                           \
        jl_ptls_t ptls = jl_get_ptls_states();                  \
        if (x < NBOX_C)                                         \
            return boxed_##typ##_cache[x];                      \
        jl_value_t *v = jl_gc_alloc(ptls, nw * sizeof(void*),   \
                                    jl_##typ##_type);           \
        *(c_type*)jl_data_ptr(v) = x;                           \
        return v;                                               \
    }
SIBOX_FUNC(int16,  int16_t, 1)
SIBOX_FUNC(int32,  int32_t, 1)
UIBOX_FUNC(uint16, uint16_t, 1)
UIBOX_FUNC(uint32, uint32_t, 1)
UIBOX_FUNC(char,   uint32_t, 1)
UIBOX_FUNC(ssavalue, size_t, 1)
UIBOX_FUNC(slotnumber, size_t, 1)
#ifdef _P64
SIBOX_FUNC(int64,  int64_t, 1)
UIBOX_FUNC(uint64, uint64_t, 1)
#else
SIBOX_FUNC(int64,  int64_t, 2)
UIBOX_FUNC(uint64, uint64_t, 2)
#endif

static jl_value_t *boxed_int8_cache[256];
JL_DLLEXPORT jl_value_t *jl_box_int8(int8_t x)
{
    return boxed_int8_cache[(uint8_t)x];
}
static jl_value_t *boxed_uint8_cache[256];
JL_DLLEXPORT jl_value_t *jl_box_uint8(uint8_t x)
{
    return boxed_uint8_cache[x];
}

void jl_init_int32_int64_cache(void)
{
    int64_t i;
    for(i=0; i < NBOX_C; i++) {
        boxed_int32_cache[i]  = jl_box32(jl_int32_type, i-NBOX_C/2);
        boxed_int64_cache[i]  = jl_box64(jl_int64_type, i-NBOX_C/2);
#ifdef _P64
        boxed_ssavalue_cache[i] = jl_box64(jl_ssavalue_type, i);
        boxed_slotnumber_cache[i] = jl_box64(jl_slotnumber_type, i);
#else
        boxed_ssavalue_cache[i] = jl_box32(jl_ssavalue_type, i);
        boxed_slotnumber_cache[i] = jl_box32(jl_slotnumber_type, i);
#endif
    }
    for(i=0; i < 256; i++) {
        boxed_uint8_cache[i] = jl_box8(jl_uint8_type, i);
    }
}

void jl_init_box_caches(void)
{
    int64_t i;
    for(i=0; i < 256; i++) {
        boxed_int8_cache[i]  = jl_box8(jl_int8_type, i);
    }
    for(i=0; i < NBOX_C; i++) {
        boxed_int16_cache[i]  = jl_box16(jl_int16_type, i-NBOX_C/2);
        boxed_uint16_cache[i] = jl_box16(jl_uint16_type, i);
        boxed_uint32_cache[i] = jl_box32(jl_uint32_type, i);
        boxed_char_cache[i]   = jl_box32(jl_char_type, i);
        boxed_uint64_cache[i] = jl_box64(jl_uint64_type, i);
    }
}

void jl_mark_box_caches(jl_ptls_t ptls)
{
    int64_t i;
    for(i=0; i < 256; i++) {
        jl_gc_setmark(ptls, boxed_int8_cache[i]);
        jl_gc_setmark(ptls, boxed_uint8_cache[i]);
    }
    for(i=0; i < NBOX_C; i++) {
        jl_gc_setmark(ptls, boxed_int16_cache[i]);
        jl_gc_setmark(ptls, boxed_int32_cache[i]);
        jl_gc_setmark(ptls, boxed_int64_cache[i]);
        jl_gc_setmark(ptls, boxed_uint16_cache[i]);
        jl_gc_setmark(ptls, boxed_uint32_cache[i]);
        jl_gc_setmark(ptls, boxed_char_cache[i]);
        jl_gc_setmark(ptls, boxed_uint64_cache[i]);
        jl_gc_setmark(ptls, boxed_ssavalue_cache[i]);
        jl_gc_setmark(ptls, boxed_slotnumber_cache[i]);
    }
}

JL_DLLEXPORT jl_value_t *jl_box_bool(int8_t x)
{
    if (x)
        return jl_true;
    return jl_false;
}

// Expr constructor for internal use ------------------------------------------

jl_expr_t *jl_exprn(jl_sym_t *head, size_t n)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_array_t *ar = n==0 ? (jl_array_t*)jl_an_empty_vec_any : jl_alloc_vec_any(n);
    JL_GC_PUSH1(&ar);
    jl_expr_t *ex = (jl_expr_t*)jl_gc_alloc(ptls, sizeof(jl_expr_t),
                                            jl_expr_type);
    ex->head = head;
    ex->args = ar;
    ex->etype = (jl_value_t*)jl_any_type;
    JL_GC_POP();
    return ex;
}

JL_CALLABLE(jl_f__expr)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    JL_NARGSV(Expr, 1);
    JL_TYPECHK(Expr, symbol, args[0]);
    jl_array_t *ar = jl_alloc_vec_any(nargs-1);
    JL_GC_PUSH1(&ar);
    for(size_t i=0; i < nargs-1; i++)
        jl_array_ptr_set(ar, i, args[i+1]);
    jl_expr_t *ex = (jl_expr_t*)jl_gc_alloc(ptls, sizeof(jl_expr_t),
                                            jl_expr_type);
    ex->head = (jl_sym_t*)args[0];
    ex->args = ar;
    ex->etype = (jl_value_t*)jl_any_type;
    JL_GC_POP();
    return (jl_value_t*)ex;
}

#ifdef __cplusplus
}
#endif

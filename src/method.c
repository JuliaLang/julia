// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  Defining and adding methods
*/

#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

extern jl_value_t *jl_builtin_getfield;
extern jl_value_t *jl_builtin_tuple;

// Resolve references to non-locally-defined variables to become references to global
// variables in `module` (unless the rvalue is one of the type parameters in `sparam_vals`).
static jl_value_t *resolve_globals(jl_value_t *expr, jl_module_t *module, jl_svec_t *sparam_vals,
                                   int binding_effects, int eager_resolve)
{
    if (jl_is_symbol(expr)) {
        if (module == NULL)
            return expr;
        return jl_module_globalref(module, (jl_sym_t*)expr);
    }
    else if (jl_is_expr(expr)) {
        jl_expr_t *e = (jl_expr_t*)expr;
        if (e->head == global_sym && binding_effects) {
            // execute the side-effects of "global x" decl immediately:
            // creates uninitialized mutable binding in module for each global
            jl_toplevel_eval_flex(module, expr, 0, 1);
            expr = jl_nothing;
        }
        if (jl_is_toplevel_only_expr(expr) || e->head == const_sym ||
            e->head == coverageeffect_sym || e->head == copyast_sym ||
            e->head == quote_sym || e->head == inert_sym ||
            e->head == meta_sym || e->head == inbounds_sym ||
            e->head == boundscheck_sym || e->head == loopinfo_sym ||
            e->head == aliasscope_sym || e->head == popaliasscope_sym) {
            // ignore these
        }
        else {
            size_t i = 0, nargs = jl_array_len(e->args);
            if (e->head == cfunction_sym) {
                JL_NARGS(cfunction method definition, 5, 5); // (type, func, rt, at, cc)
                jl_value_t *typ = jl_exprarg(e, 0);
                if (!jl_is_type(typ))
                    jl_error("first parameter to :cfunction must be a type");
                if (typ == (jl_value_t*)jl_voidpointer_type) {
                    jl_value_t *a = jl_exprarg(e, 1);
                    JL_TYPECHK(cfunction method definition, quotenode, a);
                    *(jl_value_t**)a = jl_toplevel_eval(module, *(jl_value_t**)a);
                    jl_gc_wb(a, *(jl_value_t**)a);
                }
                jl_value_t *rt = jl_exprarg(e, 2);
                jl_value_t *at = jl_exprarg(e, 3);
                if (!jl_is_type(rt)) {
                    JL_TRY {
                        rt = jl_interpret_toplevel_expr_in(module, rt, NULL, sparam_vals);
                    }
                    JL_CATCH {
                        if (jl_typeis(jl_current_exception(), jl_errorexception_type))
                            jl_error("could not evaluate cfunction return type (it might depend on a local variable)");
                        else
                            jl_rethrow();
                    }
                    jl_exprargset(e, 2, rt);
                }
                if (!jl_is_svec(at)) {
                    JL_TRY {
                        at = jl_interpret_toplevel_expr_in(module, at, NULL, sparam_vals);
                    }
                    JL_CATCH {
                        if (jl_typeis(jl_current_exception(), jl_errorexception_type))
                            jl_error("could not evaluate cfunction argument type (it might depend on a local variable)");
                        else
                            jl_rethrow();
                    }
                    jl_exprargset(e, 3, at);
                }
                if (jl_is_svec(rt))
                    jl_error("cfunction: missing return type");
                JL_TYPECHK(cfunction method definition, type, rt);
                JL_TYPECHK(cfunction method definition, simplevector, at);
                JL_TYPECHK(cfunction method definition, quotenode, jl_exprarg(e, 4));
                JL_TYPECHK(cfunction method definition, symbol, *(jl_value_t**)jl_exprarg(e, 4));
                return expr;
            }
            if (e->head == foreigncall_sym) {
                JL_NARGSV(ccall method definition, 5); // (fptr, rt, at, cc, narg)
                jl_value_t *rt = jl_exprarg(e, 1);
                jl_value_t *at = jl_exprarg(e, 2);
                if (!jl_is_type(rt)) {
                    JL_TRY {
                        rt = jl_interpret_toplevel_expr_in(module, rt, NULL, sparam_vals);
                    }
                    JL_CATCH {
                        if (jl_typeis(jl_current_exception(), jl_errorexception_type))
                            jl_error("could not evaluate ccall return type (it might depend on a local variable)");
                        else
                            jl_rethrow();
                    }
                    jl_exprargset(e, 1, rt);
                }
                if (!jl_is_svec(at)) {
                    JL_TRY {
                        at = jl_interpret_toplevel_expr_in(module, at, NULL, sparam_vals);
                    }
                    JL_CATCH {
                        if (jl_typeis(jl_current_exception(), jl_errorexception_type))
                            jl_error("could not evaluate ccall argument type (it might depend on a local variable)");
                        else
                            jl_rethrow();
                    }
                    jl_exprargset(e, 2, at);
                }
                if (jl_is_svec(rt))
                    jl_error("ccall: missing return type");
                JL_TYPECHK(ccall method definition, type, rt);
                JL_TYPECHK(ccall method definition, simplevector, at);
                JL_TYPECHK(ccall method definition, long, jl_exprarg(e, 3));
                JL_TYPECHK(ccall method definition, quotenode, jl_exprarg(e, 4));
                JL_TYPECHK(ccall method definition, symbol, *(jl_value_t**)jl_exprarg(e, 4));
                jl_exprargset(e, 0, resolve_globals(jl_exprarg(e, 0), module, sparam_vals, binding_effects, 1));
                i++;
            }
            if (e->head == method_sym || e->head == module_sym) {
                i++;
            }
            for (; i < nargs; i++) {
                // TODO: this should be making a copy, not mutating the source
                jl_exprargset(e, i, resolve_globals(jl_exprarg(e, i), module, sparam_vals, binding_effects, eager_resolve));
            }
            if (e->head == call_sym && jl_expr_nargs(e) == 3 &&
                    jl_is_globalref(jl_exprarg(e, 0)) &&
                    jl_is_globalref(jl_exprarg(e, 1)) &&
                    jl_is_quotenode(jl_exprarg(e, 2))) {
                // replace module_expr.sym with GlobalRef(module, sym)
                // for expressions pattern-matching to `getproperty(module_expr, :sym)` in a top-module
                // (this is expected to help inference performance)
                // TODO: this was broken by linear-IR
                jl_value_t *s = jl_fieldref(jl_exprarg(e, 2), 0);
                jl_value_t *me = jl_exprarg(e, 1);
                jl_value_t *fe = jl_exprarg(e, 0);
                jl_module_t *fe_mod = jl_globalref_mod(fe);
                jl_sym_t *fe_sym = jl_globalref_name(fe);
                jl_module_t *me_mod = jl_globalref_mod(me);
                jl_sym_t *me_sym = jl_globalref_name(me);
                if (fe_mod->istopmod && !strcmp(jl_symbol_name(fe_sym), "getproperty") && jl_is_symbol(s)) {
                    if (eager_resolve || jl_binding_resolved_p(me_mod, me_sym)) {
                        jl_binding_t *b = jl_get_binding(me_mod, me_sym);
                        if (b && b->constp && b->value && jl_is_module(b->value)) {
                            return jl_module_globalref((jl_module_t*)b->value, (jl_sym_t*)s);
                        }
                    }
                }
            }
            if (e->head == call_sym && nargs > 0 &&
                    jl_is_globalref(jl_exprarg(e, 0))) {
                // TODO: this hack should be deleted once llvmcall is fixed
                jl_value_t *fe = jl_exprarg(e, 0);
                jl_module_t *fe_mod = jl_globalref_mod(fe);
                jl_sym_t *fe_sym = jl_globalref_name(fe);
                if (jl_binding_resolved_p(fe_mod, fe_sym)) {
                    // look at some known called functions
                    jl_binding_t *b = jl_get_binding(fe_mod, fe_sym);
                    if (b && b->constp && b->value == jl_builtin_tuple) {
                        size_t j;
                        for (j = 1; j < nargs; j++) {
                            if (!jl_is_quotenode(jl_exprarg(e, j)))
                                break;
                        }
                        if (j == nargs) {
                            jl_value_t *val = NULL;
                            JL_TRY {
                                val = jl_interpret_toplevel_expr_in(module, (jl_value_t*)e, NULL, sparam_vals);
                            }
                            JL_CATCH {
                            }
                            if (val)
                                return val;
                        }
                    }
                }
            }
        }
    }
    return expr;
}

void jl_resolve_globals_in_ir(jl_array_t *stmts, jl_module_t *m, jl_svec_t *sparam_vals,
                              int binding_effects)
{
    size_t i, l = jl_array_len(stmts);
    for (i = 0; i < l; i++) {
        jl_value_t *stmt = jl_array_ptr_ref(stmts, i);
        jl_array_ptr_set(stmts, i, resolve_globals(stmt, m, sparam_vals, binding_effects, 0));
    }
}

// copy a :lambda Expr into its CodeInfo representation,
// including popping of known meta nodes
static void jl_code_info_set_ir(jl_code_info_t *li, jl_expr_t *ir)
{
    assert(jl_is_expr(ir));
    jl_expr_t *bodyex = (jl_expr_t*)jl_exprarg(ir, 2);
    jl_value_t *codelocs = jl_exprarg(ir, 3);
    li->linetable = jl_exprarg(ir, 4);
    size_t nlocs = jl_array_len(codelocs);
    li->codelocs = (jl_value_t*)jl_alloc_array_1d(jl_array_int32_type, nlocs);
    size_t j;
    for (j = 0; j < nlocs; j++) {
        jl_arrayset((jl_array_t*)li->codelocs, jl_box_int32(jl_unbox_long(jl_arrayref((jl_array_t*)codelocs, j))),
                    j);
    }
    assert(jl_is_expr(bodyex));
    jl_array_t *body = bodyex->args;
    li->code = body;
    jl_gc_wb(li, li->code);
    size_t n = jl_array_len(body);
    jl_value_t **bd = (jl_value_t**)jl_array_ptr_data((jl_array_t*)li->code);
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
    jl_array_t *vinfo = (jl_array_t*)jl_exprarg(ir, 1);
    jl_array_t *vis = (jl_array_t*)jl_array_ptr_ref(vinfo, 0);
    size_t nslots = jl_array_len(vis);
    jl_value_t *ssavalue_types = jl_array_ptr_ref(vinfo, 2);
    assert(jl_is_long(ssavalue_types));
    size_t nssavalue = jl_unbox_long(ssavalue_types);
    li->slotnames = jl_alloc_array_1d(jl_array_symbol_type, nslots);
    jl_gc_wb(li, li->slotnames);
    li->slotflags = jl_alloc_array_1d(jl_array_uint8_type, nslots);
    jl_gc_wb(li, li->slotflags);
    li->ssavaluetypes = jl_box_long(nssavalue);
    jl_gc_wb(li, li->ssavaluetypes);
    li->ssaflags = jl_alloc_array_1d(jl_array_uint8_type, 0);

    // Flags that need to be copied to slotflags
    const uint8_t vinfo_mask = 8 | 16 | 32 | 64;
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
                    name = empty_sym;
            }
        }
        jl_array_ptr_set(li->slotnames, i, name);
        jl_array_uint8_set(li->slotflags, i, vinfo_mask & jl_unbox_long(jl_array_ptr_ref(vi, 2)));
    }
}

JL_DLLEXPORT jl_method_instance_t *jl_new_method_instance_uninit(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_method_instance_t *li =
        (jl_method_instance_t*)jl_gc_alloc(ptls, sizeof(jl_method_instance_t),
                                           jl_method_instance_type);
    li->def.value = NULL;
    li->specTypes = NULL;
    li->sparam_vals = jl_emptysvec;
    li->uninferred = NULL;
    li->backedges = NULL;
    li->cache = NULL;
    li->inInference = 0;
    return li;
}

JL_DLLEXPORT jl_code_info_t *jl_new_code_info_uninit(void)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_code_info_t *src =
        (jl_code_info_t*)jl_gc_alloc(ptls, sizeof(jl_code_info_t),
                                       jl_code_info_type);
    src->code = NULL;
    src->codelocs = jl_nothing;
    src->ssavaluetypes = NULL;
    src->ssaflags = NULL;
    src->method_for_inference_limit_heuristics = jl_nothing;
    src->linetable = jl_nothing;
    src->slotflags = NULL;
    src->slotnames = NULL;
    src->slottypes = jl_nothing;
    src->parent = (jl_method_instance_t*)jl_nothing;
    src->rettype = (jl_value_t*)jl_any_type;
    src->min_world = 1;
    src->max_world = ~(size_t)0;
    src->inferred = 0;
    src->inlineable = 0;
    src->propagate_inbounds = 0;
    src->pure = 0;
    src->edges = jl_nothing;
    return src;
}

jl_code_info_t *jl_new_code_info_from_ir(jl_expr_t *ir)
{
    jl_code_info_t *src = NULL;
    JL_GC_PUSH1(&src);
    src = jl_new_code_info_uninit();
    jl_code_info_set_ir(src, ir);
    JL_GC_POP();
    return src;
}

void jl_linenumber_to_lineinfo(jl_code_info_t *ci, jl_value_t *name)
{
    jl_array_t *li = (jl_array_t*)ci->linetable;
    size_t i, n = jl_array_len(li);
    jl_value_t *rt = NULL;
    JL_GC_PUSH1(&rt);
    for (i = 0; i < n; i++) {
        jl_value_t *ln = jl_array_ptr_ref(li, i);
        if (jl_is_linenode(ln)) {
            rt = jl_box_long(jl_linenode_line(ln));
            rt = jl_new_struct(jl_lineinfonode_type, name, jl_linenode_file(ln), rt, jl_box_long(0));
            jl_array_ptr_set(li, i, rt);
        }
        else if (jl_is_expr(ln) && ((jl_expr_t*)ln)->head == line_sym && jl_expr_nargs(ln) == 3) {
            rt = jl_new_struct(jl_lineinfonode_type, jl_symbol("macro expansion"),
                               jl_exprarg(ln, 1), jl_exprarg(ln, 0), jl_exprarg(ln, 2));
            jl_array_ptr_set(li, i, rt);
        }
    }
    JL_GC_POP();
}

// invoke (compiling if necessary) the jlcall function pointer for a method template
STATIC_INLINE jl_value_t *jl_call_staged(jl_method_t *def, jl_value_t *generator, jl_svec_t *sparam_vals,
                                         jl_value_t **args, uint32_t nargs)
{
    size_t n_sparams = jl_svec_len(sparam_vals);
    jl_value_t **gargs;
    size_t totargs = 1 + n_sparams + nargs + def->isva;
    JL_GC_PUSHARGS(gargs, totargs);
    gargs[0] = generator;
    memcpy(&gargs[1], jl_svec_data(sparam_vals), n_sparams * sizeof(void*));
    memcpy(&gargs[1 + n_sparams], args, nargs * sizeof(void*));
    if (def->isva) {
        gargs[totargs-1] = jl_f_tuple(NULL, &gargs[1 + n_sparams + def->nargs - 1], nargs - (def->nargs - 1));
        gargs[1 + n_sparams + def->nargs - 1] = gargs[totargs - 1];
    }
    jl_value_t *code = jl_apply(gargs, 1 + n_sparams + def->nargs);
    JL_GC_POP();
    return code;
}

// Lower `ex` into Julia IR, and (if it expands into a CodeInfo) resolve global-variable
// references in light of the provided type parameters.
// Like `jl_expand`, if there is an error expanding the provided expression, the return value
// will be an error expression (an `Expr` with `error_sym` as its head), which should be eval'd
// in the caller's context.
JL_DLLEXPORT jl_code_info_t *jl_expand_and_resolve(jl_value_t *ex, jl_module_t *module,
                                                   jl_svec_t *sparam_vals) {
    jl_code_info_t *func = (jl_code_info_t*)jl_expand((jl_value_t*)ex, module);
    JL_GC_PUSH1(&func);
    if (jl_is_code_info(func)) {
        jl_array_t *stmts = (jl_array_t*)func->code;
        jl_resolve_globals_in_ir(stmts, module, sparam_vals, 1);
    }
    JL_GC_POP();
    return func;
}

// Return a newly allocated CodeInfo for the function signature
// effectively described by the tuple (specTypes, env, Method) inside linfo
JL_DLLEXPORT jl_code_info_t *jl_code_for_staged(jl_method_instance_t *linfo)
{
    JL_TIMING(STAGED_FUNCTION);
    jl_value_t *tt = linfo->specTypes;
    jl_method_t *def = linfo->def.method;
    jl_value_t *generator = def->generator;
    assert(generator != NULL);
    assert(jl_is_method(def));
    jl_code_info_t *func = NULL;
    jl_value_t *ex = NULL;
    JL_GC_PUSH2(&ex, &func);
    jl_ptls_t ptls = jl_get_ptls_states();
    int last_lineno = jl_lineno;
    int last_in = ptls->in_pure_callback;
    size_t last_age = jl_get_ptls_states()->world_age;

    JL_TRY {
        ptls->in_pure_callback = 1;
        // and the right world
        ptls->world_age = def->primary_world;

        // invoke code generator
        jl_tupletype_t *ttdt = (jl_tupletype_t*)jl_unwrap_unionall(tt);
        ex = jl_call_staged(def, generator, linfo->sparam_vals, jl_svec_data(ttdt->parameters), jl_nparams(ttdt));

        if (jl_is_code_info(ex)) {
            func = (jl_code_info_t*)ex;
        }
        else {
            // Lower the user's expression and resolve references to the type parameters
            func = jl_expand_and_resolve(ex, def->module, linfo->sparam_vals);

            if (!jl_is_code_info(func)) {
                if (jl_is_expr(func) && ((jl_expr_t*)func)->head == error_sym) {
                    ptls->in_pure_callback = 0;
                    jl_toplevel_eval(def->module, (jl_value_t*)func);
                }
                jl_error("The function body AST defined by this @generated function is not pure. This likely means it contains a closure or comprehension.");
            }
        }

        ptls->in_pure_callback = last_in;
        jl_lineno = last_lineno;
        ptls->world_age = last_age;
        jl_linenumber_to_lineinfo(func, (jl_value_t*)def->name);
    }
    JL_CATCH {
        ptls->in_pure_callback = last_in;
        jl_lineno = last_lineno;
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
jl_method_instance_t *jl_get_specialized(jl_method_t *m, jl_value_t *types, jl_svec_t *sp)
{
    assert((size_t)jl_subtype_env_size(m->sig) == jl_svec_len(sp) || sp == jl_emptysvec);
    jl_method_instance_t *new_linfo = jl_new_method_instance_uninit();
    new_linfo->def.method = m;
    new_linfo->specTypes = types;
    new_linfo->sparam_vals = sp;
    return new_linfo;
}

static void jl_method_set_source(jl_method_t *m, jl_code_info_t *src)
{
    uint8_t j;
    uint8_t called = 0;
    int gen_only = 0;
    for (j = 1; j < m->nargs && j <= sizeof(m->nospecialize) * 8; j++) {
        jl_value_t *ai = jl_array_ptr_ref(src->slotnames, j);
        if (ai == (jl_value_t*)unused_sym) {
            // TODO: enable this. currently it triggers a bug on arguments like
            // ::Type{>:Missing}
            //int sn = j-1;
            //m->nospecialize |= (1 << sn);
            continue;
        }
        if (j <= 8) {
            if (jl_array_uint8_ref(src->slotflags, j) & 64)
                called |= (1 << (j - 1));
        }
    }
    m->called = called;
    m->pure = src->pure;
    jl_linenumber_to_lineinfo(src, (jl_value_t*)m->name);

    jl_array_t *copy = NULL;
    jl_svec_t *sparam_vars = jl_outer_unionall_vars(m->sig);
    JL_GC_PUSH3(&copy, &sparam_vars, &src);
    assert(jl_typeis(src->code, jl_array_any_type));
    jl_array_t *stmts = (jl_array_t*)src->code;
    size_t i, n = jl_array_len(stmts);
    copy = jl_alloc_vec_any(n);
    for (i = 0; i < n; i++) {
        jl_value_t *st = jl_array_ptr_ref(stmts, i);
        if (jl_is_expr(st) && ((jl_expr_t*)st)->head == meta_sym) {
            size_t nargs = jl_expr_nargs(st);
            if (nargs >= 1 && jl_exprarg(st, 0) == (jl_value_t*)nospecialize_sym) {
                if (nargs == 1) // bare `@nospecialize` is special: it prevents specialization on all args
                    m->nospecialize = -1;
                size_t j;
                for (j = 1; j < nargs; j++) {
                    jl_value_t *aj = jl_exprarg(st, j);
                    if (!jl_is_slot(aj))
                        continue;
                    int sn = (int)jl_slot_number(aj) - 2;
                    if (sn < 0) // @nospecialize on self is valid but currently ignored
                        continue;
                    if (sn > (m->nargs - 2)) {
                        jl_error("@nospecialize annotation applied to a non-argument");
                    }
                    if (sn >= sizeof(m->nospecialize) * 8) {
                        jl_printf(JL_STDERR,
                                  "WARNING: @nospecialize annotation only supported on the first %d arguments.\n",
                                  (int)(sizeof(m->nospecialize) * 8));
                        continue;
                    }
                    m->nospecialize |= (1 << sn);
                }
                st = jl_nothing;
            }
            else if (nargs >= 1 && jl_exprarg(st, 0) == (jl_value_t*)specialize_sym) {
                if (nargs == 1) // bare `@specialize` is special: it causes specialization on all args
                    m->nospecialize = 0;
                st = jl_nothing;
            }
            else if (nargs == 2 && jl_exprarg(st, 0) == (jl_value_t*)generated_sym) {
                m->generator = NULL;
                jl_value_t *gexpr = jl_exprarg(st, 1);
                if (jl_expr_nargs(gexpr) == 7) {
                    // expects (new (core GeneratedFunctionStub) funcname argnames sp line file expandearly)
                    jl_value_t *funcname = jl_exprarg(gexpr, 1);
                    assert(jl_is_symbol(funcname));
                    if (jl_get_global(m->module, (jl_sym_t*)funcname) != NULL) {
                        m->generator = jl_toplevel_eval(m->module, gexpr);
                        jl_gc_wb(m, m->generator);
                    }
                }
                if (m->generator == NULL) {
                    jl_error("invalid @generated function; try placing it in global scope");
                }
                st = jl_nothing;
            }
            else if (nargs == 1 && jl_exprarg(st, 0) == (jl_value_t*)generated_only_sym) {
                gen_only = 1;
                st = jl_nothing;
            }
            else if (nargs == 2 && jl_exprarg(st, 0) == (jl_value_t*)jl_symbol("nkw")) {
                m->nkw = jl_unbox_long(jl_exprarg(st, 1));
                st = jl_nothing;
            }
        }
        else {
            st = resolve_globals(st, m->module, sparam_vars, 1, 0);
        }
        jl_array_ptr_set(copy, i, st);
    }
    src = jl_copy_code_info(src);
    src->code = copy;
    jl_gc_wb(src, copy);
    m->slot_syms = jl_compress_argnames(src->slotnames);
    jl_gc_wb(m, m->slot_syms);
    if (gen_only)
        m->source = NULL;
    else
        m->source = (jl_value_t*)jl_compress_ir(m, src);
    jl_gc_wb(m, m->source);
    JL_GC_POP();
}

JL_DLLEXPORT jl_method_t *jl_new_method_uninit(jl_module_t *module)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_method_t *m =
        (jl_method_t*)jl_gc_alloc(ptls, sizeof(jl_method_t), jl_method_type);
    m->specializations = jl_emptysvec;
    m->speckeyset = (jl_array_t*)jl_an_empty_vec_any;
    m->sig = NULL;
    m->slot_syms = NULL;
    m->ambig = jl_nothing;
    m->resorted = jl_nothing;
    m->roots = NULL;
    m->module = module;
    m->source = NULL;
    m->unspecialized = NULL;
    m->generator = NULL;
    m->name = NULL;
    m->file = empty_sym;
    m->line = 0;
    m->called = 0xff;
    m->nospecialize = module->nospecialize;
    m->nkw = 0;
    m->invokes = NULL;
    m->isva = 0;
    m->nargs = 0;
    m->primary_world = 1;
    m->deleted_world = ~(size_t)0;
    JL_MUTEX_INIT(&m->writelock);
    return m;
}

// method definition ----------------------------------------------------------

void print_func_loc(JL_STREAM *s, jl_method_t *m);

// empty generic function def
JL_DLLEXPORT jl_value_t *jl_generic_function_def(jl_sym_t *name,
                                                 jl_module_t *module,
                                                 jl_value_t **bp, jl_value_t *bp_owner,
                                                 jl_binding_t *bnd)
{
    jl_value_t *gf = NULL;

    assert(name && bp);
    if (bnd && bnd->value != NULL && !bnd->constp)
        jl_errorf("cannot define function %s; it already has a value", jl_symbol_name(bnd->name));
    if (*bp != NULL) {
        gf = *bp;
        if (!jl_is_datatype_singleton((jl_datatype_t*)jl_typeof(gf)) && !jl_is_type(gf))
            jl_errorf("cannot define function %s; it already has a value", jl_symbol_name(name));
    }
    if (bnd)
        bnd->constp = 1;
    if (*bp == NULL) {
        gf = (jl_value_t*)jl_new_generic_function(name, module);
        *bp = gf;
        if (bp_owner) jl_gc_wb(bp_owner, gf);
    }
    return gf;
}

static jl_methtable_t *first_methtable(jl_value_t *a JL_PROPAGATES_ROOT, int got_tuple1) JL_NOTSAFEPOINT
{
    if (jl_is_datatype(a)) {
        if (got_tuple1) {
            jl_methtable_t *mt = ((jl_datatype_t*)a)->name->mt;
            if (mt != NULL)
                return mt;
        }
        if (jl_is_tuple_type(a)) {
            if (jl_nparams(a) >= 1)
                return first_methtable(jl_tparam0(a), 1);
        }
    }
    else if (jl_is_typevar(a)) {
        return first_methtable(((jl_tvar_t*)a)->ub, got_tuple1);
    }
    else if (jl_is_unionall(a)) {
        return first_methtable(((jl_unionall_t*)a)->body, got_tuple1);
    }
    else if (jl_is_uniontype(a)) {
        jl_uniontype_t *u = (jl_uniontype_t*)a;
        jl_methtable_t *m1 = first_methtable(u->a, got_tuple1);
        if ((jl_value_t*)m1 != jl_nothing) {
            jl_methtable_t *m2 = first_methtable(u->b, got_tuple1);
            if (m1 == m2)
                return m1;
        }
    }
    return (jl_methtable_t*)jl_nothing;
}

// get the MethodTable for dispatch, or `nothing` if cannot be determined
JL_DLLEXPORT jl_methtable_t *jl_method_table_for(jl_value_t *argtypes JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT
{
    return first_methtable(argtypes, 0);
}

// get the MethodTable implied by a single given type, or `nothing`
JL_DLLEXPORT jl_methtable_t *jl_argument_method_table(jl_value_t *argt JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT
{
    return first_methtable(argt, 1);
}

extern tracer_cb jl_newmeth_tracer;
jl_array_t *jl_all_methods JL_GLOBALLY_ROOTED;

JL_DLLEXPORT void jl_method_def(jl_svec_t *argdata,
                                jl_code_info_t *f,
                                jl_module_t *module)
{
    // argdata is svec(svec(types...), svec(typevars...), functionloc)
    jl_svec_t *atypes = (jl_svec_t*)jl_svecref(argdata, 0);
    jl_svec_t *tvars = (jl_svec_t*)jl_svecref(argdata, 1);
    jl_value_t *functionloc = jl_svecref(argdata, 2);
    size_t nargs = jl_svec_len(atypes);
    int isva = jl_is_vararg_type(jl_svecref(atypes, nargs - 1));
    assert(jl_is_svec(atypes));
    assert(nargs > 0);
    assert(jl_is_svec(tvars));
    if (!jl_is_type(jl_svecref(atypes, 0)) || (isva && nargs == 1))
        jl_error("function type in method definition is not a type");
    jl_sym_t *name;
    jl_method_t *m = NULL;
    jl_value_t *argtype = NULL;
    JL_GC_PUSH3(&f, &m, &argtype);
    size_t i, na = jl_svec_len(atypes);

    argtype = (jl_value_t*)jl_apply_tuple_type(atypes);
    for (i = jl_svec_len(tvars); i > 0; i--) {
        jl_value_t *tv = jl_svecref(tvars, i - 1);
        if (!jl_is_typevar(tv))
            jl_type_error("method signature", (jl_value_t*)jl_tvar_type, tv);
        argtype = jl_new_struct(jl_unionall_type, tv, argtype);
    }

    jl_methtable_t *mt = jl_method_table_for(argtype);
    if ((jl_value_t*)mt == jl_nothing)
        jl_error("Method dispatch is unimplemented currently for this method signature");
    if (mt->frozen)
        jl_error("cannot add methods to a builtin function");

    // TODO: derive our debug name from the syntax instead of the type
    name = mt->name;
    if (mt == jl_type_type_mt || mt == jl_nonfunction_mt) {
        // our value for `name` is bad, try to guess what the syntax might have had,
        // like `jl_static_show_func_sig` might have come up with
        jl_datatype_t *dt = jl_first_argument_datatype(argtype);
        if (dt != NULL) {
            name = dt->name->name;
            if (jl_is_type_type((jl_value_t*)dt)) {
                dt = (jl_datatype_t*)jl_argument_datatype(jl_tparam0(dt));
                if ((jl_value_t*)dt != jl_nothing) {
                    name = dt->name->name;
                }
            }
        }
    }
    if (!jl_is_code_info(f)) {
        // this occurs when there is a closure being added to an out-of-scope function
        // the user should only do this at the toplevel
        // the result is that the closure variables get interpolated directly into the IR
        f = jl_new_code_info_from_ir((jl_expr_t*)f);
    }
    m = jl_new_method_uninit(module);
    m->sig = argtype;
    m->name = name;
    m->isva = isva;
    m->nargs = nargs;
    assert(jl_is_linenode(functionloc));
    jl_value_t *file = jl_linenode_file(functionloc);
    m->file = jl_is_symbol(file) ? (jl_sym_t*)file : empty_sym;
    m->line = jl_linenode_line(functionloc);
    jl_method_set_source(m, f);

    if (jl_has_free_typevars(argtype)) {
        jl_exceptionf(jl_argumenterror_type,
                      "method definition for %s at %s:%d has free type variables",
                      jl_symbol_name(name),
                      jl_symbol_name(m->file),
                      m->line);
    }

    for (i = 0; i < na; i++) {
        jl_value_t *elt = jl_svecref(atypes, i);
        if (!jl_is_type(elt) && !jl_is_typevar(elt)) {
            jl_sym_t *argname = (jl_sym_t*)jl_array_ptr_ref(f->slotnames, i);
            if (argname == unused_sym)
                jl_exceptionf(jl_argumenterror_type,
                              "invalid type for argument number %d in method definition for %s at %s:%d",
                              i,
                              jl_symbol_name(name),
                              jl_symbol_name(m->file),
                              m->line);
            else
                jl_exceptionf(jl_argumenterror_type,
                              "invalid type for argument %s in method definition for %s at %s:%d",
                              jl_symbol_name(argname),
                              jl_symbol_name(name),
                              jl_symbol_name(m->file),
                              m->line);
        }
        if (jl_is_vararg_type(elt) && i < na-1)
            jl_exceptionf(jl_argumenterror_type,
                          "Vararg on non-final argument in method definition for %s at %s:%d",
                          jl_symbol_name(name),
                          jl_symbol_name(m->file),
                          m->line);
    }

#ifdef RECORD_METHOD_ORDER
    if (jl_all_methods == NULL)
        jl_all_methods = jl_alloc_vec_any(0);
#endif
    if (jl_all_methods != NULL) {
        while (jl_array_len(jl_all_methods) < m->primary_world)
            jl_array_ptr_1d_push(jl_all_methods, NULL);
        jl_array_ptr_1d_push(jl_all_methods, (jl_value_t*)m);
    }

    jl_method_table_insert(mt, m, NULL);
    if (jl_newmeth_tracer)
        jl_call_tracer(jl_newmeth_tracer, (jl_value_t*)m);
    JL_GC_POP();
}

#ifdef __cplusplus
}
#endif

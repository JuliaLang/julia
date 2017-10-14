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
jl_value_t *jl_resolve_globals(jl_value_t *expr, jl_module_t *module, jl_svec_t *sparam_vals)
{
    if (jl_is_symbol(expr)) {
        if (module == NULL)
            return expr;
        return jl_module_globalref(module, (jl_sym_t*)expr);
    }
    else if (jl_is_expr(expr)) {
        jl_expr_t *e = (jl_expr_t*)expr;
        if (e->head == global_sym) {
            // execute the side-effects of "global x" decl immediately:
            // creates uninitialized mutable binding in module for each global
            jl_toplevel_eval_flex(module, expr, 0, 1);
            expr = jl_nothing;
        }
        if (jl_is_toplevel_only_expr(expr) || e->head == const_sym || e->head == copyast_sym ||
            e->head == quote_sym || e->head == inert_sym ||
            e->head == meta_sym || e->head == inbounds_sym ||
            e->head == boundscheck_sym || e->head == simdloop_sym) {
            // ignore these
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
            size_t i = 0, nargs = jl_array_len(e->args);
            if (e->head == foreigncall_sym) {
                JL_NARGSV(ccall method definition, 5); // (fptr, rt, at, cc, narg)
                jl_value_t *rt = jl_exprarg(e, 1);
                jl_value_t *at = jl_exprarg(e, 2);
                if (!jl_is_type(rt)) {
                    JL_TRY {
                        rt = jl_interpret_toplevel_expr_in(module, rt, NULL, sparam_vals);
                    }
                    JL_CATCH {
                        if (jl_typeis(jl_exception_in_transit, jl_errorexception_type))
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
                        if (jl_typeis(jl_exception_in_transit, jl_errorexception_type))
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
                JL_TYPECHK(ccall method definition, quotenode, jl_exprarg(e, 3));
                JL_TYPECHK(ccall method definition, symbol, *(jl_value_t**)jl_exprarg(e, 3));
                JL_TYPECHK(ccall method definition, long, jl_exprarg(e, 4));
            }
            if (e->head == method_sym || e->head == abstracttype_sym || e->head == structtype_sym ||
                e->head == primtype_sym || e->head == module_sym) {
                i++;
            }
            for (; i < nargs; i++) {
                // TODO: this should be making a copy, not mutating the source
                jl_exprargset(e, i, jl_resolve_globals(jl_exprarg(e, i), module, sparam_vals));
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
    // Flags that need to be copied to slotflags
    const uint8_t vinfo_mask = 16 | 32 | 64;
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
        jl_array_uint8_set(li->slotflags, i, vinfo_mask & jl_unbox_long(jl_array_ptr_ref(vi, 2)));
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
    li->def.value = NULL;
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
        size_t world = generator->def.method->min_world;
        const char *F = jl_compile_linfo(&generator, (jl_code_info_t*)generator->inferred, world, &jl_default_cgparams).functionObject;
        fptr = jl_generate_fptr(generator, F, world);
    }
    assert(jl_svec_len(generator->def.method->sparam_syms) == jl_svec_len(sparam_vals));
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
    jl_tupletype_t *tt = (jl_tupletype_t*)linfo->specTypes;
    jl_svec_t *env = linfo->sparam_vals;
    jl_expr_t *ex = NULL;
    jl_value_t *linenum = NULL;
    jl_svec_t *sparam_vals = env;
    jl_method_instance_t *generator = linfo->def.method->generator;
    assert(generator != NULL);
    assert(linfo != generator);
    jl_code_info_t *func = NULL;
    JL_GC_PUSH4(&ex, &linenum, &sparam_vals, &func);
    jl_ptls_t ptls = jl_get_ptls_states();
    int last_lineno = jl_lineno;
    int last_in = ptls->in_pure_callback;
    jl_module_t *last_m = ptls->current_module;
    jl_module_t *task_last_m = ptls->current_task->current_module;
    size_t last_age = jl_get_ptls_states()->world_age;
    assert(jl_svec_len(linfo->def.method->sparam_syms) == jl_svec_len(sparam_vals));
    JL_TRY {
        ptls->in_pure_callback = 1;
        // need to eval macros in the right module
        ptls->current_task->current_module = ptls->current_module = linfo->def.method->module;
        // and the right world
        ptls->world_age = generator->def.method->min_world;

        ex = jl_exprn(lambda_sym, 2);

        jl_array_t *argnames = jl_alloc_vec_any(linfo->def.method->nargs);
        jl_array_ptr_set(ex->args, 0, argnames);
        jl_fill_argnames((jl_array_t*)generator->inferred, argnames);

        // build the rest of the body to pass to expand
        jl_expr_t *scopeblock = jl_exprn(jl_symbol("scope-block"), 1);
        jl_array_ptr_set(ex->args, 1, scopeblock);
        jl_expr_t *body = jl_exprn(jl_symbol("block"), 3);
        jl_array_ptr_set(((jl_expr_t*)jl_exprarg(ex, 1))->args, 0, body);

        // add location meta
        linenum = jl_box_long(linfo->def.method->line);
        jl_value_t *linenode = jl_new_struct(jl_linenumbernode_type, linenum, linfo->def.method->file);
        jl_array_ptr_set(body->args, 0, linenode);
        jl_expr_t *pushloc = jl_exprn(meta_sym, 3);
        jl_array_ptr_set(body->args, 1, pushloc);
        jl_array_ptr_set(pushloc->args, 0, jl_symbol("push_loc"));
        jl_array_ptr_set(pushloc->args, 1, linfo->def.method->file); // file
        jl_array_ptr_set(pushloc->args, 2, jl_symbol("@generated body")); // function

        // invoke code generator
        assert(jl_nparams(tt) == jl_array_len(argnames) ||
               (linfo->def.method->isva && (jl_nparams(tt) >= jl_array_len(argnames) - 1)));
        jl_value_t *generated_body = jl_call_staged(sparam_vals, generator, jl_svec_data(tt->parameters), jl_nparams(tt));
        jl_array_ptr_set(body->args, 2, generated_body);

        if (jl_is_code_info(generated_body)) {
            func = (jl_code_info_t*)generated_body;
        } else {
            if (linfo->def.method->sparam_syms != jl_emptysvec) {
                // mark this function as having the same static parameters as the generator
                size_t i, nsp = jl_svec_len(linfo->def.method->sparam_syms);
                jl_expr_t *newast = jl_exprn(jl_symbol("with-static-parameters"), nsp + 1);
                jl_exprarg(newast, 0) = (jl_value_t*)ex;
                // (with-static-parameters func_expr sp_1 sp_2 ...)
                for (i = 0; i < nsp; i++)
                    jl_exprarg(newast, i+1) = jl_svecref(linfo->def.method->sparam_syms, i);
                ex = newast;
            }

            func = (jl_code_info_t*)jl_expand((jl_value_t*)ex, linfo->def.method->module);
            if (!jl_is_code_info(func)) {
                if (jl_is_expr(func) && ((jl_expr_t*)func)->head == error_sym)
                    jl_interpret_toplevel_expr_in(linfo->def.method->module, (jl_value_t*)func, NULL, NULL);
                jl_error("generated function body is not pure. this likely means it contains a closure or comprehension.");
            }

            jl_array_t *stmts = (jl_array_t*)func->code;
            size_t i, l;
            for (i = 0, l = jl_array_len(stmts); i < l; i++) {
                jl_value_t *stmt = jl_array_ptr_ref(stmts, i);
                stmt = jl_resolve_globals(stmt, linfo->def.method->module, env);
                jl_array_ptr_set(stmts, i, stmt);
            }

            // add pop_loc meta
            jl_array_ptr_1d_push(stmts, jl_nothing);
            jl_expr_t *poploc = jl_exprn(meta_sym, 1);
            jl_array_ptr_set(stmts, jl_array_len(stmts) - 1, poploc);
            jl_array_ptr_set(poploc->args, 0, jl_symbol("pop_loc"));
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
jl_method_instance_t *jl_get_specialized(jl_method_t *m, jl_value_t *types, jl_svec_t *sp)
{
    assert(jl_svec_len(m->sparam_syms) == jl_svec_len(sp) || sp == jl_emptysvec);
    jl_method_instance_t *new_linfo = jl_new_method_instance_uninit();
    new_linfo->def.method = m;
    new_linfo->specTypes = types;
    new_linfo->sparam_vals = sp;
    new_linfo->min_world = m->min_world;
    new_linfo->max_world = ~(size_t)0;
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
    m->pure = src->pure;

    jl_array_t *copy = NULL;
    jl_svec_t *sparam_vars = jl_outer_unionall_vars(m->sig);
    JL_GC_PUSH3(&copy, &sparam_vars, &src);
    assert(jl_typeis(src->code, jl_array_any_type));
    jl_array_t *stmts = (jl_array_t*)src->code;
    size_t i, n = jl_array_len(stmts);
    copy = jl_alloc_vec_any(n);
    int set_lineno = 0;
    for (i = 0; i < n; i++) {
        jl_value_t *st = jl_array_ptr_ref(stmts, i);
        if (jl_is_linenode(st)) {
            if (!set_lineno) {
                m->line = jl_linenode_line(st);
                jl_value_t *file = jl_linenode_file(st);
                if (jl_is_symbol(file))
                    m->file = (jl_sym_t*)file;
                st = jl_nothing;
                set_lineno = 1;
            }
        }
        else if (jl_is_expr(st) && ((jl_expr_t*)st)->head == meta_sym &&
                 jl_expr_nargs(st) > 1 && jl_exprarg(st, 0) == (jl_value_t*)nospecialize_sym) {
            for (size_t j=1; j < jl_expr_nargs(st); j++) {
                jl_value_t *aj = jl_exprarg(st, j);
                if (jl_is_slot(aj)) {
                    int sn = (int)jl_slot_number(aj) - 2;
                    if (sn >= 0) {  // @nospecialize on self is valid but currently ignored
                        if (sn > (m->nargs - 2)) {
                            jl_error("@nospecialize annotation applied to a non-argument");
                        }
                        else if (sn >= sizeof(m->nospecialize) * 8) {
                            jl_printf(JL_STDERR,
                                      "WARNING: @nospecialize annotation only supported on the first %d arguments.\n",
                                      (int)(sizeof(m->nospecialize) * 8));
                        }
                        else {
                            m->nospecialize |= (1 << sn);
                        }
                    }
                }
            }
            st = jl_nothing;
        }
        else {
            st = jl_resolve_globals(st, m->module, sparam_vars);
        }
        jl_array_ptr_set(copy, i, st);
    }
    src = jl_copy_code_info(src);
    src->code = copy;
    jl_gc_wb(src, copy);
    m->source = (jl_value_t*)jl_compress_ast(m, src);
    jl_gc_wb(m, m->source);
    JL_GC_POP();
}

JL_DLLEXPORT jl_method_t *jl_new_method_uninit(jl_module_t *module)
{
    jl_ptls_t ptls = jl_get_ptls_states();
    jl_method_t *m =
        (jl_method_t*)jl_gc_alloc(ptls, sizeof(jl_method_t), jl_method_type);
    m->specializations.unknown = jl_nothing;
    m->sig = NULL;
    m->sparam_syms = NULL;
    m->ambig = jl_nothing;
    m->roots = NULL;
    m->module = module;
    m->source = NULL;
    m->unspecialized = NULL;
    m->generator = NULL;
    m->name = NULL;
    m->file = empty_sym;
    m->line = 0;
    m->called = 0xff;
    m->nospecialize = 0;
    m->invokes.unknown = NULL;
    m->isva = 0;
    m->nargs = 0;
    m->traced = 0;
    m->min_world = 1;
    JL_MUTEX_INIT(&m->writelock);
    return m;
}

jl_array_t *jl_all_methods;
static jl_method_t *jl_new_method(
        jl_code_info_t *definition,
        jl_sym_t *name,
        jl_module_t *inmodule,
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
    jl_method_t *m = NULL;
    JL_GC_PUSH1(&root);

    m = jl_new_method_uninit(inmodule);
    m->sparam_syms = sparam_syms;
    root = (jl_value_t*)m;
    m->min_world = ++jl_world_counter;
    m->name = name;
    m->sig = (jl_value_t*)sig;
    m->isva = isva;
    m->nargs = nargs;
    jl_method_set_source(m, definition);
    if (isstaged) {
        // create and store generator for generated functions
        m->generator = jl_get_specialized(m, (jl_value_t*)jl_anytuple_type, jl_emptysvec);
        jl_gc_wb(m, m->generator);
        m->generator->inferred = (jl_value_t*)m->source;
        m->source = NULL;
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

// method definition ----------------------------------------------------------

extern int jl_boot_file_loaded;

void print_func_loc(JL_STREAM *s, jl_method_t *m);

static void jl_check_static_parameter_conflicts(jl_method_t *m, jl_code_info_t *src, jl_svec_t *t)
{
    size_t nvars = jl_array_len(src->slotnames);

    size_t i, n = jl_svec_len(t);
    for (i = 0; i < n; i++) {
        jl_value_t *tv = jl_svecref(t, i);
        size_t j;
        for (j = 0; j < nvars; j++) {
            if (jl_is_typevar(tv)) {
                if ((jl_sym_t*)jl_array_ptr_ref(src->slotnames, j) == ((jl_tvar_t*)tv)->name) {
                    jl_printf(JL_STDERR,
                              "WARNING: local variable %s conflicts with a static parameter in %s",
                              jl_symbol_name(((jl_tvar_t*)tv)->name),
                              jl_symbol_name(m->name));
                    print_func_loc(JL_STDERR, m);
                    jl_printf(JL_STDERR, ".\n");
                }
            }
        }
    }
}

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

static jl_datatype_t *first_arg_datatype(jl_value_t *a, int got_tuple1)
{
    if (jl_is_datatype(a)) {
        if (got_tuple1)
            return (jl_datatype_t*)a;
        if (jl_is_tuple_type(a)) {
            if (jl_nparams(a) < 1)
                return NULL;
            return first_arg_datatype(jl_tparam0(a), 1);
        }
        return NULL;
    }
    else if (jl_is_typevar(a)) {
        return first_arg_datatype(((jl_tvar_t*)a)->ub, got_tuple1);
    }
    else if (jl_is_unionall(a)) {
        return first_arg_datatype(((jl_unionall_t*)a)->body, got_tuple1);
    }
    else if (jl_is_uniontype(a)) {
        jl_uniontype_t *u = (jl_uniontype_t*)a;
        jl_datatype_t *d1 = first_arg_datatype(u->a, got_tuple1);
        if (d1 == NULL) return NULL;
        jl_datatype_t *d2 = first_arg_datatype(u->b, got_tuple1);
        if (d2 == NULL || d1->name != d2->name)
            return NULL;
        return d1;
    }
    return NULL;
}

// get DataType of first tuple element, or NULL if cannot be determined
JL_DLLEXPORT jl_datatype_t *jl_first_argument_datatype(jl_value_t *argtypes)
{
    return first_arg_datatype(argtypes, 0);
}

// get DataType implied by a single given type, or `nothing`
JL_DLLEXPORT jl_value_t *jl_argument_datatype(jl_value_t *argt)
{
    jl_datatype_t *dt = first_arg_datatype(argt, 1);
    if (dt == NULL)
        return jl_nothing;
    return (jl_value_t*)dt;
}

extern tracer_cb jl_newmeth_tracer;

JL_DLLEXPORT void jl_method_def(jl_svec_t *argdata,
                                jl_code_info_t *f,
                                jl_module_t *module,
                                jl_value_t *isstaged)
{
    // argdata is svec(svec(types...), svec(typevars...))
    jl_svec_t *atypes = (jl_svec_t*)jl_svecref(argdata, 0);
    jl_svec_t *tvars = (jl_svec_t*)jl_svecref(argdata, 1);
    size_t nargs = jl_svec_len(atypes);
    int isva = jl_is_vararg_type(jl_svecref(atypes, nargs - 1));
    assert(jl_is_svec(atypes));
    assert(nargs > 0);
    assert(jl_is_svec(tvars));
    if (!jl_is_type(jl_svecref(atypes, 0)) || (isva && nargs == 1))
        jl_error("function type in method definition is not a type");
    jl_methtable_t *mt;
    jl_sym_t *name;
    jl_method_t *m = NULL;
    jl_value_t *argtype = NULL;
    JL_GC_PUSH3(&f, &m, &argtype);
    size_t i, na = jl_svec_len(atypes);
    int32_t nospec = 0;
    for (i = 1; i < na; i++) {
        jl_value_t *ti = jl_svecref(atypes, i);
        if (ti == jl_ANY_flag ||
            (jl_is_vararg_type(ti) && jl_tparam0(jl_unwrap_unionall(ti)) == jl_ANY_flag)) {
            jl_depwarn("`x::ANY` is deprecated, use `@nospecialize(x)` instead.",
                       (jl_value_t*)jl_symbol("ANY"));
            if (i <= 32)
                nospec |= (1 << (i - 1));
            jl_svecset(atypes, i, jl_substitute_var(ti, (jl_tvar_t*)jl_ANY_flag, (jl_value_t*)jl_any_type));
        }
    }

    argtype = (jl_value_t*)jl_apply_tuple_type(atypes);
    for (i = jl_svec_len(tvars); i > 0; i--) {
        jl_value_t *tv = jl_svecref(tvars, i - 1);
        if (!jl_is_typevar(tv))
            jl_type_error_rt("method definition", "type parameter", (jl_value_t*)jl_tvar_type, tv);
        argtype = jl_new_struct(jl_unionall_type, tv, argtype);
    }

    jl_datatype_t *ftype = jl_first_argument_datatype(argtype);
    if (ftype == NULL ||
        !(jl_is_type_type((jl_value_t*)ftype) ||
          (jl_is_datatype(ftype) &&
           (!ftype->abstract || jl_is_leaf_type((jl_value_t*)ftype)) &&
           ftype->name->mt != NULL)))
        jl_error("cannot add methods to an abstract type");
    if (jl_subtype((jl_value_t*)ftype, (jl_value_t*)jl_builtin_type))
        jl_error("cannot add methods to a builtin function");

    mt = ftype->name->mt;
    name = mt->name;
    if (!jl_is_code_info(f)) {
        // this occurs when there is a closure being added to an out-of-scope function
        // the user should only do this at the toplevel
        // the result is that the closure variables get interpolated directly into the AST
        f = jl_new_code_info_from_ast((jl_expr_t*)f);
    }
    m = jl_new_method(f, name, module, (jl_tupletype_t*)argtype, nargs, isva, tvars, isstaged == jl_true);
    m->nospecialize |= nospec;

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

    jl_check_static_parameter_conflicts(m, f, tvars);
    jl_method_table_insert(mt, m, NULL);
    if (jl_newmeth_tracer)
        jl_call_tracer(jl_newmeth_tracer, (jl_value_t*)m);
    JL_GC_POP();
}

#ifdef __cplusplus
}
#endif

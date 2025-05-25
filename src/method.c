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
#include "builtin_proto.h"

#ifdef __cplusplus
extern "C" {
#endif

jl_methtable_t *jl_kwcall_mt;
jl_method_t *jl_opaque_closure_method;

static void check_c_types(const char *where, jl_value_t *rt, jl_value_t *at)
{
    if (jl_is_svec(rt))
        jl_errorf("%s: missing return type", where);
    JL_TYPECHKS(where, type, rt);
    if (!jl_type_mappable_to_c(rt))
        jl_errorf("%s: return type doesn't correspond to a C type", where);
    JL_TYPECHKS(where, simplevector, at);
    int i, l = jl_svec_len(at);
    for (i = 0; i < l; i++) {
        jl_value_t *ati = jl_svecref(at, i);
        if (jl_is_vararg(ati))
            jl_errorf("%s: Vararg not allowed for argument list", where);
        JL_TYPECHKS(where, type, ati);
        if (!jl_type_mappable_to_c(ati))
            jl_errorf("%s: argument %d type doesn't correspond to a C type", where, i + 1);
    }
}

void jl_add_scanned_method(jl_module_t *m, jl_method_t *meth)
{
    JL_LOCK(&m->lock);
    if (m->scanned_methods == jl_nothing) {
        m->scanned_methods = (jl_value_t*)jl_alloc_vec_any(0);
        jl_gc_wb(m, m->scanned_methods);
    }
    jl_array_ptr_1d_push((jl_array_t*)m->scanned_methods, (jl_value_t*)meth);
    JL_UNLOCK(&m->lock);
}

JL_DLLEXPORT void jl_scan_method_source_now(jl_method_t *m, jl_value_t *src)
{
    if (!jl_atomic_fetch_or(&m->did_scan_source, 1)) {
        jl_code_info_t *code = NULL;
        JL_GC_PUSH1(&code);
        if (!jl_is_code_info(src))
            code = jl_uncompress_ir(m, NULL, src);
        else
            code = (jl_code_info_t*)src;
        jl_array_t *stmts = code->code;
        size_t i, l = jl_array_nrows(stmts);
        int any_implicit = 0;
        for (i = 0; i < l; i++) {
            jl_value_t *stmt = jl_array_ptr_ref(stmts, i);
            if (jl_is_globalref(stmt)) {
                jl_globalref_t *gr = (jl_globalref_t*)stmt;
                jl_binding_t *b = gr->binding;
                if (!b)
                    b = jl_get_module_binding(gr->mod, gr->name, 1);
                any_implicit |= jl_maybe_add_binding_backedge(b, (jl_value_t*)m, m);
            }
        }
        if (any_implicit && !(jl_atomic_fetch_or(&m->did_scan_source, 0x2) & 0x2))
            jl_add_scanned_method(m->module, m);
        JL_GC_POP();
    }
}

// Resolve references to non-locally-defined variables to become references to global
// variables in `module` (unless the rvalue is one of the type parameters in `sparam_vals`).
static jl_value_t *resolve_definition_effects(jl_value_t *expr, jl_module_t *module, jl_svec_t *sparam_vals, jl_value_t *binding_edge,
                                   int binding_effects, int eager_resolve)
{
    if (jl_is_symbol(expr)) {
        jl_errorf("Found raw symbol %s in code returned from lowering. Expected all symbols to have been resolved to GlobalRef or slots.",
                  jl_symbol_name((jl_sym_t*)expr));
    }

    if (!jl_is_expr(expr)) {
        return expr;
    }

    jl_expr_t *e = (jl_expr_t*)expr;
    if (e->head == jl_global_sym && binding_effects) {
        // execute the side-effects of "global x" decl immediately:
        // creates uninitialized mutable binding in module for each global
        jl_eval_global_expr(module, e, 1);
        return jl_nothing;
    }
    if (e->head == jl_globaldecl_sym && binding_effects) {
        assert(jl_expr_nargs(e) == 1);
        jl_declare_global(module, jl_exprarg(e, 0), NULL, 1);
        return jl_nothing;
    }
    // These exprs are not fully linearized
    if (e->head == jl_assign_sym) {
        jl_exprargset(e, 1, resolve_definition_effects(jl_exprarg(e, 1), module, sparam_vals, binding_edge, binding_effects, eager_resolve));
        return expr;
    } else if (e->head == jl_new_opaque_closure_sym) {
        jl_exprargset(e, 4, resolve_definition_effects(jl_exprarg(e, 4), module, sparam_vals, binding_edge, binding_effects, eager_resolve));
        return expr;
    }
    size_t nargs = jl_array_nrows(e->args);
    if (e->head == jl_opaque_closure_method_sym) {
        if (nargs != 5) {
            jl_error("opaque_closure_method: invalid syntax");
        }
        jl_value_t *name = jl_exprarg(e, 0);
        jl_value_t *oc_nargs = jl_exprarg(e, 1);
        int isva = jl_exprarg(e, 2) == jl_true;
        jl_value_t *functionloc = jl_exprarg(e, 3);
        jl_value_t *ci = jl_exprarg(e, 4);
        if (!jl_is_code_info(ci)) {
            jl_error("opaque_closure_method: lambda should be a CodeInfo");
        } else if (!jl_is_long(oc_nargs)) {
            jl_type_error("opaque_closure_method", (jl_value_t*)jl_long_type, oc_nargs);
        }
        jl_method_t *m = jl_make_opaque_closure_method(module, name,
            jl_unbox_long(oc_nargs), functionloc, (jl_code_info_t*)ci, isva, /*isinferred*/0);
        return (jl_value_t*)m;
    }
    if (e->head == jl_cfunction_sym) {
        JL_NARGS(cfunction method definition, 5, 5); // (type, func, rt, at, cc)
        jl_task_t *ct = jl_current_task;
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
                if (jl_typetagis(jl_current_exception(ct), jl_errorexception_type))
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
                if (jl_typetagis(jl_current_exception(ct), jl_errorexception_type))
                    jl_error("could not evaluate cfunction argument type (it might depend on a local variable)");
                else
                    jl_rethrow();
            }
            jl_exprargset(e, 3, at);
        }
        check_c_types("cfunction method definition", rt, at);
        JL_TYPECHK(cfunction method definition, quotenode, jl_exprarg(e, 4));
        JL_TYPECHK(cfunction method definition, symbol, *(jl_value_t**)jl_exprarg(e, 4));
        return expr;
    }
    if (e->head == jl_foreigncall_sym) {
        JL_NARGSV(ccall method definition, 5); // (fptr, rt, at, nreq, (cc, effects, gc_safe))
        jl_task_t *ct = jl_current_task;
        jl_value_t *rt = jl_exprarg(e, 1);
        jl_value_t *at = jl_exprarg(e, 2);
        if (!jl_is_type(rt)) {
            JL_TRY {
                rt = jl_interpret_toplevel_expr_in(module, rt, NULL, sparam_vals);
            }
            JL_CATCH {
                if (jl_typetagis(jl_current_exception(ct), jl_errorexception_type))
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
                if (jl_typetagis(jl_current_exception(ct), jl_errorexception_type))
                    jl_error("could not evaluate ccall argument type (it might depend on a local variable)");
                else
                    jl_rethrow();
            }
            jl_exprargset(e, 2, at);
        }
        check_c_types("ccall method definition", rt, at);
        JL_TYPECHK(ccall method definition, long, jl_exprarg(e, 3));
        JL_TYPECHK(ccall method definition, quotenode, jl_exprarg(e, 4));
        jl_value_t *cc = jl_quotenode_value(jl_exprarg(e, 4));
        if (!jl_is_symbol(cc)) {
            JL_TYPECHK(ccall method definition, tuple, cc);
            if (jl_nfields(cc) != 3) {
                jl_error("In ccall calling convention, expected two argument tuple or symbol.");
            }
            JL_TYPECHK(ccall method definition, symbol, jl_get_nth_field(cc, 0));
            JL_TYPECHK(ccall method definition, uint16, jl_get_nth_field(cc, 1));
            JL_TYPECHK(ccall method definition, bool, jl_get_nth_field(cc, 2));
        }
    }
    if (e->head == jl_call_sym && nargs > 0 &&
            jl_is_globalref(jl_exprarg(e, 0))) {
        // TODO: this hack should be deleted once llvmcall is fixed
        jl_value_t *fe = jl_exprarg(e, 0);
        jl_module_t *fe_mod = jl_globalref_mod(fe);
        jl_sym_t *fe_sym = jl_globalref_name(fe);
        // look at some known called functions
        jl_binding_t *b = jl_get_binding(fe_mod, fe_sym);
        if (jl_get_latest_binding_value_if_const(b) == BUILTIN(tuple)) {
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
                    val = NULL; // To make the analyzer happy see #define JL_TRY
                }
                if (val)
                    return val;
            }
        }
    }
    return expr;
}

JL_DLLEXPORT void jl_resolve_definition_effects_in_ir(jl_array_t *stmts, jl_module_t *m, jl_svec_t *sparam_vals, jl_value_t *binding_edge,
                              int binding_effects)
{
    size_t i, l = jl_array_nrows(stmts);
    for (i = 0; i < l; i++) {
        jl_value_t *stmt = jl_array_ptr_ref(stmts, i);
        jl_array_ptr_set(stmts, i, resolve_definition_effects(stmt, m, sparam_vals, binding_edge, binding_effects, 0));
    }
}

jl_value_t *expr_arg1(jl_value_t *expr) {
    jl_array_t *args = ((jl_expr_t*)expr)->args;
    return jl_array_ptr_ref(args, 0);
}

static jl_value_t *alloc_edges(arraylist_t *edges_list)
{
    jl_value_t *jledges = (jl_value_t*)jl_alloc_svec(edges_list->len);
    jl_value_t *jledges2 = NULL;
    jl_value_t *codelocs = NULL;
    JL_GC_PUSH3(&jledges, &jledges2, &codelocs);
    size_t i;
    for (i = 0; i < edges_list->len; i++) {
        arraylist_t *edge = (arraylist_t*)edges_list->items[i];
        jl_value_t *file = (jl_value_t*)edge->items[0];
        int32_t line = 0; // not preserved by lowering (and probably lost even before that)
        arraylist_t *edges_list2 = (arraylist_t*)edge->items[1];
        size_t j, nlocs = (edge->len - 2) / 3;
        codelocs = (jl_value_t*)jl_alloc_array_1d(jl_array_int32_type, nlocs * 3);
        for (j = 0; j < nlocs; j++) {
            jl_array_data(codelocs,int32_t)[3 * j + 0] = (intptr_t)edge->items[3 * j + 0 + 2];
            jl_array_data(codelocs,int32_t)[3 * j + 1] = (intptr_t)edge->items[3 * j + 1 + 2];
            jl_array_data(codelocs,int32_t)[3 * j + 2] = (intptr_t)edge->items[3 * j + 2 + 2];
        }
        codelocs = (jl_value_t*)jl_compress_codelocs(line, codelocs, nlocs);
        jledges2 = alloc_edges(edges_list2);
        jl_value_t *debuginfo = jl_new_struct(jl_debuginfo_type, file, jl_nothing, jledges2, codelocs);
        jledges2 = NULL;
        jl_svecset(jledges, i, debuginfo);
        free(edges_list2);
        free(edge);
    }
    JL_GC_POP();
    return jledges;
}

static void add_edge(arraylist_t *edges_list, arraylist_t *inlinestack, int32_t *p_to, int32_t *p_pc)
{
    jl_value_t *locinfo = (jl_value_t*)arraylist_pop(inlinestack);
    jl_sym_t *filesym = (jl_sym_t*)jl_fieldref_noalloc(locinfo, 0);
    int32_t line = jl_unbox_int32(jl_fieldref(locinfo, 1));
    size_t i;
    arraylist_t *edge = NULL;
    for (i = 0; i < edges_list->len; i++) {
        edge = (arraylist_t*)edges_list->items[i];
        if (edge->items[0] == filesym)
            break;
    }
    if (i == edges_list->len) {
        edge = (arraylist_t*)malloc(sizeof(arraylist_t));
        arraylist_t *edge_list2 = (arraylist_t*)malloc(sizeof(arraylist_t));
        arraylist_new(edge, 0);
        arraylist_new(edge_list2, 0);
        arraylist_push(edge, (void*)filesym);
        arraylist_push(edge, (void*)edge_list2);
        arraylist_push(edges_list, (void*)edge);
    }
    *p_to = i + 1;
    int32_t to = 0, pc = 0;
    if (inlinestack->len) {
        arraylist_t *edge_list2 = (arraylist_t*)edge->items[1];
        add_edge(edge_list2, inlinestack, &to, &pc);
    }
    for (i = 2; i < edge->len; i += 3) {
        if ((intptr_t)edge->items[i + 0] == line &&
            (intptr_t)edge->items[i + 1] == to &&
            (intptr_t)edge->items[i + 2] == pc) {
            break;
        }
    }
    if (i == edge->len) {
        arraylist_push(edge, (void*)(intptr_t)line);
        arraylist_push(edge, (void*)(intptr_t)to);
        arraylist_push(edge, (void*)(intptr_t)pc);
    }
    *p_pc = (i - 2) / 3 + 1;
}

jl_debuginfo_t *jl_linetable_to_debuginfo(jl_array_t *codelocs_any, jl_array_t *linetable)
{
    size_t nlocs = jl_array_nrows(codelocs_any);
    jl_value_t *toplocinfo = jl_array_ptr_ref(linetable, 0);
    jl_sym_t *topfile = (jl_sym_t*)jl_fieldref_noalloc(toplocinfo, 0);
    int32_t topline = jl_unbox_int32(jl_fieldref(toplocinfo, 1));
    arraylist_t inlinestack;
    arraylist_new(&inlinestack, 0);
    arraylist_t edges_list;
    arraylist_new(&edges_list, 0);
    jl_value_t *jledges = NULL;
    jl_value_t *codelocs = (jl_value_t*)jl_alloc_array_1d(jl_array_int32_type, nlocs * 3);
    jl_debuginfo_t *debuginfo = NULL;
    JL_GC_PUSH3(&jledges, &codelocs, &debuginfo);
    int32_t *codelocs32 = jl_array_data(codelocs,int32_t);
    size_t j;
    for (j = 0; j < nlocs; j++) {
        size_t lineidx = jl_unbox_long(jl_array_ptr_ref((jl_array_t*)codelocs_any, j)); // 1 indexed!
        while (lineidx != 0) {
            jl_value_t *locinfo = jl_array_ptr_ref(linetable, lineidx - 1);
            lineidx = jl_unbox_int32(jl_fieldref(locinfo, 2));
            arraylist_push(&inlinestack, locinfo);
        }
        int32_t line = 0, to = 0, pc = 0;
        if (inlinestack.len) {
            jl_value_t *locinfo = (jl_value_t*)arraylist_pop(&inlinestack);
            jl_sym_t *filesym = (jl_sym_t*)jl_fieldref_noalloc(locinfo, 0);
            if (filesym == topfile)
                line = jl_unbox_int32(jl_fieldref(locinfo, 1));
            else
                arraylist_push(&inlinestack, locinfo);
            if (inlinestack.len) {
                add_edge(&edges_list, &inlinestack, &to, &pc);
            }
        }
        codelocs32[j * 3 + 0] = line;
        codelocs32[j * 3 + 1] = to;
        codelocs32[j * 3 + 2] = pc;
    }
    codelocs = (jl_value_t*)jl_compress_codelocs(topline, codelocs, nlocs);
    jledges = alloc_edges(&edges_list);
    debuginfo = (jl_debuginfo_t*)jl_new_struct(jl_debuginfo_type, topfile, jl_nothing, jledges, codelocs);
    JL_GC_POP();
    return debuginfo;
}

// copy a :lambda Expr into its CodeInfo representation,
// including popping of known meta nodes
jl_code_info_t *jl_new_code_info_from_ir(jl_expr_t *ir)
{
    jl_code_info_t *li = NULL;
    JL_GC_PUSH1(&li);
    li = jl_new_code_info_uninit();

    jl_expr_t *arglist = (jl_expr_t*)jl_exprarg(ir, 0);
    li->nargs = jl_array_len(arglist);

    assert(jl_is_expr(ir));
    jl_expr_t *bodyex = (jl_expr_t*)jl_exprarg(ir, 2);

    jl_array_t *codelocs_any = (jl_array_t*)jl_exprarg(ir, 3);
    jl_array_t *linetable = (jl_array_t*)jl_exprarg(ir, 4);
    li->debuginfo = jl_linetable_to_debuginfo(codelocs_any, linetable);
    jl_gc_wb(li, li->debuginfo);

    assert(jl_is_expr(bodyex));
    jl_array_t *body = bodyex->args;
    li->code = body;
    jl_gc_wb(li, li->code);
    size_t n = jl_array_nrows(body);
    jl_value_t **bd = (jl_value_t**)jl_array_ptr_data((jl_array_t*)li->code);
    li->ssaflags = jl_alloc_array_1d(jl_array_uint32_type, n);
    jl_gc_wb(li, li->ssaflags);
    int inbounds_depth = 0; // number of stacked inbounds
    // isempty(inline_flags): no user callsite inline annotation
    // last(inline_flags) == 1: callsite inline region
    // last(inline_flags) == 0: callsite noinline region
    arraylist_t *inline_flags = arraylist_new((arraylist_t*)malloc_s(sizeof(arraylist_t)), 0);
    arraylist_t *purity_exprs = arraylist_new((arraylist_t*)malloc_s(sizeof(arraylist_t)), 0);
    size_t j;
    for (j = 0; j < n; j++) {
        jl_value_t *st = bd[j];
        int is_flag_stmt = 0;
        // check :meta expression
        if (jl_is_expr(st) && ((jl_expr_t*)st)->head == jl_meta_sym) {
            size_t k, ins = 0, na = jl_expr_nargs(st);
            jl_array_t *meta = ((jl_expr_t*)st)->args;
            for (k = 0; k < na; k++) {
                jl_value_t *ma = jl_array_ptr_ref(meta, k);
                if (ma == (jl_value_t*)jl_inline_sym)
                    li->inlining = 1;
                else if (ma == (jl_value_t*)jl_noinline_sym)
                    li->inlining = 2;
                else if (ma == (jl_value_t*)jl_propagate_inbounds_sym)
                    li->propagate_inbounds = 1;
                else if (ma == (jl_value_t*)jl_nospecializeinfer_sym)
                    li->nospecializeinfer = 1;
                else if (ma == (jl_value_t*)jl_aggressive_constprop_sym)
                    li->constprop = 1;
                else if (ma == (jl_value_t*)jl_no_constprop_sym)
                    li->constprop = 2;
                else if (jl_is_expr(ma) && ((jl_expr_t*)ma)->head == jl_purity_sym) {
                    if (jl_expr_nargs(ma) == NUM_EFFECTS_OVERRIDES) {
                        // N.B. this code allows multiple :purity expressions to be present in a single `:meta` node
                        int8_t consistent = jl_unbox_bool(jl_exprarg(ma, 0));
                        if (consistent) li->purity.overrides.ipo_consistent = consistent;
                        int8_t effect_free = jl_unbox_bool(jl_exprarg(ma, 1));
                        if (effect_free) li->purity.overrides.ipo_effect_free = effect_free;
                        int8_t nothrow = jl_unbox_bool(jl_exprarg(ma, 2));
                        if (nothrow) li->purity.overrides.ipo_nothrow = nothrow;
                        int8_t terminates_globally = jl_unbox_bool(jl_exprarg(ma, 3));
                        if (terminates_globally) li->purity.overrides.ipo_terminates_globally = terminates_globally;
                        int8_t terminates_locally = jl_unbox_bool(jl_exprarg(ma, 4));
                        if (terminates_locally) li->purity.overrides.ipo_terminates_locally = terminates_locally;
                        int8_t notaskstate = jl_unbox_bool(jl_exprarg(ma, 5));
                        if (notaskstate) li->purity.overrides.ipo_notaskstate = notaskstate;
                        int8_t inaccessiblememonly = jl_unbox_bool(jl_exprarg(ma, 6));
                        if (inaccessiblememonly) li->purity.overrides.ipo_inaccessiblememonly = inaccessiblememonly;
                        int8_t noub = jl_unbox_bool(jl_exprarg(ma, 7));
                        if (noub) li->purity.overrides.ipo_noub = noub;
                        int8_t noub_if_noinbounds = jl_unbox_bool(jl_exprarg(ma, 8));
                        if (noub_if_noinbounds) li->purity.overrides.ipo_noub_if_noinbounds = noub_if_noinbounds;
                        int8_t consistent_overlay = jl_unbox_bool(jl_exprarg(ma, 9));
                        if (consistent_overlay) li->purity.overrides.ipo_consistent_overlay = consistent_overlay;
                        int8_t nortcall = jl_unbox_bool(jl_exprarg(ma, 10));
                        if (nortcall) li->purity.overrides.ipo_nortcall = nortcall;
                    } else {
                        assert(jl_expr_nargs(ma) == 0);
                    }
                }
                else
                    jl_array_ptr_set(meta, ins++, ma);
            }
            if (ins == 0)
                bd[j] = jl_nothing;
            else
                jl_array_del_end(meta, na - ins);
        }
        // check other flag expressions
        else if (jl_is_expr(st) && ((jl_expr_t*)st)->head == jl_inbounds_sym) {
            is_flag_stmt = 1;
            jl_value_t *arg1 = expr_arg1(st);
            if (arg1 == (jl_value_t*)jl_true)       // push
                inbounds_depth += 1;
            else if (arg1 == (jl_value_t*)jl_false) // clear
                inbounds_depth = 0;
            else if (inbounds_depth > 0)            // pop
                inbounds_depth -= 1;
            bd[j] = jl_nothing;
        }
        else if (jl_is_expr(st) && ((jl_expr_t*)st)->head == jl_inline_sym) {
            is_flag_stmt = 1;
            jl_value_t *arg1 = expr_arg1(st);
            if (arg1 == (jl_value_t*)jl_true) // enter inline region
                arraylist_push(inline_flags, (void*)1);
            else {                            // exit inline region
                assert(arg1 == (jl_value_t*)jl_false);
                arraylist_pop(inline_flags);
            }
            bd[j] = jl_nothing;
        }
        else if (jl_is_expr(st) && ((jl_expr_t*)st)->head == jl_noinline_sym) {
            is_flag_stmt = 1;
            jl_value_t *arg1 = expr_arg1(st);
            if (arg1 == (jl_value_t*)jl_true) // enter noinline region
                arraylist_push(inline_flags, (void*)0);
            else {                             // exit noinline region
                assert(arg1 == (jl_value_t*)jl_false);
                arraylist_pop(inline_flags);
            }
            bd[j] = jl_nothing;
        }
        else if (jl_is_expr(st) && ((jl_expr_t*)st)->head == jl_purity_sym) {
            is_flag_stmt = 1;
            size_t na = jl_expr_nargs(st);
            if (na == NUM_EFFECTS_OVERRIDES)
                arraylist_push(purity_exprs, (void*)st);
            else {
                assert(na == 0);
                arraylist_pop(purity_exprs);
            }
            bd[j] = jl_nothing;
        }
        else if (jl_is_expr(st) && ((jl_expr_t*)st)->head == jl_boundscheck_sym)
            // Don't set IR_FLAG_INBOUNDS on boundscheck at the same level
            is_flag_stmt = 1;
        else if (jl_is_expr(st) && ((jl_expr_t*)st)->head == jl_return_sym)
            jl_array_ptr_set(body, j, jl_new_struct(jl_returnnode_type, jl_exprarg(st, 0)));
        else if (jl_is_globalref(st)) {
            jl_globalref_t *gr = (jl_globalref_t*)st;
            if (jl_object_in_image((jl_value_t*)gr->mod))
                li->has_image_globalref = 1;
        }
        else {
            if (jl_is_expr(st) && ((jl_expr_t*)st)->head == jl_assign_sym)
                st = jl_exprarg(st, 1);
            if (jl_is_expr(st) && (((jl_expr_t*)st)->head == jl_foreigncall_sym || ((jl_expr_t*)st)->head == jl_cfunction_sym))
                li->has_fcall = 1;
        }
        if (is_flag_stmt)
            jl_array_uint32_set(li->ssaflags, j, 0);
        else {
            uint32_t flag = 0;
            if (inbounds_depth > 0)
                flag |= IR_FLAG_INBOUNDS;
            if (inline_flags->len > 0) {
                void* inline_flag = inline_flags->items[inline_flags->len-1];
                flag |= 1 << (inline_flag ? 1 : 2);
            }
            int n_purity_exprs = purity_exprs->len;
            if (n_purity_exprs > 0) {
                // apply all purity overrides
                for (int i = 0; i < n_purity_exprs; i++) {
                    void* purity_expr = purity_exprs->items[i];
                    for (int j = 0; j < NUM_EFFECTS_OVERRIDES; j++) {
                        flag |= jl_unbox_bool(jl_exprarg((jl_value_t*)purity_expr, j)) ? (1 << (NUM_IR_FLAGS+j)) : 0;
                    }
                }
            }
            jl_array_uint32_set(li->ssaflags, j, flag);
        }
    }
    assert(inline_flags->len == 0 && purity_exprs->len == 0); // malformed otherwise
    arraylist_free(inline_flags); arraylist_free(purity_exprs);
    free(inline_flags); free(purity_exprs);
    jl_array_t *vinfo = (jl_array_t*)jl_exprarg(ir, 1);
    jl_array_t *vis = (jl_array_t*)jl_array_ptr_ref(vinfo, 0);
    size_t nslots = jl_array_nrows(vis);
    jl_value_t *ssavalue_types = jl_array_ptr_ref(vinfo, 2);
    assert(jl_is_long(ssavalue_types));
    size_t nssavalue = jl_unbox_long(ssavalue_types);
    li->slotnames = jl_alloc_array_1d(jl_array_symbol_type, nslots);
    jl_gc_wb(li, li->slotnames);
    li->slotflags = jl_alloc_array_1d(jl_array_uint8_type, nslots);
    jl_gc_wb(li, li->slotflags);
    li->ssavaluetypes = jl_box_long(nssavalue);
    jl_gc_wb(li, li->ssavaluetypes);

    // Flags that need to be copied to slotflags
    const uint8_t vinfo_mask = 8 | 16 | 32 | 64;
    int i;
    for (i = 0; i < nslots; i++) {
        jl_value_t *vi = jl_array_ptr_ref(vis, i);
        jl_sym_t *name = (jl_sym_t*)jl_array_ptr_ref(vi, 0);
        assert(jl_is_symbol(name));
        char *str = jl_symbol_name(name);
        if (i > 0 && name != jl_unused_sym) {
            if (str[0] == '#') {
                // convention for renamed variables: #...#original_name
                char *nxt = strchr(str + 1, '#');
                if (nxt)
                    name = jl_symbol(nxt+1);
                else if (str[1] == 's')  // compiler-generated temporaries, #sXXX
                    name = jl_empty_sym;
            }
        }
        jl_array_ptr_set(li->slotnames, i, name);
        jl_array_uint8_set(li->slotflags, i, vinfo_mask & jl_unbox_long(jl_array_ptr_ref(vi, 2)));
    }
    JL_GC_POP();
    return li;
}

JL_DLLEXPORT jl_method_instance_t *jl_new_method_instance_uninit(void)
{
    jl_task_t *ct = jl_current_task;
    jl_method_instance_t *mi =
        (jl_method_instance_t*)jl_gc_alloc(ct->ptls, sizeof(jl_method_instance_t),
                                           jl_method_instance_type);
    mi->def.value = NULL;
    mi->specTypes = NULL;
    mi->sparam_vals = jl_emptysvec;
    mi->backedges = NULL;
    jl_atomic_store_relaxed(&mi->cache, NULL);
    mi->cache_with_orig = 0;
    jl_atomic_store_relaxed(&mi->flags, 0);
    return mi;
}

JL_DLLEXPORT jl_code_info_t *jl_new_code_info_uninit(void)
{
    jl_task_t *ct = jl_current_task;
    jl_code_info_t *src =
        (jl_code_info_t*)jl_gc_alloc(ct->ptls, sizeof(jl_code_info_t),
                                     jl_code_info_type);
    src->code = NULL;
    src->debuginfo = NULL;
    src->ssavaluetypes = NULL;
    src->ssaflags = NULL;
    src->method_for_inference_limit_heuristics = jl_nothing;
    src->slotflags = NULL;
    src->slotnames = NULL;
    src->slottypes = jl_nothing;
    src->rettype = (jl_value_t*)jl_any_type;
    src->edges = (jl_value_t*)jl_emptysvec;
    src->parent = (jl_method_instance_t*)jl_nothing;
    src->min_world = 1;
    src->max_world = ~(size_t)0;
    src->propagate_inbounds = 0;
    src->has_fcall = 0;
    src->has_image_globalref = 0;
    src->nospecializeinfer = 0;
    src->constprop = 0;
    src->inlining = 0;
    src->purity.bits = 0;
    src->nargs = 0;
    src->isva = 0;
    src->inlining_cost = UINT16_MAX;
    return src;
}

// invoke (compiling if necessary) the jlcall function pointer for a method template
static jl_value_t *jl_call_staged(jl_method_t *def, jl_value_t *generator,
        size_t world, jl_svec_t *sparam_vals, jl_value_t **args, uint32_t nargs)
{
    size_t n_sparams = jl_svec_len(sparam_vals);
    jl_value_t **gargs;
    size_t totargs = 2 + n_sparams + def->nargs;
    JL_GC_PUSHARGS(gargs, totargs);
    gargs[0] = jl_box_ulong(world);
    gargs[1] = (jl_value_t*)def;
    memcpy(&gargs[2], jl_svec_data(sparam_vals), n_sparams * sizeof(void*));
    memcpy(&gargs[2 + n_sparams], args, (def->nargs - def->isva) * sizeof(void*));
    if (def->isva)
        gargs[totargs - 1] = jl_f_tuple(NULL, &args[def->nargs - 1], nargs - def->nargs + 1);
    jl_value_t *code = jl_apply_generic(generator, gargs, totargs);
    JL_GC_POP();
    return code;
}

JL_DLLEXPORT jl_code_instance_t *jl_cached_uninferred(jl_code_instance_t *codeinst, size_t world)
{
    for (; codeinst; codeinst = jl_atomic_load_relaxed(&codeinst->next)) {
        if (codeinst->owner != (void*)jl_uninferred_sym)
            continue;
        if (jl_atomic_load_relaxed(&codeinst->min_world) <= world && world <= jl_atomic_load_relaxed(&codeinst->max_world)) {
            return codeinst;
        }
    }
    return NULL;
}

JL_DLLEXPORT jl_code_instance_t *jl_cache_uninferred(jl_method_instance_t *mi, jl_code_instance_t *checked, size_t world, jl_code_instance_t *newci)
{
    while (!jl_mi_try_insert(mi, checked, newci)) {
        jl_code_instance_t *new_checked = jl_atomic_load_relaxed(&mi->cache);
        // Check if another thread inserted a CodeInstance that covers this world
        jl_code_instance_t *other = jl_cached_uninferred(new_checked, world);
        if (other)
            return other;
        checked = new_checked;
    }
    // Successfully inserted
    return newci;
}

// Return a newly allocated CodeInfo for the function signature
// effectively described by the tuple (specTypes, env, Method) inside linfo
JL_DLLEXPORT jl_code_info_t *jl_code_for_staged(jl_method_instance_t *mi, size_t world, jl_code_instance_t **cache)
{
    jl_code_instance_t *cache_ci = jl_atomic_load_relaxed(&mi->cache);
    jl_code_instance_t *uninferred_ci = jl_cached_uninferred(cache_ci, world);
    if (uninferred_ci) {
        // The uninferred code is in `inferred`, but that is a bit of a misnomer here.
        // This is the cached output the generated function (or top-level thunk).
        // This cache has a non-standard owner (indicated by `->owner === :uninferred`),
        // so it doesn't get confused for inference results.
        jl_code_info_t *src = (jl_code_info_t*)jl_atomic_load_relaxed(&uninferred_ci->inferred);
        assert(jl_is_code_info(src)); // make sure this did not get `nothing` put here
        return (jl_code_info_t*)jl_copy_ast((jl_value_t*)src);
    }

    JL_TIMING(STAGED_FUNCTION, STAGED_FUNCTION);
    jl_value_t *tt = mi->specTypes;
    jl_method_t *def = mi->def.method;
    jl_timing_show_method_instance(mi, JL_TIMING_DEFAULT_BLOCK);
    jl_value_t *generator = def->generator;
    assert(generator != NULL);
    assert(jl_is_method(def));
    jl_code_info_t *func = NULL;
    jl_value_t *ex = NULL;
    jl_value_t *kind = NULL;
    jl_code_info_t *uninferred = NULL;
    jl_code_instance_t *ci = NULL;
    JL_GC_PUSH5(&ex, &func, &uninferred, &ci, &kind);
    jl_task_t *ct = jl_current_task;
    int last_lineno = jl_lineno;
    int last_in = ct->ptls->in_pure_callback;
    size_t last_age = ct->world_age;

    JL_TRY {
        ct->ptls->in_pure_callback = 1;
        ct->world_age = jl_atomic_load_relaxed(&def->primary_world);
        if (ct->world_age > jl_atomic_load_acquire(&jl_world_counter))
            jl_error("The generator method cannot run until it is added to a method table.");

        // invoke code generator
        jl_tupletype_t *ttdt = (jl_tupletype_t*)jl_unwrap_unionall(tt);
        ex = jl_call_staged(def, generator, world, mi->sparam_vals, jl_svec_data(ttdt->parameters), jl_nparams(ttdt));

        // do some post-processing
        if (!jl_is_code_info(ex)) {
            jl_error("As of Julia 1.12, generated functions must return `CodeInfo`. See `Base.generated_body_to_codeinfo`.");
        }
        func = (jl_code_info_t*)ex;
        jl_array_t *stmts = (jl_array_t*)func->code;
        jl_resolve_definition_effects_in_ir(stmts, def->module, mi->sparam_vals, NULL, 1);
        ex = NULL;

        // If this generated function has an opaque closure, cache it for
        // correctness of method identity. In particular, other methods that call
        // this method may end up referencing it in a PartialOpaque lattice element
        // type. If the method identity were to change (for the same world age)
        // in between invocations of this method, that return type inference would
        // no longer be correct.
        int needs_cache_for_correctness = 0;
        for (int i = 0; i < jl_array_nrows(func->code); ++i) {
            jl_value_t *stmt = jl_array_ptr_ref(func->code, i);
            if (jl_is_expr(stmt) && ((jl_expr_t*)stmt)->head == jl_new_opaque_closure_sym) {
                if (jl_expr_nargs(stmt) >= 4 && jl_is_bool(jl_exprarg(stmt, 3)) && !jl_unbox_bool(jl_exprarg(stmt, 3))) {
                    // If this new_opaque_closure is prohibited from sourcing PartialOpaque,
                    // there is no problem
                    continue;
                }
                if (jl_options.incremental && jl_generating_output())
                    jl_error("Impossible to correctly handle OpaqueClosure inside @generated returned during precompile process.");
                needs_cache_for_correctness = 1;
                break;
            }
        }

        if ((func->edges == jl_nothing || func->edges == (jl_value_t*)jl_emptysvec) && func->max_world == ~(size_t)0) {
            if (func->min_world != 1) {
                jl_error("Generated function result with `edges == nothing` and `max_world == typemax(UInt)` must have `min_world == 1`");
            }
        }

        if (cache || needs_cache_for_correctness) {
            uninferred = (jl_code_info_t*)jl_copy_ast((jl_value_t*)func);
            ci = jl_new_codeinst_for_uninferred(mi, uninferred);
            jl_code_instance_t *cached_ci = jl_cache_uninferred(mi, cache_ci, world, ci);
            if (cached_ci != ci) {
                func = (jl_code_info_t*)jl_copy_ast(jl_atomic_load_relaxed(&cached_ci->inferred));
                assert(jl_is_code_info(func));
            }
            else if (uninferred->edges != jl_nothing) {
                // N.B.: This needs to match `store_backedges` on the julia side
                jl_value_t *edges = uninferred->edges;
                size_t l;
                jl_value_t **data;
                if (jl_is_svec(edges)) {
                    l = jl_svec_len(edges);
                    data = jl_svec_data(edges);
                }
                else {
                    l = jl_array_dim0(edges);
                    data = jl_array_data(edges, jl_value_t*);
                }
                for (size_t i = 0; i < l; ) {
                    kind = data[i++];
                    if (jl_is_method_instance(kind)) {
                        jl_method_instance_add_backedge((jl_method_instance_t*)kind, jl_nothing, ci);
                    }
                    else if (jl_is_binding(kind)) {
                        jl_add_binding_backedge((jl_binding_t*)kind, (jl_value_t*)ci);
                    }
                    else if (jl_is_mtable(kind)) {
                        assert(i < l);
                        ex = data[i++];
                        jl_method_table_add_backedge((jl_methtable_t*)kind, ex, ci);
                    }
                    else {
                        assert(i < l);
                        ex = data[i++];
                        jl_method_instance_add_backedge((jl_method_instance_t*)ex, kind, ci);
                    }
                }
            }
            if (cache)
                *cache = cached_ci;
        }

        ct->ptls->in_pure_callback = last_in;
        jl_lineno = last_lineno;
        ct->world_age = last_age;
    }
    JL_CATCH {
        ct->ptls->in_pure_callback = last_in;
        jl_lineno = last_lineno;
        jl_rethrow();
    }
    JL_GC_POP();
    return func;
}

JL_DLLEXPORT jl_code_info_t *jl_copy_code_info(jl_code_info_t *src)
{
    jl_task_t *ct = jl_current_task;
    jl_code_info_t *newsrc =
        (jl_code_info_t*)jl_gc_alloc(ct->ptls, sizeof(jl_code_info_t),
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

JL_DLLEXPORT void jl_method_set_source(jl_method_t *m, jl_code_info_t *src)
{
    uint8_t j;
    uint8_t called = 0;
    int gen_only = 0;
    for (j = 1; j < m->nargs && j <= sizeof(m->nospecialize) * 8; j++) {
        jl_value_t *ai = jl_array_ptr_ref(src->slotnames, j);
        if (ai == (jl_value_t*)jl_unused_sym) {
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
    m->nospecializeinfer = src->nospecializeinfer;
    m->constprop = src->constprop;
    m->purity.bits = src->purity.bits;

    jl_array_t *copy = NULL;
    jl_svec_t *sparam_vars = jl_outer_unionall_vars(m->sig);
    JL_GC_PUSH3(&copy, &sparam_vars, &src);
    assert(jl_typetagis(src->code, jl_array_any_type));
    jl_array_t *stmts = (jl_array_t*)src->code;
    size_t i, n = jl_array_nrows(stmts);
    copy = jl_alloc_vec_any(n);
    for (i = 0; i < n; i++) {
        jl_value_t *st = jl_array_ptr_ref(stmts, i);
        if (jl_is_expr(st) && ((jl_expr_t*)st)->head == jl_meta_sym) {
            size_t nargs = jl_expr_nargs(st);
            if (nargs >= 1 && jl_exprarg(st, 0) == (jl_value_t*)jl_nospecialize_sym) {
                if (nargs == 1) // bare `@nospecialize` is special: it prevents specialization on all args
                    m->nospecialize = -1;
                size_t j;
                for (j = 1; j < nargs; j++) {
                    jl_value_t *aj = jl_exprarg(st, j);
                    if (!jl_is_slotnumber(aj) && !jl_is_argument(aj))
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
            else if (nargs >= 1 && jl_exprarg(st, 0) == (jl_value_t*)jl_specialize_sym) {
                if (nargs == 1) // bare `@specialize` is special: it causes specialization on all args
                    m->nospecialize = 0;
                for (j = 1; j < nargs; j++) {
                    jl_value_t *aj = jl_exprarg(st, j);
                    if (!jl_is_slotnumber(aj) && !jl_is_argument(aj))
                        continue;
                    int sn = (int)jl_slot_number(aj) - 2;
                    if (sn < 0) // @specialize on self is valid but currently ignored
                        continue;
                    if (sn > (m->nargs - 2)) {
                        jl_error("@specialize annotation applied to a non-argument");
                    }
                    if (sn >= sizeof(m->nospecialize) * 8) {
                        jl_printf(JL_STDERR,
                                  "WARNING: @specialize annotation only supported on the first %d arguments.\n",
                                  (int)(sizeof(m->nospecialize) * 8));
                        continue;
                    }
                    m->nospecialize &= ~(1 << sn);
                }
                st = jl_nothing;
            }
            else if (nargs == 2 && jl_exprarg(st, 0) == (jl_value_t*)jl_generated_sym) {
                if (m->generator != NULL)
                    jl_error("duplicate @generated function body");
                jl_value_t *gexpr = jl_exprarg(st, 1);
                // the frontend would put (new (core GeneratedFunctionStub) funcname argnames sp) here, for example
                m->generator = jl_toplevel_eval(m->module, gexpr);
                jl_gc_wb(m, m->generator);
                st = jl_nothing;
            }
            else if (nargs == 1 && jl_exprarg(st, 0) == (jl_value_t*)jl_generated_only_sym) {
                gen_only = 1;
                st = jl_nothing;
            }
            else if (nargs == 2 && jl_exprarg(st, 0) == (jl_value_t*)jl_symbol("nkw")) {
                m->nkw = jl_unbox_long(jl_exprarg(st, 1));
                st = jl_nothing;
            }
        }
        else {
            st = resolve_definition_effects(st, m->module, sparam_vars, (jl_value_t*)m, 1, 0);
        }
        jl_array_ptr_set(copy, i, st);
    }
    src = jl_copy_code_info(src);
    src->isva = m->isva; // TODO: It would be nice to reverse this
    assert(m->nargs == src->nargs);
    src->code = copy;
    jl_gc_wb(src, copy);
    m->slot_syms = jl_compress_argnames(src->slotnames);
    jl_gc_wb(m, m->slot_syms);
    if (gen_only) {
        m->source = NULL;
    }
    else {
        m->debuginfo = src->debuginfo;
        jl_gc_wb(m, m->debuginfo);
        m->source = (jl_value_t*)src;
        jl_gc_wb(m, m->source);
        m->source = (jl_value_t*)jl_compress_ir(m, NULL);
        jl_gc_wb(m, m->source);
    }
    JL_GC_POP();
}

JL_DLLEXPORT jl_method_t *jl_new_method_uninit(jl_module_t *module)
{
    jl_task_t *ct = jl_current_task;
    jl_method_t *m =
        (jl_method_t*)jl_gc_alloc(ct->ptls, sizeof(jl_method_t), jl_method_type);
    jl_atomic_store_relaxed(&m->specializations, (jl_value_t*)jl_emptysvec);
    jl_atomic_store_relaxed(&m->speckeyset, (jl_genericmemory_t*)jl_an_empty_memory_any);
    m->sig = NULL;
    m->slot_syms = NULL;
    m->roots = NULL;
    m->root_blocks = NULL;
    m->nroots_sysimg = 0;
    m->ccallable = NULL;
    m->module = module;
    m->external_mt = NULL;
    m->source = NULL;
    m->debuginfo = NULL;
    jl_atomic_store_relaxed(&m->unspecialized, NULL);
    m->generator = NULL;
    m->name = NULL;
    m->file = jl_empty_sym;
    m->line = 0;
    m->called = 0xff;
    m->nospecialize = module->nospecialize;
    m->nkw = 0;
    jl_atomic_store_relaxed(&m->invokes, jl_nothing);
    m->recursion_relation = NULL;
    m->isva = 0;
    m->nargs = 0;
    jl_atomic_store_relaxed(&m->primary_world, ~(size_t)0);
    jl_atomic_store_relaxed(&m->dispatch_status, 0);
    m->is_for_opaque_closure = 0;
    m->nospecializeinfer = 0;
    jl_atomic_store_relaxed(&m->did_scan_source, 0);
    m->constprop = 0;
    m->purity.bits = 0;
    m->max_varargs = UINT8_MAX;
    JL_MUTEX_INIT(&m->writelock, "method->writelock");
    return m;
}

// backedges ------------------------------------------------------------------

// Use this in a `while` loop to iterate over the backedges in a MethodInstance.
// `*invokesig` will be NULL if the call was made by ordinary dispatch, otherwise
// it will be the signature supplied in an `invoke` call.
// If you don't need `invokesig`, you can set it to NULL on input.
// Initialize iteration with `i = 0`. Returns `i` for the next backedge to be extracted.
int get_next_edge(jl_array_t *list, int i, jl_value_t** invokesig, jl_code_instance_t **caller) JL_NOTSAFEPOINT
{
    jl_value_t *item = jl_array_ptr_ref(list, i);
    if (!item || jl_is_code_instance(item)) {
        // Not an `invoke` call, it's just the CodeInstance
        if (invokesig != NULL)
            *invokesig = NULL;
        *caller = (jl_code_instance_t*)item;
        return i + 1;
    }
    assert(jl_is_type(item));
    // An `invoke` call, it's a (sig, MethodInstance) pair
    if (invokesig != NULL)
        *invokesig = item;
    *caller = (jl_code_instance_t*)jl_array_ptr_ref(list, i + 1);
    if (*caller)
        assert(jl_is_code_instance(*caller));
    return i + 2;
}

int set_next_edge(jl_array_t *list, int i, jl_value_t *invokesig, jl_code_instance_t *caller)
{
    if (invokesig)
        jl_array_ptr_set(list, i++, invokesig);
    jl_array_ptr_set(list, i++, caller);
    return i;
}

int clear_next_edge(jl_array_t *list, int i, jl_value_t *invokesig, jl_code_instance_t *caller)
{
    if (invokesig)
        jl_array_ptr_set(list, i++, NULL);
    jl_array_ptr_set(list, i++, NULL);
    return i;
}

void push_edge(jl_array_t *list, jl_value_t *invokesig, jl_code_instance_t *caller)
{
    if (invokesig)
        jl_array_ptr_1d_push(list, invokesig);
    jl_array_ptr_1d_push(list, (jl_value_t*)caller);
    return;
}

void jl_mi_done_backedges(jl_method_instance_t *mi JL_PROPAGATES_ROOT, uint8_t old_flags) {
    uint8_t flags_now = 0;
    jl_array_t *backedges = jl_mi_get_backedges_mutate(mi, &flags_now);
    if (backedges && !old_flags) {
        if (flags_now & MI_FLAG_BACKEDGES_DIRTY) {
            size_t n = jl_array_nrows(backedges);
            size_t i = 0;
            size_t insb = 0;
            while (i < n) {
                jl_value_t *invokesig;
                jl_code_instance_t *caller;
                i = get_next_edge(backedges, i, &invokesig, &caller);
                if (!caller)
                    continue;
                insb = set_next_edge(backedges, insb, invokesig, caller);
            }
            if (insb == n) {
                // All were deleted
                mi->backedges = NULL;
            } else {
                jl_array_del_end(backedges, n - insb);
            }
        }
        jl_atomic_fetch_and_relaxed(&mi->flags, ~MI_FLAG_BACKEDGES_ALL);
    }
}

// method definition ----------------------------------------------------------

jl_method_t *jl_make_opaque_closure_method(jl_module_t *module, jl_value_t *name,
    int nargs, jl_value_t *functionloc, jl_code_info_t *ci, int isva, int isinferred)
{
    jl_method_t *m = jl_new_method_uninit(module);
    JL_GC_PUSH1(&m);
    // TODO: Maybe have a signature of (parent method, stmt#)?
    m->sig = (jl_value_t*)jl_anytuple_type;
    m->isva = isva;
    m->is_for_opaque_closure = 1;
    if (name == jl_nothing) {
        m->name = jl_symbol("opaque closure");
    } else {
        assert(jl_is_symbol(name));
        m->name = (jl_sym_t*)name;
    }
    m->nargs = nargs + 1;
    assert(jl_is_linenode(functionloc));
    jl_value_t *file = jl_linenode_file(functionloc);
    m->file = jl_is_symbol(file) ? (jl_sym_t*)file : jl_empty_sym;
    m->line = jl_linenode_line(functionloc);
    if (isinferred) {
        m->slot_syms = jl_compress_argnames(ci->slotnames);
        jl_gc_wb(m, m->slot_syms);
    } else {
        jl_method_set_source(m, ci);
    }
    JL_GC_POP();
    return m;
}

JL_DLLEXPORT void jl_check_gf(jl_value_t *gf, jl_sym_t *name)
{
    if (!jl_is_datatype_singleton((jl_datatype_t*)jl_typeof(gf)) && !jl_is_type(gf))
        jl_errorf("cannot define function %s; it already has a value", jl_symbol_name(name));
}

JL_DLLEXPORT jl_value_t *jl_declare_const_gf(jl_module_t *mod, jl_sym_t *name)
{
    JL_LOCK(&world_counter_lock);
    size_t new_world = jl_atomic_load_relaxed(&jl_world_counter) + 1;
    jl_binding_t *b = jl_get_module_binding(mod, name, 1);
    jl_value_t *gf = jl_get_existing_strong_gf(b, new_world);
    if (gf) {
        jl_check_gf(gf, name);
        JL_UNLOCK(&world_counter_lock);
        return gf;
    }
    gf = (jl_value_t*)jl_new_generic_function(name, mod, new_world);
    // From this point on (if we didn't error), we're committed to raising the world age,
    // because we've used it to declare the type name.
    jl_declare_constant_val3(b, mod, name, gf, PARTITION_KIND_CONST, new_world);
    jl_atomic_store_release(&jl_world_counter, new_world);
    JL_GC_PROMISE_ROOTED(gf);
    JL_UNLOCK(&world_counter_lock);
    return gf;
}

static jl_methtable_t *nth_methtable(jl_value_t *a JL_PROPAGATES_ROOT, int n) JL_NOTSAFEPOINT
{
    if (jl_is_datatype(a)) {
        if (n == 0) {
            jl_methtable_t *mt = ((jl_datatype_t*)a)->name->mt;
            if (mt != NULL)
                return mt;
        }
        else if (jl_is_tuple_type(a)) {
            if (jl_nparams(a) >= n)
                return nth_methtable(jl_tparam(a, n - 1), 0);
        }
    }
    else if (jl_is_typevar(a)) {
        return nth_methtable(((jl_tvar_t*)a)->ub, n);
    }
    else if (jl_is_unionall(a)) {
        return nth_methtable(((jl_unionall_t*)a)->body, n);
    }
    else if (jl_is_uniontype(a)) {
        jl_uniontype_t *u = (jl_uniontype_t*)a;
        jl_methtable_t *m1 = nth_methtable(u->a, n);
        if ((jl_value_t*)m1 != jl_nothing) {
            jl_methtable_t *m2 = nth_methtable(u->b, n);
            if (m1 == m2)
                return m1;
        }
    }
    return (jl_methtable_t*)jl_nothing;
}

// get the MethodTable for dispatch, or `nothing` if cannot be determined
JL_DLLEXPORT jl_methtable_t *jl_method_table_for(jl_value_t *argtypes JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT
{
    return nth_methtable(argtypes, 1);
}

jl_methtable_t *jl_kwmethod_table_for(jl_value_t *argtypes JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT
{
    jl_methtable_t *kwmt = nth_methtable(argtypes, 3);
    if ((jl_value_t*)kwmt == jl_nothing)
        return NULL;
    return kwmt;
}

JL_DLLEXPORT jl_methtable_t *jl_method_get_table(jl_method_t *method JL_PROPAGATES_ROOT) JL_NOTSAFEPOINT
{
    return method->external_mt ? (jl_methtable_t*)method->external_mt : jl_method_table_for(method->sig);
}

JL_DLLEXPORT jl_method_t* jl_method_def(jl_svec_t *argdata,
                                        jl_methtable_t *mt,
                                        jl_code_info_t *f,
                                        jl_module_t *module)
{
    // argdata is svec(svec(types...), svec(typevars...), functionloc)
    jl_svec_t *atypes = (jl_svec_t*)jl_svecref(argdata, 0);
    jl_svec_t *tvars = (jl_svec_t*)jl_svecref(argdata, 1);
    jl_value_t *functionloc = jl_svecref(argdata, 2);
    assert(jl_is_svec(atypes));
    assert(jl_is_svec(tvars));
    size_t nargs = jl_svec_len(atypes);
    assert(nargs > 0);
    int isva = jl_is_vararg(jl_svecref(atypes, nargs - 1));
    if (!jl_is_type(jl_svecref(atypes, 0)) || (isva && nargs == 1))
        jl_error("function type in method definition is not a type");
    jl_sym_t *name;
    jl_method_t *m = NULL;
    jl_value_t *argtype = NULL;
    JL_GC_PUSH3(&f, &m, &argtype);
    size_t i, na = jl_svec_len(atypes);

    argtype = jl_apply_tuple_type(atypes, 1);
    if (!jl_is_datatype(argtype))
        jl_error("invalid type in method definition (Union{})");

    jl_methtable_t *external_mt = mt;
    if (!mt)
        mt = jl_method_table_for(argtype);
    if ((jl_value_t*)mt == jl_nothing)
        jl_error("Method dispatch is unimplemented currently for this method signature");
    if (mt->frozen)
        jl_error("cannot add methods to a builtin function");

    assert(jl_is_linenode(functionloc));
    jl_sym_t *file = (jl_sym_t*)jl_linenode_file(functionloc);
    if (!jl_is_symbol(file))
        file = jl_empty_sym;
    int32_t line = jl_linenode_line(functionloc);

    // TODO: derive our debug name from the syntax instead of the type
    jl_methtable_t *kwmt = mt == jl_kwcall_mt ? jl_kwmethod_table_for(argtype) : mt;
    // if we have a kwcall, try to derive the name from the callee argument method table
    name = (kwmt ? kwmt : mt)->name;
    if (kwmt == jl_type_type_mt || kwmt == jl_nonfunction_mt || external_mt) {
        // our value for `name` is bad, try to guess what the syntax might have had,
        // like `jl_static_show_func_sig` might have come up with
        jl_datatype_t *dt = jl_nth_argument_datatype(argtype, mt == jl_kwcall_mt ? 3 : 1);
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

    for (i = 0; i < na; i++) {
        jl_value_t *elt = jl_svecref(atypes, i);
        if (jl_is_vararg(elt)) {
            if (i < na-1)
                jl_exceptionf(jl_argumenterror_type,
                              "Vararg on non-final argument in method definition for %s at %s:%d",
                              jl_symbol_name(name),
                              jl_symbol_name(file),
                              line);
            elt = jl_unwrap_vararg(elt);
        }
        int isvalid = (jl_is_type(elt) || jl_is_typevar(elt) || jl_is_vararg(elt)) && elt != jl_bottom_type;
        if (!isvalid) {
            jl_sym_t *argname = (jl_sym_t*)jl_array_ptr_ref(f->slotnames, i);
            if (argname == jl_unused_sym)
                jl_exceptionf(jl_argumenterror_type,
                              "invalid type for argument number %d in method definition for %s at %s:%d",
                              i,
                              jl_symbol_name(name),
                              jl_symbol_name(file),
                              line);
            else
                jl_exceptionf(jl_argumenterror_type,
                              "invalid type for argument %s in method definition for %s at %s:%d",
                              jl_symbol_name(argname),
                              jl_symbol_name(name),
                              jl_symbol_name(file),
                              line);
        }
    }
    for (i = jl_svec_len(tvars); i > 0; i--) {
        jl_value_t *tv = jl_svecref(tvars, i - 1);
        if (!jl_is_typevar(tv))
            jl_type_error("method signature", (jl_value_t*)jl_tvar_type, tv);
        if (!jl_has_typevar(argtype, (jl_tvar_t*)tv)) // deprecate this to an error in v2
            jl_printf(JL_STDERR,
                      "WARNING: method definition for %s at %s:%d declares type variable %s but does not use it.\n",
                      jl_symbol_name(name),
                      jl_symbol_name(file),
                      line,
                      jl_symbol_name(((jl_tvar_t*)tv)->name));
        argtype = jl_new_struct(jl_unionall_type, tv, argtype);
    }
    if (jl_has_free_typevars(argtype)) {
        jl_exceptionf(jl_argumenterror_type,
                      "method definition for %s at %s:%d has free type variables",
                      jl_symbol_name(name),
                      jl_symbol_name(file),
                      line);
    }

    m = jl_new_method_uninit(module);
    m->external_mt = (jl_value_t*)external_mt;
    if (external_mt)
        jl_gc_wb(m, external_mt);
    m->sig = argtype;
    m->name = name;
    m->isva = isva;
    m->nargs = nargs;
    m->file = file;
    m->line = line;
    jl_method_set_source(m, f);

    jl_method_table_insert(mt, m, NULL);
    if (jl_newmeth_tracer)
        jl_call_tracer(jl_newmeth_tracer, (jl_value_t*)m);
    JL_GC_POP();

    return m;
}

void jl_ctor_def(jl_value_t *ty, jl_value_t *functionloc)
{
    jl_datatype_t *dt = (jl_datatype_t*)jl_unwrap_unionall(ty);
    JL_TYPECHK(ctor_def, datatype, (jl_value_t*)dt);
    JL_TYPECHK(ctor_def, linenumbernode, functionloc);
    jl_svec_t *fieldtypes = jl_get_fieldtypes(dt);
    size_t nfields = jl_svec_len(fieldtypes);
    size_t nparams = jl_subtype_env_size(ty);
    jl_module_t *inmodule = dt->name->module;
    jl_sym_t *file = (jl_sym_t*)jl_linenode_file(functionloc);
    if (!jl_is_symbol(file))
        file = jl_empty_sym;
    int32_t line = jl_linenode_line(functionloc);

    // argdata is svec(svec(types...), svec(typevars...), functionloc)
    jl_svec_t *argdata = jl_alloc_svec(3);
    jl_array_t *fieldkinds = NULL;
    jl_code_info_t *body = NULL;
    JL_GC_PUSH3(&argdata, &fieldkinds, &body);
    jl_svecset(argdata, 2, functionloc);
    jl_svec_t *tvars = jl_alloc_svec(nparams);
    jl_svecset(argdata, 1, tvars);
    jl_unionall_t *ua = (jl_unionall_t*)ty;
    for (size_t i = 0; i < nparams; i++) {
        assert(jl_is_unionall(ua));
        jl_svecset(tvars, i, ua->var);
        ua = (jl_unionall_t*)ua->body;
    }
    jl_svec_t *names = dt->name->names;

    // define outer constructor (if all typevars are present (thus not definitely unconstrained) by the fields or other typevars which themselves are constrained)
    int constrains_all_tvars = 1;
    for (size_t i = nparams; i > 0; i--) {
        jl_tvar_t *tv = (jl_tvar_t*)jl_svecref(tvars, i - 1);
        int constrains_tvar = 0;
        for (size_t i = 0; i < nfields; i++) {
            jl_value_t *ft = jl_svecref(fieldtypes, i);
            if (jl_has_typevar(ft, tv)) {
                constrains_tvar = 1;
                break;
            }
        }
        for (size_t j = i; j < nparams; j++) {
            jl_tvar_t *tv2 = (jl_tvar_t*)jl_svecref(tvars, j);
            if (jl_has_typevar(tv2->ub, tv)) { // lb doesn't constrain, but jl_has_typevar doesn't have a way to specify that we care about may-constrain and not merely containment
                constrains_tvar = 1;
                break;
            }
            if (tv2 == tv) {
                constrains_tvar = 0;
                break;
            }
        }
        if (!constrains_tvar) {
            constrains_all_tvars = 0;
            break;
        }
    }
    if (constrains_all_tvars) {
        jl_svec_t *atypes = jl_alloc_svec(nfields + 1);
        jl_svecset(argdata, 0, atypes);
        jl_svecset(atypes, 0, jl_wrap_Type(ty));
        for (size_t i = 0; i < nfields; i++) {
            jl_value_t *ft = jl_svecref(fieldtypes, i);
            jl_svecset(atypes, i + 1, ft);
        }
        body = jl_outer_ctor_body(ty, nfields, nparams, inmodule, jl_symbol_name(file), line);
        if (names) {
            jl_array_t *slotnames = body->slotnames;
            for (size_t i = 0; i < nfields; i++) {
                jl_array_ptr_set(slotnames, i + 1, jl_svecref(names, i));
            }
        }
        jl_method_def(argdata, NULL, body, inmodule);
        if (nparams == 0) {
            int all_Any = 1; // check if all fields are Any and the type is not parameterized, since inner constructor would be the same signature and code
            for (size_t i = 0; i < nfields; i++) {
                jl_value_t *ft = jl_svecref(fieldtypes, i);
                if (ft != (jl_value_t*)jl_any_type) {
                    all_Any = 0;
                    break;
                }
            }
            if (all_Any) {
                JL_GC_POP();
                return;
            }
        }
    }

    // define inner constructor
    jl_svec_t *atypes = jl_svec_fill(nfields + 1, (jl_value_t*)jl_any_type);
    jl_svecset(argdata, 0, atypes);
    jl_value_t *typedt = (jl_value_t*)jl_wrap_Type((jl_value_t*)dt);
    jl_svecset(atypes, 0, typedt);
    fieldkinds = jl_alloc_vec_any(nfields);
    for (size_t i = 0; i < nfields; i++) {
        jl_value_t *ft = jl_svecref(fieldtypes, i);
        int kind = ft == (jl_value_t*)jl_any_type ? -1 : 0;
        // TODO: if more efficient to do so, we could reference the sparam instead of fieldtype
        //if (jl_is_typevar(ft)) {
        //    for (size_t i = 0; i < nparams; i++) {
        //        if (jl_svecref(tvars, i) == ft) {
        //            kind = i + 1;
        //            break; // if repeated, must consider only the innermost
        //        }
        //    }
        //}
        jl_array_ptr_set(fieldkinds, i, jl_box_long(kind));
    }
    // rewrap_unionall(Type{dt}, ty)
    for (size_t i = nparams; i > 0; i--) {
        jl_value_t *tv = jl_svecref(tvars, i - 1);
        typedt = jl_new_struct(jl_unionall_type, tv, typedt);
        jl_svecset(atypes, 0, typedt);
    }
    tvars = jl_emptysvec;
    jl_svecset(argdata, 1, tvars);
    body = jl_inner_ctor_body(fieldkinds, inmodule, jl_symbol_name(file), line);
    if (names) {
        jl_array_t *slotnames = body->slotnames;
        for (size_t i = 0; i < nfields; i++) {
            jl_array_ptr_set(slotnames, i + 1, jl_svecref(names, i));
        }
    }
    jl_method_def(argdata, NULL, body, inmodule);
    JL_GC_POP();
}

// root blocks

// This section handles method roots. Roots are GC-preserved items needed to
// represent lowered, type-inferred, and/or compiled code. These items are
// stored in a flat list (`m.roots`), and during serialization and
// deserialization of code we replace C-pointers to these items with a
// relocatable reference. We use a bipartite reference, `(key, index)` pair,
// where `key` identifies the module that added the root and `index` numbers
// just those roots with the same `key`.
//
// During precompilation (serialization), we save roots that were added to
// methods that are tagged with this package's module-key, even for "external"
// methods not owned by a module currently being precompiled. During
// deserialization, we load the new roots and append them to the method. When
// code is deserialized (see ircode.c), we replace the bipartite reference with
// the pointer to the memory address in the current session. The bipartite
// reference allows us to cache both roots and references in precompilation .ji
// files using a naming scheme that is independent of which packages are loaded
// in arbitrary order.
//
// To track the module-of-origin for each root, methods also have a
// `root_blocks` field that uses run-length encoding (RLE) storing `key` and the
// (absolute) integer index within `roots` at which a block of roots with that
// key begins. This makes it possible to look up an individual `(key, index)`
// pair fairly efficiently. A given `key` may possess more than one block; the
// `index` continues to increment regardless of block boundaries.
//
// Roots with `key = 0` are considered to be of unknown origin, and
// CodeInstances referencing such roots will remain unserializable unless all
// such roots were added at the time of system image creation. To track this
// additional data, we use two fields:
//
// - methods have an `nroots_sysimg` field to count the number of roots defined
//   at the time of writing the system image (such occur first in the list of
//   roots). These are the cases with `key = 0` that do not prevent
//   serialization.

// Get the key of the current (final) block of roots
static uint64_t current_root_id(jl_array_t *root_blocks)
{
    if (!root_blocks)
        return 0;
    assert(jl_is_array(root_blocks));
    size_t nx2 = jl_array_nrows(root_blocks);
    if (nx2 == 0)
        return 0;
    uint64_t *blocks = jl_array_data(root_blocks, uint64_t);
    return blocks[nx2-2];
}

// Add a new block of `len` roots with key `modid` (module id)
static void add_root_block(jl_array_t *root_blocks, uint64_t modid, size_t len)
{
    assert(jl_is_array(root_blocks));
    jl_array_grow_end(root_blocks, 2);
    uint64_t *blocks = jl_array_data(root_blocks, uint64_t);
    int nx2 = jl_array_nrows(root_blocks);
    blocks[nx2-2] = modid;
    blocks[nx2-1] = len;
}

// Allocate storage for roots
static void prepare_method_for_roots(jl_method_t *m, uint64_t modid)
{
    if (!m->roots) {
        m->roots = jl_alloc_vec_any(0);
        jl_gc_wb(m, m->roots);
    }
    if (!m->root_blocks && modid != 0) {
        m->root_blocks = jl_alloc_array_1d(jl_array_uint64_type, 0);
        jl_gc_wb(m, m->root_blocks);
    }
}

// Add a single root with owner `mod` to a method
JL_DLLEXPORT void jl_add_method_root(jl_method_t *m, jl_module_t *mod, jl_value_t* root)
{
    JL_GC_PUSH2(&m, &root);
    uint64_t modid = 0;
    if (mod) {
        assert(jl_is_module(mod));
        modid = mod->build_id.lo;
    }
    assert(jl_is_method(m));
    prepare_method_for_roots(m, modid);
    if (current_root_id(m->root_blocks) != modid)
        add_root_block(m->root_blocks, modid, jl_array_nrows(m->roots));
    jl_array_ptr_1d_push(m->roots, root);
    JL_GC_POP();
}

// Add a list of roots with key `modid` to a method
void jl_append_method_roots(jl_method_t *m, uint64_t modid, jl_array_t* roots)
{
    JL_GC_PUSH2(&m, &roots);
    assert(jl_is_method(m));
    assert(jl_is_array(roots));
    prepare_method_for_roots(m, modid);
    add_root_block(m->root_blocks, modid, jl_array_nrows(m->roots));
    jl_array_ptr_1d_append(m->roots, roots);
    JL_GC_POP();
}

// given the absolute index i of a root, retrieve its relocatable reference
// returns 1 if the root is relocatable
int get_root_reference(rle_reference *rr, jl_method_t *m, size_t i)
{
    if (!m->root_blocks) {
        rr->key = 0;
        rr->index = i;
        return i < m->nroots_sysimg;
    }
    rle_index_to_reference(rr, i, jl_array_data(m->root_blocks, uint64_t), jl_array_nrows(m->root_blocks), 0);
    if (rr->key)
        return 1;
    return i < m->nroots_sysimg;
}

// get a root, given its key and index relative to the key
// this is the relocatable way to get a root from m->roots
jl_value_t *lookup_root(jl_method_t *m, uint64_t key, int index)
{
    if (!m->root_blocks) {
        assert(key == 0);
        return jl_array_ptr_ref(m->roots, index);
    }
    rle_reference rr = {key, index};
    size_t i = rle_reference_to_index(&rr, jl_array_data(m->root_blocks, uint64_t), jl_array_nrows(m->root_blocks), 0);
    return jl_array_ptr_ref(m->roots, i);
}

// Count the number of roots added by module with id `key`
int nroots_with_key(jl_method_t *m, uint64_t key)
{
    size_t nroots = 0;
    if (m->roots)
        nroots = jl_array_nrows(m->roots);
    if (!m->root_blocks)
        return key == 0 ? nroots : 0;
    uint64_t *rletable = jl_array_data(m->root_blocks, uint64_t);
    size_t j, nblocks2 = jl_array_nrows(m->root_blocks);
    int nwithkey = 0;
    for (j = 0; j < nblocks2; j+=2) {
        if (rletable[j] == key)
            nwithkey += (j+3 < nblocks2 ? rletable[j+3] : nroots) - rletable[j+1];
    }
    return nwithkey;
}

#ifdef __cplusplus
}
#endif

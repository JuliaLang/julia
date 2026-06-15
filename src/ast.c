// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  AST
  utilities for working with syntax trees, and entry points into the
  frontend interface (parsing, macro expansion and lowering), which is
  implemented in libjulia-frontend
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "julia.h"
#include "julia_internal.h"
#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

#define XX(name) JL_DLLEXPORT jl_sym_t *jl_##name;
JL_COMMON_SYMBOLS(XX)
#undef XX

void jl_init_common_symbols(void)
{
    jl_empty_sym = jl_symbol("");
    jl_call_sym = jl_symbol("call");
    jl_invoke_sym = jl_symbol("invoke");
    jl_invoke_modify_sym = jl_symbol("invoke_modify");
    jl_foreigncall_sym = jl_symbol("foreigncall");
    jl_cfunction_sym = jl_symbol("cfunction");
    jl_quote_sym = jl_symbol("quote");
    jl_inert_sym = jl_symbol("inert");
    jl_top_sym = jl_symbol("top");
    jl_core_sym = jl_symbol("core");
    jl_globalref_sym = jl_symbol("globalref");
    jl_line_sym = jl_symbol("line");
    jl_lineinfo_sym = jl_symbol("lineinfo");
    jl_incomplete_sym = jl_symbol("incomplete");
    jl_error_sym = jl_symbol("error");
    jl_goto_sym = jl_symbol("goto");
    jl_goto_ifnot_sym = jl_symbol("gotoifnot");
    jl_return_sym = jl_symbol("return");
    jl_lambda_sym = jl_symbol("lambda");
    jl_module_sym = jl_symbol("module");
    jl_export_sym = jl_symbol("export");
    jl_public_sym = jl_symbol("public");
    jl_assign_sym = jl_symbol("=");
    jl_method_sym = jl_symbol("method");
    jl_exc_sym = jl_symbol("the_exception");
    jl_enter_sym = jl_symbol("enter");
    jl_leave_sym = jl_symbol("leave");
    jl_pop_exception_sym = jl_symbol("pop_exception");
    jl_new_sym = jl_symbol("new");
    jl_splatnew_sym = jl_symbol("splatnew");
    jl_new_opaque_closure_sym = jl_symbol("new_opaque_closure");
    jl_opaque_closure_method_sym = jl_symbol("opaque_closure_method");
    jl_const_sym = jl_symbol("const");
    jl_global_sym = jl_symbol("global");
    jl_local_sym = jl_symbol("local");
    jl_thunk_sym = jl_symbol("thunk");
    jl_toplevel_sym = jl_symbol("toplevel");
    jl_dot_sym = jl_symbol(".");
    jl_as_sym = jl_symbol("as");
    jl_colon_sym = jl_symbol(":");
    jl_boundscheck_sym = jl_symbol("boundscheck");
    jl_inbounds_sym = jl_symbol("inbounds");
    jl_newvar_sym = jl_symbol("newvar");
    jl_copyast_sym = jl_symbol("copyast");
    jl_loopinfo_sym = jl_symbol("loopinfo");
    jl_meta_sym = jl_symbol("meta");
    jl_list_sym = jl_symbol("list");
    jl_unused_sym = jl_symbol("#unused#");
    jl_slot_sym = jl_symbol("slot");
    jl_static_parameter_sym = jl_symbol("static_parameter");
    jl_inline_sym = jl_symbol("inline");
    jl_noinline_sym = jl_symbol("noinline");
    jl_polly_sym = jl_symbol("polly");
    jl_propagate_inbounds_sym = jl_symbol("propagate_inbounds");
    jl_aggressive_constprop_sym = jl_symbol("aggressive_constprop");
    jl_no_constprop_sym = jl_symbol("no_constprop");
    jl_purity_sym = jl_symbol("purity");
    jl_isdefined_sym = jl_symbol("isdefined");
    jl_nospecialize_sym = jl_symbol("nospecialize");
    jl_specialize_sym = jl_symbol("specialize");
    jl_nospecializeinfer_sym = jl_symbol("nospecializeinfer");
    jl_optlevel_sym = jl_symbol("optlevel");
    jl_compile_sym = jl_symbol("compile");
    jl_force_compile_sym = jl_symbol("force_compile");
    jl_infer_sym = jl_symbol("infer");
    jl_max_methods_sym = jl_symbol("max_methods");
    jl_macrocall_sym = jl_symbol("macrocall");
    jl_escape_sym = jl_symbol("escape");
    jl_hygienicscope_sym = jl_symbol("hygienic-scope");
    jl_gc_preserve_begin_sym = jl_symbol("gc_preserve_begin");
    jl_gc_preserve_end_sym = jl_symbol("gc_preserve_end");
    jl_generated_sym = jl_symbol("generated");
    jl_generated_only_sym = jl_symbol("generated_only");
    jl_throw_undef_if_not_sym = jl_symbol("throw_undef_if_not");
    jl_getfield_undefref_sym = jl_symbol("##getfield##");
    jl_do_sym = jl_symbol("do");
    jl_coverageeffect_sym = jl_symbol("code_coverage_effect");
    jl_aliasscope_sym = jl_symbol("aliasscope");
    jl_popaliasscope_sym = jl_symbol("popaliasscope");
    jl_thismodule_sym = jl_symbol("thismodule");
    jl_eval_sym = jl_symbol("eval");
    jl_include_sym = jl_symbol("include");
    jl_block_sym = jl_symbol("block");
    jl_atom_sym = jl_symbol("atom");
    jl_statement_sym = jl_symbol("statement");
    jl_all_sym = jl_symbol("all");
    jl_atomic_sym = jl_symbol("atomic");
    jl_not_atomic_sym = jl_symbol("not_atomic");
    jl_unordered_sym = jl_symbol("unordered");
    jl_singlethread_sym = jl_symbol("singlethread");
    jl_system_sym = jl_symbol("system");
    jl_monotonic_sym = jl_symbol("monotonic");
    jl_acquire_sym = jl_symbol("acquire");
    jl_release_sym = jl_symbol("release");
    jl_acquire_release_sym = jl_symbol("acquire_release");
    jl_sequentially_consistent_sym = jl_symbol("sequentially_consistent");
    jl_uninferred_sym = jl_symbol("uninferred");
    jl_latestworld_sym = jl_symbol("latestworld");
}

// syntax tree accessors

JL_DLLEXPORT jl_value_t *jl_copy_ast(jl_value_t *expr)
{
    if (!expr)
        return NULL;
    if (jl_is_code_info(expr)) {
        jl_code_info_t *new_ci = (jl_code_info_t *)expr;
        jl_array_t *new_code = NULL;
        JL_GC_PUSH2(&new_ci, &new_code);
        new_ci = jl_copy_code_info(new_ci);
        new_code = jl_array_copy(new_ci->code);
        size_t clen = jl_array_nrows(new_code);
        for (int i = 0; i < clen; ++i) {
            jl_array_ptr_set(new_code, i, jl_copy_ast(
                jl_array_ptr_ref(new_code, i)
            ));
        }
        new_ci->code = new_code;
        jl_gc_wb(new_ci, new_code);
        new_ci->slotnames = jl_array_copy(new_ci->slotnames);
        jl_gc_wb(new_ci, new_ci->slotnames);
        new_ci->slotflags = jl_array_copy(new_ci->slotflags);
        jl_gc_wb(new_ci, new_ci->slotflags);
        new_ci->ssaflags = jl_array_copy(new_ci->ssaflags);
        jl_gc_wb(new_ci, new_ci->ssaflags);

        if (jl_is_array(new_ci->ssavaluetypes)) {
            new_ci->ssavaluetypes = (jl_value_t*)jl_array_copy((jl_array_t*)new_ci->ssavaluetypes);
            jl_gc_wb(new_ci, new_ci->ssavaluetypes);
        }
        JL_GC_POP();
        return (jl_value_t*)new_ci;
    }
    if (jl_is_expr(expr)) {
        jl_expr_t *e = (jl_expr_t*)expr;
        size_t i, l = jl_array_nrows(e->args);
        jl_expr_t *ne = jl_exprn(e->head, l);
        JL_GC_PUSH2(&ne, &expr);
        for (i = 0; i < l; i++) {
            jl_value_t *a = jl_exprarg(e, i);
            jl_exprargset(ne, i, jl_copy_ast(a));
        }
        JL_GC_POP();
        return (jl_value_t*)ne;
    }
    if (jl_is_phinode(expr)) {
        jl_array_t *edges = (jl_array_t*)jl_fieldref_noalloc(expr, 0);
        jl_array_t *values = (jl_array_t*)jl_fieldref_noalloc(expr, 1);
        JL_GC_PUSH2(&edges, &values);
        edges = jl_array_copy(edges);
        values = jl_array_copy(values);
        jl_value_t *ret = jl_new_struct(jl_phinode_type, edges, values);
        JL_GC_POP();
        return ret;
    }
    if (jl_is_phicnode(expr)) {
        jl_array_t *values = (jl_array_t*)jl_fieldref_noalloc(expr, 0);
        JL_GC_PUSH1(&values);
        values = jl_array_copy(values);
        jl_value_t *ret = jl_new_struct(jl_phicnode_type, values);
        JL_GC_POP();
        return ret;
    }
    return expr;
}

int jl_has_meta(jl_array_t *body, jl_sym_t *sym) JL_NOTSAFEPOINT
{
    size_t i, l = jl_array_nrows(body);
    for (i = 0; i < l; i++) {
        jl_expr_t *stmt = (jl_expr_t*)jl_array_ptr_ref(body, i);
        if (jl_is_expr((jl_value_t*)stmt) && stmt->head == jl_meta_sym) {
            size_t i, l = jl_array_nrows(stmt->args);
            for (i = 0; i < l; i++)
                if (jl_array_ptr_ref(stmt->args, i) == (jl_value_t*)sym)
                    return 1;
        }
    }
    return 0;
}

// Helpers for foreign frontends (e.g. a frontend library carrying its own
// runtime) to inspect and construct host syntax trees by name, without
// assumptions about struct layouts; part of the frontend support API.
JL_DLLEXPORT size_t jl_expr_argcount(jl_value_t *e) JL_NOTSAFEPOINT
{
    return jl_expr_nargs(e);
}

JL_DLLEXPORT jl_value_t *jl_expr_arg(jl_value_t *e, size_t i)
{
    return jl_exprarg(e, i);
}

JL_DLLEXPORT void jl_expr_setarg(jl_value_t *e, size_t i, jl_value_t *v)
{
    jl_exprargset(e, i, v);
}

JL_DLLEXPORT size_t jl_string_size(jl_value_t *s) JL_NOTSAFEPOINT
{
    return jl_string_len(s);
}

JL_DLLEXPORT size_t jl_array_length(jl_value_t *a) JL_NOTSAFEPOINT
{
    return jl_array_nrows(a);
}

// Utility function to return whether `e` is any of the special AST types or
// will always evaluate to itself exactly unchanged. This corresponds to
// `isa_ast_node` in Core.Compiler utilities.
JL_DLLEXPORT int jl_isa_ast_node(jl_value_t *e) JL_NOTSAFEPOINT
{
    return jl_is_newvarnode(e)
        || jl_is_code_info(e)
        || jl_is_linenode(e)
        || jl_is_gotonode(e)
        || jl_is_gotoifnot(e)
        || jl_is_enternode(e)
        || jl_is_returnnode(e)
        || jl_is_ssavalue(e)
        || jl_is_slotnumber(e)
        || jl_is_argument(e)
        || jl_is_quotenode(e)
        || jl_is_globalref(e)
        || jl_is_symbol(e)
        || jl_is_pinode(e)
        || jl_is_phinode(e)
        || jl_is_phicnode(e)
        || jl_is_upsilonnode(e)
        || jl_is_expr(e);
}

// Resolve and invoke a macro function in the appropriate world, for use by
// macro expansion in the frontend. On return, `*ctx` is set to the module in
// which the macro was defined, and `*lineinfo` to the macro's `__source__`
// argument; both out-arguments must be rooted by the caller.
JL_DLLEXPORT jl_value_t *jl_invoke_julia_macro(jl_array_t *args, jl_module_t *inmodule, jl_module_t **ctx, jl_value_t **lineinfo, size_t world, int throw_load_error)
{
    jl_task_t *ct = jl_current_task;
    JL_TIMING(MACRO_INVOCATION, MACRO_INVOCATION);
    size_t nargs = jl_array_nrows(args) + 1;
    JL_NARGSV("macrocall", 3); // macro name, location, and module
    jl_value_t **margs;
    JL_GC_PUSHARGS(margs, nargs);
    int i;
    margs[0] = jl_array_ptr_ref(args, 0);
    // __source__ argument
    jl_value_t *lno = jl_array_ptr_ref(args, 1);
    jl_value_t *retry_lno = NULL;
    if (!jl_is_linenumbernode(lno)) {
        if (lno != jl_nothing) {
            // Special case: The magic @VERSION macro currently gets a special
            // Core.MacroSource for its __source__ argument. However, to avoid
            // giving this to macros that do not expect it, we check for that
            // special case and retry with just the LineNumberNode if needed.
            if (jl_typeof(lno) == jl_get_global(jl_core_module, jl_symbol("MacroSource"))) {
                retry_lno = jl_fieldref_noalloc(lno, 0);
                goto lno_ok;
            }
        }
        lno = jl_new_struct(jl_linenumbernode_type, jl_box_long(0), jl_nothing);
    }
lno_ok:
    margs[1] = lno;
    margs[2] = (jl_value_t*)inmodule;
    for (i = 3; i < nargs; i++)
        margs[i] = jl_array_ptr_ref(args, i - 1);

    size_t last_age = ct->world_age;
    ct->world_age = jl_atomic_load_acquire(&jl_world_counter);
    if (ct->world_age > world)
        ct->world_age = world;
    jl_value_t *result;
    JL_TRY {
        jl_module_t *ctx_module = *ctx;
        JL_GC_PROMISE_ROOTED(ctx_module);
        margs[0] = jl_toplevel_eval(ctx_module, margs[0]);
        jl_method_instance_t *mfunc = NULL;
        mfunc = jl_apply_lookup(margs, nargs, ct->world_age);
        JL_GC_PROMISE_ROOTED(mfunc);
        if (mfunc == NULL && retry_lno != NULL) {
            margs[1] = retry_lno;
            mfunc = jl_apply_lookup(margs, nargs, ct->world_age);
            JL_GC_PROMISE_ROOTED(mfunc);
        }
        if (mfunc == NULL) {
            jl_method_error(margs[0], &margs[1], nargs, ct->world_age);
            // unreachable
        }
        // margs[1] may still be a MacroSource; timing wants the inner LineNumberNode
        jl_timing_show_macro(mfunc, retry_lno != NULL ? retry_lno : margs[1],
                             inmodule, JL_TIMING_DEFAULT_BLOCK);
        *ctx = mfunc->def.method->module;
        result = jl_invoke(margs[0], &margs[1], nargs - 1, mfunc);
    }
    JL_CATCH {
        if ((jl_loaderror_type == NULL) || !throw_load_error) {
            jl_rethrow();
        }
        else {
            jl_value_t *lno = margs[1];
            jl_value_t *file = jl_fieldref(lno, 1);
            if (jl_is_symbol(file))
                margs[0] = jl_cstr_to_string(jl_symbol_name((jl_sym_t*)file));
            else
                margs[0] = jl_cstr_to_string("<macrocall>");
            margs[1] = jl_fieldref(lno, 0); // extract and allocate line number
            jl_rethrow_other(jl_new_struct(jl_loaderror_type, margs[0], margs[1],
                                           jl_current_exception(ct)));
        }
    }
    ct->world_age = last_age;
    *lineinfo = margs[1];
    JL_GC_POP();
    return result;
}

// Main C entry point to lowering.  Calls jl_frontend_lower during bootstrap,
// and Core._lower otherwise (this is also the builtin frontend's lowering
// unless we have JuliaLowering)
JL_DLLEXPORT jl_value_t *jl_lower(jl_value_t *expr, jl_module_t *inmodule,
                                  const char *filename, int line, size_t world, bool_t warn)
{
    jl_value_t *julia_lower = NULL;
    if (inmodule) {
        julia_lower = jl_get_global(inmodule, jl_symbol("_internal_julia_lower"));
    }
    if ((!julia_lower || julia_lower == jl_nothing) && jl_core_module)
        julia_lower = jl_get_global_value(jl_core_module, jl_symbol("_lower"), jl_current_task->world_age);
    if (!julia_lower || julia_lower == jl_nothing) {
        return jl_frontend_lower(expr, inmodule, filename, line, world, warn);
    }
    jl_value_t **args;
    JL_GC_PUSHARGS(args, 7);
    args[0] = julia_lower;
    args[1] = expr;
    args[2] = (jl_value_t*)inmodule;
    args[3] = jl_cstr_to_string(filename);
    args[4] = jl_box_ulong(line);
    args[5] = jl_box_ulong(world);
    args[6] = warn ? jl_true : jl_false;
    jl_task_t *ct = jl_current_task;
    size_t last_age = ct->world_age;
    ct->world_age = jl_atomic_load_acquire(&jl_world_counter);
    jl_value_t *result = jl_apply(args, 7);
    ct->world_age = last_age;
    args[0] = result; // root during error check below
    JL_TYPECHK(parse, simplevector, result);
    if (jl_svec_len(result) < 1)
        jl_error("Result from lowering should be `svec(a::Any, x::Any...)`");
    JL_GC_POP();
    return result;
}

//------------------------------------------------------------------------------
// Parsing API and utils for calling parser from runtime

// Internal C entry point to parser
// `text` is passed as a pointer to allow raw non-String buffers to be used
// without copying.
jl_value_t *jl_parse(const char *text, size_t text_len, jl_value_t *filename,
                     size_t lineno, size_t offset, jl_value_t *options, jl_module_t *inmodule)
{
    jl_value_t *parser = NULL;
    if (inmodule) {
        parser = jl_get_global(inmodule, jl_symbol("#_internal_julia_parse"));
    }
    if ((!parser || parser == jl_nothing) && jl_core_module) {
        parser = jl_get_global(jl_core_module, jl_symbol("_parse"));
    }
    if (!parser || parser == jl_nothing) {
        // In bootstrap, directly call the builtin parser.
        jl_value_t *result = jl_frontend_parse(text, text_len, filename, lineno, offset, options);
        return result;
    }
    jl_value_t **args;
    JL_GC_PUSHARGS(args, 6);
    args[0] = parser;
    args[1] = (jl_value_t*)jl_alloc_svec(2);
    jl_svecset(args[1], 0, jl_box_uint8pointer((uint8_t*)text));
    jl_svecset(args[1], 1, jl_box_long(text_len));
    args[2] = filename;
    args[3] = jl_box_long(lineno);
    args[4] = jl_box_long(offset);
    args[5] = options;
    jl_task_t *ct = jl_current_task;
    size_t last_age = ct->world_age;
    ct->world_age = jl_atomic_load_acquire(&jl_world_counter);
    jl_value_t *result = jl_apply(args, 6);
    ct->world_age = last_age;
    args[0] = result; // root during error checks below
    JL_TYPECHK(parse, simplevector, result);
    if (jl_svec_len(result) != 2)
        jl_error("Result from parser should be `svec(a::Expr, b::Int)`");
    JL_TYPECHK(parse, expr, jl_svecref(result, 0));
    JL_TYPECHK(parse, long, jl_svecref(result, 1));
    JL_GC_POP();
    return result;
}

// parse an entire string as a file, reading multiple expressions
JL_DLLEXPORT jl_value_t *jl_parse_all(const char *text, size_t text_len,
                                      const char *filename, size_t filename_len, size_t lineno)
{
    jl_value_t *fname = jl_pchar_to_string(filename, filename_len);
    JL_GC_PUSH1(&fname);
    jl_value_t *p = jl_parse(text, text_len, fname, lineno, 0, (jl_value_t*)jl_all_sym, NULL);
    JL_GC_POP();
    return jl_svecref(p, 0);
}

// this is for parsing one expression out of a string, keeping track of
// the current position.
JL_DLLEXPORT jl_value_t *jl_parse_string(const char *text, size_t text_len,
                                         int offset, int greedy)
{
    jl_value_t *fname = jl_cstr_to_string("none");
    JL_GC_PUSH1(&fname);
    jl_value_t *result = jl_parse(text, text_len, fname, 1, offset,
                                  (jl_value_t*)(greedy ? jl_statement_sym : jl_atom_sym), NULL);
    JL_GC_POP();
    return result;
}

// deprecated
JL_DLLEXPORT jl_value_t *jl_parse_input_line(const char *text, size_t text_len,
                                             const char *filename, size_t filename_len)
{
    return jl_parse_all(text, text_len, filename, filename_len, 1);
}

#ifdef __cplusplus
}
#endif

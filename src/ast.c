// This file is a part of Julia. License is MIT: https://julialang.org/license

/*
  AST
  interface to front-end, obtains and translates syntax trees
*/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#ifdef _OS_WINDOWS_
#include <malloc.h>
#endif
#include "julia.h"
#include "julia_internal.h"
#include "flisp.h"
#include "julia_assert.h"

#ifdef __cplusplus
extern "C" {
#endif

// MSVC complains about "julia_flisp.boot.inc : error C4335: Mac file format
// detected: please convert the source file to either DOS or UNIX format"
#ifdef _MSC_VER
#pragma warning(disable:4335)
#endif

// head symbols for each expression type
jl_sym_t *call_sym;    jl_sym_t *invoke_sym;
jl_sym_t *empty_sym;   jl_sym_t *top_sym;
jl_sym_t *module_sym;  jl_sym_t *slot_sym;
jl_sym_t *export_sym;  jl_sym_t *import_sym;
jl_sym_t *toplevel_sym; jl_sym_t *quote_sym;
jl_sym_t *line_sym;    jl_sym_t *jl_incomplete_sym;
jl_sym_t *goto_sym;    jl_sym_t *goto_ifnot_sym;
jl_sym_t *return_sym;  jl_sym_t *unreachable_sym;
jl_sym_t *lambda_sym;  jl_sym_t *assign_sym;
jl_sym_t *globalref_sym; jl_sym_t *do_sym;
jl_sym_t *method_sym;  jl_sym_t *core_sym;
jl_sym_t *enter_sym;   jl_sym_t *leave_sym;
jl_sym_t *pop_exception_sym;
jl_sym_t *exc_sym;     jl_sym_t *error_sym;
jl_sym_t *new_sym;     jl_sym_t *using_sym;
jl_sym_t *splatnew_sym;
jl_sym_t *const_sym;   jl_sym_t *thunk_sym;
jl_sym_t *abstracttype_sym; jl_sym_t *primtype_sym;
jl_sym_t *structtype_sym;   jl_sym_t *foreigncall_sym;
jl_sym_t *global_sym; jl_sym_t *list_sym;
jl_sym_t *dot_sym;    jl_sym_t *newvar_sym;
jl_sym_t *boundscheck_sym; jl_sym_t *inbounds_sym;
jl_sym_t *copyast_sym; jl_sym_t *cfunction_sym;
jl_sym_t *pure_sym; jl_sym_t *loopinfo_sym;
jl_sym_t *meta_sym; jl_sym_t *inert_sym;
jl_sym_t *polly_sym; jl_sym_t *unused_sym;
jl_sym_t *static_parameter_sym; jl_sym_t *inline_sym;
jl_sym_t *noinline_sym; jl_sym_t *generated_sym;
jl_sym_t *generated_only_sym; jl_sym_t *isdefined_sym;
jl_sym_t *propagate_inbounds_sym; jl_sym_t *specialize_sym;
jl_sym_t *nospecialize_sym; jl_sym_t *macrocall_sym;
jl_sym_t *colon_sym; jl_sym_t *hygienicscope_sym;
jl_sym_t *throw_undef_if_not_sym; jl_sym_t *getfield_undefref_sym;
jl_sym_t *gc_preserve_begin_sym; jl_sym_t *gc_preserve_end_sym;
jl_sym_t *escape_sym;
jl_sym_t *aliasscope_sym; jl_sym_t *popaliasscope_sym;

void jl_init_frontend(void)
{
    if (jl_frontend.init)
        jl_frontend.init();

    empty_sym = jl_symbol("");
    call_sym = jl_symbol("call");
    invoke_sym = jl_symbol("invoke");
    foreigncall_sym = jl_symbol("foreigncall");
    cfunction_sym = jl_symbol("cfunction");
    quote_sym = jl_symbol("quote");
    inert_sym = jl_symbol("inert");
    top_sym = jl_symbol("top");
    core_sym = jl_symbol("core");
    globalref_sym = jl_symbol("globalref");
    line_sym = jl_symbol("line");
    jl_incomplete_sym = jl_symbol("incomplete");
    error_sym = jl_symbol("error");
    goto_sym = jl_symbol("goto");
    goto_ifnot_sym = jl_symbol("gotoifnot");
    return_sym = jl_symbol("return");
    unreachable_sym = jl_symbol("unreachable");
    lambda_sym = jl_symbol("lambda");
    module_sym = jl_symbol("module");
    export_sym = jl_symbol("export");
    import_sym = jl_symbol("import");
    using_sym = jl_symbol("using");
    assign_sym = jl_symbol("=");
    method_sym = jl_symbol("method");
    exc_sym = jl_symbol("the_exception");
    enter_sym = jl_symbol("enter");
    leave_sym = jl_symbol("leave");
    pop_exception_sym = jl_symbol("pop_exception");
    new_sym = jl_symbol("new");
    splatnew_sym = jl_symbol("splatnew");
    const_sym = jl_symbol("const");
    global_sym = jl_symbol("global");
    thunk_sym = jl_symbol("thunk");
    abstracttype_sym = jl_symbol("abstract_type");
    primtype_sym = jl_symbol("primitive_type");
    structtype_sym = jl_symbol("struct_type");
    toplevel_sym = jl_symbol("toplevel");
    dot_sym = jl_symbol(".");
    colon_sym = jl_symbol(":");
    boundscheck_sym = jl_symbol("boundscheck");
    inbounds_sym = jl_symbol("inbounds");
    newvar_sym = jl_symbol("newvar");
    copyast_sym = jl_symbol("copyast");
    loopinfo_sym = jl_symbol("loopinfo");
    pure_sym = jl_symbol("pure");
    meta_sym = jl_symbol("meta");
    list_sym = jl_symbol("list");
    unused_sym = jl_symbol("#unused#");
    slot_sym = jl_symbol("slot");
    static_parameter_sym = jl_symbol("static_parameter");
    inline_sym = jl_symbol("inline");
    noinline_sym = jl_symbol("noinline");
    polly_sym = jl_symbol("polly");
    propagate_inbounds_sym = jl_symbol("propagate_inbounds");
    isdefined_sym = jl_symbol("isdefined");
    nospecialize_sym = jl_symbol("nospecialize");
    specialize_sym = jl_symbol("specialize");
    macrocall_sym = jl_symbol("macrocall");
    escape_sym = jl_symbol("escape");
    hygienicscope_sym = jl_symbol("hygienic-scope");
    gc_preserve_begin_sym = jl_symbol("gc_preserve_begin");
    gc_preserve_end_sym = jl_symbol("gc_preserve_end");
    generated_sym = jl_symbol("generated");
    generated_only_sym = jl_symbol("generated_only");
    throw_undef_if_not_sym = jl_symbol("throw_undef_if_not");
    getfield_undefref_sym = jl_symbol("##getfield##");
    do_sym = jl_symbol("do");
    aliasscope_sym = jl_symbol("aliasscope");
    popaliasscope_sym = jl_symbol("popaliasscope");
}

int jl_has_meta(jl_array_t *body, jl_sym_t *sym)
{
    size_t i, l = jl_array_len(body);
    for (i = 0; i < l; i++) {
        jl_expr_t *stmt = (jl_expr_t*)jl_array_ptr_ref(body, i);
        if (jl_is_expr((jl_value_t*)stmt) && stmt->head == meta_sym) {
            size_t i, l = jl_array_len(stmt->args);
            for (i = 0; i < l; i++)
                if (jl_array_ptr_ref(stmt->args, i) == (jl_value_t*)sym)
                    return 1;
        }
    }
    return 0;
}

JL_DLLEXPORT jl_value_t *jl_copy_ast(jl_value_t *expr)
{
    if (expr && jl_is_expr(expr)) {
        jl_expr_t *e = (jl_expr_t*)expr;
        size_t i, l = jl_array_len(e->args);
        jl_expr_t *ne = jl_exprn(e->head, l);
        JL_GC_PUSH2(&ne, &expr);
        for (i = 0; i < l; i++) {
            jl_value_t *a = jl_exprarg(e, i);
            jl_exprargset(ne, i, jl_copy_ast(a));
        }
        JL_GC_POP();
        return (jl_value_t*)ne;
    }
    return expr;
}

// DLLEXPORT wrappers for the frontend
// Parsing
JL_DLLEXPORT jl_value_t *jl_parse_all(const char *str, size_t len, const char *filename, size_t filename_len)
{
    assert(jl_frontend.jl_parse_all);
    return jl_frontend.jl_parse_all(str, len, filename, filename_len);
}

JL_DLLEXPORT jl_value_t *jl_parse_string(const char *str, size_t len, int pos0, int greedy)
{
    assert(jl_frontend.jl_parse_string);
    return jl_frontend.jl_parse_string(str, len, pos0, greedy);
}

JL_DLLEXPORT jl_value_t *jl_parse_eval_all(const char *fname,
                                           const char *content, size_t contentlen,
                                           jl_module_t *inmodule)
{
    assert(jl_frontend.jl_parse_eval_all);
    return jl_frontend.jl_parse_eval_all(fname, content, contentlen, inmodule);
}

// Macro expand
JL_DLLEXPORT jl_value_t *jl_macroexpand(jl_value_t *expr, jl_module_t *inmodule)
{
    assert(jl_frontend.jl_macroexpand);
    return jl_frontend.jl_macroexpand(expr, inmodule);
}

JL_DLLEXPORT jl_value_t *jl_macroexpand1(jl_value_t *expr, jl_module_t *inmodule)
{
    assert(jl_frontend.jl_macroexpand1);
    return jl_frontend.jl_macroexpand1(expr, inmodule);
}

// Lowering
JL_DLLEXPORT jl_value_t *jl_expand_with_loc(jl_value_t *expr, jl_module_t *inmodule,
                                            const char *file, int line)
{
    assert(jl_frontend.jl_expand_with_loc);
    return jl_frontend.jl_expand_with_loc(expr, inmodule, file, line);
}

JL_DLLEXPORT jl_value_t *jl_expand_stmt_with_loc(jl_value_t *expr, jl_module_t *inmodule,
                                                 const char *file, int line)
{
    assert(jl_frontend.jl_expand_stmt_with_loc);
    return jl_frontend.jl_expand_stmt_with_loc(expr, inmodule, file, line);
}

// Informational queries
JL_DLLEXPORT int jl_is_operator(char *sym)
{
    assert(jl_frontend.jl_is_operator);
    return jl_frontend.jl_is_operator(sym);
}

JL_DLLEXPORT int jl_is_unary_operator(char *sym)
{
    assert(jl_frontend.jl_is_unary_operator);
    return jl_frontend.jl_is_unary_operator(sym);
}

JL_DLLEXPORT int jl_is_unary_and_binary_operator(char *sym)
{
    assert(jl_frontend.jl_is_unary_and_binary_operator);
    return jl_frontend.jl_is_unary_and_binary_operator(sym);
}

JL_DLLEXPORT int jl_operator_precedence(char *sym)
{
    assert(jl_frontend.jl_operator_precedence);
    return jl_frontend.jl_operator_precedence(sym);
}

// Convenience wrappers
JL_DLLEXPORT jl_value_t *jl_expand(jl_value_t *expr, jl_module_t *inmodule)
{
    return jl_expand_with_loc(expr, inmodule, "none", 0);
}

JL_DLLEXPORT jl_value_t *jl_expand_stmt(jl_value_t *expr, jl_module_t *inmodule)
{
    return jl_expand_stmt_with_loc(expr, inmodule, "none", 0);
}

JL_DLLEXPORT jl_value_t *jl_load_file_string(const char *text, size_t len,
                                             char *filename, jl_module_t *inmodule)
{
    return jl_parse_eval_all(filename, text, len, inmodule);
}

// for backwards compat
JL_DLLEXPORT jl_value_t *jl_parse_input_line(const char *str, size_t len, const char *filename, size_t filename_len)
{
    return jl_parse_all(str, len, filename, filename_len);
}

#ifdef __cplusplus
}
#endif

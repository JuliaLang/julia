#include "julia.h"
#include "julia_internal.h"

// Interface shim functions which call through to the functions from
// jl_frontend
static jl_frontend_t jl_frontend;

JL_DLLEXPORT void jl_set_frontend(jl_frontend_t frontend)
{
    jl_frontend = frontend;
}

//--------------------------------------------------
// Syntactic queries

JL_DLLEXPORT int jl_is_operator(char *sym)
{
    return jl_frontend.is_operator(sym);
}

JL_DLLEXPORT int jl_is_unary_operator(char *sym)
{
    return jl_frontend.is_unary_operator(sym);
}

JL_DLLEXPORT int jl_is_unary_and_binary_operator(char *sym)
{
    return jl_frontend.is_unary_and_binary_operator(sym);
}

JL_DLLEXPORT int jl_operator_precedence(char *sym)
{
    return jl_frontend.operator_precedence(sym);
}

//--------------------------------------------------
// Parsing

JL_DLLEXPORT jl_value_t *jl_parse_all(const char *str, size_t len,
                                      const char *filename, size_t filename_len)
{
    JL_TIMING(PARSING);
    return jl_frontend.parse_all(str, len, filename, filename_len);
}

JL_DLLEXPORT jl_value_t *jl_parse_string(const char *str, size_t len,
                                         int pos0, int greedy)
{
    JL_TIMING(PARSING);
    return jl_frontend.parse_string(str, len, pos0, greedy);
}

//--------------------------------------------------
// Macro expansion

JL_DLLEXPORT jl_value_t *jl_macroexpand(jl_value_t *expr, jl_module_t *inmodule)
{
    JL_TIMING(LOWERING);
    return jl_frontend.macroexpand(expr, inmodule);
}

JL_DLLEXPORT jl_value_t *jl_macroexpand1(jl_value_t *expr, jl_module_t *inmodule)
{
    JL_TIMING(LOWERING);
    return jl_frontend.macroexpand1(expr, inmodule);
}


//--------------------------------------------------
// Code lowering / "expansion"

JL_DLLEXPORT jl_value_t *jl_expand_with_loc(jl_value_t *expr, jl_module_t *inmodule,
                                            const char *file, int line)
{
    JL_TIMING(LOWERING);
    return jl_frontend.expand_with_loc(expr, inmodule, file, line);
}

JL_DLLEXPORT jl_value_t *jl_expand_stmt_with_loc(jl_value_t *expr, jl_module_t *inmodule,
                                                 const char *file, int line)
{
    JL_TIMING(LOWERING);
    return jl_frontend.expand_stmt_with_loc(expr, inmodule, file, line);
}

JL_DLLEXPORT jl_value_t *jl_expand_with_loc_warn(jl_value_t *expr, jl_module_t *inmodule,
                                                 const char *file, int line)
{
    JL_TIMING(LOWERING);
    return jl_frontend.expand_with_loc_warn(expr, inmodule, file, line);
}

JL_DLLEXPORT jl_value_t *jl_expand(jl_value_t *expr, jl_module_t *inmodule)
{
    return jl_expand_with_loc(expr, inmodule, "none", 0);
}

JL_DLLEXPORT jl_value_t *jl_expand_stmt(jl_value_t *expr, jl_module_t *inmodule)
{
    return jl_expand_stmt_with_loc(expr, inmodule, "none", 0);
}


//--------------------------------------------------
// Bundled parse-expand-eval functionality

// parse and eval a whole file, possibly reading from a string (`content`)
jl_value_t *jl_parse_eval_all(const char *fname,
                              const char *content, size_t contentlen,
                              jl_module_t *inmodule,
                              jl_value_t *mapexpr)
{
    return jl_frontend.parse_eval_all(fname, content, contentlen, inmodule, mapexpr);
}

JL_DLLEXPORT jl_value_t *jl_load_rewrite_file_string(const char *text, size_t len,
                                                     char *filename, jl_module_t *inmodule,
                                                     jl_value_t *mapexpr)
{
    return jl_parse_eval_all(filename, text, len, inmodule, mapexpr);
}

JL_DLLEXPORT jl_value_t *jl_load_file_string(const char *text, size_t len,
                                             char *filename, jl_module_t *inmodule)
{
    return jl_parse_eval_all(filename, text, len, inmodule, NULL);
}


//--------------------------------------------------
// Deprecated
JL_DLLEXPORT jl_value_t *jl_parse_input_line(const char *str, size_t len, const char *filename, size_t filename_len)
{
    return jl_parse_all(str, len, filename, filename_len);
}


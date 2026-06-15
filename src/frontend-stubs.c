// This file is a part of Julia. License is MIT: https://julialang.org/license

// This file provides a fallback implementation of the frontend interface,
// used when libjulia-frontend is not available.

#include "julia.h"
#include "julia_internal.h"

#define UNAVAILABLE { jl_errorf("%s: not available in this build of Julia", __func__); }

JL_DLLEXPORT void jl_frontend_init_fallback(void) { }

JL_DLLEXPORT jl_value_t *jl_frontend_parse_fallback(const char *text, size_t text_len,
                                                    jl_value_t *filename, size_t lineno,
                                                    size_t offset, jl_value_t *options) UNAVAILABLE

JL_DLLEXPORT jl_value_t *jl_frontend_lower_fallback(jl_value_t *expr, jl_module_t *inmodule,
                                                    const char *file, int line, size_t world,
                                                    bool_t warn) UNAVAILABLE

JL_DLLEXPORT jl_value_t *jl_macroexpand_fallback(jl_value_t *expr, jl_module_t *inmodule,
                                                 int recursive, int inplace, int expand_scope) UNAVAILABLE

// Conservative answers, so that printing of syntax trees stays functional
// (if less pretty) without the frontend library, even in error paths.
JL_DLLEXPORT int jl_is_operator_fallback(const char *sym) { return 0; }
JL_DLLEXPORT int jl_is_unary_operator_fallback(const char *sym) { return 0; }
JL_DLLEXPORT int jl_is_unary_and_binary_operator_fallback(const char *sym) { return 0; }
JL_DLLEXPORT int jl_is_syntactic_operator_fallback(const char *sym) { return 0; }
JL_DLLEXPORT int jl_operator_precedence_fallback(const char *sym) { return 0; }

JL_DLLEXPORT void jl_lisp_prompt_fallback(void) UNAVAILABLE
JL_DLLEXPORT void fl_profile_fallback(const char *fname) UNAVAILABLE
JL_DLLEXPORT void fl_show_profile_fallback(void) UNAVAILABLE
JL_DLLEXPORT void fl_clear_profile_fallback(void) UNAVAILABLE

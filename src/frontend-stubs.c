// This file is a part of Julia. License is MIT: https://julialang.org/license

// This file provides a fallback implementation of the frontend interface,
// used when libjulia-frontend is not available.

#include "julia.h"
#include "julia_internal.h"

#define UNAVAILABLE { jl_errorf("%s: not available in this build of Julia", __func__); }

#ifdef JL_LIBRARY_STATIC
// The static runtime does not include the frontend, so these stubs provide the
// frontend interface directly under its public names.
#include <jl_static_codegen_fallbacks.inc>
#endif

JL_DLLEXPORT void jl_frontend_init_fallback(void) { }

JL_DLLEXPORT jl_value_t *jl_frontend_parse_fallback(const char *text, size_t text_len,
                                                    jl_value_t *filename, size_t lineno,
                                                    size_t offset, jl_value_t *options) UNAVAILABLE

JL_DLLEXPORT jl_value_t *jl_frontend_lower_fallback(jl_module_t *inmodule, jl_value_t *expr, size_t world,
                                                    bool_t warn, jl_value_t *loc) UNAVAILABLE

JL_DLLEXPORT jl_value_t *jl_macroexpand_fallback(jl_value_t *expr, jl_module_t *inmodule,
                                                 int recursive, int inplace, int expand_scope) UNAVAILABLE

// Conservative answers, so that printing of syntax trees stays functional
// (if less pretty) without the frontend library, even in error paths.
JL_DLLEXPORT int jl_frontend_is_operator_fallback(const char *sym) { return 0; }
JL_DLLEXPORT int jl_frontend_is_unary_operator_fallback(const char *sym) { return 0; }
JL_DLLEXPORT int jl_frontend_is_unary_and_binary_operator_fallback(const char *sym) { return 0; }
JL_DLLEXPORT int jl_frontend_is_syntactic_operator_fallback(const char *sym) { return 0; }
JL_DLLEXPORT int jl_frontend_operator_precedence_fallback(const char *sym) { return 0; }

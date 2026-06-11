# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    JuliaFrontend

Implementation of Julia's frontend interface (the `libjulia-frontend`
boundary: `jl_frontend_init`, `jl_frontend_parse`, `jl_frontend_lower`,
`jl_macroexpand` and the operator query functions) in terms of JuliaSyntax
and JuliaLowering, with no use of the flisp reference frontend.

This package is compiled with juliac into a shared library exporting the
`*_impl` C entry points expected by the runtime's frontend trampolines (see
`JL_FRONTEND_EXPORTED_FUNCS` in `src/jl_exported_funcs.inc`). See
contrib/juliac-frontend/README.md for how the library is built and
validated, and for the current restrictions on loading it in-process.
"""
module JuliaFrontend

using JuliaSyntax
using JuliaLowering

const _JS = JuliaSyntax
include("flisp_ops.jl")

# ------------------------------------------------------------------------
# Parsing: implements the jl_frontend_parse contract,
#   (text, text_len, filename::String, lineno, offset, options::Symbol)
#   -> svec(expr, end_offset)
# `options` is one of :atom, :statement or :all; offsets are 0-based.

function frontend_parse(text::Ptr{UInt8}, text_len::Int, filename, lineno::Int,
                        offset::Int, options)
    filename isa String || throw(ArgumentError("filename must be a String"))
    options isa Symbol || throw(ArgumentError("parse options must be a Symbol"))
    if offset > text_len
        throw(BoundsError(unsafe_string(text, text_len), offset + 1))
    end
    code = Core.svec(text, text_len)
    # The C frontend interface has no syntax version parameter; like flisp,
    # always parse the current version's syntax.
    return JuliaSyntax.core_parser_hook(code, filename, lineno, offset, options;
                                        syntax_version=Base.VERSION)
end

# ------------------------------------------------------------------------
# Lowering: implements the jl_frontend_lower contract,
#   (expr, module, filename, line, world, warn) -> svec(lowered, ...)

function frontend_lower(@nospecialize(ex), mod::Module, filename::String,
                        line::Int, world::UInt, warn::Bool)
    return JuliaLowering.core_lowering_hook(ex, mod, filename, line, world, warn)
end

# ------------------------------------------------------------------------
# Macro expansion: implements the jl_macroexpand contract,
#   (expr, module, recursive, inplace, expand_scope) -> expr
# JuliaLowering performs macro expansion as its first lowering pass; we run
# just that pass and convert back to Expr. The flisp-specific
# `expand_scope`/legacyscope flag (resolution of `hygienic-scope` nodes) and
# one-level expansion are not yet supported.

function frontend_macroexpand(@nospecialize(ex), mod::Module, recursive::Bool,
                              inplace::Bool, expand_scope::Bool)
    ex isa Expr || return ex
    if !recursive
        error("one-level macro expansion (`@macroexpand1`) is not yet supported by the JuliaSyntax/JuliaLowering frontend")
    end
    st = JuliaLowering.expr_to_est(ex)
    st1 = JuliaLowering.macroexpand(mod, st; expr_compat_mode=true,
                                    world=Base.get_world_counter())
    return JuliaLowering.est_to_expr(st1)
end

# ------------------------------------------------------------------------
# C entry points matching JL_FRONTEND_EXPORTED_FUNCS. The runtime fills its
# frontend trampolines with the `*_impl` symbols exported here when this
# library is used as libjulia-frontend.

Base.@ccallable "jl_frontend_init_impl" function _c_frontend_init()::Cvoid
    return nothing
end

Base.@ccallable "jl_frontend_parse_impl" function _c_frontend_parse(
        text::Ptr{UInt8}, text_len::Csize_t, filename::Any,
        lineno::Csize_t, offset::Csize_t, options::Any)::Any
    return frontend_parse(text, Int(text_len), filename, Int(lineno),
                          Int(offset), options)
end

Base.@ccallable "jl_frontend_lower_impl" function _c_frontend_lower(
        ex::Any, mod::Module, filename::Ptr{UInt8}, line::Cint,
        world::Csize_t, warn::Cint)::Any
    fname = filename == C_NULL ? "none" : unsafe_string(filename)
    return frontend_lower(ex, mod, fname, Int(line), UInt(world), warn != 0)
end

Base.@ccallable "jl_macroexpand_impl" function _c_macroexpand(
        ex::Any, mod::Module, recursive::Cint, inplace::Cint,
        expand_scope::Cint)::Any
    return frontend_macroexpand(ex, mod, recursive != 0, inplace != 0,
                                expand_scope != 0)
end

Base.@ccallable "jl_is_operator_impl" function _c_is_operator(s::Cstring)::Cint
    return Cint(flisp_is_operator(unsafe_string(s)))
end

Base.@ccallable "jl_is_unary_operator_impl" function _c_is_unary_operator(s::Cstring)::Cint
    return Cint(flisp_is_unary_operator(unsafe_string(s)))
end

Base.@ccallable "jl_is_unary_and_binary_operator_impl" function _c_is_unary_and_binary_operator(s::Cstring)::Cint
    return Cint(flisp_is_unary_and_binary_operator(unsafe_string(s)))
end

Base.@ccallable "jl_is_syntactic_operator_impl" function _c_is_syntactic_operator(s::Cstring)::Cint
    return Cint(flisp_is_syntactic_operator(unsafe_string(s)))
end

Base.@ccallable "jl_operator_precedence_impl" function _c_operator_precedence(s::Cstring)::Cint
    return flisp_operator_precedence(unsafe_string(s))
end

# flisp-specific entry points: this frontend has no flisp.

Base.@ccallable "jl_lisp_prompt_impl" function _c_lisp_prompt()::Cvoid
    error("--lisp: this frontend library is built from JuliaSyntax/JuliaLowering and does not contain flisp")
end

Base.@ccallable "fl_profile_impl" function _c_fl_profile(fname::Cstring)::Cvoid
    error("flisp profiling is not available in the JuliaSyntax/JuliaLowering frontend")
end

Base.@ccallable "fl_show_profile_impl" function _c_fl_show_profile()::Cvoid
    error("flisp profiling is not available in the JuliaSyntax/JuliaLowering frontend")
end

Base.@ccallable "fl_clear_profile_impl" function _c_fl_clear_profile()::Cvoid
    error("flisp profiling is not available in the JuliaSyntax/JuliaLowering frontend")
end

end # module JuliaFrontend

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
    # n.b. the C frontend interface has no syntax version parameter; parse
    # with the hook's default version, like the in-process default parser.
    r = JuliaSyntax.core_parser_hook(code, filename, lineno, offset, options)
    return _legacy_error_form(r)
end

# Rich ParseError objects cannot cross the runtime boundary when this
# library runs as a standalone second runtime; rewrite error/incomplete
# results to the legacy string form, which all consumers understand.
function _legacy_error_form(r)
    ex = r[1]
    (ex isa Expr && (ex.head === :error || ex.head === :incomplete)) || return r
    length(ex.args) == 1 || return r
    arg = ex.args[1]
    arg isa Union{String,Expr,Symbol} && return r
    msg = try
        arg isa Meta.ParseError ? arg.msg : sprint(showerror, arg)
    catch
        "parse error"
    end
    if ex.head === :incomplete
        tag = arg isa Meta.ParseError && arg.detail isa JuliaSyntax.ParseError ?
            arg.detail.incomplete_tag : :other
        msg =
            tag === :string  ? "incomplete: invalid string syntax"     :
            tag === :comment ? "incomplete: unterminated multi-line comment #= ... =#" :
            tag === :block   ? "incomplete: construct requires end"    :
            tag === :cmd     ? "incomplete: invalid \"`\" syntax"      :
            tag === :char    ? "incomplete: invalid character literal" :
                               "incomplete: premature end of input"
    end
    return Core.svec(Expr(ex.head, msg), r[2])
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
# Native C entry points. These use this image's own value representation
# (arguments and results are values of *this* runtime). When the library is
# compiled standalone (see the `standalone` Makefile target), the exported
# `jl_frontend_*_impl` ABI wrappers in fe_standalone_entry.c convert between
# the host runtime's values and these natives. The results are additionally
# kept alive in `_last_result` until the next call, since the C caller holds
# them across the boundary without a GC frame in this world.

Base.@ccallable "jlfe_init" function _c_frontend_init()::Cvoid
    return nothing
end

const _last_result = Ref{Any}(nothing)

Base.@ccallable "jlfe_parse" function _c_frontend_parse(
        text::Ptr{UInt8}, text_len::Csize_t, filename::Any,
        lineno::Csize_t, offset::Csize_t, options::Any)::Any
    r = try
        frontend_parse(text, Int(text_len), filename, Int(lineno),
                       Int(offset), options)
    catch err
        Core.svec(Expr(:error, "JuliaFrontend internal error: " *
                       sprint(showerror, err)), Int(text_len))
    end
    _last_result[] = r
    return r
end

Base.@ccallable "jlfe_lower" function _c_frontend_lower(
        ex::Any, mod::Module, filename::Ptr{UInt8}, line::Cint,
        world::Csize_t, warn::Cint)::Any
    fname = filename == C_NULL ? "none" : unsafe_string(filename)
    # n.b. must not throw: in the standalone configuration there is no
    # handler in this runtime above this frame. Errors are returned as a
    # sentinel for the entry point to rethrow host-side.
    r = try
        frontend_lower(ex, mod, fname, Int(line), UInt(world), warn != 0)
    catch err
        msg = try
            sprint(showerror, err)
        catch
            "error while printing lowering error"
        end
        Core.svec(:__fe_lowering_error__, msg)
    end
    _last_result[] = r
    return r
end

Base.@ccallable "jlfe_macroexpand" function _c_macroexpand(
        ex::Any, mod::Module, recursive::Cint, inplace::Cint,
        expand_scope::Cint)::Any
    r = frontend_macroexpand(ex, mod, recursive != 0, inplace != 0,
                             expand_scope != 0)
    _last_result[] = r
    return r
end

Base.@ccallable "jlfe_is_operator" function _c_is_operator(s::Cstring)::Cint
    return Cint(flisp_is_operator(unsafe_string(s)))
end

Base.@ccallable "jlfe_is_unary_operator" function _c_is_unary_operator(s::Cstring)::Cint
    return Cint(flisp_is_unary_operator(unsafe_string(s)))
end

Base.@ccallable "jlfe_is_unary_and_binary_operator" function _c_is_unary_and_binary_operator(s::Cstring)::Cint
    return Cint(flisp_is_unary_and_binary_operator(unsafe_string(s)))
end

Base.@ccallable "jlfe_is_syntactic_operator" function _c_is_syntactic_operator(s::Cstring)::Cint
    return Cint(flisp_is_syntactic_operator(unsafe_string(s)))
end

Base.@ccallable "jlfe_operator_precedence" function _c_operator_precedence(s::Cstring)::Cint
    return flisp_operator_precedence(unsafe_string(s))
end

# flisp-specific entry points: this frontend has no flisp.

Base.@ccallable "jlfe_lisp_prompt" function _c_lisp_prompt()::Cvoid
    error("--lisp: this frontend library is built from JuliaSyntax/JuliaLowering and does not contain flisp")
end

Base.@ccallable "jlfe_fl_profile" function _c_fl_profile(fname::Cstring)::Cvoid
    error("flisp profiling is not available in the JuliaSyntax/JuliaLowering frontend")
end

Base.@ccallable "jlfe_fl_show_profile" function _c_fl_show_profile()::Cvoid
    error("flisp profiling is not available in the JuliaSyntax/JuliaLowering frontend")
end

Base.@ccallable "jlfe_fl_clear_profile" function _c_fl_clear_profile()::Cvoid
    error("flisp profiling is not available in the JuliaSyntax/JuliaLowering frontend")
end

end # module JuliaFrontend

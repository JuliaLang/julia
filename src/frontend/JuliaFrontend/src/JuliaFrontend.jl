# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    JuliaFrontend

Implementation of Julia's frontend interface (the `libjulia-frontend`
boundary: `jl_frontend_init`, `jl_frontend_parse`, `jl_frontend_lower` and
`jl_macroexpand`) in terms of JuliaSyntax and JuliaLowering.

This package is compiled, by a julia release, into the image of
libjulia-frontend, whose `*_impl` C entry points (see entry.c) the runtime's
frontend trampolines bind (see `JL_FRONTEND_EXPORTED_FUNCS` in
`src/jl_exported_funcs.inc`). See src/frontend/README.md.
"""
module JuliaFrontend

using JuliaSyntax
using JuliaLowering


# ------------------------------------------------------------------------
# Parsing: implements the jl_frontend_parse contract,
#   (text, text_len, filename::String, lineno, offset, options::Symbol)
#   -> svec(expr, end_offset)
# `options` is one of :atom, :statement or :all; offsets are 0-based.

# The C frontend interface has no edition parameter: parse with the edition
# of the frontend this replaces (`Core.FLISP_EDITION`), which is the syntax
# that code parsed through the interface (julia's own bootstrap) is written in.
const ABI_EDITION = (1, 14)

function frontend_parse(text::Ptr{UInt8}, text_len::Int, filename::String, lineno::Int,
                        offset::Int, options::Symbol)
    if offset > text_len
        throw(BoundsError(unsafe_string(text, text_len), offset + 1))
    end
    r = JuliaSyntax.core_parser_hook(Core.svec(text, text_len), filename, lineno, offset,
                                     options, ABI_EDITION)
    ex = r[1]
    # Strip per-module edition markers: they could not cross the runtime
    # boundary during bootstrap (a VersionNumber needs the host's Base), and
    # the edition is implied by the interface anyway. flisp emits unversioned
    # modules.
    ex isa Expr && _strip_module_versions!(ex)
    return _legacy_error_form(r)
end

function _strip_module_versions!(ex::Expr)
    if ex.head === :module && length(ex.args) >= 1 && ex.args[1] isa VersionNumber
        popfirst!(ex.args)
    end
    for a in ex.args
        a isa Expr && _strip_module_versions!(a)
    end
    return ex
end

# Rich ParseError objects cannot cross the runtime boundary when this
# library runs as a standalone second runtime; rewrite error/incomplete
# results to the legacy string form, which all consumers understand.
function _legacy_error_form(r)
    ex = r[1]
    if ex isa Expr && ex.head === :toplevel
        # in :all mode a parse error appears as a nested error node
        for (i, a) in enumerate(ex.args)
            if a isa Expr && (a.head === :error || a.head === :incomplete)
                ex.args[i] = _legacy_error_node(a)
            end
        end
        return r
    end
    (ex isa Expr && (ex.head === :error || ex.head === :incomplete)) || return r
    ex2 = _legacy_error_node(ex)
    return ex2 === ex ? r : Core.svec(ex2, r[2])
end

function _legacy_error_node(ex::Expr)
    length(ex.args) == 1 || return ex
    arg = ex.args[1]
    arg isa Union{String,Expr,Symbol} && return ex
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
    return Expr(ex.head, msg)
end

# ------------------------------------------------------------------------
# Lowering: implements the jl_frontend_lower contract,
#   (module, expr, world, warn, loc::LineNumberNode) -> svec(lowered, ...)
# The output must be evaluable by the runtime which called us, which (in the
# standalone configuration) is not this one: lower portably.

function frontend_lower(mod::Module, @nospecialize(ex), world::UInt, warn::Bool,
                        loc::LineNumberNode)
    return JuliaLowering.portable_lowering_hook(mod, ex, world, warn, loc)
end

# ------------------------------------------------------------------------
# Macro expansion: implements the jl_macroexpand contract,
#   (expr, module, recursive, inplace, expand_scope) -> expr
# JuliaLowering performs macro expansion as its first lowering pass; we run
# just that pass and convert back to Expr. The flisp-specific
# `expand_scope`/legacyscope flag (resolution of `hygienic-scope` nodes) is
# not supported. (In the standalone configuration macros are expanded in the
# host instead; see fe_standalone_entry.c.)

function frontend_macroexpand(@nospecialize(ex), mod::Module, recursive::Bool,
                              inplace::Bool, expand_scope::Bool)
    ex isa Expr || return ex
    st = JuliaLowering.expr_to_est(ex)
    st1 = JuliaLowering.macroexpand(mod, st; recursive)
    return JuliaLowering.est_to_expr(st1)
end

# ------------------------------------------------------------------------
# Native C entry points. These use this image's own value representation
# (arguments and results are values of *this* runtime). When the library is
# compiled standalone, the exported `jl_frontend_*_impl` ABI wrappers in
# entry.c convert between the host runtime's values and these
# natives. The results are additionally kept alive in `_last_result` until
# the next call, since the C caller holds them across the boundary without a
# GC frame in this world. Values are passed as pointers, since the julia
# that compiles this library may not accept `Any` arguments to `@ccallable`.

_obj(p::Ptr{Cvoid}) = unsafe_pointer_to_objref(p)

Base.@ccallable "jlfe_init" function _c_frontend_init()::Cvoid
    return nothing
end

const _last_result = Ref{Any}(nothing)

# See JuliaLowering._module_binding_salt
Base.@ccallable "jlfe_set_binding_salt" function _c_set_binding_salt(salt::Cstring)::Cvoid
    JuliaLowering._module_binding_salt[] = unsafe_string(salt)
    return nothing
end

Base.@ccallable "jlfe_parse" function _c_frontend_parse(
        text::Ptr{UInt8}, text_len::Csize_t, filename::Ptr{Cvoid},
        lineno::Csize_t, offset::Csize_t, options::Ptr{Cvoid})::Any
    r = try
        frontend_parse(text, Int(text_len), _obj(filename)::String, Int(lineno),
                       Int(offset), _obj(options)::Symbol)
    catch err
        Core.svec(Expr(:error, "JuliaFrontend internal error: " *
                       sprint(showerror, err)), Int(text_len))
    end
    _last_result[] = r
    return r
end

Base.@ccallable "jlfe_lower" function _c_frontend_lower(
        mod::Module, ex::Ptr{Cvoid}, world::Csize_t, warn::Cint,
        filename::Ptr{UInt8}, line::Cint)::Any
    fname = filename == C_NULL ? :none : Symbol(unsafe_string(filename))
    # n.b. must not throw: in the standalone configuration there is no
    # handler in this runtime above this frame. Errors are returned as a
    # sentinel for the entry point to rethrow host-side.
    r = try
        frontend_lower(mod, _obj(ex), UInt(world), warn != 0,
                       LineNumberNode(Int(line), fname))
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

# The operator queries (see JuliaSyntax.operator_info), which the host asks
# the frontend until its Base has started, packed into bits: 1 for operators, 2
# for unary ones, 4 for unary-and-binary ones and 8 for syntactic ones, and the
# precedence from bit 8 on.
Base.@ccallable "jlfe_operator_info" function _c_operator_info(name::Cstring)::Cint
    info = try
        JuliaSyntax.operator_info(unsafe_string(name))
    catch
        JuliaSyntax.NOT_AN_OP
    end
    return Cint(info.isop) | Cint(info.unary) << 1 | Cint(info.unary_binary) << 2 |
           Cint(info.syntactic) << 3 | info.prec << 8
end

Base.@ccallable "jlfe_macroexpand" function _c_macroexpand(
        ex::Ptr{Cvoid}, mod::Module, recursive::Cint, inplace::Cint,
        expand_scope::Cint)::Any
    r = frontend_macroexpand(_obj(ex), mod, recursive != 0, inplace != 0,
                             expand_scope != 0)
    _last_result[] = r
    return r
end

end # module JuliaFrontend

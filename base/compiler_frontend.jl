# This file is a part of Julia. License is MIT: https://julialang.org/license
baremodule CompilerFrontend

# Parsing
export AbstractCompilerFrontend, parsecode, syntaxtree, checkparse
# Lowering
export TopLevelCodeIterator, BeginModule, EndModule, ToplevelExpression
export compiler_frontend, set_compiler_frontend!
public eval, include_string, lower_init, lower_step, location, _parse_hook

using Base
using Core: CodeInfo

abstract type AbstractCompilerFrontend
end

"""
    (parse_result, next_index) = parsecode(frontend, rule, code, first_index;
                                           filename, first_line)

Parse Julia code with the provided Julia compiler `frontend`. `parse_result` is
a container for the "result of parsing" from which a syntax tree and compiler
errors / diagnostics may be extracted.
"""
function parsecode
end

"""
tree = syntaxtree(frontend, [tree_type, ] parse_result)

Return a syntax `tree` of type `tree_type` based on the `parse_result` coming
from `parse_code`. If `tree_type` is absent, return the preferred tree type for
the frontend as may be passed to `lower_init()`.
"""
function syntaxtree
end

"""
checkparse(frontend, parse_result)

Check that parsing was successful, throwing an error if not.

TODO: Add support for compiler warnings and other diagnostics here
"""
function checkparse
end

# Incremental lowering API which manages toplevel and module expressions via
# iteration. The iteration API here is bespoke because evaluation and lowering
# must pass parameters back and forth:
# * Lowering informs evaluation about the sequence of thunks, module begin/end,
#   etc
# * Evaluation manages the global state, creating modules and providing them as
#   them as context to lowering.
abstract type TopLevelCodeIterator
end

struct BeginModule
    name::Symbol
    syntax_version::Any
    standard_defs::Bool # If true, use `Base` and define `eval` and `include`
    location::Any # Currently LineNumberNode but might be something else in future
end

struct EndModule
end

# Existing flisp lowering is bypassed in some cases, deferring to the runtime
# code in Core.eval. ToplevelExpression accommodates this.
struct ToplevelExpression
    val
    location # Currently LineNumberNode
end

# Return a subtype of `TopLevelCodeIterator` which may be passed to `lower_step`
function lower_init
end

# Returns one of the types:
# * `CodeInfo`    - top level thunk to evaluate
# * `BeginModule` - start a new module (must be paired with EndModule later in iteration)
# * `EndModule`   - finish current module
# * `ToplevelExpression` - Alternative for trivial `CodeInfo`s or "toplevel-only"
#                   expressions from flisp lowering.
# * `Nothing`     - iteration is finished
function lower_step
end

function location
end

#-------------------------------------------------------------------------------
# Barebones `eval()` based on top level iteration API
"""
    simple_eval(mod, x)

`simple_eval` performs standard evaluation on the return value of `lower_init`
and any of the possible return values of `lower_step`.
"""
function simple_eval
end

function _check_top_level_effect(mod::Module)
    @ccall jl_check_top_level_effect(mod::Any, "eval"::Ptr{UInt8})::Cvoid
end

function simple_eval(mod::Module, thunk::CodeInfo)
    _check_top_level_effect(mod)
    @ccall jl_eval_thunk(mod::Any, thunk::Any, 1::Cint)::Any
end

# Shim in case we want extend the allowed types of the location field
unpack_location(loc::LineNumberNode) = (string(something(loc.file, :none)), loc.line)

function _toplevel_eval_flex(mod::Module, x::Any, fileref, lineref)
    @ccall jl_toplevel_eval_flex(mod::Any, x::Any, 1::Cint, 0::Cint,
                                 fileref::Ptr{Ptr{UInt8}}, lineref::Ptr{Csize_t})::Any
end

function simple_eval(mod::Module, v::ToplevelExpression)
    _check_top_level_effect(mod)

    file, line = unpack_location(v.location)
    fileref = Ref{Ptr{UInt8}}(Base.unsafe_convert(Ptr{UInt8}, file))
    lineref = Ref{Csize_t}(line)

    # Use toplevel expression interpreter for special cases where the existing
    # eval/lowering pipeline doesn't produce a CodeInfo. Requires latest world.
    invokelatest(_toplevel_eval_flex, mod, v.val, fileref, lineref)
end

function simple_eval(mod::Module, newmod::BeginModule)
    _check_top_level_effect(mod)
    file, line = unpack_location(newmod.location)
    @ccall jl_begin_new_module(mod::Module, newmod.name::Symbol,
                               newmod.syntax_version::Any,
                               newmod.standard_defs::Cint,
                               file::Ptr{UInt8}, line::Cint)::Module
end

function simple_eval(mod::Module, ::EndModule)
    _check_top_level_effect(mod)
    @ccall jl_end_new_module(mod::Module)::Cvoid
    return mod
end

function eval(frontend::AbstractCompilerFrontend, mod::Module, ex; throw_load_error=false, opts...)
    iter = lower_init(frontend, mod, ex; opts...)
    modules = Module[mod]
    result = nothing
    local loc
    try
        while true
            if throw_load_error
                loc = location(iter)
            end
            thunk = lower_step(iter, last(modules), Base.get_world_counter)
            if thunk === nothing
                @assert length(modules) == 1
                return result
            end
            result = simple_eval(last(modules), thunk)
            if thunk isa BeginModule
                push!(modules, result::Module)
            elseif thunk isa EndModule
                pop!(modules)
            end
        end
    catch exc
        # Ideally we'd remove LoadError as the information is reliably in the
        # stacktrace. However many packages expect LoadError especially in
        # their tests.
        if throw_load_error
            file, line = unpack_location(loc)
            rethrow(LoadError(file, line, exc))
        else
            rethrow()
        end
    end
end

function include_string(frontend::AbstractCompilerFrontend, mod::Module, code::AbstractString;
                        filename::AbstractString="string", first_line=1, opts...)
    parse_result, _ = parsecode(frontend, :all, code, 1; filename, first_line=first_line)
    checkparse(frontend, parse_result)
    ex = syntaxtree(frontend, parse_result)
    eval(frontend, mod, ex; opts...)
end

"""
    lower(frontend, mod, ex)

Lower expression `ex` in module `mod`. Expressions containing multiple top
level statements are returned unaltered.
"""
function lower(frontend::AbstractCompilerFrontend, mod::Module, ex)
    iter = lower_init(frontend, mod, ex; enter_toplevel=false)
    thunk = lower_step(iter, mod, Base.get_world_counter)
    @assert lower_step(iter, mod, Base.get_world_counter) == nothing
    return thunk
end

const _frontend_var_name = :var"#compiler-frontend#"

unwrap_frontend(fe::AbstractCompilerFrontend) = fe
unwrap_frontend(fe::Base.ScopedValues.ScopedValue) = unwrap_frontend(fe[])

"""
    compiler_frontend([versionctx=nothing])

Look up compiler frontend for `versionctx` which may be one of:

* `nothing` - Use the default system compiler at `VERSION`
* `version` - Use the default system compiler at that syntax version number
* `module` - Use the compiler recorded for the given module in the latest
             world, or the default compiler if none exists.
* `(module, world)` - use the module compiler at a particular world age
"""
function compiler_frontend(ctx::Nothing = nothing)
    unwrap_frontend(Base._default_compiler_frontend)::AbstractCompilerFrontend
end

# If VersionNumber is supplied, select based on both version number and the
# reference compiler installed in Base allowing dev versions of the compiler to
# be used.
function compiler_frontend(ctx::VersionNumber)
    compiler_frontend(compiler_frontend(), ctx)
end

function compiler_frontend(mod::Module)
    m = isdefined(mod, _frontend_var_name) ? mod : Base
    return unwrap_frontend(getglobal(m, _frontend_var_name))::AbstractCompilerFrontend
end

function compiler_frontend(mod_and_world::Tuple{Module, UInt})
    Base.invoke_in_world(mod_and_world[2], compiler_frontend, mod_and_world[1])
end

function set_compiler_frontend!(mod::Module, frontend, _use_const=true)
    if _use_const
        Core.declare_const(mod, _frontend_var_name, frontend)
    else
        Core.declare_global(mod, _frontend_var_name, true)
        @invokelatest Core.setglobal!(mod, _frontend_var_name, frontend)
    end
    return nothing
end

function eval(mod::Module, ex; opts...)
    eval(compiler_frontend(mod), mod, ex; opts...)
end

function include_string(mod::Module, ex; opts...)
    include_string(compiler_frontend(mod), mod, ex; opts...)
end

function lower(mod::Module, ex)
    lower(compiler_frontend(mod), mod, ex)
end

# Parser hook for Core._parse, used by jl_parse
# TODO: Fixed world age for frontend
function _parse_hook(code::Union{AbstractString,Core.SimpleVector}, filename::String, first_line::Int,
                     offset::Int, rule::Symbol, versionctx)
    if code isa Core.SimpleVector
        # The C entry points will pass us this form.
        (ptr,len) = code
        code = String(unsafe_wrap(Array, ptr, len))
    elseif !(code isa String || code isa SubString || code isa Vector{UInt8})
        # For non-Base string types, convert to UTF-8 encoding, using an
        # invokelatest to avoid world age issues.
        code = Base.invokelatest(String, code)
    end
    frontend = compiler_frontend(versionctx)
    parse_result, next_index = parsecode(frontend, rule, code, offset+1;
                                         filename, first_line=first_line)
    tree = syntaxtree(frontend, Expr, parse_result)
    return Core.svec(tree, next_index-1)
end

end # module CompilerFrontend

#-------------------------------------------------------------------------------
using .CompilerFrontend

# Julia's builtin flisp-based compiler frontend

#--------------------------------------------------
struct FlispCompilerFrontend <: AbstractCompilerFrontend
end

function CompilerFrontend.compiler_frontend(::FlispCompilerFrontend, ver::VersionNumber)
    # Ignore versioning assuming this will only be used internally in bootstrap.
    FlispCompilerFrontend()
end

function CompilerFrontend.parsecode(frontend::FlispCompilerFrontend, rule::Symbol, code, first_index;
                         filename::String="none", first_line::Int=1)
    ex, offset = fl_parse(code, filename, first_line, first_index-1, rule, nothing)
    return ex, offset+1
end

function CompilerFrontend.syntaxtree(frontend::FlispCompilerFrontend, ex)
    syntaxtree(frontend, Expr, ex)
end

function CompilerFrontend.syntaxtree(frontend::FlispCompilerFrontend, ::Type{Expr}, ex)
    return ex
end

function CompilerFrontend.checkparse(frontend::FlispCompilerFrontend, ex)
    if !(ex isa Expr)
        return
    end
    h = ex.head
    if h === :toplevel && !isempty(ex.args)
        checkparse(frontend, last(ex.args))
    elseif h === :error || h === :incomplete
        err = ex.args[1]
        if err isa String
            err = Meta.ParseError(err)
        end
        throw(err)
    end
    return
end

function CompilerFrontend.lower_init(::FlispCompilerFrontend, mod::Module, ex;
                                     mapexpr=nothing, filename="none", first_line=0, warn=true,
                                     logexpr=nothing, enter_toplevel=true)
    FlispLoweringIterator(ex, first_line, filename, warn, enter_toplevel, mapexpr, logexpr)
end

#--------------------------------------------------
# flisp-based lowering iterator
mutable struct FlispLoweringIterator <: TopLevelCodeIterator
    current_loc::LineNumberNode
    warn::Bool   # Print lowering warnings from flisp to the julia logger
    enter_toplevel::Bool
    todo::Vector{Tuple{Any, Bool, Int, Bool}}
    mapexpr::Any
    logexpr::Any
end

function FlispLoweringIterator(ex, first_line::Integer, filename::String, warn::Bool, enter_toplevel::Bool, mapexpr, logexpr)
    loc = LineNumberNode(first_line, filename)
    if mapexpr === identity
        mapexpr = nothing
    end
    iter = FlispLoweringIterator(loc, warn, enter_toplevel, [], mapexpr, logexpr)
    _push_next_expr!(iter, ex, false, 0, true)
    return iter
end

function _push_next_expr!(iter::FlispLoweringIterator, ex, is_module_body, child_idx,
                          outermost_expr=false)
    if child_idx == 0 && ex isa Expr && ex.head == :toplevel && iter.enter_toplevel
        child_idx = 1
    end
    if child_idx > 0
        # Eagerly consume line number nodes so we know where the next top level
        # expression will be
        while child_idx <= length(ex.args)
            e = ex.args[child_idx]
            e isa LineNumberNode || break
            iter.current_loc = e
            child_idx += 1
        end
    end
    push!(iter.todo, (ex, is_module_body, child_idx, outermost_expr))
end

function CompilerFrontend.lower_step(iter::FlispLoweringIterator, mod::Module, get_world)
    if isempty(iter.todo)
        return nothing
    end

    top_ex, is_module_body, child_idx, outermost_expr = pop!(iter.todo)
    current_loc = iter.current_loc

    if child_idx > 0
        if child_idx <= length(top_ex.args)
            ex = top_ex.args[child_idx]
            _push_next_expr!(iter, top_ex, is_module_body, child_idx+1, outermost_expr)
        elseif is_module_body
            return EndModule()
        elseif isempty(top_ex.args) || last(top_ex.args) isa LineNumberNode
            # Expr(:toplevel) evaluates to nothing in some special cases
            return ToplevelExpression(nothing, iter.current_loc)
        else
            # End of toplevel - continue to next statement
            return CompilerFrontend.lower_step(iter, mod, get_world)
        end
    else
        ex = top_ex
    end
    if outermost_expr && !isnothing(iter.mapexpr)
        ex = iter.mapexpr(ex)
    end
    if !isnothing(iter.logexpr)
        iter.logexpr(iter.current_loc, ex)
    end

    h = ex isa Expr ? ex.head : nothing
    if h === :toplevel
        if !iter.enter_toplevel
            return ToplevelExpression(ex, current_loc)
        end
        _push_next_expr!(iter, ex, false, 0)
        return CompilerFrontend.lower_step(iter, mod, get_world)
    elseif h === :module
        if !iter.enter_toplevel
            return ToplevelExpression(ex, current_loc)
        end
        if length(ex.args) == 3
            syntax_version = nothing
            std_defs, newmod_name, body = ex.args
        elseif length(ex.args) == 4
            syntax_version, std_defs, newmod_name, body = ex.args
        else
            error("syntax: malformed module expression")
        end
        std_defs = std_defs === true
        if !(newmod_name isa Symbol)
            throw(TypeError(:module, "", Symbol, newmod_name))
        end
        if !(body isa Expr && body.head == :block)
            error("syntax: malformed module expression")
        end
        loc = length(body.args) >= 1 && body.args[1] isa LineNumberNode ?
              body.args[1] : current_loc
        _push_next_expr!(iter, body, true, 1)
        return BeginModule(newmod_name, syntax_version, std_defs, loc)
    elseif !(ex isa Expr) || h === :export || h === :public || h === :error ||
                             h === :incomplete || h === :method || h === :thunk || h === :line
        # Special cases which are excluded from `fl_lower` by the C
        # implementation of `Core.eval` or not lowered by
        # `toplevel-only-expr?`. Should be kept in sync with
        # `jl_needs_lowering`.
        return ToplevelExpression(ex, current_loc)
    else
        file, line = CompilerFrontend.unpack_location(current_loc)
        lowered = fl_lower(ex, mod, file, line, get_world(), iter.warn)[1]
        if lowered isa Expr && lowered.head == :thunk
            return lowered.args[1]
        else
            # May get here when macros expand to non-thunks
            _push_next_expr!(iter, lowered, false, 0)
            return CompilerFrontend.lower_step(iter, mod, get_world)
        end
    end
end

CompilerFrontend.location(iter::FlispLoweringIterator) = iter.current_loc

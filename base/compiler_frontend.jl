# This file is a part of Julia. License is MIT: https://julialang.org/license
baremodule CompilerFrontend

# Parsing
export AbstractCompilerFrontend, parsecode, syntaxtree, checkparse
# Lowering
export TopLevelCodeIterator, BeginModule, EndModule, LoweredValue, lower_init, lower_step

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

# In simple cases flisp lowering decides to produce a Julia value rather than a
# `CodeInfo` with a trivial `return val` inside. `LoweredValue` accommodates
# this but we may want to remove it in future to avoid this special case.
struct LoweredValue
    val
    location # Currently LineNumberNode
end

# Return a subtype of `TopLevelCodeIterator` which may be passed to `lower_step`
function lower_init
end

# Returns one of
# * `CodeInfo`    - top level thunk to evaluate
# * `BeginModule` - start a new module (must be paired with EndModule later in iteration)
# * `EndModule`   - finish current module
# * `LoweredValue` - Alternative for trivial `CodeInfo`s (may be removed in future)
# * `Nothing`     - iteration is finished
function lower_step
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

function simple_eval(mod::Module, thunk::CodeInfo)
    @ccall jl_eval_thunk(mod::Any, thunk::Any)::Any
end

function simple_eval(mod::Module, v::LoweredValue)
    return v.val
end

# Shim in case we want extend the allowed types of newmod.location
module_loc(loc::LineNumberNode) = (something(loc.file, :none), loc.line)

function simple_eval(mod::Module, newmod::BeginModule)
    file, line = module_loc(newmod.location)
    @ccall jl_begin_new_module(mod::Module, newmod.name::Symbol,
                               newmod.syntax_version::Any,
                               newmod.standard_defs::Cint,
                               file::Cstring, line::Cint)::Module
end

function simple_eval(mod::Module, ::EndModule)
    @ccall jl_end_new_module(mod::Module)::Cvoid
    return mod
end

function simple_eval(mod::Module, iter::TopLevelCodeIterator)
    modules = Module[mod]
    result = nothing
    while true
        thunk = lower_step(iter, last(modules), Base.get_world_counter())
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
end

function include_string(frontend::AbstractCompilerFrontend, mod::Module, code::AbstractString;
                        filename::AbstractString="string", first_line=1, opts...)
    parse_result, _ = parsecode(frontend, :all, code, 1; filename, first_line=first_line)
    checkparse(frontend, parse_result)
    ex = syntaxtree(frontend, parse_result)
    eval(frontend, mod, ex; opts...)
end

function eval(frontend::AbstractCompilerFrontend, mod::Module, ex; opts...)
    iter = lower_init(frontend, mod, ex; opts...)
    simple_eval(mod, iter)
end

end # module CompilerFrontend

# TODO
# # Parsing entry point for Julia C runtime code
# function _parsecode(code_ptr::Ptr{UInt8}, code_len::Int, offset::Int, filename::String, first_line::Int, rule::Symbol)
#     code = ccall(:jl_pchar_to_string, Any, (Ptr{UInt8}, UInt #=Csize_t=#), p, len)::String
#     parse_result, index = parsecode(_compiler_frontend, rule, code, offset+1; filename, first_line)
#     ex = syntaxtree(Expr, parse_result)
#     return svec(ex, index-1)
# end

#-------------------------------------------------------------------------------
# TODO:
# Global eval and include
#
# eval(mod::Module, ex; opts...) = eval(_compiler_frontend, mod, ex; opts...)
#
# function include_string(mod::Module, code::AbstractString; opts...)
#     include_string(_compiler_frontend, mod, code; opts...)
# end

# TODO: Meta integration...
#
# module _Meta
#
# function parseall
#     ...
# end

#-------------------------------------------------------------------------------
using .CompilerFrontend

# Julia's builtin flisp-based compiler frontend

# Call Julia's builtin flisp-based parser. `offset` is 0-based offset into the
# byte buffer or string.
function fl_parse(text::Union{Core.SimpleVector,String},
                  filename::String, lineno, offset, options)
    if text isa Core.SimpleVector
        # Will be generated by C entry points jl_parse_string etc
        text, text_len = text
    else
        text_len = sizeof(text)
    end
    ccall(:jl_fl_parse, Any, (Ptr{UInt8}, Csize_t, Any, Csize_t, Csize_t, Any),
          text, text_len, filename, lineno, offset, options)
end

function fl_parse(text::AbstractString, filename::AbstractString, lineno, offset, options)
    fl_parse(String(text), String(filename), lineno, offset, options)
end

function fl_lower(ex, mod::Module, filename::Union{String,Ptr{UInt8}}="none",
                  lineno=0, world::Unsigned=typemax(Csize_t), warn::Bool=false)
    warn = warn ? 1 : 0
    ccall(:jl_fl_lower, Any, (Any, Any, Ptr{UInt8}, Csize_t, Csize_t, Cint),
          ex, mod, filename, lineno, world, warn)
end


struct FlispCompilerFrontend <: AbstractCompilerFrontend
end

function CompilerFrontend.parsecode(frontend::FlispCompilerFrontend, rule::Symbol, code, first_index;
                         filename::String="none", first_line::Int=1)
    fl_parse(code, filename, first_line, first_index-1, rule)
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

mutable struct FlispLoweringIterator <: TopLevelCodeIterator
    current_loc::LineNumberNode
    do_warn::Bool
    todo::Vector{Tuple{Any, Bool, Int, Int}}
    mapexpr::Any
end

function CompilerFrontend.lower_init(::FlispCompilerFrontend, mod::Module, ex;
                                     mapexpr=nothing, filename="none", first_line=0, warn=false, opts...)
    FlispLoweringIterator(LineNumberNode(first_line, filename), warn, [(ex, false, 0, 0)], mapexpr)
end

function CompilerFrontend.lower_step(iter::FlispLoweringIterator, mod::Module, macro_world)
    if isempty(iter.todo)
        return nothing
    end

    top_ex, is_module_body, child_idx, module_depth = pop!(iter.todo)

    local ex
    if child_idx > 0
        while child_idx <= length(top_ex.args)
            e = top_ex.args[child_idx]
            child_idx += 1
            if e isa LineNumberNode
                iter.current_loc = e
                if !is_module_body
                    ex = nothing
                end
            else
                ex = e
                break
            end
        end
        if !@isdefined(ex)
            if is_module_body
                return EndModule()
            else
                return lower_step(iter, mod, macro_world)
            end
        end
        push!(iter.todo, (top_ex, is_module_body, child_idx, module_depth))
    else
        ex = top_ex
    end

    h = ex isa Expr ? ex.head : nothing
    if !(h in (:toplevel, :module))
        if module_depth == 0 && !isnothing(iter.mapexpr)
            ex = iter.mapexpr(ex)
        end
        # Expand macros so that we may consume :toplevel and :module
        # expressions from macro expansions used at top level.
        ex = macroexpand(mod, ex)
        h = ex isa Expr ? ex.head : nothing
    end
    if h == :toplevel
        if length(ex.args) == 0
            return LoweredValue(nothing, iter.current_loc)
        end
        push!(iter.todo, (ex, false, 1, module_depth))
        return lower_step(iter, mod, macro_world)
    elseif h == :module
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
              body.args[1] : iter.current_loc
        push!(iter.todo, (body, true, 1, module_depth + 1))
        return BeginModule(newmod_name, syntax_version, std_defs, loc)
    elseif ex isa LineNumberNode
        # LineNumberNode in value position lowers to `nothing`. (fl_lower
        # produces a line number node which then `eval()`'s to nothing. But
        # this seems incorrect and instead we return nothing directly.)
        return LoweredValue(nothing, ex)
    else
        # Non macro expansion parts of lowering
        do_warn = true # TODO?
        thunk = fl_lower(ex, mod, string(iter.current_loc.file),
                         iter.current_loc.line, macro_world, do_warn)[1]
        if thunk isa Expr && thunk.head == :thunk
            return thunk.args[1]
        else
            # For trivial expressions, flisp lowering decides a CodeInfo is not
            # required and returns the value itself.
            return LoweredValue(thunk, iter.current_loc)
        end
    end
end


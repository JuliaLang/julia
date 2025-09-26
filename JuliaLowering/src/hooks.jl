#-------------------------------------------------------------------------------
# Experimental functionality that should be moved into Base / Core when it
# seems ready

baremodule _Core

module CompilerFrontend

# Parsing
export AbstractCompilerFrontend, parsecode, syntaxtree, checkparse
# Lowering
export TopLevelCodeIterator, BeginModule, EndModule, LoweredValue, lower_init, lower_step


using Base

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

TODO: Add support for compiler warnings and other diagnostics here?
"""
function checkparse
end

# Incremental lowering API which can manage toplevel and module expressions via
# iteration. The iterator API here is oddly bespoke because `eval()` and
# lowering must pass parameters back and forth. (Lowering to inform eval about
# modules begin/end and eval to create the actual module object and pass it back.)
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
# * `Nothing`     - iteration is finished
function lower_step
end

end # module CompilerFrontend

using .CompilerFrontend

using Core: CodeInfo, svec

#-------------------------------------------------------------------------------
# Default compiler frontend
_compiler_frontend = nothing

function _set_compiler_frontend!(frontend)
    global _compiler_frontend
    old = _compiler_frontend
    _compiler_frontend = frontend
    return old
end

# Parsing entry point for Julia C runtime code
function _parsecode(code_ptr::Ptr{UInt8}, code_len::Int, offset::Int, filename::String, first_line::Int, rule::Symbol)
    code = ccall(:jl_pchar_to_string, Any, (Ptr{UInt8}, UInt #=Csize_t=#), p, len)::String
    parse_result, index = parsecode(_compiler_frontend, rule, code, offset+1; filename, first_line)
    ex = syntaxtree(Expr, parse_result)
    return svec(ex, index-1)
end

#-------------------------------------------------------------------------------
# eval implementation

eval(mod::Module, ex; opts...) = eval(_compiler_frontend, mod, ex; opts...)

function eval(frontend::AbstractCompilerFrontend, mod::Module, ex; mapexpr=nothing, opts...)
    iter = lower_init(_compiler_frontend, mod, ex, mapexpr; opts...)
    simple_eval(mod, iter)
end

# Barebones `eval()` based on top level iteration API
function simple_eval(mod::Module, thunk::CodeInfo)
    # TODO: @ccall jl_eval_thunk instead?
    Core.eval(mod, Expr(:thunk, thunk))
end

function simple_eval(mod::Module, v::LoweredValue)
    return v.val
end

# Shim in case we want extend the allowed types of newmod.location
_module_loc(loc::LineNumberNode) = (something(loc.file, :none), loc.line)

# Need to figure these types out for bootstrap
import Base: Base, VERSION, @v_str, >=, Cint, Cstring, Cvoid, @ccall, @assert, push!, pop!

function simple_eval(mod::Module, newmod::BeginModule)
    file, line = _module_loc(newmod.location)
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
            @assert isempty(modules)
            return result
        end
        result = simple_eval(mod, thunk)
        if thunk isa BeginModule
            push!(modules, result::Module)
        elseif thunk isa EndModule
            pop!(modules)
        end
    end
end

#-------------------------------------------------------------------------------
function include_string(frontend::AbstractCompilerFrontend, mod::Module, code::AbstractString;
                        filename::AbstractString="string", mapexpr=nothing, opts...)
    parse_result, _ = parsecode(frontend, :all, code, 1; filename, first_line=1)
    checkparse(frontend, parse_result)
    ex = syntaxtree(frontend, parse_result)
    eval(mod, ex; mapexpr=mapexpr, opts...)
end

function include_string(mod::Module, code::AbstractString; opts...)
    include_string(_compiler_frontend, mod, code; opts...)
end

end # module _Core


baremodule _Base

using Base

using .._Core

# TODO: Meta integration...
#
# module _Meta
#
# function parseall
#     ...
# end

#-------------------------------------------------------------------------------
using ._Core.CompilerFrontend

# Julia's builtin flisp-based compiler frontend
struct FlispCompilerFrontend <: AbstractCompilerFrontend
end

function fl_parse(code::String,
                  filename::String, first_line, offset, options)
    ccall(:jl_fl_parse, Any, (Ptr{UInt8}, Csize_t, Any, Csize_t, Csize_t, Any),
          code, sizeof(code), filename, first_line, offset, options)
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
        throw(Meta.ParseError(ex))
    end
    return
end

mutable struct FlispLoweringIterator <: _Core.TopLevelCodeIterator
    current_loc::LineNumberNode
    do_warn::Bool
    todo::Vector{Tuple{Any, Bool, Int}}
    mapexpr::Any
end

function CompilerFrontend.lower_init(::FlispCompilerFrontend, mod::Module, ex, mapexpr;
                                      filename="none", first_line=0, warn=false, opts...)
    FlispLoweringIterator(LineNumberNode(first_line, filename), warn, [(mod, ex, false, 0)], mapexpr)
end

function fl_lower(ex, mod::Module, filename::Union{String,Ptr{UInt8}}="none",
                  first_line=0, world::Unsigned=typemax(Csize_t), warn::Bool=false)
    warn = warn ? 1 : 0
    ccall(:jl_fl_lower, Any, (Any, Any, Ptr{UInt8}, Csize_t, Csize_t, Cint),
          ex, mod, filename, first_line, world, warn)
end

function CompilerFrontend.lower_step(iter::FlispLoweringIterator, mod::Module, macro_world)
    if isempty(iter.todo)
        return nothing
    end

    ex, is_module_body, child_idx = pop!(iter.todo)

    if child_idx > 0
        next_child = child_idx + 1
        if child_idx <= length(ex.args)
            push!(iter.todo, (ex, is_module_body, next_child))
            c = ex.args[child_idx]
            if c isa LineNumberNode && next_child <= length(ex.args)
                iter.current_loc = c
                return lower_step(iter, mod, macro_world)
            else
                ex = c
            end
        else
            if is_module_body
                return EndModule()
            else
                return lower_step(iter, mod, macro_world)
            end
        end
    end

    h = ex isa Expr ? ex.head : :none
    if !(h in (:toplevel, :module))
        if !is_module_body && !isnothing(iter.mapexpr)
            ex = iter.mapexpr(ex)
        end
        # Expand macros so that we may consume :toplevel from macro expansions
        # used at top level.
        ex = macroexpand(mod, ex)
        h = ex isa Expr ? ex.head : :none
    end
    if h == :toplevel
        push!(iter.todo, (ex, false, 1))
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
        loc = length(body.args) > 1 && body.args[1] isa LineNumberNode ?
              body.args[1] : iter.current_loc
        push!(iter.todo, (body, true, 1))
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

#-------------------------------------------------------------------------------
# Current default frontend: JuliaSyntax for parsing plus flisp lowering
# implementation. (Possibly can go into JuliaSyntax?)

using JuliaSyntax

struct DefaultCompilerFrontend <: AbstractCompilerFrontend
end

struct JuliaSyntaxParseResult
    stream::JuliaSyntax.ParseStream
    rule::Symbol
    filename::String
    first_line::Int
end

function CompilerFrontend.parsecode(::DefaultCompilerFrontend, rule::Symbol, code::AbstractString,
                                    first_index::Integer; filename="none", first_line=1)
    stream = JuliaSyntax.ParseStream(code, first_index, version=VERSION)
    JuliaSyntax.parse!(stream; rule=rule, incremental=true)
    next_byte = JuliaSyntax.last_byte(stream) + 1
    return (JuliaSyntaxParseResult(stream, rule, filename, first_line), next_byte)
end

function CompilerFrontend.syntaxtree(frontend::DefaultCompilerFrontend,
                                     res::JuliaSyntaxParseResult)
    syntaxtree(frontend, Expr, res)
end

function CompilerFrontend.syntaxtree(::AbstractCompilerFrontend, ::Type{Expr},
                                     res::JuliaSyntaxParseResult)
    stream = res.stream
    ex = JuliaSyntax.all_trivia(stream) ? nothing :
         JuliaSyntax.build_base_compat_expr(stream, res.rule;
                                            filename=res.filename, first_line=res.first_line)
    return ex
end

function CompilerFrontend.syntaxtree(::AbstractCompilerFrontend, ::Type{T},
                                     res::JuliaSyntaxParseResult) where {T}
    stream = res.stream
    ex = JuliaSyntax.all_trivia(stream) ? nothing :
        JuliaSyntax.build_tree(T, stream; filename=res.filename, first_line=res.first_line)
    return ex
end

function CompilerFrontend.checkparse(::AbstractCompilerFrontend, res::JuliaSyntaxParseResult;
                                     do_warn=false)
    stream = res.stream
    if JuliaSyntax.any_error(stream)
        throw(JuliaSyntax.ParseError(stream; filename=res.filename, first_line=res.first_line))
    end
    # TODO: Show warnings to logger instead of stdout
    JuliaSyntax.show_diagnostics(stdout, stream)
    nothing
end

function CompilerFrontend.lower_init(::DefaultCompilerFrontend, mod::Module, ex, mapexpr;
                          filename="none", first_line=0, warn=false, opts...)
    FlispLoweringIterator(LineNumberNode(first_line, filename), warn, [(mod, ex, false, 0)], mapexpr)
end

#-------------------------------------------------------------------------------

function include_string(mapexpr::Function, mod::Module,
                        code::AbstractString, filename::AbstractString="string";
                        opts...)
    # `identity` is not defined in Core - need to special case it here if we
    # want to elide Expr conversion in some cases.
    _Core.include_string(mod, code;
                         mapexpr=(mapexpr===identity ? nothing : mapexpr),
                         filename=filename,
                         opts...)
end

function include_string(mod::Module, code::AbstractString, filename::AbstractString="string";
                        opts...)
    include_string(identity, mod, code, filename; opts...)
end

"""
    include([mapexpr::Function], mod::Module, path::AbstractString)

Evaluate the contents of the input source file in the global scope of module
`mod`. Every module (except those defined with baremodule) has its own
definition of `include()` omitting the `mod` argument, which evaluates the file
in that module. Returns the result of the last evaluated expression of the
input file. During including, a task-local include path is set to the directory
containing the file. Nested calls to include will search relative to that path.
This function is typically used to load source interactively, or to combine
files in packages that are broken into multiple source files.
"""
function include(mapexpr::Function, mod::Module, path::AbstractString)
    path, prev = Base._include_dependency(mod, path)
    code = read(path, String)
    tls = task_local_storage()
    tls[:SOURCE_PATH] = path
    try
        return include_string(mapexpr, mod, code, path)
    finally
        if prev === nothing
            delete!(tls, :SOURCE_PATH)
        else
            tls[:SOURCE_PATH] = prev
        end
    end
end

function include(mod::Module, path::AbstractString)
    include(identity, mod, path)
end

end # module _Base


#-------------------------------------------------------------------------------
using ._Core.CompilerFrontend
using ._Base: JuliaSyntaxParseResult

struct JuliaLoweringFrontend <: AbstractCompilerFrontend
    expr_compat_mode::Bool  # Default compat mode
    # world::UInt # TODO: fixed world age for frontend
end

function CompilerFrontend.parsecode(::JuliaLoweringFrontend, rule::Symbol, code::AbstractString,
                                    first_index::Integer; filename="none", first_line=1)
    stream = JuliaSyntax.ParseStream(code, first_index, version=VERSION)
    JuliaSyntax.parse!(stream; rule=rule, incremental=true)
    next_byte = JuliaSyntax.last_byte(stream) + 1
    return (JuliaSyntaxParseResult(stream, rule, filename, first_line), next_byte)
end

function CompilerFrontend.syntaxtree(frontend::JuliaLoweringFrontend, parse_result::JuliaSyntaxParseResult)
    syntaxtree(frontend, SyntaxTree, parse_result)
end

struct LoweringIterator <: TopLevelCodeIterator
    # frontend::JuliaLoweringFrontend  # TODO: world age?
    ctx::MacroExpansionContext
    todo::Vector{Tuple{SyntaxTree, Bool, Int}}
    mapexpr::Any
end

function CompilerFrontend.lower_init(frontend::JuliaLoweringFrontend, mod::Module, ex, mapexpr;
                                     expr_compat_mode::Bool=frontend.expr_compat_mode)
    if !(ex isa SyntaxTree)
        ex = expr_to_syntaxtree(ex)
    else
        # TODO: Copy `ex`? We don't want the underlying graph mutated :-(
    end
    graph = ensure_macro_attributes(syntax_graph(ex))
    dummy_world = zero(UInt)
    ctx = MacroExpansionContext(graph, Bindings(), ScopeLayer[], LayerId[],
                                expr_compat_mode, dummy_world)
    push_layer!(ctx, mod, false)
    ex = reparent(ctx, ex)
    LoweringIterator(ctx, [(ex, false, 0)], mapexpr)
end

function CompilerFrontend.lower_step(iter::LoweringIterator, mod::Module, macro_world)
    if isempty(iter.todo)
        return nothing
    end

    top_ex, is_module_body, child_idx = pop!(iter.todo)
    if child_idx > 0
        if child_idx == 1 && is_module_body
            push_layer!(iter.ctx, mod, false)
        end
        if child_idx <= numchildren(top_ex)
            # Lower one child of toplevel or module
            push!(iter.todo, (top_ex, is_module_body, child_idx + 1))
            ex = top_ex[child_idx]
        else
            # Finish toplevel or module
            if is_module_body
                pop_layer!(iter.ctx)
                return EndModule()
            else
                return lower_step(iter, mod, macro_world)
            end
        end
    else
        ex = top_ex
    end

    k = kind(ex)
    if !(k in KSet"toplevel module")
        if !is_module_body && !isnothing(iter.mapexpr)
            # TODO: `mapexpr` is a pretty niche tool and in principle could be
            # implemented more generally on top of expression iteration if we
            # added an option to iterate without macro expansion.
            ex = iter.ctx.expr_compat_mode ?
                 expr_to_syntaxtree(iter.ctx, iter.mapexpr(Expr(ex))) :
                 iter.mapexpr(ex)
        end
        c = iter.ctx
        # Copy context in order to set macro_world
        ctx1 = MacroExpansionContext(c.graph, c.bindings, c.scope_layers,
                                     c.scope_layer_stack, c.expr_compat_mode, macro_world)
        ex = expand_forms_1(ctx1, ex)
        k = kind(ex)
    end
    if k == K"toplevel"
        push!(iter.todo, (ex, false, 1))
        return lower_step(iter, mod, macro_world)
    elseif k == K"module"
        # TODO: Fix to always expect VERSION here.
        name_or_version = ex[1]
        syntax_version = nothing
        if kind(name_or_version) == K"VERSION"
            syntax_version = name_or_version.value
            name = ex[2]
        else
            name = name_or_version
        end
        if kind(name) != K"Identifier"
            throw(LoweringError(name, "Expected module name"))
        end
        newmod_name = Symbol(name.name_val)
        body = ex[end]
        if kind(body) != K"block"
            throw(LoweringError(body, "Expected block in module body"))
        end
        std_defs = !has_flags(ex, JuliaSyntax.BARE_MODULE_FLAG)
        loc = source_location(LineNumberNode, ex)
        push!(iter.todo, (body, true, 1))
        return BeginModule(newmod_name, syntax_version, std_defs, loc)
    else
        # Non macro expansion parts of lowering
        ctx2, ex2 = expand_forms_2(ctx1, ex)
        ctx3, ex3 = resolve_scopes(ctx2, ex2)
        ctx4, ex4 = convert_closures(ctx3, ex3)
        ctx5, ex5 = linearize_ir(ctx4, ex4)
        thunk = to_lowered_expr(ex5)
        return thunk.args[1] # TODO: Remove Expr(:thunk) wrapping in to_lowered_expr?
    end
end


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Simple lowering hook. Can be removed once we complete the frontend API above.
"""
Becomes `Core._lower()` upon activating JuliaLowering.

Returns an svec with the lowered code (usually expr) as its first element, and
(until integration is less experimental) whatever we want after it
"""
function core_lowering_hook(@nospecialize(code), mod::Module,
                            file="none", line=0, world=typemax(Csize_t), warn=false)
    if !(code isa SyntaxTree || code isa Expr)
        # e.g. LineNumberNode, integer...
        return Core.svec(code)
    end

    # TODO: fix in base
    file = file isa Ptr{UInt8} ? unsafe_string(file) : file
    line = !(line isa Int) ? Int(line) : line

    local st0, st1 = nothing, nothing
    try
        st0 = code isa Expr ? expr_to_syntaxtree(code, LineNumberNode(line, file)) : code
        if kind(st0) in KSet"toplevel module"
            return Core.svec(code)
        elseif kind(st0) === K"doc" && numchildren(st0) >= 2 && kind(st0[2]) === K"module"
            # TODO: this ignores module docstrings for now
            return Core.svec(Expr(st0[2]))
        end
        ctx1, st1 = expand_forms_1(  mod,  st0, true, world)
        ctx2, st2 = expand_forms_2(  ctx1, st1)
        ctx3, st3 = resolve_scopes(  ctx2, st2)
        ctx4, st4 = convert_closures(ctx3, st3)
        ctx5, st5 = linearize_ir(    ctx4, st4)
        ex = to_lowered_expr(st5)
        return Core.svec(ex, st5, ctx5)
    catch exc
        @info("JuliaLowering threw given input:", code=code, st0=st0, st1=st1, file=file, line=line, mod=mod)
        rethrow(exc)

        # TODO: Re-enable flisp fallback once we're done collecting errors
        # @error("JuliaLowering failed — falling back to flisp!",
        #        exception=(exc,catch_backtrace()),
        #        code=code, file=file, line=line, mod=mod)
        # return Base.fl_lower(code, mod, file, line, world, warn)
    end
end

const _has_v1_13_hooks = isdefined(Core, :_lower)

function activate!(enable=true)
    if !_has_v1_13_hooks
        error("Cannot use JuliaLowering without `Core._lower` binding or in $VERSION < 1.13")
    end

    if enable
        Core._setlowerer!(core_lowering_hook)
    else
        Core._setlowerer!(Base.fl_lower)
    end
end

#-------------------------------------------------------------------------------

_Core._set_compiler_frontend!(JuliaLoweringFrontend(false))

# Pull implementations from _Base/_Core into JuliaLowering for now
# (Assumes frontend is in _Core not Core.)
@fzone "JL: eval" function eval(mod::Module, ex; opts...)
    _Core.eval(mod, ex; opts...)
end

const include = _Base.include
const include_string = _Base.include_string

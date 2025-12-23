#-------------------------------------------------------------------------------
# Our version of eval - should be upstreamed though?
@fzone "JL: eval" function eval(mod::Module, ex::SyntaxTree; mapexpr=nothing, opts...)
    iter = lower_init(mod, ex, mapexpr; opts...)
    modules = Module[mod]
    result = nothing
    while true
        thunk = lower_step(iter, modules[end], Base.get_world_counter())::Core.SimpleVector
        type = thunk[1]::Symbol
        if type == :done
            break
        elseif type == :begin_module
            filename = something(thunk[5].file, :none)
            m = @ccall jl_begin_new_module(
                modules[end]::Any, thunk[3]::Symbol, thunk[2]::Any, thunk[4]::Cint,
                filename::Cstring, thunk[5].line::Cint)::Module
            push!(modules, m)
        elseif type == :end_module
            m = pop!(modules)
            @ccall jl_end_new_module(m::Module)::Cvoid
            result = m
        else
            @assert type == :thunk
            result = Core.eval(modules[end], thunk[2])
        end
    end
    @assert length(modules) === 1
    return result
end

# Version of eval() taking `Expr` (or Expr tree leaves of any type)
function eval(mod::Module, ex; opts...)
    eval(mod, expr_to_syntaxtree(ex); opts...)
end

"""
    include(mod::Module, path::AbstractString)

Evaluate the contents of the input source file in the global scope of module
`mod`. Every module (except those defined with baremodule) has its own
definition of `include()` omitting the `mod` argument, which evaluates the file
in that module. Returns the result of the last evaluated expression of the
input file. During including, a task-local include path is set to the directory
containing the file. Nested calls to include will search relative to that path.
This function is typically used to load source interactively, or to combine
files in packages that are broken into multiple source files.
"""
function include(mod::Module, path::AbstractString)
    path, prev = Base._include_dependency(mod, path)
    code = read(path, String)
    tls = task_local_storage()
    tls[:SOURCE_PATH] = path
    try
        return include_string(mod, code, path)
    finally
        if prev === nothing
            delete!(tls, :SOURCE_PATH)
        else
            tls[:SOURCE_PATH] = prev
        end
    end
end

"""
    include_string(mod::Module, code::AbstractString, filename::AbstractString="string")

Like `include`, except reads code from the given string rather than from a file.
"""
function include_string(mapexpr::Function, mod::Module,
                        code::AbstractString, filename::AbstractString="string";
                        opts...)
    ex = parseall(SyntaxTree, code; filename=filename)
    eval(mod, ex; mapexpr=(mapexpr === identity ? nothing : mapexpr), opts...)
end

function include_string(mod::Module, code::AbstractString, filename::AbstractString="string";
                        opts...)
    include_string(identity, mod, code, filename; opts...)
end


#-------------------------------------------------------------------------------
# Incremental lowering API which can manage toplevel and module expressions.
#
# This iteration API is oddly bespoke and arguably somewhat non-Julian for two
# reasons:
#
# * Lowering knows when new modules are required, and may request them with
#   `:begin_module`. However `eval()` generates those modules so they need to
#   be passed back into lowering. So we can't just use `Base.iterate()`. (Put a
#   different way, we have a situation which is suited to coroutines but we
#   don't want to use full Julia `Task`s for this.)
# * We might want to implement this `eval()` in Julia's C runtime code or early
#   in bootstrap. Hence using SimpleVector and Symbol as the return values of
#   `lower_step()`
#
# We might consider changing at least the second of these choices, depending on
# how we end up putting this into Base.

struct LoweringIterator
    # frontend::JuliaLoweringFrontend  # TODO: fixed world age for frontend itself?
    ctx::MacroExpansionContext
    todo::Vector{Tuple{SyntaxTree, Bool, Int}}
    mapexpr::Any
end

function lower_init(mod::Module, ex::SyntaxTree, mapexpr;
                    expr_compat_mode::Bool=false)
    graph = ensure_macro_attributes(syntax_graph(ex))
    dummy_world = zero(UInt)
    ctx = MacroExpansionContext(graph, Bindings(), ScopeLayer[], LayerId[],
                                expr_compat_mode, dummy_world)
    push_layer!(ctx, mod, false)
    ex = reparent(ctx, ex)
    LoweringIterator(ctx, [(ex, false, 0)], mapexpr)
end

function lower_step(iter, mod, macro_world)
    if isempty(iter.todo)
        return Core.svec(:done)
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
                return Core.svec(:end_module)
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
        version = nothing
        if kind(name_or_version) == K"VERSION"
            version = name_or_version.value
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
        return Core.svec(:begin_module, version, newmod_name, std_defs, loc)
    else
        # Non macro expansion parts of lowering
        ctx2, ex2 = expand_forms_2(ctx1, ex)
        ctx3, ex3 = resolve_scopes(ctx2, ex2)
        ctx4, ex4 = convert_closures(ctx3, ex3)
        ctx5, ex5 = linearize_ir(ctx4, ex4)
        thunk = to_lowered_expr(ex5)
        return Core.svec(:thunk, thunk)
    end
end

#-------------------------------------------------------------------------------
# TODO: Write a parser hook here.  The input to `core_lowering_hook` should
# eventually be a (convertible to) SyntaxTree, but we need to make updates to
# the parsing API to include a parameter for AST type.
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

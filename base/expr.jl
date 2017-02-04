# This file is a part of Julia. License is MIT: http://julialang.org/license

## symbols ##

"""
    gensym([tag])

Generates a symbol which will not conflict with other variable names.
"""
gensym() = ccall(:jl_gensym, Ref{Symbol}, ())

gensym(s::String) = ccall(:jl_tagged_gensym, Ref{Symbol}, (Ptr{UInt8}, Int32), s, sizeof(s))

gensym(ss::String...) = map(gensym, ss)
gensym(s::Symbol) =
    ccall(:jl_tagged_gensym, Ref{Symbol}, (Ptr{UInt8}, Int32), s, ccall(:strlen, Csize_t, (Ptr{UInt8},), s))

"""
    @gensym

Generates a gensym symbol for a variable. For example, `@gensym x y` is transformed into
`x = gensym("x"); y = gensym("y")`.
"""
macro gensym(names...)
    blk = Expr(:block)
    for name in names
        push!(blk.args, :($(esc(name)) = gensym($(string(name)))))
    end
    push!(blk.args, :nothing)
    return blk
end

## expressions ##

copy(e::Expr) = (n = Expr(e.head);
                 n.args = copy_exprargs(e.args);
                 n.typ = e.typ;
                 n)

# copy parts of an AST that the compiler mutates
copy_exprs(x::Expr) = copy(x)
copy_exprs(x::ANY) = x
copy_exprargs(x::Array{Any,1}) = Any[copy_exprs(a) for a in x]

==(x::Expr, y::Expr) = x.head === y.head && isequal(x.args, y.args)
==(x::QuoteNode, y::QuoteNode) = isequal(x.value, y.value)

"""
    expand(x)

Takes the expression `x` and returns an equivalent expression in lowered form.
See also [`code_lowered`](@ref).
"""
expand(x::ANY) = ccall(:jl_expand, Any, (Any,), x)

"""
    macroexpand(x)

Takes the expression `x` and returns an equivalent expression with all macros removed (expanded).
"""
macroexpand(x::ANY) = ccall(:jl_macroexpand, Any, (Any,), x)

"""
    @macroexpand

Return equivalent expression with all macros removed (expanded).

There is a subtle difference between `@macroexpand` and `macroexpand` in that expansion takes place in
different contexts. This is best seen in the following example:

```jldoctest
julia> module M
           macro m()
               1
           end
           function f()
               (@macroexpand(@m), macroexpand(:(@m)))
           end
       end
M

julia> macro m()
           2
       end
@m (macro with 1 method)

julia> M.f()
(1, 2)
```
With `@macroexpand` the expression expands where `@macroexpand` appears in the code (module
`M` in the example). With `macroexpand` the expression expands in the current module where
the code was finally called (REPL in the example).
Note that when calling `macroexpand` or `@macroexpand` directly from the REPL, both of these contexts coincide, hence there is no difference.
"""
macro macroexpand(code)
    code_expanded = macroexpand(code)
    QuoteNode(code_expanded)
end

## misc syntax ##

"""
    eval([m::Module], expr::Expr)

Evaluate an expression in the given module and return the result. Every `Module` (except
those defined with `baremodule`) has its own 1-argument definition of `eval`, which
evaluates expressions in that module.
"""
Core.eval

"""
    @eval [mod,] ex

Evaluate an expression with values interpolated into it using `eval`.
If two arguments are provided, the first is the module to evaluate in.
"""
macro eval(ex)
    :(eval($(current_module()), $(Expr(:quote,ex))))
end
macro eval(mod, ex)
    :(eval($(esc(mod)), $(Expr(:quote,ex))))
end

"""
    @inline

Give a hint to the compiler that this function is worth inlining.

Small functions typically do not need the `@inline` annotation,
as the compiler does it automatically. By using `@inline` on bigger functions,
an extra nudge can be given to the compiler to inline it.
This is shown in the following example:
```julia
@inline function bigfunction(x)
    #=
        Function Definition
    =#
end
```
"""
macro inline(ex)
    esc(isa(ex, Expr) ? pushmeta!(ex, :inline) : ex)
end

"""
    @noinline

Prevents the compiler from inlining a function.

Small functions are typically inlined automatically.
By using `@noinline` on small functions, auto-inlining can be
prevented. This is shown in the following example:
```julia
@noinline function smallfunction(x)
    #=
        Function Definition
    =#
end
```
"""
macro noinline(ex)
    esc(isa(ex, Expr) ? pushmeta!(ex, :noinline) : ex)
end

macro pure(ex)
    esc(isa(ex, Expr) ? pushmeta!(ex, :pure) : ex)
end

"""
    @propagate_inbounds

Tells the compiler to inline a function while retaining the caller's inbounds context.
"""
macro propagate_inbounds(ex)
    if isa(ex, Expr)
        pushmeta!(ex, :inline)
        pushmeta!(ex, :propagate_inbounds)
        esc(ex)
    else
        esc(ex)
    end
end

"""
    @polly

Tells the compiler to apply the polyhedral optimizer Polly to a function.
"""
macro polly(ex)
    esc(isa(ex, Expr) ? pushmeta!(ex, :polly) : ex)
end

## some macro utilities ##

function pushmeta!(ex::Expr, sym::Symbol, args::Any...)
    if isempty(args)
        tag = sym
    else
        tag = Expr(sym, args...)
    end

    inner = ex
    while inner.head == :macrocall
        inner = inner.args[end]::Expr
    end

    idx, exargs = findmeta(inner)
    if idx != 0
        push!(exargs[idx].args, tag)
    else
        body::Expr = inner.args[2]
        unshift!(body.args, Expr(:meta, tag))
    end
    ex
end

function popmeta!(body::Expr, sym::Symbol)
    body.head == :block || return false, []
    popmeta!(body.args, sym)
end
popmeta!(arg, sym) = (false, [])
function popmeta!(body::Array{Any,1}, sym::Symbol)
    idx, blockargs = findmeta_block(body, args -> findmetaarg(args,sym)!=0)
    if idx == 0
        return false, []
    end
    metaargs = blockargs[idx].args
    i = findmetaarg(blockargs[idx].args, sym)
    if i == 0
        return false, []
    end
    ret = isa(metaargs[i], Expr) ? (metaargs[i]::Expr).args : []
    deleteat!(metaargs, i)
    isempty(metaargs) && deleteat!(blockargs, idx)
    true, ret
end

# Find index of `sym` in a meta expression argument list, or 0.
function findmetaarg(metaargs, sym)
    for i = 1:length(metaargs)
        arg = metaargs[i]
        if (isa(arg, Symbol) && (arg::Symbol)    == sym) ||
           (isa(arg, Expr)   && (arg::Expr).head == sym)
            return i
        end
    end
    return 0
end

function is_short_function_def(ex)
    ex.head == :(=) || return false
    while length(ex.args) >= 1 && isa(ex.args[1], Expr)
        (ex.args[1].head == :call) && return true
        (ex.args[1].head == :where) || return false
        ex = ex.args[1]
    end
    return false
end

function findmeta(ex::Expr)
    if ex.head == :function || is_short_function_def(ex)
        body::Expr = ex.args[2]
        body.head == :block || error(body, " is not a block expression")
        return findmeta_block(ex.args)
    end
    error(ex, " is not a function expression")
end

findmeta(ex::Array{Any,1}) = findmeta_block(ex)

function findmeta_block(exargs, argsmatch=args->true)
    for i = 1:length(exargs)
        a = exargs[i]
        if isa(a, Expr)
            if (a::Expr).head == :meta && argsmatch((a::Expr).args)
                return i, exargs
            elseif (a::Expr).head == :block
                idx, exa = findmeta_block(a.args, argsmatch)
                if idx != 0
                    return idx, exa
                end
            end
        end
    end
    return 0, []
end

remove_linenums!(ex) = ex
function remove_linenums!(ex::Expr)
    filter!(x->!((isa(x,Expr) && x.head === :line) || isa(x,LineNumberNode)), ex.args)
    for subex in ex.args
        remove_linenums!(subex)
    end
    ex
end

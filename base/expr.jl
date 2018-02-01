# This file is a part of Julia. License is MIT: https://julialang.org/license

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
copy_exprs(@nospecialize(x)) = x
copy_exprargs(x::Array{Any,1}) = Any[copy_exprs(a) for a in x]

==(x::Expr, y::Expr) = x.head === y.head && isequal(x.args, y.args)
==(x::QuoteNode, y::QuoteNode) = isequal(x.value, y.value)

"""
    macroexpand(m::Module, x; recursive=true)

Take the expression `x` and return an equivalent expression with all macros removed (expanded)
for executing in module `m`.
The `recursive` keyword controls whether deeper levels of nested macros are also expanded.
This is demonstrated in the example below:
```julia-repl
julia> module M
           macro m1()
               42
           end
           macro m2()
               :(@m1())
           end
       end
M

julia> macroexpand(M, :(@m2()), recursive=true)
42

julia> macroexpand(M, :(@m2()), recursive=false)
:(#= REPL[16]:6 =# M.@m1)
```
"""
function macroexpand(m::Module, @nospecialize(x); recursive=true)
    if recursive
        ccall(:jl_macroexpand, Any, (Any, Any), x, m)
    else
        ccall(:jl_macroexpand1, Any, (Any, Any), x, m)
    end
end

"""
    @macroexpand

Return equivalent expression with all macros removed (expanded).

There are differences between `@macroexpand` and [`macroexpand`](@ref).

* While [`macroexpand`](@ref) takes a keyword argument `recursive`, `@macroexpand`
is always recursive. For a non recursive macro version, see [`@macroexpand1`](@ref).

* While [`macroexpand`](@ref) has an explicit `module` argument, `@macroexpand` always
expands with respect to the module in which it is called.
This is best seen in the following example:
```julia-repl
julia> module M
           macro m()
               1
           end
           function f()
               (@macroexpand(@m),
                macroexpand(M, :(@m)),
                macroexpand(Main, :(@m))
               )
           end
       end
M

julia> macro m()
           2
       end
@m (macro with 1 method)

julia> M.f()
(1, 1, 2)
```
With `@macroexpand` the expression expands where `@macroexpand` appears in the code (module `M` in the example).
With `macroexpand` the expression expands in the module given as the first argument.
"""
macro macroexpand(code)
    return :(macroexpand($__module__, $(QuoteNode(code)), recursive=true))
end


"""
    @macroexpand1

Non recursive version of [`@macroexpand`](@ref).
"""
macro macroexpand1(code)
    return :(macroexpand($__module__, $(QuoteNode(code)), recursive=false))
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
        pushfirst!(body.args, Expr(:meta, tag))
    end
    ex
end

popmeta!(body, sym) = _getmeta(body, sym, true)
peekmeta(body, sym) = _getmeta(body, sym, false)

function _getmeta(body::Expr, sym::Symbol, delete::Bool)
    body.head == :block || return false, []
    _getmeta(body.args, sym, delete)
end
_getmeta(arg, sym, delete::Bool) = (false, [])
function _getmeta(body::Array{Any,1}, sym::Symbol, delete::Bool)
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
    if delete
        deleteat!(metaargs, i)
        isempty(metaargs) && deleteat!(blockargs, idx)
    end
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
        (ex.args[1].head == :where || ex.args[1].head == :(::)) || return false
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
    if ex.head === :body || ex.head === :block || ex.head === :quote
        # remove line number expressions from metadata (not argument literal or inert) position
        filter!(ex.args) do x
            isa(x, Expr) && x.head === :line && return false
            isa(x, LineNumberNode) && return false
            return true
        end
    end
    for subex in ex.args
        remove_linenums!(subex)
    end
    return ex
end

macro generated()
    return Expr(:generated)
end

macro generated(f)
    if isa(f, Expr) && (f.head === :function || is_short_function_def(f))
        body = f.args[2]
        lno = body.args[1]
        return Expr(:escape,
                    Expr(f.head, f.args[1],
                         Expr(:block,
                              lno,
                              Expr(:if, Expr(:generated),
                                   body,
                                   Expr(:block,
                                        Expr(:meta, :generated_only),
                                        Expr(:return, nothing))))))
    else
        error("invalid syntax; @generated must be used with a function definition")
    end
end

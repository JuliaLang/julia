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

function copy(e::Expr)
    n = Expr(e.head)
    n.args = copy_exprargs(e.args)
    return n
end

# copy parts of an AST that the compiler mutates
copy_exprs(@nospecialize(x)) = x
copy_exprs(x::Expr) = copy(x)
function copy_exprs(x::PhiNode)
    new_values = Vector{Any}(undef, length(x.values))
    for i = 1:length(x.values)
        isassigned(x.values, i) || continue
        new_values[i] = copy_exprs(x.values[i])
    end
    return PhiNode(copy(x.edges), new_values)
end
function copy_exprs(x::PhiCNode)
    new_values = Vector{Any}(undef, length(x.values))
    for i = 1:length(x.values)
        isassigned(x.values, i) || continue
        new_values[i] = copy_exprs(x.values[i])
    end
    return PhiCNode(new_values)
end
copy_exprargs(x::Array{Any,1}) = Any[copy_exprs(x[i]) for i in 1:length(x)]

# create copies of the CodeInfo definition, and any mutable fields
function copy(c::CodeInfo)
    cnew = ccall(:jl_copy_code_info, Ref{CodeInfo}, (Any,), c)
    cnew.code = copy_exprargs(cnew.code)
    cnew.slotnames = copy(cnew.slotnames)
    cnew.slotflags = copy(cnew.slotflags)
    cnew.codelocs  = copy(cnew.codelocs)
    cnew.linetable = copy(cnew.linetable)
    cnew.ssaflags  = copy(cnew.ssaflags)
    cnew.edges     = cnew.edges === nothing ? nothing : copy(cnew.edges)
    ssavaluetypes  = cnew.ssavaluetypes
    ssavaluetypes isa Vector{Any} && (cnew.ssavaluetypes = copy(ssavaluetypes))
    return cnew
end


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
    Core.eval(m::Module, expr)

Evaluate an expression in the given module and return the result.
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

Give a hint to the compiler that it should not inline a function.

Small functions are typically inlined automatically.
By using `@noinline` on small functions, auto-inlining can be
prevented. This is shown in the following example:

```julia
@noinline function smallfunction(x)
    #=
        Function Definition
    =#
end

If the function is trivial (for example returning a constant) it might get inlined anyway.
```
"""
macro noinline(ex)
    esc(isa(ex, Expr) ? pushmeta!(ex, :noinline) : ex)
end

"""
    @pure ex
    @pure(ex)

`@pure` gives the compiler a hint for the definition of a pure function,
helping for type inference.

A pure function can only depend on immutable information.
This also means a `@pure` function cannot use any global mutable state, including
generic functions. Calls to generic functions depend on method tables which are
mutable global state.
Use with caution, incorrect `@pure` annotation of a function may introduce
hard to identify bugs. Double check for calls to generic functions.
This macro is intended for internal compiler use and may be subject to changes.
"""
macro pure(ex)
    esc(isa(ex, Expr) ? pushmeta!(ex, :pure) : ex)
end

"""
    @hide_in_stacktrace

Mark a function to be hidden from the user in stacktraces. `@hide_in_stacktrace`
currently implies `@noinline`, though this may be relaxed in the future.

See also [`stacktrace`](@ref).
"""
macro hide_in_stacktrace(ex)
    esc(ex isa Expr ? pushmeta!(pushmeta!(ex, :hide_in_stacktrace), :noinline) : ex)
end

"""
    @propagate_inbounds

Tells the compiler to inline a function while retaining the caller's inbounds context.
"""
macro propagate_inbounds(ex)
    if isa(ex, Expr)
        pushmeta!(ex, :inline)
        pushmeta!(ex, :propagate_inbounds)
    end
    esc(ex)
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
    while inner.head === :macrocall
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
    body.head === :block || return false, []
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
    ex.head === :(=) || return false
    while length(ex.args) >= 1 && isa(ex.args[1], Expr)
        (ex.args[1].head === :call) && return true
        (ex.args[1].head === :where || ex.args[1].head === :(::)) || return false
        ex = ex.args[1]
    end
    return false
end

function findmeta(ex::Expr)
    if ex.head === :function || is_short_function_def(ex) || ex.head === :->
        body::Expr = ex.args[2]
        body.head === :block || error(body, " is not a block expression")
        return findmeta_block(ex.args)
    end
    error(ex, " is not a function expression")
end

findmeta(ex::Array{Any,1}) = findmeta_block(ex)

function findmeta_block(exargs, argsmatch=args->true)
    for i = 1:length(exargs)
        a = exargs[i]
        if isa(a, Expr)
            if (a::Expr).head === :meta && argsmatch((a::Expr).args)
                return i, exargs
            elseif (a::Expr).head === :block
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
    if ex.head === :block || ex.head === :quote
        # remove line number expressions from metadata (not argument literal or inert) position
        filter!(ex.args) do x
            isa(x, Expr) && x.head === :line && return false
            isa(x, LineNumberNode) && return false
            return true
        end
    end
    for subex in ex.args
        subex isa Expr && remove_linenums!(subex)
    end
    return ex
end
function remove_linenums!(src::CodeInfo)
    src.codelocs .= 0
    length(src.linetable) > 1 && resize!(src.linetable, 1)
    return src
end

macro generated()
    return Expr(:generated)
end

"""
    @generated f
    @generated(f)
`@generated` is used to annotate a function which will be generated.
In the body of the generated function, only types of arguments can be read
(not the values). The function returns a quoted expression evaluated when the
function is called. The `@generated` macro should not be used on functions mutating
the global scope or depending on mutable elements.

See [Metaprogramming](@ref) for further details.

## Example:
```julia
julia> @generated function bar(x)
           if x <: Integer
               return :(x ^ 2)
           else
               return :(x)
           end
       end
bar (generic function with 1 method)

julia> bar(4)
16

julia> bar("baz")
"baz"
```
"""
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

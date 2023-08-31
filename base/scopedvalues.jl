# This file is a part of Julia. License is MIT: https://julialang.org/license

module ScopedValues

export ScopedValue, with, @with

"""
    ScopedValue(x)

Create a container that propagates values across dynamic scopes.
Use [`with`](@ref) to create and enter a new dynamic scope.

Values can only be set when entering a new dynamic scope,
and the value referred to will be constant during the
execution of a dynamic scope.

Dynamic scopes are propagated across tasks.

# Examples
```jldoctest
julia> const sval = ScopedValue(1);

julia> sval[]
1

julia> with(sval => 2) do
           sval[]
       end
2

julia> sval[]
1
```

!!! compat "Julia 1.11"
    Scoped values were introduced in Julia 1.11. In Julia 1.8+ a compatible
    implementation is available from the package ScopedValues.jl.
"""
mutable struct ScopedValue{T}
    const initial_value::T
end

Base.eltype(::Type{ScopedValue{T}}) where {T} = T

##
# Notes on the implementation.
# We want lookup to be unreasonably fast.
# - IDDict/Dict are ~10ns
# - ImmutableDict is faster up to about ~15 entries
# - ScopedValue are meant to be constant, Immutabilty
#   is thus a boon
# - If we were to use IDDict/Dict we would need to split
#   the cache portion and the value portion of the hash-table,
#   the value portion is read-only/write-once, but the cache version
#   would need a lock which makes ImmutableDict incredibly attractive.
#   We could also use task-local-storage, but that added about 12ns.
# - Values are GC'd when scopes become unreachable, one could use
#   a WeakKeyDict to also ensure that values get GC'd when ScopedValues
#   become unreachable.
# - Scopes are an inline implementation of an ImmutableDict, if we wanted
#   be really fancy we could use a CTrie or HAMT.

mutable struct Scope
    values::Base.PersistentDict{ScopedValue, Any}
end
function Scope(parent::Union{Nothing, Scope}, key::ScopedValue{T}, value) where T
    if parent === nothing
        values = Base.PersistentDict{ScopedValue, Any}()
    else
        values = parent.values
    end
    values = Base.PersistentDict(values, key=>convert(T, value))
    return Scope(values)
end

function Scope(scope, pairs::Pair{<:ScopedValue}...)
    for pair in pairs
        scope = Scope(scope, pair...)
    end
    return scope
end

"""
    current_scope()::Union{Nothing, Scope}

Return the current dynamic scope.
"""
current_scope() = current_task().scope::Union{Nothing, Scope}

function Base.show(io::IO, scope::Scope)
    print(io, Scope, "(")
    first = true
    for (key, value) in scope.values
        if first
            first = false
        else
            print(io, ", ")
        end
        print(io, typeof(key), "@")
        show(io, Base.objectid(key))
        print(io, " => ")
        show(IOContext(io, :typeinfo => eltype(key)), value)
    end
    print(io, ")")
end

function Base.getindex(val::ScopedValue{T})::T where T
    scope = current_scope()
    if scope === nothing
        return val.initial_value
    end
    @inline get(scope.values, val, val.initial_value)::T
end

function Base.show(io::IO, var::ScopedValue)
    print(io, ScopedValue)
    print(io, '{', eltype(var), '}')
    print(io, '(')
    show(IOContext(io, :typeinfo => eltype(var)), var[])
    print(io, ')')
end

"""
    with(f, (var::ScopedValue{T} => val::T)...)

Execute `f` in a new scope with `var` set to `val`.
"""
function with(f, pair::Pair{<:ScopedValue}, rest::Pair{<:ScopedValue}...)
    @nospecialize
    ct = Base.current_task()
    current_scope = ct.scope::Union{Nothing, Scope}
    scope = Scope(current_scope, pair, rest...)
    ct.scope = scope
    try
        return f()
    finally
        ct.scope = current_scope
    end
end

with(@nospecialize(f)) = f()

"""
    @with vars... expr

Macro version of `with(f, vars...)` but with `expr` instead of `f` function.
This is similar to using [`with`](@ref) with a `do` block, but avoids creating
a closure.
"""
macro with(exprs...)
    if length(exprs) > 1
        ex = last(exprs)
        exprs = exprs[1:end-1]
    elseif length(exprs) == 1
        ex = only(exprs)
        exprs = ()
    else
        error("@with expects at least one argument")
    end
    for expr in exprs
        if expr.head !== :call || first(expr.args) !== :(=>)
            error("@with expects arguments of the form `A => 2` got $expr")
        end
    end
    exprs = map(esc, exprs)
    ct = gensym(:ct)
    current_scope = gensym(:current_scope)
    body = Expr(:tryfinally, esc(ex), :($(ct).scope = $(current_scope)))
    quote
        $(ct) = $(Base.current_task)()
        $(current_scope) = $(ct).scope::$(Union{Nothing, Scope})
        $(ct).scope = $(Scope)($(current_scope), $(exprs...))
        $body
    end
end

end # module ScopedValues

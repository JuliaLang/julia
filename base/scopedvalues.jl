# This file is a part of Julia. License is MIT: https://julialang.org/license

module ScopedValues

export ScopedValue, scoped

"""
    ScopedValue(x)

Create a container that propagates values across dynamic scopes.
Use [`scoped`](@ref) to create and enter a new dynamic scope.

Values can only be set when entering a new dynamic scope,
and the value referred to will be constant during the
execution of a dynamic scope.

Dynamic scopes are propagated across tasks.

# Examples
```jldoctest
julia> const svar = ScopedValue(1);

julia> svar[]
1

julia> scoped(svar => 2) do
           svar[]
       end
2
```

!!! compat "Julia 1.11"
    Scoped values were introduced in Julia 1.11. In Julia 1.7+ a compatible
    implementation is available from the package ScopedValues.jl.
"""
mutable struct ScopedValue{T}
    const initial_value::T
    ScopedValue{T}(initial_value) where {T} = new{T}(initial_value)
end
ScopedValue(initial_value::T) where {T} = ScopedValue{T}(initial_value)

Base.eltype(::Type{ScopedValue{T}}) where {T} = T

import Base: ImmutableDict

##
# Notes on the implementation.
# We want lookup to be unreasonable fast.
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
#   be really fance we could use a CTrie or HAMT.

mutable struct Scope{T}
    const parent::Union{Nothing, Scope}
    const key::ScopedValue{T}
    const value::T
end
Scope(parent, key::ScopedValue{T}, value) where T =
    Scope(parent, key, convert(T, value))

current_scope() = current_task().scope::Union{Nothing, Scope}

function Base.show(io::IO, ::Scope)
    print(io, Scope)
end

function Base.getindex(var::ScopedValue{T})::T where T
    scope = current_scope()
    while scope !== nothing
        if scope.key === var
            return scope.value::T
        end
        scope = scope.parent
    end
    return var.initial_value
en

function Base.show(io::IO, var::ScopedValue)
    print(io, ScopedValue)
    print(io, '{', eltype(var), '}')
    print(io, '(')
    show(io, var[])
    print(io, ')')
end

"""
    scoped(f, (var::ScopedValue{T} => val::T)...)

Execute `f` in a new scope with `var` set to `val`.
"""
function scoped(f, pair::Pair{<:ScopedValue}, rest::Pair{<:ScopedValue}...)
    @nospecialize
    ct = Base.current_task()
    current_scope = ct.scope::Union{Nothing, Scope}
    scope = Scope(current_scope, pair...)
    for pair in rest
        scope = Scope(scope, pair...)
    end
    ct.scope = scope
    try
        return f()
    finally
        ct.scope = current_scope
    end
end

scoped(@nospecialize(f)) = f()

end # module ScopedValues

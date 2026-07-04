# This file is a part of Julia. License is MIT: https://julialang.org/license

# Early definitions of the dynamic-scope types used by `Core.current_scope()`.
# The user-facing API lives in the `ScopedValues` module (scopedvalues.jl, included
# much later in bootstrap); the types live here so that early Base code — in
# particular the cancellation machinery used by `condition.jl` and `task.jl` —
# can resolve scoped state during bootstrap.

"""
    AbstractScopedValue{T}

Abstract base type for scoped values that propagate values across
dynamic scopes. All scoped value types must extend this abstract type.

See also: [`ScopedValues.ScopedValue`](@ref), [`ScopedValues.LazyScopedValue`](@ref)

!!! compat "Julia 1.13"
    AbstractScopedValue requires Julia 1.13+.
"""
abstract type AbstractScopedValue{T} end

const ScopeStorage = PersistentDict{AbstractScopedValue, Any}

struct Scope
    values::ScopeStorage
end

Scope(scope::Scope) = scope

function Scope(parent::Union{Nothing, Scope}, key::AbstractScopedValue{T}, value) where T
    val = convert(T, value)
    if parent === nothing
        return Scope(ScopeStorage(key=>val))
    end
    return Scope(ScopeStorage(parent.values, key=>val))
end

function Scope(scope, pair::Pair{<:AbstractScopedValue})
    return Scope(scope, pair...)
end

function Scope(scope, pair1::Pair{<:AbstractScopedValue}, pair2::Pair{<:AbstractScopedValue}, pairs::Pair{<:AbstractScopedValue}...)
    # Unroll this loop through recursion to make sure that
    # our compiler optimization support works
    return Scope(Scope(scope, pair1...), pair2, pairs...)
end
Scope(::Nothing) = nothing

# This file is a part of Julia. License is MIT: https://julialang.org/license

module ScopedValues

export ScopedValue, scoped

"""
    ScopedValue(x)

Create a container that propagates values across scopes.
Use [`scoped`](@ref) to create and enter a new scope.

Values can only be set when entering a new scope,
and the value referred to will be constant during the
execution of a scope.

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
    This method requires at least Julia 1.11. In Julia 1.7+ this
    is available from the package ScopedValues.jl.
"""
mutable struct ScopedValue{T}
    const initial_value::T
    ScopedValue{T}(initial_value) where {T} = new{T}(initial_value)
end
ScopedValue(initial_value::T) where {T} = ScopedValue{T}(initial_value)

Base.eltype(::Type{ScopedValue{T}}) where {T} = T

mutable struct Scope
    const parent::Union{Nothing, Scope}
    # XXX: Probably want this to be an upgradeable RWLock
    const lock::Base.Threads.SpinLock
    # IdDict trades off some potential space savings for performance.
    # IdDict ~60ns; WeakKeyDict ~100ns
    # Space savings would come from ScopedValues being GC'd.
    # Now we hold onto them until Scope get's GC'd
    const values::IdDict{<:ScopedValue, Any}
    Scope(parent) = new(parent, Base.Threads.SpinLock(), IdDict{ScopedValue, Any}())
end

current_scope() = current_task().scope::Union{Nothing, Scope}

function Base.show(io::IO, ::Scope)
    print(io, Scope)
end

Base.lock(scope::Scope) = lock(scope.lock)
Base.unlock(scope::Scope) = unlock(scope.lock)

function Base.getindex(var::ScopedValue{T})::T where T
    cs = scope = current_scope()
    val = var.initial_value
    while scope !== nothing
        @lock scope begin
            if haskey(scope.values, var)
                val = scope.values[var]
                break
            end
        end
        scope = scope.parent
    end
    if scope !== cs
        # found the value in an upper scope, copy it down to cache.
        # this is beneficial since in contrast to storing the values in the ScopedValue
        # we now need to acquire n-Locks for an n-depth scope.
        @lock cs begin
            cs.values[var] = val
        end
    end
    return val
end

function Base.show(io::IO, var::ScopedValue)
    print(io, ScopedValue)
    print(io, '{', eltype(var), '}')
    print(io, '(')
    show(io, var[])
    print(io, ')')
end

function __set_var!(scope::Scope, var::ScopedValue{T}, val::T) where T
    # PRIVATE API! Wrong usage will break invariants of ScopedValue.
    @assert !haskey(scope.values, var)
    scope.values[var] = val
end

"""
    scoped(f, var::ScopedValue{T} => val::T)

Execute `f` in a new scope with `var` set to `val`.
"""
function scoped(f, pair::Pair{<:ScopedValue{T}, T}) where T
    @nospecialize
    ct = Base.current_task()
    current_scope = ct.scope::Union{Nothing, Scope}
    scope = Scope(current_scope)
    @lock scope begin
        __set_var!(scope, pair...)
    end

    ct.scope = scope
    try
        return f()
    finally
        ct.scope = current_scope
    end
end

"""
    scoped(f, vars...::ScopedValue{T} => val::T)

Execute `f` in a new scope with each scoped value set to the provided `val`.
"""
function scoped(f, pairs::Pair{<:ScopedValue}...)
    @nospecialize
    ct = Base.current_task()
    current_scope = ct.scope::Union{Nothing, Scope}
    scope = Scope(current_scope)
    @lock scope begin
        for (var, val) in pairs
            __set_var!(scope, var, val)
        end
    end

    ct.scope = scope
    try
        return f()
    finally
        ct.scope = current_scope
    end
end

end # module ScopedValues

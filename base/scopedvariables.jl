# This file is a part of Julia. License is MIT: https://julialang.org/license

module ScopedVariables

export ScopedVariable, scoped

mutable struct Scope
    const parent::Union{Nothing, Scope}
end

current_scope() = current_task().scope::Union{Nothing, Scope}

"""
    ScopedVariable(x)

Create a container that propagates values across scopes.
Use [`scoped`](@ref) to create and enter a new scope.

Values can only be set when entering a new scope,
and the value referred to will be constant during the
execution of a scope.

Dynamic scopes are propagated across tasks.

# Examples
```jldoctest
julia> const svar = ScopedVariable(1);

julia> svar[]
1

julia> scoped(svar => 2) do
           svar[]
       end
2
```

!!! compat "Julia 1.11"
    This method requires at least Julia 1.11. In Julia 1.7+ this
    is available from the package ScopedVariables.jl.
"""
mutable struct ScopedVariable{T}
    const values::WeakKeyDict{Scope, T}
    const initial_value::T
    ScopedVariable{T}(initial_value) where {T} = new{T}(WeakKeyDict{Scope, T}(), initial_value)
end
ScopedVariable(initial_value::T) where {T} = ScopedVariable{T}(initial_value)

Base.eltype(::Type{ScopedVariable{T}}) where {T} = T

function Base.getindex(var::ScopedVariable{T})::T where T
    scope = current_scope()
    if scope === nothing
        return var.initial_value
    end
    @lock var.values begin
        while scope !== nothing
            if haskey(var.values.ht, scope)
                return var.values.ht[scope]
            end
            scope = scope.parent
        end
    end
    return var.initial_value
end

function Base.show(io::IO, var::ScopedVariable)
    print(io, ScopedVariable)
    print(io, '{', eltype(var), '}')
    print(io, '(')
    show(io, var[])
    print(io, ')')
end

function __set_var!(scope::Scope, var::ScopedVariable{T}, val::T) where T
    # PRIVATE API! Wrong usage will break invariants of ScopedVariable.
    if scope === nothing
        error("ScopedVariable: Currently not in scope.")
    end
    @lock var.values begin
        if haskey(var.values.ht, scope)
            error("ScopedVariable: Variable is already set for this scope.")
        end
        var.values[scope] = val
    end
end

"""
    scoped(f, var::ScopedVariable{T} => val::T)

Execute `f` in a new scope with `var` set to `val`.
"""
function scoped(f, pair::Pair{<:ScopedVariable{T}, T}) where T
    @nospecialize
    ct = Base.current_task()
    current_scope = ct.scope::Union{Nothing, Scope}
    try
        scope = Scope(current_scope)
        __set_var!(scope, pair...)
        ct.scope = scope
        return f()
    finally
        ct.scope = current_scope
    end
end

"""
    scoped(f, vars...::ScopedVariable{T} => val::T)

Execute `f` in a new scope with each scoped variable set to the provided `val`.
"""
function scoped(f, pairs::Pair{<:ScopedVariable}...)
    @nospecialize
    ct = Base.current_task()
    current_scope = ct.scope::Union{Nothing, Scope}
    try
        scope = Scope(current_scope)
        for (var, val) in pairs
            __set_var!(scope, var, val)
        end
        ct.scope = scope
        return f()
    finally
        ct.scope = current_scope
    end
end

end # module ScopedVariables

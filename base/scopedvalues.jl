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
    # NOTE this struct must be defined as mutable one since it's used as a key of
    #      `ScopeStorage` dictionary and thus needs object identity
    const has_default::Bool # this field is necessary since isbitstype `default` field may be initialized with undefined value
    const default::T
    ScopedValue{T}() where T = new(false)
    ScopedValue{T}(val) where T = new{T}(true, val)
    ScopedValue(val::T) where T = new{T}(true, val)
end

Base.eltype(::ScopedValue{T}) where {T} = T

"""
    isassigned(val::ScopedValue)

Test if the ScopedValue has a default value.
"""
Base.isassigned(val::ScopedValue) = val.has_default

const ScopeStorage = Base.PersistentDict{ScopedValue, Any}

struct Scope
    values::ScopeStorage
end

function Scope(parent::Union{Nothing, Scope}, key::ScopedValue{T}, value) where T
    val = convert(T, value)
    if parent === nothing
        return Scope(ScopeStorage(key=>val))
    end
    return Scope(ScopeStorage(parent.values, key=>val))
end

function Scope(scope, pair::Pair{<:ScopedValue})
    return Scope(scope, pair...)
end

function Scope(scope, pair1::Pair{<:ScopedValue}, pair2::Pair{<:ScopedValue}, pairs::Pair{<:ScopedValue}...)
    # Unroll this loop through recursion to make sure that
    # our compiler optimization support works
    return Scope(Scope(scope, pair1...), pair2, pairs...)
end
Scope(::Nothing) = nothing

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

struct NoValue end
const novalue = NoValue()

"""
    get(val::ScopedValue{T})::Union{Nothing, Some{T}}

If the scoped value isn't set and doesn't have a default value,
return `nothing`. Otherwise returns `Some{T}` with the current
value.
"""
function get(val::ScopedValue{T}) where {T}
    scope = Core.current_scope()::Union{Scope, Nothing}
    if scope === nothing
        isassigned(val) && return Some{T}(val.default)
        return nothing
    end
    scope = scope::Scope
    if isassigned(val)
        return Some{T}(Base.get(scope.values, val, val.default)::T)
    else
        v = Base.get(scope.values, val, novalue)
        v === novalue || return Some{T}(v::T)
    end
    return nothing
end

function Base.getindex(val::ScopedValue{T})::T where T
    maybe = get(val)
    maybe === nothing && throw(KeyError(val))
    return something(maybe)::T
end

function Base.show(io::IO, val::ScopedValue)
    print(io, ScopedValue)
    print(io, '{', eltype(val), '}')
    print(io, '(')
    v = get(val)
    if v === nothing
        print(io, "undefined")
    else
        show(IOContext(io, :typeinfo => eltype(val)), something(v))
    end
    print(io, ')')
end

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
    exprs = map(esc, exprs)
    Expr(:tryfinally, esc(ex), nothing, :(Scope(Core.current_scope()::Union{Nothing, Scope}, $(exprs...))))
end

"""
    with(f, (var::ScopedValue{T} => val::T)...)

Execute `f` in a new scope with `var` set to `val`.
"""
function with(f, pair::Pair{<:ScopedValue}, rest::Pair{<:ScopedValue}...)
    @with(pair, rest..., f())
end
with(@nospecialize(f)) = f()

end # module ScopedValues

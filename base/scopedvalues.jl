# This file is a part of Julia. License is MIT: https://julialang.org/license

module ScopedValues

export ScopedValue, LazyScopedValue, with, @with
public get

"""
    AbstractScopedValue{T}

Abstract base type for scoped values that propagate values across
dynamic scopes. All scoped value types must extend this abstract type.

See also: [`ScopedValue`](@ref), [`LazyScopedValue`](@ref)

!!! compat "Julia 1.13"
    AbstractScopedValue requires Julia 1.13+.
"""
abstract type AbstractScopedValue{T} end


"""
    LazyScopedValue{T}(f::OncePerProcess{T})

A scoped value that uses an `OncePerProcess{T}` to lazily compute its default value
when none has been set in the current scope. Unlike `ScopedValue`, the default is
not evaluated at construction time but only when first accessed.

# Examples

```julia-repl
julia> using Base.ScopedValues;

julia> const editor = LazyScopedValue(OncePerProcess(() -> ENV["JULIA_EDITOR"]));

julia> editor[]
"vim"

julia> with(editor => "emacs") do
           sval[]
       end
"emacs"

julia> editor[]
"vim"
```

!!! compat "Julia 1.13"
    LazyScopedValue requires Julia 1.13+.
"""
mutable struct LazyScopedValue{T} <: AbstractScopedValue{T}
    const getdefault::OncePerProcess{T}
end


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
julia> using Base.ScopedValues;

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
mutable struct ScopedValue{T} <: AbstractScopedValue{T}
    # NOTE this struct must be defined as mutable one since it's used as a key of
    #      `ScopeStorage` dictionary and thus needs object identity
    const hasdefault::Bool # this field is necessary since isbitstype `default` field may be initialized with undefined value
    const default::T
    ScopedValue{T}() where T = new{T}(false)
    ScopedValue{T}(val) where T = new{T}(true, val)
    ScopedValue(val::T) where T = new{T}(true, val)
end

Base.eltype(::AbstractScopedValue{T}) where {T} = T

hasdefault(val::ScopedValue) = val.hasdefault
hasdefault(val::LazyScopedValue) = true

getdefault(val::ScopedValue) = val.hasdefault ? val.default : throw(KeyError(val))
getdefault(val::LazyScopedValue) = val.getdefault()

"""
    isassigned(val::ScopedValue)

Test whether a `ScopedValue` has an assigned value.

See also: [`ScopedValues.with`](@ref), [`ScopedValues.@with`](@ref), [`ScopedValues.get`](@ref).

# Examples
```jldoctest
julia> using Base.ScopedValues

julia> a = ScopedValue(1); b = ScopedValue{Int}();

julia> isassigned(a)
true

julia> isassigned(b)
false
```
"""
function Base.isassigned(val::AbstractScopedValue)
    hasdefault(val) && return true
    scope = Core.current_scope()::Union{Scope, Nothing}
    scope === nothing && return false
    return haskey((scope::Scope).values, val)
end

const ScopeStorage = Base.PersistentDict{AbstractScopedValue, Any}

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

function Base.show(io::IO, scope::Scope)
    print(io, Scope, "(")
    first = true
    for (key, value) in scope.values
        if first
            first = false
        else
            print(io, ", ")
        end
        print(io, isa(key, ScopedValue) ? ScopedValue{eltype(key)} : typeof(key), "@")
        show(io, Base.objectid(key))
        print(io, " => ")
        show(IOContext(io, :typeinfo => eltype(key)), value)
    end
    print(io, ")")
end

"""
    get(val::ScopedValue{T})::Union{Nothing, Some{T}}
    get(val::LazyScopedValue{T})::Union{Nothing, Some{T}}

If the scoped value isn't set and doesn't have a default value,
return `nothing`. Otherwise returns `Some{T}` with the current
value.

See also: [`ScopedValues.with`](@ref), [`ScopedValues.@with`](@ref), [`ScopedValues.ScopedValue`](@ref).

# Examples
```jldoctest
julia> using Base.ScopedValues

julia> a = ScopedValue(42); b = ScopedValue{Int}();

julia> ScopedValues.get(a)
Some(42)

julia> isnothing(ScopedValues.get(b))
true
```
"""
function get(val::AbstractScopedValue{T}) where {T}
    scope = Core.current_scope()::Union{Scope, Nothing}
    if scope === nothing
        !hasdefault(val) && return nothing
        return Some{T}(getdefault(val))
    end
    scope = scope::Scope
    if hasdefault(val)
        return Some{T}(Base.get(Base.Fix1(getdefault, val), scope.values, val)::T)
    else
        v = Base.KeyValue.get(scope.values, val)
        v === nothing && return nothing
        return Some{T}(only(v)::T)
    end
    return nothing
end

function Base.getindex(val::AbstractScopedValue{T})::T where T
    maybe = get(val)
    maybe === nothing && throw(KeyError(val))
    return something(maybe)::T
end

function Base.show(io::IO, val::AbstractScopedValue)
    if isa(val, ScopedValue)
        print(io, ScopedValue)
        print(io, '{', eltype(val), '}')
    else
        print(io, typeof(val))
    end
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
    @with (var::ScopedValue{T} => val)... expr

Macro version of `with`. The expression `@with var=>val expr` evaluates `expr` in a
new dynamic scope with `var` set to `val`. `val` will be converted to type `T`.
`@with var=>val expr` is equivalent to `with(var=>val) do expr end`, but `@with`
avoids creating a closure.

See also: [`ScopedValues.with`](@ref), [`ScopedValues.ScopedValue`](@ref), [`ScopedValues.get`](@ref).

# Examples
```jldoctest
julia> using Base.ScopedValues

julia> const a = ScopedValue(1);

julia> f(x) = a[] + x;

julia> @with a=>2 f(10)
12

julia> @with a=>3 begin
           x = 100
           f(x)
       end
103
```
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
    with(f, (var::ScopedValue{T} => val)...)

Execute `f` in a new dynamic scope with `var` set to `val`. `val` will be converted
to type `T`.

See also: [`ScopedValues.@with`](@ref), [`ScopedValues.ScopedValue`](@ref), [`ScopedValues.get`](@ref).

# Examples
```jldoctest
julia> using Base.ScopedValues

julia> a = ScopedValue(1);

julia> f(x) = a[] + x;

julia> f(10)
11

julia> with(a=>2) do
           f(10)
       end
12

julia> f(10)
11

julia> b = ScopedValue(2);

julia> g(x) = a[] + b[] + x;

julia> with(a=>10, b=>20) do
           g(30)
       end
60

julia> with(() -> a[] * b[], a=>3, b=>4)
12
```
"""
function with(f, pair::Pair{<:AbstractScopedValue}, rest::Pair{<:AbstractScopedValue}...)
    @with(pair, rest..., f())
end
with(@nospecialize(f)) = f()

end # module ScopedValues

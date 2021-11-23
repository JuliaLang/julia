# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
    Some{T} <: AbstractArray{T,0}

A wrapper type used in `Union{Some{T}, Nothing}` to distinguish between the absence
of a value ([`nothing`](@ref)) and the presence of a `nothing` value (i.e. `Some(nothing)`).

Use [`something`](@ref) to access the value wrapped by a `Some` object.
"""
struct Some{T} <: AbstractArray{T,0}
    value::T
end

Some(::Type{T}) where {T} = Some{Type{T}}(T)

eltype(x::Type{<:Some{T}}) where {T} = @isdefined(T) ? T : Any
size(x::Some) = ()
axes(x::Some) = ()
length(x::Some) = 1
isempty(x::Some) = false
ndims(x::Some) = 0
ndims(::Type{<:Some}) = 0
iterate(r::Some) = (r.value, nothing)
getindex(r::Some) = r.value
iterate(r::Some, s) = nothing
IteratorSize(::Type{<:Some}) = HasShape{0}()

nonnothingtype(::Type{T}) where {T} = typesplit(T, Nothing)
function nonnothingtype_checked(T::Type)
    R = nonnothingtype(T)
    R >: T && error("could not compute non-nothing type")
    return R
end

promote_rule(::Type{Some{T}}, ::Type{Some{S}}) where {T, S<:T} = Some{T}
promote_rule(T::Type{Nothing}, S::Type) = Union{S, Nothing}
function promote_rule(T::Type{>:Nothing}, S::Type)
    R = nonnothingtype(T)
    R >: T && return Any
    T = R
    R = promote_type(T, S)
    return Union{R, Nothing}
end

convert(::Type{T}, x::T) where {T>:Nothing} = x
convert(::Type{T}, x) where {T>:Nothing} = convert(nonnothingtype_checked(T), x)
convert(::Type{Nothing}, x) = throw(MethodError(convert, (Nothing, x)))
convert(::Type{Nothing}, ::Nothing) = nothing
convert(::Type{Some{T}}, x::Some{T}) where {T} = x
convert(::Type{Some{T}}, x::Some) where {T} = Some{T}(convert(T, x.value))

function show(io::IO, x::Some)
    if get(io, :typeinfo, Any) == typeof(x)
        show(io, x.value)
    else
        print(io, "Some(")
        show(io, x.value)
        print(io, ')')
    end
end

"""
    notnothing(x)

Throw an error if `x === nothing`, and return `x` if not.
"""
notnothing(x::Any) = x
notnothing(::Nothing) = throw(ArgumentError("nothing passed to notnothing"))

"""
    isnothing(x)

Return `true` if `x === nothing`, and return `false` if not.

!!! compat "Julia 1.1"
    This function requires at least Julia 1.1.

See also [`something`](@ref), [`notnothing`](@ref), [`ismissing`](@ref).
"""
isnothing(x) = x === nothing


"""
    something(x...)

Return the first value in the arguments which is not equal to [`nothing`](@ref),
if any. Otherwise throw an error.
Arguments of type [`Some`](@ref) are unwrapped.

See also [`coalesce`](@ref), [`skipmissing`](@ref), [`@something`](@ref).

# Examples
```jldoctest
julia> something(nothing, 1)
1

julia> something(Some(1), nothing)
1

julia> something(missing, nothing)
missing

julia> something(nothing, nothing)
ERROR: ArgumentError: No value arguments present
```
"""
function something end

something() = throw(ArgumentError("No value arguments present"))
something(x::Nothing, y...) = something(y...)
something(x::Some, y...) = x.value
something(x::Any, y...) = x


"""
    @something(x...)

Short-circuiting version of [`something`](@ref).

# Examples
```jldoctest
julia> f(x) = (println("f(\$x)"); nothing);

julia> a = 1;

julia> a = @something a f(2) f(3) error("Unable to find default for `a`")
1

julia> b = nothing;

julia> b = @something b f(2) f(3) error("Unable to find default for `b`")
f(2)
f(3)
ERROR: Unable to find default for `b`
[...]

julia> b = @something b f(2) f(3) Some(nothing)
f(2)
f(3)

julia> b === nothing
true
```

!!! compat "Julia 1.7"
    This macro is available as of Julia 1.7.
"""
macro something(args...)
    expr = :(nothing)
    for arg in reverse(args)
        expr = :(val = $(esc(arg)); val !== nothing ? val : ($expr))
    end
    something = GlobalRef(Base, :something)
    return :($something($expr))
end

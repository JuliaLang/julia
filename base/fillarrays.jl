
# Basic structure of a FillArray, the last version of fill.
# These definitions are *intended* to be pirated and extended by FillArrays.jl (which originally defined them)

"""
    AbstractFill{T, N, Axes} <: AbstractArray{T, N}
Supertype for lazy array types whose entries are all equal to constant.

For more `Fill` functionality, use the FillArrays.jl package.
"""
abstract type AbstractFill{T, N, Axes} <: AbstractArray{T, N} end

==(a::AbstractFill, b::AbstractFill) = axes(a) == axes(b) && getindex_value(a) == getindex_value(b)

@inline function getindex(F::AbstractFill, k::Integer)
    @boundscheck checkbounds(F, k)
    getindex_value(F)
end

@inline function getindex(F::AbstractFill{T, N}, kj::Vararg{<:Integer, N}) where {T, N}
    @boundscheck checkbounds(F, kj...)
    getindex_value(F)
end

@inline function setindex!(F::AbstractFill, v, k::Integer)
    @boundscheck checkbounds(F, k)
    v == getindex_value(F) || throw(ArgumentError("Cannot setindex! to $v for an AbstractFill with value $(getindex_value(F))."))
    F
end

@inline function setindex!(F::AbstractFill{T, N}, v, kj::Vararg{<:Integer, N}) where {T, N}
    @boundscheck checkbounds(F, kj...)
    v == getindex_value(F) || throw(ArgumentError("Cannot setindex! to $v for an AbstractFill with value $(getindex_value(F))."))
    F
end

@inline function fill!(F::AbstractFill, v)
    v == getindex_value(F) || throw(ArgumentError("Cannot fill! with $v an AbstractFill with value $(getindex_value(F))."))
    F
end

IndexStyle(::Type{<:AbstractFill{<:Any,N,<:NTuple{N,Base.OneTo{Int}}}}) where N = IndexLinear()


"""
    Fill{T, N, Axes}
    where `Axes <: Tuple{Vararg{AbstractUnitRange,N}}`
A lazy representation of an array of dimension `N`
whose entries are all equal to a constant of type `T`,
with axes of type `Axes`.
Typically created by `Fill` or `Zeros` or `Ones`
# Examples
```jldoctest
julia> Fill(1)
0-dimensional Fill{Int64, 0, Tuple{}}:
1

julia> Fill(7, (2,3))
2×3 Fill{Int64,2,Tuple{Base.OneTo{Int64},Base.OneTo{Int64}}}:
 7  7  7
 7  7  7

julia> Fill{Float64, 1, Tuple{UnitRange{Int64}}}(7., (1:2,))
2-element Fill{Float64,1,Tuple{UnitRange{Int64}}} with indices 1:2:
 7.0
 7.0
```

For more `Fill` functionality and efficient routines, use the FillArrays.jl package.
"""
struct Fill{T, N, Axes} <: AbstractFill{T, N, Axes}
    value::T
    axes::Axes

    Fill{T,N,Axes}(x::T, sz::Axes) where Axes<:Tuple{Vararg{AbstractUnitRange,N}} where {T, N} =
        new{T,N,Axes}(x,sz)
    Fill{T,0,Tuple{}}(x::T, sz::Tuple{}) where T = new{T,0,Tuple{}}(x, sz)
end

Fill{T,N,Axes}(x, sz::Axes) where Axes<:Tuple{Vararg{AbstractUnitRange,N}} where {T, N} =
    Fill{T,N,Axes}(convert(T, x)::T, sz)

Fill{T,0}(x::T, ::Tuple{}) where T = Fill{T,0,Tuple{}}(x, ()) # ambiguity fix

@inline Fill{T, N}(x::T, sz::Axes) where Axes<:Tuple{Vararg{AbstractUnitRange,N}} where {T, N} =
    Fill{T,N,Axes}(x, sz)
@inline Fill{T, N}(x, sz::Axes) where Axes<:Tuple{Vararg{AbstractUnitRange,N}} where {T, N} =
    Fill{T,N}(convert(T, x)::T, sz)

@inline Fill{T, N}(x, sz::SZ) where SZ<:Tuple{Vararg{Integer,N}} where {T, N} =
    Fill{T,N}(x, Base.OneTo.(sz))
@inline Fill{T, N}(x, sz::Vararg{Integer, N}) where {T, N} = Fill{T,N}(convert(T, x)::T, sz)


@inline Fill{T}(x, sz::Vararg{<:Integer,N}) where {T, N} = Fill{T, N}(x, sz)
@inline Fill{T}(x, sz::Tuple{Vararg{<:Any,N}}) where {T, N} = Fill{T, N}(x, sz)
""" `Fill(x, dims...)` construct lazy version of `fill(x, dims...)` """
@inline Fill(x::T, sz::Vararg{<:Integer,N}) where {T, N}  = Fill{T, N}(x, sz)
""" `Fill(x, dims)` construct lazy version of `fill(x, dims)` """
@inline Fill(x::T, sz::Tuple{Vararg{<:Any,N}}) where {T, N}  = Fill{T, N}(x, sz)

# We restrict to  when T is specified to avoid ambiguity with a Fill of a Fill
@inline Fill{T}(F::Fill{T}) where T = F
@inline Fill{T,N}(F::Fill{T,N}) where {T,N} = F
@inline Fill{T,N,Axes}(F::Fill{T,N,Axes}) where {T,N,Axes} = F

@inline axes(F::Fill) = F.axes
@inline size(F::Fill) = length.(F.axes)

AbstractArray{T}(F::Fill{T}) where T = F
AbstractArray{T,N}(F::Fill{T,N}) where {T,N} = F
AbstractArray{T}(F::Fill{V,N}) where {T,V,N} = Fill{T}(convert(T, F.value)::T, F.axes)
AbstractArray{T,N}(F::Fill{V,N}) where {T,V,N} = Fill{T}(convert(T, F.value)::T, F.axes)

convert(::Type{AbstractArray{T}}, F::Fill{T}) where T = F
convert(::Type{AbstractArray{T,N}}, F::Fill{T,N}) where {T,N} = F
convert(::Type{AbstractArray{T}}, F::Fill) where {T} = AbstractArray{T}(F)
convert(::Type{AbstractArray{T,N}}, F::Fill) where {T,N} = AbstractArray{T,N}(F)
convert(::Type{AbstractFill}, F::AbstractFill) = F
convert(::Type{AbstractFill{T}}, F::AbstractFill) where T = convert(AbstractArray{T}, F)
convert(::Type{AbstractFill{T,N}}, F::AbstractFill) where {T,N} = convert(AbstractArray{T,N}, F)

copy(F::Fill) = Fill(F.value, F.axes)

getindex(F::Fill{<:Any,0}) = getindex_value(F)

@inline getindex_value(F::Fill) = F.value

map(f::Function, r::AbstractFill) = Fill(f(getindex_value(r)), axes(r))

iterate(f::AbstractFill{<:Any, 0}) = (getindex_value(f), nothing)
iterate( ::AbstractFill{<:Any, 0}, s) = nothing

getindex(f::AbstractFill{<:Any, 0}, ::CartesianIndex{0}) = getindex_value(f)

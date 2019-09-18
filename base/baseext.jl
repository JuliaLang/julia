# This file is a part of Julia. License is MIT: https://julialang.org/license

# extensions to Core types to add features in Base

# hook up VecElement constructor to Base.convert
VecElement{T}(arg) where {T} = VecElement{T}(convert(T, arg))
convert(::Type{T}, arg::T) where {T<:VecElement} = arg
convert(::Type{T}, arg)  where {T<:VecElement} = T(arg)

# ## dims-type-converting Array constructors for convenience
# type and dimensionality specified, accepting dims as series of Integers
Vector{T}(::UndefInitializer, m::Integer) where {T} = Vector{T}(undef, Int(m))
Matrix{T}(::UndefInitializer, m::Integer, n::Integer) where {T} = Matrix{T}(undef, Int(m), Int(n))
Array{T,N}(::UndefInitializer, d::Vararg{Integer,N}) where {T,N} = Array{T,N}(undef, convert(Tuple{Vararg{Int}}, d))
# type but not dimensionality specified, accepting dims as series of Integers
Array{T}(::UndefInitializer, m::Integer) where {T} = Array{T,1}(undef, Int(m))
Array{T}(::UndefInitializer, m::Integer, n::Integer) where {T} = Array{T,2}(undef, Int(m), Int(n))
Array{T}(::UndefInitializer, m::Integer, n::Integer, o::Integer) where {T} = Array{T,3}(undef, Int(m), Int(n), Int(o))
Array{T}(::UndefInitializer, d::Integer...) where {T} = Array{T}(undef, convert(Tuple{Vararg{Int}}, d))
# dimensionality but not type specified, accepting dims as series of Integers
Vector(::UndefInitializer, m::Integer) = Vector{Any}(undef, Int(m))
Matrix(::UndefInitializer, m::Integer, n::Integer) = Matrix{Any}(undef, Int(m), Int(n))
# Dimensions as a single tuple
Array{T}(::UndefInitializer, d::NTuple{N,Integer}) where {T,N} = Array{T,N}(undef, convert(Tuple{Vararg{Int}}, d))
Array{T,N}(::UndefInitializer, d::NTuple{N,Integer}) where {T,N} = Array{T,N}(undef, convert(Tuple{Vararg{Int}}, d))
# empty vector constructor
Vector() = Vector{Any}(undef, 0)

# Array constructors for nothing and missing
# type and dimensionality specified
Array{T,N}(::Nothing, d...) where {T,N} = fill!(Array{T,N}(undef, d...), nothing)
Array{T,N}(::Missing, d...) where {T,N} = fill!(Array{T,N}(undef, d...), missing)
# type but not dimensionality specified
Array{T}(::Nothing, d...) where {T} = fill!(Array{T}(undef, d...), nothing)
Array{T}(::Missing, d...) where {T} = fill!(Array{T}(undef, d...), missing)

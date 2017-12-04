# This file is a part of Julia. License is MIT: https://julialang.org/license

# machinery for generation with Sampler

# This describes how to generate random scalars or arrays, by generating a Sampler
# and calling rand on it (which should be defined in "generation.jl").


## scalars

rand(rng::AbstractRNG, X) = rand(rng, Sampler(rng, X, Val(1)))
rand(rng::AbstractRNG=GLOBAL_RNG, ::Type{X}=Float64) where {X} =
    rand(rng, Sampler(rng, X, Val(1)))

rand(X) = rand(GLOBAL_RNG, X)
rand(::Type{X}) where {X} = rand(GLOBAL_RNG, X)

## arrays

rand!(A::AbstractArray{T}, X) where {T} = rand!(GLOBAL_RNG, A, X)
rand!(A::AbstractArray{T}, ::Type{X}=T) where {T,X} = rand!(GLOBAL_RNG, A, X)

rand!(rng::AbstractRNG, A::AbstractArray{T}, X) where {T} = rand!(rng, A, Sampler(rng, X))
rand!(rng::AbstractRNG, A::AbstractArray{T}, ::Type{X}=T) where {T,X} = rand!(rng, A, Sampler(rng, X))

function rand!(rng::AbstractRNG, A::AbstractArray{T}, sp::Sampler) where T
    for i in eachindex(A)
        @inbounds A[i] = rand(rng, sp)
    end
    A
end

rand(r::AbstractRNG, dims::Dims)       = rand(r, Float64, dims)
rand(                dims::Dims)       = rand(GLOBAL_RNG, dims)
rand(r::AbstractRNG, dims::Integer...) = rand(r, Dims(dims))
rand(                dims::Integer...) = rand(Dims(dims))

rand(r::AbstractRNG, X, dims::Dims)  = rand!(r, Array{eltype(X)}(uninitialized, dims), X)
rand(                X, dims::Dims)  = rand(GLOBAL_RNG, X, dims)

rand(r::AbstractRNG, X, d::Integer, dims::Integer...) = rand(r, X, Dims((d, dims...)))
rand(                X, d::Integer, dims::Integer...) = rand(X, Dims((d, dims...)))
# note: the above methods would trigger an ambiguity warning if d was not separated out:
# rand(r, ()) would match both this method and rand(r, dims::Dims)
# moreover, a call like rand(r, NotImplementedType()) would be an infinite loop

rand(r::AbstractRNG, ::Type{X}, dims::Dims) where {X} = rand!(r, Array{eltype(X)}(uninitialized, dims), X)
rand(                ::Type{X}, dims::Dims) where {X} = rand(GLOBAL_RNG, X, dims)

rand(r::AbstractRNG, ::Type{X}, d::Integer, dims::Integer...) where {X} = rand(r, X, Dims((d, dims...)))
rand(                ::Type{X}, d::Integer, dims::Integer...) where {X} = rand(X, Dims((d, dims...)))


## dicts

rand!(A::AbstractDict{K,V}, dist::Distribution{<:Pair}=Combine(Pair, K, V)) where {K,V} =
    rand!(GLOBAL_RNG, A, dist)

rand!(rng::AbstractRNG, A::AbstractDict{K,V},
      dist::Distribution{<:Pair}=Combine(Pair, K, V)) where {K,V} =
          rand!(GLOBAL_RNG, A, Sampler(rng, dist))

function _rand!(rng::AbstractRNG, A::Union{AbstractDict,AbstractSet}, n::Integer, sp::Sampler)
    empty!(A)
    while length(A) < n
        push!(A, rand(rng, sp))
    end
    A
end

rand!(rng::AbstractRNG, A::AbstractDict{K,V}, sp::Sampler) where {K,V} = _rand!(rng, A, length(A), sp)

rand(rng::AbstractRNG, dist::Distribution{<:Pair}, ::Type{T}, n::Integer) where {T<:AbstractDict} =
    _rand!(rng, deduce_type(T, eltype(dist).parameters...)(), n, Sampler(rng, dist))

rand(u::Distribution{<:Pair}, ::Type{T}, n::Integer) where {T<:AbstractDict} = rand(GLOBAL_RNG, u, T, n)


## sets

rand!(A::AbstractSet{T}, X) where {T} = rand!(GLOBAL_RNG, A, X)
rand!(A::AbstractSet{T}, ::Type{X}=T) where {T,X} = rand!(GLOBAL_RNG, A, X)

rand!(rng::AbstractRNG, A::AbstractSet, X) = rand!(rng, A, Sampler(rng, X))
rand!(rng::AbstractRNG, A::AbstractSet{T}, ::Type{X}=T) where {T,X} = rand!(rng, A, Sampler(rng, X))

_rand0!(rng::AbstractRNG, A::AbstractSet, n::Integer, X) = _rand!(rng, A, n, Sampler(rng, X))
_rand0!(rng::AbstractRNG, A::AbstractSet, n::Integer, ::Type{X}) where {X} = _rand!(rng, A, n, Sampler(rng, X))
_rand0!(rng::AbstractRNG, A::AbstractSet, n::Integer, sp::Sampler) = _rand!(rng, A, n, sp)

rand!(rng::AbstractRNG, A::AbstractSet, sp::Sampler) = _rand!(rng, A, length(A), sp)


rand(r::AbstractRNG, ::Type{T}, n::Integer) where {T<:AbstractSet} = rand(r, Float64, T, n)
rand(                ::Type{T}, n::Integer) where {T<:AbstractSet} = rand(GLOBAL_RNG, T, n)

rand(r::AbstractRNG, X, ::Type{T}, n::Integer) where {T<:AbstractSet} = _rand0!(r, deduce_type(T, eltype(X))(), n, X)
rand(                X, ::Type{T}, n::Integer) where {T<:AbstractSet} = rand(GLOBAL_RNG, X, T, n)

rand(r::AbstractRNG, ::Type{X}, ::Type{T}, n::Integer) where {X,T<:AbstractSet} = _rand0!(r, deduce_type(T, X)(), n, X)
rand(                ::Type{X}, ::Type{T}, n::Integer) where {X,T<:AbstractSet} = rand(GLOBAL_RNG, X, T, n)


## sparse vectors & matrices

rand(r::AbstractRNG, p::AbstractFloat, m::Integer) = sprand(r, m, p)
rand(                p::AbstractFloat, m::Integer) = sprand(GLOBAL_RNG, m, p)
rand(r::AbstractRNG, p::AbstractFloat, m::Integer, n::Integer) = sprand(r, m, n, p)
rand(                p::AbstractFloat, m::Integer, n::Integer) = sprand(GLOBAL_RNG, m, n, p)

rand(r::AbstractRNG, X::Sampler, p::AbstractFloat, m::Integer) =
    sprand(r, m, p, (r, n)->rand(r, X, n))

rand(r::AbstractRNG, X, p::AbstractFloat, m::Integer) =
    rand(r, Sampler(r, X), p, m)

rand(r::AbstractRNG, ::Type{X}, p::AbstractFloat, m::Integer) where {X} =
    rand(r, Sampler(r, X), p, m)

rand(X, p::AbstractFloat, m::Integer) = rand(GLOBAL_RNG, X, p, m)

rand(r::AbstractRNG, X::Sampler, p::AbstractFloat, m::Integer, n::Integer) =
    sprand(r, m, n, p, (r, n)->rand(r, X, n), eltype(X))

rand(r::AbstractRNG, X, p::AbstractFloat, m::Integer, n::Integer) =
    rand(r, Sampler(r, X), p, m, n)

rand(r::AbstractRNG, ::Type{X}, p::AbstractFloat, m::Integer, n::Integer) where {X} =
    rand(r, Sampler(r, X), p, m, n)

rand(X, p::AbstractFloat, m::Integer, n::Integer) = rand(GLOBAL_RNG, X, p, m, n)


## String

let b = UInt8['0':'9';'A':'Z';'a':'z']
    global rand
    rand(rng::AbstractRNG, chars, ::Type{String}, n::Integer=8) = String(rand(rng, chars, n))
    rand(                  chars, ::Type{String}, n::Integer=8) = rand(GLOBAL_RNG, chars, String, n)
    rand(rng::AbstractRNG, ::Type{String}, n::Integer=8) = rand(rng, b, String, n)
    rand(                  ::Type{String}, n::Integer=8) = rand(GLOBAL_RNG, b, String, n)
end


## BitArray

const BitArrays = Union{BitArray,BitVector,BitMatrix}

rand(r::AbstractRNG, ::Type{T}, dims::Dims) where {T<:BitArrays} =
    rand!(r, T(uninitialized, dims))

rand(r::AbstractRNG, ::Type{T}, dims::Integer...) where {T<:BitArrays} =
    rand!(r, T(uninitialized, convert(Dims, dims)))

rand(::Type{T}, dims::Dims) where {T<:BitArrays} =
    rand!(T(uninitialized, dims))

rand(::Type{T}, dims::Integer...) where {T<:BitArrays} =
    rand!(T(uninitialized, convert(Dims, dims)))

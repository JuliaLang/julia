# This file is a part of Julia. License is MIT: https://julialang.org/license

# machinery for generation with Sampler

# This describes how to generate random scalars or arrays, by generating a Sampler
# and calling rand on it (which should be defined in "generation.jl").
# NOTE: this section could be moved into a separate file when more containers are supported.

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

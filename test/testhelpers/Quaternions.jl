# This file is a part of Julia. License is MIT: https://julialang.org/license

module Quaternions
using LinearAlgebra
using Random

export Quaternion

# A custom Quaternion type with minimal defined interface and methods.
# Used to test mul and mul! methods to show non-commutativity.
struct Quaternion{T<:Real} <: Number
    s::T
    v1::T
    v2::T
    v3::T
end
Quaternion{T}(s::Real) where {T<:Real} = Quaternion{T}(T(s), zero(T), zero(T), zero(T))
Quaternion(s::Real, v1::Real, v2::Real, v3::Real) = Quaternion(promote(s, v1, v2, v3)...)
Base.convert(::Type{Quaternion{T}}, s::Real) where {T <: Real} =
    Quaternion{T}(convert(T, s), zero(T), zero(T), zero(T))
Base.promote_rule(::Type{Quaternion{T}}, ::Type{S}) where {T <: Real, S <: Real} =
    Quaternion{promote_type(T, S)}
Base.abs2(q::Quaternion) = q.s*q.s + q.v1*q.v1 + q.v2*q.v2 + q.v3*q.v3
Base.float(z::Quaternion{T}) where T = Quaternion(float(z.s), float(z.v1), float(z.v2), float(z.v3))
Base.abs(q::Quaternion) = sqrt(abs2(q))
Base.real(::Type{Quaternion{T}}) where {T} = T
Base.real(q::Quaternion) = q.s
Base.conj(q::Quaternion) = Quaternion(q.s, -q.v1, -q.v2, -q.v3)
Base.isfinite(q::Quaternion) = isfinite(q.s) & isfinite(q.v1) & isfinite(q.v2) & isfinite(q.v3)
Base.isreal(q::Quaternion) = iszero(q.v1) & iszero(q.v2) & iszero(q.v3)
Base.zero(::Type{Quaternion{T}}) where T = Quaternion{T}(zero(T), zero(T), zero(T), zero(T))
# avoid defining sqrt(::Quaternion)
LinearAlgebra.choltype(::AbstractArray{Quaternion{T}}) where T = Quaternion{promote_type(T, Float32)}

Base.:(+)(ql::Quaternion, qr::Quaternion) =
 Quaternion(ql.s + qr.s, ql.v1 + qr.v1, ql.v2 + qr.v2, ql.v3 + qr.v3)
Base.:(-)(ql::Quaternion, qr::Quaternion) =
 Quaternion(ql.s - qr.s, ql.v1 - qr.v1, ql.v2 - qr.v2, ql.v3 - qr.v3)
Base.:(*)(q::Quaternion, w::Quaternion) = Quaternion(q.s*w.s - q.v1*w.v1 - q.v2*w.v2 - q.v3*w.v3,
                                            q.s*w.v1 + q.v1*w.s + q.v2*w.v3 - q.v3*w.v2,
                                            q.s*w.v2 - q.v1*w.v3 + q.v2*w.s + q.v3*w.v1,
                                            q.s*w.v3 + q.v1*w.v2 - q.v2*w.v1 + q.v3*w.s)
Base.:(*)(q::Quaternion, r::Real) = Quaternion(q.s*r, q.v1*r, q.v2*r, q.v3*r)
Base.:(*)(q::Quaternion, r::Bool) = Quaternion(q.s*r, q.v1*r, q.v2*r, q.v3*r) # remove method ambiguity
Base.:(*)(r::Real, q::Quaternion) = q * r
Base.:(*)(r::Bool, q::Quaternion) = q * r # remove method ambiguity
Base.:(/)(q::Quaternion, w::Quaternion) = q * conj(w) * (1.0 / abs2(w))
Base.:(\)(q::Quaternion, w::Quaternion) = conj(q) * w * (1.0 / abs2(q))
Base.:(/)(q::Quaternion, r::Real) = Quaternion(q.s / r, q.v1 / r, q.v2 / r, q.v3 / r)
Base.:(==)(q::Quaternion, w::Quaternion) =
    (q.s == w.s) & (q.v1 == w.v1) & (q.v2 == w.v2) & (q.v3 == w.v3)

# adapted from https://github.com/JuliaGeometry/Quaternions.jl/pull/42
function Base.rand(rng::AbstractRNG, ::Random.SamplerType{Quaternion{T}}) where {T<:Real}
    return Quaternion{T}(rand(rng, T), rand(rng, T), rand(rng, T), rand(rng, T))
end
function Base.randn(rng::AbstractRNG, ::Type{Quaternion{T}}) where {T<:AbstractFloat}
    return Quaternion{T}(
        randn(rng, T) / 2,
        randn(rng, T) / 2,
        randn(rng, T) / 2,
        randn(rng, T) / 2,
    )
end

end

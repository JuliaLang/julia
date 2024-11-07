# This file is a part of Julia. License is MIT: https://julialang.org/license

module DualNumbers

export Dual

# Dual numbers type with minimal interface
# example of a (real) number type that subtypes Number, but not Real.
# Can be used to test generic linear algebra functions.

struct Dual{T<:Real} <: Number
    val::T
    eps::T
end
Base.:+(x::Dual, y::Dual) = Dual(x.val + y.val, x.eps + y.eps)
Base.:-(x::Dual, y::Dual) = Dual(x.val - y.val, x.eps - y.eps)
Base.:*(x::Dual, y::Dual) = Dual(x.val * y.val, x.eps * y.val + y.eps * x.val)
Base.:*(x::Number, y::Dual) = Dual(x*y.val, x*y.eps)
Base.:*(x::Dual, y::Number) = Dual(x.val*y, x.eps*y)
Base.:/(x::Dual, y::Dual) = Dual(x.val / y.val, (x.eps*y.val - x.val*y.eps)/(y.val*y.val))

Base.:(==)(x::Dual, y::Dual) = x.val == y.val && x.eps == y.eps

Base.promote_rule(::Type{Dual{T}}, ::Type{T}) where {T} = Dual{T}
Base.promote_rule(::Type{Dual{T}}, ::Type{S}) where {T,S<:Real} = Dual{promote_type(T, S)}
Base.promote_rule(::Type{Dual{T}}, ::Type{Dual{S}}) where {T,S} = Dual{promote_type(T, S)}

Base.convert(::Type{Dual{T}}, x::Dual{T}) where {T} = x
Base.convert(::Type{Dual{T}}, x::Dual) where {T} = Dual(convert(T, x.val), convert(T, x.eps))
Base.convert(::Type{Dual{T}}, x::Real) where {T} = Dual(convert(T, x), zero(T))

Base.float(x::Dual) = Dual(float(x.val), float(x.eps))
# the following two methods are needed for normalize (to check for potential overflow)
Base.typemax(x::Dual) = Dual(typemax(x.val), zero(x.eps))
Base.prevfloat(x::Dual{<:AbstractFloat}) = prevfloat(x.val)

Base.abs2(x::Dual) = x*x
Base.abs(x::Dual) = sqrt(abs2(x))
Base.sqrt(x::Dual) = Dual(sqrt(x.val), x.eps/(2sqrt(x.val)))

Base.isless(x::Dual, y::Dual) = x.val < y.val
Base.isless(x::Real, y::Dual) = x < y.val
Base.isinf(x::Dual) = isinf(x.val) & isfinite(x.eps)
Base.real(x::Dual) = x # since we currently only consider Dual{<:Real}

end # module

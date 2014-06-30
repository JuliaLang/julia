module Quaternions

import Base: convert, promote_rule, show, real, imag, conj, abs, abs2, inv, +, -, /, *

immutable Quaternion{T<:Real} <: Number
    q0::T
    q1::T
    q2::T
    q3::T
end

Quaternion(q0::Real,q1::Real,q2::Real,q3::Real) = Quaternion(promote(q0,q1,q2,q3)...)

convert{T}(::Type{Quaternion{T}}, x::Real) =
    Quaternion(convert(T,x), zero(T), zero(T), zero(T))
convert{T}(::Type{Quaternion{T}}, z::Complex) =
    Quaternion(convert(T,real(z)), convert(T,imag(z)), zero(T), zero(T))
convert{T}(::Type{Quaternion{T}}, z::Quaternion) =
    Quaternion(convert(T,z.q0), convert(T,z.q1), convert(T,z.q2), convert(T,z.q3))

promote_rule{s,S}(::Type{MathConst{s}}, ::Type{Quaternion{S}}) = Quaternion{S}
promote_rule{T,S}(::Type{Complex{T}}, ::Type{Quaternion{S}}) = Quaternion{promote_type(T,S)}
promote_rule{S}(::Type{Bool}, ::Type{Quaternion{S}}) = Quaternion{S}
promote_rule{T<:Real,S}(::Type{T}, ::Type{Quaternion{S}}) = Quaternion{promote_type(T,S)}
promote_rule{T,S}(::Type{Quaternion{T}}, ::Type{Quaternion{S}}) = Quaternion{promote_type(T,S)}

function show(io::IO, z::Quaternion)
    pm(x) = x < 0 ? " - $(-x)" : " + $x"
    print(io, z.q0, pm(z.q1), "i", pm(z.q2), "j", pm(z.q3), "k")
end

real(z::Quaternion) = z.q0
imag(z::Quaternion) = z.q1

conj(z::Quaternion) = Quaternion(z.q0, -z.q1, -z.q2, -z.q3)
abs(z::Quaternion) = sqrt(z.q0*z.q0 + z.q1*z.q1 + z.q2*z.q2 + z.q3*z.q3)
abs2(z::Quaternion) = z.q0*z.q0 + z.q1*z.q1 + z.q2*z.q2 + z.q3*z.q3
inv(z::Quaternion) = conj(z)/abs2(z)

(-)(z::Quaternion) = Quaternion(-z.q0, -z.q1, -z.q2, -z.q3)

(/)(z::Quaternion, x::Real) = Quaternion(z.q0/x, z.q1/x, z.q2/x, z.q3/x)

(+)(z::Quaternion, w::Quaternion) = Quaternion(z.q0 + w.q0, z.q1 + w.q1,
                                               z.q2 + w.q2, z.q3 + w.q3)
(-)(z::Quaternion, w::Quaternion) = Quaternion(z.q0 - w.q0, z.q1 - w.q1,
                                               z.q2 - w.q2, z.q3 - w.q3)
(*)(z::Quaternion, w::Quaternion) = Quaternion(z.q0*w.q0 - z.q1*w.q1 - z.q2*w.q2 - z.q3*w.q3,
                                               z.q0*w.q1 + z.q1*w.q0 + z.q2*w.q3 - z.q3*w.q2,
                                               z.q0*w.q2 - z.q1*w.q3 + z.q2*w.q0 + z.q3*w.q1,
                                               z.q0*w.q3 + z.q1*w.q2 - z.q2*w.q1 + z.q3*w.q0)
(/)(z::Quaternion, w::Quaternion) = z*inv(w)

end # module
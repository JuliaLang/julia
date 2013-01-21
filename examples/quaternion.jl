type Quaternion{T<:Real} <: Number
    q0::T
    q1::T
    q2::T
    q3::T
end

convert{T}(::Type{Quaternion{T}}, x::Real) =
    Quaternion(convert(T,x), convert(T,0), convert(T,0), convert(T,0))

convert{T}(::Type{Quaternion{T}}, z::Complex) =
    Quaternion(convert(T,real(z)), convert(T,imag(z)), convert(T,0), convert(T,0))

convert{T}(::Type{Quaternion{T}}, z::Quaternion) =
    Quaternion(convert(T,z.q0), convert(T,z.q1),
               convert(T,z.q2), convert(T,z.q3))

promote_rule{T,S}(::Type{Complex{T}}, ::Type{Quaternion{S}}) =
    Quaternion{promote_type(T,S)}
promote_rule{S}(::Type{Bool}, ::Type{Quaternion{S}}) = Quaternion{S}
promote_rule{T<:Real,S}(::Type{T}, ::Type{Quaternion{S}}) =
    Quaternion{promote_type(T,S)}

function show(io::IO, z::Quaternion)
    show(io, z.q0)
    i = z.q1
    if sign(i) == -1
        i = -i
        print(io, " - ")
    else
        print(io, " + ")
    end
    show(io, i)
    print(io, "i")
    j = z.q2
    if sign(j) == -1
        j = -j
        print(io, " - ")
    else
        print(io, " + ")
    end
    show(io, j)
    print(io, "j")
    k = z.q3
    if sign(k) == -1
        k = -k
        print(io, " - ")
    else
        print(io, " + ")
    end
    show(io, k)
    print(io, "k")
end

real(z::Quaternion) = z.q0
imag(z::Quaternion) = z.q1

scalar(z::Quaternion) = z.q0
vector(z::Quaternion) = vector(z.q1,z.q2,z.q3)

conj(z::Quaternion) = Quaternion(z.q0, -z.q1, -z.q2, -z.q3)
norm(z::Quaternion) = z.q0*z.q0 + z.q1*z.q1 + z.q2*z.q2 + z.q3*z.q3
inv(z::Quaternion) = conj(z)/norm(z)

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

struct Complex{T} <: Number
    re::Real{T}
    im::Real{T}
end

convert{T}(::Type{Complex{T}}, x::T) = Complex(x, convert(T,0))
convert{T}(::Type{Complex{T}}, x::Real) = Complex(convert(T,x), convert(T,0))
convert{T}(::Type{Complex{T}}, z::Complex) =
    Complex(convert(T,z.re),convert(T,z.im))

promote_table{T,S}(::Type{Real{T}}, ::Type{Complex{S}}) =
    Complex{promote_type(T,S)}
promote_table{T,S}(::Type{Complex{T}}, ::Type{Complex{S}}) =
    Complex{promote_type(T,S)}

function print(c::Complex)
    print(re(c))
    i = im(c)
    if sign(i) == -1
        i = -i
        print(" - ")
    else
        print(" + ")
    end
    print(i)
    print("i")
end

re(z::Complex) = z.re
im(z::Complex) = z.im

conj(z::Complex) = Complex(z.re,-z.im)
norm(z::Complex) = z.re*z.re + z.im*z.im
abs(z::Complex) = hypot(z.re, z.im)
inv(z::Complex) = conj(z)/norm(z)

(-)(z::Complex) = Complex(-z.re, -z.im)

(/)(z::Complex, x::Real) = Complex(z.re/x, z.im/x)

(+){T}(z::Complex{T}, w::Complex{T}) = Complex(z.re + w.re, z.im + w.im)
(+){T}(z::Complex{T}, w::T)          = Complex(z.re + w   , z.im)
(+){T}(w::T, z::Complex{T})          = Complex(w + z.re   , z.im)

(-){T}(z::Complex{T}, w::Complex{T}) = Complex(z.re - w.re, z.im - w.im)
(-){T}(z::Complex{T}, w::T)          = Complex(z.re - w   , z.im)
(-){T}(w::T, z::Complex{T})          = Complex(w - z.re   , -z.im)

(*){T}(z::Complex{T}, w::Complex{T}) = Complex(z.re*w.re - z.im*w.im,
                                               z.re*w.im + z.im*w.re)
(*)(z::Complex, w::Real) = Complex(z.re*w, z.im*w)
(*)(w::Real, z::Complex) = Complex(w*z.re, w*z.im)

(/)(z::Number, w::Complex) = z*inv(w)

=={T}(z::Complex{T}, w::Complex{T})  = (z.re == w.re && z.im == w.im)
=={T}(z::Complex{T}, w::T)           = (z.re == w    && z.im == 0)
=={T}(w::T, z::Complex{T})           = (z.re == w    && z.im == 0)

function sqrt(z::Complex)
    r = sqrt((hypot(z.re, z.im)+abs(z.re))*0.5)
    if z.re >= 0.0
        return Complex(r, z.im/r*0.5)
    end
    return Complex(abs(z.im)/r*0.5, z.im >= 0.0 ? r : -r)
end

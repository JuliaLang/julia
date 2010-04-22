struct Complex{T} <: Number
    re::T
    im::T

    convert(x::T) = Complex(x, T.convert(0))
    convert(x::Real) = Complex(T.convert(x), T.convert(0))
    convert(z::Complex) = Complex(T.convert(z.re), T.convert(z.im))
end

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
abs(z::Complex) = sqrt(norm(z))
inv(z::Complex) = conj(z)/norm(z)

(-)(z::Complex) = Complex(-z.re, -z.im)

(/)(z::Complex, x::Real) = Complex(z.re/x, z.im/x)

(+){T}(z::Complex{T}, w::Complex{T}) = Complex(z.re + w.re, z.im + w.im)
(+){T}(z::Complex{T}, w::Real)       = Complex(z.re + w   , z.im)
(+){T}(w::Real, z::Complex{T})       = Complex(w + z.re   , z.im)

(-){T}(z::Complex{T}, w::Complex{T}) = Complex(z.re - w.re, z.im - w.im)
(-){T}(z::Complex{T}, w::Real)       = Complex(z.re - w   , z.im)
(-){T}(w::Real, z::Complex{T})       = Complex(w - z.re   , -z.im)

(*){T}(z::Complex{T}, w::Complex{T}) = Complex(z.re*w.re - z.im*w.im,
                                               z.re*w.im + z.im*w.re)
(*)(z::Complex, w::Real) = Complex(z.re*w, z.im*w)
(*)(w::Real, z::Complex) = Complex(w*z.re, w*z.im)

(/){T}(z::Number, w::Complex{T}) = z*inv(w)

=={T}(z::Complex{T}, w::Complex{T})  = (z.re == w.re && z.im == w.im)
=={T}(z::Complex{T}, w::Real)        = (z.re == w    && z.im == 0)
=={T}(w::Real, z::Complex{T})        = (z.re == w    && z.im == 0)

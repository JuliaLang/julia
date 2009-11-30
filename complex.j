type Complex[`T] < Scalar
    re::T
    im::T
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

complex(re::`T, im::`T) = new(Complex[T], re, im)

re(z::Complex) = z.re
im(z::Complex) = z.im

conj(z::Complex) = complex(z.re,-z.im)
norm(z::Complex) = z.re*z.re + z.im*z.im
abs(z::Complex) = sqrt(norm(z))
inv(z::Complex) = conj(z)/norm(z)

(-)(z::Complex) = complex(-z.re, -z.im)

(/)(z::Complex, x::Real) = complex(z.re/x, z.im/x)

(+)(z::Complex[`T1], w::Complex[`T2]) = complex(z.re + w.re, z.im + w.im)
(-)(z::Complex[`T1], w::Complex[`T2]) = complex(z.re - w.re, z.im - w.im)
(*)(z::Complex[`T1], w::Complex[`T2]) = complex(z.re*w.re - z.im*w.im,
                                                z.re*w.im + z.im*w.re)
(/)(z::Complex[`T1], w::Complex[`T2]) = z*inv(w)

conversion x::Real-->Complex
    return complex(x,0)
end

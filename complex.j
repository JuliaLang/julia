type Complex[`T] < Number
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

(+)(z::Complex[`T], w::Complex[`T]) = complex(z.re + w.re, z.im + w.im)
(+)(z::Complex[`T], w::Real)        = complex(z.re + w   , z.im)
(+)(w::Real, z::Complex[`T])        = complex(w + z.re   , z.im)

(-)(z::Complex[`T], w::Complex[`T]) = complex(z.re - w.re, z.im - w.im)
(-)(z::Complex[`T], w::Real)        = complex(z.re - w   , z.im)
(-)(w::Real, z::Complex[`T])        = complex(w - z.re   , -z.im)

(*)(z::Complex[`T], w::Complex[`T]) = complex(z.re*w.re - z.im*w.im,
                                              z.re*w.im + z.im*w.re)
(*)(z::Complex, w::Real) = complex(z.re*w, z.im*w)
(*)(w::Real, z::Complex) = complex(w*z.re, w*z.im)

(/)(z::Number, w::Complex[`T]) = z*inv(w)

==(z::Complex[`T], w::Complex[`T]) = (z.re == w.re && z.im == w.im)
==(z::Complex[`T], w::Real)        = (z.re == w    && z.im == 0)
==(w::Real, z::Complex[`T])        = (z.re == w    && z.im == 0)

#conversion x::Real-->Complex[`T]
#    return complex(convert(x,T),0)
#end

conversion x::`T-->Complex[`T]
    return complex(x,0)
end

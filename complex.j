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

function complex(re::`T, im::`T)
    return new(Complex[T], re, im)
end

function re(z::Complex)
    return z.re
end

function im(z::Complex)
    return z.im
end

function (+)(z::Complex, w::Complex)
    return complex(z.re + w.re, z.im + w.im)
end

function (-)(z::Complex, w::Complex)
    return complex(z.re - w.re, z.im - w.im)
end

function -(z::Complex)
    return complex(-z.re, -z.im)
end

function *(z::Complex, w::Complex)
    return complex(z.re*w.re - z.im*w.im, z.re*w.im + z.im*w.re)
end

function /(z::Complex, x::Real)
    return complex(z.re/x, z.im/x)
end

function ctranspose(z::Complex)
    return complex(z.re,-z.im)
end

function /(z::Complex, w::Complex)
    return z*w'/(w.re*w.re + w.im*w.im)
end

conversion x::Real-->Complex
    return complex(x,0)
end

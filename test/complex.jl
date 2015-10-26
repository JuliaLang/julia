# This file is a part of Julia. License is MIT: http://julialang.org/license

@test reim(2 + 3im) == (2, 3)

for T in (Int64, Float64)
    @test real(T) == T
    @test real(Complex{T}) == T
    @test complex(T) == Complex{T}
    @test complex(Complex{T}) == Complex{T}
end

# Basic arithmetic
for T in (Float16, Float32, Float64, BigFloat)
    t = true
    f = false

    # Add and subtract
    @test isequal(T(+0.0) + im, Complex(T(+0.0), T(+1.0)))
    @test isequal(T(-0.0) + im, Complex(T(-0.0), T(+1.0)))
    @test isequal(T(+0.0) - im, Complex(T(+0.0), T(-1.0)))
    @test isequal(T(-0.0) - im, Complex(T(-0.0), T(-1.0)))
    @test isequal(T(+1.0) + im, Complex(T(+1.0), T(+1.0)))
    @test isequal(T(-1.0) + im, Complex(T(-1.0), T(+1.0)))
    @test isequal(T(+1.0) - im, Complex(T(+1.0), T(-1.0)))
    @test isequal(T(-1.0) - im, Complex(T(-1.0), T(-1.0)))
    @test isequal(im + T(+0.0), Complex(T(+0.0), T(+1.0)))
    @test isequal(im + T(-0.0), Complex(T(-0.0), T(+1.0)))
    @test isequal(im - T(+0.0), Complex(T(+0.0), T(+1.0)))
    @test isequal(im - T(-0.0), Complex(T(+0.0), T(+1.0)))
    @test isequal(im + T(+1.0), Complex(T(+1.0), T(+1.0)))
    @test isequal(im + T(-1.0), Complex(T(-1.0), T(+1.0)))
    @test isequal(im - T(+1.0), Complex(T(-1.0), T(+1.0)))
    @test isequal(im - T(-1.0), Complex(T(+1.0), T(+1.0)))
    @test isequal(T(f) + im, Complex(T(+0.0), T(+1.0)))
    @test isequal(T(t) + im, Complex(T(+1.0), T(+1.0)))
    @test isequal(T(f) - im, Complex(T(+0.0), T(-1.0)))
    @test isequal(T(t) - im, Complex(T(+1.0), T(-1.0)))
    @test isequal(im + T(f), Complex(T(+0.0), T(+1.0)))
    @test isequal(im + T(t), Complex(T(+1.0), T(+1.0)))
    @test isequal(im - T(f), Complex(T(+0.0), T(+1.0)))
    @test isequal(im - T(t), Complex(T(-1.0), T(+1.0)))

    # Multiply
    @test isequal(T(+0.0) * im, Complex(T(+0.0), T(+0.0)))
    @test isequal(T(-0.0) * im, Complex(T(-0.0), T(-0.0)))
    @test isequal(T(+1.0) * im, Complex(T(+0.0), T(+1.0)))
    @test isequal(T(-1.0) * im, Complex(T(-0.0), T(-1.0)))
    @test isequal(im * T(+0.0), Complex(T(+0.0), T(+0.0)))
    @test isequal(im * T(-0.0), Complex(T(-0.0), T(-0.0)))
    @test isequal(im * T(+1.0), Complex(T(+0.0), T(+1.0)))
    @test isequal(im * T(-1.0), Complex(T(-0.0), T(-1.0)))
end

# Test math functions. We compare to BigFloat instead of hard-coding
# values, assuming that BigFloat has an independent and independently
# tested implementation.
for T in (Float32, Float64)
    x = Complex{T}(1//3 + 1//4*im)
    y = Complex{T}(1//2 + 1//5*im)
    yi = 4
    # Test random values
    @test_approx_eq x^y big(x)^big(y)
    @test_approx_eq x^yi big(x)^yi
    @test_approx_eq abs(x) abs(big(x))
    @test_approx_eq abs2(x) abs2(big(x))
    @test_approx_eq acos(x) acos(big(x))
    @test_approx_eq acosh(1+x) acosh(1+big(x))
    @test_approx_eq angle(x) angle(big(x))
    @test_approx_eq asin(x) asin(big(x))
    @test_approx_eq asinh(x) asinh(big(x))
    @test_approx_eq atan(x) atan(big(x))
    @test_approx_eq atanh(x) atanh(big(x))
    @test_approx_eq cis(real(x)) cis(real(big(x)))
    @test_approx_eq cis(x) cis(big(x))
    @test_approx_eq cos(x) cos(big(x))
    @test_approx_eq cosh(x) cosh(big(x))
    @test_approx_eq exp(x) exp(big(x))
    @test_approx_eq exp10(x) exp10(big(x))
    @test_approx_eq exp2(x) exp2(big(x))
    @test_approx_eq_eps expm1(x) expm1(big(x)) eps(T)
    @test_approx_eq log(x) log(big(x))
    @test_approx_eq log10(x) log10(big(x))
    @test_approx_eq log1p(x) log1p(big(x))
    @test_approx_eq log2(x) log2(big(x))
    @test_approx_eq sin(x) sin(big(x))
    @test_approx_eq sinh(x) sinh(big(x))
    @test_approx_eq sqrt(x) sqrt(big(x))
    @test_approx_eq tan(x) tan(big(x))
    @test_approx_eq tanh(x) tanh(big(x))
    # Test inverses
    @test_approx_eq acos(cos(x)) x
    @test_approx_eq acosh(cosh(x)) x
    @test_approx_eq asin(sin(x)) x
    @test_approx_eq asinh(sinh(x)) x
    @test_approx_eq atan(tan(x)) x
    @test_approx_eq atanh(tanh(x)) x
    @test_approx_eq cos(acos(x)) x
    @test_approx_eq cosh(acosh(1+x)) 1+x
    @test_approx_eq exp(log(x)) x
    @test_approx_eq exp10(log10(x)) x
    @test_approx_eq exp2(log2(x)) x
    @test_approx_eq expm1(log1p(x)) x
    @test_approx_eq log(exp(x)) x
    @test_approx_eq log10(exp10(x)) x
    @test_approx_eq log1p(expm1(x)) x
    @test_approx_eq log2(exp2(x)) x
    @test_approx_eq sin(asin(x)) x
    @test_approx_eq sinh(asinh(x)) x
    @test_approx_eq sqrt(x)^2 x
    @test_approx_eq sqrt(x^2) x
    @test_approx_eq tan(atan(x)) x
    @test_approx_eq tanh(atanh(x)) x
    # Test some properties
    @test_approx_eq cosh(x) (exp(x)+exp(-x))/2
    @test_approx_eq cosh(x)^2-sinh(x)^2 1
    @test_approx_eq sin(x)^2+cos(x)^2 1
    @test_approx_eq sinh(x) (exp(x)-exp(-x))/2
    @test_approx_eq tan(x) sin(x)/cos(x)
    @test_approx_eq tanh(x) sinh(x)/cosh(x)
end

# sqrt:
# tests special values from csqrt man page
# as well as conj(sqrt(z)) = sqrt(conj(z))
@test isequal(sqrt(complex( 0.0, 0.0)), complex( 0.0, 0.0))
@test isequal(sqrt(complex( 0.0,-0.0)), complex( 0.0,-0.0))
@test isequal(sqrt(complex( 0.0, Inf)), complex( Inf, Inf))
@test isequal(sqrt(complex( 0.0,-Inf)), complex( Inf,-Inf))
@test isequal(sqrt(complex( 0.0, NaN)), complex( NaN, NaN))
@test isequal(sqrt(complex(-0.0, 0.0)), complex( 0.0, 0.0))
@test isequal(sqrt(complex(-0.0,-0.0)), complex( 0.0,-0.0))

@test isequal(sqrt(complex( 5.0, 0.0)), complex(sqrt(5.0), 0.0))
@test isequal(sqrt(complex( 5.0,-0.0)), complex(sqrt(5.0),-0.0))
@test isequal(sqrt(complex(-5.0, 0.0)), complex( 0.0, sqrt(5.0)))
@test isequal(sqrt(complex(-5.0,-0.0)), complex( 0.0,-sqrt(5.0)))

@test isequal(sqrt(complex( Inf, 0.0)), complex( Inf, 0.0))
@test isequal(sqrt(complex( Inf,-0.0)), complex( Inf,-0.0))
@test isequal(sqrt(complex( Inf, 5.0)), complex( Inf, 0.0))
@test isequal(sqrt(complex( Inf,-5.0)), complex( Inf,-0.0))
@test isequal(sqrt(complex( Inf, Inf)), complex( Inf, Inf))
@test isequal(sqrt(complex( Inf,-Inf)), complex( Inf,-Inf))
@test isequal(sqrt(complex( Inf, NaN)), complex( Inf, NaN))

@test isequal(sqrt(complex(-Inf, 0.0)), complex( 0.0, Inf))
@test isequal(sqrt(complex(-Inf,-0.0)), complex( 0.0,-Inf))
@test isequal(sqrt(complex(-Inf, 5.0)), complex( 0.0, Inf))
@test isequal(sqrt(complex(-Inf,-5.0)), complex( 0.0,-Inf))
@test isequal(sqrt(complex(-Inf, Inf)), complex( Inf, Inf))
@test isequal(sqrt(complex(-Inf,-Inf)), complex( Inf,-Inf))
@test isequal(sqrt(complex(-Inf, NaN)), complex( NaN, Inf))

@test isequal(sqrt(complex( NaN, 0.0)), complex( NaN, NaN))
@test isequal(sqrt(complex( NaN, 0.0)), complex( NaN, NaN))
@test isequal(sqrt(complex( NaN, Inf)), complex( Inf, Inf))
@test isequal(sqrt(complex( NaN,-Inf)), complex( Inf,-Inf))

# log:
#  log(conj(z)) = conj(log(z))

@test isequal(log(complex( 0.0, 0.0)), complex(-Inf, 0.0))
@test isequal(log(complex( 0.0,-0.0)), complex(-Inf,-0.0))
@test isequal(log(complex( 0.0, 1.0)), complex( 0.0, pi/2))
@test isequal(log(complex( 0.0,-1.0)), complex( 0.0,-pi/2))
@test isequal(log(complex( 0.0, Inf)), complex( Inf, pi/2))
@test isequal(log(complex( 0.0,-Inf)), complex( Inf,-pi/2))
@test isequal(log(complex( 0.0, NaN)), complex( NaN, NaN))
@test isequal(log(complex(-0.0, 0.0)), complex(-Inf, pi))
@test isequal(log(complex(-0.0,-0.0)), complex(-Inf,-pi))

@test isequal(log(complex( 5.0, 0.0)),complex(log(5.0), 0.0))
@test isequal(log(complex( 5.0,-0.0)),complex(log(5.0),-0.0))

@test isequal(log(complex( Inf, 5.0)), complex( Inf, 0.0))
@test isequal(log(complex( Inf,-5.0)), complex( Inf,-0.0))
@test isequal(log(complex( Inf, Inf)), complex( Inf, pi/4))
@test isequal(log(complex( Inf,-Inf)), complex( Inf,-pi/4))
@test isequal(log(complex( Inf, NaN)), complex( Inf, NaN))

@test isequal(log(complex(-Inf, 5.0)), complex( Inf, pi))
@test isequal(log(complex(-Inf,-5.0)), complex( Inf,-pi))
@test isequal(log(complex(-Inf, Inf)), complex( Inf, 3*pi/4))
@test isequal(log(complex(-Inf,-Inf)), complex( Inf,-3*pi/4))
@test isequal(log(complex(-Inf, NaN)), complex( Inf, NaN))

@test isequal(log(complex( NaN, 0.0)), complex( NaN, NaN))
@test isequal(log(complex( NaN, Inf)), complex( Inf, NaN))
@test isequal(log(complex( NaN,-Inf)), complex( Inf, NaN))
@test isequal(log(complex( NaN, NaN)), complex( NaN, NaN))

# exp:
#  exp(conj(z)) = conj(exp(z))

@test isequal(exp(complex( 0.0, 0.0)), complex(1.0, 0.0))
@test isequal(exp(complex( 0.0,-0.0)), complex(1.0,-0.0))
@test isequal(exp(complex( 0.0, Inf)), complex(NaN, NaN))
@test isequal(exp(complex( 0.0,-Inf)), complex(NaN, NaN))
@test isequal(exp(complex( 0.0, NaN)), complex(NaN, NaN))

@test isequal(exp(complex(-0.0, 0.0)), complex(1.0, 0.0))
@test isequal(exp(complex(-0.0,-0.0)), complex(1.0,-0.0))

@test isequal(exp(complex( 5.0, Inf)), complex(NaN, NaN))

@test isequal(exp(complex( Inf, 0.0)), complex(Inf, 0.0))
@test isequal(exp(complex( Inf,-0.0)), complex(Inf,-0.0))
@test isequal(exp(complex( Inf, 5.0)), complex(cos(5.0)*Inf,sin(5.0)* Inf))
@test isequal(exp(complex( Inf,-5.0)), complex(cos(5.0)*Inf,sin(5.0)*-Inf))
@test isequal(exp(complex( Inf, NaN)), complex(-Inf, NaN))
@test isequal(exp(complex( Inf, Inf)), complex(-Inf, NaN))
@test isequal(exp(complex( Inf,-Inf)), complex(-Inf, NaN))

@test isequal(exp(complex(-Inf, 0.0)), complex(0.0, 0.0))
@test isequal(exp(complex(-Inf,-0.0)), complex(0.0,-0.0))
@test isequal(exp(complex(-Inf, 5.0)), complex(cos(5.0)*0.0,sin(5.0)* 0.0))
@test isequal(exp(complex(-Inf,-5.0)), complex(cos(5.0)*0.0,sin(5.0)*-0.0))
@test isequal(exp(complex(-Inf, Inf)), complex(-0.0, 0.0))
@test isequal(exp(complex(-Inf,-Inf)), complex(-0.0,-0.0))
@test isequal(exp(complex(-Inf, NaN)), complex(-0.0, 0.0))

@test isequal(exp(complex( NaN, 0.0)), complex( NaN, 0.0))
@test isequal(exp(complex( NaN,-0.0)), complex( NaN,-0.0))
@test isequal(exp(complex( NaN, 5.0)), complex( NaN, NaN))
@test isequal(exp(complex( NaN, NaN)), complex( NaN, NaN))

# expm1:
#  expm1(conj(z)) = conj(expm1(z))

@test isequal(expm1(complex( 0.0, 0.0)), complex(0.0, 0.0))
@test isequal(expm1(complex( 0.0,-0.0)), complex(0.0,-0.0))
@test isequal(expm1(complex( 0.0, Inf)), complex(NaN, NaN))
@test isequal(expm1(complex( 0.0,-Inf)), complex(NaN, NaN))
@test isequal(expm1(complex( 0.0, NaN)), complex(NaN, NaN))

@test isequal(expm1(complex(-0.0, 0.0)), complex(-0.0, 0.0))
@test isequal(expm1(complex(-0.0,-0.0)), complex(-0.0,-0.0))

@test isequal(expm1(complex( 5.0, Inf)), complex(NaN, NaN))

@test isequal(expm1(complex( Inf, 0.0)), complex(Inf, 0.0))
@test isequal(expm1(complex( Inf,-0.0)), complex(Inf,-0.0))
@test isequal(expm1(complex( Inf, 5.0)), complex(cos(5.0)*Inf,sin(5.0)* Inf))
@test isequal(expm1(complex( Inf,-5.0)), complex(cos(5.0)*Inf,sin(5.0)*-Inf))
@test isequal(expm1(complex( Inf, NaN)), complex(-Inf, NaN))
@test isequal(expm1(complex( Inf, Inf)), complex(-Inf, NaN))
@test isequal(expm1(complex( Inf,-Inf)), complex(-Inf, NaN))

@test isequal(expm1(complex(-Inf, 0.0)), complex(-1.0, 0.0))
@test isequal(expm1(complex(-Inf,-0.0)), complex(-1.0,-0.0))
@test isequal(expm1(complex(-Inf, 5.0)), complex(-1.0,sin(5.0)* 0.0))
@test isequal(expm1(complex(-Inf,-5.0)), complex(-1.0,sin(5.0)*-0.0))
@test isequal(expm1(complex(-Inf, Inf)), complex(-1.0, 0.0))
@test isequal(expm1(complex(-Inf,-Inf)), complex(-1.0,-0.0))
@test isequal(expm1(complex(-Inf, NaN)), complex(-1.0, 0.0))

@test isequal(expm1(complex( NaN, 0.0)), complex( NaN, 0.0))
@test isequal(expm1(complex( NaN,-0.0)), complex( NaN,-0.0))
@test isequal(expm1(complex( NaN, 5.0)), complex( NaN, NaN))
@test isequal(expm1(complex( NaN, NaN)), complex( NaN, NaN))

@test isequal(expm1(complex( 1e-20, 0.0)), complex(expm1( 1e-20), 0.0))
@test isequal(expm1(complex(-1e-20, 0.0)), complex(expm1(-1e-20), 0.0))

@test_approx_eq expm1(complex( 1e-20, 1e-10)) complex( 5e-21, 1e-10)
@test_approx_eq expm1(complex( 1e-20,-1e-10)) complex( 5e-21,-1e-10)
@test_approx_eq expm1(complex(-1e-20, 1e-10)) complex(-1.5e-20, 1e-10)
@test_approx_eq expm1(complex(-1e-20,-1e-10)) complex(-1.5e-20,-1e-10)

@test_approx_eq expm1(complex( 10.0, 10.0)) exp(complex( 10.0, 10.0))-1
@test_approx_eq expm1(complex( 10.0,-10.0)) exp(complex( 10.0,-10.0))-1
@test_approx_eq expm1(complex(-10.0, 10.0)) exp(complex(-10.0, 10.0))-1
@test_approx_eq expm1(complex(-10.0,-10.0)) exp(complex(-10.0,-10.0))-1

# log1p:
@test isequal(log1p(complex(Inf, 3)), complex(Inf, +0.))
@test isequal(log1p(complex(Inf, -3)), complex(Inf, -0.))
@test isequal(log1p(complex(-Inf, 3)), complex(Inf, +pi))
@test isequal(log1p(complex(-Inf, -3)), complex(Inf, -pi))
@test isequal(log1p(complex(Inf, NaN)), complex(Inf, NaN))
@test isequal(log1p(complex(NaN, 0)), complex(NaN, NaN))
@test isequal(log1p(complex(0, NaN)), complex(NaN, NaN))
@test isequal(log1p(complex(-1, +0.)), complex(-Inf, +0.))
@test isequal(log1p(complex(-1, -0.)), complex(-Inf, -0.))
@test isequal(log1p(complex(-2, 1e-10)), log(1 + complex(-2, 1e-10)))
@test isequal(log1p(complex(1, Inf)), complex(Inf, pi/2))
@test isequal(log1p(complex(1, -Inf)), complex(Inf, -pi/2))
import Base.Math.@horner
for z in (1e-10+1e-9im, 1e-10-1e-9im, -1e-10+1e-9im, -1e-10-1e-9im)
    @test_approx_eq log1p(z) @horner(z, 0, 1, -0.5, 1/3, -0.25, 0.2)
end
for z in (15+4im, 0.2+3im, 0.08-0.9im)
    @test_approx_eq log1p(z) log(1+z)
end


# ^ (cpow)
#  equivalent to exp(y*log(x))
#    except for 0^0?
#  conj(x)^conj(y) = conj(x^y)
@test isequal(complex( 0.0, 0.0)^complex( 0.0, 0.0), complex(1.0, 0.0))
@test isequal(complex( 0.0, 0.0)^complex( 0.0,-0.0), complex(1.0, 0.0))
@test isequal(complex( 0.0, 0.0)^complex(-0.0, 0.0), complex(1.0,-0.0))
@test isequal(complex( 0.0, 0.0)^complex(-0.0,-0.0), complex(1.0,-0.0))

@test isequal(complex( 0.0,-0.0)^complex( 0.0, 0.0), complex(1.0,-0.0))
@test isequal(complex( 0.0,-0.0)^complex( 0.0,-0.0), complex(1.0,-0.0))
@test isequal(complex( 0.0,-0.0)^complex(-0.0, 0.0), complex(1.0, 0.0))
@test isequal(complex( 0.0,-0.0)^complex(-0.0,-0.0), complex(1.0, 0.0))

@test isequal(complex(-0.0, 0.0)^complex( 0.0, 0.0), complex(1.0, 0.0))
@test isequal(complex(-0.0, 0.0)^complex( 0.0,-0.0), complex(1.0, 0.0))
@test isequal(complex(-0.0, 0.0)^complex(-0.0, 0.0), complex(1.0,-0.0))
@test isequal(complex(-0.0, 0.0)^complex(-0.0,-0.0), complex(1.0,-0.0))

@test isequal(complex(-0.0,-0.0)^complex( 0.0, 0.0), complex(1.0,-0.0))
@test isequal(complex(-0.0,-0.0)^complex( 0.0,-0.0), complex(1.0,-0.0))
@test isequal(complex(-0.0,-0.0)^complex(-0.0, 0.0), complex(1.0, 0.0))
@test isequal(complex(-0.0,-0.0)^complex(-0.0,-0.0), complex(1.0, 0.0))

@test_approx_eq complex(0.0,1.0)^complex(2.0,0) complex(-1.0, 0.0)
@test_approx_eq complex(1.0,2.0)^complex(3.0,0) complex(-11.0, -2.0)

@test isequal(complex(0.0,0.0)^false, complex(1.0,0.0))
@test isequal(complex(0.0,0.0)^0, complex(1.0,0.0))

# sinh: has properties
#  sinh(conj(z)) = conj(sinh(z))
#  sinh(-z) = -sinh(z)

@test isequal(sinh(complex( 0.0, 0.0)), complex( 0.0, 0.0))
@test isequal(sinh(complex( 0.0,-0.0)), complex( 0.0,-0.0))
@test isequal(sinh(complex( 0.0, Inf)), complex( 0.0, NaN))
@test isequal(sinh(complex( 0.0,-Inf)), complex( 0.0, NaN))
@test isequal(sinh(complex( 0.0, NaN)), complex( 0.0, NaN))

@test isequal(sinh(complex(-0.0, 0.0)), complex(-0.0, 0.0))
@test isequal(sinh(complex(-0.0,-0.0)), complex(-0.0,-0.0))
@test isequal(sinh(complex(-0.0, Inf)), complex(-0.0, NaN))
@test isequal(sinh(complex(-0.0,-Inf)), complex(-0.0, NaN))
@test isequal(sinh(complex(-0.0, NaN)), complex(-0.0, NaN))

@test isequal(sinh(complex( 5.0, Inf)), complex( NaN, NaN))
@test isequal(sinh(complex( 5.0, NaN)), complex( NaN, NaN))

@test isequal(sinh(complex( Inf, 0.0)), complex( Inf, 0.0))
@test isequal(sinh(complex( Inf,-0.0)), complex( Inf,-0.0))
@test isequal(sinh(complex( Inf, 5.0)), complex(cos(5.0)*Inf,sin(5.0)*Inf))
@test isequal(sinh(complex( Inf, Inf)), complex( Inf, NaN))
@test isequal(sinh(complex( Inf,-Inf)), complex( Inf, NaN))
@test isequal(sinh(complex( Inf, NaN)), complex( Inf, NaN))

@test isequal(sinh(complex(-Inf, 0.0)), complex(-Inf, 0.0))
@test isequal(sinh(complex(-Inf,-0.0)), complex(-Inf,-0.0))
@test isequal(sinh(complex(-Inf, 5.0)), complex(cos(5.0)*-Inf,sin(5.0)*Inf))
@test isequal(sinh(complex(-Inf, Inf)), complex(-Inf, NaN))
@test isequal(sinh(complex(-Inf,-Inf)), complex(-Inf, NaN))
@test isequal(sinh(complex(-Inf, NaN)), complex(-Inf, NaN))

@test isequal(sinh(complex( NaN, 0.0)), complex( NaN, 0.0))
@test isequal(sinh(complex( NaN,-0.0)), complex( NaN,-0.0))
@test isequal(sinh(complex( NaN, 5.0)), complex( NaN, NaN))
@test isequal(sinh(complex( NaN, NaN)), complex( NaN, NaN))

# sin: defined in terms of sinh
#  sin(z) = -i*sinh(i*z)
#  i.e. if sinh(a+ib) = x+iy
#    then  sin(b-ia) = y-ix
#  sin(conj(z)) = conj(sin(z))
#  sin(-z) = -sin(z)

@test isequal(sin(complex( 0.0,-0.0)),complex( 0.0,-0.0))
@test isequal(sin(complex( 0.0, 0.0)),complex( 0.0, 0.0))
@test isequal(sin(complex( 0.0, Inf)),complex( 0.0, Inf))
@test isequal(sin(complex( 0.0,-Inf)),complex( 0.0,-Inf))
@test isequal(sin(complex( 0.0, NaN)),complex( 0.0, NaN))

@test isequal(sin(complex(-0.0, 0.0)),complex(-0.0, 0.0))
@test isequal(sin(complex(-0.0,-0.0)),complex(-0.0,-0.0))
@test isequal(sin(complex(-0.0, Inf)),complex(-0.0, Inf))
@test isequal(sin(complex(-0.0,-Inf)),complex(-0.0,-Inf))
@test isequal(sin(complex(-0.0, NaN)),complex(-0.0, NaN))

@test isequal(sin(complex( 5.0, Inf)),complex(sin(5.0)*Inf,cos(5.0)* Inf))
@test isequal(sin(complex( 5.0,-Inf)),complex(sin(5.0)*Inf,cos(5.0)*-Inf))
@test isequal(sin(complex( 5.0, NaN)),complex( NaN, NaN))

@test isequal(sin(complex( Inf, 0.0)), complex( NaN, 0.0))
@test isequal(sin(complex( Inf,-0.0)), complex( NaN,-0.0))
@test isequal(sin(complex( Inf, 5.0)), complex( NaN, NaN))
@test isequal(sin(complex( Inf, Inf)), complex( Inf, NaN))
@test isequal(sin(complex( Inf,-Inf)), complex( Inf, NaN))

@test isequal(sin(complex(-Inf, 0.0)), complex( NaN, 0.0))
@test isequal(sin(complex(-Inf,-0.0)), complex( NaN,-0.0))
@test isequal(sin(complex(-Inf, 5.0)), complex( NaN, NaN))
@test isequal(sin(complex(-Inf, Inf)), complex(-Inf, NaN))
@test isequal(sin(complex(-Inf,-Inf)), complex(-Inf, NaN))

@test isequal(sin(complex( NaN, 0.0)),complex( NaN, 0.0))
@test isequal(sin(complex( NaN,-0.0)),complex( NaN,-0.0))
@test isequal(sin(complex( NaN, 5.0)),complex( NaN, NaN))
@test isequal(sin(complex( NaN, Inf)),complex( NaN, Inf))
@test isequal(sin(complex( NaN,-Inf)),complex( NaN,-Inf))
@test isequal(sin(complex( NaN, NaN)),complex( NaN, NaN))

# cosh: has properties
#  cosh(conj(z)) = conj(cosh(z))
#  coshh(-z) = cosh(z)

@test isequal(cosh(complex( 0.0, 0.0)), complex( 1.0, 0.0))
@test isequal(cosh(complex( 0.0,-0.0)), complex( 1.0,-0.0))
@test isequal(cosh(complex( 0.0, Inf)), complex( NaN, 0.0))
@test isequal(cosh(complex( 0.0,-Inf)), complex( NaN,-0.0))
@test isequal(cosh(complex( 0.0, NaN)), complex( NaN, 0.0))

@test isequal(cosh(complex(-0.0,-0.0)), complex( 1.0, 0.0))
@test isequal(cosh(complex(-0.0, 0.0)), complex( 1.0,-0.0))
@test isequal(cosh(complex(-0.0, NaN)), complex( NaN, 0.0))

@test isequal(cosh(complex( 5.0, Inf)), complex( NaN, NaN))
@test isequal(cosh(complex( 5.0, NaN)), complex( NaN, NaN))

@test isequal(cosh(complex( Inf, 0.0)), complex( Inf, 0.0))
@test isequal(cosh(complex( Inf,-0.0)), complex( Inf,-0.0))
@test isequal(cosh(complex( Inf, 5.0)), complex(cos(5.0)*Inf,sin(5.0)* Inf))
@test isequal(cosh(complex( Inf,-5.0)), complex(cos(5.0)*Inf,sin(5.0)*-Inf))
@test isequal(cosh(complex( Inf, Inf)), complex( Inf, NaN))
@test isequal(cosh(complex( Inf,-Inf)), complex( Inf, NaN))
@test isequal(cosh(complex( Inf, NaN)), complex( Inf, NaN))

@test isequal(cosh(complex(-Inf,-0.0)), complex( Inf, 0.0))
@test isequal(cosh(complex(-Inf, 0.0)), complex( Inf,-0.0))
@test isequal(cosh(complex(-Inf,-5.0)), complex(cos(5.0)*Inf,sin(5.0)* Inf))
@test isequal(cosh(complex(-Inf, 5.0)), complex(cos(5.0)*Inf,sin(5.0)*-Inf))
@test isequal(cosh(complex(-Inf, Inf)), complex( Inf, NaN))
@test isequal(cosh(complex(-Inf,-Inf)), complex( Inf, NaN))
@test isequal(cosh(complex(-Inf, NaN)), complex( Inf, NaN))

@test isequal(cosh(complex( NaN, 0.0)), complex( NaN, 0.0))
@test isequal(cosh(complex( NaN, 5.0)), complex( NaN, NaN))
@test isequal(cosh(complex( NaN,-0.0)), complex( NaN,-0.0))
@test isequal(cosh(complex( NaN,-5.0)), complex( NaN, NaN))
@test isequal(cosh(complex( NaN, NaN)), complex( NaN, NaN))

# cos
#  cos(z) = cosh(iz)
#   i.e cos(b-ia) = cosh(a+ib)
#   and cos(b+ia) = cosh(a-ib)
#  cos(conj(z)) = conj(cos(z))
#  cos(-z) = cos(z)

@test isequal(cos(complex( 0.0, 0.0)), complex( 1.0,-0.0))
@test isequal(cos(complex( 0.0,-0.0)), complex( 1.0, 0.0))
@test isequal(cos(complex( 0.0,-Inf)), complex( Inf, 0.0))
@test isequal(cos(complex( 0.0, Inf)), complex( Inf,-0.0))
@test isequal(cos(complex( 0.0, NaN)), complex( NaN, 0.0))
@test isequal(cos(complex(-0.0, 0.0)), complex( 1.0, 0.0))
@test isequal(cos(complex(-0.0,-0.0)), complex( 1.0,-0.0))
@test isequal(cos(complex(-0.0,-Inf)), complex( Inf,-0.0))
@test isequal(cos(complex(-0.0, Inf)), complex( Inf, 0.0))
@test isequal(cos(complex(-0.0, NaN)), complex( NaN,-0.0))

@test isequal(cos(complex( 5.0,-Inf)), complex(cos(5.0)*Inf,sin(5.0)* Inf))
@test isequal(cos(complex( 5.0, Inf)), complex(cos(5.0)*Inf,sin(5.0)*-Inf))
@test isequal(cos(complex( 5.0, NaN)), complex( NaN, NaN))
@test isequal(cos(complex(-5.0,-Inf)), complex(cos(5.0)*Inf,sin(5.0)*-Inf))
@test isequal(cos(complex(-5.0, Inf)), complex(cos(5.0)*Inf,sin(5.0)* Inf))
@test isequal(cos(complex(-5.0, NaN)), complex( NaN, NaN))

@test isequal(cos(complex( Inf, 0.0)), complex( NaN,-0.0))
@test isequal(cos(complex( Inf, 5.0)), complex( NaN, NaN))
@test isequal(cos(complex( Inf, Inf)), complex( Inf, NaN))
@test isequal(cos(complex( Inf,-Inf)), complex( Inf, NaN))
@test isequal(cos(complex(-Inf, 0.0)), complex( NaN, 0.0))
@test isequal(cos(complex(-Inf, Inf)), complex( Inf, NaN))
@test isequal(cos(complex(-Inf,-Inf)), complex( Inf, NaN))

@test isequal(cos(complex( NaN, 0.0)), complex( NaN, 0.0))
@test isequal(cos(complex( NaN,-0.0)), complex( NaN, 0.0))
@test isequal(cos(complex( NaN, 5.0)), complex( NaN, NaN))
@test isequal(cos(complex( NaN, Inf)), complex( Inf, NaN))
@test isequal(cos(complex( NaN,-Inf)), complex( Inf, NaN))
@test isequal(cos(complex( NaN, NaN)), complex( NaN, NaN))

# tanh
#  tanh(conj(z)) = conj(tanh(z))
#  tanh(-z) = -tanh(z)
@test isequal(tanh(complex( 0.0, 0.0)),complex(0.0,0.0))
@test isequal(tanh(complex( 0.0,-0.0)),complex(0.0,-0.0))
@test_throws DomainError  tanh(complex( 0.0, Inf))
@test_throws DomainError  tanh(complex( 0.0,-Inf))
@test isequal(tanh(complex( 0.0, NaN)),complex(NaN,NaN))

@test isequal(tanh(complex(-0.0, 0.0)),complex(-0.0,0.0))
@test isequal(tanh(complex(-0.0,-0.0)),complex(-0.0,-0.0))

@test_throws DomainError  tanh(complex( 5.0, Inf))
@test isequal(tanh(complex( 5.0, NaN)),complex(NaN,NaN))

@test isequal(tanh(complex( Inf, 0.0)),complex(1.0, 0.0))
@test isequal(tanh(complex( Inf,-0.0)),complex(1.0,-0.0))
@test isequal(tanh(complex( Inf, 5.0)),complex(1.0,sin(2*5.0)* 0.0))
@test isequal(tanh(complex( Inf,-5.0)),complex(1.0,sin(2*5.0)*-0.0))
@test isequal(tanh(complex( Inf, Inf)),complex(1.0, 0.0))
@test isequal(tanh(complex( Inf,-Inf)),complex(1.0,-0.0))
@test isequal(tanh(complex( Inf, NaN)),complex(1.0, 0.0))

@test isequal(tanh(complex(-Inf, 0.0)),complex(-1.0, 0.0))
@test isequal(tanh(complex(-Inf,-0.0)),complex(-1.0,-0.0))
@test isequal(tanh(complex(-Inf, 5.0)),complex(-1.0,sin(2*5.0)* 0.0))
@test isequal(tanh(complex(-Inf,-5.0)),complex(-1.0,sin(2*5.0)*-0.0))
@test isequal(tanh(complex(-Inf, Inf)),complex(-1.0, 0.0))
@test isequal(tanh(complex(-Inf,-Inf)),complex(-1.0,-0.0))
@test isequal(tanh(complex(-Inf, NaN)),complex(-1.0, 0.0))

@test isequal(tanh(complex( NaN, 0.0)),complex(NaN, 0.0))
@test isequal(tanh(complex( NaN,-0.0)),complex(NaN,-0.0))
@test isequal(tanh(complex( NaN, 5.0)),complex(NaN, NaN))
@test isequal(tanh(complex( NaN,-5.0)),complex(NaN, NaN))
@test isequal(tanh(complex( NaN, NaN)),complex(NaN, NaN))

# tan
#  tan(z) = -i tanh(iz)

@test isequal(tan(complex( 0.0, Inf)),complex( 0.0, 1.0))
@test isequal(tan(complex( 0.0,-Inf)),complex( 0.0,-1.0))
@test isequal(tan(complex( 0.0, NaN)),complex( 0.0, NaN))
@test isequal(tan(complex(-0.0,-Inf)),complex(-0.0,-1.0))
@test isequal(tan(complex(-0.0, Inf)),complex(-0.0, 1.0))
@test isequal(tan(complex(-0.0, NaN)),complex(-0.0, NaN))

@test isequal(tan(complex( 5.0, Inf)),complex(sin(2*5.0)* 0.0, 1.0))
@test isequal(tan(complex( 5.0,-Inf)),complex(sin(2*5.0)* 0.0,-1.0))
@test isequal(tan(complex( 5.0, NaN)),complex( NaN, NaN))
@test isequal(tan(complex(-5.0, Inf)),complex(sin(2*5.0)*-0.0, 1.0))
@test isequal(tan(complex(-5.0,-Inf)),complex(sin(2*5.0)*-0.0,-1.0))
@test isequal(tan(complex(-5.0, NaN)),complex( NaN, NaN))

@test_throws DomainError  tan(complex( Inf, 5.0))
@test isequal(tan(complex( Inf, Inf)),complex( 0.0, 1.0))
@test isequal(tan(complex( Inf,-Inf)),complex( 0.0,-1.0))
@test isequal(tan(complex(-Inf, Inf)),complex(-0.0, 1.0))
@test isequal(tan(complex(-Inf,-Inf)),complex(-0.0,-1.0))

@test isequal(tan(complex( NaN, 5.0)),complex( NaN, NaN))
@test isequal(tan(complex( NaN, Inf)),complex( 0.0, 1.0))
@test isequal(tan(complex( NaN,-Inf)),complex( 0.0,-1.0))
@test isequal(tan(complex( NaN, NaN)),complex( NaN, NaN))

# acosh
#  acosh(conj(z)) = conj(acosh(z))

@test isequal(acosh(complex( 0.0, 0.0)), complex( 0.0, pi/2))
@test isequal(acosh(complex( 0.0,-0.0)), complex( 0.0,-pi/2))
@test isequal(acosh(complex( 0.0, Inf)), complex( Inf, pi/2))
@test isequal(acosh(complex( 0.0,-Inf)), complex( Inf,-pi/2))
@test isequal(acosh(complex(-0.0, 0.0)), complex( 0.0, pi/2))
@test isequal(acosh(complex(-0.0,-0.0)), complex( 0.0,-pi/2))
@test isequal(acosh(complex( 5.0, Inf)), complex( Inf, pi/2))
@test isequal(acosh(complex( 5.0,-Inf)), complex( Inf,-pi/2))
@test isequal(acosh(complex( 5.0, NaN)), complex( NaN, NaN))

@test isequal(acosh(complex( Inf, 0.0)), complex( Inf, 0.0))
@test isequal(acosh(complex( Inf,-0.0)), complex( Inf,-0.0))
@test isequal(acosh(complex( Inf, 5.0)), complex( Inf, 0.0))
@test isequal(acosh(complex( Inf,-5.0)), complex( Inf,-0.0))
@test isequal(acosh(complex( Inf, Inf)), complex( Inf, pi/4))
@test isequal(acosh(complex( Inf,-Inf)), complex( Inf,-pi/4))
@test isequal(acosh(complex( Inf, NaN)), complex( Inf, NaN))

@test isequal(acosh(complex(-Inf, 0.0)), complex( Inf, pi))
@test isequal(acosh(complex(-Inf,-0.0)), complex( Inf,-pi))
@test isequal(acosh(complex(-Inf, 5.0)), complex( Inf, pi))
@test isequal(acosh(complex(-Inf,-5.0)), complex( Inf,-pi))
@test isequal(acosh(complex(-Inf, Inf)), complex( Inf, 3*pi/4))
@test isequal(acosh(complex(-Inf,-Inf)), complex( Inf,-3*pi/4))
@test isequal(acosh(complex(-Inf, NaN)), complex( Inf, NaN))

@test isequal(acosh(complex( NaN, Inf)), complex( Inf, NaN))
@test isequal(acosh(complex( NaN,-Inf)), complex( Inf, NaN))
@test isequal(acosh(complex( NaN, NaN)), complex( NaN, NaN))

#
## acos
##  acos(conj(z)) = conj(acos(z))

@test isequal(acos(complex( 0.0, 0.0)),complex(pi/2,-0.0))
@test isequal(acos(complex( 0.0,-0.0)),complex(pi/2, 0.0))
@test isequal(acos(complex( 0.0, Inf)),complex(pi/2,-Inf))
@test isequal(acos(complex( 0.0,-Inf)),complex(pi/2, Inf))
@test isequal(acos(complex( 0.0, NaN)),complex(pi/2, NaN))

@test isequal(acos(complex(-0.0, 0.0)),complex(pi/2,-0.0))
@test isequal(acos(complex(-0.0,-0.0)),complex(pi/2, 0.0))
@test isequal(acos(complex(-0.0, NaN)),complex(pi/2, NaN))

@test isequal(acos(complex( 5.0, Inf)),complex(pi/2,-Inf))
@test isequal(acos(complex( 5.0,-Inf)),complex(pi/2, Inf))
@test isequal(acos(complex( 5.0, NaN)),complex( NaN, NaN))

@test isequal(acos(complex( Inf, 0.0)),complex( 0.0,-Inf))
@test isequal(acos(complex( Inf,-0.0)),complex( 0.0, Inf))
@test isequal(acos(complex( Inf, 5.0)),complex( 0.0,-Inf))
@test isequal(acos(complex( Inf,-5.0)),complex( 0.0, Inf))
@test isequal(acos(complex( Inf, Inf)),complex(pi/4,-Inf))
@test isequal(acos(complex( Inf,-Inf)),complex(pi/4, Inf))
@test isequal(acos(complex( Inf, NaN)),complex( NaN, Inf))

@test isequal(acos(complex(-Inf, 0.0)),complex(pi,-Inf))
@test isequal(acos(complex(-Inf,-0.0)),complex(pi, Inf))
@test isequal(acos(complex(-Inf, 5.0)),complex(pi,-Inf))
@test isequal(acos(complex(-Inf,-5.0)),complex(pi, Inf))
@test isequal(acos(complex(-Inf, Inf)),complex(3*pi/4,-Inf))
@test isequal(acos(complex(-Inf,-Inf)),complex(3*pi/4, Inf))
@test isequal(acos(complex(-Inf, NaN)),complex( NaN, Inf))

@test isequal(acos(complex( NaN, 0.0)),complex( NaN, NaN))
@test isequal(acos(complex( NaN, 5.0)),complex( NaN, NaN))
@test isequal(acos(complex( NaN, Inf)),complex( NaN,-Inf))
@test isequal(acos(complex( NaN,-Inf)),complex( NaN, Inf))
@test isequal(acos(complex( NaN, NaN)),complex( NaN, NaN))



## asinh
##  asinh(conj(z)) = conj(asinh(z))
##  asinh(-z) = -asinh(z)
@test isequal(asinh(complex( 0.0, 0.0)),complex( 0.0, 0.0))
@test isequal(asinh(complex( 0.0,-0.0)),complex( 0.0,-0.0))
@test isequal(asinh(complex( 0.0, Inf)),complex( Inf, pi/2))
@test isequal(asinh(complex( 0.0,-Inf)),complex( Inf,-pi/2))
@test isequal(asinh(complex( 0.0, NaN)),complex( NaN, NaN))

@test isequal(asinh(complex(-0.0, 0.0)),complex(-0.0, 0.0))
@test isequal(asinh(complex(-0.0,-0.0)),complex(-0.0,-0.0))
@test isequal(asinh(complex(-0.0, Inf)),complex(-Inf, pi/2))
@test isequal(asinh(complex(-0.0,-Inf)),complex(-Inf,-pi/2))

@test isequal(asinh(complex( 5.0, Inf)),complex( Inf, pi/2))
@test isequal(asinh(complex( 5.0,-Inf)),complex( Inf,-pi/2))
@test isequal(asinh(complex( 5.0, NaN)),complex( NaN, NaN))
@test isequal(asinh(complex(-5.0, Inf)),complex(-Inf, pi/2))
@test isequal(asinh(complex(-5.0,-Inf)),complex(-Inf,-pi/2))

@test isequal(asinh(complex( Inf, Inf)),complex( Inf, pi/4))
@test isequal(asinh(complex( Inf,-Inf)),complex( Inf,-pi/4))
@test isequal(asinh(complex( Inf, NaN)),complex( Inf, NaN))
@test isequal(asinh(complex(-Inf, Inf)),complex(-Inf, pi/4))
@test isequal(asinh(complex(-Inf,-Inf)),complex(-Inf,-pi/4))
@test isequal(asinh(complex(-Inf, NaN)),complex(-Inf, NaN))

@test isequal(asinh(complex( NaN, 0.0)),complex( NaN, 0.0))
@test isequal(asinh(complex( NaN,-0.0)),complex( NaN,-0.0))
@test isequal(asinh(complex( NaN, 5.0)),complex( NaN, NaN))
@test isequal(asinh(complex( NaN, Inf)),complex( Inf, NaN))
@test isequal(asinh(complex( NaN,-Inf)),complex( Inf, NaN))
@test isequal(asinh(complex( NaN, NaN)),complex( NaN, NaN))

# asin
#  asin(z) = -i*asinh(iz)

@test isequal(asin(complex( 0.0, 0.0)),complex( 0.0, 0.0))
@test isequal(asin(complex( 0.0,-0.0)),complex( 0.0,-0.0))
@test isequal(asin(complex(-0.0, 0.0)),complex(-0.0, 0.0))
@test isequal(asin(complex( 0.0, NaN)),complex( 0.0, NaN))
@test isequal(asin(complex(-0.0,-0.0)),complex(-0.0,-0.0))
@test isequal(asin(complex(-0.0, NaN)),complex(-0.0, NaN))
@test isequal(asin(complex( 5.0, NaN)),complex( NaN, NaN))

@test isequal(asin(complex( Inf, 0.0)),complex( pi/2, Inf))
@test isequal(asin(complex( Inf,-0.0)),complex( pi/2,-Inf))
@test isequal(asin(complex( Inf, 5.0)),complex( pi/2, Inf))
@test isequal(asin(complex( Inf,-5.0)),complex( pi/2,-Inf))
@test isequal(asin(complex( Inf, Inf)),complex( pi/4, Inf))
@test isequal(asin(complex( Inf,-Inf)),complex( pi/4,-Inf))
@test isequal(asin(complex( Inf, NaN)),complex( NaN, Inf))

@test isequal(asin(complex(-Inf, 0.0)),complex(-pi/2, Inf))
@test isequal(asin(complex(-Inf,-0.0)),complex(-pi/2,-Inf))
@test isequal(asin(complex(-Inf, 5.0)),complex(-pi/2, Inf))
@test isequal(asin(complex(-Inf,-5.0)),complex(-pi/2,-Inf))
@test isequal(asin(complex(-Inf, Inf)),complex(-pi/4, Inf))
@test isequal(asin(complex(-Inf,-Inf)),complex(-pi/4,-Inf))
@test isequal(asin(complex(-Inf, NaN)),complex( NaN, Inf))

@test isequal(asin(complex( NaN, 0.0)),complex( NaN, NaN))
@test isequal(asin(complex( NaN, 5.0)),complex( NaN, NaN))
@test isequal(asin(complex( NaN, Inf)),complex( NaN, Inf))
@test isequal(asin(complex( NaN,-Inf)),complex( NaN,-Inf))
@test isequal(asin(complex( NaN, NaN)),complex( NaN, NaN))

# atanh
#  atanh(conj(z)) = conj(atanh(z))
#  atanh(-z) = -atanh(z)

@test isequal(atanh(complex( 0.0, 0.0)),complex( 0.0, 0.0))
@test isequal(atanh(complex( 0.0,-0.0)),complex( 0.0,-0.0))
@test isequal(atanh(complex( 0.0, NaN)),complex( 0.0, NaN))
@test isequal(atanh(complex( 0.0, Inf)),complex( 0.0, pi/2))
@test isequal(atanh(complex( 0.0,-Inf)),complex( 0.0,-pi/2))

@test isequal(atanh(complex(-0.0, NaN)),complex(-0.0, NaN))
@test isequal(atanh(complex(-0.0, 0.0)),complex(-0.0, 0.0))
@test isequal(atanh(complex(-0.0,-0.0)),complex(-0.0,-0.0))
@test isequal(atanh(complex(-0.0, Inf)),complex(-0.0, pi/2))
@test isequal(atanh(complex(-0.0,-Inf)),complex(-0.0,-pi/2))

@test isequal(atanh(complex( 1.0, 0.0)),complex( Inf, 0.0))
@test isequal(atanh(complex(-1.0, 0.0)),complex(-Inf, 0.0))
@test isequal(atanh(complex( 5.0, Inf)),complex( 0.0, pi/2))
@test isequal(atanh(complex( 5.0,-Inf)),complex( 0.0,-pi/2))
@test isequal(atanh(complex( 5.0, NaN)),complex( NaN, NaN))
@test isequal(atanh(complex(-5.0, Inf)),complex(-0.0, pi/2))
@test isequal(atanh(complex(-5.0,-Inf)),complex(-0.0,-pi/2))
@test isequal(atanh(complex(-5.0, NaN)),complex( NaN, NaN))

@test isequal(atanh(complex( Inf, 0.0)),complex(0.0, pi/2))
@test isequal(atanh(complex( Inf,-0.0)),complex(0.0,-pi/2))
@test isequal(atanh(complex( Inf, 5.0)),complex(0.0, pi/2))
@test isequal(atanh(complex( Inf,-5.0)),complex(0.0,-pi/2))
@test isequal(atanh(complex( Inf, Inf)),complex(0.0, pi/2))
@test isequal(atanh(complex( Inf,-Inf)),complex(0.0,-pi/2))
@test isequal(atanh(complex( Inf, NaN)),complex(0.0, NaN))

@test isequal(atanh(complex(-Inf, 0.0)),complex(-0.0, pi/2))
@test isequal(atanh(complex(-Inf,-0.0)),complex(-0.0,-pi/2))
@test isequal(atanh(complex(-Inf, 5.0)),complex(-0.0, pi/2))
@test isequal(atanh(complex(-Inf,-5.0)),complex(-0.0,-pi/2))
@test isequal(atanh(complex(-Inf, Inf)),complex(-0.0, pi/2))
@test isequal(atanh(complex(-Inf,-Inf)),complex(-0.0,-pi/2))
@test isequal(atanh(complex(-Inf, NaN)),complex(-0.0, NaN))

@test isequal(atanh(complex( NaN, 0.0)),complex( NaN, NaN))
@test isequal(atanh(complex( NaN,-0.0)),complex( NaN, NaN))
@test isequal(atanh(complex( NaN, 5.0)),complex( NaN, NaN))
@test isequal(atanh(complex( NaN,-5.0)),complex( NaN, NaN))
@test isequal(atanh(complex( NaN, Inf)),complex( 0.0, pi/2))
@test isequal(atanh(complex( NaN,-Inf)),complex( 0.0,-pi/2))
@test isequal(atanh(complex( NaN, NaN)),complex( NaN, NaN))


# atan
#  atan(z) = -i*atanh(iz)

@test isequal(atan(complex( 0.0, 0.0)),complex( 0.0, 0.0))
@test isequal(atan(complex( 0.0,-0.0)),complex( 0.0,-0.0))
@test isequal(atan(complex( 0.0, 1.0)),complex( 0.0, Inf))
@test isequal(atan(complex( 0.0, Inf)),complex( pi/2, 0.0))
@test isequal(atan(complex( 0.0,-Inf)),complex( pi/2,-0.0))
@test isequal(atan(complex( 0.0, NaN)),complex( NaN, NaN))

@test isequal(atan(complex(-0.0, 0.0)),complex(-0.0, 0.0))
@test isequal(atan(complex(-0.0,-0.0)),complex(-0.0,-0.0))
@test isequal(atan(complex(-0.0, Inf)),complex(-pi/2, 0.0))
@test isequal(atan(complex(-0.0,-Inf)),complex(-pi/2,-0.0))
@test isequal(atan(complex(-0.0, NaN)),complex( NaN, NaN))

@test isequal(atan(complex( 5.0, Inf)),complex( pi/2, 0.0))
@test isequal(atan(complex( 5.0,-Inf)),complex( pi/2,-0.0))
@test isequal(atan(complex( 5.0, NaN)),complex( NaN, NaN))

@test isequal(atan(complex(-5.0, Inf)),complex(-pi/2, 0.0))
@test isequal(atan(complex(-5.0,-Inf)),complex(-pi/2,-0.0))
@test isequal(atan(complex(-5.0, NaN)),complex( NaN, NaN))

@test isequal(atan(complex( Inf, 0.0)),complex( pi/2, 0.0))
@test isequal(atan(complex( Inf,-0.0)),complex( pi/2,-0.0))
@test isequal(atan(complex( Inf, 5.0)),complex( pi/2, 0.0))
@test isequal(atan(complex( Inf,-5.0)),complex( pi/2,-0.0))
@test isequal(atan(complex( Inf, Inf)),complex( pi/2, 0.0))
@test isequal(atan(complex( Inf,-Inf)),complex( pi/2,-0.0))
@test isequal(atan(complex( Inf, NaN)),complex( pi/2, 0.0))

@test isequal(atan(complex(-Inf, 0.0)),complex(-pi/2, 0.0))
@test isequal(atan(complex(-Inf,-0.0)),complex(-pi/2,-0.0))
@test isequal(atan(complex(-Inf, 5.0)),complex(-pi/2, 0.0))
@test isequal(atan(complex(-Inf,-5.0)),complex(-pi/2,-0.0))
@test isequal(atan(complex(-Inf, Inf)),complex(-pi/2, 0.0))
@test isequal(atan(complex(-Inf,-Inf)),complex(-pi/2,-0.0))
@test isequal(atan(complex(-Inf, NaN)),complex(-pi/2, 0.0))

@test isequal(atan(complex( NaN, 0.0)),complex( NaN, 0.0))
@test isequal(atan(complex( NaN,-0.0)),complex( NaN,-0.0))
@test isequal(atan(complex( NaN, 5.0)),complex( NaN, NaN))
@test isequal(atan(complex( NaN,-5.0)),complex( NaN, NaN))
@test isequal(atan(complex( NaN, Inf)),complex( NaN, 0.0))
@test isequal(atan(complex( NaN,-Inf)),complex( NaN,-0.0))
@test isequal(atan(complex( NaN, NaN)),complex( NaN, NaN))


# lexcmp
@test lexcmp(1.0-1.0im, 1.0+0.0im) == -1
@test lexcmp(0.0+0.0im, 0.0+0.0im) == 0
@test lexcmp(1.0-1.0im, 0.0+0.0im) == 1


# misc.

@test complex(1//2,1//3)^2 === complex(5//36, 1//3)
@test complex(2,2)^2 === complex(0,8)
@test_throws DomainError complex(2,2)^(-2)
@test complex(2.0,2.0)^(-2) === complex(0.0, -0.125)

@test complex(1.0,[1.0,1.0]) == [complex(1.0,1.0), complex(1.0,1.0)]
@test complex([1.0,1.0],1.0) == [complex(1.0,1.0), complex(1.0,1.0)]
# robust division of Float64
# hard complex divisions from Fig 6 of arxiv.1210.4539
z7 = Complex{Float64}(3.898125604559113300e289, 8.174961907852353577e295)
z9 = Complex{Float64}(0.001953125, -0.001953125)
z10 = Complex{Float64}( 1.02951151789360578e-84, 6.97145987515076231e-220)
harddivs = ((1.0+im*1.0, 1.0+im*2^1023.0, 2^-1023.0-im*2^-1023.0), #1
      (1.0+im*1.0, 2^-1023.0+im*2^-1023.0, 2^1023.0+im*0.0), #2
      (2^1023.0+im*2^-1023.0, 2^677.0+im*2^-677.0, 2^346.0-im*2^-1008.0), #3
      (2^1023.0+im*2^1023.0, 1.0+im*1.0, 2^1023.0+im*0.0), #4
      (2^1020.0+im*2^-844., 2^656.0+im*2^-780.0, 2^364.0-im*2^-1072.0), #5
      (2^-71.0+im*2^1021., 2^1001.0+im*2^-323.0, 2^-1072.0+im*2^20.0), #6
      (2^-347.0+im*2^-54., 2^-1037.0+im*2^-1058.0, z7), #7
      (2^-1074.0+im*2^-1074., 2^-1073.0+im*2^-1074., 0.6+im*0.2), #8
      (2^1015.0+im*2^-989., 2^1023.0+im*2^1023.0, z9), #9
      (2^-622.0+im*2^-1071., 2^-343.0+im*2^-798.0, z10)#10
      )

# calculate "accurate bits" in range 0:53 by algorithm given in arxiv.1210.4539
function sb_accuracy(x,expected)
  min(logacc(real(x),real(expected)),
    logacc(imag(x),imag(expected)) )
end
relacc(x,expected) = abs(x-expected)/abs(expected)
function logacc(x::Float64,expected::Float64)
  x == expected && (return 53)
  expected == 0 && (return 0)
  (x == Inf || x == -Inf) && (return 0)
  isnan(x) && (return 0)
  ra = relacc(BigFloat(x),BigFloat(expected))
  max(floor(Int,-log2(ra)),0)
end
# the robust division algorithm should have 53 or 52
# bits accuracy for each of the hard divisions
@test 52 <= minimum([sb_accuracy(h[1]/h[2],h[3]) for h in harddivs])

# division of non-Float64
function cdiv_test(a,b)
  c=convert(Complex{Float64},a)/convert(Complex{Float64},b)
  50 <= sb_accuracy(c,convert(Complex{Float64},a/b))
end
@test cdiv_test(complex(1//2, 3//4), complex(17//13, 4//5))
@test cdiv_test(complex(1,2), complex(8997,2432))

# inv
@test inv(1e300+0im) == 1e-300 - 0.0im
@test inv(0+1e300im) == 0.0 - 1e-300im

# issue #7904
@test log10(10+0im) === 1.0 + 0.0im
@test log2(2+0im) === 1.0 + 0.0im

# sign
for T in (Float32, Float64)
    z = Complex{T}(1)
    @test typeof(sign(z)) == typeof(z)
    z = Complex{T}(0)
    @test typeof(sign(z)) == typeof(z)
end
for T in (Int32, Int64)
    z = Complex{T}(1)
    @test typeof(sign(z)) == typeof(float(z))
    z = Complex{T}(0)
    @test typeof(sign(z)) == typeof(float(z))
end

@test sign(0 + 0im) == 0
@test sign(2 + 0im) == 1
@test sign(-2 + 0im) == -1
@test sign(1 + im) ≈ (1 + im) / sqrt(2)
@test sign(1 - im) ≈ (1 - im) / sqrt(2)

# cis
@test_approx_eq cis(0.0+1.0im) 0.367879441171442321595523770161460867445811131031767834507836+0.0im
@test_approx_eq cis(1.0+0.0im) 0.54030230586813971740093660744297660373231042061+0.84147098480789650665250232163029899962256306079im
@test_approx_eq cis(pi) -1.0+0.0im
@test_approx_eq cis(pi/2) 0.0+1.0im

# exp2
@test exp2(0.0+0.0im) == 1.0+0.0im
@test exp2(1.0+0.0im) == 2.0+0.0im
#wolframalpha
@test_approx_eq exp2(1.0+3.0im) -0.9739888359315627962096198412+1.74681016354974281701922im

# exp10
@test exp10(0.0+0.0im) == 1.0+0.0im
@test exp10(1.0+0.0im) == 10.0+0.0im
#wolframalpha
@test_approx_eq exp10(1.0+2.0im) -1.0701348355877020772086517528518239460495529361-9.9425756941378968736161937190915602112878340717im

# round #8291
@test round(Complex(1.125, 0.875), 2) == Complex(1.12, 0.88)
@test round(Complex(1.5, 0.5), RoundDown, RoundUp) == Complex(1.0, 1.0)
@test round([1:5;] + im) == [1:5;] + im
@test round([1:5;] + 0.5im) == [1.0:5.0;]

# float #8291
@test float(Complex(1, 2)) == Complex(1.0, 2.0)
@test round(float(Complex(π, e)),3) == Complex(3.142, 2.718)

# Complex32 arithmetic #10003
@test Float16(1)+Float16(1)im === Complex32(1, 1)
@test Float16(1)-Float16(1)im === Float16(1)+Float16(-1)im === Complex32(1, -1)
@test Float16(1)*im === Complex32(im)
@test Float16(1)/im === 1.0f0/im === Complex(0.0, -1.0)
@test Float16(1)^im === Complex32(1) === Float16(1)+Float16(0)im

# issue/PR #10148
@test typeof(Int8(1) - im) == Complex{Int8}

# issue #10926
@test typeof(π - 1im) == Complex{Float64}

# issue #11839: type stability for Complex{Int64}
let x = 1+im
    @inferred sin(x)
    @inferred cos(x)
end

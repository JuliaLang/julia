# This file is a part of Julia. License is MIT: http://julialang.org/license

@test reim(2 + 3im) == (2, 3)

for T in (Int64, Float64)
    @test real(T) == T
    @test real(Complex{T}) == T
    @test complex(T) == Complex{T}
    @test complex(Complex{T}) == Complex{T}
end

#showcompact
@test sprint(io -> showcompact(io,complex(1,0))) == "1+0im"
@test sprint(io -> show(io,complex(true,true))) == "Complex(true,true)"

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
@test isequal(true + complex(true,false), complex(true,false) + complex(true,false))
@test isequal(complex(true,false) + true, complex(true,false) + complex(true,false))
@test isequal(true - complex(true,false), complex(true,false) - complex(true,false))
@test isequal(complex(true,false) - true, complex(true,false) - complex(true,false))
@test isequal(true * complex(true,false), complex(true,false) * complex(true,false))
@test isequal(complex(true,false) * true, complex(true,false) * complex(true,false))

# Test math functions. We compare to BigFloat instead of hard-coding
# values, assuming that BigFloat has an independent and independently
# tested implementation.
for T in (Float32, Float64)
    x = Complex{T}(1//3 + 1//4*im)
    y = Complex{T}(1//2 + 1//5*im)
    yi = 4
    # Test random values
    @test x^y ≈ big(x)^big(y)
    @test x^yi ≈ big(x)^yi
    @test x^true ≈ big(x)^true
    @test x^false ≈ big(x)^false
    @test x^1 ≈ big(x)^1
    @test abs(x) ≈ abs(big(x))
    @test abs2(x) ≈ abs2(big(x))
    @test acos(x) ≈ acos(big(x))
    @test acosh(1+x) ≈ acosh(1+big(x))
    @test angle(x) ≈ angle(big(x))
    @test asin(x) ≈ asin(big(x))
    @test asinh(x) ≈ asinh(big(x))
    @test atan(x) ≈ atan(big(x))
    @test atanh(x) ≈ atanh(big(x))
    @test cis(real(x)) ≈ cis(real(big(x)))
    @test cis(x) ≈ cis(big(x))
    @test cos(x) ≈ cos(big(x))
    @test cosh(x) ≈ cosh(big(x))
    @test exp(x) ≈ exp(big(x))
    @test exp10(x) ≈ exp10(big(x))
    @test exp2(x) ≈ exp2(big(x))
    @test_approx_eq_eps expm1(x) expm1(big(x)) eps(T)
    @test log(x) ≈ log(big(x))
    @test log10(x) ≈ log10(big(x))
    @test log1p(x) ≈ log1p(big(x))
    @test log2(x) ≈ log2(big(x))
    @test sin(x) ≈ sin(big(x))
    @test sinh(x) ≈ sinh(big(x))
    @test sqrt(x) ≈ sqrt(big(x))
    @test tan(x) ≈ tan(big(x))
    @test tanh(x) ≈ tanh(big(x))
    # Test inverses
    @test acos(cos(x)) ≈ x
    @test acosh(cosh(x)) ≈ x
    @test asin(sin(x)) ≈ x
    @test asinh(sinh(x)) ≈ x
    @test atan(tan(x)) ≈ x
    @test atanh(tanh(x)) ≈ x
    @test cos(acos(x)) ≈ x
    @test cosh(acosh(1+x)) ≈ 1+x
    @test exp(log(x)) ≈ x
    @test exp10(log10(x)) ≈ x
    @test exp2(log2(x)) ≈ x
    @test expm1(log1p(x)) ≈ x
    @test log(exp(x)) ≈ x
    @test log10(exp10(x)) ≈ x
    @test log1p(expm1(x)) ≈ x
    @test log2(exp2(x)) ≈ x
    @test sin(asin(x)) ≈ x
    @test sinh(asinh(x)) ≈ x
    @test sqrt(x)^2 ≈ x
    @test sqrt(x^2) ≈ x
    @test tan(atan(x)) ≈ x
    @test tanh(atanh(x)) ≈ x
    # Test some properties
    @test cosh(x) ≈ (exp(x)+exp(-x))/2
    @test cosh(x)^2-sinh(x)^2 ≈ 1
    @test sin(x)^2+cos(x)^2 ≈ 1
    @test sinh(x) ≈ (exp(x)-exp(-x))/2
    @test tan(x) ≈ sin(x)/cos(x)
    @test tanh(x) ≈ sinh(x)/cosh(x)
end

#isimag and isinf
@test isimag(complex(0.0,1.0))
@test !isimag(complex(1.0,1.0))
@test isinf(complex(Inf,0))
@test isinf(complex(-Inf,0))
@test isinf(complex(0,Inf))
@test isinf(complex(0,-Inf))
@test !isinf(complex(0,0))

# flipsign:
@test isequal(complex( 0.0, 0.0 ), flipsign(complex( 0.0, 0.0 ), 1))
@test isequal(complex( -0.0, -0.0 ), flipsign(complex( 0.0, 0.0 ), -1))
@test isequal(complex( Inf, 0.0 ), flipsign(complex( Inf, 0.0 ), 1))
@test isequal(complex( -Inf, -0.0 ), flipsign(complex( Inf, 0.0 ), -1))
@test isequal(complex( 0.0, NaN ), flipsign(complex( 0.0, NaN ), 1.0))
@test isequal(complex( -0.0, NaN ), flipsign(complex( 0.0, NaN ), -1.0))

@test isequal(complex( 5.0, 4.0 ), flipsign(complex(-5.0, -4.0), -1))
@test isequal(complex( 0.5, -0.5 ), flipsign(complex(-0.5, 0.5), -2))

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

@test expm1(complex( 1e-20, 1e-10)) ≈ complex( 5e-21, 1e-10)
@test expm1(complex( 1e-20,-1e-10)) ≈ complex( 5e-21,-1e-10)
@test expm1(complex(-1e-20, 1e-10)) ≈ complex(-1.5e-20, 1e-10)
@test expm1(complex(-1e-20,-1e-10)) ≈ complex(-1.5e-20,-1e-10)

@test expm1(complex( 10.0, 10.0)) ≈ exp(complex( 10.0, 10.0))-1
@test expm1(complex( 10.0,-10.0)) ≈ exp(complex( 10.0,-10.0))-1
@test expm1(complex(-10.0, 10.0)) ≈ exp(complex(-10.0, 10.0))-1
@test expm1(complex(-10.0,-10.0)) ≈ exp(complex(-10.0,-10.0))-1

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
    @test log1p(z) ≈ @horner(z, 0, 1, -0.5, 1/3, -0.25, 0.2)
end
for z in (15+4im, 0.2+3im, 0.08-0.9im)
    @test log1p(z) ≈ log(1+z)
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

@test complex(0.0,1.0)^complex(2.0,0) ≈ complex(-1.0, 0.0)
@test complex(1.0,2.0)^complex(3.0,0) ≈ complex(-11.0, -2.0)

@test isequal(complex(0.0,0.0)^false, complex(1.0,0.0))
@test isequal(complex(0.0,0.0)^0, complex(1.0,0.0))

# sinh: has properties
#  sinh(conj(z)) = conj(sinh(z))
#  sinh(-z) = -sinh(z)

# sin: defined in terms of sinh
#  sin(z) = -i*sinh(i*z)
#  i.e. if sinh(a+ib) = x+iy
#    then  sin(b-ia) = y-ix
#  sin(conj(z)) = conj(sin(z))
#  sin(-z) = -sin(z)

# @test isequal(sin(complex( 0, 10000)),complex( 0.0, Inf))
# @test isequal(sin(complex( 0,-10000)),complex( 0.0,-Inf))

for (x,y) in [(complex( 0.0, 0.0), complex( 0.0, 0.0)),
              (complex( 0.0, Inf), complex( 0.0, NaN)),
              (complex( 0.0, NaN), complex( 0.0, NaN)),
              (complex( 7.2, Inf), complex( NaN, NaN)),
              (complex( 7.2, NaN), complex( NaN, NaN)),
              (complex( 7.2, 0.0), complex( sinh(7.2), 0.0)),
              (complex( 1e5, 0.0), complex( sinh(1e5), 0.0)),
              (complex( Inf, 0.0), complex( Inf, 0.0)),
              (complex( Inf, 7.2), Inf*cis(7.2)),
              (complex( Inf, Inf), complex( Inf, NaN)),
              (complex( Inf, NaN), complex( Inf, NaN)),
              (complex( NaN, 0.0), complex( NaN, 0.0)),
              (complex( NaN, 7.2), complex( NaN, NaN)),
              (complex( NaN, NaN), complex( NaN, NaN)),
              ]

    @test isequal(sinh(x), y)
    @test isequal(sinh(conj(x)), conj(y))
    @test isequal(sinh(-x), -y)
    @test isequal(sinh(-conj(x)), -conj(y))

    xx = complex(imag(x),-real(x))
    yy = complex(imag(y),-real(y))

    @test isequal(sin(xx),yy)
    @test isequal(sin(conj(xx)), conj(yy))
    @test isequal(sin(-xx), -yy)
    @test isequal(sin(-conj(xx)), -conj(yy))

    yyy = sin(pi*xx)
    @test isequal(sinpi(xx), yyy)
    @test isequal(sinpi(conj(xx)),conj(yyy))
    @test isequal(sinpi(-xx),-yyy)
    @test isequal(sinpi(-conj(xx)),-conj(yyy))
end

# cosh: has properties
#  cosh(conj(z)) = conj(cosh(z))
#  coshh(-z) = cosh(z)

# cos
#  cos(z) = cosh(iz)
#   i.e cos(b-ia) = cosh(a+ib)
#   and cos(b+ia) = cosh(a-ib)
#  cos(conj(z)) = conj(cos(z))
#  cos(-z) = cos(z)


for (x,y) in [(complex( 0.0, 0.0), complex( 1.0, 0.0)),
              (complex( 0.0, Inf), complex( NaN, 0.0)),
              (complex( 0.0, NaN), complex( NaN, 0.0)),
              (complex( 7.2, Inf), complex( NaN, NaN)),
              (complex( 7.2, NaN), complex( NaN, NaN)),
              (complex( 7.2, 0.0), complex( cosh(7.2), 0.0)),
              (complex( 1e5, 0.0), complex( Inf, 0.0)),
              (complex( Inf, 0.0), complex( Inf, 0.0)),
              (complex( Inf, 7.2), Inf*cis(7.2)),
              (complex( Inf, Inf), complex( Inf, NaN)),
              (complex( Inf, NaN), complex( Inf, NaN)),
              (complex( NaN, 0.0), complex( NaN, 0.0)),
              (complex( NaN, 7.2), complex( NaN, NaN)),
              (complex( NaN, NaN), complex( NaN, NaN)),
              ]
    undef_sign = isequal(x,complex( NaN, 0.0)) || isequal(x,complex( 0.0, NaN))

    @test isequal(cosh(x), y)
    if !undef_sign
        @test isequal(cosh(conj(x)), conj(y))
        @test isequal(cosh(-x), y)
        @test isequal(cosh(-conj(x)), conj(y))
    end

    xx = complex(imag(x),-real(x))
    yy = y
    @test isequal(cos(xx),yy)
    if !undef_sign
        @test isequal(cos(conj(xx)), conj(yy))
        @test isequal(cos(-xx), yy)
        @test isequal(cos(-conj(xx)), conj(yy))
    end

    yyy = cos(pi*xx)
    @test isequal(cospi(xx), yyy)
    if !undef_sign
        @test isequal(cospi(conj(xx)), conj(yyy))
        @test isequal(cospi(-xx), yyy)
        @test isequal(cospi(-conj(xx)), conj(yyy))
    end
end


# tanh
#  tanh(conj(z)) = conj(tanh(z))
#  tanh(-z) = -tanh(z)
@test isequal(tanh(complex( 0, 0)),complex(0.0,0.0)) #integer fallback
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

## acos
##  acos(conj(z)) = conj(acos(z))

@test isequal(acos(complex( 0, 0)),complex(pi/2,-0.0)) #integer fallback
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

@test isequal(atanh(complex( 0, 0)),complex( 0.0, 0.0)) #integer fallback
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

for T in (Float16, Float32, Float64)
    z = Complex(zero(T), zero(T))
    @test sign(z) === z
    @test sign(-z) === -z
    @test sign(conj(z)) === conj(z)
    @test sign(-conj(z)) === -conj(z)
end

# cis
@test cis(0.0+1.0im) ≈ 0.367879441171442321595523770161460867445811131031767834507836+0.0im
@test cis(1.0+0.0im) ≈ 0.54030230586813971740093660744297660373231042061+0.84147098480789650665250232163029899962256306079im
@test cis(pi) ≈ -1.0+0.0im
@test cis(pi/2) ≈ 0.0+1.0im

# exp2
@test exp2(0.0+0.0im) == 1.0+0.0im
@test exp2(1.0+0.0im) == 2.0+0.0im
#wolframalpha
@test exp2(1.0+3.0im) ≈ -0.9739888359315627962096198412+1.74681016354974281701922im

# exp10
@test exp10(0.0+0.0im) == 1.0+0.0im
@test exp10(1.0+0.0im) == 10.0+0.0im
#wolframalpha
@test exp10(1.0+2.0im) ≈ -1.0701348355877020772086517528518239460495529361-9.9425756941378968736161937190915602112878340717im

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

# issue #15969: specialized muladd for complex types
for x in (3, 3+13im), y in (2, 2+7im), z in (5, 5+11im)
    @test muladd(x,y,z) === x*y + z
end

# issue #11839: type stability for Complex{Int64}
let x = 1+im
    @inferred sin(x)
    @inferred cos(x)
end

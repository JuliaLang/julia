# real constructors
with_bigcomplex_precision(53,53) do
	x = MPCComplex()
end
x = MPCComplex(12)
y = MPCComplex(x)
@test x == y
y = MPCComplex(0xc)
@test x == y
y = MPCComplex(12.)
@test x == y
y = MPCComplex(BigInt(12))
@test x == y
y = MPCComplex(BigFloat(12))
@test x == y
y = MPCComplex(MPFRFloat(12))
@test x == y
y = MPCComplex("12")
@test x == y
y = MPCComplex(float32(12.))
@test x == y
y = MPCComplex(12//1)
@test x == y

# complex constructors
x = MPCComplex(12, 42)
y = MPCComplex(x)
@test x == y
y = MPCComplex(0xc, 0x2a)
@test x == y
y = MPCComplex(12., 42.)
@test x == y
y = MPCComplex(BigInt(12), BigInt(42))
@test x == y
y = MPCComplex(BigFloat(12), BigFloat(42))
@test x == y
y = MPCComplex(MPFRFloat(12), MPFRFloat(42))
@test x == y
y = MPCComplex("(12 42)")
@test x == y
y = MPCComplex(float32(12.), float32(42))
@test x == y
y = MPCComplex(12//1, 42//1)
@test x == y
y = MPCComplex(12 + 42im)
@test x == y

# real/imag
x = MPCComplex(12, 42)
y = MPFRFloat(12)
z = MPFRFloat(42)
@test real(x) == y
@test imag(x) == z
y = MPCComplex(x)
@test real(x) == real(y)
@test imag(x) == imag(y)

# conversion
with_bigcomplex_precision(53) do
    x = MPCComplex(12, 42)
    @test typeof(convert(Complex{Float64}, x)) == Complex{Float64}
    @test typeof(convert(Complex{Float32}, x)) == Complex{Float32}
    @test typeof(convert(Complex{Int64}, x)) == Complex{Int64}
    @test typeof(convert(Complex{Int32}, x)) == Complex{Int32}
    @test 12. + 42.0im == convert(Complex{Float64}, x)
    @test 12f0 + 42f0im == convert(Complex{Float32}, x)
    @test 12 + 42im == convert(Complex{Int64}, x)
    @test 12 + 42im == convert(Complex{Int32}, x)
end

# +
x = MPCComplex(12,42)
@test (x + x) == MPCComplex(24, 84)
@test (x + 2) == MPCComplex(14, 42)
@test (x + (2+ 1im)) == MPCComplex(14, 43)
@test (x + 2 + 1im) == MPCComplex(14, 43)

# integer_valued
@test !integer_valued(MPCComplex(2,3))
@test integer_valued(MPCComplex(2))

# Kahan tests from #2845 by @simonbyrne
with_bigcomplex_precision(53,53) do 
# sqrt: 
# tests special values from csqrt man page
# as well as conj(sqrt(z)) = sqrt(conj(z))
@test isequal(sqrt(MPCComplex(0.0,0.0)), MPCComplex(0.0,0.0))
@test isequal(sqrt(MPCComplex(-0.0,0.0)), MPCComplex(0.0,0.0))
@test isequal(sqrt(MPCComplex(0.0,-0.0)), MPCComplex(0.0,-0.0))
@test isequal(sqrt(MPCComplex(-0.0,-0.0)), MPCComplex(0.0,-0.0))

@test isequal(sqrt(MPCComplex(5.0,0.0)), MPCComplex(sqrt(5.0),0.0))
@test isequal(sqrt(MPCComplex(5.0,-0.0)), MPCComplex(sqrt(5.0),-0.0))

@test isequal(sqrt(MPCComplex(-5.0,0.0)), MPCComplex(0.0,sqrt(5.0)))
@test isequal(sqrt(MPCComplex(-5.0,-0.0)), MPCComplex(0.0,-sqrt(5.0)))

@test isequal(sqrt(MPCComplex(0.0,Inf)), MPCComplex(Inf,Inf))
@test isequal(sqrt(MPCComplex(0.0,-Inf)), MPCComplex(Inf,-Inf))

@test isequal(sqrt(MPCComplex(NaN,Inf)), MPCComplex(Inf,Inf))
@test isequal(sqrt(MPCComplex(NaN,-Inf)), MPCComplex(Inf,-Inf))

@test isequal(sqrt(MPCComplex(Inf,Inf)), MPCComplex(Inf,Inf))
@test isequal(sqrt(MPCComplex(Inf,-Inf)), MPCComplex(Inf,-Inf))
@test isequal(sqrt(MPCComplex(-Inf,Inf)), MPCComplex(Inf,Inf))
@test isequal(sqrt(MPCComplex(-Inf,-Inf)), MPCComplex(Inf,-Inf))

@test isequal(sqrt(MPCComplex(0.0,NaN)), MPCComplex(NaN,NaN))

@test isequal(sqrt(MPCComplex(-Inf,0.0)), MPCComplex(0.0,Inf))
@test isequal(sqrt(MPCComplex(-Inf,5.0)), MPCComplex(0.0,Inf))
@test isequal(sqrt(MPCComplex(-Inf,-0.0)), MPCComplex(0.0,-Inf))
@test isequal(sqrt(MPCComplex(-Inf,-5.0)), MPCComplex(0.0,-Inf))

@test isequal(sqrt(MPCComplex(Inf,0.0)), MPCComplex(Inf,0.0))
@test isequal(sqrt(MPCComplex(Inf,5.0)), MPCComplex(Inf,0.0))
@test isequal(sqrt(MPCComplex(Inf,-0.0)), MPCComplex(Inf,-0.0))
@test isequal(sqrt(MPCComplex(Inf,-5.0)), MPCComplex(Inf,-0.0))

@test isequal(sqrt(MPCComplex(-Inf,NaN)), MPCComplex(NaN,Inf))
@test isequal(sqrt(MPCComplex(Inf,NaN)), MPCComplex(Inf,NaN))

@test isequal(sqrt(MPCComplex(NaN,0.0)), MPCComplex(NaN,NaN))
@test isequal(sqrt(MPCComplex(NaN,0.0)), MPCComplex(NaN,NaN))



# log: 
#  log(conj(z)) = conj(log(z))
@test isequal(log(MPCComplex(5.0,0.0)),MPCComplex(log(5.0),0.0))
@test isequal(log(MPCComplex(5.0,-0.0)),MPCComplex(log(5.0),-0.0))

@test isequal(log(MPCComplex(0.0,1.0)),MPCComplex(0.0,pi/2))
@test isequal(log(MPCComplex(0.0,-1.0)),MPCComplex(0.0,-pi/2))

# special values

# raise divide-by-zero flag
@test isequal(log(MPCComplex(-0.0,0.0)), MPCComplex(-Inf,pi))
@test isequal(log(MPCComplex(-0.0,-0.0)), MPCComplex(-Inf,-pi))

# raise divide-by-zero flag
@test isequal(log(MPCComplex(0.0,0.0)), MPCComplex(-Inf,0.0))
@test isequal(log(MPCComplex(0.0,-0.0)), MPCComplex(-Inf,-0.0))

@test isequal(log(MPCComplex(0.0,Inf)), MPCComplex(Inf,pi/2))
@test isequal(log(MPCComplex(0.0,-Inf)), MPCComplex(Inf,-pi/2))

@test isequal(log(MPCComplex(0.0,NaN)), MPCComplex(NaN,NaN))

@test isequal(log(MPCComplex(-Inf,5.0)), MPCComplex(Inf,pi))
@test isequal(log(MPCComplex(-Inf,-5.0)), MPCComplex(Inf,-pi))

@test isequal(log(MPCComplex(Inf,5.0)), MPCComplex(Inf,0.0))
@test isequal(log(MPCComplex(Inf,-5.0)), MPCComplex(Inf,-0.0))

@test isequal(log(MPCComplex(-Inf,Inf)), MPCComplex(Inf,3*pi/4))
@test isequal(log(MPCComplex(-Inf,-Inf)), MPCComplex(Inf,-3*pi/4))

@test isequal(log(MPCComplex(Inf,Inf)), MPCComplex(Inf,pi/4))
@test isequal(log(MPCComplex(Inf,-Inf)), MPCComplex(Inf,-pi/4))

@test isequal(log(MPCComplex(Inf,NaN)), MPCComplex(Inf,NaN))
@test isequal(log(MPCComplex(-Inf,NaN)), MPCComplex(Inf,NaN))

@test isequal(log(MPCComplex(NaN,0.0)), MPCComplex(NaN,NaN))

@test isequal(log(MPCComplex(NaN,Inf)), MPCComplex(Inf,NaN))
@test isequal(log(MPCComplex(NaN,-Inf)), MPCComplex(Inf,NaN))

@test isequal(log(MPCComplex(NaN,NaN)), MPCComplex(NaN,NaN))

# exp:
#  exp(conj(z)) = conj(exp(z))

@test isequal(exp(MPCComplex(0.0,0.0)), MPCComplex(1.0,0.0))
@test isequal(exp(MPCComplex(0.0,-0.0)), MPCComplex(1.0,-0.0))
@test isequal(exp(MPCComplex(-0.0,0.0)), MPCComplex(1.0,0.0))
@test isequal(exp(MPCComplex(-0.0,-0.0)), MPCComplex(1.0,-0.0))

# raise invalid flag
@test isequal(exp(MPCComplex(0.0,Inf)), MPCComplex(NaN,NaN))
@test isequal(exp(MPCComplex(0.0,-Inf)), MPCComplex(NaN,NaN))
@test isequal(exp(MPCComplex(5.0,Inf)), MPCComplex(NaN,NaN))

@test isequal(exp(MPCComplex(0.0,NaN)), MPCComplex(NaN,NaN))

@test isequal(exp(MPCComplex(Inf,0.0)), MPCComplex(Inf,0.0))
@test isequal(exp(MPCComplex(Inf,-0.0)), MPCComplex(Inf,-0.0))

@test isequal(exp(MPCComplex(-Inf,0.0)), MPCComplex(0.0,0.0))
@test isequal(exp(MPCComplex(-Inf,-0.0)), MPCComplex(0.0,-0.0))
@test isequal(exp(MPCComplex(-Inf,5.0)), MPCComplex(cos(5.0)*0.0,sin(5.0)*0.0))
@test isequal(exp(MPCComplex(-Inf,-5.0)), MPCComplex(cos(5.0)*0.0,sin(5.0)*-0.0))

@test isequal(exp(MPCComplex(Inf,5.0)), MPCComplex(cos(5.0)*Inf,sin(5.0)*Inf))
@test isequal(exp(MPCComplex(Inf,-5.0)), MPCComplex(cos(5.0)*Inf,sin(5.0)*-Inf))

@test isequal(exp(MPCComplex(-Inf,Inf)), MPCComplex(-0.0,0.0))
@test isequal(exp(MPCComplex(-Inf,-Inf)), MPCComplex(-0.0,-0.0))

# raise invalid flag
# TODO: The left side evaluates to MPCComplex(Inf, NaN)
#@test isequal(exp(MPCComplex(Inf,Inf)), MPCComplex(-Inf,NaN))
#@test isequal(exp(MPCComplex(Inf,-Inf)), MPCComplex(-Inf,NaN))

@test isequal(exp(MPCComplex(-Inf,NaN)), MPCComplex(-0.0,0.0))

# TODO: The left side evaluates to MPCComplex(Inf, NaN)
#@test isequal(exp(MPCComplex(Inf,NaN)), MPCComplex(-Inf,NaN))

@test isequal(exp(MPCComplex(NaN,0.0)), MPCComplex(NaN,0.0))
@test isequal(exp(MPCComplex(NaN,-0.0)), MPCComplex(NaN,-0.0))

@test isequal(exp(MPCComplex(NaN,5.0)), MPCComplex(NaN,NaN))

@test isequal(exp(MPCComplex(NaN,NaN)), MPCComplex(NaN,NaN))

# ^ (cpow)
#  equivalent to exp(y*log(x))
#    except for 0^0?
#  conj(x)^conj(y) = conj(x^y)
@test isequal(MPCComplex(0.0,0.0)^MPCComplex(0.0,0.0), MPCComplex(1.0,-0.0))
@test isequal(MPCComplex(0.0,-0.0)^MPCComplex(0.0,0.0), MPCComplex(1.0,-0.0))
@test isequal(MPCComplex(0.0,0.0)^MPCComplex(0.0,-0.0), MPCComplex(1.0,0.0))
@test isequal(MPCComplex(0.0,-0.0)^MPCComplex(0.0,-0.0), MPCComplex(1.0,0.0))
@test isequal(MPCComplex(-0.0,0.0)^MPCComplex(0.0,0.0), MPCComplex(1.0,-0.0))
@test isequal(MPCComplex(-0.0,-0.0)^MPCComplex(0.0,0.0), MPCComplex(1.0,-0.0))
@test isequal(MPCComplex(-0.0,0.0)^MPCComplex(0.0,-0.0), MPCComplex(1.0,0.0))
@test isequal(MPCComplex(-0.0,-0.0)^MPCComplex(0.0,-0.0), MPCComplex(1.0,0.0))
@test isequal(MPCComplex(0.0,0.0)^MPCComplex(-0.0,0.0), MPCComplex(1.0,-0.0))
@test isequal(MPCComplex(0.0,-0.0)^MPCComplex(-0.0,0.0), MPCComplex(1.0,-0.0))
@test isequal(MPCComplex(0.0,0.0)^MPCComplex(-0.0,-0.0), MPCComplex(1.0,0.0))
@test isequal(MPCComplex(0.0,-0.0)^MPCComplex(-0.0,-0.0), MPCComplex(1.0,0.0))
@test isequal(MPCComplex(-0.0,0.0)^MPCComplex(-0.0,0.0), MPCComplex(1.0,-0.0))
@test isequal(MPCComplex(-0.0,-0.0)^MPCComplex(-0.0,0.0), MPCComplex(1.0,-0.0))
@test isequal(MPCComplex(-0.0,0.0)^MPCComplex(-0.0,-0.0), MPCComplex(1.0,0.0))
@test isequal(MPCComplex(-0.0,-0.0)^MPCComplex(-0.0,-0.0), MPCComplex(1.0,0.0))




# sinh: has properties
#  sinh(conj(z)) = conj(sinh(z))
#  sinh(-z) = -sinh(z)

@test isequal(sinh(MPCComplex(0.0,0.0)),MPCComplex(0.0,0.0))
@test isequal(sinh(MPCComplex(0.0,-0.0)),MPCComplex(0.0,-0.0))
@test isequal(sinh(MPCComplex(-0.0,0.0)),MPCComplex(-0.0,0.0))
@test isequal(sinh(MPCComplex(-0.0,-0.0)),MPCComplex(-0.0,-0.0))

# raise invalid flag
@test isequal(sinh(MPCComplex(0.0,Inf)), MPCComplex(0.0,NaN))
@test isequal(sinh(MPCComplex(0.0,-Inf)), MPCComplex(0.0,NaN))
@test isequal(sinh(MPCComplex(-0.0,Inf)), MPCComplex(-0.0,NaN))
@test isequal(sinh(MPCComplex(-0.0,-Inf)), MPCComplex(-0.0,NaN))

@test isequal(sinh(MPCComplex(0.0,NaN)),MPCComplex(0.0,NaN))
@test isequal(sinh(MPCComplex(-0.0,NaN)),MPCComplex(-0.0,NaN))

# raise invalid flag
@test isequal(sinh(MPCComplex(5.0,Inf)), MPCComplex(NaN,NaN))

@test isequal(sinh(MPCComplex(5.0,NaN)),MPCComplex(NaN,NaN))

@test isequal(sinh(MPCComplex(Inf,0.0)),MPCComplex(Inf,0.0))
@test isequal(sinh(MPCComplex(Inf,-0.0)),MPCComplex(Inf,-0.0))
@test isequal(sinh(MPCComplex(-Inf,0.0)),MPCComplex(-Inf,0.0))
@test isequal(sinh(MPCComplex(-Inf,-0.0)),MPCComplex(-Inf,-0.0))

@test isequal(sinh(MPCComplex(Inf,5.0)),MPCComplex(cos(5.0)*Inf,sin(5.0)*Inf))
@test isequal(sinh(MPCComplex(-Inf,5.0)),MPCComplex(cos(5.0)*-Inf,sin(5.0)*Inf))

# raise invalid flag
@test isequal(sinh(MPCComplex(Inf,Inf)), MPCComplex(Inf,NaN))
@test isequal(sinh(MPCComplex(Inf,-Inf)), MPCComplex(Inf,NaN))
@test isequal(sinh(MPCComplex(-Inf,Inf)), MPCComplex(-Inf,NaN))
@test isequal(sinh(MPCComplex(-Inf,-Inf)), MPCComplex(-Inf,NaN))

@test isequal(sinh(MPCComplex(Inf,NaN)),MPCComplex(Inf,NaN))
@test isequal(sinh(MPCComplex(-Inf,NaN)),MPCComplex(-Inf,NaN))

@test isequal(sinh(MPCComplex(NaN,0.0)),MPCComplex(NaN,0.0))
@test isequal(sinh(MPCComplex(NaN,-0.0)),MPCComplex(NaN,-0.0))

@test isequal(sinh(MPCComplex(NaN,5.0)),MPCComplex(NaN,NaN))

@test isequal(sinh(MPCComplex(NaN,NaN)),MPCComplex(NaN,NaN))



# sin: defined in terms of sinh
#  sin(z) = -i*sinh(i*z)
#  i.e. if sinh(a+ib) = x+iy
#    then  sin(b-ia) = y-ix
#  sin(conj(z)) = conj(sin(z))
#  sin(-z) = -sin(z)


@test isequal(sin(MPCComplex(0.0,-0.0)),MPCComplex(0.0,-0.0))
@test isequal(sin(MPCComplex(-0.0,-0.0)),MPCComplex(-0.0,-0.0))
@test isequal(sin(MPCComplex(0.0,0.0)),MPCComplex(0.0,0.0))
@test isequal(sin(MPCComplex(-0.0,0.0)),MPCComplex(-0.0,0.0))

# raise invalid flag
@test isequal(sin(MPCComplex(Inf,0.0)), MPCComplex(NaN,0.0))
@test isequal(sin(MPCComplex(-Inf,0.0)), MPCComplex(NaN,0.0))
@test isequal(sin(MPCComplex(Inf,-0.0)), MPCComplex(NaN,-0.0))
@test isequal(sin(MPCComplex(-Inf,-0.0)), MPCComplex(NaN,-0.0))

@test isequal(sin(MPCComplex(NaN,0.0)),MPCComplex(NaN,0.0))
@test isequal(sin(MPCComplex(NaN,-0.0)),MPCComplex(NaN,-0.0))

# raise invalid flag
@test isequal(sin(MPCComplex(Inf,5.0)), MPCComplex(NaN,NaN))

@test isequal(sin(MPCComplex(NaN,5.0)),MPCComplex(NaN,NaN))

@test isequal(sin(MPCComplex(0.0,Inf)),MPCComplex(0.0,Inf))
@test isequal(sin(MPCComplex(-0.0,Inf)),MPCComplex(-0.0,Inf))
@test isequal(sin(MPCComplex(0.0,-Inf)),MPCComplex(0.0,-Inf))
@test isequal(sin(MPCComplex(-0.0,-Inf)),MPCComplex(-0.0,-Inf))

@test isequal(sin(MPCComplex(5.0,Inf)),MPCComplex(sin(5.0)*Inf,cos(5.0)*Inf))
@test isequal(sin(MPCComplex(5.0,-Inf)),MPCComplex(sin(5.0)*Inf,cos(5.0)*-Inf))

# raise invalid flag
@test isequal(sin(MPCComplex(Inf,Inf)), MPCComplex(Inf,NaN))
@test isequal(sin(MPCComplex(Inf,-Inf)), MPCComplex(Inf,NaN))
@test isequal(sin(MPCComplex(-Inf,Inf)), MPCComplex(-Inf,NaN))
@test isequal(sin(MPCComplex(-Inf,-Inf)), MPCComplex(-Inf,NaN))

@test isequal(sin(MPCComplex(NaN,Inf)),MPCComplex(NaN,Inf))
@test isequal(sin(MPCComplex(NaN,-Inf)),MPCComplex(NaN,-Inf))

@test isequal(sin(MPCComplex(0.0,NaN)),MPCComplex(0.0,NaN))
@test isequal(sin(MPCComplex(-0.0,NaN)),MPCComplex(-0.0,NaN))

@test isequal(sin(MPCComplex(5.0,NaN)),MPCComplex(NaN,NaN))

@test isequal(sin(MPCComplex(NaN,NaN)),MPCComplex(NaN,NaN))






# cosh: has properties
#  cosh(conj(z)) = conj(cosh(z))
#  coshh(-z) = cosh(z)

@test isequal(cosh(MPCComplex(0.0,0.0)),MPCComplex(1.0,0.0))
@test isequal(cosh(MPCComplex(0.0,-0.0)),MPCComplex(1.0,-0.0))
@test isequal(cosh(MPCComplex(-0.0,-0.0)),MPCComplex(1.0,0.0))
@test isequal(cosh(MPCComplex(-0.0,0.0)),MPCComplex(1.0,-0.0))

# raise invalid flag
@test isequal(cosh(MPCComplex(0.0,Inf)), MPCComplex(NaN,0.0))
@test isequal(cosh(MPCComplex(0.0,-Inf)), MPCComplex(NaN,-0.0))

@test isequal(cosh(MPCComplex(0.0,NaN)),MPCComplex(NaN,0.0))
@test isequal(cosh(MPCComplex(-0.0,NaN)),MPCComplex(NaN,0.0))

# raise invalid flag
@test isequal(cosh(MPCComplex(5.0,Inf)), MPCComplex(NaN,NaN))

@test isequal(cosh(MPCComplex(5.0,NaN)),MPCComplex(NaN,NaN))

@test isequal(cosh(MPCComplex(Inf,0.0)),MPCComplex(Inf,0.0))
@test isequal(cosh(MPCComplex(Inf,-0.0)),MPCComplex(Inf,-0.0))
@test isequal(cosh(MPCComplex(-Inf,-0.0)),MPCComplex(Inf,0.0))
@test isequal(cosh(MPCComplex(-Inf,0.0)),MPCComplex(Inf,-0.0))

@test isequal(cosh(MPCComplex(Inf,5.0)),MPCComplex(cos(5.0)*Inf,sin(5.0)*Inf))
@test isequal(cosh(MPCComplex(Inf,-5.0)),MPCComplex(cos(5.0)*Inf,sin(5.0)*-Inf))
@test isequal(cosh(MPCComplex(-Inf,-5.0)),MPCComplex(cos(5.0)*Inf,sin(5.0)*Inf))
@test isequal(cosh(MPCComplex(-Inf,5.0)),MPCComplex(cos(5.0)*Inf,sin(5.0)*-Inf))

# raise invalid flag
@test isequal(cosh(MPCComplex(Inf,Inf)), MPCComplex(Inf,NaN))
# TODO: The left side evaluates to MPCComplex(-Inf, NaN)
#@test isequal(cosh(MPCComplex(Inf,-Inf)), MPCComplex(Inf,NaN))
@test isequal(cosh(MPCComplex(-Inf,-Inf)), MPCComplex(Inf,NaN))
# TODO: The left side evaluates to MPCComplex(-Inf, NaN)
#@test isequal(cosh(MPCComplex(-Inf,Inf)), MPCComplex(Inf,NaN))

@test isequal(cosh(MPCComplex(Inf,NaN)),MPCComplex(Inf,NaN))
@test isequal(cosh(MPCComplex(-Inf,NaN)),MPCComplex(Inf,NaN))

@test isequal(cosh(MPCComplex(NaN,0.0)),MPCComplex(NaN,0.0))
@test isequal(cosh(MPCComplex(NaN,-0.0)),MPCComplex(NaN,-0.0))

@test isequal(cosh(MPCComplex(NaN,5.0)),MPCComplex(NaN,NaN))
@test isequal(cosh(MPCComplex(NaN,-5.0)),MPCComplex(NaN,NaN))

@test isequal(cosh(MPCComplex(NaN,NaN)),MPCComplex(NaN,NaN))


# cos
#  cos(z) = cosh(iz)
#   i.e cos(b-ia) = cosh(a+ib)
#   and cos(b+ia) = cosh(a-ib)
#  cos(conj(z)) = conj(cos(z))
#  cos(-z) = cos(z)

@test isequal(cos(MPCComplex(0.0,0.0)),MPCComplex(1.0,-0.0))
@test isequal(cos(MPCComplex(0.0,-0.0)),MPCComplex(1.0,0.0))
@test isequal(cos(MPCComplex(-0.0,0.0)),MPCComplex(1.0,0.0))
@test isequal(cos(MPCComplex(-0.0,-0.0)),MPCComplex(1.0,-0.0))

# raise invalid flag
@test isequal(cos(MPCComplex(Inf,0.0)), MPCComplex(NaN,-0.0))
@test isequal(cos(MPCComplex(-Inf,0.0)), MPCComplex(NaN,0.0))

@test isequal(cos(MPCComplex(NaN,0.0)),MPCComplex(NaN,0.0))
@test isequal(cos(MPCComplex(NaN,-0.0)),MPCComplex(NaN,0.0))

# raise invalid flag
@test isequal(cos(MPCComplex(Inf,5.0)), MPCComplex(NaN,NaN))

@test isequal(cos(MPCComplex(NaN,5.0)),MPCComplex(NaN,NaN))

@test isequal(cos(MPCComplex(0.0,-Inf)),MPCComplex(Inf,0.0))
@test isequal(cos(MPCComplex(-0.0,-Inf)),MPCComplex(Inf,-0.0))
@test isequal(cos(MPCComplex(-0.0,Inf)),MPCComplex(Inf,0.0))
@test isequal(cos(MPCComplex(0.0,Inf)),MPCComplex(Inf,-0.0))

@test isequal(cos(MPCComplex(5.0,-Inf)),MPCComplex(cos(5.0)*Inf,sin(5.0)*Inf))
@test isequal(cos(MPCComplex(-5.0,-Inf)),MPCComplex(cos(5.0)*Inf,sin(5.0)*-Inf))
@test isequal(cos(MPCComplex(-5.0,Inf)),MPCComplex(cos(5.0)*Inf,sin(5.0)*Inf))
@test isequal(cos(MPCComplex(5.0,Inf)),MPCComplex(cos(5.0)*Inf,sin(5.0)*-Inf))

# raise invalid flag
# TODO: The left side evaluates to MPCComplex(-Inf, NaN)
#@test isequal(cos(MPCComplex(Inf,Inf)), MPCComplex(Inf,NaN))
@test isequal(cos(MPCComplex(Inf,-Inf)), MPCComplex(Inf,NaN))
# TODO: The left side evaluates to MPCComplex(-Inf, NaN)
#@test isequal(cos(MPCComplex(-Inf,-Inf)), MPCComplex(Inf,NaN))
@test isequal(cos(MPCComplex(-Inf,Inf)), MPCComplex(Inf,NaN))

@test isequal(cos(MPCComplex(NaN,Inf)),MPCComplex(Inf,NaN))
@test isequal(cos(MPCComplex(NaN,-Inf)),MPCComplex(Inf,NaN))

@test isequal(cos(MPCComplex(0.0,NaN)),MPCComplex(NaN,0.0))
@test isequal(cos(MPCComplex(-0.0,NaN)),MPCComplex(NaN,-0.0))

@test isequal(cos(MPCComplex(5.0,NaN)),MPCComplex(NaN,NaN))
@test isequal(cos(MPCComplex(-5.0,NaN)),MPCComplex(NaN,NaN))

@test isequal(cos(MPCComplex(NaN,NaN)),MPCComplex(NaN,NaN))




# tanh
#  tanh(conj(z)) = conj(tanh(z))
#  tanh(-z) = -tanh(z)
@test isequal(tanh(MPCComplex(0.0,0.0)),MPCComplex(0.0,0.0))
@test isequal(tanh(MPCComplex(0.0,-0.0)),MPCComplex(0.0,-0.0))
@test isequal(tanh(MPCComplex(-0.0,0.0)),MPCComplex(-0.0,0.0))
@test isequal(tanh(MPCComplex(-0.0,-0.0)),MPCComplex(-0.0,-0.0))

# raise invalid flag
@test isequal(tanh(MPCComplex(0.0,Inf)), MPCComplex(NaN,0.0))
@test isequal(tanh(MPCComplex(0.0,-Inf)), MPCComplex(NaN,-0.0))

@test isequal(tanh(MPCComplex(0.0,NaN)),MPCComplex(NaN,NaN))

# raise invalid flag
@test isequal(tanh(MPCComplex(5.0,Inf)), MPCComplex(NaN,NaN))

@test isequal(tanh(MPCComplex(5.0,NaN)),MPCComplex(NaN,NaN))

@test isequal(tanh(MPCComplex(Inf,0.0)),MPCComplex(1.0,0.0))
@test isequal(tanh(MPCComplex(Inf,-0.0)),MPCComplex(1.0,-0.0))
@test isequal(tanh(MPCComplex(-Inf,0.0)),MPCComplex(-1.0,0.0))
@test isequal(tanh(MPCComplex(-Inf,-0.0)),MPCComplex(-1.0,-0.0))

@test isequal(tanh(MPCComplex(Inf,5.0)),MPCComplex(1.0,sin(2*5.0)*0.0))
@test isequal(tanh(MPCComplex(Inf,-5.0)),MPCComplex(1.0,sin(2*5.0)*-0.0))
@test isequal(tanh(MPCComplex(-Inf,5.0)),MPCComplex(-1.0,sin(2*5.0)*0.0))
@test isequal(tanh(MPCComplex(-Inf,-5.0)),MPCComplex(-1.0,sin(2*5.0)*-0.0))

@test isequal(tanh(MPCComplex(Inf,Inf)),MPCComplex(1.0,0.0))
@test isequal(tanh(MPCComplex(Inf,-Inf)),MPCComplex(1.0,-0.0))
@test isequal(tanh(MPCComplex(-Inf,Inf)),MPCComplex(-1.0,0.0))
@test isequal(tanh(MPCComplex(-Inf,-Inf)),MPCComplex(-1.0,-0.0))

@test isequal(tanh(MPCComplex(Inf,NaN)),MPCComplex(1.0,0.0))
@test isequal(tanh(MPCComplex(-Inf,NaN)),MPCComplex(-1.0,0.0))

@test isequal(tanh(MPCComplex(NaN,0.0)),MPCComplex(NaN,0.0))
@test isequal(tanh(MPCComplex(NaN,-0.0)),MPCComplex(NaN,-0.0))

@test isequal(tanh(MPCComplex(NaN,5.0)),MPCComplex(NaN,NaN))
@test isequal(tanh(MPCComplex(NaN,-5.0)),MPCComplex(NaN,NaN))

@test isequal(tanh(MPCComplex(NaN,NaN)),MPCComplex(NaN,NaN))

# tan
#  tan(z) = -i tanh(iz)
@test isequal(tan(MPCComplex(Inf,5.0)), MPCComplex(NaN,NaN))

@test isequal(tan(MPCComplex(NaN,5.0)),MPCComplex(NaN,NaN))

@test isequal(tan(MPCComplex(0.0,Inf)),MPCComplex(0.0,1.0))
@test isequal(tan(MPCComplex(-0.0,Inf)),MPCComplex(-0.0,1.0))
@test isequal(tan(MPCComplex(0.0,-Inf)),MPCComplex(0.0,-1.0))
@test isequal(tan(MPCComplex(-0.0,-Inf)),MPCComplex(-0.0,-1.0))

@test isequal(tan(MPCComplex(5.0,Inf)),MPCComplex(sin(2*5.0)*0.0,1.0))
@test isequal(tan(MPCComplex(-5.0,Inf)),MPCComplex(sin(2*5.0)*-0.0,1.0))
@test isequal(tan(MPCComplex(5.0,-Inf)),MPCComplex(sin(2*5.0)*0.0,-1.0))
@test isequal(tan(MPCComplex(-5.0,-Inf)),MPCComplex(sin(2*5.0)*-0.0,-1.0))

@test isequal(tan(MPCComplex(Inf,Inf)),MPCComplex(0.0,1.0))
@test isequal(tan(MPCComplex(-Inf,Inf)),MPCComplex(-0.0,1.0))
@test isequal(tan(MPCComplex(Inf,-Inf)),MPCComplex(0.0,-1.0))
@test isequal(tan(MPCComplex(-Inf,-Inf)),MPCComplex(-0.0,-1.0))

@test isequal(tan(MPCComplex(NaN,Inf)),MPCComplex(0.0,1.0))
@test isequal(tan(MPCComplex(NaN,-Inf)),MPCComplex(0.0,-1.0))

@test isequal(tan(MPCComplex(0.0,NaN)),MPCComplex(0.0,NaN))
@test isequal(tan(MPCComplex(-0.0,NaN)),MPCComplex(-0.0,NaN))

@test isequal(tan(MPCComplex(5.0,NaN)),MPCComplex(NaN,NaN))
@test isequal(tan(MPCComplex(-5.0,NaN)),MPCComplex(NaN,NaN))

@test isequal(tan(MPCComplex(NaN,NaN)),MPCComplex(NaN,NaN))




# acosh
#  acosh(conj(z)) = conj(acosh(z))
@test isequal(acosh(MPCComplex(0.0,0.0)),MPCComplex(0.0,pi/2))
@test isequal(acosh(MPCComplex(0.0,-0.0)),MPCComplex(0.0,-pi/2))
@test isequal(acosh(MPCComplex(-0.0,0.0)),MPCComplex(0.0,pi/2))
@test isequal(acosh(MPCComplex(-0.0,-0.0)),MPCComplex(0.0,-pi/2))

@test isequal(acosh(MPCComplex(0.0,Inf)),MPCComplex(Inf,pi/2))
@test isequal(acosh(MPCComplex(0.0,-Inf)),MPCComplex(Inf,-pi/2))
@test isequal(acosh(MPCComplex(5.0,Inf)),MPCComplex(Inf,pi/2))
@test isequal(acosh(MPCComplex(5.0,-Inf)),MPCComplex(Inf,-pi/2))

@test isequal(acosh(MPCComplex(5.0,NaN)),MPCComplex(NaN,NaN))

@test isequal(acosh(MPCComplex(-Inf,0.0)),MPCComplex(Inf,pi))
@test isequal(acosh(MPCComplex(-Inf,-0.0)),MPCComplex(Inf,-pi))
@test isequal(acosh(MPCComplex(-Inf,5.0)),MPCComplex(Inf,pi))
@test isequal(acosh(MPCComplex(-Inf,-5.0)),MPCComplex(Inf,-pi))

@test isequal(acosh(MPCComplex(Inf,0.0)),MPCComplex(Inf,0.0))
@test isequal(acosh(MPCComplex(Inf,-0.0)),MPCComplex(Inf,-0.0))
@test isequal(acosh(MPCComplex(Inf,5.0)),MPCComplex(Inf,0.0))
@test isequal(acosh(MPCComplex(Inf,-5.0)),MPCComplex(Inf,-0.0))

@test isequal(acosh(MPCComplex(-Inf,Inf)),MPCComplex(Inf,3*pi/4))
@test isequal(acosh(MPCComplex(-Inf,-Inf)),MPCComplex(Inf,-3*pi/4))

@test isequal(acosh(MPCComplex(Inf,Inf)),MPCComplex(Inf,pi/4))
@test isequal(acosh(MPCComplex(Inf,-Inf)),MPCComplex(Inf,-pi/4))

@test isequal(acosh(MPCComplex(Inf,NaN)),MPCComplex(Inf,NaN))
@test isequal(acosh(MPCComplex(-Inf,NaN)),MPCComplex(Inf,NaN))

@test isequal(acosh(MPCComplex(NaN,Inf)),MPCComplex(Inf,NaN))
@test isequal(acosh(MPCComplex(NaN,-Inf)),MPCComplex(Inf,NaN))

@test isequal(acosh(MPCComplex(NaN,NaN)),MPCComplex(NaN,NaN))


# acos
#  acos(conj(z)) = conj(acos(z))
@test isequal(acos(MPCComplex(0.0,0.0)),MPCComplex(pi/2,-0.0))
@test isequal(acos(MPCComplex(0.0,-0.0)),MPCComplex(pi/2,0.0))
@test isequal(acos(MPCComplex(-0.0,0.0)),MPCComplex(pi/2,-0.0))
@test isequal(acos(MPCComplex(-0.0,-0.0)),MPCComplex(pi/2,0.0))

@test isequal(acos(MPCComplex(0.0,NaN)),MPCComplex(pi/2,NaN))
@test isequal(acos(MPCComplex(-0.0,NaN)),MPCComplex(pi/2,NaN))

@test isequal(acos(MPCComplex(0.0,Inf)),MPCComplex(pi/2,-Inf))
@test isequal(acos(MPCComplex(0.0,-Inf)),MPCComplex(pi/2,Inf))
@test isequal(acos(MPCComplex(5.0,Inf)),MPCComplex(pi/2,-Inf))
@test isequal(acos(MPCComplex(5.0,-Inf)),MPCComplex(pi/2,Inf))

@test isequal(acos(MPCComplex(5.0,NaN)),MPCComplex(NaN,NaN))

@test isequal(acos(MPCComplex(-Inf,0.0)),MPCComplex(pi,-Inf))
@test isequal(acos(MPCComplex(-Inf,-0.0)),MPCComplex(pi,Inf))
@test isequal(acos(MPCComplex(-Inf,5.0)),MPCComplex(pi,-Inf))
@test isequal(acos(MPCComplex(-Inf,-5.0)),MPCComplex(pi,Inf))

@test isequal(acos(MPCComplex(Inf,0.0)),MPCComplex(0.0,-Inf))
@test isequal(acos(MPCComplex(Inf,-0.0)),MPCComplex(0.0,Inf))
@test isequal(acos(MPCComplex(Inf,5.0)),MPCComplex(0.0,-Inf))
@test isequal(acos(MPCComplex(Inf,-5.0)),MPCComplex(0.0,Inf))

@test isequal(acos(MPCComplex(-Inf,Inf)),MPCComplex(3*pi/4,-Inf))
@test isequal(acos(MPCComplex(-Inf,-Inf)),MPCComplex(3*pi/4,Inf))

@test isequal(acos(MPCComplex(Inf,Inf)),MPCComplex(pi/4,-Inf))
@test isequal(acos(MPCComplex(Inf,-Inf)),MPCComplex(pi/4,Inf))

# TODO: The left side evaluates to MPCComplex(NaN, -Inf)
#@test isequal(acos(MPCComplex(Inf,NaN)),MPCComplex(NaN,Inf))
#@test isequal(acos(MPCComplex(-Inf,NaN)),MPCComplex(NaN,Inf))

@test isequal(acos(MPCComplex(NaN,0.0)),MPCComplex(NaN,NaN))
@test isequal(acos(MPCComplex(NaN,5.0)),MPCComplex(NaN,NaN))

@test isequal(acos(MPCComplex(NaN,Inf)),MPCComplex(NaN,-Inf))
@test isequal(acos(MPCComplex(NaN,-Inf)),MPCComplex(NaN,Inf))

@test isequal(acos(MPCComplex(NaN,NaN)),MPCComplex(NaN,NaN))



# asinh
#  asinh(conj(z)) = conj(asinh(z))
#  asinh(-z) = -asinh(z)
@test isequal(asinh(MPCComplex(0.0,0.0)),MPCComplex(0.0,0.0))
@test isequal(asinh(MPCComplex(0.0,-0.0)),MPCComplex(0.0,-0.0))
@test isequal(asinh(MPCComplex(-0.0,0.0)),MPCComplex(-0.0,0.0))
@test isequal(asinh(MPCComplex(-0.0,-0.0)),MPCComplex(-0.0,-0.0))

@test isequal(asinh(MPCComplex(0.0,Inf)),MPCComplex(Inf,pi/2))
@test isequal(asinh(MPCComplex(0.0,-Inf)),MPCComplex(Inf,-pi/2))
@test isequal(asinh(MPCComplex(-0.0,Inf)),MPCComplex(-Inf,pi/2))
@test isequal(asinh(MPCComplex(-0.0,-Inf)),MPCComplex(-Inf,-pi/2))

@test isequal(asinh(MPCComplex(5.0,Inf)),MPCComplex(Inf,pi/2))
@test isequal(asinh(MPCComplex(5.0,-Inf)),MPCComplex(Inf,-pi/2))
@test isequal(asinh(MPCComplex(-5.0,Inf)),MPCComplex(-Inf,pi/2))
@test isequal(asinh(MPCComplex(-5.0,-Inf)),MPCComplex(-Inf,-pi/2))

@test isequal(asinh(MPCComplex(0.0,NaN)),MPCComplex(NaN,NaN))
@test isequal(asinh(MPCComplex(5.0,NaN)),MPCComplex(NaN,NaN))

@test isequal(asinh(MPCComplex(Inf,Inf)),MPCComplex(Inf,pi/4))
@test isequal(asinh(MPCComplex(Inf,-Inf)),MPCComplex(Inf,-pi/4))
@test isequal(asinh(MPCComplex(-Inf,Inf)),MPCComplex(-Inf,pi/4))
@test isequal(asinh(MPCComplex(-Inf,-Inf)),MPCComplex(-Inf,-pi/4))

@test isequal(asinh(MPCComplex(Inf,NaN)),MPCComplex(Inf,NaN))
@test isequal(asinh(MPCComplex(-Inf,NaN)),MPCComplex(-Inf,NaN))

@test isequal(asinh(MPCComplex(NaN,0.0)),MPCComplex(NaN,0.0))
@test isequal(asinh(MPCComplex(NaN,-0.0)),MPCComplex(NaN,-0.0))

@test isequal(asinh(MPCComplex(NaN,5.0)),MPCComplex(NaN,NaN))

@test isequal(asinh(MPCComplex(NaN,Inf)),MPCComplex(Inf,NaN))
@test isequal(asinh(MPCComplex(NaN,-Inf)),MPCComplex(Inf,NaN))

@test isequal(asinh(MPCComplex(NaN,NaN)),MPCComplex(NaN,NaN))


# asin
#  asin(z) = -i*asinh(iz)
@test isequal(asin(MPCComplex(0.0,0.0)),MPCComplex(0.0,0.0))
@test isequal(asin(MPCComplex(0.0,-0.0)),MPCComplex(0.0,-0.0))
@test isequal(asin(MPCComplex(-0.0,0.0)),MPCComplex(-0.0,0.0))
@test isequal(asin(MPCComplex(-0.0,-0.0)),MPCComplex(-0.0,-0.0))

@test isequal(asin(MPCComplex(Inf,0.0)),MPCComplex(pi/2,Inf))
@test isequal(asin(MPCComplex(-Inf,0.0)),MPCComplex(-pi/2,Inf))
@test isequal(asin(MPCComplex(Inf,-0.0)),MPCComplex(pi/2,-Inf))
@test isequal(asin(MPCComplex(-Inf,-0.0)),MPCComplex(-pi/2,-Inf))

@test isequal(asin(MPCComplex(Inf,5.0)),MPCComplex(pi/2,Inf))
@test isequal(asin(MPCComplex(-Inf,5.0)),MPCComplex(-pi/2,Inf))
@test isequal(asin(MPCComplex(Inf,-5.0)),MPCComplex(pi/2,-Inf))
@test isequal(asin(MPCComplex(-Inf,-5.0)),MPCComplex(-pi/2,-Inf))

@test isequal(asin(MPCComplex(NaN,0.0)),MPCComplex(NaN,NaN))
@test isequal(asin(MPCComplex(NaN,5.0)),MPCComplex(NaN,NaN))

@test isequal(asin(MPCComplex(Inf,Inf)),MPCComplex(pi/4,Inf))
@test isequal(asin(MPCComplex(Inf,-Inf)),MPCComplex(pi/4,-Inf))
@test isequal(asin(MPCComplex(-Inf,Inf)),MPCComplex(-pi/4,Inf))
@test isequal(asin(MPCComplex(-Inf,-Inf)),MPCComplex(-pi/4,-Inf))

@test isequal(asin(MPCComplex(NaN,Inf)),MPCComplex(NaN,Inf))
@test isequal(asin(MPCComplex(NaN,-Inf)),MPCComplex(NaN,-Inf))

@test isequal(asin(MPCComplex(0.0,NaN)),MPCComplex(0.0,NaN))
@test isequal(asin(MPCComplex(-0.0,NaN)),MPCComplex(-0.0,NaN))

@test isequal(asin(MPCComplex(5.0,NaN)),MPCComplex(NaN,NaN))

@test isequal(asin(MPCComplex(Inf,NaN)),MPCComplex(NaN,Inf))
@test isequal(asin(MPCComplex(-Inf,NaN)),MPCComplex(NaN,Inf))

@test isequal(asin(MPCComplex(NaN,NaN)),MPCComplex(NaN,NaN))



# atanh
#  atanh(conj(z)) = conj(atanh(z))
#  atang(-z) = -atanh(z)

@test isequal(atanh(MPCComplex(0.0,0.0)),MPCComplex(0.0,0.0))
@test isequal(atanh(MPCComplex(0.0,-0.0)),MPCComplex(0.0,-0.0))
@test isequal(atanh(MPCComplex(-0.0,0.0)),MPCComplex(-0.0,0.0))
@test isequal(atanh(MPCComplex(-0.0,-0.0)),MPCComplex(-0.0,-0.0))

@test isequal(atanh(MPCComplex(0.0,NaN)),MPCComplex(0.0,NaN))
@test isequal(atanh(MPCComplex(-0.0,NaN)),MPCComplex(-0.0,NaN))

# raise divide-by-zero flag
@test isequal(atanh(MPCComplex(1.0,0.0)),MPCComplex(Inf,0.0))

@test isequal(atanh(MPCComplex(0.0,Inf)),MPCComplex(0.0,pi/2))
@test isequal(atanh(MPCComplex(0.0,-Inf)),MPCComplex(0.0,-pi/2))
@test isequal(atanh(MPCComplex(-0.0,Inf)),MPCComplex(-0.0,pi/2))
@test isequal(atanh(MPCComplex(-0.0,-Inf)),MPCComplex(-0.0,-pi/2))

@test isequal(atanh(MPCComplex(5.0,Inf)),MPCComplex(0.0,pi/2))
@test isequal(atanh(MPCComplex(5.0,-Inf)),MPCComplex(0.0,-pi/2))
@test isequal(atanh(MPCComplex(-5.0,Inf)),MPCComplex(-0.0,pi/2))
@test isequal(atanh(MPCComplex(-5.0,-Inf)),MPCComplex(-0.0,-pi/2))

@test isequal(atanh(MPCComplex(5.0,NaN)),MPCComplex(NaN,NaN))
@test isequal(atanh(MPCComplex(-5.0,NaN)),MPCComplex(NaN,NaN))

@test isequal(atanh(MPCComplex(Inf,0.0)),MPCComplex(0.0,pi/2))
@test isequal(atanh(MPCComplex(Inf,-0.0)),MPCComplex(0.0,-pi/2))
@test isequal(atanh(MPCComplex(-Inf,0.0)),MPCComplex(-0.0,pi/2))
@test isequal(atanh(MPCComplex(-Inf,-0.0)),MPCComplex(-0.0,-pi/2))

@test isequal(atanh(MPCComplex(Inf,5.0)),MPCComplex(0.0,pi/2))
@test isequal(atanh(MPCComplex(Inf,-5.0)),MPCComplex(0.0,-pi/2))
@test isequal(atanh(MPCComplex(-Inf,5.0)),MPCComplex(-0.0,pi/2))
@test isequal(atanh(MPCComplex(-Inf,-5.0)),MPCComplex(-0.0,-pi/2))

@test isequal(atanh(MPCComplex(Inf,Inf)),MPCComplex(0.0,pi/2))
@test isequal(atanh(MPCComplex(Inf,-Inf)),MPCComplex(0.0,-pi/2))
@test isequal(atanh(MPCComplex(-Inf,Inf)),MPCComplex(-0.0,pi/2))
@test isequal(atanh(MPCComplex(-Inf,-Inf)),MPCComplex(-0.0,-pi/2))

@test isequal(atanh(MPCComplex(Inf,NaN)),MPCComplex(0.0,NaN))
@test isequal(atanh(MPCComplex(-Inf,NaN)),MPCComplex(-0.0,NaN))

@test isequal(atanh(MPCComplex(NaN,0.0)),MPCComplex(NaN,NaN))
@test isequal(atanh(MPCComplex(NaN,-0.0)),MPCComplex(NaN,NaN))
@test isequal(atanh(MPCComplex(NaN,5.0)),MPCComplex(NaN,NaN))
@test isequal(atanh(MPCComplex(NaN,-5.0)),MPCComplex(NaN,NaN))

@test isequal(atanh(MPCComplex(NaN,Inf)),MPCComplex(0.0,pi/2))
@test isequal(atanh(MPCComplex(NaN,-Inf)),MPCComplex(0.0,-pi/2))

@test isequal(atanh(MPCComplex(NaN,NaN)),MPCComplex(NaN,NaN))


# atan
#  atan(z) = -i*tanh(iz)

@test isequal(atan(MPCComplex(0.0,0.0)),MPCComplex(0.0,0.0))
@test isequal(atan(MPCComplex(0.0,-0.0)),MPCComplex(0.0,-0.0))
@test isequal(atan(MPCComplex(-0.0,0.0)),MPCComplex(-0.0,0.0))
@test isequal(atan(MPCComplex(-0.0,-0.0)),MPCComplex(-0.0,-0.0))

@test isequal(atan(MPCComplex(NaN,0.0)),MPCComplex(NaN,0.0))
@test isequal(atan(MPCComplex(NaN,-0.0)),MPCComplex(NaN,-0.0))

# raise divide-by-zero flag
@test isequal(atan(MPCComplex(0.0,1.0)),MPCComplex(0.0,Inf))

@test isequal(atan(MPCComplex(Inf,0.0)),MPCComplex(pi/2,0.0))
@test isequal(atan(MPCComplex(-Inf,0.0)),MPCComplex(-pi/2,0.0))
@test isequal(atan(MPCComplex(Inf,-0.0)),MPCComplex(pi/2,-0.0))
@test isequal(atan(MPCComplex(-Inf,-0.0)),MPCComplex(-pi/2,-0.0))

@test isequal(atan(MPCComplex(Inf,5.0)),MPCComplex(pi/2,0.0))
@test isequal(atan(MPCComplex(-Inf,5.0)),MPCComplex(-pi/2,0.0))
@test isequal(atan(MPCComplex(Inf,-5.0)),MPCComplex(pi/2,-0.0))
@test isequal(atan(MPCComplex(-Inf,-5.0)),MPCComplex(-pi/2,-0.0))

@test isequal(atan(MPCComplex(NaN,5.0)),MPCComplex(NaN,NaN))
@test isequal(atan(MPCComplex(NaN,-5.0)),MPCComplex(NaN,NaN))

@test isequal(atan(MPCComplex(0.0,Inf)),MPCComplex(pi/2,0.0))
@test isequal(atan(MPCComplex(-0.0,Inf)),MPCComplex(-pi/2,0.0))
@test isequal(atan(MPCComplex(0.0,-Inf)),MPCComplex(pi/2,-0.0))
@test isequal(atan(MPCComplex(-0.0,-Inf)),MPCComplex(-pi/2,-0.0))

@test isequal(atan(MPCComplex(5.0,Inf)),MPCComplex(pi/2,0.0))
@test isequal(atan(MPCComplex(-5.0,Inf)),MPCComplex(-pi/2,0.0))
@test isequal(atan(MPCComplex(5.0,-Inf)),MPCComplex(pi/2,-0.0))
@test isequal(atan(MPCComplex(-5.0,-Inf)),MPCComplex(-pi/2,-0.0))

@test isequal(atan(MPCComplex(Inf,Inf)),MPCComplex(pi/2,0.0))
@test isequal(atan(MPCComplex(Inf,-Inf)),MPCComplex(pi/2,-0.0))
@test isequal(atan(MPCComplex(-Inf,Inf)),MPCComplex(-pi/2,0.0))
@test isequal(atan(MPCComplex(-Inf,-Inf)),MPCComplex(-pi/2,-0.0))

@test isequal(atan(MPCComplex(NaN,Inf)),MPCComplex(NaN,0.0))
@test isequal(atan(MPCComplex(NaN,-Inf)),MPCComplex(NaN,-0.0))

@test isequal(atan(MPCComplex(0.0,NaN)),MPCComplex(NaN,NaN))
@test isequal(atan(MPCComplex(-0.0,NaN)),MPCComplex(NaN,NaN))
@test isequal(atan(MPCComplex(5.0,NaN)),MPCComplex(NaN,NaN))
@test isequal(atan(MPCComplex(-5.0,NaN)),MPCComplex(NaN,NaN))

@test isequal(atan(MPCComplex(Inf,NaN)),MPCComplex(pi/2,0.0))
@test isequal(atan(MPCComplex(-Inf,NaN)),MPCComplex(-pi/2,0.0))

@test isequal(atan(MPCComplex(NaN,NaN)),MPCComplex(NaN,NaN))

end # end precision change

# real constructors
x = MPCComplex{53,53}()
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

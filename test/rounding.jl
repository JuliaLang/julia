# Small sanity tests to ensure changing the rounding of float functions work
using Base.Test

## Float64 checks
# a + b returns a number exactly between prevfloat(1.) and 1., so its
# final result depends strongly on the utilized rounding direction.
a = prevfloat(0.5)
b = 0.5
c = 0x1p-54
d = prevfloat(1.)

# Default rounding direction, RoundNearest
@test a + b === 1.
@test - a - b === -1.
@test a - b === -c
@test b - a === c

# RoundToZero
with_rounding(Float64,RoundToZero) do 
    @test a + b === d
    @test - a - b === -d
    @test a - b === -c
    @test b - a === c
end

# Sanity check to see if we have returned to RoundNearest
@test a + b === 1.
@test - a - b === -1.
@test a - b == -c
@test b - a == c

# RoundUp
with_rounding(Float64,RoundUp) do 
    @test a + b === 1.
    @test - a - b === -d
    @test a - b === -c
    @test b - a === c
end

# RoundDown
with_rounding(Float64,RoundDown) do 
    @test a + b === d
    @test - a - b === -1.
    @test a - b === -c
    @test b - a === c
end

## Float32 checks

a32 = prevfloat(0.5f0)
b32 = 0.5f0
c32 = (1.f0 - prevfloat(1.f0))/2
d32 = prevfloat(1.0f0)

# Default rounding direction, RoundNearest
@test a32 + b32 === 1.0f0
@test - a32 - b32 === -1.0f0
@test a32 - b32 === -c32
@test b32 - a32 === c32

# RoundToZero
with_rounding(Float32,RoundToZero) do 
    @test a32 + b32 === d32
    @test - a32 - b32 === -d32
    @test a32 - b32 === -c32
    @test b32 - a32 === c32
end

# Sanity check to see if we have returned to RoundNearest
@test a32 + b32 === 1.0f0
@test - a32 - b32 === -1.0f0
@test a32 - b32 == -c32
@test b32 - a32 == c32

# RoundUp
with_rounding(Float32,RoundUp) do 
    @test a32 + b32 === 1.0f0
    @test - a32 - b32 === -d32
    @test a32 - b32 === -c32
    @test b32 - a32 === c32
end

# RoundDown
with_rounding(Float32,RoundDown) do 
    @test a32 + b32 === d32
    @test - a32 - b32 === -1.0f0
    @test a32 - b32 === -c32
    @test b32 - a32 === c32
end


## Floating point exceptions
using Base.Rounding
for T = [Float32,Float64,BigFloat]
    clear_floatexcept(T)
    x = realmin(T)
    @test !is_floatexcept(T,FEUnderflow)
    if T != BigFloat
        y = x/2
        @test !is_floatexcept(T,FEUnderflow) # exact should not raise underflow
    end
    y = x/3
    @test is_floatexcept(T,FEUnderflow)

    clear_floatexcept(T)
    x = realmax(T)
    @test !is_floatexcept(T,FEOverflow)
    y = x*2
    @test is_floatexcept(T,FEOverflow)

    clear_floatexcept(T)
    @test !is_floatexcept(T,FEDivByZero)
    y = one(T)/zero(T)
    @test is_floatexcept(T,FEDivByZero)

    except = T == BigFloat ? FENaN : FEInvalid
    clear_floatexcept(T)
    x = inf(T)
    @test !is_floatexcept(T,except)    
    y = x-x
    @test is_floatexcept(T,except)

    clear_floatexcept(T)
    raise_floatexcept(T,FEUnderflow)
    @test is_floatexcept(T,FEUnderflow)
end

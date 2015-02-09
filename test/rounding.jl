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

# convert with rounding
for v = [sqrt(2),-1/3,nextfloat(1.0),prevfloat(1.0),nextfloat(-1.0),
         prevfloat(-1.0),nextfloat(0.0),prevfloat(0.0)]
    pn = Float32(v,RoundNearest)
    @test pn == convert(Float32,v)
    pz = Float32(v,RoundToZero)
    @test pz == with_rounding(()->convert(Float32,v), Float64, RoundToZero)
    pd = Float32(v,RoundDown)
    @test pd == with_rounding(()->convert(Float32,v), Float64, RoundDown)
    pu = Float32(v,RoundUp)
    @test pu == with_rounding(()->convert(Float32,v), Float64, RoundUp)

    @test pn == pd || pn == pu
    @test v > 0 ? pz == pd : pz == pu
    @test pu - pd == eps(pz)
end

for T in [Float32,Float64]
    for v in [sqrt(big(2.0)),-big(1.0)/big(3.0),nextfloat(big(1.0)),
              prevfloat(big(1.0)),nextfloat(big(0.0)),prevfloat(big(0.0)),
              pi,e,eulergamma,catalan,golden,
              typemax(Int64),typemax(UInt64),typemax(Int128),typemax(UInt128),0xa2f30f6001bb2ec6]
        pn = T(v,RoundNearest)
        @test pn == convert(T,BigFloat(v))
        pz = T(v,RoundToZero)
        @test pz == with_rounding(()->convert(T,BigFloat(v)), BigFloat, RoundToZero)
        pd = T(v,RoundDown)
        @test pd == with_rounding(()->convert(T,BigFloat(v)), BigFloat, RoundDown)
        pu = T(v,RoundUp)
        @test pu == with_rounding(()->convert(T,BigFloat(v)), BigFloat, RoundUp)

        @test pn == pd || pn == pu
        @test v > 0 ? pz == pd : pz == pu
        @test isinf(pu) || pu - pd == eps(pz)
    end
end

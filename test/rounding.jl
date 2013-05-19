# Small sanity tests to ensure changing the rounding of float functions work
using Base.Test
# a + b returns a number exactly between prevfloat(1.) and 1., so its
# final result depends strongly on the utilized rounding direction.
a = prevfloat(0.5)
b = 0.5
c = 0x1p-54
d = prevfloat(1.)

# Default rounding direction, RoundToNearest
@test a + b === 1.
@test - a - b === -1.
@test a - b === -c
@test b - a === c

# RoundToZero
with_rounding(RoundToZero) do 
    @test a + b === d
    @test - a - b === -d
    @test a - b === -c
    @test b - a === c
end

# Sanity check to see if we have returned to RoundToNearest
@test a + b === 1.
@test - a - b === -1.
@test a - b == -c
@test b - a == c

# RoundUp
with_rounding(RoundUp) do 
    @test a + b === 1.
    @test - a - b === -d
    @test a - b === -c
    @test b - a === c
end

# RoundDown
with_rounding(RoundDown) do 
    @test a + b === d
    @test - a - b === -1.
    @test a - b === -c
    @test b - a === c
end

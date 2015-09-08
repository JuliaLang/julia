# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test

# test the basic floating point functions

# flipsign

for elty in (Float32,Float64)
    x = convert(elty,-2.0)
    x = flipsign(x,-1.0)
    @test flipsign(x,big(-1.0)) == convert(elty,-2.0)
end

#maxintfloat

@test maxintfloat(Float16) == Float16(2048f0)
for elty in (Float16,Float32,Float64)
    @test maxintfloat(rand(elty)) == maxintfloat(elty)
end
@test maxintfloat() == maxintfloat(Float64)

#num2hex
for elty in (Float16,Float32,Float64)
    x = rand(elty)
    @test_approx_eq hex2num(num2hex(x)) x
end

#round
for elty in (Float32,Float64)
    x = rand(elty)
    A = fill(x,(10,10))
    @test round(A,RoundToZero) == fill(trunc(x),(10,10))
    @test round(A,RoundUp) == fill(ceil(x),(10,10))
    @test round(A,RoundDown) == fill(floor(x),(10,10))
    A = fill(x,(10,10,10))
    @test round(A,RoundToZero) == fill(trunc(x),(10,10,10))
    @test round(A,RoundUp) == fill(ceil(x),(10,10,10))
    @test round(A,RoundDown) == fill(floor(x),(10,10,10))
    for elty2 in (Int32,Int64)
        A = fill(x,(10,))
        @test round(elty2,A,RoundToZero) == fill(trunc(elty2,x),(10,))
        @test round(elty2,A,RoundUp) == fill(ceil(elty2,x),(10,))
        @test round(elty2,A,RoundDown) == fill(floor(elty2,x),(10,))
        A = fill(x,(10,10))
        @test round(elty2,A,RoundToZero) == fill(trunc(elty2,x),(10,10))
        @test round(elty2,A,RoundUp) == fill(ceil(elty2,x),(10,10))
        @test round(elty2,A,RoundDown) == fill(floor(elty2,x),(10,10))
        A = fill(x,(10,10,10))
        @test round(elty2,A,RoundToZero) == fill(trunc(elty2,x),(10,10,10))
        @test round(elty2,A,RoundUp) == fill(ceil(elty2,x),(10,10,10))
        @test round(elty2,A,RoundDown) == fill(floor(elty2,x),(10,10,10))
        @test round(elty2,A) == fill(round(elty2,x),(10,10,10))
    end
end

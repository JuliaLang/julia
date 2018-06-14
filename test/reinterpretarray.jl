# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
isdefined(Main, :OffsetArrays) || @eval Main include("testhelpers/OffsetArrays.jl")
using .Main.OffsetArrays

A = Int64[1, 2, 3, 4]
B = Complex{Int64}[5+6im, 7+8im, 9+10im]
# getindex
@test reinterpret(Complex{Int64}, A) == [1 + 2im, 3 + 4im]
@test reinterpret(Float64, A) == reinterpret.(Float64, A)

@test reinterpret(NTuple{3, Int64}, B) == [(5,6,7),(8,9,10)]

# setindex
let Ac = copy(A), Bc = copy(B)
    reinterpret(Complex{Int64}, Ac)[2] = -1 - 2im
    @test Ac == [1, 2, -1, -2]
    reinterpret(NTuple{3, Int64}, Bc)[2] = (4,5,6)
    @test Bc == Complex{Int64}[5+6im, 7+4im, 5+6im]
    reinterpret(NTuple{3, Int64}, Bc)[1] = (1,2,3)
    @test Bc == Complex{Int64}[1+2im, 3+4im, 5+6im]

    A1 = reinterpret(Float64, A)
    A2 = reinterpret(Complex{Float64}, A)
    A1[1] = 1.0
    @test real(A2[1]) == 1.0
end

# same-size reinterpret where one of the types is non-primitive
let a = NTuple{4,UInt8}[(0x01,0x02,0x03,0x04)]
    @test reinterpret(Float32, a)[1] == reinterpret(Float32, 0x04030201)
    reinterpret(Float32, a)[1] = 2.0
    @test reinterpret(Float32, a)[1] == 2.0
end

# ensure that reinterpret arrays aren't erroneously classified as strided
let A = reshape(1:20, 5, 4)
    V = view(A, :, :)
    R = reinterpret(Int32, V)
    R2 = reinterpret(Int32, A)
    @test !(R isa StridedArray)
    @test !(R2 isa StridedArray)
    @test R * ones(4, 5) == R2 * ones(4,5) == copy(R) * ones(4,5) == copy(R2) * ones(4,5)
end

# but ensure that strided views of strided reinterpret arrays are still strided
let A = collect(reshape(1:20, 5, 4))
    R = reinterpret(Int32, A)
    @test R isa StridedArray
    @test view(R, :, :) isa StridedArray
    @test reshape(R, :) isa StridedArray
end

# IndexStyle
let a = fill(1.0, 5, 3)
    r = reinterpret(Int64, a)
    @test @inferred(IndexStyle(r)) == IndexLinear()
    fill!(r, 2)
    @test all(a .=== reinterpret(Float64, [Int64(2)])[1])
    @test all(r .=== Int64(2))
    for badinds in (0, 16, (0,1), (1,0), (6,3), (5,4))
        @test_throws BoundsError r[badinds...]
        @test_throws BoundsError r[badinds...] = -2
    end
    for goodinds in (1, 15, (1,1), (5,3))
        r[goodinds...] = -2
        @test r[goodinds...] == -2
    end
    r = reinterpret(Int32, a)
    @test @inferred(IndexStyle(r)) == IndexLinear()
    fill!(r, 3)
    @test all(a .=== reinterpret(Float64, [(Int32(3), Int32(3))])[1])
    @test all(r .=== Int32(3))
    for badinds in (0, 31, (0,1), (1,0), (11,3), (10,4))
        @test_throws BoundsError r[badinds...]
        @test_throws BoundsError r[badinds...] = -3
    end
    for goodinds in (1, 30, (1,1), (10,3))
        r[goodinds...] = -3
        @test r[goodinds...] == -3
    end
    r = reinterpret(Int64, view(a, 1:2:5, :))
    @test @inferred(IndexStyle(r)) == IndexCartesian()
    fill!(r, 4)
    @test all(a[1:2:5,:] .=== reinterpret(Float64, [Int64(4)])[1])
    @test all(r .=== Int64(4))
    for badinds in (0, 10, (0,1), (1,0), (4,3), (3,4))
        @test_throws BoundsError r[badinds...]
        @test_throws BoundsError r[badinds...] = -4
    end
    for goodinds in (1, 9, (1,1), (3,3))
        r[goodinds...] = -4
        @test r[goodinds...] == -4
    end
    r = reinterpret(Int32, view(a, 1:2:5, :))
    @test @inferred(IndexStyle(r)) == IndexCartesian()
    fill!(r, 5)
    @test all(a[1:2:5,:] .=== reinterpret(Float64, [(Int32(5), Int32(5))])[1])
    @test all(r .=== Int32(5))
    for badinds in (0, 19, (0,1), (1,0), (7,3), (6,4))
        @test_throws BoundsError r[badinds...]
        @test_throws BoundsError r[badinds...] = -5
    end
    for goodinds in (1, 18, (1,1), (6,3))
        r[goodinds...] = -5
        @test r[goodinds...] == -5
    end
end

# Error on reinterprets that would expose padding
struct S1
    a::Int8
    b::Int64
end

struct S2
    a::Int16
    b::Int64
end

A1 = S1[S1(0, 0)]
A2 = S2[S2(0, 0)]
@test reinterpret(S1, A2)[1] == S1(0, 0)
@test_throws Base.PaddingError (reinterpret(S1, A2)[1] = S2(1, 2))
@test_throws Base.PaddingError reinterpret(S2, A1)[1]
reinterpret(S2, A1)[1] = S2(1, 2)
@test A1[1] == S1(1, 2)

# Unconventional axes
let a = [0.1 0.2; 0.3 0.4], at = reshape([(i,i+1) for i = 1:2:8], 2, 2)
    v = OffsetArray(a, (-1, 1))
    r = reinterpret(Int64, v)
    @test axes(r) === axes(v)
    @test r[0,2] === reinterpret(Int64, v[0,2])
    @test r[1,2] === reinterpret(Int64, v[1,2])
    @test r[0,3] === reinterpret(Int64, v[0,3])
    @test r[1,3] === reinterpret(Int64, v[1,3])
    @test_throws ArgumentError("cannot reinterpret a `Float64` array to `UInt32` when the first axis is Base.Slice(0:1). Try reshaping first.") reinterpret(UInt32, v)
    v = OffsetArray(a, (0, 1))
    r = reinterpret(UInt32, v)
    axsv = axes(v)
    @test axes(r) === (oftype(axsv[1], 1:4), axsv[2])
    for i = 1:2
        rval = reinterpret(Tuple{UInt32,UInt32}, [v[i,2]])[1]
        @test r[2i-1,2]   == rval[1]
        @test r[2i,2] == rval[2]
        rval = reinterpret(Tuple{UInt32,UInt32}, [v[i,3]])[1]
        @test r[2i-1,3]   == rval[1]
        @test r[2i,3] == rval[2]
    end
    r[4,2] = 7
    @test r[4,2] === UInt32(7)
    @test a[2,1] === reinterpret(Float64, [0x33333333, UInt32(7)])[1]
    offsetvt = (-2, 4)
    vt = OffsetArray(at, offsetvt)
    istr = string(Int)
    @test_throws ArgumentError("cannot reinterpret a `Tuple{$istr,$istr}` array to `$istr` when the first axis is Base.Slice(-1:0). Try reshaping first.") reinterpret(Int, vt)
    vt = reshape(vt, 1:1, axes(vt)...)
    r = reinterpret(Int, vt)
    @test r == OffsetArray(reshape(1:8, 2, 2, 2), (0, offsetvt...))
end

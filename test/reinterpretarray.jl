# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

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

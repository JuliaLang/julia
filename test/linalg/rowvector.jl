# This file is a part of Julia. License is MIT: https://julialang.org/license

using Base.LinAlg: Adjoint, Transpose

# these definitions are what adjoint(...) and transpose(...) meant
# meant prior to the Adjoint/Transpose transition, and the tests
# below are re-expressed in them to shield them against changes
# to adjoint(...), transpose(...), .', ', and A[ct]_(mul|ldiv|rdiv)_B[ct]
using Base.LinAlg: _conj, ConjRowVector
rvadjoint(v::AbstractVector) = RowVector(_conj(v))
rvtranspose(v::AbstractVector) = RowVector(v)
rvadjoint(v::RowVector) = conj(v.vec)
rvadjoint(v::RowVector{<:Real}) = v.vec
rvtranspose(v::RowVector) = v.vec
rvtranspose(v::ConjRowVector) = copy(v.vec)

@testset "Core" begin
    v = [1,2,3]
    z = [1+im,2,3]

    @test RowVector(v) == [1 2 3]
    @test RowVector{Int}(v) == [1 2 3]
    @test size(RowVector{Int}(uninitialized, 3)) === (1,3)
    @test size(RowVector{Int}(uninitialized, 1,3)) === (1,3)
    @test size(RowVector{Int}(uninitialized, (3,))) === (1,3)
    @test size(RowVector{Int}(uninitialized, (1,3))) === (1,3)
    @test_throws ErrorException RowVector{Float64, Vector{Int}}(v)

    @test rvtranspose(v)::RowVector == [1 2 3]
    @test rvadjoint(v)::RowVector == [1 2 3]
    @test rvtranspose(z)::RowVector == [1+im 2 3]
    @test rvadjoint(z)::RowVector == [1-im 2 3]

    rv = rvtranspose(v)
    tz = rvtranspose(z)

    @test rvtranspose(rv)::Vector == [1, 2, 3]
    @test rvadjoint(rv)::Vector == [1, 2, 3]
    @test rvtranspose(tz)::Vector == [1+im, 2, 3]
    @test rvadjoint(tz)::Vector == [1-im, 2, 3]

    @test conj(rv) === rv
    @test conj(tz) == [1-im 2 3]

    @test isa(similar(rv), RowVector)
    @test isa(similar(rv, Float64), RowVector)
    @test isa(copy(rv), RowVector)

    @test rv[2] === v[2]
    @test rv[1,2] === v[2]

    @test (rv2 = copy(rv); rv2[2] = 6; rv2[2] === 6)
    @test (rv2 = copy(rv); rv2[1,2] = 6; rv2[2] === 6)

    @test length(rv) === 3
    @test size(rv) === (1,3)
    @test size(rv,1) === 1
    @test size(rv,2) === 3

    @test map(-, rv)::RowVector == [-1 -2 -3]
    @test (-).(rv)::RowVector == [-1 -2 -3]
    @test (-).(rv,1)::RowVector == [0  1  2]

    y = rand(Complex{Float64},3)
    @test sum(abs2, imag.(diag(y .+ rvadjoint(y)))) < 1e-20

    @test parent(rv) === v
    @test vec(rv) === v
end

@testset "Diagonal ambiguity methods" begin
    d = Diagonal([1,2,3])
    v = [2,3,4]
    rv = rvtranspose(v)

    @test (rv*d)::RowVector == rvtranspose([2,6,12])
    @test_throws DimensionMismatch d*rv

    @test (d*rvtranspose(rv))::Vector == [2,6,12]

    @test_throws DimensionMismatch rvtranspose(rv)*d

    @test (d*rvadjoint(rv))::Vector == [2,6,12]

    @test_throws DimensionMismatch rvadjoint(rv)*d

    @test (rv/d)::RowVector ≈ [2/1  3/2  4/3]

    @test_throws DimensionMismatch d \ rv
end

@testset "Bidiagonal ambiguity methods" begin
    bd = Bidiagonal([1,2,3], [0,0], :U)
    v = [2,3,4]
    rv = rvtranspose(v)

    @test (rv/bd)::RowVector ≈ [2/1  3/2  4/3]

    @test_throws DimensionMismatch bd \ rv
end

@testset "hcat" begin
    @test isa([rvtranspose([1, 2, 3]) 4], RowVector{Int})
    @test isa([rvtranspose([1, 2, 3]) rvtranspose([4, 5])], RowVector{Int})
end

@testset "Left Division" begin
    mat = Matrix(Diagonal([1,2,3]))
    v = [2,3,4]
    rv = rvtranspose(v)

    @test_throws DimensionMismatch mat \ rv
end

@testset "Multiplication" begin
    v = [1,2,3]
    rv = rvtranspose(v)
    mat = Matrix(Diagonal([1,2,3]))

    @test (rv*v) === 14
    @test (rv*mat)::RowVector == [1 4 9]
    @test [1]*reshape([1],(1,1)) == reshape([1], (1,1))
    @test_throws DimensionMismatch rv*rv
    @test (v*rv)::Matrix == [1 2 3; 2 4 6; 3 6 9]
    @test_throws DimensionMismatch v*v # Was previously a missing method error, now an error message
    @test_throws DimensionMismatch mat*rv

    @test_throws DimensionMismatch rv*rvtranspose(v)
    @test (rv*Transpose(mat))::RowVector == [1 4 9]
    @test [1]*Transpose(reshape([1],(1,1))) == reshape([1], (1,1))
    @test rv*rvtranspose(rv) === 14
    @test_throws DimensionMismatch v*rvtranspose(rv)
    @test (v*rvtranspose(v))::Matrix == [1 2 3; 2 4 6; 3 6 9]
    @test (mat*rvtranspose(rv))::Vector == [1,4,9]

    @test (rvtranspose(rv)*rvtranspose(v))::Matrix == [1 2 3; 2 4 6; 3 6 9]
    @test_throws DimensionMismatch rvtranspose(rv)*Transpose(mat)
    @test (rvtranspose(v)*Transpose(mat))::RowVector == [1 4 9]
    @test_throws DimensionMismatch rvtranspose(rv)*rvtranspose(rv)
    @test rvtranspose(v)*rvtranspose(rv) === 14
    @test_throws DimensionMismatch rvtranspose(v)*rvtranspose(v)
    @test (Transpose(mat)*rvtranspose(rv))::Vector == [1,4,9]

    @test_throws DimensionMismatch rvtranspose(rv)*v
    @test_throws DimensionMismatch rvtranspose(rv)*mat
    @test (rvtranspose(v)*mat)::RowVector == [1 4 9]
    @test (rvtranspose(rv)*rv)::Matrix == [1 2 3; 2 4 6; 3 6 9]
    @test_throws DimensionMismatch rvtranspose(v)*rv
    @test rvtranspose(v)*v === 14
    @test_throws DimensionMismatch Transpose(mat)*rv

    z = [1+im,2,3]
    cz = rvadjoint(z)
    mat = Matrix(Diagonal([1+im,2,3]))

    @test cz*z === 15 + 0im

    @test_throws DimensionMismatch cz*rvadjoint(z)
    @test (cz*Adjoint(mat))::RowVector == [-2im 4 9]
    @test [1]*Adjoint(reshape([1],(1,1))) == reshape([1], (1,1))
    @test cz*rvadjoint(cz) === 15 + 0im
    @test_throws DimensionMismatch z*rvadjoint(cz)
    @test (z*rvadjoint(z))::Matrix == [2 2+2im 3+3im; 2-2im 4 6; 3-3im 6 9]
    @test (mat*rvadjoint(cz))::Vector == [2im,4,9]

    @test (rvadjoint(cz)*rvadjoint(z))::Matrix == [2 2+2im 3+3im; 2-2im 4 6; 3-3im 6 9]
    @test_throws DimensionMismatch rvadjoint(cz)*Adjoint(mat)
    @test (rvadjoint(z)*Adjoint(mat))::RowVector == [-2im 4 9]
    @test_throws DimensionMismatch rvadjoint(cz)*rvadjoint(cz)
    @test rvadjoint(z)*rvadjoint(cz) === 15 + 0im
    @test_throws DimensionMismatch rvadjoint(z)*rvadjoint(z)
    @test (Adjoint(mat)*rvadjoint(cz))::Vector == [2,4,9]

    @test_throws DimensionMismatch rvadjoint(cz)*z
    @test_throws DimensionMismatch rvadjoint(cz)*mat
    @test (rvadjoint(z)*mat)::RowVector == [2 4 9]
    @test (rvadjoint(cz)*cz)::Matrix == [2 2+2im 3+3im; 2-2im 4 6; 3-3im 6 9]
    @test_throws DimensionMismatch rvadjoint(z)*cz
    @test rvadjoint(z)*z === 15 + 0im
    @test_throws DimensionMismatch Adjoint(mat)*cz
end

@testset "norm" begin
   @test norm(rvtranspose([3.0,4.0])) ≈ 5.0
   @test norm(rvtranspose([3.0,4.0]), 1) ≈ 4.0
   @test norm(rvtranspose([3.0,4.0]), Inf) ≈ 7.0
end

@testset "QR ambiguity methods" begin
    qrmat = qrfact(Matrix(1.0I, 3, 3)).Q
    v = [2,3,4]
    rv = rvtranspose(v)

    @test (rv*Adjoint(qrmat))::RowVector == [2 3 4]
end

@testset "Right Division" begin
    mat = Matrix(Diagonal([1,2,3]))
    v = [2,3,4]
    rv = rvtranspose(v)

    @test (rv/mat)::RowVector ≈ [2/1  3/2  4/3]

    @test (rvtranspose(v)/mat)::RowVector ≈ [2/1  3/2  4/3]
    @test (rvtranspose(v)/Transpose(mat))::RowVector ≈ [2/1  3/2  4/3]
    @test (rv/Transpose(mat))::RowVector ≈ [2/1  3/2  4/3]

    @test (rvadjoint(v)/mat)::RowVector ≈ [2/1  3/2  4/3]
    @test (rvadjoint(v)/Adjoint(mat))::RowVector ≈ [2/1  3/2  4/3]
    @test (rv/Adjoint(mat))::RowVector ≈ [2/1  3/2  4/3]
end

@testset "Sparse ambiguity methods" begin
    mat = sparse(Diagonal([1,2,3]))
    v = [2,3,4]
    rv = rvtranspose(v)

    @test (rv/mat)::RowVector ≈ [2/1  3/2  4/3]

    @test_throws DimensionMismatch mat\rv
end

@testset "AbstractTriangular ambiguity methods" begin
    ut = UpperTriangular([1 0 0; 0 2 0; 0 0 3])
    v = [2,3,4]
    rv = rvtranspose(v)

    @test (rv*ut)::RowVector == [2 6 12]
    @test_throws DimensionMismatch ut*rv

    @test (rv*Transpose(ut))::RowVector == [2 6 12]
    @test (ut*rvtranspose(rv))::Vector == [2,6,12]

    @test (Transpose(ut)*rvtranspose(rv))::Vector == [2,6,12]
    @test_throws DimensionMismatch rvtranspose(rv)*Transpose(ut)

    @test_throws DimensionMismatch Transpose(ut)*rv
    @test_throws DimensionMismatch rvtranspose(rv)*ut

    @test (rv*Adjoint(ut))::RowVector == [2 6 12]
    @test (ut*rvadjoint(rv))::Vector == [2,6,12]

    @test_throws DimensionMismatch rvadjoint(rv)*Adjoint(ut)
    @test (Adjoint(ut)*rvadjoint(rv))::Vector == [2,6,12]

    @test_throws DimensionMismatch Adjoint(ut)*rv
    @test_throws DimensionMismatch rvadjoint(rv)*ut

    @test (rv/ut)::RowVector ≈ [2/1  3/2  4/3]
    @test (rv/Transpose(ut))::RowVector ≈ [2/1  3/2  4/3]
    @test (rv/Adjoint(ut))::RowVector ≈ [2/1  3/2  4/3]

    @test_throws DimensionMismatch ut\rv
end

@testset "Symmetric/Hermitian ambiguity methods" begin
    S = Symmetric(rand(3, 3))
    H = Hermitian(rand(3, 3))
    v = (rvadjoint(rand(3)))::RowVector
    @test_throws DimensionMismatch S\v
    @test_throws DimensionMismatch H\v
end

# issue #20389
@testset "1 row/col vec*mat" begin
    let x=[1,2,3], A=ones(1,4), y=rvadjoint(x), B=adjoint(A), C=x.*A
        @test x*A == rvadjoint(y)*A == x*Adjoint(B) == rvadjoint(y)*Adjoint(B) == C
        @test Adjoint(A)*rvadjoint(x) == Adjoint(A)*y == B*rvadjoint(x) == B*y == adjoint(C)
    end
end
@testset "complex 1 row/col vec*mat" begin
    let x=[1,2,3]*im, A=ones(1,4)*im, y=rvadjoint(x), B=adjoint(A), C=x.*A
        @test x*A == rvadjoint(y)*A == x*Adjoint(B) == rvadjoint(y)*Adjoint(B) == C
        @test Adjoint(A)*rvadjoint(x) == Adjoint(A)*y == B*rvadjoint(x) == B*y == adjoint(C)
    end
end

@testset "issue #20979" begin
    f20979(z::Complex) = [z.re -z.im; z.im z.re]
    v = rvadjoint([1+2im])
    @test (f20979.(v))[1] == f20979(v[1])
    @test f20979.(v) == f20979.(collect(v))

    w = rand(ComplexF64, 3)
    @test f20979.(rvadjoint(v)) == f20979.(collect(rvadjoint(v))) == rvadjoint(f20979.(v))

    g20979(x, y) = [x[2,1] x[1,2]; y[1,2] y[2,1]]
    v = [rand(2,2), rand(2,2), rand(2,2)]
    @test g20979.(rvadjoint(v), rvadjoint(v)) == g20979.(collect(rvadjoint(v)), collect(rvadjoint(v))) ==
          map(g20979, rvadjoint(v), rvadjoint(v)) == map(g20979, collect(rvadjoint(v)), collect(rvadjoint(v)))
end

@testset "ambiguity between * methods with RowVectors and ConjRowVectors (#20971)" begin
    @test RowVector(ConjArray(ones(4))) * ones(4) == 4
end

@testset "setindex!/getindex" begin
    v = [2, 3, 4]
    rv = rvtranspose(v)
    @test_throws BoundsError setindex!(rv, 5, CartesianIndex((5, 4, 3)))
    rv[CartesianIndex((1, 1, 1))] = 5
    @test_throws BoundsError getindex(rv, CartesianIndex((5, 4, 3)))
    @test rv[1] == 5

    @test rv[:, 2]::Vector == [v[2]]
    @test rv[:, 2:3]::RowVector == rvtranspose(v[2:3])
    @test rv[:, :]::RowVector == rv

    v = [1]
    rv = rvtranspose(v)
    rv[CartesianIndex()] = 2
    @test rv[CartesianIndex()] == 2
    rv[CartesianIndex(1)] = 1
    @test rv[CartesianIndex(1)] == 1

    # setindex!(v::RowVector, ...) should return v rather than v's parent
    @test setindex!(RowVector([1, 2, 3]), 4, 1)::RowVector == [4 2 3]
end

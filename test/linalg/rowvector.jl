# This file is a part of Julia. License is MIT: https://julialang.org/license

@testset "Core" begin
    v = [1,2,3]
    z = [1+im,2,3]

    @test RowVector(v)::RowVector == [1 2 3]
    @test RowVector{Int}(v) == [1 2 3]
    @test size(RowVector{Int}(3)) === (1,3)
    @test size(RowVector{Int}(1,3)) === (1,3)
    @test size(RowVector{Int}((3,))) === (1,3)
    @test size(RowVector{Int}((1,3))) === (1,3)
    @test_throws TypeError RowVector{Float64, Vector{Int}}(v)

    @test (v.')::RowVector == [1 2 3]
    @test (v')::RowVector == [1 2 3]
    @test (z.')::RowVector == [1+im 2 3]
    @test parent(z.') isa Vector
    @test (z')::RowVector == [1-im 2 3]
    @test parent(z') isa Base.LinAlg.ConjVector

    rv = v.'
    tz = z.'

    @test (rv.')::Vector == [1, 2, 3]
    @test (rv')::Vector == [1, 2, 3]
    @test (tz.')::Vector == [1+im, 2, 3]
    @test (tz')::Vector == [1-im, 2, 3]

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
    @test sum(abs2, imag.(diag(y .+ y'))) < 1e-20

    @test parent(rv) === v
    @test vec(rv) === v
end

@testset "Nested arrays" begin
    v = [[1, 2]]

    @test v'::RowVector == reshape([[1 2]], (1,1))
    @test parent(v') isa Base.LinAlg.AdjointArray{<:RowVector}
    @test conj(v')::RowVector == reshape([[1 2]], (1,1))
    @test parent(conj(v')) isa Base.LinAlg.AdjointArray{<:RowVector}
    @test (v').'::Vector == [[1 2]]
    @test (v')' === v

    @test v.'::RowVector == [[1, 2]]
    @test parent(v.') isa Vector
    @test conj(v.')::RowVector == [[1, 2]]
    @test parent(conj(v.')) isa Vector{Int}
    @test (v.').' === v
    @test (v.')'::Vector == [[1 2]]

    z = [[1+im, 2]]

    @test z'::RowVector == reshape([[1-im 2]], (1,1))
    @test parent(z') isa Base.LinAlg.AdjointArray{<:RowVector}
    @test conj(z')::RowVector == reshape([[1+im 2]], (1,1))
    @test parent(conj(z')) isa Base.LinAlg.ConjAdjointArray{<:RowVector}
    @test (z').'::Vector{<:RowVector} == [[1-im 2]]
    @test (z')' === z

    @test z.'::RowVector == [[1+im, 2]]
    @test parent(z.') isa Vector
    @test conj(z.')::RowVector == [[1-im, 2]]
    @test parent(conj(z.')) isa ConjArray{<:Vector}
    @test (z.').' === z
    @test (z.')'::Vector{<:RowVector} == [[1-im 2]]
end

@testset "Diagonal ambiguity methods" begin
    d = Diagonal([1,2,3])
    v = [2,3,4]
    rv = v.'

    @test (rv*d)::RowVector == [2,6,12].'
    @test_throws DimensionMismatch d*rv

    @test (d*rv.')::Vector == [2,6,12]

    @test_throws DimensionMismatch rv.'*d

    @test (d*rv')::Vector == [2,6,12]

    @test_throws DimensionMismatch rv'*d

    @test (rv/d)::RowVector ≈ [2/1  3/2  4/3]

    @test_throws DimensionMismatch d \ rv
end

@testset "Bidiagonal ambiguity methods" begin
    bd = Bidiagonal([1,2,3], [0,0], :U)
    v = [2,3,4]
    rv = v.'

    @test (rv/bd)::RowVector ≈ [2/1  3/2  4/3]

    @test_throws DimensionMismatch bd \ rv
end

@testset "hcat" begin
    @test isa([([1, 2, 3].') 4], RowVector{Int})
    @test isa([([1, 2, 3].') ([4, 5].')], RowVector{Int})
end

@testset "Left Division" begin
    mat = Matrix(Diagonal([1,2,3]))
    v = [2,3,4]
    rv = v.'

    @test_throws DimensionMismatch mat \ rv
end

@testset "Multiplication" begin
    v = [1,2,3]
    rv = v.'
    mat = Matrix(Diagonal([1,2,3]))

    @test (rv*v) === 14
    @test (rv*mat)::RowVector == [1 4 9]
    @test [1]*reshape([1],(1,1)) == reshape([1], (1,1))
    @test_throws DimensionMismatch rv*rv
    @test (v*rv)::Matrix == [1 2 3; 2 4 6; 3 6 9]
    @test_throws DimensionMismatch v*v # Was previously a missing method error, now an error message
    @test_throws DimensionMismatch mat*rv

    @test_throws DimensionMismatch rv*v.'
    @test (rv*mat.')::RowVector == [1 4 9]
    @test [1]*reshape([1],(1,1)).' == reshape([1], (1,1))
    @test rv*rv.' === 14
    @test_throws DimensionMismatch v*rv.'
    @test (v*v.')::Matrix == [1 2 3; 2 4 6; 3 6 9]
    @test (mat*rv.')::Vector == [1,4,9]

    @test (rv.'*v.')::Matrix == [1 2 3; 2 4 6; 3 6 9]
    @test_throws DimensionMismatch rv.'*mat.'
    @test (v.'*mat.')::RowVector == [1 4 9]
    @test_throws DimensionMismatch rv.'*rv.'
    @test v.'*rv.' === 14
    @test_throws DimensionMismatch v.'*v.'
    @test (mat.'*rv.')::Vector == [1,4,9]

    @test_throws DimensionMismatch rv.'*v
    @test_throws DimensionMismatch rv.'*mat
    @test (v.'*mat)::RowVector == [1 4 9]
    @test (rv.'*rv)::Matrix == [1 2 3; 2 4 6; 3 6 9]
    @test_throws DimensionMismatch v.'*rv
    @test v.'*v === 14
    @test_throws DimensionMismatch mat.'*rv

    z = [1+im,2,3]
    cz = z'
    mat = Matrix(Diagonal([1+im,2,3]))

    @test cz*z === 15 + 0im

    @test_throws DimensionMismatch cz*z'
    @test (cz*mat')::RowVector == [-2im 4 9]
    @test [1]*reshape([1],(1,1))' == reshape([1], (1,1))
    @test cz*cz' === 15 + 0im
    @test_throws DimensionMismatch z*cz'
    @test (z*z')::Matrix == [2 2+2im 3+3im; 2-2im 4 6; 3-3im 6 9]
    @test (mat*cz')::Vector == [2im,4,9]

    @test (cz'*z')::Matrix == [2 2+2im 3+3im; 2-2im 4 6; 3-3im 6 9]
    @test_throws DimensionMismatch cz'*mat'
    @test (z'*mat')::RowVector == [-2im 4 9]
    @test_throws DimensionMismatch cz'*cz'
    @test z'*cz' === 15 + 0im
    @test_throws DimensionMismatch z'*z'
    @test (mat'*cz')::Vector == [2,4,9]

    @test_throws DimensionMismatch cz'*z
    @test_throws DimensionMismatch cz'*mat
    @test (z'*mat)::RowVector == [2 4 9]
    @test (cz'*cz)::Matrix == [2 2+2im 3+3im; 2-2im 4 6; 3-3im 6 9]
    @test_throws DimensionMismatch z'*cz
    @test z'*z === 15 + 0im
    @test_throws DimensionMismatch mat'*cz
end

@testset "norm" begin
   @test norm([3.0,4.0].') ≈ 5.0
   @test norm([3.0,4.0].', 1) ≈ 4.0
   @test norm([3.0,4.0].', Inf) ≈ 7.0
end

@testset "QR ambiguity methods" begin
    qrmat = Base.LinAlg.getq(qrfact(Matrix(1.0I, 3, 3)))
    v = [2,3,4]
    rv = v.'

    @test (rv*qrmat')::RowVector == [2 3 4]
end

@testset "Right Division" begin
    mat = Matrix(Diagonal([1,2,3]))
    v = [2,3,4]
    rv = v.'

    @test (rv/mat)::RowVector ≈ [2/1  3/2  4/3]

    @test (v.'/mat)::RowVector ≈ [2/1  3/2  4/3]
    @test (v.'/mat.')::RowVector ≈ [2/1  3/2  4/3]
    @test (rv/mat.')::RowVector ≈ [2/1  3/2  4/3]

    @test (v'/mat)::RowVector ≈ [2/1  3/2  4/3]
    @test (v'/mat')::RowVector ≈ [2/1  3/2  4/3]
    @test (rv/mat')::RowVector ≈ [2/1  3/2  4/3]
end

@testset "Sparse ambiguity methods" begin
    mat = sparse(Diagonal([1,2,3]))
    v = [2,3,4]
    rv = v.'

    @test (rv/mat)::RowVector ≈ [2/1  3/2  4/3]

    @test_throws DimensionMismatch mat\rv
end

@testset "AbstractTriangular ambiguity methods" begin
    ut = UpperTriangular([1 0 0; 0 2 0; 0 0 3])
    v = [2,3,4]
    rv = v.'

    @test (rv*ut)::RowVector == [2 6 12]
    @test_throws DimensionMismatch ut*rv

    @test (rv*ut.')::RowVector == [2 6 12]
    @test (ut*rv.')::Vector == [2,6,12]

    @test (ut.'*rv.')::Vector == [2,6,12]
    @test_throws DimensionMismatch rv.'*ut.'

    @test_throws DimensionMismatch ut.'*rv
    @test_throws DimensionMismatch rv.'*ut

    @test (rv*ut')::RowVector == [2 6 12]
    @test (ut*rv')::Vector == [2,6,12]

    @test_throws DimensionMismatch rv'*ut'
    @test (ut'*rv')::Vector == [2,6,12]

    @test_throws DimensionMismatch ut'*rv
    @test_throws DimensionMismatch rv'*ut

    @test (rv/ut)::RowVector ≈ [2/1  3/2  4/3]
    @test (rv/ut.')::RowVector ≈ [2/1  3/2  4/3]
    @test (rv/ut')::RowVector ≈ [2/1  3/2  4/3]

    @test_throws DimensionMismatch ut\rv
end

@testset "Symmetric/Hermitian ambiguity methods" begin
    S = Symmetric(rand(3, 3))
    H = Hermitian(rand(3, 3))
    v = (rand(3)')::RowVector
    @test_throws DimensionMismatch S\v
    @test_throws DimensionMismatch H\v
end

# issue #20389
@testset "1 row/col vec*mat" begin
    let x=[1,2,3], A=ones(1,4), y=x', B=A', C=x.*A
        @test x*A == y'*A == x*B' == y'*B' == C
        @test A'*x' == A'*y == B*x' == B*y == C'
    end
end
@testset "complex 1 row/col vec*mat" begin
    let x=[1,2,3]*im, A=ones(1,4)*im, y=x', B=A', C=x.*A
        @test x*A == y'*A == x*B' == y'*B' == C
        @test A'*x' == A'*y == B*x' == B*y == C'
    end
end

@testset "issue #20979" begin
    f20979(z::Complex) = [z.re -z.im; z.im z.re]
    v = [1+2im]'
    @test (f20979.(v))[1] == f20979(v[1])
    @test f20979.(v) == f20979.(collect(v))

    w = rand(Complex128, 3)
    @test f20979.(v') == f20979.(collect(v')) == (f20979.(v))'

    g20979(x, y) = [x[2,1] x[1,2]; y[1,2] y[2,1]]
    v = [rand(2,2), rand(2,2), rand(2,2)]
    @test g20979.(v', v') == g20979.(collect(v'), collect(v')) ==
          map(g20979, v', v') == map(g20979, collect(v'), collect(v'))
end

@testset "ambiguity between * methods with RowVectors and ConjRowVectors (#20971)" begin
    @test RowVector(ConjArray(ones(4))) * ones(4) == 4
end

@testset "setindex!/getindex" begin
    v = [2, 3, 4]
    rv = v.'
    @test_throws BoundsError setindex!(rv, 5, CartesianIndex((5, 4, 3)))
    rv[CartesianIndex((1, 1, 1))] = 5
    @test_throws BoundsError getindex(rv, CartesianIndex((5, 4, 3)))
    @test rv[1] == 5

    @test rv[:, 2]::Vector == [v[2]]
    @test rv[:, 2:3]::RowVector == v[2:3].'
    @test rv[:, :]::RowVector == rv

    v = [1]
    rv = v.'
    rv[CartesianIndex()] = 2
    @test rv[CartesianIndex()] == 2
    rv[CartesianIndex(1)] = 1
    @test rv[CartesianIndex(1)] == 1
end

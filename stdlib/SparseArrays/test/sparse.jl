# This file is a part of Julia. License is MIT: https://julialang.org/license

module SparseTests

using Test
using SparseArrays
using SparseArrays: getcolptr, nonzeroinds
using LinearAlgebra
using Printf: @printf # for debug
using Random
using Test: guardseed
using InteractiveUtils: @which
using Dates
include("forbidproperties.jl")
include("simplesmatrix.jl")

@testset "Issue #33169" begin
    m21 = sparse([1, 2], [2, 2], SimpleSMatrix{2,1}.([rand(2, 1), rand(2, 1)]), 2, 2)
    m12 = sparse([1, 2], [2, 2], SimpleSMatrix{1,2}.([rand(1, 2), rand(1, 2)]), 2, 2)
    m22 = sparse([1, 2], [2, 2], SimpleSMatrix{2,2}.([rand(2, 2), rand(2, 2)]), 2, 2)
    m23 = sparse([1, 2], [2, 2], SimpleSMatrix{2,3}.([rand(2, 3), rand(2, 3)]), 2, 2)
    v12 = sparsevec([2], SimpleSMatrix{1,2}.([rand(1, 2)]))
    v21 = sparsevec([2], SimpleSMatrix{2,1}.([rand(2, 1)]))
    @test m22 * m21 ≈ Matrix(m22) * Matrix(m21)
    @test m22' * m21 ≈ Matrix(m22') * Matrix(m21)
    @test m21' * m22 ≈ Matrix(m21') * Matrix(m22)
    @test m23' * m22 * m21 ≈ Matrix(m23') * Matrix(m22) * Matrix(m21)
    @test m21 * v12 ≈ Matrix(m21) * Vector(v12)
    @test m12' * v12 ≈ Matrix(m12') * Vector(v12)
    @test v21' * m22 ≈ Vector(v21)' * Matrix(m22)
    @test v12' * m21' ≈ Vector(v12)' * Matrix(m21)'
    @test v21' * v21 ≈ Vector(v21)' * Vector(v21)
    @test v21' * m22 * v21 ≈ Vector(v21)' * Matrix(m22) * Vector(v21)
end

@testset "issparse" begin
    @test issparse(sparse(fill(1,5,5)))
    @test !issparse(fill(1,5,5))
    @test nnz(zero(sparse(fill(1,5,5)))) == 0
end

@testset "iszero specialization for SparseMatrixCSC" begin
    @test !iszero(sparse(I, 3, 3))                  # test failure
    @test iszero(spzeros(3, 3))                     # test success with no stored entries
    S = sparse(I, 3, 3)
    S[:] .= 0
    @test iszero(S)  # test success with stored zeros via broadcasting
    S = sparse(I, 3, 3)
    fill!(S, 0)
    @test iszero(S)  # test success with stored zeros via fill!
    @test iszero(SparseMatrixCSC(2, 2, [1,2,3], [1,2], [0,0,1])) # test success with nonzeros beyond data range
end
@testset "isone specialization for SparseMatrixCSC" begin
    @test isone(sparse(I, 3, 3))    # test success
    @test !isone(sparse(I, 3, 4))   # test failure for non-square matrix
    @test !isone(spzeros(3, 3))     # test failure for too few stored entries
    @test !isone(sparse(2I, 3, 3))  # test failure for non-one diagonal entries
    @test !isone(sparse(Bidiagonal(fill(1, 3), fill(1, 2), :U))) # test failure for non-zero off-diag entries
end

@testset "indtype" begin
    @test SparseArrays.indtype(sparse(Int8[1,1],Int8[1,1],[1,1])) == Int8
end

@testset "spzeros de-splatting" begin
    @test spzeros(Float64, Int64, (2, 2)) == spzeros(Float64, Int64, 2, 2)
end

@testset "conversion to AbstractMatrix/SparseMatrix of same eltype" begin
    a = sprand(5, 5, 0.2)
    @test AbstractMatrix{eltype(a)}(a) == a
    @test SparseMatrixCSC{eltype(a)}(a) == a
    @test SparseMatrixCSC{eltype(a), Int}(a) == a
    @test SparseMatrixCSC{eltype(a)}(Array(a)) == a
    @test Array(SparseMatrixCSC{eltype(a), Int8}(a)) == Array(a)
end

@testset "sparse matrix construction" begin
    @test (A = fill(1.0+im,5,5); isequal(Array(sparse(A)), A))
    @test_throws ArgumentError sparse([1,2,3], [1,2], [1,2,3], 3, 3)
    @test_throws ArgumentError sparse([1,2,3], [1,2,3], [1,2], 3, 3)
    @test_throws ArgumentError sparse([1,2,3], [1,2,3], [1,2,3], 0, 1)
    @test_throws ArgumentError sparse([1,2,3], [1,2,3], [1,2,3], 1, 0)
    @test_throws ArgumentError sparse([1,2,4], [1,2,3], [1,2,3], 3, 3)
    @test_throws ArgumentError sparse([1,2,3], [1,2,4], [1,2,3], 3, 3)
    @test isequal(sparse(Int[], Int[], Int[], 0, 0), SparseMatrixCSC(0, 0, Int[1], Int[], Int[]))
    @test isequal(sparse(big.([1,1,1,2,2,3,4,5]),big.([1,2,3,2,3,3,4,5]),big.([1,2,4,3,5,6,7,8]), 6, 6),
        SparseMatrixCSC(6, 6, big.([1,2,4,7,8,9,9]), big.([1,1,2,1,2,3,4,5]), big.([1,2,3,4,5,6,7,8])))
    @test sparse(Any[1,2,3], Any[1,2,3], Any[1,1,1]) == sparse([1,2,3], [1,2,3], [1,1,1])
    @test sparse(Any[1,2,3], Any[1,2,3], Any[1,1,1], 5, 4) == sparse([1,2,3], [1,2,3], [1,1,1], 5, 4)
    # with combine
    @test sparse([1, 1, 2, 2, 2], [1, 2, 1, 2, 2], 1.0, 2, 2, +) == sparse([1, 1, 2, 2], [1, 2, 1, 2], [1.0, 1.0, 1.0, 2.0], 2, 2)
    @test sparse([1, 1, 2, 2, 2], [1, 2, 1, 2, 2], -1.0, 2, 2, *) == sparse([1, 1, 2, 2], [1, 2, 1, 2], [-1.0, -1.0, -1.0, 1.0], 2, 2)
end

@testset "SparseMatrixCSC construction from UniformScaling" begin
    @test_throws ArgumentError SparseMatrixCSC(I, -1, 3)
    @test_throws ArgumentError SparseMatrixCSC(I, 3, -1)
    @test SparseMatrixCSC(2I, 3, 3)::SparseMatrixCSC{Int,Int} == Matrix(2I, 3, 3)
    @test SparseMatrixCSC(2I, 3, 4)::SparseMatrixCSC{Int,Int} == Matrix(2I, 3, 4)
    @test SparseMatrixCSC(2I, 4, 3)::SparseMatrixCSC{Int,Int} == Matrix(2I, 4, 3)
    @test SparseMatrixCSC(2.0I, 3, 3)::SparseMatrixCSC{Float64,Int} == Matrix(2I, 3, 3)
    @test SparseMatrixCSC{Real}(2I, 3, 3)::SparseMatrixCSC{Real,Int} == Matrix(2I, 3, 3)
    @test SparseMatrixCSC{Float64}(2I, 3, 3)::SparseMatrixCSC{Float64,Int} == Matrix(2I, 3, 3)
    @test SparseMatrixCSC{Float64,Int32}(2I, 3, 3)::SparseMatrixCSC{Float64,Int32} == Matrix(2I, 3, 3)
    @test SparseMatrixCSC{Float64,Int32}(0I, 3, 3)::SparseMatrixCSC{Float64,Int32} == Matrix(0I, 3, 3)
end
@testset "sparse(S::UniformScaling, shape...) convenience constructors" begin
    # we exercise these methods only lightly as these methods call the SparseMatrixCSC
    # constructor methods well-exercised by the immediately preceding testset
    @test sparse(2I, 3, 4)::SparseMatrixCSC{Int,Int} == Matrix(2I, 3, 4)
    @test sparse(2I, (3, 4))::SparseMatrixCSC{Int,Int} == Matrix(2I, 3, 4)
end

se33 = SparseMatrixCSC{Float64}(I, 3, 3)
do33 = fill(1.,3)

@testset "sparse binary operations" begin
    @test isequal(se33 * se33, se33)

    @test Array(se33 + convert(SparseMatrixCSC{Float32,Int32}, se33)) == Matrix(2I, 3, 3)
    @test Array(se33 * convert(SparseMatrixCSC{Float32,Int32}, se33)) == Matrix(I, 3, 3)

    @testset "shape checks for sparse elementwise binary operations equivalent to map" begin
        sqrfloatmat, colfloatmat = sprand(4, 4, 0.5), sprand(4, 1, 0.5)
        @test_throws DimensionMismatch (+)(sqrfloatmat, colfloatmat)
        @test_throws DimensionMismatch (-)(sqrfloatmat, colfloatmat)
        @test_throws DimensionMismatch map(min, sqrfloatmat, colfloatmat)
        @test_throws DimensionMismatch map(max, sqrfloatmat, colfloatmat)
        sqrboolmat, colboolmat = sprand(Bool, 4, 4, 0.5), sprand(Bool, 4, 1, 0.5)
        @test_throws DimensionMismatch map(&, sqrboolmat, colboolmat)
        @test_throws DimensionMismatch map(|, sqrboolmat, colboolmat)
        @test_throws DimensionMismatch map(xor, sqrboolmat, colboolmat)
    end
end

@testset "Issue #30006" begin
    A = SparseMatrixCSC{Float64,Int32}(spzeros(3,3))
    A[:, 1] = [1, 2, 3]
    @test nnz(A) == 3
    @test nonzeros(A) == [1, 2, 3]
end

@testset "concatenation tests" begin
    sp33 = sparse(1.0I, 3, 3)

    @testset "horizontal concatenation" begin
        @test [se33 se33] == [Array(se33) Array(se33)]
        @test length(nonzeros([sp33 0I])) == 3
    end

    @testset "vertical concatenation" begin
        @test [se33; se33] == [Array(se33); Array(se33)]
        se33_32bit = convert(SparseMatrixCSC{Float32,Int32}, se33)
        @test [se33; se33_32bit] == [Array(se33); Array(se33_32bit)]
        @test length(nonzeros([sp33; 0I])) == 3
    end

    se44 = sparse(1.0I, 4, 4)
    sz42 = spzeros(4, 2)
    sz41 = spzeros(4, 1)
    sz34 = spzeros(3, 4)
    se77 = sparse(1.0I, 7, 7)
    @testset "h+v concatenation" begin
        @test [se44 sz42 sz41; sz34 se33] == se77
        @test length(nonzeros([sp33 0I; 1I 0I])) == 6
    end

    @testset "blockdiag concatenation" begin
        @test blockdiag(se33, se33) == sparse(1:6,1:6,fill(1.,6))
        @test blockdiag() == spzeros(0, 0)
        @test nnz(blockdiag()) == 0
    end

    @testset "concatenation promotion" begin
        sz41_f32 = spzeros(Float32, 4, 1)
        se33_i32 = sparse(Int32(1)I, 3, 3)
        @test [se44 sz42 sz41_f32; sz34 se33_i32] == se77
    end

    @testset "mixed sparse-dense concatenation" begin
        sz33 = spzeros(3, 3)
        de33 = Matrix(1.0I, 3, 3)
        @test [se33 de33; sz33 se33] == Array([se33 se33; sz33 se33 ])
    end

    # check splicing + concatenation on random instances, with nested vcat and also side-checks sparse ref
    @testset "splicing + concatenation on random instances" begin
        for i = 1 : 10
            a = sprand(5, 4, 0.5)
            @test [a[1:2,1:2] a[1:2,3:4]; a[3:5,1] [a[3:4,2:4]; a[5:5,2:4]]] == a
        end
    end
end

let
    a116 = copy(reshape(1:16, 4, 4))
    s116 = sparse(a116)

    @testset "sparse ref" begin
        p = [4, 1, 2, 3, 2]
        @test Array(s116[p,:]) == a116[p,:]
        @test Array(s116[:,p]) == a116[:,p]
        @test Array(s116[p,p]) == a116[p,p]
    end

    @testset "sparse assignment" begin
        p = [4, 1, 3]
        a116[p, p] .= -1
        s116[p, p] .= -1
        @test a116 == s116

        p = [2, 1, 4]
        a116[p, p] = reshape(1:9, 3, 3)
        s116[p, p] = reshape(1:9, 3, 3)
        @test a116 == s116
    end
end

@testset "dropdims" begin
    for i = 1:5
        am = sprand(20, 1, 0.2)
        av = dropdims(am, dims=2)
        @test ndims(av) == 1
        @test all(av.==am)
        am = sprand(1, 20, 0.2)
        av = dropdims(am, dims=1)
        @test ndims(av) == 1
        @test all(av' .== am)
    end
end

@testset "Issue #28963" begin
    @test_throws DimensionMismatch (spzeros(10,10)[:, :] = sprand(10,20,0.5))
end

@testset "matrix-vector multiplication (non-square)" begin
    for i = 1:5
        a = sprand(10, 5, 0.5)
        b = rand(5)
        @test maximum(abs.(a*b - Array(a)*b)) < 100*eps()
    end
end

@testset "sparse matrix * BitArray" begin
    A = sprand(5,5,0.2)
    B = trues(5)
    @test A*B ≈ Array(A)*B
    B = trues(5,5)
    @test A*B ≈ Array(A)*B
    @test B*A ≈ B*Array(A)
end

@testset "complex matrix-vector multiplication and left-division" begin
    if Base.USE_GPL_LIBS
    for i = 1:5
        a = I + 0.1*sprandn(5, 5, 0.2)
        b = randn(5,3) + im*randn(5,3)
        c = randn(5) + im*randn(5)
        d = randn(5) + im*randn(5)
        α = rand(ComplexF64)
        β = rand(ComplexF64)
        @test (maximum(abs.(a*b - Array(a)*b)) < 100*eps())
        @test (maximum(abs.(mul!(similar(b), a, b) - Array(a)*b)) < 100*eps()) # for compatibility with present matmul API. Should go away eventually.
        @test (maximum(abs.(mul!(similar(c), a, c) - Array(a)*c)) < 100*eps()) # for compatibility with present matmul API. Should go away eventually.
        @test (maximum(abs.(mul!(similar(b), transpose(a), b) - transpose(Array(a))*b)) < 100*eps()) # for compatibility with present matmul API. Should go away eventually.
        @test (maximum(abs.(mul!(similar(c), transpose(a), c) - transpose(Array(a))*c)) < 100*eps()) # for compatibility with present matmul API. Should go away eventually.
        @test (maximum(abs.(a'b - Array(a)'b)) < 100*eps())
        @test (maximum(abs.(transpose(a)*b - transpose(Array(a))*b)) < 100*eps())
        @test (maximum(abs.(a\b - Array(a)\b)) < 1000*eps())
        @test (maximum(abs.(a'\b - Array(a')\b)) < 1000*eps())
        @test (maximum(abs.(transpose(a)\b - Array(transpose(a))\b)) < 1000*eps())
        @test (maximum(abs.((a'*c + d) - (Array(a)'*c + d))) < 1000*eps())
        @test (maximum(abs.((α*transpose(a)*c + β*d) - (α*transpose(Array(a))*c + β*d))) < 1000*eps())
        @test (maximum(abs.((transpose(a)*c + d) - (transpose(Array(a))*c + d))) < 1000*eps())
        c = randn(6) + im*randn(6)
        @test_throws DimensionMismatch α*transpose(a)*c + β*c
        @test_throws DimensionMismatch α*transpose(a)*fill(1.,5) + β*c

        a = I + 0.1*sprandn(5, 5, 0.2) + 0.1*im*sprandn(5, 5, 0.2)
        b = randn(5,3)
        @test (maximum(abs.(a*b - Array(a)*b)) < 100*eps())
        @test (maximum(abs.(a'b - Array(a)'b)) < 100*eps())
        @test (maximum(abs.(transpose(a)*b - transpose(Array(a))*b)) < 100*eps())
        @test (maximum(abs.(a\b - Array(a)\b)) < 1000*eps())
        @test (maximum(abs.(a'\b - Array(a')\b)) < 1000*eps())
        @test (maximum(abs.(transpose(a)\b - Array(transpose(a))\b)) < 1000*eps())

        a = I + tril(0.1*sprandn(5, 5, 0.2))
        b = randn(5,3) + im*randn(5,3)
        @test (maximum(abs.(a*b - Array(a)*b)) < 100*eps())
        @test (maximum(abs.(a'b - Array(a)'b)) < 100*eps())
        @test (maximum(abs.(transpose(a)*b - transpose(Array(a))*b)) < 100*eps())
        @test (maximum(abs.(a\b - Array(a)\b)) < 1000*eps())
        @test (maximum(abs.(a'\b - Array(a')\b)) < 1000*eps())
        @test (maximum(abs.(transpose(a)\b - Array(transpose(a))\b)) < 1000*eps())

        a = I + tril(0.1*sprandn(5, 5, 0.2) + 0.1*im*sprandn(5, 5, 0.2))
        b = randn(5,3)
        @test (maximum(abs.(a*b - Array(a)*b)) < 100*eps())
        @test (maximum(abs.(a'b - Array(a)'b)) < 100*eps())
        @test (maximum(abs.(transpose(a)*b - transpose(Array(a))*b)) < 100*eps())
        @test (maximum(abs.(a\b - Array(a)\b)) < 1000*eps())
        @test (maximum(abs.(a'\b - Array(a')\b)) < 1000*eps())
        @test (maximum(abs.(transpose(a)\b - Array(transpose(a))\b)) < 1000*eps())

        a = I + triu(0.1*sprandn(5, 5, 0.2))
        b = randn(5,3) + im*randn(5,3)
        @test (maximum(abs.(a*b - Array(a)*b)) < 100*eps())
        @test (maximum(abs.(a'b - Array(a)'b)) < 100*eps())
        @test (maximum(abs.(transpose(a)*b - transpose(Array(a))*b)) < 100*eps())
        @test (maximum(abs.(a\b - Array(a)\b)) < 1000*eps())
        @test (maximum(abs.(a'\b - Array(a')\b)) < 1000*eps())
        @test (maximum(abs.(transpose(a)\b - Array(transpose(a))\b)) < 1000*eps())

        a = I + triu(0.1*sprandn(5, 5, 0.2) + 0.1*im*sprandn(5, 5, 0.2))
        b = randn(5,3)
        @test (maximum(abs.(a*b - Array(a)*b)) < 100*eps())
        @test (maximum(abs.(a'b - Array(a)'b)) < 100*eps())
        @test (maximum(abs.(transpose(a)*b - transpose(Array(a))*b)) < 100*eps())
        @test (maximum(abs.(a\b - Array(a)\b)) < 1000*eps())
        @test (maximum(abs.(a'\b - Array(a')\b)) < 1000*eps())
        @test (maximum(abs.(transpose(a)\b - Array(transpose(a))\b)) < 1000*eps())

        a = I + triu(0.1*sprandn(5, 5, 0.2))
        b = randn(5,3) + im*randn(5,3)
        @test (maximum(abs.(a*b - Array(a)*b)) < 100*eps())
        @test (maximum(abs.(a'b - Array(a)'b)) < 100*eps())
        @test (maximum(abs.(transpose(a)*b - transpose(Array(a))*b)) < 100*eps())
        @test (maximum(abs.(a\b - Array(a)\b)) < 1000*eps())
        @test (maximum(abs.(a'\b - Array(a')\b)) < 1000*eps())
        @test (maximum(abs.(transpose(a)\b - Array(transpose(a))\b)) < 1000*eps())

        # UpperTriangular/LowerTriangular solve
        a = UpperTriangular(I + triu(0.1*sprandn(5, 5, 0.2)))
        b = sprandn(5, 5, 0.2)
        @test (maximum(abs.(a\b - Array(a)\Array(b))) < 1000*eps())
        # test error throwing for bwdTrisolve
        @test_throws DimensionMismatch a\Matrix{Float64}(I, 6, 6)
        a = LowerTriangular(I + tril(0.1*sprandn(5, 5, 0.2)))
        b = sprandn(5, 5, 0.2)
        @test (maximum(abs.(a\b - Array(a)\Array(b))) < 1000*eps())
        # test error throwing for fwdTrisolve
        @test_throws DimensionMismatch a\Matrix{Float64}(I, 6, 6)



        a = sparse(Diagonal(randn(5) + im*randn(5)))
        b = randn(5,3)
        @test (maximum(abs.(a*b - Array(a)*b)) < 100*eps())
        @test (maximum(abs.(a'b - Array(a)'b)) < 100*eps())
        @test (maximum(abs.(transpose(a)*b - transpose(Array(a))*b)) < 100*eps())
        @test (maximum(abs.(a\b - Array(a)\b)) < 1000*eps())
        @test (maximum(abs.(a'\b - Array(a')\b)) < 1000*eps())
        @test (maximum(abs.(transpose(a)\b - Array(transpose(a))\b)) < 1000*eps())

        b = randn(5,3) + im*randn(5,3)
        @test (maximum(abs.(a*b - Array(a)*b)) < 100*eps())
        @test (maximum(abs.(a'b - Array(a)'b)) < 100*eps())
        @test (maximum(abs.(transpose(a)*b - transpose(Array(a))*b)) < 100*eps())
        @test (maximum(abs.(a\b - Array(a)\b)) < 1000*eps())
        @test (maximum(abs.(a'\b - Array(a')\b)) < 1000*eps())
        @test (maximum(abs.(transpose(a)\b - Array(transpose(a))\b)) < 1000*eps())
    end
    end
end

@testset "matrix multiplication" begin
    for (m, p, n, q, k) in (
                            (10, 0.7, 5, 0.3, 15),
                            (100, 0.01, 100, 0.01, 20),
                            (100, 0.1, 100, 0.2, 100),
                           )
        a = sprand(m, n, p)
        b = sprand(n, k, q)
        as = sparse(a')
        bs = sparse(b')
        ab = a * b
        aab = Array(a) * Array(b)
        @test maximum(abs.(ab - aab)) < 100*eps()
        @test a*bs' == ab
        @test as'*b == ab
        @test as'*bs' == ab
        f = Diagonal(rand(n))
        @test Array(a*f) == Array(a)*f
        @test Array(f*b) == f*Array(b)
        A = rand(2n, 2n)
        sA = view(A, 1:2:2n, 1:2:2n)
        @test Array(sA*b) ≈ Array(sA)*Array(b)
        @test Array(a*sA) ≈ Array(a)*Array(sA)
        c = sprandn(ComplexF32, n, n, q)
        @test Array(sA*c') ≈ Array(sA)*Array(c)'
        @test Array(c'*sA) ≈ Array(c)'*Array(sA)
    end
end

@testset "multiplication of sparse matrix and triangular matrix" begin
    _sparse_test_matrix(n, T) =  T == Int ? sparse(rand(0:4, n, n)) : sprandn(T, n, n, 0.6)
    _triangular_test_matrix(n, TA, T) = T == Int ? TA(rand(0:9, n, n)) : TA(randn(T, n, n))

    n = 5
    for T1 in (Int, Float64, ComplexF32)
        S = _sparse_test_matrix(n, T1)
        MS = Matrix(S)
        for T2 in (Int, Float64, ComplexF32)
            for TM in (LowerTriangular, UnitLowerTriangular, UpperTriangular, UnitLowerTriangular)
                T = _triangular_test_matrix(n, TM, T2)
                MT = Matrix(T)
                @test isa(T * S, DenseMatrix)
                @test isa(S * T, DenseMatrix)
                for transT in (identity, adjoint, transpose), transS in (identity, adjoint, transpose)
                    @test transT(T) * transS(S) ≈ transT(MT) * transS(MS)
                    @test transS(S) * transT(T) ≈ transS(MS) * transT(MT)
                end
            end
        end
    end
end

@testset "Issue #30502" begin
    @test nnz(sprand(UInt8(16), UInt8(16), 1.0)) == 256
    @test nnz(sprand(UInt8(16), UInt8(16), 1.0, ones)) == 256
end

@testset "kronecker product" begin
    for (m,n) in ((5,10), (13,8), (14,10))
        a = sprand(m, 5, 0.4); a_d = Matrix(a)
        b = sprand(n, 6, 0.3); b_d = Matrix(b)
        v = view(a, :, 1); v_d = Vector(v)
        x = sprand(m, 0.4); x_d = Vector(x)
        y = sprand(n, 0.3); y_d = Vector(y)
        c_di = Diagonal(rand(m)); c = sparse(c_di); c_d = Array(c_di)
        d_di = Diagonal(rand(n)); d = sparse(d_di); d_d = Array(d_di)
        # mat ⊗ mat
        @test Array(kron(a, b)) == kron(a_d, b_d)
        @test Array(kron(a_d, b)) == kron(a_d, b_d)
        @test Array(kron(a, b_d)) == kron(a_d, b_d)
        @test issparse(kron(c, d_di))
        @test Array(kron(c, d_di)) == kron(c_d, d_d)
        @test issparse(kron(c_di, d))
        @test Array(kron(c_di, d)) == kron(c_d, d_d)
        @test issparse(kron(c_di, y))
        @test Array(kron(c_di, y)) == kron(c_di, y_d)
        @test issparse(kron(x, d_di))
        @test Array(kron(x, d_di)) == kron(x_d, d_di)
        # vec ⊗ vec
        @test Vector(kron(x, y)) == kron(x_d, y_d)
        @test Vector(kron(x_d, y)) == kron(x_d, y_d)
        @test Vector(kron(x, y_d)) == kron(x_d, y_d)
        # mat ⊗ vec
        @test Array(kron(a, y)) == kron(a_d, y_d)
        @test Array(kron(a_d, y)) == kron(a_d, y_d)
        @test Array(kron(a, y_d)) == kron(a_d, y_d)
        # vec ⊗ mat
        @test Array(kron(x, b)) == kron(x_d, b_d)
        @test Array(kron(x_d, b)) == kron(x_d, b_d)
        @test Array(kron(x, b_d)) == kron(x_d, b_d)
        # vec ⊗ vec'
        @test issparse(kron(v, y'))
        @test issparse(kron(x, y'))
        @test Array(kron(v, y')) == kron(v_d, y_d')
        @test Array(kron(x, y')) == kron(x_d, y_d')
        # test different types
        z = convert(SparseVector{Float16, Int8}, y); z_d = Vector(z)
        @test Vector(kron(x, z)) == kron(x_d, z_d)
        @test Array(kron(a, z)) == kron(a_d, z_d)
        @test Array(kron(z, b)) == kron(z_d, b_d)
    end
end

@testset "sparse Frobenius dot/inner product" begin
    for i = 1:5
        A = sprand(ComplexF64,10,15,0.4)
        B = sprand(ComplexF64,10,15,0.5)
        @test dot(A,B) ≈ dot(Matrix(A),Matrix(B))
    end
    @test_throws DimensionMismatch dot(sprand(5,5,0.2),sprand(5,6,0.2))
end

@testset "generalized dot product" begin
    for i = 1:5
        A = sprand(ComplexF64, 10, 15, 0.4)
        Av = view(A, :, :)
        x = sprand(ComplexF64, 10, 0.5)
        y = sprand(ComplexF64, 15, 0.5)
        @test dot(x, A, y) ≈ dot(Vector(x), A, Vector(y)) ≈ (Vector(x)' * Matrix(A)) * Vector(y)
        @test dot(x, A, y) ≈ dot(x, Av, y)
    end
end

const BASE_TEST_PATH = joinpath(Sys.BINDIR, "..", "share", "julia", "test")
isdefined(Main, :Quaternions) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "Quaternions.jl"))
using .Main.Quaternions

sA = sprandn(3, 7, 0.5)
sC = similar(sA)
dA = Array(sA)

@testset "scaling with * and mul!, rmul!, and lmul!" begin
    b = randn(7)
    @test dA * Diagonal(b) == sA * Diagonal(b)
    @test dA * Diagonal(b) == mul!(sC, sA, Diagonal(b))
    @test dA * Diagonal(b) == rmul!(copy(sA), Diagonal(b))
    b = randn(3)
    @test Diagonal(b) * dA == Diagonal(b) * sA
    @test Diagonal(b) * dA == mul!(sC, Diagonal(b), sA)
    @test Diagonal(b) * dA == lmul!(Diagonal(b), copy(sA))

    @test dA * 0.5            == sA * 0.5
    @test dA * 0.5            == mul!(sC, sA, 0.5)
    @test dA * 0.5            == rmul!(copy(sA), 0.5)
    @test 0.5 * dA            == 0.5 * sA
    @test 0.5 * dA            == mul!(sC, sA, 0.5)
    @test 0.5 * dA            == lmul!(0.5, copy(sA))
    @test mul!(sC, 0.5, sA)   == mul!(sC, sA, 0.5)

    @testset "inverse scaling with mul!" begin
        bi = inv.(b)
        @test lmul!(Diagonal(bi), copy(dA)) ≈ ldiv!(Diagonal(b), copy(sA))
        @test lmul!(Diagonal(bi), copy(dA)) ≈ ldiv!(transpose(Diagonal(b)), copy(sA))
        @test lmul!(Diagonal(conj(bi)), copy(dA)) ≈ ldiv!(adjoint(Diagonal(b)), copy(sA))
        @test_throws DimensionMismatch ldiv!(Diagonal(fill(1., length(b)+1)), copy(sA))
        @test_throws LinearAlgebra.SingularException ldiv!(Diagonal(zeros(length(b))), copy(sA))

        dAt = copy(transpose(dA))
        sAt = copy(transpose(sA))
        @test rmul!(copy(dAt), Diagonal(bi)) ≈ rdiv!(copy(sAt), Diagonal(b))
        @test rmul!(copy(dAt), Diagonal(bi)) ≈ rdiv!(copy(sAt), transpose(Diagonal(b)))
        @test rmul!(copy(dAt), Diagonal(conj(bi))) ≈ rdiv!(copy(sAt), adjoint(Diagonal(b)))
        @test_throws DimensionMismatch rdiv!(copy(sAt), Diagonal(fill(1., length(b)+1)))
        @test_throws LinearAlgebra.SingularException rdiv!(copy(sAt), Diagonal(zeros(length(b))))
    end

    @testset "non-commutative multiplication" begin
        # non-commutative multiplication
        Avals = Quaternion.(randn(10), randn(10), randn(10), randn(10))
        sA = sparse(rand(1:3, 10), rand(1:7, 10), Avals, 3, 7)
        sC = copy(sA)
        dA = Array(sA)

        b = Quaternion.(randn(7), randn(7), randn(7), randn(7))
        D = Diagonal(b)
        @test Array(sA * D) ≈ dA * D
        @test rmul!(copy(sA), D) ≈ dA * D
        @test mul!(sC, copy(sA), D) ≈ dA * D

        b = Quaternion.(randn(3), randn(3), randn(3), randn(3))
        D = Diagonal(b)
        @test Array(D * sA) ≈ D * dA
        @test lmul!(D, copy(sA)) ≈ D * dA
        @test mul!(sC, D, copy(sA)) ≈ D * dA
    end
end

@testset "copyto!" begin
    A = sprand(5, 5, 0.2)
    B = sprand(5, 5, 0.2)
    copyto!(A, B)
    @test A == B
    @test pointer(nonzeros(A)) != pointer(nonzeros(B))
    @test pointer(rowvals(A)) != pointer(rowvals(B))
    @test pointer(getcolptr(A)) != pointer(getcolptr(B))
    # Test size(A) != size(B), but length(A) == length(B)
    B = sprand(25, 1, 0.2)
    copyto!(A, B)
    @test A[:] == B[:]
    # Test various size(A) / size(B) combinations
    for mA in [5, 10, 20], nA in [5, 10, 20], mB in [5, 10, 20], nB in [5, 10, 20]
        A = sprand(mA,nA,0.4)
        Aorig = copy(A)
        B = sprand(mB,nB,0.4)
        if mA*nA >= mB*nB
            copyto!(A,B)
            @assert(A[1:length(B)] == B[:])
            @assert(A[length(B)+1:end] == Aorig[length(B)+1:end])
        else
            @test_throws BoundsError copyto!(A,B)
        end
    end
    # Test eltype(A) != eltype(B), size(A) != size(B)
    A = sprand(5, 5, 0.2)
    Aorig = copy(A)
    B = sparse(rand(Float32, 3, 3))
    copyto!(A, B)
    @test A[1:9] == B[:]
    @test A[10:end] == Aorig[10:end]
    # Test eltype(A) != eltype(B), size(A) == size(B)
    A = sparse(rand(Float64, 3, 3))
    B = sparse(rand(Float32, 3, 3))
    copyto!(A, B)
    @test A == B
    # Test copyto!(dense, sparse)
    B = sprand(5, 5, 1.0)
    A = rand(5,5)
    A´ = similar(A)
    @test copyto!(A, B) == copyto!(A´, Matrix(B))
    # Test copyto!(dense, Rdest, sparse, Rsrc)
    A = rand(5,5)
    A´ = similar(A)
    Rsrc = CartesianIndices((3:4, 2:3))
    Rdest = CartesianIndices((2:3, 1:2))
    copyto!(A, Rdest, B, Rsrc)
    copyto!(A´, Rdest, Matrix(B), Rsrc)
    @test A[Rdest] == A´[Rdest] == Matrix(B)[Rsrc]
    # Test unaliasing of B´
    B´ = copy(B)
    copyto!(B´, Rdest, B´, Rsrc)
    @test Matrix(B´)[Rdest] == Matrix(B)[Rsrc]
    # Test that only elements at overlapping linear indices are overwritten
    A = sprand(3, 3, 1.0); B = ones(4, 4)
    copyto!(B, A)
    @test B[4, :] != B[:, 4] == ones(4)
    # Allow no-op copyto! with empty source even for incompatible eltypes
    A = sparse(fill("", 0, 0))
    @test copyto!(B, A) == B
end

@testset "conj" begin
    cA = sprandn(5,5,0.2) + im*sprandn(5,5,0.2)
    @test Array(conj.(cA)) == conj(Array(cA))
    @test Array(conj!(copy(cA))) == conj(Array(cA))
end

@testset "SparseMatrixCSC [c]transpose[!] and permute[!]" begin
    smalldim = 5
    largedim = 10
    nzprob = 0.4
    (m, n) = (smalldim, smalldim)
    A = sprand(m, n, nzprob)
    X = similar(A)
    C = copy(transpose(A))
    p = randperm(m)
    q = randperm(n)
    @testset "common error checking of [c]transpose! methods (ftranspose!)" begin
        @test_throws DimensionMismatch transpose!(A[:, 1:(smalldim - 1)], A)
        @test_throws DimensionMismatch transpose!(A[1:(smalldim - 1), 1], A)
        @test_throws ArgumentError transpose!((B = similar(A); resize!(rowvals(B), nnz(A) - 1); B), A)
        @test_throws ArgumentError transpose!((B = similar(A); resize!(nonzeros(B), nnz(A) - 1); B), A)
    end
    @testset "common error checking of permute[!] methods / source-perm compat" begin
        @test_throws DimensionMismatch permute(A, p[1:(end - 1)], q)
        @test_throws DimensionMismatch permute(A, p, q[1:(end - 1)])
    end
    @testset "common error checking of permute[!] methods / source-dest compat" begin
        @test_throws DimensionMismatch permute!(A[1:(m - 1), :], A, p, q)
        @test_throws DimensionMismatch permute!(A[:, 1:(m - 1)], A, p, q)
        @test_throws ArgumentError permute!((Y = copy(X); resize!(rowvals(Y), nnz(A) - 1); Y), A, p, q)
        @test_throws ArgumentError permute!((Y = copy(X); resize!(nonzeros(Y), nnz(A) - 1); Y), A, p, q)
    end
    @testset "common error checking of permute[!] methods / source-workmat compat" begin
        @test_throws DimensionMismatch permute!(X, A, p, q, C[1:(m - 1), :])
        @test_throws DimensionMismatch permute!(X, A, p, q, C[:, 1:(m - 1)])
        @test_throws ArgumentError permute!(X, A, p, q, (D = copy(C); resize!(rowvals(D), nnz(A) - 1); D))
        @test_throws ArgumentError permute!(X, A, p, q, (D = copy(C); resize!(nonzeros(D), nnz(A) - 1); D))
    end
    @testset "common error checking of permute[!] methods / source-workcolptr compat" begin
        @test_throws DimensionMismatch permute!(A, p, q, C, Vector{eltype(rowvals(A))}(undef, length(getcolptr(A)) - 1))
    end
    @testset "common error checking of permute[!] methods / permutation validity" begin
        @test_throws ArgumentError permute!(A, (r = copy(p); r[2] = r[1]; r), q)
        @test_throws ArgumentError permute!(A, (r = copy(p); r[2] = m + 1; r), q)
        @test_throws ArgumentError permute!(A, p, (r = copy(q); r[2] = r[1]; r))
        @test_throws ArgumentError permute!(A, p, (r = copy(q); r[2] = n + 1; r))
    end
    @testset "overall functionality of [c]transpose[!] and permute[!]" begin
        for (m, n) in ((smalldim, smalldim), (smalldim, largedim), (largedim, smalldim))
            A = sprand(m, n, nzprob)
            At = copy(transpose(A))
            # transpose[!]
            fullAt = Array(transpose(A))
            @test copy(transpose(A)) == fullAt
            @test transpose!(similar(At), A) == fullAt
            # adjoint[!]
            C = A + im*A/2
            fullCh = Array(C')
            @test copy(C') == fullCh
            @test adjoint!(similar(sparse(fullCh)), C) == fullCh
            # permute[!]
            p = randperm(m)
            q = randperm(n)
            fullPAQ = Array(A)[p,q]
            @test permute(A, p, q) == sparse(Array(A[p,q]))
            @test permute!(similar(A), A, p, q) == fullPAQ
            @test permute!(similar(A), A, p, q, similar(At)) == fullPAQ
            @test permute!(copy(A), p, q) == fullPAQ
            @test permute!(copy(A), p, q, similar(At)) == fullPAQ
            @test permute!(copy(A), p, q, similar(At), similar(getcolptr(A))) == fullPAQ
        end
    end
end

@testset "transpose of SubArrays" begin
    A = view(sprandn(10, 10, 0.3), 1:4, 1:4)
    @test copy(transpose(Array(A))) == Array(transpose(A))
    @test copy(adjoint(Array(A))) == Array(adjoint(A))
end

@testset "exp" begin
    A = sprandn(5,5,0.2)
    @test ℯ.^A ≈ ℯ.^Array(A)
end

@testset "reductions" begin
    pA = sparse(rand(3, 7))
    p28227 = sparse(Real[0 0.5])

    for arr in (se33, sA, pA, p28227)
        for f in (sum, prod, minimum, maximum)
            farr = Array(arr)
            @test f(arr) ≈ f(farr)
            @test f(arr, dims=1) ≈ f(farr, dims=1)
            @test f(arr, dims=2) ≈ f(farr, dims=2)
            @test f(arr, dims=(1, 2)) ≈ [f(farr)]
            @test isequal(f(arr, dims=3), f(farr, dims=3))
        end
    end

    for f in (sum, prod, minimum, maximum)
        # Test with a map function that maps to non-zero
        for arr in (se33, sA, pA)
            @test f(x->x+1, arr) ≈ f(arr .+ 1)
        end

        # case where f(0) would throw
        @test f(x->sqrt(x-1), pA .+ 1) ≈ f(sqrt.(pA))
        # these actually throw due to #10533
        # @test f(x->sqrt(x-1), pA .+ 1, dims=1) ≈ f(sqrt(pA), dims=1)
        # @test f(x->sqrt(x-1), pA .+ 1, dims=2) ≈ f(sqrt(pA), dims=2)
        # @test f(x->sqrt(x-1), pA .+ 1, dims=3) ≈ f(pA)
    end

    @testset "empty cases" begin
        @test sum(sparse(Int[])) === 0
        @test prod(sparse(Int[])) === 1
        @test_throws ArgumentError minimum(sparse(Int[]))
        @test_throws ArgumentError maximum(sparse(Int[]))

        for f in (sum, prod)
            @test isequal(f(spzeros(0, 1), dims=1), f(Matrix{Int}(I, 0, 1), dims=1))
            @test isequal(f(spzeros(0, 1), dims=2), f(Matrix{Int}(I, 0, 1), dims=2))
            @test isequal(f(spzeros(0, 1), dims=(1, 2)), f(Matrix{Int}(I, 0, 1), dims=(1, 2)))
            @test isequal(f(spzeros(0, 1), dims=3), f(Matrix{Int}(I, 0, 1), dims=3))
        end
        for f in (minimum, maximum, findmin, findmax)
            @test_throws ArgumentError f(spzeros(0, 1), dims=1)
            @test isequal(f(spzeros(0, 1), dims=2), f(Matrix{Int}(I, 0, 1), dims=2))
            @test_throws ArgumentError f(spzeros(0, 1), dims=(1, 2))
            @test isequal(f(spzeros(0, 1), dims=3), f(Matrix{Int}(I, 0, 1), dims=3))
        end
    end
end

@testset "issue #5190" begin
    @test_throws ArgumentError sparsevec([3,5,7],[0.1,0.0,3.2],4)
end

@testset "what used to be issue #5386" begin
    K,J,V = findnz(SparseMatrixCSC(2,1,[1,3],[1,2],[1.0,0.0]))
    @test length(K) == length(J) == length(V) == 2
end

@testset "findall" begin
    # issue described in https://groups.google.com/d/msg/julia-users/Yq4dh8NOWBQ/GU57L90FZ3EJ
    A = sparse(I, 5, 5)
    @test findall(A) == findall(x -> x == true, A) == findall(Array(A))
    # Non-stored entries are true
    @test findall(x -> x == false, A) == findall(x -> x == false, Array(A))

    # Not all stored entries are true
    @test findall(sparse([true false])) == [CartesianIndex(1, 1)]
    @test findall(x -> x > 1, sparse([1 2])) == [CartesianIndex(1, 2)]
end

@testset "issue #5824" begin
    @test sprand(4,5,0.5).^0 == sparse(fill(1,4,5))
end

@testset "issue #5985" begin
    @test sprand(Bool, 4, 5, 0.0) == sparse(zeros(Bool, 4, 5))
    @test sprand(Bool, 4, 5, 1.00) == sparse(fill(true, 4, 5))
    sprb45nnzs = zeros(5)
    for i=1:5
        sprb45 = sprand(Bool, 4, 5, 0.5)
        @test length(sprb45) == 20
        sprb45nnzs[i] = sum(sprb45)[1]
    end
    @test 4 <= sum(sprb45nnzs)/length(sprb45nnzs) <= 16
end

@testset "issue #5853, sparse diff" begin
    for i=1:2, a=Any[[1 2 3], reshape([1, 2, 3],(3,1)), Matrix(1.0I, 3, 3)]
        @test diff(sparse(a),dims=i) == diff(a,dims=i)
    end
end

@testset "access to undefined error types that initially allocate elements as #undef" begin
    @test sparse(1:2, 1:2, Number[1,2])^2 == sparse(1:2, 1:2, [1,4])
    sd1 = diff(sparse([1,1,1], [1,2,3], Number[1,2,3]), dims=1)
end

@testset "issue #6036" begin
    P = spzeros(Float64, 3, 3)
    for i = 1:3
        P[i,i] = i
    end

    @test minimum(P) === 0.0
    @test maximum(P) === 3.0
    @test minimum(-P) === -3.0
    @test maximum(-P) === 0.0

    @test maximum(P, dims=(1,)) == [1.0 2.0 3.0]
    @test maximum(P, dims=(2,)) == reshape([1.0,2.0,3.0],3,1)
    @test maximum(P, dims=(1,2)) == reshape([3.0],1,1)

    @test maximum(sparse(fill(-1,3,3))) == -1
    @test minimum(sparse(fill(1,3,3))) == 1
end

@testset "unary functions" begin
    A = sprand(5, 15, 0.5)
    C = A + im*A
    Afull = Array(A)
    Cfull = Array(C)
    # Test representatives of [unary functions that map zeros to zeros and may map nonzeros to zeros]
    @test sin.(Afull) == Array(sin.(A))
    @test tan.(Afull) == Array(tan.(A)) # should be redundant with sin test
    @test ceil.(Afull) == Array(ceil.(A))
    @test floor.(Afull) == Array(floor.(A)) # should be redundant with ceil test
    @test real.(Afull) == Array(real.(A)) == Array(real(A))
    @test imag.(Afull) == Array(imag.(A)) == Array(imag(A))
    @test conj.(Afull) == Array(conj.(A)) == Array(conj(A))
    @test real.(Cfull) == Array(real.(C)) == Array(real(C))
    @test imag.(Cfull) == Array(imag.(C)) == Array(imag(C))
    @test conj.(Cfull) == Array(conj.(C)) == Array(conj(C))
    # Test representatives of [unary functions that map zeros to zeros and nonzeros to nonzeros]
    @test expm1.(Afull) == Array(expm1.(A))
    @test abs.(Afull) == Array(abs.(A))
    @test abs2.(Afull) == Array(abs2.(A))
    @test abs.(Cfull) == Array(abs.(C))
    @test abs2.(Cfull) == Array(abs2.(C))
    # Test representatives of [unary functions that map both zeros and nonzeros to nonzeros]
    @test cos.(Afull) == Array(cos.(A))
    # Test representatives of remaining vectorized-nonbroadcast unary functions
    @test ceil.(Int, Afull) == Array(ceil.(Int, A))
    @test floor.(Int, Afull) == Array(floor.(Int, A))
    # Tests of real, imag, abs, and abs2 for SparseMatrixCSC{Int,X}s previously elsewhere
    for T in (Int, Float16, Float32, Float64, BigInt, BigFloat)
        R = rand(T[1:100;], 2, 2)
        I = rand(T[1:100;], 2, 2)
        D = R + I*im
        S = sparse(D)
        spR = sparse(R)

        @test R == real.(S) == real(S)
        @test I == imag.(S) == imag(S)
        @test conj(Array(S)) == conj.(S) == conj(S)
        @test real.(spR) == R
        @test nnz(imag.(spR)) == nnz(imag(spR)) == 0
        @test abs.(S) == abs.(D)
        @test abs2.(S) == abs2.(D)

        # test aliasing of real and conj of real valued matrix
        @test real(spR) === spR
        @test conj(spR) === spR
    end
end

@testset "getindex" begin
    ni = 23
    nj = 32
    a116 = reshape(1:(ni*nj), ni, nj)
    s116 = sparse(a116)

    ad116 = diagm(0 => diag(a116))
    sd116 = sparse(ad116)

    for (aa116, ss116) in [(a116, s116), (ad116, sd116)]
        ij=11; i=3; j=2
        @test ss116[ij] == aa116[ij]
        @test ss116[(i,j)] == aa116[i,j]
        @test ss116[i,j] == aa116[i,j]
        @test ss116[i-1,j] == aa116[i-1,j]
        ss116[i,j] = 0
        @test ss116[i,j] == 0
        ss116 = sparse(aa116)

        @test ss116[:,:] == copy(ss116)

        @test convert(SparseMatrixCSC{Float32,Int32}, sd116)[2:5,:] == convert(SparseMatrixCSC{Float32,Int32}, sd116[2:5,:])

        # range indexing
        @test Array(ss116[i,:]) == aa116[i,:]
        @test Array(ss116[:,j]) == aa116[:,j]
        @test Array(ss116[i,1:2:end]) == aa116[i,1:2:end]
        @test Array(ss116[1:2:end,j]) == aa116[1:2:end,j]
        @test Array(ss116[i,end:-2:1]) == aa116[i,end:-2:1]
        @test Array(ss116[end:-2:1,j]) == aa116[end:-2:1,j]
        # float-range indexing is not supported

        # sorted vector indexing
        @test Array(ss116[i,[3:2:end-3;]]) == aa116[i,[3:2:end-3;]]
        @test Array(ss116[[3:2:end-3;],j]) == aa116[[3:2:end-3;],j]
        @test Array(ss116[i,[end-3:-2:1;]]) == aa116[i,[end-3:-2:1;]]
        @test Array(ss116[[end-3:-2:1;],j]) == aa116[[end-3:-2:1;],j]

        # unsorted vector indexing with repetition
        p = [4, 1, 2, 3, 2, 6]
        @test Array(ss116[p,:]) == aa116[p,:]
        @test Array(ss116[:,p]) == aa116[:,p]
        @test Array(ss116[p,p]) == aa116[p,p]

        # bool indexing
        li = bitrand(size(aa116,1))
        lj = bitrand(size(aa116,2))
        @test Array(ss116[li,j]) == aa116[li,j]
        @test Array(ss116[li,:]) == aa116[li,:]
        @test Array(ss116[i,lj]) == aa116[i,lj]
        @test Array(ss116[:,lj]) == aa116[:,lj]
        @test Array(ss116[li,lj]) == aa116[li,lj]

        # empty indices
        for empty in (1:0, Int[])
            @test Array(ss116[empty,:]) == aa116[empty,:]
            @test Array(ss116[:,empty]) == aa116[:,empty]
            @test Array(ss116[empty,lj]) == aa116[empty,lj]
            @test Array(ss116[li,empty]) == aa116[li,empty]
            @test Array(ss116[empty,empty]) == aa116[empty,empty]
        end

        # out of bounds indexing
        @test_throws BoundsError ss116[0, 1]
        @test_throws BoundsError ss116[end+1, 1]
        @test_throws BoundsError ss116[1, 0]
        @test_throws BoundsError ss116[1, end+1]
        for j in (1, 1:size(s116,2), 1:1, Int[1], trues(size(s116, 2)), 1:0, Int[])
            @test_throws BoundsError ss116[0:1, j]
            @test_throws BoundsError ss116[[0, 1], j]
            @test_throws BoundsError ss116[end:end+1, j]
            @test_throws BoundsError ss116[[end, end+1], j]
        end
        for i in (1, 1:size(s116,1), 1:1, Int[1], trues(size(s116, 1)), 1:0, Int[])
            @test_throws BoundsError ss116[i, 0:1]
            @test_throws BoundsError ss116[i, [0, 1]]
            @test_throws BoundsError ss116[i, end:end+1]
            @test_throws BoundsError ss116[i, [end, end+1]]
        end
    end

    # indexing by array of CartesianIndex (issue #30981)
    S = sprand(10, 10, 0.4)
    inds_sparse = S[findall(S .> 0.2)]
    M = Matrix(S)
    inds_dense = M[findall(M .> 0.2)]
    @test Array(inds_sparse) == inds_dense
    inds_out = Array([CartesianIndex(1, 1), CartesianIndex(0, 1)])
    @test_throws BoundsError S[inds_out]
    pop!(inds_out); push!(inds_out, CartesianIndex(1, 0))
    @test_throws BoundsError S[inds_out]
    pop!(inds_out); push!(inds_out, CartesianIndex(11, 1))
    @test_throws BoundsError S[inds_out]
    pop!(inds_out); push!(inds_out, CartesianIndex(1, 11))
    @test_throws BoundsError S[inds_out]

    # workaround issue #7197: comment out let-block
    #let S = SparseMatrixCSC(3, 3, UInt8[1,1,1,1], UInt8[], Int64[])
    S1290 = SparseMatrixCSC(3, 3, UInt8[1,1,1,1], UInt8[], Int64[])
        S1290[1,1] = 1
        S1290[5] = 2
        S1290[end] = 3
        @test S1290[end] == (S1290[1] + S1290[2,2])
        @test 6 == sum(diag(S1290))
        @test Array(S1290)[[3,1],1] == Array(S1290[[3,1],1])

        # check that indexing with an abstract array returns matrix
        # with same colptr and rowval eltypes as input. Tests PR 24548
        r1 = S1290[[5,9]]
        r2 = S1290[[1 2;5 9]]
        @test isa(r1, SparseVector{Int64,UInt8})
        @test isa(r2, SparseMatrixCSC{Int64,UInt8})
    # end
end

@testset "setindex" begin
    a = spzeros(Int, 10, 10)
    @test count(!iszero, a) == 0
    a[1,:] .= 1
    @test count(!iszero, a) == 10
    @test a[1,:] == sparse(fill(1,10))
    a[:,2] .= 2
    @test count(!iszero, a) == 19
    @test a[:,2] == sparse(fill(2,10))
    b = copy(a)

    # Zero-assignment behavior of setindex!(A, v, i, j)
    a[1,3] = 0
    @test nnz(a) == 19
    @test count(!iszero, a) == 18
    a[2,1] = 0
    @test nnz(a) == 19
    @test count(!iszero, a) == 18

    # Zero-assignment behavior of setindex!(A, v, I, J)
    a[1,:] .= 0
    @test nnz(a) == 19
    @test count(!iszero, a) == 9
    a[2,:] .= 0
    @test nnz(a) == 19
    @test count(!iszero, a) == 8
    a[:,1] .= 0
    @test nnz(a) == 19
    @test count(!iszero, a) == 8
    a[:,2] .= 0
    @test nnz(a) == 19
    @test count(!iszero, a) == 0
    a = copy(b)
    a[:,:] .= 0
    @test nnz(a) == 19
    @test count(!iszero, a) == 0

    # Zero-assignment behavior of setindex!(A, B::SparseMatrixCSC, I, J)
    a = copy(b)
    a[1:2,:] = spzeros(2, 10)
    @test nnz(a) == 19
    @test count(!iszero, a) == 8
    a[1:2,1:3] = sparse([1 0 1; 0 0 1])
    @test nnz(a) == 20
    @test count(!iszero, a) == 11
    a = copy(b)
    a[1:2,:] = let c = sparse(fill(1,2,10)); fill!(nonzeros(c), 0); c; end
    @test nnz(a) == 19
    @test count(!iszero, a) == 8
    a[1:2,1:3] = let c = sparse(fill(1,2,3)); c[1,2] = c[2,1] = c[2,2] = 0; c; end
    @test nnz(a) == 20
    @test count(!iszero, a) == 11

    a[1,:] = 1:10
    @test a[1,:] == sparse([1:10;])
    a[:,2] = 1:10
    @test a[:,2] == sparse([1:10;])

    a[1,1:0] = []
    @test a[1,:] == sparse([1; 1; 3:10])
    a[1:0,2] = []
    @test a[:,2] == sparse([1:10;])
    a[1,1:0] .= 0
    @test a[1,:] == sparse([1; 1; 3:10])
    a[1:0,2] .= 0
    @test a[:,2] == sparse([1:10;])
    a[1,1:0] .= 1
    @test a[1,:] == sparse([1; 1; 3:10])
    a[1:0,2] .= 1
    @test a[:,2] == sparse([1:10;])

    @test_throws BoundsError a[:,11] = spzeros(10,1)
    @test_throws BoundsError a[11,:] = spzeros(1,10)
    @test_throws BoundsError a[:,-1] = spzeros(10,1)
    @test_throws BoundsError a[-1,:] = spzeros(1,10)
    @test_throws BoundsError a[0:9] = spzeros(1,10)
    @test_throws BoundsError (a[:,11] .= 0; a)
    @test_throws BoundsError (a[11,:] .= 0; a)
    @test_throws BoundsError (a[:,-1] .= 0; a)
    @test_throws BoundsError (a[-1,:] .= 0; a)
    @test_throws BoundsError (a[0:9] .= 0; a)
    @test_throws BoundsError (a[:,11] .= 1; a)
    @test_throws BoundsError (a[11,:] .= 1; a)
    @test_throws BoundsError (a[:,-1] .= 1; a)
    @test_throws BoundsError (a[-1,:] .= 1; a)
    @test_throws BoundsError (a[0:9] .= 1; a)

    @test_throws DimensionMismatch a[1:2,1:2] = 1:3
    @test_throws DimensionMismatch a[1:2,1] = 1:3
    @test_throws DimensionMismatch a[1,1:2] = 1:3
    @test_throws DimensionMismatch a[1:2] = 1:3

    A = spzeros(Int, 10, 20)
    A[1:5,1:10] .= 10
    A[1:5,1:10] .= 10
    @test count(!iszero, A) == 50
    @test A[1:5,1:10] == fill(10, 5, 10)
    A[6:10,11:20] .= 0
    @test count(!iszero, A) == 50
    A[6:10,11:20] .= 20
    @test count(!iszero, A) == 100
    @test A[6:10,11:20] == fill(20, 5, 10)
    A[4:8,8:16] .= 15
    @test count(!iszero, A) == 121
    @test A[4:8,8:16] == fill(15, 5, 9)

    ASZ = 1000
    TSZ = 800
    A = sprand(ASZ, 2*ASZ, 0.0001)
    B = copy(A)
    nA = count(!iszero, A)
    x = A[1:TSZ, 1:(2*TSZ)]
    nx = count(!iszero, x)
    A[1:TSZ, 1:(2*TSZ)] .= 0
    nB = count(!iszero, A)
    @test nB == (nA - nx)
    A[1:TSZ, 1:(2*TSZ)] = x
    @test count(!iszero, A) == nA
    @test A == B
    A[1:TSZ, 1:(2*TSZ)] .= 10
    @test count(!iszero, A) == nB + 2*TSZ*TSZ
    A[1:TSZ, 1:(2*TSZ)] = x
    @test count(!iszero, A) == nA
    @test A == B

    A = sparse(1I, 5, 5)
    lininds = 1:10
    X=reshape([trues(10); falses(15)],5,5)
    @test A[lininds] == A[X] == [1,0,0,0,0,0,1,0,0,0]
    A[lininds] = [1:10;]
    @test A[lininds] == A[X] == 1:10
    A[lininds] = zeros(Int, 10)
    @test nnz(A) == 13
    @test count(!iszero, A) == 3
    @test A[lininds] == A[X] == zeros(Int, 10)
    c = Vector(11:20); c[1] = c[3] = 0
    A[lininds] = c
    @test nnz(A) == 13
    @test count(!iszero, A) == 11
    @test A[lininds] == A[X] == c
    A = sparse(1I, 5, 5)
    A[lininds] = c
    @test nnz(A) == 12
    @test count(!iszero, A) == 11
    @test A[lininds] == A[X] == c

    let # prevent assignment to I from overwriting UniformSampling in enclosing scope
        S = sprand(50, 30, 0.5, x -> round.(Int, rand(x) * 100))
        I = sprand(Bool, 50, 30, 0.2)
        FS = Array(S)
        FI = Array(I)
        @test sparse(FS[FI]) == S[I] == S[FI]
        @test sum(S[FI]) + sum(S[.!FI]) == sum(S)
        @test count(!iszero, I) == count(I)

        sumS1 = sum(S)
        sumFI = sum(S[FI])
        nnzS1 = nnz(S)
        S[FI] .= 0
        sumS2 = sum(S)
        cnzS2 = count(!iszero, S)
        @test sum(S[FI]) == 0
        @test nnz(S) == nnzS1
        @test (sum(S) + sumFI) == sumS1

        S[FI] .= 10
        nnzS3 = nnz(S)
        @test sum(S) == sumS2 + 10*sum(FI)
        S[FI] .= 0
        @test sum(S) == sumS2
        @test nnz(S) == nnzS3
        @test count(!iszero, S) == cnzS2

        S[FI] .= [1:sum(FI);]
        @test sum(S) == sumS2 + sum(1:sum(FI))

        S = sprand(50, 30, 0.5, x -> round.(Int, rand(x) * 100))
        N = length(S) >> 2
        I = randperm(N) .* 4
        J = randperm(N)
        sumS1 = sum(S)
        sumS2 = sum(S[I])
        S[I] .= 0
        @test sum(S) == (sumS1 - sumS2)
        S[I] .= J
        @test sum(S) == (sumS1 - sumS2 + sum(J))
    end

    # setindex with a Matrix{Bool}
    Is = fill(false, 10, 10)
    Is[1, 1] = true
    Is[10, 10] = true
    A = sprand(10, 10, 0.2)
    A[Is] = [0.1, 0.5]
    @test A[1, 1] == 0.1
    @test A[10, 10] == 0.5
    A = spzeros(10, 10)
    A[Is] = [0.1, 0.5]
    @test nnz(A) == 2
end

@testset "dropstored!" begin
    A = spzeros(Int, 10, 10)
    # Introduce nonzeros in row and column two
    A[1,:] .= 1
    A[:,2] .= 2
    @test nnz(A) == 19

    # Test argument bounds checking for dropstored!(A, i, j)
    @test_throws BoundsError SparseArrays.dropstored!(A, 0, 1)
    @test_throws BoundsError SparseArrays.dropstored!(A, 1, 0)
    @test_throws BoundsError SparseArrays.dropstored!(A, 1, 11)
    @test_throws BoundsError SparseArrays.dropstored!(A, 11, 1)

    # Test argument bounds checking for dropstored!(A, I, J)
    @test_throws BoundsError SparseArrays.dropstored!(A, 0:1, 1:1)
    @test_throws BoundsError SparseArrays.dropstored!(A, 1:1, 0:1)
    @test_throws BoundsError SparseArrays.dropstored!(A, 10:11, 1:1)
    @test_throws BoundsError SparseArrays.dropstored!(A, 1:1, 10:11)

    # Test behavior of dropstored!(A, i, j)
    # --> Test dropping a single stored entry
    SparseArrays.dropstored!(A, 1, 2)
    @test nnz(A) == 18
    # --> Test dropping a single nonstored entry
    SparseArrays.dropstored!(A, 2, 1)
    @test nnz(A) == 18

    # Test behavior of dropstored!(A, I, J) and derivs.
    # --> Test dropping a single row including stored and nonstored entries
    SparseArrays.dropstored!(A, 1, :)
    @test nnz(A) == 9
    # --> Test dropping a single column including stored and nonstored entries
    SparseArrays.dropstored!(A, :, 2)
    @test nnz(A) == 0
    # --> Introduce nonzeros in rows one and two and columns two and three
    A[1:2,:] .= 1
    A[:,2:3] .= 2
    @test nnz(A) == 36
    # --> Test dropping multiple rows containing stored and nonstored entries
    SparseArrays.dropstored!(A, 1:3, :)
    @test nnz(A) == 14
    # --> Test dropping multiple columns containing stored and nonstored entries
    SparseArrays.dropstored!(A, :, 2:4)
    @test nnz(A) == 0
    # --> Introduce nonzeros in every other row
    A[1:2:9, :] .= 1
    @test nnz(A) == 50
    # --> Test dropping a block of the matrix towards the upper left
    SparseArrays.dropstored!(A, 2:5, 2:5)
    @test nnz(A) == 42
    # --> Test dropping all elements
    SparseArrays.dropstored!(A, :)
    @test nnz(A) == 0
    A[1:2:9, :] .= 1
    @test nnz(A) == 50
    SparseArrays.dropstored!(A, :, :)
    @test nnz(A) == 0
end

@testset "issue #7507" begin
    @test (i7507=sparsevec(Dict{Int64, Float64}(), 10))==spzeros(10)
end

@testset "issue #7650" begin
    S = spzeros(3, 3)
    @test size(reshape(S, 9, 1)) == (9,1)
end

@testset "sparsevec from matrices" begin
    X = Matrix(1.0I, 5, 5)
    M = rand(5,4)
    C = spzeros(3,3)
    SX = sparse(X); SM = sparse(M)
    VX = vec(X); VSX = vec(SX)
    VM = vec(M); VSM1 = vec(SM); VSM2 = sparsevec(M)
    VC = vec(C)
    @test VX == VSX
    @test VM == VSM1
    @test VM == VSM2
    @test size(VC) == (9,)
    @test nnz(VC) == 0
    @test nnz(VSX) == 5
end

@testset "issue #7677" begin
    A = sprand(5,5,0.5,(n)->rand(Float64,n))
    ACPY = copy(A)
    B = reshape(A,25,1)
    @test A == ACPY
end

@testset "issue #8225" begin
    @test_throws ArgumentError sparse([0],[-1],[1.0],2,2)
end

@testset "issue #8363" begin
    @test_throws ArgumentError sparsevec(Dict(-1=>1,1=>2))
end

@testset "issue #8976" begin
    @test conj.(sparse([1im])) == sparse(conj([1im]))
    @test conj!(sparse([1im])) == sparse(conj!([1im]))
end

@testset "issue #9525" begin
    @test_throws ArgumentError sparse([3], [5], 1.0, 3, 3)
end

@testset "argmax, argmin, findmax, findmin" begin
    S = sprand(100,80, 0.5)
    A = Array(S)
    @test argmax(S) == argmax(A)
    @test argmin(S) == argmin(A)
    @test findmin(S) == findmin(A)
    @test findmax(S) == findmax(A)
    for region in [(1,), (2,), (1,2)], m in [findmax, findmin]
        @test m(S, dims=region) == m(A, dims=region)
    end

    S = spzeros(10,8)
    A = Array(S)
    @test argmax(S) == argmax(A) == CartesianIndex(1,1)
    @test argmin(S) == argmin(A) == CartesianIndex(1,1)

    A = Matrix{Int}(I, 0, 0)
    S = sparse(A)
    iA = try argmax(A); catch; end
    iS = try argmax(S); catch; end
    @test iA === iS === nothing
    iA = try argmin(A); catch; end
    iS = try argmin(S); catch; end
    @test iA === iS === nothing
end

@testset "findmin/findmax/minimum/maximum" begin
    A = sparse([1.0 5.0 6.0;
                5.0 2.0 4.0])
    for (tup, rval, rind) in [((1,), [1.0 2.0 4.0], [CartesianIndex(1,1) CartesianIndex(2,2) CartesianIndex(2,3)]),
                              ((2,), reshape([1.0,2.0], 2, 1), reshape([CartesianIndex(1,1),CartesianIndex(2,2)], 2, 1)),
                              ((1,2), fill(1.0,1,1),fill(CartesianIndex(1,1),1,1))]
        @test findmin(A, tup) == (rval, rind)
    end

    for (tup, rval, rind) in [((1,), [5.0 5.0 6.0], [CartesianIndex(2,1) CartesianIndex(1,2) CartesianIndex(1,3)]),
                              ((2,), reshape([6.0,5.0], 2, 1), reshape([CartesianIndex(1,3),CartesianIndex(2,1)], 2, 1)),
                              ((1,2), fill(6.0,1,1),fill(CartesianIndex(1,3),1,1))]
        @test findmax(A, tup) == (rval, rind)
    end

    #issue 23209

    A = sparse([1.0 5.0 6.0;
                NaN 2.0 4.0])
    for (tup, rval, rind) in [((1,), [NaN 2.0 4.0], [CartesianIndex(2,1) CartesianIndex(2,2) CartesianIndex(2,3)]),
                              ((2,), reshape([1.0, NaN], 2, 1), reshape([CartesianIndex(1,1),CartesianIndex(2,1)], 2, 1)),
                              ((1,2), fill(NaN,1,1),fill(CartesianIndex(2,1),1,1))]
        @test isequal(findmin(A, tup), (rval, rind))
    end

    for (tup, rval, rind) in [((1,), [NaN 5.0 6.0], [CartesianIndex(2,1) CartesianIndex(1,2) CartesianIndex(1,3)]),
                              ((2,), reshape([6.0, NaN], 2, 1), reshape([CartesianIndex(1,3),CartesianIndex(2,1)], 2, 1)),
                              ((1,2), fill(NaN,1,1),fill(CartesianIndex(2,1),1,1))]
        @test isequal(findmax(A, tup), (rval, rind))
    end

    A = sparse([1.0 NaN 6.0;
                NaN 2.0 4.0])
    for (tup, rval, rind) in [((1,), [NaN NaN 4.0], [CartesianIndex(2,1) CartesianIndex(1,2) CartesianIndex(2,3)]),
                              ((2,), reshape([NaN, NaN], 2, 1), reshape([CartesianIndex(1,2),CartesianIndex(2,1)], 2, 1)),
                              ((1,2), fill(NaN,1,1),fill(CartesianIndex(2,1),1,1))]
        @test isequal(findmin(A, tup), (rval, rind))
    end

    for (tup, rval, rind) in [((1,), [NaN NaN 6.0], [CartesianIndex(2,1) CartesianIndex(1,2) CartesianIndex(1,3)]),
                              ((2,), reshape([NaN, NaN], 2, 1), reshape([CartesianIndex(1,2),CartesianIndex(2,1)], 2, 1)),
                              ((1,2), fill(NaN,1,1),fill(CartesianIndex(2,1),1,1))]
        @test isequal(findmax(A, tup), (rval, rind))
    end

    A = sparse([Inf -Inf Inf  -Inf;
                Inf  Inf -Inf -Inf])
    for (tup, rval, rind) in [((1,), [Inf -Inf -Inf -Inf], [CartesianIndex(1,1) CartesianIndex(1,2) CartesianIndex(2,3) CartesianIndex(1,4)]),
                              ((2,), reshape([-Inf -Inf], 2, 1), reshape([CartesianIndex(1,2),CartesianIndex(2,3)], 2, 1)),
                              ((1,2), fill(-Inf,1,1),fill(CartesianIndex(1,2),1,1))]
        @test isequal(findmin(A, tup), (rval, rind))
    end

    for (tup, rval, rind) in [((1,), [Inf Inf Inf -Inf], [CartesianIndex(1,1) CartesianIndex(2,2) CartesianIndex(1,3) CartesianIndex(1,4)]),
                              ((2,), reshape([Inf Inf], 2, 1), reshape([CartesianIndex(1,1),CartesianIndex(2,1)], 2, 1)),
                              ((1,2), fill(Inf,1,1),fill(CartesianIndex(1,1),1,1))]
        @test isequal(findmax(A, tup), (rval, rind))
    end

    A = sparse([BigInt(10)])
    for (tup, rval, rind) in [((2,), [BigInt(10)], [1])]
        @test isequal(findmin(A, dims=tup), (rval, rind))
    end

    for (tup, rval, rind) in [((2,), [BigInt(10)], [1])]
        @test isequal(findmax(A, dims=tup), (rval, rind))
    end

    A = sparse([BigInt(-10)])
    for (tup, rval, rind) in [((2,), [BigInt(-10)], [1])]
        @test isequal(findmin(A, dims=tup), (rval, rind))
    end

    for (tup, rval, rind) in [((2,), [BigInt(-10)], [1])]
        @test isequal(findmax(A, dims=tup), (rval, rind))
    end

    A = sparse([BigInt(10) BigInt(-10)])
    for (tup, rval, rind) in [((2,), reshape([BigInt(-10)], 1, 1), reshape([CartesianIndex(1,2)], 1, 1))]
        @test isequal(findmin(A, dims=tup), (rval, rind))
    end

    for (tup, rval, rind) in [((2,), reshape([BigInt(10)], 1, 1), reshape([CartesianIndex(1,1)], 1, 1))]
        @test isequal(findmax(A, dims=tup), (rval, rind))
    end

    # sparse arrays of types without zero(T) are forbidden
    @test_throws MethodError sparse(["a", "b"])
end

# Support the case when user defined `zero` and `isless` for non-numerical type
struct CustomType
    x::String
end
Base.zero(::Type{CustomType}) = CustomType("")
Base.zero(x::CustomType) = zero(CustomType)
Base.isless(x::CustomType, y::CustomType) = isless(x.x, y.x)
@testset "findmin/findmax for non-numerical type" begin
    A = sparse([CustomType("a"), CustomType("b")])

    for (tup, rval, rind) in [((1,), [CustomType("a")], [1])]
        @test isequal(findmin(A, dims=tup), (rval, rind))
    end

    for (tup, rval, rind) in [((1,), [CustomType("b")], [2])]
        @test isequal(findmax(A, dims=tup), (rval, rind))
    end
end

@testset "rotations" begin
    a = sparse( [1,1,2,3], [1,3,4,1], [1,2,3,4] )

    @test rot180(a,2) == a
    @test rot180(a,1) == sparse( [3,3,2,1], [4,2,1,4], [1,2,3,4] )
    @test rotr90(a,1) == sparse( [1,3,4,1], [3,3,2,1], [1,2,3,4] )
    @test rotl90(a,1) == sparse( [4,2,1,4], [1,1,2,3], [1,2,3,4] )
    @test rotl90(a,2) == rot180(a)
    @test rotr90(a,2) == rot180(a)
    @test rotl90(a,3) == rotr90(a)
    @test rotr90(a,3) == rotl90(a)

    #ensure we have preserved the correct dimensions!

    a = sparse(1.0I, 3, 5)
    @test size(rot180(a)) == (3,5)
    @test size(rotr90(a)) == (5,3)
    @test size(rotl90(a)) == (5,3)
end

function test_getindex_algs(A::SparseMatrixCSC{Tv,Ti}, I::AbstractVector, J::AbstractVector, alg::Int) where {Tv,Ti}
    # Sorted vectors for indexing rows.
    # Similar to getindex_general but without the transpose trick.
    (m, n) = size(A)
    !isempty(I) && ((I[1] < 1) || (I[end] > m)) && BoundsError()
    if !isempty(J)
        minj, maxj = extrema(J)
        ((minj < 1) || (maxj > n)) && BoundsError()
    end

    (alg == 0) ? SparseArrays.getindex_I_sorted_bsearch_A(A, I, J) :
    (alg == 1) ? SparseArrays.getindex_I_sorted_bsearch_I(A, I, J) :
    SparseArrays.getindex_I_sorted_linear(A, I, J)
end

@testset "test_getindex_algs" begin
    M=2^14
    N=2^4
    Irand = randperm(M)
    Jrand = randperm(N)
    SA = [sprand(M, N, d) for d in [1., 0.1, 0.01, 0.001, 0.0001, 0.]]
    IA = [sort(Irand[1:round(Int,n)]) for n in [M, M*0.1, M*0.01, M*0.001, M*0.0001, 0.]]
    debug = false

    if debug
        println("row sizes: $([round(Int,nnz(S)/size(S, 2)) for S in SA])")
        println("I sizes: $([length(I) for I in IA])")
        @printf("    S    |    I    | binary S | binary I |  linear  | best\n")
    end

    J = Jrand
    for I in IA
        for S in SA
            res = Any[1,2,3]
            times = Float64[0,0,0]
            best = [typemax(Float64), 0]
            for searchtype in [0, 1, 2]
                GC.gc()
                tres = @timed test_getindex_algs(S, I, J, searchtype)
                res[searchtype+1] = tres[1]
                times[searchtype+1] = tres[2]
                if best[1] > tres[2]
                    best[1] = tres[2]
                    best[2] = searchtype
                end
            end

            if debug
                @printf(" %7d | %7d | %4.2e | %4.2e | %4.2e | %s\n", round(Int,nnz(S)/size(S, 2)), length(I), times[1], times[2], times[3],
                            (0 == best[2]) ? "binary S" : (1 == best[2]) ? "binary I" : "linear")
            end
            if res[1] != res[2]
                println("1 and 2")
            elseif res[2] != res[3]
                println("2, 3")
            end
            @test res[1] == res[2] == res[3]
        end
    end

    M = 2^8
    N=2^3
    Irand = randperm(M)
    Jrand = randperm(N)
    II = sort([Irand; Irand; Irand])
    J = [Jrand; Jrand]

    SA = [sprand(M, N, d) for d in [1., 0.1, 0.01, 0.001, 0.0001, 0.]]
    for S in SA
        res = Any[1,2,3]
        for searchtype in [0, 1, 2]
            res[searchtype+1] = test_getindex_algs(S, II, J, searchtype)
        end

        @test res[1] == res[2] == res[3]
    end

    M = 2^14
    N=2^4
    II = randperm(M)
    J = randperm(N)
    Jsorted = sort(J)

    SA = [sprand(M, N, d) for d in [1., 0.1, 0.01, 0.001, 0.0001, 0.]]
    IA = [II[1:round(Int,n)] for n in [M, M*0.1, M*0.01, M*0.001, M*0.0001, 0.]]
    debug = false
    if debug
        @printf("         |         |         |        times        |        memory       |\n")
        @printf("    S    |    I    |    J    |  sorted  | unsorted |  sorted  | unsorted |\n")
    end
    for I in IA
        Isorted = sort(I)
        for S in SA
            GC.gc()
            ru = @timed S[I, J]
            GC.gc()
            rs = @timed S[Isorted, Jsorted]
            if debug
                @printf(" %7d | %7d | %7d | %4.2e | %4.2e | %4.2e | %4.2e |\n", round(Int,nnz(S)/size(S, 2)), length(I), length(J), rs[2], ru[2], rs[3], ru[3])
            end
        end
    end
end

@testset "getindex bounds checking" begin
    S = sprand(10, 10, 0.1)
    @test_throws BoundsError S[[0,1,2], [1,2]]
    @test_throws BoundsError S[[1,2], [0,1,2]]
    @test_throws BoundsError S[[0,2,1], [1,2]]
    @test_throws BoundsError S[[2,1], [0,1,2]]
end

@testset "test that sparse / sparsevec constructors work for AbstractMatrix subtypes" begin
    D = Diagonal(fill(1,10))
    sm = sparse(D)
    sv = sparsevec(D)

    @test count(!iszero, sm) == 10
    @test count(!iszero, sv) == 10

    @test count(!iszero, sparse(Diagonal(Int[]))) == 0
    @test count(!iszero, sparsevec(Diagonal(Int[]))) == 0
end

@testset "explicit zeros" begin
    if Base.USE_GPL_LIBS
        a = SparseMatrixCSC(2, 2, [1, 3, 5], [1, 2, 1, 2], [1.0, 0.0, 0.0, 1.0])
        @test lu(a)\[2.0, 3.0] ≈ [2.0, 3.0]
        @test cholesky(a)\[2.0, 3.0] ≈ [2.0, 3.0]
    end
end

@testset "issue #9917" begin
    @test sparse([]') == reshape(sparse([]), 1, 0)
    @test Array(sparse([])) == zeros(0)
    @test_throws BoundsError sparse([])[1]
    @test_throws BoundsError sparse([])[1] = 1
    x = sparse(1.0I, 100, 100)
    @test_throws BoundsError x[-10:10]
end

@testset "issue #10407" begin
    @test maximum(spzeros(5, 5)) == 0.0
    @test minimum(spzeros(5, 5)) == 0.0
end

@testset "issue #10411" begin
    for (m,n) in ((2,-2),(-2,2),(-2,-2))
        @test_throws ArgumentError spzeros(m,n)
        @test_throws ArgumentError sparse(1.0I, m, n)
        @test_throws ArgumentError sprand(m,n,0.2)
    end
end

@testset "issues #10837 & #32466, sparse constructors from special matrices" begin
    T = Tridiagonal(randn(4),randn(5),randn(4))
    S = sparse(T)
    S2 = SparseMatrixCSC(T)
    @test Array(T) == Array(S) == Array(S2)
    @test S == S2
    T = SymTridiagonal(randn(5),rand(4))
    S = sparse(T)
    S2 = SparseMatrixCSC(T)
    @test Array(T) == Array(S) == Array(S2)
    @test S == S2
    B = Bidiagonal(randn(5),randn(4),:U)
    S = sparse(B)
    S2 = SparseMatrixCSC(B)
    @test Array(B) == Array(S) == Array(S2)
    @test S == S2
    B = Bidiagonal(randn(5),randn(4),:L)
    S = sparse(B)
    S2 = SparseMatrixCSC(B)
    @test Array(B) == Array(S) == Array(S2)
    @test S == S2
    D = Diagonal(randn(5))
    S = sparse(D)
    S2 = SparseMatrixCSC(D)
    @test Array(D) == Array(S) == Array(S2)
    @test S == S2
end

@testset "error conditions for reshape, and dropdims" begin
    local A = sprand(Bool, 5, 5, 0.2)
    @test_throws DimensionMismatch reshape(A,(20, 2))
    @test_throws ArgumentError dropdims(A,dims=(1, 1))
end

@testset "float" begin
    local A
    A = sprand(Bool, 5, 5, 0.0)
    @test eltype(float(A)) == Float64  # issue #11658
    A = sprand(Bool, 5, 5, 0.2)
    @test float(A) == float(Array(A))
end

@testset "complex" begin
    A = sprand(Bool, 5, 5, 0.0)
    @test eltype(complex(A)) == Complex{Bool}
    A = sprand(Bool, 5, 5, 0.2)
    @test complex(A) == complex(Array(A))
end

@testset "sparsevec" begin
    local A = sparse(fill(1, 5, 5))
    @test sparsevec(A) == fill(1, 25)
    @test sparsevec([1:5;], 1) == fill(1, 5)
    @test_throws ArgumentError sparsevec([1:5;], [1:4;])
end

@testset "sparse" begin
    local A = sparse(fill(1, 5, 5))
    @test sparse(A) == A
    @test sparse([1:5;], [1:5;], 1) == sparse(1.0I, 5, 5)
end

@testset "one(A::SparseMatrixCSC)" begin
    @test_throws DimensionMismatch one(sparse([1 1 1; 1 1 1]))
    @test one(sparse([1 1; 1 1]))::SparseMatrixCSC == [1 0; 0 1]
end

@testset "istriu/istril" begin
    local A = fill(1, 5, 5)
    @test istriu(sparse(triu(A)))
    @test !istriu(sparse(A))
    @test istril(sparse(tril(A)))
    @test !istril(sparse(A))
end

@testset "droptol" begin
    local A = guardseed(1234321) do
        triu(sprand(10, 10, 0.2))
    end
    @test getcolptr(SparseArrays.droptol!(A, 0.01)) == [1, 2, 2, 3, 4, 5, 5, 6, 8, 10, 13]
    @test isequal(SparseArrays.droptol!(sparse([1], [1], [1]), 1), SparseMatrixCSC(1, 1, Int[1, 1], Int[], Int[]))
end

@testset "dropzeros[!]" begin
    smalldim = 5
    largedim = 10
    nzprob = 0.4
    targetnumposzeros = 5
    targetnumnegzeros = 5
    for (m, n) in ((largedim, largedim), (smalldim, largedim), (largedim, smalldim))
        local A = sprand(m, n, nzprob)
        struczerosA = findall(x -> x == 0, A)
        poszerosinds = unique(rand(struczerosA, targetnumposzeros))
        negzerosinds = unique(rand(struczerosA, targetnumnegzeros))
        Aposzeros = copy(A)
        Aposzeros[poszerosinds] .= 2
        Anegzeros = copy(A)
        Anegzeros[negzerosinds] .= -2
        Abothsigns = copy(Aposzeros)
        Abothsigns[negzerosinds] .= -2
        map!(x -> x == 2 ? 0.0 : x, nonzeros(Aposzeros), nonzeros(Aposzeros))
        map!(x -> x == -2 ? -0.0 : x, nonzeros(Anegzeros), nonzeros(Anegzeros))
        map!(x -> x == 2 ? 0.0 : x == -2 ? -0.0 : x, nonzeros(Abothsigns), nonzeros(Abothsigns))
        for Awithzeros in (Aposzeros, Anegzeros, Abothsigns)
            # Basic functionality / dropzeros!
            @test dropzeros!(copy(Awithzeros)) == A
            # Basic functionality / dropzeros
            @test dropzeros(Awithzeros) == A
            # Check trimming works as expected
            @test length(nonzeros(dropzeros!(copy(Awithzeros)))) == length(nonzeros(A))
            @test length(rowvals(dropzeros!(copy(Awithzeros)))) == length(rowvals(A))
        end
    end
    # original lone dropzeros test
    local A = sparse([1 2 3; 4 5 6; 7 8 9])
    nonzeros(A)[2] = nonzeros(A)[6] = nonzeros(A)[7] = 0
    @test getcolptr(dropzeros!(A)) == [1, 3, 5, 7]
    # test for issue #5169, modified for new behavior following #15242/#14798
    @test nnz(sparse([1, 1], [1, 2], [0.0, -0.0])) == 2
    @test nnz(dropzeros!(sparse([1, 1], [1, 2], [0.0, -0.0]))) == 0
    # test for issue #5437, modified for new behavior following #15242/#14798
    @test nnz(sparse([1, 2, 3], [1, 2, 3], [0.0, 1.0, 2.0])) == 3
    @test nnz(dropzeros!(sparse([1, 2, 3],[1, 2, 3],[0.0, 1.0, 2.0]))) == 2
end

@testset "trace" begin
    @test_throws DimensionMismatch tr(spzeros(5,6))
    @test tr(sparse(1.0I, 5, 5)) == 5
end

@testset "spdiagm" begin
    x = fill(1, 2)
    @test spdiagm(0 => x, -1 => x) == [1 0 0; 1 1 0; 0 1 0]
    @test spdiagm(0 => x,  1 => x) == [1 1 0; 0 1 1; 0 0 0]

    for (x, y) in ((rand(5), rand(4)),(sparse(rand(5)), sparse(rand(4))))
        @test spdiagm(-1 => x)::SparseMatrixCSC         == diagm(-1 => x)
        @test spdiagm( 0 => x)::SparseMatrixCSC         == diagm( 0 => x) == sparse(Diagonal(x))
        @test spdiagm(-1 => x)::SparseMatrixCSC         == diagm(-1 => x)
        @test spdiagm(0 => x, -1 => y)::SparseMatrixCSC == diagm(0 => x, -1 => y)
        @test spdiagm(0 => x,  1 => y)::SparseMatrixCSC == diagm(0 => x,  1 => y)
    end
    # promotion
    @test spdiagm(0 => [1,2], 1 => [3.5], -1 => [4+5im]) == [1 3.5; 4+5im 2]

    # non-square:
    for m=1:4, n=2:4
        if m < 2 || n < 3
            @test_throws DimensionMismatch spdiagm(m,n, 0 => x,  1 => x)
        else
            M = zeros(m,n)
            M[1:2,1:3] = [1 1 0; 0 1 1]
            @test spdiagm(m,n, 0 => x,  1 => x) == M
        end
    end
end

@testset "diag" begin
    for T in (Float64, ComplexF64)
        S1 = sprand(T,  5,  5, 0.5)
        S2 = sprand(T, 10,  5, 0.5)
        S3 = sprand(T,  5, 10, 0.5)
        for S in (S1, S2, S3)
            local A = Matrix(S)
            @test diag(S)::SparseVector{T,Int} == diag(A)
            for k in -size(S,1):size(S,2)
                @test diag(S, k)::SparseVector{T,Int} == diag(A, k)
            end
            @test_throws ArgumentError diag(S, -size(S,1)-1)
            @test_throws ArgumentError diag(S,  size(S,2)+1)
        end
    end
    # test that stored zeros are still stored zeros in the diagonal
    S = sparse([1,3],[1,3],[0.0,0.0]); V = diag(S)
    @test nonzeroinds(V) == [1,3]
    @test nonzeros(V) == [0.0,0.0]
end

@testset "expandptr" begin
    local A = sparse(1.0I, 5, 5)
    @test SparseArrays.expandptr(getcolptr(A)) == 1:5
    A[1,2] = 1
    @test SparseArrays.expandptr(getcolptr(A)) == [1; 2; 2; 3; 4; 5]
    @test_throws ArgumentError SparseArrays.expandptr([2; 3])
end

@testset "triu/tril" begin
    n = 5
    local A = sprand(n, n, 0.2)
    AF = Array(A)
    @test Array(triu(A,1)) == triu(AF,1)
    @test Array(tril(A,1)) == tril(AF,1)
    @test Array(triu!(copy(A), 2)) == triu(AF,2)
    @test Array(tril!(copy(A), 2)) == tril(AF,2)
    @test tril(A, -n - 2) == zero(A)
    @test tril(A, n) == A
    @test triu(A, -n) == A
    @test triu(A, n + 2) == zero(A)

    # fkeep trim option
    @test isequal(length(rowvals(tril!(sparse([1,2,3], [1,2,3], [1,2,3], 3, 4), -1))), 0)
end

@testset "norm" begin
    local A
    A = sparse(Int[],Int[],Float64[],0,0)
    @test norm(A) == zero(eltype(A))
    A = sparse([1.0])
    @test norm(A) == 1.0
    @test_throws ArgumentError opnorm(sprand(5,5,0.2),3)
    @test_throws ArgumentError opnorm(sprand(5,5,0.2),2)
end

@testset "ishermitian/issymmetric" begin
    local A
    # real matrices
    A = sparse(1.0I, 5, 5)
    @test ishermitian(A) == true
    @test issymmetric(A) == true
    A[1,3] = 1.0
    @test ishermitian(A) == false
    @test issymmetric(A) == false
    A[3,1] = 1.0
    @test ishermitian(A) == true
    @test issymmetric(A) == true

    # complex matrices
    A = sparse((1.0 + 1.0im)I, 5, 5)
    @test ishermitian(A) == false
    @test issymmetric(A) == true
    A[1,4] = 1.0 + im
    @test ishermitian(A) == false
    @test issymmetric(A) == false

    A = sparse(ComplexF64(1)I, 5, 5)
    A[3,2] = 1.0 + im
    @test ishermitian(A) == false
    @test issymmetric(A) == false
    A[2,3] = 1.0 - im
    @test ishermitian(A) == true
    @test issymmetric(A) == false

    A = sparse(zeros(5,5))
    @test ishermitian(A) == true
    @test issymmetric(A) == true

    # explicit zeros
    A = sparse(ComplexF64(1)I, 5, 5)
    A[3,1] = 2
    nonzeros(A)[2] = 0.0
    @test ishermitian(A) == true
    @test issymmetric(A) == true

    # 15504
    m = n = 5
    colptr = [1, 5, 9, 13, 13, 17]
    rowval = [1, 2, 3, 5, 1, 2, 3, 5, 1, 2, 3, 5, 1, 2, 3, 5]
    nzval = [0.0, 0.0, 0.0, 0.0, 0.0, 1.0, 1.0, 1.0, 0.0, 1.0, 1.0, 1.0, 0.0, 1.0, 1.0, 1.0]
    A = SparseMatrixCSC(m, n, colptr, rowval, nzval)
    @test issymmetric(A) == true
    nonzeros(A)[end - 3]  = 2.0
    @test issymmetric(A) == false

    # 16521
    @test issymmetric(sparse([0 0; 1 0])) == false
    @test issymmetric(sparse([0 1; 0 0])) == false
    @test issymmetric(sparse([0 0; 1 1])) == false
    @test issymmetric(sparse([1 0; 1 0])) == false
    @test issymmetric(sparse([0 1; 1 0])) == true
    @test issymmetric(sparse([1 1; 1 0])) == true
end

@testset "equality ==" begin
    A1 = sparse(1.0I, 10, 10)
    A2 = sparse(1.0I, 10, 10)
    nonzeros(A1)[end]=0
    @test A1!=A2
    nonzeros(A1)[end]=1
    @test A1==A2
    A1[1:4,end] .= 1
    @test A1!=A2
    nonzeros(A1)[end-4:end-1].=0
    @test A1==A2
    A2[1:4,end-1] .= 1
    @test A1!=A2
    nonzeros(A2)[end-5:end-2].=0
    @test A1==A2
    A2[2:3,1] .= 1
    @test A1!=A2
    nonzeros(A2)[2:3].=0
    @test A1==A2
    A1[2:5,1] .= 1
    @test A1!=A2
    nonzeros(A1)[2:5].=0
    @test A1==A2
    @test sparse([1,1,0])!=sparse([0,1,1])
end

@testset "UniformScaling" begin
    local A = sprandn(10, 10, 0.5)
    @test A + I == Array(A) + I
    @test I + A == I + Array(A)
    @test A - I == Array(A) - I
    @test I - A == I - Array(A)
end

@testset "issue #12177, error path if triplet vectors are not all the same length" begin
    @test_throws ArgumentError sparse([1,2,3], [1,2], [1,2,3], 3, 3)
    @test_throws ArgumentError sparse([1,2,3], [1,2,3], [1,2], 3, 3)
end

@testset "issue #12118: sparse matrices are closed under +, -, min, max" begin
    A12118 = sparse([1,2,3,4,5], [1,2,3,4,5], [1,2,3,4,5])
    B12118 = sparse([1,2,4,5],   [1,2,3,5],   [2,1,-1,-2])

    @test A12118 + B12118 == sparse([1,2,3,4,4,5], [1,2,3,3,4,5], [3,3,3,-1,4,3])
    @test typeof(A12118 + B12118) == SparseMatrixCSC{Int,Int}

    @test A12118 - B12118 == sparse([1,2,3,4,4,5], [1,2,3,3,4,5], [-1,1,3,1,4,7])
    @test typeof(A12118 - B12118) == SparseMatrixCSC{Int,Int}

    @test max.(A12118, B12118) == sparse([1,2,3,4,5], [1,2,3,4,5], [2,2,3,4,5])
    @test typeof(max.(A12118, B12118)) == SparseMatrixCSC{Int,Int}

    @test min.(A12118, B12118) == sparse([1,2,4,5], [1,2,3,5], [1,1,-1,-2])
    @test typeof(min.(A12118, B12118)) == SparseMatrixCSC{Int,Int}
end

@testset "sparse matrix norms" begin
    Ac = sprandn(10,10,.1) + im* sprandn(10,10,.1)
    Ar = sprandn(10,10,.1)
    Ai = ceil.(Int,Ar*100)
    @test opnorm(Ac,1) ≈ opnorm(Array(Ac),1)
    @test opnorm(Ac,Inf) ≈ opnorm(Array(Ac),Inf)
    @test norm(Ac) ≈ norm(Array(Ac))
    @test opnorm(Ar,1) ≈ opnorm(Array(Ar),1)
    @test opnorm(Ar,Inf) ≈ opnorm(Array(Ar),Inf)
    @test norm(Ar) ≈ norm(Array(Ar))
    @test opnorm(Ai,1) ≈ opnorm(Array(Ai),1)
    @test opnorm(Ai,Inf) ≈ opnorm(Array(Ai),Inf)
    @test norm(Ai) ≈ norm(Array(Ai))
    Ai = trunc.(Int, Ar*100)
    @test opnorm(Ai,1) ≈ opnorm(Array(Ai),1)
    @test opnorm(Ai,Inf) ≈ opnorm(Array(Ai),Inf)
    @test norm(Ai) ≈ norm(Array(Ai))
    Ai = round.(Int, Ar*100)
    @test opnorm(Ai,1) ≈ opnorm(Array(Ai),1)
    @test opnorm(Ai,Inf) ≈ opnorm(Array(Ai),Inf)
    @test norm(Ai) ≈ norm(Array(Ai))
    # make certain entries in nzval beyond
    # the range specified in colptr do not
    # impact norm of a sparse matrix
    foo = sparse(1.0I, 4, 4)
    resize!(nonzeros(foo), 5)
    setindex!(nonzeros(foo), NaN, 5)
    @test norm(foo) == 2.0

    # Test (m x 1) sparse matrix
    colM = sprandn(10, 1, 0.6)
    @test opnorm(colM, 1) ≈ opnorm(Array(colM), 1)
    @test opnorm(colM) ≈ opnorm(Array(colM))
    @test opnorm(colM, Inf) ≈ opnorm(Array(colM), Inf)
    @test_throws ArgumentError opnorm(colM, 3)

    # Test (1 x n) sparse matrix
    rowM = sprandn(1, 10, 0.6)
    @test opnorm(rowM, 1) ≈ opnorm(Array(rowM), 1)
    @test opnorm(rowM) ≈ opnorm(Array(rowM))
    @test opnorm(rowM, Inf) ≈ opnorm(Array(rowM), Inf)
    @test_throws ArgumentError opnorm(rowM, 3)
end

@testset "sparse matrix cond" begin
    local A = sparse(reshape([1.0], 1, 1))
    Ac = sprandn(20, 20,.5) + im*sprandn(20, 20,.5)
    Ar = sprandn(20, 20,.5) + eps()*I
    @test cond(A, 1) == 1.0
    # For a discussion of the tolerance, see #14778
    if Base.USE_GPL_LIBS
        @test 0.99 <= cond(Ar, 1) \ opnorm(Ar, 1) * opnorm(inv(Array(Ar)), 1) < 3
        @test 0.99 <= cond(Ac, 1) \ opnorm(Ac, 1) * opnorm(inv(Array(Ac)), 1) < 3
        @test 0.99 <= cond(Ar, Inf) \ opnorm(Ar, Inf) * opnorm(inv(Array(Ar)), Inf) < 3
        @test 0.99 <= cond(Ac, Inf) \ opnorm(Ac, Inf) * opnorm(inv(Array(Ac)), Inf) < 3
    end
    @test_throws ArgumentError cond(A,2)
    @test_throws ArgumentError cond(A,3)
    Arect = spzeros(10, 6)
    @test_throws DimensionMismatch cond(Arect, 1)
    @test_throws ArgumentError cond(Arect,2)
    @test_throws DimensionMismatch cond(Arect, Inf)
end

@testset "sparse matrix opnormestinv" begin
    Random.seed!(1234)
    Ac = sprandn(20,20,.5) + im* sprandn(20,20,.5)
    Aci = ceil.(Int64, 100*sprand(20,20,.5)) + im*ceil.(Int64, sprand(20,20,.5))
    Ar = sprandn(20,20,.5)
    Ari = ceil.(Int64, 100*Ar)
    if Base.USE_GPL_LIBS
        # NOTE: opnormestinv is probabilistic, so requires a fixed seed (set above in Random.seed!(1234))
        @test SparseArrays.opnormestinv(Ac,3) ≈ opnorm(inv(Array(Ac)),1) atol=1e-4
        @test SparseArrays.opnormestinv(Aci,3) ≈ opnorm(inv(Array(Aci)),1) atol=1e-4
        @test SparseArrays.opnormestinv(Ar) ≈ opnorm(inv(Array(Ar)),1) atol=1e-4
        @test_throws ArgumentError SparseArrays.opnormestinv(Ac,0)
        @test_throws ArgumentError SparseArrays.opnormestinv(Ac,21)
    end
    @test_throws DimensionMismatch SparseArrays.opnormestinv(sprand(3,5,.9))
end

@testset "issue #13008" begin
    @test_throws ArgumentError sparse(Vector(1:100), Vector(1:100), fill(5,100), 5, 5)
    @test_throws ArgumentError sparse(Int[], Vector(1:5), Vector(1:5))
end

@testset "issue #13024" begin
    A13024 = sparse([1,2,3,4,5], [1,2,3,4,5], fill(true,5))
    B13024 = sparse([1,2,4,5],   [1,2,3,5],   fill(true,4))

    @test broadcast(&, A13024, B13024) == sparse([1,2,5], [1,2,5], fill(true,3))
    @test typeof(broadcast(&, A13024, B13024)) == SparseMatrixCSC{Bool,Int}

    @test broadcast(|, A13024, B13024) == sparse([1,2,3,4,4,5], [1,2,3,3,4,5], fill(true,6))
    @test typeof(broadcast(|, A13024, B13024)) == SparseMatrixCSC{Bool,Int}

    @test broadcast(⊻, A13024, B13024) == sparse([3,4,4], [3,3,4], fill(true,3), 5, 5)
    @test typeof(broadcast(⊻, A13024, B13024)) == SparseMatrixCSC{Bool,Int}

    @test broadcast(max, A13024, B13024) == sparse([1,2,3,4,4,5], [1,2,3,3,4,5], fill(true,6))
    @test typeof(broadcast(max, A13024, B13024)) == SparseMatrixCSC{Bool,Int}

    @test broadcast(min, A13024, B13024) == sparse([1,2,5], [1,2,5], fill(true,3))
    @test typeof(broadcast(min, A13024, B13024)) == SparseMatrixCSC{Bool,Int}

    for op in (+, -)
        @test op(A13024, B13024) == op(Array(A13024), Array(B13024))
    end
    for op in (max, min, &, |, xor)
        @test op.(A13024, B13024) == op.(Array(A13024), Array(B13024))
    end
end

@testset "fillstored!" begin
    @test LinearAlgebra.fillstored!(sparse(2.0I, 5, 5), 1) == Matrix(I, 5, 5)
end

@testset "factorization" begin
    Random.seed!(123)
    local A
    A = sparse(Diagonal(rand(5))) + sprandn(5, 5, 0.2) + im*sprandn(5, 5, 0.2)
    A = A + copy(A')
    @test !Base.USE_GPL_LIBS || abs(det(factorize(Hermitian(A)))) ≈ abs(det(factorize(Array(A))))
    A = sparse(Diagonal(rand(5))) + sprandn(5, 5, 0.2) + im*sprandn(5, 5, 0.2)
    A = A*A'
    @test !Base.USE_GPL_LIBS || abs(det(factorize(Hermitian(A)))) ≈ abs(det(factorize(Array(A))))
    A = sparse(Diagonal(rand(5))) + sprandn(5, 5, 0.2)
    A = A + copy(transpose(A))
    @test !Base.USE_GPL_LIBS || abs(det(factorize(Symmetric(A)))) ≈ abs(det(factorize(Array(A))))
    A = sparse(Diagonal(rand(5))) + sprandn(5, 5, 0.2)
    A = A*transpose(A)
    @test !Base.USE_GPL_LIBS || abs(det(factorize(Symmetric(A)))) ≈ abs(det(factorize(Array(A))))
    @test factorize(triu(A)) == triu(A)
    @test isa(factorize(triu(A)), UpperTriangular{Float64, SparseMatrixCSC{Float64, Int}})
    @test factorize(tril(A)) == tril(A)
    @test isa(factorize(tril(A)), LowerTriangular{Float64, SparseMatrixCSC{Float64, Int}})
    C, b = A[:, 1:4], fill(1., size(A, 1))
    @test !Base.USE_GPL_LIBS || factorize(C)\b ≈ Array(C)\b
    @test_throws ErrorException eigen(A)
    @test_throws ErrorException inv(A)
end

@testset "issue #13792, use sparse triangular solvers for sparse triangular solves" begin
    local A, n, x
    n = 100
    A, b = sprandn(n, n, 0.5) + sqrt(n)*I, fill(1., n)
    @test LowerTriangular(A)\(LowerTriangular(A)*b) ≈ b
    @test UpperTriangular(A)\(UpperTriangular(A)*b) ≈ b
    A[2,2] = 0
    dropzeros!(A)
    @test_throws LinearAlgebra.SingularException LowerTriangular(A)\b
    @test_throws LinearAlgebra.SingularException UpperTriangular(A)\b
end

@testset "issue described in https://groups.google.com/forum/#!topic/julia-dev/QT7qpIpgOaA" begin
    @test sparse([1,1], [1,1], [true, true]) == sparse([1,1], [1,1], [true, true], 1, 1) == fill(true, 1, 1)
    @test sparsevec([1,1], [true, true]) == sparsevec([1,1], [true, true], 1) == fill(true, 1)
end

@testset "issparse for specialized matrix types" begin
    m = sprand(10, 10, 0.1)
    @test issparse(Symmetric(m))
    @test issparse(Hermitian(m))
    @test issparse(LowerTriangular(m))
    @test issparse(LinearAlgebra.UnitLowerTriangular(m))
    @test issparse(UpperTriangular(m))
    @test issparse(LinearAlgebra.UnitUpperTriangular(m))
    @test issparse(adjoint(m))
    @test issparse(transpose(m))
    @test issparse(Symmetric(Array(m))) == false
    @test issparse(Hermitian(Array(m))) == false
    @test issparse(LowerTriangular(Array(m))) == false
    @test issparse(LinearAlgebra.UnitLowerTriangular(Array(m))) == false
    @test issparse(UpperTriangular(Array(m))) == false
    @test issparse(LinearAlgebra.UnitUpperTriangular(Array(m))) == false
end

@testset "issparse for sparse vectors #34253" begin
    v = sprand(10, 0.5)
    @test issparse(v)
    @test issparse(v')
    @test issparse(transpose(v))
end

@testset "test created type of sprand{T}(::Type{T}, m::Integer, n::Integer, density::AbstractFloat)" begin
    m = sprand(Float32, 10, 10, 0.1)
    @test eltype(m) == Float32
    m = sprand(Float64, 10, 10, 0.1)
    @test eltype(m) == Float64
    m = sprand(Int32, 10, 10, 0.1)
    @test eltype(m) == Int32
end

@testset "issue #16073" begin
    @inferred sprand(1, 1, 1.0)
    @inferred sprand(1, 1, 1.0, rand, Float64)
    @inferred sprand(1, 1, 1.0, x -> round.(Int, rand(x) * 100))
end

# Test that concatenations of combinations of sparse matrices with sparse matrices or dense
# matrices/vectors yield sparse arrays
@testset "sparse and dense concatenations" begin
    N = 4
    densevec = fill(1., N)
    densemat = diagm(0 => densevec)
    spmat = spdiagm(0 => densevec)
    # Test that concatenations of pairs of sparse matrices yield sparse arrays
    @test issparse(vcat(spmat, spmat))
    @test issparse(hcat(spmat, spmat))
    @test issparse(hvcat((2,), spmat, spmat))
    @test issparse(cat(spmat, spmat; dims=(1,2)))
    # Test that concatenations of a sparse matrice with a dense matrix/vector yield sparse arrays
    @test issparse(vcat(spmat, densemat))
    @test issparse(vcat(densemat, spmat))
    for densearg in (densevec, densemat)
        @test issparse(hcat(spmat, densearg))
        @test issparse(hcat(densearg, spmat))
        @test issparse(hvcat((2,), spmat, densearg))
        @test issparse(hvcat((2,), densearg, spmat))
        @test issparse(cat(spmat, densearg; dims=(1,2)))
        @test issparse(cat(densearg, spmat; dims=(1,2)))
    end
end

@testset "issue #14816" begin
    m = 5
    intmat = fill(1, m, m)
    ltintmat = LowerTriangular(rand(1:5, m, m))
    @test \(transpose(ltintmat), sparse(intmat)) ≈ \(transpose(ltintmat), intmat)
end

# Test temporary fix for issue #16548 in PR #16979. Somewhat brittle. Expect to remove with `\` revisions.
@testset "issue #16548" begin
    ms = methods(\, (SparseMatrixCSC, AbstractVecOrMat)).ms
    @test all(m -> m.module == SparseArrays, ms)
end

@testset "row indexing a SparseMatrixCSC with non-Int integer type" begin
    local A = sparse(UInt32[1,2,3], UInt32[1,2,3], [1.0,2.0,3.0])
    @test A[1,1:3] == A[1,:] == [1,0,0]
end

# Check that `broadcast` methods specialized for unary operations over `SparseMatrixCSC`s
# are called. (Issue #18705.) EDIT: #19239 unified broadcast over a single sparse matrix,
# eliminating the former operation classes.
@testset "issue #18705" begin
    S = sparse(Diagonal(1.0:5.0))
    @test isa(sin.(S), SparseMatrixCSC)
end

@testset "issue #19225" begin
    X = sparse([1 -1; -1 1])
    for T in (Symmetric, Hermitian)
        Y = T(copy(X))
        _Y = similar(Y)
        copyto!(_Y, Y)
        @test _Y == Y

        W = T(copy(X), :L)
        copyto!(W, Y)
        @test W.data == Y.data
        @test W.uplo != Y.uplo

        W[1,1] = 4
        @test W == T(sparse([4 -1; -1 1]))
        @test_throws ArgumentError (W[1,2] = 2)

        @test Y + I == T(sparse([2 -1; -1 2]))
        @test Y - I == T(sparse([0 -1; -1 0]))
        @test Y * I == Y

        @test Y .+ 1 == T(sparse([2 0; 0 2]))
        @test Y .- 1 == T(sparse([0 -2; -2 0]))
        @test Y * 2 == T(sparse([2 -2; -2 2]))
        @test Y / 1 == Y
    end
end

@testset "issue #19304" begin
    @inferred hcat(sparse(rand(2,1)), I)
    @inferred hcat(sparse(rand(2,1)), 1.0I)
    @inferred hcat(sparse(rand(2,1)), Matrix(I, 2, 2))
    @inferred hcat(sparse(rand(2,1)), Matrix(1.0I, 2, 2))
end

# Check that `broadcast` methods specialized for unary operations over
# `SparseMatrixCSC`s determine a reasonable return type.
@testset "issue #18974" begin
    S = sparse(Diagonal(Int64(1):Int64(4)))
    @test eltype(sin.(S)) == Float64
end

# Check calling of unary minus method specialized for SparseMatrixCSCs
@testset "issue #19503" begin
    @test which(-, (SparseMatrixCSC,)).module == SparseArrays
end

@testset "issue #14398" begin
    @test collect(view(sparse(I, 10, 10), 1:5, 1:5)') ≈ Matrix(I, 5, 5)
end

@testset "dropstored issue #20513" begin
    x = sparse(rand(3,3))
    SparseArrays.dropstored!(x, 1, 1)
    @test x[1, 1] == 0.0
    @test getcolptr(x) == [1, 3, 6, 9]
    SparseArrays.dropstored!(x, 2, 1)
    @test getcolptr(x) == [1, 2, 5, 8]
    @test x[2, 1] == 0.0
    SparseArrays.dropstored!(x, 2, 2)
    @test getcolptr(x) == [1, 2, 4, 7]
    @test x[2, 2] == 0.0
    SparseArrays.dropstored!(x, 2, 3)
    @test getcolptr(x) == [1, 2, 4, 6]
    @test x[2, 3] == 0.0
end

@testset "setindex issue #20657" begin
    local A = spzeros(3, 3)
    I = [1, 1, 1]; J = [1, 1, 1]
    A[I, 1] .= 1
    @test nnz(A) == 1
    A[1, J] .= 1
    @test nnz(A) == 1
    A[I, J] .= 1
    @test nnz(A) == 1
end

@testset "setindex with vector eltype (#29034)" begin
    A = sparse([1], [1], [Vector{Float64}(undef, 3)], 3, 3)
    A[1,1] = [1.0, 2.0, 3.0]
    @test A[1,1] == [1.0, 2.0, 3.0]
end

@testset "show" begin
    io = IOBuffer()
    show(io, MIME"text/plain"(), sparse(Int64[1], Int64[1], [1.0]))
    @test String(take!(io)) == "1×1 SparseArrays.SparseMatrixCSC{Float64,Int64} with 1 stored entry:\n  [1, 1]  =  1.0"
    show(io, MIME"text/plain"(), spzeros(Float32, Int64, 2, 2))
    @test String(take!(io)) == "2×2 SparseArrays.SparseMatrixCSC{Float32,Int64} with 0 stored entries"

    ioc = IOContext(io, :displaysize => (5, 80), :limit => true)
    show(ioc, MIME"text/plain"(), sparse(Int64[1], Int64[1], [1.0]))
    @test String(take!(io)) == "1×1 SparseArrays.SparseMatrixCSC{Float64,Int64} with 1 stored entry:\n  [1, 1]  =  1.0"
    show(ioc, MIME"text/plain"(), sparse(Int64[1, 1], Int64[1, 2], [1.0, 2.0]))
    @test String(take!(io)) == "1×2 SparseArrays.SparseMatrixCSC{Float64,Int64} with 2 stored entries:\n  ⋮"

    # even number of rows
    ioc = IOContext(io, :displaysize => (8, 80), :limit => true)
    show(ioc, MIME"text/plain"(), sparse(Int64[1,2,3,4], Int64[1,1,2,2], [1.0,2.0,3.0,4.0]))
    @test String(take!(io)) == string("4×2 SparseArrays.SparseMatrixCSC{Float64,Int64} with 4 stored entries:\n  [1, 1]",
                                      "  =  1.0\n  [2, 1]  =  2.0\n  [3, 2]  =  3.0\n  [4, 2]  =  4.0")

    show(ioc, MIME"text/plain"(), sparse(Int64[1,2,3,4,5], Int64[1,1,2,2,3], [1.0,2.0,3.0,4.0,5.0]))
    @test String(take!(io)) ==  string("5×3 SparseArrays.SparseMatrixCSC{Float64,Int64} with 5 stored entries:\n  [1, 1]",
                                       "  =  1.0\n  ⋮\n  [4, 2]  =  4.0\n  [5, 3]  =  5.0")

    show(ioc, MIME"text/plain"(), sparse(fill(1.,5,3)))
    @test String(take!(io)) ==  string("5×3 SparseArrays.SparseMatrixCSC{Float64,$Int} with 15 stored entries:\n  [1, 1]",
                                       "  =  1.0\n  ⋮\n  [4, 3]  =  1.0\n  [5, 3]  =  1.0")

    # odd number of rows
    ioc = IOContext(io, :displaysize => (9, 80), :limit => true)
    show(ioc, MIME"text/plain"(), sparse(Int64[1,2,3,4,5], Int64[1,1,2,2,3], [1.0,2.0,3.0,4.0,5.0]))
    @test String(take!(io)) == string("5×3 SparseArrays.SparseMatrixCSC{Float64,Int64} with 5 stored entries:\n  [1, 1]",
                                      "  =  1.0\n  [2, 1]  =  2.0\n  [3, 2]  =  3.0\n  [4, 2]  =  4.0\n  [5, 3]  =  5.0")

    show(ioc, MIME"text/plain"(), sparse(Int64[1,2,3,4,5,6], Int64[1,1,2,2,3,3], [1.0,2.0,3.0,4.0,5.0,6.0]))
    @test String(take!(io)) ==  string("6×3 SparseArrays.SparseMatrixCSC{Float64,Int64} with 6 stored entries:\n  [1, 1]",
                                       "  =  1.0\n  [2, 1]  =  2.0\n  ⋮\n  [5, 3]  =  5.0\n  [6, 3]  =  6.0")

    show(ioc, MIME"text/plain"(), sparse(fill(1.,6,3)))
    @test String(take!(io)) ==  string("6×3 SparseArrays.SparseMatrixCSC{Float64,$Int} with 18 stored entries:\n  [1, 1]",
                                       "  =  1.0\n  [2, 1]  =  1.0\n  ⋮\n  [5, 3]  =  1.0\n  [6, 3]  =  1.0")

    ioc = IOContext(io, :displaysize => (9, 80))
    show(ioc, MIME"text/plain"(), sparse(Int64[1,2,3,4,5,6], Int64[1,1,2,2,3,3], [1.0,2.0,3.0,4.0,5.0,6.0]))
    @test String(take!(io)) ==  string("6×3 SparseArrays.SparseMatrixCSC{Float64,Int64} with 6 stored entries:\n  [1, 1]  =  1.0\n",
        "  [2, 1]  =  2.0\n  [3, 2]  =  3.0\n  [4, 2]  =  4.0\n  [5, 3]  =  5.0\n  [6, 3]  =  6.0")

    # issue #30589
    @test repr("text/plain", sparse([true true])) == "1×2 SparseArrays.SparseMatrixCSC{Bool,$Int} with 2 stored entries:\n  [1, 1]  =  1\n  [1, 2]  =  1"
end

@testset "check buffers" for n in 1:3
    local A
    rowval = [1,2,3]
    nzval1  = Int[]
    nzval2  = [1,1,1]
    A = SparseMatrixCSC(n, n, [1:n+1;], rowval, nzval1)
    @test nnz(A) == n
    @test_throws BoundsError A[n,n]
    A = SparseMatrixCSC(n, n, [1:n+1;], rowval, nzval2)
    @test nnz(A) == n
    @test A      == Matrix(I, n, n)
end

@testset "reverse search direction if step < 0 #21986" begin
    local A, B
    A = guardseed(1234) do
        sprand(5, 5, 1/5)
    end
    A = max.(A, copy(A'))
    LinearAlgebra.fillstored!(A, 1)
    B = A[5:-1:1, 5:-1:1]
    @test issymmetric(B)
end

@testset "similar should not alias the input sparse array" begin
    a = sparse(rand(3,3) .+ 0.1)
    b = similar(a, Float32, Int32)
    c = similar(b, Float32, Int32)
    SparseArrays.dropstored!(b, 1, 1)
    @test length(rowvals(c)) == 9
    @test length(nonzeros(c)) == 9
end

@testset "similar with type conversion" begin
    local A = sparse(1.0I, 5, 5)
    @test size(similar(A, ComplexF64, Int)) == (5, 5)
    @test typeof(similar(A, ComplexF64, Int)) == SparseMatrixCSC{ComplexF64, Int}
    @test size(similar(A, ComplexF64, Int8)) == (5, 5)
    @test typeof(similar(A, ComplexF64, Int8)) == SparseMatrixCSC{ComplexF64, Int8}
    @test similar(A, ComplexF64,(6, 6)) == spzeros(ComplexF64, 6, 6)
    @test convert(Matrix, A) == Array(A) # lolwut, are you lost, test?
end

@testset "similar for SparseMatrixCSC" begin
    local A = sparse(1.0I, 5, 5)
    # test similar without specifications (preserves stored-entry structure)
    simA = similar(A)
    @test typeof(simA) == typeof(A)
    @test size(simA) == size(A)
    @test getcolptr(simA) == getcolptr(A)
    @test rowvals(simA) == rowvals(A)
    @test length(nonzeros(simA)) == length(nonzeros(A))
    # test similar with entry type specification (preserves stored-entry structure)
    simA = similar(A, Float32)
    @test typeof(simA) == SparseMatrixCSC{Float32,eltype(getcolptr(A))}
    @test size(simA) == size(A)
    @test getcolptr(simA) == getcolptr(A)
    @test rowvals(simA) == rowvals(A)
    @test length(nonzeros(simA)) == length(nonzeros(A))
    # test similar with entry and index type specification (preserves stored-entry structure)
    simA = similar(A, Float32, Int8)
    @test typeof(simA) == SparseMatrixCSC{Float32,Int8}
    @test size(simA) == size(A)
    @test getcolptr(simA) == getcolptr(A)
    @test rowvals(simA) == rowvals(A)
    @test length(nonzeros(simA)) == length(nonzeros(A))
    # test similar with Dims{2} specification (preserves storage space only, not stored-entry structure)
    simA = similar(A, (6,6))
    @test typeof(simA) == typeof(A)
    @test size(simA) == (6,6)
    @test getcolptr(simA) == fill(1, 6+1)
    @test length(rowvals(simA)) == length(rowvals(A))
    @test length(nonzeros(simA)) == length(nonzeros(A))
    # test similar with entry type and Dims{2} specification (preserves storage space only)
    simA = similar(A, Float32, (6,6))
    @test typeof(simA) == SparseMatrixCSC{Float32,eltype(getcolptr(A))}
    @test size(simA) == (6,6)
    @test getcolptr(simA) == fill(1, 6+1)
    @test length(rowvals(simA)) == length(rowvals(A))
    @test length(nonzeros(simA)) == length(nonzeros(A))
    # test similar with entry type, index type, and Dims{2} specification (preserves storage space only)
    simA = similar(A, Float32, Int8, (6,6))
    @test typeof(simA) == SparseMatrixCSC{Float32, Int8}
    @test size(simA) == (6,6)
    @test getcolptr(simA) == fill(1, 6+1)
    @test length(rowvals(simA)) == length(rowvals(A))
    @test length(nonzeros(simA)) == length(nonzeros(A))
    # test similar with Dims{1} specification (preserves nothing)
    simA = similar(A, (6,))
    @test typeof(simA) == SparseVector{eltype(nonzeros(A)),eltype(getcolptr(A))}
    @test size(simA) == (6,)
    @test length(nonzeroinds(simA)) == 0
    @test length(nonzeros(simA)) == 0
    # test similar with entry type and Dims{1} specification (preserves nothing)
    simA = similar(A, Float32, (6,))
    @test typeof(simA) == SparseVector{Float32,eltype(getcolptr(A))}
    @test size(simA) == (6,)
    @test length(nonzeroinds(simA)) == 0
    @test length(nonzeros(simA)) == 0
    # test similar with entry type, index type, and Dims{1} specification (preserves nothing)
    simA = similar(A, Float32, Int8, (6,))
    @test typeof(simA) == SparseVector{Float32,Int8}
    @test size(simA) == (6,)
    @test length(nonzeroinds(simA)) == 0
    @test length(nonzeros(simA)) == 0
    # test entry points to similar with entry type, index type, and non-Dims shape specification
    @test similar(A, Float32, Int8, 6, 6) == similar(A, Float32, Int8, (6, 6))
    @test similar(A, Float32, Int8, 6) == similar(A, Float32, Int8, (6,))
end

@testset "count specializations" begin
    # count should throw for sparse arrays for which zero(eltype) does not exist
    @test_throws MethodError count(SparseMatrixCSC(2, 2, Int[1, 2, 3], Int[1, 2], Any[true, true]))
    @test_throws MethodError count(SparseVector(2, Int[1], Any[true]))
    # count should run only over nonzeros(S)[1:nnz(S)], not nonzeros(S) in full
    @test count(SparseMatrixCSC(2, 2, Int[1, 2, 3], Int[1, 2], Bool[true, true, true])) == 2
end

@testset "sparse findprev/findnext operations" begin

    x = [0,0,0,0,1,0,1,0,1,1,0]
    x_sp = sparse(x)

    for i=1:length(x)
        @test findnext(!iszero, x,i) == findnext(!iszero, x_sp,i)
        @test findprev(!iszero, x,i) == findprev(!iszero, x_sp,i)
    end

    y = [7 0 0 0 0;
         1 0 1 0 0;
         1 7 0 7 1;
         0 0 1 0 0;
         1 0 1 1 0.0]
    y_sp = [x == 7 ? -0.0 : x for x in sparse(y)]
    y = Array(y_sp)
    @test isequal(y_sp[1,1], -0.0)

    for i in keys(y)
        @test findnext(!iszero, y,i) == findnext(!iszero, y_sp,i)
        @test findprev(!iszero, y,i) == findprev(!iszero, y_sp,i)
        @test findnext(iszero, y,i) == findnext(iszero, y_sp,i)
        @test findprev(iszero, y,i) == findprev(iszero, y_sp,i)
    end

    z_sp = sparsevec(Dict(1=>1, 5=>1, 8=>0, 10=>1))
    z = collect(z_sp)

    for i in keys(z)
        @test findnext(!iszero, z,i) == findnext(!iszero, z_sp,i)
        @test findprev(!iszero, z,i) == findprev(!iszero, z_sp,i)
    end
end

# #20711
@testset "vec returns a view" begin
    local A = sparse(Matrix(1.0I, 3, 3))
    local v = vec(A)
    v[1] = 2
    @test A[1,1] == 2
end

# #25943
@testset "operations on Integer subtypes" begin
    s = sparse(UInt8[1, 2, 3], UInt8[1, 2, 3], UInt8[1, 2, 3])
    @test sum(s, dims=2) == reshape([1, 2, 3], 3, 1)
end

@testset "mapreduce of sparse matrices with trailing elements in nzval #26534" begin
    B = SparseMatrixCSC{Int,Int}(2, 3,
        [1, 3, 4, 5],
        [1, 2, 1, 2, 999, 999, 999, 999],
        [1, 2, 3, 6, 999, 999, 999, 999]
    )
    @test maximum(B) == 6
end

_length_or_count_or_five(::Colon) = 5
_length_or_count_or_five(x::AbstractVector{Bool}) = count(x)
_length_or_count_or_five(x) = length(x)
@testset "nonscalar setindex!" begin
    for I in (1:4, :, 5:-1:2, [], trues(5), setindex!(falses(5), true, 2), 3),
        J in (2:4, :, 4:-1:1, [], setindex!(trues(5), false, 3), falses(5), 4)
        V = sparse(1 .+ zeros(_length_or_count_or_five(I)*_length_or_count_or_five(J)))
        M = sparse(1 .+ zeros(_length_or_count_or_five(I), _length_or_count_or_five(J)))
        if I isa Integer && J isa Integer
            @test_throws MethodError spzeros(5,5)[I, J] = V
            @test_throws MethodError spzeros(5,5)[I, J] = M
            continue
        end
        @test setindex!(spzeros(5, 5), V, I, J) == setindex!(zeros(5,5), V, I, J)
        @test setindex!(spzeros(5, 5), M, I, J) == setindex!(zeros(5,5), M, I, J)
        @test setindex!(spzeros(5, 5), Array(M), I, J) == setindex!(zeros(5,5), M, I, J)
        @test setindex!(spzeros(5, 5), Array(V), I, J) == setindex!(zeros(5,5), V, I, J)
    end
    @test setindex!(spzeros(5, 5), 1:25, :) == setindex!(zeros(5,5), 1:25, :) == reshape(1:25, 5, 5)
    @test setindex!(spzeros(5, 5), (25:-1:1).+spzeros(25), :) == setindex!(zeros(5,5), (25:-1:1).+spzeros(25), :) == reshape(25:-1:1, 5, 5)
    for X in (1:20, sparse(1:20), reshape(sparse(1:20), 20, 1), (1:20) .+ spzeros(20, 1), collect(1:20), collect(reshape(1:20, 20, 1)))
        @test setindex!(spzeros(5, 5), X, 6:25) == setindex!(zeros(5,5), 1:20, 6:25)
        @test setindex!(spzeros(5, 5), X, 21:-1:2) == setindex!(zeros(5,5), 1:20, 21:-1:2)
        b = trues(25)
        b[[6, 8, 13, 15, 23]] .= false
        @test setindex!(spzeros(5, 5), X, b) == setindex!(zeros(5, 5), X, b)
    end
end

@testset "sparse transpose adjoint" begin
    A = sprand(10, 10, 0.75)
    @test A' == SparseMatrixCSC(A')
    @test SparseMatrixCSC(A') isa SparseMatrixCSC
    @test transpose(A) == SparseMatrixCSC(transpose(A))
    @test SparseMatrixCSC(transpose(A)) isa SparseMatrixCSC
    @test SparseMatrixCSC{eltype(A)}(transpose(A)) == transpose(A)
    @test SparseMatrixCSC{eltype(A), Int}(transpose(A)) == transpose(A)
    @test SparseMatrixCSC{Float16}(transpose(A)) == transpose(SparseMatrixCSC{Float16}(A))
    @test SparseMatrixCSC{Float16, Int}(transpose(A)) == transpose(SparseMatrixCSC{Float16}(A))
    B = sprand(ComplexF64, 10, 10, 0.75)
    @test SparseMatrixCSC{eltype(B)}(adjoint(B)) == adjoint(B)
    @test SparseMatrixCSC{eltype(B), Int}(adjoint(B)) == adjoint(B)
    @test SparseMatrixCSC{ComplexF16}(adjoint(B)) == adjoint(SparseMatrixCSC{ComplexF16}(B))
    @test SparseMatrixCSC{ComplexF16, Int8}(adjoint(B)) == adjoint(SparseMatrixCSC{ComplexF16, Int8}(B))
end

# PR 28242
@testset "forward and backward solving of transpose/adjoint triangular matrices" begin
    rng = MersenneTwister(20180730)
    n = 10
    A = sprandn(rng, n, n, 0.8); A += Diagonal((1:n) - diag(A))
    B = ones(n, 2)
    for (Ttri, triul ) in ((UpperTriangular, triu), (LowerTriangular, tril))
        for trop in (adjoint, transpose)
            AT = Ttri(A)           # ...Triangular wrapped
            AC = triul(A)          # copied part of A
            ATa = trop(AT)         # wrapped Adjoint
            ACa = sparse(trop(AC)) # copied and adjoint
            @test AT \ B ≈ AC \ B
            @test ATa \ B ≈ ACa \ B
            @test ATa \ sparse(B) == ATa \ B
            @test Matrix(ATa) \ B ≈ ATa \ B
            @test ATa * ( ATa \ B ) ≈ B
        end
    end
end

@testset "Issue #28369" begin
    M = reshape([[1 2; 3 4], [9 10; 11 12], [5 6; 7 8], [13 14; 15 16]], (2,2))
    MP = reshape([[1 2; 3 4], [5 6; 7 8], [9 10; 11 12], [13 14; 15 16]], (2,2))
    S = sparse(M)
    SP = sparse(MP)
    @test isa(transpose(S), Transpose)
    @test transpose(S) == copy(transpose(S))
    @test Array(transpose(S)) == copy(transpose(M))
    @test permutedims(S) == SP
    @test permutedims(S, (2,1)) == SP
    @test permutedims(S, (1,2)) == S
    @test permutedims(S, (1,2)) !== S
    @test_throws ArgumentError permutedims(S, (1,3))
    MC = reshape([[(1+im) 2; 3 4], [9 10; 11 12], [(5 + 2im) 6; 7 8], [13 14; 15 16]], (2,2))
    SC = sparse(MC)
    @test isa(adjoint(SC), Adjoint)
    @test adjoint(SC) == copy(adjoint(SC))
    @test adjoint(MC) == copy(adjoint(SC))
end

begin
    rng = Random.MersenneTwister(0)
    n = 1000
    B = ones(n)
    A = sprand(rng, n, n, 0.01)
    MA = Matrix(A)
    lA = sprand(rng, n, n+10, 0.01)
    @testset "triangular multiply with $tr($wr)" for tr in (identity, adjoint, transpose),
    wr in (UpperTriangular, LowerTriangular, UnitUpperTriangular, UnitLowerTriangular)
        AW = tr(wr(A))
        MAW = tr(wr(MA))
        @test AW * B ≈ MAW * B
        # and for SparseMatrixCSCView - a view of all rows and unit range of cols
        vAW = tr(wr(view(A, :, 1:n)))
        vMAW = tr(wr(view(MA, :, 1:n)))
        @test vAW * B ≈ vMAW * B
    end
    A = A - Diagonal(diag(A)) + 2I # avoid rounding errors by division
    MA = Matrix(A)
    @testset "triangular solver for $tr($wr)" for tr in (identity, adjoint, transpose),
    wr in (UpperTriangular, LowerTriangular, UnitUpperTriangular, UnitLowerTriangular)
        AW = tr(wr(A))
        MAW = tr(wr(MA))
        @test AW \ B ≈ MAW \ B
    end
    @testset "triangular singular exceptions" begin
        A = LowerTriangular(sparse([0 2.0;0 1]))
        @test_throws SingularException(1) A \ ones(2)
        A = UpperTriangular(sparse([1.0 0;0 0]))
        @test_throws SingularException(2) A \ ones(2)
    end
end

@testset "Issue #28634" begin
    a = SparseMatrixCSC{Int8, Int16}([1 2; 3 4])
    na = SparseMatrixCSC(a)
    @test typeof(a) === typeof(na)
end

#PR #29045
@testset "Issue #28934" begin
    A = sprand(5,5,0.5)
    D = Diagonal(rand(5))
    C = copy(A)
    m1 = @which mul!(C,A,D)
    m2 = @which mul!(C,D,A)
    @test m1.module == SparseArrays
    @test m2.module == SparseArrays
end

@testset "Symmetric of sparse matrix mul! dense vector" begin
    rng = Random.MersenneTwister(1)
    n = 1000
    p = 0.02
    q = 1 - sqrt(1-p)
    Areal = sprandn(rng, n, n, p)
    Breal = randn(rng, n)
    Acomplex = sprandn(rng, n, n, q) + sprandn(rng, n, n, q) * im
    Bcomplex = Breal + randn(rng, n) * im
    @testset "symmetric/Hermitian sparse multiply with $S($U)" for S in (Symmetric, Hermitian), U in (:U, :L), (A, B) in ((Areal,Breal), (Acomplex,Bcomplex))
        Asym = S(A, U)
        As = sparse(Asym) # takes most time
        @test which(mul!, (typeof(B), typeof(Asym), typeof(B))).module == SparseArrays
        @test norm(Asym * B - As * B, Inf) <= eps() * n * p * 10
    end
end

@testset "Symmetric of view of sparse matrix mul! dense vector" begin
    rng = Random.MersenneTwister(1)
    n = 1000
    p = 0.02
    q = 1 - sqrt(1-p)
    Areal = view(sprandn(rng, n, n+10, p), :, 6:n+5)
    Breal = randn(rng, n)
    Acomplex = view(sprandn(rng, n, n+10, q) + sprandn(rng, n, n+10, q) * im, :, 6:n+5)
    Bcomplex = Breal + randn(rng, n) * im
    @testset "symmetric/Hermitian sparseview multiply with $S($U)" for S in (Symmetric, Hermitian), U in (:U, :L), (A, B) in ((Areal,Breal), (Acomplex,Bcomplex))
        Asym = S(A, U)
        As = sparse(Asym) # takes most time
        @test which(mul!, (typeof(B), typeof(Asym), typeof(B))).module == SparseArrays
        @test norm(Asym * B - As * B, Inf) <= eps() * n * p * 10
    end
end

@testset "sprand" begin
    p=0.3; m=1000; n=2000;
    for s in 1:10
        # build a (dense) random matrix with randsubset + rand
        Random.seed!(s);
        v = randsubseq(1:m*n,p);
        x = zeros(m,n);
        x[v] .= rand(length(v));
        # redo the same with sprand
        Random.seed!(s);
        a = sprand(m,n,p);
        @test x == a
    end
end

@testset "sprandn with type $T" for T in (Float64, Float32, Float16, ComplexF64, ComplexF32, ComplexF16)
    @test sprandn(T, 5, 5, 0.5) isa AbstractSparseMatrix{T}
end
@testset "sprandn with invalid type $T" for T in (AbstractFloat, BigFloat, Complex)
    @test_throws MethodError sprandn(T, 5, 5, 0.5)
end

@testset "method ambiguity" begin
    # Ambiguity test is run inside a clean process.
    # https://github.com/JuliaLang/julia/issues/28804
    script = joinpath(@__DIR__, "ambiguous_exec.jl")
    cmd = `$(Base.julia_cmd()) --startup-file=no $script`
    @test success(pipeline(cmd; stdout=stdout, stderr=stderr))
end

@testset "oneunit of sparse matrix" begin
    A = sparse([Second(0) Second(0); Second(0) Second(0)])
    @test oneunit(sprand(2, 2, 0.5)) isa SparseMatrixCSC{Float64}
    @test oneunit(A) isa SparseMatrixCSC{Second}
    @test one(sprand(2, 2, 0.5)) isa SparseMatrixCSC{Float64}
    @test one(A) isa SparseMatrixCSC{Int}
end

@testset "circshift" begin
    m,n = 17,15
    A = sprand(m, n, 0.5)
    for rshift in (-1, 0, 1, 10), cshift in (-1, 0, 1, 10)
        shifts = (rshift, cshift)
        # using dense circshift to compare
        B = circshift(Matrix(A), shifts)
        # sparse circshift
        C = circshift(A, shifts)
        @test C == B
        # sparse circshift should not add structural zeros
        @test nnz(C) == nnz(A)
        # test circshift!
        D = similar(A)
        circshift!(D, A, shifts)
        @test D == B
        @test nnz(D) == nnz(A)
        # test different in/out types
        A2 = floor.(100A)
        E1 = spzeros(Int64, m, n)
        E2 = spzeros(Int64, m, n)
        circshift!(E1, A2, shifts)
        circshift!(E2, Matrix(A2), shifts)
        @test E1 == E2
    end
end

@testset "wrappers of sparse" begin
    m = n = 10
    A = spzeros(ComplexF64, m, n)
    A[:,1] = 1:m
    A[:,2] = [1 3 0 0 0 0 0 0 0 0]'
    A[:,3] = [2 4 0 0 0 0 0 0 0 0]'
    A[:,4] = [0 0 0 0 5 3 0 0 0 0]'
    A[:,5] = [0 0 0 0 6 2 0 0 0 0]'
    A[:,6] = [0 0 0 0 7 4 0 0 0 0]'
    A[:,7:n] = rand(ComplexF64, m, n-6)
    B = Matrix(A)
    dowrap(wr, A) = wr(A)
    dowrap(wr::Tuple, A) = (wr[1])(A, wr[2:end]...)

    @testset "sparse($wr(A))" for wr in (
                        Symmetric, (Symmetric, :L), Hermitian, (Hermitian, :L),
                        Transpose, Adjoint,
                        UpperTriangular, LowerTriangular,
                        UnitUpperTriangular, UnitLowerTriangular,
                        (view, 3:6, 2:5))

        @test SparseMatrixCSC(dowrap(wr, A)) == Matrix(dowrap(wr, B))
    end

    @testset "sparse($at($wr))" for at = (Transpose, Adjoint), wr =
        (UpperTriangular, LowerTriangular,
         UnitUpperTriangular, UnitLowerTriangular)

        @test SparseMatrixCSC(at(wr(A))) == Matrix(at(wr(B)))
    end

    @test sparse([1,2,3,4,5]') == SparseMatrixCSC([1 2 3 4 5])
    @test sparse(UpperTriangular(A')) == UpperTriangular(B')
    @test sparse(Adjoint(UpperTriangular(A'))) == Adjoint(UpperTriangular(B'))
    @test sparse(UnitUpperTriangular(spzeros(5,5))) == I
    deepwrap(A) = (Adjoint(LowerTriangular(view(Symmetric(A), 5:7, 4:6))))
    @test sparse(deepwrap(A)) == Matrix(deepwrap(B))
end

@testset "unary operations on matrices where length(nzval)>nnz" begin
    # this should create a sparse matrix with length(nzval)>nnz
    A = SparseMatrixCSC(Complex{BigInt}[1+im 2+2im]')'[1:1, 2:2]
    # ...ensure it does! If necessary, the test needs to be updated to use
    # another mechanism to create a suitable A.
    resize!(nonzeros(A), 2)
    @assert length(nonzeros(A)) > nnz(A)
    @test -A == fill(-2-2im, 1, 1)
    @test conj(A) == fill(2-2im, 1, 1)
    conj!(A)
    @test A == fill(2-2im, 1, 1)
end

@testset "issue #31453" for T in [UInt8, Int8, UInt16, Int16, UInt32, Int32]
    i = Int[1, 2]
    j = Int[2, 1]
    i2 = T.(i)
    j2 = T.(j)
    v = [500, 600]
    x1 = sparse(i, j, v)
    x2 = sparse(i2, j2, v)
    @test sum(x1) == sum(x2) == 1100
    @test sum(x1, dims=1) == sum(x2, dims=1)
    @test sum(x1, dims=2) == sum(x2, dims=2)
end

@testset "Ti cannot store all potential values #31024" begin
    # m * n >= typemax(Ti) but nnz < typemax(Ti)
    A = SparseMatrixCSC(12, 12, fill(Int8(1),13), Int8[], Int[])
    @test size(A) == (12,12) && nnz(A) == 0
    I1 = [Int8(i) for i in 1:20 for _ in 1:20]
    J1 = [Int8(i) for _ in 1:20 for i in 1:20]
    # m * n >= typemax(Ti) and nnz >= typemax(Ti)
    @test_throws ArgumentError sparse(I1, J1, ones(length(I1)))
    I1 = Int8.(rand(1:10, 500))
    J1 = Int8.(rand(1:10, 500))
    V1 = ones(500)
    # m * n < typemax(Ti) and length(I) >= typemax(Ti) - combining values
    @test_throws ArgumentError sparse(I1, J1, V1, 10, 10)
    # m * n >= typemax(Ti) and length(I) >= typemax(Ti)
    @test_throws ArgumentError sparse(I1, J1, V1, 12, 13)
    I1 = Int8.(rand(1:10, 126))
    J1 = Int8.(rand(1:10, 126))
    V1 = ones(126)
    # m * n >= typemax(Ti) and length(I) < typemax(Ti)
    @test size(sparse(I1, J1, V1, 100, 100)) == (100,100)
end

@testset "Typecheck too strict #31435" begin
    A = SparseMatrixCSC{Int,Int8}(70, 2, fill(Int8(1), 3), Int8[], Int[])
    A[5:67,1:2] .= ones(Int, 63, 2)
    @test nnz(A) == 126
    # nnz >= typemax
    @test_throws ArgumentError A[2,1] = 42
    # colptr short
    @test_throws ArgumentError SparseMatrixCSC(1, 1, Int[], Int[], Float64[])
    # colptr[1] must be 1
    @test_throws ArgumentError SparseMatrixCSC(10, 3, [0,1,1,1], Int[], Float64[])
    # colptr not ascending
    @test_throws ArgumentError SparseMatrixCSC(10, 3, [1,2,1,2], Int[], Float64[])
    # rowwal (and nzval) short
    @test_throws ArgumentError SparseMatrixCSC(10, 3, [1,2,2,4], [1,2], Float64[])
    # nzval short
    @test SparseMatrixCSC(10, 3, [1,2,2,4], [1,2,3], Float64[]) !== nothing
    # length(rowval) >= typemax
    @test_throws ArgumentError SparseMatrixCSC(5, 1, Int8[1,2], fill(Int8(1),127), Int[1,2,3])
    @test SparseMatrixCSC{Int,Int8}(5, 1, Int8[1,2], fill(Int8(1),127), Int[1,2,3]) != 0
    # length(nzval) >= typemax
    @test_throws ArgumentError SparseMatrixCSC(5, 1, Int8[1,2], Int8[1], fill(7, 127))
    @test SparseMatrixCSC{Int,Int8}(5, 1, Int8[1,2], Int8[1], fill(7, 127)) != 0

    # length(I) >= typemax
    @test_throws ArgumentError sparse(UInt8.(1:255), fill(UInt8(1), 255), fill(1, 255))
    # m > typemax
    @test_throws ArgumentError sparse(UInt8.(1:254), fill(UInt8(1), 254), fill(1, 254), 256, 1)
    # n > typemax
    @test_throws ArgumentError sparse(UInt8.(1:254), fill(UInt8(1), 254), fill(1, 254), 255, 256)
    # n, m maximal
    @test sparse(UInt8.(1:254), fill(UInt8(1), 254), fill(1, 254), 255, 255) !== nothing
end

@testset "Sparse promotion in sparse matmul" begin
    A = SparseMatrixCSC{Float32, Int8}(2, 2, Int8[1, 2, 3], Int8[1, 2], Float32[1., 2.])
    B = SparseMatrixCSC{ComplexF32, Int32}(2, 2, Int32[1, 2, 3], Int32[1, 2], ComplexF32[1. + im, 2. - im])
    @test A*transpose(B)                  ≈ Array(A) * transpose(Array(B))
    @test A*adjoint(B)                    ≈ Array(A) * adjoint(Array(B))
    @test transpose(A)*B                  ≈ transpose(Array(A)) * Array(B)
    @test transpose(A)*transpose(B)       ≈ transpose(Array(A)) * transpose(Array(B))
    @test adjoint(B)*A                    ≈ adjoint(Array(B)) * Array(A)
    @test adjoint(B)*adjoint(complex.(A)) ≈ adjoint(Array(B)) * adjoint(Array(complex.(A)))
end

@testset "copy a ReshapedArray of SparseMatrixCSC" begin
    A = sprand(20, 10, 0.2)
    rA = reshape(A, 10, 20)
    crA = copy(rA)
    @test reshape(crA, 20, 10) == A
end

@testset "avoid aliasing of fields during constructing $T (issue #34630)" for T in
    (SparseMatrixCSC, SparseMatrixCSC{Float64}, SparseMatrixCSC{Float64,Int16})

    A = sparse([1 1; 1 0])
    B = T(A)
    @test A == B
    A[2,2] = 1
    @test A != B
    @test getcolptr(A) !== getcolptr(B)
    @test rowvals(A) !== rowvals(B)
    @test nonzeros(A) !== nonzeros(B)
end

@testset "SparseMatrixCSCView" begin
    A  = sprand(10, 10, 0.2)
    vA = view(A, :, 1:5) # a CSCView contains all rows and a UnitRange of the columns
    @test SparseArrays.getnzval(vA)  == SparseArrays.getnzval(A)
    @test SparseArrays.getrowval(vA) == SparseArrays.getrowval(A)
    @test SparseArrays.getcolptr(vA) == SparseArrays.getcolptr(A[:, 1:5])
end

@testset "mapreducecols" begin
    n = 20
    m = 10
    A = sprand(n, m, 0.2)
    B = mapreduce(identity, +, A, dims=2)
    for row in 1:n
        @test B[row] ≈ sum(A[row, :])
    end
    @test B ≈ mapreduce(identity, +, Matrix(A), dims=2)
    # case when f(0) =\= 0
    B = mapreduce(x->x+1, +, A, dims=2)
    for row in 1:n
        @test B[row] ≈ sum(A[row, :] .+ 1)
    end
    @test B ≈ mapreduce(x->x+1, +, Matrix(A), dims=2)
    # case when there are no zeros in the sparse matrix
    A = sparse(rand(n, m))
    B = mapreduce(identity, +, A, dims=2)
    for row in 1:n
        @test B[row] ≈ sum(A[row, :])
    end
    @test B ≈ mapreduce(identity, +, Matrix(A), dims=2)
end

@testset "Symmetric and Hermitian #35325" begin
    A = sprandn(ComplexF64, 10, 10, 0.1)
    B = sprandn(ComplexF64, 10, 10, 0.1)

    @test Symmetric(real(A)) + Hermitian(B) isa Hermitian
    @test Hermitian(A) + Symmetric(real(B)) isa Hermitian
    
    @testset "$Wrapper $op" for op ∈ (+, -), (Wrapper, conjugation) ∈ ((Hermitian, adjoint), (Symmetric, transpose))
        AWU = Wrapper(A, :U)
        AWL = Wrapper(A, :L)
        BWU = Wrapper(B, :U)
        BWL = Wrapper(B, :L)
        
        @test op(AWU, B) isa SparseMatrixCSC
        @test op(A, BWL) isa SparseMatrixCSC
        
        @test op(AWU, B) ≈ op(collect(AWU), B)
        @test op(AWL, B) ≈ op(collect(AWL), B) 
        @test op(A, Wrapper(B, :U)) ≈ op(A, collect(BWU))
        @test op(A, Wrapper(B, :L)) ≈ op(A, collect(BWL))

        @test op(Wrapper(A), Wrapper(B)) isa Wrapper{ComplexF64, <:SparseMatrixCSC}

        @test op(AWU, Wrapper(B, :U)) ≈ op(collect(AWU), collect(BWU))
        @test op(AWU, Wrapper(B, :L)) ≈ op(collect(AWU), collect(BWL))
        @test op(AWL, Wrapper(B, :U)) ≈ op(collect(AWL), collect(BWU))
        @test op(AWL, Wrapper(B, :L)) ≈ op(collect(AWL), collect(BWL))
    end
end

end # module

# A = sprandn(ComplexF64, 10, 10, 0.1)
# B = sprandn(ComplexF64, 10, 10, 0.1)

# @test Symmetric(real(A)) + Hermitian(B) isa Hermitian
# @test Hermitian(A) + Symmetric(real(B)) isa Hermitian

# let op = (+), Wrapper = Hermitian, conjugation = adjoint
#     symu(A) = triu(A) + conjugation(triu(A, +1))
#     syml(A) = tril(A) + conjugation(tril(A, -1))
#     @test op(Wrapper(A, :U), B) ≈ collect(op(Wrapper(A, :U), B))
#     @test op(symu(A), B) ≈ collect(op(symu(A), B))
#     op(collect(Wrapper(A, :U)), B) ≈ collect(op(symu(A), B))
#     symu(A)[8, 8]
#     #(Wrapper(A, :U))[8, 8]
# end

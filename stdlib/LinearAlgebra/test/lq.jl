# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestLQ

using Test, LinearAlgebra, Random
using LinearAlgebra: BlasComplex, BlasFloat, BlasReal, rmul!, lmul!

n = 10

# Split n into 2 parts for tests needing two matrices
n1 = div(n, 2)
n2 = 2*n1

srand(1234321)

areal = randn(n,n)/2
aimg  = randn(n,n)/2
a2real = randn(n,n)/2
a2img  = randn(n,n)/2
breal = randn(n,2)/2
bimg  = randn(n,2)/2

# helper functions to unambiguously recover explicit forms of an LQPackedQ
squareQ(Q::LinearAlgebra.LQPackedQ) = (n = size(Q.factors, 2); lmul!(Q, Matrix{eltype(Q)}(I, n, n)))
rectangularQ(Q::LinearAlgebra.LQPackedQ) = convert(Array, Q)

@testset for eltya in (Float32, Float64, ComplexF32, ComplexF64)
    a = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(areal, aimg) : areal)
    a2 = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(a2real, a2img) : a2real)
    asym = a' + a                 # symmetric indefinite
    apd  = a' * a                 # symmetric positive-definite
    ε = εa = eps(abs(float(one(eltya))))

    @testset for eltyb in (Float32, Float64, ComplexF32, ComplexF64, Int)
        b = eltyb == Int ? rand(1:5, n, 2) : convert(Matrix{eltyb}, eltyb <: Complex ? complex.(breal, bimg) : breal)
        εb = eps(abs(float(one(eltyb))))
        ε = max(εa,εb)

        α = rand(eltya)
        aα = fill(α,1,1)
        @test lqfact(α).L*lqfact(α).Q ≈ lqfact(aα).L*lqfact(aα).Q
        @test lq(α)[1]*lq(α)[2] ≈ lqfact(aα).L*lqfact(aα).Q
        @test abs(lqfact(α).Q[1,1]) ≈ one(eltya)
        tab = promote_type(eltya,eltyb)

        for i = 1:2
            let a = i == 1 ? a : view(a, 1:n - 1, 1:n - 1), b = i == 1 ? b : view(b, 1:n - 1), n = i == 1 ? n : n - 1
                lqa   = lqfact(a)
                l,q   = lqa.L, lqa.Q
                qra   = qrfact(a)
                @testset "Basic ops" begin
                    @test size(lqa,1) == size(a,1)
                    @test size(lqa,3) == 1
                    @test size(lqa.Q,3) == 1
                    @test_throws ErrorException lqa.Z
                    @test Array(copy(adjoint(lqa))) ≈ a'
                    @test q*squareQ(q)' ≈ Matrix(I, n, n)
                    @test l*q ≈ a
                    @test Array(lqa) ≈ a
                    @test Array(copy(lqa)) ≈ a
                    lstring = sprint(show,l)
                    qstring = sprint(show,q)
                    @test sprint(show,lqa) == "$(typeof(lqa)) with factors L and Q:\n$lstring\n$qstring"
                end
                @testset "Binary ops" begin
                    @test a*(lqa\b) ≈ b atol=3000ε
                    @test lqa*b ≈ qra.Q*qra.R*b atol=3000ε
                    @test (sq = size(q.factors, 2); *(Matrix{eltyb}(I, sq, sq), adjoint(q))*squareQ(q)) ≈ Matrix(I, n, n) atol=5000ε
                    if eltya != Int
                        @test Matrix{eltyb}(I, n, n)*q ≈ convert(AbstractMatrix{tab},q)
                    end
                    @test q*b ≈ squareQ(q)*b atol=100ε
                    @test transpose(q)*b ≈ transpose(squareQ(q))*b atol=100ε
                    @test q'*b ≈ squareQ(q)'*b atol=100ε
                    @test a*q ≈ a*squareQ(q) atol=100ε
                    @test a*transpose(q) ≈ a*transpose(squareQ(q)) atol=100ε
                    @test a*q' ≈ a*squareQ(q)' atol=100ε
                    @test a'*q ≈ a'*squareQ(q) atol=100ε
                    @test a'*q' ≈ a'*squareQ(q)' atol=100ε
                    @test_throws DimensionMismatch q*b[1:n1 + 1]
                    @test_throws DimensionMismatch adjoint(q) * Matrix{eltya}(undef,n+2,n+2)
                    @test_throws DimensionMismatch Matrix{eltyb}(undef,n+2,n+2)*q
                    if isa(a, DenseArray) && isa(b, DenseArray)
                        # use this to test 2nd branch in mult code
                        pad_a = vcat(I, a)
                        pad_b = hcat(I, b)
                        @test pad_a*q ≈ pad_a*squareQ(q) atol=100ε
                        @test transpose(q)*pad_b ≈ transpose(squareQ(q))*pad_b atol=100ε
                        @test q'*pad_b ≈ squareQ(q)'*pad_b atol=100ε
                    end
                end
            end
        end

        @testset "Matmul with LQ factorizations" begin
            lqa = lqfact(a[:,1:n1])
            l,q = lqa.L, lqa.Q
            @test rectangularQ(q)*rectangularQ(q)' ≈ Matrix(I, n1, n1)
            @test squareQ(q)'*squareQ(q) ≈ Matrix(I, n1, n1)
            @test_throws DimensionMismatch rmul!(Matrix{eltya}(I, n+1, n+1),q)
            @test lmul!(adjoint(q), rectangularQ(q)) ≈ Matrix(I, n1, n1)
            @test_throws DimensionMismatch rmul!(Matrix{eltya}(I, n+1, n+1), adjoint(q))
            @test_throws BoundsError size(q,-1)
        end
    end
end

@testset "correct form of Q from lq(...) (#23729)" begin
    # where the original matrix (say A) is square or has more rows than columns,
    # then A's factorization's triangular factor (say L) should have the same shape
    # as A independent of factorization form ("full", "reduced"/"thin"), and A's factorization's
    # orthogonal factor (say Q) should be a square matrix of order of A's number of
    # columns independent of factorization form ("full", "reduced"/"thin"), and L and Q
    # should have multiplication-compatible shapes.
    local m, n = 4, 2
    A = randn(m, n)
    for full in (false, true)
        L, Q = lq(A, full = full)
        @test size(L) == (m, n)
        @test size(Q) == (n, n)
        @test isapprox(A, L*Q)
    end
    # where the original matrix has strictly fewer rows than columns ...
    m, n = 2, 4
    A = randn(m, n)
    # ... then, for a rectangular/"thin" factorization of A, L should be a square matrix
    # of order of A's number of rows, Q should have the same shape as A,
    # and L and Q should have multiplication-compatible shapes
    Lrect, Qrect = lq(A, full = false)
    @test size(Lrect) == (m, m)
    @test size(Qrect) == (m, n)
    @test isapprox(A, Lrect * Qrect)
    # ... and, for a full factorization of A, L should have the
    # same shape as A, Q should be a square matrix of order of A's number of columns,
    # and L and Q should have multiplication-compatible shape. but instead the L returned
    # has no zero-padding on the right / is L for the rectangular/"thin" factorization,
    # so for L and Q to have multiplication-compatible shapes, L must be zero-padded
    # to have the shape of A.
    Lfull, Qfull = lq(A, full = true)
    @test size(Lfull) == (m, m)
    @test size(Qfull) == (n, n)
    @test isapprox(A, [Lfull zeros(m, n - m)] * Qfull)
end

@testset "getindex on LQPackedQ (#23733)" begin
    local m, n
    function getqs(F::LinearAlgebra.LQ)
        implicitQ = F.Q
        sq = size(implicitQ.factors, 2)
        explicitQ = lmul!(implicitQ, Matrix{eltype(implicitQ)}(I, sq, sq))
        return implicitQ, explicitQ
    end

    m, n = 3, 3 # reduced Q 3-by-3, full Q 3-by-3
    implicitQ, explicitQ = getqs(lqfact(randn(m, n)))
    @test implicitQ[1, 1] == explicitQ[1, 1]
    @test implicitQ[m, 1] == explicitQ[m, 1]
    @test implicitQ[1, n] == explicitQ[1, n]
    @test implicitQ[m, n] == explicitQ[m, n]

    m, n = 3, 4 # reduced Q 3-by-4, full Q 4-by-4
    implicitQ, explicitQ = getqs(lqfact(randn(m, n)))
    @test implicitQ[1, 1] == explicitQ[1, 1]
    @test implicitQ[m, 1] == explicitQ[m, 1]
    @test implicitQ[1, n] == explicitQ[1, n]
    @test implicitQ[m, n] == explicitQ[m, n]
    @test implicitQ[m+1, 1] == explicitQ[m+1, 1]
    @test implicitQ[m+1, n] == explicitQ[m+1, n]

    m, n = 4, 3 # reduced Q 3-by-3, full Q 3-by-3
    implicitQ, explicitQ = getqs(lqfact(randn(m, n)))
    @test implicitQ[1, 1] == explicitQ[1, 1]
    @test implicitQ[n, 1] == explicitQ[n, 1]
    @test implicitQ[1, n] == explicitQ[1, n]
    @test implicitQ[n, n] == explicitQ[n, n]
end

@testset "size on LQPackedQ (#23780)" begin
    # size(Q::LQPackedQ) yields the shape of Q's full/square form
    for ((mA, nA), nQ) in (
        ((3, 3), 3), # A 3-by-3 => full/square Q 3-by-3
        ((3, 4), 4), # A 3-by-4 => full/square Q 4-by-4
        ((4, 3), 3) )# A 4-by-3 => full/square Q 3-by-3
        @test size(lqfact(randn(mA, nA)).Q) == (nQ, nQ)
    end
end

@testset "postmultiplication with / right-application of LQPackedQ (#23779)" begin
    function getqs(F::LinearAlgebra.LQ)
        implicitQ = F.Q
        explicitQ = lmul!(implicitQ, Matrix{eltype(implicitQ)}(I, size(implicitQ)...))
        return implicitQ, explicitQ
    end
    # for any shape m-by-n of LQ-factored matrix, where Q is an LQPackedQ
    # A_mul_B*(C, Q) (Ac_mul_B*(C, Q)) operations should work for
    # *-by-n (n-by-*) C, which we test below via n-by-n C
    for (mA, nA) in ((3, 3), (3, 4), (4, 3))
        implicitQ, explicitQ = getqs(lqfact(randn(mA, nA)))
        C = randn(nA, nA)
        @test *(C, implicitQ) ≈ *(C, explicitQ)
        @test *(C, adjoint(implicitQ)) ≈ *(C, adjoint(explicitQ))
        @test *(adjoint(C), implicitQ) ≈ *(adjoint(C), explicitQ)
        @test *(adjoint(C), adjoint(implicitQ)) ≈ *(adjoint(C), adjoint(explicitQ))
    end
    # where the LQ-factored matrix has at least as many rows m as columns n,
    # Q's full/square and reduced/rectangular forms have the same shape (n-by-n). hence we expect
    # _only_ *-by-n (n-by-*) C to work in A_mul_B*(C, Q) (Ac_mul_B*(C, Q)) ops.
    # and hence the n-by-n C tests above suffice.
    #
    # where the LQ-factored matrix has more columns n than rows m,
    # Q's full/square form is n-by-n whereas its reduced/rectangular form is m-by-n.
    # hence we need also test *-by-m C with
    # A*_mul_B(C, Q) ops, as below via m-by-m C.
    mA, nA = 3, 4
    implicitQ, explicitQ = getqs(lqfact(randn(mA, nA)))
    C = randn(mA, mA)
    zeroextCright = hcat(C, zeros(eltype(C), mA))
    zeroextCdown = vcat(C, zeros(eltype(C), (1, mA)))
    @test *(C, implicitQ) ≈ *(zeroextCright, explicitQ)
    @test *(adjoint(C), implicitQ) ≈ *(adjoint(zeroextCdown), explicitQ)
    @test_throws DimensionMismatch C * adjoint(implicitQ)
    @test_throws DimensionMismatch adjoint(C) * adjoint(implicitQ)
end

end # module TestLQ

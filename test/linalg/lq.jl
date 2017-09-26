# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

using Base.LinAlg: BlasComplex, BlasFloat, BlasReal


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

@testset for eltya in (Float32, Float64, Complex64, Complex128)
    a = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(areal, aimg) : areal)
    a2 = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(a2real, a2img) : a2real)
    asym = a'+a                  # symmetric indefinite
    apd  = a'*a                 # symmetric positive-definite
    ε = εa = eps(abs(float(one(eltya))))

    @testset for eltyb in (Float32, Float64, Complex64, Complex128, Int)
        b = eltyb == Int ? rand(1:5, n, 2) : convert(Matrix{eltyb}, eltyb <: Complex ? complex.(breal, bimg) : breal)
        εb = eps(abs(float(one(eltyb))))
        ε = max(εa,εb)

        α = rand(eltya)
        aα = fill(α,1,1)
        @test lqfact(α)[:L]*lqfact(α)[:Q] ≈ lqfact(aα)[:L]*lqfact(aα)[:Q]
        @test lq(α)[1]*lq(α)[2] ≈ lqfact(aα)[:L]*lqfact(aα)[:Q]
        @test abs(lqfact(α)[:Q][1,1]) ≈ one(eltya)
        tab = promote_type(eltya,eltyb)

        for i = 1:2
            let a = i == 1 ? a : view(a, 1:n - 1, 1:n - 1), b = i == 1 ? b : view(b, 1:n - 1), n = i == 1 ? n : n - 1
                lqa   = lqfact(a)
                l,q   = lqa[:L], lqa[:Q]
                qra   = qrfact(a)
                @testset "Basic ops" begin
                    @test size(lqa,1) == size(a,1)
                    @test size(lqa,3) == 1
                    @test size(lqa[:Q],3) == 1
                    @test Base.LinAlg.getq(lqa) == lqa[:Q]
                    @test_throws KeyError lqa[:Z]
                    @test full(lqa') ≈ a'
                    @test lqa * lqa' ≈ a * a'
                    @test lqa' * lqa ≈ a' * a
                    @test q*full(q, thin = false)' ≈ eye(eltya,n)
                    @test l*q ≈ a
                    @test full(lqa) ≈ a
                    @test full(copy(lqa)) ≈ a
                    lstring = sprint(show,l)
                    qstring = sprint(show,q)
                    @test sprint(show,lqa) == "$(typeof(lqa)) with factors L and Q:\n$lstring\n$qstring"
                end
                @testset "Binary ops" begin
                    @test a*(lqa\b) ≈ b atol=3000ε
                    @test lqa*b ≈ qra[:Q]*qra[:R]*b atol=3000ε
                    @test A_mul_Bc(eye(eltyb,size(q.factors,2)),q)*full(q,thin=false) ≈ eye(n) atol=5000ε
                    if eltya != Int
                        @test eye(eltyb,n)*q ≈ convert(AbstractMatrix{tab},q)
                    end
                    @test q*b ≈ full(q,thin=false)*b atol=100ε
                    @test q.'*b ≈ full(q,thin=false).'*b atol=100ε
                    @test q'*b ≈ full(q,thin=false)'*b atol=100ε
                    @test a*q ≈ a*full(q,thin=false) atol=100ε
                    @test a*q.' ≈ a*full(q,thin=false).' atol=100ε
                    @test a*q' ≈ a*full(q,thin=false)' atol=100ε
                    @test a'*q ≈ a'*full(q,thin=false) atol=100ε
                    @test a'*q' ≈ a'*full(q,thin=false)' atol=100ε
                    @test_throws DimensionMismatch q*b[1:n1 + 1]
                    @test_throws DimensionMismatch Ac_mul_B(q,ones(eltya,n+2,n+2))
                    @test_throws DimensionMismatch ones(eltyb,n+2,n+2)*q
                    if isa(a, DenseArray) && isa(b, DenseArray)
                        # use this to test 2nd branch in mult code
                        pad_a = vcat(eye(a), a)
                        pad_b = hcat(eye(b), b)
                        @test pad_a*q ≈ pad_a*full(q,thin=false) atol=100ε
                        @test q.'*pad_b ≈ full(q,thin=false).'*pad_b atol=100ε
                        @test q'*pad_b ≈ full(q,thin=false)'*pad_b atol=100ε
                    end
                end
            end
        end

        @testset "Matmul with LQ factorizations" begin
            lqa = lqfact(a[:,1:n1])
            l,q = lqa[:L], lqa[:Q]
            @test full(q)*full(q)' ≈ eye(eltya,n1)
            @test full(q,thin=false)'*full(q,thin=false) ≈ eye(eltya, n1)
            @test_throws DimensionMismatch A_mul_B!(eye(eltya,n+1),q)
            @test Ac_mul_B!(q,full(q)) ≈ eye(eltya,n1)
            @test_throws DimensionMismatch A_mul_Bc!(eye(eltya,n+1),q)
            @test_throws BoundsError size(q,-1)
        end
    end
end

@testset "correct form of Q from lq(...) (#23729)" begin
    # where the original matrix (say A) is square or has more rows than columns,
    # then A's factorization's triangular factor (say L) should have the same shape
    # as A independent of form (thin, square), and A's factorization's orthogonal
    # factor (say Q) should be a square matrix of order of A's number of columns
    # independent of form (thin, square), and L and Q should have
    # multiplication-compatible shapes.
    m, n = 4, 2
    A = randn(m, n)
    for thin in (true, false)
        L, Q = lq(A, thin = thin)
        @test size(L) == (m, n)
        @test size(Q) == (n, n)
        @test isapprox(A, L*Q)
    end
    # where the original matrix has strictly fewer rows than columns ...
    m, n = 2, 4
    A = randn(m, n)
    # ... then, for a thin factorization of A, L should be a square matrix
    # of order of A's number of rows, Q should have the same shape as A,
    # and L and Q should have multiplication-compatible shapes
    Lthin, Qthin = lq(A, thin = true)
    @test size(Lthin) == (m, m)
    @test size(Qthin) == (m, n)
    @test isapprox(A, Lthin * Qthin)
    # ... and, for a non-thin factorization of A, L should have the same shape as A,
    # Q should be a square matrix of order of A's number of columns, and L and Q
    # should have multiplication-compatible shape. but instead the L returned has
    # no zero-padding on the right / is L for the thin factorization, so for
    # L and Q to have multiplication-compatible shapes, L must be zero-padded
    # to have the shape of A.
    Lsquare, Qsquare = lq(A, thin = false)
    @test size(Lsquare) == (m, m)
    @test size(Qsquare) == (n, n)
    @test isapprox(A, [Lsquare zeros(m, n - m)] * Qsquare)
end

@testset "getindex on LQPackedQ (#23733)" begin
    function getqs(F::Base.LinAlg.LQ)
        implicitQ = F[:Q]
        explicitQ = A_mul_B!(implicitQ, eye(eltype(implicitQ), size(implicitQ.factors, 2)))
        return implicitQ, explicitQ
    end

    m, n = 3, 3 # thin Q 3-by-3, square Q 3-by-3
    implicitQ, explicitQ = getqs(lqfact(randn(m, n)))
    @test implicitQ[1, 1] == explicitQ[1, 1]
    @test implicitQ[m, 1] == explicitQ[m, 1]
    @test implicitQ[1, n] == explicitQ[1, n]
    @test implicitQ[m, n] == explicitQ[m, n]

    m, n = 3, 4 # thin Q 3-by-4, square Q 4-by-4
    implicitQ, explicitQ = getqs(lqfact(randn(m, n)))
    @test implicitQ[1, 1] == explicitQ[1, 1]
    @test implicitQ[m, 1] == explicitQ[m, 1]
    @test implicitQ[1, n] == explicitQ[1, n]
    @test implicitQ[m, n] == explicitQ[m, n]
    @test implicitQ[m+1, 1] == explicitQ[m+1, 1]
    @test implicitQ[m+1, n] == explicitQ[m+1, n]

    m, n = 4, 3 # thin Q 3-by-3, square Q 3-by-3
    implicitQ, explicitQ = getqs(lqfact(randn(m, n)))
    @test implicitQ[1, 1] == explicitQ[1, 1]
    @test implicitQ[n, 1] == explicitQ[n, 1]
    @test implicitQ[1, n] == explicitQ[1, n]
    @test implicitQ[n, n] == explicitQ[n, n]
end

@testset "size on LQPackedQ (#23780)" begin
    # size(Q::LQPackedQ) yields the shape of Q's square form
    for ((mA, nA), nQ) in (
        ((3, 3), 3), # A 3-by-3 => square Q 3-by-3
        ((3, 4), 4), # A 3-by-4 => square Q 4-by-4
        ((4, 3), 3) )# A 4-by-3 => square Q 3-by-3
        @test size(lqfact(randn(mA, nA))[:Q]) == (nQ, nQ)
    end
end

@testset "postmultiplication with / right-application of LQPackedQ (#23779)" begin
    function getqs(F::Base.LinAlg.LQ)
        implicitQ = F[:Q]
        explicitQ = A_mul_B!(implicitQ, eye(eltype(implicitQ), size(implicitQ)...))
        return implicitQ, explicitQ
    end
    # for any shape m-by-n of LQ-factored matrix, where Q is an LQPackedQ
    # A_mul_B*(C, Q) (Ac_mul_B*(C, Q)) operations should work for
    # *-by-n (n-by-*) C, which we test below via n-by-n C
    for (mA, nA) in ((3, 3), (3, 4), (4, 3))
        implicitQ, explicitQ = getqs(lqfact(randn(mA, nA)))
        C = randn(nA, nA)
        @test *(C, implicitQ) ≈ *(C, explicitQ)
        @test A_mul_Bc(C, implicitQ) ≈ A_mul_Bc(C, explicitQ)
        @test Ac_mul_B(C, implicitQ) ≈ Ac_mul_B(C, explicitQ)
        @test Ac_mul_Bc(C, implicitQ) ≈ Ac_mul_Bc(C, explicitQ)
    end
    # where the LQ-factored matrix has at least as many rows m as columns n,
    # Q's square and thin forms have the same shape (n-by-n). hence we expect
    # _only_ *-by-n (n-by-*) C to work in A_mul_B*(C, Q) (Ac_mul_B*(C, Q)) ops.
    # and hence the n-by-n C tests above suffice.
    #
    # where the LQ-factored matrix has more columns n than rows m,
    # Q's square form is n-by-n whereas its thin form is m-by-n.
    # hence we need also test *-by-m (m-by-*) C with
    # A_mul_B*(C, Q) (Ac_mul_B*(C, Q)) ops, as below via m-by-m C.
    mA, nA = 3, 4
    implicitQ, explicitQ = getqs(lqfact(randn(mA, nA)))
    C = randn(mA, mA)
    zeroextCright = hcat(C, zeros(eltype(C), mA))
    zeroextCdown = vcat(C, zeros(eltype(C), (1, mA)))
    @test *(C, implicitQ) ≈ *(zeroextCright, explicitQ)
    @test A_mul_Bc(C, implicitQ) ≈ A_mul_Bc(zeroextCright, explicitQ)
    @test Ac_mul_B(C, implicitQ) ≈ Ac_mul_B(zeroextCdown, explicitQ)
    @test Ac_mul_Bc(C, implicitQ) ≈ Ac_mul_Bc(zeroextCdown, explicitQ)
end

# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestLQ

using Test, LinearAlgebra, Random
using LinearAlgebra: BlasComplex, BlasFloat, BlasReal, rmul!, lmul!

m = 10

Random.seed!(1234321)

asquare = randn(ComplexF64, m, m) / 2
awide = randn(ComplexF64, m, m+3) / 2
bcomplex = randn(ComplexF64, m, 2) / 2

# helper functions to unambiguously recover explicit forms of an LQPackedQ
squareQ(Q::LinearAlgebra.LQPackedQ) = (n = size(Q.factors, 2); lmul!(Q, Matrix{eltype(Q)}(I, n, n)))
rectangularQ(Q::LinearAlgebra.LQPackedQ) = convert(Array, Q)

@testset for eltya in (Float32, Float64, ComplexF32, ComplexF64), n in (m, size(awide, 2))
    adata = m == n ? asquare : awide
    a = convert(Matrix{eltya}, eltya <: Complex ? adata : real(adata))
    ε = εa = eps(abs(float(one(eltya))))
    n1 = n ÷ 2

    α = rand(eltya)
    aα = fill(α,1,1)
    @test lq(α).L*lq(α).Q ≈ lq(aα).L*lq(aα).Q
    @test abs(lq(α).Q[1,1]) ≈ one(eltya)

    @testset for eltyb in (Float32, Float64, ComplexF32, ComplexF64, Int)
        b = eltyb == Int ? rand(1:5, m, 2) : convert(Matrix{eltyb}, eltyb <: Complex ? bcomplex : real(bcomplex))
        εb = eps(abs(float(one(eltyb))))
        ε = max(εa,εb)

        tab = promote_type(eltya,eltyb)

        @testset for isview in (false,true)
            let a = isview ? view(a, 1:m - 1, 1:n - 1) : a, b = isview ? view(b, 1:m - 1) : b, m = m - isview, n = n - isview
                lqa   = lq(a)
                x = lqa\b
                l,q   = lqa.L, lqa.Q
                qra   = qr(a, Val(true))
                @testset "Basic ops" begin
                    @test size(lqa,1) == size(a,1)
                    @test size(lqa,3) == 1
                    @test size(lqa.Q,3) == 1
                    @test Base.propertynames(lqa) == (:L, :Q)
                    ref_obs = (l, q)
                    for (ii, lq_obj) in enumerate(lqa)
                        @test ref_obs[ii] == lq_obj
                    end
                    @test_throws ErrorException lqa.Z
                    @test Array(copy(adjoint(lqa))) ≈ a'
                    @test q*squareQ(q)' ≈ Matrix(I, n, n)
                    @test l*q ≈ a
                    @test Array(lqa) ≈ a
                    @test Array(copy(lqa)) ≈ a
                    lstring = sprint(show, l, context = :compact=>true)
                    qstring = sprint(show, q, context = :compact=>true)
                    @test sprint(show,MIME"text/plain"(),lqa) == "$(typeof(lqa)) with factors L and Q:\n$lstring\n$qstring"
                    @test LinearAlgebra.Factorization{eltya}(lqa) === lqa
                    @test Matrix{eltya}(q) isa Matrix{eltya}
                end
                @testset "Binary ops" begin
                    @test a*x ≈ b rtol=3000ε
                    @test x ≈ qra \ b rtol=3000ε
                    @test lqa*x ≈ a*x rtol=3000ε
                    @test (sq = size(q.factors, 2); *(Matrix{eltyb}(I, sq, sq), adjoint(q))*squareQ(q)) ≈ Matrix(I, n, n) rtol=5000ε
                    if eltya != Int
                        @test Matrix{eltyb}(I, n, n)*q ≈ convert(AbstractMatrix{tab},q)
                    end
                    @test q*x ≈ squareQ(q)*x rtol=100ε
                    @test transpose(q)*x ≈ transpose(squareQ(q))*x rtol=100ε
                    @test q'*x ≈ squareQ(q)'*x rtol=100ε
                    @test a*q ≈ a*squareQ(q) rtol=100ε
                    @test a*transpose(q) ≈ a*transpose(squareQ(q)) rtol=100ε
                    @test a*q' ≈ a*squareQ(q)' rtol=100ε
                    @test q*a'≈ squareQ(q)*a' rtol=100ε
                    @test q'*a' ≈ squareQ(q)'*a' rtol=100ε
                    @test_throws DimensionMismatch q*x[1:n1 + 1]
                    @test_throws DimensionMismatch adjoint(q) * Matrix{eltya}(undef,m+2,m+2)
                    @test_throws DimensionMismatch Matrix{eltyb}(undef,m+2,m+2)*q
                    if isa(a, DenseArray) && isa(b, DenseArray)
                        # use this to test 2nd branch in mult code
                        pad_a = vcat(I, a)
                        pad_x = hcat(I, x)
                        @test pad_a*q ≈ pad_a*squareQ(q) rtol=100ε
                        @test transpose(q)*pad_x ≈ transpose(squareQ(q))*pad_x rtol=100ε
                        @test q'*pad_x ≈ squareQ(q)'*pad_x rtol=100ε
                    end
                end
            end
        end

        @testset "Matmul with LQ factorizations" begin
            lqa = lq(a[:,1:n1])
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

@testset "getindex on LQPackedQ (#23733)" begin
    local m, n
    function getqs(F::LinearAlgebra.LQ)
        implicitQ = F.Q
        sq = size(implicitQ.factors, 2)
        explicitQ = lmul!(implicitQ, Matrix{eltype(implicitQ)}(I, sq, sq))
        return implicitQ, explicitQ
    end

    m, n = 3, 3 # reduced Q 3-by-3, full Q 3-by-3
    implicitQ, explicitQ = getqs(lq(randn(m, n)))
    @test implicitQ[1, 1] == explicitQ[1, 1]
    @test implicitQ[m, 1] == explicitQ[m, 1]
    @test implicitQ[1, n] == explicitQ[1, n]
    @test implicitQ[m, n] == explicitQ[m, n]

    m, n = 3, 4 # reduced Q 3-by-4, full Q 4-by-4
    implicitQ, explicitQ = getqs(lq(randn(m, n)))
    @test implicitQ[1, 1] == explicitQ[1, 1]
    @test implicitQ[m, 1] == explicitQ[m, 1]
    @test implicitQ[1, n] == explicitQ[1, n]
    @test implicitQ[m, n] == explicitQ[m, n]
    @test implicitQ[m+1, 1] == explicitQ[m+1, 1]
    @test implicitQ[m+1, n] == explicitQ[m+1, n]

    m, n = 4, 3 # reduced Q 3-by-3, full Q 3-by-3
    implicitQ, explicitQ = getqs(lq(randn(m, n)))
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
        @test size(lq(randn(mA, nA)).Q) == (nQ, nQ)
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
        implicitQ, explicitQ = getqs(lq(randn(mA, nA)))
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
    implicitQ, explicitQ = getqs(lq(randn(mA, nA)))
    C = randn(mA, mA)
    zeroextCright = hcat(C, zeros(eltype(C), mA))
    zeroextCdown = vcat(C, zeros(eltype(C), (1, mA)))
    @test *(C, implicitQ) ≈ *(zeroextCright, explicitQ)
    @test *(adjoint(C), implicitQ) ≈ *(adjoint(zeroextCdown), explicitQ)
    @test_throws DimensionMismatch C * adjoint(implicitQ)
    @test_throws DimensionMismatch adjoint(C) * adjoint(implicitQ)
end

@testset "det(Q::LQPackedQ)" begin
    @testset for n in 1:3, m in 1:3
        @testset "real" begin
            _, Q = lq(randn(n, m))
            @test det(Q) ≈ det(collect(Q))
            @test abs(det(Q)) ≈ 1
        end
        @testset "complex" begin
            _, Q = lq(randn(ComplexF64, n, m))
            @test det(Q) ≈ det(collect(Q))
            @test abs(det(Q)) ≈ 1
        end
    end
end

end # module TestLQ

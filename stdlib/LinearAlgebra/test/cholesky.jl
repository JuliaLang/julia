# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestCholesky

using Test, LinearAlgebra, Random
using LinearAlgebra: BlasComplex, BlasFloat, BlasReal, QRPivoted, PosDefException

function unary_ops_tests(a, ca, tol; n=size(a, 1))
    @test inv(ca)*a ≈ Matrix(I, n, n)
    @test a*inv(ca) ≈ Matrix(I, n, n)
    @test abs((det(ca) - det(a))/det(ca)) <= tol # Ad hoc, but statistically verified, revisit
    @test logdet(ca) ≈ logdet(a)
    @test logdet(ca) ≈ log(det(ca))  # logdet is less likely to overflow
    @test isposdef(ca)
    @test_throws ErrorException ca.Z
    @test size(ca) == size(a)
    @test Array(copy(ca)) ≈ a
end

function factor_recreation_tests(a_U, a_L)
    c_U = cholfact(a_U)
    c_L = cholfact(a_L)
    cl  = chol(a_L)
    ls = c_L.L
    @test Array(c_U) ≈ Array(c_L) ≈ a_U
    @test ls*ls' ≈ a_U
    @test triu(c_U.factors) ≈ c_U.U
    @test tril(c_L.factors) ≈ c_L.L
    @test istriu(cl)
    @test cl'cl ≈ a_U
    @test cl'cl ≈ a_L
end

@testset "core functionality" begin
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

    for eltya in (Float32, Float64, ComplexF32, ComplexF64, BigFloat, Int)
        a = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(areal, aimg) : areal)
        a2 = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(a2real, a2img) : a2real)

        ε = εa = eps(abs(float(one(eltya))))

        # Test of symmetric pos. def. strided matrix
        apd  = a'*a
        @inferred cholfact(apd)
        @inferred chol(apd)
        capd  = factorize(apd)
        r     = capd.U
        κ     = cond(apd, 1) #condition number

        unary_ops_tests(apd, capd, ε*κ*n)

        @testset "throw for non-square input" begin
            A = rand(eltya, 2, 3)
            @test_throws DimensionMismatch chol(A)
            @test_throws DimensionMismatch LinearAlgebra.chol!(A)
            @test_throws DimensionMismatch cholfact(A)
            @test_throws DimensionMismatch cholfact!(A)
        end

        #Test error bound on reconstruction of matrix: LAWNS 14, Lemma 2.1

        #these tests were failing on 64-bit linux when inside the inner loop
        #for eltya = ComplexF32 and eltyb = Int. The E[i,j] had NaN32 elements
        #but only with srand(1234321) set before the loops.
        E = abs.(apd - r'*r)
        for i=1:n, j=1:n
            @test E[i,j] <= (n+1)ε/(1-(n+1)ε)*real(sqrt(apd[i,i]*apd[j,j]))
        end
        E = abs.(apd - Matrix(capd))
        for i=1:n, j=1:n
            @test E[i,j] <= (n+1)ε/(1-(n+1)ε)*real(sqrt(apd[i,i]*apd[j,j]))
        end
        @test LinearAlgebra.issuccess(capd)
        @inferred(logdet(capd))

        apos = apd[1,1]            # test chol(x::Number), needs x>0
        @test all(x -> x ≈ √apos, cholfact(apos).factors)
        @test_throws PosDefException chol(-one(eltya))

        # Test cholfact with Symmetric/Hermitian upper/lower
        apds  = Symmetric(apd)
        apdsL = Symmetric(apd, :L)
        apdh  = Hermitian(apd)
        apdhL = Hermitian(apd, :L)
        if eltya <: Real
            capds = cholfact(apds)
            unary_ops_tests(apds, capds, ε*κ*n)
            if eltya <: BlasReal
                capds = cholfact!(copy(apds))
                unary_ops_tests(apds, capds, ε*κ*n)
            end
            ulstring = sprint((t, s) -> show(t, "text/plain", s), capds.UL)
            @test sprint((t, s) -> show(t, "text/plain", s), capds) == "$(typeof(capds))\nU factor:\n$ulstring"
        else
            capdh = cholfact(apdh)
            unary_ops_tests(apdh, capdh, ε*κ*n)
            capdh = cholfact!(copy(apdh))
            unary_ops_tests(apdh, capdh, ε*κ*n)
            capdh = cholfact!(copy(apd))
            unary_ops_tests(apd, capdh, ε*κ*n)
            ulstring = sprint((t, s) -> show(t, "text/plain", s), capdh.UL)
            @test sprint((t, s) -> show(t, "text/plain", s), capdh) == "$(typeof(capdh))\nU factor:\n$ulstring"
        end

        # test chol of 2x2 Strang matrix
        S = Matrix{eltya}(SymTridiagonal([2, 2], [-1]))
        @test Matrix(chol(S)) ≈ [2 -1; 0 sqrt(eltya(3))] / sqrt(eltya(2))

        # test extraction of factor and re-creating original matrix
        if eltya <: Real
            factor_recreation_tests(apds, apdsL)
        else
            factor_recreation_tests(apdh, apdhL)
        end

        #pivoted upper Cholesky
        if eltya != BigFloat
            cz = cholfact(Hermitian(zeros(eltya,n,n)), Val(true))
            @test_throws LinearAlgebra.RankDeficientException LinearAlgebra.chkfullrank(cz)
            cpapd = cholfact(apdh, Val(true))
            unary_ops_tests(apdh, cpapd, ε*κ*n)
            @test rank(cpapd) == n
            @test all(diff(diag(real(cpapd.factors))).<=0.) # diagonal should be non-increasing

            @test cpapd.P*cpapd.L*cpapd.U*cpapd.P' ≈ apd
        end

        for eltyb in (Float32, Float64, ComplexF32, ComplexF64, Int)
            b = eltyb == Int ? rand(1:5, n, 2) : convert(Matrix{eltyb}, eltyb <: Complex ? complex.(breal, bimg) : breal)
            εb = eps(abs(float(one(eltyb))))
            ε = max(εa,εb)

            for b in (b, view(b, 1:n, 1)) # Array and SubArray

                # Test error bound on linear solver: LAWNS 14, Theorem 2.1
                # This is a surprisingly loose bound
                x = capd\b
                @test norm(x-apd\b,1)/norm(x,1) <= (3n^2 + n + n^3*ε)*ε/(1-(n+1)*ε)*κ
                @test norm(apd*x-b,1)/norm(b,1) <= (3n^2 + n + n^3*ε)*ε/(1-(n+1)*ε)*κ

                @test norm(a*(capd\(a'*b)) - b,1)/norm(b,1) <= ε*κ*n # Ad hoc, revisit

                if eltya != BigFloat && eltyb != BigFloat
                    lapd = cholfact(apdhL)
                    @test norm(apd * (lapd\b) - b)/norm(b) <= ε*κ*n
                    @test norm(apd * (lapd\b[1:n]) - b[1:n])/norm(b[1:n]) <= ε*κ*n
                end

                if eltya != BigFloat && eltyb != BigFloat # Note! Need to implement pivoted Cholesky decomposition in julia

                    cpapd = cholfact(apdh, Val(true))
                    @test norm(apd * (cpapd\b) - b)/norm(b) <= ε*κ*n # Ad hoc, revisit
                    @test norm(apd * (cpapd\b[1:n]) - b[1:n])/norm(b[1:n]) <= ε*κ*n

                    lpapd = cholfact(apdhL, Val(true))
                    @test norm(apd * (lpapd\b) - b)/norm(b) <= ε*κ*n # Ad hoc, revisit
                    @test norm(apd * (lpapd\b[1:n]) - b[1:n])/norm(b[1:n]) <= ε*κ*n

                end
            end
        end
        if eltya <: BlasFloat
            @testset "throw for non positive definite matrix" begin
                A = eltya[1 2; 2 1]; B = eltya[1, 1]
                C = cholfact(A)
                @test !isposdef(C)
                @test !LinearAlgebra.issuccess(C)
                Cstr = sprint((t, s) -> show(t, "text/plain", s), C)
                @test Cstr == "Failed factorization of type $(typeof(C))"
                @test_throws PosDefException C\B
                @test_throws PosDefException det(C)
                @test_throws PosDefException logdet(C)
            end

            # Test generic cholfact!
            @testset "generic cholfact!" begin
                if eltya <: Complex
                    A = complex.(randn(5,5), randn(5,5))
                else
                    A = randn(5,5)
                end
                A = convert(Matrix{eltya}, A'A)
                @test Matrix(cholfact(A).L) ≈ Matrix(invoke(LinearAlgebra._chol!, Tuple{AbstractMatrix, Type{LowerTriangular}}, copy(A), LowerTriangular)[1])
                @test Matrix(cholfact(A).U) ≈ Matrix(invoke(LinearAlgebra._chol!, Tuple{AbstractMatrix, Type{UpperTriangular}}, copy(A), UpperTriangular)[1])
            end
        end
    end
end

@testset "Cholesky factor of Matrix with non-commutative elements, here 2x2-matrices" begin
    X = Matrix{Float64}[0.1*rand(2,2) for i in 1:3, j = 1:3]
    L = Matrix(LinearAlgebra._chol!(X*X', LowerTriangular)[1])
    U = Matrix(LinearAlgebra._chol!(X*X', UpperTriangular)[1])
    XX = Matrix(X*X')

    @test sum(sum(norm, L*L' - XX)) < eps()
    @test sum(sum(norm, U'*U - XX)) < eps()
end


@testset "cholesky up- and downdates" begin
    A = complex.(randn(10,5), randn(10, 5))
    v = complex.(randn(5), randn(5))
    for uplo in (:U, :L)
        AcA = A'*A
        BcB = AcA + v*v'
        BcB = (BcB + BcB')/2
        F = cholfact(Hermitian(AcA, uplo))
        G = cholfact(Hermitian(BcB, uplo))
        @test Base.getproperty(LinearAlgebra.lowrankupdate(F, v), uplo) ≈ Base.getproperty(G, uplo)
        @test_throws DimensionMismatch LinearAlgebra.lowrankupdate(F, Vector{eltype(v)}(undef,length(v)+1))
        @test Base.getproperty(LinearAlgebra.lowrankdowndate(G, v), uplo) ≈ Base.getproperty(F, uplo)
        @test_throws DimensionMismatch LinearAlgebra.lowrankdowndate(G, Vector{eltype(v)}(undef,length(v)+1))
    end
end

@testset "issue #13243, unexpected nans in complex cholfact" begin
    apd = [5.8525753f0 + 0.0f0im -0.79540455f0 + 0.7066077f0im 0.98274714f0 + 1.3824869f0im 2.619998f0 + 1.8532984f0im -1.8306153f0 - 1.2336911f0im 0.32275113f0 + 0.015575029f0im 2.1968813f0 + 1.0640624f0im 0.27894387f0 + 0.97911835f0im 3.0476584f0 + 0.18548489f0im 0.3842994f0 + 0.7050991f0im
        -0.79540455f0 - 0.7066077f0im 8.313246f0 + 0.0f0im -1.8076122f0 - 0.8882447f0im 0.47806996f0 + 0.48494184f0im 0.5096429f0 - 0.5395974f0im -0.7285097f0 - 0.10360408f0im -1.1760061f0 - 2.7146957f0im -0.4271084f0 + 0.042899966f0im -1.7228563f0 + 2.8335886f0im 1.8942566f0 + 0.6389735f0im
        0.98274714f0 - 1.3824869f0im -1.8076122f0 + 0.8882447f0im 9.367975f0 + 0.0f0im -0.1838578f0 + 0.6468568f0im -1.8338387f0 + 0.7064959f0im 0.041852742f0 - 0.6556877f0im 2.5673025f0 + 1.9732997f0im -1.1148382f0 - 0.15693812f0im 2.4704504f0 - 1.0389464f0im 1.0858271f0 - 1.298006f0im
        2.619998f0 - 1.8532984f0im 0.47806996f0 - 0.48494184f0im -0.1838578f0 - 0.6468568f0im 3.1117508f0 + 0.0f0im -1.956626f0 + 0.22825956f0im 0.07081801f0 - 0.31801307f0im 0.3698375f0 - 0.5400855f0im 0.80686307f0 + 1.5315914f0im 1.5649154f0 - 1.6229297f0im -0.112077385f0 + 1.2014246f0im
        -1.8306153f0 + 1.2336911f0im 0.5096429f0 + 0.5395974f0im -1.8338387f0 - 0.7064959f0im -1.956626f0 - 0.22825956f0im 3.6439795f0 + 0.0f0im -0.2594722f0 + 0.48786148f0im -0.47636223f0 - 0.27821827f0im -0.61608654f0 - 2.01858f0im -2.7767487f0 + 1.7693765f0im 0.048102796f0 - 0.9741874f0im
        0.32275113f0 - 0.015575029f0im -0.7285097f0 + 0.10360408f0im 0.041852742f0 + 0.6556877f0im 0.07081801f0 + 0.31801307f0im -0.2594722f0 - 0.48786148f0im 3.624376f0 + 0.0f0im -1.6697118f0 + 0.4017511f0im -1.4397877f0 - 0.7550918f0im -0.31456697f0 - 1.0403451f0im -0.31978557f0 + 0.13701046f0im
        2.1968813f0 - 1.0640624f0im -1.1760061f0 + 2.7146957f0im 2.5673025f0 - 1.9732997f0im 0.3698375f0 + 0.5400855f0im -0.47636223f0 + 0.27821827f0im -1.6697118f0 - 0.4017511f0im 6.8273163f0 + 0.0f0im -0.10051322f0 + 0.24303961f0im 1.4415971f0 + 0.29750675f0im 1.221786f0 - 0.85654986f0im
        0.27894387f0 - 0.97911835f0im -0.4271084f0 - 0.042899966f0im -1.1148382f0 + 0.15693812f0im 0.80686307f0 - 1.5315914f0im -0.61608654f0 + 2.01858f0im -1.4397877f0 + 0.7550918f0im -0.10051322f0 - 0.24303961f0im 3.4057708f0 + 0.0f0im -0.5856801f0 - 1.0203559f0im 0.7103452f0 + 0.8422135f0im
        3.0476584f0 - 0.18548489f0im -1.7228563f0 - 2.8335886f0im 2.4704504f0 + 1.0389464f0im 1.5649154f0 + 1.6229297f0im -2.7767487f0 - 1.7693765f0im -0.31456697f0 + 1.0403451f0im 1.4415971f0 - 0.29750675f0im -0.5856801f0 + 1.0203559f0im 7.005772f0 + 0.0f0im -0.9617417f0 - 1.2486815f0im
        0.3842994f0 - 0.7050991f0im 1.8942566f0 - 0.6389735f0im 1.0858271f0 + 1.298006f0im -0.112077385f0 - 1.2014246f0im 0.048102796f0 + 0.9741874f0im -0.31978557f0 - 0.13701046f0im 1.221786f0 + 0.85654986f0im 0.7103452f0 - 0.8422135f0im -0.9617417f0 + 1.2486815f0im 3.4629636f0 + 0.0f0im]
    b = [-0.905011814118756 + 0.2847570854574069im -0.7122162951294634 - 0.630289556702497im
        -0.7620356655676837 + 0.15533508334193666im 0.39947219167701153 - 0.4576746001199889im
        -0.21782716937787788 - 0.9222220085490986im -0.727775859267237 + 0.50638268521728im
        -1.0509472322215125 + 0.5022165705328413im -0.7264975746431271 + 0.31670415674097235im
        -0.6650468984506477 - 0.5000967284800251im -0.023682508769195098 + 0.18093440285319276im
        -0.20604111555491242 + 0.10570814584017311im 0.562377322638969 - 0.2578030745663871im
        -0.3451346708401685 + 1.076948486041297im 0.9870834574024372 - 0.2825689605519449im
        0.25336108035924787 + 0.975317836492159im 0.0628393808469436 - 0.1253397353973715im
        0.11192755545114 - 0.1603741874112385im 0.8439562576196216 + 1.0850814110398734im
        -1.0568488936791578 - 0.06025820467086475im 0.12696236014017806 - 0.09853584666755086im]
    cholfact(Hermitian(apd, :L), Val(true)) \ b
    r = factorize(apd).U
    E = abs.(apd - r'*r)
    ε = eps(abs(float(one(ComplexF32))))
    n = 10
    for i=1:n, j=1:n
        @test E[i,j] <= (n+1)ε/(1-(n+1)ε)*real(sqrt(apd[i,i]*apd[j,j]))
    end
end

@testset "handling of non-Hermitian" begin
    R = randn(5, 5)
    C = complex.(R, R)
    for A in (R, C)
        @test !LinearAlgebra.issuccess(cholfact(A))
        @test !LinearAlgebra.issuccess(cholfact!(copy(A)))
        @test_throws PosDefException chol(A)
        @test_throws PosDefException LinearAlgebra.chol!(copy(A))
    end
end

@testset "fail for non-BLAS element types" begin
    @test_throws ArgumentError cholfact!(Hermitian(rand(Float16, 5,5)), Val(true))
end

end # module TestCholesky

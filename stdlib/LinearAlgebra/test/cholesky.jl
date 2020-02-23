# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestCholesky

using Test, LinearAlgebra, Random
using LinearAlgebra: BlasComplex, BlasFloat, BlasReal, QRPivoted,
    PosDefException, RankDeficientException, chkfullrank

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
    c_U = cholesky(a_U)
    c_L = cholesky(a_L)
    cl  = c_L.U
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

    Random.seed!(1234321)

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
        @inferred cholesky(apd)
        capd  = factorize(apd)
        r     = capd.U
        κ     = cond(apd, 1) #condition number

        unary_ops_tests(apd, capd, ε*κ*n)

        @testset "throw for non-square input" begin
            A = rand(eltya, 2, 3)
            @test_throws DimensionMismatch cholesky(A)
            @test_throws DimensionMismatch cholesky!(A)
        end

        #Test error bound on reconstruction of matrix: LAWNS 14, Lemma 2.1

        #these tests were failing on 64-bit linux when inside the inner loop
        #for eltya = ComplexF32 and eltyb = Int. The E[i,j] had NaN32 elements
        #but only with Random.seed!(1234321) set before the loops.
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

        apos = apd[1,1]
        @test all(x -> x ≈ √apos, cholesky(apos).factors)

        # Test cholesky with Symmetric/Hermitian upper/lower
        apds  = Symmetric(apd)
        apdsL = Symmetric(apd, :L)
        apdh  = Hermitian(apd)
        apdhL = Hermitian(apd, :L)
        if eltya <: Real
            capds = cholesky(apds)
            unary_ops_tests(apds, capds, ε*κ*n)
            if eltya <: BlasReal
                capds = cholesky!(copy(apds))
                unary_ops_tests(apds, capds, ε*κ*n)
            end
            ulstring = sprint((t, s) -> show(t, "text/plain", s), capds.UL)
            @test sprint((t, s) -> show(t, "text/plain", s), capds) == "$(typeof(capds))\nU factor:\n$ulstring"
        else
            capdh = cholesky(apdh)
            unary_ops_tests(apdh, capdh, ε*κ*n)
            capdh = cholesky!(copy(apdh))
            unary_ops_tests(apdh, capdh, ε*κ*n)
            capdh = cholesky!(copy(apd))
            unary_ops_tests(apd, capdh, ε*κ*n)
            ulstring = sprint((t, s) -> show(t, "text/plain", s), capdh.UL)
            @test sprint((t, s) -> show(t, "text/plain", s), capdh) == "$(typeof(capdh))\nU factor:\n$ulstring"
        end

        # test cholesky of 2x2 Strang matrix
        S = Matrix{eltya}(SymTridiagonal([2, 2], [-1]))
        @test Matrix(cholesky(S).U) ≈ [2 -1; 0 sqrt(eltya(3))] / sqrt(eltya(2))

        # test extraction of factor and re-creating original matrix
        if eltya <: Real
            factor_recreation_tests(apds, apdsL)
        else
            factor_recreation_tests(apdh, apdhL)
        end

        #pivoted upper Cholesky
        if eltya != BigFloat
            cpapd = cholesky(apdh, Val(true))
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
                    lapd = cholesky(apdhL)
                    @test norm(apd * (lapd\b) - b)/norm(b) <= ε*κ*n
                    @test norm(apd * (lapd\b[1:n]) - b[1:n])/norm(b[1:n]) <= ε*κ*n
                end

                if eltya != BigFloat && eltyb != BigFloat # Note! Need to implement pivoted Cholesky decomposition in julia

                    cpapd = cholesky(apdh, Val(true))
                    @test norm(apd * (cpapd\b) - b)/norm(b) <= ε*κ*n # Ad hoc, revisit
                    @test norm(apd * (cpapd\b[1:n]) - b[1:n])/norm(b[1:n]) <= ε*κ*n

                    lpapd = cholesky(apdhL, Val(true))
                    @test norm(apd * (lpapd\b) - b)/norm(b) <= ε*κ*n # Ad hoc, revisit
                    @test norm(apd * (lpapd\b[1:n]) - b[1:n])/norm(b[1:n]) <= ε*κ*n
                end
            end
        end

        for eltyb in (Float64, ComplexF64)
            Breal = convert(Matrix{BigFloat}, randn(n,n)/2)
            Bimg  = convert(Matrix{BigFloat}, randn(n,n)/2)
            B = (eltya <: Complex || eltyb <: Complex) ? complex.(Breal, Bimg) : Breal
            εb = eps(abs(float(one(eltyb))))
            ε = max(εa,εb)

            for B in (B, view(B, 1:n, 1:n)) # Array and SubArray

                # Test error bound on linear solver: LAWNS 14, Theorem 2.1
                # This is a surprisingly loose bound
                BB = copy(B)
                ldiv!(capd, BB)
                @test norm(apd \ B - BB, 1) / norm(BB, 1) <= (3n^2 + n + n^3*ε)*ε/(1-(n+1)*ε)*κ
                @test norm(apd * BB - B, 1) / norm(B, 1) <= (3n^2 + n + n^3*ε)*ε/(1-(n+1)*ε)*κ
                if eltya != BigFloat
                    cpapd = cholesky(apdh, Val(true))
                    BB = copy(B)
                    ldiv!(cpapd, BB)
                    @test norm(apd \ B - BB, 1) / norm(BB, 1) <= (3n^2 + n + n^3*ε)*ε/(1-(n+1)*ε)*κ
                    @test norm(apd * BB - B, 1) / norm(B, 1) <= (3n^2 + n + n^3*ε)*ε/(1-(n+1)*ε)*κ
                end
            end
        end

        @testset "solve with generic Cholesky" begin
            Breal = convert(Matrix{BigFloat}, randn(n,n)/2)
            Bimg  = convert(Matrix{BigFloat}, randn(n,n)/2)
            B = eltya <: Complex ? complex.(Breal, Bimg) : Breal
            εb = eps(abs(float(one(eltype(B)))))
            ε = max(εa,εb)

            for B in (B, view(B, 1:n, 1:n)) # Array and SubArray

                # Test error bound on linear solver: LAWNS 14, Theorem 2.1
                # This is a surprisingly loose bound
                cpapd = cholesky(eltya <: Complex ? apdh : apds)
                BB = copy(B)
                rdiv!(BB, cpapd)
                @test norm(B / apd - BB, 1) / norm(BB, 1) <= (3n^2 + n + n^3*ε)*ε/(1-(n+1)*ε)*κ
                @test norm(BB * apd - B, 1) / norm(B, 1) <= (3n^2 + n + n^3*ε)*ε/(1-(n+1)*ε)*κ
                cpapd = cholesky(eltya <: Complex ? apdhL : apdsL)
                BB = copy(B)
                rdiv!(BB, cpapd)
                @test norm(B / apd - BB, 1) / norm(BB, 1) <= (3n^2 + n + n^3*ε)*ε/(1-(n+1)*ε)*κ
                @test norm(BB * apd - B, 1) / norm(B, 1) <= (3n^2 + n + n^3*ε)*ε/(1-(n+1)*ε)*κ
                if eltya != BigFloat
                    cpapd = cholesky(eltya <: Complex ? apdh : apds, Val(true))
                    BB = copy(B)
                    rdiv!(BB, cpapd)
                    @test norm(B / apd - BB, 1) / norm(BB, 1) <= (3n^2 + n + n^3*ε)*ε/(1-(n+1)*ε)*κ
                    @test norm(BB * apd - B, 1) / norm(B, 1) <= (3n^2 + n + n^3*ε)*ε/(1-(n+1)*ε)*κ
                    cpapd = cholesky(eltya <: Complex ? apdhL : apdsL, Val(true))
                    BB = copy(B)
                    rdiv!(BB, cpapd)
                    @test norm(B / apd - BB, 1) / norm(BB, 1) <= (3n^2 + n + n^3*ε)*ε/(1-(n+1)*ε)*κ
                    @test norm(BB * apd - B, 1) / norm(B, 1) <= (3n^2 + n + n^3*ε)*ε/(1-(n+1)*ε)*κ
                end
            end
        end
        if eltya <: BlasFloat
            @testset "generic cholesky!" begin
                if eltya <: Complex
                    A = complex.(randn(5,5), randn(5,5))
                else
                    A = randn(5,5)
                end
                A = convert(Matrix{eltya}, A'A)
                @test Matrix(cholesky(A).L) ≈ Matrix(invoke(LinearAlgebra._chol!, Tuple{AbstractMatrix, Type{LowerTriangular}}, copy(A), LowerTriangular)[1])
                @test Matrix(cholesky(A).U) ≈ Matrix(invoke(LinearAlgebra._chol!, Tuple{AbstractMatrix, Type{UpperTriangular}}, copy(A), UpperTriangular)[1])
            end
        end
    end
end

@testset "behavior for non-positive definite matrices" for T in (Float64, ComplexF64)
    A = T[1 2; 2 1]
    B = T[1 2; 0 1]
    # check = (true|false)
    for M in (A, Hermitian(A), B)
        @test_throws PosDefException cholesky(M)
        @test_throws PosDefException cholesky!(copy(M))
        @test_throws PosDefException cholesky(M; check = true)
        @test_throws PosDefException cholesky!(copy(M); check = true)
        @test !LinearAlgebra.issuccess(cholesky(M; check = false))
        @test !LinearAlgebra.issuccess(cholesky!(copy(M); check = false))
    end
    for M in (A, Hermitian(A), B)
        @test_throws RankDeficientException cholesky(M, Val(true))
        @test_throws RankDeficientException cholesky!(copy(M), Val(true))
        @test_throws RankDeficientException cholesky(M, Val(true); check = true)
        @test_throws RankDeficientException cholesky!(copy(M), Val(true); check = true)
        C = cholesky(M, Val(true); check = false)
        @test_throws RankDeficientException chkfullrank(C)
        C = cholesky!(copy(M), Val(true); check = false)
        @test_throws RankDeficientException chkfullrank(C)
    end
    @test !isposdef(A)
    str = sprint((io, x) -> show(io, "text/plain", x), cholesky(A; check = false))
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
        F = cholesky(Hermitian(AcA, uplo))
        G = cholesky(Hermitian(BcB, uplo))
        @test Base.getproperty(LinearAlgebra.lowrankupdate(F, v), uplo) ≈ Base.getproperty(G, uplo)
        @test_throws DimensionMismatch LinearAlgebra.lowrankupdate(F, Vector{eltype(v)}(undef,length(v)+1))
        @test Base.getproperty(LinearAlgebra.lowrankdowndate(G, v), uplo) ≈ Base.getproperty(F, uplo)
        @test_throws DimensionMismatch LinearAlgebra.lowrankdowndate(G, Vector{eltype(v)}(undef,length(v)+1))
    end
end

@testset "issue #13243, unexpected nans in complex cholesky" begin
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
    cholesky(Hermitian(apd, :L), Val(true)) \ b
    r = factorize(apd).U
    E = abs.(apd - r'*r)
    ε = eps(abs(float(one(ComplexF32))))
    n = 10
    for i=1:n, j=1:n
        @test E[i,j] <= (n+1)ε/(1-(n+1)ε)*real(sqrt(apd[i,i]*apd[j,j]))
    end
end

@testset "fail for non-BLAS element types" begin
    @test_throws ArgumentError cholesky!(Hermitian(rand(Float16, 5,5)), Val(true))
end

@testset "cholesky Diagonal" begin
    # real
    d = abs.(randn(3)) .+ 0.1
    D = Diagonal(d)
    CD = cholesky(D)
    CM = cholesky(Matrix(D))
    @test CD isa Cholesky{Float64}
    @test CD.U == Diagonal(.√d) == CM.U
    @test D ≈ CD.L * CD.U
    @test CD.info == 0

    # real, failing
    @test_throws PosDefException cholesky(Diagonal([1.0, -2.0]))
    Dnpd = cholesky(Diagonal([1.0, -2.0]); check = false)
    @test Dnpd.info == 2

    # complex
    D = complex(D)
    CD = cholesky(D)
    CM = cholesky(Matrix(D))
    @test CD isa Cholesky{Complex{Float64}}
    @test CD.U == Diagonal(.√d) == CM.U
    @test D ≈ CD.L * CD.U
    @test CD.info == 0

    # complex, failing
    D[2, 2] = 0.0 + 0im
    @test_throws PosDefException cholesky(D)
    Dnpd = cholesky(D; check = false)
    @test Dnpd.info == 2

    # InexactError for Int
    @test_throws InexactError cholesky!(Diagonal([2, 1]))
end

@testset "constructor with non-BlasInt arguments" begin

    x = rand(5,5)
    chol = cholesky(x'x)

    factors, uplo, info = chol.factors, chol.uplo, chol.info

    @test Cholesky(factors, uplo, Int32(info)) == chol
    @test Cholesky(factors, uplo, Int64(info)) == chol

    cholp = cholesky(x'x, Val(true))

    factors, uplo, piv, rank, tol, info =
        cholp.factors, cholp.uplo, cholp.piv, cholp.rank, cholp.tol, cholp.info

    @test CholeskyPivoted(factors, uplo, Vector{Int32}(piv), rank, tol, info) == cholp
    @test CholeskyPivoted(factors, uplo, Vector{Int64}(piv), rank, tol, info) == cholp

    @test CholeskyPivoted(factors, uplo, piv, Int32(rank), tol, info) == cholp
    @test CholeskyPivoted(factors, uplo, piv, Int64(rank), tol, info) == cholp

    @test CholeskyPivoted(factors, uplo, piv, rank, tol, Int32(info)) == cholp
    @test CholeskyPivoted(factors, uplo, piv, rank, tol, Int64(info)) == cholp

end

@testset "issue #33704, casting low-rank CholeskyPivoted to Matrix" begin
    A = randn(1,8)
    B = A'A
    C = cholesky(B, Val(true), check=false)
    @test B ≈ Matrix(C)
end

@testset "covering _chol!(A::StridedMatrix)" begin
    # Taking a hermitian, symmetric positive definite Matrix
    b = [4 12 -16; 12 37 -43; -16 -43 98]
    res_b = LinearAlgebra._chol!(b)
    @test res_b[1] == [2 6 -8; 0 1 5; 0 0 3;]
    @test res_b[2] != -1 # as b is hermitian
    c = [2. 2+im 4.; 2-im 3 im; 4 -im 1.]
    res_c = LinearAlgebra._chol!(c)
    @test res_c[1] == Complex{Float64}[1.4142135623730951 + 0.0im 1.414213562373095 + 0.7071067811865475im 2.82842712474619 + 0.0im; 0.0 + 0.0im 0.7071067811865478 + 0.0im -5.656854249492377 + 4.242640687119283im; 0.0 + 0.0im 0.0 + 0.0im -56.99999999999994 + 0.0im]
    @test res_c[2] != -1
    non_hermit = [1 2 3; 4 5 6; 7 8 9;]
    @test LinearAlgebra._chol!(non_hermit)[2] == -1 # Since it is not hermitian
end
end # module TestCholesky

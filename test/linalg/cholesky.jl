# This file is a part of Julia. License is MIT: http://julialang.org/license

debug = false

using Base.Test

using Base.LinAlg: BlasComplex, BlasFloat, BlasReal, QRPivoted

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

for eltya in (Float32, Float64, Complex64, Complex128, BigFloat, Int)
    a = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(areal, aimg) : areal)
    a2 = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(a2real, a2img) : a2real)
    apd  = a'*a                  # symmetric positive-definite

    apds  = Symmetric(apd)
    apdsL = Symmetric(apd, :L)
    apdh  = Hermitian(apd)
    apdhL = Hermitian(apd, :L)
    ε = εa = eps(abs(float(one(eltya))))

    @inferred cholfact(apd)
    @inferred chol(apd)
    capd  = factorize(apd)
    r     = capd[:U]
    κ     = cond(apd, 1) #condition number

    #getindex
    @test_throws KeyError capd[:Z]

    #Test error bound on reconstruction of matrix: LAWNS 14, Lemma 2.1

    #these tests were failing on 64-bit linux when inside the inner loop
    #for eltya = Complex64 and eltyb = Int. The E[i,j] had NaN32 elements
    #but only with srand(1234321) set before the loops.
    E = abs.(apd - r'*r)
    for i=1:n, j=1:n
        @test E[i,j] <= (n+1)ε/(1-(n+1)ε)*real(sqrt(apd[i,i]*apd[j,j]))
    end
    E = abs.(apd - full(capd))
    for i=1:n, j=1:n
        @test E[i,j] <= (n+1)ε/(1-(n+1)ε)*real(sqrt(apd[i,i]*apd[j,j]))
    end
    @test apd*inv(capd) ≈ eye(n)
    @test abs((det(capd) - det(apd))/det(capd)) <= ε*κ*n # Ad hoc, but statistically verified, revisit
    @test @inferred(logdet(capd)) ≈ log(det(capd)) # logdet is less likely to overflow

    apos = apd[1,1]            # test chol(x::Number), needs x>0
    @test all(x -> x ≈ √apos, cholfact(apos).factors)
    @test_throws ArgumentError chol(-one(eltya))

    if eltya <: Real
        capds = cholfact(apds)
        @test inv(capds)*apds ≈ eye(n)
        @test abs((det(capds) - det(apd))/det(capds)) <= ε*κ*n
        if eltya <: BlasReal
            capds = cholfact!(copy(apds))
            @test inv(capds)*apds ≈ eye(n)
            @test abs((det(capds) - det(apd))/det(capds)) <= ε*κ*n
        end
    else
        capdh = cholfact(apdh)
        @test inv(capdh)*apdh ≈ eye(n)
        @test abs((det(capdh) - det(apd))/det(capdh)) <= ε*κ*n
        capdh = cholfact!(copy(apdh))
        @test inv(capdh)*apdh ≈ eye(n)
        @test abs((det(capdh) - det(apd))/det(capdh)) <= ε*κ*n
        capdh = cholfact!(copy(apd))
        @test inv(capdh)*apdh ≈ eye(n)
        @test abs((det(capdh) - det(apd))/det(capdh)) <= ε*κ*n
        capdh = cholfact!(copy(apd), :L)
        @test inv(capdh)*apdh ≈ eye(n)
        @test abs((det(capdh) - det(apd))/det(capdh)) <= ε*κ*n
    end

    # test chol of 2x2 Strang matrix
    S = convert(AbstractMatrix{eltya},full(SymTridiagonal([2,2],[-1])))
    U = Bidiagonal([2,sqrt(eltya(3))],[-1],true) / sqrt(eltya(2))
    @test full(chol(S)) ≈ full(U)

    #lower Cholesky factor
    lapd = cholfact(apd, :L)
    @test full(lapd) ≈ apd
    l = lapd[:L]
    @test l*l' ≈ apd
    @test triu(capd.factors) ≈ lapd[:U]
    @test tril(lapd.factors) ≈ capd[:L]
    if eltya <: Real
        capds = cholfact(apds)
        lapds = cholfact(apdsL)
        cl    = chol(apdsL)
        ls = lapds[:L]
        @test ls*ls' ≈ apd
        @test triu(capds.factors) ≈ lapds[:U]
        @test tril(lapds.factors) ≈ capds[:L]
        @test istriu(cl)
        @test cl'cl ≈ apds
        @test cl'cl ≈ apdsL
    else
        capdh = cholfact(apdh)
        lapdh = cholfact(apdhL)
        cl    = chol(apdhL)
        ls = lapdh[:L]
        @test ls*ls' ≈ apd
        @test triu(capdh.factors) ≈ lapdh[:U]
        @test tril(lapdh.factors) ≈ capdh[:L]
        @test istriu(cl)
        @test cl'cl ≈ apdh
        @test cl'cl ≈ apdhL
    end

    #pivoted upper Cholesky
    if eltya != BigFloat
        cz = cholfact(zeros(eltya,n,n), :U, Val{true})
        @test_throws Base.LinAlg.RankDeficientException Base.LinAlg.chkfullrank(cz)
        cpapd = cholfact(apd, :U, Val{true})
        @test rank(cpapd) == n
        @test all(diff(diag(real(cpapd.factors))).<=0.) # diagonal should be non-increasing
        if isreal(apd)
            @test apd*inv(cpapd) ≈ eye(n)
        end
        @test full(cpapd) ≈ apd
        #getindex
        @test_throws KeyError cpapd[:Z]

        @test size(cpapd) == size(apd)
        @test full(copy(cpapd)) ≈ apd
        @test det(cpapd) ≈ det(apd)
        @test logdet(cpapd) ≈ logdet(apd)
        @test cpapd[:P]*cpapd[:L]*cpapd[:U]*cpapd[:P]' ≈ apd
    end

    for eltyb in (Float32, Float64, Complex64, Complex128, Int)
        b = eltyb == Int ? rand(1:5, n, 2) : convert(Matrix{eltyb}, eltyb <: Complex ? complex.(breal, bimg) : breal)
        εb = eps(abs(float(one(eltyb))))
        ε = max(εa,εb)

debug && println("\ntype of a: ", eltya, " type of b: ", eltyb, "\n")
        let Bs = b
            for atype in ("Array", "SubArray")
                if atype == "Array"
                    b = Bs
                else
                    b = view(Bs, 1:n, 1)
                end

                # Test error bound on linear solver: LAWNS 14, Theorem 2.1
                # This is a surprisingly loose bound
                x = capd\b
                @test norm(x-apd\b,1)/norm(x,1) <= (3n^2 + n + n^3*ε)*ε/(1-(n+1)*ε)*κ
                @test norm(apd*x-b,1)/norm(b,1) <= (3n^2 + n + n^3*ε)*ε/(1-(n+1)*ε)*κ

                @test norm(a*(capd\(a'*b)) - b,1)/norm(b,1) <= ε*κ*n # Ad hoc, revisit

                if eltya != BigFloat && eltyb != BigFloat
                    @test norm(apd * (lapd\b) - b)/norm(b) <= ε*κ*n
                    @test norm(apd * (lapd\b[1:n]) - b[1:n])/norm(b[1:n]) <= ε*κ*n
                end

debug && println("pivoted Cholesky decomposition")
                if eltya != BigFloat && eltyb != BigFloat # Note! Need to implement pivoted Cholesky decomposition in julia

                    @test norm(apd * (cpapd\b) - b)/norm(b) <= ε*κ*n # Ad hoc, revisit
                    @test norm(apd * (cpapd\b[1:n]) - b[1:n])/norm(b[1:n]) <= ε*κ*n

                    lpapd = cholfact(apd, :L, Val{true})
                    @test norm(apd * (lpapd\b) - b)/norm(b) <= ε*κ*n # Ad hoc, revisit
                    @test norm(apd * (lpapd\b[1:n]) - b[1:n])/norm(b[1:n]) <= ε*κ*n
                end
            end
        end
    end
end

begin
    # Cholesky factor of Matrix with non-commutative elements, here 2x2-matrices

    X = Matrix{Float64}[0.1*rand(2,2) for i in 1:3, j = 1:3]
    L = full(Base.LinAlg._chol!(X*X', LowerTriangular))
    U = full(Base.LinAlg._chol!(X*X', UpperTriangular))
    XX = full(X*X')

    @test sum(sum(norm, L*L' - XX)) < eps()
    @test sum(sum(norm, U'*U - XX)) < eps()
end

# Test generic cholfact!
for elty in (Float32, Float64, Complex{Float32}, Complex{Float64})
    if elty <: Complex
        A = complex.(randn(5,5), randn(5,5))
    else
        A = randn(5,5)
    end
    A = convert(Matrix{elty}, A'A)
    @test full(cholfact(A)[:L]) ≈ full(invoke(Base.LinAlg._chol!, Tuple{AbstractMatrix, Type{LowerTriangular}}, copy(A), LowerTriangular))
    @test full(cholfact(A)[:U]) ≈ full(invoke(Base.LinAlg._chol!, Tuple{AbstractMatrix, Type{UpperTriangular}}, copy(A), UpperTriangular))
end

# Test up- and downdates
let A = complex.(randn(10,5), randn(10, 5)), v = complex.(randn(5), randn(5))
    for uplo in (:U, :L)
        AcA = A'A
        BcB = AcA + v*v'
        BcB = (BcB + BcB')/2
        F = cholfact(AcA, uplo)
        G = cholfact(BcB, uplo)
        @test LinAlg.lowrankupdate(F, v)[uplo] ≈ G[uplo]
        @test_throws DimensionMismatch LinAlg.lowrankupdate(F, ones(eltype(v), length(v)+1))
        @test LinAlg.lowrankdowndate(G, v)[uplo] ≈ F[uplo]
        @test_throws DimensionMismatch LinAlg.lowrankdowndate(G, ones(eltype(v), length(v)+1))
    end
end

# issue #13243, unexpected nans in complex cholfact
let apd = [5.8525753f0 + 0.0f0im -0.79540455f0 + 0.7066077f0im 0.98274714f0 + 1.3824869f0im 2.619998f0 + 1.8532984f0im -1.8306153f0 - 1.2336911f0im 0.32275113f0 + 0.015575029f0im 2.1968813f0 + 1.0640624f0im 0.27894387f0 + 0.97911835f0im 3.0476584f0 + 0.18548489f0im 0.3842994f0 + 0.7050991f0im
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
    cholfact(apd, :L, Val{true}) \ b
    r = factorize(apd)[:U]
    E = abs.(apd - r'*r)
    ε = eps(abs(float(one(Complex64))))
    n = 10
    for i=1:n, j=1:n
        @test E[i,j] <= (n+1)ε/(1-(n+1)ε)*real(sqrt(apd[i,i]*apd[j,j]))
    end
end

# Fail if non-Hermitian
@test_throws ArgumentError cholfact(randn(5,5))
@test_throws ArgumentError cholfact(complex.(randn(5,5), randn(5,5)))
@test_throws ArgumentError Base.LinAlg.chol!(randn(5,5))
@test_throws ArgumentError Base.LinAlg.cholfact!(randn(5,5),:U,Val{false})
@test_throws ArgumentError Base.LinAlg.cholfact!(randn(5,5),:U,Val{true})
@test_throws ArgumentError cholfact(randn(5,5),:U,Val{false})

# Fail for non-BLAS element types
@test_throws ArgumentError cholfact!(Hermitian(rand(Float16, 5,5)), Val{true})

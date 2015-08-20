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
    a = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex(areal, aimg) : areal)
    a2 = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex(a2real, a2img) : a2real)
    apd  = a'*a                  # symmetric positive-definite
    ε = εa = eps(abs(float(one(eltya))))

    for eltyb in (Float32, Float64, Complex64, Complex128, Int)
        b = eltyb == Int ? rand(1:5, n, 2) : convert(Matrix{eltyb}, eltyb <: Complex ? complex(breal, bimg) : breal)
        εb = eps(abs(float(one(eltyb))))
        ε = max(εa,εb)

debug && println("\ntype of a: ", eltya, " type of b: ", eltyb, "\n")

debug && println("(Automatic) upper Cholesky factor")

        @inferred cholfact(apd)
        @inferred chol(apd)
        capd  = factorize(apd)
        r     = capd[:U]
        κ     = cond(apd, 1) #condition number

        #getindex
        @test_throws KeyError capd[:Z]

        #Test error bound on reconstruction of matrix: LAWNS 14, Lemma 2.1
        E = abs(apd - r'*r)
        for i=1:n, j=1:n
            @test E[i,j] <= (n+1)ε/(1-(n+1)ε)*real(sqrt(apd[i,i]*apd[j,j]))
        end
        E = abs(apd - full(capd))
        for i=1:n, j=1:n
            @test E[i,j] <= (n+1)ε/(1-(n+1)ε)*real(sqrt(apd[i,i]*apd[j,j]))
        end

        #Test error bound on linear solver: LAWNS 14, Theorem 2.1
        #This is a surprisingly loose bound...
        x = capd\b
        @test norm(x-apd\b,1)/norm(x,1) <= (3n^2 + n + n^3*ε)*ε/(1-(n+1)*ε)*κ
        @test norm(apd*x-b,1)/norm(b,1) <= (3n^2 + n + n^3*ε)*ε/(1-(n+1)*ε)*κ

        @test_approx_eq apd * inv(capd) eye(n)
        @test norm(a*(capd\(a'*b)) - b,1)/norm(b,1) <= ε*κ*n # Ad hoc, revisit
        @test abs((det(capd) - det(apd))/det(capd)) <= ε*κ*n # Ad hoc, but statistically verified, revisit
        @test_approx_eq @inferred(logdet(capd)) log(det(capd)) # logdet is less likely to overflow

        apos = apd[1,1]            # test chol(x::Number), needs x>0
        @test_approx_eq cholfact(apos).factors √apos
        @test_throws ArgumentError chol(-one(eltya))

        # test chol of 2x2 Strang matrix
        S = convert(AbstractMatrix{eltya},full(SymTridiagonal([2,2],[-1])))
        U = Bidiagonal([2,sqrt(eltya(3))],[-1],true) / sqrt(eltya(2))
        @test_approx_eq full(chol(S)) full(U)

debug && println("lower Cholesky factor")
        lapd = cholfact(apd, :L)
        @test_approx_eq full(lapd) apd
        l = lapd[:L]
        @test_approx_eq l*l' apd

debug && println("pivoted Choleksy decomposition")
        if eltya != BigFloat && eltyb != BigFloat # Note! Need to implement pivoted cholesky decomposition in julia
            cpapd = cholfact(apd, :U, Val{true})
            @test rank(cpapd) == n
            @test all(diff(diag(real(cpapd.factors))).<=0.) # diagonal should be non-increasing
            @test norm(apd * (cpapd\b) - b)/norm(b) <= ε*κ*n # Ad hoc, revisit
            if isreal(apd)
                @test_approx_eq apd * inv(cpapd) eye(n)
            end
            @test_approx_eq full(cpapd) apd

            #getindex
            @test_throws KeyError cpapd[:Z]
        end
    end
end

begin
    # Cholesky factor of Matrix with non-commutative elements, here 2x2-matrices

    X = Matrix{Float64}[0.1*rand(2,2) for i in 1:3, j = 1:3]
    L = full(Base.LinAlg.chol!(X*X', Val{:L}))
    U = full(Base.LinAlg.chol!(X*X', Val{:U}))
    XX = full(X*X')

    @test sum(sum(norm, L*L' - XX)) < eps()
    @test sum(sum(norm, U'*U - XX)) < eps()
end

# Test generic cholfact!
for elty in (Float32, Float64, Complex{Float32}, Complex{Float64})
    if elty <: Complex
        A = complex(randn(5,5), randn(5,5))
    else
        A = randn(5,5)
    end
    A = convert(Matrix{elty}, A'A)
    for ul in (:U, :L)
        @test_approx_eq full(cholfact(A, ul)[ul]) full(invoke(Base.LinAlg.chol!, Tuple{AbstractMatrix, Type{Val{ul}}},copy(A), Val{ul}))
    end
end

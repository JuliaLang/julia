using Base.Test

debug = false
n = 10 #Size of test matrix
srand(1)

debug && println("Bidiagonal matrices")
for relty in (Float32, Float64, BigFloat), elty in (relty, Complex{relty})
    debug && println("elty is $(elty), relty is $(relty)")
    dv = convert(Vector{elty}, randn(n))
    ev = convert(Vector{elty}, randn(n-1))
    b = convert(Matrix{elty}, randn(n, 2))
    if (elty <: Complex)
        dv += im*convert(Vector{elty}, randn(n))
        ev += im*convert(Vector{elty}, randn(n-1))
        b += im*convert(Matrix{elty}, randn(n, 2))
    end

    debug && println("Test upper and lower bidiagonal matrices")
    for isupper in (true, false)
        debug && println("isupper is: $(isupper)")
        T = Bidiagonal(dv, ev, isupper)

        @test size(T, 1) == size(T, 2) == n
        @test size(T) == (n, n)
        @test full(T) == diagm(dv) + diagm(ev, isupper?1:-1)
        @test Bidiagonal(full(T), isupper) == T
        z = zeros(elty, n)

        debug && println("Idempotent tests")
        for func in (conj, transpose, ctranspose)
            @test func(func(T)) == T
        end

        debug && println("Linear solver")
        Tfull = full(T)
        condT = cond(map(Complex128,Tfull))
        x = T \ b
        tx = Tfull \ b
        @test norm(x-tx,Inf) <= 4*condT*max(eps()*norm(tx,Inf), eps(relty)*norm(x,Inf))

        debug && println("Eigensystems")
        d1, v1 = eig(T)
        d2, v2 = eig(map(elty<:Complex ? Complex128 : Float64,Tfull))
        @test_approx_eq isupper?d1:reverse(d1) d2
        if elty <: Real
            Test.test_approx_eq_modphase(v1, isupper?v2:v2[:,n:-1:1])
        end

        debug && println("Singular systems")
        if (elty <: Base.LinAlg.BlasReal)
            @test_approx_eq svdvals(Tfull) svdvals(T)
            u1, d1, v1 = svd(Tfull)
            u2, d2, v2 = svd(T)
            @test_approx_eq d1 d2
            if elty <: Real
                Test.test_approx_eq_modphase(u1, u2)
                Test.test_approx_eq_modphase(v1, v2)
            end
            @test_approx_eq_eps 0 vecnorm(u2*diagm(d2)*v2'-Tfull) n*max(n^2*eps(relty), vecnorm(u1*diagm(d1)*v1' - Tfull))
            @inferred svdvals(T)
            @inferred svd(T)
        end

        debug && println("Binary operations")
        for isupper2 in (true, false)
            dv = convert(Vector{elty}, randn(n))
            ev = convert(Vector{elty}, randn(n-1))
            T2 = Bidiagonal(dv, ev, isupper2)
            Tfull2 = full(T2)
            for op in (+, -, *)
                @test_approx_eq full(op(T, T2)) op(Tfull, Tfull2)
            end
        end

        debug && println("Inverse")
        @test_approx_eq inv(T)*Tfull eye(n)
    end
end

#Issue 10742
let A = Bidiagonal([1,2,3], [0,0], true)
    @test istril(A)
end


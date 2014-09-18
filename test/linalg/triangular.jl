debug = false
using Base.Test
using Base.LinAlg: BlasFloat, naivesub!

debug && println("Triangular matrices")
n = 11
for relty in (Float32, Float64, BigFloat), elty in (relty, Complex{relty})

    debug && println("elty is $(elty), relty is $(relty)")
    A = convert(Matrix{elty}, randn(n, n))
    b = convert(Matrix{elty}, randn(n, 2))
    if elty <: Complex
        A += im*convert(Matrix{elty}, randn(n, n))
        b += im*convert(Matrix{elty}, randn(n, 2))
    end

    for M1 in (Triangular(A, :U), Triangular(A, :L), Triangular(A, :U, true), Triangular(A, :L, true))
    	debug && println("M1 is of $(typeof(M1))")
    	for M2 in (Triangular(A, :U), Triangular(A, :L), Triangular(A, :U, true), Triangular(A, :L, true))
    		debug && println("M2 is of $(typeof(M2))")
    		@test full(M1 + M2) == full(M1) + full(M2)
    		@test full(M1 - M2) == full(M1) - full(M2)
    	end
    	α = randn()
    	@test full(α*M1) == full(M1*α)
    	@test full(M1*α) == full(M1)*α
    	@test full(α\M1) == full(M1/α)
    	@test full(M1/α) == full(M1)/α
    end

    for (M, TM) in ((triu(A), Triangular(A, :U)), 
    				(tril(A), Triangular(A, :L)))
        
        ##Idempotent tests #XXX - not implemented
        #for func in (conj, transpose, ctranspose)
        #    @test full(func(func(TM))) == M
        #end

        debug && println("Linear solver")
        x = M \ b
        tx = TM \ b
        condM = elty <:BlasFloat ? cond(TM, Inf) : convert(relty, cond(complex128(M), Inf))
        @test norm(x-tx,Inf) <= 4*condM*max(eps()*norm(tx,Inf), eps(relty)*norm(x,Inf))
        if elty <: BlasFloat #test naivesub! against LAPACK
            tx = [naivesub!(TM, b[:,1]) naivesub!(TM, b[:,2])]
            @test norm(x-tx,Inf) <= 4*condM*max(eps()*norm(tx,Inf), eps(relty)*norm(x,Inf))
        end

        debug && println("Eigensystems")
        vals1, vecs1 = eig(complex128(M))
        vals2, vecs2 = eig(TM)
        res1=norm(complex128(vecs1*diagm(vals1)*inv(vecs1) - M))
        res2=norm(complex128(vecs2*diagm(vals2)*inv(vecs2) - full(TM)))
        @test_approx_eq_eps res1 res2 res1+res2

        if elty <:BlasFloat
            debug && println("Condition number tests - can be VERY approximate")
            for p in [1.0, Inf]
                @test_approx_eq_eps cond(TM, p) cond(M, p) (cond(TM,p)+cond(M,p))
            end
        end

        debug && println("Binary operations")
        B = convert(Matrix{elty}, randn(n, n))
        for (M2, TM2) in ((triu(B), Triangular(B, :U)), (tril(B), Triangular(B, :L)))
            for op in (*, +, -)
                @test_approx_eq full(op(TM, TM2)) op(M, M2)
                @test_approx_eq full(op(TM, M2)) op(M, M2)
                @test_approx_eq full(op(M, TM2)) op(M, M2)
            end
            @test M2*0.5 == full(TM2*0.5)
            @test 0.5*M2 == full(0.5*TM2)
            @test M2/0.5 == full(TM2/0.5)
            @test 0.5\M2 == full(0.5\TM2)
        end
    end
end
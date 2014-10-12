debug = false
using Base.Test
using Base.LinAlg: BlasFloat, errorbounds, naivesub!

debug && println("Triangular matrices")

n = 20
srand(123)

# A = rand(n, n)

Areal   = randn(n, n)/2
Aimg    = randn(n, n)/2
A2real  = randn(n, n)/2
A2img   = randn(n, n)/2

for eltya in (Float32, Float64, Complex64, Complex128, BigFloat, Int)
    A = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex(Areal, Aimg) : Areal)
    # a2 = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex(a2real, a2img) : a2real)
    εa = eps(abs(float(one(eltya))))

    for eltyb in (Float32, Float64, Complex64, Complex128)
        εb = eps(abs(float(one(eltyb))))
        ε = max(εa,εb)

        debug && println("\ntype of A: ", eltya, " type of b: ", eltyb, "\n")
    
        debug && println("Solve upper triangular system")
        Atri = Triangular(lufact(A)[:U], :U) |> t -> eltya <: Complex && eltyb <: Real ? real(t) : t # Here the triangular matrix can't be too badly conditioned
        b = convert(Matrix{eltyb}, eltya <: Complex ? full(Atri)*ones(n, 2) : full(Atri)*ones(n, 2))
        x = full(Atri) \ b
    
        debug && println("Test error estimates")
        if eltya != BigFloat && eltyb != BigFloat
            for i = 1:2
                @test  norm(x[:,1] .- 1) <= errorbounds(Triangular(A, :U), x, b)[1][i]
            end
        end
        debug && println("Test forward error [JIN 5705] if this is not a BigFloat")
        
        x = Atri \ b
        γ = n*ε/(1 - n*ε)
        if eltya != BigFloat
            bigA = big(Atri)
            x̂ = ones(n, 2)
            for i = 1:size(b, 2)
                @test norm(x̂[:,i] - x[:,i], Inf)/norm(x̂[:,i], Inf) <= condskeel(bigA, x̂[:,i])*γ/(1 - condskeel(bigA)*γ)
            end
        end
        
        debug && println("Test backward error [JIN 5705]")
        for i = 1:size(b, 2)
            @test norm(abs(b[:,i] - Atri*x[:,i]), Inf) <= γ * norm(Atri, Inf) * norm(x[:,i], Inf)
        end
    
        debug && println("Solve lower triangular system")
        Atri = Triangular(lufact(A)[:U], :U) |> t -> eltya <: Complex && eltyb <: Real ? real(t) : t # Here the triangular matrix can't be too badly conditioned
        b = convert(Matrix{eltyb}, eltya <: Complex ? full(Atri)*ones(n, 2) : full(Atri)*ones(n, 2))
        x = full(Atri)\b
    
        debug && println("Test error estimates")
        if eltya != BigFloat && eltyb != BigFloat
            for i = 1:2
                @test  norm(x[:,1] .- 1) <= errorbounds(Triangular(A, :U), x, b)[1][i]
            end
        end

        debug && println("Test forward error [JIN 5705] if this is not a BigFloat")
        b = eltyb == Int ? itrunc(Atri*ones(n, 2)) : convert(Matrix{eltyb}, Atri*ones(eltya, n, 2))
        x = Atri \ b
        γ = n*ε/(1 - n*ε)
        if eltya != BigFloat
            bigA = big(Atri)
            x̂ = ones(n, 2)
            for i = 1:size(b, 2)
                @test norm(x̂[:,i] - x[:,i], Inf)/norm(x̂[:,i], Inf) <= condskeel(bigA, x̂[:,i])*γ/(1 - condskeel(bigA)*γ)
            end
        end
    
        debug && println("Test backward error [JIN 5705]")
        for i = 1:size(b, 2)
            @test norm(abs(b[:,i] - Atri*x[:,i]), Inf) <= γ * norm(Atri, Inf) * norm(x[:,i], Inf)
        end
    end
end

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

        debug && println("Linear solver")
        x = M \ b
        tx = TM \ b
        condM = elty <:BlasFloat ? cond(TM, Inf) : convert(relty, cond(complex128(M), Inf))
        @test norm(x - tx,Inf) <= 4*condM*max(eps()*norm(tx,Inf), eps(relty)*norm(x,Inf))
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

@test_throws DimensionMismatch Triangular(randn(5, 4), :L)


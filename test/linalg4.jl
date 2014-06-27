import Base.LinAlg
import Base.LinAlg: BlasComplex, BlasFloat, BlasReal

srand(1)

#Test equivalence of eigenvectors/singular vectors taking into account possible phase (sign) differences
function test_approx_eq_vecs{S<:Real,T<:Real}(a::StridedVecOrMat{S}, b::StridedVecOrMat{T}, error=nothing)
    n = size(a, 1)
    @test n==size(b,1) && size(a,2)==size(b,2)
    error==nothing && (error=n^3*(eps(S)+eps(T)))
    for i=1:n
        ev1, ev2 = a[:,i], b[:,i]
        deviation = min(abs(norm(ev1-ev2)),abs(norm(ev1+ev2)))
        if !isnan(deviation)
            @test_approx_eq_eps deviation 0.0 error
        end
    end
end

##############################
# Tests for special matrices #
##############################

n=12 #Size of matrix problem to test

#Triangular matrices
for relty in (Float32, Float64, BigFloat), elty in (relty, Complex{relty})
    A = convert(Matrix{elty}, randn(n, n))
    b = convert(Matrix{elty}, randn(n, 2))
    if elty <: Complex
        A += im*convert(Matrix{elty}, randn(n, n))
        b += im*convert(Matrix{elty}, randn(n, 2))
    end

    for (M, TM) in ((triu(A), Triangular(A, :U)), (tril(A), Triangular(A, :L)))
        
        ##Idempotent tests #XXX - not implemented
        #for func in (conj, transpose, ctranspose)
        #    @test full(func(func(TM))) == M
        #end

        #Linear solver
        x = M \ b
        tx = TM \ b
        condM = elty <:BlasFloat ? cond(TM, Inf) : convert(relty, cond(complex128(M), Inf))
        @test norm(x-tx,Inf) <= 4*condM*max(eps()*norm(tx,Inf), eps(relty)*norm(x,Inf))
        if elty <: BlasFloat #test naivesub! against LAPACK
            tx = [LinAlg.naivesub!(TM, b[:,1]) LinAlg.naivesub!(TM, b[:,2])]
            @test norm(x-tx,Inf) <= 4*condM*max(eps()*norm(tx,Inf), eps(relty)*norm(x,Inf))
        end

        #Eigensystems
        vals1, vecs1 = eig(complex128(M))
        vals2, vecs2 = eig(TM)
        res1=norm(complex128(vecs1*diagm(vals1)*inv(vecs1) - M))
        res2=norm(complex128(vecs2*diagm(vals2)*inv(vecs2) - full(TM)))
        @test_approx_eq_eps res1 res2 res1+res2

        if elty <:BlasFloat
            #Condition number tests - can be VERY approximate
            for p in [1.0, Inf]
                @test_approx_eq_eps cond(TM, p) cond(M, p) (cond(TM,p)+cond(M,p))
            end
        end

        #Binary operations
        B = convert(Matrix{elty}, randn(n, n))
        for (M2, TM2) in ((triu(B), Triangular(B, :U)), (tril(B), Triangular(B, :L)))
            for op in (*, +, -)
                @test_approx_eq full(op(TM, TM2)) op(M, M2)
                @test_approx_eq full(op(TM, M2)) op(M, M2)
                @test_approx_eq full(op(M, TM2)) op(M, M2)
            end
        end
    end
end

#Tridiagonal matrices
for relty in (Float32, Float64), elty in (relty, Complex{relty})
    a = convert(Vector{elty}, randn(n-1))
    b = convert(Vector{elty}, randn(n))
    c = convert(Vector{elty}, randn(n-1))
    if elty <: Complex
        a += im*convert(Vector{elty}, randn(n-1))
        b += im*convert(Vector{elty}, randn(n))
        c += im*convert(Vector{elty}, randn(n-1))
    end

    A=Tridiagonal(a, b, c)
    fA=(elty<:Complex?complex128:float64)(full(A))

    #Simple unary functions
    for func in (det, inv)
        @test_approx_eq_eps func(A) func(fA) n^2*sqrt(eps(relty))
    end

    #Binary operations
    a = convert(Vector{elty}, randn(n-1))
    b = convert(Vector{elty}, randn(n))
    c = convert(Vector{elty}, randn(n-1))
    if elty <: Complex
        a += im*convert(Vector{elty}, randn(n-1))
        b += im*convert(Vector{elty}, randn(n))
        c += im*convert(Vector{elty}, randn(n-1))
    end

    B=Tridiagonal(a, b, c)
    fB=(elty<:Complex?complex128:float64)(full(B))

    for op in (+, -, *)
        @test_approx_eq full(op(A, B)) op(fA, fB)
    end
end

#SymTridiagonal (symmetric tridiagonal) matrices
for relty in (Float32, Float64), elty in (relty, )#XXX Complex{relty}) doesn't work
    a = convert(Vector{elty}, randn(n))
    b = convert(Vector{elty}, randn(n-1))
    if elty <: Complex
        a += im*convert(Vector{elty}, randn(n))
        b += im*convert(Vector{elty}, randn(n-1))
    end

    A=SymTridiagonal(a, b)
    fA=(elty<:Complex?complex128:float64)(full(A))
    
    #Idempotent tests
    for func in (conj, transpose, ctranspose)
        @test func(func(A)) == A
    end

    #Simple unary functions
    for func in (det, inv)
        @test_approx_eq_eps func(A) func(fA) n^2*sqrt(eps(relty))
    end

    #Eigensystems
    zero, infinity = convert(elty, 0), convert(elty, Inf)
    #This tests eigenvalue and eigenvector computations using stebz! and stein!
    w, iblock, isplit = LinAlg.LAPACK.stebz!('V','B',-infinity,infinity,0,0,zero,a,b)
    evecs = LinAlg.LAPACK.stein!(a,b,w)

    (e, v)=eig(SymTridiagonal(a,b))
    @test_approx_eq e w
    test_approx_eq_vecs(v, evecs)

    #stein! call using iblock and isplit
    w, iblock, isplit = LinAlg.LAPACK.stebz!('V','B',-infinity,infinity,0,0,zero,a,b)
    evecs = LinAlg.LAPACK.stein!(a,b,w,iblock,isplit)
    test_approx_eq_vecs(v, evecs)

    #Binary operations
    a = convert(Vector{elty}, randn(n))
    b = convert(Vector{elty}, randn(n-1))
    if elty <: Complex
        a += im*convert(Vector{elty}, randn(n-1))
        b += im*convert(Vector{elty}, randn(n))
    end

    B=SymTridiagonal(a, b)
    fB=(elty<:Complex?complex128:float64)(full(B))

    for op in (+, -, *)
        @test_approx_eq full(op(A, B)) op(fA, fB)
    end
end

#Bidiagonal matrices
for relty in (Float32, Float64, BigFloat), elty in (relty, Complex{relty})
    dv = convert(Vector{elty}, randn(n))
    ev = convert(Vector{elty}, randn(n-1))
    b = convert(Matrix{elty}, randn(n, 2))
    if (elty <: Complex)
        dv += im*convert(Vector{elty}, randn(n))
        ev += im*convert(Vector{elty}, randn(n-1))
        b += im*convert(Matrix{elty}, randn(n, 2))
    end

    for isupper in (true, false) #Test upper and lower bidiagonal matrices
        T = Bidiagonal(dv, ev, isupper)

        @test size(T, 1) == size(T, 2) == n
        @test size(T) == (n, n)
        @test full(T) == diagm(dv) + diagm(ev, isupper?1:-1)
        @test Bidiagonal(full(T), isupper) == T
        z = zeros(elty, n)

        #Idempotent tests
        for func in (conj, transpose, ctranspose)
            @test func(func(T)) == T
        end

        #Linear solver
        Tfull = full(T)
        condT = cond(complex128(Tfull))
        x = T \ b
        tx = Tfull \ b
        @test norm(x-tx,Inf) <= 4*condT*max(eps()*norm(tx,Inf), eps(relty)*norm(x,Inf))

        #Eigensystems
        d1, v1 = eig(T)
        d2, v2 = eig((elty<:Complex?complex128:float64)(Tfull))
        @test_approx_eq isupper?d1:reverse(d1) d2
        if elty <: Real
            test_approx_eq_vecs(v1, isupper?v2:v2[:,n:-1:1])
        end

        #Singular systems
        if (elty <: BlasReal)
            @test_approx_eq svdvals(Tfull) svdvals(T)
            u1, d1, v1 = svd(Tfull)
            u2, d2, v2 = svd(T)
            @test_approx_eq d1 d2
            if elty <: Real
                test_approx_eq_vecs(u1, u2)
                test_approx_eq_vecs(v1, v2)
            end
            @test_approx_eq_eps 0 vecnorm(u2*diagm(d2)*v2'-Tfull) n*max(n^2*eps(relty), vecnorm(u1*diagm(d1)*v1'-Tfull))
        end

        #Binary operations
        for isupper2 in (true, false) 
            dv = convert(Vector{elty}, randn(n))
            ev = convert(Vector{elty}, randn(n-1))
            T2 = Bidiagonal(dv, ev, isupper2)
            Tfull2 = full(T2)
            for op in (+, -, *)
                @test_approx_eq full(op(T, T2)) op(Tfull, Tfull2)
            end
        end
    end
end

#Diagonal matrices
for relty in (Float32, Float64, BigFloat), elty in (relty, Complex{relty})
    d=convert(Vector{elty}, randn(n))
    v=convert(Vector{elty}, randn(n))
    U=convert(Matrix{elty}, randn(n,n))
    if elty <: Complex
        d+=im*convert(Vector{elty}, randn(n))
        v+=im*convert(Vector{elty}, randn(n))
        U+=im*convert(Matrix{elty}, randn(n,n))
    end
    D = Diagonal(d)
    DM = diagm(d)

    #Linear solve
    @test_approx_eq_eps D*v DM*v n*eps(relty)*(elty<:Complex ? 2:1)
    @test_approx_eq_eps D*U DM*U n^2*eps(relty)*(elty<:Complex ? 2:1)
    if relty != BigFloat
        @test_approx_eq_eps D\v DM\v 2n^2*eps(relty)*(elty<:Complex ? 2:1)
        @test_approx_eq_eps D\U DM\U 2n^3*eps(relty)*(elty<:Complex ? 2:1)
    end

    #Simple unary functions
    for func in (det, trace)
        @test_approx_eq_eps func(D) func(DM) n^2*eps(relty)*(elty<:Complex ? 2:1)
    end
    if relty <: BlasFloat
        for func in (expm,)
            @test_approx_eq_eps func(D) func(DM) n^3*eps(relty)
        end
    end
    if elty <: BlasComplex
        for func in (logdet, sqrtm)
            @test_approx_eq_eps func(D) func(DM) n^2*eps(relty)*2
        end
    end
    #Binary operations
    d = convert(Vector{elty}, randn(n))
    D2 = Diagonal(d)
    DM2= diagm(d)
    for op in (+, -, *)
        @test_approx_eq full(op(D, D2)) op(DM, DM2)
    end
end


#Test interconversion between special matrix types
a=[1.0:n]
A=Diagonal(a)
for newtype in [Diagonal, Bidiagonal, SymTridiagonal, Tridiagonal, Triangular, Matrix]
    @test full(convert(newtype, A)) == full(A)
end

for isupper in (true, false)
    A=Bidiagonal(a, [1.0:n-1], isupper)
    for newtype in [Bidiagonal, Tridiagonal, Triangular, Matrix]
        @test full(convert(newtype, A)) == full(A)
    end
    A=Bidiagonal(a, zeros(n-1), isupper) #morally Diagonal
    for newtype in [Diagonal, Bidiagonal, SymTridiagonal, Tridiagonal, Triangular, Matrix]
        @test full(convert(newtype, A)) == full(A)
    end
end

A=SymTridiagonal(a, [1.0:n-1])
for newtype in [Tridiagonal, Matrix]
    @test full(convert(newtype, A)) == full(A)
end

A=Tridiagonal(zeros(n-1), [1.0:n], zeros(n-1)) #morally Diagonal
for newtype in [Diagonal, Bidiagonal, SymTridiagonal, Matrix]
    @test full(convert(newtype, A)) == full(A)
end

A=Triangular(full(Diagonal(a)), :L) #morally Diagonal
for newtype in [Diagonal, Bidiagonal, SymTridiagonal, Triangular, Matrix]
    @test full(convert(newtype, A)) == full(A)
end



debug = false

import Base.LinAlg
import Base.LinAlg: BlasComplex, BlasFloat, BlasReal


# basic tridiagonal operations
n = 5

srand(123)

d = 1 + rand(n)
dl = -rand(n-1)
du = -rand(n-1)
v = randn(n)
B = randn(n,2)

# Woodbury
U = randn(n,2)
V = randn(2,n)
C = randn(2,2)

for elty in (Float32, Float64, Complex64, Complex128, Int)
    if elty == Int
        srand(61516384)
        d = rand(1:100, n)
        dl = -rand(0:10, n-1)
        du = -rand(0:10, n-1)
        v = rand(1:100, n)
        B = rand(1:100, n, 2)

        # Woodbury
        U = rand(1:100, n, 2)
        V = rand(1:100, 2, n)
        C = rand(1:100, 2, 2)
    else 
        d = convert(Vector{elty}, d)
        dl = convert(Vector{elty}, dl)
        du = convert(Vector{elty}, du)
        v = convert(Vector{elty}, v)
        B = convert(Matrix{elty}, B)
        U = convert(Matrix{elty}, U)
        V = convert(Matrix{elty}, V)
        C = convert(Matrix{elty}, C)
    end
    T = Tridiagonal(dl, d, du)
    @test size(T, 1) == n
    @test size(T) == (n, n)
    F = diagm(d)
    for i = 1:n-1
        F[i,i+1] = du[i]
        F[i+1,i] = dl[i]
    end
    @test full(T) == F

    # elementary operations on tridiagonals
    @test conj(T) == Tridiagonal(conj(dl), conj(d), conj(du))
    @test transpose(T) == Tridiagonal(du, d, dl)
    @test ctranspose(T) == Tridiagonal(conj(du), conj(d), conj(dl))

    # test interconversion of Tridiagonal and SymTridiagonal
    @test Tridiagonal(dl, d, dl) == SymTridiagonal(d, dl)
    @test Tridiagonal(dl, d, du) + Tridiagonal(du, d, dl) == SymTridiagonal(2d, dl+du)
    @test SymTridiagonal(d, dl) + Tridiagonal(du, d, du) == SymTridiagonal(2d, dl+du)

    # tridiagonal linear algebra
    @test_approx_eq T*v F*v
    invFv = F\v
    @test_approx_eq T\v invFv
    @test_approx_eq Base.solve(T,v) invFv
    @test_approx_eq Base.solve(T, B) F\B
    Tlu = factorize(T)
    x = Tlu\v
    @test_approx_eq x invFv
    @test_approx_eq det(T) det(F)

    # symmetric tridiagonal
    Ts = SymTridiagonal(d, dl)
    Fs = full(Ts)
    invFsv = Fs\v
    Tldlt = Base.ldltd(Ts)
    x = Tldlt\v
    @test_approx_eq x invFsv

    # eigenvalues/eigenvectors of symmetric tridiagonal
    if elty === Float32 || elty === Float64
        DT, VT = eig(Ts)
        D, Vecs = eig(Fs)
        @test_approx_eq DT D
        @test_approx_eq abs(VT'Vecs) eye(elty, n)
    end

    # Woodbury
    W = Woodbury(T, U, C, V)
    F = full(W)
    @test_approx_eq W*v F*v
    iFv = F\v
    @test_approx_eq W\v iFv
    @test_approx_eq det(W) det(F)
    iWv = similar(iFv)
    if elty != Int
        Base.LinAlg.solve!(iWv, W, v)
        @test_approx_eq iWv iFv
    end

    # Test det(A::Matrix)
    # In the long run, these tests should step through Strang's
    #  axiomatic definition of determinants.
    # If all axioms are satisfied and all the composition rules work,
    #  all determinants will be correct except for floating point errors.
    
    # The determinant of the identity matrix should always be 1.
    for i = 1:10
        A = eye(elty, i)
        @test_approx_eq det(A) one(elty)
    end

    # The determinant of a Householder reflection matrix should always be -1.
    for i = 1:10
        A = eye(elty, 10)
        A[i, i] = -one(elty)
        @test_approx_eq det(A) -one(elty)
    end

    # The determinant of a rotation matrix should always be 1.
    if elty != Int
        for theta = convert(Vector{elty}, pi ./ [1:4])
            R = [cos(theta) -sin(theta);
                 sin(theta) cos(theta)]
            @test_approx_eq convert(elty, det(R)) one(elty)
        end

    # issue 1490
    @test_approx_eq_eps det(ones(elty, 3,3)) zero(elty) 3*eps(real(one(elty)))
    end
end

# Generic BLAS tests
srand(100)
# syr2k! and her2k!
for elty in (Float32, Float64, Complex64, Complex128)
    U = randn(5,2)
    V = randn(5,2)
    if elty == Complex64 || elty == Complex128
        U = complex(U, U)
        V = complex(V, V)
    end
    U = convert(Array{elty, 2}, U)
    V = convert(Array{elty, 2}, V)
    @test_approx_eq tril(LinAlg.BLAS.syr2k('L','N',U,V)) tril(U*V.' + V*U.')
    @test_approx_eq triu(LinAlg.BLAS.syr2k('U','N',U,V)) triu(U*V.' + V*U.')
    @test_approx_eq tril(LinAlg.BLAS.syr2k('L','T',U,V)) tril(U.'*V + V.'*U)
    @test_approx_eq triu(LinAlg.BLAS.syr2k('U','T',U,V)) triu(U.'*V + V.'*U)        
end

for elty in (Complex64, Complex128)
    U = randn(5,2)
    V = randn(5,2)
    if elty == Complex64 || elty == Complex128
        U = complex(U, U)
        V = complex(V, V)
    end
    U = convert(Array{elty, 2}, U)
    V = convert(Array{elty, 2}, V)
    @test_approx_eq tril(LinAlg.BLAS.her2k('L','N',U,V)) tril(U*V' + V*U')
    @test_approx_eq triu(LinAlg.BLAS.her2k('U','N',U,V)) triu(U*V' + V*U')
    @test_approx_eq tril(LinAlg.BLAS.her2k('L','C',U,V)) tril(U'*V + V'*U)
    @test_approx_eq triu(LinAlg.BLAS.her2k('U','C',U,V)) triu(U'*V + V'*U)        
end

# LAPACK tests
srand(123)
Ainit = randn(5,5)
for elty in (Float32, Float64, Complex64, Complex128)
    # syevr!
    if elty == Complex64 || elty == Complex128
        A = complex(Ainit, Ainit)
    else
        A = Ainit
    end
    A = convert(Array{elty, 2}, A)
    Asym = A'A
    vals, Z = LinAlg.LAPACK.syevr!('V', copy(Asym))
    @test_approx_eq Z*scale(vals, Z') Asym
    @test all(vals .> 0.0)
    @test_approx_eq LinAlg.LAPACK.syevr!('N','V','U',copy(Asym),0.0,1.0,4,5,-1.0)[1] vals[vals .< 1.0]
    @test_approx_eq LinAlg.LAPACK.syevr!('N','I','U',copy(Asym),0.0,1.0,4,5,-1.0)[1] vals[4:5]
    @test_approx_eq vals LinAlg.LAPACK.syev!('N','U',copy(Asym))
end

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

#Triangular matrices
n=12
for relty in (Float16, Float32, Float64, BigFloat), elty in (relty, Complex{relty})
    A = convert(Matrix{elty}, randn(n, n))
    b = convert(Matrix{elty}, randn(n, 2))
    if elty <: Complex
        A += im*convert(Matrix{elty}, randn(n, n))
        b += im*convert(Matrix{elty}, randn(n, 2))
    end

    for M in (triu(A), tril(A))
        TM = Triangular(M)
        condM = elty <:BlasFloat ? cond(TM, Inf) : convert(relty, cond(complex128(M), Inf))
        #Linear solver
        x = M \ b
        tx = TM \ b
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
                @test_approx_eq_eps cond(TM, p) cond(M, p) (cond(TM,p)+cond(M,p))*0.2
            end
        end
    end
end

#Tridiagonal matrices
for relty in (Float16, Float32, Float64), elty in (relty, Complex{relty})
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
    for func in (det, inv)
        @test_approx_eq_eps func(A) func(fA) n^2*sqrt(eps(relty))
    end
end

#SymTridiagonal (symmetric tridiagonal) matrices
for relty in (Float16, Float32, Float64), elty in (relty, )#XXX Complex{relty}) doesn't work
    a = convert(Vector{elty}, randn(n))
    b = convert(Vector{elty}, randn(n-1))
    if elty <: Complex
        relty==Float16 && continue
        a += im*convert(Vector{elty}, randn(n))
        b += im*convert(Vector{elty}, randn(n-1))
    end

    A=SymTridiagonal(a, b)
    fA=(elty<:Complex?complex128:float64)(full(A))
    for func in (det, inv)
        @test_approx_eq_eps func(A) func(fA) n^2*sqrt(eps(relty))
    end
end

Ainit = randn(n)
Binit = randn(n-1)
for elty in (Float32, Float64)
    A = convert(Array{elty, 1}, Ainit)
    B = convert(Array{elty, 1}, Binit)
    zero, infinity = convert(elty, 0), convert(elty, Inf)
    #This tests eigenvalue and eigenvector computations using stebz! and stein!
    w, iblock, isplit = LinAlg.LAPACK.stebz!('V','B',-infinity,infinity,0,0,zero,A,B) 
    evecs = LinAlg.LAPACK.stein!(A,B,w)
    
    (e, v)=eig(SymTridiagonal(A,B))
    @test_approx_eq e w
    #Take into account possible phase (sign) difference in eigenvectors
    for i=1:n
        ev1 = v[:,i]
        ev2 = evecs[:,i]
        deviation = min(abs(norm(ev1-ev2)),abs(norm(ev1+ev2)))
        @test_approx_eq_eps deviation 0.0 n^2*eps(abs(convert(elty, 2.0)))
    end

    #Test stein! call using iblock and isplit
    w, iblock, isplit = LinAlg.LAPACK.stebz!('V','B',-infinity,infinity,0,0,zero,A,B) 
    evecs = LinAlg.LAPACK.stein!(A, B, w, iblock, isplit)
    test_approx_eq_vecs(v, evecs)
end

#Bidiagonal matrices
for relty in (Float16, Float32, Float64, BigFloat), elty in (relty, Complex{relty})
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

        # idempotent tests
        for func in (conj, transpose, ctranspose)
            @test func(func(T)) == T
        end

        #Linear solver
        Tfull = full(T)
        condT = cond(complex128(Tfull))
        x = T \ b
        tx = Tfull \ b
        @test norm(x-tx,Inf) <= 4*condT*max(eps()*norm(tx,Inf), eps(relty)*norm(x,Inf))
     
        #Test eigenvalues/vectors
        d1, v1 = eig(T)
        d2, v2 = eig((elty<:Complex?complex128:float64)(Tfull))
        @test_approx_eq isupper?d1:reverse(d1) d2
        if elty <: Real
            test_approx_eq_vecs(v1, isupper?v2:v2[:,n:-1:1])
        end

        if (elty <: BlasReal)
            #Test singular values/vectors
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
    end
end

#Diagonal matrices
n=12
for relty in (Float16, Float32, Float64, BigFloat), elty in (relty, Complex{relty})
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
    @test_approx_eq_eps D*v DM*v n*eps(relty)*(elty<:Complex ? 2:1)
    @test_approx_eq_eps D*U DM*U n^2*eps(relty)*(elty<:Complex ? 2:1)
    if relty != BigFloat 
        @test_approx_eq_eps D\v DM\v 2n^2*eps(relty)*(elty<:Complex ? 2:1)
        @test_approx_eq_eps D\U DM\U 2n^3*eps(relty)*(elty<:Complex ? 2:1)
    end
    for func in (det, trace)
        @test_approx_eq_eps func(D) func(DM) n^2*eps(relty)
    end
    if relty <: BlasFloat
        for func in (expm,)
            @test_approx_eq_eps func(D) func(DM) n^2*eps(relty)
        end
    end        
    if elty <: BlasComplex
        for func in (logdet, sqrtm)
            @test_approx_eq_eps func(D) func(DM) n^2*eps(relty)
        end
    end
end

#Test interconversion between special matrix types
N=12
A=Diagonal([1:N]*1.0)
for newtype in [Diagonal, Bidiagonal, SymTridiagonal, Tridiagonal, Triangular, Matrix]
    @test full(convert(newtype, A)) == full(A)
end

for isupper in (true, false)
    A=Bidiagonal([1:N]*1.0, [1:N-1]*1.0, isupper)
    for newtype in [Bidiagonal, Tridiagonal, Triangular, Matrix]
        @test full(convert(newtype, A)) == full(A)
    end
    A=Bidiagonal([1:N]*1.0, [1:N-1]*0.0, isupper) #morally Diagonal
    for newtype in [Diagonal, Bidiagonal, SymTridiagonal, Tridiagonal, Triangular, Matrix]
        @test full(convert(newtype, A)) == full(A)
    end
end

A=SymTridiagonal([1:N]*1.0, [1:N-1]*1.0)
for newtype in [Tridiagonal, Matrix]
    @test full(convert(newtype, A)) == full(A)
end

A=Tridiagonal([1:N-1]*0.0, [1:N]*1.0, [1:N-1]*0.0) #morally Diagonal
for newtype in [Diagonal, Bidiagonal, SymTridiagonal, Triangular, Matrix]
    @test full(convert(newtype, A)) == full(A)
end

A=Triangular(full(Diagonal([1:N]*1.0))) #morally Diagonal
for newtype in [Diagonal, Bidiagonal, SymTridiagonal, Triangular, Matrix]
    @test full(convert(newtype, A)) == full(A)
end

# Test gglse
for elty in (Float32, Float64, Complex64, Complex128)
    A = convert(Array{elty, 2}, [1 1 1 1; 1 3 1 1; 1 -1 3 1; 1 1 1 3; 1 1 1 -1])
    c = convert(Array{elty, 1}, [2, 1, 6, 3, 1])
    B = convert(Array{elty, 2}, [1 1 1 -1; 1 -1 1 1; 1 1 -1 1])
    d = convert(Array{elty, 1}, [1, 3, -1])
    @test_approx_eq LinAlg.LAPACK.gglse!(A, c, B, d)[1] convert(Array{elty}, [0.5, -0.5, 1.5, 0.5])
end

# Test givens rotations
for elty in (Float32, Float64, Complex64, Complex128)
    if elty <: Real
        A = convert(Matrix{elty}, randn(10,10))
    else
        A = convert(Matrix{elty}, complex(randn(10,10),randn(10,10)))
    end
    Ac = copy(A)
    R = Base.LinAlg.Rotation(Base.LinAlg.Givens{elty}[])
    for j = 1:8
        for i = j+2:10
            G = givens(A, j+1, i, j)
            A_mul_B!(G, A)
            A_mul_Bc!(A, G)
            A_mul_B!(G, R)
        end
    end
    @test_approx_eq abs(A) abs(hessfact(Ac)[:H])
    @test_approx_eq norm(R*eye(elty, 10)) one(elty)
end

# Test gradient
for elty in (Int32, Int64, Float32, Float64, Complex64, Complex128)
    if elty <: Real
        x = convert(Vector{elty}, [1:3])
        g = ones(elty, 3)
    else
        x = convert(Vector{elty}, complex([1:3],[1:3]))
        g = convert(Vector{elty}, complex(ones(3), ones(3)))
    end
    @test_approx_eq gradient(x) g
end

# Test our own linear algebra functionaly against LAPACK
for elty in (Float32, Float64, Complex{Float32}, Complex{Float64})
    for nn in (5,10,15)
        if elty <: Real
            A = convert(Matrix{elty}, randn(10,nn))
        else
            A = convert(Matrix{elty}, complex(randn(10,nn),randn(10,nn)))
        end    ## LU (only equal for real because LAPACK uses different absolute value when choosing permutations)
        if elty <: Real
            FJulia  = invoke(lufact!, (AbstractMatrix,), copy(A)) 
            FLAPACK = Base.LinAlg.LAPACK.getrf!(copy(A))
            @test_approx_eq FJulia.factors FLAPACK[1]
            @test_approx_eq FJulia.ipiv FLAPACK[2]
            @test_approx_eq FJulia.info FLAPACK[3]
        end
        
        ## QR
        FJulia  = invoke(qrfact!, (AbstractMatrix,), copy(A)) 
        FLAPACK = Base.LinAlg.LAPACK.geqrf!(copy(A))
        @test_approx_eq FJulia.factors FLAPACK[1]
        @test_approx_eq FJulia.τ FLAPACK[2]
    end
end

# Test rational matrices
## Integrate in general tests when more linear algebra is implemented in julia
a = convert(Matrix{Rational{BigInt}}, rand(1:10//1,n,n))/n
b = rand(1:10,n,2)
lua   = factorize(a)
l,u,p = lua[:L], lua[:U], lua[:p]
@test_approx_eq l*u a[p,:]
@test_approx_eq l[invperm(p),:]*u a
@test_approx_eq a * inv(lua) eye(n)
@test_approx_eq a*(lua\b) b
@test_approx_eq det(a) det(float64(float(a)))
## Hilbert Matrix (very ill conditioned)
## Testing Rational{BigInt} and BigFloat version
nHilbert = 50
H = Rational{BigInt}[1//(i+j-1) for i = 1:nHilbert,j = 1:nHilbert]
Hinv = Rational{BigInt}[(-1)^(i+j)*(i+j-1)*binomial(nHilbert+i-1,nHilbert-j)*binomial(nHilbert+j-1,nHilbert-i)*binomial(i+j-2,i-1)^2 for i = big(1):nHilbert,j=big(1):nHilbert]
@test inv(H) == Hinv
with_bigfloat_precision(2^10) do
    @test norm(float64(inv(float(H)) - float(Hinv))) < 1e-100
end

# Test balancing in eigenvector calculations
for elty in (Float32, Float64, Complex64, Complex128)
    A = convert(Matrix{elty}, [ 3.0     -2.0      -0.9     2*eps(real(one(elty)));
                               -2.0      4.0       1.0    -eps(real(one(elty)));
                               -eps(real(one(elty)))/4  eps(real(one(elty)))/2  -1.0     0;
                               -0.5     -0.5       0.1     1.0])
    F = eigfact(A,permute=false,scale=false)
    @test_approx_eq F[:vectors]*Diagonal(F[:values])/F[:vectors] A
    F = eigfact(A)
    # @test norm(F[:vectors]*Diagonal(F[:values])/F[:vectors] - A) > 0.01
end

# Tests norms
nnorm = 1000
mmat = 100
nmat = 80
for elty in (Float16, Float32, Float64, BigFloat, Complex{Float16}, Complex{Float32}, Complex{Float64}, Complex{BigFloat}, Int32, Int64, BigInt)
    debug && println(elty)

    ## Vector
    x = ones(elty,10)
    xs = sub(x,1:2:10)
    @test_approx_eq norm(x, -Inf) 1
    @test_approx_eq norm(x, -1) 1/10
    @test_approx_eq norm(x, 0) 10
    @test_approx_eq norm(x, 1) 10
    @test_approx_eq norm(x, 2) sqrt(10)
    @test_approx_eq norm(x, 3) cbrt(10)
    @test_approx_eq norm(x, Inf) 1
    @test_approx_eq norm(xs, -Inf) 1
    @test_approx_eq norm(xs, -1) 1/5
    @test_approx_eq norm(xs, 0) 5
    @test_approx_eq norm(xs, 1) 5
    @test_approx_eq norm(xs, 2) sqrt(5)
    @test_approx_eq norm(xs, 3) cbrt(5)
    @test_approx_eq norm(xs, Inf) 1

    ## Number
    norm(x[1:1]) === norm(x[1], -Inf)
    norm(x[1:1]) === norm(x[1], 0)
    norm(x[1:1]) === norm(x[1], 1)
    norm(x[1:1]) === norm(x[1], 2)
    norm(x[1:1]) === norm(x[1], Inf)

    for i = 1:10    
        x = elty <: Integer ? convert(Vector{elty}, rand(1:10, nnorm)) : 
            elty <: Complex ? convert(Vector{elty}, complex(randn(nnorm), randn(nnorm))) : 
            convert(Vector{elty}, randn(nnorm))
        xs = sub(x,1:2:nnorm)
        y = elty <: Integer ? convert(Vector{elty}, rand(1:10, nnorm)) : 
            elty <: Complex ? convert(Vector{elty}, complex(randn(nnorm), randn(nnorm))) : 
            convert(Vector{elty}, randn(nnorm))        
        ys = sub(y,1:2:nnorm)
        α = elty <: Integer ? randn() : 
            elty <: Complex ? convert(elty, complex(randn(),randn())) : 
            convert(elty, randn())
        # Absolute homogeneity
        @test_approx_eq norm(α*x,-Inf) abs(α)*norm(x,-Inf)
        @test_approx_eq norm(α*x,-1) abs(α)*norm(x,-1)
        @test_approx_eq norm(α*x,1) abs(α)*norm(x,1)
        @test_approx_eq norm(α*x) abs(α)*norm(x) # two is default
        @test_approx_eq norm(α*x,3) abs(α)*norm(x,3)
        @test_approx_eq norm(α*x,Inf) abs(α)*norm(x,Inf)
        
        @test_approx_eq norm(α*xs,-Inf) abs(α)*norm(xs,-Inf)
        @test_approx_eq norm(α*xs,-1) abs(α)*norm(xs,-1)
        @test_approx_eq norm(α*xs,1) abs(α)*norm(xs,1)
        @test_approx_eq norm(α*xs) abs(α)*norm(xs) # two is default
        @test_approx_eq norm(α*xs,3) abs(α)*norm(xs,3)
        @test_approx_eq norm(α*xs,Inf) abs(α)*norm(xs,Inf)

        # Triangle inequality
        @test norm(x + y,1) <= norm(x,1) + norm(y,1)
        @test norm(x + y) <= norm(x) + norm(y) # two is default
        @test norm(x + y,3) <= norm(x,3) + norm(y,3)
        @test norm(x + y,Inf) <= norm(x,Inf) + norm(y,Inf)
        
        @test norm(xs + ys,1) <= norm(xs,1) + norm(ys,1)
        @test norm(xs + ys) <= norm(xs) + norm(ys) # two is default
        @test norm(xs + ys,3) <= norm(xs,3) + norm(ys,3)
        @test norm(xs + ys,Inf) <= norm(xs,Inf) + norm(ys,Inf)

        # Against vectorized versions
        @test_approx_eq norm(x,-Inf) minimum(abs(x))
        @test_approx_eq norm(x,-1) inv(sum(1./abs(x)))
        @test_approx_eq norm(x,0) sum(x .!= 0)
        @test_approx_eq norm(x,1) sum(abs(x))
        @test_approx_eq norm(x) sqrt(sum(abs2(x)))
        @test_approx_eq norm(x,3) cbrt(sum(abs(x).^3.))
        @test_approx_eq norm(x,Inf) maximum(abs(x))
    end
    ## Matrix (Operator)
        A = ones(elty,10,10)
        As = sub(A,1:5,1:5)
        @test_approx_eq norm(A, 1) 10
        elty <: Union(BigFloat,Complex{BigFloat},BigInt) || @test_approx_eq norm(A, 2) 10
        @test_approx_eq norm(A, Inf) 10
        @test_approx_eq norm(As, 1) 5
        elty <: Union(BigFloat,Complex{BigFloat},BigInt) || @test_approx_eq norm(As, 2) 5
        @test_approx_eq norm(As, Inf) 5

    for i = 1:10
        A = elty <: Integer ? convert(Matrix{elty}, rand(1:10, mmat, nmat)) : 
            elty <: Complex ? convert(Matrix{elty}, complex(randn(mmat, nmat), randn(mmat, nmat))) : 
            convert(Matrix{elty}, randn(mmat, nmat))
        As = sub(A,1:nmat,1:nmat)
        B = elty <: Integer ? convert(Matrix{elty}, rand(1:10, mmat, nmat)) : 
            elty <: Complex ? convert(Matrix{elty}, complex(randn(mmat, nmat), randn(mmat, nmat))) : 
            convert(Matrix{elty}, randn(mmat, nmat))        
        Bs = sub(B,1:nmat,1:nmat)
        α = elty <: Integer ? randn() :
            elty <: Complex ? convert(elty, complex(randn(),randn())) : 
            convert(elty, randn())

        # Absolute homogeneity
        @test_approx_eq norm(α*A,1) abs(α)*norm(A,1)
        elty <: Union(BigFloat,Complex{BigFloat},BigInt) || @test_approx_eq norm(α*A) abs(α)*norm(A) # two is default
        @test_approx_eq norm(α*A,Inf) abs(α)*norm(A,Inf)
        
        @test_approx_eq norm(α*As,1) abs(α)*norm(As,1)
        elty <: Union(BigFloat,Complex{BigFloat},BigInt) || @test_approx_eq norm(α*As) abs(α)*norm(As) # two is default
        @test_approx_eq norm(α*As,Inf) abs(α)*norm(As,Inf)

        # Triangle inequality
        @test norm(A + B,1) <= norm(A,1) + norm(B,1)
        elty <: Union(BigFloat,Complex{BigFloat},BigInt) || @test norm(A + B) <= norm(A) + norm(B) # two is default
        @test norm(A + B,Inf) <= norm(A,Inf) + norm(B,Inf)
        
        @test norm(As + Bs,1) <= norm(As,1) + norm(Bs,1)
        elty <: Union(BigFloat,Complex{BigFloat},BigInt) || @test norm(As + Bs) <= norm(As) + norm(Bs) # two is default
        @test norm(As + Bs,Inf) <= norm(As,Inf) + norm(Bs,Inf)

        # vecnorm:
        for p = -2:3
            @test norm(reshape(A, length(A)), p) == vecnorm(A, p)
        end
    end
end

## Issue related tests
# issue 1447
let
    A = [1.+0.im 0; 0 1]
    B = pinv(A)
    for i = 1:4
        @test_approx_eq A[i] B[i]
    end
end

# issue 2246
let
    A = [1 2 0 0; 0 1 0 0; 0 0 0 0; 0 0 0 0]
    Asq = sqrtm(A)
    @test_approx_eq Asq*Asq A
    A2 = sub(A, 1:2, 1:2)
    A2sq = sqrtm(A2)
    @test_approx_eq A2sq*A2sq A2
end

let
    N = 3
    @test_approx_eq log(det(eye(N))) logdet(eye(N))
end

# issue 2637
let
  a = [1, 2, 3]
  b = [4, 5, 6]
  @test kron(eye(2),eye(2)) == eye(4)
  @test kron(a,b) == [4,5,6,8,10,12,12,15,18]             
  @test kron(a',b') == [4 5 6 8 10 12 12 15 18]           
  @test kron(a,b')  == [4 5 6; 8 10 12; 12 15 18]         
  @test kron(a',b)  == [4 8 12; 5 10 15; 6 12 18]         
  @test kron(a,eye(2)) == [1 0; 0 1; 2 0; 0 2; 3 0; 0 3]  
  @test kron(eye(2),a) == [ 1 0; 2 0; 3 0; 0 1; 0 2; 0 3] 
  @test kron(eye(2),2) == 2*eye(2)                        
  @test kron(3,eye(3)) == 3*eye(3)                        
  @test kron(a,2) == [2, 4, 6]                            
  @test kron(b',2) == [8 10 12]                              
end

# issue #4796
let
    dim=2
    S=zeros(Complex,dim,dim)
    T=zeros(Complex,dim,dim)
    T[:] = 1
    z = 2.5 + 1.5im
    S[1] = z
    @test S*T == [z z; 0 0]
end



# This file is a part of Julia. License is MIT: http://julialang.org/license

debug = false

import Base.LinAlg
import Base.LinAlg: BlasComplex, BlasFloat, BlasReal

# basic tridiagonal operations
n = 5

srand(123)

d = 1 .+ rand(n)
dl = -rand(n-1)
du = -rand(n-1)
v = randn(n)
B = randn(n,2)

for elty in (Float32, Float64, Complex64, Complex128, Int)
    if elty == Int
        srand(61516384)
        d = rand(1:100, n)
        dl = -rand(0:10, n-1)
        du = -rand(0:10, n-1)
        v = rand(1:100, n)
        B = rand(1:100, n, 2)
    else
        d = convert(Vector{elty}, d)
        dl = convert(Vector{elty}, dl)
        du = convert(Vector{elty}, du)
        v = convert(Vector{elty}, v)
        B = convert(Matrix{elty}, B)
    end
    ε = eps(abs2(float(one(elty))))
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
    # @test_approx_eq Base.solve(T,v) invFv
    # @test_approx_eq Base.solve(T, B) F\B
    Tlu = factorize(T)
    x = Tlu\v
    @test_approx_eq x invFv
    @test_approx_eq det(T) det(F)

    # symmetric tridiagonal
    if elty <: Real
        Ts = SymTridiagonal(d, dl)
        Fs = full(Ts)
        invFsv = Fs\v
        Tldlt = ldltfact(Ts)
        x = Tldlt\v
        @test_approx_eq x invFsv
        @test_approx_eq full(full(Tldlt)) Fs
    end

    # eigenvalues/eigenvectors of symmetric tridiagonal
    if elty === Float32 || elty === Float64
        DT, VT = eig(Ts)
        D, Vecs = eig(Fs)
        @test_approx_eq DT D
        @test_approx_eq abs(VT'Vecs) eye(elty, n)
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
        for theta = convert(Vector{elty}, pi ./ [1:4;])
            R = [cos(theta) -sin(theta);
                 sin(theta) cos(theta)]
            @test_approx_eq convert(elty, det(R)) one(elty)
        end

    # issue #1490
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

# Test gradient
for elty in (Int32, Int64, Float32, Float64, Complex64, Complex128)
    if elty <: Real
        x = convert(Vector{elty}, [1:3;])
        g = ones(elty, 3)
    else
        x = convert(Vector{elty}, complex([1:3;], [1:3;]))
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
            FJulia  = Base.LinAlg.generic_lufact!(copy(A))
            FLAPACK = Base.LinAlg.LAPACK.getrf!(copy(A))
            @test_approx_eq FJulia.factors FLAPACK[1]
            @test_approx_eq FJulia.ipiv FLAPACK[2]
            @test_approx_eq FJulia.info FLAPACK[3]
        end

        ## QR
        FJulia  = invoke(qrfact!, Tuple{AbstractMatrix, Type{Val{false}}},
                         copy(A), Val{false})
        FLAPACK = Base.LinAlg.LAPACK.geqrf!(copy(A))
        @test_approx_eq FJulia.factors FLAPACK[1]
        @test_approx_eq FJulia.τ FLAPACK[2]
    end
end

# Tests norms
nnorm = 10
mmat = 10
nmat = 8
for elty in (Float32, Float64, BigFloat, Complex{Float32}, Complex{Float64}, Complex{BigFloat}, Int32, Int64, BigInt)
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

        # issue #10234
        if elty <: FloatingPoint || elty <: Complex
            let z = zeros(elty, 100)
                z[1] = -Inf
                for p in [-2,-1.5,-1,-0.5,0.5,1,1.5,2,Inf]
                    @test norm(z, p) == (p < 0 ? 0 : Inf)
                    @test norm(elty[Inf],p) == Inf
                end
            end
        end
    end
end

# issue #10234
@test norm(Any[Inf],-2) == norm(Any[Inf],-1) == norm(Any[Inf],1) == norm(Any[Inf],1.5) == norm(Any[Inf],2) == norm(Any[Inf],Inf) == Inf

# overflow/underflow in norms:
@test_approx_eq norm(Float64[1e-300, 1], -3)*1e300 1
@test_approx_eq norm(Float64[1e300, 1], 3)*1e-300 1

# Uniform scaling
@test I[1,1] == 1 # getindex
@test I[1,2] == 0 # getindex
@test I === I' # transpose
@test one(UniformScaling{Float32}) == UniformScaling(one(Float32))
@test zero(UniformScaling{Float32}) == UniformScaling(zero(Float32))
@test zero(UniformScaling(rand(Complex128))) == zero(UniformScaling{Complex128})
@test one(UniformScaling(rand(Complex128))) == one(UniformScaling{Complex128})
@test eltype(one(UniformScaling(rand(Complex128)))) == Complex128
@test -one(UniformScaling(2)) == UniformScaling(-1)
α = randn()
@test α .* UniformScaling(1.0) == UniformScaling(1.0) .* α
@test UniformScaling(α)./α == UniformScaling(1.0)
@test α + UniformScaling(1.0) == UniformScaling(1.0) + α
@test α - UniformScaling(1.0) == -(UniformScaling(1.0) - α)
λ = complex(randn(),randn())
J = UniformScaling(λ)
@test ndims(J) == 2
@test transpose(J) == J
@test J*eye(2) == conj(J'eye(2)) # ctranpose (and A(c)_mul_B)
@test I + I === UniformScaling(2) # +
@test inv(I) == I
@test inv(J) == UniformScaling(inv(λ))
B = bitrand(2,2)
@test B + I == B + eye(B)
@test I + B == B + eye(B)
A = randn(2,2)
@test A + I == A + eye(A)
@test I + A == A + eye(A)
@test I - I === UniformScaling(0)
@test B - I == B - eye(B)
@test I - B == eye(B) - B
@test A - I == A - eye(A)
@test I - A == eye(A) - A
@test I*J === UniformScaling(λ)
@test B*J == B*λ
@test J*B == B*λ
S = sprandn(3,3,0.5)
@test S*J == S*λ
@test J*S == S*λ
@test A*J == A*λ
@test J*A == A*λ
@test J*ones(3) == ones(3)*λ
@test λ*J === UniformScaling(λ*J.λ)
@test J*λ === UniformScaling(λ*J.λ)
@test J/I === J
@test I/A == inv(A)
@test A/I == A
@test I/λ === UniformScaling(1/λ)
@test I\J === J
T = LowerTriangular(randn(3,3))
@test T\I == inv(T)
@test I\A == A
@test A\I == inv(A)
@test λ\I === UniformScaling(1/λ)

## Issue related tests
# issue #1447
let
    A = [1.+0.im 0; 0 1]
    B = pinv(A)
    for i = 1:4
        @test_approx_eq A[i] B[i]
    end
end

# issue #2246
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

# issue #2637
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

#Issue 7304
let
    A=[-√.5 -√.5; -√.5 √.5]
    Q=full(qrfact(A)[:Q])
    @test vecnorm(A-Q) < eps()
end

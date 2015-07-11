# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test
import Base.LinAlg: BlasFloat, BlasComplex

debug = false

n=12 #Size of matrix problem to test
srand(1)

debug && println("Diagonal matrices")
for relty in (Float32, Float64, BigFloat), elty in (relty, Complex{relty})
    debug && println("elty is $(elty), relty is $(relty)")
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

    @test_throws ArgumentError size(D,0)
    @test eye(Diagonal{elty},n) == Diagonal(ones(elty,n))
    debug && println("Linear solve")
    @test_approx_eq_eps D*v DM*v n*eps(relty)*(elty<:Complex ? 2:1)
    @test_approx_eq_eps D*U DM*U n^2*eps(relty)*(elty<:Complex ? 2:1)
    if relty != BigFloat
        @test_approx_eq_eps D\v DM\v 2n^2*eps(relty)*(elty<:Complex ? 2:1)
        @test_approx_eq_eps D\U DM\U 2n^3*eps(relty)*(elty<:Complex ? 2:1)
        @test_approx_eq_eps A_ldiv_B!(D,copy(v)) DM\v 2n^2*eps(relty)*(elty<:Complex ? 2:1)
        @test_approx_eq_eps A_ldiv_B!(D,copy(U)) DM\U 2n^3*eps(relty)*(elty<:Complex ? 2:1)
        @test_approx_eq_eps A_ldiv_B!(D,eye(D)) D\eye(D) 2n^3*eps(relty)*(elty<:Complex ? 2:1)
        @test_throws DimensionMismatch A_ldiv_B!(D, ones(elty, n + 1))
        b = rand(elty,n,n)
        b = sparse(b)
        @test_approx_eq A_ldiv_B!(D,copy(b)) full(D)\full(b)
        b = rand(elty,n+1,n+1)
        b = sparse(b)
        @test_throws DimensionMismatch A_ldiv_B!(D,copy(b))
    end

    debug && println("Simple unary functions")
    for op in (-,)
      @test op(D)==op(DM)
    end

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
    debug && println("Binary operations")
    d = convert(Vector{elty}, randn(n))
    D2 = Diagonal(d)
    DM2= diagm(d)
    for op in (+, -, *)
        @test_approx_eq full(op(D, D2)) op(DM, DM2)
    end
    # binary ops with plain numbers
    a = rand()
    @test_approx_eq full(a*D) a*DM
    @test_approx_eq full(D*a) DM*a
    @test_approx_eq full(D/a) DM/a
    if relty <: BlasFloat
        b = rand(elty,n,n)
        b = sparse(b)
        @test_approx_eq A_mul_B!(copy(D), copy(b)) full(D)*full(b)
        @test_approx_eq At_mul_B!(copy(D), copy(b)) full(D).'*full(b)
        @test_approx_eq Ac_mul_B!(copy(D), copy(b)) full(D)'*full(b)
    end

    #division of two Diagonals
    @test_approx_eq D/D2 Diagonal(D.diag./D2.diag)

    # test triu/tril
    @test triu!(copy(D),1) == zeros(D)
    @test triu!(copy(D),0) == D
    @test tril!(copy(D),1) == zeros(D)
    @test tril!(copy(D),0) == D

    # factorize
    @test factorize(D) == D

    debug && println("Eigensystem")
    eigD = eigfact(D)
    @test_approx_eq Diagonal(eigD[:values]) D
    @test eigD[:vectors] == eye(D)

    debug && println("ldiv")
    v = rand(n + 1)
    @test_throws DimensionMismatch D\v
    v = rand(n)
    @test_approx_eq D\v DM\v
    V = rand(n + 1, n)
    @test_throws DimensionMismatch D\V
    V = rand(n, n)
    @test_approx_eq D\V DM\V

    debug && println("conj and transpose")
    @test transpose(D) == D
    if elty <: BlasComplex
        @test_approx_eq full(conj(D)) conj(DM)
        @test ctranspose(D) == conj(D)
    end

    #logdet
    if relty <: Real
        ld=convert(Vector{relty},rand(n))
        @test_approx_eq logdet(Diagonal(ld)) logdet(diagm(ld))
    end

    #similar
    @test_throws ArgumentError similar(D, eltype(D), (n,n+1))
    @test length(diag(similar(D, eltype(D), (n,n)))) == n

    #10036
    @test issym(D2)
    @test ishermitian(D2)
    if elty <: Complex
        dc = d + im*convert(Vector{elty}, ones(n))
        D3 = Diagonal(dc)
        @test issym(D3)
        @test !ishermitian(D3)
    end
end

#isposdef
@test !isposdef(Diagonal(-1.0 * rand(n)))

# Indexing
let d = randn(n), D = Diagonal(d)
    for i=1:n
        @test D[i,i] == d[i]
    end
    for i=1:n
        for j=1:n
            @test D[i,j] == (i==j ? d[i] : 0)
        end
    end
    D2 = copy(D)
    for i=1:n
        D2[i,i] = i
    end
    for i=1:n
        for j=1:n
            if i == j
                @test D2[i,j] == i
            else
                @test D2[i,j] == 0
                D2[i,j] = 0
                @test_throws ArgumentError (D2[i,j] = 1)
            end
        end
    end
    @test_throws BoundsError D[0, 0]
    @test_throws BoundsError (D[0, 0] = 0)
    @test_throws BoundsError D[-1,-2]
    @test_throws BoundsError (D[-1,-2] = 0)
    @test_throws BoundsError D[n+1,n+1]
    @test_throws BoundsError (D[n+1,n+1] = 0)
    @test_throws BoundsError D[n,n+1]
    @test_throws BoundsError (D[n,n+1] = 0)
end

# inv
let d = randn(n), D = Diagonal(d)
    @test_approx_eq inv(D) inv(full(D))
end

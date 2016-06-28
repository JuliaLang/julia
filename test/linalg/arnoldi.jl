# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "arnoldi" begin
debug = false

debug && println("eigs")
let
    srand(1234)
    local n,a,asym,b,bsym,d,v
    n = 10
    areal  = sprandn(n,n,0.4)
    breal  = sprandn(n,n,0.4)
    acmplx = complex(sprandn(n,n,0.4), sprandn(n,n,0.4))
    bcmplx = complex(sprandn(n,n,0.4), sprandn(n,n,0.4))

    testtol = 1e-6

    for elty in (Float64, Complex128)
        if elty == Complex64 || elty == Complex128
            a = acmplx
            b = bcmplx
        else
            a = areal
            b = breal
        end
        a     = convert(SparseMatrixCSC{elty}, a)
        asym  = a' + a                  # symmetric indefinite
        apd   = a'*a                    # symmetric positive-definite

        b     = convert(SparseMatrixCSC{elty}, b)
        bsym  = b' + b
        bpd   = b'*b

        (d,v) = eigs(a, nev=3)
        @test a*v[:,2] ≈ d[2]*v[:,2]
        @test norm(v) > testtol # eigenvectors cannot be null vectors
        # (d,v) = eigs(a, b, nev=3, tol=1e-8) # not handled yet
        # @test_approx_eq_eps a*v[:,2] d[2]*b*v[:,2] testtol
        # @test norm(v) > testtol # eigenvectors cannot be null vectors

        (d,v) = eigs(asym, nev=3)
        @test asym*v[:,1] ≈ d[1]*v[:,1]
        @test eigs(asym; nev=1, sigma=d[3])[1][1] ≈ d[3]
        @test norm(v) > testtol # eigenvectors cannot be null vectors

        (d,v) = eigs(apd, nev=3)
        @test apd*v[:,3] ≈ d[3]*v[:,3]
        @test eigs(apd; nev=1, sigma=d[3])[1][1] ≈ d[3]

        (d,v) = eigs(apd, bpd, nev=3, tol=1e-8)
        @test_approx_eq_eps apd*v[:,2] d[2]*bpd*v[:,2] testtol
        @test norm(v) > testtol # eigenvectors cannot be null vectors

        # test (shift-and-)invert mode
        (d,v) = eigs(apd, nev=3, sigma=0)
        @test apd*v[:,3] ≈ d[3]*v[:,3]
        @test norm(v) > testtol # eigenvectors cannot be null vectors

        (d,v) = eigs(apd, bpd, nev=3, sigma=0, tol=1e-8)
        @test_approx_eq_eps apd*v[:,1] d[1]*bpd*v[:,1] testtol
        @test norm(v) > testtol # eigenvectors cannot be null vectors

        @test_throws ArgumentError eigs(rand(elty,2,2))
        @test_throws ArgumentError eigs(a, nev=-1)
        @test_throws ArgumentError eigs(a, which=:Z)
        @test_throws ArgumentError eigs(a, which=:BE)
        @test_throws DimensionMismatch eigs(a, v0=zeros(elty,n+2))
        @test_throws ArgumentError eigs(a, v0=zeros(Int,n))
        if elty == Float64
            @test_throws ArgumentError eigs(a+a.',which=:SI)
            @test_throws ArgumentError eigs(a+a.',which=:LI)
            @test_throws ArgumentError eigs(a,sigma=rand(Complex64))
        end
        @test_throws Base.LinAlg.PosDefException eigs(a,b)
    end
end

# Problematic example from #6965
let A6965 = [
         1.0   1.0   1.0   1.0   1.0   1.0   1.0  1.0
        -1.0   2.0   0.0   0.0   0.0   0.0   0.0  1.0
        -1.0   0.0   3.0   0.0   0.0   0.0   0.0  1.0
        -1.0   0.0   0.0   4.0   0.0   0.0   0.0  1.0
        -1.0   0.0   0.0   0.0   5.0   0.0   0.0  1.0
        -1.0   0.0   0.0   0.0   0.0   6.0   0.0  1.0
        -1.0   0.0   0.0   0.0   0.0   0.0   7.0  1.0
        -1.0  -1.0  -1.0  -1.0  -1.0  -1.0  -1.0  8.0
       ]

    d, = eigs(A6965,which=:SM,nev=2,ncv=4,tol=eps())
    @test d[1] ≈ 2.5346936860350002
    @test real(d[2]) ≈ 2.6159972444834976
    @test abs(imag(d[2])) ≈ 1.2917858749046127

    # Requires ARPACK 3.2 or a patched 3.1.5
    #T6965 = [ 0.9  0.05  0.05
    #          0.8  0.1   0.1
    #          0.7  0.1   0.2 ]
    #d,v,nconv = eigs(T6965,nev=1,which=:LM)
    #@test_approx_eq_eps T6965*v d[1]*v 1e-6
end

# Example from Quantum Information Theory
import Base: size, issymmetric, ishermitian

type CPM{T<:Base.LinAlg.BlasFloat}<:AbstractMatrix{T} # completely positive map
    kraus::Array{T,3} # kraus operator representation
end
size(Phi::CPM)=(size(Phi.kraus,1)^2,size(Phi.kraus,3)^2)
issymmetric(Phi::CPM)=false
ishermitian(Phi::CPM)=false
import Base: A_mul_B!
function A_mul_B!{T<:Base.LinAlg.BlasFloat}(rho2::StridedVector{T},Phi::CPM{T},rho::StridedVector{T})
    rho=reshape(rho,(size(Phi.kraus,3),size(Phi.kraus,3)))
    rho1=zeros(T,(size(Phi.kraus,1),size(Phi.kraus,1)))
    for s=1:size(Phi.kraus,2)
        As=view(Phi.kraus,:,s,:)
        rho1+=As*rho*As'
    end
    return copy!(rho2,rho1)
end

let
    # Generate random isometry
    (Q,R)=qr(randn(100,50))
    Q=reshape(Q,(50,2,50))
    # Construct trace-preserving completely positive map from this
    Phi=CPM(copy(Q))
    (d,v,nconv,numiter,numop,resid) = eigs(Phi,nev=1,which=:LM)
    # Properties: largest eigenvalue should be 1, largest eigenvector, when reshaped as matrix
    # should be a Hermitian positive definite matrix (up to an arbitrary phase)

    @test d[1] ≈ 1. # largest eigenvalue should be 1.
    v=reshape(v,(50,50)) # reshape to matrix
    v/=trace(v) # factor out arbitrary phase
    @test isapprox(vecnorm(imag(v)),0.) # it should be real
    v=real(v)
    # @test isapprox(vecnorm(v-v')/2,0.) # it should be Hermitian
    # Since this fails sometimes (numerical precision error),this test is commented out
    v=(v+v')/2
    @test isposdef(v)

    # Repeat with starting vector
    (d2,v2,nconv2,numiter2,numop2,resid2) = eigs(Phi,nev=1,which=:LM,v0=reshape(v,(2500,)))
    v2=reshape(v2,(50,50))
    v2/=trace(v2)
    @test numiter2<numiter
    @test v ≈ v2

    @test eigs(speye(50), nev=10)[1] ≈ ones(10) #Issue 4246
end

debug && println("real svds")
let # svds test
    A = sparse([1, 1, 2, 3, 4], [2, 1, 1, 3, 1], [2.0, -1.0, 6.1, 7.0, 1.5])
    S1 = svds(A, nsv = 2)
    S2 = svd(full(A))

    ## singular values match:
    @test S1[1][:S] ≈ S2[2][1:2]

    ## 1st left singular vector
    s1_left = sign(S1[1][:U][3,1]) * S1[1][:U][:,1]
    s2_left = sign(S2[1][3,1]) * S2[1][:,1]
    @test s1_left ≈ s2_left

    ## 1st right singular vector
    s1_right = sign(S1[1][:Vt][3,1]) * S1[1][:Vt][:,1]
    s2_right = sign(S2[3][3,1]) * S2[3][:,1]
    @test s1_right ≈ s2_right

    #10329
    debug && println("Issue 10329")
    B = sparse(diagm([1.0, 2.0, 34.0, 5.0, 6.0]))
    S3 = svds(B, ritzvec=false, nsv=2)
    @test S3[1][:S] ≈ [34.0, 6.0]
    S4 = svds(B, nsv=2)
    @test S4[1][:S] ≈ [34.0, 6.0]

    ## test passing guess for Krylov vectors
    S1 = svds(A, nsv = 2, u0=rand(eltype(A),size(A,1)))
    @test S1[1][:S] ≈ S2[2][1:2]
    S1 = svds(A, nsv = 2, v0=rand(eltype(A),size(A,2)))
    @test S1[1][:S] ≈ S2[2][1:2]
    S1 = svds(A, nsv = 2, u0=rand(eltype(A),size(A,1)), v0=rand(eltype(A),size(A,2)))
    @test S1[1][:S] ≈ S2[2][1:2]

    @test_throws ArgumentError svds(A,nsv=0)
    @test_throws ArgumentError svds(A,nsv=20)
    @test_throws DimensionMismatch svds(A,nsv=2,u0=rand(size(A,1)+1))
    @test_throws DimensionMismatch svds(A,nsv=2,v0=rand(size(A,2)+1))
end

debug && println("complex svds")
let # complex svds test
    A = sparse([1, 1, 2, 3, 4], [2, 1, 1, 3, 1], exp(im*[2.0:2:10;]))
    S1 = svds(A, nsv = 2)
    S2 = svd(full(A))

    ## singular values match:
    @test S1[1][:S] ≈ S2[2][1:2]

    ## left singular vectors
    s1_left = abs(S1[1][:U][:,1:2])
    s2_left = abs(S2[1][:,1:2])
    @test s1_left ≈ s2_left

    ## right singular vectors
    s1_right = abs(S1[1][:Vt][:,1:2])
    s2_right = abs(S2[3][:,1:2])
    @test s1_right ≈ s2_right

    ## test passing guess for Krylov vectors
    S1 = svds(A, nsv = 2, u0=rand(eltype(A),size(A,1)))
    @test S1[1][:S] ≈ S2[2][1:2]
    S1 = svds(A, nsv = 2, v0=rand(eltype(A),size(A,2)))
    @test S1[1][:S] ≈ S2[2][1:2]
    S1 = svds(A, nsv = 2, u0=rand(eltype(A),size(A,1)), v0=rand(eltype(A),size(A,2)))
    @test S1[1][:S] ≈ S2[2][1:2]

    @test_throws ArgumentError svds(A,nsv=0)
    @test_throws ArgumentError svds(A,nsv=20)
    @test_throws DimensionMismatch svds(A,nsv=2,u0=complex(rand(size(A,1)+1)))
    @test_throws DimensionMismatch svds(A,nsv=2,v0=complex(rand(size(A,2)+1)))
end

# test promotion
eigs(rand(1:10, 10, 10))
eigs(rand(1:10, 10, 10), rand(1:10, 10, 10) |> t -> t't)
svds(rand(1:10, 10, 8))
@test_throws MethodError eigs(big(rand(1:10, 10, 10)))
@test_throws MethodError eigs(big(rand(1:10, 10, 10)), rand(1:10, 10, 10))
@test_throws MethodError svds(big(rand(1:10, 10, 8)))

end

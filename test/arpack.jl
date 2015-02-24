begin
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
        @test_approx_eq a*v[:,2] d[2]*v[:,2]
        @test norm(v) > testtol # eigenvectors cannot be null vectors
        # (d,v) = eigs(a, b, nev=3, tol=1e-8) # not handled yet
        # @test_approx_eq_eps a*v[:,2] d[2]*b*v[:,2] testtol
        # @test norm(v) > testtol # eigenvectors cannot be null vectors

        (d,v) = eigs(asym, nev=3)
        @test_approx_eq asym*v[:,1] d[1]*v[:,1]
        @test_approx_eq eigs(asym; nev=1, sigma=d[3])[1][1] d[3]
        @test norm(v) > testtol # eigenvectors cannot be null vectors

        (d,v) = eigs(apd, nev=3)
        @test_approx_eq apd*v[:,3] d[3]*v[:,3]
        @test_approx_eq eigs(apd; nev=1, sigma=d[3])[1][1] d[3]

        (d,v) = eigs(apd, bpd, nev=3, tol=1e-8)
        @test_approx_eq_eps apd*v[:,2] d[2]*bpd*v[:,2] testtol
        @test norm(v) > testtol # eigenvectors cannot be null vectors

        # test (shift-and-)invert mode
        (d,v) = eigs(apd, nev=3, sigma=0)
        @test_approx_eq apd*v[:,3] d[3]*v[:,3]
        @test norm(v) > testtol # eigenvectors cannot be null vectors

        (d,v) = eigs(apd, bpd, nev=3, sigma=0, tol=1e-8)
        @test_approx_eq_eps apd*v[:,1] d[1]*bpd*v[:,1] testtol
        @test norm(v) > testtol # eigenvectors cannot be null vectors

    end
end

# Problematic example from #6965
A6965 = [
         1.0   1.0   1.0   1.0   1.0   1.0   1.0  1.0
        -1.0   2.0   0.0   0.0   0.0   0.0   0.0  1.0
        -1.0   0.0   3.0   0.0   0.0   0.0   0.0  1.0
        -1.0   0.0   0.0   4.0   0.0   0.0   0.0  1.0
        -1.0   0.0   0.0   0.0   5.0   0.0   0.0  1.0
        -1.0   0.0   0.0   0.0   0.0   6.0   0.0  1.0
        -1.0   0.0   0.0   0.0   0.0   0.0   7.0  1.0
        -1.0  -1.0  -1.0  -1.0  -1.0  -1.0  -1.0  8.0
       ];

d, = eigs(A6965,which=:SM,nev=2,ncv=4,tol=eps())
@test_approx_eq d[1] 2.5346936860350002
@test_approx_eq real(d[2]) 2.6159972444834976
@test_approx_eq abs(imag(d[2])) 1.2917858749046127

# Requires ARPACK 3.2 or a patched 3.1.5
#T6965 = [ 0.9  0.05  0.05
#          0.8  0.1   0.1
#          0.7  0.1   0.2 ]
#d,v,nconv = eigs(T6965,nev=1,which=:LM)
#@test_approx_eq_eps T6965*v d[1]*v 1e-6

# Example from Quantum Information Theory
import Base: size, issym, ishermitian

type CPM{T<:Base.LinAlg.BlasFloat}<:AbstractMatrix{T} # completely positive map
    kraus::Array{T,3} # kraus operator representation
end

size(Phi::CPM)=(size(Phi.kraus,1)^2,size(Phi.kraus,3)^2)
issym(Phi::CPM)=false
ishermitian(Phi::CPM)=false

function *{T<:Base.LinAlg.BlasFloat}(Phi::CPM{T},rho::Vector{T})
    rho=reshape(rho,(size(Phi.kraus,3),size(Phi.kraus,3)))
    rho2=zeros(T,(size(Phi.kraus,1),size(Phi.kraus,1)))
    for s=1:size(Phi.kraus,2)
        As=slice(Phi.kraus,:,s,:)
        rho2+=As*rho*As'
    end
    return reshape(rho2,(size(Phi.kraus,1)^2,))
end
# Generate random isometry
(Q,R)=qr(randn(100,50))
Q=reshape(Q,(50,2,50))
# Construct trace-preserving completely positive map from this
Phi=CPM(Q)
(d,v,nconv,numiter,numop,resid) = eigs(Phi,nev=1,which=:LM)
# Properties: largest eigenvalue should be 1, largest eigenvector, when reshaped as matrix
# should be a Hermitian positive definite matrix (up to an arbitrary phase)

@test_approx_eq d[1] 1. # largest eigenvalue should be 1.
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
@test_approx_eq v v2

@test_approx_eq eigs(speye(50), nev=10)[1] ones(10) #Issue 4246

# This file is a part of Julia. License is MIT: http://julialang.org/license

debug = false

n = 10

# Split n into 2 parts for tests needing two matrices
n1 = div(n, 2)
n2 = 2*n1

srand(1234321)

a = rand(n,n)

areal = randn(n,n)/2
aimg  = randn(n,n)/2
breal = randn(n,2)/2
bimg  = randn(n,2)/2
creal = randn(n)/2
cimg  = randn(n)/2
dureal = randn(n-1)/2
duimg  = randn(n-1)/2
dlreal = randn(n-1)/2
dlimg  = randn(n-1)/2
dreal = randn(n)/2
dimg  = randn(n)/2

for eltya in (Float32, Float64, Complex64, Complex128, BigFloat, Int)
    a = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex(areal, aimg) : areal)
    d = eltya == Int ? Tridiagonal(rand(1:7, n-1), rand(1:7, n), rand(1:7, n-1)) : convert(Tridiagonal{eltya}, eltya <: Complex ? Tridiagonal(complex(dlreal, dlimg), complex(dreal, dimg), complex(dureal, duimg)) : Tridiagonal(dlreal, dreal, dureal))
    ε = εa = eps(abs(float(one(eltya))))

    for eltyb in (Float32, Float64, Complex64, Complex128, Int)
        b = eltyb == Int ? rand(1:5, n, 2) : convert(Matrix{eltyb}, eltyb <: Complex ? complex(breal, bimg) : breal)
        c = eltyb == Int ? rand(1:5, n) : convert(Vector{eltyb}, eltyb <: Complex ? complex(creal, cimg) : creal)
        εb = eps(abs(float(one(eltyb))))
        ε = max(εa,εb)

debug && println("(Automatic) Square LU decomposition")
        κ     = cond(a,1)
        lua   = factorize(a)
        l,u,p = lua[:L], lua[:U], lua[:p]
        @test_approx_eq l*u a[p,:]
        @test_approx_eq (l*u)[invperm(p),:] a
        @test_approx_eq a * inv(lua) eye(n)
        @test norm(a*(lua\b) - b, 1) < ε*κ*n*2 # Two because the right hand side has two columns
        @test norm(a'*(lua'\b) - b, 1) < ε*κ*n*2 # Two because the right hand side has two columns
        @test norm(a'*(lua'\a') - a', 1) < ε*κ*n^2
        @test norm(a*(lua\c) - c, 1) < ε*κ*n # c is a vector
        @test norm(a'*(lua'\c) - c, 1) < ε*κ*n # c is a vector
        if eltya <: Real && eltyb <: Real
            @test norm(a.'*(lua.'\b) - b,1) < ε*κ*n*2 # Two because the right hand side has two columns
            @test norm(a.'*(lua.'\c) - c,1) < ε*κ*n
        end
        @test_approx_eq full(lua) a

debug && println("Tridiagonal LU")
        κd    = cond(full(d),1)
        lud   = lufact(d)
        @test norm(d*(lud\b) - b, 1) < ε*κd*n*2 # Two because the right hand side has two columns
        if eltya <: Real
            @test norm((lud.'\b) - full(d.')\b, 1) < ε*κd*n*2 # Two because the right hand side has two columns
        end
        if eltya <: Complex
            @test norm((lud'\b) - full(d')\b, 1) < ε*κd*n*2 # Two because the right hand side has two columns
        end
        f = zeros(eltyb, n+1)
        @test_throws DimensionMismatch lud\f

debug && println("Thin LU")
        lua   = @inferred lufact(a[:,1:n1])
        @test_approx_eq lua[:L]*lua[:U] lua[:P]*a[:,1:n1]

debug && println("Fat LU")
        lua   = lufact(a[1:n1,:])
        @test_approx_eq lua[:L]*lua[:U] lua[:P]*a[1:n1,:]
    end
end

# Test rational matrices
## Integrate in general tests when more linear algebra is implemented in julia
a = convert(Matrix{Rational{BigInt}}, rand(1:10//1,n,n))/n
b = rand(1:10,n,2)
@inferred lufact(a)
lua   = factorize(a)
l,u,p = lua[:L], lua[:U], lua[:p]
@test_approx_eq l*u a[p,:]
@test_approx_eq l[invperm(p),:]*u a
@test_approx_eq a * inv(lua) eye(n)
@test_approx_eq a*(lua\b) b
@test_approx_eq @inferred(det(a)) det(Array{Float64}(a))
## Hilbert Matrix (very ill conditioned)
## Testing Rational{BigInt} and BigFloat version
nHilbert = 50
H = Rational{BigInt}[1//(i+j-1) for i = 1:nHilbert,j = 1:nHilbert]
Hinv = Rational{BigInt}[(-1)^(i+j)*(i+j-1)*binomial(nHilbert+i-1,nHilbert-j)*binomial(nHilbert+j-1,nHilbert-i)*binomial(i+j-2,i-1)^2 for i = big(1):nHilbert,j=big(1):nHilbert]
@test inv(H) == Hinv
with_bigfloat_precision(2^10) do
    @test norm(Array{Float64}(inv(float(H)) - float(Hinv))) < 1e-100
end

# Test balancing in eigenvector calculations
for elty in (Float32, Float64, Complex64, Complex128)
    A = convert(Matrix{elty}, [ 3.0     -2.0      -0.9     2*eps(real(one(elty)));
                               -2.0      4.0       1.0    -eps(real(one(elty)));
                               -eps(real(one(elty)))/4  eps(real(one(elty)))/2  -1.0     0;
                               -0.5     -0.5       0.1     1.0])
    F = eigfact(A, permute=false, scale=false)
    eig(A, permute=false, scale=false)
    @test_approx_eq F[:vectors]*Diagonal(F[:values])/F[:vectors] A
    F = eigfact(A)
    # @test norm(F[:vectors]*Diagonal(F[:values])/F[:vectors] - A) > 0.01
end


@test @inferred(logdet(Complex64[1.0f0 0.5f0; 0.5f0 -1.0f0])) === 0.22314355f0 + 3.1415927f0im
@test_throws DomainError logdet([1 1; 1 -1])

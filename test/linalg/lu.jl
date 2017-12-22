# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
using Base.LinAlg: ldiv!, Adjoint, Transpose
import Base.LinAlg.BlasInt, Base.LinAlg.BlasFloat

n = 10

# Split n into 2 parts for tests needing two matrices
n1 = div(n, 2)
n2 = 2*n1

srand(1234321)

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

@testset for eltya in (Float32, Float64, ComplexF32, ComplexF64, BigFloat, Int)
    a = eltya == Int ? rand(1:7, n, n) :
        convert(Matrix{eltya}, eltya <: Complex ? complex.(areal, aimg) : areal)
    d = if eltya == Int
        Tridiagonal(rand(1:7, n-1), rand(1:7, n), rand(1:7, n-1))
    elseif eltya <: Complex
        convert(Tridiagonal{eltya}, Tridiagonal(
            complex.(dlreal, dlimg), complex.(dreal, dimg), complex.(dureal, duimg)))
    else
        convert(Tridiagonal{eltya}, Tridiagonal(dlreal, dreal, dureal))
    end
    ε = εa = eps(abs(float(one(eltya))))

    if eltya <: BlasFloat
        @testset "LU factorization for Number" begin
            num = rand(eltya)
            @test lu(num) == (one(eltya),num,1)
            @test convert(Array, lufact(num)) ≈ eltya[num]
        end
        @testset "Balancing in eigenvector calculations" begin
            A = convert(Matrix{eltya}, [ 3.0     -2.0      -0.9     2*eps(real(one(eltya)));
                                       -2.0      4.0       1.0    -eps(real(one(eltya)));
                                       -eps(real(one(eltya)))/4  eps(real(one(eltya)))/2  -1.0     0;
                                       -0.5     -0.5       0.1     1.0])
            F = eigfact(A, permute=false, scale=false)
            eig(A, permute=false, scale=false)
            @test F.vectors*Diagonal(F.values)/F.vectors ≈ A
            F = eigfact(A)
            # @test norm(F.vectors*Diagonal(F.values)/F.vectors - A) > 0.01
        end
    end
    @testset "Singular LU" begin
        lua = lufact(zeros(eltya, 3, 3))
        @test !LinAlg.issuccess(lua)
        @test sprint((t, s) -> show(t, "text/plain", s), lua) == "Failed factorization of type $(typeof(lua))"
    end
    κ  = cond(a,1)
    @testset "(Automatic) Square LU decomposition" begin
        lua   = factorize(a)
        @test_throws ErrorException lua.Z
        l,u,p = lua.L, lua.U, lua.p
        ll,ul,pl = lu(a)
        @test ll * ul ≈ a[pl,:]
        @test l*u ≈ a[p,:]
        @test (l*u)[invperm(p),:] ≈ a
        @test a * inv(lua) ≈ Matrix(I, n, n)
        @test copy(lua) == lua
        if eltya <: BlasFloat
            # test conversion of LU factorization's numerical type
            bft = eltya <: Real ? Base.LinAlg.LU{BigFloat} : Base.LinAlg.LU{Complex{BigFloat}}
            bflua = convert(bft, lua)
            @test bflua.L*bflua.U ≈ big.(a)[p,:] rtol=ε
        end
        # compact printing
        lstring = sprint(show,l)
        ustring = sprint(show,u)
        # @test sprint(show,lua) == "$(typeof(lua)) with factors L and U:\n$lstring\n$ustring"
    end
    κd    = cond(Array(d),1)
    @testset "Tridiagonal LU" begin
        lud   = lufact(d)
        @test LinAlg.issuccess(lud)
        @test lufact(lud) == lud
        @test_throws ErrorException lud.Z
        @test lud.L*lud.U ≈ lud.P*Array(d)
        @test lud.L*lud.U ≈ Array(d)[lud.p,:]
        @test AbstractArray(lud) ≈ d
    end
    @testset for eltyb in (Float32, Float64, ComplexF32, ComplexF64, Int)
        b  = eltyb == Int ? rand(1:5, n, 2) :
            convert(Matrix{eltyb}, eltyb <: Complex ? complex.(breal, bimg) : breal)
        c  = eltyb == Int ? rand(1:5, n) :
            convert(Vector{eltyb}, eltyb <: Complex ? complex.(creal, cimg) : creal)
        εb = eps(abs(float(one(eltyb))))
        ε  = max(εa,εb)
        @testset "(Automatic) Square LU decomposition" begin
            lua   = factorize(a)
            let Bs = copy(b), Cs = copy(c)
                for (bb, cc) in ((Bs, Cs), (view(Bs, 1:n, 1), view(Cs, 1:n)))
                    @test norm(a*(lua\bb) - bb, 1) < ε*κ*n*2 # Two because the right hand side has two columns
                    @test norm(a'*(lua'\bb) - bb, 1) < ε*κ*n*2 # Two because the right hand side has two columns
                    @test norm(a'*(lua'\a') - adjoint(a), 1) < ε*κ*n^2
                    @test norm(a*(lua\cc) - cc, 1) < ε*κ*n # cc is a vector
                    @test norm(a'*(lua'\cc) - cc, 1) < ε*κ*n # cc is a vector
                    @test AbstractArray(lua) ≈ a
                    @test norm(Transpose(a)*(Transpose(lua)\bb) - bb,1) < ε*κ*n*2 # Two because the right hand side has two columns
                    @test norm(Transpose(a)*(Transpose(lua)\cc) - cc,1) < ε*κ*n
                end

                # Test whether Ax_ldiv_B!(y, LU, x) indeed overwrites y
                resultT = typeof(oneunit(eltyb) / oneunit(eltya))

                b_dest = similar(b, resultT)
                c_dest = similar(c, resultT)

                ldiv!(b_dest, lua, b)
                ldiv!(c_dest, lua, c)
                @test norm(b_dest - lua \ b, 1) < ε*κ*2n
                @test norm(c_dest - lua \ c, 1) < ε*κ*n

                ldiv!(b_dest, Transpose(lua), b)
                ldiv!(c_dest, Transpose(lua), c)
                @test norm(b_dest - Transpose(lua) \ b, 1) < ε*κ*2n
                @test norm(c_dest - Transpose(lua) \ c, 1) < ε*κ*n

                ldiv!(b_dest, Adjoint(lua), b)
                ldiv!(c_dest, Adjoint(lua), c)
                @test norm(b_dest - lua' \ b, 1) < ε*κ*2n
                @test norm(c_dest - lua' \ c, 1) < ε*κ*n
            end
            if eltya <: BlasFloat && eltyb <: BlasFloat
                e = rand(eltyb,n,n)
                @test norm(e/lua - e/a,1) < ε*κ*n^2
            end
        end
        @testset "Tridiagonal LU" begin
            lud   = factorize(d)
            f = zeros(eltyb, n+1)
            @test_throws DimensionMismatch lud\f
            @test_throws DimensionMismatch Transpose(lud)\f
            @test_throws DimensionMismatch lud'\f
            @test_throws DimensionMismatch Base.LinAlg.ldiv!(Transpose(lud), f)
            let Bs = copy(b)
                for bb in (Bs, view(Bs, 1:n, 1))
                    @test norm(d*(lud\bb) - bb, 1) < ε*κd*n*2 # Two because the right hand side has two columns
                    if eltya <: Real
                        @test norm((Transpose(lud)\bb) - Array(transpose(d))\bb, 1) < ε*κd*n*2 # Two because the right hand side has two columns
                        if eltya != Int && eltyb != Int
                            @test norm(Base.LinAlg.ldiv!(Transpose(lud), copy(bb)) - Array(transpose(d))\bb, 1) < ε*κd*n*2
                        end
                    end
                    if eltya <: Complex
                        @test norm((lud'\bb) - Array(d')\bb, 1) < ε*κd*n*2 # Two because the right hand side has two columns
                    end
                end
            end
            if eltya <: BlasFloat && eltyb <: BlasFloat
                e = rand(eltyb,n,n)
                @test norm(e/lud - e/d,1) < ε*κ*n^2
                @test norm((Transpose(lud)\e') - Array(transpose(d))\e',1) < ε*κd*n^2
                #test singular
                du = rand(eltya,n-1)
                dl = rand(eltya,n-1)
                dd = rand(eltya,n)
                dd[1] = zero(eltya)
                du[1] = zero(eltya)
                dl[1] = zero(eltya)
                zT = Tridiagonal(dl,dd,du)
                @test !LinAlg.issuccess(lufact(zT))
            end
        end
        @testset "Thin LU" begin
            lua   = @inferred lufact(a[:,1:n1])
            @test lua.L*lua.U ≈ lua.P*a[:,1:n1]
        end
        @testset "Fat LU" begin
            lua   = lufact(a[1:n1,:])
            @test lua.L*lua.U ≈ lua.P*a[1:n1,:]
        end
    end

    @testset "LU of Symmetric/Hermitian" begin
        for HS in (Hermitian(a'a), Symmetric(a'a))
            luhs = lufact(HS)
            @test luhs.L*luhs.U ≈ luhs.P*Matrix(HS)
        end
    end
end

@testset "conversion" begin
    srand(3)
    a = Tridiagonal(rand(9),rand(10),rand(9))
    fa = Array(a)
    falu = lufact(fa)
    alu = lufact(a)
    falu = convert(typeof(falu),alu)
    @test AbstractArray(alu) == fa
end

@testset "Rational Matrices" begin
    ## Integrate in general tests when more linear algebra is implemented in julia
    a = convert(Matrix{Rational{BigInt}}, rand(1:10//1,n,n))/n
    b = rand(1:10,n,2)
    @inferred lufact(a)
    lua   = factorize(a)
    l,u,p = lua.L, lua.U, lua.p
    @test l*u ≈ a[p,:]
    @test l[invperm(p),:]*u ≈ a
    @test a*inv(lua) ≈ Matrix(I, n, n)
    let Bs = b
        for b in (Bs, view(Bs, 1:n, 1))
            @test a*(lua\b) ≈ b
        end
    end
    @test @inferred(det(a)) ≈ det(Array{Float64}(a))
end

@testset "Rational{BigInt} and BigFloat Hilbert Matrix" begin
    ## Hilbert Matrix (very ill conditioned)
    ## Testing Rational{BigInt} and BigFloat version
    nHilbert = 50
    H = Rational{BigInt}[1//(i+j-1) for i = 1:nHilbert,j = 1:nHilbert]
    Hinv = Rational{BigInt}[(-1)^(i+j)*(i+j-1)*binomial(nHilbert+i-1,nHilbert-j)*
        binomial(nHilbert+j-1,nHilbert-i)*binomial(i+j-2,i-1)^2
        for i = big(1):nHilbert,j=big(1):nHilbert]
    @test inv(H) == Hinv
    setprecision(2^10) do
        @test norm(Array{Float64}(inv(float(H)) - float(Hinv))) < 1e-100
    end
end

@testset "logdet" begin
    @test @inferred(logdet(ComplexF32[1.0f0 0.5f0; 0.5f0 -1.0f0])) === 0.22314355f0 + 3.1415927f0im
    @test_throws DomainError logdet([1 1; 1 -1])
end

@testset "Issue 21453" begin
    @test_throws ArgumentError LinAlg._cond1Inf(lufact(randn(5,5)), 2, 2.0)
end

@testset "REPL printing" begin
        bf = IOBuffer()
        show(bf, "text/plain", lufact(Matrix(I, 4, 4)))
        seekstart(bf)
        @test String(take!(bf)) == """
Base.LinAlg.LU{Float64,Array{Float64,2}}
L factor:
4×4 Array{Float64,2}:
 1.0  0.0  0.0  0.0
 0.0  1.0  0.0  0.0
 0.0  0.0  1.0  0.0
 0.0  0.0  0.0  1.0
U factor:
4×4 Array{Float64,2}:
 1.0  0.0  0.0  0.0
 0.0  1.0  0.0  0.0
 0.0  0.0  1.0  0.0
 0.0  0.0  0.0  1.0"""
end

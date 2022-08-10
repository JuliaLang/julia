# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestBidiagonal

using Test, LinearAlgebra, Random
using LinearAlgebra: BlasReal, BlasFloat

const BASE_TEST_PATH = joinpath(Sys.BINDIR, "..", "share", "julia", "test")

isdefined(Main, :Furlongs) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "Furlongs.jl"))
using .Main.Furlongs

isdefined(Main, :Quaternions) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "Quaternions.jl"))
using .Main.Quaternions

include("testutils.jl") # test_approx_eq_modphase

n = 10 #Size of test matrix
Random.seed!(1)

@testset for relty in (Int, Float32, Float64, BigFloat), elty in (relty, Complex{relty})
    if relty <: AbstractFloat
        dv = convert(Vector{elty}, randn(n))
        ev = convert(Vector{elty}, randn(n-1))
        if (elty <: Complex)
            dv += im*convert(Vector{elty}, randn(n))
            ev += im*convert(Vector{elty}, randn(n-1))
        end
    elseif relty <: Integer
        dv = convert(Vector{elty}, rand(1:10, n))
        ev = convert(Vector{elty}, rand(1:10, n-1))
        if (elty <: Complex)
            dv += im*convert(Vector{elty}, rand(1:10, n))
            ev += im*convert(Vector{elty}, rand(1:10, n-1))
        end
    end
    dv0 = zeros(elty, 0)
    ev0 = zeros(elty, 0)

    @testset "Constructors" begin
        for (x, y) in ((dv0, ev0), (dv, ev), (GenericArray(dv), GenericArray(ev)))
            # from vectors
            ubd = Bidiagonal(x, y, :U)
            lbd = Bidiagonal(x, y, :L)
            @test ubd != lbd || x === dv0
            @test ubd.dv === x
            @test lbd.ev === y
            @test_throws ArgumentError Bidiagonal(x, y, :R)
            @test_throws ArgumentError Bidiagonal(x, y, 'R')
            x == dv0 || @test_throws DimensionMismatch Bidiagonal(x, x, :U)
            @test_throws MethodError Bidiagonal(x, y)
            # from matrix
            @test Bidiagonal(ubd, :U) == Bidiagonal(Matrix(ubd), :U) == ubd
            @test Bidiagonal(lbd, :L) == Bidiagonal(Matrix(lbd), :L) == lbd
        end
        @test eltype(Bidiagonal{elty}([1,2,3,4], [1.0f0,2.0f0,3.0f0], :U)) == elty
        @test eltype(Bidiagonal([1,2,3,4], [1.0f0,2.0f0,3.0f0], :U)) == Float32 # promotion test
        @test isa(Bidiagonal{elty,Vector{elty}}(GenericArray(dv), ev, :U), Bidiagonal{elty,Vector{elty}})
        @test_throws MethodError Bidiagonal(dv, GenericArray(ev), :U)
        @test_throws MethodError Bidiagonal(GenericArray(dv), ev, :U)
        BI = Bidiagonal([1,2,3,4], [1,2,3], :U)
        @test Bidiagonal(BI) === BI
        @test isa(Bidiagonal{elty}(BI), Bidiagonal{elty})
    end

    @testset "getindex, setindex!, size, and similar" begin
        ubd = Bidiagonal(dv, ev, :U)
        lbd = Bidiagonal(dv, ev, :L)
        # bidiagonal getindex / upper & lower
        @test_throws BoundsError ubd[n + 1, 1]
        @test_throws BoundsError ubd[1, n + 1]
        @test ubd[2, 2] == dv[2]
        # bidiagonal getindex / upper
        @test ubd[2, 3] == ev[2]
        @test iszero(ubd[3, 2])
        # bidiagonal getindex / lower
        @test lbd[3, 2] == ev[2]
        @test iszero(lbd[2, 3])
        # bidiagonal setindex! / upper
        cubd = copy(ubd)
        @test_throws ArgumentError ubd[2, 1] = 1
        @test_throws ArgumentError ubd[3, 1] = 1
        @test (cubd[2, 1] = 0; cubd == ubd)
        @test ((cubd[1, 2] = 10) == 10; cubd[1, 2] == 10)
        # bidiagonal setindex! / lower
        clbd = copy(lbd)
        @test_throws ArgumentError lbd[1, 2] = 1
        @test_throws ArgumentError lbd[1, 3] = 1
        @test (clbd[1, 2] = 0; clbd == lbd)
        @test ((clbd[2, 1] = 10) == 10; clbd[2, 1] == 10)
        # bidiagonal setindex! / upper & lower
        @test_throws BoundsError ubd[n + 1, 1] = 1
        @test_throws BoundsError ubd[1, n + 1] = 1
        @test ((cubd[2, 2] = 10) == 10; cubd[2, 2] == 10)
        # bidiagonal size
        @test_throws ArgumentError size(ubd, 0)
        @test size(ubd, 1) == size(ubd, 2) == n
        @test size(ubd, 3) == 1
        # bidiagonal similar
        @test isa(similar(ubd), Bidiagonal{elty})
        @test similar(ubd).uplo == ubd.uplo
        @test isa(similar(ubd, Int), Bidiagonal{Int})
        @test similar(ubd, Int).uplo == ubd.uplo
        @test isa(similar(ubd, (3, 2)), Matrix)
        @test isa(similar(ubd, Int, (3, 2)), Matrix{Int})

        # setindex! when off diagonal is zero bug
        Bu = Bidiagonal(rand(elty, 10), zeros(elty, 9), 'U')
        Bl = Bidiagonal(rand(elty, 10), zeros(elty, 9), 'L')
        @test_throws ArgumentError Bu[5, 4] = 1
        @test_throws ArgumentError Bl[4, 5] = 1
    end

    @testset "show" begin
        BD = Bidiagonal(dv, ev, :U)
        dstring = sprint(Base.print_matrix,BD.dv')
        estring = sprint(Base.print_matrix,BD.ev')
        @test sprint(show,BD) == "$(summary(BD)):\n diag:$dstring\n super:$estring"
        BD = Bidiagonal(dv,ev,:L)
        @test sprint(show,BD) == "$(summary(BD)):\n diag:$dstring\n sub:$estring"
    end

    @testset for uplo in (:U, :L)
        T = Bidiagonal(dv, ev, uplo)

        @testset "Constructor and basic properties" begin
            @test size(T, 1) == size(T, 2) == n
            @test size(T) == (n, n)
            @test Array(T) == diagm(0 => dv, (uplo === :U ? 1 : -1) => ev)
            @test Bidiagonal(Array(T), uplo) == T
            @test big.(T) == T
            @test Array(abs.(T)) == abs.(diagm(0 => dv, (uplo === :U ? 1 : -1) => ev))
            @test Array(real(T)) == real(diagm(0 => dv, (uplo === :U ? 1 : -1) => ev))
            @test Array(imag(T)) == imag(diagm(0 => dv, (uplo === :U ? 1 : -1) => ev))
        end

        @testset for func in (conj, transpose, adjoint)
            @test func(func(T)) == T
        end

        @testset "permutedims(::Bidiagonal)" begin
            @test permutedims(permutedims(T)) === T
            @test permutedims(T) == transpose.(transpose(T))
            @test permutedims(T, [1, 2]) === T
            @test permutedims(T, (2, 1)) == permutedims(T)
        end

        @testset "triu and tril" begin
            zerosdv = zeros(elty, length(dv))
            zerosev = zeros(elty, length(ev))
            bidiagcopy(dv, ev, uplo) = Bidiagonal(copy(dv), copy(ev), uplo)

            @test istril(Bidiagonal(dv,ev,:L))
            @test istril(Bidiagonal(dv,ev,:L), 1)
            @test !istril(Bidiagonal(dv,ev,:L), -1)
            @test istril(Bidiagonal(zerosdv,ev,:L), -1)
            @test !istril(Bidiagonal(zerosdv,ev,:L), -2)
            @test istril(Bidiagonal(zerosdv,zerosev,:L), -2)
            @test !istril(Bidiagonal(dv,ev,:U))
            @test istril(Bidiagonal(dv,ev,:U), 1)
            @test !istril(Bidiagonal(dv,ev,:U), -1)
            @test !istril(Bidiagonal(zerosdv,ev,:U), -1)
            @test istril(Bidiagonal(zerosdv,zerosev,:U), -1)
            @test tril!(bidiagcopy(dv,ev,:U),-1) == Bidiagonal(zerosdv,zerosev,:U)
            @test tril!(bidiagcopy(dv,ev,:L),-1) == Bidiagonal(zerosdv,ev,:L)
            @test tril!(bidiagcopy(dv,ev,:U),-2) == Bidiagonal(zerosdv,zerosev,:U)
            @test tril!(bidiagcopy(dv,ev,:L),-2) == Bidiagonal(zerosdv,zerosev,:L)
            @test tril!(bidiagcopy(dv,ev,:U),1)  == Bidiagonal(dv,ev,:U)
            @test tril!(bidiagcopy(dv,ev,:L),1)  == Bidiagonal(dv,ev,:L)
            @test tril!(bidiagcopy(dv,ev,:U))    == Bidiagonal(dv,zerosev,:U)
            @test tril!(bidiagcopy(dv,ev,:L))    == Bidiagonal(dv,ev,:L)
            @test_throws ArgumentError tril!(bidiagcopy(dv, ev, :U), -n - 2)
            @test_throws ArgumentError tril!(bidiagcopy(dv, ev, :U), n)

            @test istriu(Bidiagonal(dv,ev,:U))
            @test istriu(Bidiagonal(dv,ev,:U), -1)
            @test !istriu(Bidiagonal(dv,ev,:U), 1)
            @test istriu(Bidiagonal(zerosdv,ev,:U), 1)
            @test !istriu(Bidiagonal(zerosdv,ev,:U), 2)
            @test istriu(Bidiagonal(zerosdv,zerosev,:U), 2)
            @test !istriu(Bidiagonal(dv,ev,:L))
            @test istriu(Bidiagonal(dv,ev,:L), -1)
            @test !istriu(Bidiagonal(dv,ev,:L), 1)
            @test !istriu(Bidiagonal(zerosdv,ev,:L), 1)
            @test istriu(Bidiagonal(zerosdv,zerosev,:L), 1)
            @test triu!(bidiagcopy(dv,ev,:L),1)  == Bidiagonal(zerosdv,zerosev,:L)
            @test triu!(bidiagcopy(dv,ev,:U),1)  == Bidiagonal(zerosdv,ev,:U)
            @test triu!(bidiagcopy(dv,ev,:U),2)  == Bidiagonal(zerosdv,zerosev,:U)
            @test triu!(bidiagcopy(dv,ev,:L),2)  == Bidiagonal(zerosdv,zerosev,:L)
            @test triu!(bidiagcopy(dv,ev,:U),-1) == Bidiagonal(dv,ev,:U)
            @test triu!(bidiagcopy(dv,ev,:L),-1) == Bidiagonal(dv,ev,:L)
            @test triu!(bidiagcopy(dv,ev,:L))    == Bidiagonal(dv,zerosev,:L)
            @test triu!(bidiagcopy(dv,ev,:U))    == Bidiagonal(dv,ev,:U)
            @test_throws ArgumentError triu!(bidiagcopy(dv, ev, :U), -n)
            @test_throws ArgumentError triu!(bidiagcopy(dv, ev, :U), n + 2)
            @test !isdiag(Bidiagonal(dv,ev,:U))
            @test !isdiag(Bidiagonal(dv,ev,:L))
            @test isdiag(Bidiagonal(dv,zerosev,:U))
            @test isdiag(Bidiagonal(dv,zerosev,:L))
        end

        @testset "iszero and isone" begin
            for uplo in (:U, :L)
                BDzero = Bidiagonal(zeros(elty, 10), zeros(elty, 9), uplo)
                BDone = Bidiagonal(ones(elty, 10), zeros(elty, 9), uplo)
                BDmix = Bidiagonal(zeros(elty, 10), zeros(elty, 9), uplo)
                BDmix[end,end] = one(elty)

                @test iszero(BDzero)
                @test !isone(BDzero)
                @test !iszero(BDone)
                @test isone(BDone)
                @test !iszero(BDmix)
                @test !isone(BDmix)
            end
        end

        Tfull = Array(T)
        @testset "Linear solves" begin
            if relty <: AbstractFloat
                c = convert(Matrix{elty}, randn(n,n))
                b = convert(Matrix{elty}, randn(n, 2))
                if (elty <: Complex)
                    b += im*convert(Matrix{elty}, randn(n, 2))
                end
            elseif relty <: Integer
                c = convert(Matrix{elty}, rand(1:10, n, n))
                b = convert(Matrix{elty}, rand(1:10, n, 2))
                if (elty <: Complex)
                    b += im*convert(Matrix{elty}, rand(1:10, n, 2))
                end
            end
            condT = cond(map(ComplexF64,Tfull))
            promty = typeof((zero(relty)*zero(relty) + zero(relty)*zero(relty))/one(relty))
            if relty != BigFloat
                x = transpose(T)\transpose(c)
                tx = transpose(Tfull) \ transpose(c)
                elty <: AbstractFloat && @test norm(x-tx,Inf) <= 4*condT*max(eps()*norm(tx,Inf), eps(promty)*norm(x,Inf))
                @test_throws DimensionMismatch transpose(T)\transpose(b)
                x = T'\copy(transpose(c))
                tx = Tfull'\copy(transpose(c))
                @test norm(x-tx,Inf) <= 4*condT*max(eps()*norm(tx,Inf), eps(promty)*norm(x,Inf))
                @test_throws DimensionMismatch T'\copy(transpose(b))
                x = T\transpose(c)
                tx = Tfull\transpose(c)
                @test norm(x-tx,Inf) <= 4*condT*max(eps()*norm(tx,Inf), eps(promty)*norm(x,Inf))
                @test_throws DimensionMismatch T\transpose(b)
            end
            offsizemat = Matrix{elty}(undef, n+1, 2)
            @test_throws DimensionMismatch T \ offsizemat
            @test_throws DimensionMismatch transpose(T) \ offsizemat
            @test_throws DimensionMismatch T' \ offsizemat

            if elty <: BigFloat
                @test_throws SingularException ldiv!(Bidiagonal(zeros(elty, n), ones(elty, n-1), :U), rand(elty, n))
                @test_throws SingularException ldiv!(Bidiagonal(zeros(elty, n), ones(elty, n-1), :L), rand(elty, n))
            end
            let bb = b, cc = c
                for atype in ("Array", "SubArray")
                    if atype == "Array"
                        b = bb
                        c = cc
                    else
                        b = view(bb, 1:n)
                        c = view(cc, 1:n, 1:2)
                    end
                end
                x = T \ b
                tx = Tfull \ b
                @test_throws DimensionMismatch ldiv!(T, Vector{elty}(undef, n+1))
                @test norm(x-tx,Inf) <= 4*condT*max(eps()*norm(tx,Inf), eps(promty)*norm(x,Inf))
                x = transpose(T) \ b
                tx = transpose(Tfull) \ b
                @test norm(x-tx,Inf) <= 4*condT*max(eps()*norm(tx,Inf), eps(promty)*norm(x,Inf))
                x = copy(transpose(b)) / T
                tx = copy(transpose(b)) / Tfull
                @test_throws DimensionMismatch rdiv!(Matrix{elty}(undef, 1, n+1), T)
                @test norm(x-tx,Inf) <= 4*condT*max(eps()*norm(tx,Inf), eps(promty)*norm(x,Inf))
                x = copy(transpose(b)) / transpose(T)
                tx = copy(transpose(b)) / transpose(Tfull)
                @test norm(x-tx,Inf) <= 4*condT*max(eps()*norm(tx,Inf), eps(promty)*norm(x,Inf))
                @testset "Generic Mat-vec ops" begin
                    @test T*b ≈ Tfull*b
                    @test T'*b ≈ Tfull'*b
                    if relty != BigFloat # not supported by pivoted QR
                        @test T/b' ≈ Tfull/b'
                    end
                end
            end
            zdv = Vector{elty}(undef, 0)
            zev = Vector{elty}(undef, 0)
            zA  = Bidiagonal(zdv, zev, :U)
            zb  = Vector{elty}(undef, 0)
            @test ldiv!(zA, zb) === zb
            @testset "linear solves with abstract matrices" begin
                diag = b[:,1]
                D = Diagonal(diag)
                x = T \ D
                tx = Tfull \ D
                @test norm(x-tx,Inf) <= 4*condT*max(eps()*norm(tx,Inf), eps(promty)*norm(x,Inf))
                x = D / T
                tx = D / Tfull
                @test norm(x-tx,Inf) <= 4*condT*max(eps()*norm(tx,Inf), eps(promty)*norm(x,Inf))
                x = transpose(T) \ D
                tx = transpose(Tfull) \ D
                @test norm(x-tx,Inf) <= 4*condT*max(eps()*norm(tx,Inf), eps(promty)*norm(x,Inf))
                x = D / transpose(T)
                tx = D / transpose(Tfull)
                @test norm(x-tx,Inf) <= 4*condT*max(eps()*norm(tx,Inf), eps(promty)*norm(x,Inf))
            end
            @testset "Specialized multiplication/division" begin
                function _bidiagdivmultest(T,
                        x,
                        typemul=T.uplo == 'U' ? UpperTriangular : Matrix,
                        typediv=T.uplo == 'U' ? UpperTriangular : Matrix,
                        typediv2=T.uplo == 'U' ? UpperTriangular : Matrix)
                    TM = Matrix(T)
                    @test (T*x)::typemul ≈  TM*x #broken=eltype(x) <: Furlong
                    @test (x*T)::typemul ≈ x*TM #broken=eltype(x) <: Furlong
                    @test (x\T)::typediv ≈ x\TM #broken=eltype(T) <: Furlong
                    @test (T/x)::typediv ≈ TM/x #broken=eltype(T) <: Furlong
                    if !isa(x, Number)
                        @test (T\x)::typediv2 ≈ TM\x #broken=eltype(x) <: Furlong
                        @test (x/T)::typediv2 ≈ x/TM #broken=eltype(x) <: Furlong
                    end
                    return nothing
                end
                A = randn(n,n)
                d = randn(n)
                dl = randn(n-1)
                t = T
                for t in (T, #=Furlong.(T)=#), (A, d, dl) in ((A, d, dl), #=(Furlong.(A), Furlong.(d), Furlong.(dl))=#)
                    _bidiagdivmultest(t, 5, Bidiagonal, Bidiagonal)
                    _bidiagdivmultest(t, 5I, Bidiagonal, Bidiagonal, t.uplo == 'U' ? UpperTriangular : LowerTriangular)
                    _bidiagdivmultest(t, Diagonal(d), Bidiagonal, Bidiagonal, t.uplo == 'U' ? UpperTriangular : LowerTriangular)
                    _bidiagdivmultest(t, UpperTriangular(A))
                    _bidiagdivmultest(t, UnitUpperTriangular(A))
                    _bidiagdivmultest(t, LowerTriangular(A), t.uplo == 'L' ? LowerTriangular : Matrix, t.uplo == 'L' ? LowerTriangular : Matrix, t.uplo == 'L' ? LowerTriangular : Matrix)
                    _bidiagdivmultest(t, UnitLowerTriangular(A), t.uplo == 'L' ? LowerTriangular : Matrix, t.uplo == 'L' ? LowerTriangular : Matrix, t.uplo == 'L' ? LowerTriangular : Matrix)
                    _bidiagdivmultest(t, Bidiagonal(d, dl, :U), Matrix, Matrix, Matrix)
                    _bidiagdivmultest(t, Bidiagonal(d, dl, :L), Matrix, Matrix, Matrix)
                end
            end
        end

        if elty <: BlasReal
            @testset "$f" for f in (floor, trunc, round, ceil)
                @test (f.(Int, T))::Bidiagonal == Bidiagonal(f.(Int, T.dv), f.(Int, T.ev), T.uplo)
                @test (f.(T))::Bidiagonal == Bidiagonal(f.(T.dv), f.(T.ev), T.uplo)
            end
        end

        @testset "diag" begin
            @test (@inferred diag(T))::typeof(dv) == dv
            @test (@inferred diag(T, uplo === :U ? 1 : -1))::typeof(dv) == ev
            @test (@inferred diag(T,2))::typeof(dv) == zeros(elty, n-2)
            @test_throws ArgumentError diag(T, -n - 1)
            @test_throws ArgumentError diag(T,  n + 1)
            # test diag with another wrapped vector type
            gdv, gev = GenericArray(dv), GenericArray(ev)
            G = Bidiagonal(gdv, gev, uplo)
            @test (@inferred diag(G))::typeof(gdv) == gdv
            @test (@inferred diag(G, uplo === :U ? 1 : -1))::typeof(gdv) == gev
            @test (@inferred diag(G,2))::typeof(gdv) == GenericArray(zeros(elty, n-2))
        end

        @testset "Eigensystems" begin
            if relty <: AbstractFloat
                d1, v1 = eigen(T)
                d2, v2 = eigen(map(elty<:Complex ? ComplexF64 : Float64,Tfull), sortby=nothing)
                @test (uplo === :U ? d1 : reverse(d1)) ≈ d2
                if elty <: Real
                    test_approx_eq_modphase(v1, uplo === :U ? v2 : v2[:,n:-1:1])
                end
            end
        end

        @testset "Singular systems" begin
            if (elty <: BlasReal)
                @test AbstractArray(svd(T)) ≈ AbstractArray(svd!(copy(Tfull)))
                @test svdvals(Tfull) ≈ svdvals(T)
                u1, d1, v1 = svd(Tfull)
                u2, d2, v2 = svd(T)
                @test d1 ≈ d2
                if elty <: Real
                    test_approx_eq_modphase(u1, u2)
                    test_approx_eq_modphase(copy(v1), copy(v2))
                end
                @test 0 ≈ norm(u2*Diagonal(d2)*v2'-Tfull) atol=n*max(n^2*eps(relty),norm(u1*Diagonal(d1)*v1'-Tfull))
                @inferred svdvals(T)
                @inferred svd(T)
            end
        end

        @testset "Binary operations" begin
            @test -T == Bidiagonal(-T.dv,-T.ev,T.uplo)
            @test convert(elty,-1.0) * T == Bidiagonal(-T.dv,-T.ev,T.uplo)
            @test T / convert(elty,-1.0) == Bidiagonal(-T.dv,-T.ev,T.uplo)
            @test T * convert(elty,-1.0) == Bidiagonal(-T.dv,-T.ev,T.uplo)
            @testset for uplo2 in (:U, :L)
                dv = convert(Vector{elty}, relty <: AbstractFloat ? randn(n) : rand(1:10, n))
                ev = convert(Vector{elty}, relty <: AbstractFloat ? randn(n-1) : rand(1:10, n-1))
                T2 = Bidiagonal(dv, ev, uplo2)
                Tfull2 = Array(T2)
                for op in (+, -, *)
                    @test Array(op(T, T2)) ≈ op(Tfull, Tfull2)
                end
            end
            # test pass-through of mul! for SymTridiagonal*Bidiagonal
            TriSym = SymTridiagonal(T.dv, T.ev)
            @test Array(TriSym*T) ≈ Array(TriSym)*Array(T)
            # test pass-through of mul! for AbstractTriangular*Bidiagonal
            Tri = UpperTriangular(diagm(1 => T.ev))
            Dia = Diagonal(T.dv)
            @test Array(Tri*T) ≈ Array(Tri)*Array(T)
            # test mul! itself for these types
            for AA in (Tri, Dia)
                for f in (identity, transpose, adjoint)
                    C = rand(elty, n, n)
                    D = copy(C) + 2.0 * Array(f(AA) * T)
                    mul!(C, f(AA), T, 2.0, 1.0) ≈ D
                end
            end
            # test mul! for BiTrySym * adjoint/transpose AbstractMat
            for f in (identity, transpose, adjoint)
                C = relty == Int ? rand(float(elty), n, n) : rand(elty, n, n)
                B = rand(elty, n, n)
                D = copy(C) + 2.0 * Array(T*f(B))
                mul!(C, T, f(B), 2.0, 1.0) ≈ D
            end

            # Issue #31870
            # Bi/Tri/Sym times Diagonal
            Diag = Diagonal(rand(elty, 10))
            BidiagU = Bidiagonal(rand(elty, 10), rand(elty, 9), 'U')
            BidiagL = Bidiagonal(rand(elty, 10), rand(elty, 9), 'L')
            Tridiag = Tridiagonal(rand(elty, 9), rand(elty, 10), rand(elty, 9))
            SymTri = SymTridiagonal(rand(elty, 10), rand(elty, 9))

            mats = Any[Diag, BidiagU, BidiagL, Tridiag, SymTri]
            for a in mats
                for b in mats
                    @test a*b ≈ Matrix(a)*Matrix(b)
                end
            end

            @test typeof(BidiagU*Diag) <: Bidiagonal
            @test typeof(BidiagL*Diag) <: Bidiagonal
            @test typeof(Tridiag*Diag) <: Tridiagonal
            @test typeof(SymTri*Diag)  <: Tridiagonal

            @test typeof(BidiagU*Diag) <: Bidiagonal
            @test typeof(Diag*BidiagL) <: Bidiagonal
            @test typeof(Diag*Tridiag) <: Tridiagonal
            @test typeof(Diag*SymTri)  <: Tridiagonal
        end

        @test inv(T)*Tfull ≈ Matrix(I, n, n)
        @test factorize(T) === T
    end
    BD = Bidiagonal(dv, ev, :U)
    @test Matrix{ComplexF64}(BD) == BD
end

# Issue 10742 and similar
let A = Bidiagonal([1,2,3], [0,0], :U)
    @test istril(A)
    @test isdiag(A)
end

# test construct from range
@test Bidiagonal(1:3, 1:2, :U) == [1 1 0; 0 2 2; 0 0 3]

@testset "promote_rule" begin
    A = Bidiagonal(fill(1f0,10),fill(1f0,9),:U)
    B = rand(Float64,10,10)
    C = Tridiagonal(rand(Float64,9),rand(Float64,10),rand(Float64,9))
    @test promote_rule(Matrix{Float64}, Bidiagonal{Float64}) == Matrix{Float64}
    @test promote(B,A) == (B, convert(Matrix{Float64}, A))
    @test promote(B,A) isa Tuple{Matrix{Float64}, Matrix{Float64}}
    @test promote(C,A) == (C,Tridiagonal(zeros(Float64,9),convert(Vector{Float64},A.dv),convert(Vector{Float64},A.ev)))
    @test promote(C,A) isa Tuple{Tridiagonal, Tridiagonal}
end

using LinearAlgebra: fillstored!, UnitLowerTriangular
@testset "fill! and fillstored!" begin
    let # fillstored!
        A = Tridiagonal(randn(2), randn(3), randn(2))
        @test fillstored!(A, 3) == Tridiagonal([3, 3], [3, 3, 3], [3, 3])
        B = Bidiagonal(randn(3), randn(2), :U)
        @test fillstored!(B, 2) == Bidiagonal([2,2,2], [2,2], :U)
        S = SymTridiagonal(randn(3), randn(2))
        @test fillstored!(S, 1) == SymTridiagonal([1,1,1], [1,1])
        Ult = UnitLowerTriangular(randn(3,3))
        @test fillstored!(Ult, 3) == UnitLowerTriangular([1 0 0; 3 1 0; 3 3 1])
    end
    let # fill!(exotic, 0)
        exotic_arrays = Any[Tridiagonal(randn(3), randn(4), randn(3)),
        Bidiagonal(randn(3), randn(2), rand([:U,:L])),
        SymTridiagonal(randn(3), randn(2)),
        Diagonal(randn(5)),
        # LowerTriangular(randn(3,3)), # AbstractTriangular fill! deprecated, see below
        # UpperTriangular(randn(3,3)) # AbstractTriangular fill! deprecated, see below
        ]
        for A in exotic_arrays
            @test iszero(fill!(A, 0))
        end

        # Diagonal fill! is no longer deprecated. See #29780
        # AbstractTriangular fill! was defined as fillstored!,
        # not matching the general behavior of fill!, and so it has been deprecated.
        # In a future dev cycle, this fill! methods should probably be reintroduced
        # with behavior matching that of fill! for other structured matrix types.
        # In the interim, equivalently test fillstored! below
        @test iszero(fillstored!(Diagonal(fill(1, 3)), 0))
        @test iszero(fillstored!(LowerTriangular(fill(1, 3, 3)), 0))
        @test iszero(fillstored!(UpperTriangular(fill(1, 3, 3)), 0))
    end
    let # fill!(small, x)
        val = randn()
        b = Bidiagonal(randn(1,1), :U)
        st = SymTridiagonal(randn(1,1))
        d = Diagonal(rand(1))
        for x in (b, st, d)
            @test Array(fill!(x, val)) == fill!(Array(x), val)
        end
        b = Bidiagonal(randn(2,2), :U)
        st = SymTridiagonal(randn(3), randn(2))
        t = Tridiagonal(randn(3,3))
        d = Diagonal(rand(3))
        for x in (b, t, st, d)
            @test_throws ArgumentError fill!(x, val)
            @test Array(fill!(x, 0)) == fill!(Array(x), 0)
        end
    end
end

@testset "pathological promotion (#24707)" begin
    @test promote_type(Matrix{Int}, Bidiagonal{Tuple{S}} where S<:Integer) <: Matrix
    @test promote_type(Matrix{Tuple{T}} where T<:Integer, Bidiagonal{Tuple{S}} where S<:Integer) <: Matrix
    @test promote_type(Matrix{Tuple{T}} where T<:Integer, Bidiagonal{Int}) <: Matrix
    @test promote_type(Tridiagonal{Int}, Bidiagonal{Tuple{S}} where S<:Integer) <: Tridiagonal
    @test promote_type(Tridiagonal{Tuple{T}} where T<:Integer, Bidiagonal{Tuple{S}} where S<:Integer) <: Tridiagonal
    @test promote_type(Tridiagonal{Tuple{T}} where T<:Integer, Bidiagonal{Int}) <: Tridiagonal
end

@testset "solve with matrix elements" begin
    A = triu(tril(randn(9, 9), 3), -3)
    b = randn(9)
    Alb = Bidiagonal(Any[tril(A[1:3,1:3]), tril(A[4:6,4:6]), tril(A[7:9,7:9])],
                     Any[triu(A[4:6,1:3]), triu(A[7:9,4:6])], 'L')
    Aub = Bidiagonal(Any[triu(A[1:3,1:3]), triu(A[4:6,4:6]), triu(A[7:9,7:9])],
                     Any[tril(A[1:3,4:6]), tril(A[4:6,7:9])], 'U')
    bb = Any[b[1:3], b[4:6], b[7:9]]
    @test vcat((Alb\bb)...) ≈ LowerTriangular(A)\b
    @test vcat((Aub\bb)...) ≈ UpperTriangular(A)\b
    Alb = Bidiagonal([tril(A[1:3,1:3]), tril(A[4:6,4:6]), tril(A[7:9,7:9])],
                     [triu(A[4:6,1:3]), triu(A[7:9,4:6])], 'L')
    Aub = Bidiagonal([triu(A[1:3,1:3]), triu(A[4:6,4:6]), triu(A[7:9,7:9])],
                     [tril(A[1:3,4:6]), tril(A[4:6,7:9])], 'U')
    d = [randn(3,3) for _ in 1:3]
    dl = [randn(3,3) for _ in 1:2]
    B = [randn(3,3) for _ in 1:3, _ in 1:3]
    for W in (UpperTriangular, LowerTriangular), t in (identity, adjoint, transpose)
        @test Matrix(t(Alb) \ W(B)) ≈ t(Alb) \ Matrix(W(B))
        @test Matrix(t(Aub) \ W(B)) ≈ t(Aub) \ Matrix(W(B))
        @test Matrix(W(B) / t(Alb)) ≈ Matrix(W(B)) / t(Alb)
        @test Matrix(W(B) / t(Aub)) ≈ Matrix(W(B)) / t(Aub)
    end
end

@testset "sum, mapreduce" begin
    Bu = Bidiagonal([1,2,3], [1,2], :U)
    Budense = Matrix(Bu)
    Bl = Bidiagonal([1,2,3], [1,2], :L)
    Bldense = Matrix(Bl)
    @test sum(Bu) == 9
    @test sum(Bl) == 9
    @test_throws ArgumentError sum(Bu, dims=0)
    @test sum(Bu, dims=1) == sum(Budense, dims=1)
    @test sum(Bu, dims=2) == sum(Budense, dims=2)
    @test sum(Bu, dims=3) == sum(Budense, dims=3)
    @test typeof(sum(Bu, dims=1)) == typeof(sum(Budense, dims=1))
    @test mapreduce(one, min, Bu, dims=1) == mapreduce(one, min, Budense, dims=1)
    @test mapreduce(one, min, Bu, dims=2) == mapreduce(one, min, Budense, dims=2)
    @test mapreduce(one, min, Bu, dims=3) == mapreduce(one, min, Budense, dims=3)
    @test typeof(mapreduce(one, min, Bu, dims=1)) == typeof(mapreduce(one, min, Budense, dims=1))
    @test mapreduce(zero, max, Bu, dims=1) == mapreduce(zero, max, Budense, dims=1)
    @test mapreduce(zero, max, Bu, dims=2) == mapreduce(zero, max, Budense, dims=2)
    @test mapreduce(zero, max, Bu, dims=3) == mapreduce(zero, max, Budense, dims=3)
    @test typeof(mapreduce(zero, max, Bu, dims=1)) == typeof(mapreduce(zero, max, Budense, dims=1))
    @test_throws ArgumentError sum(Bl, dims=0)
    @test sum(Bl, dims=1) == sum(Bldense, dims=1)
    @test sum(Bl, dims=2) == sum(Bldense, dims=2)
    @test sum(Bl, dims=3) == sum(Bldense, dims=3)
    @test typeof(sum(Bl, dims=1)) == typeof(sum(Bldense, dims=1))
    @test mapreduce(one, min, Bl, dims=1) == mapreduce(one, min, Bldense, dims=1)
    @test mapreduce(one, min, Bl, dims=2) == mapreduce(one, min, Bldense, dims=2)
    @test mapreduce(one, min, Bl, dims=3) == mapreduce(one, min, Bldense, dims=3)
    @test typeof(mapreduce(one, min, Bl, dims=1)) == typeof(mapreduce(one, min, Bldense, dims=1))
    @test mapreduce(zero, max, Bl, dims=1) == mapreduce(zero, max, Bldense, dims=1)
    @test mapreduce(zero, max, Bl, dims=2) == mapreduce(zero, max, Bldense, dims=2)
    @test mapreduce(zero, max, Bl, dims=3) == mapreduce(zero, max, Bldense, dims=3)
    @test typeof(mapreduce(zero, max, Bl, dims=1)) == typeof(mapreduce(zero, max, Bldense, dims=1))

    Bu = Bidiagonal([2], Int[], :U)
    Budense = Matrix(Bu)
    Bl = Bidiagonal([2], Int[], :L)
    Bldense = Matrix(Bl)
    @test sum(Bu) == 2
    @test sum(Bl) == 2
    @test_throws ArgumentError sum(Bu, dims=0)
    @test sum(Bu, dims=1) == sum(Budense, dims=1)
    @test sum(Bu, dims=2) == sum(Budense, dims=2)
    @test sum(Bu, dims=3) == sum(Budense, dims=3)
    @test typeof(sum(Bu, dims=1)) == typeof(sum(Budense, dims=1))
end

@testset "empty sub-diagonal" begin
    # `mul!` must use non-specialized method when sub-diagonal is empty
    A = [1 2 3 4]'
    @test A * Tridiagonal(ones(1, 1)) == A
end

@testset "generalized dot" begin
    for elty in (Float64, ComplexF64)
        dv = randn(elty, 5)
        ev = randn(elty, 4)
        x = randn(elty, 5)
        y = randn(elty, 5)
        for uplo in (:U, :L)
            B = Bidiagonal(dv, ev, uplo)
            @test dot(x, B, y) ≈ dot(B'x, y) ≈ dot(x, Matrix(B), y)
        end
        dv = Vector{elty}(undef, 0)
        ev = Vector{elty}(undef, 0)
        x = Vector{elty}(undef, 0)
        y = Vector{elty}(undef, 0)
        for uplo in (:U, :L)
            B = Bidiagonal(dv, ev, uplo)
            @test dot(x, B, y) ≈ dot(zero(elty), zero(elty), zero(elty))
        end
    end
end

@testset "multiplication of bidiagonal and triangular matrix" begin
    n = 5
    for eltyB in (Int, ComplexF64)
        if eltyB == Int
            BU = Bidiagonal(rand(1:7, n), rand(1:7, n - 1), :U)
            BL = Bidiagonal(rand(1:7, n), rand(1:7, n - 1), :L)
        else
            BU = Bidiagonal(randn(eltyB, n), randn(eltyB, n - 1), :U)
            BL = Bidiagonal(randn(eltyB, n), randn(eltyB, n - 1), :L)
        end
        for eltyT in (Int, ComplexF64)
            for TriT in (LowerTriangular, UnitLowerTriangular, UpperTriangular, UnitUpperTriangular)
                if eltyT == Int
                    T = TriT(rand(1:7, n, n))
                else
                    T = TriT(randn(eltyT, n, n))
                end
                for B in (BU, BL)
                    MB = Matrix(B)
                    MT = Matrix(T)
                    for transB in (identity, adjoint, transpose), transT in (identity, adjoint, transpose)
                        @test transB(B) * transT(T) ≈ transB(MB) * transT(MT)
                        @test transT(T) * transB(B) ≈ transT(MT) * transB(MB)
                    end
                end
            end
        end
    end
end

struct MyNotANumberType
    n::Float64
end
Base.zero(n::MyNotANumberType)      = MyNotANumberType(zero(Float64))
Base.zero(T::Type{MyNotANumberType}) = MyNotANumberType(zero(Float64))
Base.copy(n::MyNotANumberType)      = MyNotANumberType(copy(n.n))
Base.transpose(n::MyNotANumberType) = n

@testset "transpose for a non-numeric eltype" begin
    @test !(MyNotANumberType(1.0) isa Number)
    a = [MyNotANumberType(1.0), MyNotANumberType(2.0), MyNotANumberType(3.0)]
    b = [MyNotANumberType(5.0), MyNotANumberType(6.0)]
    B = Bidiagonal(a, b, :U)
    tB = transpose(B)
    @test tB == Bidiagonal(a, b, :L)
    @test transpose(copy(tB)) == B
end

@testset "empty bidiagonal matrices" begin
    dv0 = zeros(0)
    ev0 = zeros(0)
    zm = zeros(0, 0)
    ubd = Bidiagonal(dv0, ev0, :U)
    lbd = Bidiagonal(dv0, ev0, :L)
    @test size(ubd) == (0, 0)
    @test_throws BoundsError getindex(ubd, 1, 1)
    @test_throws BoundsError setindex!(ubd, 0.0, 1, 1)
    @test similar(ubd) == ubd
    @test similar(lbd, Int) == zeros(Int, 0, 0)
    @test ubd == zm
    @test lbd == zm
    @test ubd == lbd
    @test ubd * ubd == ubd
    @test lbd + lbd == lbd
    @test lbd' == ubd
    @test ubd' == lbd
    @test triu(ubd, 1) == ubd
    @test triu(lbd, 1) == ubd
    @test tril(ubd, -1) == ubd
    @test tril(lbd, -1) == ubd
    @test_throws ArgumentError triu(ubd)
    @test_throws ArgumentError tril(ubd)
    @test sum(ubd) == 0.0
    @test reduce(+, ubd) == 0.0
    @test reduce(+, ubd, dims=1) == zeros(1, 0)
    @test reduce(+, ubd, dims=2) == zeros(0, 1)
    @test hcat(ubd, ubd) == zm
    @test vcat(ubd, lbd) == zm
    @test hcat(lbd, ones(0, 3)) == ones(0, 3)
    @test fill!(copy(ubd), 1.0) == ubd
    @test map(abs, ubd) == zm
    @test lbd .+ 1 == zm
    @test lbd + ubd isa Bidiagonal
    @test lbd .+ ubd isa Bidiagonal
    @test ubd * 5 == ubd
    @test ubd .* 3 == ubd
end

@testset "non-commutative algebra (#39701)" begin
    A = Bidiagonal(Quaternion.(randn(5), randn(5), randn(5), randn(5)), Quaternion.(randn(4), randn(4), randn(4), randn(4)), :U)
    c = Quaternion(1,2,3,4)
    @test A * c ≈ Matrix(A) * c
    @test A / c ≈ Matrix(A) / c
    @test c * A ≈ c * Matrix(A)
    @test c \ A ≈ c \ Matrix(A)
end

isdefined(Main, :ImmutableArrays) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "ImmutableArrays.jl"))
using .Main.ImmutableArrays

@testset "Conversion to AbstractArray" begin
    # tests corresponding to #34995
    dv = ImmutableArray([1, 2, 3, 4])
    ev = ImmutableArray([7, 8, 9])
    Bu = Bidiagonal(dv, ev, :U)
    Bl = Bidiagonal(dv, ev, :L)

    @test convert(AbstractArray{Float64}, Bu)::Bidiagonal{Float64,ImmutableArray{Float64,1,Array{Float64,1}}} == Bu
    @test convert(AbstractMatrix{Float64}, Bu)::Bidiagonal{Float64,ImmutableArray{Float64,1,Array{Float64,1}}} == Bu
    @test convert(AbstractArray{Float64}, Bl)::Bidiagonal{Float64,ImmutableArray{Float64,1,Array{Float64,1}}} == Bl
    @test convert(AbstractMatrix{Float64}, Bl)::Bidiagonal{Float64,ImmutableArray{Float64,1,Array{Float64,1}}} == Bl
end

@testset "block-bidiagonal matrix indexing" begin
    dv = [ones(4,3), ones(2,2).*2, ones(2,3).*3, ones(4,4).*4]
    evu = [ones(4,2), ones(2,3).*2, ones(2,4).*3]
    evl = [ones(2,3), ones(2,2).*2, ones(4,3).*3]
    BU = Bidiagonal(dv, evu, :U)
    BL = Bidiagonal(dv, evl, :L)
    # check that all the matrices along a column have the same number of columns,
    # and the matrices along a row have the same number of rows
    for j in axes(BU, 2), i in 2:size(BU, 1)
        @test size(BU[i,j], 2) == size(BU[1,j], 2)
        @test size(BU[i,j], 1) == size(BU[i,1], 1)
        if j < i || j > i + 1
            @test iszero(BU[i,j])
        end
    end
    for j in axes(BL, 2), i in 2:size(BL, 1)
        @test size(BL[i,j], 2) == size(BL[1,j], 2)
        @test size(BL[i,j], 1) == size(BL[i,1], 1)
        if j < i-1 || j > i
            @test iszero(BL[i,j])
        end
    end
end

end # module TestBidiagonal

# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test
import Base.LinAlg: BlasReal, BlasFloat

n = 10 #Size of test matrix
srand(1)

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

    @testset "Constructors" begin
        for (x, y) in ((dv, ev), (GenericArray(dv), GenericArray(ev)))
            # from vectors
            ubd = Bidiagonal(x, y, :U)
            lbd = Bidiagonal(x, y, :L)
            @test ubd != lbd
            @test ubd.dv === x
            @test lbd.ev === y
            @test_throws ArgumentError Bidiagonal(x, y, :R)
            @test_throws DimensionMismatch Bidiagonal(x, x, :U)
            @test_throws MethodError Bidiagonal(x, y)
            # from matrix
            @test Bidiagonal(ubd, :U) == Bidiagonal(Matrix(ubd), :U) == ubd
            @test Bidiagonal(lbd, :L) == Bidiagonal(Matrix(lbd), :L) == lbd
        end
        # enable when deprecations for 0.7 are dropped
        # @test_throws MethodError Bidiagonal(dv, GenericArray(ev), :U)
        # @test_throws MethodError Bidiagonal(GenericArray(dv), ev, :U)
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
        @test isa(similar(ubd, Int), Bidiagonal{Int})
        @test isa(similar(ubd, Int, (3, 2)), Matrix{Int})
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
            @test Array(T) == diagm(dv) + diagm(ev, uplo == :U ? 1 : -1)
            @test Bidiagonal(Array(T), uplo) == T
            @test big.(T) == T
            @test Array(abs.(T)) == abs.(diagm(dv)) + abs.(diagm(ev, uplo == :U ? 1 : -1))
            @test Array(real(T)) == real(diagm(dv)) + real(diagm(ev, uplo == :U ? 1 : -1))
            @test Array(imag(T)) == imag(diagm(dv)) + imag(diagm(ev, uplo == :U ? 1 : -1))
        end

        @testset for func in (conj, transpose, adjoint)
            @test func(func(T)) == T
        end

        @testset "triu and tril" begin
            bidiagcopy(dv, ev, uplo) = Bidiagonal(copy(dv), copy(ev), uplo)
            @test istril(Bidiagonal(dv,ev,:L))
            @test !istril(Bidiagonal(dv,ev,:U))
            @test tril!(bidiagcopy(dv,ev,:U),-1) == Bidiagonal(zeros(dv),zeros(ev),:U)
            @test tril!(bidiagcopy(dv,ev,:L),-1) == Bidiagonal(zeros(dv),ev,:L)
            @test tril!(bidiagcopy(dv,ev,:U),-2) == Bidiagonal(zeros(dv),zeros(ev),:U)
            @test tril!(bidiagcopy(dv,ev,:L),-2) == Bidiagonal(zeros(dv),zeros(ev),:L)
            @test tril!(bidiagcopy(dv,ev,:U),1)  == Bidiagonal(dv,ev,:U)
            @test tril!(bidiagcopy(dv,ev,:L),1)  == Bidiagonal(dv,ev,:L)
            @test tril!(bidiagcopy(dv,ev,:U))    == Bidiagonal(dv,zeros(ev),:U)
            @test tril!(bidiagcopy(dv,ev,:L))    == Bidiagonal(dv,ev,:L)
            @test_throws ArgumentError tril!(bidiagcopy(dv, ev, :U), -n - 2)
            @test_throws ArgumentError tril!(bidiagcopy(dv, ev, :U), n)

            @test istriu(Bidiagonal(dv,ev,:U))
            @test !istriu(Bidiagonal(dv,ev,:L))
            @test triu!(bidiagcopy(dv,ev,:L),1)  == Bidiagonal(zeros(dv),zeros(ev),:L)
            @test triu!(bidiagcopy(dv,ev,:U),1)  == Bidiagonal(zeros(dv),ev,:U)
            @test triu!(bidiagcopy(dv,ev,:U),2)  == Bidiagonal(zeros(dv),zeros(ev),:U)
            @test triu!(bidiagcopy(dv,ev,:L),2)  == Bidiagonal(zeros(dv),zeros(ev),:L)
            @test triu!(bidiagcopy(dv,ev,:U),-1) == Bidiagonal(dv,ev,:U)
            @test triu!(bidiagcopy(dv,ev,:L),-1) == Bidiagonal(dv,ev,:L)
            @test triu!(bidiagcopy(dv,ev,:L))    == Bidiagonal(dv,zeros(ev),:L)
            @test triu!(bidiagcopy(dv,ev,:U))    == Bidiagonal(dv,ev,:U)
            @test_throws ArgumentError triu!(bidiagcopy(dv, ev, :U), -n)
            @test_throws ArgumentError triu!(bidiagcopy(dv, ev, :U), n + 2)
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
            condT = cond(map(Complex128,Tfull))
            promty = typeof((zero(relty)*zero(relty) + zero(relty)*zero(relty))/one(relty))
            if relty != BigFloat
                x = T.'\c.'
                tx = Tfull.' \ c.'
                elty <: AbstractFloat && @test norm(x-tx,Inf) <= 4*condT*max(eps()*norm(tx,Inf), eps(promty)*norm(x,Inf))
                @test_throws DimensionMismatch T.'\b.'
                x = T'\c.'
                tx = Tfull' \ c.'
                @test norm(x-tx,Inf) <= 4*condT*max(eps()*norm(tx,Inf), eps(promty)*norm(x,Inf))
                @test_throws DimensionMismatch T'\b.'
                x = T\c.'
                tx = Tfull\c.'
                @test norm(x-tx,Inf) <= 4*condT*max(eps()*norm(tx,Inf), eps(promty)*norm(x,Inf))
                @test_throws DimensionMismatch T\b.'
            end
            @test_throws DimensionMismatch T \ ones(elty,n+1,2)
            @test_throws DimensionMismatch T.' \ ones(elty,n+1,2)
            @test_throws DimensionMismatch T' \ ones(elty,n+1,2)

            @test_throws DimensionMismatch T \ RowVector(ones(elty,n+1))
            @test_throws DimensionMismatch T.' \ RowVector(ones(elty,n+1))
            @test_throws DimensionMismatch T' \ RowVector(ones(elty,n+1))
            @test_throws DimensionMismatch Base.LinAlg.At_ldiv_B(T, RowVector(ones(elty,n+1)))
            @test_throws DimensionMismatch Base.LinAlg.Ac_ldiv_B(T, RowVector(ones(elty,n+1)))
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
                @test_throws DimensionMismatch Base.LinAlg.naivesub!(T,ones(elty,n+1))
                @test norm(x-tx,Inf) <= 4*condT*max(eps()*norm(tx,Inf), eps(promty)*norm(x,Inf))
                @testset "Generic Mat-vec ops" begin
                    @test T*b ≈ Tfull*b
                    @test T'*b ≈ Tfull'*b
                    if relty != BigFloat # not supported by pivoted QR
                        @test T/b' ≈ Tfull/b'
                    end
                end
                # test DimensionMismatch for RowVectors
                @test_throws DimensionMismatch T \ b'
                @test_throws DimensionMismatch T.' \ b'
                @test_throws DimensionMismatch T' \ b'
            end
        end

        if elty <: BlasReal
            @testset "$f" for f in (floor, trunc, round, ceil)
                @test (f.(Int, T))::Bidiagonal == Bidiagonal(f.(Int, T.dv), f.(Int, T.ev), T.uplo)
                @test (f.(T))::Bidiagonal == Bidiagonal(f.(T.dv), f.(T.ev), T.uplo)
            end
        end

        @testset "Diagonals" begin
            @test diag(T,2) == zeros(elty, n-2)
            @test_throws ArgumentError diag(T, -n - 1)
            @test_throws ArgumentError diag(T, n + 1)
        end

        @testset "Eigensystems" begin
            if relty <: AbstractFloat
                d1, v1 = eig(T)
                d2, v2 = eig(map(elty<:Complex ? Complex128 : Float64,Tfull))
                @test (uplo == :U ? d1 : reverse(d1)) ≈ d2
                if elty <: Real
                    Test.test_approx_eq_modphase(v1, uplo == :U ? v2 : v2[:,n:-1:1])
                end
            end
        end

        @testset "Singular systems" begin
            if (elty <: BlasReal)
                @test AbstractArray(svdfact(T)) ≈ AbstractArray(svdfact!(copy(Tfull)))
                @test svdvals(Tfull) ≈ svdvals(T)
                u1, d1, v1 = svd(Tfull)
                u2, d2, v2 = svd(T)
                @test d1 ≈ d2
                if elty <: Real
                    Test.test_approx_eq_modphase(u1, u2)
                    Test.test_approx_eq_modphase(v1, v2)
                end
                @test 0 ≈ vecnorm(u2*diagm(d2)*v2'-Tfull) atol=n*max(n^2*eps(relty),vecnorm(u1*diagm(d1)*v1'-Tfull))
                @inferred svdvals(T)
                @inferred svd(T)
            end
        end

        @testset "Binary operations" begin
            @test -T == Bidiagonal(-T.dv,-T.ev,T.uplo)
            @test convert(elty,-1.0) * T == Bidiagonal(-T.dv,-T.ev,T.uplo)
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
            # test pass-through of A_mul_B! for SymTridiagonal*Bidiagonal
            TriSym = SymTridiagonal(T.dv, T.ev)
            @test Array(TriSym*T) ≈ Array(TriSym)*Array(T)
            # test pass-through of A_mul_B! for AbstractTriangular*Bidiagonal
            Tri = UpperTriangular(diagm(T.ev, 1))
            @test Array(Tri*T) ≈ Array(Tri)*Array(T)
        end

        @test inv(T)*Tfull ≈ eye(elty,n)
    end
    BD = Bidiagonal(dv, ev, :U)
    @test Matrix{Complex{Float64}}(BD) == BD
end

# Issue 10742 and similar
let A = Bidiagonal([1,2,3], [0,0], :U)
    @test istril(A)
    @test isdiag(A)
end

# test construct from range
@test Bidiagonal(1:3, 1:2, :U) == [1 1 0; 0 2 2; 0 0 3]

@testset "promote_rule" begin
    A = Bidiagonal(ones(Float32,10),ones(Float32,9),:U)
    B = rand(Float64,10,10)
    C = Tridiagonal(rand(Float64,9),rand(Float64,10),rand(Float64,9))
    @test promote_rule(Matrix{Float64}, Bidiagonal{Float64}) == Matrix{Float64}
    @test promote(B,A) == (B, convert(Matrix{Float64}, A))
    @test promote(B,A) isa Tuple{Matrix{Float64}, Matrix{Float64}}
    @test promote(C,A) == (C,Tridiagonal(zeros(Float64,9),convert(Vector{Float64},A.dv),convert(Vector{Float64},A.ev)))
    @test promote(C,A) isa Tuple{Tridiagonal, Tridiagonal}
end

import Base.LinAlg: fillslots!, UnitLowerTriangular
@testset "fill! and fillslots!" begin
    let #fill!
        let # fillslots!
            A = Tridiagonal(randn(2), randn(3), randn(2))
            @test fillslots!(A, 3) == Tridiagonal([3, 3.], [3, 3, 3.], [3, 3.])
            B = Bidiagonal(randn(3), randn(2), :U)
            @test fillslots!(B, 2) == Bidiagonal([2.,2,2], [2,2.], :U)
            S = SymTridiagonal(randn(3), randn(2))
            @test fillslots!(S, 1) == SymTridiagonal([1,1,1.], [1,1.])
            Ult = UnitLowerTriangular(randn(3,3))
            @test fillslots!(Ult, 3) == UnitLowerTriangular([1 0 0; 3 1 0; 3 3 1])
        end
        let # fill!(exotic, 0)
            exotic_arrays = Any[Tridiagonal(randn(3), randn(4), randn(3)),
            Bidiagonal(randn(3), randn(2), rand([:U,:L])),
            SymTridiagonal(randn(3), randn(2)),
            sparse(randn(3,4)),
            Diagonal(randn(5)),
            sparse(rand(3)),
            LowerTriangular(randn(3,3)),
            UpperTriangular(randn(3,3))
            ]
            for A in exotic_arrays
                fill!(A, 0)
                for a in A
                    @test a == 0
                end
            end
        end
        let # fill!(small, x)
            val = randn()
            b = Bidiagonal(randn(1,1), :U)
            st = SymTridiagonal(randn(1,1))
            for x in (b, st)
                @test Array(fill!(x, val)) == fill!(Array(x), val)
            end
            b = Bidiagonal(randn(2,2), :U)
            st = SymTridiagonal(randn(3), randn(2))
            t = Tridiagonal(randn(3,3))
            for x in (b, t, st)
                @test_throws ArgumentError fill!(x, val)
                @test Array(fill!(x, 0)) == fill!(Array(x), 0)
            end
        end
    end
end

# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestBidiagonal

using Test, LinearAlgebra, SparseArrays, Random
using LinearAlgebra: BlasReal, BlasFloat

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
        @test eltype(Bidiagonal{elty}([1,2,3,4], [1.0f0,2.0f0,3.0f0], :U)) == elty
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
        @test isa(similar(ubd, (3, 2)), SparseMatrixCSC)
        @test isa(similar(ubd, Int, (3, 2)), SparseMatrixCSC{Int})
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
            @test Array(T) == diagm(0 => dv, (uplo == :U ? 1 : -1) => ev)
            @test Bidiagonal(Array(T), uplo) == T
            @test big.(T) == T
            @test Array(abs.(T)) == abs.(diagm(0 => dv, (uplo == :U ? 1 : -1) => ev))
            @test Array(real(T)) == real(diagm(0 => dv, (uplo == :U ? 1 : -1) => ev))
            @test Array(imag(T)) == imag(diagm(0 => dv, (uplo == :U ? 1 : -1) => ev))
        end

        @testset for func in (conj, transpose, adjoint)
            @test func(func(T)) == T
        end

        @testset "triu and tril" begin
            zerosdv = zeros(elty, length(dv))
            zerosev = zeros(elty, length(ev))
            bidiagcopy(dv, ev, uplo) = Bidiagonal(copy(dv), copy(ev), uplo)

            @test istril(Bidiagonal(dv,ev,:L))
            @test !istril(Bidiagonal(dv,ev,:U))
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
            @test !istriu(Bidiagonal(dv,ev,:L))
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
                @test_throws DimensionMismatch LinearAlgebra.naivesub!(T,Vector{elty}(undef,n+1))
                @test norm(x-tx,Inf) <= 4*condT*max(eps()*norm(tx,Inf), eps(promty)*norm(x,Inf))
                @testset "Generic Mat-vec ops" begin
                    @test T*b ≈ Tfull*b
                    @test T'*b ≈ Tfull'*b
                    if relty != BigFloat # not supported by pivoted QR
                        @test T/b' ≈ Tfull/b'
                    end
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
            @test (@inferred diag(T, uplo == :U ? 1 : -1))::typeof(dv) == ev
            @test (@inferred diag(T,2))::typeof(dv) == zeros(elty, n-2)
            @test_throws ArgumentError diag(T, -n - 1)
            @test_throws ArgumentError diag(T,  n + 1)
            # test diag with another wrapped vector type
            gdv, gev = GenericArray(dv), GenericArray(ev)
            G = Bidiagonal(gdv, gev, uplo)
            @test (@inferred diag(G))::typeof(gdv) == gdv
            @test (@inferred diag(G, uplo == :U ? 1 : -1))::typeof(gdv) == gev
            @test (@inferred diag(G,2))::typeof(gdv) == GenericArray(zeros(elty, n-2))
        end

        @testset "Eigensystems" begin
            if relty <: AbstractFloat
                d1, v1 = eigen(T)
                d2, v2 = eigen(map(elty<:Complex ? ComplexF64 : Float64,Tfull))
                @test (uplo == :U ? d1 : reverse(d1)) ≈ d2
                if elty <: Real
                    test_approx_eq_modphase(v1, uplo == :U ? v2 : v2[:,n:-1:1])
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
            @test Array(Tri*T) ≈ Array(Tri)*Array(T)
        end

        @test inv(T)*Tfull ≈ Matrix(I, n, n)
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
        sparse(randn(3,4)),
        # Diagonal(randn(5)), # Diagonal fill! deprecated, see below
        sparse(rand(3)),
        # LowerTriangular(randn(3,3)), # AbstractTriangular fill! deprecated, see below
        # UpperTriangular(randn(3,3)) # AbstractTriangular fill! deprecated, see below
        ]
        for A in exotic_arrays
            @test iszero(fill!(A, 0))
        end
        # Diagonal and AbstractTriangular fill! were defined as fillstored!,
        # not matching the general behavior of fill!, and so have been deprecated.
        # In a future dev cycle, these fill! methods should probably be reintroduced
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
end

end # module TestBidiagonal

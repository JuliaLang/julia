# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestTridiagonal

using Test, LinearAlgebra, Random

const BASE_TEST_PATH = joinpath(Sys.BINDIR, "..", "share", "julia", "test")

isdefined(Main, :Quaternions) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "Quaternions.jl"))
using .Main.Quaternions

isdefined(Main, :InfiniteArrays) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "InfiniteArrays.jl"))
using .Main.InfiniteArrays

isdefined(Main, :FillArrays) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "FillArrays.jl"))
using .Main.FillArrays

isdefined(Main, :OffsetArrays) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "OffsetArrays.jl"))
using .Main.OffsetArrays

include("testutils.jl") # test_approx_eq_modphase

#Test equivalence of eigenvectors/singular vectors taking into account possible phase (sign) differences
function test_approx_eq_vecs(a::StridedVecOrMat{S}, b::StridedVecOrMat{T}, error=nothing) where {S<:Real,T<:Real}
    n = size(a, 1)
    @test n==size(b,1) && size(a,2)==size(b,2)
    error===nothing && (error=n^3*(eps(S)+eps(T)))
    for i=1:n
        ev1, ev2 = a[:,i], b[:,i]
        deviation = min(abs(norm(ev1-ev2)),abs(norm(ev1+ev2)))
        if !isnan(deviation)
            @test deviation ≈ 0.0 atol=error
        end
    end
end

@testset for elty in (Float32, Float64, ComplexF32, ComplexF64, Int)
    n = 12 #Size of matrix problem to test
    Random.seed!(123)
    if elty == Int
        Random.seed!(61516384)
        d = rand(1:100, n)
        dl = -rand(0:10, n-1)
        du = -rand(0:10, n-1)
        v = rand(1:100, n)
        B = rand(1:100, n, 2)
        a = rand(1:100, n-1)
        b = rand(1:100, n)
        c = rand(1:100, n-1)
    else
        d = convert(Vector{elty}, 1 .+ randn(n))
        dl = convert(Vector{elty}, randn(n - 1))
        du = convert(Vector{elty}, randn(n - 1))
        v = convert(Vector{elty}, randn(n))
        B = convert(Matrix{elty}, randn(n, 2))
        a = convert(Vector{elty}, randn(n - 1))
        b = convert(Vector{elty}, randn(n))
        c = convert(Vector{elty}, randn(n - 1))
        if elty <: Complex
            a += im*convert(Vector{elty}, randn(n - 1))
            b += im*convert(Vector{elty}, randn(n))
            c += im*convert(Vector{elty}, randn(n - 1))
        end
    end
    @test_throws DimensionMismatch SymTridiagonal(dl, fill(elty(1), n+1))
    @test_throws ArgumentError SymTridiagonal(rand(n, n))
    @test_throws ArgumentError Tridiagonal(dl, dl, dl)
    @test_throws ArgumentError convert(SymTridiagonal{elty}, Tridiagonal(dl, d, du))

    if elty != Int
        @testset "issue #1490" begin
            @test det(fill(elty(1),3,3)) ≈ zero(elty) atol=3*eps(real(one(elty)))
            @test det(SymTridiagonal(elty[],elty[])) == one(elty)
        end
    end

    @testset "constructor" begin
        for (x, y) in ((d, dl), (GenericArray(d), GenericArray(dl)))
            ST = (SymTridiagonal(x, y))::SymTridiagonal{elty, typeof(x)}
            @test ST == Matrix(ST)
            @test ST.dv === x
            @test ST.ev === y
            @test typeof(ST)(ST) === ST
            TT = (Tridiagonal(y, x, y))::Tridiagonal{elty, typeof(x)}
            @test TT == Matrix(TT)
            @test TT.dl === y
            @test TT.d  === x
            @test TT.du == y
            @test typeof(TT)(TT) === TT
        end
        ST = SymTridiagonal{elty}([1,2,3,4], [1,2,3])
        @test eltype(ST) == elty
        @test SymTridiagonal{elty, Vector{elty}}(ST) === ST
        @test SymTridiagonal{Int64, Vector{Int64}}(ST) isa SymTridiagonal{Int64, Vector{Int64}}
        TT = Tridiagonal{elty}([1,2,3], [1,2,3,4], [1,2,3])
        @test eltype(TT) == elty
        ST = SymTridiagonal{elty,Vector{elty}}(d, GenericArray(dl))
        @test isa(ST, SymTridiagonal{elty,Vector{elty}})
        TT = Tridiagonal{elty,Vector{elty}}(GenericArray(dl), d, GenericArray(dl))
        @test isa(TT, Tridiagonal{elty,Vector{elty}})
        @test_throws ArgumentError SymTridiagonal(d, GenericArray(dl))
        @test_throws ArgumentError SymTridiagonal(GenericArray(d), dl)
        @test_throws ArgumentError Tridiagonal(GenericArray(dl), d, GenericArray(dl))
        @test_throws ArgumentError Tridiagonal(dl, GenericArray(d), dl)
        @test_throws ArgumentError SymTridiagonal{elty}(d, GenericArray(dl))
        @test_throws ArgumentError Tridiagonal{elty}(GenericArray(dl), d,GenericArray(dl))
        STI = SymTridiagonal([1,2,3,4], [1,2,3])
        TTI = Tridiagonal([1,2,3], [1,2,3,4], [1,2,3])
        TTI2 = Tridiagonal([1,2,3], [1,2,3,4], [1,2,3], [1,2])
        @test SymTridiagonal(STI) === STI
        @test Tridiagonal(TTI)    === TTI
        @test Tridiagonal(TTI2)   === TTI2
        @test isa(SymTridiagonal{elty}(STI), SymTridiagonal{elty})
        @test isa(Tridiagonal{elty}(TTI), Tridiagonal{elty})
        TTI2y = Tridiagonal{elty}(TTI2)
        @test isa(TTI2y, Tridiagonal{elty})
        @test TTI2y.du2 == convert(Vector{elty}, [1,2])
    end
    @testset "interconversion of Tridiagonal and SymTridiagonal" begin
        @test Tridiagonal(dl, d, dl) == SymTridiagonal(d, dl)
        @test SymTridiagonal(d, dl) == Tridiagonal(dl, d, dl)
        @test Tridiagonal(dl, d, du) + Tridiagonal(du, d, dl) == SymTridiagonal(2d, dl+du)
        @test SymTridiagonal(d, dl) + Tridiagonal(dl, d, du) == Tridiagonal(dl + dl, d+d, dl+du)
        @test convert(SymTridiagonal,Tridiagonal(SymTridiagonal(d, dl))) == SymTridiagonal(d, dl)
        @test Array(convert(SymTridiagonal{ComplexF32},Tridiagonal(SymTridiagonal(d, dl)))) == convert(Matrix{ComplexF32}, SymTridiagonal(d, dl))
    end
    @testset "tril/triu" begin
        zerosd = fill!(similar(d), 0)
        zerosdl = fill!(similar(dl), 0)
        zerosdu = fill!(similar(du), 0)
        @test_throws ArgumentError tril!(SymTridiagonal(d, dl), -n - 2)
        @test_throws ArgumentError tril!(SymTridiagonal(d, dl), n)
        @test_throws ArgumentError tril!(Tridiagonal(dl, d, du), -n - 2)
        @test_throws ArgumentError tril!(Tridiagonal(dl, d, du), n)
        @test tril(SymTridiagonal(d,dl))    == Tridiagonal(dl,d,zerosdl)
        @test tril(SymTridiagonal(d,dl),1)  == Tridiagonal(dl,d,dl)
        @test tril(SymTridiagonal(d,dl),-1) == Tridiagonal(dl,zerosd,zerosdl)
        @test tril(SymTridiagonal(d,dl),-2) == Tridiagonal(zerosdl,zerosd,zerosdl)
        @test tril(Tridiagonal(dl,d,du))    == Tridiagonal(dl,d,zerosdu)
        @test tril(Tridiagonal(dl,d,du),1)  == Tridiagonal(dl,d,du)
        @test tril(Tridiagonal(dl,d,du),-1) == Tridiagonal(dl,zerosd,zerosdu)
        @test tril(Tridiagonal(dl,d,du),-2) == Tridiagonal(zerosdl,zerosd,zerosdu)

        @test_throws ArgumentError triu!(SymTridiagonal(d, dl), -n)
        @test_throws ArgumentError triu!(SymTridiagonal(d, dl), n + 2)
        @test_throws ArgumentError triu!(Tridiagonal(dl, d, du), -n)
        @test_throws ArgumentError triu!(Tridiagonal(dl, d, du), n + 2)
        @test triu(SymTridiagonal(d,dl))    == Tridiagonal(zerosdl,d,dl)
        @test triu(SymTridiagonal(d,dl),-1) == Tridiagonal(dl,d,dl)
        @test triu(SymTridiagonal(d,dl),1)  == Tridiagonal(zerosdl,zerosd,dl)
        @test triu(SymTridiagonal(d,dl),2)  == Tridiagonal(zerosdl,zerosd,zerosdl)
        @test triu(Tridiagonal(dl,d,du))    == Tridiagonal(zerosdl,d,du)
        @test triu(Tridiagonal(dl,d,du),-1) == Tridiagonal(dl,d,du)
        @test triu(Tridiagonal(dl,d,du),1)  == Tridiagonal(zerosdl,zerosd,du)
        @test triu(Tridiagonal(dl,d,du),2)  == Tridiagonal(zerosdl,zerosd,zerosdu)

        @test !istril(SymTridiagonal(d,dl))
        @test istril(SymTridiagonal(d,zerosdl))
        @test !istril(SymTridiagonal(d,dl),-2)
        @test !istriu(SymTridiagonal(d,dl))
        @test istriu(SymTridiagonal(d,zerosdl))
        @test !istriu(SymTridiagonal(d,dl),2)
        @test istriu(Tridiagonal(zerosdl,d,du))
        @test !istriu(Tridiagonal(dl,d,zerosdu))
        @test istriu(Tridiagonal(zerosdl,zerosd,du),1)
        @test !istriu(Tridiagonal(dl,d,zerosdu),2)
        @test istril(Tridiagonal(dl,d,zerosdu))
        @test !istril(Tridiagonal(zerosdl,d,du))
        @test istril(Tridiagonal(dl,zerosd,zerosdu),-1)
        @test !istril(Tridiagonal(dl,d,zerosdu),-2)

        @test isdiag(SymTridiagonal(d,zerosdl))
        @test !isdiag(SymTridiagonal(d,dl))
        @test isdiag(Tridiagonal(zerosdl,d,zerosdu))
        @test !isdiag(Tridiagonal(dl,d,zerosdu))
        @test !isdiag(Tridiagonal(zerosdl,d,du))
        @test !isdiag(Tridiagonal(dl,d,du))

        # Test methods that could fail due to dv and ev having the same length
        # see #41089

        badev = zero(d)
        badev[end] = 1
        S = SymTridiagonal(d, badev)

        @test istriu(S, -2)
        @test istriu(S, 0)
        @test !istriu(S, 2)

        @test isdiag(S)
    end

    @testset "iszero and isone" begin
        Tzero = Tridiagonal(zeros(elty, 9), zeros(elty, 10), zeros(elty, 9))
        Tone = Tridiagonal(zeros(elty, 9), ones(elty, 10), zeros(elty, 9))
        Tmix = Tridiagonal(zeros(elty, 9), zeros(elty, 10), zeros(elty, 9))
        Tmix[end, end] = one(elty)

        Szero = SymTridiagonal(zeros(elty, 10), zeros(elty, 9))
        Sone = SymTridiagonal(ones(elty, 10), zeros(elty, 9))
        Smix = SymTridiagonal(zeros(elty, 10), zeros(elty, 9))
        Smix[end, end] = one(elty)

        @test iszero(Tzero)
        @test !isone(Tzero)
        @test !iszero(Tone)
        @test isone(Tone)
        @test !iszero(Tmix)
        @test !isone(Tmix)

        @test iszero(Szero)
        @test !isone(Szero)
        @test !iszero(Sone)
        @test isone(Sone)
        @test !iszero(Smix)
        @test !isone(Smix)

        badev = zeros(elty, 3)
        badev[end] = 1

        @test isone(SymTridiagonal(ones(elty, 3), badev))
        @test iszero(SymTridiagonal(zeros(elty, 3), badev))
    end

    @testset for mat_type in (Tridiagonal, SymTridiagonal)
        A = mat_type == Tridiagonal ? mat_type(dl, d, du) : mat_type(d, dl)
        fA = map(elty <: Complex ? ComplexF64 : Float64, Array(A))
        @testset "similar, size, and copyto!" begin
            B = similar(A)
            @test size(B) == size(A)
            copyto!(B, A)
            @test B == A
            @test isa(similar(A), mat_type{elty})
            @test isa(similar(A, Int), mat_type{Int})
            @test isa(similar(A, (3, 2)), Matrix)
            @test isa(similar(A, Int, (3, 2)), Matrix{Int})
            @test size(A, 3) == 1
            @test size(A, 1) == n
            @test size(A) == (n, n)
            @test_throws BoundsError size(A, 0)
        end
        @testset "getindex" begin
            @test_throws BoundsError A[n + 1, 1]
            @test_throws BoundsError A[1, n + 1]
            @test A[1, n] == convert(elty, 0.0)
            @test A[1, 1] == d[1]
        end
        @testset "setindex!" begin
            @test_throws BoundsError A[n + 1, 1] = 0 # test bounds check
            @test_throws BoundsError A[1, n + 1] = 0 # test bounds check
            @test_throws ArgumentError A[1, 3]   = 1 # test assignment off the main/sub/super diagonal
            if mat_type == Tridiagonal
                @test (A[3, 3] = A[3, 3]; A == fA) # test assignment on the main diagonal
                @test (A[3, 2] = A[3, 2]; A == fA) # test assignment on the subdiagonal
                @test (A[2, 3] = A[2, 3]; A == fA) # test assignment on the superdiagonal
                @test ((A[1, 3] = 0) == 0; A == fA) # test zero assignment off the main/sub/super diagonal
            else # mat_type is SymTridiagonal
                @test ((A[3, 3] = A[3, 3]) == A[3, 3]; A == fA) # test assignment on the main diagonal
                @test_throws ArgumentError A[3, 2] = 1 # test assignment on the subdiagonal
                @test_throws ArgumentError A[2, 3] = 1 # test assignment on the superdiagonal
            end
        end
        @testset "diag" begin
            @test (@inferred diag(A))::typeof(d) == d
            @test (@inferred diag(A, 0))::typeof(d) == d
            @test (@inferred diag(A, 1))::typeof(d) == (mat_type == Tridiagonal ? du : dl)
            @test (@inferred diag(A, -1))::typeof(d) == dl
            @test (@inferred diag(A, n-1))::typeof(d) == zeros(elty, 1)
            @test_throws ArgumentError diag(A, -n - 1)
            @test_throws ArgumentError diag(A, n + 1)
            GA = mat_type == Tridiagonal ? mat_type(GenericArray.((dl, d, du))...) : mat_type(GenericArray.((d, dl))...)
            @test (@inferred diag(GA))::typeof(GenericArray(d)) == GenericArray(d)
            @test (@inferred diag(GA, -1))::typeof(GenericArray(d)) == GenericArray(dl)
        end
        @testset "trace" begin
            if real(elty) <: Integer
                @test tr(A) == tr(fA)
            else
                @test tr(A) ≈ tr(fA) rtol=2eps(real(elty))
            end
        end
        @testset "Idempotent tests" begin
            for func in (conj, transpose, adjoint)
                @test func(func(A)) == A
            end
        end
        @testset "permutedims(::[Sym]Tridiagonal)" begin
            @test permutedims(permutedims(A)) === A
            @test permutedims(A) == transpose.(transpose(A))
            @test permutedims(A, [1, 2]) === A
            @test permutedims(A, (2, 1)) == permutedims(A)
        end
        if elty != Int
            @testset "Simple unary functions" begin
                for func in (det, inv)
                    @test func(A) ≈ func(fA) atol=n^2*sqrt(eps(real(one(elty))))
                end
            end
        end
        ds = mat_type == Tridiagonal ? (dl, d, du) : (d, dl)
        for f in (real, imag)
            @test f(A)::mat_type == mat_type(map(f, ds)...)
        end
        if elty <: Real
            for f in (round, trunc, floor, ceil)
                fds = [f.(d) for d in ds]
                @test f.(A)::mat_type == mat_type(fds...)
                @test f.(Int, A)::mat_type == f.(Int, fA)
            end
        end
        fds = [abs.(d) for d in ds]
        @test abs.(A)::mat_type == mat_type(fds...)
        @testset "Multiplication with strided matrix/vector" begin
            @test (x = fill(1.,n); A*x ≈ Array(A)*x)
            @test (X = fill(1.,n,2); A*X ≈ Array(A)*X)
        end
        @testset "Binary operations" begin
            B = mat_type == Tridiagonal ? mat_type(a, b, c) : mat_type(b, a)
            fB = map(elty <: Complex ? ComplexF64 : Float64, Array(B))
            for op in (+, -, *)
                @test Array(op(A, B)) ≈ op(fA, fB)
            end
            α = rand(elty)
            @test Array(α*A) ≈ α*Array(A)
            @test Array(A*α) ≈ Array(A)*α
            @test Array(A/α) ≈ Array(A)/α

            @testset "Matmul with Triangular types" begin
                @test A*LinearAlgebra.UnitUpperTriangular(Matrix(1.0I, n, n)) ≈ fA
                @test A*LinearAlgebra.UnitLowerTriangular(Matrix(1.0I, n, n)) ≈ fA
                @test A*UpperTriangular(Matrix(1.0I, n, n)) ≈ fA
                @test A*LowerTriangular(Matrix(1.0I, n, n)) ≈ fA
            end
            @testset "mul! errors" begin
                Cnn, Cnm, Cmn = Matrix{elty}.(undef, ((n,n), (n,n+1), (n+1,n)))
                @test_throws DimensionMismatch LinearAlgebra.mul!(Cnn,A,Cnm)
                @test_throws DimensionMismatch LinearAlgebra.mul!(Cnn,A,Cmn)
                @test_throws DimensionMismatch LinearAlgebra.mul!(Cnn,B,Cmn)
                @test_throws DimensionMismatch LinearAlgebra.mul!(Cmn,B,Cnn)
                @test_throws DimensionMismatch LinearAlgebra.mul!(Cnm,B,Cnn)
            end
        end
        @testset "Negation" begin
            mA = -A
            @test mA isa mat_type
            @test -mA == A
        end
        if mat_type == SymTridiagonal
            @testset "Tridiagonal/SymTridiagonal mixing ops" begin
                B = convert(Tridiagonal{elty}, A)
                @test B == A
                @test B + A == A + B
                @test B - A == A - B
            end
            if elty <: LinearAlgebra.BlasReal
                @testset "Eigensystems" begin
                    zero, infinity = convert(elty, 0), convert(elty, Inf)
                    @testset "stebz! and stein!" begin
                        w, iblock, isplit = LAPACK.stebz!('V', 'B', -infinity, infinity, 0, 0, zero, b, a)
                        evecs = LAPACK.stein!(b, a, w)

                        (e, v) = eigen(SymTridiagonal(b, a))
                        @test e ≈ w
                        test_approx_eq_vecs(v, evecs)
                    end
                    @testset "stein! call using iblock and isplit" begin
                        w, iblock, isplit = LAPACK.stebz!('V', 'B', -infinity, infinity, 0, 0, zero, b, a)
                        evecs = LAPACK.stein!(b, a, w, iblock, isplit)
                        test_approx_eq_vecs(v, evecs)
                    end
                    @testset "stegr! call with index range" begin
                        F = eigen(SymTridiagonal(b, a),1:2)
                        fF = eigen(Symmetric(Array(SymTridiagonal(b, a))),1:2)
                        test_approx_eq_modphase(F.vectors, fF.vectors)
                        @test F.values ≈ fF.values
                    end
                    @testset "stegr! call with value range" begin
                        F = eigen(SymTridiagonal(b, a),0.0,1.0)
                        fF = eigen(Symmetric(Array(SymTridiagonal(b, a))),0.0,1.0)
                        test_approx_eq_modphase(F.vectors, fF.vectors)
                        @test F.values ≈ fF.values
                    end
                    @testset "eigenvalues/eigenvectors of symmetric tridiagonal" begin
                        if elty === Float32 || elty === Float64
                            DT, VT = @inferred eigen(A)
                            @inferred eigen(A, 2:4)
                            @inferred eigen(A, 1.0, 2.0)
                            D, Vecs = eigen(fA)
                            @test DT ≈ D
                            @test abs.(VT'Vecs) ≈ Matrix(elty(1)I, n, n)
                            test_approx_eq_modphase(eigvecs(A), eigvecs(fA))
                            #call to LAPACK.stein here
                            test_approx_eq_modphase(eigvecs(A,eigvals(A)),eigvecs(A))
                        elseif elty != Int
                            # check that undef is determined accurately even if type inference
                            # bails out due to the number of try/catch blocks in this code.
                            @test_throws UndefVarError fA
                        end
                    end
                end
            end
            if elty <: Real
                Ts = SymTridiagonal(d, dl)
                Fs = Array(Ts)
                Tldlt = factorize(Ts)
                @testset "symmetric tridiagonal" begin
                    @test_throws DimensionMismatch Tldlt\rand(elty,n+1)
                    @test size(Tldlt) == size(Ts)
                    if elty <: AbstractFloat
                        @test LinearAlgebra.LDLt{elty,SymTridiagonal{elty,Vector{elty}}}(Tldlt) === Tldlt
                        @test LinearAlgebra.LDLt{elty}(Tldlt) === Tldlt
                        @test typeof(convert(LinearAlgebra.LDLt{Float32,Matrix{Float32}},Tldlt)) ==
                            LinearAlgebra.LDLt{Float32,Matrix{Float32}}
                        @test typeof(convert(LinearAlgebra.LDLt{Float32},Tldlt)) ==
                            LinearAlgebra.LDLt{Float32,SymTridiagonal{Float32,Vector{Float32}}}
                    end
                    for vv in (copy(v), view(v, 1:n))
                        invFsv = Fs\vv
                        x = Ts\vv
                        @test x ≈ invFsv
                        @test Array(Tldlt) ≈ Fs
                    end

                    @testset "similar" begin
                        @test isa(similar(Ts), SymTridiagonal{elty})
                        @test isa(similar(Ts, Int), SymTridiagonal{Int})
                        @test isa(similar(Ts, (3, 2)), Matrix)
                        @test isa(similar(Ts, Int, (3, 2)), Matrix{Int})
                    end

                    @test first(logabsdet(Tldlt)) ≈ first(logabsdet(Fs))
                    @test last(logabsdet(Tldlt))  ≈ last(logabsdet(Fs))
                    # just test that the det method exists. The numerical value of the
                    # determinant is unreliable
                    det(Tldlt)
                end
            end
        else # mat_type is Tridiagonal
            @testset "tridiagonal linear algebra" begin
                for vv in (copy(v), view(copy(v), 1:n))
                    @test A*vv ≈ fA*vv
                    invFv = fA\vv
                    @test A\vv ≈ invFv
                    Tlu = factorize(A)
                    x = Tlu\vv
                    @test x ≈ invFv
                end
                elty != Int && @test A \ v ≈ ldiv!(copy(A), copy(v))
            end
            F = lu(A)
            L1, U1, p1 = F
            G = lu!(F, 2A)
            L2, U2, p2 = F
            @test L1 ≈ L2
            @test 2U1 ≈ U2
            @test p1 == p2
        end
        @testset "generalized dot" begin
            x = fill(convert(elty, 1), n)
            y = fill(convert(elty, 1), n)
            @test dot(x, A, y) ≈ dot(A'x, y) ≈ dot(x, A*y)
            @test dot([1], SymTridiagonal([1], Int[]), [1]) == 1
            @test dot([1], Tridiagonal(Int[], [1], Int[]), [1]) == 1
            @test dot(Int[], SymTridiagonal(Int[], Int[]), Int[]) === 0
            @test dot(Int[], Tridiagonal(Int[], Int[], Int[]), Int[]) === 0
        end
    end
end

@testset "SymTridiagonal/Tridiagonal block matrix" begin
    M = [1 2; 2 4]
    n = 5
    A = SymTridiagonal(fill(M, n), fill(M, n-1))
    @test @inferred A[1,1] == Symmetric(M)
    @test @inferred A[1,2] == M
    @test @inferred A[2,1] == transpose(M)
    @test @inferred diag(A, 1) == fill(M, n-1)
    @test @inferred diag(A, 0) == fill(Symmetric(M), n)
    @test @inferred diag(A, -1) == fill(transpose(M), n-1)
    @test_throws ArgumentError diag(A, -2)
    @test_throws ArgumentError diag(A, 2)
    @test_throws ArgumentError diag(A, n+1)
    @test_throws ArgumentError diag(A, -n-1)

    A = Tridiagonal(fill(M, n-1), fill(M, n), fill(M, n-1))
    @test @inferred A[1,1] == M
    @test @inferred A[1,2] == M
    @test @inferred A[2,1] == M
    @test @inferred diag(A, 1) == fill(M, n-1)
    @test @inferred diag(A, 0) == fill(M, n)
    @test @inferred diag(A, -1) == fill(M, n-1)
    @test_throws MethodError diag(A, -2)
    @test_throws MethodError diag(A, 2)
    @test_throws ArgumentError diag(A, n+1)
    @test_throws ArgumentError diag(A, -n-1)

    for n in 0:2
        dv, ev = fill(M, n), fill(M, max(n-1,0))
        A = SymTridiagonal(dv, ev)
        @test A == Matrix{eltype(A)}(A)

        A = Tridiagonal(ev, dv, ev)
        @test A == Matrix{eltype(A)}(A)
    end
end

@testset "Issue 12068" begin
    @test SymTridiagonal([1, 2], [0])^3 == [1 0; 0 8]
end

@testset "Issue #48505" begin
    @test SymTridiagonal([1,2,3],[4,5.0]) == [1.0 4.0 0.0; 4.0 2.0 5.0; 0.0 5.0 3.0]
    @test Tridiagonal([1, 2], [4, 5, 1], [6.0, 7]) == [4.0 6.0 0.0; 1.0 5.0 7.0; 0.0 2.0 1.0]
end

@testset "convert for SymTridiagonal" begin
    STF32 = SymTridiagonal{Float32}(fill(1f0, 5), fill(1f0, 4))
    @test convert(SymTridiagonal{Float64}, STF32)::SymTridiagonal{Float64} == STF32
    @test convert(AbstractMatrix{Float64}, STF32)::SymTridiagonal{Float64} == STF32
end

@testset "constructors from matrix" begin
    @test SymTridiagonal([1 2 3; 2 5 6; 0 6 9]) == [1 2 0; 2 5 6; 0 6 9]
    @test Tridiagonal([1 2 3; 4 5 6; 7 8 9]) == [1 2 0; 4 5 6; 0 8 9]
end

@testset "constructors with range and other abstract vectors" begin
    @test SymTridiagonal(1:3, 1:2) == [1 1 0; 1 2 2; 0 2 3]
    @test Tridiagonal(4:5, 1:3, 1:2) == [1 1 0; 4 2 2; 0 5 3]
end

@testset "Prevent off-diagonal aliasing in Tridiagonal" begin
    e = ones(4)
    f = e[1:end-1]
    T = Tridiagonal(f, 2e, f)
    T ./= 10
    @test all(==(0.1), f)
end

@testset "Issue #26994 (and the empty case)" begin
    T = SymTridiagonal([1.0],[3.0])
    x = ones(1)
    @test T*x == ones(1)
    @test SymTridiagonal(ones(0), ones(0)) * ones(0, 2) == ones(0, 2)
end

@testset "Issue 29630" begin
    function central_difference_discretization(N; dfunc = x -> 12x^2 - 2N^2,
                                               dufunc = x -> N^2 + 4N*x,
                                               dlfunc = x -> N^2 - 4N*x,
                                               bfunc = x -> 114ℯ^-x * (1 + 3x),
                                               b0 = 0, bf = 57/ℯ,
                                               x0 = 0, xf = 1)
        h = 1/N
        d, du, dl, b = map(dfunc, (x0+h):h:(xf-h)), map(dufunc, (x0+h):h:(xf-2h)),
                       map(dlfunc, (x0+2h):h:(xf-h)), map(bfunc, (x0+h):h:(xf-h))
        b[1] -= dlfunc(x0)*b0     # subtract the boundary term
        b[end] -= dufunc(xf)*bf   # subtract the boundary term
        Tridiagonal(dl, d, du), b
    end

    A90, b90 = central_difference_discretization(90)

    @test A90\b90 ≈ inv(A90)*b90
end

@testset "singular values of SymTridiag" begin
    @test svdvals(SymTridiagonal([-4,2,3], [0,0])) ≈ [4,3,2]
    @test svdvals(SymTridiagonal(collect(0.:10.), zeros(10))) ≈ reverse(0:10)
    @test svdvals(SymTridiagonal([1,2,1], [1,1])) ≈ [3,1,0]
    # test that dependent methods such as `cond` also work
    @test cond(SymTridiagonal([1,2,3], [0,0])) ≈ 3
end

@testset "sum, mapreduce" begin
    T = Tridiagonal([1,2], [1,2,3], [7,8])
    Tdense = Matrix(T)
    S = SymTridiagonal([1,2,3], [1,2])
    Sdense = Matrix(S)
    @test sum(T) == 24
    @test sum(S) == 12
    @test_throws ArgumentError sum(T, dims=0)
    @test sum(T, dims=1) == sum(Tdense, dims=1)
    @test sum(T, dims=2) == sum(Tdense, dims=2)
    @test sum(T, dims=3) == sum(Tdense, dims=3)
    @test typeof(sum(T, dims=1)) == typeof(sum(Tdense, dims=1))
    @test mapreduce(one, min, T, dims=1) == mapreduce(one, min, Tdense, dims=1)
    @test mapreduce(one, min, T, dims=2) == mapreduce(one, min, Tdense, dims=2)
    @test mapreduce(one, min, T, dims=3) == mapreduce(one, min, Tdense, dims=3)
    @test typeof(mapreduce(one, min, T, dims=1)) == typeof(mapreduce(one, min, Tdense, dims=1))
    @test mapreduce(zero, max, T, dims=1) == mapreduce(zero, max, Tdense, dims=1)
    @test mapreduce(zero, max, T, dims=2) == mapreduce(zero, max, Tdense, dims=2)
    @test mapreduce(zero, max, T, dims=3) == mapreduce(zero, max, Tdense, dims=3)
    @test typeof(mapreduce(zero, max, T, dims=1)) == typeof(mapreduce(zero, max, Tdense, dims=1))
    @test_throws ArgumentError sum(S, dims=0)
    @test sum(S, dims=1) == sum(Sdense, dims=1)
    @test sum(S, dims=2) == sum(Sdense, dims=2)
    @test sum(S, dims=3) == sum(Sdense, dims=3)
    @test typeof(sum(S, dims=1)) == typeof(sum(Sdense, dims=1))
    @test mapreduce(one, min, S, dims=1) == mapreduce(one, min, Sdense, dims=1)
    @test mapreduce(one, min, S, dims=2) == mapreduce(one, min, Sdense, dims=2)
    @test mapreduce(one, min, S, dims=3) == mapreduce(one, min, Sdense, dims=3)
    @test typeof(mapreduce(one, min, S, dims=1)) == typeof(mapreduce(one, min, Sdense, dims=1))
    @test mapreduce(zero, max, S, dims=1) == mapreduce(zero, max, Sdense, dims=1)
    @test mapreduce(zero, max, S, dims=2) == mapreduce(zero, max, Sdense, dims=2)
    @test mapreduce(zero, max, S, dims=3) == mapreduce(zero, max, Sdense, dims=3)
    @test typeof(mapreduce(zero, max, S, dims=1)) == typeof(mapreduce(zero, max, Sdense, dims=1))

    T = Tridiagonal(Int[], Int[], Int[])
    Tdense = Matrix(T)
    S = SymTridiagonal(Int[], Int[])
    Sdense = Matrix(S)
    @test sum(T) == 0
    @test sum(S) == 0
    @test_throws ArgumentError sum(T, dims=0)
    @test sum(T, dims=1) == sum(Tdense, dims=1)
    @test sum(T, dims=2) == sum(Tdense, dims=2)
    @test sum(T, dims=3) == sum(Tdense, dims=3)
    @test typeof(sum(T, dims=1)) == typeof(sum(Tdense, dims=1))
    @test_throws ArgumentError sum(S, dims=0)
    @test sum(S, dims=1) == sum(Sdense, dims=1)
    @test sum(S, dims=2) == sum(Sdense, dims=2)
    @test sum(S, dims=3) == sum(Sdense, dims=3)
    @test typeof(sum(S, dims=1)) == typeof(sum(Sdense, dims=1))

    T = Tridiagonal(Int[], Int[2], Int[])
    Tdense = Matrix(T)
    S = SymTridiagonal(Int[2], Int[])
    Sdense = Matrix(S)
    @test sum(T) == 2
    @test sum(S) == 2
    @test_throws ArgumentError sum(T, dims=0)
    @test sum(T, dims=1) == sum(Tdense, dims=1)
    @test sum(T, dims=2) == sum(Tdense, dims=2)
    @test sum(T, dims=3) == sum(Tdense, dims=3)
    @test typeof(sum(T, dims=1)) == typeof(sum(Tdense, dims=1))
    @test_throws ArgumentError sum(S, dims=0)
    @test sum(S, dims=1) == sum(Sdense, dims=1)
    @test sum(S, dims=2) == sum(Sdense, dims=2)
    @test sum(S, dims=3) == sum(Sdense, dims=3)
    @test typeof(sum(S, dims=1)) == typeof(sum(Sdense, dims=1))
end

@testset "Issue #28994 (sum of Tridigonal and UniformScaling)" begin
    dl = [1., 1.]
    d = [-2., -2., -2.]
    T = Tridiagonal(dl, d, dl)
    S = SymTridiagonal(T)

    @test diag(T + 2I) == zero(d)
    @test diag(S + 2I) == zero(d)
end

@testset "convert Tridiagonal to SymTridiagonal error" begin
    du = rand(Float64, 4)
    d  = rand(Float64, 5)
    dl = rand(Float64, 4)
    T = Tridiagonal(dl, d, du)
    @test_throws ArgumentError SymTridiagonal{Float32}(T)
end

# Issue #38765
@testset "Eigendecomposition with different lengths" begin
    # length(A.ev) can be either length(A.dv) or length(A.dv) - 1
    A = SymTridiagonal(fill(1.0, 3), fill(-1.0, 3))
    F = eigen(A)
    A2 = SymTridiagonal(fill(1.0, 3), fill(-1.0, 2))
    F2 = eigen(A2)
    test_approx_eq_modphase(F.vectors, F2.vectors)
    @test F.values ≈ F2.values ≈ eigvals(A) ≈ eigvals(A2)
    @test eigvecs(A) ≈ eigvecs(A2)
    @test eigvecs(A, eigvals(A)[1:1]) ≈ eigvecs(A2, eigvals(A2)[1:1])
end

@testset "non-commutative algebra (#39701)" begin
    for A in (SymTridiagonal(Quaternion.(randn(5), randn(5), randn(5), randn(5)), Quaternion.(randn(4), randn(4), randn(4), randn(4))),
              Tridiagonal(Quaternion.(randn(4), randn(4), randn(4), randn(4)), Quaternion.(randn(5), randn(5), randn(5), randn(5)), Quaternion.(randn(4), randn(4), randn(4), randn(4))))
        c = Quaternion(1,2,3,4)
        @test A * c ≈ Matrix(A) * c
        @test A / c ≈ Matrix(A) / c
        @test c * A ≈ c * Matrix(A)
        @test c \ A ≈ c \ Matrix(A)
    end
end

@testset "adjoint of LDLt" begin
    Sr = SymTridiagonal(randn(5), randn(4))
    Sc = SymTridiagonal(complex.(randn(5)) .+ 1im, complex.(randn(4), randn(4)))
    b = ones(size(Sr, 1))

    F = ldlt(Sr)
    @test F\b == F'\b

    F = ldlt(Sc)
    @test copy(Sc')\b == F'\b
end

@testset "symmetric and hermitian tridiagonals" begin
    A = [im 0; 0 -im]
    @test issymmetric(A)
    @test !ishermitian(A)

    # real
    A = SymTridiagonal(randn(5), randn(4))
    @test issymmetric(A)
    @test ishermitian(A)

    A = Tridiagonal(A.ev, A.dv, A.ev .+ 1)
    @test !issymmetric(A)
    @test !ishermitian(A)

    # complex
    # https://github.com/JuliaLang/julia/pull/41037#discussion_r645524081
    S = SymTridiagonal(randn(5) .+ 0im, randn(5) .+ 0im)
    S.ev[end] = im
    @test issymmetric(S)
    @test ishermitian(S)

    S = SymTridiagonal(randn(5) .+ 1im, randn(4) .+ 1im)
    @test issymmetric(S)
    @test !ishermitian(S)

    S = Tridiagonal(S.ev, S.dv, adjoint.(S.ev))
    @test !issymmetric(S)
    @test !ishermitian(S)

    S = Tridiagonal(S.dl, real.(S.d) .+ 0im, S.du)
    @test !issymmetric(S)
    @test ishermitian(S)
end

isdefined(Main, :ImmutableArrays) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "ImmutableArrays.jl"))
using .Main.ImmutableArrays

@testset "Conversion to AbstractArray" begin
    # tests corresponding to #34995
    v1 = ImmutableArray([1, 2])
    v2 = ImmutableArray([3, 4, 5])
    v3 = ImmutableArray([6, 7])
    T = Tridiagonal(v1, v2, v3)
    Tsym = SymTridiagonal(v2, v1)

    @test convert(AbstractArray{Float64}, T)::Tridiagonal{Float64,ImmutableArray{Float64,1,Array{Float64,1}}} == T
    @test convert(AbstractMatrix{Float64}, T)::Tridiagonal{Float64,ImmutableArray{Float64,1,Array{Float64,1}}} == T
    @test convert(AbstractArray{Float64}, Tsym)::SymTridiagonal{Float64,ImmutableArray{Float64,1,Array{Float64,1}}} == Tsym
    @test convert(AbstractMatrix{Float64}, Tsym)::SymTridiagonal{Float64,ImmutableArray{Float64,1,Array{Float64,1}}} == Tsym
end

@testset "dot(x,A,y) for A::Tridiagonal or SymTridiagonal" begin
    for elty in (Float32, Float64, ComplexF32, ComplexF64, Int)
        x = fill(convert(elty, 1), 0)
        T = Tridiagonal(x, x, x)
        Tsym = SymTridiagonal(x, x)
        @test dot(x, T, x) == 0.0
        @test dot(x, Tsym, x) == 0.0
    end
end

isdefined(Main, :SizedArrays) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "SizedArrays.jl"))
using .Main.SizedArrays
@testset "non-number eltype" begin
    @testset "sum for SymTridiagonal" begin
        dv = [SizedArray{(2,2)}(rand(1:2048,2,2)) for i in 1:10]
        ev = [SizedArray{(2,2)}(rand(1:2048,2,2)) for i in 1:10]
        S = SymTridiagonal(dv, ev)
        Sdense = Matrix(S)
        @test Sdense == collect(S)
        @test sum(S) == sum(Sdense)
        @test sum(S, dims = 1) == sum(Sdense, dims = 1)
        @test sum(S, dims = 2) == sum(Sdense, dims = 2)
    end
    @testset "issymmetric/ishermitian for Tridiagonal" begin
        @test !issymmetric(Tridiagonal([[1 2;3 4]], [[1 2;2 3], [1 2;2 3]], [[1 2;3 4]]))
        @test !issymmetric(Tridiagonal([[1 3;2 4]], [[1 2;3 4], [1 2;3 4]], [[1 2;3 4]]))
        @test issymmetric(Tridiagonal([[1 3;2 4]], [[1 2;2 3], [1 2;2 3]], [[1 2;3 4]]))

        @test ishermitian(Tridiagonal([[1 3;2 4].+im], [[1 2;2 3].+0im, [1 2;2 3].+0im], [[1 2;3 4].-im]))
        @test !ishermitian(Tridiagonal([[1 3;2 4].+im], [[1 2;2 3].+0im, [1 2;2 3].+0im], [[1 2;3 4].+im]))
        @test !ishermitian(Tridiagonal([[1 3;2 4].+im], [[1 2;2 3].+im, [1 2;2 3].+0im], [[1 2;3 4].-im]))
    end
    @testset "== between Tridiagonal and SymTridiagonal" begin
        dv = [SizedArray{(2,2)}([1 2;3 4]) for i in 1:4]
        ev = [SizedArray{(2,2)}([3 4;1 2]) for i in 1:4]
        S = SymTridiagonal(dv, ev)
        Sdense = Matrix(S)
        @test S == Tridiagonal(diag(Sdense, -1), diag(Sdense),  diag(Sdense, 1)) == S
        @test S !== Tridiagonal(diag(Sdense, 1), diag(Sdense),  diag(Sdense, 1)) !== S
    end
end

@testset "copyto! between SymTridiagonal and Tridiagonal" begin
    ev, dv = [1:4;], [1:5;]
    S = SymTridiagonal(dv, ev)
    T = Tridiagonal(zero(ev), zero(dv), zero(ev))
    @test copyto!(T, S) == S
    @test copyto!(zero(S), T) == T

    ev2 = [1:5;]
    S = SymTridiagonal(dv, ev2)
    T = Tridiagonal(zeros(length(ev2)-1), zero(dv), zeros(length(ev2)-1))
    @test copyto!(T, S) == S
    @test copyto!(zero(S), T) == T

    T2 = Tridiagonal(ones(length(ev)), zero(dv), zero(ev))
    @test_throws "cannot copy a non-symmetric Tridiagonal matrix to a SymTridiagonal" copyto!(zero(S), T2)

    @testset "mismatched sizes" begin
        dv2 = [4; @view dv[2:end]]
        @test copyto!(S, SymTridiagonal([4], Int[])) == SymTridiagonal(dv2, ev)
        @test copyto!(T, SymTridiagonal([4], Int[])) == Tridiagonal(ev, dv2, ev)
        @test copyto!(S, Tridiagonal(Int[], [4], Int[])) == SymTridiagonal(dv2, ev)
        @test copyto!(T, Tridiagonal(Int[], [4], Int[])) == Tridiagonal(ev, dv2, ev)
        @test copyto!(S, SymTridiagonal(Int[], Int[])) == SymTridiagonal(dv, ev)
        @test copyto!(T, SymTridiagonal(Int[], Int[])) == Tridiagonal(ev, dv, ev)
        @test copyto!(S, Tridiagonal(Int[], Int[], Int[])) == SymTridiagonal(dv, ev)
        @test copyto!(T, Tridiagonal(Int[], Int[], Int[])) == Tridiagonal(ev, dv, ev)
    end
end

@testset "copyto! with UniformScaling" begin
    @testset "Tridiagonal" begin
        @testset "Fill" begin
            for len in (4, InfiniteArrays.Infinity())
                d = FillArrays.Fill(1, len)
                ud = FillArrays.Fill(0, len-1)
                T = Tridiagonal(ud, d, ud)
                @test copyto!(T, I) === T
            end
        end
        T = Tridiagonal(fill(3, 3), fill(2, 4), fill(3, 3))
        copyto!(T, I)
        @test all(isone, diag(T))
        @test all(iszero, diag(T, 1))
        @test all(iszero, diag(T, -1))
    end
    @testset "SymTridiagonal" begin
        @testset "Fill" begin
            for len in (4, InfiniteArrays.Infinity())
                d = FillArrays.Fill(1, len)
                ud = FillArrays.Fill(0, len-1)
                ST = SymTridiagonal(d, ud)
                @test copyto!(ST, I) === ST
            end
        end
        ST = SymTridiagonal(fill(2, 4), fill(3, 3))
        copyto!(ST, I)
        @test all(isone, diag(ST))
        @test all(iszero, diag(ST, 1))
        @test all(iszero, diag(ST, -1))
    end
end

@testset "custom axes" begin
    dv, uv = OffsetArray(1:4), OffsetArray(1:3)
    B = Tridiagonal(uv, dv, uv)
    ax = axes(dv, 1)
    @test axes(B) === (ax, ax)
    B = SymTridiagonal(dv, uv)
    @test axes(B) === (ax, ax)
end

@testset "Reverse operation on Tridiagonal" begin
    for n in 5:6
        d = randn(n)
        dl = randn(n - 1)
        du = randn(n - 1)
        T = Tridiagonal(dl, d, du)
        @test reverse(T, dims=1) == reverse(Matrix(T), dims=1)
        @test reverse(T, dims=2) == reverse(Matrix(T), dims=2)
        @test reverse(T) == reverse(Matrix(T)) == reverse!(copy(T))
    end
end

@testset "Reverse operation on SymTridiagonal" begin
    for n  in 5
        d = randn(n)
        dl = randn(n - 1)
        ST = SymTridiagonal(d, dl)
        @test reverse(ST, dims=1) == reverse(Matrix(ST), dims=1)
        @test reverse(ST, dims=2) == reverse(Matrix(ST), dims=2)
        @test reverse(ST) == reverse(Matrix(ST))
    end
end


@testset "Matrix conversion for non-numeric and undef" begin
    T = Tridiagonal(fill(big(3), 3), Vector{BigInt}(undef, 4), fill(big(3), 3))
    M = Matrix(T)
    T[diagind(T)] .= 4
    M[diagind(M)] .= 4
    @test diag(T) == diag(M)
end

end # module TestTridiagonal

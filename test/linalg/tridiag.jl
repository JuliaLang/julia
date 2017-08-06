# This file is a part of Julia. License is MIT: https://julialang.org/license

# basic tridiagonal operations
n = 5

srand(123)

d = 1 .+ rand(n)
dl = -rand(n-1)
du = -rand(n-1)
v = randn(n)
B = randn(n,2)

@testset for elty in (Float32, Float64, Complex64, Complex128, Int)
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
    Ts = SymTridiagonal(d, dl)
    F = diagm(d)
    for i = 1:n-1
        F[i,i+1] = du[i]
        F[i+1,i] = dl[i]
    end

    @testset "constructor" begin
        for (x, y) in ((d, dl), (GenericArray(d), GenericArray(dl)))
            ST = (SymTridiagonal(x, y))::SymTridiagonal{elty, typeof(x)}
            @test ST == Matrix(ST)
            @test ST.dv === x
            @test ST.ev === y
            TT = (Tridiagonal(y, x, y))::Tridiagonal{elty, typeof(x)}
            @test TT == Matrix(TT)
            @test TT.dl === y
            @test TT.d  === x
            @test TT.du === y
        end
        # enable when deprecations for 0.7 are dropped
        # @test_throws MethodError SymTridiagonal(dv, GenericArray(ev))
        # @test_throws MethodError SymTridiagonal(GenericArray(dv), ev)
        # @test_throws MethodError Tridiagonal(GenericArray(ev), dv, GenericArray(ev))
        # @test_throws MethodError Tridiagonal(ev, GenericArray(dv), ev)
    end

    @testset "size and Array" begin
        @test_throws ArgumentError size(Ts,0)
        @test size(Ts,3) == 1
        @test size(T, 1) == n
        @test size(T) == (n, n)
        @test Array(T) == F
    end

    @testset "elementary operations" begin
        @test conj(T) == Tridiagonal(conj(dl), conj(d), conj(du))
        @test transpose(T) == Tridiagonal(du, d, dl)
        @test ctranspose(T) == Tridiagonal(conj(du), conj(d), conj(dl))

        @test abs.(T) == Tridiagonal(abs.(dl),abs.(d),abs.(du))
        if elty <: Real
            @test round.(T) == Tridiagonal(round.(dl),round.(d),round.(du))
            @test isa(round.(T), Tridiagonal)
            @test trunc.(T) == Tridiagonal(trunc.(dl),trunc.(d),trunc.(du))
            @test isa(trunc.(T), Tridiagonal)
            @test floor.(T) == Tridiagonal(floor.(dl),floor.(d),floor.(du))
            @test isa(floor.(T), Tridiagonal)
            @test ceil.(T) == Tridiagonal(ceil.(dl),ceil.(d),ceil.(du))
            @test isa(ceil.(T), Tridiagonal)
        end
        @test real(T) == Tridiagonal(real(dl),real(d),real(du))
        @test imag(T) == Tridiagonal(imag(dl),imag(d),imag(du))
        @test abs.(Ts) == SymTridiagonal(abs.(d),abs.(dl))
        if elty <: Real
            @test round.(Ts) == SymTridiagonal(round.(d),round.(dl))
            @test isa(round.(Ts), SymTridiagonal)
            @test trunc.(Ts) == SymTridiagonal(trunc.(d),trunc.(dl))
            @test isa(trunc.(Ts), SymTridiagonal)
            @test floor.(Ts) == SymTridiagonal(floor.(d),floor.(dl))
            @test isa(floor.(Ts), SymTridiagonal)
            @test ceil.(Ts) == SymTridiagonal(ceil.(d),ceil.(dl))
            @test isa(ceil.(Ts), SymTridiagonal)
        end
        @test real(Ts) == SymTridiagonal(real(d),real(dl))
        @test imag(Ts) == SymTridiagonal(imag(d),imag(dl))
    end

    @testset "interconversion of Tridiagonal and SymTridiagonal" begin
        @test Tridiagonal(dl, d, dl) == SymTridiagonal(d, dl)
        @test SymTridiagonal(d, dl) == Tridiagonal(dl, d, dl)
        @test Tridiagonal(dl, d, du) + Tridiagonal(du, d, dl) == SymTridiagonal(2d, dl+du)
        @test SymTridiagonal(d, dl) + Tridiagonal(dl, d, du) == Tridiagonal(dl + dl, d+d, dl+du)
        @test convert(SymTridiagonal,Tridiagonal(Ts)) == Ts
        @test Array(convert(SymTridiagonal{Complex64},Tridiagonal(Ts))) == convert(Matrix{Complex64}, Ts)
    end
    if elty == Int
        vs = rand(1:100, n)
        Bs = rand(1:100, n, 2)
    else
        vs = convert(Vector{elty}, v)
        Bs = convert(Matrix{elty}, B)
    end

    @testset "tridiagonal linear algebra" begin
        for (BB, vv) in ((Bs, vs), (view(Bs, 1:n, 1), view(vs, 1:n)))
            @test T*vv ≈ F*vv
            invFv = F\vv
            @test T\vv ≈ invFv
            # @test Base.solve(T,v) ≈ invFv
            # @test Base.solve(T, B) ≈ F\B
            Tlu = factorize(T)
            x = Tlu\vv
            @test x ≈ invFv
        end
    end
    @test det(T) ≈ det(F)

    @testset "Matmul with Triangular types" begin
        @test T*Base.LinAlg.UnitUpperTriangular(eye(n)) ≈ F*eye(n)
        @test T*Base.LinAlg.UnitLowerTriangular(eye(n)) ≈ F*eye(n)
        @test T*UpperTriangular(eye(n)) ≈ F*eye(n)
        @test T*LowerTriangular(eye(n)) ≈ F*eye(n)
    end
    if elty <: Real
        Ts = SymTridiagonal(d, dl)
        Fs = Array(Ts)
        Tldlt = factorize(Ts)
        @testset "symmetric tridiagonal" begin
            @test_throws DimensionMismatch Tldlt\rand(elty,n+1)
            @test size(Tldlt) == size(Ts)
            if elty <: AbstractFloat
                @test typeof(convert(Base.LinAlg.LDLt{Float32},Tldlt)) ==
                    Base.LinAlg.LDLt{Float32,SymTridiagonal{elty,Vector{elty}}}
            end
            for vv in (vs, view(vs, 1:n))
                invFsv = Fs\vv
                x = Ts\vv
                @test x ≈ invFsv
                @test Array(AbstractArray(Tldlt)) ≈ Fs
            end

            @testset "similar" begin
                @test isa(similar(Ts), SymTridiagonal{elty})
                @test isa(similar(Ts, Int), SymTridiagonal{Int})
                @test isa(similar(Ts, Int, (3,2)), Matrix{Int})
            end
        end
    end

    @testset "eigenvalues/eigenvectors of symmetric tridiagonal" begin
        if elty === Float32 || elty === Float64
            DT, VT = @inferred eig(Ts)
            @inferred eig(Ts, 2:4)
            @inferred eig(Ts, 1.0, 2.0)
            D, Vecs = eig(Fs)
            @test DT ≈ D
            @test abs.(VT'Vecs) ≈ eye(elty, n)
            @test eigvecs(Ts) == eigvecs(Fs)
            #call to LAPACK.stein here
            Test.test_approx_eq_modphase(eigvecs(Ts,eigvals(Ts)),eigvecs(Fs))
        elseif elty != Int
            # check that undef is determined accurately even if type inference
            # bails out due to the number of try/catch blocks in this code.
            @test_throws UndefVarError Fs
        end
    end

    if elty != Int
        @testset "issue #1490" begin
            @test det(ones(elty,3,3)) ≈ zero(elty) atol=3*eps(real(one(elty)))

            @test det(SymTridiagonal(elty[],elty[])) == one(elty)
        end
    end
    @testset "tril/triu" begin
        @test_throws ArgumentError tril!(SymTridiagonal(d,dl),n+1)
        @test_throws ArgumentError tril!(Tridiagonal(dl,d,du),n+1)
        @test tril(SymTridiagonal(d,dl))    == Tridiagonal(dl,d,zeros(dl))
        @test tril(SymTridiagonal(d,dl),1)  == Tridiagonal(dl,d,dl)
        @test tril(SymTridiagonal(d,dl),-1) == Tridiagonal(dl,zeros(d),zeros(dl))
        @test tril(SymTridiagonal(d,dl),-2) == Tridiagonal(zeros(dl),zeros(d),zeros(dl))
        @test tril(Tridiagonal(dl,d,du))    == Tridiagonal(dl,d,zeros(du))
        @test tril(Tridiagonal(dl,d,du),1)  == Tridiagonal(dl,d,du)
        @test tril(Tridiagonal(dl,d,du),-1) == Tridiagonal(dl,zeros(d),zeros(du))
        @test tril(Tridiagonal(dl,d,du),-2) == Tridiagonal(zeros(dl),zeros(d),zeros(du))

        @test_throws ArgumentError triu!(SymTridiagonal(d,dl),n+1)
        @test_throws ArgumentError triu!(Tridiagonal(dl,d,du),n+1)
        @test triu(SymTridiagonal(d,dl))    == Tridiagonal(zeros(dl),d,dl)
        @test triu(SymTridiagonal(d,dl),-1) == Tridiagonal(dl,d,dl)
        @test triu(SymTridiagonal(d,dl),1)  == Tridiagonal(zeros(dl),zeros(d),dl)
        @test triu(SymTridiagonal(d,dl),2)  == Tridiagonal(zeros(dl),zeros(d),zeros(dl))
        @test triu(Tridiagonal(dl,d,du))    == Tridiagonal(zeros(dl),d,du)
        @test triu(Tridiagonal(dl,d,du),-1) == Tridiagonal(dl,d,du)
        @test triu(Tridiagonal(dl,d,du),1)  == Tridiagonal(zeros(dl),zeros(d),du)
        @test triu(Tridiagonal(dl,d,du),2)  == Tridiagonal(zeros(dl),zeros(d),zeros(du))

        @test !istril(SymTridiagonal(d,dl))
        @test !istriu(SymTridiagonal(d,dl))
        @test istriu(Tridiagonal(zeros(dl),d,du))
        @test istril(Tridiagonal(dl,d,zeros(du)))
    end
end

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

let n = 12 #Size of matrix problem to test
    srand(123)
    @testset "SymTridiagonal (symmetric tridiagonal) matrices" begin
        for relty in (Float32, Float64), elty in (relty, Complex{relty})
            a = convert(Vector{elty}, randn(n))
            b = convert(Vector{elty}, randn(n-1))
            if elty <: Complex
                a += im*convert(Vector{elty}, randn(n))
                b += im*convert(Vector{elty}, randn(n-1))
            end

            @test_throws DimensionMismatch SymTridiagonal(a, ones(elty, n+1))
            @test_throws ArgumentError SymTridiagonal(rand(n,n))

            A = SymTridiagonal(a, b)
            fA = map(elty <: Complex ? Complex128 : Float64, Array(A))

            @testset "getindex" begin
                @test_throws BoundsError A[n+1,1]
                @test_throws BoundsError A[1,n+1]
                @test A[1,n] == convert(elty,0.0)
                @test A[1,1] == a[1]
            end
            @testset "setindex!" begin
                @test_throws BoundsError A[n + 1, 1] = 0 # test bounds check
                @test_throws BoundsError A[1, n + 1] = 0 # test bounds check
                @test ((A[3, 3] = A[3, 3]) == A[3, 3]; A == fA) # test assignment on the main diagonal
                @test_throws ArgumentError A[3, 2] = 1 # test assignment on the subdiagonal
                @test_throws ArgumentError A[2, 3] = 1 # test assignment on the superdiagonal
                @test_throws ArgumentError A[1, 3] = 1 # test assignment off the main/sub/super diagonal
            end
            @testset "Diagonal extraction" begin
                @test diag(A,1) == b
                @test diag(A,-1) == b
                @test diag(A,0) == a
                @test diag(A,n-1) == zeros(elty,1)
                @test_throws ArgumentError diag(A,n+1)
            end
            @testset "Idempotent tests" begin
                for func in (conj, transpose, ctranspose)
                    @test func(func(A)) == A
                end
            end
            @testset "Simple unary functions" begin
                for func in (det, inv)
                    @test func(A) ≈ func(fA) atol=n^2*sqrt(eps(relty))
                end
            end

            if elty <: Real
                @testset "Rounding to Ints" begin
                    @test round.(Int,A) == round.(Int,fA)
                    @test isa(round.(Int,A), SymTridiagonal)
                    @test trunc.(Int,A) == trunc.(Int,fA)
                    @test isa(trunc.(Int,A), SymTridiagonal)
                    @test ceil.(Int,A) == ceil.(Int,fA)
                    @test isa(ceil.(Int,A), SymTridiagonal)
                    @test floor.(Int,A) == floor.(Int,fA)
                    @test isa(floor.(Int,A), SymTridiagonal)
                end
            end

            @testset "Tridiagonal/SymTridiagonal mixing ops" begin
                B = convert(Tridiagonal{elty},A)
                @test B == A
                @test B + A == A + B
                @test B - A == A - B
            end
            @testset "Multiplication with strided matrix/vector" begin
                @test A*ones(n) ≈ Array(A)*ones(n)
                @test A*ones(n, 2) ≈ Array(A)*ones(n, 2)
            end
            if elty <: Real
                @testset "Eigensystems" begin
                    zero, infinity = convert(elty, 0), convert(elty, Inf)
                    @testset "stebz! and stein!" begin
                        w, iblock, isplit = LAPACK.stebz!('V', 'B', -infinity, infinity, 0, 0, zero, a, b)
                        evecs = LAPACK.stein!(a, b, w)

                        (e, v) = eig(SymTridiagonal(a, b))
                        @test e ≈ w
                        test_approx_eq_vecs(v, evecs)
                    end
                    @testset "stein! call using iblock and isplit" begin
                        w, iblock, isplit = LAPACK.stebz!('V', 'B', -infinity, infinity, 0, 0, zero, a, b)
                        evecs = LAPACK.stein!(a, b, w, iblock, isplit)
                        test_approx_eq_vecs(v, evecs)
                    end
                    @testset "stegr! call with index range" begin
                        F = eigfact(SymTridiagonal(a, b),1:2)
                        fF = eigfact(Symmetric(Array(SymTridiagonal(a, b))),1:2)
                        Test.test_approx_eq_modphase(F[:vectors], fF[:vectors])
                        @test F[:values] ≈ fF[:values]
                    end
                    @testset "stegr! call with value range" begin
                        F = eigfact(SymTridiagonal(a, b),0.0,1.0)
                        fF = eigfact(Symmetric(Array(SymTridiagonal(a, b))),0.0,1.0)
                        Test.test_approx_eq_modphase(F[:vectors], fF[:vectors])
                        @test F[:values] ≈ fF[:values]
                    end
                end
            end

            @testset "Binary operations" begin
                a = convert(Vector{elty}, randn(n))
                b = convert(Vector{elty}, randn(n - 1))
                if elty <: Complex
                    a += im*convert(Vector{elty}, randn(n))
                    b += im*convert(Vector{elty}, randn(n - 1))
                end

                B = SymTridiagonal(a, b)
                fB = map(elty <: Complex ? Complex128 : Float64, Array(B))

                for op in (+, -, *)
                    @test Array(op(A, B)) ≈ op(fA, fB)
                end
                α = rand(elty)
                @test Array(α*A) ≈ α*Array(A)
                @test Array(A*α) ≈ Array(A)*α
                @test Array(A/α) ≈ Array(A)/α

                @testset "A_mul_B! errors" begin
                    @test_throws DimensionMismatch A_mul_B!(zeros(elty,n,n),B,ones(elty,n+1,n))
                    @test_throws DimensionMismatch A_mul_B!(zeros(elty,n+1,n),B,ones(elty,n,n))
                    @test_throws DimensionMismatch A_mul_B!(zeros(elty,n,n+1),B,ones(elty,n,n))
                end
            end
        end
    end
    @testset "Tridiagonal matrices" begin
        for relty in (Float32, Float64), elty in (relty, Complex{relty})
            a = convert(Vector{elty}, randn(n - 1))
            b = convert(Vector{elty}, randn(n))
            c = convert(Vector{elty}, randn(n - 1))
            if elty <: Complex
                a += im*convert(Vector{elty}, randn(n - 1))
                b += im*convert(Vector{elty}, randn(n))
                c += im*convert(Vector{elty}, randn(n - 1))
            end

            @test_throws ArgumentError Tridiagonal(a,a,a)
            A = Tridiagonal(a, b, c)
            fA = map(elty <: Complex ? Complex128 : Float64, Array(A))

            @testset "similar, size, and copy!" begin
                B = similar(A)
                @test size(B) == size(A)
                copy!(B,A)
                @test B == A
                @test isa(similar(A), Tridiagonal{elty})
                @test isa(similar(A, Int), Tridiagonal{Int})
                @test isa(similar(A, Int, (3,2)), Matrix{Int})
                @test size(A,3) == 1
                @test_throws ArgumentError size(A,0)
            end
            @testset "Diagonal extraction" begin
                @test diag(A,-1) == a
                @test diag(A,0) == b
                @test diag(A,1) == c
                @test diag(A,n-1) == zeros(elty,1)
                @test_throws ArgumentError diag(A,n+1)
            end
            @testset "Simple unary functions" begin
                for func in (det, inv)
                    @test func(A) ≈ func(fA) atol=n^2*sqrt(eps(relty))
                end
            end
            if elty <: Real
                @testset "Rounding to Ints" begin
                    @test round.(Int,A) == round.(Int,fA)
                    @test isa(round.(Int,A), Tridiagonal)
                    @test trunc.(Int,A) == trunc.(Int,fA)
                    @test isa(trunc.(Int,A), Tridiagonal)
                    @test ceil.(Int,A) == ceil.(Int,fA)
                    @test isa(ceil.(Int,A), Tridiagonal)
                    @test floor.(Int,A) == floor.(Int,fA)
                    @test isa(floor.(Int,A), Tridiagonal)
                end
            end
            @testset "Binary operations" begin
                a = convert(Vector{elty}, randn(n - 1))
                b = convert(Vector{elty}, randn(n))
                c = convert(Vector{elty}, randn(n - 1))
                if elty <: Complex
                    a += im*convert(Vector{elty}, randn(n - 1))
                    b += im*convert(Vector{elty}, randn(n))
                    c += im*convert(Vector{elty}, randn(n - 1))
                end
            end

            @testset "Multiplication with strided matrix/vector" begin
                @test A*ones(n) ≈ Array(A)*ones(n)
                @test A*ones(n, 2) ≈ Array(A)*ones(n, 2)
            end

            B = Tridiagonal(a, b, c)
            fB = map(elty <: Complex ? Complex128 : Float64, Array(B))
            @testset "Binary ops" begin
                for op in (+, -, *)
                    @test Array(op(A, B)) ≈ op(fA, fB)
                end
                α = rand(elty)
                @test Array(α*A) ≈ α*Array(A)
                @test Array(A*α) ≈ Array(A)*α
                @test Array(A/α) ≈ Array(A)/α
            end
            @test_throws ArgumentError convert(SymTridiagonal{elty},A)

            @testset "A_mul_B! errors" begin
                @test_throws DimensionMismatch Base.LinAlg.A_mul_B!(zeros(fA),A,ones(elty,n,n+1))
                @test_throws DimensionMismatch Base.LinAlg.A_mul_B!(zeros(fA),A,ones(elty,n+1,n))
            end
            @testset "getindex" begin
                @test_throws BoundsError A[n+1,1]
                @test_throws BoundsError A[1,n+1]
            end
            @testset "setindex!" begin
                @test_throws BoundsError A[n + 1, 1] = 0 # test bounds check
                @test_throws BoundsError A[1, n + 1] = 0 # test bounds check
                @test (A[3, 3] = A[3, 3]; A == fA) # test assignment on the main diagonal
                @test (A[3, 2] = A[3, 2]; A == fA) # test assignment on the subdiagonal
                @test (A[2, 3] = A[2, 3]; A == fA) # test assignment on the superdiagonal
                @test ((A[1, 3] = 0) == 0; A == fA) # test zero assignment off the main/sub/super diagonal
                @test_throws ArgumentError A[1, 3] = 1 # test non-zero assignment off the main/sub/super diagonal
            end
        end
    end
end

@testset "Issue 12068" begin
    @test SymTridiagonal([1, 2], [0])^3 == [1 0; 0 8]
end

@testset "convert for SymTridiagonal" begin
    @test convert(SymTridiagonal{Float64},SymTridiagonal(ones(Float32,5),ones(Float32,4))) == SymTridiagonal(ones(Float64,5),ones(Float64,4))
    @test convert(AbstractMatrix{Float64},SymTridiagonal(ones(Float32,5),ones(Float32,4))) == SymTridiagonal(ones(Float64,5),ones(Float64,4))
end

@testset "constructors from matrix" begin
    @test SymTridiagonal([1 2 3; 2 5 6; 0 6 9]) == [1 2 0; 2 5 6; 0 6 9]
    @test Tridiagonal([1 2 3; 4 5 6; 7 8 9]) == [1 2 0; 4 5 6; 0 8 9]
end

@testset "constructors with range and other abstract vectors" begin
    @test SymTridiagonal(1:3, 1:2) == [1 1 0; 1 2 2; 0 2 3]
    @test Tridiagonal(4:5, 1:3, 1:2) == [1 1 0; 4 2 2; 0 5 3]
end

# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestSymmetric

using Test, LinearAlgebra, SparseArrays, Random

Random.seed!(101)

@testset "Pauli σ-matrices: $σ" for σ in map(Hermitian,
        Any[ [1 0; 0 1], [0 1; 1 0], [0 -im; im 0], [1 0; 0 -1] ])
    @test ishermitian(σ)
end

@testset "Hermitian matrix exponential/log" begin
    A1 = randn(4,4) + im*randn(4,4)
    A2 = A1 + A1'
    @test exp(A2) ≈ exp(Hermitian(A2))
    @test log(A2) ≈ log(Hermitian(A2))
    A3 = A1 * A1' # posdef
    @test exp(A3) ≈ exp(Hermitian(A3))
    @test log(A3) ≈ log(Hermitian(A3))

    A1 = randn(4,4)
    A3 = A1 * A1'
    A4 = A1 + transpose(A1)
    @test exp(A4) ≈ exp(Symmetric(A4))
    @test log(A3) ≈ log(Symmetric(A3))
    @test log(A3) ≈ log(Hermitian(A3))
end

@testset "Core functionality" begin
    n = 10
    areal = randn(n,n)/2
    aimg  = randn(n,n)/2
    @testset for eltya in (Float32, Float64, ComplexF32, ComplexF64, BigFloat, Int)
        a = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(areal, aimg) : areal)
        asym = transpose(a) + a                 # symmetric indefinite
        aherm = a' + a                 # Hermitian indefinite
        apos  = a' * a                 # Hermitian positive definite
        aposs = apos + transpose(apos)        # Symmetric positive definite
        ε = εa = eps(abs(float(one(eltya))))

        x = randn(n)
        y = randn(n)
        b = randn(n,n)/2
        x = eltya == Int ? rand(1:7, n) : convert(Vector{eltya}, eltya <: Complex ? complex.(x, zeros(n)) : x)
        y = eltya == Int ? rand(1:7, n) : convert(Vector{eltya}, eltya <: Complex ? complex.(y, zeros(n)) : y)
        b = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(b, zeros(n,n)) : b)
        @testset "basic ops" begin
            @testset "constructor" begin
                @test Symmetric(Symmetric(asym, :U))     === Symmetric(asym, :U)
                @test Hermitian(Hermitian(aherm, :U))    === Hermitian(aherm, :U)
                @test Symmetric(Symmetric(asym, :U), :U) === Symmetric(asym, :U)
                @test Hermitian(Hermitian(aherm, :U), :U) === Hermitian(aherm, :U)
                @test_throws ArgumentError Symmetric(Symmetric(asym, :U), :L)
                @test_throws ArgumentError Hermitian(Hermitian(aherm, :U), :L)
                # mixed cases with Hermitian/Symmetric
                if eltya <: Real
                    @test Symmetric(Hermitian(aherm, :U))     === Symmetric(aherm, :U)
                    @test Hermitian(Symmetric(asym, :U))     === Hermitian(asym, :U)
                    @test Symmetric(Hermitian(aherm, :U), :U) === Symmetric(aherm, :U)
                    @test Hermitian(Symmetric(asym, :U), :U) === Hermitian(asym, :U)
                    @test_throws ArgumentError Symmetric(Hermitian(aherm, :U), :L)
                    @test_throws ArgumentError Hermitian(Symmetric(aherm, :U), :L)
                end
            end
            @testset "similar" begin
                @test isa(similar(Symmetric(asym)), Symmetric{eltya})
                @test isa(similar(Hermitian(aherm)), Hermitian{eltya})
                @test isa(similar(Symmetric(asym), Int), Symmetric{Int})
                @test isa(similar(Hermitian(aherm), Int), Hermitian{Int})
                @test isa(similar(Symmetric(asym), (3,2)), Matrix{eltya})
                @test isa(similar(Hermitian(aherm), (3,2)), Matrix{eltya})
                @test isa(similar(Symmetric(asym), Int, (3,2)), Matrix{Int})
                @test isa(similar(Hermitian(aherm), Int, (3,2)), Matrix{Int})
            end

            @testset "Array/Matrix constructor from Symmetric/Hermitian" begin
                @test asym  == Matrix(Symmetric(asym))  == Array(Symmetric(asym))
                @test aherm == Matrix(Hermitian(aherm)) == Array(Hermitian(aherm))
            end

            @testset "parent" begin
                @test asym === parent(Symmetric(asym))
                @test aherm === parent(Hermitian(aherm))
            end
            # Unary minus for Symmetric/Hermitian matrices
            @testset "Unary minus for Symmetric/Hermitian matrices" begin
                @test (-Symmetric(asym))::typeof(Symmetric(asym)) == -asym
                @test (-Hermitian(aherm))::typeof(Hermitian(aherm)) == -aherm
                @test (-Symmetric([true true; false false]))::Symmetric{Int,Matrix{Int}} == [-1 -1; -1 0]
                @test (-Hermitian([true false; true false]))::Hermitian{Int,Matrix{Int}} == [-1 0; 0 0]
            end

            @testset "Addition and subtraction for Symmetric/Hermitian matrices" begin
                for f in (+, -)
                    @test (f(Symmetric(asym), Symmetric(aposs)))::typeof(Symmetric(asym)) == f(asym, aposs)
                    @test (f(Hermitian(aherm), Hermitian(apos)))::typeof(Hermitian(aherm)) == f(aherm, apos)
                    @test (f(Symmetric(real(asym)), Hermitian(aherm)))::typeof(Hermitian(aherm)) == f(real(asym), aherm)
                    @test (f(Hermitian(aherm), Symmetric(real(asym))))::typeof(Hermitian(aherm)) == f(aherm, real(asym))
                    @test (f(Symmetric(asym), Hermitian(aherm))) == f(asym, aherm)
                    @test (f(Hermitian(aherm), Symmetric(asym))) == f(aherm, asym)
                end
            end

            @testset "getindex and unsafe_getindex" begin
                @test aherm[1,1] == Hermitian(aherm)[1,1]
                @test asym[1,1] == Symmetric(asym)[1,1]
                @test Symmetric(asym)[1:2,1:2] == asym[1:2,1:2]
                @test Hermitian(aherm)[1:2,1:2] == aherm[1:2,1:2]
            end

            @testset "conversion" begin
                @test Symmetric(asym) == convert(Symmetric,Symmetric(asym))
                if eltya <: Real
                    typs = [Float16,Float32,Float64]
                    for typ in typs
                        @test Symmetric(convert(Matrix{typ},asym)) == convert(Symmetric{typ,Matrix{typ}},Symmetric(asym))
                    end
                end
                if eltya <: Complex
                    typs = [ComplexF32,ComplexF64]
                    for typ in typs
                        @test Symmetric(convert(Matrix{typ},asym)) == convert(Symmetric{typ,Matrix{typ}},Symmetric(asym))
                        @test Hermitian(convert(Matrix{typ},aherm)) == convert(Hermitian{typ,Matrix{typ}},Hermitian(aherm))
                    end
                end
                @test Symmetric{eltya, Matrix{eltya}}(Symmetric(asym, :U)) === Symmetric(asym, :U)
                @test Hermitian{eltya, Matrix{eltya}}(Hermitian(aherm, :U)) === Hermitian(aherm, :U)
            end

            @testset "issymmetric, ishermitian" begin
                @test issymmetric(Symmetric(asym))
                @test ishermitian(Hermitian(aherm))
                if eltya <: Real
                    @test ishermitian(Symmetric(asym))
                    @test issymmetric(Hermitian(asym))
                elseif eltya <: Complex
                    # test that zero imaginary component is
                    # handled properly
                    @test ishermitian(Symmetric(b + b'))
                end
            end

            @testset "tril/triu" begin
                for (op, validks) in (
                        (triu, (-n + 1):(n + 1)),
                        (tril, (-n - 1):(n - 1)) )
                    for di in validks
                        @test op(Symmetric(asym), di) == op(asym, di)
                        @test op(Hermitian(aherm), di) == op(aherm, di)
                        @test op(Symmetric(asym, :L), di) == op(asym, di)
                        @test op(Hermitian(aherm, :L), di) == op(aherm, di)
                    end
                end
            end

            @testset "transpose, adjoint" begin
                S = Symmetric(asym)
                H = Hermitian(aherm)
                @test transpose(S) === S == asym
                @test adjoint(H) === H == aherm
                if eltya <: Real
                    @test adjoint(S) === S == asym
                    @test  transpose(H) === H == aherm
                else
                    @test adjoint(S) ==  Symmetric(conj(asym))
                    @test transpose(H) ==  Hermitian(copy(transpose(aherm)))
                end
                @test copy(adjoint(H)) == copy(aherm)
                @test copy(transpose(S)) == copy(asym)
            end

            @testset "real, imag" begin
                S = Symmetric(asym)
                H = Hermitian(aherm)
                @test issymmetric(real(S))
                @test ishermitian(real(H))
                if eltya <: Real
                    @test real(S) === S == asym
                    @test real(H) === H == aherm
                elseif eltya <: Complex
                    @test issymmetric(imag(S))
                    @test !ishermitian(imag(H))
                end
            end

        end

        @testset "linalg unary ops" begin
            @testset "tr" begin
                @test tr(asym) == tr(Symmetric(asym))
                @test tr(aherm) == tr(Hermitian(aherm))
            end

            @testset "isposdef[!]" begin
                @test isposdef(Symmetric(asym))  == isposdef(asym)
                @test isposdef(Symmetric(aposs)) == isposdef(aposs) == true
                @test isposdef(Hermitian(aherm)) == isposdef(aherm)
                @test isposdef(Hermitian(apos))  == isposdef(apos) == true
                if eltya != Int #chol! won't work with Int
                    @test isposdef!(Symmetric(copy(asym)))  == isposdef(asym)
                    @test isposdef!(Symmetric(copy(aposs))) == isposdef(aposs) == true
                    @test isposdef!(Hermitian(copy(aherm))) == isposdef(aherm)
                    @test isposdef!(Hermitian(copy(apos)))  == isposdef(apos) == true
                end
            end

            @testset "$f" for f in (det, logdet, logabsdet)
                for uplo in (:U, :L)
                    @test all(f(apos)  .≈ f(Hermitian(apos, uplo)))
                    @test all(f(aposs) .≈ f(Symmetric(aposs, uplo)))
                    if f != logdet
                        @test all(f(aherm) .≈ f(Hermitian(aherm, uplo)))
                        @test all(f(asym)  .≈ f(Symmetric(asym, uplo)))
                    end
                end
            end

            @testset "inversion" begin
                for uplo in (:U, :L)
                    @test inv(Symmetric(asym, uplo))::Symmetric ≈ inv(asym)
                    @test inv(Hermitian(aherm, uplo))::Hermitian ≈ inv(aherm)
                    @test inv(Symmetric(a, uplo))::Symmetric ≈ inv(Matrix(Symmetric(a, uplo)))
                    if eltya <: Real
                        @test inv(Hermitian(a, uplo))::Hermitian ≈ inv(Matrix(Hermitian(a, uplo)))
                    end
                end
                if eltya <: LinearAlgebra.BlasComplex
                    @testset "inverse edge case with complex Hermitian" begin
                        # Hermitian matrix, where inv(lu(A)) generates non-real diagonal elements
                        for T in (ComplexF32, ComplexF64)
                            A = T[0.650488+0.0im 0.826686+0.667447im; 0.826686-0.667447im 1.81707+0.0im]
                            H = Hermitian(A)
                            @test inv(H) ≈ inv(A)
                            @test ishermitian(Matrix(inv(H)))
                        end
                    end
                end
            end

            # Revisit when implemented in julia
            if eltya != BigFloat
                @testset "cond" begin
                    if eltya <: Real #svdvals! has no method for Symmetric{Complex}
                        @test cond(Symmetric(asym)) ≈ cond(asym)
                    end
                    @test cond(Hermitian(aherm)) ≈ cond(aherm)
                end

                @testset "symmetric eigendecomposition" begin
                    if eltya <: Real # the eigenvalues are only real and ordered for Hermitian matrices
                        d, v = eigen(asym)
                        @test asym*v[:,1] ≈ d[1]*v[:,1]
                        @test v*Diagonal(d)*transpose(v) ≈ asym
                        @test isequal(eigvals(asym[1]), eigvals(asym[1:1,1:1])[1])
                        @test abs.(eigen(Symmetric(asym), 1:2).vectors'v[:,1:2]) ≈ Matrix(I, 2, 2)
                        @test abs.(eigen(Symmetric(asym), d[1] - 1, (d[2] + d[3])/2).vectors'v[:,1:2]) ≈ Matrix(I, 2, 2)
                        @test eigvals(Symmetric(asym), 1:2) ≈ d[1:2]
                        @test eigvals(Symmetric(asym), d[1] - 1, (d[2] + d[3])/2) ≈ d[1:2]
                        # eigen doesn't support Symmetric{Complex}
                        @test Matrix(eigen(asym)) ≈ asym
                        @test eigvecs(Symmetric(asym)) ≈ eigvecs(asym)
                    end

                    d, v = eigen(aherm)
                    @test aherm*v[:,1] ≈ d[1]*v[:,1]
                    @test v*Diagonal(d)*v' ≈ aherm
                    @test isequal(eigvals(aherm[1]), eigvals(aherm[1:1,1:1])[1])
                    @test abs.(eigen(Hermitian(aherm), 1:2).vectors'v[:,1:2]) ≈ Matrix(I, 2, 2)
                    @test abs.(eigen(Hermitian(aherm), d[1] - 1, (d[2] + d[3])/2).vectors'v[:,1:2]) ≈ Matrix(I, 2, 2)
                    @test eigvals(Hermitian(aherm), 1:2) ≈ d[1:2]
                    @test eigvals(Hermitian(aherm), d[1] - 1, (d[2] + d[3])/2) ≈ d[1:2]
                    @test Matrix(eigen(aherm)) ≈ aherm
                    @test eigvecs(Hermitian(aherm)) ≈ eigvecs(aherm)

                    # relation to svdvals
                    if eltya <: Real #svdvals! has no method for Symmetric{Complex}
                        @test sum(sort(abs.(eigvals(Symmetric(asym))))) == sum(sort(svdvals(Symmetric(asym))))
                    end
                    @test sum(sort(abs.(eigvals(Hermitian(aherm))))) == sum(sort(svdvals(Hermitian(aherm))))
                end

                @testset "rank" begin
                    let A = a[:,1:5]*a[:,1:5]'
                        # Make sure A is Hermitian even in the presence of rounding error
                        # xianyi/OpenBLAS#729
                        A = (A + A') / 2
                        @test rank(A) == rank(Hermitian(A))
                    end
                end

                @testset "pow" begin
                    # Integer power
                    @test (asym)^2   ≈ (Symmetric(asym)^2)::Symmetric
                    @test (asym)^-2  ≈ (Symmetric(asym)^-2)::Symmetric
                    @test (aposs)^2  ≈ (Symmetric(aposs)^2)::Symmetric
                    @test (aherm)^2  ≈ (Hermitian(aherm)^2)::Hermitian
                    @test (aherm)^-2 ≈ (Hermitian(aherm)^-2)::Hermitian
                    @test (apos)^2   ≈ (Hermitian(apos)^2)::Hermitian
                    # integer floating point power
                    @test (asym)^2.0   ≈ (Symmetric(asym)^2.0)::Symmetric
                    @test (asym)^-2.0  ≈ (Symmetric(asym)^-2.0)::Symmetric
                    @test (aposs)^2.0  ≈ (Symmetric(aposs)^2.0)::Symmetric
                    @test (aherm)^2.0  ≈ (Hermitian(aherm)^2.0)::Hermitian
                    @test (aherm)^-2.0 ≈ (Hermitian(aherm)^-2.0)::Hermitian
                    @test (apos)^2.0   ≈ (Hermitian(apos)^2.0)::Hermitian
                    # non-integer floating point power
                    @test (asym)^2.5   ≈ (Symmetric(asym)^2.5)::Symmetric
                    @test (asym)^-2.5  ≈ (Symmetric(asym)^-2.5)::Symmetric
                    @test (aposs)^2.5  ≈ (Symmetric(aposs)^2.5)::Symmetric
                    @test (aherm)^2.5  ≈ (Hermitian(aherm)^2.5)#::Hermitian
                    @test (aherm)^-2.5 ≈ (Hermitian(aherm)^-2.5)#::Hermitian
                    @test (apos)^2.5   ≈ (Hermitian(apos)^2.5)::Hermitian
                end
            end
        end

        @testset "linalg binary ops" begin
            @testset "mat * vec" begin
                @test Symmetric(asym)*x+y ≈ asym*x+y
                # testing fallbacks for transpose-vector * transpose(SymHerm)
                xadj = transpose(x)
                @test xadj * transpose(Symmetric(asym)) ≈ xadj * asym
                @test x' * Symmetric(asym) ≈ x' * asym

                @test Hermitian(aherm)*x+y ≈ aherm*x+y
                # testing fallbacks for adjoint-vector * SymHerm'
                xadj = x'
                @test x' * Hermitian(aherm) ≈ x' * aherm
                @test xadj * Hermitian(aherm)' ≈ xadj * aherm
            end

            @testset "mat * mat" begin
                C = zeros(eltya,n,n)
                @test Hermitian(aherm) * a ≈ aherm * a
                @test a * Hermitian(aherm) ≈ a * aherm
                @test Hermitian(aherm) * Hermitian(aherm) ≈ aherm*aherm
                @test_throws DimensionMismatch Hermitian(aherm) * Vector{eltya}(undef, n+1)
                LinearAlgebra.mul!(C,a,Hermitian(aherm))
                @test C ≈ a*aherm

                @test Symmetric(asym) * Symmetric(asym) ≈ asym*asym
                @test Symmetric(asym) * a ≈ asym * a
                @test a * Symmetric(asym) ≈ a * asym
                @test_throws DimensionMismatch Symmetric(asym) * Vector{eltya}(undef, n+1)
                LinearAlgebra.mul!(C,a,Symmetric(asym))
                @test C ≈ a*asym

                tri_b = UpperTriangular(triu(b))
                @test Array(transpose(Hermitian(aherm)) * tri_b) ≈ transpose(aherm) * Array(tri_b)
                @test Array(tri_b * transpose(Hermitian(aherm))) ≈ Array(tri_b) * transpose(aherm)
                @test Array(Hermitian(aherm)' * tri_b) ≈ aherm' * Array(tri_b)
                @test Array(tri_b * Hermitian(aherm)') ≈ Array(tri_b) * aherm'

                @test Array(transpose(Symmetric(asym)) * tri_b) ≈ transpose(asym) * Array(tri_b)
                @test Array(tri_b * transpose(Symmetric(asym))) ≈ Array(tri_b) * transpose(asym)
                @test Array(Symmetric(asym)' * tri_b) ≈ asym' * Array(tri_b)
                @test Array(tri_b * Symmetric(asym)') ≈ Array(tri_b) * asym'
            end
            @testset "solver" begin
                @test Hermitian(aherm)\x ≈ aherm\x
                @test Hermitian(aherm)\b ≈ aherm\b
                @test Symmetric(asym)\x  ≈ asym\x
                @test Symmetric(asym)\b  ≈ asym\b
            end
        end
        @testset "generalized dot product" begin
            for uplo in (:U, :L)
                @test dot(x, Hermitian(aherm, uplo), y) ≈ dot(x, Hermitian(aherm, uplo)*y) ≈ dot(x, Matrix(Hermitian(aherm, uplo)), y)
                @test dot(x, Hermitian(aherm, uplo), x) ≈ dot(x, Hermitian(aherm, uplo)*x) ≈ dot(x, Matrix(Hermitian(aherm, uplo)), x)
            end
            if eltya <: Real
                for uplo in (:U, :L)
                    @test dot(x, Symmetric(aherm, uplo), y) ≈ dot(x, Symmetric(aherm, uplo)*y) ≈ dot(x, Matrix(Symmetric(aherm, uplo)), y)
                    @test dot(x, Symmetric(aherm, uplo), x) ≈ dot(x, Symmetric(aherm, uplo)*x) ≈ dot(x, Matrix(Symmetric(aherm, uplo)), x)
                end
            end
        end

        @testset "dot product of symmetric and Hermitian matrices" begin
            for mtype in (Symmetric, Hermitian)
                symau = mtype(a, :U)
                symal = mtype(a, :L)
                msymau = Matrix(symau)
                msymal = Matrix(symal)
                @test_throws DimensionMismatch dot(symau, mtype(zeros(eltya, n-1, n-1)))
                for eltyc in (Float32, Float64, ComplexF32, ComplexF64, BigFloat, Int)
                    creal = randn(n, n)/2
                    cimag = randn(n, n)/2
                    c = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(creal, cimag) : creal)
                    symcu = mtype(c, :U)
                    symcl = mtype(c, :L)
                    msymcu = Matrix(symcu)
                    msymcl = Matrix(symcl)
                    @test dot(symau, symcu) ≈ dot(msymau, msymcu)
                    @test dot(symau, symcl) ≈ dot(msymau, msymcl)
                    @test dot(symal, symcu) ≈ dot(msymal, msymcu)
                    @test dot(symal, symcl) ≈ dot(msymal, msymcl)
                end

                # block matrices
                blockm = [eltya == Int ? rand(1:7, 3, 3) : convert(Matrix{eltya}, eltya <: Complex ? complex.(randn(3, 3)/2, randn(3, 3)/2) : randn(3, 3)/2) for _ in 1:3, _ in 1:3]
                symblockmu = mtype(blockm, :U)
                symblockml = mtype(blockm, :L)
                msymblockmu = Matrix(symblockmu)
                msymblockml = Matrix(symblockml)
                @test dot(symblockmu, symblockmu) ≈ dot(msymblockmu, msymblockmu)
                @test dot(symblockmu, symblockml) ≈ dot(msymblockmu, msymblockml)
                @test dot(symblockml, symblockmu) ≈ dot(msymblockml, msymblockmu)
                @test dot(symblockml, symblockml) ≈ dot(msymblockml, msymblockml)
            end
        end
    end
end

#Issue #7647: test xsyevr, xheevr, xstevr drivers.
@testset "Eigenvalues in interval for $(typeof(Mi7647))" for Mi7647 in
        (Symmetric(diagm(0 => 1.0:3.0)),
         Hermitian(diagm(0 => 1.0:3.0)),
         Hermitian(diagm(0 => complex(1.0:3.0))),
         SymTridiagonal([1.0:3.0;], zeros(2)))
    @test eigmin(Mi7647)  == eigvals(Mi7647, 0.5, 1.5)[1] == 1.0
    @test eigmax(Mi7647)  == eigvals(Mi7647, 2.5, 3.5)[1] == 3.0
    @test eigvals(Mi7647) == eigvals(Mi7647, 0.5, 3.5) == [1.0:3.0;]
end

@testset "Hermitian wrapper ignores imaginary parts on diagonal" begin
    A = [1.0+im 2.0; 2.0 0.0]
    @test !ishermitian(A)
    @test Hermitian(A)[1,1] == 1
end

@testset "Issue #7933" begin
    A7933 = [1 2; 3 4]
    B7933 = copy(A7933)
    C7933 = Matrix(Symmetric(A7933))
    @test A7933 == B7933
end

@testset "Issues #8057 and #8058. f=$f, A=$A" for f in
        (eigen, eigvals),
            A in (Symmetric([0 1; 1 0]), Hermitian([0 im; -im 0]))
    @test_throws ArgumentError f(A, 3, 2)
    @test_throws ArgumentError f(A, 1:4)
end

@testset "Ignore imaginary part of Hermitian diagonal" begin
    A = [1.0+im 2.0; 2.0 0.0]
    @test !ishermitian(A)
    @test diag(Hermitian(A)) == real(diag(A))
end

@testset "Issue #17780" begin
    a = randn(2,2)
    a = a'a
    b = complex.(a,a)
    c = Symmetric(b)
    @test conj(c) == conj(Array(c))
    cc = copy(c)
    @test conj!(c) == conj(Array(cc))
    c = Hermitian(b + b')
    @test conj(c) == conj(Array(c))
    cc = copy(c)
    @test conj!(c) == conj(Array(cc))
end

@testset "Issue # 19225" begin
    X = [1 -1; -1 1]
    for T in (Symmetric, Hermitian)
        Y = T(copy(X))
        _Y = similar(Y)
        copyto!(_Y, Y)
        @test _Y == Y

        W = T(copy(X), :L)
        copyto!(W, Y)
        @test W.data == Y.data
        @test W.uplo != Y.uplo

        W[1,1] = 4
        @test W == T([4 -1; -1 1])
        @test_throws ArgumentError (W[1,2] = 2)
        if T == Hermitian
            @test_throws ArgumentError (W[2,2] = 3+4im)
        end

        @test Y + I == T([2 -1; -1 2])
        @test Y - I == T([0 -1; -1 0])
        @test Y * I == Y

        @test Y .+ 1 == T([2 0; 0 2])
        @test Y .- 1 == T([0 -2; -2 0])
        @test Y * 2 == T([2 -2; -2 2])
        @test Y / 1 == Y

        @test T([true false; false true]) .+ true == T([2 1; 1 2])
    end
end

@testset "Issue #21981" begin
    B = complex(rand(4,4))
    B[4,1] += 1im;
    @test ishermitian(Symmetric(B, :U))
    @test issymmetric(Hermitian(B, :U))
    B[4,1]  = real(B[4,1])
    B[1,4] += 1im
    @test ishermitian(Symmetric(B, :L))
    @test issymmetric(Hermitian(B, :L))
end

@testset "$HS solver with $RHS RHS - $T" for HS in (Hermitian, Symmetric),
        RHS in (Hermitian, Symmetric, Diagonal, UpperTriangular, LowerTriangular),
        T   in (Float64, ComplexF64)
    D = rand(T, 10, 10); D = D'D
    A = HS(D)
    B = RHS(D)
    @test A\B ≈ Matrix(A)\Matrix(B)
end

@testset "inversion of Hilbert matrix" begin
    for T in (Float64, ComplexF64)
        H = T[1/(i + j - 1) for i in 1:8, j in 1:8]
        @test norm(inv(Symmetric(H))*(H*fill(1., 8)) .- 1) ≈ 0 atol = 1e-5
        @test norm(inv(Hermitian(H))*(H*fill(1., 8)) .- 1) ≈ 0 atol = 1e-5
    end
end

@testset "similar should preserve underlying storage type and uplo flag" begin
    m, n = 4, 3
    sparsemat = sprand(m, m, 0.5)
    for SymType in (Symmetric, Hermitian)
        symsparsemat = SymType(sparsemat)
        @test isa(similar(symsparsemat), typeof(symsparsemat))
        @test similar(symsparsemat).uplo == symsparsemat.uplo
        @test isa(similar(symsparsemat, Float32), SymType{Float32,<:SparseMatrixCSC{Float32}})
        @test similar(symsparsemat, Float32).uplo == symsparsemat.uplo
        @test isa(similar(symsparsemat, (n, n)), typeof(sparsemat))
        @test isa(similar(symsparsemat, Float32, (n, n)), SparseMatrixCSC{Float32})
    end
end

@testset "#24572: eltype(A::HermOrSym) === eltype(parent(A))" begin
    A = rand(Float32, 3, 3)
    @test_throws TypeError Symmetric{Float64,Matrix{Float32}}(A, 'U')
    @test_throws TypeError Hermitian{Float64,Matrix{Float32}}(A, 'U')
end

@testset "fill[stored]!" begin
    for uplo in (:U, :L)
        # Hermitian
        A = Hermitian(fill(1.0+0im, 2, 2), uplo)
        @test fill!(A, 2) == fill(2, 2, 2)
        @test A.data == (uplo == :U ? [2 2; 1.0+0im 2] : [2 1.0+0im; 2 2])
        @test_throws ArgumentError fill!(A, 2+im)

        # Symmetric
        A = Symmetric(fill(1.0+im, 2, 2), uplo)
        @test fill!(A, 2) == fill(2, 2, 2)
        @test A.data == (uplo == :U ? [2 2; 1.0+im 2] : [2 1.0+im; 2 2])
    end
end

@testset "#25625 recursive transposition" begin
    A = Matrix{Matrix{Int}}(undef, 2, 2)
    A[1,1] = [1 2; 2 3]
    A[1,2] = [4 5 6; 7 8 9]
    A[2,1] = [4 7; 5 8; 6 9]
    A[2,2] = [1 2; 3 4]
    for uplo in (:U, :L)
        S = Symmetric(A, uplo)
        @test S[1,1] == A[1,1]
        @test S[1,2] == transpose(S[2,1]) == A[1,2]
        @test S[2,2] == Symmetric(A[2,2], uplo)
        @test S == transpose(S) == Matrix(S) == Matrix(transpose(S)) == transpose(Matrix(S))
    end

    B = Matrix{Matrix{Complex{Int}}}(undef, 2, 2)
    B[1,1] = [1 2+im; 2-im 3]
    B[1,2] = [4 5+1im 6-2im; 7+3im 8-4im 9+5im]
    B[2,1] = [4 7-3im; 5-1im 8+4im; 6+2im 9-5im]
    B[2,2] = [1+1im 2+2im; 3-3im 4-2im]
    for uplo in (:U, :L)
        H = Hermitian(B, uplo)
        @test H[1,1] == Hermitian(B[1,1], uplo)
        @test H[1,2] == adjoint(H[2,1]) == B[1,2]
        @test H[2,1] == adjoint(H[1,2]) == B[2,1]
        @test H[2,2] == Hermitian(B[2,2], uplo)
        @test H == adjoint(H) == Matrix(H) == Matrix(adjoint(H)) == adjoint(Matrix(H))
    end
end

@testset "getindex of diagonal element (#25972)" begin
    A = rand(ComplexF64, 2, 2)
    @test Hermitian(A, :U)[1,1] == Hermitian(A, :L)[1,1] == real(A[1,1])
end

@testset "issue #29392: SymOrHerm scaled with Number" begin
    R = rand(Float64, 2, 2); C = rand(ComplexF64, 2, 2)
    # Symmetric * Real, Real * Symmetric
    A = Symmetric(R); x = 2.0
    @test (A * x)::Symmetric == (x * A)::Symmetric
    A = Symmetric(C); x = 2.0
    @test (A * x)::Symmetric == (x * A)::Symmetric
    # Symmetric * Complex, Complex * Symmetrics
    A = Symmetric(R); x = 2.0im
    @test (A * x)::Symmetric == (x * A)::Symmetric
    A = Symmetric(C); x = 2.0im
    @test (A * x)::Symmetric == (x * A)::Symmetric
    # Hermitian * Real, Real * Hermitian
    A = Hermitian(R); x = 2.0
    @test (A * x)::Hermitian == (x * A)::Hermitian
    A = Hermitian(C); x = 2.0
    @test (A * x)::Hermitian == (x * A)::Hermitian
    # Hermitian * Complex, Complex * Hermitian
    A = Hermitian(R); x = 2.0im
    @test (A * x)::Matrix == (x * A)::Matrix
    A = Hermitian(C); x = 2.0im
    @test (A * x)::Matrix == (x * A)::Matrix
    # Symmetric / Real
    A = Symmetric(R); x = 2.0
    @test (A / x)::Symmetric == Matrix(A) / x
    A = Symmetric(C); x = 2.0
    @test (A / x)::Symmetric == Matrix(A) / x
    # Symmetric / Complex
    A = Symmetric(R); x = 2.0im
    @test (A / x)::Symmetric == Matrix(A) / x
    A = Symmetric(C); x = 2.0im
    @test (A / x)::Symmetric == Matrix(A) / x
    # Hermitian / Real
    A = Hermitian(R); x = 2.0
    @test (A / x)::Hermitian == Matrix(A) / x
    A = Hermitian(C); x = 2.0
    @test (A / x)::Hermitian == Matrix(A) / x
    # Hermitian / Complex
    A = Hermitian(R); x = 2.0im
    @test (A / x)::Matrix == Matrix(A) / x
    A = Hermitian(C); x = 2.0im
    @test (A / x)::Matrix == Matrix(A) / x
end

@testset "issue #30814: Symmetric of Hermitian if diag is not real" begin
    A = [1 2; 3 4] * (1 + im)
    B = Hermitian(A)
    @test_throws ArgumentError Symmetric(B) == Symmetric(Matrix(B))
    A[1,1] = 1; A[2,2] = 4
    @test Symmetric(B) == Symmetric(Matrix(B))
end

@testset "issue #32079: det for singular Symmetric matrix" begin
    A = ones(Float64, 3, 3)
    @test det(Symmetric(A))::Float64 == det(A) == 0.0
    @test det(Hermitian(A))::Float64 == det(A) == 0.0
    A = ones(ComplexF64, 3, 3)
    @test det(Symmetric(A))::ComplexF64 == det(A) == 0.0
    @test det(Hermitian(A))::Float64 == det(A) == 0.0
end

@testset "symmetric()/hermitian() for Numbers" begin
    @test LinearAlgebra.symmetric(1, :U) == 1
    @test LinearAlgebra.symmetric_type(Int) == Int
    @test LinearAlgebra.hermitian(1, :U) == 1
    @test LinearAlgebra.hermitian_type(Int) == Int
end

@testset "sqrt(nearly semidefinite)" begin
    let A = [0.9999999999999998 4.649058915617843e-16 -1.3149405273715513e-16 9.9959579317056e-17; -8.326672684688674e-16 1.0000000000000004 2.9280733590254494e-16 -2.9993900031619594e-16; 9.43689570931383e-16 -1.339206523454095e-15 1.0000000000000007 -8.550505126287743e-16; -6.245004513516506e-16 -2.0122792321330962e-16 1.183061278035052e-16 1.0000000000000002],
        B = [0.09648289218436859 0.023497875751503007 0.0 0.0; 0.023497875751503007 0.045787575150300804 0.0 0.0; 0.0 0.0 0.0 0.0; 0.0 0.0 0.0 0.0],
        C = Symmetric(A*B*A'), # semidefinite up to roundoff
        Csqrt = sqrt(C)
        @test Csqrt isa Symmetric{Float64}
        @test Csqrt*Csqrt ≈ C rtol=1e-14
    end
    let D = Symmetric(Matrix(Diagonal([1 0; 0 -1e-14])))
        @test sqrt(D) ≈ [1 0; 0 1e-7im] rtol=1e-14
        @test sqrt(D, rtol=1e-13) ≈ [1 0; 0 0] rtol=1e-14
        @test sqrt(D, rtol=1e-13)^2 ≈ D rtol=1e-13
    end
end

end # module TestSymmetric

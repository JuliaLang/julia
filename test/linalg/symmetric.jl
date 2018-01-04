# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test

srand(101)

@testset "Pauli σ-matrices: $σ" for σ in map(Hermitian,
        Any[ [1 0; 0 1], [0 1; 1 0], [0 -im; im 0], [1 0; 0 -1] ])
    @test ishermitian(σ)
end

@testset "Hermitian matrix exponential/log" begin
    A1 = randn(4,4) + im*randn(4,4)
    A2 = A1 + adjoint(A1)
    @test exp(A2) ≈ exp(Hermitian(A2))
    @test log(A2) ≈ log(Hermitian(A2))
    A3 = A1 * adjoint(A1) # posdef
    @test exp(A3) ≈ exp(Hermitian(A3))
    @test log(A3) ≈ log(Hermitian(A3))

    A1 = randn(4,4)
    A3 = A1 * adjoint(A1)
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
        asym = transpose(a)+a                 # symmetric indefinite
        aherm = adjoint(a)+a                 # Hermitian indefinite
        apos  = a'*a                 # Hermitian positive definite
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
                    @test ishermitian(Symmetric(b + adjoint(b)))
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
                @test  transpose(S) === S == asym
                @test adjoint(H) === H == aherm
                if eltya <: Real
                    @test adjoint(S) === S == asym
                    @test  transpose(H) === H == aherm
                else
                    @test adjoint(S) ==  Symmetric(conj(asym))
                    @test  transpose(H) ==  Hermitian(transpose(aherm))
                end
            end
        end

        @testset "linalg unary ops" begin
            @testset "trace" begin
                @test trace(asym) == trace(Symmetric(asym))
                @test trace(aherm) == trace(Hermitian(aherm))
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
                if eltya <: Base.LinAlg.BlasComplex
                    @testset "inverse edge case with complex Hermitian" begin
                        # Hermitian matrix, where inv(lufact(A)) generates non-real diagonal elements
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
                        d, v = eig(asym)
                        @test asym*v[:,1] ≈ d[1]*v[:,1]
                        @test v*Diagonal(d)*Transpose(v) ≈ asym
                        @test isequal(eigvals(asym[1]), eigvals(asym[1:1,1:1]))
                        @test abs.(eigfact(Symmetric(asym), 1:2).vectors'v[:,1:2]) ≈ Matrix(I, 2, 2)
                        eig(Symmetric(asym), 1:2) # same result, but checks that method works
                        @test abs.(eigfact(Symmetric(asym), d[1] - 1, (d[2] + d[3])/2).vectors'v[:,1:2]) ≈ Matrix(I, 2, 2)
                        eig(Symmetric(asym), d[1] - 1, (d[2] + d[3])/2) # same result, but checks that method works
                        @test eigvals(Symmetric(asym), 1:2) ≈ d[1:2]
                        @test eigvals(Symmetric(asym), d[1] - 1, (d[2] + d[3])/2) ≈ d[1:2]
                        # eigfact doesn't support Symmetric{Complex}
                        @test Matrix(eigfact(asym)) ≈ asym
                        @test eigvecs(Symmetric(asym)) ≈ eigvecs(asym)
                    end

                    d, v = eig(aherm)
                    @test aherm*v[:,1] ≈ d[1]*v[:,1]
                    @test v*Diagonal(d)*v' ≈ aherm
                    @test isequal(eigvals(aherm[1]), eigvals(aherm[1:1,1:1]))
                    @test abs.(eigfact(Hermitian(aherm), 1:2).vectors'v[:,1:2]) ≈ Matrix(I, 2, 2)
                    eig(Hermitian(aherm), 1:2) # same result, but checks that method works
                    @test abs.(eigfact(Hermitian(aherm), d[1] - 1, (d[2] + d[3])/2).vectors'v[:,1:2]) ≈ Matrix(I, 2, 2)
                    eig(Hermitian(aherm), d[1] - 1, (d[2] + d[3])/2) # same result, but checks that method works
                    @test eigvals(Hermitian(aherm), 1:2) ≈ d[1:2]
                    @test eigvals(Hermitian(aherm), d[1] - 1, (d[2] + d[3])/2) ≈ d[1:2]
                    @test Matrix(eigfact(aherm)) ≈ aherm
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
                        A = (adjoint(A) + A) / 2
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
                # testing fallbacks for Transpose-vector * Transpose(SymHerm)
                xadj = Transpose(x)
                @test xadj * Transpose(Symmetric(asym)) ≈ xadj * asym
                @test x' * Symmetric(asym) ≈ x' * asym

                @test Hermitian(aherm)*x+y ≈ aherm*x+y
                # testing fallbacks for Adjoint-vector * SymHerm'
                xadj = x'
                @test x' * Hermitian(aherm) ≈ x' * aherm
                @test xadj * Hermitian(aherm)' ≈ xadj * aherm
            end

            @testset "mat * mat" begin
                C = zeros(eltya,n,n)
                @test Hermitian(aherm) * a ≈ aherm * a
                @test a * Hermitian(aherm) ≈ a * aherm
                @test Hermitian(aherm) * Hermitian(aherm) ≈ aherm*aherm
                @test_throws DimensionMismatch Hermitian(aherm) * ones(eltya,n+1)
                Base.LinAlg.mul!(C,a,Hermitian(aherm))
                @test C ≈ a*aherm

                @test Symmetric(asym) * Symmetric(asym) ≈ asym*asym
                @test Symmetric(asym) * a ≈ asym * a
                @test a * Symmetric(asym) ≈ a * asym
                @test_throws DimensionMismatch Symmetric(asym) * ones(eltya,n+1)
                Base.LinAlg.mul!(C,a,Symmetric(asym))
                @test C ≈ a*asym

                tri_b = UpperTriangular(triu(b))
                @test Array(Transpose(Hermitian(aherm)) * tri_b) ≈ Transpose(aherm) * Array(tri_b)
                @test Array(tri_b * Transpose(Hermitian(aherm))) ≈ Array(tri_b) * Transpose(aherm)
                @test Array(Hermitian(aherm)' * tri_b) ≈ aherm' * Array(tri_b)
                @test Array(tri_b * Hermitian(aherm)') ≈ Array(tri_b) * aherm'

                @test Array(Transpose(Symmetric(asym)) * tri_b) ≈ Transpose(asym) * Array(tri_b)
                @test Array(tri_b * Transpose(Symmetric(asym))) ≈ Array(tri_b) * Transpose(asym)
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
        (eigfact, eigvals, eig),
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
    c = Hermitian(b + adjoint(b))
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

        @test Y + I == T([2 -1; -1 2])
        @test Y - I == T([0 -1; -1 0])
        @test Y * I == Y

        @test Y .+ 1 == T([2 0; 0 2])
        @test Y .- 1 == T([0 -2; -2 0])
        @test Y * 2 == T([2 -2; -2 2])
        @test Y / 1 == Y

        @test T([true false; false true]) .+ true == T([2 1; 1 2])
    end

    @test_throws ArgumentError Hermitian(X) + 2im*I
    @test_throws ArgumentError Hermitian(X) - 2im*I
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
        @test norm(inv(Symmetric(H))*(H*ones(8)) .- 1) ≈ 0 atol = 1e-5
        @test norm(inv(Hermitian(H))*(H*ones(8)) .- 1) ≈ 0 atol = 1e-5
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

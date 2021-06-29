# This file is a part of Julia. License is MIT: https://julialang.org/license

module TestTriangular

debug = false
using Test, LinearAlgebra, SparseArrays, Random
using LinearAlgebra: BlasFloat, errorbounds, full!, transpose!,
    UnitUpperTriangular, UnitLowerTriangular,
    mul!, rdiv!, rmul!, lmul!

debug && println("Triangular matrices")

n = 9
Random.seed!(123)

debug && println("Test basic type functionality")
@test_throws DimensionMismatch LowerTriangular(randn(5, 4))
@test LowerTriangular(randn(3, 3)) |> t -> [size(t, i) for i = 1:3] == [size(Matrix(t), i) for i = 1:3]

# The following test block tries to call all methods in base/linalg/triangular.jl in order for a combination of input element types. Keep the ordering when adding code.
for elty1 in (Float32, Float64, BigFloat, ComplexF32, ComplexF64, Complex{BigFloat}, Int)
    # Begin loop for first Triangular matrix
    for (t1, uplo1) in ((UpperTriangular, :U),
                        (UnitUpperTriangular, :U),
                        (LowerTriangular, :L),
                        (UnitLowerTriangular, :L))

        # Construct test matrix
        A1 = t1(elty1 == Int ? rand(1:7, n, n) : convert(Matrix{elty1}, (elty1 <: Complex ? complex.(randn(n, n), randn(n, n)) : randn(n, n)) |> t -> cholesky(t't).U |> t -> uplo1 == :U ? t : copy(t')))
        @test t1(A1) === A1
        @test t1{elty1}(A1) === A1
        # test the ctor works for AbstractMatrix
        symm = Symmetric(rand(Int8, n, n))
        t1s = t1{elty1}(symm)
        @test typeof(t1s) == t1{elty1, Symmetric{elty1, Matrix{elty1}}}
        t1t = t1{elty1}(t1(rand(Int8, n, n)))
        @test typeof(t1t) == t1{elty1, Matrix{elty1}}

        debug && println("elty1: $elty1, A1: $t1")

        # Convert
        @test convert(AbstractMatrix{elty1}, A1) == A1
        @test convert(Matrix, A1) == A1
        @test t1{elty1}(convert(AbstractMatrix{elty1}, A1)) == A1

        # full!
        @test full!(copy(A1)) == A1

        # similar
        @test isa(similar(A1), t1)
        @test eltype(similar(A1)) == elty1
        @test isa(similar(A1, Int), t1)
        @test eltype(similar(A1, Int)) == Int
        @test isa(similar(A1, (3,2)), Matrix{elty1})
        @test isa(similar(A1, Int, (3,2)), Matrix{Int})

        #copyto!
        simA1 = similar(A1)
        copyto!(simA1, A1)
        @test simA1 == A1

        # getindex
        let mA1 = Matrix(A1)
            # linear indexing
            for i in 1:length(A1)
                @test A1[i] == mA1[i]
            end
            # cartesian indexing
            for i in 1:size(A1, 1), j in 1:size(A1, 2)
                @test A1[i,j] == mA1[i,j]
            end
        end
        @test isa(A1[2:4,1], Vector)


        # setindex! (and copy)
        A1c = copy(A1)
        for i = 1:size(A1, 1)
            for j = 1:size(A1, 2)
                if uplo1 == :U
                    if i > j
                        A1c[i,j] = 0
                        @test_throws ArgumentError A1c[i,j] = 1
                    elseif i == j && t1 == UnitUpperTriangular
                        A1c[i,j] = 1
                        @test_throws ArgumentError A1c[i,j] = 0
                    else
                        A1c[i,j] = 0
                        @test A1c[i,j] == 0
                    end
                else
                    if i < j
                        A1c[i,j] = 0
                        @test_throws ArgumentError A1c[i,j] = 1
                    elseif i == j && t1 == UnitLowerTriangular
                        A1c[i,j] = 1
                        @test_throws ArgumentError A1c[i,j] = 0
                    else
                        A1c[i,j] = 0
                        @test A1c[i,j] == 0
                    end
                end
            end
        end

        # istril/istriu
        if uplo1 == :L
            @test istril(A1)
            @test !istriu(A1)
            @test istriu(A1')
            @test istriu(transpose(A1))
            @test !istril(A1')
            @test !istril(transpose(A1))
        else
            @test istriu(A1)
            @test !istril(A1)
            @test istril(A1')
            @test istril(transpose(A1))
            @test !istriu(A1')
            @test !istriu(transpose(A1))
        end

        #tril/triu
        if uplo1 == :L
            @test tril(A1,0)  == A1
            @test tril(A1,-1) == LowerTriangular(tril(Matrix(A1), -1))
            @test tril(A1,1)  == t1(tril(tril(Matrix(A1), 1)))
            @test tril(A1, -n - 2) == zeros(size(A1))
            @test tril(A1, n) == A1
            @test triu(A1,0)  == t1(diagm(0 => diag(A1)))
            @test triu(A1,-1) == t1(tril(triu(A1.data,-1)))
            @test triu(A1,1)  == zeros(size(A1)) # or just @test iszero(triu(A1,1))?
            @test triu(A1, -n) == A1
            @test triu(A1, n + 2) == zeros(size(A1))
        else
            @test triu(A1,0)  == A1
            @test triu(A1,1)  == UpperTriangular(triu(Matrix(A1), 1))
            @test triu(A1,-1) == t1(triu(triu(Matrix(A1), -1)))
            @test triu(A1, -n) == A1
            @test triu(A1, n + 2) == zeros(size(A1))
            @test tril(A1,0)  == t1(diagm(0 => diag(A1)))
            @test tril(A1,1)  == t1(triu(tril(A1.data,1)))
            @test tril(A1,-1) == zeros(size(A1)) # or just @test iszero(tril(A1,-1))?
            @test tril(A1, -n - 2) == zeros(size(A1))
            @test tril(A1, n) == A1
        end

        # factorize
        @test factorize(A1) == A1

        # [c]transpose[!] (test views as well, see issue #14317)
        let vrange = 1:n-1, viewA1 = t1(view(A1.data, vrange, vrange))
            # transpose
            @test copy(transpose(A1)) == transpose(Matrix(A1))
            @test copy(transpose(viewA1)) == transpose(Matrix(viewA1))
            # adjoint
            @test copy(A1') == Matrix(A1)'
            @test copy(viewA1') == Matrix(viewA1)'
            # transpose!
            @test transpose!(copy(A1)) == transpose(A1)
            @test typeof(transpose!(copy(A1))).name == typeof(transpose(A1)).name
            @test transpose!(t1(view(copy(A1).data, vrange, vrange))) == transpose(viewA1)
            # adjoint!
            @test adjoint!(copy(A1)) == adjoint(A1)
            @test typeof(adjoint!(copy(A1))).name == typeof(adjoint(A1)).name
            @test adjoint!(t1(view(copy(A1).data, vrange, vrange))) == adjoint(viewA1)
        end

        # diag
        @test diag(A1) == diag(Matrix(A1))

        # real
        @test real(A1) == real(Matrix(A1))
        @test imag(A1) == imag(Matrix(A1))
        @test abs.(A1) == abs.(Matrix(A1))

        # Unary operations
        @test -A1 == -Matrix(A1)

        # copy and copyto! (test views as well, see issue #14317)
        let vrange = 1:n-1, viewA1 = t1(view(A1.data, vrange, vrange))
            # copy
            @test copy(A1) == copy(Matrix(A1))
            @test copy(viewA1) == copy(Matrix(viewA1))
            # copyto!
            B = similar(A1)
            copyto!(B, A1)
            @test B == A1
            B = similar(copy(transpose(A1)))
            copyto!(B, copy(transpose(A1)))
            @test B == copy(transpose(A1))
            B = similar(viewA1)
            copyto!(B, viewA1)
            @test B == viewA1
            B = similar(copy(transpose(viewA1)))
            copyto!(B, copy(transpose(viewA1)))
            @test B == transpose(viewA1)
        end

        #exp/log
        if elty1 ∈ (Float32,Float64,ComplexF32,ComplexF64)
            @test exp(Matrix(log(A1))) ≈ A1
        end

        # scale
        if (t1 == UpperTriangular || t1 == LowerTriangular)
            unitt = istriu(A1) ? UnitUpperTriangular : UnitLowerTriangular
            if elty1 == Int
                cr = 2
            else
                cr = 0.5
            end
            ci = cr * im
            if elty1 <: Real
                A1tmp = copy(A1)
                rmul!(A1tmp, cr)
                @test A1tmp == cr*A1
                A1tmp = copy(A1)
                lmul!(cr, A1tmp)
                @test A1tmp == cr*A1
                A1tmp = copy(A1)
                A2tmp = unitt(A1)
                mul!(A1tmp, A2tmp, cr)
                @test A1tmp == cr * A2tmp
                A1tmp = copy(A1)
                A2tmp = unitt(A1)
                mul!(A1tmp, cr, A2tmp)
                @test A1tmp == cr * A2tmp
            else
                A1tmp = copy(A1)
                rmul!(A1tmp, ci)
                @test A1tmp == ci*A1
                A1tmp = copy(A1)
                lmul!(ci, A1tmp)
                @test A1tmp == ci*A1
                A1tmp = copy(A1)
                A2tmp = unitt(A1)
                mul!(A1tmp, ci, A2tmp)
                @test A1tmp == ci * A2tmp
                A1tmp = copy(A1)
                A2tmp = unitt(A1)
                mul!(A1tmp, A2tmp, ci)
                @test A1tmp == A2tmp*ci
            end
        end

        # generalized dot
        for eltyb in (Float32, Float64, BigFloat, ComplexF32, ComplexF64, Complex{BigFloat})
            b1 = convert(Vector{eltyb}, (elty1 <: Complex ? real(A1) : A1)*fill(1., n))
            b2 = convert(Vector{eltyb}, (elty1 <: Complex ? real(A1) : A1)*randn(n))
            if elty1 in (BigFloat, Complex{BigFloat}) || eltyb in (BigFloat, Complex{BigFloat})
                @test dot(b1, A1, b2) ≈ dot(A1'b1, b2)  atol=sqrt(max(eps(real(float(one(elty1)))),eps(real(float(one(eltyb))))))*n*n
            else
                @test dot(b1, A1, b2) ≈ dot(A1'b1, b2)  atol=sqrt(max(eps(real(float(one(elty1)))),eps(real(float(one(eltyb))))))*n*n
            end
        end

        # Binary operations
        @test A1*0.5 == Matrix(A1)*0.5
        @test 0.5*A1 == 0.5*Matrix(A1)
        @test A1/0.5 == Matrix(A1)/0.5
        @test 0.5\A1 == 0.5\Matrix(A1)

        # inversion
        @test inv(A1) ≈ inv(lu(Matrix(A1)))
        inv(Matrix(A1)) # issue #11298
        @test isa(inv(A1), t1)
        # make sure the call to LAPACK works right
        if elty1 <: BlasFloat
            @test LinearAlgebra.inv!(copy(A1)) ≈ inv(lu(Matrix(A1)))
        end

        # Determinant
        @test det(A1) ≈ det(lu(Matrix(A1))) atol=sqrt(eps(real(float(one(elty1)))))*n*n
        @test logdet(A1) ≈ logdet(lu(Matrix(A1))) atol=sqrt(eps(real(float(one(elty1)))))*n*n
        lada, ladb = logabsdet(A1)
        flada, fladb = logabsdet(lu(Matrix(A1)))
        @test lada ≈ flada atol=sqrt(eps(real(float(one(elty1)))))*n*n
        @test ladb ≈ fladb atol=sqrt(eps(real(float(one(elty1)))))*n*n

        # Matrix square root
        @test sqrt(A1) |> (t -> (t*t)::typeof(t)) ≈ A1

        # naivesub errors
        @test_throws DimensionMismatch ldiv!(A1, Vector{elty1}(undef, n+1))

        # eigenproblems
        if !(elty1 in (BigFloat, Complex{BigFloat})) # Not handled yet
            vals, vecs = eigen(A1)
            if (t1 == UpperTriangular || t1 == LowerTriangular) && elty1 != Int # Cannot really handle degenerate eigen space and Int matrices will probably have repeated eigenvalues.
                @test vecs*diagm(0 => vals)/vecs ≈ A1 atol=sqrt(eps(float(real(one(vals[1])))))*(opnorm(A1,Inf)*n)^2
            end
        end

        # Condition number tests - can be VERY approximate
        if elty1 <:BlasFloat
            for p in (1.0, Inf)
                @test cond(A1,p) ≈ cond(A1,p) atol=(cond(A1,p)+cond(A1,p))
            end
            @test cond(A1,2) == cond(Matrix(A1),2)
        end

        if !(elty1 in (BigFloat, Complex{BigFloat})) # Not implemented yet
            svd(A1)
            elty1 <: BlasFloat && svd!(copy(A1))
            svdvals(A1)
        end

        @test ((A1*A1)::t1) ≈ Matrix(A1) * Matrix(A1)
        @test ((A1/A1)::t1) ≈ Matrix(A1) / Matrix(A1)
        @test ((A1\A1)::t1) ≈ Matrix(A1) \ Matrix(A1)

        # Begin loop for second Triangular matrix
        for elty2 in (Float32, Float64, BigFloat, ComplexF32, ComplexF64, Complex{BigFloat}, Int)
            for (t2, uplo2) in ((UpperTriangular, :U),
                                (UnitUpperTriangular, :U),
                                (LowerTriangular, :L),
                                (UnitLowerTriangular, :L))

                debug && println("elty1: $elty1, A1: $t1, elty2: $elty2")

                A2 = t2(elty2 == Int ? rand(1:7, n, n) : convert(Matrix{elty2}, (elty2 <: Complex ? complex.(randn(n, n), randn(n, n)) : randn(n, n)) |> t -> cholesky(t't).U |> t -> uplo2 == :U ? t : copy(t')))

                # Convert
                if elty1 <: Real && !(elty2 <: Integer)
                    @test convert(AbstractMatrix{elty2}, A1) == t1(convert(Matrix{elty2}, A1.data))
                elseif elty2 <: Real && !(elty1 <: Integer)
                    @test_throws InexactError convert(AbstractMatrix{elty2}, A1) == t1(convert(Matrix{elty2}, A1.data))
                end

                # Binary operations
                @test A1 + A2 == Matrix(A1) + Matrix(A2)
                @test A1 - A2 == Matrix(A1) - Matrix(A2)

                # Triangular-Triangualar multiplication and division
                @test A1*A2 ≈ Matrix(A1)*Matrix(A2)
                @test transpose(A1)*A2 ≈ transpose(Matrix(A1))*Matrix(A2)
                @test transpose(A1)*adjoint(A2) ≈ transpose(Matrix(A1))*adjoint(Matrix(A2))
                @test adjoint(A1)*transpose(A2) ≈ adjoint(Matrix(A1))*transpose(Matrix(A2))
                @test A1'A2 ≈ Matrix(A1)'Matrix(A2)
                @test A1*transpose(A2) ≈ Matrix(A1)*transpose(Matrix(A2))
                @test A1*A2' ≈ Matrix(A1)*Matrix(A2)'
                @test transpose(A1)*transpose(A2) ≈ transpose(Matrix(A1))*transpose(Matrix(A2))
                @test A1'A2' ≈ Matrix(A1)'Matrix(A2)'
                @test A1/A2 ≈ Matrix(A1)/Matrix(A2)
                @test A1\A2 ≈ Matrix(A1)\Matrix(A2)
                if uplo1 === :U && uplo2 === :U
                    if t1 === UnitUpperTriangular && t2 === UnitUpperTriangular
                        @test A1*A2 isa UnitUpperTriangular
                        @test A1/A2 isa UnitUpperTriangular
                        @test A1\A2 isa UnitUpperTriangular
                    else
                        @test A1*A2 isa UpperTriangular
                        @test A1/A2 isa UpperTriangular
                        @test A1\A2 isa UpperTriangular
                    end
                elseif uplo1 === :L && uplo2 === :L
                    if t1 === UnitLowerTriangular && t2 === UnitLowerTriangular
                        @test A1*A2 isa UnitLowerTriangular
                        @test A1/A2 isa UnitLowerTriangular
                        @test A1\A2 isa UnitLowerTriangular
                    else
                        @test A1*A2 isa LowerTriangular
                        @test A1/A2 isa LowerTriangular
                        @test A1\A2 isa LowerTriangular
                    end
                end
                offsizeA = Matrix{Float64}(I, n+1, n+1)
                @test_throws DimensionMismatch offsizeA / A2
                @test_throws DimensionMismatch offsizeA / transpose(A2)
                @test_throws DimensionMismatch offsizeA / A2'
                @test_throws DimensionMismatch offsizeA * A2
                @test_throws DimensionMismatch offsizeA * transpose(A2)
                @test_throws DimensionMismatch offsizeA * A2'
                @test_throws DimensionMismatch transpose(A2) * offsizeA
                @test_throws DimensionMismatch A2'  * offsizeA
                @test_throws DimensionMismatch A2   * offsizeA
                if (uplo1 == uplo2 && elty1 == elty2 != Int && t1 != UnitLowerTriangular && t1 != UnitUpperTriangular)
                    @test rdiv!(copy(A1), copy(A2)) ≈ A1/A2 ≈ Matrix(A1)/Matrix(A2)
                end
                if (uplo1 != uplo2 && elty1 == elty2 != Int && t2 != UnitLowerTriangular && t2 != UnitUpperTriangular)
                    @test lmul!(adjoint(copy(A1)), copy(A2)) ≈ A1'*A2 ≈ Matrix(A1)'*Matrix(A2)
                    @test lmul!(transpose(copy(A1)), copy(A2)) ≈ transpose(A1)*A2 ≈ transpose(Matrix(A1))*Matrix(A2)
                    @test ldiv!(adjoint(copy(A1)), copy(A2)) ≈ A1'\A2 ≈ Matrix(A1)'\Matrix(A2)
                    @test ldiv!(transpose(copy(A1)), copy(A2)) ≈ transpose(A1)\A2 ≈ transpose(Matrix(A1))\Matrix(A2)
                end
                if (uplo1 != uplo2 && elty1 == elty2 != Int && t1 != UnitLowerTriangular && t1 != UnitUpperTriangular)
                    @test rmul!(copy(A1), adjoint(copy(A2))) ≈ A1*A2' ≈ Matrix(A1)*Matrix(A2)'
                    @test rmul!(copy(A1), transpose(copy(A2))) ≈ A1*transpose(A2) ≈ Matrix(A1)*transpose(Matrix(A2))
                    @test rdiv!(copy(A1), adjoint(copy(A2))) ≈ A1/A2' ≈ Matrix(A1)/Matrix(A2)'
                    @test rdiv!(copy(A1), transpose(copy(A2))) ≈ A1/transpose(A2) ≈ Matrix(A1)/transpose(Matrix(A2))
                end
            end
        end

        for eltyB in (Float32, Float64, BigFloat, ComplexF32, ComplexF64, Complex{BigFloat})
            B = convert(Matrix{eltyB}, (elty1 <: Complex ? real(A1) : A1)*fill(1., n, n))

            debug && println("elty1: $elty1, A1: $t1, B: $eltyB")

            if !(eltyB in (BigFloat, Complex{BigFloat})) # rand does not support BigFloat and Complex{BigFloat} as of Dec 2015
                Tri = Tridiagonal(rand(eltyB,n-1),rand(eltyB,n),rand(eltyB,n-1))
                @test lmul!(Tri,copy(A1)) ≈ Tri*Matrix(A1)
                Tri = Tridiagonal(rand(eltyB,n-1),rand(eltyB,n),rand(eltyB,n-1))
                C = Matrix{promote_type(elty1,eltyB)}(undef, n, n)
                mul!(C, Tri, copy(A1))
                @test C ≈ Tri*Matrix(A1)
                Tri = Tridiagonal(rand(eltyB,n-1),rand(eltyB,n),rand(eltyB,n-1))
                mul!(C, copy(A1), Tri)
                @test C ≈ Matrix(A1)*Tri
            end

            # Triangular-dense Matrix/vector multiplication
            @test A1*B[:,1] ≈ Matrix(A1)*B[:,1]
            @test A1*B ≈ Matrix(A1)*B
            @test transpose(A1)*B[:,1] ≈ transpose(Matrix(A1))*B[:,1]
            @test A1'B[:,1] ≈ Matrix(A1)'B[:,1]
            @test transpose(A1)*B ≈ transpose(Matrix(A1))*B
            @test A1'B ≈ Matrix(A1)'B
            @test A1*transpose(B) ≈ Matrix(A1)*transpose(B)
            @test adjoint(A1)*transpose(B) ≈ Matrix(A1)'*transpose(B)
            @test transpose(A1)*adjoint(B) ≈ transpose(Matrix(A1))*adjoint(B)
            @test A1*B' ≈ Matrix(A1)*B'
            @test B*A1 ≈ B*Matrix(A1)
            @test transpose(B[:,1])*A1 ≈ transpose(B[:,1])*Matrix(A1)
            @test B[:,1]'A1 ≈ B[:,1]'Matrix(A1)
            @test transpose(B)*A1 ≈ transpose(B)*Matrix(A1)
            @test transpose(B)*adjoint(A1) ≈ transpose(B)*Matrix(A1)'
            @test adjoint(B)*transpose(A1) ≈ adjoint(B)*transpose(Matrix(A1))
            @test B'A1 ≈ B'Matrix(A1)
            @test B*transpose(A1) ≈ B*transpose(Matrix(A1))
            @test B*A1' ≈ B*Matrix(A1)'
            @test transpose(B[:,1])*transpose(A1) ≈ transpose(B[:,1])*transpose(Matrix(A1))
            @test B[:,1]'A1' ≈ B[:,1]'Matrix(A1)'
            @test transpose(B)*transpose(A1) ≈ transpose(B)*transpose(Matrix(A1))
            @test B'A1' ≈ B'Matrix(A1)'

            if eltyB == elty1
                @test mul!(similar(B), A1, B) ≈ Matrix(A1)*B
                @test mul!(similar(B), A1, adjoint(B)) ≈ Matrix(A1)*B'
                @test mul!(similar(B), A1, transpose(B)) ≈ Matrix(A1)*transpose(B)
                @test mul!(similar(B), adjoint(A1), adjoint(B)) ≈ Matrix(A1)'*B'
                @test mul!(similar(B), transpose(A1), transpose(B)) ≈ transpose(Matrix(A1))*transpose(B)
                @test mul!(similar(B), transpose(A1), adjoint(B)) ≈ transpose(Matrix(A1))*B'
                @test mul!(similar(B), adjoint(A1), transpose(B)) ≈ Matrix(A1)'*transpose(B)
                @test mul!(similar(B), adjoint(A1), B) ≈ Matrix(A1)'*B
                @test mul!(similar(B), transpose(A1), B) ≈ transpose(Matrix(A1))*B
                # test also vector methods
                B1 = vec(B[1,:])
                @test mul!(similar(B1), A1, B1)  ≈ Matrix(A1)*B1
                @test mul!(similar(B1), adjoint(A1), B1) ≈ Matrix(A1)'*B1
                @test mul!(similar(B1), transpose(A1), B1) ≈ transpose(Matrix(A1))*B1
            end
            #error handling
            Ann, Bmm, bm = A1, Matrix{eltyB}(undef, n+1, n+1), Vector{eltyB}(undef, n+1)
            @test_throws DimensionMismatch lmul!(Ann, bm)
            @test_throws DimensionMismatch rmul!(Bmm, Ann)
            @test_throws DimensionMismatch lmul!(transpose(Ann), bm)
            @test_throws DimensionMismatch lmul!(adjoint(Ann), bm)
            @test_throws DimensionMismatch rmul!(Bmm, adjoint(Ann))
            @test_throws DimensionMismatch rmul!(Bmm, transpose(Ann))

            # ... and division
            @test A1\B[:,1] ≈ Matrix(A1)\B[:,1]
            @test A1\B ≈ Matrix(A1)\B
            @test transpose(A1)\B[:,1] ≈ transpose(Matrix(A1))\B[:,1]
            @test A1'\B[:,1] ≈ Matrix(A1)'\B[:,1]
            @test transpose(A1)\B ≈ transpose(Matrix(A1))\B
            @test A1'\B ≈ Matrix(A1)'\B
            @test A1\transpose(B) ≈ Matrix(A1)\transpose(B)
            @test A1\B' ≈ Matrix(A1)\B'
            @test transpose(A1)\transpose(B) ≈ transpose(Matrix(A1))\transpose(B)
            @test A1'\B' ≈ Matrix(A1)'\B'
            Ann, bm = A1, Vector{elty1}(undef,n+1)
            @test_throws DimensionMismatch Ann\bm
            @test_throws DimensionMismatch Ann'\bm
            @test_throws DimensionMismatch transpose(Ann)\bm
            if t1 == UpperTriangular || t1 == LowerTriangular
                if elty1 === eltyB <: BlasFloat
                    @test_throws LAPACKException ldiv!(t1(zeros(elty1, n, n)), fill(eltyB(1), n))
                else
                    @test_throws SingularException ldiv!(t1(zeros(elty1, n, n)), fill(eltyB(1), n))
                end
            end
            @test B/A1 ≈ B/Matrix(A1)
            @test B/transpose(A1) ≈ B/transpose(Matrix(A1))
            @test B/A1' ≈ B/Matrix(A1)'
            @test transpose(B)/A1 ≈ transpose(B)/Matrix(A1)
            @test B'/A1 ≈ B'/Matrix(A1)
            @test transpose(B)/transpose(A1) ≈ transpose(B)/transpose(Matrix(A1))
            @test B'/A1' ≈ B'/Matrix(A1)'

            # Error bounds
            !(elty1 in (BigFloat, Complex{BigFloat})) && !(eltyB in (BigFloat, Complex{BigFloat})) && errorbounds(A1, A1\B, B)

        end
    end
end

# Matrix square root
Atn = UpperTriangular([-1 1 2; 0 -2 2; 0 0 -3])
Atp = UpperTriangular([1 1 2; 0 2 2; 0 0 3])
Atu = UnitUpperTriangular([1 1 2; 0 1 2; 0 0 1])
@test sqrt(Atn) |> t->t*t ≈ Atn
@test sqrt(Atn) isa UpperTriangular
@test typeof(sqrt(Atn)[1,1]) <: Complex
@test sqrt(Atp) |> t->t*t ≈ Atp
@test sqrt(Atp) isa UpperTriangular
@test typeof(sqrt(Atp)[1,1]) <: Real
@test typeof(sqrt(complex(Atp))[1,1]) <: Complex
@test sqrt(Atu) |> t->t*t ≈ Atu
@test sqrt(Atu) isa UnitUpperTriangular
@test typeof(sqrt(Atu)[1,1]) <: Real
@test typeof(sqrt(complex(Atu))[1,1]) <: Complex

@testset "check matrix logarithm type-inferrable" for elty in (Float32,Float64,ComplexF32,ComplexF64)
    A = UpperTriangular(exp(triu(randn(elty, n, n))))
    @inferred Union{typeof(A),typeof(complex(A))} log(A)
    @test exp(Matrix(log(A))) ≈ A
    if elty <: Real
        @test typeof(log(A)) <: UpperTriangular{elty}
        @test typeof(log(complex(A))) <: UpperTriangular{complex(elty)}
        @test isreal(log(complex(A)))
        @test log(complex(A)) ≈ log(A)
    end

    Au = UnitUpperTriangular(exp(triu(randn(elty, n, n), 1)))
    @inferred Union{typeof(A),typeof(complex(A))} log(Au)
    @test exp(Matrix(log(Au))) ≈ Au
    if elty <: Real
        @test typeof(log(Au)) <: UpperTriangular{elty}
        @test typeof(log(complex(Au))) <: UpperTriangular{complex(elty)}
        @test isreal(log(complex(Au)))
        @test log(complex(Au)) ≈ log(Au)
    end
end

Areal   = randn(n, n)/2
Aimg    = randn(n, n)/2
A2real  = randn(n, n)/2
A2img   = randn(n, n)/2

for eltya in (Float32, Float64, ComplexF32, ComplexF64, BigFloat, Int)
    A = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(Areal, Aimg) : Areal)
    # a2 = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex.(a2real, a2img) : a2real)
    εa = eps(abs(float(one(eltya))))

    for eltyb in (Float32, Float64, ComplexF32, ComplexF64)
        εb = eps(abs(float(one(eltyb))))
        ε = max(εa,εb)

        debug && println("\ntype of A: ", eltya, " type of b: ", eltyb, "\n")

        debug && println("Solve upper triangular system")
        Atri = UpperTriangular(lu(A).U) |> t -> eltya <: Complex && eltyb <: Real ? real(t) : t # Here the triangular matrix can't be too badly conditioned
        b = convert(Matrix{eltyb}, Matrix(Atri)*fill(1., n, 2))
        x = Matrix(Atri) \ b

        debug && println("Test error estimates")
        if eltya != BigFloat && eltyb != BigFloat
            for i = 1:2
                @test  norm(x[:,1] .- 1) <= errorbounds(UpperTriangular(A), x, b)[1][i]
            end
        end
        debug && println("Test forward error [JIN 5705] if this is not a BigFloat")

        x = Atri \ b
        γ = n*ε/(1 - n*ε)
        if eltya != BigFloat
            bigA = big.(Atri)
            x̂ = fill(1., n, 2)
            for i = 1:size(b, 2)
                @test norm(x̂[:,i] - x[:,i], Inf)/norm(x̂[:,i], Inf) <= condskeel(bigA, x̂[:,i])*γ/(1 - condskeel(bigA)*γ)
            end
        end

        debug && println("Test backward error [JIN 5705]")
        for i = 1:size(b, 2)
            @test norm(abs.(b[:,i] - Atri*x[:,i]), Inf) <= γ * norm(Atri, Inf) * norm(x[:,i], Inf)
        end

        debug && println("Solve lower triangular system")
        Atri = UpperTriangular(lu(A).U) |> t -> eltya <: Complex && eltyb <: Real ? real(t) : t # Here the triangular matrix can't be too badly conditioned
        b = convert(Matrix{eltyb}, Matrix(Atri)*fill(1., n, 2))
        x = Matrix(Atri)\b

        debug && println("Test error estimates")
        if eltya != BigFloat && eltyb != BigFloat
            for i = 1:2
                @test  norm(x[:,1] .- 1) <= errorbounds(UpperTriangular(A), x, b)[1][i]
            end
        end

        debug && println("Test forward error [JIN 5705] if this is not a BigFloat")
        b = (b0 = Atri*fill(1, n, 2); convert(Matrix{eltyb}, eltyb == Int ? trunc.(b0) : b0))
        x = Atri \ b
        γ = n*ε/(1 - n*ε)
        if eltya != BigFloat
            bigA = big.(Atri)
            x̂ = fill(1., n, 2)
            for i = 1:size(b, 2)
                @test norm(x̂[:,i] - x[:,i], Inf)/norm(x̂[:,i], Inf) <= condskeel(bigA, x̂[:,i])*γ/(1 - condskeel(bigA)*γ)
            end
        end

        debug && println("Test backward error [JIN 5705]")
        for i = 1:size(b, 2)
            @test norm(abs.(b[:,i] - Atri*x[:,i]), Inf) <= γ * norm(Atri, Inf) * norm(x[:,i], Inf)
        end
    end
end

# Issue 10742 and similar
@test istril(UpperTriangular(diagm(0 => [1,2,3,4])))
@test istriu(LowerTriangular(diagm(0 => [1,2,3,4])))
@test isdiag(UpperTriangular(diagm(0 => [1,2,3,4])))
@test isdiag(LowerTriangular(diagm(0 => [1,2,3,4])))
@test !isdiag(UpperTriangular(rand(4, 4)))
@test !isdiag(LowerTriangular(rand(4, 4)))

# Test throwing in fallbacks for non BlasFloat/BlasComplex in A_rdiv_Bx!
let n = 5
    A = rand(Float16, n, n)
    B = rand(Float16, n-1, n-1)
    @test_throws DimensionMismatch rdiv!(A, LowerTriangular(B))
    @test_throws DimensionMismatch rdiv!(A, UpperTriangular(B))
    @test_throws DimensionMismatch rdiv!(A, UnitLowerTriangular(B))
    @test_throws DimensionMismatch rdiv!(A, UnitUpperTriangular(B))

    @test_throws DimensionMismatch rdiv!(A, adjoint(LowerTriangular(B)))
    @test_throws DimensionMismatch rdiv!(A, adjoint(UpperTriangular(B)))
    @test_throws DimensionMismatch rdiv!(A, adjoint(UnitLowerTriangular(B)))
    @test_throws DimensionMismatch rdiv!(A, adjoint(UnitUpperTriangular(B)))

    @test_throws DimensionMismatch rdiv!(A, transpose(LowerTriangular(B)))
    @test_throws DimensionMismatch rdiv!(A, transpose(UpperTriangular(B)))
    @test_throws DimensionMismatch rdiv!(A, transpose(UnitLowerTriangular(B)))
    @test_throws DimensionMismatch rdiv!(A, transpose(UnitUpperTriangular(B)))
end

@test isdiag(LowerTriangular(UpperTriangular(randn(3,3))))
@test isdiag(UpperTriangular(LowerTriangular(randn(3,3))))

# Issue 16196
@test UpperTriangular(Matrix(1.0I, 3, 3)) \ view(fill(1., 3), [1,2,3]) == fill(1., 3)

# dimensional correctness:
const BASE_TEST_PATH = joinpath(Sys.BINDIR, "..", "share", "julia", "test")
isdefined(Main, :Furlongs) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "Furlongs.jl"))
using .Main.Furlongs
LinearAlgebra.sylvester(a::Furlong,b::Furlong,c::Furlong) = -c / (a + b)

let A = UpperTriangular([Furlong(1) Furlong(4); Furlong(0) Furlong(1)])
    @test sqrt(A) == Furlong{1//2}.(UpperTriangular([1 2; 0 1]))
end

@testset "similar should preserve underlying storage type" begin
    local m, n = 4, 3
    sparsemat = sprand(m, m, 0.5)
    for TriType in (UpperTriangular, LowerTriangular, UnitUpperTriangular, UnitLowerTriangular)
        trisparsemat = TriType(sparsemat)
        @test isa(similar(trisparsemat), typeof(trisparsemat))
        @test isa(similar(trisparsemat, Float32), TriType{Float32,<:SparseMatrixCSC{Float32}})
        @test isa(similar(trisparsemat, (n, n)), typeof(sparsemat))
        @test isa(similar(trisparsemat, Float32, (n, n)), SparseMatrixCSC{Float32})
    end
end

isdefined(Main, :ImmutableArrays) || @eval Main include(joinpath($(BASE_TEST_PATH), "testhelpers", "ImmutableArrays.jl"))
using .Main.ImmutableArrays

@testset "AbstractArray constructor should preserve underlying storage type" begin
    # tests corresponding to #34995
    local m = 4
    local T, S = Float32, Float64
    immutablemat = ImmutableArray(randn(T,m,m))
    for TriType in (UpperTriangular, LowerTriangular, UnitUpperTriangular, UnitLowerTriangular)
        trimat = TriType(immutablemat)
        @test convert(AbstractArray{S}, trimat).data isa ImmutableArray{S}
        @test convert(AbstractMatrix{S}, trimat).data isa ImmutableArray{S}
        @test AbstractArray{S}(trimat).data isa ImmutableArray{S}
        @test AbstractMatrix{S}(trimat).data isa ImmutableArray{S}
        @test convert(AbstractArray{S}, trimat) == trimat
        @test convert(AbstractMatrix{S}, trimat) == trimat
    end
end

@testset "inplace mul of appropriate types should preserve triagular structure" begin
    for elty1 in (Float64, ComplexF32), elty2 in (Float64, ComplexF32)
        T = promote_type(elty1, elty2)
        M1 = rand(elty1, 5, 5)
        M2 = rand(elty2, 5, 5)
        A = UpperTriangular(M1)
        A2 = UpperTriangular(M2)
        Au = UnitUpperTriangular(M1)
        Au2 = UnitUpperTriangular(M2)
        B = LowerTriangular(M1)
        B2 = LowerTriangular(M2)
        Bu = UnitLowerTriangular(M1)
        Bu2 = UnitLowerTriangular(M2)

        @test mul!(similar(A), A, A)::typeof(A) == A*A
        @test mul!(similar(A, T), A, A2) ≈ A*A2
        @test mul!(similar(A, T), A2, A) ≈ A2*A
        @test mul!(typeof(similar(A, T))(A), A, A2, 2.0, 3.0) ≈ 2.0*A*A2 + 3.0*A
        @test mul!(typeof(similar(A2, T))(A2), A2, A, 2.0, 3.0) ≈ 2.0*A2*A + 3.0*A2

        @test mul!(similar(A), A, Au)::typeof(A) == A*Au
        @test mul!(similar(A), Au, A)::typeof(A) == Au*A
        @test mul!(similar(Au), Au, Au)::typeof(Au) == Au*Au
        @test mul!(similar(A, T), A, Au2) ≈ A*Au2
        @test mul!(similar(A, T), Au2, A) ≈ Au2*A
        @test mul!(similar(Au2), Au2, Au2) == Au2*Au2

        @test mul!(similar(B), B, B)::typeof(B) == B*B
        @test mul!(similar(B, T), B, B2) ≈ B*B2
        @test mul!(similar(B, T), B2, B) ≈ B2*B
        @test mul!(typeof(similar(B, T))(B), B, B2, 2.0, 3.0) ≈ 2.0*B*B2 + 3.0*B
        @test mul!(typeof(similar(B2, T))(B2), B2, B, 2.0, 3.0) ≈ 2.0*B2*B + 3.0*B2

        @test mul!(similar(B), B, Bu)::typeof(B) == B*Bu
        @test mul!(similar(B), Bu, B)::typeof(B) == Bu*B
        @test mul!(similar(Bu), Bu, Bu)::typeof(Bu) == Bu*Bu
        @test mul!(similar(B, T), B, Bu2) ≈ B*Bu2
        @test mul!(similar(B, T), Bu2, B) ≈ Bu2*B
    end
end

@testset "special printing of Lower/UpperTriangular" begin
    @test occursin(r"3×3 (LinearAlgebra\.)?LowerTriangular{Int64, Matrix{Int64}}:\n 2  ⋅  ⋅\n 2  2  ⋅\n 2  2  2",
                   sprint(show, MIME"text/plain"(), LowerTriangular(2ones(Int64,3,3))))
    @test occursin(r"3×3 (LinearAlgebra\.)?UnitLowerTriangular{Int64, Matrix{Int64}}:\n 1  ⋅  ⋅\n 2  1  ⋅\n 2  2  1",
                   sprint(show, MIME"text/plain"(), UnitLowerTriangular(2ones(Int64,3,3))))
    @test occursin(r"3×3 (LinearAlgebra\.)?UpperTriangular{Int64, Matrix{Int64}}:\n 2  2  2\n ⋅  2  2\n ⋅  ⋅  2",
                   sprint(show, MIME"text/plain"(), UpperTriangular(2ones(Int64,3,3))))
    @test occursin(r"3×3 (LinearAlgebra\.)?UnitUpperTriangular{Int64, Matrix{Int64}}:\n 1  2  2\n ⋅  1  2\n ⋅  ⋅  1",
                   sprint(show, MIME"text/plain"(), UnitUpperTriangular(2ones(Int64,3,3))))
end

@testset "adjoint/transpose triangular/vector multiplication" begin
    for elty in (Float64, ComplexF64), trity in (UpperTriangular, LowerTriangular)
        A1 = trity(rand(elty, 1, 1))
        b1 = rand(elty, 1)
        A4 = trity(rand(elty, 4, 4))
        b4 = rand(elty, 4)
        @test A1 * b1' ≈ Matrix(A1) * b1'
        @test_throws DimensionMismatch A4 * b4'
        @test A1 * transpose(b1) ≈ Matrix(A1) * transpose(b1)
        @test_throws DimensionMismatch A4 * transpose(b4)
        @test A1' * b1' ≈ Matrix(A1') * b1'
        @test_throws DimensionMismatch A4' * b4'
        @test A1' * transpose(b1) ≈  Matrix(A1') * transpose(b1)
        @test_throws DimensionMismatch A4' * transpose(b4)
        @test transpose(A1) * transpose(b1) ≈  Matrix(transpose(A1)) * transpose(b1)
        @test_throws DimensionMismatch transpose(A4) * transpose(b4)
        @test transpose(A1) * b1' ≈ Matrix(transpose(A1)) * b1'
        @test_throws DimensionMismatch transpose(A4) * b4'
        @test b1' * transpose(A1) ≈ b1' * Matrix(transpose(A1))
        @test b4' * transpose(A4) ≈ b4' * Matrix(transpose(A4))
        @test transpose(b1) * A1' ≈ transpose(b1) * Matrix(A1')
        @test transpose(b4) * A4' ≈ transpose(b4) * Matrix(A4')
    end
end

@testset "Error condition for powm" begin
    A = UpperTriangular(rand(ComplexF64, 10, 10))
    @test_throws ArgumentError LinearAlgebra.powm!(A, 2.2)
    A = LowerTriangular(rand(ComplexF64, 10, 10))
    At = copy(transpose(A))
    p = rand()
    @test LinearAlgebra.powm(A, p) == transpose(LinearAlgebra.powm!(At, p))
    @test_throws ArgumentError LinearAlgebra.powm(A, 2.2)
end

# Issue 35058
let A = [0.9999999999999998 4.649058915617843e-16 -1.3149405273715513e-16 9.9959579317056e-17; -8.326672684688674e-16 1.0000000000000004 2.9280733590254494e-16 -2.9993900031619594e-16; 9.43689570931383e-16 -1.339206523454095e-15 1.0000000000000007 -8.550505126287743e-16; -6.245004513516506e-16 -2.0122792321330962e-16 1.183061278035052e-16 1.0000000000000002],
    B = [0.09648289218436859 0.023497875751503007 0.0 0.0; 0.023497875751503007 0.045787575150300804 0.0 0.0; 0.0 0.0 0.0 0.0; 0.0 0.0 0.0 0.0]
    @test sqrt(A*B*A')^2 ≈ A*B*A'
end
end # module TestTriangular

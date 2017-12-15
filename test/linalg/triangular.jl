# This file is a part of Julia. License is MIT: https://julialang.org/license

debug = false
using Test
using Base.LinAlg: BlasFloat, errorbounds, full!, naivesub!, transpose!,
                    UnitUpperTriangular, UnitLowerTriangular,
                    mul!, rdiv!, Adjoint, Transpose

debug && println("Triangular matrices")

n = 9
srand(123)

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
        A1 = t1(elty1 == Int ? rand(1:7, n, n) : convert(Matrix{elty1}, (elty1 <: Complex ? complex.(randn(n, n), randn(n, n)) : randn(n, n)) |> t -> chol(t't) |> t -> uplo1 == :U ? t : adjoint(t)))


        debug && println("elty1: $elty1, A1: $t1")

        # Convert
        @test convert(AbstractMatrix{elty1}, A1) == A1
        @test convert(Matrix, A1) == A1

        # full!
        @test full!(copy(A1)) == A1

        # similar
        @test isa(similar(A1), t1)
        @test eltype(similar(A1)) == elty1
        @test isa(similar(A1, Int), t1)
        @test eltype(similar(A1, Int)) == Int
        @test isa(similar(A1, (3,2)), Matrix{elty1})
        @test isa(similar(A1, Int, (3,2)), Matrix{Int})

        #copy!
        simA1 = similar(A1)
        copy!(simA1, A1)
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
        else
            @test istriu(A1)
            @test !istril(A1)
        end

        #tril/triu
        if uplo1 == :L
            @test tril(A1,0)  == A1
            @test tril(A1,-1) == LowerTriangular(tril(Matrix(A1), -1))
            @test tril(A1,1)  == t1(tril(tril(Matrix(A1), 1)))
            @test_throws ArgumentError tril!(A1, -n - 2)
            @test_throws ArgumentError tril!(A1, n)
            @test triu(A1,0)  == t1(diagm(0 => diag(A1)))
            @test triu(A1,-1) == t1(tril(triu(A1.data,-1)))
            @test triu(A1,1)  == zeros(size(A1)) # or just @test iszero(triu(A1,1))?
            @test_throws ArgumentError triu!(A1, -n)
            @test_throws ArgumentError triu!(A1, n + 2)
        else
            @test triu(A1,0)  == A1
            @test triu(A1,1)  == UpperTriangular(triu(Matrix(A1), 1))
            @test triu(A1,-1) == t1(triu(triu(Matrix(A1), -1)))
            @test_throws ArgumentError triu!(A1, -n)
            @test_throws ArgumentError triu!(A1, n + 2)
            @test tril(A1,0)  == t1(diagm(0 => diag(A1)))
            @test tril(A1,1)  == t1(triu(tril(A1.data,1)))
            @test tril(A1,-1) == zeros(size(A1)) # or just @test iszero(tril(A1,-1))?
            @test_throws ArgumentError tril!(A1, -n - 2)
            @test_throws ArgumentError tril!(A1, n)
        end

        # factorize
        @test factorize(A1) == A1

        # [c]transpose[!] (test views as well, see issue #14317)
        let vrange = 1:n-1, viewA1 = t1(view(A1.data, vrange, vrange))
            # transpose
            @test A1.' == Matrix(A1).'
            @test viewA1.' == Matrix(viewA1).'
            # adjoint
            @test A1' == Matrix(A1)'
            @test viewA1' == Matrix(viewA1)'
            # transpose!
            @test transpose!(copy(A1)) == A1.'
            @test transpose!(t1(view(copy(A1).data, vrange, vrange))) == viewA1.'
            # adjoint!
            @test adjoint!(copy(A1)) == A1'
            @test adjoint!(t1(view(copy(A1).data, vrange, vrange))) == viewA1'
        end

        # diag
        @test diag(A1) == diag(Matrix(A1))

        # real
        @test real(A1) == real(Matrix(A1))
        @test imag(A1) == imag(Matrix(A1))
        @test abs.(A1) == abs.(Matrix(A1))

        # Unary operations
        @test -A1 == -Matrix(A1)

        # copy and copy! (test views as well, see issue #14317)
        let vrange = 1:n-1, viewA1 = t1(view(A1.data, vrange, vrange))
            # copy
            @test copy(A1) == copy(Matrix(A1))
            @test copy(viewA1) == copy(Matrix(viewA1))
            # copy!
            B = similar(A1)
            copy!(B, A1)
            @test B == A1
            B = similar(A1.')
            copy!(B, A1.')
            @test B == A1.'
            B = similar(viewA1)
            copy!(B, viewA1)
            @test B == viewA1
            B = similar(viewA1.')
            copy!(B, viewA1.')
            @test B == viewA1.'
        end

        #exp/log
        if (elty1 == Float64 || elty1 == ComplexF64) && (t1 == UpperTriangular || t1 == LowerTriangular)
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
                scale!(A1tmp,cr)
                @test A1tmp == cr*A1
                A1tmp = copy(A1)
                scale!(cr,A1tmp)
                @test A1tmp == cr*A1
                A1tmp = copy(A1)
                A2tmp = unitt(A1)
                scale!(A1tmp,A2tmp,cr)
                @test A1tmp == cr * A2tmp
            else
                A1tmp = copy(A1)
                scale!(A1tmp,ci)
                @test A1tmp == ci*A1
                A1tmp = copy(A1)
                scale!(ci,A1tmp)
                @test A1tmp == ci*A1
                A1tmp = copy(A1)
                A2tmp = unitt(A1)
                scale!(A1tmp,A2tmp,ci)
                @test A1tmp == ci * A2tmp
            end
        end

        # Binary operations
        @test A1*0.5 == Matrix(A1)*0.5
        @test 0.5*A1 == 0.5*Matrix(A1)
        @test A1/0.5 == Matrix(A1)/0.5
        @test 0.5\A1 == 0.5\Matrix(A1)

        # inversion
        @test inv(A1) ≈ inv(lufact(Matrix(A1)))
        inv(Matrix(A1)) # issue #11298
        @test isa(inv(A1), t1)
        # make sure the call to LAPACK works right
        if elty1 <: BlasFloat
            @test Base.LinAlg.inv!(copy(A1)) ≈ inv(lufact(Matrix(A1)))
        end

        # Determinant
        @test det(A1) ≈ det(lufact(Matrix(A1))) atol=sqrt(eps(real(float(one(elty1)))))*n*n
        @test logdet(A1) ≈ logdet(lufact(Matrix(A1))) atol=sqrt(eps(real(float(one(elty1)))))*n*n
        lada, ladb = logabsdet(A1)
        flada, fladb = logabsdet(lufact(Matrix(A1)))
        @test lada ≈ flada atol=sqrt(eps(real(float(one(elty1)))))*n*n
        @test ladb ≈ fladb atol=sqrt(eps(real(float(one(elty1)))))*n*n

        # Matrix square root
        @test sqrt(A1) |> t -> t*t ≈ A1

        # naivesub errors
        @test_throws DimensionMismatch naivesub!(A1,ones(elty1,n+1))

        # eigenproblems
        if !(elty1 in (BigFloat, Complex{BigFloat})) # Not handled yet
            vals, vecs = eig(A1)
            if (t1 == UpperTriangular || t1 == LowerTriangular) && elty1 != Int # Cannot really handle degenerate eigen space and Int matrices will probably have repeated eigenvalues.
                @test vecs*diagm(0 => vals)/vecs ≈ A1 atol=sqrt(eps(float(real(one(vals[1])))))*(norm(A1,Inf)*n)^2
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
            svdfact(A1)
            elty1 <: BlasFloat && svdfact!(copy(A1))
            svdvals(A1)
        end

        # Begin loop for second Triangular matrix
        for elty2 in (Float32, Float64, BigFloat, ComplexF32, ComplexF64, Complex{BigFloat}, Int)
            for (t2, uplo2) in ((UpperTriangular, :U),
                                (UnitUpperTriangular, :U),
                                (LowerTriangular, :L),
                                (UnitLowerTriangular, :L))

                debug && println("elty1: $elty1, A1: $t1, elty2: $elty2")

                A2 = t2(elty2 == Int ? rand(1:7, n, n) : convert(Matrix{elty2}, (elty2 <: Complex ? complex.(randn(n, n), randn(n, n)) : randn(n, n)) |> t -> chol(t't) |> t -> uplo2 == :U ? t : adjoint(t)))

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
                @test A1.'A2 ≈ Matrix(A1).'Matrix(A2)
                @test A1'A2 ≈ Matrix(A1)'Matrix(A2)
                @test A1*A2.' ≈ Matrix(A1)*Matrix(A2).'
                @test A1*A2' ≈ Matrix(A1)*Matrix(A2)'
                @test A1.'A2.' ≈ Matrix(A1).'Matrix(A2).'
                @test A1'A2' ≈ Matrix(A1)'Matrix(A2)'
                @test A1/A2 ≈ Matrix(A1)/Matrix(A2)
                @test A1\A2 ≈ Matrix(A1)\Matrix(A2)
                offsizeA = Matrix{Float64}(I, n+1, n+1)
                @test_throws DimensionMismatch offsizeA / A2
                @test_throws DimensionMismatch offsizeA / A2.'
                @test_throws DimensionMismatch offsizeA / A2'
                @test_throws DimensionMismatch offsizeA * A2
                @test_throws DimensionMismatch offsizeA * A2.'
                @test_throws DimensionMismatch offsizeA * A2'
                @test_throws DimensionMismatch A2.' * offsizeA
                @test_throws DimensionMismatch A2'  * offsizeA
                @test_throws DimensionMismatch A2   * offsizeA
            end
        end

        for eltyB in (Float32, Float64, BigFloat, ComplexF32, ComplexF64, Complex{BigFloat})
            B = convert(Matrix{eltyB}, elty1 <: Complex ? real(A1)*ones(n, n) : A1*ones(n, n))

            debug && println("elty1: $elty1, A1: $t1, B: $eltyB")

            if !(eltyB in (BigFloat, Complex{BigFloat})) # rand does not support BigFloat and Complex{BigFloat} as of Dec 2015
                Tri = Tridiagonal(rand(eltyB,n-1),rand(eltyB,n),rand(eltyB,n-1))
                @test mul!(Tri,copy(A1)) ≈ Tri*Matrix(A1)
            end

            # Triangular-dense Matrix/vector multiplication
            @test A1*B[:,1] ≈ Matrix(A1)*B[:,1]
            @test A1*B ≈ Matrix(A1)*B
            @test A1.'B[:,1] ≈ Matrix(A1).'B[:,1]
            @test A1'B[:,1] ≈ Matrix(A1)'B[:,1]
            @test A1.'B ≈ Matrix(A1).'B
            @test A1'B ≈ Matrix(A1)'B
            @test A1*B.' ≈ Matrix(A1)*B.'
            @test A1*B' ≈ Matrix(A1)*B'
            @test B*A1 ≈ B*Matrix(A1)
            @test B[:,1].'A1 ≈ B[:,1].'Matrix(A1)
            @test B[:,1]'A1 ≈ B[:,1]'Matrix(A1)
            @test B.'A1 ≈ B.'Matrix(A1)
            @test B'A1 ≈ B'Matrix(A1)
            @test B*A1.' ≈ B*Matrix(A1).'
            @test B*A1' ≈ B*Matrix(A1)'
            @test B[:,1].'A1.' ≈ B[:,1].'Matrix(A1).'
            @test B[:,1]'A1' ≈ B[:,1]'Matrix(A1)'
            @test B.'A1.' ≈ B.'Matrix(A1).'
            @test B'A1' ≈ B'Matrix(A1)'

            if eltyB == elty1
                @test mul!(similar(B),A1,B)  ≈ A1*B
                @test mul!(similar(B), A1, Adjoint(B)) ≈ A1*B'
                @test mul!(similar(B), A1, Transpose(B)) ≈ A1*B.'
                @test mul!(similar(B), Adjoint(A1), B) ≈ A1'*B
                @test mul!(similar(B), Transpose(A1), B) ≈ A1.'*B
                # test also vector methods
                B1 = vec(B[1,:])
                @test mul!(similar(B1),A1,B1)  ≈ A1*B1
                @test mul!(similar(B1), Adjoint(A1), B1) ≈ A1'*B1
                @test mul!(similar(B1), Transpose(A1), B1) ≈ A1.'*B1
            end
            #error handling
            @test_throws DimensionMismatch mul!(A1, ones(eltyB,n+1))
            @test_throws DimensionMismatch mul!(ones(eltyB,n+1,n+1), A1)
            @test_throws DimensionMismatch mul!(Transpose(A1), ones(eltyB,n+1))
            @test_throws DimensionMismatch mul!(Adjoint(A1), ones(eltyB,n+1))
            @test_throws DimensionMismatch mul!(ones(eltyB,n+1,n+1), Adjoint(A1))
            @test_throws DimensionMismatch mul!(ones(eltyB,n+1,n+1), Transpose(A1))

            # ... and division
            @test A1\B[:,1] ≈ Matrix(A1)\B[:,1]
            @test A1\B ≈ Matrix(A1)\B
            @test A1.'\B[:,1] ≈ Matrix(A1).'\B[:,1]
            @test A1'\B[:,1] ≈ Matrix(A1)'\B[:,1]
            @test A1.'\B ≈ Matrix(A1).'\B
            @test A1'\B ≈ Matrix(A1)'\B
            @test A1\B.' ≈ Matrix(A1)\B.'
            @test A1\B' ≈ Matrix(A1)\B'
            @test A1.'\B.' ≈ Matrix(A1).'\B.'
            @test A1'\B' ≈ Matrix(A1)'\B'
            @test_throws DimensionMismatch A1\ones(elty1,n+2)
            @test_throws DimensionMismatch A1'\ones(elty1,n+2)
            @test_throws DimensionMismatch A1.'\ones(elty1,n+2)
            if t1 == UpperTriangular || t1 == LowerTriangular
                @test_throws Base.LinAlg.SingularException naivesub!(t1(zeros(elty1,n,n)),ones(eltyB,n))
            end
            @test B/A1 ≈ B/Matrix(A1)
            @test B/A1.' ≈ B/Matrix(A1).'
            @test B/A1' ≈ B/Matrix(A1)'
            @test B.'/A1 ≈ B.'/Matrix(A1)
            @test B'/A1 ≈ B'/Matrix(A1)
            @test B.'/A1.' ≈ B.'/Matrix(A1).'
            @test B'/A1' ≈ B'/Matrix(A1)'

            # Error bounds
            !(elty1 in (BigFloat, Complex{BigFloat})) && !(eltyB in (BigFloat, Complex{BigFloat})) && errorbounds(A1, A1\B, B)

        end
    end
end

# Matrix square root
Atn = UpperTriangular([-1 1 2; 0 -2 2; 0 0 -3])
Atp = UpperTriangular([1 1 2; 0 2 2; 0 0 3])
@test sqrt(Atn) |> t->t*t ≈ Atn
@test typeof(sqrt(Atn)[1,1]) <: Complex
@test sqrt(Atp) |> t->t*t ≈ Atp
@test typeof(sqrt(Atp)[1,1]) <: Real

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
        Atri = UpperTriangular(lufact(A)[:U]) |> t -> eltya <: Complex && eltyb <: Real ? real(t) : t # Here the triangular matrix can't be too badly conditioned
        b = convert(Matrix{eltyb}, eltya <: Complex ? Matrix(Atri)*ones(n, 2) : Matrix(Atri)*ones(n, 2))
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
            x̂ = ones(n, 2)
            for i = 1:size(b, 2)
                @test norm(x̂[:,i] - x[:,i], Inf)/norm(x̂[:,i], Inf) <= condskeel(bigA, x̂[:,i])*γ/(1 - condskeel(bigA)*γ)
            end
        end

        debug && println("Test backward error [JIN 5705]")
        for i = 1:size(b, 2)
            @test norm(abs.(b[:,i] - Atri*x[:,i]), Inf) <= γ * norm(Atri, Inf) * norm(x[:,i], Inf)
        end

        debug && println("Solve lower triangular system")
        Atri = UpperTriangular(lufact(A)[:U]) |> t -> eltya <: Complex && eltyb <: Real ? real(t) : t # Here the triangular matrix can't be too badly conditioned
        b = convert(Matrix{eltyb}, eltya <: Complex ? Matrix(Atri)*ones(n, 2) : Matrix(Atri)*ones(n, 2))
        x = Matrix(Atri)\b

        debug && println("Test error estimates")
        if eltya != BigFloat && eltyb != BigFloat
            for i = 1:2
                @test  norm(x[:,1] .- 1) <= errorbounds(UpperTriangular(A), x, b)[1][i]
            end
        end

        debug && println("Test forward error [JIN 5705] if this is not a BigFloat")
        b = eltyb == Int ? trunc.(Int,Atri*ones(n, 2)) : convert(Matrix{eltyb}, Atri*ones(eltya, n, 2))
        x = Atri \ b
        γ = n*ε/(1 - n*ε)
        if eltya != BigFloat
            bigA = big.(Atri)
            x̂ = ones(n, 2)
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

    @test_throws DimensionMismatch rdiv!(A, Adjoint(LowerTriangular(B)))
    @test_throws DimensionMismatch rdiv!(A, Adjoint(UpperTriangular(B)))
    @test_throws DimensionMismatch rdiv!(A, Adjoint(UnitLowerTriangular(B)))
    @test_throws DimensionMismatch rdiv!(A, Adjoint(UnitUpperTriangular(B)))

    @test_throws DimensionMismatch rdiv!(A, Transpose(LowerTriangular(B)))
    @test_throws DimensionMismatch rdiv!(A, Transpose(UpperTriangular(B)))
    @test_throws DimensionMismatch rdiv!(A, Transpose(UnitLowerTriangular(B)))
    @test_throws DimensionMismatch rdiv!(A, Transpose(UnitUpperTriangular(B)))
end

# Test that UpperTriangular(LowerTriangular) throws. See #16201
@test_throws ArgumentError LowerTriangular(UpperTriangular(randn(3,3)))
@test_throws ArgumentError UpperTriangular(LowerTriangular(randn(3,3)))

# Issue 16196
@test UpperTriangular(Matrix(1.0I, 3, 3)) \ view(ones(3), [1,2,3]) == ones(3)

# dimensional correctness:
isdefined(Main, :TestHelpers) || @eval Main include("../TestHelpers.jl")
using Main.TestHelpers.Furlong
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

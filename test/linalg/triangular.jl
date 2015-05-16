# This file is a part of Julia. License is MIT: http://julialang.org/license

debug = false
using Base.Test
using Base.LinAlg: BlasFloat, errorbounds, full!, naivesub!, transpose!, UnitUpperTriangular, UnitLowerTriangular

debug && println("Triangular matrices")

n = 9
srand(123)

debug && println("Test basic type functionality")
@test_throws DimensionMismatch LowerTriangular(randn(5, 4))
@test LowerTriangular(randn(3, 3)) |> t -> [size(t, i) for i = 1:3] == [size(full(t), i) for i = 1:3]

# The following test block tries to call all methods in base/linalg/triangular.jl in order for a combination of input element types. Keep the ordering when adding code.
for elty1 in (Float32, Float64, Complex64, Complex128, BigFloat, Int)
    # Begin loop for first Triangular matrix
    for (t1, uplo1) in ((UpperTriangular, :U),
                        (UnitUpperTriangular, :U),
                        (LowerTriangular, :L),
                        (UnitLowerTriangular, :L))

        # Construct test matrix
        A1 = t1(elty1 == Int ? rand(1:7, n, n) : convert(Matrix{elty1}, (elty1 <: Complex ? complex(randn(n, n), randn(n, n)) : randn(n, n)) |> t -> chol(t't, Val{uplo1})))

        debug && println("elty1: $elty1, A1: $t1")

        # Convert
        @test convert(AbstractMatrix{elty1}, A1) == A1
        @test convert(Matrix, A1) == full(A1)

        # full!
        @test full!(copy(A1)) == full(A1)

        # fill!
        @test full!(fill!(copy(A1), 1)) == full(t1(ones(size(A1)...)))

        # similar
        @test isa(similar(A1), t1)

        # getindex
        ## Linear indexing
        for i = 1:length(A1)
            @test A1[i] == full(A1)[i]
        end

        ## Cartesian indexing
        for i = 1:size(A1, 1)
            for j = 1:size(A1, 2)
                @test A1[i,j] == full(A1)[i,j]
            end
        end

        # setindex! (and copy)
        A1c = copy(A1)
        for i = 1:size(A1, 1)
            for j = 1:size(A1, 2)
                if uplo1 == :U
                    if i > j || (i == j && t1 == UnitUpperTriangular)
                        @test_throws BoundsError A1c[i,j] = 0
                    else
                        A1c[i,j] = 0
                        @test A1c[i,j] == 0
                    end
                else
                    if i < j || (i == j && t1 == UnitLowerTriangular)
                        @test_throws BoundsError A1c[i,j] = 0
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

        # (c)transpose
        @test full(A1') == full(A1)'
        @test full(A1.') == full(A1).'
        @test transpose!(copy(A1)) == A1.'

        # diag
        @test diag(A1) == diag(full(A1))

        # real
        @test full(real(A1)) == real(full(A1))

        # Unary operations
        @test full(-A1) == -full(A1)

        # Binary operations
        @test A1*0.5 == full(A1)*0.5
        @test 0.5*A1 == 0.5*full(A1)
        @test A1/0.5 == full(A1)/0.5
        @test 0.5\A1 == 0.5\full(A1)

        # inversion
        @test_approx_eq inv(A1) inv(lufact(full(A1)))
        inv(full(A1)) # issue #11298
        @test isa(inv(A1), t1)

        # Determinant
        @test_approx_eq_eps det(A1) det(lufact(full(A1))) sqrt(eps(real(float(one(elty1)))))*n*n

        # Matrix square root
        @test_approx_eq sqrtm(A1) |> t->t*t A1

        # eigenproblems
        if elty1 != BigFloat # Not handled yet
            vals, vecs = eig(A1)
            if (t1 == UpperTriangular || t1 == LowerTriangular) && elty1 != Int # Cannot really handle degenerate eigen space and Int matrices will probably have repeated eigenvalues.
                @test_approx_eq_eps vecs*diagm(vals)/vecs full(A1) sqrt(eps(float(real(one(vals[1])))))*(norm(A1, Inf)*n)^2
            end
        end

        # Condition number tests - can be VERY approximate
        if elty1 <:BlasFloat
            for p in (1.0, Inf)
                @test_approx_eq_eps cond(A1, p) cond(A1, p) (cond(A1, p) + cond(A1, p))
            end
        end

        if elty1 != BigFloat # Not implemented yet
            svd(A1)
            svdfact(A1)
            elty1 <: BlasFloat && svdfact!(copy(A1))
            svdvals(A1)
        end

        # Begin loop for second Triangular matrix
        for elty2 in (Float32, Float64, Complex64, Complex128, BigFloat, Int)
            for (t2, uplo2) in ((UpperTriangular, :U),
                                (UnitUpperTriangular, :U),
                                (LowerTriangular, :L),
                                (UnitLowerTriangular, :L))

                debug && println("elty1: $elty1, A1: $t1, elty2: $elty2")

                A2 = t2(elty2 == Int ? rand(1:7, n, n) : convert(Matrix{elty2}, (elty2 <: Complex ? complex(randn(n, n), randn(n, n)) : randn(n, n)) |> t-> chol(t't, Val{uplo2})))

                # Convert
                if elty1 <: Real && !(elty2 <: Integer)
                    @test convert(AbstractMatrix{elty2}, A1) == t1(convert(Matrix{elty2}, A1.data))
                elseif elty2 <: Real && !(elty1 <: Integer)
                    @test_throws InexactError convert(AbstractMatrix{elty2}, A1) == t1(convert(Matrix{elty2}, A1.data))
                end

                # Binary operations
                @test full(A1 + A2) == full(A1) + full(A2)
                @test full(A1 - A2) == full(A1) - full(A2)

                # Triangular-Triangular multiplication and division
                elty1 != BigFloat && elty2 != BigFloat && @test_approx_eq full(A1*A2) full(A1)*full(A2)
                @test_approx_eq full(A1'A2) full(A1)'full(A2)
                @test_approx_eq full(A1*A2') full(A1)*full(A2)'
                @test_approx_eq full(A1'A2') full(A1)'full(A2)'
                @test_approx_eq full(A1/A2) full(A1)/full(A2)

            end
        end

        for eltyB in (Float32, Float64, Complex64, Complex128)
            B = convert(Matrix{eltyB}, elty1 <: Complex ? real(A1)*ones(n, n) : A1*ones(n, n))

            debug && println("elty1: $elty1, A1: $t1, B: $eltyB")

            # Triangular-dense Matrix/vector multiplication
            @test_approx_eq A1*B[:,1] full(A1)*B[:,1]
            @test_approx_eq A1*B full(A1)*B
            @test_approx_eq A1'B[:,1] full(A1)'B[:,1]
            @test_approx_eq A1'B full(A1)'B
            @test_approx_eq A1*B' full(A1)*B'
            @test_approx_eq B*A1 B*full(A1)
            @test_approx_eq B[:,1]'A1 B[:,1]'full(A1)
            @test_approx_eq B'A1 B'full(A1)
            @test_approx_eq B*A1' B*full(A1)'
            @test_approx_eq B[:,1]'A1' B[:,1]'full(A1)'
            @test_approx_eq B'A1' B'full(A1)'

            # ... and division
            @test_approx_eq A1\B[:,1] full(A1)\B[:,1]
            @test_approx_eq A1\B full(A1)\B
            @test_approx_eq A1'\B[:,1] full(A1)'\B[:,1]
            @test_approx_eq A1'\B full(A1)'\B
            @test_approx_eq A1\B' full(A1)\B'
            @test_approx_eq A1'\B' full(A1)'\B'
            @test_approx_eq B/A1 B/full(A1)
            @test_approx_eq B/A1' B/full(A1)'
            @test_approx_eq B'/A1 B'/full(A1)
            @test_approx_eq B'/A1' B'/full(A1)'

            # Error bounds
            elty1 != BigFloat && errorbounds(A1, A1\B, B)
        end
    end
end

Areal   = randn(n, n)/2
Aimg    = randn(n, n)/2
A2real  = randn(n, n)/2
A2img   = randn(n, n)/2

for eltya in (Float32, Float64, Complex64, Complex128, BigFloat, Int)
    A = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex(Areal, Aimg) : Areal)
    # a2 = eltya == Int ? rand(1:7, n, n) : convert(Matrix{eltya}, eltya <: Complex ? complex(a2real, a2img) : a2real)
    εa = eps(abs(float(one(eltya))))

    for eltyb in (Float32, Float64, Complex64, Complex128)
        εb = eps(abs(float(one(eltyb))))
        ε = max(εa,εb)

        debug && println("\ntype of A: ", eltya, " type of b: ", eltyb, "\n")

        debug && println("Solve upper triangular system")
        Atri = UpperTriangular(lufact(A)[:U]) |> t -> eltya <: Complex && eltyb <: Real ? real(t) : t # Here the triangular matrix can't be too badly conditioned
        b = convert(Matrix{eltyb}, eltya <: Complex ? full(Atri)*ones(n, 2) : full(Atri)*ones(n, 2))
        x = full(Atri) \ b

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
            bigA = big(Atri)
            x̂ = ones(n, 2)
            for i = 1:size(b, 2)
                @test norm(x̂[:,i] - x[:,i], Inf)/norm(x̂[:,i], Inf) <= condskeel(bigA, x̂[:,i])*γ/(1 - condskeel(bigA)*γ)
            end
        end

        debug && println("Test backward error [JIN 5705]")
        for i = 1:size(b, 2)
            @test norm(abs(b[:,i] - Atri*x[:,i]), Inf) <= γ * norm(Atri, Inf) * norm(x[:,i], Inf)
        end

        debug && println("Solve lower triangular system")
        Atri = UpperTriangular(lufact(A)[:U]) |> t -> eltya <: Complex && eltyb <: Real ? real(t) : t # Here the triangular matrix can't be too badly conditioned
        b = convert(Matrix{eltyb}, eltya <: Complex ? full(Atri)*ones(n, 2) : full(Atri)*ones(n, 2))
        x = full(Atri)\b

        debug && println("Test error estimates")
        if eltya != BigFloat && eltyb != BigFloat
            for i = 1:2
                @test  norm(x[:,1] .- 1) <= errorbounds(UpperTriangular(A), x, b)[1][i]
            end
        end

        debug && println("Test forward error [JIN 5705] if this is not a BigFloat")
        b = eltyb == Int ? trunc(Int,Atri*ones(n, 2)) : convert(Matrix{eltyb}, Atri*ones(eltya, n, 2))
        x = Atri \ b
        γ = n*ε/(1 - n*ε)
        if eltya != BigFloat
            bigA = big(Atri)
            x̂ = ones(n, 2)
            for i = 1:size(b, 2)
                @test norm(x̂[:,i] - x[:,i], Inf)/norm(x̂[:,i], Inf) <= condskeel(bigA, x̂[:,i])*γ/(1 - condskeel(bigA)*γ)
            end
        end

        debug && println("Test backward error [JIN 5705]")
        for i = 1:size(b, 2)
            @test norm(abs(b[:,i] - Atri*x[:,i]), Inf) <= γ * norm(Atri, Inf) * norm(x[:,i], Inf)
        end
    end
end

# Issue 10742 and similar
@test istril(UpperTriangular(diagm([1,2,3,4])))
@test istriu(LowerTriangular(diagm([1,2,3,4])))
@test isdiag(UpperTriangular(diagm([1,2,3,4])))
@test isdiag(LowerTriangular(diagm([1,2,3,4])))
@test !isdiag(UpperTriangular(rand(4, 4)))
@test !isdiag(LowerTriangular(rand(4, 4)))

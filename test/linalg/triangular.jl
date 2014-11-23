debug = false
using Base.Test
using Base.LinAlg: BlasFloat, errorbounds, full!, naivesub!, transpose!

debug && println("Triangular matrices")

n = 9
srand(123)

debug && println("Test basic type functionality")
@test_throws DimensionMismatch Triangular(randn(5, 4), :L)
@test Triangular(randn(3, 3), :L) |> t -> [size(t, i) for i = 1:3] == [size(full(t), i) for i = 1:3]

# The following test block tries to call all methods in base/linalg/triangular.jl in order for a combination of input element types. Keep the ordering when adding code.
for elty1 in (Float32, Float64, Complex64, Complex128, BigFloat, Int)
    for elty2 in (Float32, Float64, Complex64, Complex128, BigFloat, Int)
        for uplo1 in (:U, :L)
            for uplo2 in (:U, :L)
                for isunit1 in (true, false)
                    for isunit2 in (true, false)
                        A1 = Triangular(elty1 == Int ? rand(1:7, n, n) : convert(Matrix{elty1}, (elty1 <: Complex ? complex(randn(n, n), randn(n, n)) : randn(n, n)) |> t-> chol(t't, uplo1)), uplo1, isunit1)
                        A2 = Triangular(elty2 == Int ? rand(1:7, n, n) : convert(Matrix{elty2}, (elty2 <: Complex ? complex(randn(n, n), randn(n, n)) : randn(n, n)) |> t-> chol(t't, uplo2)), uplo2, isunit2)

                        for eltyB in (Float32, Float64, Complex64, Complex128)
                            B = convert(Matrix{eltyB}, elty1 <: Complex ? real(A1)*ones(n, n) : A1*ones(n, n))

                            debug && println("A1: $elty1, $uplo1, $isunit1.A2: $elty2, $uplo2, $isunit2. B: $eltyB")

                            # Convert
                            @test convert(Triangular{elty1}, A1) == A1
                            if elty1 <: Real && !(elty2 <: Integer)
                                @test convert(Triangular{elty2}, A1) == Triangular(convert(Matrix{elty2}, A1.data), uplo1, isunit1)
                            elseif elty2 <: Real && !(elty1 <: Integer)
                                @test_throws InexactError convert(Triangular{elty2}, A1) == Triangular(convert(Matrix{elty2}, A1.data), uplo1, isunit1)
                            end
                            @test convert(Matrix, A1) == full(A1)

                            # full!
                            @test full!(copy(A1)) == full(A1)

                            # fill!
                            @test full!(fill!(copy(A1), 1)) == full(Triangular(ones(size(A1)...), uplo1, isunit1))

                            # similar
                            @test isa(similar(A1), Triangular{elty1, Matrix{elty1}, uplo1, isunit1})

                            # Linear indexing
                            for i = 1:length(A1)
                                @test A1[i] == full(A1)[i]
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
                            @test full(A1 + A2) == full(A1) + full(A2)
                            @test full(A1 - A2) == full(A1) - full(A2)
                            @test A1*0.5 == full(A1*0.5)
                            @test 0.5*A1 == full(0.5*A1)
                            @test A1/0.5 == full(A1/0.5)
                            @test 0.5\A1 == full(0.5\A1)

                            # Triangular-Triangular multiplication and division
                            elty1 != BigFloat && elty2 != BigFloat && @test_approx_eq full(A1*A2) full(A1)*full(A2)
                            @test_approx_eq full(A1'A2) full(A1)'full(A2)
                            @test_approx_eq full(A1*A2') full(A1)*full(A2)'
                            @test_approx_eq full(A1'A2') full(A1)'full(A2)'
                            @test_approx_eq full(A1/A2) full(A1)/full(A2)

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

                            # inversion
                            @test_approx_eq inv(A1) inv(lufact(full(A1)))

                            # Error bounds
                            elty1 != BigFloat && errorbounds(A1, A1\B, B)

                            # Determinant
                            @test_approx_eq det(A1) det(lufact(full(A1)))

                            # Matri square root
                            @test_approx_eq sqrtm(A1) |> t->t*t A1

                            # eigenproblems
                            if elty1 != BigFloat # Not handled yet
                                vals, vecs = eig(A1)
                                if !isunit1 && elty1 != Int # Cannot really handle degenerate eigen space and Int matrices will probably have repeated eigenvalues.
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
                        end
                    end
                end
            end
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
        Atri = Triangular(lufact(A)[:U], :U) |> t -> eltya <: Complex && eltyb <: Real ? real(t) : t # Here the triangular matrix can't be too badly conditioned
        b = convert(Matrix{eltyb}, eltya <: Complex ? full(Atri)*ones(n, 2) : full(Atri)*ones(n, 2))
        x = full(Atri) \ b

        debug && println("Test error estimates")
        if eltya != BigFloat && eltyb != BigFloat
            for i = 1:2
                @test  norm(x[:,1] .- 1) <= errorbounds(Triangular(A, :U), x, b)[1][i]
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
        Atri = Triangular(lufact(A)[:U], :U) |> t -> eltya <: Complex && eltyb <: Real ? real(t) : t # Here the triangular matrix can't be too badly conditioned
        b = convert(Matrix{eltyb}, eltya <: Complex ? full(Atri)*ones(n, 2) : full(Atri)*ones(n, 2))
        x = full(Atri)\b

        debug && println("Test error estimates")
        if eltya != BigFloat && eltyb != BigFloat
            for i = 1:2
                @test  norm(x[:,1] .- 1) <= errorbounds(Triangular(A, :U), x, b)[1][i]
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

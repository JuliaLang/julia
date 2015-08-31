# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test

let
debug = false

# Test givens rotations
for elty in (Float32, Float64, Complex64, Complex128)

    debug && println("elty is $elty")

    if elty <: Real
        A = convert(Matrix{elty}, randn(10,10))
    else
        A = convert(Matrix{elty}, complex(randn(10,10),randn(10,10)))
    end
    Ac = copy(A)
    R = Base.LinAlg.Rotation(Base.LinAlg.Givens{elty}[])
    for j = 1:8
        for i = j+2:10
            G, _ = givens(A, j+1, i, j)
            A_mul_B!(G, A)
            A_mul_Bc!(A, G)
            A_mul_B!(G, R)

            # test transposes
            @test_approx_eq ctranspose(G)*G*eye(10) eye(elty, 10)
            @test_approx_eq ctranspose(R)*(R*eye(10)) eye(elty, 10)
            @test_throws ErrorException transpose(G)
            @test_throws ErrorException transpose(R)
        end
    end
    @test_throws ArgumentError givens(A, 3, 2, 2)
    @test_throws ArgumentError givens(one(elty),zero(elty),3,2)
    G, _ = givens(one(elty),zero(elty),11,12)
    @test_throws DimensionMismatch A_mul_B!(G, A)
    @test_throws DimensionMismatch A_mul_Bc!(A,G)
    @test_approx_eq abs(A) abs(hessfact(Ac)[:H])
    @test_approx_eq norm(R*eye(elty, 10)) one(elty)

    G, _ = givens(one(elty),zero(elty),9,10)
    @test_approx_eq ctranspose(G*eye(elty,10))*(G*eye(elty,10)) eye(elty, 10)
    K, _ = givens(zero(elty),one(elty),9,10)
    @test_approx_eq ctranspose(K*eye(elty,10))*(K*eye(elty,10)) eye(elty, 10)

    # test that Givens * work for vectors
    x = A[:,1]
    G, r = givens(x[2], x[4], 2, 4)
    @test (G*x)[2] ≈ r
    @test abs((G*x)[4]) < eps(real(elty))

end
end #let

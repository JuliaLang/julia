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
    @test_approx_eq abs(A) abs(hessfact(Ac)[:H])
    @test_approx_eq norm(R*eye(elty, 10)) one(elty)

end
end #let
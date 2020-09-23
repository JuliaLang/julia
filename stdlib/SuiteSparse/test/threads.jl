using Test, LinearAlgebra, SparseArrays

@testset "threaded SuiteSparse tests" begin
    A = sprandn(200, 200, 0.2)
    b = rand(200)

    function test(n::Integer)
        _A = A[1:n, 1:n]
        _b = b[1:n]
        x = qr(_A) \ _b
        return norm(x)
    end

    res_threads = zeros(100)
    Threads.@threads for i in 1:100
        res_threads[i] = test(i + 100)
    end

    @test res_threads ≈ [test(i + 100) for i in 1:100]
end

# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "binomial" begin
    @test binomial(5,-1) == 0
    @test binomial(5,10) == 0
    @test binomial(5,3) == 10
    @test binomial(2,1) == 2
    @test binomial(1,2) == 0
    @test binomial(-2,1) == -2 # let's agree
    @test binomial(2,-1) == 0

    #Issue 6154
    @test binomial(Int32(34), Int32(15)) == binomial(BigInt(34), BigInt(15)) == 1855967520
    @test binomial(Int64(67), Int64(29)) == binomial(BigInt(67), BigInt(29)) == 7886597962249166160
    @test binomial(Int128(131), Int128(62)) == binomial(BigInt(131), BigInt(62)) == 157311720980559117816198361912717812000
    @test_throws InexactError binomial(Int64(67), Int64(30))
end

@testset "permutations" begin
    p = shuffle([1:1000;])
    @test isperm(p)
    @test all(invperm(invperm(p)) .== p)
    @test isperm(()) == true
    @test isperm((1,)) == true
    @test isperm((2,)) == false
    @test isperm((1,2)) == true
    @test isperm((2,1)) == true
    @test isperm((2,2)) == false
    @test isperm((1,3)) == false
    @test invperm(()) == ()
    @test invperm((1,)) == (1,)
    @test invperm((1,2)) == (1,2)
    @test invperm((2,1)) == (2,1)
    @test_throws ArgumentError invperm((1,3))

    push!(p, 1)
    @test !isperm(p)

    a = randcycle(10)
    @test ipermute!(permute!([1:10;], a),a) == [1:10;]

    # PR 12785
    let a = 2:-1:1
        @test ipermute!(permute!([1, 2], a), a) == [1, 2]
    end
end

@testset "factorial" begin
    @test factorial(7) == 5040
    @test factorial(Int8(7)) == 5040
    @test factorial(UInt8(7)) == 5040
    @test factorial(Int16(7)) == 5040
    @test factorial(UInt16(7)) == 5040
    @test factorial(Int32(7)) == 5040
    @test factorial(UInt32(7)) == 5040
    @test factorial(Int64(7)) == 5040
    @test factorial(UInt64(7)) == 5040
    @test factorial(Int128(7)) == 5040
    @test factorial(UInt128(7)) == 5040
    @test factorial(0) == 1
    @test_throws DomainError factorial(-1)
    @test factorial(Int64(20)) == 2432902008176640000
    # issue #6579
    @test_throws OverflowError factorial(Int64(21))
    @test typeof(factorial(Int8(2))) == typeof(factorial(Int8(1)))
    if Int === Int32
        @test factorial(Int32(12)) === Int32(479001600)
        @test_throws OverflowError factorial(Int32(13))
    end
end

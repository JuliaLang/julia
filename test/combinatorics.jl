# This file is a part of Julia. License is MIT: https://julialang.org/license

using Random: randcycle

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
    @test_throws OverflowError binomial(Int64(67), Int64(30))
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
    @test invpermute!(permute!([1:10;], a),a) == [1:10;]

    # PR 12785
    let ai = 2:-1:1
        @test invpermute!(permute!([1, 2], ai), ai) == [1, 2]
    end

    # PR 35234
    for N in 3:1:20
        A=randcycle(N)
        T=Tuple(A)
        K=Tuple(A.-1)
        @test A[collect(invperm(T))] == 1:N
        @test_throws ArgumentError invperm(K)
        @test isperm(T) == true
        @test isperm(K) == false
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

    _fact_table64 =
        Int64[1,2,6,24,120,720,5040,40320,362880,3628800,39916800,479001600,6227020800,
              87178291200,1307674368000,20922789888000,355687428096000,6402373705728000,
              121645100408832000,2432902008176640000]

    _fact_table128 =
        UInt128[0x00000000000000000000000000000001, 0x00000000000000000000000000000002,
                0x00000000000000000000000000000006, 0x00000000000000000000000000000018,
                0x00000000000000000000000000000078, 0x000000000000000000000000000002d0,
                0x000000000000000000000000000013b0, 0x00000000000000000000000000009d80,
                0x00000000000000000000000000058980, 0x00000000000000000000000000375f00,
                0x00000000000000000000000002611500, 0x0000000000000000000000001c8cfc00,
                0x0000000000000000000000017328cc00, 0x0000000000000000000000144c3b2800,
                0x00000000000000000000013077775800, 0x00000000000000000000130777758000,
                0x00000000000000000001437eeecd8000, 0x00000000000000000016beecca730000,
                0x000000000000000001b02b9306890000, 0x000000000000000021c3677c82b40000,
                0x0000000000000002c5077d36b8c40000, 0x000000000000003ceea4c2b3e0d80000,
                0x000000000000057970cd7e2933680000, 0x00000000000083629343d3dcd1c00000,
                0x00000000000cd4a0619fb0907bc00000, 0x00000000014d9849ea37eeac91800000,
                0x00000000232f0fcbb3e62c3358800000, 0x00000003d925ba47ad2cd59dae000000,
                0x0000006f99461a1e9e1432dcb6000000, 0x00000d13f6370f96865df5dd54000000,
                0x0001956ad0aae33a4560c5cd2c000000, 0x0032ad5a155c6748ac18b9a580000000,
                0x0688589cc0e9505e2f2fee5580000000, 0xde1bc4d19efcac82445da75b00000000]

    for expected in Any[_fact_table64, _fact_table128]
        for (n, factn) in enumerate(expected)
            @test factorial(oftype(factn, n)) === factn
        end
    end
end

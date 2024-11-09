# This file is a part of Julia. License is MIT: https://julialang.org/license

using
    Test,
    Base._TypeDomainNumbers.PositiveIntegers,
    Base._TypeDomainNumbers.IntegersGreaterThanOne,
    Base._TypeDomainNumbers.Constants,
    Base._TypeDomainNumberTupleUtils

@testset "type domain numbers" begin
    @test n0 isa NonnegativeInteger
    @test n1 isa NonnegativeInteger
    @test n1 isa PositiveInteger
    @testset "succ" begin
        for x ∈ (n0, n1)
            @test x === natural_predecessor(@inferred natural_successor(x))
            @test x === natural_predecessor_predecessor(natural_successor(natural_successor(x)))
        end
    end
    @testset "type safety" begin
        @test_throws TypeError PositiveInteger{Int}
    end
    @testset "tuple utils" begin
        @test n0 === @inferred tuple_type_domain_length(())
        @test n1 === @inferred tuple_type_domain_length((7,))
        @test ((), ()) === @inferred split_tuple((), n0)
        @test ((), (7,)) === @inferred split_tuple((7,), n0)
        @test ((7,), ()) === @inferred split_tuple((7,), n1)
        @test ((), (3, 7)) === @inferred split_tuple((3, 7), n0)
        @test ((3,), (7,)) === @inferred split_tuple((3, 7), n1)
        @test ((), (3, 7, 9)) === @inferred split_tuple((3, 7, 9), n0)
        @test ((3,), (7, 9)) === @inferred split_tuple((3, 7, 9), n1)
    end
end

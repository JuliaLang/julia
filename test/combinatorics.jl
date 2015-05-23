# This file is a part of Julia. License is MIT: http://julialang.org/license

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

p = shuffle([1:1000;])
@test isperm(p)
@test all(invperm(invperm(p)) .== p)

push!(p, 1)
@test !isperm(p)

a = randcycle(10)
@test ipermute!(permute!([1:10;], a),a) == [1:10;]

@test collect(combinations("abc",3)) == Any[['a','b','c']]
@test collect(combinations("abc",2)) == Any[['a','b'],['a','c'],['b','c']]
@test collect(combinations("abc",1)) == Any[['a'],['b'],['c']]
@test collect(combinations("abc",0)) == Any[Char[]]
@test collect(combinations("abc",-1)) == Any[]
@test collect(permutations("abc")) == Any[['a','b','c'],['a','c','b'],['b','a','c'],
                                          ['b','c','a'],['c','a','b'],['c','b','a']]

@test collect(filter(x->(iseven(x[1])),permutations([1,2,3]))) == Any[[2,1,3],[2,3,1]]
@test collect(filter(x->(iseven(x[3])),permutations([1,2,3]))) == Any[[1,3,2],[3,1,2]]
@test collect(filter(x->(iseven(x[1])),combinations([1,2,3],2))) == Any[[2,3]]

@test collect(partitions(4)) ==  Any[[4], [3,1], [2,2], [2,1,1], [1,1,1,1]]
@test collect(partitions(8,3)) == Any[[6,1,1], [5,2,1], [4,3,1], [4,2,2], [3,3,2]]
@test collect(partitions(8, 1)) == Any[[8]]
@test collect(partitions(8, 9)) == []
@test collect(partitions([1,2,3])) == Any[Any[[1,2,3]], Any[[1,2],[3]], Any[[1,3],[2]], Any[[1],[2,3]], Any[[1],[2],[3]]]
@test collect(partitions([1,2,3,4],3)) == Any[Any[[1,2],[3],[4]], Any[[1,3],[2],[4]], Any[[1],[2,3],[4]],
                                              Any[[1,4],[2],[3]], Any[[1],[2,4],[3]], Any[[1],[2],[3,4]]]
@test collect(partitions([1,2,3,4],1)) == Any[Any[[1, 2, 3, 4]]]
@test collect(partitions([1,2,3,4],5)) == []

@test length(partitions(0)) == 1
@test length(partitions(-1)) == 0
@test length(collect(partitions(30))) == length(partitions(30))
@test length(collect(partitions(90,4))) == length(partitions(90,4))
@test length(collect(partitions('a':'h'))) == length(partitions('a':'h'))
@test length(collect(partitions('a':'h',5))) == length(partitions('a':'h',5))

for n = 0:7, k = 1:factorial(n)
    p = nthperm!([1:n;], k)
    @test isperm(p)
    @test nthperm(p) == k
end

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
@test factorial(7,3) == 7*6*5*4
@test_throws DomainError factorial(3,7)
@test_throws DomainError factorial(-3,-7)
@test_throws DomainError factorial(-7,-3)
@test factorial(0) == 1
@test_throws DomainError factorial(-1)
#Issue 9943
@test factorial(big(100), (80)) == 1303995018204712451095685346159820800000
#Issue 9950
@test_throws OverflowError factorial(1000,80)
@test factorial(Int64(20)) == 2432902008176640000
# issue #6579
@test_throws OverflowError factorial(Int64(21))
@test typeof(factorial(Int8(2))) == typeof(factorial(Int8(1)))
if Int === Int32
    @test factorial(Int32(12)) === Int32(479001600)
    @test_throws OverflowError factorial(Int32(13))
end

@test_throws ArgumentError parity([0])
@test_throws ArgumentError parity([1,2,3,3])
@test levicivita([1,1,2,3]) == 0
@test levicivita([1]) == 1 && parity([1]) == 0
@test map(levicivita, collect(permutations([1,2,3]))) == [1, -1, -1, 1, 1, -1]
@test let p = [3, 4, 6, 10, 5, 2, 1, 7, 8, 9]; levicivita(p) == 1 && parity(p) == 0; end
@test let p = [4, 3, 6, 10, 5, 2, 1, 7, 8, 9]; levicivita(p) == -1 && parity(p) == 1; end

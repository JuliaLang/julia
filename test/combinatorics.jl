@test factorial(7) == 5040
@test factorial(7,3) == 7*6*5*4
@test binomial(5,3) == 10
p = shuffle([1:1000])
@test isperm(p)
@test all(invperm(invperm(p)) .== p)
push!(p, 1)
@test !isperm(p)
a = randcycle(10)
@test ipermute!(permute!([1:10], a),a) == [1:10]
@test collect(combinations("abc",2)) == ["ab","ac","bc"]
@test collect(permutations("abc")) == ["abc","acb","bac","bca","cab","cba"]
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

@test length(collect(partitions(30))) == length(partitions(30))
@test length(collect(partitions(90,4))) == length(partitions(90,4))
@test length(collect(partitions('a':'h'))) == length(partitions('a':'h'))
@test length(collect(partitions('a':'h',5))) == length(partitions('a':'h',5))

for n = 0:7, k = 1:factorial(n)
    p = nthperm!([1:n], k)
    @test isperm(p)
    @test nthperm(p) == k
end

#Issue 6154
@test binomial(int32(34), int32(15)) == binomial(BigInt(34), BigInt(15)) == 1855967520
@test binomial(int64(67), int64(29)) == binomial(BigInt(67), BigInt(29)) == 7886597962249166160
@test binomial(int128(131), int128(62)) == binomial(BigInt(131), BigInt(62)) == 157311720980559117816198361912717812000

# issue #6579
@test factorial(0) == 1
@test factorial(int64(20)) == 2432902008176640000
@test_throws OverflowError factorial(int64(21))
@test_throws DomainError factorial(-1)
@test typeof(factorial(int8(2))) == typeof(factorial(int8(1)))
if Int === Int32
@test factorial(int32(12)) === int32(479001600)
@test_throws OverflowError factorial(int32(13))
end

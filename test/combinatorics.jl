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
@test collect(filter(x->(iseven(x[1])),permutations([1,2,3]))) == {[2,1,3],[2,3,1]}
@test collect(filter(x->(iseven(x[3])),permutations([1,2,3]))) == {[1,3,2],[3,1,2]}
@test collect(filter(x->(iseven(x[1])),combinations([1,2,3],2))) == {[2,3]}
@test collect(partitions(4)) ==  {[4], [3,1], [2,2], [2,1,1], [1,1,1,1]}
@test collect(partitions(8,3)) == {[6,1,1], [5,2,1], [4,3,1], [4,2,2], [3,3,2]}
@test collect(partitions([1,2,3])) == {{[1,2,3]}, {[1,2],[3]}, {[1,3],[2]}, {[1],[2,3]}, {[1],[2],[3]}}
@test collect(partitions([1,2,3,4],3)) == {{[1,2],[3],[4]}, {[1,3],[2],[4]}, {[1],[2,3],[4]}, {[1,4],[2],[3]}, {[1],[2,4],[3]},{[1],[2],[3,4]}}

@test length(collect(partitions(30))) == length(partitions(30))
@test length(collect(partitions(90,4))) == length(partitions(90,4))
@test length(collect(partitions('a':'h'))) == length(partitions('a':'h'))
@test length(collect(partitions('a':'h',5))) == length(partitions('a':'h',5))

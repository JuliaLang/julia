@test factorial(7) == 5040
@test factorial(7,3) == 7*6*5*4
@test binomial(5,3) == 10
@test isperm(shuffle([1:1000]))
a = randcycle(10)
@test ipermute!(permute!([1:10], a),a) == [1:10]
@test collect(combinations("abc",2)) == ["ab","ac","bc"]
@test collect(permutations("abc")) == ["abc","acb","bac","bca","cab","cba"]
@test collect(partitions(4)) ==  {[4], [3,1], [2,2], [2,1,1], [1,1,1,1]}
@test collect(partitions(8,3)) == {[6,1,1], [5,2,1], [4,3,1], [4,2,2], [3,3,2]}
@test collect(partitions([1,2,3])) == {{[1,2,3]}, {[1,2],[3]}, {[1,3],[2]}, {[1],[2,3]}, {[1],[2],[3]}}

@test length(collect(partitions(30))) == length(partitions(30))
@test length(collect(partitions(90,4))) == length(partitions(90,4))
@test length(collect(partitions('a':'h'))) == length(partitions('a':'h'))

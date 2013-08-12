
@test factorial(7) = 5040
@test factorial(7,3) == 7*6*5*4
@test binomial(5,3) == 10
@test isperm(shuffle([1:1000]))
a = randcycle(10)
@test ipermute!(permute!([1:10], a),a) == [1:10]
@test collect(combinations("abc",2)) == ["ab","ac","bc"]
@test collect(collect(permutations("abc"))) == ["abc","acb","bac","bca","cab","cba"]

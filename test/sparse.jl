# check matrix operations
se33 = speye(3)
do33 = ones(3)
@test isequal(se33 * se33, se33)

# check horiz concatenation
@test all([se33 se33] == sparse([1, 2, 3, 1, 2, 3], [1, 2, 3, 4, 5, 6], ones(6)))

# check vert concatenation
@test all([se33; se33] == sparse([1, 4, 2, 5, 3, 6], [1, 1, 2, 2, 3, 3], ones(6)))

# check h+v concatenation
se44 = speye(4)
sz42 = spzeros(4, 2)
sz41 = spzeros(4, 1)
sz34 = spzeros(3, 4)
se77 = speye(7)
@test all([se44 sz42 sz41; sz34 se33] == se77)

# check concatenation promotion
sz41_f32 = spzeros(Float32, 4, 1)
se33_i32 = speye(Int32, 3, 3)
@test all([se44 sz42 sz41_f32; sz34 se33_i32] == se77)

# check mixed sparse-dense concatenation
sz33 = spzeros(3)
de33 = eye(3)
@test  all([se33 de33; sz33 se33] == dense([se33 se33; sz33 se33 ]))

# check splicing + concatenation on
# random instances, with nested vcat
# also side-checks sparse ref
for i = 1 : 10
    a = sprand(5, 4, 0.5)
    @test all([a[1:2,1:2] a[1:2,3:4]; a[3:5,1] [a[3:4,2:4]; a[5,2:4]]] == a)
end

# sparse ref
a116 = reshape(1:16, 4, 4)
s116 = sparse(a116)
p = [4, 1, 2, 3, 2]
@test dense(s116[p,:]) == a116[p,:]
@test dense(s116[:,p]) == a116[:,p]
@test dense(s116[p,p]) == a116[p,p]

# sparse assign
p = [4, 1, 3]
a116[p, p] = -1
s116[p, p] = -1
@test a116 == s116

p = [2, 1, 4]
a116[p, p] = reshape(1:9, 3, 3)
s116[p, p] = reshape(1:9, 3, 3)
@test a116 == s116

# check matrix multiplication
for i = 1:5
    a = sprand(10, 5, 0.5)
    b = sprand(5, 10, 0.1)
    @test_approx_eq max(abs(a*b - dense(a)*dense(b))) 0.0
end

# reductions
@test sum(se33)[1] == 3.0
@test sum(se33, 1) == [1.0 1.0 1.0]
@test sum(se33, 2) == [1.0 1.0 1.0]'
@test prod(se33)[1] == 0.0
@test prod(se33, 1) == [0.0 0.0 0.0]
@test prod(se33, 2) == [0.0 0.0 0.0]'

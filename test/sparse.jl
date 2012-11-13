cd("../extras") do
require("suitesparse")

# check matrix operations
se33 = speye(3)
@assert isequal(se33 * se33, se33)

# check mixed sparse-dense matrix operations
do33 = ones(3)
@assert isequal(se33 \ do33, do33)

# check horiz concatenation
@assert all([se33 se33] == sparse([1, 2, 3, 1, 2, 3], [1, 2, 3, 4, 5, 6], ones(6)))

# check vert concatenation
@assert all([se33; se33] == sparse([1, 4, 2, 5, 3, 6], [1, 1, 2, 2, 3, 3], ones(6)))

# check h+v concatenation
se44 = speye(4)
sz42 = spzeros(4, 2)
sz41 = spzeros(4, 1)
sz34 = spzeros(3, 4)
se77 = speye(7)
@assert all([se44 sz42 sz41; sz34 se33] == se77)

# check concatenation promotion
sz41_f32 = spzeros(Float32, 4, 1)
se33_i32 = speye(Int32, 3, 3)
@assert all([se44 sz42 sz41_f32; sz34 se33_i32] == se77)

# check mixed sparse-dense concatenation
sz33 = spzeros(3)
de33 = eye(3)
@assert  all([se33 de33; sz33 se33] == full([se33 se33; sz33 se33 ]))

# check splicing + concatenation on
# random instances, with nested vcat
# (also side-checks sparse ref, which uses
# sparse multiplication)
for i = 1 : 10
    a = sprand(5, 4, 0.5)
    @assert all([a[1:2,1:2] a[1:2,3:4]; a[3:5,1] [a[3:4,2:4]; a[5,2:4]]] == a)
end

# reductions
@assert sum(se33)[1] == 3.0
@assert sum(se33, 1) == [1.0 1.0 1.0]
@assert sum(se33, 2) == [1.0 1.0 1.0]'
@assert prod(se33)[1] == 0.0
@assert prod(se33, 1) == [0.0 0.0 0.0]
@assert prod(se33, 2) == [0.0 0.0 0.0]'

end # cd

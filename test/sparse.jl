# check sparse matrix construction
@test isequal(full(sparse(complex(ones(5,5),ones(5,5)))), complex(ones(5,5),ones(5,5)))

# check matrix operations
se33 = speye(3)
do33 = ones(3)
@test isequal(se33 * se33, se33)

# check sparse binary op
@test all(full(se33 + convert(SparseMatrixCSC{Float32,Int32}, se33)) == 2*eye(3))
@test all(full(se33 * convert(SparseMatrixCSC{Float32,Int32}, se33)) == eye(3))

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

# check blkdiag concatenation
@test all(blkdiag(se33, se33) == sparse([1, 2, 3, 4, 5, 6], [1, 2, 3, 4, 5, 6], ones(6)))

# check concatenation promotion
sz41_f32 = spzeros(Float32, 4, 1)
se33_i32 = speye(Int32, 3, 3)
@test all([se44 sz42 sz41_f32; sz34 se33_i32] == se77)

# check mixed sparse-dense concatenation
sz33 = spzeros(3, 3)
de33 = eye(3)
@test  all([se33 de33; sz33 se33] == full([se33 se33; sz33 se33 ]))

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
@test full(s116[p,:]) == a116[p,:]
@test full(s116[:,p]) == a116[:,p]
@test full(s116[p,p]) == a116[p,p]

# sparse assign
p = [4, 1, 3]
a116[p, p] = -1
s116[p, p] = -1
@test a116 == s116

p = [2, 1, 4]
a116[p, p] = reshape(1:9, 3, 3)
s116[p, p] = reshape(1:9, 3, 3)
@test a116 == s116

# matrix-vector multiplication (non-square)
for i = 1:5
    a = sprand(10, 5, 0.5)
    b = rand(5)
    @test maximum(abs(a*b - full(a)*b)) < 100*eps()
end

# complex matrix-vector multiplication and left-division
for i = 1:5
    a = speye(5) + 0.1*sprandn(5, 5, 0.2)
    b = randn(5,3) + im*randn(5,3)
    @test (maximum(abs(a*b - full(a)*b)) < 100*eps())
    @test (maximum(abs(a'b - full(a)'b)) < 100*eps())
    @test (maximum(abs(a\b - full(a)\b)) < 1000*eps())
    @test (maximum(abs(a'\b - full(a')\b)) < 1000*eps())
    @test (maximum(abs(a.'\b - full(a.')\b)) < 1000*eps())
    
    a = speye(5) + 0.1*sprandn(5, 5, 0.2) + 0.1*im*sprandn(5, 5, 0.2)
    b = randn(5,3)
    @test (maximum(abs(a*b - full(a)*b)) < 100*eps())
    @test (maximum(abs(a'b - full(a)'b)) < 100*eps())
    @test (maximum(abs(a\b - full(a)\b)) < 1000*eps())
    @test (maximum(abs(a'\b - full(a')\b)) < 1000*eps())
    @test (maximum(abs(a.'\b - full(a.')\b)) < 1000*eps())

    a = speye(5) + tril(0.1*sprandn(5, 5, 0.2))
    b = randn(5,3) + im*randn(5,3)
    @test (maximum(abs(a*b - full(a)*b)) < 100*eps())
    @test (maximum(abs(a'b - full(a)'b)) < 100*eps())
    @test (maximum(abs(a\b - full(a)\b)) < 1000*eps())
    @test (maximum(abs(a'\b - full(a')\b)) < 1000*eps())
    @test (maximum(abs(a.'\b - full(a.')\b)) < 1000*eps())

    a = speye(5) + tril(0.1*sprandn(5, 5, 0.2) + 0.1*im*sprandn(5, 5, 0.2))
    b = randn(5,3)
    @test (maximum(abs(a*b - full(a)*b)) < 100*eps())
    @test (maximum(abs(a'b - full(a)'b)) < 100*eps())
    @test (maximum(abs(a\b - full(a)\b)) < 1000*eps())
    @test (maximum(abs(a'\b - full(a')\b)) < 1000*eps())
    @test (maximum(abs(a.'\b - full(a.')\b)) < 1000*eps())

    a = speye(5) + triu(0.1*sprandn(5, 5, 0.2))
    b = randn(5,3) + im*randn(5,3)
    @test (maximum(abs(a*b - full(a)*b)) < 100*eps())
    @test (maximum(abs(a'b - full(a)'b)) < 100*eps())
    @test (maximum(abs(a\b - full(a)\b)) < 1000*eps())
    @test (maximum(abs(a'\b - full(a')\b)) < 1000*eps())
    @test (maximum(abs(a.'\b - full(a.')\b)) < 1000*eps())

    a = speye(5) + triu(0.1*sprandn(5, 5, 0.2) + 0.1*im*sprandn(5, 5, 0.2))
    b = randn(5,3)
    @test (maximum(abs(a*b - full(a)*b)) < 100*eps())
    @test (maximum(abs(a'b - full(a)'b)) < 100*eps())
    @test (maximum(abs(a\b - full(a)\b)) < 1000*eps())
    @test (maximum(abs(a'\b - full(a')\b)) < 1000*eps())
    @test (maximum(abs(a.'\b - full(a.')\b)) < 1000*eps())

    a = speye(5) + triu(0.1*sprandn(5, 5, 0.2))
    b = randn(5,3) + im*randn(5,3)
    @test (maximum(abs(a*b - full(a)*b)) < 100*eps())
    @test (maximum(abs(a'b - full(a)'b)) < 100*eps())
    @test (maximum(abs(a\b - full(a)\b)) < 1000*eps())
    @test (maximum(abs(a'\b - full(a')\b)) < 1000*eps())
    @test (maximum(abs(a.'\b - full(a.')\b)) < 1000*eps())

    a = spdiagm(randn(5)) + im*spdiagm(randn(5))
    b = randn(5,3)
    @test (maximum(abs(a*b - full(a)*b)) < 100*eps())
    @test (maximum(abs(a'b - full(a)'b)) < 100*eps())
    @test (maximum(abs(a\b - full(a)\b)) < 1000*eps())
    @test (maximum(abs(a'\b - full(a')\b)) < 1000*eps())
    @test (maximum(abs(a.'\b - full(a.')\b)) < 1000*eps())

    b = randn(5,3) + im*randn(5,3)
    @test (maximum(abs(a*b - full(a)*b)) < 100*eps())
    @test (maximum(abs(a'b - full(a)'b)) < 100*eps())
    @test (maximum(abs(a\b - full(a)\b)) < 1000*eps())
    @test (maximum(abs(a'\b - full(a')\b)) < 1000*eps())
    @test (maximum(abs(a.'\b - full(a.')\b)) < 1000*eps())
end

# matrix multiplication and kron
for i = 1:5
    a = sprand(10, 5, 0.7)
    b = sprand(5, 10, 0.3)
    @test maximum(abs(a*b - full(a)*full(b))) < 100*eps()
    @test full(kron(a,b)) == kron(full(a), full(b))
end

# reductions
@test sum(se33)[1] == 3.0
@test sum(se33, 1) == [1.0 1.0 1.0]
@test sum(se33, 2) == [1.0 1.0 1.0]'
@test prod(se33)[1] == 0.0
@test prod(se33, 1) == [0.0 0.0 0.0]
@test prod(se33, 2) == [0.0 0.0 0.0]'

# spdiagm
@test full(spdiagm((ones(2), ones(2)), (0, -1), 3, 3)) ==  
                       [1.0  0.0  0.0; 1.0  1.0  0.0;  0.0  1.0  0.0]

# elimination tree
## upper triangle of the pattern test matrix from Figure 4.2 of
## "Direct Methods for Sparse Linear Systems" by Tim Davis, SIAM, 2006
rowval = int32([1,2,2,3,4,5,1,4,6,1,7,2,5,8,6,9,3,4,6,8,10,3,5,7,8,10,11])
colval = int32([1,2,3,3,4,5,6,6,6,7,7,8,8,8,9,9,10,10,10,10,10,11,11,11,11,11,11])
A = sparse(rowval, colval, ones(length(rowval)))
P,post = Base.LinAlg.etree(A, true)
@test P == int32([6,3,8,6,8,7,9,10,10,11,0])
@test post == int32([2,3,5,8,1,4,6,7,9,10,11])

# reinterpret issue 4986
sfe22 = speye(Float64, 2)
mfe22 = eye(Float64, 2)
@test reinterpret(Int64, sfe22) == reinterpret(Int64, mfe22)

# Issue 5190
@test_throws sparsevec([3,5,7],[0.1,0.0,3.2],4)

# issue #5169
@test nfilled(sparse([1,1],[1,2],[0.0,-0.0])) == 0

# issue #5386
I,J,V = findnz(SparseMatrixCSC(2,1,[1,3],[1,2],[1.0,0.0]))
@test length(I) == length(J) == length(V) == 1

# issue #5437
@test nfilled(sparse([1,2,3],[1,2,3],[0.0,1.0,2.0])) == 2

# issue 5824
@test sprand(4,5,0.5).^0 == sparse(ones(4,5))

#issue 5985
@test sprandbool(4, 5, 0.0) == sparse(zeros(Bool, 4, 5))
@test sprandbool(4, 5, 1.00) == sparse(ones(Bool, 4, 5))
sprb45 = sprandbool(4, 5, 0.5)
@test length(sprb45) == 20
@test (sum(sprb45)[1] - 10) <= 5

# issue 5853, sparse diff
for i=1:2, a={[1 2 3], [1 2 3]', speye(3)}
    @test all(diff(sparse(a),i) == diff(a,i))
end

# test for "access to undefined error" types that initially allocate elements as #undef
@test all(sparse(1:2, 1:2, Number[1,2])^2 == sparse(1:2, 1:2, [1,4]))
sd1 = diff(sparse([1,1,1], [1,2,3], Number[1,2,3]), 1)

# issue #6036
P = spzeros(Float64, 3, 3)
for i = 1:3
    P[i,i] = i
end
@test minimum(P) == [0]
@test maximum(P) == [3]

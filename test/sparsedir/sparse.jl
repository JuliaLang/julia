# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test

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
se33_32bit = convert(SparseMatrixCSC{Float32,Int32}, se33)
@test all([se33; se33_32bit] == sparse([1, 4, 2, 5, 3, 6], [1, 1, 2, 2, 3, 3], ones(6)))

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

# sparse matrix * BitArray
A = sprand(5,5,0.2)
B = trues(5)
@test_approx_eq A*B full(A)*B
B = trues(5,5)
@test_approx_eq A*B full(A)*B
@test_approx_eq B*A B*full(A)

# complex matrix-vector multiplication and left-division
if Base.USE_GPL_LIBS
for i = 1:5
    a = speye(5) + 0.1*sprandn(5, 5, 0.2)
    b = randn(5,3) + im*randn(5,3)
    c = randn(5) + im*randn(5)
    d = randn(5) + im*randn(5)
    α = rand(Complex128)
    β = rand(Complex128)
    @test (maximum(abs(a*b - full(a)*b)) < 100*eps())
    @test (maximum(abs(A_mul_B!(similar(b), a, b) - full(a)*b)) < 100*eps()) # for compatibility with present matmul API. Should go away eventually.
    @test (maximum(abs(A_mul_B!(similar(c), a, c) - full(a)*c)) < 100*eps()) # for compatibility with present matmul API. Should go away eventually.
    @test (maximum(abs(a'b - full(a)'b)) < 100*eps())
    @test (maximum(abs(a.'b - full(a).'b)) < 100*eps())
    @test (maximum(abs(a\b - full(a)\b)) < 1000*eps())
    @test (maximum(abs(a'\b - full(a')\b)) < 1000*eps())
    @test (maximum(abs(a.'\b - full(a.')\b)) < 1000*eps())
    @test (maximum(abs((a'*c + d) - (full(a)'*c + d))) < 1000*eps())
    @test (maximum(abs((α*a.'*c + β*d) - (α*full(a).'*c + β*d))) < 1000*eps())
    @test (maximum(abs((a.'*c + d) - (full(a).'*c + d))) < 1000*eps())
    c = randn(6) + im*randn(6)
    @test_throws DimensionMismatch α*a.'*c + β*c
    @test_throws DimensionMismatch α*a.'*ones(5) + β*c

    a = speye(5) + 0.1*sprandn(5, 5, 0.2) + 0.1*im*sprandn(5, 5, 0.2)
    b = randn(5,3)
    @test (maximum(abs(a*b - full(a)*b)) < 100*eps())
    @test (maximum(abs(a'b - full(a)'b)) < 100*eps())
    @test (maximum(abs(a.'b - full(a).'b)) < 100*eps())
    @test (maximum(abs(a\b - full(a)\b)) < 1000*eps())
    @test (maximum(abs(a'\b - full(a')\b)) < 1000*eps())
    @test (maximum(abs(a.'\b - full(a.')\b)) < 1000*eps())

    a = speye(5) + tril(0.1*sprandn(5, 5, 0.2))
    b = randn(5,3) + im*randn(5,3)
    @test (maximum(abs(a*b - full(a)*b)) < 100*eps())
    @test (maximum(abs(a'b - full(a)'b)) < 100*eps())
    @test (maximum(abs(a.'b - full(a).'b)) < 100*eps())
    @test (maximum(abs(a\b - full(a)\b)) < 1000*eps())
    @test (maximum(abs(a'\b - full(a')\b)) < 1000*eps())
    @test (maximum(abs(a.'\b - full(a.')\b)) < 1000*eps())
    @test (maximum(abs(A_ldiv_B!(a,copy(b)) - full(a)\b)) < 1000*eps())

    a = speye(5) + tril(0.1*sprandn(5, 5, 0.2) + 0.1*im*sprandn(5, 5, 0.2))
    b = randn(5,3)
    @test (maximum(abs(a*b - full(a)*b)) < 100*eps())
    @test (maximum(abs(a'b - full(a)'b)) < 100*eps())
    @test (maximum(abs(a.'b - full(a).'b)) < 100*eps())
    @test (maximum(abs(a\b - full(a)\b)) < 1000*eps())
    @test (maximum(abs(a'\b - full(a')\b)) < 1000*eps())
    @test (maximum(abs(a.'\b - full(a.')\b)) < 1000*eps())

    a = speye(5) + triu(0.1*sprandn(5, 5, 0.2))
    b = randn(5,3) + im*randn(5,3)
    @test (maximum(abs(a*b - full(a)*b)) < 100*eps())
    @test (maximum(abs(a'b - full(a)'b)) < 100*eps())
    @test (maximum(abs(a.'b - full(a).'b)) < 100*eps())
    @test (maximum(abs(a\b - full(a)\b)) < 1000*eps())
    @test (maximum(abs(a'\b - full(a')\b)) < 1000*eps())
    @test (maximum(abs(a.'\b - full(a.')\b)) < 1000*eps())
    @test (maximum(abs(A_ldiv_B!(a,copy(b)) - full(a)\b)) < 1000*eps())

    a = speye(5) + triu(0.1*sprandn(5, 5, 0.2) + 0.1*im*sprandn(5, 5, 0.2))
    b = randn(5,3)
    @test (maximum(abs(a*b - full(a)*b)) < 100*eps())
    @test (maximum(abs(a'b - full(a)'b)) < 100*eps())
    @test (maximum(abs(a.'b - full(a).'b)) < 100*eps())
    @test (maximum(abs(a\b - full(a)\b)) < 1000*eps())
    @test (maximum(abs(a'\b - full(a')\b)) < 1000*eps())
    @test (maximum(abs(a.'\b - full(a.')\b)) < 1000*eps())

    a = speye(5) + triu(0.1*sprandn(5, 5, 0.2))
    b = randn(5,3) + im*randn(5,3)
    @test (maximum(abs(a*b - full(a)*b)) < 100*eps())
    @test (maximum(abs(a'b - full(a)'b)) < 100*eps())
    @test (maximum(abs(a.'b - full(a).'b)) < 100*eps())
    @test (maximum(abs(a\b - full(a)\b)) < 1000*eps())
    @test (maximum(abs(a'\b - full(a')\b)) < 1000*eps())
    @test (maximum(abs(a.'\b - full(a.')\b)) < 1000*eps())
    @test (maximum(abs(A_ldiv_B!(a,copy(b)) - full(a)\b)) < 1000*eps())

    a = spdiagm(randn(5)) + im*spdiagm(randn(5))
    b = randn(5,3)
    @test (maximum(abs(a*b - full(a)*b)) < 100*eps())
    @test (maximum(abs(a'b - full(a)'b)) < 100*eps())
    @test (maximum(abs(a.'b - full(a).'b)) < 100*eps())
    @test (maximum(abs(a\b - full(a)\b)) < 1000*eps())
    @test (maximum(abs(a'\b - full(a')\b)) < 1000*eps())
    @test (maximum(abs(a.'\b - full(a.')\b)) < 1000*eps())

    b = randn(5,3) + im*randn(5,3)
    @test (maximum(abs(a*b - full(a)*b)) < 100*eps())
    @test (maximum(abs(a'b - full(a)'b)) < 100*eps())
    @test (maximum(abs(a.'b - full(a).'b)) < 100*eps())
    @test (maximum(abs(a\b - full(a)\b)) < 1000*eps())
    @test (maximum(abs(a'\b - full(a')\b)) < 1000*eps())
    @test (maximum(abs(a.'\b - full(a.')\b)) < 1000*eps())
end
end

# matrix multiplication and kron
for i = 1:5
    a = sprand(10, 5, 0.7)
    b = sprand(5, 15, 0.3)
    @test maximum(abs(a*b - full(a)*full(b))) < 100*eps()
    @test maximum(abs(Base.SparseArrays.spmatmul(a,b,sortindices=:sortcols) - full(a)*full(b))) < 100*eps()
    @test maximum(abs(Base.SparseArrays.spmatmul(a,b,sortindices=:doubletranspose) - full(a)*full(b))) < 100*eps()
    @test full(kron(a,b)) == kron(full(a), full(b))
    @test full(kron(full(a),b)) == kron(full(a), full(b))
    @test full(kron(a,full(b))) == kron(full(a), full(b))
    c = sparse(rand(Float32,5,5))
    d = sparse(rand(Float64,5,5))
    @test full(kron(c,d)) == kron(full(c),full(d))
end

# scale and scale!
sA = sprandn(3, 7, 0.5)
sC = similar(sA)
dA = full(sA)
b = randn(7)
@test scale(dA, b) == scale(sA, b)
@test scale(dA, b) == scale!(sC, sA, b)
@test scale(dA, b) == scale!(copy(sA), b)
b = randn(3)
@test scale(b, dA) == scale(b, sA)
@test scale(b, dA) == scale!(sC, b, sA)
@test scale(b, dA) == scale!(b, copy(sA))

@test scale(dA, 0.5) == scale(sA, 0.5)
@test scale(dA, 0.5) == scale!(sC, sA, 0.5)
@test scale(dA, 0.5) == scale!(copy(sA), 0.5)
@test scale(0.5, dA) == scale(0.5, sA)
@test scale(0.5, dA) == scale!(sC, sA, 0.5)
@test scale(0.5, dA) == scale!(0.5, copy(sA))
@test scale!(sC, 0.5, sA) == scale!(sC, sA, 0.5)

# reductions
pA = sparse(rand(3, 7))

for arr in (se33, sA, pA)
    for f in (sum, prod, minimum, maximum, var)
        farr = full(arr)
        @test_approx_eq f(arr) f(farr)
        @test_approx_eq f(arr, 1) f(farr, 1)
        @test_approx_eq f(arr, 2) f(farr, 2)
        @test_approx_eq f(arr, (1, 2)) [f(farr)]
        @test isequal(f(arr, 3), f(farr, 3))
    end
end

for f in (sum, prod, minimum, maximum)
    # Test with a map function that maps to non-zero
    for arr in (se33, sA, pA)
        @test_approx_eq f(x->x+1, arr) f(arr+1)
    end

    # case where f(0) would throw
    @test_approx_eq f(x->sqrt(x-1), pA+1) f(sqrt(pA))
    # these actually throw due to #10533
    # @test_approx_eq f(x->sqrt(x-1), pA+1, 1) f(sqrt(pA), 1)
    # @test_approx_eq f(x->sqrt(x-1), pA+1, 2) f(sqrt(pA), 2)
    # @test_approx_eq f(x->sqrt(x-1), pA+1, 3) f(pA)
end

# empty cases
@test sum(sparse(Int[])) === 0
@test prod(sparse(Int[])) === 1
@test_throws ArgumentError minimum(sparse(Int[]))
@test_throws ArgumentError maximum(sparse(Int[]))
@test var(sparse(Int[])) === NaN

for f in (sum, prod, minimum, maximum, var)
    @test isequal(f(spzeros(0, 1), 1), f(Array(Int, 0, 1), 1))
    @test isequal(f(spzeros(0, 1), 2), f(Array(Int, 0, 1), 2))
    @test isequal(f(spzeros(0, 1), (1, 2)), f(Array(Int, 0, 1), (1, 2)))
    @test isequal(f(spzeros(0, 1), 3), f(Array(Int, 0, 1), 3))
end

# spdiagm
@test full(spdiagm((ones(2), ones(2)), (0, -1), 3, 3)) ==
                       [1.0  0.0  0.0; 1.0  1.0  0.0;  0.0  1.0  0.0]

# elimination tree
## upper triangle of the pattern test matrix from Figure 4.2 of
## "Direct Methods for Sparse Linear Systems" by Tim Davis, SIAM, 2006
rowval = Int32[1,2,2,3,4,5,1,4,6,1,7,2,5,8,6,9,3,4,6,8,10,3,5,7,8,10,11]
colval = Int32[1,2,3,3,4,5,6,6,6,7,7,8,8,8,9,9,10,10,10,10,10,11,11,11,11,11,11]
A = sparse(rowval, colval, ones(length(rowval)))
p = etree(A)
P,post = etree(A, true)
@test P == p
@test P == Int32[6,3,8,6,8,7,9,10,10,11,0]
@test post == Int32[2,3,5,8,1,4,6,7,9,10,11]
@test isperm(post)


# issue #4986, reinterpret
sfe22 = speye(Float64, 2)
mfe22 = eye(Float64, 2)
@test reinterpret(Int64, sfe22) == reinterpret(Int64, mfe22)

# issue #5190
@test_throws ArgumentError sparsevec([3,5,7],[0.1,0.0,3.2],4)

# issue #5169
@test nnz(sparse([1,1],[1,2],[0.0,-0.0])) == 0

# issue #5386
K,J,V = findnz(SparseMatrixCSC(2,1,[1,3],[1,2],[1.0,0.0]))
@test length(K) == length(J) == length(V) == 1

# https://groups.google.com/d/msg/julia-users/Yq4dh8NOWBQ/GU57L90FZ3EJ
A = speye(Bool, 5)
@test find(A) == find(x -> x == true, A) == find(full(A))

# issue #5437
@test nnz(sparse([1,2,3],[1,2,3],[0.0,1.0,2.0])) == 2

# issue #5824
@test sprand(4,5,0.5).^0 == sparse(ones(4,5))

# issue #5985
@test sprandbool(4, 5, 0.0) == sparse(zeros(Bool, 4, 5))
@test sprandbool(4, 5, 1.00) == sparse(ones(Bool, 4, 5))
sprb45nnzs = zeros(5)
for i=1:5
    sprb45 = sprandbool(4, 5, 0.5)
    @test length(sprb45) == 20
    sprb45nnzs[i] = sum(sprb45)[1]
end
@test 4 <= mean(sprb45nnzs) <= 16

# issue #5853, sparse diff
for i=1:2, a=Any[[1 2 3], [1 2 3]', eye(3)]
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

@test minimum(P) === 0.0
@test maximum(P) === 3.0
@test minimum(-P) === -3.0
@test maximum(-P) === 0.0

@test maximum(P, (1,)) == [1.0 2.0 3.0]
@test maximum(P, (2,)) == reshape([1.0,2.0,3.0],3,1)
@test maximum(P, (1,2)) == reshape([3.0],1,1)

@test maximum(sparse(-ones(3,3))) == -1
@test minimum(sparse(ones(3,3))) == 1

# Unary functions
a = sprand(5,15, 0.5)
afull = full(a)
for op in (:sin, :cos, :tan, :ceil, :floor, :abs, :abs2)
    @eval begin
        @test ($op)(afull) == full($(op)(a))
    end
end

for op in (:ceil, :floor)
    @eval begin
        @test ($op)(Int,afull) == full($(op)(Int,a))
    end
end


# getindex tests
ni = 23
nj = 32
a116 = reshape(1:(ni*nj), ni, nj)
s116 = sparse(a116)

ad116 = diagm(diag(a116))
sd116 = sparse(ad116)

for (aa116, ss116) in [(a116, s116), (ad116, sd116)]
    ij=11; i=3; j=2
    @test ss116[ij] == aa116[ij]
    @test ss116[(i,j)] == aa116[i,j]
    @test ss116[i,j] == aa116[i,j]
    @test ss116[i-1,j] == aa116[i-1,j]
    ss116[i,j] = 0
    @test ss116[i,j] == 0
    ss116 = sparse(aa116)

    # range indexing
    @test full(ss116[i,:]) == aa116[i,:]
    @test full(ss116[:,j]) == aa116[:,j]
    @test full(ss116[i,1:2:end]) == aa116[i,1:2:end]
    @test full(ss116[1:2:end,j]) == aa116[1:2:end,j]
    @test full(ss116[i,end:-2:1]) == aa116[i,end:-2:1]
    @test full(ss116[end:-2:1,j]) == aa116[end:-2:1,j]
    # float-range indexing is not supported

    # sorted vector indexing
    @test full(ss116[i,[3:2:end-3;]]) == aa116[i,[3:2:end-3;]]
    @test full(ss116[[3:2:end-3;],j]) == aa116[[3:2:end-3;],j]
    @test full(ss116[i,[end-3:-2:1;]]) == aa116[i,[end-3:-2:1;]]
    @test full(ss116[[end-3:-2:1;],j]) == aa116[[end-3:-2:1;],j]

    # unsorted vector indexing with repetition
    p = [4, 1, 2, 3, 2, 6]
    @test full(ss116[p,:]) == aa116[p,:]
    @test full(ss116[:,p]) == aa116[:,p]
    @test full(ss116[p,p]) == aa116[p,p]

    # bool indexing
    li = bitrand(size(aa116,1))
    lj = bitrand(size(aa116,2))
    @test full(ss116[li,j]) == aa116[li,j]
    @test full(ss116[li,:]) == aa116[li,:]
    @test full(ss116[i,lj]) == aa116[i,lj]
    @test full(ss116[:,lj]) == aa116[:,lj]
    @test full(ss116[li,lj]) == aa116[li,lj]

    # empty indices
    for empty in (1:0, Int[])
        @test full(ss116[empty,:]) == aa116[empty,:]
        @test full(ss116[:,empty]) == aa116[:,empty]
        @test full(ss116[empty,lj]) == aa116[empty,lj]
        @test full(ss116[li,empty]) == aa116[li,empty]
        @test full(ss116[empty,empty]) == aa116[empty,empty]
    end

    # out of bounds indexing
    @test_throws BoundsError ss116[0, 1]
    @test_throws BoundsError ss116[end+1, 1]
    @test_throws BoundsError ss116[1, 0]
    @test_throws BoundsError ss116[1, end+1]
    for j in (1, 1:size(s116,2), 1:1, Int[1], trues(size(s116, 2)), 1:0, Int[])
        @test_throws BoundsError ss116[0:1, j]
        @test_throws BoundsError ss116[[0, 1], j]
        @test_throws BoundsError ss116[end:end+1, j]
        @test_throws BoundsError ss116[[end, end+1], j]
    end
    for i in (1, 1:size(s116,1), 1:1, Int[1], trues(size(s116, 1)), 1:0, Int[])
        @test_throws BoundsError ss116[i, 0:1]
        @test_throws BoundsError ss116[i, [0, 1]]
        @test_throws BoundsError ss116[i, end:end+1]
        @test_throws BoundsError ss116[i, [end, end+1]]
    end
end

# workaround issue #7197: comment out let-block
#let S = SparseMatrixCSC(3, 3, UInt8[1,1,1,1], UInt8[], Int64[])
S1290 = SparseMatrixCSC(3, 3, UInt8[1,1,1,1], UInt8[], Int64[])
    S1290[1,1] = 1
    S1290[5] = 2
    S1290[end] = 3
    @test S1290[end] == (S1290[1] + S1290[2,2])
    @test 6 == sum(diag(S1290))
    @test full(S1290)[[3,1],1] == full(S1290[[3,1],1])
# end


# setindex tests
let a = spzeros(Int, 10, 10)
    @test countnz(a) == 0
    a[1,:] = 1
    @test countnz(a) == 10
    @test a[1,:] == sparse(ones(Int,1,10))
    a[:,2] = 2
    @test countnz(a) == 19
    @test a[:,2] == 2*sparse(ones(Int,10))

    a[1,:] = 1:10
    @test a[1,:] == sparse([1:10;]')
    a[:,2] = 1:10
    @test a[:,2] == sparse([1:10;])
end

let A = spzeros(Int, 10, 20)
    A[1:5,1:10] = 10
    A[1:5,1:10] = 10
    @test countnz(A) == 50
    @test A[1:5,1:10] == 10 * ones(Int, 5, 10)
    A[6:10,11:20] = 0
    @test countnz(A) == 50
    A[6:10,11:20] = 20
    @test countnz(A) == 100
    @test A[6:10,11:20] == 20 * ones(Int, 5, 10)
    A[4:8,8:16] = 15
    @test countnz(A) == 121
    @test A[4:8,8:16] == 15 * ones(Int, 5, 9)
end

let ASZ = 1000, TSZ = 800
    A = sprand(ASZ, 2*ASZ, 0.0001)
    B = copy(A)
    nA = countnz(A)
    x = A[1:TSZ, 1:(2*TSZ)]
    nx = countnz(x)
    A[1:TSZ, 1:(2*TSZ)] = 0
    nB = countnz(A)
    @test nB == (nA - nx)
    A[1:TSZ, 1:(2*TSZ)] = x
    @test countnz(A) == nA
    @test A == B
    A[1:TSZ, 1:(2*TSZ)] = 10
    @test countnz(A) == nB + 2*TSZ*TSZ
    A[1:TSZ, 1:(2*TSZ)] = x
    @test countnz(A) == nA
    @test A == B
end

let A = speye(Int, 5), I=1:10, X=reshape([trues(10); falses(15)],5,5)
    @test A[I] == A[X] == [1,0,0,0,0,0,1,0,0,0]
    A[I] = [1:10;]
    @test A[I] == A[X] == collect(1:10)
end

let S = sprand(50, 30, 0.5, x->round(Int,rand(x)*100)), I = sprandbool(50, 30, 0.2)
    FS = full(S)
    FI = full(I)
    @test sparse(FS[FI]) == S[I] == S[FI]
    @test sum(S[FI]) + sum(S[!FI]) == sum(S)

    sumS1 = sum(S)
    sumFI = sum(S[FI])
    S[FI] = 0
    @test sum(S[FI]) == 0
    sumS2 = sum(S)
    @test (sum(S) + sumFI) == sumS1

    S[FI] = 10
    @test sum(S) == sumS2 + 10*sum(FI)
    S[FI] = 0
    @test sum(S) == sumS2

    S[FI] = [1:sum(FI);]
    @test sum(S) == sumS2 + sum(1:sum(FI))
end

let S = sprand(50, 30, 0.5, x->round(Int,rand(x)*100))
    N = length(S) >> 2
    I = randperm(N) .* 4
    J = randperm(N)
    sumS1 = sum(S)
    sumS2 = sum(S[I])
    S[I] = 0
    @test sum(S) == (sumS1 - sumS2)
    S[I] = J
    @test sum(S) == (sumS1 - sumS2 + sum(J))
end

#Issue 7507
@test (i7507=sparsevec(Dict{Int64, Float64}(), 10))==spzeros(10)

#Issue 7650
let S = spzeros(3, 3)
    @test size(reshape(S, 9, 1)) == (9,1)
end

let X = eye(5), M = rand(5,4), C = spzeros(3,3)
    SX = sparse(X); SM = sparse(M)
    VX = vec(X); VSX = vec(SX)
    VM = vec(M); VSM1 = vec(SM); VSM2 = sparsevec(M)
    VC = vec(C)
    @test VX == VSX
    @test VM == VSM1
    @test VM == VSM2
    @test size(VC) == (9,)
    @test nnz(VC) == 0
    @test nnz(VSX) == 5
end

#Issue 7677
let A = sprand(5,5,0.5,(n)->rand(Float64,n)), ACPY = copy(A)
    B = reshape(A,25,1)
    @test A == ACPY
    C = reinterpret(Int64, A, (25, 1))
    @test A == ACPY
    D = reinterpret(Int64, B)
    @test C == D
end

# indmax, indmin, findmax, findmin
let S = sprand(100,80, 0.5), A = full(S)
    @test indmax(S) == indmax(A)
    @test indmin(S) == indmin(A)
    @test findmin(S) == findmin(A)
    @test findmax(S) == findmax(A)
    for region in [(1,), (2,), (1,2)], m in [findmax, findmin]
        @test m(S, region) == m(A, region)
    end
end

let S = spzeros(10,8), A = full(S)
    @test indmax(S) == indmax(A) == 1
    @test indmin(S) == indmin(A) == 1
end

let A = Array(Int,0,0), S = sparse(A)
    iA = try indmax(A) end
    iS = try indmax(S) end
    @test iA === iS === nothing
    iA = try indmin(A) end
    iS = try indmin(S) end
    @test iA === iS === nothing
end

# issue #8225
@test_throws ArgumentError sparse([0],[-1],[1.0],2,2)

# issue #8363
@test_throws ArgumentError sparsevec(Dict(-1=>1,1=>2))

# issue #8976
@test conj(sparse([1im])) == sparse(conj([1im]))
@test conj!(sparse([1im])) == sparse(conj!([1im]))

# issue #9525
@test_throws ArgumentError sparse([3], [5], 1.0, 3, 3)

#findn
b = findn( speye(4) )
@test (length(b[1]) == 4)
@test (length(b[2]) == 4)

#rotations
a = sparse( [1,1,2,3], [1,3,4,1], [1,2,3,4] )

@test rot180(a,2) == a
@test rot180(a,1) == sparse( [3,3,2,1], [4,2,1,4], [1,2,3,4] )
@test rotr90(a,1) == sparse( [1,3,4,1], [3,3,2,1], [1,2,3,4] )
@test rotl90(a,1) == sparse( [4,2,1,4], [1,1,2,3], [1,2,3,4] )
@test rotl90(a,2) == rot180(a)
@test rotr90(a,2) == rot180(a)
@test rotl90(a,3) == rotr90(a)
@test rotr90(a,3) == rotl90(a)

#ensure we have preserved the correct dimensions!

a = speye(3,5)
@test size(rot180(a)) == (3,5)
@test size(rotr90(a)) == (5,3)
@test size(rotl90(a)) == (5,3)

function test_getindex_algs{Tv,Ti}(A::SparseMatrixCSC{Tv,Ti}, I::AbstractVector, J::AbstractVector, alg::Int)
    # Sorted vectors for indexing rows.
    # Similar to getindex_general but without the transpose trick.
    (m, n) = size(A)
    !isempty(I) && ((I[1] < 1) || (I[end] > m)) && BoundsError()
    if !isempty(J)
        minj, maxj = extrema(J)
        ((minj < 1) || (maxj > n)) && BoundsError()
    end

    (alg == 0) ? Base.SparseArrays.getindex_I_sorted_bsearch_A(A, I, J) :
    (alg == 1) ? Base.SparseArrays.getindex_I_sorted_bsearch_I(A, I, J) :
    Base.SparseArrays.getindex_I_sorted_linear(A, I, J)
end

let M=2^14, N=2^4
    Irand = randperm(M);
    Jrand = randperm(N);
    SA = [sprand(M, N, d) for d in [1., 0.1, 0.01, 0.001, 0.0001, 0.]];
    IA = [sort(Irand[1:round(Int,n)]) for n in [M, M*0.1, M*0.01, M*0.001, M*0.0001, 0.]];
    debug = false

    if debug
        println("row sizes: $([round(Int,nnz(S)/S.n) for S in SA])");
        println("I sizes: $([length(I) for I in IA])");
        @printf("    S    |    I    | binary S | binary I |  linear  | best\n")
    end

    J = Jrand;
    for I in IA
        for S in SA
            res = Any[1,2,3]
            times = Float64[0,0,0]
            best = [typemax(Float64), 0]
            for searchtype in [0, 1, 2]
                gc()
                tres = @timed test_getindex_algs(S, I, J, searchtype)
                res[searchtype+1] = tres[1]
                times[searchtype+1] = tres[2]
                if best[1] > tres[2]
                    best[1] = tres[2]
                    best[2] = searchtype
                end
            end

            if debug
                @printf(" %7d | %7d | %4.2e | %4.2e | %4.2e | %s\n", round(Int,nnz(S)/S.n), length(I), times[1], times[2], times[3],
                            (0 == best[2]) ? "binary S" : (1 == best[2]) ? "binary I" : "linear")
            end
            if res[1] != res[2]
                println("1 and 2")
            elseif res[2] != res[3]
                println("2, 3")
            end
            @test res[1] == res[2] == res[3]
        end
    end
end

let M = 2^8, N=2^3
    Irand = randperm(M)
    Jrand = randperm(N)
    I = sort([Irand; Irand; Irand])
    J = [Jrand; Jrand]

    SA = [sprand(M, N, d) for d in [1., 0.1, 0.01, 0.001, 0.0001, 0.]];
    for S in SA
        res = Any[1,2,3]
        for searchtype in [0, 1, 2]
            res[searchtype+1] = test_getindex_algs(S, I, J, searchtype)
        end

        @test res[1] == res[2] == res[3]
    end
end

let M = 2^14, N=2^4
    I = randperm(M)
    J = randperm(N)
    Jsorted = sort(J)

    SA = [sprand(M, N, d) for d in [1., 0.1, 0.01, 0.001, 0.0001, 0.]];
    IA = [I[1:round(Int,n)] for n in [M, M*0.1, M*0.01, M*0.001, M*0.0001, 0.]];
    debug = false
    if debug
        @printf("         |         |         |        times        |        memory       |\n")
        @printf("    S    |    I    |    J    |  sorted  | unsorted |  sorted  | unsorted |\n")
    end
    for I in IA
        Isorted = sort(I)
        for S in SA
            gc()
            ru = @timed S[I, J]
            gc()
            rs = @timed S[Isorted, Jsorted]
            if debug
                @printf(" %7d | %7d | %7d | %4.2e | %4.2e | %4.2e | %4.2e |\n", round(Int,nnz(S)/S.n), length(I), length(J), rs[2], ru[2], rs[3], ru[3])
            end
        end
    end
end

let S = sprand(10, 10, 0.1)
    @test_throws BoundsError S[[0,1,2], [1,2]]
    @test_throws BoundsError S[[1,2], [0,1,2]]
    @test_throws BoundsError S[[0,2,1], [1,2]]
    @test_throws BoundsError S[[2,1], [0,1,2]]
end

# Test that sparse / sparsevec constructors work for AbstractMatrix subtypes
let D = Diagonal(ones(10,10)),
    sm = sparse(D),
    sv = sparsevec(D)

    @test countnz(sm) == 10
    @test countnz(sv) == 10

    @test countnz(sparse(Diagonal(Int[]))) == 0
    @test countnz(sparsevec(Diagonal(Int[]))) == 0
end

# explicit zeros
if Base.USE_GPL_LIBS
a = SparseMatrixCSC(2, 2, [1, 3, 5], [1, 2, 1, 2], [1.0, 0.0, 0.0, 1.0])
@test_approx_eq lufact(a)\[2.0, 3.0] [2.0, 3.0]
@test_approx_eq cholfact(a)\[2.0, 3.0] [2.0, 3.0]
end

# issue #9917
@test sparse([]') == reshape(sparse([]), 1, 0)
@test full(sparse([])) == zeros(0)
@test_throws BoundsError sparse([])[1]
@test_throws BoundsError sparse([])[1] = 1
x = speye(100)
@test_throws BoundsError x[-10:10]

for T in (Int, Float16, Float32, Float64, BigInt, BigFloat)
    let R=rand(T[1:100;],2,2), I=rand(T[1:100;],2,2)
        D = R + I*im
        S = sparse(D)
        @test R == real(S)
        @test I == imag(S)
        @test real(sparse(R)) == R
        @test nnz(imag(sparse(R))) == 0
        @test abs(S) == abs(D)
        @test abs2(S) == abs2(D)
    end
end

# issue #10407
@test maximum(spzeros(5, 5)) == 0.0
@test minimum(spzeros(5, 5)) == 0.0

# issue #10411
for (m,n) in ((2,-2),(-2,2),(-2,-2))
    @test_throws ArgumentError spzeros(m,n)
    @test_throws ArgumentError speye(m,n)
    @test_throws ArgumentError sprand(m,n,0.2)
end

# issue #10837
# test sparse constructors from special matrices
T = Tridiagonal(randn(4),randn(5),randn(4))
S = sparse(T)
@test norm(full(T) - full(S)) == 0.0
T = SymTridiagonal(randn(5),rand(4))
S = sparse(T)
@test norm(full(T) - full(S)) == 0.0
B = Bidiagonal(randn(5),randn(4),true)
S = sparse(B)
@test norm(full(B) - full(S)) == 0.0
B = Bidiagonal(randn(5),randn(4),false)
S = sparse(B)
@test norm(full(B) - full(S)) == 0.0

# promotion in spdiagm
@test spdiagm(([1,2],[3.5],[4+5im]), (0,1,-1), 2,2) == [1 3.5; 4+5im 2]

#Test broadcasting of sparse matrixes
let  A = sprand(10,10,0.3), B = sprand(10,10,0.3), CF = rand(10,10), AF = full(A), BF = full(B), C = sparse(CF)
    @test A .* B == AF .* BF
    @test A[1,:] .* B == AF[1,:] .* BF
    @test A[:,1] .* B == AF[:,1] .* BF
    @test A .* B[1,:] == AF .*  BF[1,:]
    @test A .* B[:,1] == AF .*  BF[:,1]

    @test A .* B == AF .* BF
    @test A[1,:] .* BF == AF[1,:] .* BF
    @test A[:,1] .* BF == AF[:,1] .* BF
    @test A .* BF[1,:] == AF .*  BF[1,:]
    @test A .* BF[:,1] == AF .*  BF[:,1]

    @test A .* B == AF .* BF
    @test AF[1,:] .* B == AF[1,:] .* BF
    @test AF[:,1] .* B == AF[:,1] .* BF
    @test AF .* B[1,:] == AF .*  BF[1,:]
    @test AF .* B[:,1] == AF .*  BF[:,1]

    @test A .* B == AF .* BF
    @test A[1,:] .* B == AF[1,:] .* BF
    @test A[:,1] .* B == AF[:,1] .* BF
    @test A .* B[1,:] == AF .*  BF[1,:]
    @test A .* B[:,1] == AF .*  BF[:,1]

    @test A .* 3 == AF .* 3
    @test 3 .* A == 3 .* AF
    #@test A[1,:] .* 3 == AF[1,:] .* 3
    @test all(A[1,:] .* 3 .== AF[1,:] .* 3)
    #@test A[:,1] .* 3 == AF[:,1] .* 3
    @test all(A[:,1] .* 3 .== AF[:,1] .* 3)
    #TODO: simple comparation with == returns false because the left side is a (two-dimensional) SparseMatrixCSC
    #      while the right side is a Vector


    @test A .- 3 == AF .- 3
    @test 3 .- A == 3 .- AF
    @test A .- B == AF .- BF
    @test A - AF == zeros(AF)
    @test AF - A == zeros(AF)
    @test A[1,:] .- B == AF[1,:] .- BF
    @test A[:,1] .- B == AF[:,1] .- BF
    @test A .- B[1,:] == AF .-  BF[1,:]
    @test A .- B[:,1] == AF .-  BF[:,1]

    @test A .+ 3 == AF .+ 3
    @test 3 .+ A == 3 .+ AF
    @test A .+ B == AF .+ BF
    @test A + AF == AF + A
    @test (A .< B) == (AF .< BF)
    @test (A .!= B) == (AF .!= BF)

    @test A ./ 3 == AF ./ 3
    @test A .\ 3 == AF .\ 3
    @test 3 ./ A == 3 ./ AF
    @test 3 .\ A == 3 .\ AF
    @test A .\ C == AF .\ CF
    @test A ./ C == AF ./ CF
    @test A ./ CF[:,1] == AF ./ CF[:,1]
    @test A .\ CF[:,1] == AF .\ CF[:,1]
    @test BF ./ C == BF ./ CF
    @test BF .\ C == BF .\ CF

    @test A .^ 3 == AF .^ 3
    @test 3 .^ A == 3 .^ AF
    @test A .^ BF[:,1] == AF .^ BF[:,1]
    @test BF[:,1] .^ A == BF[:,1] .^ AF
end

# test throws
A = sprandbool(5,5,0.2)
@test_throws ArgumentError reinterpret(Complex128,A,(5,5))
@test_throws DimensionMismatch reinterpret(Int8,A,(20,))
@test_throws DimensionMismatch reshape(A,(20,2))
@test_throws ArgumentError squeeze(A,(1,1))

# test similar with type conversion
A = speye(5)
@test size(similar(A,Complex128,Int)) == (5,5)
@test typeof(similar(A,Complex128,Int)) == SparseMatrixCSC{Complex128,Int}
@test size(similar(A,Complex128,Int8)) == (5,5)
@test typeof(similar(A,Complex128,Int8)) == SparseMatrixCSC{Complex128,Int8}
@test similar(A,Complex128,(6,6)) == spzeros(Complex128,6,6)
@test convert(Matrix,A) == full(A)

# test float
A = sprandbool(5,5,0.0)
@test eltype(float(A)) == Float64  # issue #11658
A = sprandbool(5,5,0.2)
@test float(A) == float(full(A))

# test sparsevec
A = sparse(ones(5,5))
@test all(full(sparsevec(A)) .== ones(25))
@test all(full(sparsevec([1:5;],1)) .== ones(5))
@test_throws ArgumentError sparsevec([1:5;], [1:4;])

#test sparse
@test sparse(A) == A
@test sparse([1:5;],[1:5;],1) == speye(5)

#test speye and one
@test speye(A) == speye(5)
@test eye(A) == speye(5)
@test one(A) == speye(5)
@test_throws DimensionMismatch one(sprand(5,6,0.2))

#istriu/istril
A = sparse(triu(rand(5,5)))
@test istriu(A)
@test !istriu(sparse(ones(5,5)))
A = sparse(tril(rand(5,5)))
@test istril(A)
@test !istril(sparse(ones(5,5)))

# symperm
srand(1234321)
A = triu(sprand(10,10,0.2))       # symperm operates on upper triangle
perm = randperm(10)
@test symperm(A,perm).colptr == [1,2,3,3,3,4,5,5,7,9,10]

# droptol
@test Base.droptol!(A,0.01).colptr == [1,1,1,2,2,3,4,6,6,7,9]

#trace
@test_throws DimensionMismatch trace(sparse(ones(5,6)))
@test trace(speye(5)) == 5

#diagm on a matrix
@test_throws DimensionMismatch diagm(sparse(ones(5,2)))
@test_throws DimensionMismatch diagm(sparse(ones(2,5)))
@test diagm(sparse(ones(1,5))) == speye(5)

# triu/tril
A = sprand(5,5,0.2)
AF = full(A)
@test full(triu(A,1)) == triu(AF,1)
@test full(tril(A,1)) == tril(AF,1)
@test full(triu!(copy(A), 2)) == triu(AF,2)
@test full(tril!(copy(A), 2)) == tril(AF,2)
@test_throws BoundsError tril(A,6)
@test_throws BoundsError tril(A,-6)
@test_throws BoundsError triu(A,6)
@test_throws BoundsError triu(A,-6)

# test norm

A = sparse(Int[],Int[],Float64[],0,0)
@test norm(A) == zero(eltype(A))
A = sparse([1.0])
@test norm(A) == 1.0
@test_throws ArgumentError norm(sprand(5,5,0.2),3)
@test_throws ArgumentError norm(sprand(5,5,0.2),2)

# test ishermitian and issym real matrices
A = speye(5,5)
@test ishermitian(A) == true
@test issym(A) == true
A[1,3] = 1.0
@test ishermitian(A) == false
@test issym(A) == false
A[3,1] = 1.0
@test ishermitian(A) == true
@test issym(A) == true

# test ishermitian and issym complex matrices
A = speye(5,5) + im*speye(5,5)
@test ishermitian(A) == false
@test issym(A) == true
A[1,4] = 1.0 + im
@test ishermitian(A) == false
@test issym(A) == false

A = speye(Complex128, 5,5)
A[3,2] = 1.0 + im
@test ishermitian(A) == false
@test issym(A) == false
A[2,3] = 1.0 - im
@test ishermitian(A) == true
@test issym(A) == false

A = sparse(zeros(5,5))
@test ishermitian(A) == true
@test issym(A) == true

# Test with explicit zeros
A = speye(Complex128, 5,5)
A[3,1] = 2
A.nzval[2] = 0.0
@test ishermitian(A) == true
@test issym(A) == true

# equality ==
A1 = speye(10)
A2 = speye(10)
nonzeros(A1)[end]=0
@test A1!=A2
nonzeros(A1)[end]=1
@test A1==A2
A1[1:4,end] = 1
@test A1!=A2
nonzeros(A1)[end-4:end-1]=0
@test A1==A2
A2[1:4,end-1] = 1
@test A1!=A2
nonzeros(A2)[end-5:end-2]=0
@test A1==A2
A2[2:3,1] = 1
@test A1!=A2
nonzeros(A2)[2:3]=0
@test A1==A2
A1[2:5,1] = 1
@test A1!=A2
nonzeros(A1)[2:5]=0
@test A1==A2
@test sparse([1,1,0])!=sparse([0,1,1])

# UniformScaling
A = sprandn(10,10,0.5)
@test A + I == full(A) + I
@test I + A == I + full(A)
@test A - I == full(A) - I
@test I - A == I - full(A)

# Test error path if triplet vectors are not all the same length (#12177)
@test_throws ArgumentError sparse([1,2,3], [1,2], [1,2,3], 3, 3)
@test_throws ArgumentError sparse([1,2,3], [1,2,3], [1,2], 3, 3)

#Issue 12118: sparse matrices are closed under +, -, min, max
let
    A12118 = sparse([1,2,3,4,5], [1,2,3,4,5], [1,2,3,4,5])
    B12118 = sparse([1,2,4,5],   [1,2,3,5],   [2,1,-1,-2])

    @test A12118 + B12118 == sparse([1,2,3,4,4,5], [1,2,3,3,4,5], [3,3,3,-1,4,3])
    @test typeof(A12118 + B12118) == SparseMatrixCSC{Int,Int}

    @test A12118 - B12118 == sparse([1,2,3,4,4,5], [1,2,3,3,4,5], [-1,1,3,1,4,7])
    @test typeof(A12118 - B12118) == SparseMatrixCSC{Int,Int}

    @test max(A12118, B12118) == sparse([1,2,3,4,5], [1,2,3,4,5], [2,2,3,4,5])
    @test typeof(max(A12118, B12118)) == SparseMatrixCSC{Int,Int}

    @test min(A12118, B12118) == sparse([1,2,4,5], [1,2,3,5], [1,1,-1,-2])
    @test typeof(min(A12118, B12118)) == SparseMatrixCSC{Int,Int}
end

# test sparse matrix norms
Ac = sprandn(10,10,.1) + im* sprandn(10,10,.1)
Ar = sprandn(10,10,.1)
Ai = ceil(Int,Ar*100)
@test_approx_eq norm(Ac,1)     norm(full(Ac),1)
@test_approx_eq norm(Ac,Inf)   norm(full(Ac),Inf)
@test_approx_eq vecnorm(Ac)    vecnorm(full(Ac))
@test_approx_eq norm(Ar,1)     norm(full(Ar),1)
@test_approx_eq norm(Ar,Inf)   norm(full(Ar),Inf)
@test_approx_eq vecnorm(Ar)    vecnorm(full(Ar))
@test_approx_eq norm(Ai,1)     norm(full(Ai),1)
@test_approx_eq norm(Ai,Inf)   norm(full(Ai),Inf)
@test_approx_eq vecnorm(Ai)    vecnorm(full(Ai))

# test sparse matrix cond
A = sparse(reshape([1.0],1,1))
Ac = sprandn(20,20,.5) + im* sprandn(20,20,.5)
Ar = sprandn(20,20,.5)
@test cond(A,1) == 1.0
@test_approx_eq_eps cond(Ar,1) cond(full(Ar),1) 1e-4
@test_approx_eq_eps cond(Ac,1) cond(full(Ac),1) 1e-4
@test_approx_eq_eps cond(Ar,Inf) cond(full(Ar),Inf) 1e-4
@test_approx_eq_eps cond(Ac,Inf) cond(full(Ac),Inf) 1e-4
@test_throws ArgumentError cond(A,2)
@test_throws ArgumentError cond(A,3)
let Arect = spzeros(10, 6)
    @test_throws DimensionMismatch cond(Arect, 1)
    @test_throws ArgumentError cond(Arect,2)
    @test_throws DimensionMismatch cond(Arect, Inf)
end

# test sparse matrix normestinv
Ac = sprandn(20,20,.5) + im* sprandn(20,20,.5)
Aci = ceil(Int64,100*sprand(20,20,.5))+ im*ceil(Int64,sprand(20,20,.5))
Ar = sprandn(20,20,.5)
Ari = ceil(Int64,100*Ar)
@test_approx_eq_eps Base.SparseArrays.normestinv(Ac,3) norm(inv(full(Ac)),1) 1e-4
@test_approx_eq_eps Base.SparseArrays.normestinv(Aci,3) norm(inv(full(Aci)),1) 1e-4
@test_approx_eq_eps Base.SparseArrays.normestinv(Ar) norm(inv(full(Ar)),1) 1e-4
@test_throws ArgumentError Base.SparseArrays.normestinv(Ac,0)
@test_throws ArgumentError Base.SparseArrays.normestinv(Ac,21)
@test_throws DimensionMismatch Base.SparseArrays.normestinv(sprand(3,5,.9))

@test_throws ErrorException transpose(sub(sprandn(10, 10, 0.3), 1:4, 1:4))
@test_throws ErrorException ctranspose(sub(sprandn(10, 10, 0.3), 1:4, 1:4))

# csc_permute
A = sprand(10,10,0.2)
p = randperm(10)
q = randperm(10)
@test Base.SparseArrays.csc_permute(A, invperm(p), q) == full(A)[p, q]

# issue #13008
@test_throws ArgumentError sparse(collect(1:100), collect(1:100), fill(5,100), 5, 5)
@test_throws ArgumentError sparse(Int[], collect(1:5), collect(1:5))

# issue #13024
let
    A13024 = sparse([1,2,3,4,5], [1,2,3,4,5], fill(true,5))
    B13024 = sparse([1,2,4,5],   [1,2,3,5],   fill(true,4))

    @test A13024 & B13024 == sparse([1,2,5], [1,2,5], fill(true,3))
    @test typeof(A13024 & B13024) == SparseMatrixCSC{Bool,Int}

    @test A13024 | B13024 == sparse([1,2,3,4,4,5], [1,2,3,3,4,5], fill(true,6))
    @test typeof(A13024 | B13024) == SparseMatrixCSC{Bool,Int}

    @test A13024 $ B13024 == sparse([3,4,4], [3,3,4], fill(true,3), 5, 5)
    @test typeof(A13024 $ B13024) == SparseMatrixCSC{Bool,Int}

    @test max(A13024, B13024) == sparse([1,2,3,4,4,5], [1,2,3,3,4,5], fill(true,6))
    @test typeof(max(A13024, B13024)) == SparseMatrixCSC{Bool,Int}

    @test min(A13024, B13024) == sparse([1,2,5], [1,2,5], fill(true,3))
    @test typeof(min(A13024, B13024)) == SparseMatrixCSC{Bool,Int}

    for op in (+, -, &, |, $, max, min)
        @test op(A13024, B13024) == op(full(A13024), full(B13024))
    end
end

let A = 2. * speye(5,5)
    @test full(spones(A)) == eye(full(A))
end

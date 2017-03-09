using Base.Test

function test_bounds(A::ANY)
    @test_throws BoundsError A[0]
    @test_throws BoundsError A[end+1]
    @test_throws BoundsError A[1, 0]
    @test_throws BoundsError A[1, end+1]
    @test_throws BoundsError A[1, 1, 0]
    @test_throws BoundsError A[1, 1, end+1]
    @test_throws BoundsError A[0, 1]
    @test_throws BoundsError A[end+1, 1]
    @test_throws BoundsError A[0, 1, 1]
    @test_throws BoundsError A[end+1, 1, 1]
    @test_throws BoundsError A[1, 0, 1]
    @test_throws BoundsError A[1, end+1, 1]
end

####### "Classical" tests #######

# issue #11289
x11289 = randn(5,5)
@test isempty(view(x11289, Int[], :))
@test isempty(view(x11289, [2,5], Int[]))
@test isempty(view(x11289, Int[], 2))


# Tests where non-trailing dimensions are preserved
A = copy(reshape(1:120, 3, 5, 8))
sA = view(A, 2:2, 1:5, :)
@test strides(sA) == (1, 3, 15)
@test parent(sA) == A
@test parentindexes(sA) == (2:2, 1:5, Base.Slice(1:8))
@test Base.parentdims(sA) == [1:3;]
@test size(sA) == (1, 5, 8)
@test indices(sA) === (Base.OneTo(1), Base.OneTo(5), Base.OneTo(8))
@test sA[1, 2, 1:8][:] == [5:15:120;]
sA[2:5:end] = -1
@test all(sA[2:5:end] .== -1)
@test all(A[5:15:120] .== -1)
@test strides(sA) == (1,3,15)
@test stride(sA,3) == 15
@test stride(sA,4) == 120
test_bounds(sA)
sA = view(A, 1:3, 1:5, 5)
@test Base.parentdims(sA) == [1:2;]
sA[1:3,1:5] = -2
@test all(A[:,:,5] .== -2)
sA[:] = -3
@test all(A[:,:,5] .== -3)
@test strides(sA) == (1,3)
test_bounds(sA)
sA = view(A, 1:3, 3:3, 2:5)
@test Base.parentdims(sA) == [1:3;]
@test size(sA) == (3,1,4)
@test indices(sA) === (Base.OneTo(3), Base.OneTo(1), Base.OneTo(4))
@test sA == A[1:3,3:3,2:5]
@test sA[:] == A[1:3,3,2:5][:]
test_bounds(sA)
sA = view(A, 1:2:3, 1:3:5, 1:2:8)
@test Base.parentdims(sA) == [1:3;]
@test strides(sA) == (2,9,30)
@test sA[:] == A[1:2:3, 1:3:5, 1:2:8][:]
# issue #8807
@test view(view([1:5;], 1:5), 1:5) == [1:5;]
# Test with mixed types
@test sA[:, Int16[1,2], big(2)] == [31 40; 33 42]
test_bounds(sA)
sA = view(A, 1:1, 1:5, [1 3; 4 2])
@test ndims(sA) == 4
@test indices(sA) === (Base.OneTo(1), Base.OneTo(5), Base.OneTo(2), Base.OneTo(2))
sA = view(A, 1:2, 3, [1 3; 4 2])
@test ndims(sA) == 3
@test indices(sA) === (Base.OneTo(2), Base.OneTo(2), Base.OneTo(2))

# logical indexing #4763
A = view([1:10;], 5:8)
@test A[A.<7] == view(A, A.<7) == [5, 6]
@test Base.unsafe_getindex(A, A.<7) == [5, 6]
B = reshape(1:16, 4, 4)
sB = view(B, 2:3, 2:3)
@test sB[sB.>8] == view(sB, sB.>8) == [10, 11]
@test Base.unsafe_getindex(sB, sB.>8) == [10, 11]

# Tests where dimensions are dropped
A = copy(reshape(1:120, 3, 5, 8))
sA = view(A, 2, :, 1:8)
@test parent(sA) == A
@test parentindexes(sA) == (2, Base.Slice(1:5), 1:8)
@test Base.parentdims(sA) == [2:3;]
@test size(sA) == (5, 8)
@test indices(sA) === (Base.OneTo(5), Base.OneTo(8))
@test strides(sA) == (3,15)
@test sA[2, 1:8][:] == [5:15:120;]
@test sA[:,1] == [2:3:14;]
@test sA[2:5:end] == [5:15:110;]
sA[2:5:end] = -1
@test all(sA[2:5:end] .== -1)
@test all(A[5:15:120] .== -1)
test_bounds(sA)
sA = view(A, 1:3, 1:5, 5)
@test Base.parentdims(sA) == [1:2;]
@test size(sA) == (3,5)
@test indices(sA) === (Base.OneTo(3),Base.OneTo(5))
@test strides(sA) == (1,3)
test_bounds(sA)
sA = view(A, 1:2:3, 3, 1:2:8)
@test Base.parentdims(sA) == [1,3]
@test size(sA) == (2,4)
@test indices(sA) === (Base.OneTo(2), Base.OneTo(4))
@test strides(sA) == (2,30)
@test sA[:] == A[sA.indexes...][:]
test_bounds(sA)

a = [5:8;]
@test parent(a) == a
@test parentindexes(a) == (1:4,)

# issue #6218 - logical indexing
A = rand(2, 2, 3)
msk = ones(Bool, 2, 2)
msk[2,1] = false
sA = view(A, :, :, 1)
sA[msk] = 1.0
@test sA[msk] == ones(countnz(msk))

# bounds checking upon construction; see #4044, #10296
@test_throws BoundsError view(1:10, 8:11)
A = reshape(1:20, 5, 4)
sA = view(A, 1:2, 1:3)
@test_throws BoundsError view(sA, 1:3, 1:3)
@test_throws BoundsError view(sA, 1:2, 1:4)
view(sA, 1:2, 1:2)
@test_throws BoundsError view(A, 17:23)
view(A, 17:20)

# Linear indexing by one multidimensional array:
A = reshape(1:120, 3, 5, 8)
sA = view(A, :, :, :)
@test sA[[72 17; 107 117]] == [72 17; 107 117]
@test sA[[99 38 119 14 76 81]] == [99 38 119 14 76 81]
@test sA[[ones(Int, 2, 2, 2); 2ones(Int, 2, 2, 2)]] == [ones(Int, 2, 2, 2); 2ones(Int, 2, 2, 2)]
sA = view(A, 1:2, 2:3, 3:4)
@test sA[(1:8)'] == [34 35 37 38 49 50 52 53]
@test sA[[1 2 4 4; 6 1 1 4]] == [34 35 38 38; 50 34 34 38]

# issue #11871
let a = ones(Float64, (2,2)),
    b = view(a, 1:2, 1:2)
    b[2] = 2
    @test b[2] === 2.0
end

# issue #15138
let a = [1,2,3],
    b = view(a, UInt(1):UInt(2))
    @test b == view(a, UInt(1):UInt(2)) == view(view(a, :), UInt(1):UInt(2)) == [1,2]
end

let A = reshape(1:4, 2, 2)
    B = view(A, :, :)
    @test parent(B) === A
    @test parent(view(B, 0x1, :)) === parent(view(B, 0x1, :)) === A
end

# issue #15168
let A = rand(10), sA = view(copy(A), :)
    @test sA[Int16(1)] === sA[Int32(1)] === sA[Int64(1)] === A[1]
    permute!(sA, collect(Int16, 1:10))
    @test A == sA
end

# the following segfaults with LLVM 3.8 on Windows, ref #15417
@test collect(view(view(reshape(1:13^3, 13, 13, 13), 3:7, 6:6, :), 1:2:5, :, 1:2:5)) ==
    cat(3,[68,70,72],[406,408,410],[744,746,748])

# tests @view (and replace_ref_end!)
X = reshape(1:24,2,3,4)
Y = 4:-1:1

@test isa(@view(X[1:3]), SubArray)

@test X[1:end] == @.(@view X[1:end]) # test compatibility of @. and @view
@test X[1:end-3] == @view X[1:end-3]
@test X[1:end,2,2] == @view X[1:end,2,2]
# @test X[1,1:end-2] == @view X[1,1:end-2] # TODO: Re-enable after partial linear indexing deprecation
@test X[1,2,1:end-2] == @view X[1,2,1:end-2]
@test X[1,2,Y[2:end]] == @view X[1,2,Y[2:end]]
@test X[1:end,2,Y[2:end]] == @view X[1:end,2,Y[2:end]]

u = (1,2:3)
@test X[u...,2:end] == @view X[u...,2:end]
@test X[(1,)...,(2,)...,2:end] == @view X[(1,)...,(2,)...,2:end]

# test macro hygiene
let size=(x,y)-> error("should not happen"), Base=nothing
    @test X[1:end,2,2] == @view X[1:end,2,2]
end

# test that side effects occur only once
let foo = [X]
    @test X[2:end-1] == @view (push!(foo,X)[1])[2:end-1]
    @test foo == [X, X]
end

# test @views macro
@views let f!(x) = x[1:end-1] .+= x[2:end].^2
    x = [1,2,3,4]
    f!(x)
    @test x == [5,11,19,4]
    @test x[1:3] isa SubArray
    @test x[2] === 11
    @test Dict((1:3) => 4)[1:3] === 4
    x[1:2] = 0
    @test x == [0,0,19,4]
    x[1:2] .= 5:6
    @test x == [5,6,19,4]
    f!(x[3:end])
    @test x == [5,6,35,4]
    x[Y[2:3]] .= 7:8
    @test x == [5,8,7,4]
    @. x[(3,)..., ()...] += 3 # @. should convert to .+=, test compatibility with @views
    @test x == [5,8,10,4]
    i = Int[]
    # test that lhs expressions in update operations are evaluated only once:
    x[push!(i,4)[1]] += 5
    @test x == [5,8,10,9] && i == [4]
    x[push!(i,3)[end]] += 2
    @test x == [5,8,12,9] && i == [4,3]
    @. x[3:end] = 0       # make sure @. works with end expressions in @views
    @test x == [5,8,0,0]
end
@views @test isa(X[1:3], SubArray)
@test X[1:end] == @views X[1:end]
@test X[1:end-3] == @views X[1:end-3]
@test X[1:end,2,2] == @views X[1:end,2,2]
# @test X[1,1:end-2] == @views X[1,1:end-2] # TODO: Re-enable after partial linear indexing deprecation
@test X[1,2,1:end-2] == @views X[1,2,1:end-2]
@test X[1,2,Y[2:end]] == @views X[1,2,Y[2:end]]
@test X[1:end,2,Y[2:end]] == @views X[1:end,2,Y[2:end]]
@test X[u...,2:end] == @views X[u...,2:end]
@test X[(1,)...,(2,)...,2:end] == @views X[(1,)...,(2,)...,2:end]
# test macro hygiene
let size=(x,y)-> error("should not happen"), Base=nothing
    @test X[1:end,2,2] == @views X[1:end,2,2]
end

# issue #18034
# ensure that it is possible to create an isbits, IndexLinear view of an immutable Array
let
    struct ImmutableTestArray{T, N} <: Base.DenseArray{T, N}
    end
    Base.size(::Union{ImmutableTestArray, Type{ImmutableTestArray}}) = (0, 0)
    Base.IndexStyle(::Union{ImmutableTestArray, Type{ImmutableTestArray}}) = Base.IndexLinear()
    a = ImmutableTestArray{Float64, 2}()
    @test Base.IndexStyle(view(a, :, :)) == Base.IndexLinear()
    @test isbits(view(a, :, :))
end

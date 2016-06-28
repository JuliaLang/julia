# This file is a part of Julia. License is MIT: http://julialang.org/license

@testset "subarray" begin
# import Base: ViewIndex, dimsizeexpr, rangetype, merge_indexes, first_index, stride1expr, tailsize
using Base.Cartesian

print_underestimates = false

######## Utilities ###########

# Generate an array similar to A[indx1, indx2, ...], but only call
# getindex with scalar-valued indexes. This will be safe even after
# getindex starts calling sub/slice.

# The "nodrop" variant is similar to current getindex/sub, except it
# doesn't drop any dimensions (not even trailing ones)
function Agen_nodrop(A::AbstractArray, I...)
    irep = replace_colon(A, I)
    _Agen(A, irep...)
end

# This does the same thing as slice
function Agen_slice(A::AbstractArray, I...)
    irep = replace_colon(A, I)
    B = _Agen(A, irep...)
    sd = Int[]
    for i = 1:length(I)
        if isa(I[i], Real)
            push!(sd, i)
        end
    end
    squeeze(B, sd)
end

_Agen(A, i1) = [A[j1] for j1 in i1]
_Agen(A, i1, i2) = [A[j1,j2] for j1 in i1, j2 in i2]
_Agen(A, i1, i2, i3) = [A[j1,j2,j3] for j1 in i1, j2 in i2, j3 in i3]
_Agen(A, i1, i2, i3, i4) = [A[j1,j2,j3,j4] for j1 in i1, j2 in i2, j3 in i3, j4 in i4]

function replace_colon(A::AbstractArray, I)
    Iout = Array{Any}(length(I))
    for d = 1:length(I)-1
        Iout[d] = isa(I[d], Colon) ? (1:size(A,d)) : I[d]
    end
    d = length(I)
    Iout[d] = isa(I[d], Colon) ? (1:prod(size(A)[d:end])) : I[d]
    Iout
end

# To avoid getting confused by manipulations that are implemented for SubArrays,
# it's good to copy the contents to an Array. This version protects against
# `similar` ever changing its meaning.
function copy_to_array(A::AbstractArray)
    Ac = Array{eltype(A)}(size(A))
    copy!(Ac, A)
end

# Discover the highest dimension along which the values in A are
# separated by a single increment.  If A was extracted via getindex
# from reshape(1:N, ...), this is equivalent to finding the highest
# dimension of the SubArray consistent with a single stride in the
# parent array.
function single_stride_dim(A::Array)
    ld = 0
    while ld < ndims(A)
        # Collapse all dimensions up to & including ld+1 into the first dimension
        shp = [prod(size(A)[1:ld+1])]
        for j = ld+2:ndims(A)
            push!(shp, size(A,j))
        end
        Ar = reshape(A, shp...)
        # Compute the diff along dimension 1
        if size(Ar, 1) > 1
            indexes = map(d->1:size(Ar,d), [1:ndims(Ar);])
            indexesp = copy(indexes); indexesp[1] = 2:size(Ar,1)
            indexesm = copy(indexes); indexesm[1] = 1:size(Ar,1)-1
            dA = Ar[indexesp...] - Ar[indexesm...]
            ustride = unique(dA[:])
            if length(ustride) == 1  # is it a single stride?
                ld += 1
            else
                break
            end
        else
            ld += 1
        end
    end
    ld
end
single_stride_dim(A::ANY) = single_stride_dim(copy_to_array(A))

# Testing equality of AbstractArrays, using several different methods to access values
function test_cartesian(A::ANY, B::ANY)
    isgood = true
    for (IA, IB) in zip(eachindex(A), eachindex(B))
        if A[IA] != B[IB]
            @show IA IB A[IA] B[IB]
            isgood = false
            break
        end
    end
    if !isgood
        @show A
        @show B
        error("Mismatch")
    end
end

function test_linear(A::ANY, B::ANY)
    length(A) == length(B) || error("length mismatch")
    isgood = true
    for (iA, iB) in zip(1:length(A), 1:length(B))
        if A[iA] != B[iB]
            isgood = false
            break
        end
    end
    if !isgood
        @show A
        @show A.indexes
        @show B
        error("Mismatch")
    end
end

# "mixed" means 2 indexes even for N-dimensional arrays
test_mixed{T}(::AbstractArray{T,1}, ::Array) = nothing
test_mixed{T}(::AbstractArray{T,2}, ::Array) = nothing
test_mixed(A, B::Array) = _test_mixed(A, reshape(B, size(A)))
function _test_mixed(A::ANY, B::ANY)
    L = length(A)
    m = size(A, 1)
    n = div(L, m)
    isgood = true
    for j = 1:n, i = 1:m
        if A[i,j] != B[i,j]
            isgood = false
            break
        end
    end
    if !isgood
        @show A
        @show B
        error("Mismatch")
    end
    nothing
end

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

function dim_break_linindex(I)
    i = 1
    while i <= length(I) && !isa(I[i], Vector{Int})
        i += 1
    end
    i - 1
end

function runsubarraytests(A::Array, I...)
    # Direct test of linear indexing inference
    C = Agen_nodrop(A, I...)
    ld = min(single_stride_dim(C), dim_break_linindex(I))
    S = view(A, I...)
    if Base.iscontiguous(S)
        @test S.stride1 == 1
    end
    test_linear(S, C)
    test_cartesian(S, C)
    test_mixed(S, C)
end

function runsubarraytests(A::ANY, I...)
    # When A was created with sub, we have to check bounds, since some
    # of the "residual" dimensions have size 1. It's possible that we
    # need dedicated tests for sub.
    for d = 1:length(I)-1
        if !isa(I[d], Colon) && any(I[d] .> size(A,d))
            return nothing
        end
    end
    if !isa(I[end], Colon) && any(I[end] .> prod(size(A)[length(I):end]))
        return nothing
    end
    AA = copy_to_array(A)
    # Direct test of linear indexing inference
    C = Agen_nodrop(AA, I...)
    Cld = ld = min(single_stride_dim(C), dim_break_linindex(I))
    Cdim = AIindex = 0
    while Cdim <= Cld && AIindex < length(A.indexes)
        AIindex += 1
        if isa(A.indexes[AIindex], Real)
            ld += 1
        else
            Cdim += 1
        end
    end
    # sub
    local S
    try
        S = view(A, I...)
    catch err
        @show typeof(A)
        @show A.indexes
        @show I
        rethrow(err)
    end
    test_linear(S, C)
    test_cartesian(S, C)
    test_mixed(S, C)
    # slice
    try
        S = view(A, I...)
    catch err
        @show typeof(A)
        @show A.indexes
        @show I
        rethrow(err)
    end
    test_linear(S, C)
    test_cartesian(S, C)
    test_mixed(S, C)
end

# indexN is a cartesian index, indexNN is a linear index for 2 dimensions, and indexNNN is a linear index for 3 dimensions
function runviews(SB::AbstractArray, indexN, indexNN, indexNNN)
    @assert ndims(SB) > 2
    for i3 in indexN, i2 in indexN, i1 in indexN
        runsubarraytests(SB, i1, i2, i3)
    end
    for i2 in indexNN, i1 in indexN
        runsubarraytests(SB, i1, i2)
    end
    for i1 in indexNNN
        runsubarraytests(SB, i1)
    end
end

function runviews{T}(SB::AbstractArray{T,2}, indexN, indexNN, indexNNN)
    for i2 in indexN, i1 in indexN
        runsubarraytests(SB, i1, i2)
    end
    for i1 in indexNN
        runsubarraytests(SB, i1)
    end
end

function runviews{T}(SB::AbstractArray{T,1}, indexN, indexNN, indexNNN)
    for i1 in indexN
        runsubarraytests(SB, i1)
    end
end

runviews{T}(SB::AbstractArray{T,0}, indexN, indexNN, indexNNN) = nothing

######### Tests #########

testfull = Bool(parse(Int,(get(ENV, "JULIA_TESTFULL", "0"))))

### Views from Arrays ###
index5 = (1, :, 2:5, [4,1,5], reshape([2]), view(1:5,[2 3 4 1]))  # all work with at least size 5
index25 = (3, :, 2:11, [19,9,7], reshape([10]), view(1:25,[19 15; 4 24]))
index125 = (113, :, 85:121, [99,14,103], reshape([72]), view(1:125,reshape([25,4,102,67], 1, 2, 2)))

if testfull
    let A = copy(reshape(1:5*7*11, 11, 7, 5))
        runviews(A, index5, index25, index125)
    end
end

### Views from views ###

# "outer" indexes create snips that have at least size 5 along each dimension,
# with the exception of Int-slicing
oindex = (:, 6, 3:7, reshape([12]), [8,4,6,12,5,7], [3:7 1:5 2:6 4:8 5:9])

if testfull
    let B = copy(reshape(1:13^3, 13, 13, 13))
        for o3 in oindex, o2 in oindex, o1 in oindex
            viewB = view(B, o1, o2, o3)
            runviews(viewB, index5, index25, index125)
        end
    end
end

if !testfull
    let B = copy(reshape(1:13^3, 13, 13, 13))
        for oind in ((:,:,:),
                     (:,:,6),
                     (:,6,:),
                     (6,:,:),
                     (:,3:7,:),
                     (3:7,:,:),
                     (3:7,6,:),
                     (3:7,6,0x6),
                     (6,UInt(3):UInt(7),3:7),
                     (13:-2:1,:,:),
                     ([8,4,6,12,5,7],:,3:7),
                     (6,6,[8,4,6,12,5,7]),
                     (1,:,view(1:13,[9,12,4,13,1])),
                     (view(1:13,[9,12,4,13,1]),2:6,4),
                     ([1:5 2:6 3:7 4:8 5:9], :, 3),
                     (:, [46:-1:42 88:-1:84 22:-1:18 49:-1:45 8:-1:4]))
            runsubarraytests(B, oind...)
            viewB = view(B, oind...)
            runviews(viewB, index5, index25, index125)
        end
    end
end

# issue #11289
x11289 = randn(5,5)
@test isempty(view(x11289, Int[], :))
@test isempty(view(x11289, [2,5], Int[]))
@test isempty(view(x11289, Int[], 2))

####### "Classical" tests #######

# sub
A = copy(reshape(1:120, 3, 5, 8))
sA = view(A, 2:2, 1:5, :)
@test strides(sA) == (1, 3, 15)
@test parent(sA) == A
@test parentindexes(sA) == (2:2, 1:5, :)
@test Base.parentdims(sA) == [1:3;]
@test size(sA) == (1, 5, 8)
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

# sub logical indexing #4763
A = view([1:10;], 5:8)
@test A[A.<7] == [5, 6]
@test Base.unsafe_getindex(A, A.<7) == [5, 6]
B = reshape(1:16, 4, 4)
sB = view(B, 2:3, 2:3)
@test sB[sB.>8] == [10, 11]
@test Base.unsafe_getindex(sB, sB.>8) == [10, 11]

# slice
A = copy(reshape(1:120, 3, 5, 8))
sA = view(A, 2, :, 1:8)
@test parent(sA) == A
@test parentindexes(sA) == (2, :, 1:8)
@test Base.parentdims(sA) == [2:3;]
@test size(sA) == (5, 8)
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
@test strides(sA) == (1,3)
test_bounds(sA)
sA = view(A, 1:2:3, 3, 1:2:8)
@test Base.parentdims(sA) == [1,3]
@test size(sA) == (2,4)
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


@test X[1:end] == @view X[1:end]
@test X[1:end-3] == @view X[1:end-3]
@test X[1:end,2,2] == @view X[1:end,2,2]
@test X[1,1:end-2] == @view X[1,1:end-2]
@test X[1,2,1:end-2] == @view X[1,2,1:end-2]
@test X[1,2,Y[2:end]] == @view X[1,2,Y[2:end]]
@test X[1:end,2,Y[2:end]] == @view X[1:end,2,Y[2:end]]

u = (1,2:3)
@test X[u...,2:end] == @view X[u...,2:end]
@test X[(1,)...,(2,)...,2:end] == @view X[(1,)...,(2,)...,2:end]

# test macro hygiene
let size=(x,y)-> error("should not happen")
    @test X[1:end,2,2] == @view X[1:end,2,2]
end
end

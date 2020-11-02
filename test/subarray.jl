# This file is a part of Julia. License is MIT: https://julialang.org/license

using Test, Random, LinearAlgebra

######## Utilities ###########

# Generate an array similar to A[indx1, indx2, ...], but only call
# getindex with scalar-valued indices. This will be safe even if
# `getindex` someday calls `view`.

# The "nodrop" variant does not drop any dimensions (not even trailing ones)
function Agen_nodrop(A::AbstractArray, I...)
    irep = replace_colon(A, I)
    _Agen(A, ensure_iterable(irep)...)
end

# This drops scalar dimensions
function Agen_slice(A::AbstractArray, I...)
    irep = replace_colon(A, I)
    B = _Agen(A, ensure_iterable(irep)...)
    sd = Int[]
    for i = 1:length(I)
        if isa(I[i], Real)
            push!(sd, i)
        end
    end
    dropdims(B, dims=sd)
end

_Agen(A, i1) = [A[j1] for j1 in i1]
_Agen(A, i1, i2) = [A[j1,j2] for j1 in i1, j2 in i2]
_Agen(A, i1, i2, i3) = [A[j1,j2,j3] for j1 in i1, j2 in i2, j3 in i3]
_Agen(A, i1, i2, i3, i4) = [A[j1,j2,j3,j4] for j1 in i1, j2 in i2, j3 in i3, j4 in i4]
_Agen(A, i1, i2, i3, i4, i5) = [A[j1,j2,j3,j4,j5] for j1 in i1, j2 in i2, j3 in i3, j4 in i4, j5 in i5]
_Agen(A, i1, i2, i3, i4, i5, i6) = [A[j1,j2,j3,j4,j5,j6] for j1 in i1, j2 in i2, j3 in i3, j4 in i4, j5 in i5, j6 in i6]

function replace_colon(A::AbstractArray, I)
    Iout = Vector{Any}(undef, length(I))
    I === (:,) && return (1:length(A),)
    for d = 1:length(I)
        Iout[d] = isa(I[d], Colon) ? (1:size(A,d)) : I[d]
    end
    (Iout...,)
end

ensure_iterable(::Tuple{}) = ()
ensure_iterable(t::Tuple{Union{Number, CartesianIndex}, Vararg{Any}}) = ((t[1],), ensure_iterable(Base.tail(t))...)
ensure_iterable(t::Tuple{Any, Vararg{Any}}) = (t[1], ensure_iterable(Base.tail(t))...)

index_ndims(t::Tuple) = tup2val(Base.index_ndims(t))
tup2val(::NTuple{N}) where {N} = Val(N)

# To avoid getting confused by manipulations that are implemented for SubArrays,
# it's good to copy the contents to an Array. This version protects against
# `similar` ever changing its meaning.
function copy_to_array(A::AbstractArray)
    Ac = Array{eltype(A)}(undef, size(A))
    copyto!(Ac, A)
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
            indices = map(d->1:size(Ar,d), [1:ndims(Ar);])
            indicesp = copy(indices); indicesp[1] = 2:size(Ar,1)
            indicesm = copy(indices); indicesm[1] = 1:size(Ar,1)-1
            dA = Ar[indicesp...] - Ar[indicesm...]
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
single_stride_dim(@nospecialize(A)) = single_stride_dim(copy_to_array(A))

# Testing equality of AbstractArrays, using several different methods to access values
function test_cartesian(@nospecialize(A), @nospecialize(B))
    isgood = true
    for (IA, IB) in zip(CartesianIndices(A), CartesianIndices(B))
        @test A[IA] == B[IB]
        if A isa StridedArray
            v1 = GC.@preserve A unsafe_load(pointer(A.parent, sum((0,(strides(A) .* (IA.I .- 1))...))+Base.first_index(A)))
            @test v1 == B[IB]
        end
    end
end

function test_linear(@nospecialize(A), @nospecialize(B))
    @test length(A) == length(B)
    isgood = true
    for (iA, iB) in zip(1:length(A), 1:length(B))
        @test A[iA] == B[iB]
        if A isa StridedArray
            v1 = GC.@preserve A unsafe_load(pointer(A, iA))
            v2 = Ref(A, iA)[]
            @test v1 == v2 == B[iB]
        end
    end
end

# "mixed" means 2 indices even for N-dimensional arrays
test_mixed(::AbstractArray{T,1}, ::Array) where {T} = nothing
test_mixed(::AbstractArray{T,2}, ::Array) where {T} = nothing
test_mixed(A, B::Array) = _test_mixed(A, reshape(B, size(A)))
function _test_mixed(@nospecialize(A), @nospecialize(B))
    m = size(A, 1)
    n = size(A, 2)
    isgood = true
    for J in CartesianIndices(size(A)[2:end]), i in 1:m
        @test A[i,J] == B[i,J]
    end
    nothing
end

function test_bounds(@nospecialize(A))
    @test_throws BoundsError A[0]
    @test_throws BoundsError A[end+1]
    trailing2 = ntuple(x->1, max(ndims(A)-2, 0))
    trailing3 = ntuple(x->1, max(ndims(A)-3, 0))
    @test_throws BoundsError A[1, 0, trailing2...]
    @test_throws BoundsError A[1, end+1, trailing2...]
    @test_throws BoundsError A[1, 1, 0, trailing3...]
    @test_throws BoundsError A[1, 1, end+1, trailing3...]
    @test_throws BoundsError A[0, 1, trailing2...]
    @test_throws BoundsError A[end+1, 1, trailing2...]
    @test_throws BoundsError A[0, 1, 1, trailing3...]
    @test_throws BoundsError A[end+1, 1, 1, trailing3...]
    @test_throws BoundsError A[1, 0, 1, trailing3...]
    @test_throws BoundsError A[1, end+1, 1, trailing3...]
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

function runsubarraytests(@nospecialize(A), I...)
    # When A was created with view, we have to check bounds, since some
    # of the "residual" dimensions have size 1. It's possible that we
    # need dedicated tests for view.
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
    while Cdim <= Cld && AIindex < length(A.indices)
        AIindex += 1
        if isa(A.indices[AIindex], Real)
            ld += 1
        else
            Cdim += 1
        end
    end
    S = view(A, I...)
    test_linear(S, C)
    test_cartesian(S, C)
    test_mixed(S, C)
end

# indexN is a cartesian index, indexNN is a linear index for 2 dimensions, and indexNNN is a linear index for 3 dimensions
function runviews(SB::AbstractArray, indexN, indexNN, indexNNN)
    @assert ndims(SB) > 2
    for i3 in indexN, i2 in indexN, i1 in indexN
        runsubarraytests(SB, i1, i2, i3, ntuple(x->1, max(ndims(SB)-3, 0))...)
    end
    for i2 in indexN, i1 in indexN
        runsubarraytests(SB, i1, i2, ntuple(x->1, max(ndims(SB)-2, 0))...)
    end
    for i1 in indexNNN
        runsubarraytests(SB, i1)
    end
end

function runviews(SB::AbstractArray{T, 3} where T, indexN, indexNN, indexNNN)
    @assert ndims(SB) > 2
    for i3 in indexN, i2 in indexN, i1 in indexN
        runsubarraytests(SB, i1, i2, i3)
    end
    for i2 in indexN, i1 in indexN
        runsubarraytests(SB, i1, i2, 1)
    end
    for i1 in indexNNN
        runsubarraytests(SB, i1)
    end
end

function runviews(SB::AbstractArray{T,2}, indexN, indexNN, indexNNN) where T
    for i2 in indexN, i1 in indexN
        runsubarraytests(SB, i1, i2)
    end
    for i1 in indexNN
        runsubarraytests(SB, i1)
    end
end

function runviews(SB::AbstractArray{T,1}, indexN, indexNN, indexNNN) where T
    for i1 in indexN
        runsubarraytests(SB, i1)
    end
end

runviews(SB::AbstractArray{T,0}, indexN, indexNN, indexNNN) where {T} = nothing

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

# "outer" indices create snips that have at least size 5 along each dimension,
# with the exception of Int-slicing
oindex = (:, 6, 3:7, reshape([12]), [8,4,6,12,5,7], [3:7 1:5 2:6 4:8 5:9], reshape(2:11, 2, 5))

_ndims(::AbstractArray{T,N}) where {T,N} = N
_ndims(x) = 1

if testfull
    let B = copy(reshape(1:13^3, 13, 13, 13))
        @testset "full tests: ($o1,$o2,$o3)" for o3 in oindex, o2 in oindex, o1 in oindex
            viewB = view(B, o1, o2, o3)
            runviews(viewB, index5, index25, index125)
        end
    end
end

let B = copy(reshape(1:13^3, 13, 13, 13))
    @testset "spot checks: $oind" for oind in ((:,:,:),
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
                 (6,CartesianIndex.(6,[8,4,6,12,5,7])),
                 (CartesianIndex(13,6),[8,4,6,12,5,7]),
                 (1,:,view(1:13,[9,12,4,13,1])),
                 (2,:,reshape(2:11,2,5)),
                 (2,:,reshape(2:2:13,2,3)),
                 (3,reshape(2:11,5,2),4),
                 (3,reshape(2:2:13,3,2),4),
                 (view(1:13,[9,12,4,13,1]),2:6,4),
                 ([1:5 2:6 3:7 4:8 5:9], :, 3))
        runsubarraytests(B, oind...)
        viewB = view(B, oind...)
        runviews(viewB, index5, index25, index125)
    end
end

####### "Classical" tests #######

@testset "non-trailing dimensions" begin
    A = copy(reshape(1:120, 3, 5, 8))
    sA = view(A, 2:2, 1:5, :)
    @test @inferred(strides(sA)) == (1, 3, 15)
    @test parent(sA) == A
    @test parentindices(sA) == (2:2, 1:5, Base.Slice(1:8))
    @test size(sA) == (1, 5, 8)
    @test axes(sA) === (Base.OneTo(1), Base.OneTo(5), Base.OneTo(8))
    @test sA[1, 2, 1:8][:] == [5:15:120;]
    sA[2:5:end] .= -1
    @test all(sA[2:5:end] .== -1)
    @test all(A[5:15:120] .== -1)
    @test @inferred(strides(sA)) == (1,3,15)
    @test stride(sA,3) == 15
    @test stride(sA,4) == 120
    test_bounds(sA)
    sA = view(A, 1:3, 1:5, 5)
    sA[1:3,1:5] .= -2
    @test all(A[:,:,5] .== -2)
    fill!(sA, -3)
    @test all(A[:,:,5] .== -3)
    sA[:] .= 4
    @test all(A[:,:,5] .== 4)
    @test @inferred(strides(sA)) == (1,3)
    test_bounds(sA)
    sA = view(A, 1:3, 3:3, 2:5)
    @test size(sA) == (3,1,4)
    @test axes(sA) === (Base.OneTo(3), Base.OneTo(1), Base.OneTo(4))
    @test sA == A[1:3,3:3,2:5]
    @test sA[:] == A[1:3,3,2:5][:]
    test_bounds(sA)
    sA = view(A, 1:2:3, 1:3:5, 1:2:8)
    @test @inferred(strides(sA)) == (2,9,30)
    @test sA[:] == A[1:2:3, 1:3:5, 1:2:8][:]
    # issue #8807
    @test view(view([1:5;], 1:5), 1:5) == [1:5;]
    # Test with mixed types
    @test sA[:, Int16[1,2], big(2)] == [31 40; 33 42]
    test_bounds(sA)
    sA = view(A, 1:1, 1:5, [1 3; 4 2])
    @test ndims(sA) == 4
    @test axes(sA) === (Base.OneTo(1), Base.OneTo(5), Base.OneTo(2), Base.OneTo(2))
    sA = view(A, 1:2, 3, [1 3; 4 2])
    @test ndims(sA) == 3
    @test axes(sA) === (Base.OneTo(2), Base.OneTo(2), Base.OneTo(2))
end

@testset "logical indexing #4763" begin
    A = view([1:10;], 5:8)
    @test A[A.<7] == view(A, A.<7) == [5, 6]
    @test Base.unsafe_getindex(A, A.<7) == [5, 6]
    B = reshape(1:16, 4, 4)
    sB = view(B, 2:3, 2:3)
    @test sB[sB.>8] == view(sB, sB.>8) == [10, 11]
    @test Base.unsafe_getindex(sB, sB.>8) == [10, 11]
end

@testset "with dropped dimensions" begin
    A = copy(reshape(1:120, 3, 5, 8))
    sA = view(A, 2, :, 1:8)
    @test parent(sA) == A
    @test parentindices(sA) == (2, Base.Slice(1:5), 1:8)
    @test size(sA) == (5, 8)
    @test axes(sA) === (Base.OneTo(5), Base.OneTo(8))
    @test @inferred(strides(sA)) == (3,15)
    @test sA[2, 1:8][:] == [5:15:120;]
    @test sA[:,1] == [2:3:14;]
    @test sA[2:5:end] == [5:15:110;]
    sA[2:5:end] .= -1
    @test all(sA[2:5:end] .== -1)
    @test all(A[5:15:120] .== -1)
    test_bounds(sA)
    sA = view(A, 1:3, 1:5, 5)
    @test size(sA) == (3,5)
    @test axes(sA) === (Base.OneTo(3),Base.OneTo(5))
    @test @inferred(strides(sA)) == (1,3)
    test_bounds(sA)
    sA = view(A, 1:2:3, 3, 1:2:8)
    @test size(sA) == (2,4)
    @test axes(sA) === (Base.OneTo(2), Base.OneTo(4))
    @test @inferred(strides(sA)) == (2,30)
    @test sA[:] == A[sA.indices...][:]
    test_bounds(sA)
end

@testset "parent" begin
    a = [5:8;]
    @test parent(a) == a
    @test parentindices(a) == (1:4,)
end

@testset "issue #11289" begin
    x11289 = randn(5,5)
    @test isempty(view(x11289, Int[], :))
    @test isempty(view(x11289, [2,5], Int[]))
    @test isempty(view(x11289, Int[], 2))
end


@testset "issue #6218 - logical indexing" begin
    A = rand(2, 2, 3)
    msk = fill(true, 2, 2)
    msk[2,1] = false
    sA = view(A, :, :, 1)
    sA[msk] .= 1.0
    @test sA[msk] == fill(1, count(msk))
end

@testset "bounds checking upon construction; see #4044, #10296" begin
    @test_throws BoundsError view(1:10, 8:11)
    A = reshape(1:20, 5, 4)
    sA = view(A, 1:2, 1:3)
    @test_throws BoundsError view(sA, 1:3, 1:3)
    @test_throws BoundsError view(sA, 1:2, 1:4)
    view(sA, 1:2, 1:2)
    @test_throws BoundsError view(A, 17:23)
    view(A, 17:20)
end

@testset "Linear indexing by one multidimensional array" begin
    A = reshape(1:120, 3, 5, 8)
    sA = view(A, :, :, :)
    @test sA[[72 17; 107 117]] == [72 17; 107 117]
    @test sA[[99 38 119 14 76 81]] == [99 38 119 14 76 81]
    @test sA[[fill(1, (2, 2, 2)); fill(2, (2, 2, 2))]] == [fill(1, (2, 2, 2)); fill(2, (2, 2, 2))]
    sA = view(A, 1:2, 2:3, 3:4)
    @test sA[(1:8)'] == [34 35 37 38 49 50 52 53]
    @test sA[[1 2 4 4; 6 1 1 4]] == [34 35 38 38; 50 34 34 38]
end

@testset "issue #11871" begin
    a = fill(1., (2,2))
    b = view(a, 1:2, 1:2)
    b[2] = 2
    @test b[2] === 2.0
end

@testset "issue #15138" begin
    a = [1,2,3]
    b = view(a, UInt(1):UInt(2))
    @test b == view(a, UInt(1):UInt(2)) == view(view(a, :), UInt(1):UInt(2)) == [1,2]
end

@testset "unsigned index" begin
    A = reshape(1:4, 2, 2)
    B = view(A, :, :)
    @test parent(B) === A
    @test parent(view(B, 0x1, :)) === parent(view(B, 0x1, :)) === A
end

@testset "issue #15168" begin
    A = rand(10)
    sA = view(copy(A), :)
    @test sA[Int16(1)] === sA[Int32(1)] === sA[Int64(1)] === A[1]
    permute!(sA, Vector{Int16}(1:10))
    @test A == sA
end

# the following segfaults with LLVM 3.8 on Windows, ref #15417
@test Array(view(view(reshape(1:13^3, 13, 13, 13), 3:7, 6:6, :), 1:2:5, :, 1:2:5)) ==
    cat([68,70,72],[406,408,410],[744,746,748]; dims=3)

@testset "@view (and replace_ref_begin_end!)" begin
    @test_throws ArgumentError(
        "Invalid use of @view macro: argument must be a reference expression A[...]."
    ) var"@view"(LineNumberNode(@__LINE__), @__MODULE__, 1)

    X = reshape(1:24,2,3,4)
    Y = 4:-1:1

    @test isa(@view(X[1:3]), SubArray)

    @test X[begin:end] == @.(@view X[begin:end]) # test compatibility of @. and @view
    @test X[begin:end-3] == @view X[begin:end-3]
    @test X[1:end,2,begin+1] == @view X[1:end,2,begin+1]
    @test X[begin,1:end-2,1] == @view X[begin,1:end-2,1]
    @test X[begin,begin+1,begin:end-2] == @view X[begin,begin+1,begin:end-2]
    @test X[begin,2,Y[2:end]] == @view X[begin,2,Y[2:end]]
    @test X[begin:end,2,Y[begin+1:end]] == @view X[begin:end,2,Y[begin+1:end]]

    u = (1,2:3)
    @test X[u...,begin+1:end] == @view X[u...,begin+1:end]
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
    @views let f!(x) = x[begin:end-1] .+= x[begin+1:end].^2
        x = [1,2,3,4]
        f!(x)
        @test x == [5,11,19,4]
        @test x[1:3] isa SubArray
        @test x[2] === 11
        @test Dict((1:3) => 4)[1:3] === 4
        x[1:2] .= 0
        @test x == [0,0,19,4]
        x[1:2] .= 5:6
        @test x == [5,6,19,4]
        f!(x[3:end])
        @test x == [5,6,35,4]
        x[Y[2:3]] .= 7:8
        @test x == [5,8,7,4]
        x[(3,)..., ()...] += 3
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
    @test X[begin:end] == @views X[begin:end]
    @test X[begin:end-3] == @views X[begin:end-3]
    @test X[1:end,2,begin+1] == @views X[1:end,2,begin+1]
    @test X[begin,2,1:end-2] == @views X[begin,2,1:end-2]
    @test X[begin,2,Y[2:end]] == @views X[begin,2,Y[2:end]]
    @test X[begin:end,2,Y[begin+1:end]] == @views X[begin:end,2,Y[begin+1:end]]
    @test X[u...,begin+1:end] == @views X[u...,begin+1:end]
    @test X[(1,)...,(2,)...,2:end] == @views X[(1,)...,(2,)...,2:end]

    # @views for zero dimensional arrays
    A = Array{Int, 0}(undef)
    A[] = 2
    @test (@views A[]) == 2

    # test macro hygiene
    let size=(x,y)-> error("should not happen"), Base=nothing
        @test X[1:end,2,2] == @views X[1:end,2,2]
    end
end

@testset "issue #18034: an isbits, IndexLinear view of an immutable Array" begin
    struct ImmutableTestArray{T, N} <: Base.DenseArray{T, N}
    end
    Base.size(::Union{ImmutableTestArray, Type{ImmutableTestArray}}) = (0, 0)
    Base.IndexStyle(::Union{ImmutableTestArray, Type{ImmutableTestArray}}) = Base.IndexLinear()
    a = ImmutableTestArray{Float64, 2}()
    @test Base.IndexStyle(view(a, :, :)) == Base.IndexLinear()
    @test isbits(view(a, :, :))
end

@testset "inference; issue #17351, #25321" begin
    @test @inferred(reverse(view([1 2; 3 4], :, 1), dims=1)) == [3, 1]
    s = view(reshape(1:6, 2, 3), 1:2, 1:2)
    @test @inferred(s[2,2,1]) === 4

    A = rand(5,5,5,5)
    V = view(A, 1:1 ,:, 1:3, :)
    @test @inferred(strides(V)) == (1, 5, 25, 125)
end

@testset "issue #18581: slices with OneTo axes can be linear" begin
    A18581 = rand(5, 5)
    B18581 = view(A18581, :, axes(A18581,2))
    @test IndexStyle(B18581) === IndexLinear()
end

primitive type UInt48 48 end
UInt48(x::UInt64) = Core.Intrinsics.trunc_int(UInt48, x)
UInt48(x::UInt32) = Core.Intrinsics.zext_int(UInt48, x)

@testset "sizeof" begin
    @test sizeof(view(zeros(UInt8, 10), 1:4)) == 4
    @test sizeof(view(zeros(UInt8, 10), 1:3)) == 3
    @test sizeof(view(zeros(Float64, 10, 10), 1:3, 2:6)) == 120

    # Test non-power of 2 types (Issue #35884)
    a = UInt48(0x00000001);
    b = UInt48(0x00000002);
    c = UInt48(0x00000003);
    arrayOfUInt48 = [a, b, c];

    @test sizeof(view(arrayOfUInt48, 1:2)) == 16

    @test sizeof(view(Diagonal(zeros(UInt8, 10)), 1:4)) == 4
    @test sizeof(view(Diagonal(zeros(UInt8, 10)), 1:3)) == 3
    @test sizeof(view(Diagonal(zeros(Float64, 10)), 1:3, 2:6)) == 120
end

@testset "write" begin
    io = IOBuffer()
    a = UInt48[ UInt48(UInt32(i+j)) for i = 1:5, j = 1:5 ]
    @test write(io, view(a, :, 2)) == 40
    seekstart(io)
    v = Vector{UInt48}(undef, 5)
    read!(io, v)
    @test v == view(a, :, 2)

    seekstart(io)
    @test write(io, view(a, 2:5, 1:4)) == 4*4*8
    seekstart(io)
    v = Matrix{UInt48}(undef, 4, 4)
    read!(io, v)
    @test v == view(a, 2:5, 1:4)

    seekstart(io)
    @test write(io, view(a, 5:-1:1, 3)) == 5*8
    seekstart(io)
    v = Vector{UInt48}(undef, 5)
    read!(io, v)
    @test v == view(a, 5:-1:1, 3)

    seekstart(io)
    @test write(io, view(a, 1:2:5, :)) == 3*5*8
    seekstart(io)
    v = Matrix{UInt48}(undef, 3, 5)
    read!(io, v)
    @test v == view(a, 1:2:5, :)
end

@testset "unaliascopy trimming; Issue #26263" begin
    A = rand(5,5,5,5)
    V = view(A, 2:5, :, 2:5, 1:2:5)
    @test @inferred(Base.unaliascopy(V)) == V == A[2:5, :, 2:5, 1:2:5]
    @test @inferred(sum(Base.unaliascopy(V))) ≈ sum(V) ≈ sum(A[2:5, :, 2:5, 1:2:5])
end

@testset "issue #27632" begin
    function _test_27632(A)
        for J in CartesianIndices(size(A)[2:end])
            A[1, J]
        end
        nothing
    end
    # check that this doesn't crash
    @test _test_27632(view(ones(Int64, (1, 1, 1)), 1, 1, 1)) === nothing
end

@testset "issue #37199: 1-d views with offset range indices" begin
    b = zeros(6, 3)
    b[Base.IdentityUnitRange(4:6), 2] .= 3
    @test b == [zeros(6, 1) [0,0,0,3,3,3] zeros(6,1)]
    b[4, Base.IdentityUnitRange(2:3)] .= 4
    @test b == [zeros(6,1) [0,0,0,4,3,3] [0,0,0,4,0,0]]
    b[Base.IdentityUnitRange(2:3), :] .= 5
    @test b == [zeros(1, 3); fill(5, 2, 3); [zeros(3) [4,3,3] [4,0,0]]]
    b[:, Base.IdentityUnitRange(3:3)] .= 6
    @test b == [[zeros(1, 2); fill(5, 2, 2); [zeros(3) [4,3,3]]] fill(6, 6)]

    A = reshape(1:5*7*11, 11, 7, 5)
    inds = (1:4, 2:5, 2, :, fill(3))
    offset(x) = x
    offset(r::UnitRange) = Base.IdentityUnitRange(r)
    for i1 in inds
        for i2 in inds
            for i3 in inds
                vo = @view A[offset(i1), offset(i2), offset(i3)]
                v = @view A[i1, i2, i3]
                @test first(vo) == first(v) == first(A[i1, i2, i3])
                @test collect(A[i1, i2, i3]) == collect(vo) == collect(v)
            end
        end
    end
end

@testset "issue #29608; contiguousness" begin
    @test Base.iscontiguous(view(ones(1), 1))
    @test Base.iscontiguous(view(ones(10), 1:10))
    @test Base.iscontiguous(view(ones(10), :))
end

import InteractiveUtils
@testset "blas-enabled reshaped indices" begin
    p = rand(30)
    M = view(p, reshape(2:25, 6, 4))
    v = rand(4)
    @test M isa StridedArray
    @test M*v == copy(M)*v
    @test (InteractiveUtils.@which M*v) == (InteractiveUtils.@which copy(M)*v)
end

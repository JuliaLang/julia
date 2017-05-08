# This file is a part of Julia. License is MIT: http://julialang.org/license

using Base.Test

######## Utilities ###########

# Generate an array similar to A[indx1, indx2, ...], but only call
# getindex with scalar-valued indexes. This will be safe even if
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
    (Iout...,)
end

ensure_iterable(::Tuple{}) = ()
ensure_iterable(t::Tuple{Union{Number, CartesianIndex}, Vararg{Any}}) = ((t[1],), ensure_iterable(Base.tail(t))...)
ensure_iterable(t::Tuple{Any, Vararg{Any}}) = (t[1], ensure_iterable(Base.tail(t))...)

index_ndims(t::Tuple) = tup2val(Base.index_ndims(t))
tup2val{N}(::NTuple{N}) = Val{N}

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
    m = size(A, 1)
    n = size(A, 2)
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
    while Cdim <= Cld && AIindex < length(A.indexes)
        AIindex += 1
        if isa(A.indexes[AIindex], Real)
            ld += 1
        else
            Cdim += 1
        end
    end
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
end

# indexN is a cartesian index, indexNN is a linear index for 2 dimensions, and indexNNN is a linear index for 3 dimensions
function runviews(SB::AbstractArray, indexN, indexNN, indexNNN)
    @assert ndims(SB) > 2
    for i3 in indexN, i2 in indexN, i1 in indexN
        ndims(SB) > 3 && i3 isa Colon && continue # TODO: Re-enable once Colon no longer spans partial trailing dimensions
        runsubarraytests(SB, i1, i2, i3)
    end
    for i2 in indexN, i1 in indexN
        i2 isa Colon && continue # TODO: Re-enable once Colon no longer spans partial trailing dimensions
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


### Views from views ###

# "outer" indexes create snips that have at least size 5 along each dimension,
# with the exception of Int-slicing
oindex = (:, 6, 3:7, reshape([12]), [8,4,6,12,5,7], [3:7 1:5 2:6 4:8 5:9])

_ndims{T,N}(::AbstractArray{T,N}) = N
_ndims(x) = 1

if testfull
    let B = copy(reshape(1:13^3, 13, 13, 13))
        for o3 in oindex, o2 in oindex, o1 in oindex
            if (o3 isa Colon && (_ndims(o1) + _ndims(o2) != 2))
                continue # TODO: remove once Colon no longer spans partial trailing dimensions
            end
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
                     (13:-2:1,:,:))
            runsubarraytests(B, oind...)
            viewB = view(B, oind...)
            runviews(viewB, index5, index25, index125)
        end
    end
end

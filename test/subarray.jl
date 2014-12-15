# import Base: ViewIndex, nextLD, dimsizeexpr, rangetype, merge_indexes, first_index, stride1expr, tailsize, subarray_linearindexing_dim
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
    Iout = Array(Any, length(I))
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
    Ac = Array(eltype(A), size(A))
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
            indexes = map(d->1:size(Ar,d), [1:ndims(Ar)])
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
single_stride_dim(A::AbstractArray) = single_stride_dim(copy_to_array(A))

# Extract the "linear indexing dimension" from a SubArray
getLD{T,N,P,I,LD}(::SubArray{T,N,P,I,LD}) = LD

# Compare the linear indexing dimension of a SubArray
# to a direct computation of strides
function cmpLD(Atest::SubArray, Acomp)
    # Compute ld, skipping over dropped dimensions
    LD = getLD(Atest)
    ld = LD
    for i = 1:LD
        if isa(Atest.indexes[i], Real)
            ld -= 1
        end
    end
    ld, single_stride_dim(Acomp)
end

# Testing linear dimension inference for views-of-views
for N = 1:4
    @eval begin
        function test_viewview{T}(SB, A::Array{T,$N}, f, vindex)
            local SSB
            @nloops $N j d->(1:length(vindex)) d->(i_d = vindex[j_d]) begin
                I = @ntuple $N d->i_d
                try
                    SSB = f(SB, I...)
                catch err
                    println(summary(SB))
                    println(I)
                    rethrow(err)
                end
                SA = f(A, I...)
                ld, ldc = cmpLD(SSB, SA)
                if ld == ldc
                elseif ld <= ldc
                    if print_underestimates
                        println("Underestimate f = ", f, " on ", summary(SB), " with I = ", I, ", producing ", summary(SSB))
                    end
                else
                    println(summary(SB))
                    println(summary(SSB))
                    error("failed on ", I)
                end
            end
        end
    end
end

# Testing equality of AbstractArrays, using several different methods to access values
function test_cartesian(A, B)
    isgood = true
    for (IA, IB) in zip(eachindex(A), eachindex(B))
        if A[IA] != B[IB]
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

function test_linear(A, B)
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
        @show B
        error("Mismatch")
    end
end

# "mixed" means 2 indexes even for N-dimensional arrays
test_mixed{T}(A::AbstractArray{T,1}, B::Array) = nothing
test_mixed{T}(A::AbstractArray{T,2}, B::Array) = nothing
test_mixed(A, B::Array) = _test_mixed(A, reshape(B, size(A)))
function _test_mixed(A, B)
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
end

function err_li(I::Tuple, ld::Int, ldc::Int)
    @show I
    @show ld, ldc
    error("Linear indexing inference mismatch")
end

function err_li(S::SubArray, ldc::Int)
    println(summary(S))
    @show S.indexes
    @show ldc
    error("Linear indexing inference mismatch")
end

function runtests(A::Array, I...)
    # Direct test of linear indexing inference
    C = Agen_nodrop(A, I...)
    ld = single_stride_dim(C)
    ldc = Base.subarray_linearindexing_dim(typeof(A), typeof(I))
    ld == ldc || err_li(I, ld, ldc)
    # sub
    S = sub(A, I...)
    getLD(S) == ldc || err_li(S, ldc)
    if Base.iscontiguous(S)
        @test S.stride1 == 1
    end
    test_linear(S, C)
    test_cartesian(S, C)
    test_mixed(S, C)
    # slice
    S = slice(A, I...)
    getLD(S) == ldc || err_li(S, ldc)
    test_linear(S, C)
    test_cartesian(S, C)
    test_mixed(S, C)
end

function runtests(A::SubArray, I...)
    AA = copy_to_array(A)
    # Direct test of linear indexing inference
    C = Agen_nodrop(AA, I...)
    ld = single_stride_dim(C)
    # sub
    S = sub(A, I...)
    ldc = getLD(S)
    ldc <= ld || err_li(S, ld)
    test_linear(S, C)
    test_cartesian(S, C)
    test_mixed(S, C)
    # slice
    S = slice(A, I...)
    ldc = getLD(S)
    ldc <= ld || err_li(S, ld)
    test_linear(S, C)
    test_cartesian(S, C)
    test_mixed(S, C)
end

# indexN is a cartesian index, indexNN is a linear index for 2 dimensions, and indexNNN is a linear index for 3 dimensions
function runviews{T}(SB::AbstractArray{T,3}, indexN, indexNN, indexNNN)
    for i3 in indexN, i2 in indexN, i1 in indexN
        runtests(A, i1, i2, i3)
    end
    for i2 in indexNN, i1 in indexN
        runtests(A, i1, i2)
    end
    for i1 in indexNNN
        runtests(A, i1)
    end
end

function runviews{T}(SB::AbstractArray{T,2}, indexN, indexNN, indexNNN)
    for i2 in indexN, i1 in indexN
        runtests(A, i1, i2)
    end
    for i1 in indexNN
        runtests(A, i1)
    end
end

function runviews{T}(SB::AbstractArray{T,1}, indexN, indexNN, indexNNN)
    for i1 in indexN
        runtests(A, i1)
    end
end

runviews{T}(SB::AbstractArray{T,0}, indexN, indexNN, indexNNN) = nothing

######### Tests #########

### Views from Arrays ###

A = reshape(1:5*7*11, 11, 7, 5)
index5 = (2, :, 2:5, 1:2:5, [4,1,5])  # all work with at least size 5
index25 = (8, :, 2:11, 12:3:22, [4,1,5,9])
index125 = (113, :, 85:121, 2:15:92, [99,14,103])
runviews(A, index5, index25, index125)

### Views from views ###

B = reshape(1:13^3, 13, 13, 13)
# "outer" indexes create snips that have at least size 5 along each dimension, with the exception of Int-slicing
oindex = (:, 6, 3:7, 13:-2:1, [8,4,6,12,5,7])

for o3 in oindex, o2 in oindex, o1 in oindex
    sliceB = slice(B, o1, o2, o3)
    runviews(sliceB, index5, index25, index125)
end


####### "Classical" tests #######

# sub
A = reshape(1:120, 3, 5, 8)
sA = sub(A, 2, 1:5, :)
@test parent(sA) == A
@test parentindexes(sA) == (2:2, 1:5, :)
@test Base.parentdims(sA) == [1:3]
@test size(sA) == (1, 5, 8)
@test sA[1, 2, 1:8][:] == [5:15:120]
sA[2:5:end] = -1
@test all(sA[2:5:end] .== -1)
@test all(A[5:15:120] .== -1)
@test strides(sA) == (1,3,15)
@test stride(sA,3) == 15
@test stride(sA,4) == 120
sA = sub(A, 1:3, 1:5, 5)
@test Base.parentdims(sA) == [1:2]
sA[1:3,1:5] = -2
@test all(A[:,:,5] .== -2)
sA[:] = -3
@test all(A[:,:,5] .== -3)
@test strides(sA) == (1,3)
sA = sub(A, 1:3, 3, 2:5)
@test Base.parentdims(sA) == [1:3]
@test size(sA) == (3,1,4)
@test sA == A[1:3,3,2:5]
@test sA[:] == A[1:3,3,2:5][:]
sA = sub(A, 1:2:3, 1:3:5, 1:2:8)
@test Base.parentdims(sA) == [1:3]
@test strides(sA) == (2,9,30)
@test sA[:] == A[1:2:3, 1:3:5, 1:2:8][:]
# issue #8807
@test sub(sub([1:5], 1:5), 1:5) == [1:5]

# sub logical indexing #4763
A = sub([1:10], 5:8)
@test A[A.<7] == [5, 6]
B = reshape(1:16, 4, 4)
sB = sub(B, 2:3, 2:3)
@test sB[sB.>8] == [10, 11]

# slice
A = reshape(1:120, 3, 5, 8)
sA = slice(A, 2, :, 1:8)
@test parent(sA) == A
@test parentindexes(sA) == (2, :, 1:8)
@test Base.parentdims(sA) == [2:3]
@test size(sA) == (5, 8)
@test strides(sA) == (3,15)
@test sA[2, 1:8][:] == [5:15:120]
@test sA[:,1] == [2:3:14]
@test sA[2:5:end] == [5:15:110]
sA[2:5:end] = -1
@test all(sA[2:5:end] .== -1)
@test all(A[5:15:120] .== -1)
sA = slice(A, 1:3, 1:5, 5)
@test Base.parentdims(sA) == [1:2]
@test size(sA) == (3,5)
@test strides(sA) == (1,3)
sA = slice(A, 1:2:3, 3, 1:2:8)
@test Base.parentdims(sA) == [1,3]
@test size(sA) == (2,4)
@test strides(sA) == (2,30)
@test sA[:] == A[sA.indexes...][:]

a = [5:8]
@test parent(a) == a
@test parentindexes(a) == (1:4,)

# issue #6218 - logical indexing
A = rand(2, 2, 3)
msk = ones(Bool, 2, 2)
msk[2,1] = false
sA = sub(A, :, :, 1)
sA[msk] = 1.0
@test sA[msk] == ones(countnz(msk))

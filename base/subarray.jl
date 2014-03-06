## subarrays ##

typealias RangeIndex Union(Int, Range{Int}, Range1{Int})

type SubArray{T,N,A<:StoredArray,I<:(RangeIndex...,)} <: StoredArray{T,N}
    parent::A
    indexes::I
    dims::NTuple{N,Int}
    strides::Array{Int,1}  # for accessing parent with linear indexes
    first_index::Int

    # Note: no bounds-checking on construction. See issue #4044
    #linear indexing constructor (scalar)
    if N == 0 && length(I) == 1 && A <: Array
        function SubArray(p::A, i::(Int,))
            new(p, i, (), Int[], i[1])
        end
    #linear indexing constructor (ranges)
    elseif N == 1 && length(I) == 1 && A <: Array
        function SubArray(p::A, i::(Range1{Int},))
            new(p, i, (length(i[1]),), [1], first(i[1]))
        end
        function SubArray(p::A, i::(Range{Int},))
            new(p, i, (length(i[1]),), [step(i[1])], first(i[1]))
        end
    else
        function SubArray(p::A, i::I)
            newdims = Array(Int, 0)
            newstrides = Array(Int, 0)
            newfirst = 1
            pstride = 1
            for j = 1:length(i)
                if isa(i[j], Int)
                    newfirst += (i[j]-1)*pstride
                else
                    push!(newdims, length(i[j]))
                    #may want to return error if step(i[j]) <= 0
                    push!(newstrides, isa(i[j],Range1) ? pstride :
                         pstride * step(i[j]))
                    newfirst += (first(i[j])-1)*pstride
                end
                pstride *= size(p,j)
            end
            new(p, i, tuple(newdims...), newstrides, newfirst)
        end
    end
end

#linear indexing sub (may want to rename as slice)
function sub{T,N}(A::Array{T,N}, i::(Union(Range{Int}, Range1{Int}),))
    SubArray{T,1,typeof(A),typeof(i)}(A, i)
end

# if `I` were a vector, index_ranges would do the following:
# j = length(I)
# while j > 0 && isa(I[j], Int)
#     j -= 1
# end
# for i = 1:j
#     if isa(I[i], Int)
#         I[i] = I[i]:I[i]
#     end
# end
to_range(j::Int) = j:j
to_range(j::RangeIndex) = j
index_ranges(I::Int...) = I
index_ranges(i, I...) = tuple(to_range(i), index_ranges(I...)...)

function sub_internal{T,N,L}(A::AbstractArray{T,N}, i::NTuple{N,RangeIndex}, ::NTuple{L,Int})
    SubArray{T,L,typeof(A),typeof(i)}(A, i)
end

function sub{T,N}(A::AbstractArray{T,N}, i::NTuple{N,RangeIndex})
    sub_internal(A, index_ranges(i...), index_shape(i...))
end

sub{N}(A::SubArray, i::NTuple{N,RangeIndex}) = sub(A, i...)

sub(A::AbstractArray, i::RangeIndex...) = sub(A, i)

function sub(A::SubArray, i::RangeIndex...)
    L = length(i)
    while L > 0 && isa(i[L], Int); L-=1; end
    j = 1
    newindexes = Array(RangeIndex,length(A.indexes))
    for k = 1:length(A.indexes)
        if isa(A.indexes[k], Int)
            newindexes[k] = A.indexes[k]
        else
            r = A.indexes[k]
            ri = (isa(i[j],Int) && j<=L) ? (i[j]:i[j]) : i[j]
            newindexes[k] = step(r) == 1 ? (first(r)-1) + ri : first(r) + (ri-1)*step(r)
            j += 1
        end
    end
    ni = tuple(newindexes...)
    SubArray{eltype(A),L,typeof(A.parent),typeof(ni)}(A.parent, ni)
end

# Drops all Ints from a tuple of RangeIndexes
ranges_only(I::Int...) = ()
ranges_only(i::Int, I...) = ranges_only(I...)
ranges_only(i::Union(Range{Int}, Range1{Int}), I...) = tuple(i, ranges_only(I...)...)

function slice_internal{T,N,L}(A::AbstractArray{T,N}, i::NTuple{N,RangeIndex}, ::NTuple{L,RangeIndex})
    SubArray{T,L,typeof(A),typeof(i)}(A, i)
end
slice{T,N}(A::AbstractArray{T,N}, i::NTuple{N,RangeIndex}) = slice_internal(A, i, ranges_only(i...))

# Throw error on slice dimension mismatch
slice{T,N,M}(A::AbstractArray{T,N}, i::NTuple{M,RangeIndex}) = throw(BoundsError())

slice(A::AbstractArray, i::RangeIndex...) = slice(A, i)

function slice(A::SubArray, i::RangeIndex...)
    j = 1
    newindexes = Array(RangeIndex,length(A.indexes))
    for k = 1:length(A.indexes)
        if isa(A.indexes[k], Int)
            newindexes[k] = A.indexes[k]
        else
            r = A.indexes[k]
            newindexes[k] = step(r) == 1 ? (first(r)-1) + i[j] : first(r) + (i[j]-1)*step(r)
            j += 1
        end
    end
    slice(A.parent, tuple(newindexes...))
end

# Colon translation
sub(A::AbstractArray, I::Union(RangeIndex, Colon)...) = sub(A, ntuple(length(I), i-> isa(I[i], Colon) ? (1:size(A,i)) : I[i])...)
slice(A::AbstractArray, I::Union(RangeIndex, Colon)...) = slice(A, ntuple(length(I), i-> isa(I[i], Colon) ? (1:size(A,i)) : I[i])...)


### rename the old slice function ###
##squeeze all dimensions of length 1
#slice{T,N}(a::AbstractArray{T,N}) = sub(a, map(i-> i == 1 ? 1 : (1:i), size(a)))
#slice{T,N}(s::SubArray{T,N}) =
#    sub(s.parent, map(i->!isa(i, Int) && length(i)==1 ?i[1] : i, s.indexes))
#
##slice dimensions listed, error if any have length > 1
##silently ignores dimensions that are greater than N
#function slice{T,N}(a::AbstractArray{T,N}, sdims::Integer...)
#    newdims = ()
#    for i = 1:N
#        next = 1:size(a, i)
#        for j in sdims
#            if i == j
#                if size(a, i) != 1
#                    error("dimension ", i, " has length greater than 1")
#                end
#                next = 1
#                break
#            end
#        end
#        newdims = tuple(newdims..., next)
#    end
#    sub(a, newdims)
#end
#function slice{T,N}(s::SubArray{T,N}, sdims::Integer...)
#    newdims = ()
#    for i = 1:length(s.indexes)
#        next = s.indexes[i]
#        for j in sdims
#            if i == j
#                if length(next) != 1
#                    error("dimension ", i," has length greater than 1")
#                end
#                next = isa(next, Int) ? next : first(next)
#                break
#            end
#        end
#        newdims = tuple(newdims..., next)
#    end
#    sub(s.parent, newdims)
#end
### end commented code ###

size(s::SubArray) = s.dims
ndims{T,N}(s::SubArray{T,N}) = N

parent(s::SubArray) = s.parent
parentindexes(s::SubArray) = s.indexes

parent(a::AbstractArray) = a
parentindexes(a::AbstractArray) = ntuple(ndims(a), i->1:size(a,i))

copy(s::SubArray) = copy!(similar(s.parent, size(s)), s)
similar(s::SubArray, T, dims::Dims) = similar(s.parent, T, dims)

getindex{T}(s::SubArray{T,0}) = s.parent[s.first_index]

getindex{T}(s::SubArray{T,1}, i::Integer) =
    s.parent[s.first_index + (i-1)*s.strides[1]]
getindex{T}(s::SubArray{T,1}, i::Integer, j::Integer) =
    j==1 ? s.parent[s.first_index + (i-1)*s.strides[1]] : throw(BoundsError())
getindex{T}(s::SubArray{T,2}, i::Integer, j::Integer) =
    s.parent[s.first_index + (i-1)*s.strides[1] + (j-1)*s.strides[2]]
getindex{T}(s::SubArray{T,3}, i::Integer, j::Integer, k::Integer) =
    s.parent[s.first_index + (i-1)*s.strides[1] + (j-1)*s.strides[2] + (k-1)*s.strides[3]]
getindex{T}(s::SubArray{T,4}, i::Integer, j::Integer, k::Integer, l::Integer) =
    s.parent[s.first_index + (i-1)*s.strides[1] + (j-1)*s.strides[2] + (k-1)*s.strides[3] + (l-1)*s.strides[4]]
getindex{T}(s::SubArray{T,5}, i::Integer, j::Integer, k::Integer, l::Integer, m::Integer) =
    s.parent[s.first_index + (i-1)*s.strides[1] + (j-1)*s.strides[2] + (k-1)*s.strides[3] + (l-1)*s.strides[4] + (m-1)*s.strides[5]]

getindex(s::SubArray, i::Real) = getindex(s, to_index(i))
getindex(s::SubArray, i0::Real, i1::Real) =
    getindex(s, to_index(i0), to_index(i1))
getindex(s::SubArray, i0::Real, i1::Real, i2::Real) =
    getindex(s, to_index(i0), to_index(i1), to_index(i2))
getindex(s::SubArray, i0::Real, i1::Real, i2::Real, i3::Real) =
    getindex(s, to_index(i0), to_index(i1), to_index(i2), to_index(i3))
getindex(s::SubArray, i0::Real, i1::Real, i2::Real, i3::Real, i4::Real) =
    getindex(s, to_index(i0), to_index(i1), to_index(i2), to_index(i3), to_index(i4))
getindex(s::SubArray, i0::Real, i1::Real, i2::Real, i3::Real, i4::Real, i5::Real) =
    getindex(s, to_index(i0), to_index(i1), to_index(i2), to_index(i3), to_index(i4), to_index(i5))
getindex(s::SubArray, i0::Real, i1::Real, i2::Real, i3::Real, i4::Real, i5::Real, is::Real...) =
    getindex(s, to_index(i0), to_index(i1), to_index(i2), to_index(i3), to_index(i4), to_index(i5), to_index(is)...)

getindex(s::SubArray, i::Integer) = s[ind2sub(size(s), i)...]

function getindex(s::SubArray, is::Integer...)
    index = s.first_index
    for i = 1:length(is)
        isi = is[i]
        if isi != 1
            index += (is[i]-1)*s.strides[i]
        end
    end
    s.parent[index]
end

function getindex_bool_1d(S::SubArray, I::AbstractArray{Bool})
    n = sum(I)
    out = similar(S, n)
    c = 1
    for i = 1:length(I)
        if I[i]
            out[c] = S[i]
            c += 1
        end
    end
    out
end

getindex{T}(S::SubArray{T,1}, I::AbstractArray{Bool,1}) = getindex_bool_1d(S, I)
getindex{T}(S::SubArray{T,2}, I::AbstractArray{Bool,2}) = getindex_bool_1d(S, I)
getindex{T}(S::SubArray{T,3}, I::AbstractArray{Bool,3}) = getindex_bool_1d(S, I)
getindex{T}(S::SubArray{T,4}, I::AbstractArray{Bool,4}) = getindex_bool_1d(S, I)
getindex{T}(S::SubArray{T,5}, I::AbstractArray{Bool,5}) = getindex_bool_1d(S, I)

getindex{T}(s::SubArray{T,1}, I::Range1{Int}) =
    getindex(s.parent, (s.first_index+(first(I)-1)*s.strides[1]):s.strides[1]:(s.first_index+(last(I)-1)*s.strides[1]))

getindex{T}(s::SubArray{T,1}, I::Range{Int}) =
    getindex(s.parent, (s.first_index+(first(I)-1)*s.strides[1]):(s.strides[1]*step(I)):(s.first_index+(last(I)-1)*s.strides[1]))

function getindex{T,S<:Integer}(s::SubArray{T,1}, I::AbstractVector{S})
    t = Array(Int, length(I))
    for i = 1:length(I)
        t[i] = s.first_index + (I[i]-1)*s.strides[1]
    end
    getindex(s.parent, t)
end

function translate_indexes(s::SubArray, I::Union(Real,AbstractArray)...)
    n = length(I)
    newindexes = Any[s.indexes...]
    pdims = parentdims(s)
    havelinear = n < ndims(s)
    for i = 1:n-havelinear
        newindexes[pdims[i]] = s.indexes[pdims[i]][I[i]]
    end
    lastdim = pdims[n]
    if havelinear
        newindexes = newindexes[1:lastdim]
        newindexes[pdims[n]] = translate_linear_indexes(s, n, I[end], pdims)
    end
    newindexes
end

# translate a linear index vector I for dim n to a linear index vector for
# the parent array
function translate_linear_indexes(s, n, I, pdims)
    idx = Array(Int, length(I))
    ssztail = size(s)[n:end]
    indexestail = s.indexes[pdims[n:end]]
    # The next gets the strides of dimensions listed in pdims[n:end], relative to the stride of pdims[n]
    pstrd = [1]
    j = n+1
    strd = 1
    for i = pdims[n]+1:ndims(s.parent)
        strd *= size(s.parent, i-1)
        if j <= length(pdims) && i == pdims[j]
            push!(pstrd, strd)
            j += 1
        end
    end
    # Compute the offset from any omitted dimensions
    taildimsoffset = 0
    for i = pdims[n]+1:ndims(s.parent)
        thisI = s.indexes[i]
        if isa(thisI, Integer)
            taildimsoffset += (thisI-1)*stride(s.parent, i)
        end
    end
    nd = length(pstrd)
    for j=1:length(I)
        su = ind2sub(ssztail,I[j])  # convert to particular location within indexes
        K = taildimsoffset + 1
        for k = 1:nd
            K += pstrd[k]*(indexestail[k][su[k]]-1)   # convert to particular location in parent
        end
        idx[j] = K
    end
    idx
end

function parentdims(s::SubArray)
    nd = ndims(s)
    dimindex = Array(Int, nd)
    sp = strides(s.parent)
    j = 1
    for i = 1:ndims(s.parent)
        r = s.indexes[i]
        if j <= nd && (isa(r,Range) ? sp[i]*step(r) : sp[i]) == s.strides[j]
            dimindex[j] = i
            j += 1
        end
    end
    dimindex
end

function getindex(s::SubArray, I::Union(Real,AbstractVector)...)
    newindexes = translate_indexes(s, I...)

    rs = index_shape(I...)
    result = getindex(s.parent, newindexes...)
    if isequal(rs, size(result))
        return result
    else
        return reshape(result, rs)
    end
end

setindex!(s::SubArray, v, i::Integer) = setindex!(s, v, ind2sub(size(s), i)...)

function setindex!(s::SubArray, v, is::Integer...)
    index = s.first_index
    for i = 1:length(is)
        index += (is[i]-1)*s.strides[i]
    end
    s.parent[index] = v
    return s
end

setindex!{T}(s::SubArray{T,0}, v) = setindex!(s.parent, v, s.first_index)


setindex!{T}(s::SubArray{T,1}, v, i::Integer) =
    setindex!(s.parent, v, s.first_index + (i-1)*s.strides[1])

setindex!{T}(s::SubArray{T,2}, v, i::Integer, j::Integer) =
    setindex!(s.parent, v, s.first_index +(i-1)*s.strides[1]+(j-1)*s.strides[2])

setindex!{T}(s::SubArray{T,3}, v, i::Integer, j::Integer, k::Integer) =
    setindex!(s.parent, v, s.first_index +(i-1)*s.strides[1]+(j-1)*s.strides[2]+(k-1)*s.strides[3])

setindex!{T}(s::SubArray{T,4}, v, i::Integer, j::Integer, k::Integer, l::Integer) =
    setindex!(s.parent, v, s.first_index +(i-1)*s.strides[1]+(j-1)*s.strides[2]+(k-1)*s.strides[3]+(l-1)*s.strides[4])

setindex!{T}(s::SubArray{T,5}, v, i::Integer, j::Integer, k::Integer, l::Integer, m::Integer) =
    setindex!(s.parent, v, s.first_index +(i-1)*s.strides[1]+(j-1)*s.strides[2]+(k-1)*s.strides[3]+(l-1)*s.strides[4]+(m-1)*s.strides[5])

setindex!{T}(s::SubArray{T,1}, v, I::Range1{Int}) =
    setindex!(s.parent, v, (s.first_index+(first(I)-1)*s.strides[1]):s.strides[1]:(s.first_index+(last(I)-1)*s.strides[1]))

setindex!{T}(s::SubArray{T,1}, v, I::Range{Int}) =
    setindex!(s.parent, v, (s.first_index+(first(I)-1)*s.strides[1]):(s.strides[1]*step(I)):(s.first_index+(last(I)-1)*s.strides[1]))

function setindex!{T,S<:Integer}(s::SubArray{T,1}, v, I::AbstractVector{S})
    t = Array(Int, length(I))
    for i = 1:length(I)
        t[i] = s.first_index + (I[i]-1)*s.strides[1]
    end
    setindex!(s.parent, v, t)
end

# to avoid ambiguity warning
function setindex!(s::SubArray, v, I::Real)
    newindexes = translate_indexes(s, (I,))
    setindex!(s.parent, v, newindexes...)
end
function setindex!(s::SubArray, v, I::Union(Real,AbstractArray)...)
    newindexes = translate_indexes(s, I...)
    setindex!(s.parent, v, newindexes...)
end

stride(s::SubArray, i::Integer) = i <= length(s.strides) ? s.strides[i] : s.strides[end]*s.dims[end]

convert{T}(::Type{Ptr{T}}, x::SubArray{T}) =
    pointer(x.parent) + (x.first_index-1)*sizeof(T)
convert{T,S,N}(::Type{Array{T,N}},A::SubArray{S,N}) = copy!(Array(T,size(A)), A)

pointer(s::SubArray, i::Int) = pointer(s, ind2sub(size(s), i))

function pointer(s::SubArray, is::(Int...))
    index = s.first_index
    for n = 1:length(is)
        index += (is[n]-1)*s.strides[n]
    end
    return pointer(s.parent, index)
end

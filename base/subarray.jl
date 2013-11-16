## subarrays ##

typealias RangeIndex Union(Int, Range{Int}, Range1{Int})

type SubArray{T,N,A<:AbstractArray,I<:(RangeIndex...,)} <: AbstractArray{T,N}
    parent::A
    indexes::I
    dims::Dims
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
function sub{T,N}(A::Array{T,N}, i::(RangeIndex,))
    SubArray{T,(isa(i[1], Int) ? 0 : 1),typeof(A),typeof(i)}(A, i)
end

function sub{T,N}(A::AbstractArray{T,N}, i::NTuple{N,RangeIndex})
    L = length(i)
    while L > 0 && isa(i[L], Int); L-=1; end
    i0 = map(j -> isa(j, Int) ? (j:j) : j, i[1:L])
    i = ntuple(length(i), k->(k<=L ? i0[k] : i[k]))
    SubArray{T,L,typeof(A),typeof(i)}(A, i)
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


function slice{T,N}(A::AbstractArray{T,N}, i::NTuple{N,RangeIndex})
    n = 0
    for j = i; if !isa(j, Int); n += 1; end; end
    SubArray{T,n,typeof(A),typeof(i)}(A, i)
end

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

# Generic fallback for Colon translation
sub(A::AbstractArray, I...) = sub(A, ntuple(length(I), i-> isa(I[i], Colon) ? (1:size(A,i)) : I[i])...)
slice(A::AbstractArray, I...) = slice(A, ntuple(length(I), i-> isa(I[i], Colon) ? (1:size(A,i)) : I[i])...)


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
#                    error("slice: dimension ", i, " has length greater than 1")
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
#                    error("slice: dimension ", i," has length greater than 1")
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

getindex{T}(s::SubArray{T,0,AbstractArray{T,0}}) = s.parent[]
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
getindex(s::SubArray, i0::Real, i1::Real, i2::Real, i3::Real, i4::Real, i5::Real, is::Int...) =
    getindex(s, to_index(i0), to_index(i1), to_index(i2), to_index(i3), to_index(i4), to_index(5), is...)

getindex(s::SubArray, i::Integer) = s[ind2sub(size(s), i)...]

function getindex{T}(s::SubArray{T,2}, ind::Integer)
    @inbounds strd2 = s.dims[1]
    ind -= 1
    i2 = div(ind,strd2)
    i1 = ind-i2*strd2
    s.parent[s.first_index + i1*s.strides[1] + i2*s.strides[2]]
end

function getindex{T}(s::SubArray{T,3}, ind::Integer)
    @inbounds strd2 = s.dims[1]
    @inbounds strd3 = strd2*s.dims[2]
    ind -= 1
    i3 = div(ind,strd3)
    ind -= i3*strd3
    i2 = div(ind,strd2)
    i1 = ind-i2*strd2
    s.parent[s.first_index + i1*s.strides[1] + i2*s.strides[2] + i3*s.strides[3]]
end

function getindex{T}(s::SubArray{T,4}, ind::Integer)
    @inbounds strd2 = s.dims[1]
    @inbounds strd3 = strd2*s.dims[2]
    @inbounds strd4 = strd3*s.dims[3]
    ind -= 1
    i4 = div(ind,strd4)
    ind -= i4*strd4
    i3 = div(ind,strd3)
    ind -= i3*strd3
    i2 = div(ind,strd2)
    i1 = ind-i2*strd2
    s.parent[s.first_index + i1*s.strides[1] + i2*s.strides[2] + i3*s.strides[3] + i4*s.strides[4]]
end

function getindex{T}(s::SubArray{T,5}, ind::Integer)
    @inbounds strd2 = s.dims[1]
    @inbounds strd3 = strd2*s.dims[2]
    @inbounds strd4 = strd3*s.dims[3]
    @inbounds strd5 = strd4*s.dims[4]
    ind -= 1
    i5 = div(ind,strd5)
    ind -= i5*strd5
    i4 = div(ind,strd4)
    ind -= i4*strd4
    i3 = div(ind,strd3)
    ind -= i3*strd3
    i2 = div(ind,strd2)
    i1 = ind-i2*strd2
    s.parent[s.first_index + i1*s.strides[1] + i2*s.strides[2] + i3*s.strides[3] + i4*s.strides[4] + i5*s.strides[5]]
end

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
    ssztail = size(s)[n:]
    indexestail = s.indexes[pdims[n:]]
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

function setindex!{T}(s::SubArray{T,2}, v, ind::Integer)
    @inbounds strd2 = s.dims[1]
    ind -= 1
    i2 = div(ind,strd2)
    i1 = ind-i2*strd2
    s.parent[s.first_index + i1*s.strides[1] + i2*s.strides[2]] = v
    s
end

function setindex!{T}(s::SubArray{T,3}, v, ind::Integer)
    @inbounds strd2 = s.dims[1]
    @inbounds strd3 = strd2*s.dims[2]
    ind -= 1
    i3 = div(ind,strd3)
    ind -= i3*strd3
    i2 = div(ind,strd2)
    i1 = ind-i2*strd2
    s.parent[s.first_index + i1*s.strides[1] + i2*s.strides[2] + i3*s.strides[3]] = v
    s
end

function setindex!{T}(s::SubArray{T,4}, v, ind::Integer)
    @inbounds strd2 = s.dims[1]
    @inbounds strd3 = strd2*s.dims[2]
    @inbounds strd4 = strd3*s.dims[3]
    ind -= 1
    i4 = div(ind,strd4)
    ind -= i4*strd4
    i3 = div(ind,strd3)
    ind -= i3*strd3
    i2 = div(ind,strd2)
    i1 = ind-i2*strd2
    s.parent[s.first_index + i1*s.strides[1] + i2*s.strides[2] + i3*s.strides[3] + i4*s.strides[4]] = v
    s
end

function setindex!{T}(s::SubArray{T,5}, v, ind::Integer)
    @inbounds strd2 = s.dims[1]
    @inbounds strd3 = strd2*s.dims[2]
    @inbounds strd4 = strd3*s.dims[3]
    @inbounds strd5 = strd4*s.dims[4]
    ind -= 1
    i5 = div(ind,strd5)
    ind -= i5*strd5
    i4 = div(ind,strd4)
    ind -= i4*strd4
    i3 = div(ind,strd3)
    ind -= i3*strd3
    i2 = div(ind,strd2)
    i1 = ind-i2*strd2
    s.parent[s.first_index + i1*s.strides[1] + i2*s.strides[2] + i3*s.strides[3] + i4*s.strides[4] + i5*s.strides[5]] = v
    s
end

function setindex!(s::SubArray, v, is::Integer...)
    index = s.first_index
    for i = 1:length(is)
        index += (is[i]-1)*s.strides[i]
    end
    s.parent[index] = v
    return s
end

setindex!{T}(s::SubArray{T,0,AbstractArray{T,0}},v) = setindex!(s.parent, v)

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

pointer(s::SubArray, i::Int) = pointer(s, ind2sub(size(s), i))

function pointer(s::SubArray, is::(Int...))
    index = s.first_index
    for n = 1:length(is)
        index += (is[n]-1)*s.strides[n]
    end
    return pointer(s.parent, index)
end

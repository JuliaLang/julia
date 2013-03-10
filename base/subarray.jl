## subarrays ##

typealias RangeIndex Union(Int, Range{Int}, Range1{Int})

type SubArray{T,N,A<:AbstractArray,I<:(RangeIndex...,)} <: AbstractArray{T,N}
    parent::A
    indexes::I
    dims::Dims
    strides::Array{Int,1}  # for accessing parent with linear indexes
    first_index::Int

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
            newindexes[k] = A.indexes[k][(isa(i[j],Int) && j<=L) ? (i[j]:i[j]) : i[j]]
            j += 1
        end
    end
    sub(A.parent, tuple(newindexes...))
end

function slice{T,N}(A::AbstractArray{T,N}, i::NTuple{N,RangeIndex})
    n = 0
    for j = i; if !isa(j, Int); n += 1; end; end
    SubArray{T,n,typeof(A),typeof(i)}(A, i)
end

slice(A::AbstractArray, i::RangeIndex...) = slice(A, i)

function slice(A::SubArray, i::RangeIndex...)
    j = 1
    newindexes = Array(RangeIndex,length(A.indexes))
    for k = 1:length(A.indexes)
        if isa(A.indexes[k], Int)
            newindexes[k] = A.indexes[k]
        else
            newindexes[k] = A.indexes[k][i[j]]
            j += 1
        end
    end
    slice(A.parent, tuple(newindexes...))
end

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

copy(s::SubArray) = copy!(similar(s.parent, size(s)), s)
similar(s::SubArray, T, dims::Dims) = similar(s.parent, T, dims)

getindex{T}(s::SubArray{T,0,AbstractArray{T,0}}) = s.parent[]
getindex{T}(s::SubArray{T,0}) = s.parent[s.first_index]

getindex{T}(s::SubArray{T,1}, i::Integer) = s.parent[s.first_index + (i-1)*s.strides[1]]
getindex{T}(s::SubArray{T,2}, i::Integer, j::Integer) =
    s.parent[s.first_index + (i-1)*s.strides[1] + (j-1)*s.strides[2]]

getindex(s::SubArray, i::Integer) = s[ind2sub(size(s), i)...]

function getindex{T}(s::SubArray{T,2}, ind::Integer)
    ld = size(s,1)
    i = rem(ind-1,ld)+1
    j = div(ind-1,ld)+1
    s.parent[s.first_index + (i-1)*s.strides[1] + (j-1)*s.strides[2]]
end

function getindex(s::SubArray, is::Integer...)
    index = s.first_index
    for i = 1:length(is)
        index += (is[i]-1)*s.strides[i]
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

# translate a linear index vector I for dim n to a linear index vector for
# the parent array
function translate_linear_indexes(s, n, I)
    idx = Array(Int, length(I))
    ssztail = size(s)[n:]
    psztail = size(s.parent)[n:]
    for j=1:length(I)
        su = ind2sub(ssztail,I[j])
        idx[j] = sub2ind(psztail, [ s.indexes[n+k-1][su[k]] for k=1:length(su) ]...)
    end
    idx
end

function getindex(s::SubArray, I::Union(Real,AbstractArray)...)
    I = indices(I)
    ndp = ndims(s.parent)
    n = length(I)
    newindexes = Array(Any, n)
    for i = 1:n
        t = s.indexes[i]
        #TODO: don't generate the dense vector indexes if they can be ranges
        if i==n && n < ndp
            newindexes[i] = translate_linear_indexes(s, i, I[i])
        else
            newindexes[i] = isa(t, Int) ? t : t[I[i]]
        end
    end

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
    ld = size(s,1)
    i = rem(ind-1,ld)+1
    j = div(ind-1,ld)+1
    s.parent[s.first_index + (i-1)*s.strides[1] + (j-1)*s.strides[2]] = v
    return s
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

function setindex!(s::SubArray, v, I::Union(Real,AbstractArray)...)
    I = indices(I)
    j = 1 #the jth dimension in subarray
    ndp = ndims(s.parent)
    n = length(I)
    newindexes = cell(n)
    for i = 1:n
        t = s.indexes[i]
        #TODO: don't generate the dense vector indexes if they can be ranges
        if i==n && n < ndp
            newindexes[i] = translate_linear_indexes(s, i, I[i])
        else
            newindexes[i] = isa(t, Int) ? t : t[I[j]]
        end
        j += 1
    end

    setindex!(s.parent, v, newindexes...)
end

function stride(s::SubArray, i::Integer)
    k = stride(s.parent, i)
    j = s.indexes[i]
    if isa(j,Range)
        return k*step(j)
    end
    return k
end

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

summary(s::SubArray) =
    string(dims2string(size(s)), " SubArray of ", summary(s.parent))

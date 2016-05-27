# This file is a part of Julia. License is MIT: http://julialang.org/license

## array.jl: Dense arrays

typealias Vector{T} Array{T,1}
typealias Matrix{T} Array{T,2}
typealias VecOrMat{T} Union{Vector{T}, Matrix{T}}

typealias DenseVector{T} DenseArray{T,1}
typealias DenseMatrix{T} DenseArray{T,2}
typealias DenseVecOrMat{T} Union{DenseVector{T}, DenseMatrix{T}}

## Basic functions ##

import Core: arraysize, arrayset, arrayref

size(a::Array, d) = arraysize(a, d)
size(a::Vector) = (arraysize(a,1),)
size(a::Matrix) = (arraysize(a,1), arraysize(a,2))
size(a::Array) = _size((), a)
_size{_,N}(out::NTuple{N}, A::Array{_,N}) = out
_size{_,M,N}(out::NTuple{M}, A::Array{_,N}) = _size((out..., size(A,M+1)), A)

asize_from(a::Array, n) = n > ndims(a) ? () : (arraysize(a,n), asize_from(a, n+1)...)

length(a::Array) = arraylen(a)
elsize{T}(a::Array{T}) = isbits(T) ? sizeof(T) : sizeof(Ptr)
sizeof(a::Array) = elsize(a) * length(a)

function isassigned{T}(a::Array{T}, i::Int...)
    ii = sub2ind(size(a), i...)
    1 <= ii <= length(a) || return false
    ccall(:jl_array_isassigned, Cint, (Any, UInt), a, ii-1) == 1
end

## copy ##

function unsafe_copy!{T}(dest::Ptr{T}, src::Ptr{T}, n)
    # Do not use this to copy data between pointer arrays.
    # It can't be made safe no matter how carefully you checked.
    ccall(:memmove, Ptr{Void}, (Ptr{Void}, Ptr{Void}, UInt),
          dest, src, n*sizeof(T))
    return dest
end

function unsafe_copy!{T}(dest::Array{T}, doffs, src::Array{T}, soffs, n)
    if isbits(T)
        unsafe_copy!(pointer(dest, doffs), pointer(src, soffs), n)
    else
        ccall(:jl_array_ptr_copy, Void, (Any, Ptr{Void}, Any, Ptr{Void}, Int),
              dest, pointer(dest, doffs), src, pointer(src, soffs), n)
    end
    return dest
end

function copy!{T}(dest::Array{T}, doffs::Integer, src::Array{T}, soffs::Integer, n::Integer)
    n == 0 && return dest
    n > 0 || throw(ArgumentError(string("tried to copy n=", n, " elements, but n should be nonnegative")))
    if soffs < 1 || doffs < 1 || soffs+n-1 > length(src) || doffs+n-1 > length(dest)
        throw(BoundsError())
    end
    unsafe_copy!(dest, doffs, src, soffs, n)
end

copy!{T}(dest::Array{T}, src::Array{T}) = copy!(dest, 1, src, 1, length(src))

copy{T<:Array}(a::T) = ccall(:jl_array_copy, Ref{T}, (Any,), a)

function reinterpret{T,S}(::Type{T}, a::Array{S,1})
    nel = Int(div(length(a)*sizeof(S),sizeof(T)))
    # TODO: maybe check that remainder is zero?
    return reinterpret(T, a, (nel,))
end

function reinterpret{T,S}(::Type{T}, a::Array{S})
    if sizeof(S) != sizeof(T)
        throw(ArgumentError("result shape not specified"))
    end
    reinterpret(T, a, size(a))
end

function reinterpret{T,S,N}(::Type{T}, a::Array{S}, dims::NTuple{N,Int})
    if !isbits(T)
        throw(ArgumentError("cannot reinterpret Array{$(S)} to ::Type{Array{$(T)}}, type $(T) is not a bitstype"))
    end
    if !isbits(S)
        throw(ArgumentError("cannot reinterpret Array{$(S)} to ::Type{Array{$(T)}}, type $(S) is not a bitstype"))
    end
    nel = div(length(a)*sizeof(S),sizeof(T))
    if prod(dims) != nel
        throw(DimensionMismatch("new dimensions $(dims) must be consistent with array size $(nel)"))
    end
    ccall(:jl_reshape_array, Array{T,N}, (Any, Any, Any), Array{T,N}, a, dims)
end

# reshaping to same # of dimensions
function reshape{T,N}(a::Array{T,N}, dims::NTuple{N,Int})
    if prod(dims) != length(a)
        throw(DimensionMismatch("new dimensions $(dims) must be consistent with array size $(length(a))"))
    end
    if dims == size(a)
        return a
    end
    ccall(:jl_reshape_array, Array{T,N}, (Any, Any, Any), Array{T,N}, a, dims)
end

# reshaping to different # of dimensions
function reshape{T,N}(a::Array{T}, dims::NTuple{N,Int})
    if prod(dims) != length(a)
        throw(DimensionMismatch("new dimensions $(dims) must be consistent with array size $(length(a))"))
    end
    ccall(:jl_reshape_array, Array{T,N}, (Any, Any, Any), Array{T,N}, a, dims)
end

## Constructors ##

similar(a::Array, T::Type, dims::Dims) = Array{T}(dims)
similar{T}(a::Array{T,1})              = Array{T}(size(a,1))
similar{T}(a::Array{T,2})              = Array{T}(size(a,1), size(a,2))
similar{T}(a::Array{T,1}, dims::Dims)  = Array{T}(dims)
similar{T}(a::Array{T,1}, m::Int)      = Array{T}(m)
similar{T}(a::Array{T,1}, S::Type)     = Array{S}(size(a,1))
similar{T}(a::Array{T,2}, dims::Dims)  = Array{T}(dims)
similar{T}(a::Array{T,2}, m::Int)      = Array{T}(m)
similar{T}(a::Array{T,2}, S::Type)     = Array{S}(size(a,1), size(a,2))

# T[x...] constructs Array{T,1}
function getindex(T::Type, vals...)
    a = Array{T}(length(vals))
    @inbounds for i = 1:length(vals)
        a[i] = vals[i]
    end
    return a
end

getindex(T::Type) = Array{T}(0)
getindex(T::Type, x) = (a = Array{T}(1); @inbounds a[1] = x; a)
getindex(T::Type, x, y) = (a = Array{T}(2); @inbounds (a[1] = x; a[2] = y); a)
getindex(T::Type, x, y, z) = (a = Array{T}(3); @inbounds (a[1] = x; a[2] = y; a[3] = z); a)

function getindex(::Type{Any}, vals::ANY...)
    a = Array{Any}(length(vals))
    @inbounds for i = 1:length(vals)
        a[i] = vals[i]
    end
    return a
end
getindex(::Type{Any}) = Array{Any}(0)

function fill!(a::Union{Array{UInt8}, Array{Int8}}, x::Integer)
    ccall(:memset, Ptr{Void}, (Ptr{Void}, Cint, Csize_t), a, x, length(a))
    return a
end

function fill!{T<:Union{Integer,AbstractFloat}}(a::Array{T}, x)
    xT = convert(T, x)
    for i in eachindex(a)
        @inbounds a[i] = xT
    end
    return a
end

fill(v, dims::Dims)       = fill!(Array{typeof(v)}(dims), v)
fill(v, dims::Integer...) = fill!(Array{typeof(v)}(dims...), v)

for (fname, felt) in ((:zeros,:zero), (:ones,:one))
    @eval begin
        ($fname)(T::Type, dims...)       = fill!(Array{T}(dims...), ($felt)(T))
        ($fname)(dims...)                = fill!(Array{Float64}(dims...), ($felt)(Float64))
        ($fname){T}(A::AbstractArray{T}) = fill!(similar(A), ($felt)(T))
    end
end

function eye(T::Type, m::Integer, n::Integer)
    a = zeros(T,m,n)
    for i = 1:min(m,n)
        a[i,i] = one(T)
    end
    return a
end
eye(m::Integer, n::Integer) = eye(Float64, m, n)
eye(T::Type, n::Integer) = eye(T, n, n)
eye(n::Integer) = eye(Float64, n)
eye{T}(x::AbstractMatrix{T}) = eye(T, size(x, 1), size(x, 2))

function one{T}(x::AbstractMatrix{T})
    m,n = size(x)
    m==n || throw(DimensionMismatch("multiplicative identity defined only for square matrices"))
    eye(T, m)
end

## Conversions ##

convert{T,n}(::Type{Array{T}}, x::Array{T,n}) = x
convert{T,n}(::Type{Array{T,n}}, x::Array{T,n}) = x

convert{T,n,S}(::Type{Array{T}}, x::AbstractArray{S, n}) = convert(Array{T, n}, x)
convert{T,n,S}(::Type{Array{T,n}}, x::AbstractArray{S,n}) = copy!(Array{T}(size(x)), x)

promote_rule{T,n,S}(::Type{Array{T,n}}, ::Type{Array{S,n}}) = Array{promote_type(T,S),n}

## copying iterators to containers

"""
    collect(element_type, collection)

Return an array of type `Array{element_type,1}` of all items in a collection.
"""
collect{T}(::Type{T}, itr) = _collect(T, itr, iteratorsize(itr))

_collect{T}(::Type{T}, itr, isz::HasLength) = copy!(Array{T,1}(Int(length(itr)::Integer)), itr)
_collect{T}(::Type{T}, itr, isz::HasShape)  = copy!(Array{T}(convert(Dims,size(itr))), itr)
function _collect{T}(::Type{T}, itr, isz::SizeUnknown)
    a = Array{T,1}(0)
    for x in itr
        push!(a,x)
    end
    return a
end

# make a collection similar to `c` and appropriate for collecting `itr`
_similar_for(c::AbstractArray, T, itr, ::SizeUnknown) = similar(c, T, 0)
_similar_for(c::AbstractArray, T, itr, ::HasLength) = similar(c, T, Int(length(itr)::Integer))
_similar_for(c::AbstractArray, T, itr, ::HasShape) = similar(c, T, convert(Dims,size(itr)))
_similar_for(c, T, itr, isz) = similar(c, T)

"""
    collect(collection)

Return an array of all items in a collection. For associative collections, returns Pair{KeyType, ValType}.
"""
collect(itr) = _collect(1:1 #= Array =#, itr, iteratoreltype(itr), iteratorsize(itr))

collect_similar(cont, itr) = _collect(cont, itr, iteratoreltype(itr), iteratorsize(itr))

_collect(cont, itr, ::HasEltype, isz::Union{HasLength,HasShape}) =
    copy!(_similar_for(cont, eltype(itr), itr, isz), itr)

function _collect(cont, itr, ::HasEltype, isz::SizeUnknown)
    a = _similar_for(cont, eltype(itr), itr, isz)
    for x in itr
        push!(a,x)
    end
    return a
end

if isdefined(Core, :Inference)
    function _default_eltype(itrt::ANY)
        rt = Core.Inference.return_type(first, Tuple{itrt})
        return isleaftype(rt) ? rt : Union{}
    end
else
    _default_eltype(itr::ANY) = Union{}
end
_default_eltype{I,T}(::Type{Generator{I,Type{T}}}) = T

_collect(c, itr, ::EltypeUnknown, isz::SizeUnknown) =
    grow_to!(_similar_for(c, _default_eltype(typeof(itr)), itr, isz), itr)

function _collect(c, itr, ::EltypeUnknown, isz::Union{HasLength,HasShape})
    st = start(itr)
    if done(itr,st)
        return _similar_for(c, _default_eltype(typeof(itr)), itr, isz)
    end
    v1, st = next(itr, st)
    collect_to_with_first!(_similar_for(c, typeof(v1), itr, isz), v1, itr, st)
end

function collect_to_with_first!(dest::AbstractArray, v1, itr, st)
    dest[1] = v1
    return collect_to!(dest, itr, 2, st)
end

function collect_to_with_first!(dest, v1, itr, st)
    push!(dest, v1)
    return grow_to!(dest, itr, st)
end

function collect_to!{T}(dest::AbstractArray{T}, itr, offs, st)
    # collect to dest array, checking the type of each result. if a result does not
    # match, widen the result type and re-dispatch.
    i = offs
    while !done(itr, st)
        el, st = next(itr, st)
        S = typeof(el)
        if S === T || S <: T
            @inbounds dest[i] = el::T
            i += 1
        else
            R = typejoin(T, S)
            new = similar(dest, R)
            copy!(new,1, dest,1, i-1)
            @inbounds new[i] = el
            return collect_to!(new, itr, i+1, st)
        end
    end
    return dest
end

function grow_to!(dest, itr, st = start(itr))
    T = eltype(dest)
    while !done(itr, st)
        el, st = next(itr, st)
        S = typeof(el)
        if S === T || S <: T
            push!(dest, el::T)
        else
            new = similar(dest, typejoin(T, S))
            copy!(new, dest)
            push!(new, el)
            return grow_to!(new, itr, st)
        end
    end
    return dest
end

## Iteration ##
start(A::Array) = 1
next(a::Array,i) = (@_propagate_inbounds_meta; (a[i],i+1))
done(a::Array,i) = (@_inline_meta; i == length(a)+1)

## Indexing: getindex ##

# This is more complicated than it needs to be in order to get Win64 through bootstrap
getindex(A::Array, i1::Real) = arrayref(A, to_index(i1))
getindex(A::Array, i1::Real, i2::Real, I::Real...) = arrayref(A, to_index(i1), to_index(i2), to_indexes(I...)...) # TODO: REMOVE FOR #14770

# Faster contiguous indexing using copy! for UnitRange and Colon
function getindex(A::Array, I::UnitRange{Int})
    @_inline_meta
    @boundscheck checkbounds(A, I)
    lI = length(I)
    X = similar(A, lI)
    if lI > 0
        unsafe_copy!(X, 1, A, first(I), lI)
    end
    return X
end
function getindex(A::Array, c::Colon)
    lI = length(A)
    X = similar(A, lI)
    if lI > 0
        unsafe_copy!(X, 1, A, 1, lI)
    end
    return X
end

# This is redundant with the abstract fallbacks, but needed for bootstrap
function getindex{S,T<:Real}(A::Array{S}, I::Range{T})
    return S[ A[to_index(i)] for i in I ]
end

## Indexing: setindex! ##
setindex!{T}(A::Array{T}, x, i1::Real) = arrayset(A, convert(T,x)::T, to_index(i1))
setindex!{T}(A::Array{T}, x, i1::Real, i2::Real, I::Real...) = arrayset(A, convert(T,x)::T, to_index(i1), to_index(i2), to_indexes(I...)...) # TODO: REMOVE FOR #14770

# These are redundant with the abstract fallbacks but needed for bootstrap
function setindex!(A::Array, x, I::AbstractVector{Int})
    is(A, I) && (I = copy(I))
    for i in I
        A[i] = x
    end
    return A
end
function setindex!(A::Array, X::AbstractArray, I::AbstractVector{Int})
    setindex_shape_check(X, length(I))
    count = 1
    if is(X,A)
        X = copy(X)
        is(I,A) && (I = X::typeof(I))
    elseif is(I,A)
        I = copy(I)
    end
    for i in I
        A[i] = X[count]
        count += 1
    end
    return A
end

# Faster contiguous setindex! with copy!
function setindex!{T}(A::Array{T}, X::Array{T}, I::UnitRange{Int})
    @_inline_meta
    @boundscheck checkbounds(A, I)
    lI = length(I)
    setindex_shape_check(X, lI)
    if lI > 0
        unsafe_copy!(A, first(I), X, 1, lI)
    end
    return A
end
function setindex!{T}(A::Array{T}, X::Array{T}, c::Colon)
    lI = length(A)
    setindex_shape_check(X, lI)
    if lI > 0
        unsafe_copy!(A, 1, X, 1, lI)
    end
    return A
end

setindex!(A::Array, x::Number, ::Colon) = fill!(A, x)
setindex!{T, N}(A::Array{T, N}, x::Number, ::Vararg{Colon, N}) = fill!(A, x)

# efficiently grow an array

_growat!(a::Vector, i::Integer, delta::Integer) =
    ccall(:jl_array_grow_at, Void, (Any, Int, UInt), a, i - 1, delta)

# efficiently delete part of an array

_deleteat!(a::Vector, i::Integer, delta::Integer) =
    ccall(:jl_array_del_at, Void, (Any, Int, UInt), a, i - 1, delta)

## Dequeue functionality ##

function push!{T}(a::Array{T,1}, item)
    # convert first so we don't grow the array if the assignment won't work
    itemT = convert(T, item)
    ccall(:jl_array_grow_end, Void, (Any, UInt), a, 1)
    a[end] = itemT
    return a
end

function push!(a::Array{Any,1}, item::ANY)
    ccall(:jl_array_grow_end, Void, (Any, UInt), a, 1)
    arrayset(a, item, length(a))
    return a
end

function append!{T}(a::Array{T,1}, items::AbstractVector)
    n = length(items)
    ccall(:jl_array_grow_end, Void, (Any, UInt), a, n)
    copy!(a, length(a)-n+1, items, 1, n)
    return a
end

function prepend!{T}(a::Array{T,1}, items::AbstractVector)
    n = length(items)
    ccall(:jl_array_grow_beg, Void, (Any, UInt), a, n)
    if a === items
        copy!(a, 1, items, n+1, n)
    else
        copy!(a, 1, items, 1, n)
    end
    return a
end

function resize!(a::Vector, nl::Integer)
    l = length(a)
    if nl > l
        ccall(:jl_array_grow_end, Void, (Any, UInt), a, nl-l)
    else
        if nl < 0
            throw(ArgumentError("new length must be ≥ 0"))
        end
        ccall(:jl_array_del_end, Void, (Any, UInt), a, l-nl)
    end
    return a
end

function sizehint!(a::Vector, sz::Integer)
    ccall(:jl_array_sizehint, Void, (Any, UInt), a, sz)
    a
end

function pop!(a::Vector)
    if isempty(a)
        throw(ArgumentError("array must be non-empty"))
    end
    item = a[end]
    ccall(:jl_array_del_end, Void, (Any, UInt), a, 1)
    return item
end

function unshift!{T}(a::Array{T,1}, item)
    item = convert(T, item)
    ccall(:jl_array_grow_beg, Void, (Any, UInt), a, 1)
    a[1] = item
    return a
end

function shift!(a::Vector)
    if isempty(a)
        throw(ArgumentError("array must be non-empty"))
    end
    item = a[1]
    ccall(:jl_array_del_beg, Void, (Any, UInt), a, 1)
    return item
end

function insert!{T}(a::Array{T,1}, i::Integer, item)
    # Throw convert error before changing the shape of the array
    _item = convert(T, item)
    _growat!(a, i, 1)
    # _growat! already did bound check
    @inbounds a[i] = _item
    return a
end

deleteat!(a::Vector, i::Integer) = (_deleteat!(a, i, 1); a)

function deleteat!{T<:Integer}(a::Vector, r::UnitRange{T})
    n = length(a)
    isempty(r) || _deleteat!(a, first(r), length(r))
    return a
end

function deleteat!(a::Vector, inds)
    n = length(a)
    s = start(inds)
    done(inds, s) && return a
    (p, s) = next(inds, s)
    q = p+1
    while !done(inds, s)
        (i,s) = next(inds, s)
        if !(q <= i <= n)
            if i < q
                throw(ArgumentError("indices must be unique and sorted"))
            else
                throw(BoundsError())
            end
        end
        while q < i
            @inbounds a[p] = a[q]
            p += 1; q += 1
        end
        q = i+1
    end
    while q <= n
        @inbounds a[p] = a[q]
        p += 1; q += 1
    end
    ccall(:jl_array_del_end, Void, (Any, UInt), a, n-p+1)
    return a
end

const _default_splice = []

function splice!(a::Vector, i::Integer, ins=_default_splice)
    v = a[i]
    m = length(ins)
    if m == 0
        _deleteat!(a, i, 1)
    elseif m == 1
        a[i] = ins[1]
    else
        _growat!(a, i, m-1)
        k = 1
        for x in ins
            a[i+k-1] = x
            k += 1
        end
    end
    return v
end

function splice!{T<:Integer}(a::Vector, r::UnitRange{T}, ins=_default_splice)
    v = a[r]
    m = length(ins)
    if m == 0
        deleteat!(a, r)
        return v
    end

    n = length(a)
    f = first(r)
    l = last(r)
    d = length(r)

    if m < d
        delta = d - m
        _deleteat!(a, (f - 1 < n - l) ? f : (l - delta + 1), delta)
    elseif m > d
        _growat!(a, (f - 1 < n - l) ? f : (l + 1), m - d)
    end

    k = 1
    for x in ins
        a[f+k-1] = x
        k += 1
    end
    return v
end

function empty!(a::Vector)
    ccall(:jl_array_del_end, Void, (Any, UInt), a, length(a))
    return a
end

# use memcmp for lexcmp on byte arrays
function lexcmp(a::Array{UInt8,1}, b::Array{UInt8,1})
    c = ccall(:memcmp, Int32, (Ptr{UInt8}, Ptr{UInt8}, UInt),
              a, b, min(length(a),length(b)))
    return c < 0 ? -1 : c > 0 ? +1 : cmp(length(a),length(b))
end

function reverse(A::AbstractVector, s=1, n=length(A))
    B = similar(A)
    for i = 1:s-1
        B[i] = A[i]
    end
    for i = s:n
        B[i] = A[n+s-i]
    end
    for i = n+1:length(A)
        B[i] = A[i]
    end
    return B
end
reverseind(a::AbstractVector, i::Integer) = length(a) + 1 - i

function reverse!(v::AbstractVector, s=1, n=length(v))
    if n <= s  # empty case; ok
    elseif !(1 ≤ s ≤ endof(v))
        throw(BoundsError(v, s))
    elseif !(1 ≤ n ≤ endof(v))
        throw(BoundsError(v, n))
    end
    r = n
    @inbounds for i in s:div(s+n-1, 2)
        v[i], v[r] = v[r], v[i]
        r -= 1
    end
    return v
end

function vcat{T}(arrays::Vector{T}...)
    n = 0
    for a in arrays
        n += length(a)
    end
    arr = Array{T}(n)
    ptr = pointer(arr)
    if isbits(T)
        elsz = Core.sizeof(T)
    else
        elsz = Core.sizeof(Ptr{Void})
    end
    for a in arrays
        na = length(a)
        nba = na * elsz
        if isbits(T)
            ccall(:memcpy, Ptr{Void}, (Ptr{Void}, Ptr{Void}, UInt),
                  ptr, a, nba)
        else
            ccall(:jl_array_ptr_copy, Void, (Any, Ptr{Void}, Any, Ptr{Void}, Int),
                  arr, ptr, a, pointer(a), na)
        end
        ptr += nba
    end
    return arr
end

function hcat{T}(V::Vector{T}...)
    height = length(V[1])
    for j = 2:length(V)
        if length(V[j]) != height
            throw(DimensionMismatch("vectors must have same lengths"))
        end
    end
    return [ V[j][i]::T for i=1:length(V[1]), j=1:length(V) ]
end

hcat(A::Matrix...) = typed_hcat(promote_eltype(A...), A...)
hcat{T}(A::Matrix{T}...) = typed_hcat(T, A...)

vcat(A::Matrix...) = typed_vcat(promote_eltype(A...), A...)
vcat{T}(A::Matrix{T}...) = typed_vcat(T, A...)

hcat(A::Union{Matrix, Vector}...) = typed_hcat(promote_eltype(A...), A...)
hcat{T}(A::Union{Matrix{T}, Vector{T}}...) = typed_hcat(T, A...)


vcat(A::Union{Matrix, Vector}...) = typed_vcat(promote_eltype(A...), A...)
vcat{T}(A::Union{Matrix{T}, Vector{T}}...) = typed_vcat(T, A...)

hvcat(rows::Tuple{Vararg{Int}}, xs::Vector...) = typed_hvcat(promote_eltype(xs...), rows, xs...)
hvcat{T}(rows::Tuple{Vararg{Int}}, xs::Vector{T}...) = typed_hvcat(T, rows, xs...)

hvcat(rows::Tuple{Vararg{Int}}, xs::Matrix...) = typed_hvcat(promote_eltype(xs...), rows, xs...)
hvcat{T}(rows::Tuple{Vararg{Int}}, xs::Matrix{T}...) = typed_hvcat(T, rows, xs...)

## find ##

# returns the index of the next non-zero element, or 0 if all zeros
function findnext(A, start::Integer)
    for i = start:length(A)
        if A[i] != 0
            return i
        end
    end
    return 0
end
findfirst(A) = findnext(A, 1)

# returns the index of the next matching element
function findnext(A, v, start::Integer)
    for i = start:length(A)
        if A[i] == v
            return i
        end
    end
    return 0
end
findfirst(A, v) = findnext(A, v, 1)

# returns the index of the next element for which the function returns true
function findnext(testf::Function, A, start::Integer)
    for i = start:length(A)
        if testf(A[i])
            return i
        end
    end
    return 0
end
findfirst(testf::Function, A) = findnext(testf, A, 1)

# returns the index of the previous non-zero element, or 0 if all zeros
function findprev(A, start::Integer)
    for i = start:-1:1
        A[i] != 0 && return i
    end
    return 0
end
findlast(A) = findprev(A, length(A))

# returns the index of the matching element, or 0 if no matching
function findprev(A, v, start::Integer)
    for i = start:-1:1
        A[i] == v && return i
    end
    return 0
end
findlast(A, v) = findprev(A, v, length(A))

# returns the index of the previous element for which the function returns true, or zero if it never does
function findprev(testf::Function, A, start::Integer)
    for i = start:-1:1
        testf(A[i]) && return i
    end
    return 0
end
findlast(testf::Function, A) = findprev(testf, A, length(A))

function find(testf::Function, A)
    # use a dynamic-length array to store the indexes, then copy to a non-padded
    # array for the return
    tmpI = Array{Int}(0)
    for (i,a) = enumerate(A)
        if testf(a)
            push!(tmpI, i)
        end
    end
    I = Array{Int}(length(tmpI))
    copy!(I, tmpI)
    return I
end

function find(A)
    nnzA = countnz(A)
    I = Vector{Int}(nnzA)
    count = 1
    for (i,a) in enumerate(A)
        if a != 0
            I[count] = i
            count += 1
        end
    end
    return I
end

find(x::Number) = x == 0 ? Array{Int}(0) : [1]
find(testf::Function, x::Number) = !testf(x) ? Array{Int}(0) : [1]

findn(A::AbstractVector) = find(A)

function findn(A::AbstractMatrix)
    nnzA = countnz(A)
    I = similar(A, Int, nnzA)
    J = similar(A, Int, nnzA)
    count = 1
    for j=1:size(A,2), i=1:size(A,1)
        if A[i,j] != 0
            I[count] = i
            J[count] = j
            count += 1
        end
    end
    return (I, J)
end

function findnz{T}(A::AbstractMatrix{T})
    nnzA = countnz(A)
    I = zeros(Int, nnzA)
    J = zeros(Int, nnzA)
    NZs = Array{T}(nnzA)
    count = 1
    if nnzA > 0
        for j=1:size(A,2), i=1:size(A,1)
            Aij = A[i,j]
            if Aij != 0
                I[count] = i
                J[count] = j
                NZs[count] = Aij
                count += 1
            end
        end
    end
    return (I, J, NZs)
end

function findmax(a)
    if isempty(a)
        throw(ArgumentError("collection must be non-empty"))
    end
    s = start(a)
    mi = i = 1
    m, s = next(a, s)
    while !done(a, s)
        ai, s = next(a, s)
        i += 1
        if ai > m || m!=m
            m = ai
            mi = i
        end
    end
    return (m, mi)
end

function findmin(a)
    if isempty(a)
        throw(ArgumentError("collection must be non-empty"))
    end
    s = start(a)
    mi = i = 1
    m, s = next(a, s)
    while !done(a, s)
        ai, s = next(a, s)
        i += 1
        if ai < m || m!=m
            m = ai
            mi = i
        end
    end
    return (m, mi)
end

indmax(a) = findmax(a)[2]
indmin(a) = findmin(a)[2]

# similar to Matlab's ismember
# returns a vector containing the highest index in b for each value in a that is a member of b
function indexin(a::AbstractArray, b::AbstractArray)
    bdict = Dict(zip(b, 1:length(b)))
    [get(bdict, i, 0) for i in a]
end

function findin(a, b)
    ind = Array{Int}(0)
    bset = Set(b)
    @inbounds for (i,ai) in enumerate(a)
        ai in bset && push!(ind, i)
    end
    ind
end

# Copying subregions
# TODO: DEPRECATE FOR #14770
function indcopy(sz::Dims, I::Vector)
    n = length(I)
    s = sz[n]
    for i = n+1:length(sz)
        s *= sz[i]
    end
    dst = eltype(I)[findin(I[i], i < n ? (1:sz[i]) : (1:s)) for i = 1:n]
    src = eltype(I)[I[i][findin(I[i], i < n ? (1:sz[i]) : (1:s))] for i = 1:n]
    dst, src
end

function indcopy(sz::Dims, I::Tuple{Vararg{RangeIndex}})
    n = length(I)
    s = sz[n]
    for i = n+1:length(sz)
        s *= sz[i]
    end
    dst::typeof(I) = ntuple(i-> findin(I[i], i < n ? (1:sz[i]) : (1:s)), n)::typeof(I)
    src::typeof(I) = ntuple(i-> I[i][findin(I[i], i < n ? (1:sz[i]) : (1:s))], n)::typeof(I)
    dst, src
end

## Filter ##

# given a function returning a boolean and an array, return matching elements
filter(f, As::AbstractArray) = As[map(f, As)::AbstractArray{Bool}]

function filter!(f, a::Vector)
    insrt = 1
    for acurr in a
        if f(acurr)
            a[insrt] = acurr
            insrt += 1
        end
    end
    deleteat!(a, insrt:length(a))
    return a
end

function filter(f, a::Vector)
    r = Array{eltype(a)}(0)
    for ai in a
        if f(ai)
            push!(r, ai)
        end
    end
    return r
end

# set-like operators for vectors
# These are moderately efficient, preserve order, and remove dupes.

function intersect(v1, vs...)
    ret = Array{eltype(v1)}(0)
    for v_elem in v1
        inall = true
        for vsi in vs
            if !in(v_elem, vsi)
                inall=false; break
            end
        end
        if inall
            push!(ret, v_elem)
        end
    end
    ret
end

function union(vs...)
    ret = Array{promote_eltype(vs...)}(0)
    seen = Set()
    for v in vs
        for v_elem in v
            if !in(v_elem, seen)
                push!(ret, v_elem)
                push!(seen, v_elem)
            end
        end
    end
    ret
end
# setdiff only accepts two args
function setdiff(a, b)
    args_type = promote_type(eltype(a), eltype(b))
    bset = Set(b)
    ret = Array{args_type}(0)
    seen = Set{eltype(a)}()
    for a_elem in a
        if !in(a_elem, seen) && !in(a_elem, bset)
            push!(ret, a_elem)
            push!(seen, a_elem)
        end
    end
    ret
end
# symdiff is associative, so a relatively clean
# way to implement this is by using setdiff and union, and
# recursing. Has the advantage of keeping order, too, but
# not as fast as other methods that make a single pass and
# store counts with a Dict.
symdiff(a) = a
symdiff(a, b) = union(setdiff(a,b), setdiff(b,a))
symdiff(a, b, rest...) = symdiff(a, symdiff(b, rest...))

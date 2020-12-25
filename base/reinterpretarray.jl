# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Gives a reinterpreted view (of element type T) of the underlying array (of element type S).
If the size of `T` differs from the size of `S`, the array will be compressed/expanded in
the first dimension. The variant `reinterpret(reshape, T, a)` instead adds or consumes the first dimension
depending on the ratio of element sizes.
"""
struct ReinterpretArray{T,N,S,A<:AbstractArray{S},IsReshaped} <: AbstractArray{T, N}
    parent::A
    readable::Bool
    writable::Bool

    function throwbits(S::Type, T::Type, U::Type)
        @_noinline_meta
        throw(ArgumentError("cannot reinterpret `$(S)` as `$(T)`, type `$(U)` is not a bits type"))
    end
    function throwsize0(S::Type, T::Type, msg)
        @_noinline_meta
        throw(ArgumentError("cannot reinterpret a zero-dimensional `$(S)` array to `$(T)` which is of a $msg size"))
    end

    global reinterpret
    function reinterpret(::Type{T}, a::A) where {T,N,S,A<:AbstractArray{S, N}}
        function thrownonint(S::Type, T::Type, dim)
            @_noinline_meta
            throw(ArgumentError("""
                cannot reinterpret an `$(S)` array to `$(T)` whose first dimension has size `$(dim)`.
                The resulting array would have non-integral first dimension.
                """))
        end
        function throwaxes1(S::Type, T::Type, ax1)
            @_noinline_meta
            throw(ArgumentError("cannot reinterpret a `$(S)` array to `$(T)` when the first axis is $ax1. Try reshaping first."))
        end
        isbitstype(T) || throwbits(S, T, T)
        isbitstype(S) || throwbits(S, T, S)
        (N != 0 || sizeof(T) == sizeof(S)) || throwsize0(S, T, "different")
        if N != 0 && sizeof(S) != sizeof(T)
            ax1 = axes(a)[1]
            dim = length(ax1)
            rem(dim*sizeof(S),sizeof(T)) == 0 || thrownonint(S, T, dim)
            first(ax1) == 1 || throwaxes1(S, T, ax1)
        end
        readable = array_subpadding(T, S)
        writable = array_subpadding(S, T)
        new{T, N, S, A, false}(a, readable, writable)
    end
    reinterpret(::Type{T}, a::AbstractArray{T}) where {T} = a

    # With reshaping
    function reinterpret(::typeof(reshape), ::Type{T}, a::A) where {T,S,A<:AbstractArray{S}}
        function throwintmult(S::Type, T::Type)
            @_noinline_meta
            throw(ArgumentError("`reinterpret(reshape, T, a)` requires that one of `sizeof(T)` (got $(sizeof(T))) and `sizeof(eltype(a))` (got $(sizeof(S))) be an integer multiple of the other"))
        end
        function throwsize1(a::AbstractArray, T::Type)
            @_noinline_meta
            throw(ArgumentError("`reinterpret(reshape, $T, a)` where `eltype(a)` is $(eltype(a)) requires that `axes(a, 1)` (got $(axes(a, 1))) be equal to 1:$(sizeof(T) ÷ sizeof(eltype(a))) (from the ratio of element sizes)"))
        end
        isbitstype(T) || throwbits(S, T, T)
        isbitstype(S) || throwbits(S, T, S)
        if sizeof(S) == sizeof(T)
            N = ndims(a)
        elseif sizeof(S) > sizeof(T)
            rem(sizeof(S), sizeof(T)) == 0 || throwintmult(S, T)
            N = ndims(a) + 1
        else
            rem(sizeof(T), sizeof(S)) == 0 || throwintmult(S, T)
            N = ndims(a) - 1
            N > -1 || throwsize0(S, T, "larger")
            axes(a, 1) == Base.OneTo(sizeof(T) ÷ sizeof(S)) || throwsize1(a, T)
        end
        readable = array_subpadding(T, S)
        writable = array_subpadding(S, T)
        new{T, N, S, A, true}(a, readable, writable)
    end
    reinterpret(::typeof(reshape), ::Type{T}, a::AbstractArray{T}) where {T} = a
end

ReshapedReinterpretArray{T,N,S,A<:AbstractArray{S}} = ReinterpretArray{T,N,S,A,true}
NonReshapedReinterpretArray{T,N,S,A<:AbstractArray{S, N}} = ReinterpretArray{T,N,S,A,false}

"""
    reinterpret(reshape, T, A::AbstractArray{S}) -> B

Change the type-interpretation of `A` while consuming or adding a "channel dimension."

If `sizeof(T) = n*sizeof(S)` for `n>1`, `A`'s first dimension must be
of size `n` and `B` lacks `A`'s first dimension. Conversely, if `sizeof(S) = n*sizeof(T)` for `n>1`,
`B` gets a new first dimension of size `n`. The dimensionality is unchanged if `sizeof(T) == sizeof(S)`.

# Examples

```jldoctest
julia> A = [1 2; 3 4]
2×2 Matrix{$Int}:
 1  2
 3  4

julia> reinterpret(reshape, Complex{Int}, A)    # the result is a vector
2-element reinterpret(reshape, Complex{$Int}, ::Matrix{$Int}) with eltype Complex{$Int}:
 1 + 3im
 2 + 4im

julia> a = [(1,2,3), (4,5,6)]
2-element Vector{Tuple{$Int, $Int, $Int}}:
 (1, 2, 3)
 (4, 5, 6)

julia> reinterpret(reshape, Int, a)             # the result is a matrix
3×2 reinterpret(reshape, $Int, ::Vector{Tuple{$Int, $Int, $Int}}) with eltype $Int:
 1  4
 2  5
 3  6
```
"""
reinterpret(::typeof(reshape), T::Type, a::AbstractArray)

reinterpret(::Type{T}, a::NonReshapedReinterpretArray) where {T} = reinterpret(T, a.parent)
reinterpret(::typeof(reshape), ::Type{T}, a::ReshapedReinterpretArray) where {T} = reinterpret(reshape, T, a.parent)

# Definition of StridedArray
StridedFastContiguousSubArray{T,N,A<:DenseArray} = FastContiguousSubArray{T,N,A}
StridedReinterpretArray{T,N,A<:Union{DenseArray,StridedFastContiguousSubArray},IsReshaped} = ReinterpretArray{T,N,S,A,IsReshaped} where S
StridedReshapedArray{T,N,A<:Union{DenseArray,StridedFastContiguousSubArray,StridedReinterpretArray}} = ReshapedArray{T,N,A}
StridedSubArray{T,N,A<:Union{DenseArray,StridedReshapedArray,StridedReinterpretArray},
    I<:Tuple{Vararg{Union{RangeIndex, ReshapedUnitRange, AbstractCartesianIndex}}}} = SubArray{T,N,A,I}
StridedArray{T,N} = Union{DenseArray{T,N}, StridedSubArray{T,N}, StridedReshapedArray{T,N}, StridedReinterpretArray{T,N}}
StridedVector{T} = StridedArray{T,1}
StridedMatrix{T} = StridedArray{T,2}
StridedVecOrMat{T} = Union{StridedVector{T}, StridedMatrix{T}}

# the definition of strides for Array{T,N} is tuple() if N = 0, otherwise it is
# a tuple containing 1 and a cumulative product of the first N-1 sizes
# this definition is also used for StridedReshapedArray and StridedReinterpretedArray
# which have the same memory storage as Array
stride(a::Union{DenseArray,StridedReshapedArray,StridedReinterpretArray}, i::Int) = _stride(a, i)

function stride(a::ReinterpretArray, i::Int)
    a.parent isa StridedArray || ArgumentError("Parent must be strided.") |> throw
    return _stride(a, i)
end

function _stride(a, i)
    if i > ndims(a)
        return length(a)
    end
    s = 1
    for n = 1:(i-1)
        s *= size(a, n)
    end
    return s
end

function strides(a::ReinterpretArray)
    a.parent isa StridedArray || ArgumentError("Parent must be strided.") |> throw
    size_to_strides(1, size(a)...)
end
strides(a::Union{DenseArray,StridedReshapedArray,StridedReinterpretArray}) = size_to_strides(1, size(a)...)

similar(a::ReinterpretArray, T::Type, d::Dims) = similar(a.parent, T, d)

function check_readable(a::ReinterpretArray{T, N, S} where N) where {T,S}
    # See comment in check_writable
    if !a.readable && !array_subpadding(T, S)
        throw(PaddingError(T, S))
    end
end

function check_writable(a::ReinterpretArray{T, N, S} where N) where {T,S}
    # `array_subpadding` is relatively expensive (compared to a simple arrayref),
    # so it is cached in the array. However, it is computable at compile time if,
    # inference has the types available. By using this form of the check, we can
    # get the best of both worlds for the success case. If the types were not
    # available to inference, we simply need to check the field (relatively cheap)
    # and if they were we should be able to fold this check away entirely.
    if !a.writable && !array_subpadding(S, T)
        throw(PaddingError(T, S))
    end
end

## IndexStyle specializations

# For `reinterpret(reshape, T, a)` where we're adding a channel dimension and with
# `IndexStyle(a) == IndexLinear()`, it's advantageous to retain pseudo-linear indexing.
struct IndexSCartesian2{K} <: IndexStyle end   # K = sizeof(S) ÷ sizeof(T), a static-sized 2d cartesian iterator

IndexStyle(::Type{ReinterpretArray{T,N,S,A,false}}) where {T,N,S,A<:AbstractArray{S,N}} = IndexStyle(A)
function IndexStyle(::Type{ReinterpretArray{T,N,S,A,true}}) where {T,N,S,A<:AbstractArray{S}}
    if sizeof(T) < sizeof(S)
        IndexStyle(A) === IndexLinear() && return IndexSCartesian2{sizeof(S) ÷ sizeof(T)}()
        return IndexCartesian()
    end
    return IndexStyle(A)
end
IndexStyle(::IndexSCartesian2{K}, ::IndexSCartesian2{K}) where {K} = IndexSCartesian2{K}()

struct SCartesianIndex2{K}   # can't make <:AbstractCartesianIndex without N, and 2 would be a bit misleading
    i::Int
    j::Int
end
to_index(i::SCartesianIndex2) = i

struct SCartesianIndices2{K,R<:AbstractUnitRange{Int}} <: AbstractMatrix{SCartesianIndex2{K}}
    indices2::R
end
SCartesianIndices2{K}(indices2::AbstractUnitRange{Int}) where {K} = (@assert K::Int > 1; SCartesianIndices2{K,typeof(indices2)}(indices2))

eachindex(::IndexSCartesian2{K}, A::ReshapedReinterpretArray) where {K} = SCartesianIndices2{K}(eachindex(IndexLinear(), parent(A)))
@inline function eachindex(style::IndexSCartesian2{K}, A::AbstractArray, B::AbstractArray...) where {K}
    iter = eachindex(style, A)
    Base._all_match_first(C->eachindex(style, C), iter, B...) || Base.throw_eachindex_mismatch_indices(IndexSCartesian2{K}(), axes(A), axes.(B)...)
    return iter
end

size(iter::SCartesianIndices2{K}) where K = (K, length(iter.indices2))
axes(iter::SCartesianIndices2{K}) where K = (Base.OneTo(K), iter.indices2)

first(iter::SCartesianIndices2{K}) where {K} = SCartesianIndex2{K}(1, first(iter.indices2))
last(iter::SCartesianIndices2{K}) where {K}  = SCartesianIndex2{K}(K, last(iter.indices2))

@inline function getindex(iter::SCartesianIndices2{K}, i::Int, j::Int) where {K}
    @boundscheck checkbounds(iter, i, j)
    return SCartesianIndex2{K}(i, iter.indices2[j])
end

function iterate(iter::SCartesianIndices2{K}) where {K}
    ret = iterate(iter.indices2)
    ret === nothing && return nothing
    item2, state2 = ret
    return SCartesianIndex2{K}(1, item2), (1, item2, state2)
end

function iterate(iter::SCartesianIndices2{K}, (state1, item2, state2)) where {K}
    if state1 < K
        item1 = state1 + 1
        return SCartesianIndex2{K}(item1, item2), (item1, item2, state2)
    end
    ret = iterate(iter.indices2, state2)
    ret === nothing && return nothing
    item2, state2 = ret
    return SCartesianIndex2{K}(1, item2), (1, item2, state2)
end

SimdLoop.simd_outer_range(iter::SCartesianIndices2) = iter.indices2
SimdLoop.simd_inner_length(::SCartesianIndices2{K}, ::Any) where K = K
@inline function SimdLoop.simd_index(::SCartesianIndices2{K}, Ilast::Int, I1::Int) where {K}
    SCartesianIndex2{K}(I1+1, Ilast)
end

_maybe_reshape(::IndexSCartesian2, A::ReshapedReinterpretArray, I...) = A

# fallbacks
function _getindex(::IndexSCartesian2, A::AbstractArray{T,N}, I::Vararg{Int, N}) where {T,N}
    @_propagate_inbounds_meta
    getindex(A, I...)
end
function _setindex!(::IndexSCartesian2, A::AbstractArray{T,N}, v, I::Vararg{Int, N}) where {T,N}
    @_propagate_inbounds_meta
    setindex!(A, v, I...)
end
# fallbacks for array types that use "pass-through" indexing (e.g., `IndexStyle(A) = IndexStyle(parent(A))`)
# but which don't handle SCartesianIndex2
function _getindex(::IndexSCartesian2, A::AbstractArray{T,N}, ind::SCartesianIndex2) where {T,N}
    @_propagate_inbounds_meta
    J = _ind2sub(tail(axes(A)), ind.j)
    getindex(A, ind.i, J...)
end
function _setindex!(::IndexSCartesian2, A::AbstractArray{T,N}, v, ind::SCartesianIndex2) where {T,N}
    @_propagate_inbounds_meta
    J = _ind2sub(tail(axes(A)), ind.j)
    setindex!(A, v, ind.i, J...)
end
eachindex(style::IndexSCartesian2, A::AbstractArray) = eachindex(style, parent(A))

## AbstractArray interface

parent(a::ReinterpretArray) = a.parent
dataids(a::ReinterpretArray) = dataids(a.parent)
unaliascopy(a::ReinterpretArray{T}) where {T} = reinterpret(T, unaliascopy(a.parent))

function size(a::NonReshapedReinterpretArray{T,N,S} where {N}) where {T,S}
    psize = size(a.parent)
    size1 = div(psize[1]*sizeof(S), sizeof(T))
    tuple(size1, tail(psize)...)
end
function size(a::ReshapedReinterpretArray{T,N,S} where {N}) where {T,S}
    psize = size(a.parent)
    sizeof(S) > sizeof(T) && return (div(sizeof(S), sizeof(T)), psize...)
    sizeof(S) < sizeof(T) && return Base.tail(psize)
    return psize
end
size(a::NonReshapedReinterpretArray{T,0}) where {T} = ()

function axes(a::NonReshapedReinterpretArray{T,N,S} where {N}) where {T,S}
    paxs = axes(a.parent)
    f, l = first(paxs[1]), length(paxs[1])
    size1 = div(l*sizeof(S), sizeof(T))
    tuple(oftype(paxs[1], f:f+size1-1), tail(paxs)...)
end
function axes(a::ReshapedReinterpretArray{T,N,S} where {N}) where {T,S}
    paxs = axes(a.parent)
    sizeof(S) > sizeof(T) && return (Base.OneTo(div(sizeof(S), sizeof(T))), paxs...)
    sizeof(S) < sizeof(T) && return Base.tail(paxs)
    return paxs
end
axes(a::NonReshapedReinterpretArray{T,0}) where {T} = ()

elsize(::Type{<:ReinterpretArray{T}}) where {T} = sizeof(T)
unsafe_convert(::Type{Ptr{T}}, a::ReinterpretArray{T,N,S} where N) where {T,S} = Ptr{T}(unsafe_convert(Ptr{S},a.parent))

@inline @propagate_inbounds getindex(a::NonReshapedReinterpretArray{T,0}) where {T} = reinterpret(T, a.parent[])
@inline @propagate_inbounds getindex(a::ReinterpretArray) = a[1]

@inline @propagate_inbounds function getindex(a::ReinterpretArray{T,N,S}, inds::Vararg{Int, N}) where {T,N,S}
    check_readable(a)
    _getindex_ra(a, inds[1], tail(inds))
end

@inline @propagate_inbounds function getindex(a::ReinterpretArray{T,N,S}, i::Int) where {T,N,S}
    check_readable(a)
    if isa(IndexStyle(a), IndexLinear)
        return _getindex_ra(a, i, ())
    end
    # Convert to full indices here, to avoid needing multiple conversions in
    # the loop in _getindex_ra
    inds = _to_subscript_indices(a, i)
    isempty(inds) ? _getindex_ra(a, 1, ()) : _getindex_ra(a, inds[1], tail(inds))
end

@inline @propagate_inbounds function getindex(a::ReshapedReinterpretArray{T,N,S}, ind::SCartesianIndex2) where {T,N,S}
    check_readable(a)
    n = sizeof(S) ÷ sizeof(T)
    t = Ref{NTuple{n,T}}()
    s = Ref{S}(a.parent[ind.j])
    GC.@preserve t s begin
        tptr = Ptr{UInt8}(unsafe_convert(Ref{T}, t))
        sptr = Ptr{UInt8}(unsafe_convert(Ref{S}, s))
        _memcpy!(tptr, sptr, sizeof(S))
    end
    return t[][ind.i]
end

@inline _memcpy!(dst, src, n) = ccall(:memcpy, Cvoid, (Ptr{UInt8}, Ptr{UInt8}, Csize_t), dst, src, n)

@inline @propagate_inbounds function _getindex_ra(a::NonReshapedReinterpretArray{T,N,S}, i1::Int, tailinds::TT) where {T,N,S,TT}
    # Make sure to match the scalar reinterpret if that is applicable
    if sizeof(T) == sizeof(S) && (fieldcount(T) + fieldcount(S)) == 0
        return reinterpret(T, a.parent[i1, tailinds...])
    else
        @boundscheck checkbounds(a, i1, tailinds...)
        ind_start, sidx = divrem((i1-1)*sizeof(T), sizeof(S))
        t = Ref{T}()
        s = Ref{S}()
        GC.@preserve t s begin
            tptr = Ptr{UInt8}(unsafe_convert(Ref{T}, t))
            sptr = Ptr{UInt8}(unsafe_convert(Ref{S}, s))
            # Optimizations that avoid branches
            if sizeof(T) % sizeof(S) == 0
                # T is bigger than S and contains an integer number of them
                n = sizeof(T) ÷ sizeof(S)
                for i = 1:n
                    s[] = a.parent[ind_start + i, tailinds...]
                    _memcpy!(tptr + (i-1)*sizeof(S), sptr, sizeof(S))
                end
            elseif sizeof(S) % sizeof(T) == 0
                # S is bigger than T and contains an integer number of them
                s[] = a.parent[ind_start + 1, tailinds...]
                _memcpy!(tptr, sptr + sidx, sizeof(T))
            else
                i = 1
                nbytes_copied = 0
                # This is a bit complicated to deal with partial elements
                # at both the start and the end. LLVM will fold as appropriate,
                # once it knows the data layout
                while nbytes_copied < sizeof(T)
                    s[] = a.parent[ind_start + i, tailinds...]
                    nb = min(sizeof(S) - sidx, sizeof(T)-nbytes_copied)
                    _memcpy!(tptr + nbytes_copied, sptr + sidx, nb)
                    nbytes_copied += nb
                    sidx = 0
                    i += 1
                end
            end
        end
        return t[]
    end
end

@inline @propagate_inbounds function _getindex_ra(a::ReshapedReinterpretArray{T,N,S}, i1::Int, tailinds::TT) where {T,N,S,TT}
    # Make sure to match the scalar reinterpret if that is applicable
    if sizeof(T) == sizeof(S) && (fieldcount(T) + fieldcount(S)) == 0
        return reinterpret(T, a.parent[i1, tailinds...])
    end
    @boundscheck checkbounds(a, i1, tailinds...)
    if sizeof(T) >= sizeof(S)
        t = Ref{T}()
        s = Ref{S}()
        GC.@preserve t s begin
            tptr = Ptr{UInt8}(unsafe_convert(Ref{T}, t))
            sptr = Ptr{UInt8}(unsafe_convert(Ref{S}, s))
            if sizeof(T) > sizeof(S)
                # Extra dimension in the parent array
                n = sizeof(T) ÷ sizeof(S)
                if isempty(tailinds) && IndexStyle(a.parent) === IndexLinear()
                    offset = n * (i1 - firstindex(a))
                    for i = 1:n
                        s[] = a.parent[i + offset]
                        _memcpy!(tptr + (i-1)*sizeof(S), sptr, sizeof(S))
                    end
                else
                    for i = 1:n
                        s[] = a.parent[i, i1, tailinds...]
                        _memcpy!(tptr + (i-1)*sizeof(S), sptr, sizeof(S))
                    end
                end
            else
                # No extra dimension
                s[] = a.parent[i1, tailinds...]
                _memcpy!(tptr, sptr, sizeof(S))
            end
        end
        return t[]
    end
    # S is bigger than T and contains an integer number of them
    n = sizeof(S) ÷ sizeof(T)
    t = Ref{NTuple{n,T}}()
    s = Ref{S}()
    GC.@preserve t s begin
        tptr = Ptr{UInt8}(unsafe_convert(Ref{T}, t))
        sptr = Ptr{UInt8}(unsafe_convert(Ref{S}, s))
        s[] = a.parent[tailinds...]
        _memcpy!(tptr, sptr, sizeof(S))
    end
    return t[][i1]
end

@inline @propagate_inbounds setindex!(a::NonReshapedReinterpretArray{T,0,S} where T, v) where {S} = (a.parent[] = reinterpret(S, v))
@inline @propagate_inbounds setindex!(a::ReinterpretArray, v) = (a[1] = v)

@inline @propagate_inbounds function setindex!(a::ReinterpretArray{T,N,S}, v, inds::Vararg{Int, N}) where {T,N,S}
    check_writable(a)
    _setindex_ra!(a, v, inds[1], tail(inds))
end

@inline @propagate_inbounds function setindex!(a::ReinterpretArray{T,N,S}, v, i::Int) where {T,N,S}
    check_writable(a)
    if isa(IndexStyle(a), IndexLinear)
        return _setindex_ra!(a, v, i, ())
    end
    inds = _to_subscript_indices(a, i)
    _setindex_ra!(a, v, inds[1], tail(inds))
end

@inline @propagate_inbounds function setindex!(a::ReshapedReinterpretArray{T,N,S}, v, ind::SCartesianIndex2) where {T,N,S}
    check_writable(a)
    v = convert(T, v)::T
    t = Ref{T}(v)
    s = Ref{S}(a.parent[ind.j])
    GC.@preserve t s begin
        tptr = Ptr{UInt8}(unsafe_convert(Ref{T}, t))
        sptr = Ptr{UInt8}(unsafe_convert(Ref{S}, s))
        _memcpy!(sptr + (ind.i-1)*sizeof(T), tptr, sizeof(T))
    end
    a.parent[ind.j] = s[]
    return a
end

@inline @propagate_inbounds function _setindex_ra!(a::NonReshapedReinterpretArray{T,N,S}, v, i1::Int, tailinds::TT) where {T,N,S,TT}
    v = convert(T, v)::T
    # Make sure to match the scalar reinterpret if that is applicable
    if sizeof(T) == sizeof(S) && (fieldcount(T) + fieldcount(S)) == 0
        return setindex!(a.parent, reinterpret(S, v), i1, tailinds...)
    else
        @boundscheck checkbounds(a, i1, tailinds...)
        ind_start, sidx = divrem((i1-1)*sizeof(T), sizeof(S))
        t = Ref{T}(v)
        s = Ref{S}()
        GC.@preserve t s begin
            tptr = Ptr{UInt8}(unsafe_convert(Ref{T}, t))
            sptr = Ptr{UInt8}(unsafe_convert(Ref{S}, s))
            # Optimizations that avoid branches
            if sizeof(T) % sizeof(S) == 0
                # T is bigger than S and contains an integer number of them
                n = sizeof(T) ÷ sizeof(S)
                for i = 0:n-1
                    _memcpy!(sptr, tptr + i*sizeof(S), sizeof(S))
                    a.parent[ind_start + i + 1, tailinds...] = s[]
                end
            elseif sizeof(S) % sizeof(T) == 0
                # S is bigger than T and contains an integer number of them
                s[] = a.parent[ind_start + 1, tailinds...]
                _memcpy!(sptr + sidx, tptr, sizeof(T))
                a.parent[ind_start + 1, tailinds...] = s[]
            else
                nbytes_copied = 0
                i = 1
                # Deal with any partial elements at the start. We'll have to copy in the
                # element from the original array and overwrite the relevant parts
                if sidx != 0
                    s[] = a.parent[ind_start + i, tailinds...]
                    nb = min((sizeof(S) - sidx) % UInt, sizeof(T) % UInt)
                    _memcpy!(sptr + sidx, tptr, nb)
                    nbytes_copied += nb
                    a.parent[ind_start + i, tailinds...] = s[]
                    i += 1
                    sidx = 0
                end
                # Deal with the main body of elements
                while nbytes_copied < sizeof(T) && (sizeof(T) - nbytes_copied) > sizeof(S)
                    nb = min(sizeof(S), sizeof(T) - nbytes_copied)
                    _memcpy!(sptr, tptr + nbytes_copied, nb)
                    nbytes_copied += nb
                    a.parent[ind_start + i, tailinds...] = s[]
                    i += 1
                end
                # Deal with trailing partial elements
                if nbytes_copied < sizeof(T)
                    s[] = a.parent[ind_start + i, tailinds...]
                    nb = min(sizeof(S), sizeof(T) - nbytes_copied)
                    _memcpy!(sptr, tptr + nbytes_copied, nb)
                    a.parent[ind_start + i, tailinds...] = s[]
                end
            end
        end
    end
    return a
end

@inline @propagate_inbounds function _setindex_ra!(a::ReshapedReinterpretArray{T,N,S}, v, i1::Int, tailinds::TT) where {T,N,S,TT}
    v = convert(T, v)::T
    # Make sure to match the scalar reinterpret if that is applicable
    if sizeof(T) == sizeof(S) && (fieldcount(T) + fieldcount(S)) == 0
        return setindex!(a.parent, reinterpret(S, v), i1, tailinds...)
    end
    @boundscheck checkbounds(a, i1, tailinds...)
    t = Ref{T}(v)
    s = Ref{S}()
    GC.@preserve t s begin
        tptr = Ptr{UInt8}(unsafe_convert(Ref{T}, t))
        sptr = Ptr{UInt8}(unsafe_convert(Ref{S}, s))
        if sizeof(T) >= sizeof(S)
            if sizeof(T) > sizeof(S)
                # Extra dimension in the parent array
                n = sizeof(T) ÷ sizeof(S)
                if isempty(tailinds) && IndexStyle(a.parent) === IndexLinear()
                    offset = n * (i1 - firstindex(a))
                    for i = 1:n
                        _memcpy!(sptr, tptr + (i-1)*sizeof(S), sizeof(S))
                        a.parent[i + offset] = s[]
                    end
                else
                    for i = 1:n
                        _memcpy!(sptr, tptr + (i-1)*sizeof(S), sizeof(S))
                        a.parent[i, i1, tailinds...] = s[]
                    end
                end
            else
                # No extra dimension
                _memcpy!(sptr, tptr, sizeof(S))
                a.parent[i1, tailinds...] = s[]
            end
        else
            # S is bigger than T and contains an integer number of them
            s[] = a.parent[tailinds...]
            _memcpy!(sptr + (i1-1)*sizeof(T), tptr, sizeof(T))
            a.parent[tailinds...] = s[]
        end
    end
    return a
end

# Padding
struct Padding
    offset::Int
    size::Int
end
function intersect(p1::Padding, p2::Padding)
    start = max(p1.offset, p2.offset)
    stop = min(p1.offset + p1.size, p2.offset + p2.size)
    Padding(start, max(0, stop-start))
end

struct PaddingError
    S::Type
    T::Type
end

function showerror(io::IO, p::PaddingError)
    print(io, "Padding of type $(p.S) is not compatible with type $(p.T).")
end

"""
    CyclePadding(padding, total_size)

Cylces an iterator of `Padding` structs, restarting the padding at `total_size`.
E.g. if `padding` is all the padding in a struct and `total_size` is the total
aligned size of that array, `CyclePadding` will correspond to the padding in an
infinite vector of such structs.
"""
struct CyclePadding{P}
    padding::P
    total_size::Int
end
eltype(::Type{<:CyclePadding}) = Padding
IteratorSize(::Type{<:CyclePadding}) = IsInfinite()
isempty(cp::CyclePadding) = isempty(cp.padding)
function iterate(cp::CyclePadding)
    y = iterate(cp.padding)
    y === nothing && return nothing
    y[1], (0, y[2])
end
function iterate(cp::CyclePadding, state::Tuple)
    y = iterate(cp.padding, tail(state)...)
    y === nothing && return iterate(cp, (state[1]+cp.total_size,))
    Padding(y[1].offset+state[1], y[1].size), (state[1], tail(y)...)
end

"""
    Compute the location of padding in a type.
"""
function padding(T)
    padding = Padding[]
    last_end::Int = 0
    for i = 1:fieldcount(T)
        offset = fieldoffset(T, i)
        fT = fieldtype(T, i)
        if offset != last_end
            push!(padding, Padding(offset, offset-last_end))
        end
        last_end = offset + sizeof(fT)
    end
    padding
end

function CyclePadding(T::DataType)
    a, s = datatype_alignment(T), sizeof(T)
    as = s + (a - (s % a)) % a
    pad = padding(T)
    s != as && push!(pad, Padding(s, as - s))
    CyclePadding(pad, as)
end

using .Iterators: Stateful
@pure function array_subpadding(S, T)
    checked_size = 0
    lcm_size = lcm(sizeof(S), sizeof(T))
    s, t = Stateful{<:Any, Any}(CyclePadding(S)),
           Stateful{<:Any, Any}(CyclePadding(T))
    isempty(t) && return true
    isempty(s) && return false
    while checked_size < lcm_size
        # Take padding in T
        pad = popfirst!(t)
        # See if there's corresponding padding in S
        while true
            ps = peek(s)
            ps.offset > pad.offset && return false
            intersect(ps, pad) == pad && break
            popfirst!(s)
        end
        checked_size = pad.offset + pad.size
    end
    return true
end

# Reductions with IndexSCartesian2

function _mapreduce(f::F, op::OP, style::IndexSCartesian2{K}, A::AbstractArrayOrBroadcasted) where {F,OP,K}
    inds = eachindex(style, A)
    n = size(inds)[2]
    if n == 0
        return mapreduce_empty_iter(f, op, A, IteratorEltype(A))
    else
        return mapreduce_impl(f, op, A, first(inds), last(inds))
    end
end

@noinline function mapreduce_impl(f::F, op::OP, A::AbstractArrayOrBroadcasted,
                                  ifirst::SCI, ilast::SCI, blksize::Int) where {F,OP,SCI<:SCartesianIndex2{K}} where K
    if ifirst.j + blksize > ilast.j
        # sequential portion
        @inbounds a1 = A[ifirst]
        @inbounds a2 = A[SCI(2,ifirst.j)]
        v = op(f(a1), f(a2))
        @simd for i = ifirst.i + 2 : K
            @inbounds ai = A[SCI(i,ifirst.j)]
            v = op(v, f(ai))
        end
        # Remaining columns
        for j = ifirst.j+1 : ilast.j
            @simd for i = 1:K
                @inbounds ai = A[SCI(i,j)]
                v = op(v, f(ai))
            end
        end
        return v
    else
        # pairwise portion
        jmid = (ifirst.j + ilast.j) >> 1
        v1 = mapreduce_impl(f, op, A, ifirst, SCI(K,jmid), blksize)
        v2 = mapreduce_impl(f, op, A, SCI(1,jmid+1), ilast, blksize)
        return op(v1, v2)
    end
end

mapreduce_impl(f::F, op::OP, A::AbstractArrayOrBroadcasted, ifirst::SCartesianIndex2, ilast::SCartesianIndex2) where {F,OP} =
    mapreduce_impl(f, op, A, ifirst, ilast, pairwise_blocksize(f, op))

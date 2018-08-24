# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Gives a reinterpreted view (of element type T) of the underlying array (of element type S).
If the size of `T` differs from the size of `S`, the array will be compressed/expanded in
the first dimension.
"""
struct ReinterpretArray{T,N,S,A<:AbstractArray{S, N}} <: AbstractArray{T, N}
    parent::A
    readable::Bool
    writable::Bool
    global reinterpret
    function reinterpret(::Type{T}, a::A) where {T,N,S,A<:AbstractArray{S, N}}
        function throwbits(::Type{S}, ::Type{T}, ::Type{U}) where {S,T,U}
            @_noinline_meta
            throw(ArgumentError("cannot reinterpret `$(S)` `$(T)`, type `$(U)` is not a bits type"))
        end
        function throwsize0(::Type{S}, ::Type{T})
            @_noinline_meta
            throw(ArgumentError("cannot reinterpret a zero-dimensional `$(S)` array to `$(T)` which is of a different size"))
        end
        function thrownonint(::Type{S}, ::Type{T}, dim)
            @_noinline_meta
            throw(ArgumentError("""
                cannot reinterpret an `$(S)` array to `$(T)` whose first dimension has size `$(dim)`.
                The resulting array would have non-integral first dimension.
            """))
        end
        function throwaxes1(::Type{S}, ::Type{T}, ax1)
            @_noinline_meta
            throw(ArgumentError("cannot reinterpret a `$(S)` array to `$(T)` when the first axis is $ax1. Try reshaping first."))
        end
        isbitstype(T) || throwbits(S, T, T)
        isbitstype(S) || throwbits(S, T, S)
        (N != 0 || sizeof(T) == sizeof(S)) || throwsize0(S, T)
        ax1 = axes(a)[1]
        if N != 0 && sizeof(S) != sizeof(T)
            dim = length(ax1)
            rem(dim*sizeof(S),sizeof(T)) == 0 || thrownonint(S, T, dim)
            first(ax1) == 1 || throwaxes1(S, T, ax1)
        end
        readable = array_subpadding(T, S)
        writable = array_subpadding(S, T)
        new{T, N, S, A}(a, readable, writable)
    end
end

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

IndexStyle(a::ReinterpretArray) = IndexStyle(a.parent)

parent(a::ReinterpretArray) = a.parent
dataids(a::ReinterpretArray) = dataids(a.parent)

function size(a::ReinterpretArray{T,N,S} where {N}) where {T,S}
    psize = size(a.parent)
    size1 = div(psize[1]*sizeof(S), sizeof(T))
    tuple(size1, tail(psize)...)
end

function axes(a::ReinterpretArray{T,N,S} where {N}) where {T,S}
    paxs = axes(a.parent)
    f, l = first(paxs[1]), length(paxs[1])
    size1 = div(l*sizeof(S), sizeof(T))
    tuple(oftype(paxs[1], f:f+size1-1), tail(paxs)...)
end

elsize(::Type{<:ReinterpretArray{T}}) where {T} = sizeof(T)
unsafe_convert(::Type{Ptr{T}}, a::ReinterpretArray{T,N,S} where N) where {T,S} = Ptr{T}(unsafe_convert(Ptr{S},a.parent))

@inline @propagate_inbounds getindex(a::ReinterpretArray{T,0}) where {T} = reinterpret(T, a.parent[])
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
    _getindex_ra(a, inds[1], tail(inds))
end

@inline _memcpy!(dst, src, n) = ccall(:memcpy, Cvoid, (Ptr{UInt8}, Ptr{UInt8}, Csize_t), dst, src, n)

@inline @propagate_inbounds function _getindex_ra(a::ReinterpretArray{T,N,S}, i1::Int, tailinds::TT) where {T,N,S,TT}
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
        return t[]
    end
end


@inline @propagate_inbounds setindex!(a::ReinterpretArray{T,0,S} where T, v) where {S} = (a.parent[] = reinterpret(S, v))
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

@inline @propagate_inbounds function _setindex_ra!(a::ReinterpretArray{T,N,S}, v, i1::Int, tailinds::TT) where {T,N,S,TT}
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
            nbytes_copied = 0
            i = 1
            # Deal with any partial elements at the start. We'll have to copy in the
            # element from the original array and overwrite the relevant parts
            if sidx != 0
                s[] = a.parent[ind_start + i, tailinds...]
                nb = min(sizeof(S) - sidx, sizeof(T))
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

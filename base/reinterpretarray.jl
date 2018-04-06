# This file is a part of Julia. License is MIT: https://julialang.org/license

"""
Gives a reinterpreted view (of element type T) of the underlying array (of element type S).
If the size of `T` differs from the size of `S`, the array will be compressed/expanded in
the first dimension.
"""
struct ReinterpretArray{T,N,S,A<:AbstractArray{S, N}} <: AbstractArray{T, N}
    parent::A
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
        isbits(T) || throwbits(S, T, T)
        isbits(S) || throwbits(S, T, S)
        (N != 0 || sizeof(T) == sizeof(S)) || throwsize0(S, T)
        if N != 0 && sizeof(S) != sizeof(T)
            dim = size(a)[1]
            rem(dim*sizeof(S),sizeof(T)) == 0 || thrownonint(S, T, dim)
        end
        new{T, N, S, A}(a)
    end
end

parent(a::ReinterpretArray) = a.parent
dataids(a::ReinterpretArray) = dataids(a.parent)

function size(a::ReinterpretArray{T,N,S} where {N}) where {T,S}
    psize = size(a.parent)
    size1 = div(psize[1]*sizeof(S), sizeof(T))
    tuple(size1, tail(psize)...)
end

elsize(::Type{<:ReinterpretArray{T}}) where {T} = sizeof(T)
unsafe_convert(::Type{Ptr{T}}, a::ReinterpretArray{T,N,S} where N) where {T,S} = Ptr{T}(unsafe_convert(Ptr{S},a.parent))

@inline @propagate_inbounds getindex(a::ReinterpretArray{T,0}) where {T} = reinterpret(T, a.parent[])
@inline @propagate_inbounds getindex(a::ReinterpretArray) = a[1]

@inline @propagate_inbounds function getindex(a::ReinterpretArray{T,N,S}, inds::Vararg{Int, N}) where {T,N,S}
    # Make sure to match the scalar reinterpret if that is applicable
    if sizeof(T) == sizeof(S) && (fieldcount(T) + fieldcount(S)) == 0
        return reinterpret(T, a.parent[inds...])
    else
        ind_start, sidx = divrem((inds[1]-1)*sizeof(T), sizeof(S))
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
                s[] = a.parent[ind_start + i, tail(inds)...]
                while nbytes_copied < sizeof(T) && sidx < sizeof(S)
                    unsafe_store!(tptr, unsafe_load(sptr, sidx + 1), nbytes_copied + 1)
                    sidx += 1
                    nbytes_copied += 1
                end
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
    v = convert(T, v)::T
    # Make sure to match the scalar reinterpret if that is applicable
    if sizeof(T) == sizeof(S) && (fieldcount(T) + fieldcount(S)) == 0
        return setindex!(a.parent, reinterpret(S, v), inds...)
    else
        ind_start, sidx = divrem((inds[1]-1)*sizeof(T), sizeof(S))
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
                s[] = a.parent[ind_start + i, tail(inds)...]
                while nbytes_copied < sizeof(T) && sidx < sizeof(S)
                    unsafe_store!(sptr, unsafe_load(tptr, nbytes_copied + 1), sidx + 1)
                    sidx += 1
                    nbytes_copied += 1
                end
                a.parent[ind_start + i, tail(inds)...] = s[]
                i += 1
                sidx = 0
            end
            # Deal with the main body of elements
            while nbytes_copied < sizeof(T) && (sizeof(T) - nbytes_copied) > sizeof(S)
                while nbytes_copied < sizeof(T) && sidx < sizeof(S)
                    unsafe_store!(sptr, unsafe_load(tptr, nbytes_copied + 1), sidx + 1)
                    sidx += 1
                    nbytes_copied += 1
                end
                a.parent[ind_start + i, tail(inds)...] = s[]
                i += 1
                sidx = 0
            end
            # Deal with trailing partial elements
            if nbytes_copied < sizeof(T)
                s[] = a.parent[ind_start + i, tail(inds)...]
                while nbytes_copied < sizeof(T) && sidx < sizeof(S)
                    unsafe_store!(sptr, unsafe_load(tptr, nbytes_copied + 1), sidx + 1)
                    sidx += 1
                    nbytes_copied += 1
                end
                a.parent[ind_start + i, tail(inds)...] = s[]
            end
        end
    end
    return a
end

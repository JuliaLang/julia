"""
    Buffer{T} <: DenseVector{T}

A one-dimensional vector of mutable elements with a fixed length.
"""
Buffer

"""
    DynamicBuffer{T} <: DenseVector{T}

A one-dimensional vector of mutable elements dynamic length.
"""
DynamicBuffer

if !isdefined(Main, :Base)
    Buffer{T}(::UndefInitializer, len::Int) where {T} =
        ccall(:jl_new_buffer, Buffer{T}, (Any, Int), Buffer{T}, len)
    Buffer(a::AbstractArray{T}) where {T} = Buffer{T}(a)
    function Buffer{T}(a::AbstractArray) where {T}
        n = length(a)
        b = Buffer{T}(undef, n)
        i = 1
        for a_i in a
            @inbounds b[i] = a_i
            i += 1
        end
        return b
    end

    DynamicBuffer{T}(::UndefInitializer, len::Int) where {T} =
        ccall(:jl_new_buffer, DynamicBuffer{T}, (Any, Int), DynamicBuffer{T}, len)
    DynamicBuffer(a::AbstractArray{T}) where {T} = Dynamicfer{T}(a)
    function DynamicBuffer{T}(a::AbstractArray) where {T}
        n = length(a)
        b = DynamicBuffer{T}(undef, n)
        i = 1
        for a_i in a
            @inbounds b[i] = a_i
            i += 1
        end
        return b
    end

end

eltype(::Type{<:BufferType{T}}) where {T} = T

elsize(@nospecialize T::Type{<:BufferType}) = aligned_sizeof(eltype(T))

length(b::BufferType) = Core.Intrinsics.bufferlen(b)

firstindex(b::BufferType) = 1

lastindex(b::BufferType) = length(b)

keys(b::BufferType) = OneTo(length(b))

axes(b::BufferType) = (OneTo(length(b)),)
axes(b::BufferType, d::Integer) = d <= 1 ? OneTo(length(b)) : OneTo(1)

iterate(v::BufferType, i=1) = (length(v) < i ? nothing : (v[i], i + 1))

isempty(b::BufferType) = (length(b) == 0)

@eval getindex(b::BufferType{T}, i::Int) where {T} = Core.bufref($(Expr(:boundscheck)), b, i)

# Faster contiguous indexing using copyto! for AbstractUnitRange and Colon
function getindex(b::BufferType{T}, idxs::AbstractUnitRange{Int}) where {T}
    @inline
    @boundscheck checkbounds(b, idxs)
    len = length(idxs)
    dst = similar(b, length(idxs))
    if len > 0
        copyto!(dst, 1, b, first(idxs), len)
    end
    return dst
end
function getindex(b::BufferType, ::Colon)
    len = length(A)
    dst = similar(b, length(idxs))
    if len > 0
        unsafe_copyto!(dst, 1, b, 1, len)
    end
    return dst
end

# TODO: old note from array.jl, may not applly anymore
# This is redundant with the abstract fallbacks, but needed for bootstrap
function getindex(b::BufferType{T}, idxs::AbstractRange{Int}) where {T}
    @inline
    @boundscheck checkbounds(b, idxs)
    dst = similar(b, length(idxs))
    dst_idx = 1
    @inbounds for idx in idxs
        dst[dst_idx] = b[idx]
    end
    return dst
end

@eval setindex!(b::BufferType{T}, x, i::Int) where {T} =
    Core.bufset($(Expr(:boundscheck)), b, convert(T, x)::T, i)

# This is redundant with the abstract fallbacks but needed and helpful for bootstrap
function setindex!(b::BufferType, vals::AbstractArray, idxs::AbstractArray{Int, 1})
    @_propagate_inbounds_meta
    @boundscheck setindex_shape_check(vals, length(idxs))
    require_one_based_indexing(vals)
    vals′ = unalias(b, vals)
    vals_idx = 1
    @inbounds for idx in unalias(b, idxs)
        b[idx] = vals′[vals_idx]
        vals_idx += 1
    end
    return b
end

# Faster contiguous setindex! with copyto!
function setindex!(b::BufferType{T}, vals::Buffer{T}, idxs::AbstractUnitRange{Int}) where {T}
    @inline
    @boundscheck checkbounds(b, idxs)
    len = length(idxs)
    @boundscheck setindex_shape_check(vals, len)
    if len > 0
        unsafe_copyto!(b, first(idxs), vals, 1, len)
    end
    return A
end
function setindex!(b::BufferType{T}, vals::Buffer{T}, ::Colon) where {T}
    @inline
    len = length(b)
    @boundscheck setindex_shape_check(vals, len)
    if len > 0
        unsafe_copyto!(b, 1, vals, 1, len)
    end
    return b
end

function isassigned(b::BufferType, i::Int, ii::Int...)
    @inline
    @boundscheck all(isone, ii) || return false
    return isassigned(b, i)
end
function isassigned(b::BufferType, i::Int)
    @inline
    @boundscheck 1 <= i <= length(b) || return false
    ccall(:jl_buffer_isassigned, Cint, (Any, UInt), b, i - 1) == 1
end

function ==(v1::BufferType, v2::BufferType)
    length(v1) == length(v2) || return false
    for i in 1:length(v1)
        v1[i] == v2[i] || return false
    end
    return true
end

copy(b::T) where {T<:Buffer} = ccall(:jl_buffer_copy, Ref{T}, (Any,), b)

sizeof(b::BufferType) = Core.sizeof(b)

# TODO promotion between Buffer and DynamicBuffer
function promote_rule(a::Type{Buffer{T}}, b::Type{Buffer{S}}) where {T, S}
    el_same(promote_type(T, S), a, b)
end
function promote_rule(a::Type{DynamicBuffer{T}}, b::Type{DynamicBuffer{S}}) where {T, S}
    el_same(promote_type(T, S), a, b)
end

function similar(b::Buffer, T::Type=eltype(b), dims::Tuple{Int}=(length(b),))
    Buffer{T}(undef, getfield(dims, 1))
end
function similar(b::DynamicBuffer, T::Type=eltype(b), dims::Tuple{Int}=(length(b),))
    DynamicBuffer{T}(undef, getfield(dims, 1))
end

function unsafe_grow_end!(b::DynamicBuffer, delta::Integer)
    ccall(:jl_buffer_grow_end, Cvoid, (Any, UInt), b, delta)
end
function unsafe_grow_at!(b::DynamicBuffer, i::Integer, delta::Integer)
    ccall(:jl_buffer_grow_at, Cvoid, (Any, Int, UInt), b, i - 1, delta)
end

function unsafe_delete_end!(b::DynamicBuffer, delta::Integer)
    ccall(:jl_buffer_del_end, Cvoid, (Any, UInt), b, delta)
end
function unsafe_delete_at!(b::DynamicBuffer, i::Integer, delta::Integer)
    ccall(:jl_buffer_del_at, Cvoid, (Any, Int, UInt), b, i - 1, delta)
end

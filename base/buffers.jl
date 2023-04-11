# This file is a part of Julia. License is MIT: https://julialang.org/license

function tag_to_variant(t::Union, tag::Int)
    @_total_meta
    a = t.a
    b = t.b
    while true
        tag === 0 && return a
        isa(b, Union) || return (tag === 0 ? b : Union{})
        a = b.a
        b = b.b
        tag -= 1
    end
end

function variant_to_tag(t::Union, @nospecialize(vt))
    @_total_meta
    tag = 0
    a = t.a
    b = t.b
    while true
        vt <: a && return tag
        tag += 1
        # if last member of union doesn'vt match `vt` then return tag that will never be
        # reached by `tag_to_variant`
        isa(b, Union) || return (b == vt ? tag : (tag + 1))
        a = b.a
        b = b.b
    end
end

const BUFFER_IMPL_NULL = false
const N_CALL_CACHE = 4096
const ARRAY_INLINE_NBYTES = 2048 * sizeof(Int)
const ARRAY_CACHE_ALIGN_THRESHOLD = 2048
const CACHE_BYTE_ALIGNMENT = 64
const SMALL_BYTE_ALIGNMENT = 16
const MIN_BUFFER_SIZE = sizeof(Int) + sizeof(Int)

struct ElementLayout
    nvariants::Int
    elsize::Int
    alignment::Int
    hasptr::Bool
    requires_initialization::Bool

    function ElementLayout(@nospecialize(T::Type))
        @_foldable_meta
        sz = RefValue{Csize_t}(0)
        al = RefValue{Csize_t}(0)
        cnt = ccall(:jl_islayout_inline, Cint, (Any, Ptr{Csize_t}, Ptr{Csize_t}), T, sz, al)
        # the number of inline elements represented at each position
        # 0  : boxed pointer
        # 1  : one type
        # >1 : bits union
        nvariants = (cnt == 0 || cnt > 127) ? 0 : Int(cnt)
        if nvariants === 0
            elsz = alignment = Core.sizeof(Ptr{Cvoid})
        else
            alignment = Int(al[])
            elsz = Int(LLT_ALIGN(sz[], alignment))
        end
        hasptr = nvariants !== 0 && isa(T, DataType) && !datatype_pointerfree(T)
        zeroinit = nvariants !== 1 || hasptr || (isa(T, DataType) && (T.flags & 0x0010) == 0x0010)
        return new(nvariants, elsz, alignment, hasptr, zeroinit)
    end
end

struct BufferLayout
    element_layout::ElementLayout
    data_size::Int
    object_size::Int
    isinline::Bool

    function BufferLayout(@nospecialize(T::Type), len::Int)
        @_foldable_meta
        lyt = ElementLayout(T)
        data_size = len * lyt.elsize
        if lyt.nvariants !== 0
            if lyt.nvariants === 1
                data_size += 1
            else
                data_size += len
            end
        end
        object_size = MIN_BUFFER_SIZE
        if data_size <= ARRAY_INLINE_NBYTES
            if data_size >= ARRAY_CACHE_ALIGN_THRESHOLD
                object_size = LLT_ALIGN(object_size, CACHE_BYTE_ALIGNMENT)
            elseif lyt.nvariants !== 0 && lyt.elsize >= 4
                object_size = LLT_ALIGN(object_size, SMALL_BYTE_ALIGNMENT)
            end
            object_size += data_size
            isinline = true
        else
            isinline = false
        end
        return new(lyt, data_size, object_size, isinline)
    end
end

macro _preserved_pointer_meta()
    return _is_internal(__module__) && Expr(:meta, Expr(:purity,
        #=:consistent=#false,
        #=:effect_free=#true,
        #=:nothrow=#true,
        #=:terminates_globally=#true,
        #=:terminates_locally=#false,
        #=:notaskstate=#true,
        #=:inaccessiblememonly=#false))
end

# FIXME this is just a hack to improve some of the effects for `pointer(b)` when we know
# that `b` is preserved from garbage collection. This should probably be replaced with a
# a more flexible/safe solution but for now allows us to work on how much we can improve
# native Julia implementations (reducing allocations, const-prop, etc.)
function _preserved_pointer(b::BufferType{T}) where {T}
    @_preserved_pointer_meta
    unsafe_convert(Ptr{T}, b)
end

function get_buffer_value(b::BufferType{T}, i::Int, bounds_check::Bool, assigned_check::Bool) where {T}
    @inline
    lyt = ElementLayout(T)
    idx0 = i - 1
    if lyt.nvariants === 0
        bounds_check && (1 <= i <= length(b) || throw(BoundsError(b, i)))
        t = @_gc_preserve_begin b
        p = _preserved_pointer(b) + (idx0 * Core.sizeof(Ptr{Cvoid}))
        pt = unsafe_load(unsafe_convert(Ptr{Ptr{Cvoid}}, p))
        assigned_check || (pt === C_NULL && throw(UndefRefError()))
        out = unsafe_load(unsafe_convert(Ptr{T}, pt))
        @_gc_preserve_end t
        return out
    elseif lyt.nvariants === 1
        bounds_check && (1 <= i <= length(b) || throw(BoundsError(b, i)))
        if isdefined(T, :instance)
            return getfield(T, :instance)
        else
            t = @_gc_preserve_begin b
            out = unsafe_load(_preserved_pointer(b) + (idx0 * lyt.elsize))
            @_gc_preserve_end t
            return out
        end
    else
        len = length(b)
        bounds_check && (1 <= i <= len || throw(BoundsError(b, i)))
        t = @_gc_preserve_begin b
        p = _preserved_pointer(b)
        tag = Int(unsafe_load(unsafe_convert(Ptr{UInt8}, p) + (lyt.elsize * len) + idx0))
        vt = tag_to_variant(T, tag)
        if isdefined(vt, :instance)
            out = vt.instance
        else
            out = unsafe_load(unsafe_convert(Ptr{vt}, p) + (idx0 * lyt.elsize))
        end
        @_gc_preserve_end t
        return out
    end
end

function set_buffer_value!(b::BufferType{T}, v::T, i::Int, bounds_check::Bool) where {T}
    lyt = ElementLayout(T)
    idx0 = i - 1
    if lyt.nvariants === 0
        bounds_check && (1 <= i <= length(b) || throw(BoundsError(b, i)))
        t = @_gc_preserve_begin b
        p = _preserved_pointer(b) + (idx0 * lyt.elsize)
        # FIXME this won't work for immutable values and probably needs updating with
        #`jl_gc_wb`
        unsafe_store!(unsafe_convert(Ptr{Ptr{Cvoid}}, p), pointer_from_objref(v))
        @_gc_preserve_end t
    elseif lyt.nvariants === 1
        bounds_check && (1 <= i <= length(b) || throw(BoundsError(b, i)))
        if !isdefined(vt, :instance)
            t = @_gc_preserve_begin b
            unsafe_store!(_preserved_pointer(b) + (idx0 * lyt.elsize), v)
            @_gc_preserve_end t
        end
    else
        len = length(b)
        bounds_check && (1 <= i <= len || throw(BoundsError(b, i)))
        t = @_gc_preserve_begin b
        p = _preserved_pointer(b)
        vt = typeof(v)
        tag = variant_to_tag(T, vt)
        unsafe_store!(convert(Ptr{UInt8}, p) + (lyt.elsize * len) + idx0, UInt8(tag))
        if !isdefined(vt, :instance)
            unsafe_store!(unsafe_convert(Ptr{vt}, p) + (idx0 * lyt.elsize), v)
        end
        @_gc_preserve_end t
    end
    return nothing
end

"""
    Buffer{T} <: DenseVector{T}

A one-dimensional vector of mutable elements with a fixed length.

!!! warning
    `Buffer` is experimental and subject to change without deprecation.
"""
Buffer

"""
    DynamicBuffer{T} <: DenseVector{T}

A one-dimensional vector of mutable elements dynamic length.

!!! warning
    `DynamicBuffer` is experimental and subject to change without deprecation.
"""
DynamicBuffer

if !isdefined(Main, :Base)

function Buffer{T}(::UndefInitializer, len::Int) where {T}
    lyt = BufferLayout(T, len)
    if lyt.isinline
        buf = ccall(:jl_gc_alloc_buffer_inline, Buffer{T}, (Any, Csize_t, Csize_t),  Buffer{T}, len, lyt.object_size)
    else
        buf = ccall(:jl_gc_malloc_buffer, Buffer{T}, (Any, Csize_t, Csize_t, Csize_t), Buffer{T}, len, lyt.data_size, lyt.object_size)
    end
    if lyt.element_layout.requires_initialization
        ccall(:memset, Ptr{Cvoid}, (Ptr{Cvoid}, Cint, Csize_t), buf, 0, lyt.data_size)
    elseif BUFFER_IMPL_NULL && lyt.elsize === 1
        t = @_gc_preserve_begin buf
        unsafe_store!(unsafe_convert(Ptr{UInt8}, _preserved_pointer(buf)) + len, 0x00)
        @_gc_preserve_end t
    end
    return buf
end
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

function DynamicBuffer{T}(::UndefInitializer, len::Int) where {T}
    lyt = BufferLayout(T, len)
    if lyt.isinline
        buf = ccall(:jl_gc_alloc_buffer_inline, DynamicBuffer{T}, (Any, Csize_t, Csize_t),  DynamicBuffer{T}, len, lyt.object_size)
    else
        buf = ccall(:jl_gc_malloc_buffer, DynamicBuffer{T}, (Any, Csize_t, Csize_t, Csize_t), DynamicBuffer{T}, len, lyt.data_size, lyt.object_size)
    end
    if lyt.element_layout.requires_initialization
        ccall(:memset, Ptr{Cvoid}, (Ptr{Cvoid}, Cint, Csize_t), buf, 0, lyt.data_size)
    elseif BUFFER_IMPL_NULL && lyt.elsize === 1
        t = @_gc_preserve_begin buf
        unsafe_store!(unsafe_convert(Ptr{UInt8}, _preserved_pointer(buf)) + len, 0x00)
        @_gc_preserve_end t
    end
    return buf
end
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

length(b::Buffer) = Core.Intrinsics.bufferlen(b)
# FIXME DynamicBuffer effects: this is set correctly for `Core.bufferlen` but not for `length`
length(b::DynamicBuffer) = Core.Intrinsics.bufferlen(b)

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

# TODO: old note from array.jl, may not apply anymore
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
    return b
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

fill!(b::BufferType{T}, x) where {T} = _fill!(b, convert(T, x))
function _fill!(b::BufferType{T}, x::T) where {T}
    for i in eachindex(b)
        @inbounds b[i] = x
    end
    return b
end

collect(b::BufferType) = copy(b)

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

copy(b::T) where {T<:BufferType} = ccall(:jl_buffer_copy, Ref{T}, (Any,), b)

function unsafe_copyto!(dest::Union{Buffer{T}, DynamicBuffer{T}}, doffs, src::Union{Buffer{T}, DynamicBuffer{T}}, soffs, n) where T
    t1 = @_gc_preserve_begin dest
    t2 = @_gc_preserve_begin src
    tsz = elsize(dest)
    doffset = doffs - 1
    soffset = soffs - 1
    dtoffset = tsz * doffset
    stoffset = tsz * soffset
    destp = unsafe_convert(Ptr{T}, dest)
    srcp = unsafe_convert(Ptr{T}, src)
    if !allocatedinline(T)
        ccall(:jl_buffer_ptr_copy, Cvoid, (Any, Ptr{Cvoid}, Any, Ptr{Cvoid}, Int),
              dest, destp + dtoffset, src, srcp + stoffset, n)
    elseif isbitstype(T)
        ccall(:memmove, Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}, Csize_t),
              destp + dtoffset, srcp + stoffset, n * tsz)
    elseif isbitsunion(T)
        ccall(:memmove, Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}, Csize_t),
              destp + dtoffset, srcp + stoffset, n * tsz)
        # copy selector bytes
        ccall(:memmove, Ptr{Cvoid}, (Ptr{Cvoid}, Ptr{Cvoid}, Csize_t),
              destp + (length(dest) * tsz) + doffset, srcp + (length(src) * tsz) + soffset, n)
    else
        _unsafe_copyto!(dest, doffs, src, soffs, n)
    end
    @_gc_preserve_end t2
    @_gc_preserve_end t1
    return dest
end

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

# TODO memset

# resizing methods
function resize!(b::DynamicBuffer, sz::Integer)
    len = length(b)
    if len > sz
        unsafe_delete_at!(b, len, len - sz, len)
    elseif len < sz
        unsafe_grow_at!(b, len, len + sz, len)
    end
    return b
end

function unsafe_grow_at!(b::DynamicBuffer, i::Integer, delta::Integer, len::Integer=length(b))
    ccall(:jl_buffer_grow_at_end, Cvoid, (Any, UInt, UInt, UInt), b, i - 1, delta, len)
end

function unsafe_delete_at!(b::DynamicBuffer, i::Integer, delta::Integer, len::Integer=length(b))
    ccall(:jl_buffer_del_at_end, Cvoid, (Any, UInt, UInt, UInt), b, i - 1, delta, len)
end

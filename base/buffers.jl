# This file is a part of Julia. License is MIT: https://julialang.org/license

# TODO test native julia immplementations of delete and grow

const BUFFER_IMPL_NULL = false
const N_CALL_CACHE = UInt(4096)
const ARRAY_INLINE_NBYTES = UInt(2048 * sizeof(Int))
const ARRAY_CACHE_ALIGN_THRESHOLD = UInt(2048)
const MALLOC_THRESH = UInt(1048576)
const CACHE_BYTE_ALIGNMENT = UInt(64)
const SMALL_BYTE_ALIGNMENT = UInt(16)
const PTR_SIZE = bitcast(UInt, Core.sizeof(Ptr{Cvoid}))
const JL_BUFFER_SIZEOF = Core.sizeof(Ptr{Cvoid}) + Core.sizeof(Ptr{Cvoid})

const BITS_KIND = 0x00
const UNION_KIND = 0x01
const BOX_KIND = 0x02
const HAS_PTR_KIND = 0x03
const BOOL_KIND = 0x04

struct ElementLayout
    elsize::UInt
    alignment::UInt
    kind::UInt8
    requires_initialization::Bool

    function ElementLayout(@nospecialize(T::Type))
        @_total_meta
        sz = RefValue{Csize_t}(0)
        al = RefValue{Csize_t}(0)
        cnt = ccall(:jl_islayout_inline, UInt, (Any, Ptr{Csize_t}, Ptr{Csize_t}), T, sz, al)
        if cnt === UInt(0) || cnt > UInt(127)
            kind = BOX_KIND
            elsz = alignment = PTR_SIZE
            reqinit = true
        else
            if cnt > UInt(1)
                kind = UNION_KIND
                reqinit = true
            elseif isa(T, DataType) && !datatype_pointerfree(T)
                kind = HAS_PTR_KIND
                reqinit = true
            else
                kind = BITS_KIND
                # zeroinit field may still be true if this is an immutable struct with a
                # field of bits union type
                reqinit = isa(T, DataType) && (T.flags & 0x0010) === 0x0010
            end
            alignment = al[]
            elsz = LLT_ALIGN(sz[], alignment)
        end
        return new(elsz, alignment, kind, reqinit)
    end
end

# this is a version of memcpy that preserves atomic memory ordering
# which makes it safe to use for objects that can contain memory references
# without risk of creating pointers out of thin air
# (based on memmove_refs in "julia_internal.h")
function memmove_ptrs!(dst::Ptr{Ptr{Cvoid}}, src_start::Ptr{Ptr{Cvoid}}, nbytes::UInt)
    @_foldable_meta
    src_stop = src_start + nbytes
    if (dst < src_start || dst > src_stop)
        Core.Intrinsics.atomic_pointerset(dst, Core.Intrinsics.atomic_pointerref(src_start, :acquire), :monotonic)
        dst += PTR_SIZE
        src_start += PTR_SIZE
        while src_start < src_stop
            Core.Intrinsics.atomic_pointerset(dst, Core.Intrinsics.atomic_pointerref(src_start, :acquire), :monotonic)
            dst += PTR_SIZE
            src_start += PTR_SIZE
        end
    else
        src_stop -= PTR_SIZE
        dst = dst + (nbytes - PTR_SIZE)
        Core.Intrinsics.atomic_pointerset(dst, Core.Intrinsics.atomic_pointerref(src_stop, :acquire), :monotonic)
        while src_start <= src_stop
            Core.Intrinsics.atomic_pointerset(dst, Core.Intrinsics.atomic_pointerref(src_stop, :acquire), :monotonic)
            dst -= PTR_SIZE
            src_stop -= PTR_SIZE
        end
    end
    return nothing
end

function memcpy!(dst::Ptr, src::Ptr, nbytes::Integer)
    ccall(:memcpy, Cvoid, (Ptr{Cvoid}, Ptr{Cvoid}, Csize_t), dst, src, nbytes)
end
function memmove(dst::Ptr, src::Ptr, nbytes::Integer)
    ccall(:memmove, Cvoid, (Ptr{Cvoid}, Ptr{Cvoid}, Csize_t), dst, src, nbytes)
end
function memset(p::Ptr, val, nbytes::Integer)
    ccall(:memset, Ptr{Cvoid}, (Ptr{Cvoid}, Cint, Csize_t), p, val, nbytes)
end

macro _preserved_pointer_meta()
    return _is_internal(__module__) && Expr(:meta, Expr(:purity,
        #=:consistent=#false,
        #=:effect_free=#true,
        #=:nothrow=#true,
        #=:terminates_globally=#false,
        #=:terminates_locally=#true,
        #=:notaskstate=#true,
        #=:inaccessiblememonly=#false))
end

# FIXME this is just a hack to improve some of the effects for `pointer(b)` when we know
# that `b` is preserved from garbage collection. This should probably be replaced with a
# a more flexible/safe solution but for now allows us to work on how much we can improve
# native Julia implementations (reducing allocations, const-prop, etc.)

# function _preserved_pointer(b::BufferType{T}) where {T}
#     @_preserved_pointer_meta
#     unsafe_convert(Ptr{T}, b)
# end

function _pointer_from_buffer(b::BufferType)
    @_preserved_pointer_meta
    ccall(:jl_pointer_from_buffer, Ptr{Cvoid}, (Any,), b)
end

function _get_variant_ptr(p::Ptr{T}, offset::UInt, len::UInt, elsz::UInt) where {T}
    tag = unsafe_load(unsafe_convert(Ptr{UInt8}, p) + ((elsz * len) + offset))
    a = T.a
    b = T.b
    while true
        tag === 0x00 && return unsafe_convert(Ptr{a}, p)
        isa(b, Union) || return unsafe_convert(Ptr{b}, p)
        tag -= 0x01
        a = b.a
        b = b.b
    end
end

function _variant_to_tag(U::Union, vt::DataType)
    @_total_meta
    tag = 0x00
    a = U.a
    b = U.b
    while true
        a === vt && return tag
        tag += 0x01
        isa(b, Union) ||return tag # `utail.b` is last variant available
        a = b.a
        b = b.b
    end
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

# should have these effects (?c,+e,!n,+t,+s,+m,+i)
for BT in (Buffer, DynamicBuffer)
    Core.eval(Core, :(
        function allocate_buffer(::Type{$(BT)}, ::Type{T}, len::Int) where {T}
            lyt = Core.Compiler.ElementLayout(T)
            Core.Intrinsics.ule_int(0, len) || throw(ArgumentError("buffer type cannot have a negative length"))
            ulen = bitcast(UInt, len)
            data_size = Core.Intrinsics.mul_int(ulen, lyt.elsize)
            if Core.Intrinsics.not_int(lyt.kind === Core.Compiler.BOX_KIND)
                if lyt.kind === Core.Compiler.BITS_KIND && lyt.elsize === UInt(1)
                    data_size = Core.add_int(data_size, UInt(1))
                elseif lyt.kind === Core.Compiler.UNION_KIND
                    # extra byte tag for each element
                    data_size = Core.add_int(data_size, ulen)
                end
            elseif lyt.kind === Core.Compiler.BOOL_KIND
                data_size = Core.Compiler.:>>(Core.add_int(ulen, UInt(63)), UInt(6))
            end
            object_size = Core.Compiler.JL_BUFFER_SIZEOF
            if Core.Intrinsics.ule_int(data_size, Core.Compiler.ARRAY_INLINE_NBYTES)
                if Core.Intrinsics.ule_int(data_size, Core.Compiler.ARRAY_CACHE_ALIGN_THRESHOLD)
                    object_size = Core.and_int(Core.add_int(object_size, Core.sub_int(Core.Compiler.CACHE_BYTE_ALIGNMENT, UInt(1))),
                        Core.Intrinsics.neg_int(Core.Compiler.CACHE_BYTE_ALIGNMENT))
                elseif Core.Intrinsics.not_int(lyt.kind === Core.Compiler.BOX_KIND && Core.Intrinsics.ule_int(UInt(4), lyt.elsize))
                    object_size = Core.and_int(Core.add_int(object_size, Core.sub_int(Core.Compiler.SMALL_BYTE_ALIGNMENT, UInt(1))),
                        Core.Intrinsics.neg_int(Core.Compiler.SMALL_BYTE_ALIGNMENT))
                end
                object_size = Core.add_int(object_size, data_size)
                b = ccall(:jl_gc_alloc, $(BT){T}, (Any, UInt, Any), Core.getptls(), object_size, $(BT){T});
                t = Core.Compiler.@_gc_preserve_begin b
                bptr = Core.Compiler._pointer_from_buffer(b)
                # TODO if this is just checking GC markings can we do this directly so that
                # allocating buffers can sometimes assume no throw?
                # @assert iszero((UInt(bptr) & UInt(15)))
                # this is printing out right now in order to interactively see how the data is being marked by the GC
                println(stdout, Core.and_int(UInt(bptr), UInt(15)))
                Core.Intrinsics.pointerset(Core.Compiler.unsafe_convert(Ptr{UInt}, bptr), ulen, 1, 1)
                data_start = Core.Compiler.unsafe_convert(Ptr{Cvoid}, Core.Compiler.:+(bptr, Core.Compiler.JL_BUFFER_SIZEOF))
                Core.Intrinsics.pointerset(Core.Compiler.unsafe_convert(Ptr{Ptr{Cvoid}}, Core.Compiler.:+(bptr, sizeof(UInt))), data_start, 1, 1)
                if lyt.requires_initialization
                    Core.Compiler.memset(data_start, Int32(0), data_size)
                elseif Core.Compiler.BUFFER_IMPL_NULL && lyt.elsize === UInt(1)
                    Core.Intrinsics.pointerset(Core.Compiler.:+(Core.Compiler.unsafe_convert(Ptr{UInt8}, data_start), data_size), 0x00, 1, 1)
                end
                Core.Compiler.@_gc_preserve_end t
            else
                data_start = ccall(:jl_gc_managed_malloc, Ptr{Cvoid}, (UInt,), data_size)
                t = Core.Compiler.@_gc_preserve_begin b
                b = ccall(:jl_gc_alloc, $(BT){T}, (Any, UInt, Any), Core.getptls(), object_size, $(BT){T});
                bptr = Core.Compiler._pointer_from_buffer(b)
                Core.Intrinsics.pointerset(Core.Compiler.unsafe_convert(Ptr{UInt}, bptr), ulen, 1, 1)
                Core.Intrinsics.pointerset(Core.Compiler.unsafe_convert(Ptr{Ptr{Cvoid}}, Core.Compiler.:+(bptr, sizeof(UInt))), data_start, 1, 1)
                if lyt.requires_initialization
                    Core.Compiler.memset(data_start, Int32(0), data_size)
                elseif Core.Compiler.BUFFER_IMPL_NULL && lyt.elsize === UInt(1)
                    Core.Intrinsics.pointerset(Core.Comipler.:+(Core.Compiler.unsafe_convert(Ptr{UInt8}, data_start), data_size), 0x00, 1, 1)
                end
                Core.Compiler.@_gc_preserve_end t
            end
            return b
        end)
    )
end

function Buffer{T}(::UndefInitializer, len::Int) where {T}
    ccall(:jl_new_buffer, Buffer{T}, (Any, Csize_t), Buffer{T}, len)
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
    ccall(:jl_new_buffer, DynamicBuffer{T}, (Any, Csize_t), DynamicBuffer{T}, len)
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

# this is computed directly from the pointer to ensure that the assumed effects aren't
# as strong as those for buffer variants that don't change sizes
function length(b::DynamicBuffer)
    @_terminates_locally_meta
    unsafe_load(unsafe_convert(Ptr{Int}, _pointer_from_buffer(b)))
end

firstindex(b::BufferType) = 1

lastindex(b::BufferType) = length(b)

keys(b::BufferType) = OneTo(length(b))

axes(b::BufferType) = (OneTo(length(b)),)
axes(b::BufferType, d::Integer) = d <= 1 ? OneTo(length(b)) : OneTo(1)

iterate(v::BufferType, i=1) = (length(v) < i ? nothing : (v[i], i + 1))

isempty(b::BufferType) = (length(b) == 0)

# @eval getindex(b::BufferType{T}, i::Int) where {T} = Core.bufref($(Expr(:boundscheck)), b, i)

# * `Int`s get converted to an unsigned integer when doing pointer math. Once bounds
#   checking is complete, we know that this won't throw an error but we have to help out
#   effect analysis by performing an explicit bitcast
# * we keep conversion of pointers close to variant type look-up for inference (in `_get_variant_ptr`)
#   Otherwise, inference falls apart quickly pointer conversion becomes very costly
function getindex(b::BufferType{T}, i::Int) where {T}
    @inline
    lyt = ElementLayout(T)
    elsz = lyt.elsize
    if lyt.kind === BOX_KIND
        @boundscheck (1 <= i <= length(b)) || throw(BoundsError(b, i))
        t = @_gc_preserve_begin b
        p = unsafe_load(unsafe_convert(Ptr{Ptr{Ptr{Cvoid}}}, _pointer_from_buffer(b) + PTR_SIZE))
        p = p + ((bitcast(UInt, i) - UInt(1)) * elsz)
        out = ccall(:jl_value_ptr, Any, (Ptr{Cvoid},), unsafe_load(p))
        @_gc_preserve_end t
        return out
    elseif lyt.kind === UNION_KIND
        len = length(b)
        @boundscheck (1 <= i <= len) || throw(BoundsError(b, i))
        offset = bitcast(UInt, i) - UInt(1)
        t = @_gc_preserve_begin b
        data_start = unsafe_load(unsafe_convert(Ptr{Ptr{T}}, _pointer_from_buffer(b) + PTR_SIZE))
        p = _get_variant_ptr(data_start, offset, bitcast(UInt, len), elsz) + (offset * elsz)
        out = unsafe_load(p)
        @_gc_preserve_end t
        return out
    elseif lyt.kind === BOOL_KIND
        @boundscheck (1 <= i <= length(b)) || throw(BoundsError(b, i))
        idx0 = bitcast(UInt, i) - UInt(1)
        t = @_gc_preserve_begin b
        data_start = unsafe_load(unsafe_convert(Ptr{Ptr{UInt64}}, _pointer_from_buffer(b) + PTR_SIZE))
        out = unsafe_load(data_start + (idx >> 6)) & (UInt64(1) << (idx0 & 63)) !== UInt(0)
        @_gc_preserve_end t
        return out
    else
        @boundscheck (1 <= i <= length(b)) || throw(BoundsError(b, i))
        isdefined(T, :instance) && return getfield(T, :instance)
        t = @_gc_preserve_begin b
        p = unsafe_load(unsafe_convert(Ptr{Ptr{T}}, _pointer_from_buffer(b) + PTR_SIZE))
        out = unsafe_load(p + ((bitcast(UInt, i) - UInt(1)) * elsz))
        @_gc_preserve_end t
        return out
    end
end

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

# @eval setindex!(b::BufferType{T}, x, i::Int) where {T} =
#     Core.bufset($(Expr(:boundscheck)), b, convert(T, x)::T, i)

setindex!(b::BufferType{T}, v, i::Int) where {T} = setindex!(b, convert(T, v), i)
function setindex!(b::BufferType{T}, v::T, i::Int) where {T}
    lyt = ElementLayout(T)
    elsz = lyt.elsize
    if lyt.kind === BOX_KIND
        @boundscheck (1 <= i <= length(b)) || throw(BoundsError(b, i))
        t1 = @_gc_preserve_begin v
        t2 = @_gc_preserve_begin b
        bptr = _pointer_from_buffer(b)
        data = unsafe_load(unsafe_convert(Ptr{Ptr{Ptr{Cvoid}}}, bptr + PTR_SIZE))
        if ismutable(v)
            vptr = pointer_from_objref(v)
        else
            vptr = unsafe_load(unsafe_convert(Ptr{Ptr{Cvoid}}, pointer_from_objref(RefValue{Any}(v))))
        end
        Core.Intrinsics.atomic_pointerset(data + ((bitcast(UInt, i) - UInt(1)) * elsz), vptr, :monotonic)
        # vptr = unsafe_convert(Ptr{Cvoid}, vptr)
        ccall(:jl_gc_queue_root, Cvoid, (Ptr{Cvoid}, Ptr{Cvoid}), bptr, vptr)
        @_gc_preserve_end t2
        @_gc_preserve_end t1
    elseif lyt.kind === BITS_KIND
        @boundscheck (1 <= i <= length(b)) || throw(BoundsError(b, i))
        if !isdefined(T, :instance)
            t1 = @_gc_preserve_begin v
            t2 = @_gc_preserve_begin b
            bptr = _pointer_from_buffer(b)
            data = unsafe_load(unsafe_convert(Ptr{Ptr{T}}, bptr + PTR_SIZE))
            unsafe_store!(unsafe_convert(Ptr{T}, data + ((bitcast(UInt, i) - UInt(1)) * elsz)), v)
            @_gc_preserve_end t2
            @_gc_preserve_end t1
        end
    elseif lyt.kind === HAS_PTR_KIND
        @boundscheck (1 <= i <= length(b)) || throw(BoundsError(b, i))
        t1 = @_gc_preserve_begin v
        t2 = @_gc_preserve_begin b
        bptr = _pointer_from_buffer(b)
        data = unsafe_load(unsafe_convert(Ptr{Ptr{Ptr{Cvoid}}}, bptr + PTR_SIZE))
        vptr = unsafe_convert(Ptr{Ptr{Cvoid}}, pointer_from_objref(Ref(v)))
        memmove_ptrs!(data + ((bitcast(UInt, i) - UInt(1)) * elsz), vptr, elsz)
        ccall(:jl_gc_queue_multiroot, Cvoid, (Ptr{Cvoid}, Ptr{Cvoid}), bptr, vptr)
        @_gc_preserve_end t2
        @_gc_preserve_end t1
    elseif lyt.kind === UNION_KIND
        len = length(b)
        @boundscheck (1 <= i <= len) || throw(BoundsError(b, i))
        t1 = @_gc_preserve_begin v
        t2 = @_gc_preserve_begin b
        offset = (bitcast(UInt, i) - UInt(1))
        bptr = _pointer_from_buffer(b)
        data_ptr = unsafe_load(unsafe_convert(Ptr{Ptr{UInt8}}, bptr + PTR_SIZE))
        vt = typeof(v)
        unsafe_store!(data_ptr + (elsz * len) + offset, _variant_to_tag(T, vt))
        if !isdefined(vt, :instance)
            unsafe_store!(unsafe_convert(Ptr{vt}, data_ptr) + (offset * elsz), v)
        end
        @_gc_preserve_end t2
        @_gc_preserve_end t1
    elseif lyt.kind === BOOL_KIND
        @boundscheck (1 <= i <= length(b) || throw(BoundsError(b, i)))
        idx0 = bitcast(UInt, i) - UInt(1)
        t = @_gc_preserve_begin b
        chunk_ptr = unsafe_load(unsafe_convert(Ptr{Ptr{UInt64}}, _pointer_from_buffer(b) + PTR_SIZE))
        chunk_ptr = chunk_ptr + (idx >> UInt(6))
        if v
            unsafe_store!(unsafe_load(chunk_ptr) | (UInt64(1) << (idx0 & 63)))
        else
            unsafe_store!(unsafe_load(chunk_ptr) & (~(UInt64(1) << (idx0 & 63))))
        end
        @_gc_preserve_end t
    end
    return nothing
end

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
function isassigned(b::BufferType{T}, i::Int) where {T}
    @inline
    @boundscheck 1 <= i <= length(b) || return false
    lyt = ElementLayout(T)
    if lyt.kind === BOX_KIND
        t = @_gc_preserve_begin b
        data_start = unsafe_load(unsafe_convert(Ptr{Ptr{Ptr{Cvoid}}}, _pointer_from_buffer(b) + PTR_SIZE))
        return unsafe_load(data_start + ((bitcast(UInt, i) - UInt(1)) * lyt.elsize)) !== C_NULL
        @_gc_preserve_end t
    elseif lyt.kind === HAS_PTR_KIND
        t = @_gc_preserve_begin b
        bptr = _pointer_from_buffer(b)
        data_i = unsafe_load(unsafe_convert(Ptr{Ptr{Ptr{Cvoid}}}, bptr + PTR_SIZE))
        data_i = data_i + ((bitcast(UInt, i) - UInt(1)) * lyt.elsize)
        off = fieldoffset(T, unsafe_load(unsafe_convert(Ptr{DataTypeLayout}, T.layout)).firstptr + 1)
        return unsafe_load(data_i + off) !== C_NULL
        @_gc_preserve_end t
    else
        return true
    end
end

function ==(v1::BufferType, v2::BufferType)
    length(v1) == length(v2) || return false
    for i in 1:length(v1)
        v1[i] == v2[i] || return false
    end
    return true
end

function copy(b::BufferType{T}) where {T}
    len = length(b)
    dst = similar(b, len)
    unsafe_copyto!(dst, 1, b, 1, len)
    return dst
end

# this is only needed to avoid possible ambiguities with methods added in some packages
function copyto!(dest::BufferType{T}, doffs::Integer, src::BufferType{T}, soffs::Integer, n::Integer) where T
    return _copyto_impl!(dest, doffs, src, soffs, n)
end
function _copyto_impl!(dest::BufferType, doffs::BufferType, src::Array, soffs::Integer, n::Integer)
    n == 0 && return dest
    n > 0 || _throw_argerror("Number of elements to copy must be nonnegative.")
    @boundscheck checkbounds(dest, doffs:doffs+n-1)
    @boundscheck checkbounds(src, soffs:soffs+n-1)
    unsafe_copyto!(dest, doffs, src, soffs, n)
    return dest
end

# TODO does HAS_PTR_KIND need special management for the pointers?
#  - (this doesn't seem to be the case in array.c)
function unsafe_copyto!(dest::Union{Buffer{T}, DynamicBuffer{T}}, doffs, src::BufferType{T}, soffs, n) where T
    lyt = ElementLayout(T)
    elsz = UInt(lyt.elsize)
    t1 = @_gc_preserve_begin dest
    t2 = @_gc_preserve_begin src
    doffset = (doffs - 1) * elsz
    soffset = (soffs - 1) * elsz
    dst_obj = _pointer_from_buffer(dest)
    src_obj = _pointer_from_buffer(src)
    nbytes = (n * elsz)
    if lyt.kind === BOX_KIND
        dst_data = unsafe_load(unsafe_convert(Ptr{Ptr{Ptr{Cvoid}}}, dst_obj +
            Core.sizeof(Ptr{Cvoid}))) + doffset
        src_start = unsafe_load(unsafe_convert(Ptr{Ptr{Ptr{Cvoid}}}, src_obj +
            Core.sizeof(Ptr{Cvoid}))) + soffset
        src_stop = src_start + nbytes
        if (dst_data < src_start || dst_data > src_stop)
            valp = Intrinsics.atomic_pointerref(src_start, :acquire)
            Intrinsics.atomic_pointerset(dst_data, valp, :monotonic)
            dst_data += Core.sizeof(Ptr{Cvoid})
            src_start += Core.sizeof(Ptr{Cvoid})
            while src_start < src_stop
                valp = Core.Intrinsics.atomic_pointerref(src_start, :acquire)
                Core.Intrinsics.atomic_pointerset(dst_data, valp, :monotonic)
                ccall(:jl_gc_queue_root, Cvoid, (Ptr{Cvoid}, Ptr{Cvoid}), dst_obj, valp)
                dst_data += Core.sizeof(Ptr{Cvoid})
                src_start += Core.sizeof(Ptr{Cvoid})
            end
        else
            src_stop -= Core.sizeof(Ptr{Cvoid})
            dst_data += (nbytes - Core.sizeof(Ptr{Cvoid}))
            while src_start <= src_stop
                valp = Intrinsics.atomic_pointerref(src_stop, :acquire)
                Intrinsics.atomic_pointerset(dst_data, valp, :monotonic)
                ccall(:jl_gc_queue_root, Cvoid, (Ptr{Cvoid}, Ptr{Cvoid}), dst_obj, valp)
                dst_data -= Core.sizeof(Ptr{Cvoid})
                src_stop -= Core.sizeof(Ptr{Cvoid})
            end
        end
    elseif lyt.kind === UNION_KIND
        destp = unsafe_load(unsafe_convert(Ptr{Ptr{Cvoid}}, dst_obj + Core.sizeof(Ptr{Cvoid})))
        srcp = unsafe_load(unsafe_convert(Ptr{Ptr{Cvoid}}, src_obj + Core.sizeof(Ptr{Cvoid})))
        memmove(destp + doffset, srcp + soffset, nbytes)
        # copy selector bytes
        memmove(destp + (length(dest) * elsz) + doffset, srcp + (length(src) * elsz) + soffset, n)
    elseif lyt.kind === BOOL_KIND
        destp = unsafe_load(unsafe_convert(Ptr{Ptr{Cvoid}}, dst_obj + Core.sizeof(Ptr{Cvoid})))
        srcp = unsafe_load(unsafe_convert(Ptr{Ptr{Cvoid}}, src_obj + Core.sizeof(Ptr{Cvoid})))
        # FIXME BOOL_KIND unsafe_copyto!
    else
        destp = unsafe_load(unsafe_convert(Ptr{Ptr{Cvoid}}, dst_obj + Core.sizeof(Ptr{Cvoid})))
        srcp = unsafe_load(unsafe_convert(Ptr{Ptr{Cvoid}}, src_obj + Core.sizeof(Ptr{Cvoid})))
        memmove(destp + doffset, srcp + soffset, nbytes)
    end
    @_gc_preserve_end t2
    @_gc_preserve_end t1
    return dest
end

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

# function unsafe_grow_at!(b::DynamicBuffer, i::Integer, delta::Integer, len::Integer=length(b))
#     ccall(:jl_buffer_grow_at_end, Cvoid, (Any, UInt, UInt, UInt), b, i - 1, delta, len)
# end

# function unsafe_delete_at!(b::DynamicBuffer, i::Integer, delta::Integer, len::Integer=length(b))
#     ccall(:jl_buffer_del_at_end, Cvoid, (Any, UInt, UInt, UInt), b, i - 1, delta, len)
# end

function unsafe_delete_at!(b::DynamicBuffer{T}, i::UInt, delta::UInt) where {T}
    lyt = ElementLayout(T)
    elsz = lyt.elsize
    offset = (i - UInt(1))
    stop = offset + delta
    t = @_gc_preserve_begin b
    bptr = _pointer_from_buffer(b)
    lenptr = unsafe_convert(Ptr{Csize_t}, bptr)
    len = unsafe_load(lenptr)
    if len > stop
        nmoved = len - stop
        data = unsafe_convert(Ptr{Ptr{Cvoid}}, bptr + Core.sizeof(Ptr{Cvoid}))
        if lyt.kind === UNION_KIND
            typetag_data = unsafe_convert(Ptr{UInt8}, data) + (len * elsz)
            memmove(typetag_data + offset, typetag_data + stop, nmoved)
        end
        data + (offset * elsz)
        data + (stop * elsz)
        nbytes = (len - stop) * elsz
        if lyt.hasptr
            memmove_ptrs!(data + (offset * elsz), data + (stop * elsz), nmoved * elsz)
        else
            memmove(data + (offset * elsz), data + (stop * elsz), nmoved * elsz)
        end
    end
    if lyt.elsize === UInt(1) && lyt.kind === BITS_KIND
        unsafe_store!(unsafe_convert(Ptr{UInt8}, data + len), 0x00)
    end
    unsafe_store!(lenptr, len - delta)
    @_gc_preserve_end t
    return nothing
end

function unsafe_grow_at!(b::DynamicBuffer{T}, i::UInt, delta::UInt) where {T}
    lyt = ElementLayout(T)
    t = @_gc_preserve_begin b
    bptr = _pointer_from_buffer(b)
    lenptr = unsafe_convert(Ptr{Csize_t}, bptr)
    data_ptr = unsafe_convert(Ptr{Ptr{Cvoid}}, bptr + Core.sizeof(Ptr{Cvoid}))
    old_data = unsafe_load(data_ptr)
    oldlen = unsafe_load(lenptr)
    newlen = delta + oldlen
    idx = i - UInt(1)
    old_nbytes = oldlen * lyt.elsize
    new_nbytes = newlen * lyt.elsize
    if old_nbytes > ARRAY_INLINE_NBYTES
        # already malloc'd, use realloc
        new_data = ccall(:jl_gc_managed_realloc, Cvoid, (Ptr{Cvoid}, Csize_t, Csize_t, Cint, Any),
             old_data, new_nbytes, old_nbytes, isaligned, bptr)
        has_newbuf = false
    else
        if new_nbytes >= MALLOC_THRESH
            new_data = ccall(:jl_gc_managed_malloc, Ptr{Cvoid}, (Csize_t,), new_nbytes)
            ccall(:jl_gc_track_malloced_buffer, Cvoid, (Any, Any), Core.getptls(), bptr)
        else # TODO bit tag as unmarked
            btag = ccall(:jl_get_buff_tag, Ptr{Cvoid}, ())
            new_data = ccall(:jl_gc_alloc, Ptr{Cvoid}, (Any, UInt, Any),
                Core.getptls(), new_nbytes, btag);
            ccall(:jl_gc_wb_bufnew, Cvoid, (Ptr{Cvoid}, Ptr{Cvoid}, Csize_t),
                bptr, new_data, new_nbytes)
        end
        has_newbuf = true
    end
    if BUFFER_IMPL_NULL && elsize === UInt(1)
        memset(new_data + old_nbytes - 1, Int32(0), new_nbytes - old_nbytes + 1);
    end
    nbinc = delta * lyt.elsize;
    has_gap = oldlen > idx
    if has_newbuf
        nb1 = idx * lyt.elsize
        memcpy!(new_data, old_data, nb1)
        if lyt.kind === UNION_KIND
            memcpy(newtypetagdata, typetagdata, idx)
            if has_gap
                memcpy!(newtypetagdata + idx + delta, typetagdata + idx, len - idx)
            end
            memset(newtypetagdata + idx, Int32(0), delta)
        end
        if has_gap
            memcpy!(new_data + nb1 + nbinc, old_data + nb1, oldlen * lyt.elsize - nb1)
        end
    else
        if lyt.kind === UNION_KIND
            typetagdata = new_data + old_nbytes;
            if has_gap
                memmove(newtypetagdata + idx + delta, typetagdata + idx, oldlen - idx);
            end
            memmove(newtypetagdata, typetagdata, idx)
            memset(newtypetagdata + idx, Int32(0), delta)
        end
        if has_gap
            nb1 = idx * lyt.elsize
            if lyt.hasptr
                memmove_ptrs!(new_data + nb1 + nbinc,
                new_data + nb1, oldlen * lyt.elsize - nb1)
            else
                memmove(new_data + nb1 + nbinc, new_data + nb1, oldlen * lyt.elsize - nb1)
            end
        end
    end
    unsafe_store!(data_ptr, new_data)
    unsafe_store!(lenptr, newlen)
    if lyt.requires_initialization
        memset(new_data + idx * lyt.elsize, Int32(0), delta * lyt.elsize)
    end
    @_gc_preserve_end t
    return nothing
end


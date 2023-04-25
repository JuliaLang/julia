# This file is a part of Julia. License is MIT: https://julialang.org/license

const BUFFER_IMPL_NULL = false
const N_CALL_CACHE = UInt(4096)
const ARRAY_INLINE_NBYTES = UInt(2048 * sizeof(Int))
const ARRAY_CACHE_ALIGN_THRESHOLD = UInt(2048)
const MALLOC_THRESH = UInt(1048576)
const CACHE_BYTE_ALIGNMENT = UInt(64)
const SMALL_BYTE_ALIGNMENT = UInt(16)
const JL_BUFFER_SIZEOF = UInt(Core.sizeof(UInt) + Core.sizeof(Ptr{Cvoid}))

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
            elsz = alignment = UInt(Core.sizeof(Ptr{Cvoid}))
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
        dst += Core.sizeof(Ptr{Cvoid})
        src_start += Core.sizeof(Ptr{Cvoid})
        while src_start < src_stop
            Core.Intrinsics.atomic_pointerset(dst, Core.Intrinsics.atomic_pointerref(src_start, :acquire), :monotonic)
            dst += Core.sizeof(Ptr{Cvoid})
            src_start += Core.sizeof(Ptr{Cvoid})
        end
    else
        src_stop -= Core.sizeof(Ptr{Cvoid})
        dst += (nbytes - Core.sizeof(Ptr{Cvoid}))
        Core.Intrinsics.atomic_pointerset(dst, Core.Intrinsics.atomic_pointerref(src_stop, :acquire), :monotonic)
        while src_start <= src_stop
            Core.Intrinsics.atomic_pointerset(dst, Core.Intrinsics.atomic_pointerref(src_stop, :acquire), :monotonic)
            dst -= Core.sizeof(Ptr{Cvoid})
            src_stop -= Core.sizeof(Ptr{Cvoid})
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
        #=:terminates_globally=#true,
        #=:terminates_locally=#false,
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
        function $(BT){T}(::UndefInitializer, len::Int) where {T}
            lyt = Core.Compiler.ElementLayout(T)
            Intrinsics.ule_int(0, len) || throw(ArgumentError("buffer type cannot have a negative length"))
            ulen = bitcast(UInt, len)
            data_size = Intrinsics.mul_int(ulen, lyt.elsize)
            if Intrinsics.not_int(lyt.kind === Core.Compiler.BOX_KIND)
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
            if Intrinsics.ule_int(data_size, Core.Compiler.ARRAY_INLINE_NBYTES)
                if Intrinsics.ule_int(data_size, Core.Compiler.ARRAY_CACHE_ALIGN_THRESHOLD)
                    object_size = Core.and_int(Core.add_int(object_size, Core.sub_int(Core.Compiler.CACHE_BYTE_ALIGNMENT, UInt(1))),
                        Intrinsics.neg_int(Core.Compiler.CACHE_BYTE_ALIGNMENT))
                elseif Intrinsics.not_int(lyt.kind === Core.Compiler.BOX_KIND && Intrinsics.ule_int(UInt(4), lyt.elsize))
                    object_size = Core.and_int(Core.add_int(object_size, Core.sub_int(Core.Compiler.SMALL_BYTE_ALIGNMENT, UInt(1))),
                        Intrinsics.neg_int(Core.Compiler.SMALL_BYTE_ALIGNMENT))
                end
                object_size = Core.add_int(object_size, data_size)
                b = ccall(:jl_gc_alloc_buffer_inline, $(BT){T}, (Any, UInt, UInt), $(BT){T}, ulen, object_size)
            else
                b = ccall(:jl_gc_malloc_buffer, $(BT){T}, (Any, UInt, UInt, UInt), $(BT){T}, ulen, data_size, object_size)
            end
            if lyt.requires_initialization
                data = ccall(:jl_buffer_ptr, Ptr{Cvoid}, (Any,), b)
                ccall(:memset, Ptr{Cvoid}, (Ptr{Cvoid}, Int32, UInt), data, Int32(0), data_size)
            elseif Core.Compiler.BUFFER_IMPL_NULL && lyt.elsize === UInt(1)
                Intrinsics.pointerset(Core.Compiler.:+(Core.Compiler.unsafe_convert(Ptr{UInt8}, unsafe_convert(Ptr{T}, b)), data_size), 0x00, 1, 1)
            end
            return b
        end
    ))
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

DynamicBuffer(a::AbstractArray{T}) where {T} = DynamicBuffer{T}(a)
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

# * `Core.Compiler.get_buffer_index` is used for the eval of `getindex` because it needs to
#   be part of the explicit boot process to avoid issues when there's resizing.
# * `Int`s get converted to an unsigned integer when doing pointer math. Once bounds
#   checking is complete, we know that this won't throw an error but we have to help out
#   effect analysis by performing an explicit bitcast
# * we keep conversion of pointers close to variant type look-up for inference (in `_get_variant_ptr`)
#   Otherwise, inference falls apart quickly pointer conversion becomes very costly
function get_buffer_index(boundscheck::Bool, b::BufferType{T}, i::Int) where {T}
    @inline
    lyt = ElementLayout(T)
    elsz = lyt.elsize
    if lyt.kind === BOX_KIND
        boundscheck && ((1 <= i <= length(b)) || throw(BoundsError(b, i)))
        t = @_gc_preserve_begin b
        p = unsafe_load(unsafe_convert(Ptr{Ptr{Ptr{Cvoid}}}, _pointer_from_buffer(b) + Core.sizeof(Ptr{Cvoid})))
        p = p + ((bitcast(UInt, i) - UInt(1)) * elsz)
        out = ccall(:jl_value_ptr, Any, (Ptr{Cvoid},), unsafe_load(p))
        @_gc_preserve_end t
        return out
    elseif lyt.kind === UNION_KIND
        len = length(b)
        boundscheck && ((1 <= i <= len) || throw(BoundsError(b, i)))
        offset = bitcast(UInt, i) - UInt(1)
        t = @_gc_preserve_begin b
        data_start = unsafe_load(unsafe_convert(Ptr{Ptr{T}}, _pointer_from_buffer(b) + Core.sizeof(Ptr{Cvoid})))
        p = _get_variant_ptr(data_start, offset, bitcast(UInt, len), elsz) + (offset * elsz)
        out = unsafe_load(p)
        @_gc_preserve_end t
        return out
    elseif lyt.kind === BOOL_KIND
        boundscheck && ((1 <= i <= length(b)) || throw(BoundsError(b, i)))
        idx0 = bitcast(UInt, i) - UInt(1)
        t = @_gc_preserve_begin b
        data_start = unsafe_load(unsafe_convert(Ptr{Ptr{UInt64}}, _pointer_from_buffer(b) + Core.sizeof(Ptr{Cvoid})))
        out = unsafe_load(data_start + (idx >> 6)) & (UInt64(1) << (idx0 & 63)) !== UInt(0)
        @_gc_preserve_end t
        return out
    else
        boundscheck && ((1 <= i <= length(b)) || throw(BoundsError(b, i)))
        isdefined(T, :instance) && return getfield(T, :instance)
        t = @_gc_preserve_begin b
        p = unsafe_load(unsafe_convert(Ptr{Ptr{T}}, _pointer_from_buffer(b) + Core.sizeof(Ptr{Cvoid})))
        out = unsafe_load(p + ((bitcast(UInt, i) - UInt(1)) * elsz))
        @_gc_preserve_end t
        return out
    end
end
eval(:(getindex(b::BufferType{T}, i::Int) where {T} = (@inline; Core.Compiler.get_buffer_index($(Expr(:boundscheck)), b, i))))
function unsafe_getindex(b::BufferType{T}, i::Int) where {T}
    @inline
    get_buffer_index(false, b, i)
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

setindex!(b::BufferType{T}, v, i::Int) where {T} = setindex!(b, convert(T, v), i)
function set_buffer_index!(boundscheck::Bool, b::BufferType{T}, v::T, i::Int) where {T}
    lyt = ElementLayout(T)
    elsz = lyt.elsize
    if lyt.kind === BOX_KIND
        boundscheck && ((1 <= i <= length(b)) || throw(BoundsError(b, i)))
        t1 = @_gc_preserve_begin v
        t2 = @_gc_preserve_begin b
        bptr = _pointer_from_buffer(b)
        data = unsafe_load(unsafe_convert(Ptr{Ptr{Ptr{Cvoid}}}, bptr + Core.sizeof(Ptr{Cvoid})))
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
        boundscheck && ((1 <= i <= length(b)) || throw(BoundsError(b, i)))
        if !isdefined(T, :instance)
            t1 = @_gc_preserve_begin v
            t2 = @_gc_preserve_begin b
            bptr = _pointer_from_buffer(b)
            data = unsafe_load(unsafe_convert(Ptr{Ptr{T}}, bptr + Core.sizeof(Ptr{Cvoid})))
            unsafe_store!(unsafe_convert(Ptr{T}, data + ((bitcast(UInt, i) - UInt(1)) * elsz)), v)
            @_gc_preserve_end t2
            @_gc_preserve_end t1
        end
    elseif lyt.kind === HAS_PTR_KIND
        boundscheck && ((1 <= i <= length(b)) || throw(BoundsError(b, i)))
        t1 = @_gc_preserve_begin v
        t2 = @_gc_preserve_begin b
        bptr = _pointer_from_buffer(b)
        data = unsafe_load(unsafe_convert(Ptr{Ptr{Ptr{Cvoid}}}, bptr + Core.sizeof(Ptr{Cvoid})))
        vptr = unsafe_convert(Ptr{Ptr{Cvoid}}, pointer_from_objref(Ref(v)))
        memmove_ptrs!(data + ((bitcast(UInt, i) - UInt(1)) * elsz), vptr, elsz)
        ccall(:jl_gc_queue_multiroot, Cvoid, (Ptr{Cvoid}, Ptr{Cvoid}), bptr, vptr)
        @_gc_preserve_end t2
        @_gc_preserve_end t1
    elseif lyt.kind === UNION_KIND
        len = length(b)
        boundscheck && ((1 <= i <= len) || throw(BoundsError(b, i)))
        t1 = @_gc_preserve_begin v
        t2 = @_gc_preserve_begin b
        offset = (bitcast(UInt, i) - UInt(1))
        bptr = _pointer_from_buffer(b)
        data_ptr = unsafe_load(unsafe_convert(Ptr{Ptr{UInt8}}, bptr + Core.sizeof(Ptr{Cvoid})))
        vt = typeof(v)
        unsafe_store!(data_ptr + (elsz * len) + offset, _variant_to_tag(T, vt))
        if !isdefined(vt, :instance)
            unsafe_store!(unsafe_convert(Ptr{vt}, data_ptr) + (offset * elsz), v)
        end
        @_gc_preserve_end t2
        @_gc_preserve_end t1
    elseif lyt.kind === BOOL_KIND
        boundscheck && ((1 <= i <= length(b)) || throw(BoundsError(b, i)))
        idx0 = bitcast(UInt, i) - UInt(1)
        t = @_gc_preserve_begin b
        chunk_ptr = unsafe_load(unsafe_convert(Ptr{Ptr{UInt64}}, _pointer_from_buffer(b) + Core.sizeof(Ptr{Cvoid})))
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
eval(:(setindex!(b::BufferType{T}, v::T, i::Int) where {T} = (@inline; Core.Compiler.set_buffer_index!($(Expr(:boundscheck)), b, v, i))))
function unsafe_setindex!(b::BufferType{T}, v::T, i::Int) where {T}
    @inline
    set_buffer_index!(false, b, v, i)
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
        data_start = unsafe_load(unsafe_convert(Ptr{Ptr{Ptr{Cvoid}}},
            _pointer_from_buffer(b) + Core.sizeof(Ptr{Cvoid})))
        return unsafe_load(data_start + (bitcast(UInt, i - 1) * lyt.elsize)) !== C_NULL
        @_gc_preserve_end t
    elseif lyt.kind === HAS_PTR_KIND
        t = @_gc_preserve_begin b
        bptr = _pointer_from_buffer(b)
        data_i = unsafe_load(unsafe_convert(Ptr{Ptr{Ptr{Cvoid}}}, bptr + Core.sizeof(Ptr{Cvoid})))
        data_i = data_i + (bitcast(UInt, i - 1) * lyt.elsize)
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
    return _copyto_buffer_impl!(dest, doffs, src, soffs, n)
end
function _copyto_buffer_impl!(dest::BufferType, doffs::Integer, src::BufferType, soffs::Integer, n::Integer)
    n == 0 && return dest
    n > 0 || _throw_argerror("Number of elements to copy must be nonnegative.")
    @boundscheck checkbounds(dest, doffs:doffs+n-1)
    @boundscheck checkbounds(src, soffs:soffs+n-1)
    unsafe_copyto!(dest, doffs, src, soffs, n)
    return dest
end

# TODO does HAS_PTR_KIND need special management for the pointers?
#  - (this doesn't seem to be the case in array.c)
function unsafe_copyto!(dst::Union{Buffer{T}, DynamicBuffer{T}}, doffs, src::BufferType{T}, soffs, n) where T
    lyt = ElementLayout(T)
    elsz = UInt(lyt.elsize)
    t1 = @_gc_preserve_begin dst
    t2 = @_gc_preserve_begin src
    doffset = (doffs - 1) * elsz
    soffset = (soffs - 1) * elsz
    dst_obj = _pointer_from_buffer(dst)
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
        memmove(destp + (length(dst) * elsz) + doffset, srcp + (length(src) * elsz) + soffset, n)
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
    return dst
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

function unsafe_grow_at!(b::DynamicBuffer, i::Integer, delta::Integer, len::Integer=length(b))
    ccall(:jl_buffer_grow_at, Cvoid, (Any, Csize_t, Csize_t, Csize_t), b, i - 1, delta, len)
end

function insert!(b::DynamicBuffer{T}, i::Integer, item) where {T}
    @noinline
    len = length(b)
    @boundscheck (1 <= i <= (len + 1)) || throw(BoundsError(b, i))
    # Throw convert error before changing the shape of the array
    _item = item isa T ? item : convert(T, item)::T
    unsafe_grow_at!(b, i, 1, len)
    set_buffer_index!(false, b, item, i)
    return b
end

function push!(b::DynamicBuffer{T}, item) where {T}
    @noinline
    itemT = item isa T ? item : convert(T, item)::T
    oldlen = length(b)
    newlen = oldlen + 1
    unsafe_grow_at!(b, newlen, 1, oldlen)
    unsafe_setindex!(b, itemT, newlen)
    return b
end

function pushfirst!(b::DynamicBuffer{T}, item) where {T}
    @noinline
    itemT = item isa T ? item : convert(T, item)::T
    unsafe_grow_at!(b, 1, 1, length(b))
    unsafe_setindex!(b, itemT, 1)
    return b
end

# TODO append!
function append!(b::DynamicBuffer, items)
    itemindices = eachindex(items)
    n = length(itemindices)
    len = length(b)
    unsafe_grow_at!(b, len + 1, n, len)
    copyto!(b, length(b)-n+1, items, first(itemindices), n)
    return b
end

# TODO prepend!
function prepend!(b::DynamicBuffer, items)
    itemindices = eachindex(items)
    n = length(itemindices)
    len = length(b)
    unsafe_grow_at!(b, 1, n, len)
    if b === items
        copyto!(b, 1, items, n+1, n)
    else
        copyto!(b, 1, items, first(itemindices), n)
    end
    return b
end

function unsafe_delete_at!(b::DynamicBuffer, i::Integer, delta::Integer, len::Integer=length(b))
    ccall(:jl_buffer_delete_at, Cvoid, (Any, Csize_t, Csize_t, Csize_t), b, i - 1, delta, len)
end

# TODO keepat!

function deleteat!(b::DynamicBuffer, i::Integer)
    i isa Bool && depwarn("passing Bool as an index is deprecated", :deleteat!)
    len = length(b)
    @boundscheck (1 <= i <= len) || throw(BoundsError(b, i))
    unsafe_delete_at!(b, i, 1, len)
    return b
end

function deleteat!(b::DynamicBuffer, r::AbstractUnitRange{<:Integer})
    n = length(b)
    start = first(r)
    stop = last(r)
    @boundscheck (1 <= start && stop <= stop) || throw(BoundsError(b, r))
    unsafe_delete_at!(b, start, stop - start + 1, n)
    return b
end


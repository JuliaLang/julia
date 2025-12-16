# Used in fast_reinterpret below for size-0 structs
macro _new(T)
    return Expr(:new, esc(T))
end

@inline function fast_reinterpret(::Type{T}, x) where {T}
    @assert isconcretetype(T) && isbitstype(T) && isbitstype(typeof(x))
    x isa T && return x
    if Base.packedsize(T) != Base.packedsize(typeof(x))
        throw(ArgumentError("""
            Expected matching packed sizes: `$(Base.packedsize(T)) != $(Base.packedsize(typeof(x)))`
        """))
    end
    # Special-case for zero-sized types, which for some reason don't get compiled away:
    if Base.packedsize(T) == 0
        return @_new(T)
    # # For types with many fields (>32), avoid the recursive unrolling overhead
    # # by falling back to reinterpret. This prevents allocation explosion for
    # # large tuples like Tuple{UInt8, Int64, Vararg{UInt8, 100}}.
    # elseif fieldcount(typeof(x)) > 32 || fieldcount(T) > 32
    #     return @inline reinterpret(T, x)
    # For packed structs, our bytecast is slightly faster:
    elseif Base.packedsize(typeof(x)) == sizeof(x) && sizeof(T) == sizeof(x)
        return byte_cast(T, x)
    else
        return fast_reinterpret_padded_src_to_dst(T, x)
    end
end

# I'm not sure why reinterpret doesn't produce as good of code as this for same-size
# bit-types, but it doesn't. We need to investigate that with the Julia compiler team.
# For now, we'll use this instead as a faster reinterpret.
# REQUIRES `T` and `V` to be the same size and have no padding.
@inline function byte_cast(::Type{T}, x::V) where {T,V}
    r = Ref{T}()
    @assert sizeof(T) == sizeof(V)
    GC.@preserve r begin
        # lint-disable-next-line: An `unsafe_` function should be called only from an `unsafe_` function.
        unsafe_store!(reinterpret(Ptr{V}, pointer_from_objref(r)), x)
        return r[]
    end
end

struct PackedRegion
    offset::Int
    size::Int
end

# Recursively compute the packed regions of each field of T, and then for T itself,
# combine them into a list of (offset, size) tuples.
Base.@assume_effects :foldable function packed_regions(::Type{T}) where {T}
    field_regions = _packed_regions(T, 0)
    # Merge adjacent regions
    return _compress_packed_regions(field_regions)
end
Base.@assume_effects :foldable function _compress_packed_regions(field_regions)
    merged_regions = PackedRegion[]
    for region in field_regions
        if !isempty(merged_regions)
            last_region = merged_regions[end]
            if last_region.offset + last_region.size == region.offset
                # Merge with last region
                merged_regions[end] = PackedRegion(last_region.offset, last_region.size + region.size)
                continue
            end
        end
        push!(merged_regions, region)
    end
    return Core.svec(merged_regions...)
end
Base.@assume_effects :foldable function _packed_regions(::Type{T}, baseoffset::Int) where {T}
    if Base.packedsize(T) == 0
        return PackedRegion[]
    end
    if isprimitivetype(T) || fieldcount(T) == 0
        return [PackedRegion(baseoffset, Base.sizeof(T))]
    end

    regions = sizehint!(PackedRegion[], fieldcount(T)) # Rough guess: at least one per field
    stack = Tuple{Type, Int}[(T, baseoffset)]

    while !isempty(stack)
        current_type, current_offset = pop!(stack)

        for i = 1:fieldcount(current_type)
            offset = current_offset + Int(fieldoffset(current_type, i))
            fT = fieldtype(current_type, i)::Type
            if isprimitivetype(fT) || fieldcount(fT) == 0
                push!(regions, PackedRegion(offset, Base.sizeof(fT)))
            else
                push!(stack, (fT, offset))
            end
        end
    end
    # TODO: Why is it better to return Core.svec here and below instead of an Array?
    return Core.svec(regions...)
end

Base.@assume_effects :foldable function match_packed_regions(SRC_regions, DST_regions)
    @assert !isempty(SRC_regions) && !isempty(DST_regions)
    out_regions = sizehint!(Tuple{Int,Int,Int}[],
        max(length(SRC_regions), length(DST_regions)))

    src_idx, dst_idx = 1, 1
    src_padding = @inbounds SRC_regions[src_idx]
    dst_padding = @inbounds DST_regions[dst_idx]
    src_off, src_rem = src_padding.offset, src_padding.size
    dst_off, dst_rem = dst_padding.offset, dst_padding.size

    while src_idx <= length(SRC_regions) && dst_idx <= length(DST_regions)
        # Copy the minimum of what's remaining in current src and dst regions
        n = min(src_rem, dst_rem)
        push!(out_regions, (src_off, dst_off, n))

        # Advance offsets and reduce remaining bytes
        src_off += n
        dst_off += n
        src_rem -= n
        dst_rem -= n

        # Move to next source region if current one is exhausted
        if src_rem == 0
            src_idx += 1
            if src_idx <= length(SRC_regions)
                src_padding = @inbounds SRC_regions[src_idx]
                src_off, src_rem = src_padding.offset, src_padding.size
            end
        end

        # Move to next destination region if current one is exhausted
        if dst_rem == 0
            dst_idx += 1
            if dst_idx <= length(DST_regions)
                dst_padding = @inbounds DST_regions[dst_idx]
                dst_off, dst_rem = dst_padding.offset, dst_padding.size
            end
        end
    end

    return Tuple(out_regions)
end

Base.@assume_effects :foldable function packing_equal(SRC_regions, DST_regions)
    return SRC_regions == DST_regions
end

@inline function fast_reinterpret_padded_src_to_dst(::Type{DST}, x::SRC) where {DST, SRC}
    SRC_regions = packed_regions(SRC)
    DST_regions = packed_regions(DST)

    # OPTIMIZATION: If the packed regions match exactly, we can do a single memcpy.
    # Apparently this is *always* faster, even if the padding ratio is >80%. (I might have
    # expected that at very high padding ratios, it's cheaper to copy only the real bytes,
    # but the number of generated instructions outweighs the savings from skipping padding
    # for very large structs.)
    if packing_equal(SRC_regions, DST_regions)
        return byte_cast(DST, x)
    end

    offsets_to_copy = match_packed_regions(SRC_regions, DST_regions)

    src_ref = Ref{SRC}(x)
    dest_ref = Ref{DST}()
    GC.@preserve src_ref dest_ref begin
        # Cast to "bytes pointers" since all our operations are in bytes
        src_ptr = reinterpret(Ptr{UInt8}, pointer_from_objref(src_ref))
        dst_ptr = reinterpret(Ptr{UInt8}, pointer_from_objref(dest_ref))

        _fast_reinterpret_padded_src_to_dst(
            dst_ptr,
            src_ptr,
            offsets_to_copy,
        )
    end
    return dest_ref[]
end
@inline _fast_reinterpret_padded_src_to_dst(::Ptr, ::Ptr, ::Tuple{}) = nothing
@inline function _fast_reinterpret_padded_src_to_dst(
    dst_ptr::Ptr,
    src_ptr::Ptr,
    offsets_to_copy::Tuple,
)::Nothing
    (src_offset, dst_offset, bytes_to_copy) = @inbounds Base.first(offsets_to_copy)
    unsafe_copyto!(
        dst_ptr + dst_offset,
        src_ptr + src_offset,
        bytes_to_copy,
    )
    return _fast_reinterpret_padded_src_to_dst(
        dst_ptr,
        src_ptr,
        Base.tail(offsets_to_copy),
    )
    return nothing
end

@inline function _reinterpret(::Type{Out}, x::In) where {Out, In}
    # handle non-primitive types
    isbitstype(Out) || throw(ArgumentError(LazyString("Target type for `reinterpret` must be isbits. Got ", Out)))
    isbitstype(In) || throw(ArgumentError(LazyString("Source type for `reinterpret` must be isbits. Got ", In)))
    inpackedsize = _packedsize(In)
    outpackedsize = _packedsize(Out)
    inpackedsize == outpackedsize ||
        throw(ArgumentError(LazyString("Packed sizes of types ", Out, " and ", In,
            " do not match; got ", outpackedsize, " and ", inpackedsize, ", respectively.")))
    # Special-case for zero-sized types, which for some reason don't get compiled away:
    if sizeof(Out) == 0
        return _new_empty(Out)
    elseif _packedsize(typeof(x)) == sizeof(x) && sizeof(Out) == sizeof(x)
        return byte_cast(Out, x)
    else
        return _reinterpret_padded_src_to_dst(Out, x)
    end
end

@eval @inline function _new_empty(T)
    return $(Expr(:new, :(T)))
end

# Simple memcopy between two types of the same size.
@inline function byte_cast(::Type{T}, x::V) where {T,V}
    r = Ref{T}()
    @assert sizeof(T) == sizeof(V) "Cannot byte_cast($(T), $(V)). Expected: $(sizeof(T)) == $(sizeof(V))"
    GC.@preserve r begin
        unsafe_store!(reinterpret(Ptr{V}, pointer_from_objref(r)), x)
        return r[]
    end
end

@inline function _byte_cast_smaller_src(::Type{T}, x::V) where {T,V}
    r = Ref{T}()
    @assert sizeof(T) >= sizeof(V) "Cannot _byte_cast_smaller_src($(T), $(V)). Expected: $(sizeof(T)) >= $(sizeof(V))"
    GC.@preserve r begin
        unsafe_store!(reinterpret(Ptr{V}, pointer_from_objref(r)), x)
        return r[]
    end
end
@inline function _byte_cast_smaller_dst(::Type{T}, x::V) where {T,V}
    r = Ref{T}()
    xr = Ref{V}(x)
    @assert sizeof(T) <= sizeof(V) "Cannot _byte_cast_smaller_dst($(T), $(V)). Expected: $(sizeof(T)) <= $(sizeof(V))"
    GC.@preserve r xr begin
        unsafe_store!(reinterpret(Ptr{T}, pointer_from_objref(r)),
            unsafe_load(reinterpret(Ptr{T}, pointer_from_objref(xr))))
        return r[]
    end
end

# Our own copy of packedsize, so that we don't need to compile both padding() and
# _packed_regions(); instead we can reuse just _packed_regions().
Base.@assume_effects :foldable function _packedsize(::Type{T}) where {T}
    @assert isconcretetype(T)
    out = 0
    for p in _packed_regions(T, 0)
        out += p.size
    end
    return out
end
struct PackedRegion
    offset::Int
    size::Int
end

# Recursively compute the packed regions of each field of T, and then for T itself,
# combine them into a list of (offset, size) tuples.
Base.@assume_effects :foldable function packed_regions(::Type{T}) where {T}
    @assert isconcretetype(T)
    field_regions = _packed_regions(T, 0)
    # Merge adjacent regions
    return _compress_packed_regions(field_regions)
end
@inline function _compress_packed_regions(field_regions)
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
# PERF: For some reason this quite dramatically outperforms Tuple{DataType, Int} in the
# stack, and i'm not sure why. When it's a tuple, each tuple was being heap-allocated.
struct PackedRegionStackEntry
    T::DataType
    offset::Int
end
@inline function _packed_regions(T::DataType, baseoffset::Int)
    if Base.sizeof(T) == 0
        return PackedRegion[]
    end
    if isprimitivetype(T) || fieldcount(T) == 0
        return [PackedRegion(baseoffset, Base.sizeof(T))]
    end

    # PERFORMANCE OPTIMIZATION: Use depthfirst traversal with an explicit stack rather than
    # implementing the traversal with recursion. This avoids the overhead from recursive
    # calls inside the compiler, where we need to create a MethodInstance for each call.
    # The outcome is equivalent to recursively calling _packed_regions for each field.
    regions = sizehint!(PackedRegion[], fieldcount(T)) # Rough guess: at least one per field
    stack = PackedRegionStackEntry[PackedRegionStackEntry(T, baseoffset)]

    # For each type: if it's packed, add its region; else, push its fields on the stack.
    while !isempty(stack)
        entry = pop!(stack)
        current_type, current_offset = entry.T, entry.offset
        if isprimitivetype(current_type) || fieldcount(current_type) == 0
            push!(regions, PackedRegion(current_offset, Base.sizeof(current_type)))
        else
            # Push the fields in reverse order so that we process them in the original order
            for i = fieldcount(current_type):-1:1
                offset = current_offset + Int(fieldoffset(current_type, i))
                fT = fieldtype(current_type, i)::DataType
                push!(stack, PackedRegionStackEntry(fT, offset))
            end
        end
    end
    # We return an svec here, since functions marked `:foldable` must return egal outputs for
    # egal inputs, which means they shouldn't return a mutable type.
    return Core.svec(regions...)
end

Base.@assume_effects :foldable function match_packed_regions(
    SRC_regions::Core.SimpleVector,
    DST_regions::Core.SimpleVector,
)
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

Base.@assume_effects :foldable function _packing_equal(
    SRC_regions::Core.SimpleVector,
    DST_regions::Core.SimpleVector
)
    return SRC_regions == DST_regions
end

@inline function _reinterpret_padded_src_to_dst(::Type{DST}, x::SRC) where {DST, SRC}
    SRC_regions = packed_regions(SRC)
    DST_regions = packed_regions(DST)

    # OPTIMIZATION: If the packed regions match exactly, we can do a single memcpy.
    # This is always faster than the region-matching logic - computers are very fast at
    # copying bytes, and this can possibly be entirely compiled away to only a type-cast.
    if _packing_equal(SRC_regions, DST_regions)
        if sizeof(DST) < sizeof(SRC)
            return _byte_cast_smaller_dst(DST, x)
        elseif sizeof(DST) > sizeof(SRC)
            return _byte_cast_smaller_src(DST, x)
        else  # Sizes are equal
            return byte_cast(DST, x)
        end
    end

    offsets_to_copy = match_packed_regions(SRC_regions, DST_regions)

    src_ref = Ref{SRC}(x)
    dest_ref = Ref{DST}()
    GC.@preserve src_ref dest_ref begin
        # Cast to "bytes pointers" since all our operations are in bytes
        src_ptr = reinterpret(Ptr{UInt8}, pointer_from_objref(src_ref))
        dst_ptr = reinterpret(Ptr{UInt8}, pointer_from_objref(dest_ref))

        _reinterpret_padded_src_to_dst(
            dst_ptr,
            src_ptr,
            offsets_to_copy,
        )
    end
    return dest_ref[]
end
@inline _reinterpret_padded_src_to_dst(::Ptr, ::Ptr, ::Tuple{}) = nothing
@inline function _reinterpret_padded_src_to_dst(
    dst_ptr::Ptr,
    src_ptr::Ptr,
    offsets_to_copy::Tuple,
)::Nothing
    (src_offset, dst_offset, bytes_to_copy) = @inbounds Base.first(offsets_to_copy)
    Base.memcpy(
        dst_ptr + dst_offset,
        src_ptr + src_offset,
        bytes_to_copy,
    )
    return _reinterpret_padded_src_to_dst(
        dst_ptr,
        src_ptr,
        Base.tail(offsets_to_copy),
    )
    return nothing
end

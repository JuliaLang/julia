# Used in fast_reinterpret below for size-0 structs
macro _new(T)
    return Expr(:new, esc(T))
end

@inline function fast_reinterpret(::Type{T}, x) where {T}
    @assert isconcretetype(T) && isbitstype(T) && isbitstype(typeof(x))
    x isa T && return x
    @assert Base.packedsize(T) == Base.packedsize(typeof(x)) """
        Expected matching packed sizes: `$(Base.packedsize(T)) != $(Base.packedsize(typeof(x)))`
    """
    # Special-case for zero-sized types, which for some reason don't get compiled away:
    if Base.packedsize(T) == 0
        return @_new(T)
    # Fast path for primitive types (no fields): use LLVM's builtin reinterpret
    # which compiles to a no-op for same-size primitives
    elseif fieldcount(T) == 0 && fieldcount(typeof(x)) == 0
        return @inline reinterpret(T, x)
    # For types with many fields (>32), avoid the recursive unrolling overhead
    # by falling back to reinterpret. This prevents allocation explosion for
    # large tuples like Tuple{UInt8, Int64, Vararg{UInt8, 100}}.
    elseif fieldcount(typeof(x)) > 32 || fieldcount(T) > 32
        return @inline reinterpret(T, x)
    # For packed structs, our bytecast is slightly faster:
    elseif Base.packedsize(typeof(x)) == sizeof(x) && sizeof(T) == sizeof(x)
        return byte_cast(T, x)
    # For non-packed (padded) structs, our fast_reinterpret is much faster:
    elseif Base.ispacked(T)
        return fast_reinterpret_packed_dest(T, x)
    elseif Base.ispacked(typeof(x))
        return fast_reinterpret_packed_src(T, x)
    else
        # Our fast_reinterpret currently doesn't handle padded => padded reinterpret.
        # This is still slow.
        @assert Base.ispacked(T) && Base.ispacked(typeof(x)) """
            Padded => Padded is not currently supported in fast_reinterpret.
              Unexpected types: `!(ispacked($(T)) && ispacked($(typeof(x))))`"""
        return reinterpret(T, x)
    end
end

# I'm not sure why reinterpret doesn't produce as good of code as this for same-size
# bit-types, but it doesn't. We need to investigate that with the Julia compiler team.
# For now, we'll use this instead as a faster reinterpret.
# REQUIRES `T` and `V` to be the same size and have no padding.
@inline function byte_cast(::Type{T}, x::V) where {T,V}
    @assert Base.packedsize(V) == sizeof(x) == sizeof(T)
    r = Ref{T}()
    @assert sizeof(T) == sizeof(V)
    GC.@preserve r begin
        # lint-disable-next-line: An `unsafe_` function should be called only from an `unsafe_` function.
        unsafe_store!(reinterpret(Ptr{V}, pointer_from_objref(r)), x)
        return r[]
    end
end

@inline function fast_reinterpret_packed_dest(::Type{DEST}, value::T) where {DEST, T}
    dest = Ref{DEST}()
    GC.@preserve dest begin
        ptr = pointer_from_objref(dest)
        store_fields_packed!(ptr, value, Val(fieldcount(T)))
    end
    return dest[]
end

# The `Val{I}` argument is the number of fields remaining to store, and it controls the
# recursion unrolling. Using a recursive Integer will unroll even for large structs, whereas
# using a Tuple type will stop unrolling around 32 fields, due to large-tuple heuristics.
@inline store_fields_packed!(::Ptr{Cvoid}, ::T, ::Val{0}) where {T} = nothing
@inline function store_fields_packed!(blob::Ptr{Cvoid}, value::T, ::Val{I}) where {T, I}
    # lint-disable-next-line: An `unsafe_` function should be called only from an `unsafe_` function.
    _unsafe_store_field!(blob, value, Val(I))
    store_fields_packed!(blob, value, Val(I-1))
    return nothing
end

@inline function _unsafe_store_field!(blob::Ptr{Cvoid}, value::T, ::Val{FIELD}) where {T, FIELD}
    FT = fieldtype(T, FIELD)
    fp = blob + packed_offset(T, FIELD)
    fv = @inbounds(getfield(value, FIELD))
    # If FT is packed, we can directly memcopy it, otherwise we recursively
    # pack and reinterpret it into place as well.
    if Base.ispacked(FT)
        unsafe_store!(reinterpret(Ptr{FT}, fp), fv)
    else
        store_fields_packed!(fp, fv, Val(fieldcount(FT)))
    end
    return nothing
end

# Return the offset in bytes for the i-th field of T, if it were bit-packed.
@inline function packed_offset(::Type{T}, i::Int) where {T}
    # This is the sum of the packed sizes of all preceding fields. We use this
    # `+(0, (sizes)...)` formulation since it fully unrolls in type inference.
    +(0, (Base.packedsize(fieldtype(T, j)) for j in 1:(i-1))...)
end

@inline function fast_reinterpret_packed_src(::Type{PADDED}, value::T) where {PADDED, T}
    src = Ref{T}(value)
    dest = Ref{PADDED}()
    GC.@preserve src begin
        src_ptr = pointer_from_objref(src)
        dest_ptr = reinterpret(Ptr{PADDED}, pointer_from_objref(dest))
        copy_fields_with_padding!(dest_ptr, src_ptr, PADDED, Val(fieldcount(PADDED)))
    end
    return dest[]
end

# The `Val{I}` argument is the number of fields remaining to store, and it controls the
# recursion unrolling. Using a recursive Integer will unroll even for large structs, whereas
# using a Tuple type will stop unrolling around 32 fields, due to large-tuple heuristics.
@inline copy_fields_with_padding!(::Ptr{T}, ::Ptr{Cvoid}, ::Type{T}, ::Val{0}) where {T} = nothing
@inline function copy_fields_with_padding!(dest::Ptr{T}, src::Ptr{Cvoid}, ::Type{T}, ::Val{I}) where {T, I}
    # lint-disable-next-line: An `unsafe_` function should be called only from an `unsafe_` function.
    _unsafe_load_into_field!(dest, src, T, Val(I))
    copy_fields_with_padding!(dest, src, T, Val(I-1))
    return nothing
end

@inline function _unsafe_load_into_field!(dest::Ptr{T}, src::Ptr{Cvoid}, ::Type{T}, ::Val{FIELD}) where {T, FIELD}
    FT = fieldtype(T, FIELD)
    fp = src + packed_offset(T, FIELD)
    dest_ptr = reinterpret(Ptr{FT}, dest + fieldoffset(T, FIELD))
    # If FT is packed, we can directly memcopy it, otherwise we recursively
    # reinterpret and unpack it into place as well.
    if Base.ispacked(FT)
        fv = unsafe_load(reinterpret(Ptr{FT}, fp))
        unsafe_store!(dest_ptr, fv)
    else
        copy_fields_with_padding!(dest_ptr, fp, FT, Val(fieldcount(FT)))
    end
    return nothing
end

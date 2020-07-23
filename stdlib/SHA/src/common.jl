# This file is a part of Julia. License is MIT: https://julialang.org/license

# Common update and digest functions which work across SHA1 and SHA2

# update! takes in variable-length data, buffering it into blocklen()-sized pieces,
# calling transform!() when necessary to update the internal hash state.
"""
    update!(context, data[, datalen])

Update the SHA context with the bytes in data. See also [`digest!`](@ref) for
finalizing the hash.

# Examples
```julia-repl
julia> ctx = SHA1_CTX()
SHA1 hash state

julia> update!(ctx, b"data to to be hashed")
```
"""
function update!(context::T, data::U, datalen=length(data)) where {T<:SHA_CTX, U<:AbstractBytes}
    # We need to do all our arithmetic in the proper bitwidth
    UIntXXX = typeof(context.bytecount)

    # Process as many complete blocks as possible
    0 ≤ datalen ≤ length(data) || throw(BoundsError(data, firstindex(data)+datalen-1))
    len = convert(UIntXXX, datalen)
    data_idx = convert(UIntXXX, firstindex(data)-1)
    usedspace = context.bytecount % blocklen(T)
    while len - data_idx + usedspace >= blocklen(T)
        # Fill up as much of the buffer as we can with the data given us
        copyto!(context.buffer, usedspace + 1, data, data_idx + 1, blocklen(T) - usedspace)

        transform!(context)
        context.bytecount += blocklen(T) - usedspace
        data_idx += blocklen(T) - usedspace
        usedspace = convert(UIntXXX, 0)
    end

    # There is less than a complete block left, but we need to save the leftovers into context.buffer:
    if len > data_idx
        copyto!(context.buffer, usedspace + 1, data, data_idx + 1, len - data_idx)
        context.bytecount += len - data_idx
    end
end

# Pad the remainder leaving space for the bitcount
function pad_remainder!(context::T) where T<:SHA_CTX
    usedspace = context.bytecount % blocklen(T)
    # If we have anything in the buffer still, pad and transform that data
    if usedspace > 0
        # Begin padding with a 1 bit:
        context.buffer[usedspace+1] = 0x80
        usedspace += 1

        # If we have room for the bitcount, then pad up to the short blocklen
        if usedspace <= short_blocklen(T)
            for i = 1:(short_blocklen(T) - usedspace)
                context.buffer[usedspace + i] = 0x0
            end
        else
            # Otherwise, pad out this entire block, transform it, then pad up to short blocklen
            for i = 1:(blocklen(T) - usedspace)
                context.buffer[usedspace + i] = 0x0
            end
            transform!(context)
            for i = 1:short_blocklen(T)
                context.buffer[i] = 0x0
            end
        end
    else
        # If we don't have anything in the buffer, pad an entire shortbuffer
        context.buffer[1] = 0x80
        for i = 2:short_blocklen(T)
            context.buffer[i] = 0x0
        end
    end
end


# Clear out any saved data in the buffer, append total bitlength, and return our precious hash!
# Note: SHA3_CTX has a more specialised method
"""
    digest!(context)

Finalize the SHA context and return the hash as array of bytes (Array{Uint8, 1})

# Examples
```julia-repl
julia> ctx = SHA1_CTX()
SHA1 hash state

julia> update!(ctx, b"data to to be hashed")

julia> digest!(ctx)
20-element Array{UInt8,1}:
 0x83
 0xe4
 ⋮
 0x89
 0xf5
```
"""
function digest!(context::T) where T<:SHA_CTX
    pad_remainder!(context)
    # Store the length of the input data (in bits) at the end of the padding
    bitcount_idx = div(short_blocklen(T), sizeof(context.bytecount)) + 1
    pbuf = Ptr{typeof(context.bytecount)}(pointer(context.buffer))
    unsafe_store!(pbuf, bswap(context.bytecount * 8), bitcount_idx)

    # Final transform:
    transform!(context)

    # Return the digest
    return reinterpret(UInt8, bswap!(context.state))[1:digestlen(T)]
end

# Common update and digest functions which work across SHA1 and SHA2

# update! takes in variable-length data, buffering it into blocklen()-sized pieces,
# calling transform!() when necessary to update the internal hash state.
function update!{T<:Union{SHA1_CTX,SHA2_CTX,SHA3_CTX}}(context::T, data::Array{UInt8,1})
    # We need to do all our arithmetic in the proper bitwidth
    UIntXXX = typeof(context.bytecount)

    # Process as many complete blocks as possible
    len = UIntXXX(length(data))
    data_idx = UIntXXX(0)
    usedspace = context.bytecount % blocklen(T)
    while len - data_idx + usedspace >= blocklen(T)
        # Fill up as much of the buffer as we can with the data given us
        for i in 1:(blocklen(T) - usedspace)
            context.buffer[usedspace + i] = data[data_idx + i]
        end

        transform!(context)
        context.bytecount += blocklen(T) - usedspace
        data_idx += blocklen(T) - usedspace
        usedspace = UIntXXX(0)
    end

    # There is less than a complete block left, but we need to save the leftovers into context.buffer:
    if len > data_idx
        for i = 1:(len - data_idx)
            context.buffer[usedspace + i] = data[data_idx + i]
        end
        context.bytecount += len - data_idx
    end
end


# Clear out any saved data in the buffer, append total bitlength, and return our precious hash!
function digest!{T<:Union{SHA1_CTX,SHA2_CTX}}(context::T)
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

    # Store the length of the input data (in bits) at the end of the padding
    bitcount_buffer = reinterpret(typeof(context.bytecount), context.buffer)
    bitcount_idx = div(short_blocklen(T), sizeof(context.bytecount))+1
    bitcount_buffer[bitcount_idx] = bswap(context.bytecount*8)

    # Final transform:
    transform!(context)

    # Return the digest
    return reinterpret(UInt8, bswap!(context.state))[1:digestlen(T)]
end

# Common update and digest functions which work across SHA1 and SHA2

# update! takes in variable-length data, buffering it into blocklen()-sized pieces,
# calling transform!() when necessary to update the internal hash state.
function update!{T<:SHA_CTX}(context::T, data::Array{UInt8,1})
    if length(data) == 0
        return
    end

    data_idx = 0
    len = convert(typeof(context.bytecount), length(data))
    usedspace = context.bytecount % blocklen(T)
    if usedspace > 0
        # Calculate how much free space is available in the buffer
        freespace = blocklen(T) - usedspace

        if len >= freespace
            # Fill the buffer completely and process it
            for i in 1:freespace
                context.buffer[usedspace + i] = data[data_idx + i]
            end

            # Round bytecount up to the nearest blocklen
            context.bytecount += freespace
            data_idx += freespace
            len -= freespace
            transform!(context)
        else
            # The buffer is not yet full
            for i = 1:len
                context.buffer[usedspace + i] = data[data_idx + i]
            end
            context.bytecount += len
            return
        end
    end


    # Process as many complete blocks as possible, now that the buffer is full
    data_idx = one(len)
    while len - (data_idx - 1) >= blocklen(T)
        for i in 1:blocklen(T)
            context.buffer[i] = data[data_idx + i - 1]
        end
        transform!(context)
        data_idx += blocklen(T)
    end
    context.bytecount += (data_idx - 1)

    # If there are leftovers, save them in buffer until next update!() or digest!()
    if data_idx < len
        # There's left-overs, so save 'em
        for i = 1:(len - data_idx + 1)
            context.buffer[i] = data[data_idx + i - 1]
        end
        context.bytecount += (len - data_idx + 1)
    end
end


# Clear out any saved data in the buffer, append total bitlength, and return our precious hash!
function digest!{T<:SHA_CTX}(context::T)
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

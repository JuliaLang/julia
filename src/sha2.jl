# A mod function which maps [0..N-1] to [1..N]
mod1(i, N) = mod(i-1, N) + 1

function transform!{T<:SHA_CTX_SMALL}(context::T, data::Array{Uint32,1}, startidx = 0)
    W256 = context.buffer
    
    # Initialize registers with the prev. intermediate value
    a = context.state[1]
    b = context.state[2]
    c = context.state[3]
    d = context.state[4]
    e = context.state[5]
    f = context.state[6]
    g = context.state[7]
    h = context.state[8]

    for j = 1:16
        @inbounds begin
            W256[j] = bswap(data[startidx + j])

            # Apply the SHA-256 compression function to update a..h
            T1 = h + Sigma1_256(e) + Ch(e, f, g) + K256[j] + W256[j];
            T2 = Sigma0_256(a) + Maj(a, b, c);
            h = g;
            g = f;
            f = e;
            e = uint32(d + T1);
            d = c;
            c = b;
            b = a;
            a = uint32(T1 + T2);
        end
    end

    for j = 17:64
        @inbounds begin
            # Part of the message block expansion:
            s0 = W256[mod1(j + 1, 16)];
            s0 = sigma0_256(s0);
            s1 = W256[mod1(j + 14, 16)];
            s1 = sigma1_256(s1);
            
            # Apply the SHA-256 compression function to update a..h
            T1 = h + Sigma1_256(e) + Ch(e, f, g) + K256[j] + (W256[mod1(j, 16)] += s1 + W256[mod1(j + 9,16)] + s0);
            T2 = Sigma0_256(a) + Maj(a, b, c);
            h = g;
            g = f;
            f = e;
            e = uint32(d + T1);
            d = c;
            c = b;
            b = a;
            a = uint32(T1 + T2);
        end
    end

    # Compute the current intermediate hash value
    context.state[1] += a
    context.state[2] += b
    context.state[3] += c
    context.state[4] += d
    context.state[5] += e
    context.state[6] += f
    context.state[7] += g
    context.state[8] += h
end


function transform!(context::SHA_CTX_BIG, data::Array{Uint64,1}, startidx = 0)
    W512 = context.buffer # reinterpret( Uint64, context.buffer )

    # Initialize registers with the prev. intermediate value
    a = context.state[1]
    b = context.state[2]
    c = context.state[3]
    d = context.state[4]
    e = context.state[5]
    f = context.state[6]
    g = context.state[7]
    h = context.state[8]

    for j = 1:16
        @inbounds begin
            W512[j] = bswap(data[startidx + j])

            # Apply the SHA-512 compression function to update a..h
            T1 = h + Sigma1_512(e) + Ch(e, f, g) + K512[j] + W512[j];
            T2 = Sigma0_512(a) + Maj(a, b, c);
            h = g;
            g = f;
            f = e;
            e = d + T1;
            d = c;
            c = b;
            b = a;
            a = T1 + T2;
        end
    end

    for j = 17:80
        @inbounds begin
            # Part of the message block expansion:
            s0 = W512[mod1(j + 1, 16)];
            s0 = sigma0_512(s0);
            s1 = W512[mod1(j+14, 16)];
            s1 = sigma1_512(s1);

            # Apply the SHA-512 compression function to update a..h
            T1 = h + Sigma1_512(e) + Ch(e, f, g) + K512[j] + (W512[mod1(j, 16)] += s1 + W512[mod1(j+9, 16)] + s0);
            T2 = Sigma0_512(a) + Maj(a, b, c);
            h = g;
            g = f;
            f = e;
            e = d + T1;
            d = c;
            c = b;
            b = a;
            a = T1 + T2;
        end
    end

    # Compute the current intermediate hash value
    context.state[1] += a
    context.state[2] += b
    context.state[3] += c
    context.state[4] += d
    context.state[5] += e
    context.state[6] += f
    context.state[7] += g
    context.state[8] += h
end


function update!(context::SHA_CTX, data::Array{Uint8,1})
    if length(data) == 0
        return
    end

    data_idx = 0
    len = convert(typeof(context.bitcount), length(data))
    usedspace = div(context.bitcount, 8) % context.blocklen
    freespace = 0
    if usedspace > 0
        # Calculate how much free space is available in the buffer 
        freespace = context.blocklen - usedspace

        if len >= freespace
            # Fill the buffer completely and process it
            buffer = reinterpret(Uint8, context.buffer)
            for i = 1:freespace
                buffer[usedspace + i] = data[data_idx + i]
            end
            context.bitcount += freespace * 8
            len -= freespace
            data_idx += freespace
            transform!(context, context.buffer)
        else
            # The buffer is not yet full
            buffer = reinterpret(Uint8, context.buffer)
            for i = 1:len
                buffer[usedspace + i] = data[data_idx + i]
            end
            context.bitcount += len * 8
            return
        end
    end


    # Process as many complete blocks as possible.
    data_idx = one(len)
    data_conv = reinterpret(eltype(context.state), data)
    while len - (data_idx - 1) >= context.blocklen
        data_idx_conv = div(data_idx,sizeof(eltype(context.state)))
        transform!(context, data_conv, data_idx_conv)
        data_idx += context.blocklen
    end
    context.bitcount += (data_idx - 1)*8    

    if data_idx < len
        # There's left-overs, so save 'em 
        buffer = reinterpret(Uint8, context.buffer)
        for i = 1:(len - data_idx + 1)
            buffer[i] = data[data_idx + i - 1]
        end
        context.bitcount += (len - data_idx + 1) * 8
    end
end

# add in a convenience method for strings
update!(context::SHA_CTX, str::ASCIIString) = update!(context, str.data)

function digest!(context::SHA_CTX)
    usedspace = div(context.bitcount, 8) % context.blocklen;
    context.bitcount = bswap(context.bitcount)
    buffer = reinterpret(Uint8, context.buffer)
    
    if usedspace > 0
        # Begin padding with a 1 bit:
        buffer[usedspace+1] = 0x80
        usedspace += 1
        if usedspace <= context.short_blocklen
            # Set-up for the last transform:
            for i = 1:(context.short_blocklen - usedspace)
                buffer[usedspace + i] = 0x0
            end
        else
            for i = 1:(context.blocklen - usedspace)
                buffer[usedspace + i] = 0x0
            end
            # Do second-to-last transform:
            transform!(context, context.buffer)
            # And set-up for the last transform:
            for i = 1:context.short_blocklen
                buffer[i] = 0x0
            end
        end
    else
        # Set-up for the last transform:
        for i = 1:context.short_blocklen
            buffer[i] = 0x0
        end
        # Begin padding with a 1 bit:
        buffer[1] = 0x80
    end

    # Store the length of the input data (in bits)
    bitcount_buffer = reinterpret(typeof(context.bitcount), context.buffer)
    bitcount_idx = div(context.short_blocklen, sizeof(context.bitcount))+1
    bitcount_buffer[bitcount_idx] = context.bitcount

    # Final transform:
    transform!(context, context.buffer)

    # Return the digest
    return reinterpret(Uint8, bswap!(context.state))[1:context.digest_len]
end

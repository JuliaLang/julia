# This file is a part of Julia. License is MIT: https://julialang.org/license

function transform!(context::T) where {T<:Union{SHA2_224_CTX,SHA2_256_CTX}}
    pbuf = buffer_pointer(context)
    # Initialize registers with the previous intermediate values (our state)
    a = context.state[1]
    b = context.state[2]
    c = context.state[3]
    d = context.state[4]
    e = context.state[5]
    f = context.state[6]
    g = context.state[7]
    h = context.state[8]

    # Run initial rounds
    for j = 1:16
        @inbounds begin
            # We bitswap every input byte
            v = bswap(unsafe_load(pbuf, j))
            unsafe_store!(pbuf, v, j)

            # Apply the SHA-256 compression function to update a..h
            T1 = h + Sigma1_256(e) + Ch(e, f, g) + K256[j] + v
            T2 = Sigma0_256(a) + Maj(a, b, c)
            h = g
            g = f
            f = e
            e = UInt32(d + T1)
            d = c
            c = b
            b = a
            a = UInt32(T1 + T2)
        end
    end

    for j = 17:64
        @inbounds begin
            # Implicit message block expansion:
            s0 = unsafe_load(pbuf, mod1(j + 1, 16))
            s0 = sigma0_256(s0)
            s1 = unsafe_load(pbuf, mod1(j + 14, 16))
            s1 = sigma1_256(s1)

            # Apply the SHA-256 compression function to update a..h
            v = unsafe_load(pbuf, mod1(j, 16)) + s1 + unsafe_load(pbuf, mod1(j + 9, 16)) + s0
            unsafe_store!(pbuf, v, mod1(j, 16))
            T1 = h + Sigma1_256(e) + Ch(e, f, g) + K256[j] + v
            T2 = Sigma0_256(a) + Maj(a, b, c)
            h = g
            g = f
            f = e
            e = UInt32(d + T1)
            d = c
            c = b
            b = a
            a = UInt32(T1 + T2)
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


function transform!(context::Union{SHA2_384_CTX,SHA2_512_CTX})
    pbuf = buffer_pointer(context)
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
            v = bswap(unsafe_load(pbuf, j))
            unsafe_store!(pbuf, v, j)

            # Apply the SHA-512 compression function to update a..h
            T1 = h + Sigma1_512(e) + Ch(e, f, g) + K512[j] + v
            T2 = Sigma0_512(a) + Maj(a, b, c)
            h = g
            g = f
            f = e
            e = d + T1
            d = c
            c = b
            b = a
            a = T1 + T2
        end
    end

    for j = 17:80
        @inbounds begin
            # Implicit message block expansion:
            s0 = unsafe_load(pbuf, mod1(j + 1, 16))
            s0 = sigma0_512(s0)
            s1 = unsafe_load(pbuf, mod1(j + 14, 16))
            s1 = sigma1_512(s1)

            # Apply the SHA-512 compression function to update a..h
            v = unsafe_load(pbuf, mod1(j, 16)) + s1 + unsafe_load(pbuf, mod1(j + 9, 16)) + s0
            unsafe_store!(pbuf, v, mod1(j, 16))
            T1 = h + Sigma1_512(e) + Ch(e, f, g) + K512[j] + v
            T2 = Sigma0_512(a) + Maj(a, b, c)
            h = g
            g = f
            f = e
            e = d + T1
            d = c
            c = b
            b = a
            a = T1 + T2
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

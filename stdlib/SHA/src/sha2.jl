macro R1_16(j)

    ww = (:a, :b, :c, :d, :e, :f, :g, :h)

    a = ww[((17 - j) % 8) + 1]
    b = ww[((18 - j) % 8) + 1]
    c = ww[((19 - j) % 8) + 1]
    d = ww[((20 - j) % 8) + 1]
    e = ww[((21 - j) % 8) + 1]
    f = ww[((22 - j) % 8) + 1]
    g = ww[((23 - j) % 8) + 1]
    h = ww[((24 - j) % 8) + 1]


    return esc(quote
        # We byteswap every input byte
        v = bswap(unsafe_load(pbuf, $j))
        unsafe_store!(pbuf, v, $j)

        # Apply the SHA-256 compression function to update a..h
        T1 = $h + Sigma1_256($e) + Ch($e, $f, $g) + K256[$j] + v
        $h = Sigma0_256($a) + Maj($a, $b, $c)
        $d += UInt32(T1)
        $h += UInt32(T1)
    end)
end

macro R17_64(j)

    ww = (:a, :b, :c, :d, :e, :f, :g, :h)

    a = ww[((65 - j) % 8) + 1]
    b = ww[((66 - j) % 8) + 1]
    c = ww[((67 - j) % 8) + 1]
    d = ww[((68 - j) % 8) + 1]
    e = ww[((69 - j) % 8) + 1]
    f = ww[((70 - j) % 8) + 1]
    g = ww[((71 - j) % 8) + 1]
    h = ww[((72 - j) % 8) + 1]

    return esc(quote
        s0 = unsafe_load(pbuf, mod1($j + 1, 16))
        s0 = sigma0_256(s0)
        s1 = unsafe_load(pbuf, mod1($j + 14, 16))
        s1 = sigma1_256(s1)

        # Apply the SHA-256 compression function to update a..h
        v = unsafe_load(pbuf, mod1($j, 16)) + s1 + unsafe_load(pbuf, mod1($j + 9, 16)) + s0
        unsafe_store!(pbuf, v, mod1($j, 16))
        T1 = $h + Sigma1_256($e) + Ch($e, $f, $g) + K256[$j] + v
        $h = Sigma0_256($a) + Maj($a, $b, $c)
        $d += UInt32(T1)
        $h += UInt32(T1)
    end)
end

function transform!(context::T) where {T<:Union{SHA2_224_CTX,SHA2_256_CTX}}
    pbuf = buffer_pointer(context)
    # Initialize registers with the previous intermediate values (our state)
    a, b, c, d, e, f, g, h = context.state

    # Initial Rounds
    @R1_16( 1); @R1_16( 2); @R1_16( 3); @R1_16( 4); @R1_16( 5); @R1_16( 6); @R1_16( 7); @R1_16( 8);
    @R1_16( 9); @R1_16(10); @R1_16(11); @R1_16(12); @R1_16(13); @R1_16(14); @R1_16(15); @R1_16(16);


    # Other Rounds
    @R17_64(17); @R17_64(18); @R17_64(19); @R17_64(20); @R17_64(21); @R17_64(22); @R17_64(23); @R17_64(24);
    @R17_64(25); @R17_64(26); @R17_64(27); @R17_64(28); @R17_64(29); @R17_64(30); @R17_64(31); @R17_64(32);
    @R17_64(33); @R17_64(34); @R17_64(35); @R17_64(36); @R17_64(37); @R17_64(38); @R17_64(39); @R17_64(40);
    @R17_64(41); @R17_64(42); @R17_64(43); @R17_64(44); @R17_64(45); @R17_64(46); @R17_64(47); @R17_64(48);
    @R17_64(49); @R17_64(50); @R17_64(51); @R17_64(52); @R17_64(53); @R17_64(54); @R17_64(55); @R17_64(56);
    @R17_64(57); @R17_64(58); @R17_64(59); @R17_64(60); @R17_64(61); @R17_64(62); @R17_64(63); @R17_64(64);

    # Compute the current intermediate hash value
    @inbounds begin
        context.state[1] += a
        context.state[2] += b
        context.state[3] += c
        context.state[4] += d
        context.state[5] += e
        context.state[6] += f
        context.state[7] += g
        context.state[8] += h
    end
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

macro R1_16(j, T)

    ww = (:a, :b, :c, :d, :e, :f, :g, :h)

    a = ww[((81 - j) % 8) + 1]
    b = ww[((82 - j) % 8) + 1]
    c = ww[((83 - j) % 8) + 1]
    d = ww[((84 - j) % 8) + 1]
    e = ww[((85 - j) % 8) + 1]
    f = ww[((86 - j) % 8) + 1]
    g = ww[((87 - j) % 8) + 1]
    h = ww[((88 - j) % 8) + 1]

    if T == 512
        Sigma0 = :Sigma0_512
        Sigma1 = :Sigma1_512
        sigma0 = :sigma0_512
        sigma1 = :sigma1_512
        K = :K512
    elseif T == 256
        Sigma0 = :Sigma0_256
        Sigma1 = :Sigma1_256
        sigma0 = :sigma0_256
        sigma1 = :sigma1_256
        K = :K256
    end

    return esc(quote
        # We byteswap every input byte
        v = bswap(unsafe_load(pbuf, $j))
        unsafe_store!(pbuf, v, $j)

        # Apply the SHA-256 compression function to update a..h
        T1 = $h + $Sigma1($e) + Ch($e, $f, $g) + $K[$j] + v
        $h = $Sigma0($a) + Maj($a, $b, $c)
        $d += T1
        $h += T1
    end)
end

macro R17_80(j, T)

    ww = (:a, :b, :c, :d, :e, :f, :g, :h)

    a = ww[((81 - j) % 8) + 1]
    b = ww[((82 - j) % 8) + 1]
    c = ww[((83 - j) % 8) + 1]
    d = ww[((84 - j) % 8) + 1]
    e = ww[((85 - j) % 8) + 1]
    f = ww[((86 - j) % 8) + 1]
    g = ww[((87 - j) % 8) + 1]
    h = ww[((88 - j) % 8) + 1]

    if T == 512
        Sigma0 = :Sigma0_512
        Sigma1 = :Sigma1_512
        sigma0 = :sigma0_512
        sigma1 = :sigma1_512
        K = :K512
    elseif T == 256
        Sigma0 = :Sigma0_256
        Sigma1 = :Sigma1_256
        sigma0 = :sigma0_256
        sigma1 = :sigma1_256
        K = :K256
    end

    return esc(quote
        s0 = unsafe_load(pbuf, mod1($j + 1, 16))
        s0 = $sigma0(s0)
        s1 = unsafe_load(pbuf, mod1($j + 14, 16))
        s1 = $sigma1(s1)

        # Apply the SHA-256 compression function to update a..h
        v = unsafe_load(pbuf, mod1($j, 16)) + s1 + unsafe_load(pbuf, mod1($j + 9, 16)) + s0
        unsafe_store!(pbuf, v, mod1($j, 16))
        T1 = $h + $Sigma1($e) + Ch($e, $f, $g) + $K[$j] + v
        $h = $Sigma0($a) + Maj($a, $b, $c)
        $d += T1
        $h += T1
    end)
end

function transform!(context::Union{SHA2_224_CTX,SHA2_256_CTX})
    pbuf = buffer_pointer(context)
    # Initialize registers with the previous intermediate values (our state)
    a, b, c, d, e, f, g, h = context.state

    # Initial Rounds
    @R1_16( 1, 256); @R1_16( 2, 256); @R1_16( 3, 256); @R1_16( 4, 256);
    @R1_16( 5, 256); @R1_16( 6, 256); @R1_16( 7, 256); @R1_16( 8, 256);
    @R1_16( 9, 256); @R1_16(10, 256); @R1_16(11, 256); @R1_16(12, 256);
    @R1_16(13, 256); @R1_16(14, 256); @R1_16(15, 256); @R1_16(16, 256);

    # Other Rounds 64
    @R17_80(17, 256); @R17_80(18, 256); @R17_80(19, 256); @R17_80(20, 256);
    @R17_80(21, 256); @R17_80(22, 256); @R17_80(23, 256); @R17_80(24, 256);
    @R17_80(25, 256); @R17_80(26, 256); @R17_80(27, 256); @R17_80(28, 256);
    @R17_80(29, 256); @R17_80(30, 256); @R17_80(31, 256); @R17_80(32, 256);
    @R17_80(33, 256); @R17_80(34, 256); @R17_80(35, 256); @R17_80(36, 256);
    @R17_80(37, 256); @R17_80(38, 256); @R17_80(39, 256); @R17_80(40, 256);
    @R17_80(41, 256); @R17_80(42, 256); @R17_80(43, 256); @R17_80(44, 256);
    @R17_80(45, 256); @R17_80(46, 256); @R17_80(47, 256); @R17_80(48, 256);
    @R17_80(49, 256); @R17_80(50, 256); @R17_80(51, 256); @R17_80(52, 256);
    @R17_80(53, 256); @R17_80(54, 256); @R17_80(55, 256); @R17_80(56, 256);
    @R17_80(57, 256); @R17_80(58, 256); @R17_80(59, 256); @R17_80(60, 256);
    @R17_80(61, 256); @R17_80(62, 256); @R17_80(63, 256); @R17_80(64, 256);

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
    # Initialize registers with the previous intermediate values (our state)
    a, b, c, d, e, f, g, h = context.state

    # Initial Rounds
    @R1_16( 1, 512); @R1_16( 2, 512); @R1_16( 3, 512); @R1_16( 4, 512);
    @R1_16( 5, 512); @R1_16( 6, 512); @R1_16( 7, 512); @R1_16( 8, 512);
    @R1_16( 9, 512); @R1_16(10, 512); @R1_16(11, 512); @R1_16(12, 512);
    @R1_16(13, 512); @R1_16(14, 512); @R1_16(15, 512); @R1_16(16, 512);

    # Other Rounds 80
    @R17_80(17, 512); @R17_80(18, 512); @R17_80(19, 512); @R17_80(20, 512);
    @R17_80(21, 512); @R17_80(22, 512); @R17_80(23, 512); @R17_80(24, 512);
    @R17_80(25, 512); @R17_80(26, 512); @R17_80(27, 512); @R17_80(28, 512);
    @R17_80(29, 512); @R17_80(30, 512); @R17_80(31, 512); @R17_80(32, 512);
    @R17_80(33, 512); @R17_80(34, 512); @R17_80(35, 512); @R17_80(36, 512);
    @R17_80(37, 512); @R17_80(38, 512); @R17_80(39, 512); @R17_80(40, 512);
    @R17_80(41, 512); @R17_80(42, 512); @R17_80(43, 512); @R17_80(44, 512);
    @R17_80(45, 512); @R17_80(46, 512); @R17_80(47, 512); @R17_80(48, 512);
    @R17_80(49, 512); @R17_80(50, 512); @R17_80(51, 512); @R17_80(52, 512);
    @R17_80(53, 512); @R17_80(54, 512); @R17_80(55, 512); @R17_80(56, 512);
    @R17_80(57, 512); @R17_80(58, 512); @R17_80(59, 512); @R17_80(60, 512);
    @R17_80(61, 512); @R17_80(62, 512); @R17_80(63, 512); @R17_80(64, 512);
    @R17_80(65, 512); @R17_80(66, 512); @R17_80(67, 512); @R17_80(68, 512);
    @R17_80(69, 512); @R17_80(70, 512); @R17_80(71, 512); @R17_80(72, 512);
    @R17_80(73, 512); @R17_80(74, 512); @R17_80(75, 512); @R17_80(76, 512);
    @R17_80(77, 512); @R17_80(78, 512); @R17_80(79, 512); @R17_80(80, 512);

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

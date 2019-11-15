@inline function writefixed(buf, pos, v::T,
    precision=-1, plus=false, space=false, hash=false,
    decchar=UInt8('.'), trimtrailingzeros=false) where {T <: Base.IEEEFloat}
    @assert 0 < pos <= length(buf)
    x = Float64(v)
    neg = signbit(x)
    # special cases
    if x == 0
        if neg
            buf[pos] = UInt8('-')
            pos += 1
        elseif plus
            buf[pos] = UInt8('+')
            pos += 1
        elseif space
            buf[pos] = UInt8(' ')
            pos += 1
        end
        buf[pos] = UInt8('0')
        pos += 1
        if precision > 0
            buf[pos] = decchar
            pos += 1
            if trimtrailingzeros
                precision = 1
            end
            for _ = 1:precision
                buf[pos] = UInt8('0')
                pos += 1
            end
        elseif hash
            buf[pos] = decchar
            pos += 1
        end
        return pos
    elseif isnan(x)
        buf[pos] = UInt8('N')
        buf[pos + 1] = UInt8('a')
        buf[pos + 2] = UInt8('N')
        return pos + 3
    elseif !isfinite(x)
        if neg
            buf[pos] = UInt8('-')
            pos += 1
        elseif plus
            buf[pos] = UInt8('+')
            pos += 1
        elseif space
            buf[pos] = UInt8(' ')
            pos += 1
        end
        buf[pos] = UInt8('I')
        buf[pos + 1] = UInt8('n')
        buf[pos + 2] = UInt8('f')
        return pos + 3
    end

    bits = Core.bitcast(UInt64, x)
    mant = bits & MANTISSA_MASK
    exp = Int((bits >> 52) & EXP_MASK)

    if exp == 0
        e2 = 1 - 1023 - 52
        m2 = mant
    else
        e2 = exp - 1023 - 52
        m2 = (Int64(1) << 52) | mant
    end
    nonzero = false
    if neg
        buf[pos] = UInt8('-')
        pos += 1
    elseif plus
        buf[pos] = UInt8('+')
        pos += 1
    elseif space
        buf[pos] = UInt8(' ')
        pos += 1
    end
    if e2 >= -52
        idx = e2 < 0 ? 0 : indexforexp(e2)
        p10bits = pow10bitsforindex(idx)
        len = lengthforindex(idx)
        i = len - 1
        while i >= 0
            j = p10bits - e2
            #=@inbounds=# mula, mulb, mulc = POW10_SPLIT[POW10_OFFSET[idx + 1] + i + 1]
            digits = mulshiftmod1e9(m2 << 8, mula, mulb, mulc, j + 8)
            if nonzero
                pos = append_nine_digits(digits, buf, pos)
            elseif digits != 0
                olength = decimallength(digits)
                pos = append_n_digits(olength, digits, buf, pos)
                nonzero = true
            end
            i -= 1
        end
    end
    if !nonzero
        buf[pos] = UInt8('0')
        pos += 1
    end
    if precision > 0 || hash
        buf[pos] = decchar
        pos += 1
    end
    if e2 < 0
        idx = div(-e2, 16)
        blocks = div(precision, 9) + 1
        roundUp = 0
        i = 0
        if blocks <= MIN_BLOCK_2[idx + 1]
            i = blocks
            for _ = 1:precision
                buf[pos] = UInt8('0')
                pos += 1
            end
        elseif i < MIN_BLOCK_2[idx + 1]
            i = MIN_BLOCK_2[idx + 1]
            for _ = 1:(9 * i)
                buf[pos] = UInt8('0')
                pos += 1
            end
        end
        while i < blocks
            j = 120 + (-e2 - 16 * idx)
            p = POW10_OFFSET_2[idx + 1] + UInt32(i) - MIN_BLOCK_2[idx + 1]
            if p >= POW10_OFFSET_2[idx + 2]
                for _ = 1:(precision - 9 * i)
                    buf[pos] = UInt8('0')
                    pos += 1
                end
                break
            end
            #=@inbounds=# mula, mulb, mulc = POW10_SPLIT_2[p + 1]
            digits = mulshiftmod1e9(m2 << 8, mula, mulb, mulc, j + 8)
            if i < blocks - 1
                pos = append_nine_digits(digits, buf, pos)
            else
                maximum = precision - 9 * i
                lastDigit = 0
                k = 0
                while k < 9 - maximum
                    # global digits, lastDigit, k
                    lastDigit = digits % 10
                    digits = div(digits, 10)
                    k += 1
                end
                if lastDigit != 5
                    roundUp = lastDigit > 5
                else
                    requiredTwos = -e2 - precision - 1
                    trailingZeros = requiredTwos <= 0 || (requiredTwos < 60 && pow2(m2, requiredTwos))
                    roundUp = trailingZeros ? 2 : 1
                end
                if maximum > 0
                    pos = append_c_digits(maximum, digits, buf, pos)
                end
                break
            end
            i += 1
        end
        if roundUp != 0
            roundPos = pos
            dotPos = 1
            while true
                roundPos -= 1
                if roundPos == 0 || (buf[roundPos] == UInt8('-'))
                    buf[roundPos + 1] = UInt8('1')
                    if dotPos > 1
                        buf[dotPos] = UInt8('0')
                        buf[dotPos + 1] = decchar
                    end
                    buf[pos] = UInt8('0')
                    pos += 1
                    break
                end
                c = roundPos > 0 ? buf[roundPos] : 0x00
                if c == decchar
                    dotPos = roundPos
                    continue
                elseif c == UInt8('9')
                    buf[roundPos] = UInt8('0')
                    roundUp = 1
                    continue
                else
                    if roundUp == 2 && UInt8(c) % 2 == 0
                        break
                    end
                    buf[roundPos] = c + 1
                    break
                end
            end
        end
    else
        for _ = 1:precision
            buf[pos] = UInt8('0')
            pos += 1
        end
    end
    if trimtrailingzeros
        while buf[pos - 1] == UInt8('0')
            pos -= 1
        end
        if buf[pos - 1] == decchar && !hash
            pos -= 1
        end
    end
    return pos
end

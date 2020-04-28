@inline function writeexp(buf, pos, v::T,
    precision=-1, plus=false, space=false, hash=false,
    expchar=UInt8('e'), decchar=UInt8('.'), trimtrailingzeros=false) where {T <: Base.IEEEFloat}
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
            for _ = 1:precision
                buf[pos] = UInt8('0')
                pos += 1
            end
        elseif hash
            buf[pos] = decchar
            pos += 1
        end
        buf[pos] = expchar
        buf[pos + 1] = UInt8('+')
        buf[pos + 2] = UInt8('0')
        buf[pos + 3] = UInt8('0')
        return pos + 4
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
    precision += 1
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
    digits = 0
    printedDigits = 0
    availableDigits = 0
    e = 0
    if e2 >= -52
        idx = e2 < 0 ? 0 : indexforexp(e2)
        p10bits = pow10bitsforindex(idx)
        len = lengthforindex(idx)
        i = len - 1
        while i >= 0
            j = p10bits - e2
            #=@inbounds=# mula, mulb, mulc = POW10_SPLIT[POW10_OFFSET[idx + 1] + i + 1]
            digits = mulshiftmod1e9(m2 << 8, mula, mulb, mulc, j + 8)
            if printedDigits != 0
                if printedDigits + 9 > precision
                    availableDigits = 9
                    break
                end
                pos = append_nine_digits(digits, buf, pos)
                printedDigits += 9
            elseif digits != 0
                availableDigits = decimallength(digits)
                e = i * 9 + availableDigits - 1
                if availableDigits > precision
                    break
                end
                if precision > 1
                    pos = append_d_digits(availableDigits, digits, buf, pos, decchar)
                else
                    buf[pos] = UInt8('0') + digits
                    pos += 1
                    if hash
                        buf[pos] = decchar
                        pos += 1
                    end
                end
                printedDigits = availableDigits
                availableDigits = 0
            end
            i -= 1
        end
    end
    if e2 < 0 && availableDigits == 0
        idx = div(-e2, 16)
        i = MIN_BLOCK_2[idx + 1]
        while i < 200
            j = 120 + (-e2 - 16 * idx)
            p = POW10_OFFSET_2[idx + 1] + i - MIN_BLOCK_2[idx + 1]
            if p >= POW10_OFFSET_2[idx + 2]
                digits = 0
            else
                #=@inbounds=# mula, mulb, mulc = POW10_SPLIT_2[p + 1]
                digits = mulshiftmod1e9(m2 << 8, mula, mulb, mulc, j + 8)
            end
            if printedDigits != 0
                if printedDigits + 9 > precision
                    availableDigits = 9
                    break
                end
                pos = append_nine_digits(digits, buf, pos)
                printedDigits += 9
            elseif digits != 0
                availableDigits = decimallength(digits)
                e = -(i + 1) * 9 + availableDigits - 1
                if availableDigits > precision
                    break
                end
                if precision > 1
                    pos = append_d_digits(availableDigits, digits, buf, pos, decchar)
                else
                    buf[pos] = UInt8('0') + digits
                    pos += 1
                    if hash
                        buf[pos] = decchar
                        pos += 1
                    end
                end
                printedDigits = availableDigits
                availableDigits = 0
            end
            i += 1
        end
    end
    maximum = precision - printedDigits
    if availableDigits == 0
        digits = 0
    end
    lastDigit = 0
    if availableDigits > maximum
        for k = 0:(availableDigits - maximum - 1)
            lastDigit = digits % 10
            digits = div(digits, 10)
        end
    end
    roundUp = 0
    if lastDigit != 5
        roundUp = lastDigit > 5
    else
        rexp = precision - e
        requiredTwos = -e2 - rexp
        trailingZeros = requiredTwos <= 0 ||
            (requiredTwos < 60 && pow2(m2, requiredTwos))
        if rexp < 0
            requiredFives = -rexp
            trailingZeros = trailingZeros & pow5(m2, requiredFives)
        end
        roundUp = trailingZeros ? 2 : 1
    end
    if printedDigits != 0
        if digits == 0
            for _ = 1:maximum
                buf[pos] = UInt8('0')
                pos += 1
            end
        else
            pos = append_c_digits(maximum, digits, buf, pos)
        end
    else
        if precision > 1
            pos = append_d_digits(maximum, digits, buf, pos, decchar)
        else
            buf[pos] = UInt8('0') + digits
            pos += 1
            if hash
                buf[pos] = decchar
                pos += 1
            end
        end
    end
    if roundUp != 0
        roundPos = pos
        while true
            roundPos -= 1
            if roundPos == 0 || buf[roundPos] == UInt8('-')
                buf[roundPos + 1] = UInt8('1')
                e += 1
                break
            end
            c = roundPos > 0 ? buf[roundPos] : 0x00
            if c == decchar
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
    if trimtrailingzeros
        while buf[pos - 1] == UInt8('0')
            pos -= 1
        end
        if buf[pos - 1] == decchar && !hash
            pos -= 1
        end
    end
    buf[pos] = expchar
    pos += 1
    if e < 0
        buf[pos] = UInt8('-')
        pos += 1
        e = -e
    else
        buf[pos] = UInt8('+')
        pos += 1
    end
    if e >= 100
        c = e % 10
        unsafe_copyto!(buf, pos, DIGIT_TABLE, 2 * div(e, 10) + 1, 2)
        buf[pos + 2] = UInt8('0') + c
        pos += 3
    else
        unsafe_copyto!(buf, pos, DIGIT_TABLE, 2 * e + 1, 2)
        pos += 2
    end
    return pos
end

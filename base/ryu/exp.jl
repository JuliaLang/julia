function writeexp(buf, pos, v::T,
    precision=-1, plus=false, space=false, hash=false,
    expchar=UInt8('e'), decchar=UInt8('.'), trimtrailingzeros=false) where {T <: Base.IEEEFloat}
    @assert 0 < pos <= length(buf)
    startpos = pos
    x = Float64(v)
    pos = append_sign(x, plus, space, buf, pos)

    # special cases
    if iszero(x)
        @inbounds buf[pos] = UInt8('0')
        pos += 1
        if precision > 0 && !trimtrailingzeros
            @inbounds buf[pos] = decchar
            pos += 1
            for _ = 1:precision
                @inbounds buf[pos] = UInt8('0')
                pos += 1
            end
        elseif hash
            @inbounds buf[pos] = decchar
            pos += 1
        end
        @inbounds buf[pos] = expchar
        @inbounds buf[pos + 1] = UInt8('+')
        @inbounds buf[pos + 2] = UInt8('0')
        @inbounds buf[pos + 3] = UInt8('0')
        return pos + 4
    elseif isnan(x)
        @inbounds buf[pos] = UInt8('N')
        @inbounds buf[pos + 1] = UInt8('a')
        @inbounds buf[pos + 2] = UInt8('N')
        return pos + 3
    elseif !isfinite(x)
        @inbounds buf[pos] = UInt8('I')
        @inbounds buf[pos + 1] = UInt8('n')
        @inbounds buf[pos + 2] = UInt8('f')
        return pos + 3
    end

    bits = Core.bitcast(UInt64, x)
    mant = bits & MANTISSA_MASK
    exp = Int((bits >> 52) & EXP_MASK)

    if iszero(exp)
        e2 = 1 - 1023 - 52
        m2 = mant
    else
        e2 = exp - 1023 - 52
        m2 = (Int64(1) << 52) | mant
    end
    nonzero = false
    precision += 1
    digits = zero(UInt32)
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
            if !iszero(printedDigits)
                if printedDigits + 9 > precision
                    availableDigits = 9
                    break
                end
                pos = append_nine_digits(digits, buf, pos)
                printedDigits += 9
            elseif !iszero(digits)
                availableDigits = decimallength(digits)
                e = i * 9 + availableDigits - 1
                if availableDigits > precision
                    break
                end
                if precision > 1
                    pos = append_d_digits(availableDigits, digits, buf, pos, decchar)
                else
                    @inbounds buf[pos] = UInt8('0') + digits
                    pos += 1
                    if hash
                        @inbounds buf[pos] = decchar
                        pos += 1
                    end
                end
                printedDigits = availableDigits
                availableDigits = 0
            end
            i -= 1
        end
    end
    if e2 < 0 && iszero(availableDigits)
        idx = div(-e2, 16)
        i = Int(MIN_BLOCK_2[idx + 1])
        while i < 200
            j = 120 + (-e2 - 16 * idx)
            p = POW10_OFFSET_2[idx + 1] + i - MIN_BLOCK_2[idx + 1]
            if p >= POW10_OFFSET_2[idx + 2]
                digits = zero(UInt32)
            else
                #=@inbounds=# mula, mulb, mulc = POW10_SPLIT_2[p + 1]
                digits = mulshiftmod1e9(m2 << 8, mula, mulb, mulc, j + 8)
            end
            if !iszero(printedDigits)
                if printedDigits + 9 > precision
                    availableDigits = 9
                    break
                end
                pos = append_nine_digits(digits, buf, pos)
                printedDigits += 9
            elseif !iszero(digits)
                availableDigits = decimallength(digits)
                e = -(i + 1) * 9 + availableDigits - 1
                if availableDigits > precision
                    break
                end
                if precision > 1
                    pos = append_d_digits(availableDigits, digits, buf, pos, decchar)
                else
                    @inbounds buf[pos] = UInt8('0') + digits
                    pos += 1
                    if hash
                        @inbounds buf[pos] = decchar
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
    if iszero(availableDigits)
        digits = zero(UInt32)
    end
    lastDigit = zero(UInt32)
    if availableDigits > maximum
        for k = 0:(availableDigits - maximum - 1)
            lastDigit = digits % UInt32(10)
            digits = div(digits, UInt32(10))
        end
    end
    roundUp = 0
    if lastDigit != 5
        roundUp = lastDigit > 5 ? 1 : 0
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
    if !iszero(printedDigits)
        if iszero(digits)
            for _ = 1:maximum
                @inbounds buf[pos] = UInt8('0')
                pos += 1
            end
        else
            pos = append_c_digits(maximum, digits, buf, pos)
        end
    else
        if precision > 1
            pos = append_d_digits(maximum, digits, buf, pos, decchar)
        else
            @inbounds buf[pos] = UInt8('0') + digits
            pos += 1
            if hash
                @inbounds buf[pos] = decchar
                pos += 1
            end
        end
    end
    if !iszero(roundUp)
        roundPos = pos
        while true
            roundPos -= 1
            if roundPos == (startpos - 1) || (@inbounds buf[roundPos]) == UInt8('-') || (plus && (@inbounds buf[roundPos]) == UInt8('+')) || (space && (@inbounds buf[roundPos]) == UInt8(' '))
                @inbounds buf[roundPos + 1] = UInt8('1')
                e += 1
                break
            end
            c = roundPos > 0 ? (@inbounds buf[roundPos]) : 0x00
            if c == decchar
                continue
            elseif c == UInt8('9')
                @inbounds buf[roundPos] = UInt8('0')
                roundUp = 1
                continue
            else
                if roundUp == 2 && iseven(c)
                    break
                end
                @inbounds buf[roundPos] = c + 1
                break
            end
        end
    end
    if trimtrailingzeros
        while @inbounds buf[pos - 1] == UInt8('0')
            pos -= 1
        end
        if @inbounds buf[pos - 1] == decchar && !hash
            pos -= 1
        end
    end
    buf[pos] = expchar
    pos += 1
    if e < 0
        @inbounds buf[pos] = UInt8('-')
        pos += 1
        e = -e
    else
        @inbounds buf[pos] = UInt8('+')
        pos += 1
    end
    if e >= 100
        c = (e % 10) % UInt8
        @inbounds d100 = DIGIT_TABLE16[div(e, 10) + 1]
        @inbounds buf[pos] = d100 % UInt8
        @inbounds buf[pos + 1] = (d100 >> 0x8) % UInt8
        @inbounds buf[pos + 2] = UInt8('0') + c
        pos += 3
    else
        @inbounds d100 = DIGIT_TABLE16[e + 1]
        @inbounds buf[pos] = d100 % UInt8
        @inbounds buf[pos + 1] = (d100 >> 0x8) % UInt8
        pos += 2
    end
    return pos
end

function roundweed(buffer,len,rest,tk,unit,kappa)
    unit >= tk && return false, kappa
    tk - unit <= unit && return false, kappa
    tk - rest > rest && (tk - 2 * rest >= 2 * unit) && return true, kappa
    if rest > unit && (tk - (rest - unit) <= (rest - unit))
        buffer[len-1] += 1
        for i = (len-1):-1:2
            buffer[i] != 0x30 + 10 && break
            buffer[i] = 0x30
            buffer[i-1] += 1
        end
        if buffer[1] == 0x30 + 10
            buffer[1] = 0x31
            kappa += 1
        end
        return true, kappa
    end
    return false, kappa
end

function digitgen(w,buffer,requested_digits=1000)
    unit::UInt64 = 1
    one = Float(unit << -w.e, w.e)
    integrals = w.s >> -one.e
    fractionals = w.s & (one.s-1)
    divisor, kappa = bigpowten(integrals, 64 + one.e)
    len = 1
    rest = 0
    while kappa > 0
        digit = div(integrals,divisor)
        buffer[len] = 0x30 + digit
        len += 1
        requested_digits -= 1
        integrals %= divisor
        kappa -= 1
        if requested_digits == 0
            rest = (uint64(integrals) << -one.e) + fractionals
            r, kappa = roundweed(buffer, len, rest, uint64(divisor) << -one.e,
                    unit,kappa)
            return r, kappa, len
        end
        divisor = div(divisor,10)
    end
    while requested_digits > 0 && fractionals > unit
        fractionals *= 10
        unit *= 10
        digit = fractionals >> -one.e
        buffer[len] = 0x30 + digit
        len += 1
        requested_digits -= 1
        fractionals &= one.s - 1
        kappa -= 1
    end
    requested_digits != 0 && return false, kappa, len
    r, kappa = roundweed(buffer,len,fractionals,one.s,
                         unit,kappa)
    return r, kappa, len
end

function fastprecision(v,requested_digits,buffer=Array(UInt8,100))
    f = normalize(float64(v))
    ten_mk_min_exp = kMinExp - (f.e + FloatSignificandSize)
    ten_mk_max_exp = kMaxExp - (f.e + FloatSignificandSize)
    cp = binexp_cache(ten_mk_min_exp,ten_mk_max_exp)
    scaled_w = f * cp
    r, kappa, len = digitgen(scaled_w,buffer,requested_digits)
    decimal_exponent = -cp.de + kappa
    return r, len, decimal_exponent+len-1, buffer
end

const kMinExp = -60
const kMaxExp = -32

function roundweed(buffer,len,rest,tk,unit,kappa,too_high::UInt64,unsafe_interval::UInt64)
    small = too_high - unit
    big = too_high + unit
    while rest < small &&
            unsafe_interval - rest >= tk &&
            (rest + tk < small ||
            small - rest >= rest + tk - small)
        buffer[len-1] -= 1
        rest += tk
    end
    if rest < big &&
        unsafe_interval - rest >= tk &&
        (rest + tk < big ||
        big - rest > rest + tk - big)
        return false, kappa
    end
    return (2 * unit <= rest) && (rest <= unsafe_interval - 4 * unit), kappa
end

const SmallPowersOfTen = [
        0, 1, 10, 100, 1000, 10000, 100000,
        1000000, 10000000, 100000000, 1000000000]

function bigpowten(n,n_bits)
    guess = (n_bits + 1) * 1233 >> 12
    guess += 1
    i = SmallPowersOfTen[guess+1]
    return n < i ? (SmallPowersOfTen[guess], guess-1) : (i,guess)
end

function digitgen(low,w,high,buffer)
    unit::UInt64 = 1
    one = Float(unit << -w.e, w.e)
    too_high = Float(high.s+unit,high.e)
    unsafe_interval = too_high - Float(low.s-unit,low.e)
    integrals = too_high.s >> -one.e
    fractionals = too_high.s & (one.s-1)
    divisor, kappa = bigpowten(integrals, 64 + one.e)
    len = 1
    rest = uint64(0)
    while kappa > 0
        digit = div(integrals,divisor)
        buffer[len] = 0x30 + digit
        len += 1
        integrals %= divisor
        kappa -= 1
        rest = (uint64(integrals) << -one.e) + fractionals
        if rest < unsafe_interval.s
            r, kappa = roundweed(buffer, len, rest, uint64(divisor) << -one.e,
                        unit,kappa,(too_high - w).s,unsafe_interval.s)
            return r, kappa, len
        end
        divisor = div(divisor,10)
    end
    while true
        fractionals *= 10
        unit *= 10
        unsafe_interval = Float(unsafe_interval.s*10,unsafe_interval.e)
        digit = fractionals >> -one.e
        buffer[len] = 0x30 + digit
        len += 1
        fractionals &= one.s - 1
        kappa -= 1
        if fractionals < unsafe_interval.s
            r, kappa = roundweed(buffer,len,fractionals,one.s,
                        unit,kappa,(too_high - w).s*unit,unsafe_interval.s)
            return r, kappa, len
        end
    end
end

function fastshortest(v,buffer=Array(UInt8,17))
    f = normalize(float64(v))
    bound_minus, bound_plus = normalizedbound(v)
    ten_mk_min_exp = kMinExp - (f.e + FloatSignificandSize)
    ten_mk_max_exp = kMaxExp - (f.e + FloatSignificandSize)
    cp = binexp_cache(ten_mk_min_exp,ten_mk_max_exp)
    scaled_w = f * cp
    scaled_bound_minus = bound_minus * cp
    scaled_bound_plus = bound_plus * cp
    r, kappa, len = digitgen(scaled_bound_minus,scaled_w,
                             scaled_bound_plus,buffer)
    decimal_exponent = -cp.de + kappa
    return r, len, decimal_exponent+len-1, buffer
end

# 128-bit significands of strict overestimates of powers of 10.
# Negate dec_pow_min and dec_pow_max because we need negative powers 10^-k.
const pow10_significands = Memory{UInt128}(undef, length(-292:325))
for (i, dec_exp) in enumerate(-292:325)
    # dec_exp is -k in the paper.
    bin_exp = floor(Int, dec_exp * log2(10)) - 127
    bin_pow = big(2)^(abs(bin_exp))
    dec_pow = big(10)^(abs(dec_exp))
    if dec_exp < 0
        result = bin_pow ÷ dec_pow
    elseif bin_exp < 0
        result = dec_pow * bin_pow
    else
        result = dec_pow ÷ bin_pow
    end
    pow10_significands[i] = result + 1
end


high64(x::UInt128) = x >> 64 % UInt64
function umul192_upper128(x::UInt128, y::UInt64)
    x_hi, x_lo = high64(x), x%UInt64
    p = widemul(x_hi, y)
    lo = (p%UInt64) + (widemul(x_lo, y) >> 64) % UInt64
    return reinterpret(UInt128, (lo, high64(p) + (lo < (p%UInt64))))
end

# Computes upper 64 bits of multiplication of x and y,
# discards the least significant bit and rounds to odd.
function umul192_upper64_inexact_to_odd(x::UInt128, y::UInt64)
    r = umul192_upper128(x, y)
    return high64(r) | !iszero((r%Int64) >> 1)
end

"""
    b, exp = reduce_shortest(v)

Reduce positive Float64 to shortest decimal representation 
`abs(v) == b * 10^exp` and `b` is an integer.
"""
function reduce_shortest(v::T) where {T<:Float64}
    bits = reinterpret(UInt64, v)
    bin_sig = bits & significand_mask(T);  # binary_significand
    bin_exp = ((bits & exponent_mask(T)) >> significand_bits(T)) % Int  # binary exponent

    regular = bin_sig != 0
    implicit_bit = significand_mask(T)+1
    # Handle subnormals if value is subnormal.
    if bits < implicit_bit
        bin_sig |= implicit_bit
        bin_exp = 1
        regular = true
    end

    bin_sig ⊻= implicit_bit
    bin_exp -= significand_bits(T) + exponent_bias(T)

    # Handle small integers.
    if -significand_bits(T) <= bin_exp < 0
        f = bin_sig >> -bin_exp
        if (f << -bin_exp) == bin_sig
            return return (f, 0)
        end
    end

    # Compute the decimal exponent as floor(log10(exp2(bin_exp))) if regular or
    # floor(log10(3/4 * exp2(bin_exp))) otherwise, without branching.
    # log10_3_over_4_sig = round(log10(3/4) * exp2(log10_2_exp))
    log10_3_over_4_sig = -131008
    # log10_2_sig = round(log10(2) * exp2(log10_2_exp))
    log10_2_sig = 315653
    log10_2_exp = 20
    dec_exp = (bin_exp * log10_2_sig + (!regular) * log10_3_over_4_sig) >> log10_2_exp

    dec_exp_min = -292
    pow10 = pow10_significands[0x1 - dec_exp - dec_exp_min]

    # log2_pow10_sig = round(log2(10) * exp2(log2_pow10_exp)) + 1
    log2_pow10_sig = 217707
    log2_pow10_exp = 16
    # pow10_bin_exp = floor(-dec_exp*log2(10))
    pow10_bin_exp = (-dec_exp * log2_pow10_sig) >> log2_pow10_exp;
    # pow10 = pow10 * exp2(pow10_bin_exp - 127)

    # Shift to ensure the intermediate result of multiplying by a power of 10
    # has a fixed 128-bit fractional part. For example, 3 * 2**59 and 3 * 2**60
    # both have dec_exp = 2 and dividing them by 10**dec_exp would have the
    # decimal point in different (bit) positions without the shift:
    #   3 * 2**59 / 100 = 1.72...e+16 (shift = 1 + 1)
    #   3 * 2**60 / 100 = 3.45...e+16 (shift = 2 + 1)
    shift = bin_exp + pow10_bin_exp + 1

    if regular
        r = umul192_upper128(pow10 - 1, bin_sig << shift)
        digit = high64(r) % 10

        num_fractional_bits = 60
        ten = UInt64(10) << num_fractional_bits
        # Fixed-point remainder of the scaled significand modulo 10.
        rem10 = (digit << num_fractional_bits) | ((r%UInt64) >> 4)
        half_ulp = pow10 >> (64+5 - shift)
        upper = rem10 + half_ulp

        # An optimization from yy_double by Yaoyuan Guo:
        if r%UInt64 != (UInt64(1) << 63) && rem10 != half_ulp && ten - upper > 1
            round = (upper >> num_fractional_bits) >= 10
            shorter = high64(r) - digit + round * 10
            longer = high64(r) + (r%UInt64 >= (UInt64(1) << 63))

            return (((half_ulp >= rem10) + round)!=0 ? shorter : longer, dec_exp)
        end
    end

    # Shift the significand so that boundaries are integer.
    bin_sig_shifted = bin_sig << 2

    # Compute the estimates of lower and upper bounds of the rounding interval
    # by multiplying them by the power of 10 and applying modified rounding.
    lsb = bin_sig & 1;
    lower = umul192_upper64_inexact_to_odd(
          pow10, (bin_sig_shifted - (regular + 1)) << shift) + lsb
    upper = umul192_upper64_inexact_to_odd(
        pow10, (bin_sig_shifted + 2) << shift) - lsb

    # The idea of using a single shorter candidate is by Cassio Neri.
    # It is less or equal to the upper bound by construction.
    shorter = 10 * ((upper >> 2) ÷ 10)
    if (shorter << 2) >= lower
        return (shorter, dec_exp)
    end

    scaled_sig = umul192_upper64_inexact_to_odd(pow10, bin_sig_shifted << shift);
    under = scaled_sig >> 2
    over = under + 1

    # Pick the closest of dec_sig_under and dec_sig_over and check if it's in
    # the rounding interval.
    cmp = Int64(scaled_sig - ((under + over) << 1))
    under_closer = cmp < 0 || (cmp == 0 && !(under & 1))
    under_in = (under << 2) >= lower
    return ((under_closer && under_in) ? under : over, dec_exp)
end

function writeshortest(buf::AbstractVector{UInt8}, pos, x::T,
                       plus=false, space=false, hash=true,
                       precision=-1, expchar=UInt8('e'), padexp=false, decchar=UInt8('.'),
                       typed=false, compact=false) where {T}
    @assert 0 < pos <= length(buf)
    # special cases
    if x == 0
        if typed && x isa Float16
            @inbounds buf[pos] = UInt8('F')
            @inbounds buf[pos + 1] = UInt8('l')
            @inbounds buf[pos + 2] = UInt8('o')
            @inbounds buf[pos + 3] = UInt8('a')
            @inbounds buf[pos + 4] = UInt8('t')
            @inbounds buf[pos + 5] = UInt8('1')
            @inbounds buf[pos + 6] = UInt8('6')
            @inbounds buf[pos + 7] = UInt8('(')
            pos += 8
        end
        pos = append_sign(x, plus, space, buf, pos)
        @inbounds buf[pos] = UInt8('0')
        pos += 1
        if hash
            @inbounds buf[pos] = decchar
            pos += 1
        end
        if precision == -1
            if hash
                @inbounds buf[pos] = UInt8('0')
                pos += 1
            end
            if typed && x isa Float32
                @inbounds buf[pos] = UInt8('f')
                @inbounds buf[pos + 1] = UInt8('0')
                pos += 2
            end
            if typed && x isa Float16
                @inbounds buf[pos] = UInt8(')')
                pos += 1
            end
            return pos
        end
        while hash && precision > 1
            @inbounds buf[pos] = UInt8('0')
            pos += 1
            precision -= 1
        end
        if typed && x isa Float32
            @inbounds buf[pos] = UInt8('f')
            @inbounds buf[pos + 1] = UInt8('0')
            pos += 2
        end
        if typed && x isa Float16
            @inbounds buf[pos] = UInt8(')')
            pos += 1
        end
        return pos
    elseif isnan(x)
        pos = append_sign(x, plus, space, buf, pos)
        @inbounds buf[pos] = UInt8('N')
        @inbounds buf[pos + 1] = UInt8('a')
        @inbounds buf[pos + 2] = UInt8('N')
        if typed
            if x isa Float32
                @inbounds buf[pos + 3] = UInt8('3')
                @inbounds buf[pos + 4] = UInt8('2')
            elseif x isa Float16
                @inbounds buf[pos + 3] = UInt8('1')
                @inbounds buf[pos + 4] = UInt8('6')
            end
        end
        return pos + 3 + (typed && x isa Union{Float32, Float16} ? 2 : 0)
    elseif !isfinite(x)
        pos = append_sign(x, plus, space, buf, pos)
        @inbounds buf[pos] = UInt8('I')
        @inbounds buf[pos + 1] = UInt8('n')
        @inbounds buf[pos + 2] = UInt8('f')
        if typed
            if x isa Float32
                @inbounds buf[pos + 3] = UInt8('3')
                @inbounds buf[pos + 4] = UInt8('2')
            elseif x isa Float16
                @inbounds buf[pos + 3] = UInt8('1')
                @inbounds buf[pos + 4] = UInt8('6')
            end
        end
        return pos + 3 + (typed && x isa Union{Float32, Float16} ? 2 : 0)
    end

    output, nexp = reduce_shortest(x, compact ? 999_999 : nothing)

    if typed && x isa Float16
        @inbounds buf[pos] = UInt8('F')
        @inbounds buf[pos + 1] = UInt8('l')
        @inbounds buf[pos + 2] = UInt8('o')
        @inbounds buf[pos + 3] = UInt8('a')
        @inbounds buf[pos + 4] = UInt8('t')
        @inbounds buf[pos + 5] = UInt8('1')
        @inbounds buf[pos + 6] = UInt8('6')
        @inbounds buf[pos + 7] = UInt8('(')
        pos += 8
    end
    pos = append_sign(x, plus, space, buf, pos)

    olength = decimallength(output)
    exp_form = true
    pt = nexp + olength
    if -4 < pt <= (precision == -1 ? (T == Float16 ? 3 : 6) : precision) &&
        !(pt >= olength && abs(mod(x + 0.05, 10^(pt - olength)) - 0.05) > 0.05)
        exp_form = false
        if pt <= 0
            @inbounds buf[pos] = UInt8('0')
            pos += 1
            @inbounds buf[pos] = decchar
            pos += 1
            for _ = 1:abs(pt)
                @inbounds buf[pos] = UInt8('0')
                pos += 1
            end
        # elseif pt >= olength
            # nothing to do at this point
        # else
            # nothing to do at this point
        end
    else
        # make space for decchar
        pos += 1
    end

    append_c_digits(olength, output, buf, pos)

    if !exp_form
        if pt <= 0
            pos += olength
            precision -= olength
        elseif pt >= olength
            pos += olength
            precision -= olength
            for _ = 1:nexp
                @inbounds buf[pos] = UInt8('0')
                pos += 1
                precision -= 1
            end
            if hash
                @inbounds buf[pos] = decchar
                pos += 1
                if precision < 0
                    @inbounds buf[pos] = UInt8('0')
                    pos += 1
                end
            end
        else
            pointoff = olength - abs(nexp)
            # shift bytes after pointoff to make room for decchar
            buf_cconv = Base.cconvert(Ptr{UInt8}, buf)
            GC.@preserve buf_cconv begin
                ptr = Base.unsafe_convert(Ptr{UInt8}, buf_cconv)
                memmove(ptr + pos + pointoff, ptr + pos + pointoff - 1, (olength - pointoff + 1)%Csize_t)
            end
            @inbounds buf[pos + pointoff] = decchar
            pos += olength + 1
            precision -= olength
        end
        if hash
            while precision > 0
                @inbounds buf[pos] = UInt8('0')
                pos += 1
                precision -= 1
            end
        end
        if typed && x isa Float32
            @inbounds buf[pos] = UInt8('f')
            @inbounds buf[pos + 1] = UInt8('0')
            pos += 2
        end
    else
        # move leading digit into place
        @inbounds buf[pos - 1] = buf[pos]
        if olength > 1 || hash
            @inbounds buf[pos] = decchar
            pos += olength
            precision -= olength
        end
        if hash
            if olength == 1
                @inbounds buf[pos] = UInt8('0')
                pos += 1
            end
            while precision > 0
                @inbounds buf[pos] = UInt8('0')
                pos += 1
                precision -= 1
            end
        end

        @inbounds buf[pos] = expchar
        pos += 1
        exp2 = nexp + olength - 1
        if exp2 < 0
            @inbounds buf[pos] = UInt8('-')
            pos += 1
            exp2 = -exp2
        elseif padexp
            @inbounds buf[pos] = UInt8('+')
            pos += 1
        end

        if exp2 >= 100
            c = exp2 % 10
            @inbounds d100 = DIGIT_TABLE16[(div(exp2, 10) % Int) + 1]
            @inbounds buf[pos] = d100 % UInt8
            @inbounds buf[pos + 1] = (d100 >> 0x8) % UInt8
            @inbounds buf[pos + 2] = UInt8('0') + (c % UInt8)
            pos += 3
        elseif exp2 >= 10
            @inbounds d100 = DIGIT_TABLE16[(exp2 % Int) + 1]
            @inbounds buf[pos] = d100 % UInt8
            @inbounds buf[pos + 1] = (d100 >> 0x8) % UInt8
            pos += 2
        else
            if padexp
                @inbounds buf[pos] = UInt8('0')
                pos += 1
            end
            @inbounds buf[pos] = UInt8('0') + (exp2 % UInt8)
            pos += 1
        end
    end
    if typed && x isa Float16
        @inbounds buf[pos] = UInt8(')')
        pos += 1
    end

    return pos
end

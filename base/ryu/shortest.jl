"""
    b, e10 = reduce_shortest(f[, maxsignif])

Reduce to shortest decimal representation where `abs(f) == b * 10^e10` and `b` is an
integer. If a `maxsignif` argument is provided, then `b < maxsignif`.
"""
@inline function reduce_shortest(f::T, maxsignif=nothing) where {T}
    U = uinttype(T)
    uf = reinterpret(U, f)
    m = uf & significand_mask(T)
    e = ((uf & exponent_mask(T)) >> significand_bits(T)) % Int

    ## Step 1
    #  mf * 2^ef == f
    mf = (one(U) << significand_bits(T)) | m
    ef = e - exponent_bias(T) - significand_bits(T)
    f_isinteger = mf & ((one(U) << -ef) - one(U)) == 0

    if ef > 0 || ef < -Base.significand_bits(T) || !f_isinteger
        # fixup subnormals
        if e == 0
            ef = 1 - exponent_bias(T) - significand_bits(T)
            mf = m
        end

        ## Step 2
        #  u * 2^e2 == (f + prevfloat(f))/2
        #  v * 2^e2 == f
        #  w * 2^e2 == (f + nextfloat(f))/2
        e2 = ef - 2
        mf_iseven = iseven(mf) # trailing bit of significand is zero

        v = U(4) * mf
        w = v + U(2)
        u_shift_half = m == 0 && e > 1 # if first element of binade, other than first normal one
        u = v - U(2) + u_shift_half

        ## Step 3
        #  a == floor(u * 2^e2 / 10^e10), exact if a_allzero
        #  b == floor(v * 2^e2 / 10^e10), exact if b_allzero
        #  c == floor(w * 2^e2 / 10^e10)
        a_allzero = false
        b_allzero = false
        b_lastdigit = 0x00
        if e2 >= 0
            q = log10pow2(e2) - (T == Float64 ? (e2 > 3) : 0)
            e10 = q
            k = pow5_inv_bitcount(T) + pow5bits(q) - 1
            i = -e2 + q + k
            a, b, c = mulshiftinvsplit(T, u, v, w, q, i)
            if T == Float32 || T == Float16
                if q != 0 && div(c - 1, 10) <= div(a, 10)
                    l = pow5_inv_bitcount(T) + pow5bits(q - 1) - 1
                    mul = pow5invsplit_lookup(T, q-1)
                    b_lastdigit = (mulshift(v, mul, -e2 + q - 1 + l) % 10) % UInt8
                end
            end
            if q <= qinvbound(T)
                if ((v % UInt32) - 5 * div(v, 5)) == 0
                    b_allzero = pow5(v, q)
                elseif mf_iseven
                    a_allzero = pow5(u, q)
                else
                    c -= pow5(w, q)
                end
            end
        else
            q = log10pow5(-e2) - (T == Float64 ? (-e2 > 1) : 0)
            e10 = q + e2
            i = -e2 - q
            k = pow5bits(i) - pow5_bitcount(T)
            j = q - k
            a, b, c = mulshiftsplit(T, u, v, w, i, j)
            if T == Float32 || T == Float16
                if q != 0 && div(c - 1, 10) <= div(a, 10)
                    j = q - 1 - (pow5bits(i + 1) - pow5_bitcount(T))
                    mul = pow5split_lookup(T, i+1)
                    b_lastdigit = (mulshift(v, mul, j) % 10) % UInt8
                end
            end
            if q <= 1
                b_allzero = true
                if mf_iseven
                    a_allzero = !u_shift_half
                else
                    c -= 1
                end
            elseif q < qbound(T)
                b_allzero = pow2(v, q - (T != Float64))
            end
        end

        ## Step 4: reduction
        if a_allzero || b_allzero
            # a) slow loop
            while true
                c_div10 = div(c, 10)
                a_div10 = div(a, 10)
                if c_div10 <= a_div10
                    break
                end
                a_mod10 = (a % UInt32) - UInt32(10) * (a_div10 % UInt32)
                b_div10 = div(b, 10)
                b_mod10 = (b % UInt32) - UInt32(10) * (b_div10 % UInt32)
                a_allzero &= a_mod10 == 0
                b_allzero &= b_lastdigit == 0
                b_lastdigit = b_mod10 % UInt8
                b = b_div10
                c = c_div10
                a = a_div10
                e10 += 1
            end
            if a_allzero
                while true
                    a_div10 = div(a, 10)
                    a_mod10 = (a % UInt32) - UInt32(10) * (a_div10 % UInt32)
                    if a_mod10 != 0 && (maxsignif === nothing || b < maxsignif)
                        break
                    end
                    c_div10 = div(c, 10)
                    b_div10 = div(b, 10)
                    b_mod10 = (b % UInt32) - UInt32(10) * (b_div10 % UInt32)
                    b_allzero &= b_lastdigit == 0
                    b_lastdigit = b_mod10 % UInt8
                    b = b_div10
                    c = c_div10
                    a = a_div10
                    e10 += 1
                end
            end
            if b_allzero && b_lastdigit == 5 && iseven(b)
                b_lastdigit = UInt8(4)
            end
            roundup = (b == a && (!mf_iseven || !a_allzero)) || b_lastdigit >= 5
        else
            # b) specialized for common case (99% Float64, 96% Float32)
            roundup = b_lastdigit >= 5
            c_div100 = div(c, 100)
            a_div100 = div(a, 100)
            if c_div100 > a_div100
                b_div100 = div(b, 100)
                b_mod100 = (b % UInt32) - UInt32(100) * (b_div100 % UInt32)
                roundup = b_mod100 >= 50
                b = b_div100
                c = c_div100
                a = a_div100
                e10 += 2
            end
            while true
                c_div10 = div(c, 10)
                a_div10 = div(a, 10)
                if c_div10 <= a_div10
                    break
                end
                b_div10 = div(b, 10)
                b_mod10 = (b % UInt32) - UInt32(10) * (b_div10 % UInt32)
                roundup = b_mod10 >= 5
                b = b_div10
                c = c_div10
                a = a_div10
                e10 += 1
            end
            roundup = (b == a || roundup)
        end
        if maxsignif !== nothing && b > maxsignif
            # reduce to max significant digits
            while true
                b_div10 = div(b, 10)
                b_mod10 = (b % UInt32) - UInt32(10) * (b_div10 % UInt32)
                if b <= maxsignif
                    break
                end
                b = b_div10
                roundup = (b_allzero && iseven(b)) ? b_mod10 > 5 : b_mod10 >= 5
                b_allzero &= b_mod10 == 0
                e10 += 1
            end
            b = b + roundup

            # remove trailing zeros
            while true
                b_div10 = div(b, 10)
                b_mod10 = (b % UInt32) - UInt32(10) * (b_div10 % UInt32)
                if b_mod10 != 0
                    break
                end
                b = b_div10
                e10 += 1
            end
        else
            b = b + roundup
        end
    else
        # c) specialized f an integer < 2^53
        b = mf >> -ef
        e10 = 0

        if maxsignif !== nothing && b > maxsignif
            roundup = false
            b_allzero = true
            # reduce to max significant digits
            while true
                b_div10 = div(b, 10)
                b_mod10 = (b % UInt32) - UInt32(10) * (b_div10 % UInt32)
                if b <= maxsignif
                    break
                end
                b = b_div10
                roundup = (b_allzero && iseven(b)) ? b_mod10 > 5 : b_mod10 >= 5
                b_allzero &= b_mod10 == 0
                e10 += 1
            end
            b = b + roundup
        end
        while true
            b_div10 = div(b, 10)
            b_mod10 = (b % UInt32) - UInt32(10) * (b_div10 % UInt32)
            if b_mod10 != 0
                break
            end
            b = b_div10
            e10 += 1
        end
    end
    return b, e10
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
            @inbounds buf[pos] = UInt8('0')
            pos += 1
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
            ptr = pointer(buf)
            memmove(ptr + pos + pointoff, ptr + pos + pointoff - 1, olength - pointoff + 1)
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

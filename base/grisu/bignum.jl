function normalizedexponent(significand, exponent::Int32)
    significand::UInt64
    while (significand & HiddenBit(Float64)) == 0
        significand <<= 1
        exponent -= 1
    end
    return int32(exponent)
end

function bignumdtoa(v,mode,requested_digits::Int,buffer)
    significand = _significand(v)
    exponent = _exponent(v)
    lower_boundary_is_closer = lowerboundaryiscloser(v)
    need_boundary_deltas = mode == SHORTEST

    is_even = (significand & 1) == 0
    normalized_exponent = normalizedexponent(significand, exponent)
    estimated_power = estimatepower(normalized_exponent)

    if mode == FIXED && -estimated_power - 1 > requested_digits
        buffer[1] = 0
        len = 1
        decimal_point = -requested_digits
        return true, len, decimal_point, buffer
    end
    num = den = minus = plus = BigInt(0)
    num, den, minus, plus = initialscaledstartvalues(significand,exponent,lower_boundary_is_closer,
                              estimated_power,need_boundary_deltas,
                              num,den,minus,plus);
    num, den, minus, plus, decimal_point = fixupmultiply10(
        estimated_power,is_even,num,den,minus,plus);

    if mode == SHORTEST
        len, buffer = generateshortestdigits(num,den,minus,plus,is_even,buffer)
    elseif mode == FIXED
        len, decimal_point, buffer = bignumtofixed(requested_digits,num,den,buffer,decimal_point)
    elseif mode == PRECISION
        len, decimal_point, buffer = generatecounteddigits(requested_digits,num,den,buffer,decimal_point)
    end
    buffer[len] = 0
    return true, len, decimal_point, buffer
end

function generateshortestdigits(num,den,minus,plus,is_even,buffer)
    len = 1
    while true
        digit, num = divrem(num,den)
        buffer[len] = 0x30 + digit
        len += 1
        in_delta_room_minus = is_even ? num <= minus : num < minus
        in_delta_room_plus = is_even ? num + plus >= den : num + plus > den

        if !in_delta_room_minus && !in_delta_room_plus
            num *= 10
            minus *= 10
            if minus != plus
                plus *= 10
            end
        elseif in_delta_room_minus && in_delta_room_plus
            compare = num + num
            if compare < den
            elseif compare > den
                buffer[len - 1] += 1
            else
                if (buffer[len - 1] - 0x30) % 2 == 0
                else
                    buffer[len - 1] += 1
                end
            end
            return len, buffer
        elseif in_delta_room_minus
            return len, buffer
        else
            buffer[len - 1] += 1
            return len, buffer
        end
    end
end

function generatecounteddigits(count,num,den,buffer,decimal_point)
    for i = 1:(count-1)
        digit, num = divrem(num,den)
        buffer[i] = 0x30 + digit
        num *= 10
    end
    digit, num = divrem(num,den)
    if num + num >= den
        digit += 1
    end
    buffer[count] = 0x30 + digit
    for i = count:-1:2
        buffer[i] != 0x30 + 10 && break
        buffer[i] = 0x30
        buffer[i - 1] += 1
    end
    if buffer[1] == 0x30 + 10
        buffer[1] = 0x31
        decimal_point += 1
    end
    len = count+1
    return len, decimal_point, buffer
end

function bignumtofixed(requested_digits,num,den,buffer,decimal_point)
    if -decimal_point > requested_digits
        decimal_point = -requested_digits
        len = 1
        return len, decimal_point, buffer
    elseif -decimal_point == requested_digits
        den *= 10
        if num + num >= den
            buffer[1] = 0x31
            len = 2
            decimal_point += 1
        else
            len = 1
        end
        return len, decimal_point, buffer
    else
        needed_digits = decimal_point + requested_digits
        len, decimal_point, buffer = generatecounteddigits(
              needed_digits,num,den,buffer,decimal_point)
    end
    return len, decimal_point, buffer
end


const k1Log10 = 0.30102999566398114
function estimatepower(exponent)
    kSignificandSize = SignificandSize(Float64)
    estimate = ceil((exponent + kSignificandSize - 1) * k1Log10 - 1e-10)
    return estimate
end


function init3(
        significand,exponent,estimated_power,need_boundary_deltas,
        num,den,minus,plus)
    num::BigInt = BigInt(significand)
    num <<= exponent
    den::BigInt = BigInt(10)^estimated_power
    if need_boundary_deltas
        den <<= 1
        num <<= 1
        plus::BigInt = BigInt(1)
        plus <<= exponent
        minus::BigInt = BigInt(1)
        minus <<= exponent
    end
    return num, den, minus, plus
end


function init1(
        significand,exponent,estimated_power,need_boundary_deltas,
        num,den,minus,plus)
    num::BigInt = BigInt(significand)
    den::BigInt = BigInt(10)^estimated_power
    den <<= -exponent
    if need_boundary_deltas
        den <<= 1
        num <<= 1
        plus::BigInt = 1
        minus::BigInt = 1
    end
    return num, den, minus, plus
end

function init2(
        significand,exponent,estimated_power,need_boundary_deltas,
        num,den,minus,plus)
    num::BigInt = BigInt(10)^-estimated_power
    if need_boundary_deltas
        plus = num
        minus = num
    end
    num *= significand
    den::BigInt = BigInt(1)
    den <<= -exponent
    if need_boundary_deltas
        num <<= 1
        den <<= 1
    end
    return num, den, minus, plus
end

function initialscaledstartvalues(significand,
            exponent,lower_boundary_is_closer,estimated_power,
            need_boundary_deltas,num,den,minus,plus)
    if exponent >= 0
        num,den,minus,plus = init3(
                significand, exponent, estimated_power, need_boundary_deltas,
                num,den,minus,plus)
    elseif estimated_power >= 0
        num,den,minus,plus = init1(
                significand, exponent, estimated_power, need_boundary_deltas,
                num,den,minus,plus)
    else
        num,den,minus,plus = init2(
                significand, exponent, estimated_power, need_boundary_deltas,
                num,den,minus,plus)
    end
    if need_boundary_deltas && lower_boundary_is_closer
         den::BigInt <<= 1
         num::BigInt <<= 1
        plus::BigInt <<= 1
    end
    return num, den, minus, plus
end

function fixupmultiply10(estimated_power,is_even,num,den,minus,plus)
    in_range = is_even ? num + plus >= den : num + plus > den
    if in_range
        decimal_point::Int32 = estimated_power + 1
    else
        decimal_point = estimated_power
        num::BigInt *= 10
        if minus == plus
            minus::BigInt *= 10
             plus::BigInt = minus
        else
            minus *= 10
            plus *= 10
        end
    end
    return num, den, minus, plus, decimal_point
end

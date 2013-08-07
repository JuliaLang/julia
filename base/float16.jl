
function convert(::Type{Float32}, val::Float16)
    val = uint32(reinterpret(Uint16, val))
    sign = (val & 0x8000) >> 15
    exp  = (val & 0x7c00) >> 10
    sig  = (val & 0x3ff) >> 0
    ret::Uint32

    if exp == 0
        if sig == 0
            sign = sign << 31
            ret = sign | exp | sig
        else
            n_bit = 1
            bit = 0x0200
            while (bit & sig) == 0
                n_bit = n_bit + 1
                bit = bit >> 1
            end
            sign = sign << 31
            exp = (-14 - n_bit + 127) << 23
            sig = ((sig & (~bit)) << n_bit) << (23 - 10)
            ret = sign | exp | sig
        end
    elseif exp == 0x1f
        if sig == 0
            if sign == 0
                ret = 0x7f800000
            else
                ret = 0xff800000
            end
        else
            ret = 0xffffffff
        end
    else
        sign = sign << 31
        exp  = (exp - 15 + 127) << 23
        sig  = sig << (23 - 10)
        ret = sign | exp | sig
    end
    return reinterpret(Float32, ret)
end


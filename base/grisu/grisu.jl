# This file is a part of Julia. License is MIT: https://julialang.org/license

module Grisu

importall ..Base.Operators

export print_shortest
export DIGITS, grisu

const SHORTEST = 1
const FIXED = 2
const PRECISION = 3

const DIGITS = Vector{UInt8}(309+17)

include(joinpath("grisu", "float.jl"))
include(joinpath("grisu", "fastshortest.jl"))
include(joinpath("grisu", "fastprecision.jl"))
include(joinpath("grisu", "fastfixed.jl"))
include(joinpath("grisu", "bignums.jl"))
include(joinpath("grisu", "bignum.jl"))

const BIGNUMS = [Bignums.Bignum(),Bignums.Bignum(),Bignums.Bignum(),Bignums.Bignum()]

function grisu(v::AbstractFloat,mode,requested_digits,buffer=DIGITS,bignums=BIGNUMS)
    if signbit(v)
        neg = true
        v = -v
    else
        neg = false
    end
    if mode == PRECISION && requested_digits == 0
        buffer[1] = 0x00
        len = 0
        return 0, 0, neg
    end
    if v == 0.0
        buffer[1] = 0x30
        buffer[2] = 0x00
        len = point = 1
        return len, point, neg
    end
    if mode == SHORTEST
        status,len,point = fastshortest(v,buffer)
    elseif mode == FIXED
        status,len,point = fastfixedtoa(v,0,requested_digits,buffer)
    elseif mode == PRECISION
        status,len,point = fastprecision(v,requested_digits,buffer)
    end
    status && return len-1, point, neg
    status, len, point = bignumdtoa(v,mode,requested_digits,buffer,bignums)
    return len-1, point, neg
end

nanstr(x::AbstractFloat) = "NaN"
nanstr(x::Float32) = "NaN32"
nanstr(x::Float16) = "NaN16"
infstr(x::AbstractFloat) = "Inf"
infstr(x::Float32) = "Inf32"
infstr(x::Float16) = "Inf16"

function _show(io::IO, x::AbstractFloat, mode, n::Int, typed, compact)
    isnan(x) && return write(io, typed ? nanstr(x) : "NaN")
    if isinf(x)
        signbit(x) && write(io,'-')
        write(io, typed ? infstr(x) : "Inf")
        return
    end
    typed && isa(x,Float16) && write(io, "Float16(")
    (len,pt,neg),buffer = grisu(x,mode,n),DIGITS
    pdigits = pointer(buffer)
    if mode == PRECISION
        while len > 1 && buffer[len] == 0x30
            len -= 1
        end
    end
    neg && write(io,'-')
    exp_form = pt <= -4 || pt > 6
    exp_form = exp_form || (pt >= len && abs(mod(x + 0.05, 10^(pt - len)) - 0.05) > 0.05) # see issue #6608
    if exp_form # .00001 to 100000.
        # => #.#######e###
        unsafe_write(io, pdigits, 1)
        write(io, '.')
        if len > 1
            unsafe_write(io, pdigits+1, len-1)
        else
            write(io, '0')
        end
        write(io, (typed && isa(x,Float32)) ? 'f' : 'e')
        write(io, dec(pt-1))
        typed && isa(x,Float16) && write(io, ")")
        return
    elseif pt <= 0
        # => 0.00########
        write(io, "0.")
        while pt < 0
            write(io, '0')
            pt += 1
        end
        unsafe_write(io, pdigits, len)
    elseif pt >= len
        # => ########00.0
        unsafe_write(io, pdigits, len)
        while pt > len
            write(io, '0')
            len += 1
        end
        write(io, ".0")
    else # => ####.####
        unsafe_write(io, pdigits, pt)
        write(io, '.')
        unsafe_write(io, pdigits+pt, len-pt)
    end
    typed && !compact && isa(x,Float32) && write(io, "f0")
    typed && isa(x,Float16) && write(io, ")")
    nothing
end

function Base.show(io::IO, x::Union{Float64,Float32})
    if get(io, :compact, false)
        _show(io, x, PRECISION, 6, true, true)
    else
        _show(io, x, SHORTEST, 0, true, false)
    end
end

function Base.show(io::IO, x::Float16)
    if get(io, :compact, false)
        _show(io, x, PRECISION, 5, false, true)
    else
        _show(io, x, SHORTEST, 0, true, false)
    end
end

Base.print(io::IO, x::Float32) = _show(io, x, SHORTEST, 0, false, false)
Base.print(io::IO, x::Float16) = _show(io, x, SHORTEST, 0, false, false)

# normal:
#   0 < pt < len        ####.####           len+1
#   pt <= 0             0.000########       len-pt+1
#   len <= pt (dot)     ########000.        pt+1
#   len <= pt (no dot)  ########000         pt
# exponential:
#   pt <= 0             ########e-###       len+k+2
#   0 < pt              ########e###        len+k+1

function _print_shortest(io::IO, x::AbstractFloat, dot::Bool, mode, n::Int)
    isnan(x) && return write(io, "NaN")
    x < 0 && write(io,'-')
    isinf(x) && return write(io, "Inf")
    (len,pt,neg),buffer = grisu(x,mode,n),DIGITS
    pdigits = pointer(buffer)
    e = pt-len
    k = -9<=e<=9 ? 1 : 2
    if -pt > k+1 || e+dot > k+1
        # => ########e###
        unsafe_write(io, pdigits+0, len)
        write(io, 'e')
        write(io, dec(e))
        return
    elseif pt <= 0
        # => 0.000########
        write(io, "0.")
        while pt < 0
            write(io, '0')
            pt += 1
        end
        unsafe_write(io, pdigits+0, len)
    elseif e >= dot
        # => ########000.
        unsafe_write(io, pdigits+0, len)
        while e > 0
            write(io, '0')
            e -= 1
        end
        if dot
            write(io, '.')
        end
    else # => ####.####
        unsafe_write(io, pdigits+0, pt)
        write(io, '.')
        unsafe_write(io, pdigits+pt, len-pt)
    end
    nothing
end

print_shortest(io::IO, x::AbstractFloat, dot::Bool) = _print_shortest(io, x, dot, SHORTEST, 0)
print_shortest(io::IO, x::Union{AbstractFloat,Integer}) = print_shortest(io, float(x), false)

end # module

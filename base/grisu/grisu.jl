# This file is a part of Julia. License is MIT: https://julialang.org/license

module Grisu

export print_shortest
export DIGITS, DIGITSs, grisu

const SHORTEST = 1
const FIXED = 2
const PRECISION = 3

include("grisu/float.jl")
include("grisu/fastshortest.jl")
include("grisu/fastprecision.jl")
include("grisu/fastfixed.jl")
include("grisu/bignums.jl")
include("grisu/bignum.jl")

const DIGITS = Vector{UInt8}(undef, 309+17)
const BIGNUMS = [Bignums.Bignum(),Bignums.Bignum(),Bignums.Bignum(),Bignums.Bignum()]

# NOTE: DIGITS[s] is deprecated; you should use getbuf() instead.
const DIGITSs = [DIGITS]
const BIGNUMSs = [BIGNUMS]
function __init__()
    Threads.resize_nthreads!(DIGITSs)
    Threads.resize_nthreads!(BIGNUMSs)
end

function getbuf()
    tls = task_local_storage()
    d = get(tls, :DIGITS, nothing)
    if d === nothing
        d = Vector{UInt8}(undef, 309+17)
        tls[:DIGITS] = d
    end
    return d::Vector{UInt8}
end

"""
    (len, point, neg) = Grisu.grisu(v::AbstractFloat, mode, requested_digits, [buffer], [bignums])

Convert the number `v` to decimal using the Grisu algorithm.

`mode` can be one of:
 - `Grisu.SHORTEST`: convert to the shortest decimal representation which can be "round-tripped" back to `v`.
 - `Grisu.FIXED`: round to `requested_digits` digits.
 - `Grisu.PRECISION`: round to `requested_digits` significant digits.

The characters are written as bytes to `buffer`, with a terminating NUL byte, and `bignums` are used internally as part of the correction step. You can call `Grisu.getbuf()` to obtain a suitable task-local buffer.

The returned tuple contains:

 - `len`: the number of digits written to `buffer` (excluding NUL)
 - `point`: the location of the radix point relative to the start of the array (e.g. if
   `point == 3`, then the radix point should be inserted between the 3rd and 4th
   digit). Note that this can be negative (for very small values), or greater than `len`
   (for very large values).
 - `neg`: the signbit of `v` (see [`signbit`](@ref)).
"""
function grisu(v::AbstractFloat,mode,requested_digits,buffer=DIGITSs[Threads.threadid()],bignums=BIGNUMSs[Threads.threadid()])
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
    isnan(x) && return print(io, typed ? nanstr(x) : "NaN")
    if isinf(x)
        signbit(x) && print(io,'-')
        print(io, typed ? infstr(x) : "Inf")
        return
    end
    typed && isa(x,Float16) && print(io, "Float16(")
    buffer = getbuf()
    len, pt, neg = grisu(x,mode,n,buffer)
    pdigits = pointer(buffer)
    if mode == PRECISION
        while len > 1 && buffer[len] == 0x30
            len -= 1
        end
    end
    neg && print(io,'-')
    exp_form = pt <= -4 || pt > 6
    exp_form = exp_form || (pt >= len && abs(mod(x + 0.05, 10^(pt - len)) - 0.05) > 0.05) # see issue #6608
    if exp_form # .00001 to 100000.
        # => #.#######e###
        # assumes ASCII/UTF8 encoding of digits is okay for out:
        unsafe_write(io, pdigits, 1)
        print(io, '.')
        if len > 1
            unsafe_write(io, pdigits+1, len-1)
        else
            print(io, '0')
        end
        print(io, (typed && isa(x,Float32)) ? 'f' : 'e')
        print(io, string(pt - 1))
        typed && isa(x,Float16) && print(io, ")")
        return
    elseif pt <= 0
        # => 0.00########
        print(io, "0.")
        while pt < 0
            print(io, '0')
            pt += 1
        end
        unsafe_write(io, pdigits, len)
    elseif pt >= len
        # => ########00.0
        unsafe_write(io, pdigits, len)
        while pt > len
            print(io, '0')
            len += 1
        end
        print(io, ".0")
    else # => ####.####
        unsafe_write(io, pdigits, pt)
        print(io, '.')
        unsafe_write(io, pdigits+pt, len-pt)
    end
    typed && !compact && isa(x,Float32) && print(io, "f0")
    typed && isa(x,Float16) && print(io, ")")
    nothing
end

# normal:
#   0 < pt < len        ####.####           len+1
#   pt <= 0             0.000########       len-pt+1
#   len <= pt (dot)     ########000.        pt+1
#   len <= pt (no dot)  ########000         pt
# exponential:
#   pt <= 0             ########e-###       len+k+2
#   0 < pt              ########e###        len+k+1

function _print_shortest(io::IO, x::AbstractFloat, dot::Bool, mode, n::Int)
    isnan(x) && return print(io, "NaN")
    x < 0 && print(io,'-')
    isinf(x) && return print(io, "Inf")
    buffer = getbuf()
    len, pt, neg = grisu(x,mode,n,buffer)
    pdigits = pointer(buffer)
    e = pt-len
    k = -9<=e<=9 ? 1 : 2
    if -pt > k+1 || e+dot > k+1
        # => ########e###
        unsafe_write(io, pdigits+0, len)
        print(io, 'e')
        print(io, string(e))
        return
    elseif pt <= 0
        # => 0.000########
        print(io, "0.")
        while pt < 0
            print(io, '0')
            pt += 1
        end
        unsafe_write(io, pdigits+0, len)
    elseif e >= dot
        # => ########000.
        unsafe_write(io, pdigits+0, len)
        while e > 0
            print(io, '0')
            e -= 1
        end
        if dot
            print(io, '.')
        end
    else # => ####.####
        unsafe_write(io, pdigits+0, pt)
        print(io, '.')
        unsafe_write(io, pdigits+pt, len-pt)
    end
    nothing
end

"""
    print_shortest(io::IO, x)

Print the shortest possible representation, with the minimum number of consecutive non-zero
digits, of number `x`, ensuring that it would parse to the exact same number.
"""
print_shortest(io::IO, x::AbstractFloat, dot::Bool) = _print_shortest(io, x, dot, SHORTEST, 0)
print_shortest(io::IO, x::Union{AbstractFloat,Integer}) = print_shortest(io, float(x), false)

end # module

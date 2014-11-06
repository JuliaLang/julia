module Grisu

export print_shortest
export DIGITS, grisu

const SHORTEST = 1
const FIXED = 2
const PRECISION = 3

const DIGITS = Array(UInt8,309+17)

include("grisu/float.jl")
include("grisu/fastshortest.jl")
include("grisu/fastprecision.jl")
include("grisu/fastfixed.jl")
include("grisu/bignum.jl")

function grisu(v::FloatingPoint,mode,requested_digits,buffer=DIGITS)
    if signbit(v)
        neg = true
        v = -v
    else
        neg = false
    end
    if mode == PRECISION && requested_digits == 0
        buffer[1] = 0x00
        len = 0
        return 0, 0, neg, buffer
    end
    if v == 0.0
        buffer[1] = 0x30
        buffer[2] = 0x00
        len = point = 1
        return len, point, neg, buffer
    end
    if mode == SHORTEST
        status,len,point,buf = fastshortest(v,buffer)
    elseif mode == FIXED
        status,len,point,buf = fastfixedtoa(v,0,requested_digits,buffer)
    elseif mode == PRECISION
        status,len,point,buf = fastprecision(v,requested_digits,buffer)
    end
    status && return len-1, point, neg, buf
    status, len, point, buf = bignumdtoa(v,mode,requested_digits,buffer)
    return len-1, point, neg, buf
end

_show(io::IO, x::FloatingPoint, mode, n::Int, t) =
    _show(io, x, mode, n, t, "NaN", "Inf")
_show(io::IO, x::Float32, mode, n::Int, t) =
    _show(io, x, mode, n, t, "NaN32", "Inf32")
_show(io::IO, x::Float16, mode, n::Int, t) =
    _show(io, x, mode, n, t, "NaN16", "Inf16")

function _show(io::IO, x::FloatingPoint, mode, n::Int, typed, nanstr, infstr)
    isnan(x) && return write(io, typed ? nanstr : "NaN")
    if isinf(x)
        signbit(x) && write(io,'-')
        write(io, typed ? infstr : "Inf")
        return
    end
    typed && isa(x,Float16) && write(io, "float16(")
    len,pt,neg,buffer = grisu(x,mode,n)
    pdigits = pointer(buffer)
    if mode == PRECISION
        while len > 1 && buffer[len] == 0x30
            len -= 1
        end
    end
    neg && write(io,'-')
    if pt <= -4 || pt > 6 # .00001 to 100000.
        # => #.#######e###
        write(io, pdigits, 1)
        write(io, '.')
        if len > 1
            write(io, pdigits+1, len-1)
        else
            write(io, '0')
        end
        write(io, typed && isa(x,Float32) ? 'f' : 'e')
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
        write(io, pdigits, len)
    elseif pt >= len
        # => ########00.0
        write(io, pdigits, len)
        while pt > len
            write(io, '0')
            len += 1
        end
        write(io, ".0")
    else # => ####.####
        write(io, pdigits, pt)
        write(io, '.')
        write(io, pdigits+pt, len-pt)
    end
    typed && isa(x,Float32) && write(io, "f0")
    typed && isa(x,Float16) && write(io, ")")
    nothing
end

Base.show(io::IO, x::FloatingPoint) = _show(io, x, SHORTEST, 0, true)

Base.print(io::IO, x::Float32) = _show(io, x, SHORTEST, 0, false)
Base.print(io::IO, x::Float16) = _show(io, x, SHORTEST, 0, false)

Base.showcompact(io::IO, x::Float64) = _show(io, x, PRECISION, 6, false)
Base.showcompact(io::IO, x::Float32) = _show(io, x, PRECISION, 6, false)
Base.showcompact(io::IO, x::Float16) = _show(io, x, PRECISION, 5, false)

# normal:
#   0 < pt < len        ####.####           len+1
#   pt <= 0             .000########        len-pt+1
#   len <= pt (dot)     ########000.        pt+1
#   len <= pt (no dot)  ########000         pt
# exponential:
#   pt <= 0             ########e-###       len+k+2
#   0 < pt              ########e###        len+k+1

function _print_shortest(io::IO, x::FloatingPoint, dot::Bool, mode, n::Int)
    isnan(x) && return write(io, "NaN")
    x < 0 && write(io,'-')
    isinf(x) && return write(io, "Inf")
    len,pt,neg,buffer = grisu(x,mode,n)
    pdigits = pointer(buffer)
    e = pt-len
    k = -9<=e<=9 ? 1 : 2
    if -pt > k+1 || e+dot > k+1
        # => ########e###
        write(io, pdigits+0, len)
        write(io, 'e')
        write(io, dec(e))
        return
    elseif pt <= 0
        # => .000########
        write(io, '.')
        while pt < 0
            write(io, '0')
            pt += 1
        end
        write(io, pdigits+0, len)
    elseif e >= dot
        # => ########000.
        write(io, pdigits+0, len)
        while e > 0
            write(io, '0')
            e -= 1
        end
        if dot
            write(io, '.')
        end
    else # => ####.####
        write(io, pdigits+0, pt)
        write(io, '.')
        write(io, pdigits+pt, len-pt)
    end
    nothing
end

print_shortest(io::IO, x::FloatingPoint, dot::Bool) = _print_shortest(io, x, dot, SHORTEST, 0)
print_shortest(io::IO, x::Union(FloatingPoint,Integer)) = print_shortest(io, float(x), false)

end # module

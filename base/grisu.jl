module Grisu
export print_shortest
export @grisu_ccall, NEG, DIGITS, BUFLEN, LEN, POINT

import Base.show, Base.showcompact

const NEG    = Array(Bool,1)
const DIGITS = Array(Uint8,309+17)
const BUFLEN = int32(length(DIGITS)+1)
const LEN    = Array(Int32,1)
const POINT  = Array(Int32,1)

macro grisu_ccall(value, mode, ndigits)
    quote
        ccall((:grisu, :libgrisu), Void,
              (Float64, Int32, Int32, Ptr{Uint8}, Int32,
               Ptr{Bool}, Ptr{Int32}, Ptr{Int32}),
              $(esc(value)), $(esc(mode)), $(esc(ndigits)),
              DIGITS, BUFLEN, NEG, LEN, POINT)
    end
end

const SHORTEST        = int32(0) # shortest exact representation for doubles
const SHORTEST_SINGLE = int32(1) # shortest exact representation for singles
const FIXED           = int32(2) # fixed number of trailing decimal points
const PRECISION       = int32(3) # fixed precision regardless of magnitude

# wrapper for the core grisu function, primarily for debugging
function grisu(x::Float64, mode::Integer, ndigits::Integer)
    if !isfinite(x); error("non-finite value: $x"); end
    if ndigits < 0; error("negative digits requested"); end
    @grisu_ccall x mode ndigits
    NEG[1], DIGITS[1:LEN[1]], POINT[1]
end

grisu(x::Float64) = grisu(x, SHORTEST, int32(0))
grisu(x::Float32) = grisu(float64(x), SHORTEST_SINGLE, int32(0))
grisu(x::Real) = grisu(float(x))

function grisu_fix(x::Real, n::Integer)
    if n > 17; n = 17; end
    grisu(float64(x), FIXED, int32(n))
end
function grisu_sig(x::Real, n::Integer)
    if n > 309; n = 309; end
    grisu(float64(x), PRECISION, int32(n))
end

function _show(io::IO, x::FloatingPoint, mode::Int32, n::Int)
    if isnan(x) return write(io, isa(x,Float32) ? "NaN32" : "NaN") end
    if isinf(x)
        if x < 0 write(io,'-') end
        write(io, isa(x,Float32) ? "Inf32" : "Inf")
        return
    end
    @grisu_ccall x mode n
    pdigits = pointer(DIGITS)
    neg = NEG[1]
    len = LEN[1]
    pt  = POINT[1]
    if mode == PRECISION
        while len > 1 && DIGITS[len] == '0'
            len -= 1
        end
    end
    if neg write(io,'-') end
    if pt <= -4 || pt > 6 # .00001 to 100000.
        # => #.#######e###
        write(io, pdigits, 1)
        write(io, '.')
        if len > 1
            write(io, pdigits+1, len-1)
        else
            write(io, '0')
        end
        write(io, isa(x,Float32) ? 'f' : 'e')
        write(io, dec(pt-1))
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
    if isa(x,Float32) write(io, "f0") end
    nothing
end

show(io::IO, x::Float64) = _show(io, x, SHORTEST, 0)
show(io::IO, x::Float32) = _show(io, x, SHORTEST_SINGLE, 0)
showcompact(io::IO, x::FloatingPoint) = _show(io, x, PRECISION, 6)

# normal:
#   0 < pt < len        ####.####           len+1
#   pt <= 0             .000########        len-pt+1
#   len <= pt (dot)     ########000.        pt+1
#   len <= pt (no dot)  ########000         pt
# exponential:
#   pt <= 0             ########e-###       len+k+2
#   0 < pt              ########e###        len+k+1

function _print_shortest(io::IO, x::FloatingPoint, dot::Bool, mode::Int32)
    if isnan(x); return write(io, isa(x,Float32) ? "NaN32" : "NaN"); end
    if x < 0 write(io,'-') end
    if isinf(x); return write(io, isa(x,Float32) ? "Inf32" : "Inf"); end
    @grisu_ccall x mode 0
    pdigits = pointer(DIGITS)
    len = LEN[1]
    pt  = POINT[1]
    e = pt-len
    k = -9<=e<=9 ? 1 : 2
    if -pt > k+1 || e+dot > k+1
        # => ########e###
        write(io, pdigits+0, len)
        write(io, isa(x,Float32) ? 'f' : 'e')
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
    if isa(x,Float32) write(io, "f0") end
    nothing
end

print_shortest(io::IO, x::Float64, dot::Bool) = _print_shortest(io, x, dot, SHORTEST)
print_shortest(io::IO, x::Float32, dot::Bool) = _print_shortest(io, x, dot, SHORTEST_SINGLE)
print_shortest(io::IO, x::Union(FloatingPoint,Integer)) = print_shortest(io, float(x), false)

end # module

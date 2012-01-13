_jl_libgrisu = dlopen("libgrisu")

macro grisu_ccall(value, mode, ndigits)
    quote
        ccall(dlsym(_jl_libgrisu, :grisu), Void,
              (Float64, Int32, Int32, Ptr{Uint8}, Int32,
               Ptr{Bool}, Ptr{Int32}, Ptr{Int32}),
              float64($value), int32($mode), int32($ndigits),
              digits, buflen, _neg, _len, _pt)
        pdigits = pointer(digits)
        neg = _neg[1]
        len = _len[1]
        pt  = _pt[1]
    end
end

let digits = Array(Uint8,17), # maximum decimal digits for Float64
    _neg = Array(Bool, 1),
    _len = Array(Int32,1),
    _pt  = Array(Int32,1)

const buflen = int32(length(digits)+1)

const SHORTEST        = int32(0)
const SHORTEST_SINGLE = int32(1)
const FIXED           = int32(2)
const PRECISION       = int32(3)

# wrapper for the core grisu function, primarily for debugging
global grisu
function grisu(x::Float64, mode::Int32, ndigits::Integer)
    @grisu_ccall x mode ndigits
    neg, ASCIIString(digits[1:len]), pt
end
grisu(x::Float64) = grisu(x, SHORTEST, int32(0))
grisu(x::Float32) = grisu(float64(x), SHORTEST_SINGLE, int32(0))
grisu(x::Real) = grisu(float(x))
grisu(x::Real, n::Integer) = n >= 0 ? grisu(float64(x), PRECISION, int32(n)) :
                                      grisu(float64(x), FIXED,    -int32(n))

function _show(x::Float, mode::Int32)
    s = current_output_stream()
    if isnan(x); return write(s, "NaN"); end
    if isinf(x); return write(s, x < 0 ? "-Inf" : "Inf"); end
    @grisu_ccall x mode 0
    if neg
        write(s,'-')
    end
    if pt <= -4 || pt > 6 # .00001 to 100000.
        # => #.#######e###
        write(s, digits[1])
        write(s, '.')
        if len > 1
            write(s, pdigits+1, len-1)
        else
            write(s, '0')
        end
        write(s, 'e')
        write(s, dec(pt-1))
    elseif pt <= 0
        # => 0.00########
        write(s, "0.")
        while pt < 0
            write(s, '0')
            pt += 1
        end
        write(s, pdigits+0, len)
    elseif pt >= len
        # => ########00.0
        write(s, pdigits+0, len)
        while pt > len
            write(s, '0')
            len += 1
        end
        write(s, ".0")
    else # => ####.####
        write(s, pdigits+0, pt)
        write(s, '.')
        write(s, pdigits+pt, len-pt)
    end
    nothing
end

global show
global showcompact
show(x::Float64) = _show(x, SHORTEST)
show(x::Float32) = _show(x, SHORTEST_SINGLE)
showcompact(x::Float) = _show(x, SHORTEST_SINGLE)

# normal:
#   0 < pt < len        ####.####           len+1
#   pt <= 0             .000########        len-pt+1
#   len <= pt (dot)     ########000.        pt+1
#   len <= pt (no dot)  ########000         pt
# exponential:
#   pt <= 0             ########e-###       len+k+2
#   0 < pt              ########e###        len+k+1

function _print_shortest(x::Float, dot::Bool, mode::Int32)
    s = current_output_stream()
    if isnan(x); return write(s, "NaN"); end
    if isinf(x); return write(s, x < 0 ? "-Inf" : "Inf"); end
    @grisu_ccall x mode 0
    if neg
        write(s, '-')
    end
    e = pt-len
    k = -9<=e<=9 ? 1 : 2
    if -pt > k+1 || e+dot > k+1
        # => ########e###
        write(s, pdigits+0, len)
        write(s, 'e')
        write(s, dec(e))
    elseif pt <= 0
        # => .000########
        write(s, '.')
        while pt < 0
            write(s, '0')
            pt += 1
        end
        write(s, pdigits+0, len)
    elseif e >= dot
        # => ########000.
        write(s, pdigits+0, len)
        while e > 0
            write(s, '0')
            e -= 1
        end
        if dot
            write(s, '.')
        end
    else # => ####.####
        write(s, pdigits+0, pt)
        write(s, '.')
        write(s, pdigits+pt, len-pt)
    end
    nothing
end

global print_shortest
print_shortest(x::Float32, dot::Bool) = _print_shortest(x, dot, SHORTEST)
print_shortest(x::Float64, dot::Bool) = _print_shortest(x, dot, SHORTEST_SINGLE)
print_shortest(x::Union(Float,Integer)) = print_shortest(float(x), false)

end # let

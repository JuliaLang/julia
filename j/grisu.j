_jl_libgrisu = dlopen("libgrisu")

macro grisu_ccall(value, mode, ndigits)
    quote
        ccall(dlsym(_jl_libgrisu, :grisu), Void,
              (Float64, Int32, Int32, Ptr{Uint8}, Int32,
               Ptr{Bool}, Ptr{Int32}, Ptr{Int32}),
              float64($value), int32($mode), int32($ndigits),
              _digits, buflen, _neg, _len, _pt)
        pdigits = pointer(_digits)
        neg = _neg[1]
        len = _len[1]
        pt  = _pt[1]
    end
end

let _digits = Array(Uint8,309+17),
    _neg = Array(Bool, 1),
    _len = Array(Int32,1),
    _pt  = Array(Int32,1)

const buflen = int32(length(_digits)+1)

const SHORTEST        = int32(0) # shortest exact representation for doubles
const SHORTEST_SINGLE = int32(1) # shortest exact representation for singles
const FIXED           = int32(2) # fixed number of trailing decimal points
const PRECISION       = int32(3) # fixed precision regardless of magnitude

# wrapper for the core grisu function, primarily for debugging
global grisu, grisu_fix, grisu_sig
function grisu(x::Float64, mode::Integer, ndigits::Integer)
    if !isfinite(x); error("non-finite value: $x"); end
    if ndigits < 0; error("negative digits requested"); end
    @grisu_ccall x mode ndigits
    neg, pointer(_digits), len, pt
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
        write(s, pdigits, 1)
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
        write(s, pdigits, len)
    elseif pt >= len
        # => ########00.0
        write(s, pdigits, len)
        while pt > len
            write(s, '0')
            len += 1
        end
        write(s, ".0")
    else # => ####.####
        write(s, pdigits, pt)
        write(s, '.')
        write(s, pdigits+pt, len-pt)
    end
    nothing
end

global show
global showcompact
show(x::Float64) = _show(x, SHORTEST)
show(x::Float32) = _show(x, SHORTEST_SINGLE)
# showcompact(x::Float) = show(x, SHORTEST_SINGLE) # TODO: better short float printing.

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

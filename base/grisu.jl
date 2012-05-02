_jl_libgrisu = dlopen("libgrisu")

const _jl_neg    = Array(Bool,1)
const _jl_digits = Array(Uint8,309+17)
const _jl_buflen = int32(length(_jl_digits)+1)
const _jl_length = Array(Int32,1)
const _jl_point  = Array(Int32,1)

macro grisu_ccall(value, mode, ndigits)
    quote
        ccall(dlsym(_jl_libgrisu, :grisu), Void,
              (Float64, Int32, Int32, Ptr{Uint8}, Int32,
               Ptr{Bool}, Ptr{Int32}, Ptr{Int32}),
              $value, $mode, $ndigits,
              _jl_digits, _jl_buflen, _jl_neg, _jl_length, _jl_point)
    end
end

const GRISU_SHORTEST        = int32(0) # shortest exact representation for doubles
const GRISU_SHORTEST_SINGLE = int32(1) # shortest exact representation for singles
const GRISU_FIXED           = int32(2) # fixed number of trailing decimal points
const GRISU_PRECISION       = int32(3) # fixed precision regardless of magnitude

# wrapper for the core grisu function, primarily for debugging
global grisu, grisu_fix, grisu_sig
function grisu(x::Float64, mode::Integer, ndigits::Integer)
    if !isfinite(x); error("non-finite value: $x"); end
    if ndigits < 0; error("negative digits requested"); end
    @grisu_ccall x mode ndigits
    _jl_neg[1], _jl_digits[1:_jl_length[1]], _jl_point[1]
end

grisu(x::Float64) = grisu(x, GRISU_SHORTEST, int32(0))
grisu(x::Float32) = grisu(float64(x), GRISU_SHORTEST_SINGLE, int32(0))
grisu(x::Real) = grisu(float(x))

function grisu_fix(x::Real, n::Integer)
    if n > 17; n = 17; end
    grisu(float64(x), GRISU_FIXED, int32(n))
end
function grisu_sig(x::Real, n::Integer)
    if n > 309; n = 309; end
    grisu(float64(x), GRISU_PRECISION, int32(n))
end

function _show(s, x::Float, mode::Int32, n::Int)
    if isnan(x); return write(s, "NaN"); end
    if isinf(x); return write(s, x < 0 ? "-Inf" : "Inf"); end
    @grisu_ccall x mode n
    pdigits = pointer(_jl_digits)
    neg = _jl_neg[1]
    len = _jl_length[1]
    pt  = _jl_point[1]
    if mode == GRISU_PRECISION
        while len > 1 && _jl_digits[len] == '0'
            len -= 1
        end
    end
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

show(io, x::Float64) = _show(io, x, GRISU_SHORTEST, 0)
show(io, x::Float32) = _show(io, x, GRISU_SHORTEST_SINGLE, 0)
showcompact(io, x::Float) = _show(io, x, GRISU_PRECISION, 6)

# normal:
#   0 < pt < len        ####.####           len+1
#   pt <= 0             .000########        len-pt+1
#   len <= pt (dot)     ########000.        pt+1
#   len <= pt (no dot)  ########000         pt
# exponential:
#   pt <= 0             ########e-###       len+k+2
#   0 < pt              ########e###        len+k+1

function f_jl_print_shortest(io, x::Float, dot::Bool, mode::Int32)
    s = current_output_stream()
    if isnan(x); return write(s, "NaN"); end
    if isinf(x); return write(s, x < 0 ? "-Inf" : "Inf"); end
    @grisu_ccall x mode 0
    pdigits = pointer(_jl_digits)
    neg = _jl_neg[1]
    len = _jl_length[1]
    pt  = _jl_point[1]
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

print_shortest(io, x::Float64, dot::Bool) = f_jl_print_shortest(io, x, dot, GRISU_SHORTEST)
print_shortest(io, x::Float32, dot::Bool) = f_jl_print_shortest(io, x, dot, GRISU_SHORTEST_SINGLE)
print_shortest(io, x::Union(Float,Integer)) = print_shortest(io, float(x), false)

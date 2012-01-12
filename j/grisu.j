_jl_libgrisu = dlopen("libgrisu")

const GRISU_SHORTEST        = int32(0)
const GRISU_SHORTEST_SINGLE = int32(1)
const GRISU_FIXED           = int32(2)
const GRISU_PRECISION       = int32(3)

let sign = Array(Int32, 1), len = Array(Int32, 1), pt = Array(Int32, 1)
    global grisu
    function grisu(x::Float64, mode::Int32, digits::Integer, buf::Array{Uint8})
        ccall(dlsym(_jl_libgrisu, :grisu), Void,
              (Float64, Int32, Int32, Ptr{Uint8}, Int32,
               Ptr{Int32}, Ptr{Int32}, Ptr{Int32}),
              x, mode, int32(digits), buf, int32(length(buf)+1), sign, len, pt)
        bool(uint8(sign[1])), int(len[1]), int(pt[1])
    end
end
let buf = Array(Uint8, 17) # maximum decimal digits for Float64
    global grisu
    function grisu(x::Float64, mode::Int32, digits::Integer)
        sign, len, pt = grisu(x, mode, int32(digits), buf)
        sign, ASCIIString(buf[1:len]), pt
    end
end

grisu(x::Float64) = grisu(x, GRISU_SHORTEST, int32(0))
grisu(x::Float32) = grisu(float64(x), GRISU_SHORTEST_SINGLE, int32(0))
grisu(x::Real) = grisu(float(x))
grisu(x::Real, n::Integer) = n >= 0 ? grisu(float64(x), GRISU_PRECISION, int32(n)) :
                                      grisu(float64(x), GRISU_FIXED,    -int32(n))

# normal:
#   0 < pt < n          ####.####           n+1
#   pt <= 0             .000########        n-pt+1
#   n <= pt (dot)       ########000.        pt+1
#   n <= pt (no dot)    ########000         pt
# exponential:
#   pt <= 0             ########e-###       n+k+2
#   0 < pt              ########e###        n+k+1

function print_shortest(x::Real, dot::Bool)
    if isnan(x); return print("NaN"); end
    if isinf(x); return print(x < 0 ? "-Inf" : "Inf"); end
    sign, digits, pt = grisu(x)
    n = length(digits)
    if sign
        print('-')
    end
    e = pt-n
    k = -9<=e<=9 ? 1 : 2
    if -pt > k+1 || e+dot > k+1
        # => ########e###
        print(digits)
        print('e')
        print(e)
    elseif pt <= 0
        # => .000########
        print('.')
        while pt < 0
            print('0')
            pt += 1
        end
        print(digits)
    elseif e >= dot
        # => ########000.
        print(digits)
        while e > 0
            print('0')
            e -= 1
        end
        if dot
            print('.')
        end
    else # => ####.####
        print(digits[1:pt])
        print('.')
        print(digits[pt+1:])
    end
end
print_shortest(x::Real) = print_shortest(x, false)

function show(x::Float)
    if isnan(x); return print("NaN"); end
    if isinf(x); return print(x < 0 ? "-Inf" : "Inf"); end
    sign, digits, pt = grisu(x)
    n = length(digits)
    if sign
        print('-')
    end
    if pt <= -4 || pt > 6 # .00001 to 100000.
        # => #.#######e###
        print(digits[1])
        print('.')
        if n > 1
            print(digits[2:])
        else
            print('0')
        end
        print('e')
        print(pt-1)
    elseif pt <= 0
        # => 0.00########
        print("0.")
        while pt < 0
            print('0')
            pt += 1
        end
        print(digits)
    elseif pt >= n
        # => ########00.0
        print(digits)
        while pt > n
            print('0')
            n += 1
        end
        print(".0")
    else # => ####.####
        print(digits[1:pt])
        print('.')
        print(digits[pt+1:])
    end
end

showcompact(x::Float) = print(float32(x))

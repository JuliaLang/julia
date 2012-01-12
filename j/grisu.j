libgrisu = dlopen("libgrisu")

const GRISU_SHORTEST        = int32(0)
const GRISU_SHORTEST_SINGLE = int32(1)
const GRISU_FIXED           = int32(2)
const GRISU_PRECISION       = int32(3)

let sign = Array(Int32, 1), len = Array(Int32, 1), pt = Array(Int32, 1)
    global grisu
    function grisu(x::Float64, mode::Int32, digits::Integer, buf::Array{Uint8})
        ccall(dlsym(libgrisu, :grisu), Void,
              (Float64, Int32, Int32, Ptr{Uint8}, Int32,
               Ptr{Int32}, Ptr{Int32}, Ptr{Int32}),
              x, mode, int32(digits), buf, int32(length(buf)+1), sign, len, pt)
        bool(sign[1]), int(len[1]), int(pt[1])
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
#   0 <= pt <= n                ####.####           n+1
#   pt < 0                      .000########        n-pt+1
#   n < pt                      ########000.        pt+1
# scientific:
#   pt <= 0                     #.#######e-###      n+3+k
#   0 < pt                      #.#######e###       n+2+k

function print_shortest_float(x::Real)
    sign, digits, pt = grisu(x)
    n = length(digits)
    if sign
        print('-')
    end
    k = -9<=pt<=9 ? 1 : -99<=pt<=99 ? 2 : 3
    if n == 1
        k -= 1
    end
    if pt < -k-2 || pt > n+k+1
        # => #.#######e###
        print(digits[1])
        if n != 1
            print('.')
        end
        print(digits[2:],'e',pt-1)
    elseif pt < 0
        # => .000########
        print('.')
        while pt < 0
            print('0')
            pt += 1
        end
        print(digits)
    elseif pt > n
        # => ########000.
        print(digits)
        while pt > n
            print('0')
            n += 1
        end
        print('.')
    else # 0 <= pt <= n
        print(digits[1:pt],'.',digits[pt+1:])
    end
end

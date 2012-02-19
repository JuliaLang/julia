_jl_libfdm = dlopen("libfdm")
_jl_libm = dlopen("libm")
_jl_libamos = dlopen("libamos")

macro _jl_libmfunc_1arg_float(T,f)
    quote
        ($f)(x::Float64) = ccall(dlsym(_jl_libm,$string(f)), Float64, (Float64,), x)
        ($f)(x::Float32) = ccall(dlsym(_jl_libm,$strcat(string(f),"f")), Float32, (Float32,), x)
        ($f)(x::Real) = ($f)(float(x))
        @vectorize_1arg $T $f
    end
end

macro _jl_libfdmfunc_1arg_float(T,f)
    quote
        ($f)(x::Float64) = ccall(dlsym(_jl_libfdm,$string(f)), Float64, (Float64,), x)
        ($f)(x::Float32) = ccall(dlsym(_jl_libfdm,$strcat(string(f),"f")), Float32, (Float32,), x)
        ($f)(x::Real) = ($f)(float(x))
        @vectorize_1arg $T $f
    end
end

macro _jl_libmfunc_1arg_int(T,f,name...)
    if length(name)>0
        fname = name[1]
    else
        fname = f
    end
    quote
        ($fname)(x::Float64) = ccall(dlsym(_jl_libm,$string(f)), Int32, (Float64,), x)
        ($fname)(x::Float32) = ccall(dlsym(_jl_libm,$strcat(string(f),"f")), Int32, (Float32,), x)
        @vectorize_1arg $T $fname
    end
end

macro _jl_libfdmfunc_2arg(T,f)
    quote
        ($f)(x::Float64, y::Float64) = ccall(dlsym(_jl_libfdm,$string(f)), Float64, (Float64, Float64,), x, y)
        ($f)(x::Float32, y::Float32) = ccall(dlsym(_jl_libfdm,$strcat(string(f),"f")), Float32, (Float32, Float32), x, y)
        @vectorize_2arg $T $f
    end
end

@_jl_libfdmfunc_1arg_float Number cbrt
@_jl_libfdmfunc_1arg_float Number sin
@_jl_libfdmfunc_1arg_float Number cos
@_jl_libfdmfunc_1arg_float Number tan
@_jl_libfdmfunc_1arg_float Number sinh
@_jl_libfdmfunc_1arg_float Number cosh
@_jl_libfdmfunc_1arg_float Number tanh
@_jl_libfdmfunc_1arg_float Number asin
@_jl_libfdmfunc_1arg_float Number acos
@_jl_libfdmfunc_1arg_float Number atan
@_jl_libfdmfunc_1arg_float Number log
@_jl_libfdmfunc_1arg_float Number log2
@_jl_libfdmfunc_1arg_float Number log10
@_jl_libfdmfunc_1arg_float Real   log1p
#@_jl_libfdmfunc_1arg_float Real   logb
@_jl_libfdmfunc_1arg_float Number exp
@_jl_libfdmfunc_1arg_float Real   expm1
@_jl_libfdmfunc_1arg_float Number erf
@_jl_libfdmfunc_1arg_float Number erfc
@_jl_libfdmfunc_1arg_float Real   ceil
@_jl_libfdmfunc_1arg_float Real   floor
#@_jl_libfdmfunc_1arg_float Real   rint
#@_jl_libfdmfunc_1arg_float Number lgamma

@_jl_libmfunc_1arg_float Number sqrt
@_jl_libmfunc_1arg_float Number exp2
#@_jl_libmfunc_1arg_float Real   nearbyint
@_jl_libmfunc_1arg_float Real   trunc
@_jl_libmfunc_1arg_float Real   round

#@_jl_libmfunc_1arg_int Real lrint
#@_jl_libmfunc_1arg_int Real lround iround
#@_jl_libmfunc_1arg_int Real ilogb

@_jl_libfdmfunc_2arg Number atan2
atan2(x::Real, y::Real) = atan2(float64(x), float64(y))
#@_jl_libfdmfunc_2arg Real   copysign
@_jl_libfdmfunc_2arg Number hypot
hypot(x::Float32, y::Float64) = hypot(float64(x), y)
hypot(x::Float64, y::Float32) = hypot(x, float64(y))

ipart(x) = trunc(x)
fpart(x) = x - trunc(x)
@vectorize_1arg Real ipart
@vectorize_1arg Real fpart

#abs(x::Float64) = ccall(dlsym(_jl_libfdm, :fabs),  Float64, (Float64,), x)
#abs(x::Float32) = ccall(dlsym(_jl_libfdm, :fabsf), Float32, (Float32,), x)
#@vectorize_1arg Number abs

lgamma(x::Float64) = ccall(dlsym(_jl_libamos,:dgamln_),Float64,(Ptr{Float64},Ptr{Int32}),x,int32(0))
lgamma(x::Float32) = ccall(dlsym(_jl_libamos,:gamln_),Float32,(Ptr{Float32},Ptr{Int32}),x,int32(0))
lgamma(x::Real) = lgamma(float(x))
@vectorize_1arg Number lgamma

gamma(x::Float64) = ccall(dlsym(_jl_libfdm, :tgamma),  Float64, (Float64,), x)
gamma(x::Float32) = float32(gamma(float64(x)))
gamma(x::Real) = gamma(float(x))
@vectorize_1arg Number gamma

lfact(x::Real) = (x<=1 ? zero(x) : lgamma(x+one(x)))
@vectorize_1arg Number lfact

max(x::Float64, y::Float64) = ccall(dlsym(_jl_libm, :fmax),  Float64, (Float64,Float64), x, y)
max(x::Float32, y::Float32) = ccall(dlsym(_jl_libm, :fmaxf), Float32, (Float32,Float32), x, y)
@vectorize_2arg Real max

min(x::Float64, y::Float64) = ccall(dlsym(_jl_libm, :fmin),  Float64, (Float64,Float64), x, y)
min(x::Float32, y::Float32) = ccall(dlsym(_jl_libm, :fminf), Float32, (Float32,Float32), x, y)
@vectorize_2arg Real min

ldexp(x::Float64,e::Int) = ccall(dlsym(_jl_libfdm, :ldexp),  Float64, (Float64,Int32), x, int32(e))
ldexp(x::Float32,e::Int) = ccall(dlsym(_jl_libfdm, :ldexpf), Float32, (Float32,Int32), x, int32(e))
# TODO: vectorize does not do the right thing for these argument types
#@vectorize_2arg Real ldexp

begin
    local exp::Array{Int32,1} = zeros(Int32,1)
    global frexp
    function frexp(x::Float64)
        s = ccall(dlsym(_jl_libfdm,:frexp), Float64, (Float64, Ptr{Int32}), x, exp)
        (s, int(exp[1]))
    end
    function frexp(x::Float32)
        s = ccall(dlsym(_jl_libfdm,:frexpf), Float32, (Float32, Ptr{Int32}), x, exp)
        (s, int(exp[1]))
    end
    function frexp(A::Array{Float64})
        f = similar(A)
        e = Array(Int, size(A))
        for i = 1:numel(A)
            f[i] = ccall(dlsym(_jl_libfdm,:frexp), Float64, (Float64, Ptr{Int32}), A[i], exp)
            e[i] = exp[1]
        end
        return (f, e)
    end
    function frexp(A::Array{Float32})
        f = similar(A)
        e = Array(Int, size(A))
        for i = 1:numel(A)
            f[i] = ccall(dlsym(_jl_libfdm,:frexpf), Float32, (Float32, Ptr{Int32}), A[i], exp)
            e[i] = exp[1]
        end
        return (f, e)
    end
end

^(x::Float64, y::Float64) = ccall(dlsym(_jl_libfdm, :pow),  Float64, (Float64,Float64), x, y)
^(x::Float32, y::Float32) = ccall(dlsym(_jl_libfdm, :powf), Float32, (Float32,Float32), x, y)

^(x::Float64, y::Integer) = x^float64(y)
^(x::Float32, y::Integer) = x^float32(y)

# alias
const pow = ^

besselj0(x::Float64) = ccall(dlsym(_jl_libfdm, :j0),  Float64, (Float64,), x)
besselj0(x::Float32) = ccall(dlsym(_jl_libfdm, :j0f), Float32, (Float32,), x)
@vectorize_1arg Real besselj0
besselj1(x::Float64) = ccall(dlsym(_jl_libfdm, :j1),  Float64, (Float64,), x)
besselj1(x::Float32) = ccall(dlsym(_jl_libfdm, :j1f), Float32, (Float32,), x)
@vectorize_1arg Real besselj1

bessely0(x::Float64) = ccall(dlsym(_jl_libfdm, :y0),  Float64, (Float64,), x)
bessely0(x::Float32) = ccall(dlsym(_jl_libfdm, :y0f), Float32, (Float32,), x)
@vectorize_1arg Real bessely0
bessely1(x::Float64) = ccall(dlsym(_jl_libfdm, :y1),  Float64, (Float64,), x)
bessely1(x::Float32) = ccall(dlsym(_jl_libfdm, :y1f), Float32, (Float32,), x)
@vectorize_1arg Real bessely1

let
    const ai::Array{Float64,1} = Array(Float64,2)
    const ae::Array{Int32,1} = Array(Int32,2)
global airy
function airy(k::Int, z::Complex128)
    id = int32(k==1 || k==3)
    if k == 0 || k == 1
        ccall(dlsym(_jl_libamos, :zairy_), Void,
              (Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32},
               Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32}),
              real(z), imag(z),
              id, int32(1),
              pointer(ai,1), pointer(ai,2),
              pointer(ae,1), pointer(ae,2))
        return complex(ai[1],ai[2])
    elseif k == 2 || k == 3
        ccall(dlsym(_jl_libamos, :zbiry_), Void,
              (Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32},
               Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32}),
              real(z), imag(z),
              id, int32(1),
              pointer(ai,1), pointer(ai,2),
              pointer(ae,1), pointer(ae,2))
        return complex(ai[1],ai[2])
    else
        error("airy: invalid argument")
    end
end
end

airy(z) = airy(0,z)
airy(k, x::Float) = oftype(x, real(airy(k, complex(x))))
airy(k, x::Real) = airy(k, float(x))
airy(k, z::Complex64) = complex64(airy(k, complex128(z)))
airy(k, z::Complex) = airy(k, complex128(z))

let
    const cy::Array{Float64,1} = Array(Float64,2)
    const ae::Array{Int32,1} = Array(Int32,2)
    const wrk::Array{Float64,1} = Array(Float64,2)

    function _besseli(nu::Float64, z::Complex128)
        ccall(dlsym(_jl_libamos, :zbesi_), Void,
              (Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32},
               Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32}),
              real(z), imag(z), nu, int32(1), int32(1),
              pointer(cy,1), pointer(cy,2),
              pointer(ae,1), pointer(ae,2))
        return complex(cy[1],cy[2])
    end

    function _besselj(nu::Float64, z::Complex128)
        ccall(dlsym(_jl_libamos, :zbesj_), Void,
              (Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32},
               Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32}),
              real(z), imag(z), nu, int32(1), int32(1),
              pointer(cy,1), pointer(cy,2),
              pointer(ae,1), pointer(ae,2))
        return complex(cy[1],cy[2])
    end

    function _besselk(nu::Float64, z::Complex128)
        ccall(dlsym(_jl_libamos, :zbesk_), Void,
              (Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32},
               Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32}),
              real(z), imag(z), nu, int32(1), int32(1),
              pointer(cy,1), pointer(cy,2),
              pointer(ae,1), pointer(ae,2))
        return complex(cy[1],cy[2])
    end

    function _bessely(nu::Float64, z::Complex128)
        ccall(dlsym(_jl_libamos, :zbesy_), Void,
              (Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32},
               Ptr{Int32}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32},
               Ptr{Float64}, Ptr{Float64}, Ptr{Int32}),
              real(z), imag(z), nu, int32(1), int32(1),
              pointer(cy,1), pointer(cy,2),
              pointer(ae,1), pointer(wrk,1),
              pointer(wrk,2), pointer(ae,2))
        return complex(cy[1],cy[2])
    end

    global besseli
    function besseli(nu::Float64, z::Complex128)
        if nu < 0
            return _besseli(-nu,z) - 2_besselk(-nu,z)sin(pi*nu)/pi
        else
            return _besseli(nu, z)
        end
    end

    global besselj
    function besselj(nu::Float64, z::Complex128)
        if nu < 0
            return _besselj(-nu,z)cos(pi*nu) + _bessely(-nu,z)sin(pi*nu)
        else
            return _besselj(nu, z)
        end
    end

    function besselj(nu::Int, x::Float)
        if x == 0
            return (nu == 0) ? one(x) : zero(x)
        end
        if nu < 0
            nu = -nu
            x = -x
        end
        ans = _besselj(float64(nu), complex128(abs(x)))
        if (x < 0) && (nu % 2 == 1)
            ans = -ans
        end
        oftype(x, real(ans))
    end

    global besselk
    besselk(nu::Float64, z::Complex128) = _besselk(abs(nu), z)

    global bessely
    function bessely(nu::Float64, z::Complex128)
        if nu < 0
            return _bessely(-nu,z)cos(pi*nu) - _besselj(-nu,z)sin(pi*nu)
        else
            return _bessely(nu, z)
        end
    end
end

besseli(nu::Real, z::Complex64) = complex64(bessely(float64(nu), complex128(z)))
besseli(nu::Real, z::Complex) = besseli(float64(nu), complex128(z))
besseli(nu::Real, x::Real) = besseli(float64(nu), complex128(x))

function besselj(nu::Float, x::Float)
    ans = besselj(float64(nu), complex128(x))
    (x > 0) ? oftype(x, real(ans)) : ans
end

besselj(nu::Real, z::Complex64) = complex64(besselj(float64(nu), complex128(z)))
besselj(nu::Real, z::Complex) = besselj(float64(nu), complex128(z))
besselj(nu::Int, x::Real) = besselj(nu, float(x))
besselj(nu::Real, x::Real) = besselj(float(nu), float(x))

besselk(nu::Real, z::Complex64) = complex64(besselk(float64(nu), complex128(z)))
besselk(nu::Real, z::Complex) = besselk(float64(nu), complex128(z))
besselk(nu::Real, x::Real) = besselk(float64(nu), complex128(x))

bessely(nu::Real, z::Complex64) = complex64(bessely(float64(nu), complex128(z)))
bessely(nu::Real, z::Complex) = bessely(float64(nu), complex128(z))
bessely(nu::Real, x::Real) = bessely(float64(nu), complex128(x))

hankelh1(nu, z) = besselj(nu, z) + bessely(nu, z)im
hankelh2(nu, z) = besselj(nu, z) - bessely(nu, z)im

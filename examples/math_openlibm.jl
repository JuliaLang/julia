_jl_libopenlibm = dlopen("libopenlibm")
_jl_libm = dlopen("libm")

macro _jl_libmfunc_1arg_float(T,f)
    quote
        ($f)(x::Float64) = ccall(dlsym(_jl_libm,$string(f)), Float64, (Float64,), x)
        ($f)(x::Float32) = ccall(dlsym(_jl_libm,$strcat(string(f),"f")), Float32, (Float32,), x)
        ($f)(x::Real) = ($f)(float(x))
        @vectorize_1arg $T $f
    end
end

macro _jl_libopenlibmfunc_1arg_float(T,f)
    quote
        ($f)(x::Float64) = ccall(dlsym(_jl_libopenlibm,$string(f)), Float64, (Float64,), x)
        ($f)(x::Float32) = ccall(dlsym(_jl_libopenlibm,$strcat(string(f),"f")), Float32, (Float32,), x)
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

macro _jl_libopenlibmfunc_2arg(T,f)
    quote
        ($f)(x::Float64, y::Float64) = ccall(dlsym(_jl_libopenlibm,$string(f)), Float64, (Float64, Float64,), x, y)
        ($f)(x::Float32, y::Float32) = ccall(dlsym(_jl_libopenlibm,$strcat(string(f),"f")), Float32, (Float32, Float32), x, y)
        @vectorize_2arg $T $f
    end
end

@_jl_libopenlibmfunc_1arg_float Number cbrt
@_jl_libopenlibmfunc_1arg_float Number sin
@_jl_libopenlibmfunc_1arg_float Number cos
@_jl_libopenlibmfunc_1arg_float Number tan
@_jl_libopenlibmfunc_1arg_float Number sinh
@_jl_libopenlibmfunc_1arg_float Number cosh
@_jl_libopenlibmfunc_1arg_float Number tanh
@_jl_libopenlibmfunc_1arg_float Number asin
@_jl_libopenlibmfunc_1arg_float Number acos
@_jl_libopenlibmfunc_1arg_float Number atan
@_jl_libopenlibmfunc_1arg_float Number log
@_jl_libopenlibmfunc_1arg_float Number log2
@_jl_libopenlibmfunc_1arg_float Number log10
@_jl_libopenlibmfunc_1arg_float Real   log1p
@_jl_libopenlibmfunc_1arg_float Real   logb
@_jl_libopenlibmfunc_1arg_float Number exp
@_jl_libopenlibmfunc_1arg_float Real   expm1
@_jl_libopenlibmfunc_1arg_float Number erf
@_jl_libopenlibmfunc_1arg_float Number erfc
@_jl_libopenlibmfunc_1arg_float Real   ceil
@_jl_libopenlibmfunc_1arg_float Real   floor
#@_jl_libopenlibmfunc_1arg_float Real   rint
@_jl_libopenlibmfunc_1arg_float Number lgamma

@_jl_libmfunc_1arg_float Number sqrt
@_jl_libmfunc_1arg_float Number exp2
#@_jl_libmfunc_1arg_float Real   nearbyint
@_jl_libmfunc_1arg_float Real   trunc
@_jl_libmfunc_1arg_float Real   round

#@_jl_libmfunc_1arg_int Real lrint
#@_jl_libmfunc_1arg_int Real lround iround
@_jl_libmfunc_1arg_int Real ilogb

@_jl_libopenlibmfunc_2arg Number atan2
atan2(x::Real, y::Real) = atan2(float64(x), float64(y))
#@_jl_libopenlibmfunc_2arg Real   copysign
@_jl_libopenlibmfunc_2arg Number hypot
hypot(x::Float32, y::Float64) = hypot(float64(x), y)
hypot(x::Float64, y::Float32) = hypot(x, float64(y))

ipart(x) = trunc(x)
fpart(x) = x - trunc(x)
@vectorize_1arg Real ipart
@vectorize_1arg Real fpart

#abs(x::Float64) = ccall(dlsym(_jl_libopenlibm, :fabs),  Float64, (Float64,), x)
#abs(x::Float32) = ccall(dlsym(_jl_libopenlibm, :fabsf), Float32, (Float32,), x)
#@vectorize_1arg Number abs

gamma(x::Float64) = ccall(dlsym(_jl_libopenlibm, :tgamma),  Float64, (Float64,), x)
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

ldexp(x::Float64,e::Int32) = ccall(dlsym(_jl_libopenlibm, :ldexp),  Float64, (Float64,Int32), x, e)
ldexp(x::Float32,e::Int32) = ccall(dlsym(_jl_libopenlibm, :ldexpf), Float32, (Float32,Int32), x, e)
# TODO: vectorize does not do the right thing for these argument types
#@vectorize_2arg Real ldexp

begin
    local exp::Array{Int32,1} = zeros(Int32,1)
    global frexp
    function frexp(x::Float64)
        s = ccall(dlsym(_jl_libopenlibm,:frexp), Float64, (Float64, Ptr{Int32}), x, exp)
        (s, exp[1])
    end
    function frexp(x::Float32)
        s = ccall(dlsym(_jl_libopenlibm,:frexpf), Float32, (Float32, Ptr{Int32}), x, exp)
        (s, exp[1])
    end
end
#@vectorize_1arg Real frexp

^(x::Float64, y::Float64) = ccall(dlsym(_jl_libopenlibm, :pow),  Float64, (Float64,Float64), x, y)
^(x::Float32, y::Float32) = ccall(dlsym(_jl_libopenlibm, :powf), Float32, (Float32,Float32), x, y)

# alias
const pow = ^

besselj0(x::Float64) = ccall(dlsym(_jl_libopenlibm, :j0),  Float64, (Float64,), x)
besselj0(x::Float32) = ccall(dlsym(_jl_libopenlibm, :j0f), Float32, (Float32,), x)
@vectorize_1arg Real besselj0
besselj1(x::Float64) = ccall(dlsym(_jl_libopenlibm, :j1),  Float64, (Float64,), x)
besselj1(x::Float32) = ccall(dlsym(_jl_libopenlibm, :j1f), Float32, (Float32,), x)
@vectorize_1arg Real besselj1

bessely0(x::Float64) = ccall(dlsym(_jl_libopenlibm, :y0),  Float64, (Float64,), x)
bessely0(x::Float32) = ccall(dlsym(_jl_libopenlibm, :y0f), Float32, (Float32,), x)
@vectorize_1arg Real bessely0
bessely1(x::Float64) = ccall(dlsym(_jl_libopenlibm, :y1),  Float64, (Float64,), x)
bessely1(x::Float32) = ccall(dlsym(_jl_libopenlibm, :y1f), Float32, (Float32,), x)
@vectorize_1arg Real bessely1

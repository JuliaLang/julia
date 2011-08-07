libfdm = dlopen("libfdm")
libm = dlopen("libm")

macro libmfunc_1arg_float(T,f)
    quote
        ($f)(x::Float64) = ccall(dlsym(libm,$string(f)), Float64, (Float64,), x)
        ($f)(x::Float32) = ccall(dlsym(libm,$strcat(string(f),"f")), Float32, (Float32,), x)
        ($f)(x::Real) = ($f)(float(x))
        @vectorize_1arg $T $f
    end
end

macro libfdmfunc_1arg_float(T,f)
    quote
        ($f)(x::Float64) = ccall(dlsym(libfdm,$string(f)), Float64, (Float64,), x)
        ($f)(x::Float32) = ccall(dlsym(libfdm,$strcat(string(f),"f")), Float32, (Float32,), x)
        ($f)(x::Real) = ($f)(float(x))
        @vectorize_1arg $T $f
    end
end

macro libmfunc_1arg_int(T,f,name...)
    if length(name)>0
        fname = name[1]
    else
        fname = f
    end
    quote
        ($fname)(x::Float64) = ccall(dlsym(libm,$string(f)), Int32, (Float64,), x)
        ($fname)(x::Float32) = ccall(dlsym(libm,$strcat(string(f),"f")), Int32, (Float32,), x)
        @vectorize_1arg $T $fname
    end
end

macro libfdmfunc_2arg(T,f)
    quote
        ($f)(x::Float64, y::Float64) = ccall(dlsym(libfdm,$string(f)), Float64, (Float64, Float64,), x, y)
        ($f)(x::Float32, y::Float32) = ccall(dlsym(libfdm,$strcat(string(f),"f")), Float32, (Float32, Float32), x, y)
        ($f)(x::Real, y::Real) = ($f)(float(x),float(y))
        @vectorize_2arg $T $f
    end
end

@libfdmfunc_1arg_float Number cbrt
@libfdmfunc_1arg_float Number sin
@libfdmfunc_1arg_float Number cos
@libfdmfunc_1arg_float Number tan
@libfdmfunc_1arg_float Number sinh
@libfdmfunc_1arg_float Number cosh
@libfdmfunc_1arg_float Number tanh
@libfdmfunc_1arg_float Number asin
@libfdmfunc_1arg_float Number acos
@libfdmfunc_1arg_float Number atan
@libfdmfunc_1arg_float Number log
@libfdmfunc_1arg_float Number log2
@libfdmfunc_1arg_float Number log10
@libfdmfunc_1arg_float Real   log1p
@libfdmfunc_1arg_float Real   logb
@libfdmfunc_1arg_float Number exp
@libfdmfunc_1arg_float Real   expm1
@libfdmfunc_1arg_float Number erf
@libfdmfunc_1arg_float Number erfc
@libfdmfunc_1arg_float Real   ceil
@libfdmfunc_1arg_float Real   floor
#@libfdmfunc_1arg_float Real   rint
@libfdmfunc_1arg_float Number lgamma

@libmfunc_1arg_float Number sqrt
@libmfunc_1arg_float Number exp2
#@libmfunc_1arg_float Real   nearbyint
@libmfunc_1arg_float Real   trunc
@libmfunc_1arg_float Real   round

#@libmfunc_1arg_int Real lrint
@libmfunc_1arg_int Real lround iround
@libmfunc_1arg_int Real ilogb

@libfdmfunc_2arg Number atan2
@libfdmfunc_2arg Real   copysign
@libfdmfunc_2arg Number hypot

ipart(x) = trunc(x)
fpart(x) = x - trunc(x)
@vectorize_1arg Real ipart
@vectorize_1arg Real fpart

abs(x::Float64) = ccall(dlsym(libfdm, :fabs),  Float64, (Float64,), x)
abs(x::Float32) = ccall(dlsym(libfdm, :fabsf), Float32, (Float32,), x)
@vectorize_1arg Number abs

gamma(x::Float64) = ccall(dlsym(libfdm, :tgamma),  Float64, (Float64,), x)
gamma(x::Float32) = float32(gamma(float64(x)))
gamma(x::Real) = gamma(float(x))
@vectorize_1arg Number gamma

max(x::Float64, y::Float64) = ccall(dlsym(libm, :fmax),  Float64, (Float64,Float64), x, y)
max(x::Float32, y::Float32) = ccall(dlsym(libm, :fmaxf), Float32, (Float32,Float32), x, y)
@vectorize_2arg Real max

min(x::Float64, y::Float64) = ccall(dlsym(libm, :fmin),  Float64, (Float64,Float64), x, y)
min(x::Float32, y::Float32) = ccall(dlsym(libm, :fminf), Float32, (Float32,Float32), x, y)
@vectorize_2arg Real min

ldexp(x::Float64,e::Int32) = ccall(dlsym(libfdm, :ldexp),  Float64, (Float64,Int32), x, e)
ldexp(x::Float32,e::Int32) = ccall(dlsym(libfdm, :ldexpf), Float32, (Float32,Int32), x, e)
@vectorize_2arg Real ldexp

function frexp(x::Float64)
    exp = zeros(Int32,1)
    s = ccall(dlsym(libfdm,:frexp), Float64, (Float64, Ptr{Int32}), x, exp)
    (s, exp[1])
end
function frexp(x::Float32)
    exp = zeros(Int32,1)
    s = ccall(dlsym(libfdm,:frexpf), Float32, (Float32, Ptr{Int32}), x, exp)
    (s, exp[1])
end
@vectorize_1arg Real frexp

^(x::Float64, y::Float64) = ccall(dlsym(libfdm, :pow),  Float64, (Float64,Float64), x, y)
^(x::Float32, y::Float32) = ccall(dlsym(libfdm, :powf), Float32, (Float32,Float32), x, y)

# alias
pow = ^

_jl_libm = dlopen("libm")

macro _jl_libmfunc_1arg_float(T,f)
    quote
        $(esc(f))(x::Float64) = ccall(dlsym(_jl_libm,$(string(f))), Float64, (Float64,), x)
        $(esc(f))(x::Float32) = ccall(dlsym(_jl_libm,$(string(f,"f"))), Float32, (Float32,), x)
        $(esc(f))(x::Real) = ($f)(float(x))
        @vectorize_1arg $T $f
    end
end

macro _jl_libfdmfunc_1arg_float(T,f)
    quote
        $(esc(f))(x::Float64) = ccall(dlsym(_jl_libfdm,$(string(f))), Float64, (Float64,), x)
        $(esc(f))(x::Float32) = ccall(dlsym(_jl_libfdm,$(string(f,"f"))), Float32, (Float32,), x)
        $(esc(f))(x::Real) = ($f)(float(x))
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
        $(esc(fname))(x::Float64) = ccall(dlsym(_jl_libm,$(string(f))), Int32, (Float64,), x)
        $(esc(fname))(x::Float32) = ccall(dlsym(_jl_libm,$(string(f,"f"))), Int32, (Float32,), x)
        @vectorize_1arg $T $fname
    end
end

macro _jl_libfdmfunc_2arg(T,f)
    quote
        $(esc(f))(x::Float64, y::Float64) = ccall(dlsym(_jl_libfdm,$(string(f))), Float64, (Float64, Float64,), x, y)
        $(esc(f))(x::Float32, y::Float32) = ccall(dlsym(_jl_libfdm,$(string(f,"f"))), Float32, (Float32, Float32), x, y)
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
@_jl_libfdmfunc_1arg_float Number asinh
@_jl_libfdmfunc_1arg_float Number acosh
@_jl_libfdmfunc_1arg_float Number atanh
@_jl_libfdmfunc_1arg_float Number log
@_jl_libfdmfunc_1arg_float Number log2
@_jl_libfdmfunc_1arg_float Number log10
@_jl_libfdmfunc_1arg_float Real   log1p
@_jl_libfdmfunc_1arg_float Real   logb
@_jl_libfdmfunc_1arg_float Number exp
@_jl_libfdmfunc_1arg_float Real   expm1
@_jl_libfdmfunc_1arg_float Number erf
@_jl_libfdmfunc_1arg_float Number erfc
@_jl_libfdmfunc_1arg_float Real   ceil
@_jl_libfdmfunc_1arg_float Real   floor
#@_jl_libfdmfunc_1arg_float Real   rint
@_jl_libfdmfunc_1arg_float Number lgamma

@_jl_libmfunc_1arg_float Number sqrt
@_jl_libmfunc_1arg_float Number exp2
#@_jl_libmfunc_1arg_float Real   nearbyint
@_jl_libmfunc_1arg_float Real   trunc
@_jl_libmfunc_1arg_float Real   round

@_jl_libfdmfunc_2arg Number atan2
atan2(x::Real, y::Real) = atan2(float64(x), float64(y))
@_jl_libfdmfunc_2arg Number hypot
hypot(x::Float32, y::Float64) = hypot(float64(x), y)
hypot(x::Float64, y::Float32) = hypot(x, float64(y))

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

#@_jl_libmfunc_1arg_int Real lrint
#@_jl_libmfunc_1arg_int Real lround iround
function ilogb(x::Float64)
    if x==0 || isnan(x)
        throw(DomainError())
    end
    int(ccall(dlsym(_jl_libm,:ilogb), Int32, (Float64,), x))
end
function ilogb(x::Float32)
    if x==0 || isnan(x)
        throw(DomainError())
    end
    int(ccall(dlsym(_jl_libm,:ilogbf), Int32, (Float32,), x))
end
@vectorize_1arg Real ilogb

@_jl_libmfunc_1arg_float Real significand

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

modf(x) = rem(x,one(x)), trunc(x)

^(x::Float64, y::Float64) = ccall(dlsym(_jl_libfdm, :pow),  Float64, (Float64,Float64), x, y)
^(x::Float32, y::Float32) = ccall(dlsym(_jl_libfdm, :powf), Float32, (Float32,Float32), x, y)

^(x::Float64, y::Integer) = x^float64(y)
^(x::Float32, y::Integer) = x^float32(y)

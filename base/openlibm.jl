libopenlibm = dlopen("libopenlibm")

macro libopenlibmfunc_1arg_float(T,f)
    quote
        $(esc(f))(x::Float64) = ccall(dlsym(libopenlibm,$(string(f))), Float64, (Float64,), x)
        $(esc(f))(x::Float32) = ccall(dlsym(libopenlibm,$(string(f,"f"))), Float32, (Float32,), x)
        $(esc(f))(x::Real) = ($f)(float(x))
        @vectorize_1arg $T $f
    end
end

macro libopenlibmfunc_1arg_float(T,f)
    quote
        $(esc(f))(x::Float64) = ccall(dlsym(libopenlibm,$(string(f))), Float64, (Float64,), x)
        $(esc(f))(x::Float32) = ccall(dlsym(libopenlibm,$(string(f,"f"))), Float32, (Float32,), x)
        $(esc(f))(x::Real) = ($f)(float(x))
        @vectorize_1arg $T $f
    end
end

macro libopenlibmfunc_1arg_int(T,f,name...)
    if length(name)>0
        fname = name[1]
    else
        fname = f
    end
    quote
        $(esc(fname))(x::Float64) = ccall(dlsym(libopenlibm,$(string(f))), Int32, (Float64,), x)
        $(esc(fname))(x::Float32) = ccall(dlsym(libopenlibm,$(string(f,"f"))), Int32, (Float32,), x)
        @vectorize_1arg $T $fname
    end
end

macro libopenlibmfunc_2arg(T,f)
    quote
        $(esc(f))(x::Float64, y::Float64) = ccall(dlsym(libopenlibm,$(string(f))), Float64, (Float64, Float64,), x, y)
        $(esc(f))(x::Float32, y::Float32) = ccall(dlsym(libopenlibm,$(string(f,"f"))), Float32, (Float32, Float32), x, y)
        @vectorize_2arg $T $f
    end
end

@libopenlibmfunc_1arg_float Number cbrt
@libopenlibmfunc_1arg_float Number sin
@libopenlibmfunc_1arg_float Number cos
@libopenlibmfunc_1arg_float Number tan
@libopenlibmfunc_1arg_float Number sinh
@libopenlibmfunc_1arg_float Number cosh
@libopenlibmfunc_1arg_float Number tanh
@libopenlibmfunc_1arg_float Number asin
@libopenlibmfunc_1arg_float Number acos
@libopenlibmfunc_1arg_float Number atan
@libopenlibmfunc_1arg_float Number asinh
@libopenlibmfunc_1arg_float Number acosh
@libopenlibmfunc_1arg_float Number atanh
@libopenlibmfunc_1arg_float Number log
@libopenlibmfunc_1arg_float Number log2
@libopenlibmfunc_1arg_float Number log10
@libopenlibmfunc_1arg_float Real   log1p
@libopenlibmfunc_1arg_float Real   logb
@libopenlibmfunc_1arg_float Number exp
@libopenlibmfunc_1arg_float Real   expm1
@libopenlibmfunc_1arg_float Number erf
@libopenlibmfunc_1arg_float Number erfc
@libopenlibmfunc_1arg_float Real   ceil
@libopenlibmfunc_1arg_float Real   floor
#@libopenlibmfunc_1arg_float Real   rint
@libopenlibmfunc_1arg_float Number lgamma

@libopenlibmfunc_1arg_float Number sqrt
@libopenlibmfunc_1arg_float Number exp2
#@libopenlibmfunc_1arg_float Real   nearbyint
@libopenlibmfunc_1arg_float Real   trunc
@libopenlibmfunc_1arg_float Real   round

@libopenlibmfunc_2arg Number atan2
atan2(x::Real, y::Real) = atan2(float64(x), float64(y))
@libopenlibmfunc_2arg Number hypot
hypot(x::Float32, y::Float64) = hypot(float64(x), y)
hypot(x::Float64, y::Float32) = hypot(x, float64(y))

gamma(x::Float64) = ccall(dlsym(libopenlibm, :tgamma),  Float64, (Float64,), x)
gamma(x::Float32) = float32(gamma(float64(x)))
gamma(x::Real) = gamma(float(x))
@vectorize_1arg Number gamma

lfact(x::Real) = (x<=1 ? zero(x) : lgamma(x+one(x)))
@vectorize_1arg Number lfact

max(x::Float64, y::Float64) = ccall(dlsym(libopenlibm, :fmax),  Float64, (Float64,Float64), x, y)
max(x::Float32, y::Float32) = ccall(dlsym(libopenlibm, :fmaxf), Float32, (Float32,Float32), x, y)
@vectorize_2arg Real max

min(x::Float64, y::Float64) = ccall(dlsym(libopenlibm, :fmin),  Float64, (Float64,Float64), x, y)
min(x::Float32, y::Float32) = ccall(dlsym(libopenlibm, :fminf), Float32, (Float32,Float32), x, y)
@vectorize_2arg Real min

#@libopenlibmfunc_1arg_int Real lrint
#@libopenlibmfunc_1arg_int Real lround iround
function ilogb(x::Float64)
    if x==0 || isnan(x)
        throw(DomainError())
    end
    int(ccall(dlsym(libopenlibm,:ilogb), Int32, (Float64,), x))
end
function ilogb(x::Float32)
    if x==0 || isnan(x)
        throw(DomainError())
    end
    int(ccall(dlsym(libopenlibm,:ilogbf), Int32, (Float32,), x))
end
@vectorize_1arg Real ilogb

@libopenlibmfunc_1arg_float Real significand

ldexp(x::Float64,e::Int) = ccall(dlsym(libopenlibm, :ldexp),  Float64, (Float64,Int32), x, int32(e))
ldexp(x::Float32,e::Int) = ccall(dlsym(libopenlibm, :ldexpf), Float32, (Float32,Int32), x, int32(e))
# TODO: vectorize does not do the right thing for these argument types
#@vectorize_2arg Real ldexp

begin
    local exp::Array{Int32,1} = zeros(Int32,1)
    global frexp
    function frexp(x::Float64)
        s = ccall(dlsym(libopenlibm,:frexp), Float64, (Float64, Ptr{Int32}), x, exp)
        (s, int(exp[1]))
    end
    function frexp(x::Float32)
        s = ccall(dlsym(libopenlibm,:frexpf), Float32, (Float32, Ptr{Int32}), x, exp)
        (s, int(exp[1]))
    end
    function frexp(A::Array{Float64})
        f = similar(A)
        e = Array(Int, size(A))
        for i = 1:numel(A)
            f[i] = ccall(dlsym(libopenlibm,:frexp), Float64, (Float64, Ptr{Int32}), A[i], exp)
            e[i] = exp[1]
        end
        return (f, e)
    end
    function frexp(A::Array{Float32})
        f = similar(A)
        e = Array(Int, size(A))
        for i = 1:numel(A)
            f[i] = ccall(dlsym(libopenlibm,:frexpf), Float32, (Float32, Ptr{Int32}), A[i], exp)
            e[i] = exp[1]
        end
        return (f, e)
    end
end

modf(x) = rem(x,one(x)), trunc(x)

^(x::Float64, y::Float64) = ccall(dlsym(libopenlibm, :pow),  Float64, (Float64,Float64), x, y)
^(x::Float32, y::Float32) = ccall(dlsym(libopenlibm, :powf), Float32, (Float32,Float32), x, y)

^(x::Float64, y::Integer) = x^float64(y)
^(x::Float32, y::Integer) = x^float32(y)

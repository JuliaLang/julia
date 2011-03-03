libm = dlopen("libm")

macro vectorize(f)
    quote
        ($f)(x::Vector) = [ ($f)(x[i]) | i=1:length(x) ]
        ($f)(x::Matrix) = [ ($f)(x[i,j]) | i=1:size(x,1), j=1:size(x,2) ]
    end
end

for f = {:tan, :sinh, :cosh, :tanh, :asin, :acos, :atan, :log, :log2,
         :log10, :log1p, :logb, :exp, :exp2, :expm1, :erf, :erfc,
         :cbrt, :ceil, :floor, :nearbyint, :round, :rint, :trunc}
    @eval begin
        ($f)(x::Float64) = ccall(dlsym(libm,$string(f)), Float64, (Float64,), x)
        ($f)(x::Float32) = ccall(dlsym(libm,$strcat(string(f),"f")), Float32, (Float32,), x)
        ($f)(x::Real) = ($f)(float(x))
        @vectorize $f
    end
end

ipart(x) = trunc(x)
fpart(x) = x - trunc(x)

for f = {:lrint, :lround, :ilogb}
    @eval begin
        ($f)(x::Float64) = ccall(dlsym(libm,$string(f)), Int32, (Float64,), x)
        ($f)(x::Float32) = ccall(dlsym(libm,$strcat(string(f),"f")), Int32, (Float32,), x)
        @vectorize $f
    end
end

abs(x::Float64) = ccall(dlsym(libm, :fabs),  Float64, (Float64,), x)
abs(x::Float32) = ccall(dlsym(libm, :fabsf), Float32, (Float32,), x)
@vectorize abs

for f = {:atan2, :pow, :fmod, :copysign, :hypot, :fmin, :fmax, :fdim}
    @eval begin
        ($f)(x::Float64, y::Float64) =
            ccall(dlsym(libm,$string(f)), Float64, (Float64, Float64,), x, y)
        ($f)(x::Float32, y::Float32) =
            ccall(dlsym(libm,$strcat(string(f),"f")), Float32, (Float32, Float32), x, y)
        ($f)(x::Real, y::Real) = ($f)(float(x),float(y))
    end
end

ldexp(x::Float64,e::Int32) = ccall(dlsym(libm, :ldexp),  Float64, (Float64,Int32), x, e)
ldexp(x::Float32,e::Int32) = ccall(dlsym(libm, :ldexpf), Float32, (Float32,Int32), x, e)

function frexp(x::Float64)
    exp = zeros(Int32,1)
    s = ccall(dlsym(libm,:frexp), Float64, (Float64, Ptr{Int32}), x, exp)
    (s, exp[1])
end
function frexp(x::Float32)
    exp = zeros(Int32,1)
    s = ccall(dlsym(libm,:frexpf), Float32, (Float32, Ptr{Int32}), x, exp)
    (s, exp[1])
end

rand()     = ccall(:rand_double,   Float64, ())
randf()    = ccall(:rand_float,    Float32, ())
randui32() = ccall(:genrand_int32, Uint32,  ())
randn()    = ccall(:randn,         Float64, ())
srand(s::Union(Int32,Uint32)) =
    ccall(:randomseed32, Void, (Uint32,), uint32(s))
srand(s::Union(Int64,Uint64)) =
    ccall(:randomseed64, Void, (Uint64,), uint64(s))

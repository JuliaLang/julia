libfdm = dlopen("libfdm")
libm = dlopen("libm")

macro vectorize(f)
    quote
        ($f)(x::Vector) = [ ($f)(x[i]) | i=1:length(x) ]
        ($f)(x::Matrix) = [ ($f)(x[i,j]) | i=1:size(x,1), j=1:size(x,2) ]
    end
end

macro libfdmfunc1(f)
    quote
        ($f)(x::Float64) = ccall(dlsym(libfdm,$string(f)), Float64, (Float64,), x)
        ($f)(x::Float32) = ccall(dlsym(libfdm,$strcat(string(f),"f")), Float32, (Float32,), x)
        ($f)(x::Real) = ($f)(float(x))
        @vectorize $f
    end
end

macro libfdmfunc2(f)
    quote
        ($f)(x::Float64, y::Float64) = ccall(dlsym(libfdm,$string(f)), Float64, (Float64, Float64,), x, y)
        ($f)(x::Float32, y::Float32) = ccall(dlsym(libfdm,$strcat(string(f),"f")), Float32, (Float32, Float32), x, y)
        ($f)(x::Real, y::Real) = ($f)(float(x),float(y))
        @vectorize $f
    end
end

macro libfdmfunc3(f)
    quote
        ($f)(x::Float64) = ccall(dlsym(libfdm,$string(f)), Int32, (Float64,), x)
        ($f)(x::Float32) = ccall(dlsym(libfdm,$strcat(string(f),"f")), Int32, (Float32,), x)
        @vectorize $f
    end
end

@vectorize sqrt
@vectorize sin
@vectorize cos

@libfdmfunc1 tan 
@libfdmfunc1 sinh 
@libfdmfunc1 cosh 
@libfdmfunc1 tanh 
@libfdmfunc1 asin 
@libfdmfunc1 acos 
@libfdmfunc1 atan 
@libfdmfunc1 log 
@libfdmfunc1 log2         
@libfdmfunc1 log10 
@libfdmfunc1 log1p 
@libfdmfunc1 logb 
@libfdmfunc1 exp 
@libfdmfunc1 exp2 
@libfdmfunc1 expm1
@libfdmfunc1 erf 
@libfdmfunc1 erfc
@libfdmfunc1 cbrt 
@libfdmfunc1 ceil 
@libfdmfunc1 floor 
@libfdmfunc1 nearbyint 
@libfdmfunc1 round 
@libfdmfunc1 rint 
@libfdmfunc1 trunc

@libfdmfunc2 atan2
@libfdmfunc2 pow
@libfdmfunc2 fmod
@libfdmfunc2 copysign
@libfdmfunc2 hypot

@libfdmfunc3 lrint
@libfdmfunc3 lround
@libfdmfunc3 ilogb

ipart(x) = trunc(x)
fpart(x) = x - trunc(x)
@vectorize ipart
@vectorize fpart

abs(x::Float64) = ccall(dlsym(libfdm, :fabs),  Float64, (Float64,), x)
abs(x::Float32) = ccall(dlsym(libfdm, :fabsf), Float32, (Float32,), x)
@vectorize abs

ldexp(x::Float64,e::Int32) = ccall(dlsym(libfdm, :ldexp),  Float64, (Float64,Int32), x, e)
ldexp(x::Float32,e::Int32) = ccall(dlsym(libfdm, :ldexpf), Float32, (Float32,Int32), x, e)
@vectorize ldexp

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

@vectorize frexp

rand()     = ccall(:rand_double,   Float64, ())
randf()    = ccall(:rand_float,    Float32, ())
randui32() = ccall(:genrand_int32, Uint32,  ())
randn()    = ccall(:randn,         Float64, ())
srand(s::Union(Int32,Uint32)) = ccall(:randomseed32, Void, (Uint32,), uint32(s))
srand(s::Union(Int64,Uint64)) = ccall(:randomseed64, Void, (Uint64,), uint64(s))

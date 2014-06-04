for jy in ("j","y"), nu in (0,1)
    jynu = Expr(:quote, symbol(string(jy,nu)))
    jynuf = Expr(:quote, symbol(string(jy,nu,"f")))
    bjynu = symbol(string("bessel",jy,nu))
    if jy == "y"
        @eval begin
            $bjynu(x::Float64) = nan_dom_err(ccall(($jynu,libm),  Float64, (Float64,), x), x)
            $bjynu(x::Float32) = nan_dom_err(ccall(($jynuf,libm), Float32, (Float32,), x), x)
        end
    else
        @eval begin
            $bjynu(x::Float64) = ccall(($jynu,libm),  Float64, (Float64,), x)
            $bjynu(x::Float32) = ccall(($jynuf,libm), Float32, (Float32,), x)
        end
    end
    @eval begin
        $bjynu(x::Real) = $bjynu(float(x))
        $bjynu(x::Complex) = $(symbol(string("bessel",jy)))($nu,x)
        @vectorize_1arg Number $bjynu
    end
end
        
type AmosException <: Exception
    info::Int32
end

let
    const ai::Array{Float64,1} = Array(Float64,2)
    const ae::Array{Int32,1} = Array(Int32,2)
global airy
function airy(k::Int, z::Complex128)
    id = int32(k==1 || k==3)
    if k == 0 || k == 1
        ccall((:zairy_,openspecfun), Void,
              (Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32},
               Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32}),
              &real(z), &imag(z),
              &id, &1,
              pointer(ai,1), pointer(ai,2),
              pointer(ae,1), pointer(ae,2))
        if ae[2] == 0 || ae[2] == 3 
            return complex(ai[1],ai[2])
        else
            throw(AmosException(ae[2]))
        end
    elseif k == 2 || k == 3
        ccall((:zbiry_,openspecfun), Void,
              (Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32},
               Ptr{Float64}, Ptr{Float64}, Ptr{Int32}),
              &real(z), &imag(z),
              &id, &1,
              pointer(ai,1), pointer(ai,2),
              pointer(ae,1))
        if ae[1] == 0 || ae[1] == 3  # ignore underflow
            return complex(ai[1],ai[2]) 
        else
            throw(AmosException(ae[2]))
        end
    else
        error("invalid argument")
    end
end
end

airy(z) = airy(0,z)
@vectorize_1arg Number airy
airyprime(z) = airy(1,z)
@vectorize_1arg Number airyprime
airyai(z) = airy(0,z)
@vectorize_1arg Number airyai
airyaiprime(z) = airy(1,z)
@vectorize_1arg Number airyaiprime
airybi(z) = airy(2,z)
@vectorize_1arg Number airybi
airybiprime(z) = airy(3,z)
@vectorize_1arg Number airybiprime

airy(k::Number, x::FloatingPoint) = oftype(x, real(airy(k, complex(x))))
airy(k::Number, x::Real) = airy(k, float(x))
airy(k::Number, z::Complex64) = complex64(airy(k, complex128(z)))
airy(k::Number, z::Complex) = airy(convert(Int,k), complex128(z))
@vectorize_2arg Number airy

const cy = Array(Float64,2)
const ae = Array(Int32,2)
const wrk = Array(Float64,2)

function _besselh(nu::Float64, k::Integer, z::Complex128)
    ccall((:zbesh_,openspecfun), Void,
          (Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32},
           Ptr{Int32}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32}),
          &real(z), &imag(z), &nu, &1, &k, &1,
          pointer(cy,1), pointer(cy,2),
          pointer(ae,1), pointer(ae,2))
    if ae[2] == 0 || ae[2] == 3 
        return complex(cy[1],cy[2]) 
    else
        throw(AmosException(ae[2]))
    end
end

function _besseli(nu::Float64, z::Complex128)
    ccall((:zbesi_,openspecfun), Void,
          (Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32},
           Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32}),
          &real(z), &imag(z), &nu, &1, &1,
          pointer(cy,1), pointer(cy,2),
          pointer(ae,1), pointer(ae,2))
    if ae[2] == 0 || ae[2] == 3 
        return complex(cy[1],cy[2]) 
    else
        throw(AmosException(ae[2]))
    end
end

function _besselj(nu::Float64, z::Complex128)
    ccall((:zbesj_,openspecfun), Void,
          (Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32},
           Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32}),
          &real(z), &imag(z), &nu, &1, &1,
          pointer(cy,1), pointer(cy,2),
          pointer(ae,1), pointer(ae,2))
    if ae[2] == 0 || ae[2] == 3 
        return complex(cy[1],cy[2]) 
    else
        throw(AmosException(ae[2]))
    end
end

function _besselk(nu::Float64, z::Complex128)
    ccall((:zbesk_,openspecfun), Void,
          (Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32},
           Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32}),
          &real(z), &imag(z), &nu, &1, &1,
          pointer(cy,1), pointer(cy,2),
          pointer(ae,1), pointer(ae,2))
    if ae[2] == 0 || ae[2] == 3 
        return complex(cy[1],cy[2]) 
    else
        throw(AmosException(ae[2]))
    end
end

function _bessely(nu::Float64, z::Complex128)
    ccall((:zbesy_,openspecfun), Void,
          (Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32},
           Ptr{Int32}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32},
           Ptr{Float64}, Ptr{Float64}, Ptr{Int32}),
          &real(z), &imag(z), &nu, &1, &1,
          pointer(cy,1), pointer(cy,2),
          pointer(ae,1), pointer(wrk,1),
          pointer(wrk,2), pointer(ae,2))
    if ae[2] == 0 || ae[2] == 3 
        return complex(cy[1],cy[2]) 
    else
        throw(AmosException(ae[2]))
    end
end

function besselh(nu::Float64, k::Integer, z::Complex128)
    if nu < 0
        s = (k == 1) ? 1 : -1
        return _besselh(-nu, k, z) * complex(cospi(nu),-s*sinpi(nu))
    end
    return _besselh(nu, k, z)
end

function besseli(nu::Float64, z::Complex128)
    if nu < 0
        return _besseli(-nu,z) - 2_besselk(-nu,z)*sinpi(nu)/pi
    else
        return _besseli(nu, z)
    end
end

function besselj(nu::Float64, z::Complex128)
    if nu < 0
        return _besselj(-nu,z)cos(pi*nu) + _bessely(-nu,z)*sinpi(nu)
    else
        return _besselj(nu, z)
    end
end

besselj(nu::Integer, x::FloatingPoint) = typemin(Int32) <= nu <= typemax(Int32) ?
    oftype(x, ccall((:jn, libm), Float64, (Cint, Float64), nu, x)) :
    besselj(float64(nu), x)

besselj(nu::Integer, x::Float32) = typemin(Int32) <= nu <= typemax(Int32) ?
    ccall((:jnf, libm), Float32, (Cint, Float32), nu, x) :
    besselj(float64(nu), x)

besselk(nu::Float64, z::Complex128) = _besselk(abs(nu), z)

function bessely(nu::Float64, z::Complex128)
    if nu < 0
        return _bessely(-nu,z)*cospi(nu) - _besselj(-nu,z)*sinpi(nu)
    else
        return _bessely(nu, z)
    end
end

besselh(nu, z) = besselh(nu, 1, z)
besselh(nu::Real, k::Integer, z::Complex64) = complex64(besselh(float64(nu), k, complex128(z)))
besselh(nu::Real, k::Integer, z::Complex) = besselh(float64(nu), k, complex128(z))
besselh(nu::Real, k::Integer, x::Real) = besselh(float64(nu), k, complex128(x))
@vectorize_2arg Number besselh

besseli(nu::Real, z::Complex64) = complex64(besseli(float64(nu), complex128(z)))
besseli(nu::Real, z::Complex) = besseli(float64(nu), complex128(z))
besseli(nu::Real, x::Integer) = besseli(nu, float64(x))
function besseli(nu::Real, x::FloatingPoint)
    if x < 0 && !isinteger(nu)
        throw(DomainError())
    end
    oftype(x, real(besseli(float64(nu), complex128(x))))
end
@vectorize_2arg Number besseli

function besselj(nu::FloatingPoint, x::FloatingPoint)
    if isinteger(nu)
        if typemin(Int32) <= nu <= typemax(Int32)
            return besselj(int(nu), x)
        end
    elseif x < 0
        throw(DomainError())
    end
    oftype(x, real(besselj(float64(nu), complex128(x))))
end

besselj(nu::Real, z::Complex64) = complex64(besselj(float64(nu), complex128(z)))
besselj(nu::Real, z::Complex) = besselj(float64(nu), complex128(z))
besselj(nu::Real, x::Integer) = besselj(nu, float(x))
@vectorize_2arg Number besselj

besselk(nu::Real, z::Complex64) = complex64(besselk(float64(nu), complex128(z)))
besselk(nu::Real, z::Complex) = besselk(float64(nu), complex128(z))
besselk(nu::Real, x::Integer) = besselk(nu, float64(x))
function besselk(nu::Real, x::FloatingPoint)
    if x < 0
        throw(DomainError())
    end
    if x == 0
        return oftype(x, Inf)
    end
    oftype(x, real(besselk(float64(nu), complex128(x))))
end
@vectorize_2arg Number besselk

bessely(nu::Real, z::Complex64) = complex64(bessely(float64(nu), complex128(z)))
bessely(nu::Real, z::Complex) = bessely(float64(nu), complex128(z))
bessely(nu::Real, x::Integer) = bessely(nu, float64(x))
function bessely(nu::Real, x::FloatingPoint)
    if x < 0
        throw(DomainError())
    end
    if isinteger(nu) && typemin(Int32) <= nu <= typemax(Int32)
        return bessely(int(nu), x)
    end
    oftype(x, real(bessely(float64(nu), complex128(x))))
end
function bessely(nu::Integer, x::FloatingPoint)
    if x < 0
        throw(DomainError())
    end
    return oftype(x, ccall((:yn, libm), Float64, (Cint, Float64), nu, x))
end
function bessely(nu::Integer, x::Float32)
    if x < 0
        throw(DomainError())
    end
    return ccall((:ynf, libm), Float32, (Cint, Float32), nu, x)
end
@vectorize_2arg Number bessely

hankelh1(nu, z) = besselh(nu, 1, z)
@vectorize_2arg Number hankelh1

hankelh2(nu, z) = besselh(nu, 2, z)
@vectorize_2arg Number hankelh2


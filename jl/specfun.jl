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
              &real(z), &imag(z),
              &id, &1,
              pointer(ai,1), pointer(ai,2),
              pointer(ae,1), pointer(ae,2))
        return complex(ai[1],ai[2])
    elseif k == 2 || k == 3
        ccall(dlsym(_jl_libamos, :zbiry_), Void,
              (Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32},
               Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32}),
              &real(z), &imag(z),
              &id, &1,
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

    function _besselh(nu::Float64, k::Integer, z::Complex128)
        ccall(dlsym(_jl_libamos, :zbesh_), Void,
              (Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32},
               Ptr{Int32}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32}),
              &real(z), &imag(z), &nu, &1, &k, &1,
              pointer(cy,1), pointer(cy,2),
              pointer(ae,1), pointer(ae,2))
        return complex(cy[1],cy[2])
    end

    function _besseli(nu::Float64, z::Complex128)
        ccall(dlsym(_jl_libamos, :zbesi_), Void,
              (Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32},
               Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32}),
              &real(z), &imag(z), &nu, &1, &1,
              pointer(cy,1), pointer(cy,2),
              pointer(ae,1), pointer(ae,2))
        return complex(cy[1],cy[2])
    end

    function _besselj(nu::Float64, z::Complex128)
        ccall(dlsym(_jl_libamos, :zbesj_), Void,
              (Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32},
               Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32}),
              &real(z), &imag(z), &nu, &1, &1,
              pointer(cy,1), pointer(cy,2),
              pointer(ae,1), pointer(ae,2))
        return complex(cy[1],cy[2])
    end

    function _besselk(nu::Float64, z::Complex128)
        ccall(dlsym(_jl_libamos, :zbesk_), Void,
              (Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32},
               Ptr{Float64}, Ptr{Float64}, Ptr{Int32}, Ptr{Int32}),
              &real(z), &imag(z), &nu, &1, &1,
              pointer(cy,1), pointer(cy,2),
              pointer(ae,1), pointer(ae,2))
        return complex(cy[1],cy[2])
    end

    function _bessely(nu::Float64, z::Complex128)
        ccall(dlsym(_jl_libamos, :zbesy_), Void,
              (Ptr{Float64}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32},
               Ptr{Int32}, Ptr{Float64}, Ptr{Float64}, Ptr{Int32},
               Ptr{Float64}, Ptr{Float64}, Ptr{Int32}),
              &real(z), &imag(z), &nu, &1, &1,
              pointer(cy,1), pointer(cy,2),
              pointer(ae,1), pointer(wrk,1),
              pointer(wrk,2), pointer(ae,2))
        return complex(cy[1],cy[2])
    end

    global besselh
    function besselh(nu::Float64, k::Integer, z::Complex128)
        if nu < 0
            s = (k == 1) ? 1 : -1
            return _besselh(-nu, k, z) * exp(-s*nu*im*pi)
        end
        return _besselh(nu, k, z)
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

    function besselj(nu::Integer, x::Float)
        if x == 0
            return (nu == 0) ? one(x) : zero(x)
        end
        if nu < 0
            nu = -nu
            x = -x
        end
        ans = _besselj(float64(nu), complex128(abs(x)))
        if (x < 0) && isodd(nu)
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

besselh(nu, z) = besselh(nu, 1, z)
besselh(nu::Real, k::Integer, z::Complex64) = complex64(besselh(float64(nu), k, complex128(z)))
besselh(nu::Real, k::Integer, z::Complex) = besselh(float64(nu), k, complex128(z))
besselh(nu::Real, k::Integer, x::Real) = besselh(float64(nu), k, complex128(x))

besseli(nu::Real, z::Complex64) = complex64(bessely(float64(nu), complex128(z)))
besseli(nu::Real, z::Complex) = besseli(float64(nu), complex128(z))
besseli(nu::Real, x::Real) = besseli(float64(nu), complex128(x))

function besselj(nu::Float, x::Float)
    ans = besselj(float64(nu), complex128(x))
    (x > 0) ? oftype(x, real(ans)) : ans
end

besselj(nu::Real, z::Complex64) = complex64(besselj(float64(nu), complex128(z)))
besselj(nu::Real, z::Complex) = besselj(float64(nu), complex128(z))
besselj(nu::Integer, x::Real) = besselj(nu, float(x))
besselj(nu::Real, x::Real) = besselj(float(nu), float(x))

besselk(nu::Real, z::Complex64) = complex64(besselk(float64(nu), complex128(z)))
besselk(nu::Real, z::Complex) = besselk(float64(nu), complex128(z))
besselk(nu::Real, x::Real) = besselk(float64(nu), complex128(x))

bessely(nu::Real, z::Complex64) = complex64(bessely(float64(nu), complex128(z)))
bessely(nu::Real, z::Complex) = bessely(float64(nu), complex128(z))
bessely(nu::Real, x::Real) = bessely(float64(nu), complex128(x))

hankelh1(nu, z) = besselh(nu, 1, z)
hankelh2(nu, z) = besselh(nu, 2, z)

_jl_libamos = dlopen("libamos")

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
airyprime(z) = airy(1,z)
airyai(z) = airy(0,z)
airyaiprime(z) = airy(1,z)
airybi(z) = airy(2,z)
airybiprime(z) = airy(3,z)

airy(k, x::FloatingPoint) = oftype(x, real(airy(k, complex(x))))
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

    function besselj(nu::Integer, x::FloatingPoint)
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

function besselj(nu::FloatingPoint, x::FloatingPoint)
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

function angle_restrict_symm(theta)
    P1 = 4 * 7.8539812564849853515625e-01
    P2 = 4 * 3.7748947079307981766760e-08
    P3 = 4 * 2.6951514290790594840552e-15

    y = 2*floor(theta/(2*pi))
    r = ((theta - y*P1) - y*P2) - y*P3
    if (r > pi)
        r -= (2*pi)
    end
    return r
end

const clg_coeff = [76.18009172947146,
                   -86.50532032941677,
                   24.01409824083091,
                   -1.231739572450155,
                   0.1208650973866179e-2,
                   -0.5395239384953e-5]

function clgamma_lanczos(z)
    sqrt2pi = 2.5066282746310005
    
    y = x = z
    temp = x + 5.5
    zz = log(temp)
    zz = zz * (x+0.5)
    temp -= zz
    ser = complex(1.000000000190015, 0)
    for j=1:6
        y += 1.0
        zz = clg_coeff[j]/y
        ser += zz
    end
    zz = sqrt2pi*ser / x
    return log(zz) - temp
end

function lgamma(z::Complex)
    if real(z) <= 0.5
        a = clgamma_lanczos(1-z)
        b = log(sin(pi * z))
        logpi = 1.14472988584940017
        z = logpi - b - a
    else
        z = clgamma_lanczos(z)
    end
    complex(real(z), angle_restrict_symm(imag(z)))
end

gamma(z::Complex) = exp(lgamma(z))

beta(x::Number, w::Number) = exp(lgamma(x)+lgamma(w)-lgamma(x+w))
lbeta(x::Number, w::Number) = lgamma(x)+lgamma(w)-lgamma(x+w)

const eta_coeffs =
    [.99999999999999999997,
     -.99999999999999999821,
     .99999999999999994183,
     -.99999999999999875788,
     .99999999999998040668,
     -.99999999999975652196,
     .99999999999751767484,
     -.99999999997864739190,
     .99999999984183784058,
     -.99999999897537734890,
     .99999999412319859549,
     -.99999996986230482845,
     .99999986068828287678,
     -.99999941559419338151,
     .99999776238757525623,
     -.99999214148507363026,
     .99997457616475604912,
     -.99992394671207596228,
     .99978893483826239739,
     -.99945495809777621055,
     .99868681159465798081,
     -.99704078337369034566,
     .99374872693175507536,
     -.98759401271422391785,
     .97682326283354439220,
     -.95915923302922997013,
     .93198380256105393618,
     -.89273040299591077603,
     .83945793215750220154,
     -.77148960729470505477,
     .68992761745934847866,
     -.59784149990330073143,
     .50000000000000000000,
     -.40215850009669926857,
     .31007238254065152134,
     -.22851039270529494523,
     .16054206784249779846,
     -.10726959700408922397,
     .68016197438946063823e-1,
     -.40840766970770029873e-1,
     .23176737166455607805e-1,
     -.12405987285776082154e-1,
     .62512730682449246388e-2,
     -.29592166263096543401e-2,
     .13131884053420191908e-2,
     -.54504190222378945440e-3,
     .21106516173760261250e-3,
     -.76053287924037718971e-4,
     .25423835243950883896e-4,
     -.78585149263697370338e-5,
     .22376124247437700378e-5,
     -.58440580661848562719e-6,
     .13931171712321674741e-6,
     -.30137695171547022183e-7,
     .58768014045093054654e-8,
     -.10246226511017621219e-8,
     .15816215942184366772e-9,
     -.21352608103961806529e-10,
     .24823251635643084345e-11,
     -.24347803504257137241e-12,
     .19593322190397666205e-13,
     -.12421162189080181548e-14,
     .58167446553847312884e-16,
     -.17889335846010823161e-17,
     .27105054312137610850e-19]

function eta(z::Union(Float64,Complex128))
    if z == 0
        return oftype(z, 0.5)
    end
    re, im = reim(z)
    if im==0 && re < 0 && re==round(re/2)*2
        return zero(z)
    end
    reflect = false
    if re < 0.5
        z = 1-z
        reflect = true
    end
    s = zero(z)
    for n = length(eta_coeffs):-1:1
        c = eta_coeffs[n]
        p = n^-z
        s += c * p
    end
    if reflect
        z2 = 2.0^z
        b = 2.0 - (2.0*z2)
        f = z2 - 2
        piz = pi^z
        
        b = b/f/piz
        
        return s * gamma(z) * b * cos(pi/2*z)
    end
    return s
end

eta(x::Real)    = eta(float64(x))
eta(z::Complex) = eta(complex128(z))

function zeta(z::Number)
    zz = 2.0^z
    eta(z) * zz/(zz-2)
end

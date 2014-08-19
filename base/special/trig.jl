immutable Double64
    hi::Float64
    lo::Float64
end
immutable Double32
    hi::Float64
end

# kernel functions are only valid for |x| < pi/4 = 0.7854
sin_kernel(x::Double64) = ccall((:__kernel_sin,Base.Math.libm),Float64,(Float64,Float64,Cint),x.hi,x.lo,1)
cos_kernel(x::Double64) = ccall((:__kernel_cos,Base.Math.libm),Float64,(Float64,Float64),x.hi,x.lo)
sin_kernel(x::Float64) = ccall((:__kernel_sin,Base.Math.libm),Float64,(Float64,Float64,Cint),x,0.0,0)
cos_kernel(x::Float64) = ccall((:__kernel_cos,Base.Math.libm),Float64,(Float64,Float64),x,0.0)

sin_kernel(x::Double32) = ccall((:__kernel_sindf,Base.Math.libm),Float32,(Float64,),x.hi)
cos_kernel(x::Double32) = ccall((:__kernel_cosdf,Base.Math.libm),Float32,(Float64,),x.hi)

sin_kernel(x::Real) = sin(x)
cos_kernel(x::Real) = cos(x)

# multiply in extended precision
function mulpi_ext(x::Float64)
    m = 3.141592653589793
    m_hi = 3.1415926218032837
    m_lo = 3.178650954705639e-8

    u = 134217729.0*x # 0x1p27 + 1
    x_hi = u-(u-x)
    x_lo = x-x_hi
    
    y_hi = m*x
    y_lo = x_hi * m_lo + (x_lo* m_hi + ((x_hi*m_hi-y_hi) + x_lo*m_lo))

    Double64(y_hi,y_lo)
end
mulpi_ext(x::Float32) = Double32(pi*float64(x))
mulpi_ext(x::Real) = pi*x # Fallback

function sinpi(x::Real)
    if isinf(x)
        return throw(DomainError())
    elseif isnan(x)
        return nan(x)
    end

    rx = copysign(float(rem(x,2)),x)
    arx = abs(rx)

    if rx == zero(rx)
        return rx 
    elseif arx < oftype(rx,0.25)
        return sin_kernel(mulpi_ext(rx))
    elseif arx <= oftype(rx,0.75)
        y = mulpi_ext(oftype(rx,0.5) - arx)
        return copysign(cos_kernel(y),rx)
    elseif arx == one(x)
        return copysign(zero(rx),rx)
    elseif arx < oftype(rx,1.25)
        y = mulpi_ext((one(rx) - arx)*sign(rx))
        return sin_kernel(y)
    elseif arx <= oftype(rx,1.75)
        y = mulpi_ext(oftype(rx,1.5) - arx)
        return -copysign(cos_kernel(y),rx)
    else
        y = mulpi_ext(rx - copysign(oftype(rx,2.0),rx))
        return sin_kernel(y)
    end
end

function cospi(x::Real)
    if isinf(x)
        return throw(DomainError())
    elseif isnan(x)
        return nan(x)
    end

    rx = abs(float(rem(x,2)))

    if rx <= oftype(rx,0.25)
        return cos_kernel(mulpi_ext(rx))
    elseif rx < oftype(rx,0.75)
        y = mulpi_ext(oftype(rx,0.5) - rx)
        return sin_kernel(y)
    elseif rx <= oftype(rx,1.25)
        y = mulpi_ext(one(rx) - rx)
        return -cos_kernel(y)
    elseif rx < oftype(rx,1.75)
        y = mulpi_ext(rx - oftype(rx,1.5))
        return sin_kernel(y)
    else
        y = mulpi_ext(oftype(rx,2.0) - rx)
        return cos_kernel(y)
    end
end


sinpi(x::Integer) = zero(x)
cospi(x::Integer) = isodd(x) ? -one(x) : one(x)

function sinpi(z::Complex)
    zr, zi = reim(z)
    if !isfinite(zi) && zr == 0 return complex(zr, zi) end
    if isnan(zr) && !isfinite(zi) return complex(zr, zi) end
    if !isfinite(zr) && zi == 0 return complex(oftype(zr, NaN), zi) end
    if !isfinite(zr) && isfinite(zi) return complex(oftype(zr, NaN), oftype(zi, NaN)) end
    if !isfinite(zr) && !isfinite(zi) return complex(zr, oftype(zi, NaN)) end
    pizi = pi*zi
    complex(sinpi(zr)*cosh(pizi), cospi(zr)*sinh(pizi))
end

function cospi(z::Complex)
    zr, zi = reim(z)
    if !isfinite(zi) && zr == 0
        return complex(isnan(zi) ? zi : oftype(zi, Inf),
                       isnan(zi) ? zr : zr*-sign(zi))
    end
    if !isfinite(zr) && isinf(zi)
        return complex(oftype(zr, Inf), oftype(zi, NaN))
    end
    if isinf(zr)
        return complex(oftype(zr, NaN), zi==0 ? -copysign(zi, zr) : oftype(zi, NaN))
    end
    if isnan(zr) && zi==0 return complex(zr, abs(zi)) end
    pizi = pi*zi
    complex(cospi(zr)*cosh(pizi), -sinpi(zr)*sinh(pizi))
end
@vectorize_1arg Number sinpi
@vectorize_1arg Number cospi


sinc(x::Number) = x==0 ? one(x)  : oftype(x,sinpi(x)/(pi*x))
sinc(x::Integer) = x==0 ? one(x) : zero(x)
sinc{T<:Integer}(x::Complex{T}) = sinc(float(x))
@vectorize_1arg Number sinc
cosc(x::Number) = x==0 ? zero(x) : oftype(x,(cospi(x)-sinpi(x)/(pi*x))/x)
cosc(x::Integer) = cosc(float(x))
cosc{T<:Integer}(x::Complex{T}) = cosc(float(x))
@vectorize_1arg Number cosc

for (finv, f) in ((:sec, :cos), (:csc, :sin), (:cot, :tan),
                  (:sech, :cosh), (:csch, :sinh), (:coth, :tanh),
                  (:secd, :cosd), (:cscd, :sind), (:cotd, :tand))
    @eval begin
        ($finv){T<:Number}(z::T) = one(T) / (($f)(z))
        ($finv){T<:Number}(z::AbstractArray{T}) = one(T) ./ (($f)(z))
    end
end

for (fa, fainv) in ((:asec, :acos), (:acsc, :asin), (:acot, :atan),
                    (:asech, :acosh), (:acsch, :asinh), (:acoth, :atanh))
    @eval begin
        ($fa){T<:Number}(y::T) = ($fainv)(one(T) / y)
        ($fa){T<:Number}(y::AbstractArray{T}) = ($fainv)(one(T) ./ y)
    end
end


# multiply in extended precision
function deg2rad_ext(x::Float64)
    m = 0.017453292519943295
    m_hi = 0.01745329238474369
    m_lo = 1.3519960527851425e-10

    u = 134217729.0*x # 0x1p27 + 1
    x_hi = u-(u-x)
    x_lo = x-x_hi
    
    y_hi = m*x
    y_lo = x_hi * m_lo + (x_lo* m_hi + ((x_hi*m_hi-y_hi) + x_lo*m_lo))

    Double64(y_hi,y_lo)
end
deg2rad_ext(x::Float32) = Double32(deg2rad(float64(x)))
deg2rad_ext(x::Real) = deg2rad(x) # Fallback

function sind(x::Real)
    if isinf(x)
        return throw(DomainError())
    elseif isnan(x)
        return nan(x)
    end

    rx = copysign(float(rem(x,360)),x)
    arx = abs(rx)

    if rx == zero(rx)
        return rx 
    elseif arx < oftype(rx,45)
        return sin_kernel(deg2rad_ext(rx))
    elseif arx <= oftype(rx,135)
        y = deg2rad_ext(oftype(rx,90) - arx)
        return copysign(cos_kernel(y),rx)
    elseif arx == oftype(rx,180)
        return copysign(zero(rx),rx)
    elseif arx < oftype(rx,225)
        y = deg2rad_ext((oftype(rx,180) - arx)*sign(rx))
        return sin_kernel(y)
    elseif arx <= oftype(rx,315)
        y = deg2rad_ext(oftype(rx,270) - arx)
        return -copysign(cos_kernel(y),rx)
    else
        y = deg2rad_ext(rx - copysign(oftype(rx,360),rx))
        return sin_kernel(y)
    end
end
@vectorize_1arg Real sind

function cosd(x::Real)
    if isinf(x)
        return throw(DomainError())
    elseif isnan(x)
        return nan(x)
    end

    rx = abs(float(rem(x,360)))

    if rx <= oftype(rx,45)
        return cos_kernel(deg2rad_ext(rx))
    elseif rx < oftype(rx,135)
        y = deg2rad_ext(oftype(rx,90) - rx)
        return sin_kernel(y)
    elseif rx <= oftype(rx,225)
        y = deg2rad_ext(oftype(rx,180) - rx)
        return -cos_kernel(y)
    elseif rx < oftype(rx,315)
        y = deg2rad_ext(rx - oftype(rx,270))
        return sin_kernel(y)
    else
        y = deg2rad_ext(oftype(rx,360) - rx)
        return cos_kernel(y)
    end
end
@vectorize_1arg Real cosd

tand(x::Real) = sind(x) / cosd(x)
@vectorize_1arg Real tand

for (fd, f) in ((:sind, :sin), (:cosd, :cos), (:tand, :tan))
    @eval begin
        ($fd)(z) = ($f)(deg2rad(z))
    end
end

for (fd, f) in ((:asind, :asin), (:acosd, :acos), (:atand, :atan),
                (:asecd, :asec), (:acscd, :acsc), (:acotd, :acot))
    @eval begin
        ($fd)(y) = rad2deg(($f)(y))
        @vectorize_1arg Real $fd
    end
end

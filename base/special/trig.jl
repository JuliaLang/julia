function sinpi(x::Real)
    if isinf(x)
        return throw(DomainError())
    elseif isnan(x)
        return nan(x)
    end

    rx = copysign(float(rem(x,2)),x)
    arx = abs(rx)

    if arx < oftype(rx,0.25)
        return sin(pi*rx)
    elseif arx <= oftype(rx,0.75)
        arx = oftype(rx,0.5) - arx
        return copysign(cos(pi*arx),rx)
    elseif arx < oftype(rx,1.25)
        rx = (one(rx) - arx)*sign(rx)
        return sin(pi*rx)
    elseif arx <= oftype(rx,1.75)
        arx = oftype(rx,1.5) - arx
        return -copysign(cos(pi*arx),rx)
    else
        rx = rx - copysign(oftype(rx,2.0),rx)
        return sin(pi*rx)
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
        return cos(pi*rx)
    elseif rx < oftype(rx,0.75)
        rx = oftype(rx,0.5) - rx
        return sin(pi*rx)
    elseif rx <= oftype(rx,1.25)
        rx = one(rx) - rx
        return -cos(pi*rx)
    elseif rx < oftype(rx,1.75)
        rx = rx - oftype(rx,1.5)
        return sin(pi*rx)
    else
        rx = oftype(rx,2.0) - rx
        return cos(pi*rx)
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

function sind(x::Real)
    if isinf(x)
        return throw(DomainError())
    elseif isnan(x)
        return nan(x)
    end

    rx = copysign(float(rem(x,360)),x)
    arx = abs(rx)

    if arx < oftype(rx,45.0)
        return sin(deg2rad(rx))
    elseif arx <= oftype(rx,135.0)
        arx = oftype(rx,90.0) - arx
        return copysign(cos(deg2rad(arx)),rx)
    elseif arx < oftype(rx,225.0)
        rx = (oftype(rx,180.0) - arx)*sign(rx)
        return sin(deg2rad(rx))
    elseif arx <= 315.0
        arx = oftype(rx,270.0) - arx
        return -copysign(cos(deg2rad(arx)),rx)
    else
        rx = rx - copysign(oftype(rx,360.0),rx)
        return sin(deg2rad(rx))
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

    if rx <= oftype(rx,45.0)
        return cos(deg2rad(rx))
    elseif rx < oftype(rx,135.0)
        rx = oftype(rx,90.0) - rx
        return sin(deg2rad(rx))
    elseif rx <= oftype(rx,225.0)
        rx = oftype(rx,180.0) - rx
        return -cos(deg2rad(rx))
    elseif rx < oftype(rx,315.0)
        rx = rx - oftype(rx,270.0)
        return sin(deg2rad(rx))
    else
        rx = oftype(rx,360.0) - rx
        return cos(deg2rad(rx))
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

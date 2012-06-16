## non-type-specific math functions ##

clamp(x::Real, lo::Real, hi::Real) = (x > hi ? hi : (x < lo ? lo : x))

sec(z) = 1 ./cos(z)
csc(z) = 1 ./sin(z)
cot(z) = 1 ./tan(z)
asec(y) = acos(1 ./y)
acsc(y) = asin(1 ./y)
acot(y) = atan(1 ./y)
sech(z) = 1 ./cosh(z)
csch(z) = 1 ./sinh(z)
coth(z) = 1 ./tanh(z)
asech(y) = acosh(1 ./y)
acsch(y) = asinh(1 ./y)
acoth(y) = atanh(1 ./y)

sinc(x::Number) = x==0 ? one(x)  : (pix=pi*x; oftype(x,sin(pix)/pix))
cosc(x::Number) = x==0 ? zero(x) : (pix=pi*x; oftype(x,cos(pix)/x-sin(pix)/(pix*x)))

radians2degrees(z::Real) = oftype(z, (180/pi) * z)
degrees2radians(z::Real) = oftype(z, (pi/180) * z)

cosd(z) = cos(degrees2radians(z))
cotd(z) = cot(degrees2radians(z))
cscd(z) = csc(degrees2radians(z))
secd(z) = sec(degrees2radians(z))
sind(z) = sin(degrees2radians(z))
tand(z) = tan(degrees2radians(z))

acosd(y) = radians2degrees(acos(y))
acotd(y) = radians2degrees(acot(y))
acscd(y) = radians2degrees(acsc(y))
asecd(y) = radians2degrees(asec(y))
asind(y) = radians2degrees(asin(y))
atand(y) = radians2degrees(atan(y))

log(b,x) = log(x)/log(b)

function hypot(x::Real, y::Real)
    x = abs(x)
    y = abs(y)
    if x > y
        r = y/x
        return x*sqrt(one(r)+r*r)
    end
    if y == 0
        return sqrt(y)  # to give same type as other cases
    end
    r = x/y
    return y*sqrt(one(r)+r*r)
end

square(x::Number) = x*x

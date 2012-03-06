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

log(b,x) = log(x)/log(b)

function hypot(x::Real, y::Real)
    x = abs(x)
    y = abs(y)
    if x > y
        r = y/x
        return x*sqrt(1+r*r)
    end
    if y == 0
        return sqrt(y)  # to give same type as other cases
    end
    r = x/y
    return y*sqrt(1+r*r)
end

square(x::Number) = x*x

function polyval(a::AbstractVector, x::Number)
    y = a[1]
    for i = 2:length(a)
        y = a[i] + x.*y
    end
    return y
end

function polyval(a::AbstractVector, x::AbstractVector)
    y = zeros(size(x))
    for i = 1:length(x)
        y[i] = polyval(a, x[i])
    end
    return y
end

function polyint(a::AbstractVector, k::Number)
    vcat(a, k)./flipud(1:(length(a)+1))
end
polyint(a::AbstractVector) = polyint(a, 0)

function poly(r::AbstractVector)
    n = length(r)
    c = zeros(n+1,1)
    c[1] = 1
    for j = 1:n
        c[2:j+1] = c[2:j+1]-r[j]*c[1:j]
    end
    return c
end
poly(A::Matrix) = poly(eig(A)[1])

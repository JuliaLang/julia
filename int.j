Int8.convert(x::Uint8) = boxsi8(unbox8(x))
Int8.convert(x::Union(Int16,Uint16)) = boxsi8(trunc8(unbox16(x)))
Int8.convert(x::Union(Int32,Uint32)) = boxsi8(trunc8(unbox32(x)))
Int8.convert(x::Union(Int64,Uint64)) = boxsi8(trunc8(unbox64(x)))
Int8.convert(x::Float32) = boxsi8(fptosi8(unbox32(x)))
Int8.convert(x::Float64) = boxsi8(fptosi8(unbox64(x)))
Uint8.convert(x::Int8) = boxui8(unbox8(x))
Uint8.convert(x::Union(Int16,Uint16)) = boxui8(trunc8(unbox16(x)))
Uint8.convert(x::Union(Int32,Uint32)) = boxui8(trunc8(unbox32(x)))
Uint8.convert(x::Union(Int64,Uint64)) = boxui8(trunc8(unbox64(x)))
Uint8.convert(x::Float32) = boxui8(fptoui8(unbox32(x)))
Uint8.convert(x::Float64) = boxui8(fptoui8(unbox64(x)))

Int16.convert(x::Int8) = boxsi16(sext16(unbox8(x)))
Int16.convert(x::Uint8) = boxsi16(zext16(unbox8(x)))
Int16.convert(x::Uint16) = boxsi16(unbox16(x))
Int16.convert(x::Union(Int32,Uint32)) = boxsi16(trunc16(unbox32(x)))
Int16.convert(x::Union(Int64,Uint64)) = boxsi16(trunc16(unbox64(x)))
Int16.convert(x::Float32) = boxsi16(fptosi16(unbox32(x)))
Int16.convert(x::Float64) = boxsi16(fptosi16(unbox64(x)))
Uint16.convert(x::Int8) = boxui16(sext16(unbox8(x)))
Uint16.convert(x::Uint8) = boxui16(zext16(unbox8(x)))
Uint16.convert(x::Int16) = boxui16(unbox16(x))
Uint16.convert(x::Union(Int32,Uint32)) = boxui16(trunc16(unbox32(x)))
Uint16.convert(x::Union(Int64,Uint64)) = boxui16(trunc16(unbox64(x)))
Uint16.convert(x::Float32) = boxui16(fptoui16(unbox32(x)))
Uint16.convert(x::Float64) = boxui16(fptoui16(unbox64(x)))

Int32.convert(x::Int8) = boxsi32(sext32(unbox8(x)))
Int32.convert(x::Uint8) = boxsi32(zext32(unbox8(x)))
Int32.convert(x::Int16) = boxsi32(sext32(unbox16(x)))
Int32.convert(x::Uint16) = boxsi32(zext32(unbox16(x)))
Int32.convert(x::Uint32) = boxsi32(unbox32(x))
Int32.convert(x::Union(Int64,Uint64)) = boxsi32(trunc32(unbox64(x)))
Int32.convert(x::Float32) = boxsi32(fptosi32(unbox32(x)))
Int32.convert(x::Float64) = boxsi32(fptosi32(unbox64(x)))
Uint32.convert(x::Int8) = boxui32(sext32(unbox8(x)))
Uint32.convert(x::Uint8) = boxui32(zext32(unbox8(x)))
Uint32.convert(x::Int16) = boxui32(sext32(unbox16(x)))
Uint32.convert(x::Uint16) = boxui32(zext32(unbox16(x)))
Uint32.convert(x::Int32) = boxui32(unbox32(x))
Uint32.convert(x::Union(Int64,Uint64)) = boxui32(trunc32(unbox64(x)))
Uint32.convert(x::Float32) = boxui32(fptoui32(unbox32(x)))
Uint32.convert(x::Float64) = boxui32(fptoui32(unbox64(x)))

Int64.convert(x::Int8) = boxsi64(sext64(unbox8(x)))
Int64.convert(x::Uint8) = boxsi64(zext64(unbox8(x)))
Int64.convert(x::Int16) = boxsi64(sext64(unbox16(x)))
Int64.convert(x::Uint16) = boxsi64(zext64(unbox16(x)))
Int64.convert(x::Int32) = boxsi64(sext64(unbox32(x)))
Int64.convert(x::Uint32) = boxsi64(zext64(unbox32(x)))
Int64.convert(x::Uint64) = boxsi64(unbox64(x))
Int64.convert(x::Float32) = boxsi64(fptosi64(unbox32(x)))
Int64.convert(x::Float64) = boxsi64(fptosi64(unbox64(x)))
Uint64.convert(x::Int8) = boxui64(sext64(unbox8(x)))
Uint64.convert(x::Uint8) = boxui64(zext64(unbox8(x)))
Uint64.convert(x::Int16) = boxui64(sext64(unbox16(x)))
Uint64.convert(x::Uint16) = boxui64(zext64(unbox16(x)))
Uint64.convert(x::Int32) = boxui64(sext64(unbox32(x)))
Uint64.convert(x::Uint32) = boxui64(zext64(unbox32(x)))
Uint64.convert(x::Int64) = boxui64(unbox64(x))
Uint64.convert(x::Float32) = boxui64(fptoui64(unbox32(x)))
Uint64.convert(x::Float64) = boxui64(fptoui64(unbox64(x)))


int8(x::Scalar)   = Int8.convert(x)
uint8(x::Scalar)  = Uint8.convert(x)
int16(x::Scalar)  = Int16.convert(x)
uint16(x::Scalar) = Uint16.convert(x)
int32(x::Scalar)  = Int32.convert(x)
uint32(x::Scalar) = Uint32.convert(x)
int64(x::Scalar)  = Int64.convert(x)
uint64(x::Scalar) = Uint64.convert(x)

(+)(x::Int32, y::Int32) = boxsi32(add_int(unbox32(x), unbox32(y)))
(-)(x::Int32, y::Int32) = boxsi32(sub_int(unbox32(x), unbox32(y)))
(-)(x::Int64) = boxsi64(neg_int(unbox64(x)))
(-)(x::Int32) = boxsi32(neg_int(unbox32(x)))
(-)(x::Int16) = boxsi16(neg_int(unbox16(x)))
(-)(x::Int8 ) = boxsi8 (neg_int(unbox8 (x)))
(*)(x::Int32, y::Int32) = boxsi32(mul_int(unbox32(x), unbox32(y)))
(/)(x::Int32, y::Int32) = float64(x)/float64(y)
(//)(x::Int32, y::Int32) = rational(x,y)
div(x::Int32, y::Int32) = boxsi32(sdiv_int(unbox32(x), unbox32(y)))
(%)(x::Int32, y::Int32) = boxsi32(smod_int(unbox32(x), unbox32(y)))

==(x::Int32, y::Int32) = eq_int(unbox32(x),unbox32(y))
<=(x::Int32, y::Int32) = slt_int(unbox32(x),unbox32(y)) || eq_int(unbox32(x),unbox32(y))
< (x::Int32, y::Int32) = slt_int(unbox32(x),unbox32(y))
> (x::Int32, y::Int32) = slt_int(unbox32(y),unbox32(x))
>=(x::Int32, y::Int32) = (x>y) || eq_int(unbox32(x),unbox32(y))

isodd(n::Int)  = ((n%2)==1)
iseven(n::Int) = ((n%2)==0)

function gcd(a::Int, b::Int)
    while b != 0
        t = b
        b = a % b
        a = t
    end
    return a
end

lcm(a::Int, b::Int) = div(a*b,gcd(a,b))

# return (gcd(a,b),x,y) such that ax+by == gcd(a,b)
function gcdx(a, b)
    if b == 0
        (a, 1, 0)
    else
        m = a%b
        k = div((a-m), b)
        (g, x, y) = gcdx(b, m)
        (g, y, x-k*y)
    end
end

# multiplicative inverse of x mod m, false if none
function invmod(n, m)
    (g, x, y) = gcdx(n, m)
    if g != 1
        false
    elseif x<0
        m+x
    else
        x
    end
end

function ^(x::Tensor, p::Int)
    if p == 1
        return x
    elseif p == 0
        return convert(1,typeof(x))
    elseif p < 0
        return 1/(x^(-p))
    elseif p == 2
        return x*x
    end
    t = 1
    while t <= p
        t *= 2
    end
    t = div(t,2)
    p -= t
    a = x
    while true
        t = div(t,2)
        if t > 0
            x = x*x
        else
            break
        end

        if p >= t
            x = x*a
            p -= t
        end
    end
    return x
end

# x^p mod m
function powermod(x::Int, p::Int, m::Int)
    if p == 0
        return convert(1,typeof(x))
    elseif p < 0
        error("powermod: exponent must be >= 0")
    end
    t = 1
    while t <= p
        t *= 2
    end
    t = div(t,2)
    r = 1
    while true
        if p >= t
            r = (r*x) % m
            p -= t
        end
        t = div(t,2)
        if t > 0
            r = (r*r) % m
        else
            break
        end
    end
    return r
end

function nPr(n::Int, r::Int)
    if (r < 0 || n < 0 || r > n)
        return 0
    end

    ans = 1
    while (r > 0)
        ans *= n
        n -= 1
        r -= 1
    end
    return ans
end

function nCr(n::Int, r::Int)
    if (r < 0 || n == 0)
        return 0
    end

    neg = false
    if (n < 0)
        n = (-n)+r-1
        if isodd(r)
            neg = true
        end
    end

    if (r > n)
        return 0
    end
    if (r == 0 || r == n)
        return 1
    end

    if r > div(n,2)
        r = (n - r)
    end

    ans = nn = n - r + 1.0
    nn += 1.0
    rr = 2.0
    while (rr <= r+0.0)
        ans *= (nn/rr)
        rr += 1
        nn += 1
    end
    if neg
        return -truncate(ans)
    end
    return truncate(ans)
end

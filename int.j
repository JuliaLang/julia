## conversions ##

convert(::Type{Int8}, x::Uint8) = boxsi8(unbox8(x))
convert(::Type{Int8}, x::Union(Int16,Uint16)) = boxsi8(trunc8(unbox16(x)))
convert(::Type{Int8}, x::Union(Int32,Uint32)) = boxsi8(trunc8(unbox32(x)))
convert(::Type{Int8}, x::Union(Int64,Uint64)) = boxsi8(trunc8(unbox64(x)))
convert(::Type{Int8}, x::Float32) = boxsi8(fptosi8(unbox32(x)))
convert(::Type{Int8}, x::Float64) = boxsi8(fptosi8(unbox64(x)))
convert(::Type{Uint8}, x::Int8) = boxui8(unbox8(x))
convert(::Type{Uint8}, x::Union(Int16,Uint16)) = boxui8(trunc8(unbox16(x)))
convert(::Type{Uint8}, x::Union(Int32,Uint32)) = boxui8(trunc8(unbox32(x)))
convert(::Type{Uint8}, x::Union(Int64,Uint64)) = boxui8(trunc8(unbox64(x)))
convert(::Type{Uint8}, x::Float32) = boxui8(fptoui8(unbox32(x)))
convert(::Type{Uint8}, x::Float64) = boxui8(fptoui8(unbox64(x)))

convert(::Type{Int16}, x::Int8) = boxsi16(sext16(unbox8(x)))
convert(::Type{Int16}, x::Uint8) = boxsi16(zext16(unbox8(x)))
convert(::Type{Int16}, x::Uint16) = boxsi16(unbox16(x))
convert(::Type{Int16}, x::Union(Int32,Uint32)) = boxsi16(trunc16(unbox32(x)))
convert(::Type{Int16}, x::Union(Int64,Uint64)) = boxsi16(trunc16(unbox64(x)))
convert(::Type{Int16}, x::Float32) = boxsi16(fptosi16(unbox32(x)))
convert(::Type{Int16}, x::Float64) = boxsi16(fptosi16(unbox64(x)))
convert(::Type{Uint16}, x::Int8) = boxui16(sext16(unbox8(x)))
convert(::Type{Uint16}, x::Uint8) = boxui16(zext16(unbox8(x)))
convert(::Type{Uint16}, x::Int16) = boxui16(unbox16(x))
convert(::Type{Uint16}, x::Union(Int32,Uint32)) = boxui16(trunc16(unbox32(x)))
convert(::Type{Uint16}, x::Union(Int64,Uint64)) = boxui16(trunc16(unbox64(x)))
convert(::Type{Uint16}, x::Float32) = boxui16(fptoui16(unbox32(x)))
convert(::Type{Uint16}, x::Float64) = boxui16(fptoui16(unbox64(x)))

convert(::Type{Int32}, x::Int8) = boxsi32(sext32(unbox8(x)))
convert(::Type{Int32}, x::Uint8) = boxsi32(zext32(unbox8(x)))
convert(::Type{Int32}, x::Int16) = boxsi32(sext32(unbox16(x)))
convert(::Type{Int32}, x::Uint16) = boxsi32(zext32(unbox16(x)))
convert(::Type{Int32}, x::Uint32) = boxsi32(unbox32(x))
convert(::Type{Int32}, x::Union(Int64,Uint64)) = boxsi32(trunc32(unbox64(x)))
convert(::Type{Int32}, x::Float32) = boxsi32(fptosi32(unbox32(x)))
convert(::Type{Int32}, x::Float64) = boxsi32(fptosi32(unbox64(x)))
convert(::Type{Uint32}, x::Int8) = boxui32(sext32(unbox8(x)))
convert(::Type{Uint32}, x::Uint8) = boxui32(zext32(unbox8(x)))
convert(::Type{Uint32}, x::Int16) = boxui32(sext32(unbox16(x)))
convert(::Type{Uint32}, x::Uint16) = boxui32(zext32(unbox16(x)))
convert(::Type{Uint32}, x::Int32) = boxui32(unbox32(x))
convert(::Type{Uint32}, x::Union(Int64,Uint64)) = boxui32(trunc32(unbox64(x)))
convert(::Type{Uint32}, x::Float32) = boxui32(fptoui32(unbox32(x)))
convert(::Type{Uint32}, x::Float64) = boxui32(fptoui32(unbox64(x)))

convert(::Type{Int64}, x::Int8) = boxsi64(sext64(unbox8(x)))
convert(::Type{Int64}, x::Uint8) = boxsi64(zext64(unbox8(x)))
convert(::Type{Int64}, x::Int16) = boxsi64(sext64(unbox16(x)))
convert(::Type{Int64}, x::Uint16) = boxsi64(zext64(unbox16(x)))
convert(::Type{Int64}, x::Int32) = boxsi64(sext64(unbox32(x)))
convert(::Type{Int64}, x::Uint32) = boxsi64(zext64(unbox32(x)))
convert(::Type{Int64}, x::Uint64) = boxsi64(unbox64(x))
convert(::Type{Int64}, x::Float32) = boxsi64(fptosi64(unbox32(x)))
convert(::Type{Int64}, x::Float64) = boxsi64(fptosi64(unbox64(x)))
convert(::Type{Uint64}, x::Int8) = boxui64(sext64(unbox8(x)))
convert(::Type{Uint64}, x::Uint8) = boxui64(zext64(unbox8(x)))
convert(::Type{Uint64}, x::Int16) = boxui64(sext64(unbox16(x)))
convert(::Type{Uint64}, x::Uint16) = boxui64(zext64(unbox16(x)))
convert(::Type{Uint64}, x::Int32) = boxui64(sext64(unbox32(x)))
convert(::Type{Uint64}, x::Uint32) = boxui64(zext64(unbox32(x)))
convert(::Type{Uint64}, x::Int64) = boxui64(unbox64(x))
convert(::Type{Uint64}, x::Float32) = boxui64(fptoui64(unbox32(x)))
convert(::Type{Uint64}, x::Float64) = boxui64(fptoui64(unbox64(x)))

int8(x::Scalar)   = convert(Int8, x)
uint8(x::Scalar)  = convert(Uint8, x)
int16(x::Scalar)  = convert(Int16, x)
uint16(x::Scalar) = convert(Uint16, x)
int32(x::Scalar)  = convert(Int32, x)
uint32(x::Scalar) = convert(Uint32, x)
int64(x::Scalar)  = convert(Int64, x)
uint64(x::Scalar) = convert(Uint64, x)

## promotions ##

promote_rule(::Type{Int16}, ::Type{Int8} ) = Int16
promote_rule(::Type{Int32}, ::Type{Int8} ) = Int32
promote_rule(::Type{Int32}, ::Type{Int16}) = Int32
promote_rule(::Type{Int64}, ::Type{Int8} ) = Int64
promote_rule(::Type{Int64}, ::Type{Int16}) = Int64
promote_rule(::Type{Int64}, ::Type{Int32}) = Int64

## basic arithmetic ##

(-)(x::Int8 ) = boxsi8 (neg_int(unbox8 (x)))
(-)(x::Int16) = boxsi16(neg_int(unbox16(x)))
(-)(x::Int32) = boxsi32(neg_int(unbox32(x)))
(-)(x::Int64) = boxsi64(neg_int(unbox64(x)))

(+)(x::Int8 , y::Int8 ) = boxsi8 (add_int(unbox8 (x), unbox8 (y)))
(+)(x::Int16, y::Int16) = boxsi16(add_int(unbox16(x), unbox16(y)))
(+)(x::Int32, y::Int32) = boxsi32(add_int(unbox32(x), unbox32(y)))
(+)(x::Int64, y::Int64) = boxsi64(add_int(unbox64(x), unbox64(y)))

(-)(x::Int8 , y::Int8 ) = boxsi8 (sub_int(unbox8 (x), unbox8 (y)))
(-)(x::Int16, y::Int16) = boxsi16(sub_int(unbox16(x), unbox16(y)))
(-)(x::Int32, y::Int32) = boxsi32(sub_int(unbox32(x), unbox32(y)))
(-)(x::Int64, y::Int64) = boxsi64(sub_int(unbox64(x), unbox64(y)))

(*)(x::Int8 , y::Int8 ) = boxsi8 (mul_int(unbox8 (x), unbox8 (y)))
(*)(x::Int16, y::Int16) = boxsi16(mul_int(unbox16(x), unbox16(y)))
(*)(x::Int32, y::Int32) = boxsi32(mul_int(unbox32(x), unbox32(y)))
(*)(x::Int64, y::Int64) = boxsi64(mul_int(unbox64(x), unbox64(y)))

(/)(x::Int, y::Int) = float64(x)/float64(y)

div(x::Int8 , y::Int8 ) = boxsi8 (sdiv_int(unbox8 (x), unbox8 (y)))
div(x::Int16, y::Int16) = boxsi16(sdiv_int(unbox16(x), unbox16(y)))
div(x::Int32, y::Int32) = boxsi32(sdiv_int(unbox32(x), unbox32(y)))
div(x::Int64, y::Int64) = boxsi64(sdiv_int(unbox64(x), unbox64(y)))

(%)(x::Int8 , y::Int8 ) = boxsi8 (smod_int(unbox8 (x), unbox8 (y)))
(%)(x::Int16, y::Int16) = boxsi16(smod_int(unbox16(x), unbox16(y)))
(%)(x::Int32, y::Int32) = boxsi32(smod_int(unbox32(x), unbox32(y)))
(%)(x::Int64, y::Int64) = boxsi64(smod_int(unbox64(x), unbox64(y)))

## bitwise operations ##

(~)(x::Int8 ) = boxsi8 (not_int(unbox8 (x)))
(~)(x::Int16) = boxsi16(not_int(unbox16(x)))
(~)(x::Int32) = boxsi32(not_int(unbox32(x)))
(~)(x::Int64) = boxsi64(not_int(unbox64(x)))

(&)(x::Int8 , y::Int8 ) = boxsi8 (and_int(unbox8 (x), unbox8 (y)))
(&)(x::Int16, y::Int16) = boxsi16(and_int(unbox16(x), unbox16(y)))
(&)(x::Int32, y::Int32) = boxsi32(and_int(unbox32(x), unbox32(y)))
(&)(x::Int64, y::Int64) = boxsi64(and_int(unbox64(x), unbox64(y)))

(|)(x::Int8 , y::Int8 ) = boxsi8 (or_int(unbox8 (x), unbox8 (y)))
(|)(x::Int16, y::Int16) = boxsi16(or_int(unbox16(x), unbox16(y)))
(|)(x::Int32, y::Int32) = boxsi32(or_int(unbox32(x), unbox32(y)))
(|)(x::Int64, y::Int64) = boxsi64(or_int(unbox64(x), unbox64(y)))

($)(x::Int8 , y::Int8 ) = boxsi8 (xor_int(unbox8 (x), unbox8 (y)))
($)(x::Int16, y::Int16) = boxsi16(xor_int(unbox16(x), unbox16(y)))
($)(x::Int32, y::Int32) = boxsi32(xor_int(unbox32(x), unbox32(y)))
($)(x::Int64, y::Int64) = boxsi64(xor_int(unbox64(x), unbox64(y)))

## integer comparisons ##

==(x::Int8 , y::Int8 ) = eq_int(unbox8 (x),unbox8 (y))
==(x::Int16, y::Int16) = eq_int(unbox16(x),unbox16(y))
==(x::Int32, y::Int32) = eq_int(unbox32(x),unbox32(y))
==(x::Int64, y::Int64) = eq_int(unbox64(x),unbox64(y))

< (x::Int8 , y::Int8 ) = slt_int(unbox8 (x),unbox8 (y))
< (x::Int16, y::Int16) = slt_int(unbox16(x),unbox16(y))
< (x::Int32, y::Int32) = slt_int(unbox32(x),unbox32(y))
< (x::Int64, y::Int64) = slt_int(unbox64(x),unbox64(y))

## character operations & comparisons ##
# TODO: implement real unisgned stuff

(+)(x::Uint8, y::Uint8) = uint8(int8(x) + int8(y))
(-)(x::Uint8, y::Uint8) = uint8(int8(x) - int8(y))
==(x::Uint8, y::Uint8) = int8(x) == int8(y)
< (x::Uint8, y::Uint8) = int16(x) < int16(y)

## integer-specific arithmetic promotions ##

(%)(x::Int, y::Int) = (%)(promote(x,y)...)
div(x::Int, y::Int) = div(promote(x,y)...)

(&)(x::Int...) = (&)(promote(x...)...)
(|)(x::Int...) = (|)(promote(x...)...)
($)(x::Int...) = ($)(promote(x...)...)

## string to integer functions ##

function digit(c::Uint8)
    "0"[1] <= c <= "9"[1] ? int32(c - "0"[1]) :
    "A"[1] <= c <= "Z"[1] ? int32(c - "A"[1]) + 10 :
    "a"[1] <= c <= "z"[1] ? int32(c - "a"[1]) + 10 :
    error("non alphanumeric digit")
end

function parse_int(T::Type{Int}, str::String, base::Int32)
    n = convert(T,0)
    base = convert(T,base)
    for p = 0:length(str)-1
        d = convert(T,digit(str[length(str)-p]))
        if base <= d
            error("digit not valid in base")
        end
        n += d*base^convert(T,p)
    end
    return n
end

bin(str::String) = parse_int(Int64, str,  2)
oct(str::String) = parse_int(Int64, str,  8)
dec(str::String) = parse_int(Int64, str, 10)
hex(str::String) = parse_int(Int64, str, 16)

## integer functions ##

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
        m = a % b
        k = div((a-m), b)
        (g, x, y) = gcdx(b, m)
        (g, y, x-k*y)
    end
end

# multiplicative inverse of x mod m, false if none
function invmod(n, m)
    g, x, y = gcdx(n, m)
    g != 1 ? false : (x < 0 ? m + x : x)
end

# ^ for any x supporting *
function ^(x, p::Int)
    if p == 1
        return x
    elseif p == 0
        return convert(typeof(x),1)
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
        return convert(typeof(x),1)
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

function fact{T}(n::Int{T})
    if n < 0
        return convert(T,0)
    end
    p = convert(T,1)
    for i=2:n
        p*=i
    end
    p
end

function nPr(n::Int, r::Int)
    if r < 0 || n < 0 || r > n
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
    if r < 0
        return 0
    end

    neg = false
    if n < 0
        n = (-n)+r-1
        if isodd(r)
            neg = true
        end
    end

    if r > n
        return 0
    end
    if r == 0 || r == n
        return 1
    end

    if r > div(n,2)
        r = (n - r)
    end

    ans = nn = n - r + 1.0
    nn += 1.0
    rr = 2.0
    while (rr <= r)
        ans *= (nn/rr)
        rr += 1
        nn += 1
    end
    if neg
        return -truncate(ans)
    end
    return truncate(ans)
end

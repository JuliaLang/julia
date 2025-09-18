@inline function use_power_by_squaring(n::Integer)
    -2^12 <= n <= 3 * 2^13
end

# @constprop aggressive to help the compiler see the switch between the integer and float
# variants for callers with constant `y`
@constprop :aggressive function ^(x::Float64, y::Float64)
    xu = reinterpret(UInt64, x)
    xu == reinterpret(UInt64, 1.0) && return 1.0
    # Exponents greater than this will always overflow or underflow.
    # Note that NaN can pass through this, but that will end up fine.
    if !(abs(y)<0x1.8p62)
        isnan(y) && return y
        y = sign(y)*0x1.8p62
    end
    yint = unsafe_trunc(Int64, y) # This is actually safe since julia freezes the result
    yisint = y == yint
    if yisint
        yint == 0 && return 1.0
        use_power_by_squaring(yint) && return @noinline pow_body(x, yint)
    end
    2*xu==0 && return abs(y)*Inf*(!(y>0)) # if x === +0.0 or -0.0 (Inf * false === 0.0)
    s = 1
    if x < 0
        !yisint && throw_exp_domainerror(x) # y isn't an integer
        s = ifelse(isodd(yint), -1, 1)
    end
    !isfinite(x) && return copysign(x,s)*(y>0 || isnan(x))           # x is inf or NaN
    return copysign(pow_body(abs(x), y), s)
end

# @constprop aggressive to help the compiler see the switch between the integer and float
# variants for callers with constant `y`
@constprop :aggressive function ^(x::T, y::T) where T <: Union{Float16, Float32}
    x == 1 && return one(T)
    # Exponents greater than this will always overflow or underflow.
    # Note that NaN can pass through this, but that will end up fine.
    max_exp = T == Float16 ? T(3<<14) : T(0x1.Ap30)
    if !(abs(y)<max_exp)
        isnan(y) && return y
        y = sign(y)*max_exp
    end
    yint = unsafe_trunc(Int32, y) # This is actually safe since julia freezes the result
    yisint = y == yint
    if yisint
        yint == 0 && return one(T)
        use_power_by_squaring(yint) && return pow_body(x, yint)
    end
    s = 1
    if x < 0
        !yisint && throw_exp_domainerror(x) # y isn't an integer
        s = ifelse(isodd(yint), -1, 1)
    end
    !isfinite(x) && return copysign(x,s)*(y>0 || isnan(x)) # x is inf or NaN
    return copysign(pow_body(abs(x), y), s)
end

@constprop :aggressive @inline function ^(x::Float64, n::Integer)
    n = clamp(n, Int64)
    n == 0 && return one(x)
    if use_power_by_squaring(n)
        return pow_body(x, n)
    else
        s = ifelse(x < 0 && isodd(n), -1.0, 1.0)
        x = abs(x)
        y = float(n)
        if y == n
            return copysign(pow_body(x, y), s)
        else
            n2 = n % 1024
            y = float(n - n2)
            return pow_body(x, y) * copysign(pow_body(x, n2), s)
        end
    end
end

# @constprop aggressive to help the compiler see the switch between the integer and float
# variants for callers with constant `y`
@constprop :aggressive @inline function ^(x::T, n::Integer) where T <: Union{Float16, Float32}
    n = clamp(n, Int32)
    # Exponents greater than this will always overflow or underflow.
    # Note that NaN can pass through this, but that will end up fine.
    n == 0 && return one(x)
    use_power_by_squaring(n) && return pow_body(x, n)
    s = ifelse(x < 0 && isodd(n), -one(T), one(T))
    x = abs(x)
    return pow_body(x, widen(T)(n))
end

@assume_effects :foldable @noinline function pow_body(x::Float64, y::Float64)
    xu = reinterpret(UInt64, x)
    if xu < (UInt64(1)<<52) # x is subnormal
        xu = reinterpret(UInt64, x * 0x1p52) # normalize x
        xu &= ~sign_mask(Float64)
        xu -= UInt64(52) << 52 # mess with the exponent
    end
    logxhi,logxlo = _log_ext(xu)
    xyhi, xylo = two_mul(logxhi,y)
    xylo = muladd(logxlo, y, xylo)
    hi = xyhi+xylo
    return @inline Base.Math.exp_impl(hi, xylo-(hi-xyhi), Val(:ℯ))
end

@inline function pow_body(x::T, y) where T <: Union{Float16, Float32}
    return T(exp2(log2(abs(widen(x))) * y))
end

@inline function pow_body(x::Union{Float16, Float32}, n::Int32)
    n == -2 && return (i=inv(x); i*i)
    n == 3 && return x*x*x #keep compatibility with literal_pow
    n < 0 && return oftype(x, Base.power_by_squaring(inv(widen(x)), -n))
    return oftype(x, Base.power_by_squaring(widen(x), n))
end

# compensated power by squaring
# this method is only reliable for -2^20 < n < 2^20 (cf. #53881 #53886)
@assume_effects :terminates_locally @noinline function pow_body(x::Float64, n::Integer)
    y = 1.0
    xnlo = -0.0
    ynlo = 0.0
    n == 3 && return x*x*x # keep compatibility with literal_pow
    if n < 0
        rx = inv(x)
        n==-2 && return rx*rx #keep compatibility with literal_pow
        isfinite(x) && (xnlo = -fma(x, rx, -1.) * rx)
        x = rx
        n = -n
    end
    while n > 1
        if n&1 > 0
            err = muladd(y, xnlo, x*ynlo)
            y, ynlo = two_mul(x,y)
            ynlo += err
        end
        err = x*2*xnlo
        x, xnlo = two_mul(x, x)
        xnlo += err
        n >>>= 1
    end
    err = muladd(y, xnlo, x*ynlo)
    return ifelse(isfinite(x) & isfinite(err), muladd(x, y, err), x*y)
end

# This file is a part of Julia. License is MIT: http://julialang.org/license

# Twice-precision arithmetic.

# Necessary for creating nicely-behaved ranges like r = 0.1:0.1:0.3
# that return r[3] == 0.3.  Otherwise, we have roundoff error due to
#     0.1 + 2*0.1 = 0.30000000000000004

"""
    TwicePrecision{T}(hi::T, lo::T)
    TwicePrecision{T}((num, denom))

A number with twice the precision of `T`, e.g., quad-precision if `T =
Float64`. `hi` represents the high bits (most significant bits) and
`lo` the low bits (least significant bits). Rational values
`num//denom` can be approximated conveniently using the syntax
`TwicePrecision{T}((num, denom))`.

When used with `T<:AbstractFloat` to construct an exact
`StepRangeLen`, `ref` should be the range element with smallest
magnitude and `offset` set to the corresponding index.  For
efficiency, multiplication of `step` by the index is not performed at
twice precision: `step.hi` should have enough trailing zeros in its
`bits` representation that `(0:len-1)*step.hi` is exact (has no
roundoff error).  If `step` has an exact rational representation
`num//denom`, then you can construct `step` using

    step = TwicePrecision{T}((num, denom), nb)

where `nb` is the number of trailing zero bits of `step.hi`.  For
ranges, you can set `nb = ceil(Int, log2(len-1))`.
"""
immutable TwicePrecision{T}
    hi::T    # most significant bits
    lo::T    # least significant bits
end

function (::Type{TwicePrecision{T}}){T,I}(nd::Tuple{I,I})
    n, d = nd
    TwicePrecision{T}(n, zero(T)) / d
end

function (::Type{TwicePrecision{T}}){T,I}(nd::Tuple{I,I}, nb::Integer)
    twiceprecision(TwicePrecision{T}(nd), nb)
end

function twiceprecision{T<:Number}(val::T, nb::Integer)
    hi = truncbits(val, nb)
    TwicePrecision{T}(hi, val - hi)
end

function twiceprecision{T<:Number}(val::TwicePrecision{T}, nb::Integer)
    hi = truncbits(val.hi, nb)
    TwicePrecision{T}(hi, (val.hi - hi) + val.lo)
end

nbitslen(r::StepRangeLen) = nbitslen(eltype(r), length(r), r.offset)
nbitslen(::Type{Float64}, len, offset) = min(26, nbitslen(len, offset))
nbitslen(::Type{Float32}, len, offset) = min(12, nbitslen(len, offset))
nbitslen(::Type{Float16}, len, offset) = min(5,  nbitslen(len, offset))
nbitslen(len, offset) = len < 2 ? 0 : ceil(Int, log2(max(offset-1, len-offset)))

eltype{T}(::Type{TwicePrecision{T}}) = T

promote_rule{R,S}(::Type{TwicePrecision{R}}, ::Type{TwicePrecision{S}}) =
    TwicePrecision{promote_type(R,S)}
promote_rule{R,S}(::Type{TwicePrecision{R}}, ::Type{S}) =
    TwicePrecision{promote_type(R,S)}

convert{T}(::Type{TwicePrecision{T}}, x::TwicePrecision{T}) = x
convert{T}(::Type{TwicePrecision{T}}, x::TwicePrecision) =
    TwicePrecision{T}(convert(T, x.hi), convert(T, x.lo))

convert{T<:Number}(::Type{T}, x::TwicePrecision{T}) = convert(T, x.hi + x.lo)
convert{T}(::Type{TwicePrecision{T}}, x::Number) = TwicePrecision{T}(convert(T, x), zero(T))

float{T<:AbstractFloat}(x::TwicePrecision{T}) = x
float(x::TwicePrecision) = TwicePrecision(float(x.hi), float(x.lo))

big(x::TwicePrecision) = big(x.hi) + big(x.lo)

-(x::TwicePrecision) = TwicePrecision(-x.hi, -x.lo)

zero{T}(::Type{TwicePrecision{T}}) = TwicePrecision{T}(0, 0)

## StepRangeLen

# If using TwicePrecision numbers, deliberately force user to specify offset
StepRangeLen{T}(ref::TwicePrecision{T}, step::TwicePrecision{T}, len::Integer, offset::Integer) =
    StepRangeLen{T,TwicePrecision{T},TwicePrecision{T}}(ref, step, len, offset)

# Construct range for rational start=start_n/den, step=step_n/den
function floatrange{T}(::Type{T}, start_n::Integer, step_n::Integer, len::Integer, den::Integer)
    if len < 2
        return StepRangeLen(TwicePrecision{T}((start_n, den)),
                            TwicePrecision{T}((step_n, den)), Int(len), 1)
    end
    # index of smallest-magnitude value
    imin = clamp(round(Int, -start_n/step_n+1), 1, Int(len))
    # Compute smallest-magnitude element to 2x precision
    ref_n = start_n+(imin-1)*step_n  # this shouldn't overflow, so don't check
    nb = nbitslen(T, len, imin)
    StepRangeLen(TwicePrecision{T}((ref_n, den)),
                 TwicePrecision{T}((step_n, den), nb), Int(len), imin)
end

function floatrange(a::AbstractFloat, st::AbstractFloat, len::Real, divisor::AbstractFloat)
    T = promote_type(typeof(a), typeof(st), typeof(divisor))
    m = maxintfloat(T)
    if abs(a) <= m && abs(st) <= m && abs(divisor) <= m
        ia, ist, idivisor = round(Int, a), round(Int, st), round(Int, divisor)
        if ia == a && ist == st && idivisor == divisor
            # We can return the high-precision range
            return floatrange(T, ia, ist, Int(len), idivisor)
        end
    end
    # Fallback (misses the opportunity to set offset different from 1,
    # but otherwise this is still high-precision)
    StepRangeLen(TwicePrecision{T}((a,divisor)),
                 TwicePrecision{T}((st,divisor), nbitslen(T, len, 1)), Int(len), 1)
end

function colon{T<:Union{Float16,Float32,Float64}}(start::T, step::T, stop::T)
    step == 0 && throw(ArgumentError("range step cannot be zero"))
    len = max(0, floor(Int, (stop-start)/step) + 1)
    # Because len might be too small by 1 due to roundoff error, let's
    # see if the inputs have exact rational approximations (and if so,
    # perform all computations in terms of the rationals)
    step_n, step_d = rat(step)
    if step_d != 0 && T(step_n/step_d) == step
        start_n, start_d = rat(start)
        stop_n, stop_d = rat(stop)
        if start_d != 0 && stop_d != 0 &&
                T(start_n/start_d) == start && T(stop_n/stop_d) == stop
            den = lcm(start_d, step_d) # use same denominator for start and step
            m = maxintfloat(T)
            if den != 0 && abs(start*den) <= m && abs(step*den) <= m &&  # will round succeed?
                    rem(den, start_d) == 0 && rem(den, step_d) == 0      # check lcm overflow
                start_n = round(Int, start*den)
                step_n = round(Int, step*den)
                len = max(0, div(den*stop_n - stop_d*start_n + step_n*stop_d, step_n*stop_d))
                # Integer ops could overflow, so check that this makes sense
                if isbetween(start, start + (len-1)*step, stop + step/2) &&
                        !isbetween(start, start + len*step, stop)
                    # Return a 2x precision range
                    return floatrange(T, start_n, step_n, len, den)
                end
            end
        end
    end
    # Fallback, taking start and step literally
    StepRangeLen(TwicePrecision(start, zero(T)), twiceprecision(step, nbitslen(T, len, 1)), len)
end

function range{T<:Union{Float16,Float32,Float64}}(a::T, st::T, len::Integer)
    start_n, start_d = rat(a)
    step_n, step_d = rat(st)
    if start_d != 0 && step_d != 0 &&
            T(start_n/start_d) == a && T(step_n/step_d) == st
        den = lcm(start_d, step_d)
        m = maxintfloat(T)
        if abs(den*a) <= m && abs(den*st) <= m &&
                rem(den, start_d) == 0 && rem(den, step_d) == 0
            start_n = round(Int, den*a)
            step_n = round(Int, den*st)
            return floatrange(T, start_n, step_n, len, den)
        end
    end
    StepRangeLen(TwicePrecision(a, zero(T)), TwicePrecision(st, zero(T)), len)
end

step{T,R,S<:TwicePrecision}(r::StepRangeLen{T,R,S}) = convert(eltype(S), r.step)

start{T,R<:TwicePrecision,S<:TwicePrecision}(r::StepRangeLen{T,R,S}) = 1
done{T,R<:TwicePrecision,S<:TwicePrecision}(r::StepRangeLen{T,R,S}, i::Int) = length(r) < i
function next{T,R<:TwicePrecision,S<:TwicePrecision}(r::StepRangeLen{T,R,S}, i::Int)
    @_inline_meta
    unsafe_getindex(r, i), i+1
end

# This assumes that r.step has already been split so that (0:len-1)*r.step.hi is exact
function unsafe_getindex{T,R<:TwicePrecision,S<:TwicePrecision}(r::StepRangeLen{T,R,S}, i::Integer)
    # Very similar to _getindex_hiprec, but optimized to avoid a 2nd call to add2
    @_inline_meta
    u = i - r.offset
    shift_hi, shift_lo = u*r.step.hi, u*r.step.lo
    x_hi, x_lo = add2(r.ref.hi, shift_hi)
    T(x_hi + (x_lo + (shift_lo + r.ref.lo)))
end

function _getindex_hiprec{T,R<:TwicePrecision,S<:TwicePrecision}(r::StepRangeLen{T,R,S}, i::Integer)
    u = i - r.offset
    shift_hi, shift_lo = u*r.step.hi, u*r.step.lo
    x_hi, x_lo = add2(r.ref.hi, shift_hi)
    x_hi, x_lo = add2(x_hi, x_lo + (shift_lo + r.ref.lo))
    TwicePrecision(x_hi, x_lo)
end

function getindex{T,R<:TwicePrecision,S<:TwicePrecision,I<:Integer}(r::StepRangeLen{T,R,S}, s::OrdinalRange{I})
    @_inline_meta
    @boundscheck checkbounds(r, s)
    soffset = 1 + round(Int, (r.offset - first(s))/step(s))
    soffset = clamp(soffset, 1, length(s))
    ioffset = start(s) + (soffset-1)*step(s)
    if step(s) == 1 || length(s) < 2
        newstep = r.step
    else
        newstep = twiceprecision(r.step*step(s), nbitslen(T, length(s), soffset))
    end
    if ioffset == r.offset
        StepRangeLen(r.ref, newstep, length(s), max(1,soffset))
    else
        StepRangeLen(r.ref + (ioffset-r.offset)*r.step, newstep, length(s), max(1,soffset))
    end
end

*{T<:Real,R<:TwicePrecision}(x::Real, r::StepRangeLen{T,R}) =
    StepRangeLen(x*r.ref, twiceprecision(x*r.step, nbitslen(r)), length(r), r.offset)
*{T<:Real,R<:TwicePrecision}(r::StepRangeLen{T,R}, x::Real) = x*r
/{T<:Real,R<:TwicePrecision}(r::StepRangeLen{T,R}, x::Real) =
    StepRangeLen(r.ref/x, twiceprecision(r.step/x, nbitslen(r)), length(r), r.offset)

convert{T<:AbstractFloat,R<:TwicePrecision,S<:TwicePrecision}(::Type{StepRangeLen{T,R,S}}, r::StepRangeLen{T,R,S}) = r

convert{T<:AbstractFloat,R<:TwicePrecision,S<:TwicePrecision}(::Type{StepRangeLen{T,R,S}}, r::StepRangeLen) =
    _convertSRL(StepRangeLen{T,R,S}, r)

convert{T<:Union{Float16,Float32,Float64}}(::Type{StepRangeLen{T}}, r::StepRangeLen) =
    _convertSRL(StepRangeLen{T,TwicePrecision{T},TwicePrecision{T}}, r)

convert{T<:Union{Float16,Float32,Float64}}(::Type{StepRangeLen{T}}, r::Range) =
    _convertSRL(StepRangeLen{T,TwicePrecision{T},TwicePrecision{T}}, r)

function _convertSRL{T,R,S,I<:Integer}(::Type{StepRangeLen{T,R,S}}, r::StepRangeLen{I})
    StepRangeLen{T,R,S}(R(r.ref), S(r.step), length(r), r.offset)
end

function _convertSRL{T,R,S,I<:Integer}(::Type{StepRangeLen{T,R,S}}, r::Range{I})
    StepRangeLen{T,R,S}(R(first(r)), S(step(r)), length(r))
end

function _convertSRL{T,R,S,U}(::Type{StepRangeLen{T,R,S}}, r::Range{U})
    # if start and step have a rational approximation in the old type,
    # then we transfer that rational approximation to the new type
    f, s = first(r), step(r)
    start_n, start_d = rat(f)
    step_n, step_d = rat(s)
    if start_d != 0 && step_d != 0 &&
            U(start_n/start_d) == f && U(step_n/step_d) == s
        den = lcm(start_d, step_d)
        m = maxintfloat(T)
        if den != 0 && abs(f*den) <= m && abs(s*den) <= m &&
                rem(den, start_d) == 0 && rem(den, step_d) == 0
            start_n = round(Int, f*den)
            step_n = round(Int, s*den)
            return floatrange(T, start_n, step_n, length(r), den)
        end
    end
    __convertSRL(StepRangeLen{T,R,S}, r)
end

function __convertSRL{T,R,S,U}(::Type{StepRangeLen{T,R,S}}, r::StepRangeLen{U})
    StepRangeLen{T,R,S}(R(r.ref), S(r.step), length(r), r.offset)
end
function __convertSRL{T,R,S,U}(::Type{StepRangeLen{T,R,S}}, r::Range{U})
    StepRangeLen{T,R,S}(R(first(r)), S(step(r)), length(r))
end

function sum(r::StepRangeLen)
    l = length(r)
    # Compute the contribution of step over all indexes.
    # Indexes on opposite side of r.offset contribute with opposite sign,
    #    r.step * (sum(1:np) - sum(1:nn))
    np, nn = l - r.offset, r.offset - 1  # positive, negative
    # To prevent overflow in sum(1:n), multiply its factors by the step
    sp, sn = sumpair(np), sumpair(nn)
    tp = _prod(r.step, sp[1], sp[2])
    tn = _prod(r.step, sn[1], sn[2])
    s_hi, s_lo = add2(tp.hi, -tn.hi)
    s_lo += tp.lo - tn.lo
    # Add in contributions of ref
    ref = r.ref * l
    sm_hi, sm_lo = add2(s_hi, ref.hi)
    add2(sm_hi, sm_lo + ref.lo)[1]
end

# sum(1:n) as a product of two integers
sumpair(n::Integer) = iseven(n) ? (n+1, n>>1) : (n, (n+1)>>1)

function +{T,R<:TwicePrecision}(r1::StepRangeLen{T,R}, r2::StepRangeLen{T,R})
    len = length(r1)
    (len == length(r2) ||
        throw(DimensionMismatch("argument dimensions must match")))
    if r1.offset == r2.offset
        imid = r1.offset
        ref = r1.ref + r2.ref
    else
        imid = round(Int, (r1.offset+r2.offset)/2)
        ref1mid = _getindex_hiprec(r1, imid)
        ref2mid = _getindex_hiprec(r2, imid)
        ref = ref1mid + ref2mid
    end
    step = twiceprecision(r1.step + r2.step, nbitslen(T, len, imid))
    StepRangeLen{T,typeof(ref),typeof(step)}(ref, step, len, imid)
end

## LinSpace

# For Float16, Float32, and Float64, linspace returns a StepRangeLen
function linspace{T<:Union{Float16,Float32,Float64}}(start::T, stop::T, len::Integer)
    len < 2 && return _linspace1(T, start, stop, len)
    # Attempt to find exact rational approximations
    start_n, start_d = rat(start)
    stop_n, stop_d = rat(stop)
    if start_d != 0 && stop_d != 0
        den = lcm(start_d, stop_d)
        m = maxintfloat(T)
        if den != 0 && abs(den*start) <= m && abs(den*stop) <= m
            start_n = round(Int, den*start)
            stop_n = round(Int, den*stop)
            if T(start_n/den) == start && T(stop_n/den) == stop
                return linspace(T, start_n, stop_n, len, den)
            end
        end
    end
    _linspace(start, stop, len)
end

function _linspace{T<:Union{Float16,Float32,Float64}}(start::T, stop::T, len::Integer)
    (isfinite(start) && isfinite(stop)) || throw(ArgumentError("start and stop must be finite, got $start and $stop"))
    # Find the index that returns the smallest-magnitude element
    Δ, Δfac = stop-start, 1
    if !isfinite(Δ)   # handle overflow for large endpoints
        Δ, Δfac = stop/len - start/len, Int(len)
    end
    tmin = -(start/Δ)/Δfac            # interpolation t such that return value is 0
    imin = round(Int, tmin*(len-1)+1)
    if 1 < imin < len
        # The smallest-magnitude element is in the interior
        t = (imin-1)/(len-1)
        ref = T((1-t)*start + t*stop)
        step = imin-1 < len-imin ? (ref-start)/(imin-1) : (stop-ref)/(len-imin)
    elseif imin <= 1
        imin = 1
        ref = start
        step = (Δ/(len-1))*Δfac
    else
        imin = Int(len)
        ref = stop
        step = (Δ/(len-1))*Δfac
    end
    if len == 2 && !isfinite(step)
        # For very large endpoints where step overflows, exploit the
        # split-representation to handle the overflow
        return StepRangeLen(TwicePrecision(start, zero(T)),
                            TwicePrecision(-start, stop), 2)
    end
    # 2x calculations to get high precision endpoint matching while also
    # preventing overflow in ref_hi+(i-offset)*step_hi
    m, k = prevfloat(realmax(T)), max(imin-1, len-imin)
    step_hi_pre = clamp(step, max(-(m+ref)/k, (-m+ref)/k), min((m-ref)/k, (m+ref)/k))
    nb = nbitslen(T, len, imin)
    step_hi = truncbits(step_hi_pre, nb)
    x1_hi, x1_lo = add2((1-imin)*step_hi, ref)
    x2_hi, x2_lo = add2((len-imin)*step_hi, ref)
    a, b = (start - x1_hi) - x1_lo, (stop - x2_hi) - x2_lo
    step_lo = (b - a)/(len - 1)
    ref_lo = a - (1 - imin)*step_lo
    StepRangeLen(TwicePrecision(ref, ref_lo), TwicePrecision(step_hi, step_lo), Int(len), imin)
end

# linspace for rational numbers, start = start_n/den, stop = stop_n/den
# Note this returns a StepRangeLen
function linspace{T}(::Type{T}, start_n::Integer, stop_n::Integer, len::Integer, den::Integer)
    len < 2 && return _linspace1(T, start_n/den, stop_n/den, len)
    start_n == stop_n && return StepRangeLen(TwicePrecision{T}((start_n, den)), zero(TwicePrecision{T}), len)
    tmin = -start_n/(Float64(stop_n) - Float64(start_n))
    imin = round(Int, tmin*(len-1)+1)
    imin = clamp(imin, 1, Int(len))
    # Compute (1-t)*a and t*b separately in 2x precision (itp = interpolant)...
    dent = (den, len-1)  # represent products as a tuple to eliminate risk of overflow
    start_itp = proddiv(T, (len-imin, start_n), dent)
    stop_itp = proddiv(T, (imin-1, stop_n), dent)
    # ...and then combine them to make ref
    ref = start_itp + stop_itp
    # Compute step to 2x precision without risking overflow...
    rend = proddiv(T, (stop_n,), dent)
    rbeg = proddiv(T, (-start_n,), dent)
    step = twiceprecision(rbeg + rend, nbitslen(T, len, imin)) # ...and truncate hi-bits as needed
    StepRangeLen(ref, step, Int(len), imin)
end

# For len < 2
function _linspace1{T}(::Type{T}, start, stop, len::Integer)
    len >= 0 || throw(ArgumentError("linspace($start, $stop, $len): negative length"))
    if len <= 1
        len == 1 && (start == stop || throw(ArgumentError("linspace($start, $stop, $len): endpoints differ")))
        # Ensure that first(r)==start and last(r)==stop even for len==0
        return StepRangeLen(TwicePrecision(start, zero(T)), TwicePrecision(start, -stop), len, 1)
    end
    throw(ArgumentError("should only be called for len < 2, got $len"))
end

### Numeric utilities

# Approximate x with a rational representation. Guaranteed to return,
# but not guaranteed to return a precise answer.
# https://en.wikipedia.org/wiki/Continued_fraction#Best_rational_approximations
function rat(x)
    y = x
    a = d = 1
    b = c = 0
    m = maxintfloat(narrow(typeof(x)))
    while abs(y) <= m
        f = trunc(Int,y)
        y -= f
        a, c = f*a + c, a
        b, d = f*b + d, b
        max(abs(a), abs(b)) <= convert(Int,m) || return c, d
        oftype(x,a)/oftype(x,b) == x && break
        y = inv(y)
    end
    return a, b
end

narrow(::Type{Float64}) = Float32
narrow(::Type{Float32}) = Float16
narrow(::Type{Float16}) = Float16

function add2{T<:Number}(u::T, v::T)
    @_inline_meta
    u, v = ifelse(abs(v) > abs(u), (v, u), (u, v))
    w = u + v
    w, (u-w) + v
end

add2(u, v) = _add2(promote(u, v)...)
_add2{T<:Number}(u::T, v::T) = add2(u, v)
_add2(u, v) = error("$u::$(typeof(u)) and $v::$(typeof(v)) cannot be promoted to a common type")

function +(x::TwicePrecision, y::Number)
    s_hi, s_lo = add2(x.hi, y)
    TwicePrecision(s_hi, s_lo+x.lo)
end
+(x::Number, y::TwicePrecision) = y+x

function +{T}(x::TwicePrecision{T}, y::TwicePrecision{T})
    r = x.hi + y.hi
    s = abs(x.hi) > abs(y.hi) ? (((x.hi - r) + y.hi) + y.lo) + x.lo : (((y.hi - r) + x.hi) + x.lo) + y.lo
    TwicePrecision(r, s)
end
+(x::TwicePrecision, y::TwicePrecision) = _add2(promote(x, y)...)
_add2{T<:TwicePrecision}(x::T, y::T) = x + y
_add2(x::TwicePrecision, y::TwicePrecision) = TwicePrecision(x.hi+y.hi, x.lo+y.lo)

function *(x::TwicePrecision, v::Integer)
    v == 0 && return TwicePrecision(x.hi*v, x.lo*v)
    nb = ceil(Int, log2(abs(v)))
    u = truncbits(x.hi, nb)
    y_hi, y_lo = add2(u*v, ((x.hi-u) + x.lo)*v)
    TwicePrecision(y_hi, y_lo)
end

function _mul2{T<:Union{Float16,Float32,Float64}}(x::TwicePrecision{T}, v::T)
    v == 0 && return TwicePrecision(T(0), T(0))
    xhh, xhl = splitprec(x.hi)
    vh, vl = splitprec(v)
    y_hi, y_lo = add2(xhh*vh, xhh*vl + xhl*vh)
    TwicePrecision(y_hi, y_lo + xhl*vl + x.lo*v)
end

_mul2(x::TwicePrecision, v::Number) = TwicePrecision(x.hi*v, x.lo*v)

function *{R,S<:Number}(x::TwicePrecision{R}, v::S)
    T = promote_type(R, S)
    _mul2(convert(TwicePrecision{T}, x), convert(T, v))
end

*(v::Number, x::TwicePrecision) = x*v

function /(x::TwicePrecision, v::Number)
    hi = x.hi/v
    w = TwicePrecision(hi, zero(hi)) * v
    lo = (((x.hi - w.hi) - w.lo) + x.lo)/v
    y_hi, y_lo = add2(hi, lo)
    TwicePrecision(y_hi, y_lo)
end

# hi-precision version of prod(num)/prod(den)
# num and den are tuples to avoid risk of overflow
function proddiv(T, num, den)
    @_inline_meta
    t = TwicePrecision(T(num[1]), zero(T))
    t = _prod(t, tail(num)...)
    _divt(t, den...)
end
function _prod(t::TwicePrecision, x, y...)
    @_inline_meta
    _prod(t * x, y...)
end
_prod(t::TwicePrecision) = t
function _divt(t::TwicePrecision, x, y...)
    @_inline_meta
    _divt(t / x, y...)
end
_divt(t::TwicePrecision) = t

isbetween(a, x, b) = a <= x <= b || b <= x <= a

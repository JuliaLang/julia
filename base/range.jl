# This file is a part of Julia. License is MIT: http://julialang.org/license

## 1-dimensional ranges ##

typealias Dims Tuple{Vararg{Int}}

abstract Range{T} <: AbstractArray{T,1}

## ordinal ranges

abstract OrdinalRange{T,S} <: Range{T}

immutable StepRange{T,S} <: OrdinalRange{T,S}
    start::T
    step::S
    stop::T

    function StepRange(start::T, step::S, stop::T)
        new(start, step, steprange_last(start,step,stop))
    end
end

# to make StepRange constructor inlineable, so optimizer can see `step` value
function steprange_last{T}(start::T, step, stop)
    if isa(start,FloatingPoint) || isa(step,FloatingPoint)
        throw(ArgumentError("StepRange should not be used with floating point"))
    end
    z = zero(step)
    step == z && throw(ArgumentError("step cannot be zero"))

    if stop == start
        last = stop
    else
        if (step > z) != (stop > start)
            # empty range has a special representation where stop = start-1
            # this is needed to avoid the wrap-around that can happen computing
            # start - step, which leads to a range that looks very large instead
            # of empty.
            if step > z
                last = start - one(stop-start)
            else
                last = start + one(stop-start)
            end
        else
            diff = stop - start
            if T<:Signed && (diff > zero(diff)) != (stop > start)
                # handle overflowed subtraction with unsigned rem
                if diff > zero(diff)
                    remain = -convert(T, unsigned(-diff) % step)
                else
                    remain = convert(T, unsigned(diff) % step)
                end
            else
                remain = steprem(start,stop,step)
            end
            last = stop - remain
        end
    end
    last
end

steprem(start,stop,step) = (stop-start) % step

StepRange{T,S}(start::T, step::S, stop::T) = StepRange{T,S}(start, step, stop)

immutable UnitRange{T<:Real} <: OrdinalRange{T,Int}
    start::T
    stop::T
    UnitRange(start, stop) = new(start, unitrange_last(start,stop))
end
UnitRange{T<:Real}(start::T, stop::T) = UnitRange{T}(start, stop)

unitrange_last(::Bool, stop::Bool) = stop
unitrange_last{T<:Integer}(start::T, stop::T) =
    ifelse(stop >= start, stop, convert(T,start-one(stop-start)))
unitrange_last{T}(start::T, stop::T) =
    ifelse(stop >= start, convert(T,start+floor(stop-start)),
                          convert(T,start-one(stop-start)))

colon(a::Real, b::Real) = colon(promote(a,b)...)

colon{T<:Real}(start::T, stop::T) = UnitRange{T}(start, stop)

range(a::Real, len::Integer) = UnitRange{typeof(a)}(a, a+len-1)

colon{T}(start::T, stop::T) = StepRange(start, one(stop-start), stop)

range{T}(a::T, len::Integer) =
    StepRange{T, typeof(a-a)}(a, one(a-a), a+oftype(a-a,(len-1)))

# first promote start and stop, leaving step alone
# this is for non-numeric ranges where step can be quite different
colon{A<:Real,C<:Real}(a::A, b, c::C) = colon(convert(promote_type(A,C),a), b, convert(promote_type(A,C),c))

colon{T<:Real}(start::T, step, stop::T) = StepRange(start, step, stop)
colon{T<:Real}(start::T, step::T, stop::T) = StepRange(start, step, stop)
colon{T<:Real}(start::T, step::Real, stop::T) = StepRange(promote(start, step, stop)...)

colon{T}(start::T, step, stop::T) = StepRange(start, step, stop)

range{T,S}(a::T, step::S, len::Integer) = StepRange{T,S}(a, step, convert(T, a+step*(len-1)))

## floating point ranges

immutable FloatRange{T<:FloatingPoint} <: Range{T}
    start::T
    step::T
    len::T
    divisor::T
end
FloatRange(a::FloatingPoint, s::FloatingPoint, l::Real, d::FloatingPoint) =
    FloatRange{promote_type(typeof(a),typeof(s),typeof(d))}(a,s,l,d)

# float rationalization helper
function rat(x)
    y = x
    a = d = 1
    b = c = 0
    m = maxintfloat(Float32)
    while abs(y) <= m
        f = trunc(Int,y)
        y -= f
        a, c = f*a + c, a
        b, d = f*b + d, b
        max(abs(a),abs(b)) <= convert(Int,m) || return c, d
        oftype(x,a)/oftype(x,b) == x && break
        y = inv(y)
    end
    return a, b
end

function colon{T<:FloatingPoint}(start::T, step::T, stop::T)
    step == 0 && throw(ArgumentError("range step cannot be zero"))
    start == stop && return FloatRange{T}(start,step,1,1)
    (0 < step) != (start < stop) && return FloatRange{T}(start,step,0,1)

    # float range "lifting"
    r = (stop-start)/step
    n = round(r)
    lo = prevfloat((prevfloat(stop)-nextfloat(start))/n)
    hi = nextfloat((nextfloat(stop)-prevfloat(start))/n)
    if lo <= step <= hi
        a0, b = rat(start)
        a = convert(T,a0)
        if a/convert(T,b) == start
            c0, d = rat(step)
            c = convert(T,c0)
            if c/convert(T,d) == step
                e = lcm(b,d)
                a *= div(e,b)
                c *= div(e,d)
                eT = convert(T,e)
                if (a+n*c)/eT == stop
                    return FloatRange{T}(a, c, n+1, eT)
                end
            end
        end
    end
    FloatRange{T}(start, step, floor(r)+1, one(step))
end

colon{T<:FloatingPoint}(a::T, b::T) = colon(a, one(a), b)

colon{T<:Real}(a::T, b::FloatingPoint, c::T) = colon(promote(a,b,c)...)
colon{T<:FloatingPoint}(a::T, b::FloatingPoint, c::T) = colon(promote(a,b,c)...)
colon{T<:FloatingPoint}(a::T, b::Real, c::T) = colon(promote(a,b,c)...)

range(a::FloatingPoint, len::Integer) = FloatRange(a,one(a),len,one(a))
range(a::FloatingPoint, st::FloatingPoint, len::Integer) = FloatRange(a,st,len,one(a))
range(a::Real, st::FloatingPoint, len::Integer) = FloatRange(float(a), st, len, one(st))
range(a::FloatingPoint, st::Real, len::Integer) = FloatRange(a, float(st), len, one(a))

## linspace and logspace

immutable LinSpace{T<:FloatingPoint} <: Range{T}
    start::T
    stop::T
    len::T
    divisor::T
end

function linspace{T<:FloatingPoint}(start::T, stop::T, len::T)
    len == round(len) || throw(InexactError())
    0 <= len || error("linspace($start, $stop, $len): negative length")
    if len == 0
        n = convert(T, 2)
        if isinf(n*start) || isinf(n*stop)
            start /= n; stop /= n; n = one(T)
        end
        return LinSpace(-start, -stop, -one(T), n)
    end
    if len == 1
        start == stop || error("linspace($start, $stop, $len): endpoints differ")
        return LinSpace(-start, -start, zero(T), one(T))
    end
    n = convert(T, len - 1)
    len - n == 1 || error("linspace($start, $stop, $len): too long for $T")
    a0, b = rat(start)
    a = convert(T,a0)
    if a/convert(T,b) == start
        c0, d = rat(stop)
        c = convert(T,c0)
        if c/convert(T,d) == stop
            e = lcm(b,d)
            a *= div(e,b)
            c *= div(e,d)
            s = convert(T,n*e)
            if isinf(a*n) || isinf(c*n)
                s, p = frexp(s)
                p = oftype(s,2)^p
                a /= p; c /= p
            end
            if a*n/s == start && c*n/s == stop
                return LinSpace(a, c, len, s)
            end
        end
    end
    a, c, s = start, stop, n
    if isinf(a*n) || isinf(c*n)
        s, p = frexp(s)
        p = oftype(s,2)^p
        a /= p; c /= p
    end
    if a*n/s == start && c*n/s == stop
        return LinSpace(a, c, len, s)
    end
    return LinSpace(start, stop, len, n)
end
function linspace{T<:FloatingPoint}(start::T, stop::T, len::Real)
    T_len = convert(T, len)
    T_len == len || throw(InexactError())
    linspace(start, stop, T_len)
end
linspace(start::Real, stop::Real, len::Real=50) =
    linspace(promote(FloatingPoint(start), FloatingPoint(stop))..., len)

function show(io::IO, r::LinSpace)
    print(io, "linspace(")
    show(io, first(r))
    print(io, ',')
    show(io, last(r))
    print(io, ',')
    show(io, length(r))
    print(io, ')')
end

logspace(start::Real, stop::Real, n::Integer=50) = 10.^linspace(start, stop, n)

## interface implementations

similar(r::Range, T::Type, dims::Tuple{Vararg{Integer}}) = Array(T, dims...)
similar(r::Range, T::Type, dims::Dims) = Array(T, dims)

size(r::Range) = (length(r),)

isempty(r::StepRange) =
    (r.start != r.stop) & ((r.step > zero(r.step)) != (r.stop > r.start))
isempty(r::UnitRange) = r.start > r.stop
isempty(r::FloatRange) = length(r) == 0
isempty(r::LinSpace) = length(r) == 0

step(r::StepRange) = r.step
step(r::UnitRange) = 1
step(r::FloatRange) = r.step/r.divisor
step{T}(r::LinSpace{T}) = ifelse(r.len <= 0, convert(T,NaN), (r.stop-r.start)/r.divisor)

function length(r::StepRange)
    n = Integer(div(r.stop+r.step - r.start, r.step))
    isempty(r) ? zero(n) : n
end
length(r::UnitRange) = Integer(r.stop - r.start + 1)
length(r::FloatRange) = Integer(r.len)
length(r::LinSpace) = Integer(r.len + signbit(r.len - 1))

function length{T<:Union(Int,UInt,Int64,UInt64)}(r::StepRange{T})
    isempty(r) && return zero(T)
    if r.step > 1
        return checked_add(convert(T, div(unsigned(r.stop - r.start), r.step)), one(T))
    elseif r.step < -1
        return checked_add(convert(T, div(unsigned(r.start - r.stop), -r.step)), one(T))
    else
        checked_add(div(checked_sub(r.stop, r.start), r.step), one(T))
    end
end

length{T<:Union(Int,UInt,Int64,UInt64)}(r::UnitRange{T}) =
    checked_add(checked_sub(r.stop, r.start), one(T))

# some special cases to favor default Int type
let smallint = (Int === Int64 ?
                Union(Int8,UInt8,Int16,UInt16,Int32,UInt32) :
                Union(Int8,UInt8,Int16,UInt16))
    global length

    function length{T <: smallint}(r::StepRange{T})
        isempty(r) && return Int(0)
        div(Int(r.stop)+Int(r.step) - Int(r.start), Int(r.step))
    end

    length{T <: smallint}(r::UnitRange{T}) = Int(r.stop) - Int(r.start) + 1
end

first{T}(r::OrdinalRange{T}) = convert(T, r.start)
first{T}(r::FloatRange{T}) = convert(T, r.start/r.divisor)
first{T}(r::LinSpace{T}) = convert(T, (r.len-1)*r.start/r.divisor)

last{T}(r::StepRange{T}) = r.stop
last(r::UnitRange) = r.stop
last{T}(r::FloatRange{T}) = convert(T, (r.start + (r.len-1)*r.step)/r.divisor)
last{T}(r::LinSpace{T}) = convert(T, (r.len-1)*r.stop/r.divisor)

minimum(r::UnitRange) = isempty(r) ? throw(ArgumentError("range must be non-empty")) : first(r)
maximum(r::UnitRange) = isempty(r) ? throw(ArgumentError("range must be non-empty")) : last(r)
minimum(r::Range)  = isempty(r) ? throw(ArgumentError("range must be non-empty")) : min(first(r), last(r))
maximum(r::Range)  = isempty(r) ? throw(ArgumentError("range must be non-empty")) : max(first(r), last(r))

ctranspose(r::Range) = [x for _=1, x=r]
transpose(r::Range) = r'

# Ranges are immutable
copy(r::Range) = r


## iteration

start(r::FloatRange) = 0
done(r::FloatRange, i::Int) = length(r) <= i
next{T}(r::FloatRange{T}, i::Int) =
    (convert(T, (r.start + i*r.step)/r.divisor), i+1)

start(r::LinSpace) = 1
done(r::LinSpace, i::Int) = length(r) < i
next{T}(r::LinSpace{T}, i::Int) =
    (convert(T, ((r.len-i)*r.start + (i-1)*r.stop)/r.divisor), i+1)

# NOTE: For ordinal ranges, we assume start+step might be from a
# lifted domain (e.g. Int8+Int8 => Int); use that for iterating.
start(r::StepRange) = convert(typeof(r.start+r.step), r.start)
next{T}(r::StepRange{T}, i) = (convert(T,i), i+r.step)
done{T,S}(r::StepRange{T,S}, i) = isempty(r) | (i < min(r.start, r.stop)) | (i > max(r.start, r.stop))
done{T,S}(r::StepRange{T,S}, i::Integer) = isempty(r) | (i == r.stop+r.step)

start(r::UnitRange) = oftype(r.start+1, r.start)
next{T}(r::UnitRange{T}, i) = (convert(T,i), i+1)
done(r::UnitRange, i) = i==oftype(i,r.stop)+1


## indexing

getindex(r::Range, i::Real) = getindex(r, to_index(i))

function getindex{T}(r::Range{T}, i::Integer)
    1 <= i <= length(r) || throw(BoundsError())
    convert(T, first(r) + (i-1)*step(r))
end
function getindex{T}(r::FloatRange{T}, i::Integer)
    1 <= i <= length(r) || throw(BoundsError())
    convert(T, (r.start + (i-1)*r.step)/r.divisor)
end
function getindex{T}(r::LinSpace{T}, i::Integer)
    1 <= i <= length(r) || throw(BoundsError())
    convert(T, ((r.len-i)*r.start + (i-1)*r.stop)/r.divisor)
end

function check_indexingrange(s, r)
    sl = length(s)
    rl = length(r)
    sl == 0 || 1 <= first(s) <= rl &&
               1 <=  last(s) <= rl || throw(BoundsError())
    sl
end

function getindex(r::UnitRange, s::UnitRange{Int})
    sl = check_indexingrange(s, r)
    st = oftype(r.start, r.start + s.start-1)
    range(st, sl)
end

function getindex(r::UnitRange, s::StepRange{Int})
    sl = check_indexingrange(s, r)
    st = oftype(r.start, r.start + s.start-1)
    range(st, step(s), sl)
end

function getindex(r::StepRange, s::Range{Int})
    sl = check_indexingrange(s, r)
    st = oftype(r.start, r.start + (first(s)-1)*step(r))
    range(st, step(r)*step(s), sl)
end

function getindex(r::FloatRange, s::OrdinalRange)
    sl = check_indexingrange(s, r)
    FloatRange(r.start + (first(s)-1)*r.step, step(s)*r.step, sl, r.divisor)
end

function getindex{T}(r::LinSpace{T}, s::OrdinalRange)
    sl::T = check_indexingrange(s, r)
    ifirst = first(s)
    ilast = last(s)
    vfirst::T = ((r.len - ifirst) * r.start + (ifirst - 1) * r.stop) / r.divisor
    vlast::T = ((r.len - ilast) * r.start + (ilast - 1) * r.stop) / r.divisor
    return linspace(vfirst, vlast, sl)
end

function show(io::IO, r::Range)
    print(io, repr(first(r)), ':', repr(step(r)), ':', repr(last(r)))
end
show(io::IO, r::UnitRange) = print(io, repr(first(r)), ':', repr(last(r)))

=={T<:Range}(r::T, s::T) = (first(r) == first(s)) & (step(r) == step(s)) & (last(r) == last(s))
==(r::OrdinalRange, s::OrdinalRange) = (first(r) == first(s)) & (step(r) == step(s)) & (last(r) == last(s))

function ==(r::Range, s::Range)
    lr = length(r)
    if lr != length(s)
        return false
    end
    u, v = start(r), start(s)
    while !done(r, u)
        x, u = next(r, u)
        y, v = next(s, v)
        if x != y
            return false
        end
    end
    return true
end

intersect{T1<:Integer, T2<:Integer}(r::UnitRange{T1}, s::UnitRange{T2}) = max(r.start,s.start):min(last(r),last(s))

intersect{T<:Integer}(i::Integer, r::UnitRange{T}) =
    i < first(r) ? (first(r):i) :
    i > last(r)  ? (i:last(r))  : (i:i)

intersect{T<:Integer}(r::UnitRange{T}, i::Integer) = intersect(i, r)

function intersect{T1<:Integer, T2<:Integer}(r::UnitRange{T1}, s::StepRange{T2})
    if isempty(s)
        range(first(r), 0)
    elseif step(s) == 0
        intersect(first(s), r)
    elseif step(s) < 0
        intersect(r, reverse(s))
    else
        sta = first(s)
        ste = step(s)
        sto = last(s)
        lo = first(r)
        hi = last(r)
        i0 = max(sta, lo + mod(sta - lo, ste))
        i1 = min(sto, hi - mod(hi - sta, ste))
        i0:ste:i1
    end
end

function intersect{T1<:Integer, T2<:Integer}(r::StepRange{T1}, s::UnitRange{T2})
    if step(r) < 0
        reverse(intersect(s, reverse(r)))
    else
        intersect(s, r)
    end
end

function intersect(r::StepRange, s::StepRange)
    if isempty(r) || isempty(s)
        return range(first(r), step(r), 0)
    elseif step(s) < 0
        return intersect(r, reverse(s))
    elseif step(r) < 0
        return reverse(intersect(reverse(r), s))
    end

    start1 = first(r)
    step1 = step(r)
    stop1 = last(r)
    start2 = first(s)
    step2 = step(s)
    stop2 = last(s)
    a = lcm(step1, step2)

    # if a == 0
    #     # One or both ranges have step 0.
    #     if step1 == 0 && step2 == 0
    #         return start1 == start2 ? r : Range(start1, 0, 0)
    #     elseif step1 == 0
    #         return start2 <= start1 <= stop2 && rem(start1 - start2, step2) == 0 ? r : Range(start1, 0, 0)
    #     else
    #         return start1 <= start2 <= stop1 && rem(start2 - start1, step1) == 0 ? (start2:step1:start2) : Range(start1, step1, 0)
    #     end
    # end

    g, x, y = gcdx(step1, step2)

    if rem(start1 - start2, g) != 0
        # Unaligned, no overlap possible.
        return range(start1, a, 0)
    end

    z = div(start1 - start2, g)
    b = start1 - x * z * step1
    # Possible points of the intersection of r and s are
    # ..., b-2a, b-a, b, b+a, b+2a, ...
    # Determine where in the sequence to start and stop.
    m = max(start1 + mod(b - start1, a), start2 + mod(b - start2, a))
    n = min(stop1 - mod(stop1 - b, a), stop2 - mod(stop2 - b, a))
    m:a:n
end

function intersect(r1::Range, r2::Range, r3::Range, r::Range...)
    i = intersect(intersect(r1, r2), r3)
    for t in r
        i = intersect(i, t)
    end
    i
end

# findin (the index of intersection)
function _findin{T1<:Integer, T2<:Integer}(r::Range{T1}, span::UnitRange{T2})
    local ifirst
    local ilast
    fspan = first(span)
    lspan = last(span)
    fr = first(r)
    lr = last(r)
    sr = step(r)
    if sr > 0
        ifirst = fr >= fspan ? 1 : ceil(Integer,(fspan-fr)/sr)+1
        ilast = lr <= lspan ? length(r) : length(r) - ceil(Integer,(lr-lspan)/sr)
    elseif sr < 0
        ifirst = fr <= lspan ? 1 : ceil(Integer,(lspan-fr)/sr)+1
        ilast = lr >= fspan ? length(r) : length(r) - ceil(Integer,(lr-fspan)/sr)
    else
        ifirst = fr >= fspan ? 1 : length(r)+1
        ilast = fr <= lspan ? length(r) : 0
    end
    ifirst, ilast
end

function findin{T1<:Integer, T2<:Integer}(r::UnitRange{T1}, span::UnitRange{T2})
    ifirst, ilast = _findin(r, span)
    ifirst:ilast
end

function findin{T1<:Integer, T2<:Integer}(r::Range{T1}, span::UnitRange{T2})
    ifirst, ilast = _findin(r, span)
    ifirst:1:ilast
end

## linear operations on ranges ##

-(r::OrdinalRange) = range(-r.start, -step(r), length(r))
-(r::FloatRange)   = FloatRange(-r.start, -r.step, r.len, r.divisor)
-(r::LinSpace)     = LinSpace(-r.start, -r.stop, r.len, r.divisor)

.+(x::Real, r::UnitRange)  = range(x + r.start, length(r))
.+(x::Real, r::Range) = (x+first(r)):step(r):(x+last(r))
#.+(x::Real, r::StepRange)  = range(x + r.start, r.step, length(r))
.+(x::Real, r::FloatRange) = FloatRange(r.divisor*x + r.start, r.step, r.len, r.divisor)
.+(x::Real, r::LinSpace)   = LinSpace(x + r.start, x + r.stop, r.len, r.divisor)
.+(r::Range, x::Real)      = x + r
#.+(r::FloatRange, x::Real) = x + r

.-(x::Real, r::Range)      = (x-first(r)):-step(r):(x-last(r))
.-(x::Real, r::FloatRange) = FloatRange(r.divisor*x - r.start, -r.step, r.len, r.divisor)
.-(x::Real, r::LinSpace)   = LinSpace(x - r.start, x - r.stop, r.len, r.divisor)
.-(r::UnitRange, x::Real)  = range(r.start-x, length(r))
.-(r::StepRange , x::Real) = range(r.start-x, r.step, length(r))
.-(r::FloatRange, x::Real) = FloatRange(r.start - r.divisor*x, r.step, r.len, r.divisor)
.-(r::LinSpace, x::Real)   = LinSpace(r.start - x, r.stop - x, r.len, r.divisor)

.*(x::Real, r::OrdinalRange) = range(x*r.start, x*step(r), length(r))
.*(x::Real, r::FloatRange)   = FloatRange(x*r.start, x*r.step, r.len, r.divisor)
.*(x::Real, r::LinSpace)     = LinSpace(x * r.start, x * r.stop, r.len, r.divisor)
.*(r::Range, x::Real)        = x .* r
.*(r::FloatRange, x::Real)   = x .* r
.*(r::LinSpace, x::Real)     = x .* r

./(r::OrdinalRange, x::Real) = range(r.start/x, step(r)/x, length(r))
./(r::FloatRange, x::Real)   = FloatRange(r.start/x, r.step/x, r.len, r.divisor)
./(r::LinSpace, x::Real)     = LinSpace(r.start / x, r.stop / x, r.len, r.divisor)

promote_rule{T1,T2}(::Type{UnitRange{T1}},::Type{UnitRange{T2}}) =
    UnitRange{promote_type(T1,T2)}
convert{T}(::Type{UnitRange{T}}, r::UnitRange{T}) = r
convert{T}(::Type{UnitRange{T}}, r::UnitRange) = UnitRange{T}(r.start, r.stop)

promote_rule{T1a,T1b,T2a,T2b}(::Type{StepRange{T1a,T1b}},::Type{StepRange{T2a,T2b}}) =
    StepRange{promote_type(T1a,T2a),promote_type(T1b,T2b)}
convert{T1,T2}(::Type{StepRange{T1,T2}}, r::StepRange{T1,T2}) = r

promote_rule{T1a,T1b,T2}(::Type{StepRange{T1a,T1b}},::Type{UnitRange{T2}}) =
    StepRange{promote_type(T1a,T2),promote_type(T1b,T2)}
convert{T1,T2}(::Type{StepRange{T1,T2}}, r::Range) =
    StepRange{T1,T2}(convert(T1, first(r)), convert(T2, step(r)), convert(T1, last(r)))
convert{T}(::Type{StepRange}, r::UnitRange{T}) =
    StepRange{T,T}(first(r), step(r), last(r))

promote_rule{T1,T2}(::Type{FloatRange{T1}},::Type{FloatRange{T2}}) =
    FloatRange{promote_type(T1,T2)}
convert{T}(::Type{FloatRange{T}}, r::FloatRange{T}) = r
convert{T}(::Type{FloatRange{T}}, r::FloatRange) =
    FloatRange{T}(r.start,r.step,r.len,r.divisor)

promote_rule{F,OR<:OrdinalRange}(::Type{FloatRange{F}}, ::Type{OR}) =
    FloatRange{promote_type(F,eltype(OR))}
convert{T}(::Type{FloatRange{T}}, r::OrdinalRange) =
    FloatRange{T}(first(r), step(r), length(r), one(T))
convert{T}(::Type{FloatRange}, r::OrdinalRange{T}) =
    FloatRange{typeof(float(first(r)))}(first(r), step(r), length(r), one(T))

promote_rule{T1,T2}(::Type{LinSpace{T1}},::Type{LinSpace{T2}}) =
    LinSpace{promote_type(T1,T2)}
convert{T}(::Type{LinSpace{T}}, r::LinSpace{T}) = r
convert{T}(::Type{LinSpace{T}}, r::LinSpace) =
    LinSpace{T}(r.start, r.stop, r.len, r.divisor)

promote_rule{F,OR<:OrdinalRange}(::Type{LinSpace{F}}, ::Type{OR}) =
    LinSpace{promote_type(F,eltype(OR))}
convert{T}(::Type{LinSpace{T}}, r::OrdinalRange) =
    linspace(convert(T, first(r)), convert(T, last(r)), convert(T, length(r)))
convert{T}(::Type{LinSpace}, r::OrdinalRange{T}) =
    convert(LinSpace{typeof(float(first(r)))}, r)


# +/- of ranges is defined in operators.jl (to be able to use @eval etc.)

## non-linear operations on ranges and fallbacks for non-real numbers ##

.+(x::Number, r::Range) = [ x+y for y=r ]
.+(r::Range, y::Number) = [ x+y for x=r ]

.-(x::Number, r::Range) = [ x-y for y=r ]
.-(r::Range, y::Number) = [ x-y for x=r ]

.*(x::Number, r::Range) = [ x*y for y=r ]
.*(r::Range, y::Number) = [ x*y for x=r ]

./(x::Number, r::Range) = [ x/y for y=r ]
./(r::Range, y::Number) = [ x/y for x=r ]

.^(x::Number, r::Range) = [ x^y for y=r ]
.^(r::Range, y::Number) = [ x^y for x=r ]

## concatenation ##

function vcat{T}(rs::Range{T}...)
    n::Int = 0
    for ra in rs
        n += length(ra)
    end
    a = Array(T,n)
    i = 1
    for ra in rs, x in ra
        @inbounds a[i] = x
        i += 1
    end
    return a
end

convert{T}(::Type{Array{T,1}}, r::Range{T}) = vcat(r)
collect(r::Range) = vcat(r)

reverse(r::OrdinalRange) = colon(last(r), -step(r), first(r))
reverse(r::FloatRange)   = FloatRange(r.start + (r.len-1)*r.step, -r.step, r.len, r.divisor)
reverse(r::LinSpace)     = LinSpace(r.stop, r.start, r.len, r.divisor)

## sorting ##

issorted(r::UnitRange) = true
issorted(r::Range) = step(r) >= zero(step(r))

sort(r::UnitRange) = r
sort!(r::UnitRange) = r

sort(r::Range) = issorted(r) ? r : reverse(r)

sortperm(r::UnitRange) = 1:length(r)
sortperm(r::Range) = issorted(r) ? (1:1:length(r)) : (length(r):-1:1)

function sum{T<:Real}(r::Range{T})
    l = length(r)
    # note that a little care is required to avoid overflow in l*(l-1)/2
    return l * first(r) + (iseven(l) ? (step(r) * (l-1)) * (l>>1)
                                     : (step(r) * l) * ((l-1)>>1))
end

function sum(r::FloatRange)
    l = length(r)
    if iseven(l)
        s = r.step * (l-1) * (l>>1)
    else
        s = (r.step * l) * ((l-1)>>1)
    end
    return (l * r.start + s)/r.divisor
end


function mean{T<:Real}(r::Range{T})
    isempty(r) && throw(ArgumentError("mean of an empty range is undefined"))
    (first(r) + last(r)) / 2
end

median{T<:Real}(r::Range{T}) = mean(r)

function in(x, r::Range)
    n = step(r) == 0 ? 1 : round(Integer,(x-first(r))/step(r))+1
    n >= 1 && n <= length(r) && r[n] == x
end

in{T<:Integer}(x, r::Range{T}) = isinteger(x) && !isempty(r) && x>=minimum(r) && x<=maximum(r) && (mod(convert(T,x),step(r))-mod(first(r),step(r)) == 0)
in(x::Char, r::Range{Char}) = !isempty(r) && x >= minimum(r) && x <= maximum(r) && (mod(Int(x) - Int(first(r)), step(r)) == 0)

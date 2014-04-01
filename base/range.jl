## 1-dimensional ranges ##

typealias Dims (Int...)

abstract Range{T} <: AbstractArray{T,1}

## ordinal ranges

abstract OrdinalRange{T,S} <: Range{T}

# A StepRange has a start point, and moves by some step up through a last point.
# The element type is T, but we allow T+S to give a value from a lifted
# domain D instead of from T. We store a sentinel value stop+step from domain D,
# since T might not have any available sentinel value.
immutable StepRange{T,S,D} <: OrdinalRange{T,S}
    start::D
    step::S
    sentinel::D

    function StepRange(start, step::S, stop)
        if D<:FloatingPoint || S<:FloatingPoint
            error("StepRange should not be used with floating point")
        end
        step == 0 && error("step cannot be zero")
        step != step && error("step cannot be NaN")

        start = convert(D, start)
        stop = convert(D, stop)

        if (step>0 && stop<start) || (step<0 && stop>start)
            sentinel = start
        else
            remain = (stop - start) % step  # should be robust to overflow
            sentinel = stop + (step - remain)
        end

        new(start, step, sentinel)
    end
end

StepRange{T,S}(start::T, step::S, stop::T) =
    StepRange{T, S, typeof(start+step)}(start, step, stop)

immutable UnitRange{T<:Real,D} <: OrdinalRange{T,Int}
    start::D
    sentinel::D

    UnitRange(start, stop) = new(start, ifelse(stop >= start, stop+1, convert(D,start)))
end
UnitRange{T<:Real}(start::T, stop::T) = UnitRange{T, typeof(start+1)}(start, stop)

# deprecated
export Range1
const Range1 = UnitRange

colon(a, b) = colon(promote(a,b)...)

colon{T<:Real}(start::T, stop::T) = UnitRange(start, stop)
range(a::Real, len::Integer) = UnitRange{typeof(a), typeof(a+len-1)}(a, a+len-1)

colon{T}(start::T, stop::T) = StepRange(start, one(stop-start), stop)
range{T}(a::T, len::Integer) =
    StepRange{T, typeof(a-a), typeof(a+one(a-a))}(a, one(a-a), a+oftype(a-a,(len-1)))

# first promote start and stop, leaving step alone
# this is for non-numeric ranges where step can be quite different
colon{A,C}(a::A, b, c::C) = colon(convert(promote_type(A,C),a), b, convert(promote_type(A,C),c))

colon{T}(start::T, step, stop::T) = StepRange(start, step, stop)

range{T,S}(a::T, step::S, len::Integer) =
    StepRange{T, S, typeof(a+one(S))}(a, step, a+step*(len-1))

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
        f = itrunc(y)
        y -= f
        a, c = f*a + c, a
        b, d = f*b + d, b
        max(abs(a),abs(b)) <= convert(Int,m) || return c, d
        oftype(x,a)/oftype(x,b) == x && break
        y = inv(y)
    end
    return a, b
end

# float range "lifting" helper
function frange{T<:FloatingPoint}(start::T, step::T, stop::T)
    r = (stop-start)/step
    n = round(r)
    lo = prevfloat((prevfloat(stop)-nextfloat(start))/n)
    hi = nextfloat((nextfloat(stop)-prevfloat(start))/n)
    if lo <= step <= hi
        a, b = rat(start)
        a = convert(T,a)
        if a/convert(T,b) == start
            c, d = rat(step)
            c = convert(T,c)
            if c/convert(T,d) == step
                e = lcm(b,d)
                a *= div(e,b)
                c *= div(e,d)
                e = convert(T,e)
                if (a+n*c)/e == stop
                    return a, c, n+1, e
                end
            end
        end
    end
    start, step, floor(r)+1, one(step)
end

colon{T<:FloatingPoint}(a::T, b::T) = colon(a, one(a), b)

colon{T<:Real}(a::T, b::FloatingPoint, c::T) = colon(promote(a,b,c)...)
colon{T<:FloatingPoint}(a::T, b::FloatingPoint, c::T) = colon(promote(a,b,c)...)
colon{T<:FloatingPoint}(a::T, b::Real, c::T) = colon(promote(a,b,c)...)

colon{T<:FloatingPoint}(start::T, step::T, stop::T) =
          step == 0              ? error("range step cannot be zero") :
         start == stop           ? FloatRange{T}(start,step,1,1) :
    (0 < step) != (start < stop) ? FloatRange{T}(start,step,0,1) :
                                   FloatRange{T}(frange(start,step,stop)...)

range(a::FloatingPoint, len::Integer) = colon(a, oftype(a,a+len-1))
range(a::FloatingPoint, st::FloatingPoint, len::Integer) = colon(a, st, a+oftype(st,(len-1)*st))
range(a::Real, st::FloatingPoint, len::Integer) = colon(a, st, a+oftype(st,(len-1)*st))
range(a::FloatingPoint, st::Real, len::Integer) = colon(a, st, oftype(a,a+(len-1)*st))

## interface implementations

similar(r::Range, T::Type, dims::Dims) = Array(T, dims)

size(r::Range) = (length(r),)

isempty(r::FloatRange) = length(r)==0
isempty(r::OrdinalRange) = r.start == r.sentinel

step(r::StepRange) = r.step
step(r::UnitRange) = one(r.sentinel - r.start)
step(r::FloatRange) = r.step/r.divisor

length(r::StepRange) = integer(div(r.sentinel - r.start, r.step))
length(r::UnitRange) = integer(r.sentinel - r.start)
length(r::FloatRange) = integer(r.len)

length{T<:Union(Int,Uint)}(r::StepRange{T}) =
    checked_add(div(checked_sub(r.sentinel-r.step, r.start), r.step), one(T))
length{T<:Union(Int,Uint)}(r::UnitRange{T}) =
    checked_add(checked_sub(r.sentinel-one(T), r.start), one(T))

first{T}(r::OrdinalRange{T}) = oftype(T, r.start)
first(r::FloatRange) = r.start/r.divisor

last{T}(r::OrdinalRange{T}) = oftype(T, r.sentinel-step(r))
last{T}(r::FloatRange{T}) = oftype(T, (r.start + (r.len-1)*r.step)/r.divisor)

minimum(r::UnitRange) = isempty(r) ? error("range must be non-empty") : first(r)
maximum(r::UnitRange) = isempty(r) ? error("range must be non-empty") : last(r)
minimum(r::Range)  = isempty(r) ? error("range must be non-empty") : min(first(r), last(r))
maximum(r::Range)  = isempty(r) ? error("range must be non-empty") : max(first(r), last(r))

ctranspose(r::Range) = [x for _=1, x=r]
transpose(r::Range) = r'

# Ranges are immutable
copy(r::Range) = r


## iteration

start(r::FloatRange) = 0
next{T}(r::FloatRange{T}, i) = (oftype(T, (r.start + i*r.step)/r.divisor), i+1)
done(r::FloatRange, i) = (length(r) <= i)

start(r::OrdinalRange) = r.start
next{T}(r::OrdinalRange{T}, i) = (oftype(T,i), i+step(r))
done(r::OrdinalRange, i) = i==r.sentinel

next{T,D}(r::UnitRange{T,D}, i) = (oftype(T,i), oftype(D, i+1))


## indexing

getindex(r::Range, i::Real) = getindex(r, to_index(i))

function getindex{T}(r::Range{T}, i::Integer)
    1 <= i <= length(r) || error(BoundsError)
    oftype(T, first(r) + (i-1)*step(r))
end
function getindex{T}(r::FloatRange{T}, i::Integer)
    1 <= i <= length(r) || error(BoundsError)
    oftype(T, (r.start + (i-1)*r.step)/r.divisor)
end

function getindex(r::UnitRange, s::UnitRange{Int})
    sl = length(s)
    if sl > 0
        if !(1 <= last(s) <= length(r))
            throw(BoundsError())
        end
        st = r[s.start]
    else
        st = oftype(r.start, r.start + s.start-1)
    end
    range(st, sl)
end

function getindex(r::StepRange, s::Range{Int})
    sl = length(s)
    if sl > 0
        if !(1 <= last(s) <= length(r))
            throw(BoundsError())
        end
        st = r[first(s)]
    else
        st = oftype(r.start, r.start + (first(s)-1)*step(r))
    end
    range(st, step(r)*step(s), sl)
end

getindex(r::FloatRange, s::UnitRange) = r[first(s)]:step(r):r[last(s)]

function show(io::IO, r::Range)
    step(r) == 0 ? invoke(show,(IO,Any),io,r) :
    print(io, repr(first(r)), ':', repr(step(r)), ':', repr(last(r)))
end
show(io::IO, r::UnitRange) = print(io, repr(first(r)), ':', repr(last(r)))

isequal{T<:Range}(r::T, s::T) =
    (first(r)==first(s)) & (step(r)==step(s)) & (last(r)==last(s))

isequal(r::Range, s::Range) = false

=={T<:Range}(r::T, s::T) = isequal(r, s)

=={T<:Integer, S<:Integer}(r::Range{T}, s::Range{S}) =
    (first(r)==first(s)) & (step(r)==step(s)) & (last(r)==last(s))

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

# hashing ranges by component at worst leads to collisions for very similar ranges
hash(r::Range) =
    bitmix(hash(first(r)), bitmix(hash(step(r)), bitmix(hash(last(r)), uint(0xaaeeaaee))))

# TODO: isless?

intersect{T1<:Integer, T2<:Integer}(r::UnitRange{T1}, s::UnitRange{T2}) = max(r.start,s.start):min(last(r),last(s))

intersect{T<:Integer}(i::Integer, r::UnitRange{T}) =
    i < first(r) ? (first(r):i) :
    i > last(r)  ? (i:last(r))  : (i:i)

intersect{T<:Integer}(r::UnitRange{T}, i::Integer) = intersect(i, r)

function intersect{T1<:Integer, T2<:Integer}(r::UnitRange{T1}, s::StepRange{T2})
    if length(s) == 0
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

function intersect{T1<:Integer, T2<:Integer}(r::StepRange{T1}, s::StepRange{T2})
    if length(r) == 0 || length(s) == 0
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

function intersect(r::Range, s::Range...)
    i = r
    for t in s
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
        ifirst = fr >= fspan ? 1 : iceil((fspan-fr)/sr)+1
        ilast = lr <= lspan ? length(r) : length(r) - iceil((lr-lspan)/sr)
    elseif sr < 0
        ifirst = fr <= lspan ? 1 : iceil((lspan-fr)/sr)+1
        ilast = lr >= fspan ? length(r) : length(r) - iceil((lr-fspan)/sr)
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

+(x::Integer, r::UnitRange)  = range(x + r.start, length(r))
+(x::Real, r::Range) = (x+first(r)):step(r):(x+last(r))
#+(x::Real, r::StepRange)  = range(x + r.start, r.step, length(r))
+(x::Real, r::FloatRange) = FloatRange(r.divisor*x + r.start, r.step, r.len, r.divisor)
+(r::Range, x::Real)      = x + r
#+(r::FloatRange, x::Real) = x + r

-(x::Real, r::Range)      = (x-first(r)):-step(r):(x-last(r))
-(x::Real, r::FloatRange) = FloatRange(r.divisor*x - r.start, -r.step, r.len, r.divisor)
-(r::UnitRange, x::Integer)  = range(r.start-x, length(r))
-(r::StepRange , x::Real) = range(r.start-x, r.step, length(r))
-(r::FloatRange, x::Real) = FloatRange(r.start - r.divisor*x, r.step, r.len, r.divisor)

.*(x::Real, r::OrdinalRange) = range(x*r.start, x*step(r), length(r))
.*(x::Real, r::FloatRange)   = FloatRange(x*r.start, x*r.step, r.len, r.divisor)
.*(r::Range, x::Real)        = x .* r
.*(r::FloatRange, x::Real)   = x .* r

./(r::OrdinalRange, x::Real) = range(r.start/x, step(r)/x, length(r))
./(r::FloatRange, x::Real)   = FloatRange(r.start/x, r.step/x, r.len, r.divisor)

# TODO: better implementations for FloatRanges?
function +(r1::OrdinalRange, r2::OrdinalRange)
    r1l = length(r1)
    r1l == length(r2) || error("argument dimensions must match")
    range(r1.start+r2.start, step(r1)+step(r2), r1l)
end

function -(r1::OrdinalRange, r2::OrdinalRange)
    r1l = length(r1)
    r1l == length(r2) || error("argument dimensions must match")
    range(r1.start-r2.start, step(r1)-step(r2), r1l)
end

## non-linear operations on ranges ##

./(x::Number, r::Range) = [ x/y for y=r ]
./(r::Range, y::Number) = [ x/y for x=r ]

.^(x::Number, r::Range) = [ x^y for y=r ]
.^(r::Range, y::Number) = [ x^y for x=r ]

## concatenation ##

function vcat{T}(r::Range{T})
    n = length(r)
    a = Array(T,n)
    i = 1
    for x in r
        @inbounds a[i] = x
        i += 1
    end
    return a
end

convert{T}(::Type{Array{T,1}}, r::Range{T}) = vcat(r)

function vcat{T}(rs::Range{T}...)
    n = sum(length,rs)::Int
    a = Array(T,n)
    i = 1
    for r in rs
        for x in r
            @inbounds a[i] = x
            i += 1
        end
    end
    return a
end

reverse(r::OrdinalRange) = range(last(r), -step(r), length(r))
reverse(r::FloatRange)   = FloatRange(last(r), -r.step, r.len, r.divisor)

## sorting ##

issorted(r::UnitRange) = true
issorted(r::Range) = step(r) >= 0

sort(r::UnitRange) = r
sort!(r::UnitRange) = r

sort{T<:Real}(r::Range{T}) = issorted(r) ? r : reverse(r)

sortperm(r::UnitRange) = 1:length(r)
sortperm{T<:Real}(r::Range{T}) = issorted(r) ? (1:1:length(r)) : (length(r):-1:1)

function sum{T<:Real}(r::Range{T})
    l = length(r)
    # note that a little care is required to avoid overflow in l*(l-1)/2
    return l * first(r) + (iseven(l) ? (step(r) * (l-1)) * (l>>1)
                                     : (step(r) * l) * ((l-1)>>1))
end

function map!(f::Callable, dest, r::Range)
    i = 1
    for ri in r dest[i] = f(ri); i+=1; end
    dest
end

function map_range_to!(f::Callable, first, dest, r::Range, state)
    dest[1] = first
    i = 2
    while !done(r, state)
        ri, state = next(r, state)
        dest[i] = f(ri)
        i += 1
    end
    dest
end

function map(f::Callable, r::Range)
    if isempty(r); return {}; end
    state = start(r)
    (ri, state) = next(r, state)
    first = f(ri)
    map_range_to!(f, first, Array(typeof(first), length(r)), r, state)
end

function in(x, r::Range)
    n = step(r) == 0 ? 1 : iround((x-first(r))/step(r))+1
    n >= 1 && n <= length(r) && r[n] == x
end

in{T<:Integer}(x, r::Range{T}) = isinteger(x) && x>=minimum(r) && x<=maximum(r) && (step(r)==0 || mod(int(x)-first(r),step(r))==0)

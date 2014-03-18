## 1-dimensional ranges ##

typealias Dims (Int...)

abstract Ranges{T} <: AbstractArray{T,1}

immutable Range{T<:Real} <: Ranges{T}
    start::T
    step::T
    len::Int

    function Range(start::T, step::T, len::Int)
        if step != step; error("step cannot be NaN"); end
        if !(len >= 0);  error("length must be non-negative"); end
        new(start, step, len)
    end
    Range(start::T, step::T, len::Integer) = Range(start, step, int(len))
    Range(start::T, step, len::Integer) = Range(start, convert(T,step), int(len))
end
Range{T}(start::T, step, len::Integer) = Range{T}(start, step, len)

immutable Range1{T<:Real} <: Ranges{T}
    start::T
    len::Int

    function Range1(start::T, len::Int)
        if len < 0; error("length must be non-negative"); end
        new(start, len)
    end
    Range1(start::T, len::Integer) = Range1(start, int(len))

    # TODO: this is a hack to elide the len<0 check for colon.
    # should store start and stop for integer ranges instead
    Range1(start::T, len::Integer, _) = new(start, int(len))
end
Range1{T}(start::T, len::Integer) = Range1{T}(start, len)

immutable FloatRange{T<:FloatingPoint} <: Ranges{T}
    start::T
    step::T
    len::T
    divisor::T
end
FloatRange(a::FloatingPoint, s::FloatingPoint, l::Real, d::FloatingPoint) =
    FloatRange{promote_type(typeof(a),typeof(s),typeof(d))}(a,s,l,d)

function colon{T<:Integer}(start::T, step::T, stop::T)
    step != 0 || error("step cannot be zero in colon syntax")
    Range{T}(start, step, max(0, 1 + fld(stop-start, step)))
end

colon{T<:Integer}(start::T, stop::T) =
    Range1{T}(start, ifelse(stop<start, 0, int(stop-start+1)))

if Int === Int32
colon{T<:Union(Int8,Int16,Int32,Uint8,Uint16)}(start::T, stop::T) =
    Range1{T}(start,
              ifelse(stop < start, 0,
                     checked_add(checked_sub(convert(Int,stop),convert(Int,start)),1)),
              0)  # hack to elide negative length check
else
colon{T<:Union(Int8,Int16,Int32,Int64,Uint8,Uint16,Uint32)}(start::T, stop::T) =
    Range1{T}(start,
              ifelse(stop < start, 0,
                     checked_add(checked_sub(convert(Int,stop),convert(Int,start)),1)),
              0)  # hack to elide negative length check
end

function colon{T<:Real}(start::T, step::T, stop::T)
    step != 0 || error("step cannot be zero in colon syntax")
    if (step < 0) != (stop < start)
        len = 0
    else
        nf = (stop-start)/step + 1
        if T <: FloatingPoint
            n = round(nf)
            if n > 1 && abs(n-nf) < eps(n)*3
                # adjust step to try to hit stop exactly
                step = (stop-start)/(n-1)
                len = itrunc(n)
            else
                len = itrunc(nf)
            end
        else
            n = nf
            len = itrunc(n)
        end
        if n >= typemax(Int)
            error("length ",n," is too large")
        end
    end
    Range(start, step, len)
end
function colon{T<:Real}(start::T, stop::T)
    if stop < start
        len = 0
    else
        nf = stop - start + 1
        if T <: FloatingPoint
            n = round(nf)
            len = abs(n-nf) < eps(n)*3 ? itrunc(n) : itrunc(nf)
        else
            n = nf
            len = itrunc(n)
        end
        if n >= typemax(Int)
            error("length ",n," is too large")
        end
    end
    Range1(start, len)
end

colon(start::Real, step::Real, stop::Real) = colon(promote(start, step, stop)...)
colon(start::Real, stop::Real) = colon(promote(start, stop)...)

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

colon{T<:FloatingPoint}(start::T, step::T, stop::T) =
          step == 0              ? error("step cannot be zero in colon syntax") :
         start == stop           ? FloatRange{T}(start,step,1,1) :
    (0 < step) != (start < stop) ? FloatRange{T}(start,step,0,1) :
                                   FloatRange{T}(frange(start,step,stop)...)

similar(r::Ranges, T::Type, dims::Dims) = Array(T, dims)

length(r::Ranges) = integer(r.len)
size(r::Ranges) = (length(r),)
isempty(r::Ranges) = r.len==0
first(r::Ranges) = r.start
first(r::FloatRange) = r.start/r.divisor
last{T}(r::Range1{T}) = oftype(T, r.start + r.len-1)
last{T}(r::Range{T})  = oftype(T, r.start + (r.len-1)*r.step)
last{T}(r::FloatRange{T}) = oftype(T, (r.start + (r.len-1)*r.step)/r.divisor)

step(r::Range) = r.step
step(r::Range1) = one(r.start)
step(r::FloatRange) = r.step/r.divisor

minimum(r::Range1) = isempty(r) ? error("range must be non-empty") : first(r)
maximum(r::Range1) = isempty(r) ? error("range must be non-empty") : last(r)
minimum(r::Ranges) = isempty(r) ? error("range must be non-empty") : step(r) > 0 ? first(r) : last(r)
maximum(r::Ranges) = isempty(r) ? error("range must be non-empty") : step(r) > 0 ? last(r) : first(r)

ctranspose(r::Ranges) = [x for _=1, x=r]
transpose(r::Ranges) = r'

# Ranges are intended to be immutable
copy(r::Ranges) = r

getindex(r::Ranges, i::Real) = getindex(r, to_index(i))

function getindex{T}(r::Ranges{T}, i::Integer)
    1 <= i <= r.len || error(BoundsError)
    oftype(T, r.start + (i-1)*step(r))
end
function getindex{T}(r::FloatRange{T}, i::Integer)
    1 <= i <= r.len || error(BoundsError)
    oftype(T, (r.start + (i-1)*r.step)/r.divisor)
end

function getindex(r::Range1, s::Range1{Int})
    if s.len > 0
        if !(1 <= last(s) <= r.len)
            throw(BoundsError())
        end
        Range1(r[s.start], s.len)
    else
        Range1(r.start + s.start-1, s.len)
    end
end

function getindex(r::Ranges, s::Ranges{Int})
    if s.len > 0
        if !(1 <= last(s) <= r.len)
            throw(BoundsError())
        end
        Range(r[s.start], step(r)*step(s), s.len)
    else
        Range(r.start + (s.start-1)*step(r), step(r)*step(s), s.len)
    end
end

function show(io::IO, r::Ranges)
    step(r) == 0 ? invoke(show,(IO,Any),io,r) :
    print(io, repr(first(r)), ':', repr(step(r)), ':', repr(last(r)))
end
show(io::IO, r::Range1) = print(io, repr(first(r)), ':', repr(last(r)))

show{T<:FloatingPoint}(io::IO, r::Range{T}) = invoke(show, (IO,Any), io, r)

start(r::Ranges) = 0
next{T}(r::Range{T}, i) = (oftype(T, r.start + i*step(r)), i+1)
next{T}(r::Range1{T}, i) = (oftype(T, r.start + i), i+1)
next{T}(r::FloatRange{T}, i) = (oftype(T, (r.start + i*r.step)/r.divisor), i+1)
done(r::Ranges, i) = (length(r) <= i)

# though these look very similar to the above, for some reason LLVM generates
# much better code for these.
start{T<:Integer}(r::Range1{T}) = r.start
next{T<:Integer}(r::Range1{T}, i) = (i, oftype(T, i+1))
done{T<:Integer}(r::Range1{T}, i) = i==oftype(T, r.start+r.len)

isequal{T<:Ranges}(r::T, s::T) =
    (first(r)==first(s)) & (step(r)==step(s)) & (length(r)==length(s))

isequal(r::Ranges, s::Ranges) = false

=={T<:Ranges}(r::T, s::T) = isequal(r, s)

=={T<:Integer, S<:Integer}(r::Ranges{T}, s::Ranges{S}) =
    (first(r)==first(s)) & (step(r)==step(s)) & (length(r)==length(s))

function ==(r::Ranges, s::Ranges)
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
hash(r::Ranges) =
    bitmix(hash(first(r)), bitmix(hash(step(r)), bitmix(hash(length(r)), uint(0xaaeeaaee))))

# TODO: isless?

intersect{T1<:Integer, T2<:Integer}(r::Range1{T1}, s::Range1{T2}) = max(r.start,s.start):min(last(r),last(s))

intersect{T<:Integer}(i::Integer, r::Range1{T}) =
    i < first(r) ? (first(r):i) :
    i > last(r)  ? (i:last(r))  : (i:i)

intersect{T<:Integer}(r::Range1{T}, i::Integer) = intersect(i, r)

function intersect{T1<:Integer, T2<:Integer}(r::Range1{T1}, s::Range{T2})
    if length(s) == 0
        Range1(first(r), 0)
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

function intersect{T1<:Integer, T2<:Integer}(r::Range{T1}, s::Range1{T2})
    if step(r) == 0
        first(s) <= first(r) <= last(s) ? r : Range(first(r), 0, 0)
    elseif step(r) < 0
        reverse(intersect(s, reverse(r)))
    else
        intersect(s, r)
    end
end

function intersect{T1<:Integer, T2<:Integer}(r::Range{T1}, s::Range{T2})
    if length(r) == 0 || length(s) == 0
        return Range(first(r), step(r), 0)
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

    if a == 0
        # One or both ranges have step 0.
        if step1 == 0 && step2 == 0
            return start1 == start2 ? r : Range(start1, 0, 0)
        elseif step1 == 0
            return start2 <= start1 <= stop2 && rem(start1 - start2, step2) == 0 ? r : Range(start1, 0, 0)
        else
            return start1 <= start2 <= stop1 && rem(start2 - start1, step1) == 0 ? (start2:step1:start2) : Range(start1, step1, 0)
        end
    end

    g, x, y = gcdx(step1, step2)

    if rem(start1 - start2, g) != 0
        # Unaligned, no overlap possible.
        return Range(start1, a, 0)
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

function intersect(r::Ranges, s::Ranges...)
    i = r
    for t in s
        i = intersect(i, t)
    end
    i
end

# findin (the index of intersection)
function _findin{T1<:Integer, T2<:Integer}(r::Ranges{T1}, span::Range1{T2})
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

function findin{T1<:Integer, T2<:Integer}(r::Range1{T1}, span::Range1{T2})
    ifirst, ilast = _findin(r, span)
    ifirst:ilast
end

function findin{T1<:Integer, T2<:Integer}(r::Range{T1}, span::Range1{T2})
    ifirst, ilast = _findin(r, span)
    ifirst:1:ilast
end

## linear operations on ranges ##

-(r::Ranges)     = Range(-r.start, -step(r), r.len)
-(r::FloatRange) = FloatRange(-r.start, -r.step, r.len, r.divisor)

+(x::Real, r::Range1)     = Range1(x + r.start, r.len)
+(x::Real, r::Range)      = Range(x + r.start, r.step, r.len)
+(x::Real, r::FloatRange) = FloatRange(r.divisor*x + r.start, r.step, r.len, r.divisor)
+(r::Ranges, x::Real)     = x + r
+(r::FloatRange, x::Real) = x + r

-(x::Real, r::Ranges)     = Range(x - r.start, -step(r), r.len)
-(x::Real, r::FloatRange) = FloatRange(r.divisor*x - r.start, -r.step, r.len, r.divisor)
-(r::Range1, x::Real)     = Range1(r.start-x, r.len)
-(r::Range , x::Real)     = Range(r.start-x, r.step, r.len)
-(r::FloatRange, x::Real) = FloatRange(r.start - r.divisor*x, r.step, r.len, r.divisor)

.*(x::Real, r::Ranges)     = Range(x*r.start, x*step(r), r.len)
.*(x::Real, r::FloatRange) = FloatRange(x*r.start, x*r.step, r.len, r.divisor)
.*(r::Ranges, x::Real)     = x .* r
.*(r::FloatRange, x::Real) = x .* r

./(r::Ranges, x::Real)     = Range(r.start/x, step(r)/x, r.len)
./(r::FloatRange, x::Real) = FloatRange(r.start/x, r.step/x, r.len, r.divisor)

# TODO: better implementations for FloatRanges?
function +(r1::Ranges, r2::Ranges)
    r1.len == r2.len || error("argument dimensions must match")
    Range(r1.start+r2.start, step(r1)+step(r2), r1.len)
end

function -(r1::Ranges, r2::Ranges)
    r1.len == r2.len || error("argument dimensions must match")
    Range(r1.start-r2.start, step(r1)-step(r2), r1.len)
end

## non-linear operations on ranges ##

./(x::Number, r::Ranges) = [ x/y for y=r ]
./(r::Ranges, y::Number) = [ x/y for x=r ]

.^(x::Number, r::Ranges) = [ x^y for y=r ]
.^(r::Ranges, y::Number) = [ x^y for x=r ]

## concatenation ##

function vcat{T}(r::Ranges{T})
    n = length(r)
    a = Array(T,n)
    i = 1
    for x in r
        @inbounds a[i] = x
        i += 1
    end
    return a
end

convert{T}(::Type{Array{T,1}}, r::Ranges{T}) = vcat(r)

function vcat{T}(rs::Ranges{T}...)
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

reverse(r::Ranges) = Range(last(r), -step(r), r.len)
reverse(r::FloatRange) = FloatRange(last(r), -r.step, r.len, r.divisor)

## sorting ##

issorted(r::Range1) = true
issorted(r::Ranges) = step(r) >= 0

sort(r::Range1) = r
sort!(r::Range1) = r

sort{T<:Real}(r::Range{T}) = issorted(r) ? r : reverse(r)

sortperm(r::Range1) = 1:length(r)
sortperm{T<:Real}(r::Range{T}) = issorted(r) ? (1:1:length(r)) : (length(r):-1:1)

function sum{T<:Real}(r::Ranges{T})
    l = length(r)
    # note that a little care is required to avoid overflow in l*(l-1)/2
    return l * first(r) + (iseven(l) ? (step(r) * (l-1)) * (l>>1)
                                     : (step(r) * l) * ((l-1)>>1))
end

function map!(f::Callable, dest, r::Ranges)
    i = 1
    for ri in r dest[i] = f(ri); i+=1; end
    dest
end

function map_range_to!(f::Callable, first, dest, r::Ranges, state)
    dest[1] = first
    i = 2
    while !done(r, state)
        ri, state = next(r, state)
        dest[i] = f(ri)
        i += 1
    end
    dest
end

function map(f::Callable, r::Ranges)
    if isempty(r); return {}; end
    state = start(r)
    (ri, state) = next(r, state)
    first = f(ri)
    map_range_to!(f, first, Array(typeof(first), length(r)), r, state)
end

function in(x, r::Ranges)
    n = step(r) == 0 ? 1 : iround((x-first(r))/step(r))+1
    n >= 1 && n <= length(r) && r[n] == x
end

in{T<:Integer}(x, r::Ranges{T}) = isinteger(x) && x>=minimum(r) && x<=maximum(r) && (step(r)==0 || mod(int(x)-first(r),step(r))==0)

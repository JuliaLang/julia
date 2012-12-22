## 1-dimensional ranges ##

typealias Dims (Int...)

abstract Ranges{T<:Real} <: AbstractArray{T,1}

type Range{T<:Real} <: Ranges{T}
    start::T
    step::T
    len::Int

    function Range(start::T, step::T, len::Int)
        if step != step; error("Range: step cannot be NaN"); end
        if step == 0;    error("Range: step cannot be zero"); end
        if !(len >= 0);  error("Range: length must be non-negative"); end
        new(start, step, len)
    end
    Range(start::T, step::T, len::Integer) = Range(start, step, int(len))
    Range(start::T, step, len::Integer) = Range(start, convert(T,step), int(len))
end
Range{T}(start::T, step, len::Integer) = Range{T}(start, step, len)

type Range1{T<:Real} <: Ranges{T}
    start::T
    len::Int

    function Range1(start::T, len::Int)
        if !(len >= 0); error("Range: length must be non-negative"); end
        new(start, len)
    end
    Range1(start::T, len::Integer) = Range1(start, int(len))
end
Range1{T}(start::T, len::Integer) = Range1{T}(start, len)

colon{T<:Integer}(start::T, step::T, stop::T) =
    Range(start, step, max(0, div(stop-start+step, step)))
colon{T<:Integer}(start::T, stop::T) =
    Range1(start, max(0, stop-start+1))

function colon{T<:Real}(start::T, step::T, stop::T)
    len = (stop-start)/step
    if len >= typemax(Int)
        error("Range: length ",len," is too large")
    end
    Range(start, step, max(0, ifloor(len)+1))
end
function colon{T<:Real}(start::T, stop::T)
    len = stop-start
    if len >= typemax(Int)
    error("Range: length ",len," is too large")
    end
    Range1(start, max(0, ifloor(len)+1))
end

colon(start::Real, step::Real, stop::Real) = colon(promote(start, step, stop)...)
colon(start::Real, stop::Real) = colon(promote(start, stop)...)

similar(r::Ranges, T::Type, dims::Dims) = Array(T, dims)

length(r::Ranges) = r.len
const numel = length
size(r::Ranges) = (r.len,)
isempty(r::Ranges) = r.len==0
first(r::Ranges) = r.start
last{T}(r::Range{T}) = r.start + oftype(T,r.len-1)*step(r)
last{T}(r::Range1{T}) = r.start + oftype(T,r.len-1)

step(r::Range)  = r.step
step(r::Range1) = one(r.start)

min(r::Range1) = r.start
max(r::Range1) = last(r)
min(r::Range) = r.step > 0 ? r.start : last(r)
max(r::Range) = r.step > 0 ? last(r) : r.start

# Ranges are intended to be immutable
copy(r::Ranges) = r

function ref{T}(r::Range{T}, i::Integer)
    if !(1 <= i <= r.len); error(BoundsError); end
    r.start + oftype(T,i-1)*step(r)
end
function ref{T}(r::Range1{T}, i::Integer)
    if !(1 <= i <= r.len); error(BoundsError); end
    r.start + oftype(T,i-1)
end

ref(r::Range, s::Range{Int}) =
    r.len < last(s) ? error(BoundsError) : Range(r[s.start], r.step*s.step, s.len)
ref(r::Range1, s::Range{Int}) =
    r.len < last(s) ? error(BoundsError) : Range(r[s.start], s.step, s.len)
ref(r::Range, s::Range1{Int}) =
    r.len < last(s) ? error(BoundsError) : Range(r[s.start], r.step, s.len)
ref(r::Range1, s::Range1{Int}) =
    r.len < last(s) ? error(BoundsError) : Range1(r[s.start], s.len)

show(io, r::Range)  = print(io, r.start,':',r.step,':',last(r))
show(io, r::Range1) = print(io, r.start,':',last(r))

start(r::Ranges) = 0
next(r::Range,  i) = (r.start + oftype(r.start,i)*step(r), i+1)
next(r::Range1, i) = (r.start + oftype(r.start,i), i+1)
done(r::Ranges, i) = (length(r) <= i)

isequal(r::Ranges, s::Ranges) = (r.start==s.start) & (step(r)==step(s)) & (r.len==s.len)
isequal(r::Range1, s::Range1) = (r.start==s.start) & (r.len==s.len)

# TODO: isless?

intersect(r::Range1, s::Range1) = max(r.start,s.start):min(last(r),last(s))

intersect{T<:Integer}(i::Integer, r::Range1{T}) =
    i < first(r) ? (first(r):i) :
    i > last(r)  ? (i:last(r))  : (i:i)

intersect{T<:Integer}(r::Range1{T}, i::Integer) = intersect(i, r)

# TODO: general intersect?
function intersect(r::Range1, s::Range)
    sta = first(s)
    ste = step(s)
    sto = last(s)
    lo = first(r)
    hi = last(r)
    i0 = max(lo, sta + ste*div((lo-sta)+ste-1, ste))
    i1 = min(hi, sta + ste*div((hi-sta), ste))
    i0 = max(i0, sta)
    i1 = min(i1, sto)
    i0:ste:i1
end
intersect(r::Range, s::Range1) = intersect(s, r)

## linear operations on ranges ##

-(r::Ranges) = Range(-r.start, -step(r), r.len)

+(x::Real, r::Range ) = Range(x+r.start, r.step, r.len)
+(x::Real, r::Range1) = Range1(x+r.start, r.len)
+(r::Ranges, x::Real) = x+r

-(x::Real, r::Ranges) = Range(x-r.start, -step(r), r.len)
-(r::Range , x::Real) = Range(r.start-x, r.step, r.len)
-(r::Range1, x::Real) = Range1(r.start-x, r.len)

.*(x::Real, r::Ranges) = Range(x*r.start, x*step(r), r.len)
.*(r::Ranges, x::Real) = x*r

./(r::Ranges, x::Real) = Range(r.start/x, step(r)/x, r.len)

function +(r1::Ranges, r2::Ranges)
    if r1.len != r2.len
        error("argument dimensions must match")
    end
    Range(r1.start+r2.start, step(r1)+step(r2), r1.len)
end

function -(r1::Ranges, r2::Ranges)
    if r1.len != r2.len
        error("argument dimensions must match")
    end
    Range(r1.start-r2.start, step(r1)-step(r2), r1.len)
end

## non-linear operations on ranges ##

./(x::Number, r::Ranges) = [ x/y for y=r ]
./(r::Ranges, y::Number) = [ x/y for x=r ]
function ./(r::Ranges, s::Ranges)
    if length(r) != length(s)
        error("argument dimensions must match")
    end
    [ r[i]/s[i] for i = 1:length(r) ]
end

function .*(r::Ranges, s::Ranges)
    if length(r) != length(s)
        error("argument dimensions must match")
    end
    [ r[i]*s[i] for i = 1:length(r) ]
end

.^(x::Number, r::Ranges) = [ x^y for y=r ]
.^(r::Ranges, y::Number) = [ x^y for x=r ]
function .^(r::Ranges, s::Ranges)
    if length(r) != length(s)
        error("argument dimensions must match")
    end
    [ r[i]^s[i] for i = 1:length(r) ]
end

## concatenation ##

function vcat{T}(r::Ranges{T})
    n = length(r)
    a = Array(T,n)
    i = 1
    for x in r
        a[i] = x
        i += 1
    end
    return a
end

function vcat{T}(rs::Ranges{T}...)
    n = sum(length,rs)::Int
    a = Array(T,n)
    i = 1
    for r in rs
        for x in r
            a[i] = x
            i += 1
        end
    end
    return a
end

reverse{T<:Real}(r::Ranges{T}) = Range(last(r), -step(r), r.len)

## sorting ##

issorted(r::Range1) = true
issorted(r::Range) = r.step > 0

sort(r::Range1) = r
sort!(r::Range1) = r

sort{T<:Real}(r::Range{T}) = issorted(r) ? r : reverse(r)

sortperm(r::Range1) = (r, 1:length(r))
sortperm{T<:Real}(r::Range{T}) = issorted(r) ? (r, 1:1:length(r)) :
                                               (reverse(r), length(r):-1:1)

function sum(r::Ranges)
    l = length(r)
    return l * first(r) + step(r) * div(l * (l - 1), 2)
end

function map_to(f, dest, r::Ranges)
    i = 1
    for ri in r dest[i] = f(ri); i+=1; end
    dest
end

map(f, r::Ranges) = [ f(x) for x in r ]

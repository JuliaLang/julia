## 1-dimensional ranges ##

typealias Dims (Int...)

abstract Ranges{T} <: AbstractArray{T,1}

immutable Range{T<:Real} <: Ranges{T}
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

immutable Range1{T<:Real} <: Ranges{T}
    start::T
    len::Int

    function Range1(start::T, len::Int)
        if !(len >= 0); error("Range: length must be non-negative"); end
        new(start, len)
    end
    Range1(start::T, len::Integer) = Range1(start, int(len))
end
Range1{T}(start::T, len::Integer) = Range1{T}(start, len)

immutable OrdinalRange{T} <: Ranges{T}
    start::T
    step::Int
    len::Int

    function OrdinalRange(start::T, step::Int, len::Int)
        if step == 0;    error("Range: step cannot be zero"); end
        if !(len >= 0);  error("Range: length must be non-negative"); end
        new(start, step, len)
    end
    OrdinalRange(start::T, step::Integer, len::Integer) = OrdinalRange(start, int(step), int(len))
end
OrdinalRange{T}(start::T, step::Integer, len::Integer) = OrdinalRange{T}(start, step, len)

colon{T<:Integer}(start::T, step::T, stop::T) =
    Range(start, step, max(0, div(stop-start+step, step)))
colon{T<:Integer}(start::T, stop::T) =
    Range1(start, max(0, stop-start+1))

colon{T}(start::T, step, stop::T) =
    OrdinalRange(start, step, max(0, div(stop-start+step, step)))
colon{T}(start::T, stop::T) =
    OrdinalRange(start, 1, max(0, stop-start+1))

colon(start::Char, step::Int, stop::Char) =
    OrdinalRange(start, step, max(0, div(stop-start+step, step)))
colon(start::Char, stop::Char) =
    OrdinalRange(start, 1, max(0, stop-start+1))

function colon{T<:Real}(start::T, step::T, stop::T)
    if (step<0) != (stop<start)
        len = 0
    else
        nf = (stop-start)/step + 1
        if T <: FloatingPoint
            n = round(nf)
            if abs(n-nf) < eps(n)*3
                # adjust step to try to hit stop exactly
                step = (stop-start)/(n-1)
                len = itrunc(n)
            else
                len = itrunc(nf)
            end
        else
            n = nf
            len = iround(n)
        end
        if n >= typemax(Int)
            error("Range: length ",n," is too large")
        end
    end
    Range(start, step, len)
end
function colon{T<:Real}(start::T, stop::T)
    if stop < start
        len = 0
    else
        n = round(stop-start+1)
        if n >= typemax(Int)
            error("Range: length ",n," is too large")
        end
        len = itrunc(n)
    end
    Range1(start, len)
end

colon(start::Real, step::Real, stop::Real) = colon(promote(start, step, stop)...)
colon(start::Real, stop::Real) = colon(promote(start, stop)...)

similar(r::Ranges, T::Type, dims::Dims) = Array(T, dims)

length(r::Ranges) = r.len
size(r::Ranges) = (r.len,)
isempty(r::Ranges) = r.len==0
first(r::Ranges) = r.start
last{T}(r::Range{T}) = r.start + oftype(T,r.len-1)*step(r)
last{T}(r::Range1{T}) = r.start + oftype(T,r.len-1)
last{T}(r::OrdinalRange{T}) = r.start + (r.len-1)*r.step

step(r::Range)  = r.step
step(r::Range1) = one(r.start)
step(r::OrdinalRange) = r.step

min(r::Range1) = (isempty(r)&&error("min: range is empty")) || first(r)
max(r::Range1) = (isempty(r)&&error("max: range is empty")) || last(r)
min(r::Ranges) = (isempty(r)&&error("min: range is empty")) || (step(r) > 0 ? first(r) :  last(r))
max(r::Ranges) = (isempty(r)&&error("max: range is empty")) || (step(r) > 0 ?  last(r) : first(r))

# Ranges are intended to be immutable
copy(r::Ranges) = r

function getindex{T}(r::Ranges{T}, i::Integer)
    if !(1 <= i <= r.len); error(BoundsError); end
    oftype(T, r.start + (i-1)*step(r))
end

getindex(r::Range, s::Range{Int}) =
    r.len < last(s) ? error(BoundsError) : Range(r[s.start], r.step*s.step, s.len)
getindex(r::Range1, s::Range{Int}) =
    r.len < last(s) ? error(BoundsError) : Range(r[s.start], s.step, s.len)
getindex(r::Range, s::Range1{Int}) =
    r.len < last(s) ? error(BoundsError) : Range(r[s.start], r.step, s.len)
getindex(r::Range1, s::Range1{Int}) =
    r.len < last(s) ? error(BoundsError) : Range1(r[s.start], s.len)

show(io::IO, r::Range)  = print(io, r.start,':',step(r),':',last(r))
show(io::IO, r::Range1) = print(io, r.start,':',last(r))
function show(io::IO, r::OrdinalRange)
    show(io, r.start)
    print(io, ':')
    if step(r) != 1
        print(io, step(r),':')
    end
    show(io, last(r))
end

start(r::Ranges) = 0
next(r::Range,  i) = (r.start + oftype(r.start,i)*step(r), i+1)
next(r::Range1, i) = (r.start + oftype(r.start,i), i+1)
next(r::OrdinalRange, i) = (r.start + i*step(r), i+1)
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

# findin (the index of intersection)
function findin(r::Ranges, span::Range1)
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
    else
        ifirst = fr <= lspan ? 1 : iceil((lspan-fr)/sr)+1
        ilast = lr >= fspan ? length(r) : length(r) - iceil((lr-fspan)/sr)
    end
    ifirst:ilast
end

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

function .*{T<:Number,S<:Number}(r::Ranges{T}, s::Ranges{S})
    if length(r) != length(s)
        error("argument dimensions must match")
    end
    [ r[i]*s[i] for i = 1:length(r) ]
end

.^(x::Number, r::Ranges) = [ x^y for y=r ]
.^(r::Ranges, y::Number) = [ x^y for x=r ]
function .^{T<:Number,S<:Number}(r::Ranges{T}, s::Ranges{S})
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

convert{T}(::Type{Array{T,1}}, r::Ranges{T}) = vcat(r)

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
issorted(r::Ranges) = step(r) > 0

sort(r::Range1) = r
sort!(r::Range1) = r

sort{T<:Real}(r::Range{T}) = issorted(r) ? r : reverse(r)

sortperm(r::Range1) = 1:length(r)
sortperm{T<:Real}(r::Range{T}) = issorted(r) ? (1:1:length(r)) : (length(r):-1:1)

function sum{T<:Real}(r::Ranges{T})
    l = length(r)
    return l * first(r) + step(r) * div(l * (l - 1), 2)
end

function map!(f, dest, r::Ranges)
    i = 1
    for ri in r dest[i] = f(ri); i+=1; end
    dest
end

map(f, r::Ranges) = [ f(x) for x in r ]

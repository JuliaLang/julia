## 1-dimensional ranges ##

typealias Dims (Int...)

abstract Ranges{T} <: AbstractArray{T,1}

immutable Range{T<:Real} <: Ranges{T}
    start::T
    step::T
    len::Int

    function Range(start::T, step::T, len::Int)
        if step != step; error("Range: step cannot be NaN"); end
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

function colon{T<:Integer}(start::T, step::T, stop::T)
    step != 0 || error("step cannot be zero in colon syntax")
    Range(start, step, max(0, div(stop-start+step, step)))
end
colon{T<:Integer}(start::T, stop::T) =
    Range1(start, max(0, stop-start+1))

function colon{T<:Real}(start::T, step::T, stop::T)
    step != 0 || error("step cannot be zero in colon syntax")
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
            len = itrunc(n)
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
        nf = stop - start + 1
        if T <: FloatingPoint
            n = round(nf)
            len = abs(n-nf) < eps(n)*3 ? itrunc(n) : itrunc(nf)
        else
            n = nf
            len = itrunc(n)
        end
        if n >= typemax(Int)
            error("Range: length ",n," is too large")
        end
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
last{T}(r::Range1{T}) = oftype(T, r.start + r.len-1)
last{T}(r::Range{T})  = oftype(T, r.start + (r.len-1)*r.step)

step(r::Range)  = r.step
step(r::Range1) = one(r.start)

min(r::Range1) = (isempty(r)&&error("min: range is empty")) || first(r)
max(r::Range1) = (isempty(r)&&error("max: range is empty")) || last(r)
min(r::Ranges) = (isempty(r)&&error("min: range is empty")) || (step(r) > 0 ? first(r) :  last(r))
max(r::Ranges) = (isempty(r)&&error("max: range is empty")) || (step(r) > 0 ?  last(r) : first(r))

# Ranges are intended to be immutable
copy(r::Ranges) = r

getindex(r::Ranges, i::Real) = getindex(r, to_index(i))

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

function show(io::IO, r::Range)
    if step(r) == 0
        print(io, "Range(",r.start,",",step(r),",",r.len,")")
    else
        print(io, repr(r.start),':',repr(step(r)),':',repr(last(r)))
    end
end
show(io::IO, r::Range1) = print(io, repr(r.start),':',repr(last(r)))

start(r::Ranges) = 0
next{T}(r::Range{T},  i) = (oftype(T, r.start + i*step(r)), i+1)
next{T}(r::Range1{T}, i) = (oftype(T, r.start + i), i+1)
done(r::Ranges, i) = (length(r) <= i)

==(r::Ranges, s::Ranges) = (r.start==s.start) & (step(r)==step(s)) & (r.len==s.len)
==(r::Range1, s::Range1) = (r.start==s.start) & (r.len==s.len)
isequal(r::Ranges, s::Ranges) =
    isequal(r.start,s.start) & isequal(step(r),step(s)) & (r.len==s.len)
isequal(r::Range1, s::Range1) = isequal(r.start,s.start) & (r.len==s.len)

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
function findin{T1<:Integer, T2<:Integer}(r::Ranges{T1}, span::Range1{T2})
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
issorted(r::Ranges) = step(r) >= 0

sort(r::Range1) = r
sort!(r::Range1) = r

sort{T<:Real}(r::Range{T}) = issorted(r) ? r : reverse(r)

sortperm(r::Range1) = 1:length(r)
sortperm{T<:Real}(r::Range{T}) = issorted(r) ? (1:1:length(r)) : (length(r):-1:1)

function sum{T<:Real}(r::Ranges{T})
    l = length(r)
    return l * first(r) + step(r) * div(l * (l - 1), 2)
end

function map!(f::Union(Function,DataType), dest, r::Ranges)
    i = 1
    for ri in r dest[i] = f(ri); i+=1; end
    dest
end

map(f::Union(Function,DataType), r::Ranges) = [ f(x) for x in r ]

function contains(r::Ranges, x)
    n = step(r) == 0 ? 1 : iround((x-first(r))/step(r))+1
    n >= 1 && n <= length(r) && r[n] == x
end

contains{T<:Integer}(r::Ranges{T}, x) = isinteger(x) && x>=min(r) && x<=max(r) && (step(r)==0 || mod(int(x)-first(r),step(r))==0)

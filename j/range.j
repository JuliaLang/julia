## 1-dimensional ranges ##

typealias Dims (Int...)

abstract Ranges{T<:Real} <: AbstractArray{T,1}

type Range{T<:Real} <: Ranges{T}
    start::T
    step::T
    stop::T

    function Range(start::T, step::T, stop::T)
        if step == 0
            error("Range: step cannot be zero")
        end
        new(start, step, stop)
    end
end
Range{T}(start::T, step::T, stop::T) = Range{T}(start, step, stop)
Range(start, step, stop) = Range(promote(start, step, stop)...)

type Range1{T<:Real} <: Ranges{T}
    start::T
    stop::T
end
Range1{T}(start::T, stop::T) = Range1{T}(start, stop)
Range1(start, stop) = Range1(promote(start, stop)...)

colon(start::Real, stop::Real, step::Real) = Range(start, step, stop)
colon(start::Real, stop::Real) = Range1(start, stop)

similar(r::Ranges, T::Type, dims::Dims) = Array(T, dims)

step(r::Range)  = r.step
step(r::Range1) = one(r.start)

show(r::Range)  = print(r.start,':',r.step,':',r.stop)
show(r::Range1) = print(r.start,':',r.stop)

length{T<:Integer}(r::Range{T}) = max(0, int(div(r.stop-r.start+r.step, r.step)))
length{T<:Integer}(r::Range1{T}) = max(0, int(r.stop-r.start+1))
length{T}(r::Range{T}) = max(0, int(itrunc((r.stop-r.start)/r.step+1)))
length{T}(r::Range1{T}) = max(0, int(itrunc(r.stop-r.start+1)))
size(r::Ranges) = tuple(length(r))
numel(r::Ranges) = length(r)

isempty(r::Range) = (r.step > 0 ? r.stop < r.start : r.stop > r.start)
isempty(r::Range1) = (r.stop < r.start)

start{T<:Integer}(r::Ranges{T}) = r.start
next{T<:Integer}(r::Ranges{T}, i::T) = (i, i+step(r))
done{T<:Integer}(r::Ranges{T}, i::T) = (step(r) < 0 ? i < r.stop : i > r.stop)
done{T<:Integer}(r::Range1{T}, i::T) = (i > r.stop)

start(r::Ranges) = 0
next(r::Ranges, i::Integer) = (r.start+i*step(r), i+1)
done(r::Ranges, i::Integer) = (step(r) < 0 ? r.start+i*step(r) < r.stop :
                                         r.start+i*step(r) > r.stop)
done(r::Range1, i::Integer) = (r.start+i > r.stop)

ref(r::Range, s::Range{Int}) = Range(r[s[1]],r.step*s.step,r[s[end]])
ref(r::Range1, s::Range{Int}) = Range(r[s[1]],s.step,r[s[end]])
ref(r::Range, s::Range1{Int}) = Range(r[s[1]],r.step,r[s[end]])
ref(r::Range1, s::Range1{Int}) = Range1(r[s[1]],r[s[end]])

function ref(r::Range, i::Integer)
    if i < 1; error(BoundsError); end
    x = r.start + (i-1)*step(r)
    if step(r) > 0 ? x > r.stop : x < r.stop
        error(BoundsError)
    end
    return x
end

function ref(r::Range1, i::Integer)
    if i < 1; error(BoundsError); end
    x = r.start + (i-1)
    if x > r.stop
        error(BoundsError)
    end
    return x
end

last{T<:Integer}(r::Range1{T}) = r.stop
last(r::Ranges) = r.start + (length(r)-1)*step(r)

isequal(r::Range1, s::Range1) = r.start==s.start && r.stop==s.stop
isequal(r::Ranges, s::Ranges) = r.start==s.start && step(r)==step(s) && last(r)==last(s)

intersect(r::Range1, s::Range1) = max(r.start,s.start):min(r.stop,s.stop)

function intersect(r::Range1, s::Range)
    sta = start(s)
    ste = step(s)
    sto = s[end]
    lo = r.start
    hi = r.stop
    i0 = max(lo, sta + ste*div((lo-sta)+ste-1, ste))
    i1 = min(hi, sta + ste*div((hi-sta), ste))
    i0 = max(i0, sta)
    i1 = min(i1, sto)
    i0:ste:i1
end
intersect(r::Range, s::Range1) = intersect(s, r)

## linear operations on ranges ##

-(r::Ranges) = Range(-r.start, -step(r), -r.stop)

+(x::Real, r::Range ) = Range(x+r.start, r.step, x+r.stop)
+(x::Real, r::Range1) = Range1(x+r.start, x+r.stop)
+(r::Ranges, x::Real) = x+r

-(x::Real, r::Ranges) = Range(x-r.start, -step(r), x-r.stop)
-(r::Range , x::Real) = Range(r.start-x, r.step, r.stop-x)
-(r::Range1, x::Real) = Range1(r.start-x, r.stop-x)

.*(x::Real, r::Ranges) = Range(x*r.start, x*step(r), x*r.stop)
.*(r::Ranges, x::Real) = x*r

./(r::Ranges, x::Real) = Range(r.start/x, step(r)/x, r.stop/x)

function +(r1::Ranges, r2::Ranges)
    if length(r1) != length(r2); error("shape mismatch"); end
    Range(r1.start+r2.start, step(r1)+step(r2), last(r1)+last(r2))
end

function -(r1::Ranges, r2::Ranges)
    if length(r1) != length(r2); error("shape mismatch"); end
    Range(r1.start-r2.start, step(r1)-step(r2), last(r1)-last(r2))
end

## non-linear operations on ranges ##

./(x::Number, r::Ranges) = [ x/y | y=r ]
./(r::Ranges, y::Number) = [ x/y | x=r ]
function ./(r::Ranges, s::Ranges)
    if length(r) != length(s)
        error("argument dimensions must match")
    end
    [ r[i]/s[i] | i = 1:length(r) ]
end

function .*(r::Ranges, s::Ranges)
    if length(r) != length(s)
        error("argument dimensions must match")
    end
    [ r[i]*s[i] | i = 1:length(r) ]
end

.^(x::Number, r::Ranges) = [ x^y | y=r ]
.^(r::Ranges, y::Number) = [ x^y | x=r ]
function .^(r::Ranges, s::Ranges)
    if length(r) != length(s)
        error("argument dimensions must match")
    end
    [ r[i]^s[i] | i = 1:length(r) ]
end

## concatenation ##

function vcat{T}(r::Ranges{T})
    n = length(r)
    a = Array(T,n)
    i = 1
    for x = r
        a[i] = x
        i += 1
    end
    a
end

function vcat{T}(rs::Ranges{T}...)
    n = sum(length,rs)::Int
    a = Array(T,n)
    i = 1
    for r = rs
        for x = r
            a[i] = x
            i += 1
        end
    end
    a
end

reverse{T<:Real}(v::Ranges{T}) = (last(v)):(-step(v)):(v.start)

## sorting ##

issorted(v::Range1) = true
issorted(v::Range) = v.step >= 0

sort(v::Range1) = v
sort!(v::Range1) = v

sort{T<:Real}(v::Range{T}) = issorted(v) ? v : reverse(v)

sortperm(v::Range1) = (v, 1:length(v))
sortperm{T<:Real}(v::Range{T}) = issorted(v) ? (v, 1:1:length(v)) : (reverse(v),length(v):-1:1) 

function sum(v::Range1)
    n1 = v.start-1
    n2 = v.stop
    return div((n2*(n2+1) - n1*(n1+1)), 2)
end

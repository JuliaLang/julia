## 1-dimensional ranges ##

typealias Dims (Size...)

type Range{T<:Real} <: AbstractArray{T,1}
    start::T
    step::T
    stop::T
end
Range(start, step, stop) = Range(promote(start, step, stop)...)
Range{T}(start::T, step::T, stop::T) =
    throw(MethodError(Range, (start,step,stop)))

type Range1{T<:Real} <: AbstractArray{T,1}
    start::T
    stop::T
end
Range1(start, stop) = Range1(promote(start, stop)...)
Range1{T}(start::T, stop::T) =
    throw(MethodError(Range1, (start,stop)))

similar(r::Range, T::Type, dims::Dims) = Range(convert(T, r.start), convert(T, r.step), convert(T, r.stop))
similar(r::Range1, T::Type, dims::Dims) = Range1(convert(T, r.start), convert(T, r.stop))

typealias Ranges Union(Range,Range1)

ref(r::Range, s::Range{Index}) = Range(r[s[1]],r.step*s.step,r[s[end]])
ref(r::Range1, s::Range{Index}) = Range(r[s[1]],s.step,r[s[end]])
ref(r::Range, s::Range1{Index}) = Range(r[s[1]],r.step,r[s[end]])
ref(r::Range1, s::Range1{Index}) = Range1(r[s[1]],r[s[end]])

step(r::Range)  = r.step
step(r::Range1) = one(r.start)

show(r::Range)  = print(r.start,':',r.step,':',r.stop)
show(r::Range1) = print(r.start,':',r.stop)

numel(r::Ranges) = length(r)
size(r::Ranges) = tuple(length(r))
length{T<:Int}(r::Range{T}) = max(0, div(r.stop-r.start+r.step, r.step))
length{T<:Int}(r::Range1{T}) = max(0, r.stop-r.start+1)
length(r::Range) = max(0, long((r.stop-r.start)/r.step+1))
length(r::Range1) = max(0, long(r.stop-r.start+1))

isempty(r::Range) = (r.step > 0 ? r.stop < r.start : r.stop > r.start)
isempty(r::Range1) = (r.stop < r.start)

start{T<:Int}(r::Range{T}) = r.start
done{T<:Int}(r::Range{T}, i::T) = (r.step < 0 ? i < r.stop : i > r.stop)
next{T<:Int}(r::Range{T}, i::T) = (i, i+r.step)

start(r::Range1) = r.start
done(r::Range1, i) = (i > r.stop)
next{T}(r::Range1{T}, i) = (i, i+one(T))

# floating point ranges need to keep an integer counter
start(r::Range) = 0
done{T}(r::Range{T}, st) =
    (r.step < 0 ? r.start+st*r.step < r.stop :
                  r.start+st*r.step > r.stop)
next{T}(r::Range{T}, st) = (r.start+st*r.step, st+1)

colon(start::Real, stop::Real, step::Real) = Range(start, step, stop)
colon(start::Real, stop::Real) = Range1(start, stop)

function ref(r::Ranges, i::Int)
    if i < 1; error(BoundsError); end
    x = r.start + (i-1)*step(r)
    if step(r) > 0 ? x > r.stop : x < r.stop
        error(BoundsError)
    end
    return x
end

## linear operations on 1-d ranges ##

-(r::Ranges) = Range(-r.start, -step(r), -r.stop)

+(x::Real, r::Range ) = Range(x+r.start, r.step, x+r.stop)
+(x::Real, r::Range1) = Range1(x+r.start, x+r.stop)
+(r::Ranges, x::Real) = x+r

-(x::Real, r::Ranges) = Range(x-r.start, -step(r), x-r.stop)
-(r::Range , x::Real) = Range(r.start-x, r.step, r.stop-x)
-(r::Range1, x::Real) = Range1(r.start-x, r.stop-x)

*(x::Real, r::Ranges) = Range(x*r.start, x*step(r), x*r.stop)
*(r::Ranges, x::Real) = x*r

/(r::Ranges, x::Real) = Range(r.start/x, step(r)/x, r.stop/x)

## adding and subtracting ranges ##

# TODO: if steps combine to zero, create sparse zero vector

function +(r1::Ranges, r2::Ranges)
    if length(r1) != length(r2); error("shape mismatch"); end
    Range(r1.start+r2.start, step(r1)+step(r2), r1.stop+r2.stop)
end

function -(r1::Ranges, r2::Ranges)
    if length(r1) != length(r2); error("shape mismatch"); end
    Range(r1.start-r2.start, step(r1)-step(r2), r1.stop-r2.stop)
end

## concatenation ##

function vcat{T}(rs::Union(Range{T},Range1{T})...)
    n = sum(map(length,rs))
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

## N-dimensional ranges ##

type NDRange{N}
    ranges::NTuple{N,Any}
    empty::Bool

    if eq_int(unbox(Size,N),unbox(Size,0))
        NDRange(r::())           = new(r, false)
    elseif eq_int(unbox(Size,N),unbox(Size,1))
        NDRange(r::(Any,))       = new(r, isempty(r[1]))
    elseif eq_int(unbox(Size,N),unbox(Size,2))
        NDRange(r::(Any,Any))    = new(r, isempty(r[1])||isempty(r[2]))
    elseif eq_int(unbox(Size,N),unbox(Size,3))
        NDRange(r::(Any,Any,Any))= new(r,
                                       isempty(r[1])||isempty(r[2])||isempty(r[3]))
    else
        NDRange(r::Tuple)        = new(r, anyp(isempty,r))
    end
end

NDRange{N}(r::NTuple{N,Any}) = NDRange{N}(r)
NDRange(rs...) = NDRange(rs)

start(r::NDRange{0}) = false
done(r::NDRange{0}, st) = st
next(r::NDRange{0}, st) = ((), true)

start(r::NDRange) = { start(r.ranges[i]) | i=1:length(r.ranges) }
done(r::NDRange, st) = r.empty || !bool(st)

function next{N}(r::NDRange{N}, st)
    nxt = ntuple(N, i->next(r.ranges[i], st[i]))
    vals = map(n->n[1], nxt)

    for itr=1:N
        ri = r.ranges[itr]
        ni = nxt[itr][2]
        if !done(ri, ni)
            st[itr] = ni
            return (vals, st)
        else
            st[itr] = start(ri)
        end
    end
    (vals, false)
end

function next(r::NDRange{2}, st)
    (r1, r2) = r.ranges
    (v1, n1) = next(r1, st[1])
    (v2, n2) = next(r2, st[2])
    vals = (v1, v2)

    if !done(r1, n1)
        st[1] = n1
        return (vals, st)
    else
        st[1] = start(r1)
    end
    if !done(r2, n2)
        st[2] = n2
        return (vals, st)
    end
    (vals, false)
end

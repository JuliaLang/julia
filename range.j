struct Range{T}
    start::T
    step::T
    stop::T
end

struct Range1{T}
    start::T
    stop::T
end

struct RangeFrom{T}
    start::T
    step::T
end

struct RangeTo{T}
    step::T
    stop::T
end

struct RangeBy{T}
    step::T
end

numel(r::Union(Range,Range1)) = length(r)
size(r::Union(Range,Range1)) = tuple(length(r))
length{T<:Int}(r::Range{T}) = max(0, div((r.stop-r.start+r.step), r.step))
length{T<:Int}(r::Range1{T}) = max(0, (r.stop-r.start + 1))
length(r::Range) = max(0, int32((r.stop-r.start) / r.step + 1))
length(r::Range1) = max(0, int32(r.stop-r.start + 1))

isempty(r::Range) = (r.step > 0 ? r.stop < r.start : r.stop > r.start)
isempty(r::Range1) = (r.stop < r.start)

start{T<:Int}(r::Range{T}) = r.start
done{T<:Int}(r::Range{T}, i) = (r.step < 0 ? (i < r.stop) : (i > r.stop))
next{T<:Int}(r::Range{T}, i) = (i, i+r.step)

start(r::Range1) = r.start
done(r::Range1, i) = (i > r.stop)
next(r::Range1, i) = (i, i+1)

start{T<:Int}(r::RangeFrom{T}) = r.start
done{T<:Int}(r::RangeFrom{T}, st) = false
next{T<:Int}(r::RangeFrom{T}, i) = (i, i+r.step)

# floating point ranges need to keep an integer counter
start(r::Range) = (1, r.start)
done{T}(r::Range{T}, st) =
    (r.step < 0 ? (st[2]::T < r.stop) : (st[2]::T > r.stop))
next{T}(r::Range{T}, st) =
    (st[2]::T, (st[1]::Int+1, r.start + st[1]::Int*r.step))

start(r::RangeFrom) = (1, r.start)
done(r::RangeFrom, st) = false
next{T}(r::RangeFrom{T}, st) =
    (st[2]::T, (st[1]::Int+1, r.start + st[1]::Int*r.step))

start(r::RangeTo) = error("range ",r," has no initial value")
start(r::RangeBy) = error("range ",r," has no initial value")

colon(start::Real, stop::Real, step::Real) = Range(promote(start, step, stop)...)

ref(r::Range, i::Index) = (res = r.start + i*r.step; res > r.stop ? error("Out of bounds") : res)
ref(r::Range1, i::Index) = (res = r.start + r.step; res > r.stop ? error("Out of bounds") : res)

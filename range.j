struct Range{T}
    start::T
    step::T
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

numel(r::Range) = length(r)
length{T<:Int}(r::Range{T}) = (r.step > 0 ?
                               div((r.stop-r.start+r.step), r.step) :
                               div((r.start-r.stop-r.step), -r.step))
length(r::Range) = (r.step > 0 ?
                    int32(floor((r.stop-r.start+r.step) / r.step)) :
                    int32(floor((r.start-r.stop-r.step) / -r.step)))

isempty(r::Range) = (r.step > 0 ? r.stop < r.start : r.stop > r.start)

start{T<:Int}(r::Range{T}) = r.start
done{T<:Int}(r::Range{T}, i) = (r.step < 0 ? (i < r.stop) : (i > r.stop))
next{T<:Int}(r::Range{T}, i) = (i, i+r.step)

start{T<:Int}(r::RangeFrom{T}) = r.start
done{T<:Int}(r::RangeFrom{T}, st) = false
next{T<:Int}(r::RangeFrom{T}, i) = (i, i+r.step)

# floating point ranges need to keep an integer counter
start(r::Range) = (1, r.start)
done(r::Range, st) = (r.step < 0 ? (st[2] < r.stop) : (st[2] > r.stop))
next(r::Range, st) = (st[2], (st[1]+1, r.start + st[1]*r.step))

start(r::RangeFrom) = (1, r.start)
done(r::RangeFrom, st) = false
next(r::RangeFrom, st) = (st[2], (st[1]+1, r.start + st[1]*r.step))

start(r::RangeTo) = error("range has no initial value")
start(r::RangeBy) = error("range has no initial value")

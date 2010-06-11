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
                    ceil((r.stop-r.start+r.step) / r.step) :
                    ceil((r.start-r.stop-r.step) / -r.step))

start(r::Range) = r.start
done(r::Range, i) = (r.step < 0 ? (i < r.stop) : (i > r.stop))
next(r::Range, i) = (i, i+r.step)

start(r::RangeFrom) = r.start
done(r::RangeFrom) = false
next(r::RangeFrom, i) = (i, i+r.step)

start(r::RangeTo) = error("range has no initial value")
start(r::RangeBy) = error("range has no initial value")

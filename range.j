struct Range
    start
    step
    stop
end

struct RangeFrom
    start
    step
end

struct RangeTo
    step
    stop
end

struct RangeBy
    step
end

numel(r::Range) = length(r)
length(r::Range) = (r.step > 0 ?
    div((r.stop-r.start+1),r.step) :
    div((r.start-r.stop+1),-r.step))

start(r::Range) = r.start
done(r::Range, i) = (r.step < 0 ? (i < r.stop) : (i > r.stop))
next(r::Range, i) = (i, i+r.step)

start(r::RangeFrom) = r.start
done(r::RangeFrom) = false
next(r::RangeFrom, i) = (i, i+r.step)

start(r::RangeTo) = error("range has no initial value")
start(r::RangeBy) = error("range has no initial value")

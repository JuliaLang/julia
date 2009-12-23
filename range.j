type Range
    step
    start
    stop
end

type RangeFrom
    step
    start
end

type RangeTo
    step
    stop
end

type RangeBy
    step
end

# arguments always in the order start,step,stop
range(a,b,c) = new(Range,b,a,c)
rangefrom(a,b) = new(RangeFrom,b,a)
rangeto(b,c) = new(RangeTo,b,c)
rangeby(b) = new(RangeBy,b)

start(r::Range) = r.start
done(r::Range, i) = ((r.step <  0 && i < r.stop) ||
                     (r.step >= 0 && i > r.stop))
next(r::Range, i) = (i, i+r.step)

start(r::RangeFrom) = r.start
done(r::RangeFrom) = false
next(r::RangeFrom, i) = (i, i+r.step)

start(r::RangeTo) = error("range has no initial value")
start(r::RangeBy) = error("range has no initial value")

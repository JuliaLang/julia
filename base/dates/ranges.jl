# Date/DateTime Ranges
# Given a start and end date, how many steps/periods are in between
function Base.length(r::StepRange{Date,Day})
    n = integer(div(r.stop+r.step - r.start, r.step))
    isempty(r) ? zero(n) : n
end
function Base.length(r::StepRange{Date,Week})
    n = integer(div(r.stop+r.step - r.start, 7*value(r.step)))
    isempty(r) ? zero(n) : n
end
function Base.length{T<:Union(Week,Day,TimePeriod)}(r::StepRange{DateTime,T})
    n = integer(div(r.stop+r.step - r.start, toms(r.step)))
    isempty(r) ? zero(n) : n
end

function Base.length{T<:TimeType,P<:Period}(r::StepRange{T,P})
    isempty(r) && return 0
    start,stop = r.start > r.stop ? (r.stop,r.start) : (r.start,r.stop)
    step = r.step < zero(r.step) ? -r.step : r.step
    i = 0
    while (start+step*i) <= stop
        i += 1
    end
    return i
end
#Period overflow detection
function Base.length{T<:Period}(r::StepRange{T})
    isempty(r) && return zero(T)
    if r.step > one(T)
        return Base.checked_add(div(value(r.stop) - value(r.start), value(r.step)), int64(1))
    elseif value(r.step) < int64(-1)
        return Base.checked_add(div(value(r.start) - value(r.stop), -value(r.step)), int64(1))
    else
        Base.checked_add(div(Base.checked_sub(value(r.stop), value(r.start)), value(r.step)), int64(1))
    end
end

# Given a start and stop date, calculate the difference between
# the given stop date and the last valid date given the Period step
# last = stop - steprem(start,stop,step)
toobig(start::Date,stop::Date,step::Year) = (stop-start) > Day(3652425000*value(step))
toobig(start::Date,stop::Date,step::Month) = (stop-start) > Day(365242500*value(step))
toobig(start::DateTime,stop::DateTime,step::Year) = (stop-start) > Day(3652425000*value(step))
toobig(start::DateTime,stop::DateTime,step::Month) = (stop-start) > Day(365242500*value(step))

Base.steprem(a::Date,b::Date,c::Day) = (b-a) % c
Base.steprem(a::Date,b::Date,c::Week) = (b-a) % (7*value(c))

toms(c::Week)        = 604800000*value(c)
toms(c::Day)         = 86400000*value(c)
toms(c::Hour)        = 3600000*value(c)
toms(c::Minute)      = 60000*value(c)
toms(c::Second)      = 1000*value(c)
toms(c::Millisecond) = value(c)

Base.steprem(a::DateTime,b::DateTime,c::Union(Week,Day,TimePeriod)) = (b-a) % toms(c)
function Base.steprem(start::TimeType,stop::TimeType,step::Period)
    start,stop = start > stop ? (stop,start) : (start,stop)
    step = step < zero(step) ? -step : step
    toobig(start,stop,step) && throw(ArgumentError("Desired range is too big"))
    i = 1
    while (start+step*i) <= stop
        i += 1
    end
    return stop - (start+step*(i-1))
end

# Specialize for Date-Day, DateTime-Millisecond?
import Base.in
# TODO: use binary search
function in{T<:TimeType,S<:Period}(x, r::StepRange{T,S})
    isempty(r) && return false
    for d in r
        d == x && return true
    end
    return false
end

Base.start(r::StepRange{Date}) = 0
Base.next(r::StepRange{Date}, i) = (r.start+r.step*i,i+1)
Base.done{S<:Period}(r::StepRange{Date,S}, i::Integer) = length(r) <= i

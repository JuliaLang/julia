# This file is a part of Julia. License is MIT: https://julialang.org/license

# Date/DateTime Ranges

StepRange{<:Dates.DatePeriod,<:Real}(start, step, stop) =
    throw(ArgumentError("must specify step as a Period when constructing Dates ranges"))
Base.:(:)(a::T, b::T) where {T<:Date} = (:)(a, Day(1), b)

# Given a start and end date, how many steps/periods are in between. Packages define `guess`
# for their own TimeTypes, for example by forwarding to the DateTime method.
guess(a::DateTime, b::DateTime, c) = floor(Int64, (Int128(value(b)) - Int128(value(a))) / toms(c))
len(a::Time, b::Time, c) = Int64(div(value(b - a), tons(c)))
function len(a, b, c)
    lo, hi, st = min(a, b), max(a, b), abs(c)
    i = guess(a, b, c)
    v = lo + st * i
    prev = v  # Ensure `v` does not overflow
    while v <= hi && prev <= v
        prev = v
        v += st
        i += 1
    end
    return i - 1
end
# The largest n for which a + c * n does not pass b. Adding months keeps the time of day and
# clamps the day, so only the step landing in the month of b can pass it.
function len(a::T, b::T, c::Union{Year, Quarter, Month}) where {T <: Union{Date, DateTime, Timestamp}}
    months = value(Month(c))
    ya, ma, da = yearmonthday(a)
    yb, mb = yearmonth(b)
    n, r = fldmod(12 * (yb - ya) + mb - ma, months)
    if iszero(r)
        landed = (min(da, daysinmonth(yb, mb)), nsofday(a))
        target = (day(b), nsofday(b))
        n -= months > 0 ? landed > target : landed < target
    end
    return n
end
# Period ranges hook into Int64 overflow detection
Base.length(r::StepRange{<:Period}) = length(StepRange(value(r.start), value(r.step), value(r.stop)))
Base.checked_length(r::StepRange{<:Period}) = Base.checked_length(StepRange(value(r.start), value(r.step), value(r.stop)))

# A fixed step is a whole number of the units of `value` for Date, DateTime, and Timestamp, so
# their ranges map to ranges of `value` and behave like integer ranges at the ends of the type.
valuestep(::TimeType, step) = nothing
valuestep(::Date, step::Union{Day, Week}) = days(step)
valuestep(::DateTime, step::FixedPeriod) = toms(step)
valuestep(::Timestamp{P}, step::Union{FixedPeriod, TimePeriod}) where {P} =
    timestamp_period_ticks(P, step) % timestamp_count_type(P)
function valuerange(start, step, stop)
    s = valuestep(start, step)
    return isnothing(s) ? nothing : StepRange(value(start), s, value(stop))
end

function Base.length(r::StepRange{<:TimeType})
    vr = valuerange(r.start, r.step, r.stop)
    isnothing(vr) || return length(vr)
    return isempty(r) ? Int64(0) : len(r.start, r.stop, r.step) + 1
end
function Base.checked_length(r::StepRange{<:TimeType})
    vr = valuerange(r.start, r.step, r.stop)
    return isnothing(vr) ? length(r) : Base.checked_length(vr)
end

# Overload Base.steprange_last because `step::Period` may be a variable amount of time (e.g. for Month and Year)
function Base.steprange_last(start::T, step, stop) where T<:TimeType
    if isa(step, AbstractFloat)
        throw(ArgumentError("StepRange should not be used with floating point"))
    end
    vr = valuerange(start, step, stop)
    isnothing(vr) || return T(UTInstant(typeof(start.instant.periods)(Base.last(vr))))
    z = zero(step)
    step == z && throw(ArgumentError("step cannot be zero"))

    if stop == start
        last = stop
    else
        if (step > z) != (stop > start)
            last = Base.steprange_last_empty(start, step, stop)
        else
            # The month-based `len` of calendar steps works even when `stop - start` overflows
            if !(start isa Union{Date, DateTime, Timestamp} && step isa Union{Year, Quarter, Month})
                diff = stop - start
                if (diff > zero(diff)) != (stop > start)
                    throw(OverflowError("Difference between stop and start overflowed"))
                end
            end
            remain = stop - (start + step * len(start, stop, step))
            last = stop - remain
        end
    end
    return last
end

import Base.in
function in(x::T, r::StepRange{T}) where T<:TimeType
    vr = valuerange(first(r), step(r), last(r))
    isnothing(vr) || return value(x) in vr
    n = len(first(r), x, step(r)) + 1
    n >= 1 && n <= length(r) && r[n] == x
end

Base.iterate(r::StepRange{<:TimeType}) = length(r) <= 0 ? nothing : (r.start, (length(r), 1))
Base.iterate(r::StepRange{<:TimeType}, (l, i)) = l <= i ? nothing : (r.start + r.step * i, (l, i + 1))

+(x::Period, r::AbstractRange{<:TimeType}) = (x + first(r)):step(r):(x + last(r))
+(r::AbstractRange{<:TimeType}, x::Period) = x + r
-(r::AbstractRange{<:TimeType}, x::Period) = (first(r)-x):step(r):(last(r)-x)
*(x::Period, r::AbstractRange{<:Real}) = (x * first(r)):(x * step(r)):(x * last(r))
*(r::AbstractRange{<:Real}, x::Period) = x * r
/(r::AbstractRange{<:P}, x::P) where {P<:Period} = (first(r)/x):(step(r)/x):(last(r)/x)

# Combinations of types and periods for which the range step is regular
Base.RangeStepStyle(::Type{<:OrdinalRange{<:TimeType, <:FixedPeriod}}) =
    Base.RangeStepRegular()
